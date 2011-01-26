{-# LANGUAGE NamedFieldPuns #-}

module Language.Pads.PadsParser where

-- These are the combinators used to build PADS parsers

import qualified Language.Pads.Source as S
import Language.Pads.Errors
import Language.Pads.MetaData
import Language.Pads.RegExp
import Char

import Control.Monad

{- Parsing Monad -}

newtype PadsParser a = PadsParser { (#) :: S.Source -> Result (a,S.Source) }
data Result a = Good a | Bad a
  deriving Show



val :: PadsParser t -> S.Source -> t
val p bs = case p # bs of
             Good (v,bs') -> v
             Bad (v,bs')  -> v

parseStringInput pp cs = case pp #  (S.padsSourceFromString cs) of
                           Good (r,rest) -> (r, S.padsSourceToString rest)  
                           Bad  (r,rest) -> (r, S.padsSourceToString rest)  

instance Functor PadsParser where
  fmap f p = PadsParser $ \bs -> case p # bs of
                                  Good (x,bs') -> Good (f x, bs')
                                  Bad (x,bs')  -> Bad (f x, bs')

-- if any results on the way are bad, then the whole thing will be bad
instance Monad PadsParser where
  return r = PadsParser $ \bs -> Good (r,bs)
  p >>= f  = PadsParser $ \bs -> 
               case p # bs of
                  Good (v,bs') -> f v # bs'
                  Bad (v,bs')  -> case f v # bs' of
                                    Good (w,bs'') -> Bad (w,bs'')
                                    Bad (w,bs'')  -> Bad (w,bs'')

goodReturn r = return r
badReturn  r = PadsParser $ \bs -> Bad (r,bs)
mdReturn r @ (rep,md) = PadsParser $ \bs -> 
    if numErrors (get_md_header md) == 0 
    then Good (r,bs)
    else Bad  (r,bs)

returnClean x         = return (x, cleanBasePD)
returnError x err loc = badReturn (x, mkErrBasePDfromLoc err loc)




--------------------------
-- Source manipulation functions


queryP :: (S.Source -> a) -> PadsParser a
queryP f = PadsParser $ \bs -> Good (f bs,bs)

primPads :: (S.Source -> (a,S.Source)) -> PadsParser a
primPads f = PadsParser $ \bs -> Good (f bs)

liftStoP :: (S.Source -> Maybe (a,S.Source)) -> a -> PadsParser a
liftStoP f def = PadsParser $ \bs -> 
                 case f bs of
                   Nothing      -> Bad (def,bs)
                   Just (v,bs') -> Good (v,bs')

replaceSource :: S.Source -> (Result (a,S.Source)) -> (Result (a,S.Source))
replaceSource bs res = case res of
  Good (v,_) -> Good (v,bs)
  Bad  (v,_) -> Bad (v,bs)



------- Choice
-- The monad is non-backtracking. The only choice point is at ChoiceP

choiceP :: [PadsParser a] -> PadsParser a
choiceP ps = foldr1 (<||>) ps

(<||>) :: PadsParser a -> PadsParser a -> PadsParser a
p <||> q = PadsParser $ \bs -> (p # bs) <++> (q # bs) 

(<++>) :: Result a -> Result a -> Result a
(Good r)  <++> _  = Good r
(Bad r1)  <++> r2 = r2 -- A number of functions rely on r2




-------------------------

parseMaybe :: PadsMD md => PadsParser (rep,md) -> PadsParser (Maybe rep, (Base_md, Maybe md))
parseMaybe p = choiceP [parseJust p, parseNothing]

parseJust :: PadsMD md => PadsParser (rep,md) -> PadsParser (Maybe rep, (Base_md, Maybe md))
parseJust p = do
  (r,md) <- p
  let bmd = get_md_header md
  return (Just r, (bmd, Just md))

parseNothing :: PadsParser (Maybe t, (Base_md, Maybe s))
parseNothing = return (Nothing, (cleanBasePD, Nothing))
                        

-----------------------

parseTry :: PadsParser a -> PadsParser a
parseTry p = PadsParser $ \bs -> replaceSource bs (p # bs)


-------------

parseConstraint :: PadsMD md => PadsParser(rep,md) -> ( rep -> md -> Bool) -> PadsParser(rep, (Base_md, md))
parseConstraint p pred = do 
 { (rep,md) <- p
 ; let bmd @ Base_md{numErrors, errInfo} = get_md_header md
 ; let (isGood, totErrors) = if pred rep md then (True, numErrors) else (False, numErrors + 1)
 ; let constraint_md = Base_md { numErrors = totErrors
                               , errInfo = buildError totErrors isGood errInfo}
 ; return (rep, (constraint_md, md))
 }


buildError numErrs isGood errInfo = 
 if numErrs == 0 then Nothing
 else 
   Just(ErrInfo{msg = if isGood then FUnderlyingTypedefFail
                                  else FPredicateFailure,
                position = join $ fmap position errInfo})

-------------------------------------------------


parseListNoTermNoSep :: PadsMD md => PadsParser (rep,md) -> PadsParser ([rep], (Base_md, [md]))
parseListNoTermNoSep p = buildListReport (parseMany p)

parseListNoTermSep :: (PadsMD md, PadsMD mdSep) => PadsParser (repSep,mdSep) -> PadsParser(rep, md) -> PadsParser ([rep], (Base_md, [md]))
parseListNoTermSep sep p = buildListReport (parseManySep sep p)

parseListTermLengthNoSep :: (PadsMD md) => Int -> PadsParser(rep, md) -> PadsParser ([rep], (Base_md, [md]))
parseListTermLengthNoSep i p = buildListReport (parseCount i p)

parseListTermLengthSep :: (PadsMD md, PadsMD mdSep) => Int -> PadsParser (repSep,mdSep) -> PadsParser(rep, md) -> PadsParser ([rep], (Base_md, [md]))
parseListTermLengthSep n sep p = buildListReport (parseCountSep n sep p)

parseListTermNoSep :: (PadsMD md, PadsMD mdTerm) => 
                     PadsParser (repTerm,mdTerm) -> PadsParser(rep, md) -> PadsParser ([rep], (Base_md, [md]))
parseListTermNoSep term p = buildListReport (parseManyTerm term p)

parseListTermSep :: (PadsMD md, PadsMD mdSep, PadsMD mdTerm) => 
                     PadsParser (repSep,mdSep) -> PadsParser (repTerm,mdTerm) -> PadsParser(rep, md) -> PadsParser ([rep], (Base_md, [md]))
parseListTermSep sep term p = buildListReport (parseManySepTerm sep term p)


buildListReport  :: (Monad m, PadsMD b) => m [(a, b)] -> m ([a], (Base_md, [b]))
buildListReport p = do 
  elems <- p
  let (reps, mds) = unzip elems
  let hmds = map get_md_header mds
  let md = (mergeBaseMDs hmds, mds)
  return (reps,md)


-----------------------------------

parseMany :: PadsMD md => PadsParser (rep,md) -> PadsParser [(rep,md)]
parseMany p = do { (r,m) <- p
                 ; if (numErrors (get_md_header m) == 0)
                   then do { rms <- parseMany p
                           ; return ((r,m) : rms)}
                   else badReturn [] -- terminate the recursion if errors found
                                     -- return would work too, as p will create the Bad
                 } 
            <||>
              return []



-- BUG :: If (sep>>p) is unproductive, it terminates, but returns Good
parseManySep :: (PadsMD md, PadsMD mdSep) => PadsParser (repSep,mdSep) -> PadsParser(rep, md) -> PadsParser [(rep,md)]
parseManySep sep p = do { rm <- p
                        ; rms <- parseManySep1 sep p
                        ; return (rm : rms)
                        }

parseManySep1 :: (PadsMD md, PadsMD mdSep) => PadsParser (repSep,mdSep) -> PadsParser(rep, md) -> PadsParser [(rep,md)]
parseManySep1 sep p = do { (r,m) <- sep
                         ; if (numErrors (get_md_header m) == 0)
                           then parseManySep sep p
                           else badReturn [] -- terminate the recursion if separator error found
                                             -- return would work too, as p will create the Bad
                         } 
                      <||>
                        return []

-----------------------------------


parseCount :: (PadsMD md) => Int -> PadsParser(rep, md) -> PadsParser [(rep,md)]
parseCount n p  | n <= 0    = return []
                | otherwise = sequence (replicate n p)


parseCountSep :: (PadsMD md) => Int -> PadsParser rmdSep -> PadsParser(rep, md) -> PadsParser [(rep,md)]
parseCountSep n sep p  | n <= 0    = return []
parseCountSep n sep p = do { rm <- p
                           ; rms <- parseCountSep1 (n-1) sep p
                           ; return (rm:rms) }

parseCountSep1 n sep p  | n <= 0    = return []
parseCountSep1 n sep p = do { smd <- sep -- <||> scanForSep [] sep
                            ; parseCountSep n sep p  -- we lose the metadata for sep, which will detail any problems
                            }

-----------------------------------



parseManyTerm :: (PadsMD md, PadsMD mdTerm) =>  PadsParser (repTerm,mdTerm) -> PadsParser(rep, md) -> PadsParser [(rep,md)]
parseManyTerm term p = (term >> return [])
                  <||> (ifEOFP >> return [])
                  <||> do { rm <- p
                          ; rms <- parseManyTerm term p
                          ; return (rm:rms) }



parseManySepTerm :: (PadsMD md, PadsMD mdSep, PadsMD mdTerm) =>  
                     PadsParser (repSep,mdSep) -> PadsParser (repTerm,mdTerm) -> PadsParser(rep, md) -> PadsParser [(rep,md)]
parseManySepTerm sep term p = (term >> return [])
                         <||> (ifEOFP >> return [])
                         <||> scan
   where 
	   scan = do { (rep, md) <- p
	             ; sepLoc <- getLoc
  		     ; (terminated,junk) <- seekSep sep term
	             ; if terminated then
			      case junk of
	         	        { []  -> return [(rep,md)]
                                ;  _  -> badReturn [(rep,buildJunkReport md sepLoc junk)]
                                }
		       else
		    	     do { rms <- scan 
				; case junk of
                                    { [] -> return ((rep,md):rms)  
                                    ; _  -> badReturn ((rep,buildJunkReport md sepLoc junk) : rms)
                                    }
				}
                     }

seekSep sep term = (term >> return (True, []))
              <||> (ifEOFP >> return (True, []))
              <||> (sep >> return (False, []))
              <||> recoverSep sep term

recoverSep sep term = do { b <- isEORP
                         ; if b then badReturn (False, []) else
                           do { c <- takeHeadP
                              ; (b,cs) <- seekSep sep term
                              ; badReturn (b, c:cs) 
                              }
                         }

buildJunkReport md loc junk = md'
  where
    mdSep   = mkErrBasePDfromLoc (ExtraStuffBeforeTy junk "seperator" ) loc
    mdHead  = get_md_header md
    mergeMD = mergeBaseMDs [mdHead,mdSep]
    md'     = replace_md_header md mergeMD 



-------------------------------------------------



getLoc :: PadsParser S.Loc
getLoc = queryP S.getSrcLoc

isEOFP, isEORP :: PadsParser Bool 
isEOFP = queryP S.isEOF
isEORP = queryP S.isEOR

ifEOFP, ifEORP :: PadsParser () 
ifEOFP = do { b <- isEOFP; if b then return () else badReturn ()}
ifEORP = do { b <- isEORP; if b then return () else badReturn ()}


{- Is this what we should do if there isn't sufficient input? -}
takeP_new :: Int -> PadsParser String
takeP_new n = liftStoP (S.take' (fromInt n)) (take n def)
  where
    def = '0':def

takeP :: Integral a => a -> PadsParser String
takeP n = primPads (S.take (fromInt n))

fromInt :: (Integral a1, Num a) => a1 -> a
fromInt n = fromInteger $ toInteger n


-------------------------------------------------


matchP :: String -> PadsParser ()
matchP str = fmap (const ()) $ liftStoP (S.matchString str) undefined

whileP :: (Char -> Bool) -> PadsParser String
whileP pred = liftStoP (S.whileS pred) undefined

satP :: (Char -> Bool) -> PadsParser Char
satP pred = do [c] <- takeP 1
               if pred c then return c else badReturn '0'



peakHeadP :: PadsParser Char 
peakHeadP = queryP S.head

takeHeadP :: PadsParser Char
takeHeadP = primPads S.takeHead

takeHeadStrP :: String -> PadsParser Bool
takeHeadStrP str = primPads (S.takeHeadStr str)

-- Return string is junk before found string
scanStrP :: String -> PadsParser (Maybe String)
scanStrP str = primPads (S.scanStr str)


regexMatchP ::RE -> PadsParser (Maybe String)
regexMatchP re = primPads (S.regexMatch re)

regexStopP :: RE -> PadsParser (Maybe String)
regexStopP re = primPads (S.regexStop re)

scanP :: Char -> PadsParser Bool
scanP c = primPads (\s -> let (f,r,e) = S.scanTo c s in (f,r))

getAllP :: PadsParser String
getAllP = primPads S.drainSource

getAllBinP :: PadsParser S.RawStream
getAllBinP = primPads S.rawSource

satisfy p = primPads loop
 where loop s = if S.isEOF s || S.isEOR s then ([],s) else
          let c = S.head s in
          if p c then 
            let (xs,s') = loop (S.tail s) in
            (c:xs, s')
          else
            ([],s)

digitListToInt :: Bool -> [Char] -> Int
digitListToInt isNeg digits = let raw = foldl (\a d ->10*a + (Char.digitToInt d)) 0 digits
   in if isNeg then negate raw else raw


lineBegin,lineEnd :: PadsParser (Maybe String)
lineBegin = primPads S.srcLineBegin
lineEnd = primPads S.srcLineEnd

doLineEnd :: PadsParser ((), Base_md)
doLineEnd = do
  rendErr <- lineEnd
  case rendErr of
    Nothing -> return ((), cleanBasePD)
    Just err -> do loc <- getLoc
                   returnError () (LineError err) loc

doLineBegin :: PadsParser ((), Base_md)
doLineBegin = do
  rbegErr <- lineBegin
  case rbegErr of
    Nothing -> return ((), cleanBasePD)
    Just err -> do loc <- getLoc
                   returnError () (LineError err) loc


parseLine :: PadsMD md => PadsParser (r,md) -> PadsParser (r,md)
parseLine p =  do 
   (_,bmd) <- doLineBegin
   (r, md) <- p
   (_,emd) <- doLineEnd
   let hmd = get_md_header md
   let new_hd = mergeBaseMDs [bmd,hmd,emd]
   let new_md = replace_md_header md new_hd
   return (r,new_md)





