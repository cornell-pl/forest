module Language.Pads.PadsParser where

import qualified Language.Pads.Source as S
import qualified Language.Pads.Errors as E
import Language.Pads.MetaData
import Char

{- Parsing Monad -}
{- Invariant: Good [] never arises -}
data Result a =  Good [a] | Bad a
  deriving Show
newtype PadsParser a = PadsParser (S.Source -> Result (a,S.Source))

runPP :: PadsParser t -> S.Source -> Result (t, S.Source)
runPP (PadsParser p) = p

appendResult :: Result a -> Result a -> Result a
appendResult (Good r) (Good s) = Good (r++s)
appendResult (Good r) _ = Good r
appendResult _ (Good r) = Good r
appendResult (Bad r1) (Bad r2) = Bad r1

alwaysSucceeds :: PadsParser a -> PadsParser a
alwaysSucceeds (PadsParser p)= PadsParser $ \s -> reallyGood (p s)

reallyGood ~(Good (x:xs)) = Good (x:xs)


concatResult = foldr appendResult (Bad (error "Should never arise: empty good list"))

instance Monad PadsParser where
  return r = PadsParser (\bs -> Good [(r,bs)])
  p >>= f  = PadsParser $ \bs -> 
               case runPP p bs of
                  Good results -> 
                       concatResult [runPP (f a) bs' | (a,bs') <- results ]
                  Bad (errVal,bs') -> case runPP (f errVal) bs' of
                                         Good results -> Bad (head results)   -- Should always succeed by Result invariant.
                                         Bad v -> Bad v
badReturn  r = PadsParser $ \bs -> Bad (r,bs)
goodReturn r = PadsParser $ \bs -> Good [(r,bs)]
mdReturn r @ (rep,md) = PadsParser $ \bs -> 
    if numErrors (get_md_header md) == 0 
    then Good [(r,bs)]
    else Bad (r,bs)

commit :: PadsParser a -> PadsParser a
commit p = PadsParser $ \bs -> 
               case runPP p bs of
                  Good [] -> Good []
                  Good (x:xs) -> Good [x]      
                  Bad x   -> Bad x

eitherP :: PadsParser a -> PadsParser a -> PadsParser a
eitherP p q = PadsParser $ \s -> 
   runPP p s `appendResult` runPP q s 

choiceP :: [PadsParser a] -> PadsParser a
choiceP ps = foldr1 eitherP ps

{- 
This combinator seems natural because it is the unit for eitherP, but it breaks the 
invariant on Result.  So, either we don't need failP or the invariant is wrong.

failP :: PadsParser [a]
failP = PadsParser $ \s -> Good []
-}


productive :: PadsParser a -> PadsParser (Maybe a)
productive p = PadsParser $ \s -> 
    case runPP p s of
       Good v  -> case [ (Just results,s') | (results,s') <- v, not (S.eqCurrent s s')] of
                    [] -> Bad (Nothing,s)
                    w  -> Good w
       Bad (r,s) -> Bad (Nothing,s)

parseAll :: PadsParser(rep,md) -> PadsParser([rep],[md])
parseAll p = ifEofP $ do 
  m <- productive p 
  case m of Nothing -> return ([],[])
            Just (r,md) -> do 
               (rs,mds) <- parseAll p
               return (r:rs, md:mds)


parseAllS :: PadsParser (rep,md) -> String -> ([rep],[md])
parseAllS p inputS = 
  let psource = (S.padsSourceFromString inputS)
  in case runPP (parseAll p) psource of
      Good ((r,rest):alternates) -> r
      Bad (r,rest) -> r

{-

meta p s = PadsParser $ \s ->
   case runPP p s of    

idea:
 turn Good gs with meta-data problems into Bad
 in either onFail or appendResult

-}


onFail :: PadsParser a -> PadsParser a -> PadsParser a
onFail p q = PadsParser $ \s -> 
             case runPP p s of 
               Bad a   -> runPP q s
--               Good [] -> runPP q s
               Good v  -> Good v

replaceSource :: S.Source -> (Result (a,S.Source)) -> (Result (a,S.Source))
replaceSource s res = case res of
  Good gs     -> Good $ map (\(v,_)->(v,s)) gs
  Bad  (a,_)  -> Bad (a,s)

parseTry :: PadsParser a -> PadsParser a
parseTry p = PadsParser $ \s ->  replaceSource s (runPP p s)


parseOpt :: PadsParser a -> a -> PadsParser a
parseOpt p x = p `onFail` return x

primPads :: (S.Source -> (a,S.Source)) -> PadsParser a
primPads f = PadsParser $ \s -> Good [f s]

queryPads :: (S.Source -> a) -> PadsParser a
queryPads f = PadsParser $ \s -> Good [(f s,s)]

getPos :: PadsParser S.Pos
getPos = queryPads S.getSrcPos

isEofP :: PadsParser Bool 
isEofP = queryPads S.isEOF

ifEofP :: PadsParser ([rep],[md]) -> PadsParser ([rep],[md])
ifEofP p = do
  b <- isEofP
  if b then return ([],[]) else p


peakHeadP :: PadsParser Char 
peakHeadP = queryPads S.head

takeHeadP :: PadsParser Char
takeHeadP = primPads S.takeHead

takeHeadStrP :: String -> PadsParser Bool
takeHeadStrP str = primPads (S.takeHeadStr str)

-- Return string is junk before found string
scanStrP :: String -> PadsParser (Maybe String)
scanStrP str = primPads (S.scanStr str)

takeP :: Integral a => a -> PadsParser String
takeP n = primPads (S.take (fromInteger $ toInteger n))

regexMatchP :: S.RE -> PadsParser (Maybe String)
regexMatchP re = primPads (S.regexMatch re)

regexStopP :: S.RE -> PadsParser (Maybe String)
regexStopP re = primPads (S.regexStop re)

scanP :: Char -> PadsParser Bool
scanP c = primPads (\s -> let (f,r,e) = S.scanTo c s in (f,r))

satisfy p = primPads loop
 where loop s = if S.isEOF s || S.isEOR s then ([],s) else
          let c = S.head s in
          if p c then 
            let (xs,s') = loop (S.tail s) in
            (c:xs, s')
          else
            ([],s)

digitListToInt :: Bool->[Char] -> Int
digitListToInt isNeg digits = let raw = foldl (\a d ->10*a + (Char.digitToInt d)) 0 digits
   in if isNeg then negate raw else raw

isEorP :: PadsParser Bool
isEorP = queryPads S.isEOR

ifEorP :: PadsParser ([rep],[md]) -> PadsParser ([rep],[md])
ifEorP p = do
  b <- isEorP
  if b then return ([],[]) else p


lineBegin,lineEnd :: PadsParser (Maybe String)
lineBegin = primPads S.srcLineBegin
lineEnd = primPads S.srcLineEnd

doLineEnd :: PadsParser ((), Base_md)
doLineEnd = do
  rendErr <- lineEnd
  case rendErr of
    Nothing -> goodReturn ((), cleanBasePD)
    Just err -> do p <- getPos
                   badReturn ((), mkErrBasePD (E.LineError err) (Just p))

doLineBegin :: PadsParser ((), Base_md)
doLineBegin = do
  rbegErr <- lineBegin
  case rbegErr of
    Nothing -> return ((), cleanBasePD)
    Just err -> do p <- getPos
                   badReturn ((), mkErrBasePD (E.LineError err) (Just p))


parseLine :: PadsMD md => PadsParser (r,md) -> PadsParser (r,md)
parseLine p = commit $ do 
   (_,bmd) <- doLineBegin
   (r, md) <- p
   (_,emd) <- doLineEnd
   let hmd = get_md_header md
   let new_hd = mergeBaseMDs [bmd,hmd,emd]
   let new_md = replace_md_header md new_hd
   return (r,new_md)


many1 p = do {x <-p; xs <- many p; return (x:xs)}

many :: PadsParser a -> PadsParser [a]
many p = scan id
       where scan f = do { x <- p     
                         ; scan (\tail -> f (x:tail))
                         } 
                      `onFail`
                         (return (f []))

parseMany' :: PadsMD md => PadsParser (rep,md) -> PadsParser [(rep,md)]
parseMany' p =  alwaysSucceeds (
                      do { (r,m) <- p     
                         ; rms <- parseMany' p
                         ; return ((r,m):rms)  
                         } 
                      `onFail`
                         return [])

parseMany :: PadsMD md => PadsParser (rep,md) -> PadsParser [(rep,md)]
parseMany p = scan id
       where scan f = alwaysSucceeds $ 
                      do { (r,m) <- p     
                         ; if (numErrors (get_md_header m)) == 0 then scan (\tail -> f ((r,m):tail)) else badReturn [(r,m)]
                         } 
                      `onFail`
                         (return (f []))


parseSepBy1 :: (PadsMD md, PadsMD mdSep) => PadsParser (repSep,mdSep) -> PadsParser(rep, md) -> PadsParser [(rep,md)]
parseSepBy1 sep p = do { rm <- p
                       ; rms <- parseMany (sep >> p)
                       ; return (rm : rms)
                       }

parseSepBy :: (PadsMD md, PadsMD mdSep) => PadsParser (repSep,mdSep) -> PadsParser(rep, md) -> PadsParser [(rep,md)]
parseSepBy sep p = parseSepBy1 sep p `onFail` (return [])

parseCount :: (PadsMD md) => Int -> PadsParser(rep, md) -> PadsParser [(rep,md)]
parseCount n p  | n <= 0    = return []
                | otherwise = sequence (replicate n p)

parseCountSep :: (PadsMD md) => Int -> PadsParser (repSep,mdSep) -> PadsParser(rep, md) -> PadsParser [(rep,md)]
parseCountSep n sep p  | n <= 0    = return []
                       | otherwise = sequence (p : replicate (n - 1)  (sep>>p))

parseManyTill :: (PadsMD md, PadsMD mdTerm) =>  PadsParser (repTerm,mdTerm) -> PadsParser(rep, md) -> PadsParser [(rep,md)]
parseManyTill term p = scan
                     where 
                       scan = do { term; return [] }
                          `onFail` 
                              do { b <- isEofP
                                 ; if b then goodReturn [] else badReturn [] }
                          `onFail` 
                              do { rm <- p; rms <- scan; return (rm:rms) }

parseManyTillSep :: (PadsMD md, PadsMD mdSep, PadsMD mdTerm) =>  
                     PadsParser (repSep,mdSep) -> PadsParser (repTerm,mdTerm) -> PadsParser(rep, md) -> PadsParser [(rep,md)]
parseManyTillSep sep term p = (termOrEof []) `onFail` scan
                     where 
                       scan = do { (rep, md) <- p
                                 ; sepPos <- getPos
                                 ; sepResult <- scanForUntil [] sep (termOrEof [(rep,md)])
                                 ; case sepResult of
                                     Left [] ->       -- Found termination condition, no extra junk.  We're done.
                                          goodReturn [(rep,md)]
                                     Left junk  ->    -- Found termination condition; but there was extra junk
                                          do { let mdSep   = mkErrBasePD (E.ExtraStuffBeforeTy junk "seperator" ) (Just sepPos)
                                             ; let mdHead  = get_md_header md
                                             ; let mergeMD = mergeBaseMDs [mdHead,mdSep]
                                             ; let md'     = replace_md_header md mergeMD 
                                             ; badReturn [(rep,md')]
                                             }
                                     Right [] ->     -- Found separator, no extra junk.  Look for next element.
                                          do { rms <- scan 
                                             ; return ((rep,md):rms) }   
                                     Right junk ->   -- Found separator, but with extra junk.  Look for next element
                                          do { let mdSep   = mkErrBasePD (E.ExtraStuffBeforeTy junk "seperator" ) (Just sepPos)
                                             ; let mdHead  = get_md_header md
                                             ; let mergeMD = mergeBaseMDs [mdHead,mdSep]
                                             ; let md'     = replace_md_header md mergeMD 
                                             ; rms <- scan
                                             ; badReturn ((rep,md'):rms)
                                             }
                                 }
                       termOrEof r = do { term; goodReturn r }
                          `onFail` 
                                     do { b <- isEofP
                                        ; if b then goodReturn r else badReturn r }


scanForUntil s sep end = do { end        
                            ; goodReturn (Left (reverse s))
                            }
                       `onFail` 
                         do { sep
                            ; goodReturn (Right (reverse s))
                            } 
                       `onFail`
                         do { c <- takeHeadP
                            ; scanForUntil (c:s) sep end
                            }




parseList  :: (Monad m, PadsMD b) =>  t -> (t -> m [(a, b)]) -> m ([a], (Base_md, [b]))
parseList p combine = do 
  elems <- combine p
  let (reps, mds) = unzip elems
  let hmds = map get_md_header mds
  let md = (mergeBaseMDs hmds, mds)
  return (reps,md)

parseListNoTermNoSep :: PadsMD md => PadsParser (rep,md) -> PadsParser ([rep], (Base_md, [md]))
parseListNoTermNoSep p = parseList p parseMany

parseListNoTermSep :: (PadsMD md, PadsMD mdSep) => PadsParser (repSep,mdSep) -> PadsParser(rep, md) -> PadsParser ([rep], (Base_md, [md]))
parseListNoTermSep sep p = parseList p (parseSepBy sep)

parseListTermLengthNoSep :: (PadsMD md) => Int -> PadsParser(rep, md) -> PadsParser ([rep], (Base_md, [md]))
parseListTermLengthNoSep i p = parseList p (parseCount i)

parseListTermLengthSep :: (PadsMD md, PadsMD mdSep) => Int -> PadsParser (repSep,mdSep) -> PadsParser(rep, md) -> PadsParser ([rep], (Base_md, [md]))
parseListTermLengthSep n sep p = parseList p (parseCountSep n sep)

parseListTermNoSep :: (PadsMD md, PadsMD mdTerm) => 
                     PadsParser (repTerm,mdTerm) -> PadsParser(rep, md) -> PadsParser ([rep], (Base_md, [md]))
parseListTermNoSep term p = parseList p (parseManyTill term)

parseListTermSep :: (PadsMD md, PadsMD mdSep, PadsMD mdTerm) => 
                     PadsParser (repSep,mdSep) -> PadsParser (repTerm,mdTerm) -> PadsParser(rep, md) -> PadsParser ([rep], (Base_md, [md]))
parseListTermSep sep term p = parseList p (parseManyTillSep sep term)




parseNothing :: PadsParser (Maybe t, (Base_md, Maybe s))
parseNothing = return (Nothing, (cleanBasePD, Nothing))
                        
parseJust :: PadsMD md => PadsParser (rep,md) -> PadsParser (Maybe rep, (Base_md, Maybe md))
parseJust p = do
  (r,md) <- p
  let bmd = get_md_header md
  let result = (Just r, (bmd, Just md))
  mdReturn result

parseMaybe :: PadsMD md => PadsParser (rep,md) -> PadsParser (Maybe rep, (Base_md, Maybe md))
parseMaybe p = choiceP [parseJust p, parseNothing]

