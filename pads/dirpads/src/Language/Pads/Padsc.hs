{-# LANGUAGE TypeSynonymInstances, GeneralizedNewtypeDeriving #-}
module Language.Pads.Padsc where

import Language.Pads.Syntax as PS
import Language.Haskell.TH as TH
import Language.Haskell.TH.Syntax
import Data.Data
import Data.List
import qualified Data.Map as M
import Monad
import Char
import Text.PrettyPrint.Mainland as PP
import qualified Language.Pads.Source as S
import qualified Language.Pads.Errors as E


{- Pretty prints a list of declarations -}
ppP d = liftM ppr_list (runQ d)

{- Base type library support -}
data Base_md = Base_md { numErrors :: Int
                       , errInfo   :: Maybe E.ErrInfo
                        -- Need to add location information, etc.
                       }
   deriving (Typeable, Data, Eq)

pprBaseMD Base_md {numErrors=num, errInfo = info} = text "Errors:" <+> PP.ppr num <+> 
                                                    case info of Nothing -> empty
                                                                 Just e -> PP.ppr e

instance Pretty Base_md where
  ppr = pprBaseMD 

cleanBasePD = Base_md {numErrors = 0, errInfo = Nothing }
mkErrBasePD msg pos = Base_md {numErrors = 1, 
                               errInfo = Just (E.ErrInfo{msg=msg,position=pos}) }

mergeBaseMDs mds = case mds of 
                     [] -> cleanBasePD
                     otherwise ->  Data.List.foldl1 (\(Base_md {numErrors=num1,errInfo=i1}) (Base_md {numErrors=num2,errInfo=i2}) ->
                                                          (Base_md {numErrors=num1 + num2, errInfo= Nothing })) mds

{- Generic function for computing the default for any type supporting Data a interface -}
getConstr :: DataType -> Constr
getConstr ty = 
   case dataTypeRep ty of
        AlgRep cons -> head cons
        IntRep      -> mkIntegralConstr ty 0
        FloatRep    -> mkRealConstr ty 0.0 
        CharRep     -> mkCharConstr ty '\NUL'
        NoRep       -> error "PADSC: Unexpected NoRep in PADS type"

gdef :: Data a => a
gdef = def_help 
  where
    def_help
     =   let ty = dataTypeOf (def_help)
             constr = getConstr ty
         in fromConstrB gdef constr 


{- Meta data type class -}
class PadsMD md where
  get_md_header :: md -> Base_md
  replace_md_header :: md -> Base_md -> md

instance PadsMD Base_md where
  get_md_header b = b
  replace_md_header old new = new

instance PadsMD (Base_md,b) where
  get_md_header (h,b) = h
  replace_md_header (h1,b) h2 = (h2,b)

{-
instance PadsMD md => PadsMD (rep,md) where
  get_md_header (r,md) = get_md_header md
  replace_md_header (r,md1) md2 = (r,replace_md_header md1 md2)
-}

{-
We'd like to have a type class like the following so that we can have a single name for the PADS parsers instead
of having to have pads_foo, pads_bar, etc.  

But, with parameterized types, such parse functions take an extra argument. 
At any PADS call site, the compiler knows what values to supply.
At the top-level though, we don't.  
We could define the top-level function parse to be the type-specific version parse_bar applied to a set of
default arguments.  That would work as long as there were sensible defaults for the arguments...

The compiler would need to call the type-specific versions, though.
-}

{-
 We are now generating one PADS instance for the pads type, not for the rep type.
 We could add a method strip :: pads -> rep that could be coded generically (?) with some 
  loss of abstraction, or could be generated...  Omit for now.
-}

class (Data pads, PadsMD md) => Pads pads md | pads -> md  where
  def :: pads
  def = gdef
  getMetaD :: (rep, md) -> Base_md
  getMetaD (rep, md) = get_md_header md
  parsePP  :: PadsParser(pads,md)
  parseS   :: String -> ((pads, md), String) 
  parseS cs = case (runPP parsePP  (S.padsSourceFromString cs)) of
                 Good ((r,rest):alternates) -> (r, S.padsSourceToString rest)  
                 Bad   (r,rest)             -> (r, S.padsSourceToString rest)  


class Data pads => Pads1 arg pads md | pads->md, pads->arg where
  def1 :: arg -> pads
  def1 =  \_ -> gdef
  parsePP1  :: arg -> PadsParser(pads,md)
  parseS1   :: arg -> String -> ((pads, md), String) 
  parseS1 arg cs = case (runPP (parsePP1 arg)) (S.padsSourceFromString cs) of
                      Good ((r,rest):alternates) -> (r, S.padsSourceToString rest)  
                      Bad   (r,rest)             -> (r, S.padsSourceToString rest)  


{-  Base types 
    Pads type name, underlying representation type name, list of parameter types.
-}

baseTypesList = [
  ("Pint",      (''Int,     [])),
  ("Pchar",     (''Char,    [])),
  ("Pdigit",    (''Int,    [])),
  ("Pstring",   (''String,  [''Char])),
  ("PstringFW", (''String,  [''Int])),
  ("PstringME", (''String,  [''S.RE])),
  ("PstringSE", (''String,  [''S.RE]))
 ]


baseTypesMap :: M.Map String (Name, [Name]) = M.fromList baseTypesList

{- XXX: These declarations should be generated from table above . -}
{- XXX: Default values should be defined to use parameter, so for example, we can generate a string of the proper length. -}
newtype Pint = Pint Int
  deriving (Eq, Show, Data, Typeable, Num, Ord, Integral, Real, Enum)
newtype Pchar = Pchar Char
  deriving (Eq, Show, Data, Typeable, Ord)
newtype Pdigit = Pdigit Int
  deriving (Eq, Show, Data, Typeable, Num, Ord, Integral, Real, Enum)

instance Pretty Pchar where
  ppr (Pchar c) = text (show c)

instance Pads Pint Base_md where
  parsePP = pint_parseM

instance Pads Pchar Base_md where
  parsePP = pchar_parseM

instance Pads Pdigit Base_md where
  parsePP = pdigit_parseM

newtype Pstring    = Pstring    String
  deriving (Eq, Show, Data, Typeable)
newtype PstringFW = PstringFW String
  deriving (Eq, Show, Data, Typeable)
newtype PstringME = PstringME String
  deriving (Eq, Show, Data, Typeable)
newtype PstringSE = PstringSE String
  deriving (Eq, Show, Data, Typeable)

instance Pads1 Char Pstring Base_md where 
  parsePP1 = pstring_parseM 

instance Pads1 Int PstringFW Base_md where 
  parsePP1  = pstringFW_parseM 

instance Pads1 S.RE PstringME Base_md where
  parsePP1 = pstringME_parseM

instance Pads1 S.RE PstringSE Base_md where
  parsePP1 = pstringSE_parseM


{- Parsing Monad -}
{- Invariant: Good [] never arises -}
data Result a =  Good [a] | Bad a
newtype PadsParser a = PadsParser (S.Source -> Result (a,S.Source))

runPP :: PadsParser t -> S.Source -> Result (t, S.Source)
runPP (PadsParser p) = p

appendResult :: Result a -> Result a -> Result a
appendResult (Good r) (Good s) = Good (r++s)
appendResult (Good r) _ = Good r
appendResult _ (Good r) = Good r
appendResult (Bad r1) (Bad r2) = Bad r1

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
               Good [] -> runPP q s
               Good v  -> Good v


parseOpt :: PadsParser a -> a -> PadsParser a
parseOpt p x = p `onFail` return x

primPads :: (S.Source -> (a,S.Source)) -> PadsParser a
primPads f = PadsParser $ \s -> Good [f s]

queryPads :: (S.Source -> a) -> PadsParser a
queryPads f = PadsParser $ \s -> Good [(f s,s)]

getPos :: PadsParser S.Pos
getPos = queryPads S.getPos

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

takeP :: Int -> PadsParser String
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
lineBegin = primPads S.lineBegin
lineEnd = primPads S.lineEnd

doLineEnd = do
  rendErr <- lineEnd
  case rendErr of
    Nothing -> return cleanBasePD
    Just err -> do p <- getPos
                   badReturn (mkErrBasePD (E.LineError err) (Just p))

doLineBegin = do
  rbegErr <- lineBegin
  case rbegErr of
    Nothing -> return cleanBasePD
    Just err -> do p <- getPos
                   badReturn (mkErrBasePD (E.LineError err) (Just p))


parseLine :: PadsMD md => PadsParser (r,md) -> PadsParser (r,md)
parseLine p = do 
   bmd <- doLineBegin
   (r,md) <- p
   emd <- doLineEnd
   let hmd = get_md_header md
   let new_hd = mergeBaseMDs [bmd,hmd,emd]
   let new_md = replace_md_header md new_hd
   return (r,new_md)

parseJustRep p = do
  (r,md) <- p
  let bmd = get_md_header md
  let result = (Just r, (bmd, Just md))
  mdReturn result

parseJustNoRep p = do
  md <- p
  let result = (Just (), (md, Just md))
  mdReturn result

many1 p = do {x <-p; xs <- many p; return (x:xs)}

many :: PadsParser a -> PadsParser [a]
many p = scan id
       where scan f = do { x <- p     
                         ; scan (\tail -> f (x:tail))
                         } 
                      `onFail`
                         (return (f []))

parseMany :: PadsMD md => PadsParser (rep,md) -> PadsParser [(rep,md)]
parseMany p = scan id
       where scan f = do { (r,m) <- p     
                         ; if (numErrors (get_md_header m)) == 0 then scan (\tail -> f ((r,m):tail)) else badReturn [(r,m)]
                         } 
                      `onFail`
                         (return (f []))

{-
parseManySepRep p = scan id
      where scan f = do { 

parseSep :: (PadsMD md, PadsMD mdSep) => PadsParser (rep,md) -> PadsParser(repSep, mdSep) -> PadsParser [(rep,md)]
parseSep p sep = scan id
      where scan f = undefined
--          do { (r,m) <- p
--
-}

parseListNoTermNoSep :: PadsMD md => PadsParser (rep,md) -> PadsParser ([rep], (Base_md, [md]))
parseListNoTermNoSep p = do 
  elems <- parseMany p
  let (reps, mds) = unzip elems
  let hmds = map get_md_header mds
  let md = (mergeBaseMDs hmds, mds)
  return (reps,md)
                        

parseNothing :: PadsParser (Maybe t, (Base_md, Maybe s))
parseNothing = return (Nothing, (cleanBasePD, Nothing))

parseMaybeRep   p = choiceP [parseJustRep   p, parseNothing]
parseMaybeNoRep p = choiceP [parseJustNoRep p, parseNothing]

pstringFW_parseM :: Int -> PadsParser (PstringFW, Base_md)
pstringFW_parseM n = do 
  initPos <- getPos
  isEof <- isEofP 
  if isEof then badReturn (def1 n, mkErrBasePD (E.FoundWhenExpecting "EOF" "PstringFW") (Just initPos))
   else do 
     isEor <- isEorP
     if isEor && n /= 0 then badReturn (def1 n, mkErrBasePD (E.FoundWhenExpecting "EOR" "PstringFW") (Just initPos))
      else do  
          str <- takeP n 
          if (length str) /= n then badReturn (def1 n, mkErrBasePD (E.Insufficient (length str) n) (Just initPos))
            else goodReturn (PstringFW str, cleanBasePD)


pstringME_parseM :: S.RE -> PadsParser (PstringME, Base_md)
pstringME_parseM re = do 
  initPos <- getPos
  isEof <- isEofP 
  if isEof then badReturn (def1 re, mkErrBasePD (E.FoundWhenExpecting "EOF" "PstringME") (Just initPos))
   else do 
      match <- regexMatchP re
      case match of 
        Nothing  -> badReturn  (def1 re, mkErrBasePD (E.RegexMatchFail (show re)) (Just initPos))
        Just str -> goodReturn (PstringME str, cleanBasePD)



pstringSE_parseM :: S.RE -> PadsParser (PstringSE, Base_md)
pstringSE_parseM re = do 
  initPos <- getPos
  isEof <- isEofP 
  if isEof then badReturn (def1 re, mkErrBasePD (E.FoundWhenExpecting "EOF" "PstringSE") (Just initPos))
   else do 
      match <- regexStopP re
      case match of 
        Nothing  -> badReturn  (def1 re, mkErrBasePD (E.RegexMatchFail (show re)) (Just initPos))
        Just str -> goodReturn (PstringSE str, cleanBasePD)

pstring_parseM :: Char -> PadsParser (Pstring, Base_md)
pstring_parseM c = do 
  initPos <- getPos
  isEof <- isEofP 
  if isEof then badReturn (def1 c, mkErrBasePD (E.FoundWhenExpecting "EOF" "Pstring") (Just initPos))
   else do 
     isEor <- isEorP
     if isEor then badReturn (def1 c, mkErrBasePD (E.FoundWhenExpecting "EOR" "Pstring") (Just initPos))
      else do  
          str <- satisfy (\c'-> c /= c')
          goodReturn (Pstring str, cleanBasePD)


pchar_parseM :: PadsParser (Pchar, Base_md)
pchar_parseM  = do 
  initPos <- getPos
  isEof <- isEofP 
  if isEof then badReturn (def, mkErrBasePD (E.FoundWhenExpecting "EOF" "Pchar") (Just initPos))
   else do 
     isEor <- isEorP
     if isEor then badReturn (def, mkErrBasePD (E.FoundWhenExpecting "EOR" "Pchar") (Just initPos))
      else do  
          c <- takeHeadP
          goodReturn (Pchar c, cleanBasePD)

pdigit_parseM :: PadsParser (Pdigit, Base_md)
pdigit_parseM  = do 
  initPos <- getPos
  isEof <- isEofP 
  if isEof then badReturn (def, mkErrBasePD (E.FoundWhenExpecting "EOF" "Pdigit") (Just initPos))
   else do 
     isEor <- isEorP
     if isEor then badReturn (def, mkErrBasePD (E.FoundWhenExpecting "EOR" "Pdigit") (Just initPos))
      else do  
          c <- takeHeadP
          if isDigit c then goodReturn (Pdigit (digitToInt c), cleanBasePD)
                       else badReturn  (def, mkErrBasePD (E.FoundWhenExpecting [c] "Pdigit") (Just initPos))


pint_parseM :: PadsParser (Pint,Base_md)
pint_parseM = do
  initPos <- getPos
  isEof <- isEofP 
  if isEof then badReturn (def, mkErrBasePD (E.FoundWhenExpecting "EOF" "Pint") (Just initPos))
   else do 
     isEor <- isEorP
     if isEor then badReturn (def, mkErrBasePD (E.FoundWhenExpecting "EOR" "Pint") (Just initPos))
      else do  
          c <- peakHeadP 
          let isNeg = c == '-'
          if isNeg then takeHeadP else peakHeadP
          digits <- satisfy Char.isDigit
          if null digits then badReturn (def, mkErrBasePD (E.FoundWhenExpecting (mkStr c) "Pint") (Just initPos))
            else goodReturn (Pint $ digitListToInt isNeg digits, cleanBasePD)


pcharLit_parseM :: Char -> PadsParser Base_md
pcharLit_parseM c = do 
  let cStr = mkStr c
  initPos <- getPos
  isEof <- isEofP
  if isEof then badReturn (mkErrBasePD (E.FoundWhenExpecting "EOF" cStr) (Just initPos))
   else do 
     isEor <- isEorP
     if isEor then badReturn (mkErrBasePD (E.FoundWhenExpecting "EOR" cStr) (Just initPos))
      else do
         c' <- takeHeadP 
         if c == c' then goodReturn cleanBasePD
          else do
           foundIt <- scanP c
           errPos <- getPos
           if foundIt then badReturn (mkErrBasePD (E.ExtraBeforeLiteral cStr) (Just errPos))
                      else badReturn (mkErrBasePD (E.MissingLiteral     cStr) (Just errPos))


pstrLit_parseM :: String -> PadsParser Base_md
pstrLit_parseM s = do 
  initPos <- getPos
  isEof <- isEofP
  if isEof then badReturn (mkErrBasePD (E.FoundWhenExpecting "EOF" s) (Just initPos))
   else do 
     isEor <- isEorP
     if isEor then badReturn (mkErrBasePD (E.FoundWhenExpecting "EOR" s) (Just initPos))
      else do
         match <- scanStrP s
         errPos <- getPos
         case match of
           Nothing   -> badReturn (mkErrBasePD (E.MissingLiteral     s) (Just errPos))
           Just []   -> goodReturn cleanBasePD
           Just junk -> badReturn (mkErrBasePD (E.ExtraBeforeLiteral s) (Just errPos))

peorLit_parseM :: PadsParser Base_md
peorLit_parseM = doLineEnd

peofLit_parseM :: PadsParser Base_md
peofLit_parseM = do
  initPos <- getPos
  isEof <- isEofP
  if isEof then goodReturn cleanBasePD
           else badReturn (mkErrBasePD ( E.ExtraBeforeLiteral "Eof")(Just initPos))
   

{- Code generation routines -}
make_pads_declarations :: [PadsDecl] -> Q [Dec]
make_pads_declarations ds = fmap concat (mapM make_pads_declaration ds)

make_pads_declaration :: PadsDecl -> Q [Dec]
make_pads_declaration (PadsDecl (id, pat, padsTy)) = do
   let p_name = case id of
                   Id str -> str
                   AntiId str -> error "Did not expect antiquotation in defining context."
   let ty_name    = getTyName    p_name
   let md_ty_name = getMDName    p_name
   let parse_name = getParseName p_name
   let arg_info_opt = mergeMaybe pat (fmap patToTy pat)
   let (ty_decl, md_ty_decls, md_ty) = genRepMDDecl padsTy ty_name md_ty_name  -- Generate reprsentation and meta-data decls for padsTy
   let padsInstance   :: [Dec] = genPadsInstance      parse_name    ty_name md_ty             arg_info_opt
   parseM :: [Dec]            <- genPadsParseM        parse_name    ty_name md_ty_name padsTy arg_info_opt
   parseS :: [Dec]            <- genPadsParseS p_name parse_name    ty_name md_ty_name padsTy arg_info_opt
   return ([ty_decl]    ++
           md_ty_decls  ++ 
           padsInstance ++  
           parseM       ++ 
           parseS )

{- Functions for generating representation and meta-data declarations and meta-data type. -}

genRepMDDecl :: PadsTy -> Name -> Name -> (TH.Dec, [TH.Dec], TH.Type)
genRepMDDecl ty ty_name md_ty_name = case ty of
  Precord _ tys -> genRepMDDeclStruct ty_name md_ty_name tys
  Punion  _ tys -> genRepMDDeclUnion  ty_name md_ty_name tys
  other -> let (rep,md) = genRepMDTy other
           in  (mk_newTyD ty_name rep, [mk_TySynD md_ty_name md], md)

{- Generate type and meta-data representations. -}
genRepMDTy ::  PadsTy -> (TH.Type, TH.Type)
genRepMDTy ty = case ty of
  Plit  _      -> (ConT ''(),  ConT ''Base_md)
  Pname p_name -> (ConT (getTyName p_name), ConT (getMDName p_name))
  Ptuple tys   -> genRepMDTuple tys
  Pline ty     -> genRepMDTy ty
  Pmaybe ty    -> genRepMDMaybe ty
  Plist ty sep term -> genRepMDList ty
  Papp ty arg  -> genRepMDTy ty
  Ptrans tySrc tyDest exp -> genRepMDTy tyDest    -- rep and md for transform are rep and md for destination type
  Ptypedef pat ty pred    -> genRepMDTypedef ty
  Precord _ tys ->   error "Lines can only appear at the top level."

{- Generate a representation and meta-data for a list:
  type list = [ty] 
  type list_md = (baseMD, [ty_md])
-}
genRepMDList :: PadsTy -> (TH.Type, TH.Type)
genRepMDList ty =
  let (rep_orig, md_orig) = genRepMDTy ty
      rep_ty    = TH.AppT TH.ListT rep_orig
      md_nested = TH.AppT TH.ListT  md_orig
      md_ty = tyListToTupleTy [ConT ''Base_md, md_nested]    
  in (rep_ty, md_ty)

{- Generate a representation and meta-data type for a typedef. -}
genRepMDTypedef :: PadsTy -> (TH.Type, TH.Type)
genRepMDTypedef ty = 
  let (rep_ty, md_orig) = genRepMDTy ty
      md_ty = tyListToTupleTy [ConT ''Base_md, md_orig]    -- md is a pair of a base md for the typedef and the underlying md.
  in (rep_ty, md_ty)

{- Generate a representation and meta-data type for maybe. -}
genRepMDMaybe :: PadsTy -> (TH.Type, TH.Type)
genRepMDMaybe ty = 
  let (rep_orig, md_orig) = genRepMDTy ty
      rep_ty = AppT (ConT ''Maybe) rep_orig                 -- rep is Maybe ty where ty is rep of nested type
      md'_ty = AppT (ConT ''Maybe) md_orig                  -- underyling md is Maybe of md of nested type
      md_ty  = tyListToTupleTy [ConT ''Base_md, md'_ty ]    -- md is a pair of a base md for the maybe and the underlying md.
  in (rep_ty, md_ty)

{- Generate a representation and meta-data type for a struct. -}
genRepMDDeclStruct :: Name -> Name -> [(Maybe String, PadsTy, Maybe TH.Exp)] -> (TH.Dec, [TH.Dec], TH.Type)
genRepMDDeclStruct ty_name md_ty_name fields = 
  let (vsts', md_vsts') = unzip $ flattenMaybeList $ map genRepMDField fields
      derives      = [''Show, ''Eq, ''Typeable, ''Data]
      ty_con       = TH.RecC ty_name vsts'
      ty_decl      = TH.DataD [] ty_name [] [ty_con] derives
      inner_md_name = getStructInnerMDName ty_name   -- ty name is the same as the declared pads type name
      imd_con       = TH.RecC inner_md_name md_vsts'
      imd_decl      = TH.DataD [] inner_md_name [] [imd_con] derives   -- declaration of line for nested components
      imd_ty        = TH.ConT inner_md_name
      md_ty         = tyListToTupleTy [ConT ''Base_md, imd_ty]
      md_decl       = mk_TySynD md_ty_name md_ty
  in if length vsts' == 0 then 
        error ("Error: Record " ++ (show ty_name) ++ " must contain at least one named field.")
     else 
        (ty_decl, [imd_decl,md_decl], md_ty)
 

{- Generate a representation and meta-data type for a union. -}
genRepMDDeclUnion :: Name -> Name -> [(Maybe String, PadsTy, Maybe TH.Exp)] -> (TH.Dec, [TH.Dec], TH.Type)
genRepMDDeclUnion ty_name md_ty_name branches = 
  let (cons', md_cons') = unzip $ map genRepMDUnion branches
      derives      = [''Show, ''Eq, ''Typeable, ''Data]
      ty_decl      = TH.DataD [] ty_name [] cons' derives
      inner_md_name = getStructInnerMDName ty_name   -- ty name is the same as the declared pads type name
      imd_decl      = TH.DataD [] inner_md_name [] md_cons' derives   -- declaration of line for nested components
      imd_ty        = TH.ConT inner_md_name
      md_ty         = tyListToTupleTy [ConT ''Base_md, imd_ty]
      md_decl       = mk_TySynD md_ty_name md_ty
  in if length cons' == 0 then 
        error ("Error: Union " ++ (show ty_name) ++ " must contain at least one named field.")
     else 
        (ty_decl, [imd_decl,md_decl], md_ty)
 
{-
  runQ [d| data FooUnion = UBar Int | Baz String |]
  [DataD [] FooUnion [] [NormalC UBar [(NotStrict,ConT GHC.Types.Int)],NormalC Baz [(NotStrict,ConT GHC.Base.String)]] []]
-}
genRepMDUnion :: (Maybe String, PadsTy, Maybe TH.Exp) -> (TH.Con, TH.Con)
genRepMDUnion (Nothing, ty, exp)  = error "Unions are required to have names for all branches."
genRepMDUnion (Just str, ty, exp) = let
   (rep_ty, md_ty) = genRepMDTy ty
   rep_arg = if rep_ty == ConT ''() then [] else [(TH.NotStrict,  rep_ty)]
   in (TH.NormalC (getBranchNameU   str) rep_arg,
       TH.NormalC (getBranchMDNameU str) [(TH.NotStrict, md_ty)])

type VST = (TH.Name, TH.Strict, TH.Type)
genRepMDField :: (Maybe String, PadsTy, Maybe TH.Exp) -> Maybe (VST, VST)
genRepMDField (Nothing, ty, exp)  = Nothing
genRepMDField (Just str, ty, exp) = let
   (rep_ty, md_ty) = genRepMDTy ty
   in Just ((getFieldName   str, TH.NotStrict, rep_ty),
            (getFieldMDName str, TH.NotStrict, md_ty))

{- Generate a representation and meta-data types for a tuple -}
genRepMDTuple :: [PadsTy] -> (TH.Type, TH.Type)
genRepMDTuple tys = 
  let (tys', mds') = unzip (map genRepMDTy tys)
      r_tys = filter (\t -> t /= ConT ''()) tys'
      ty =  case r_tys of          -- Construct rep type for a tuple.
             []   -> ConT ''()     -- Tuple contained no non-singleton types, so its rep is the unit type.
             [ty] -> ty            -- Tuple contains one non-singleton type T, so its rep type is just T
             (r_ty:r_tys') ->      -- Rep is tuple of non-singleton types T.
                 tyListToTupleTy r_tys
      md_ty_nested = case mds' of  
                      []      -> ConT ''Base_md   -- Tuple contains no types, so its nested pd is just a base pd
                      [md_ty] -> md_ty            -- Tuple contains a single type, so its nested pd is just the pd of the type
                      (md_ty:md_tys') -> tyListToTupleTy mds'
     {- Pd of a tuple is a pair of a base pd and a tuple of pds for each element in the tuple. -}
      md_ty = tyListToTupleTy [(ConT ''Base_md), md_ty_nested]
  in
      (ty, md_ty)

genPadsInstance parse_name ty_name md_ty mpat_info = 
  let (inst, parsePP) = case mpat_info of
                          Nothing -> (AppT (AppT (ConT ''Pads) (ConT ty_name)) md_ty,   -- Pads RepTy MDTy
                                      mkName "parsePP")
                          Just (p,arg_ty) -> 
                                     (AppT 
                                        (AppT (AppT (ConT ''Pads1) arg_ty) (ConT ty_name)) 
                                        md_ty,   -- Pads Arg RepTy MDTy
                                      mkName "parsePP1")
      parsePP_method = ValD (VarP parsePP) (NormalB (VarE parse_name)) []
  in [InstanceD [] inst [parsePP_method]]

{- This generates a type-specific name for the parseS function by redirecting to the generic function. -}
genPadsParseS :: String -> Name -> Name -> Name -> PadsTy -> Maybe(TH.Pat, TH.Type) -> Q [Dec]
genPadsParseS p_name parse_name rep_name pd_name padsTy mpat_info = do
   let parseSName  = getParseSName p_name
   let stringTy    = ConT ''String
   let padsPairTy  = AppT (AppT (TupleT 2) (ConT rep_name)) (ConT pd_name)
   let resultTy    = AppT (AppT (TupleT 2) padsPairTy) stringTy
   let core_ty     = arrowTy stringTy resultTy
   let (bodyE, ty) = case mpat_info of
                      Nothing -> (VarE 'parseS, core_ty)
                      Just (pat,pat_ty) -> (LamE [pat] (AppE (VarE 'parseS1) (patToExp pat)),
                                            arrowTy pat_ty core_ty)
   let sigD = SigD parseSName ty
   let funD = ValD (VarP parseSName) (NormalB bodyE ) []
   return [sigD, funD]


genPadsParseM :: Name -> Name -> Name -> PadsTy -> Maybe (TH.Pat, TH.Type) -> Q [Dec]
genPadsParseM parse_name rep_name pd_name padsTy mpat_info = do 
   let core_ty = AppT (ConT ''PadsParser) (AppT (AppT (TupleT 2) (ConT rep_name)) (ConT pd_name))
   core_bodyE <- genParseBody rep_name pd_name padsTy
   let (bodyE,ty) = case mpat_info of
                     Nothing -> (core_bodyE, core_ty)
                     Just (pat,pat_ty) -> ( LamE [pat] core_bodyE,
                                            arrowTy pat_ty core_ty)
   let sigD = SigD parse_name ty
   let funD = ValD (VarP parse_name) (NormalB bodyE) []
   return [sigD, funD]

wrapRep :: Name -> PadsTy -> TH.Exp -> TH.Exp
wrapRep repN ty repE = case ty of
  Precord _ _ -> repE
  Punion  _ _ -> repE
  otherwise   -> AppE  (ConE repN) repE

{-
 Generate body of parseM function, which has the form:
  do (rep,md) <- rhsE
     return (Rep rep, md)
-}
genParseBody :: Name -> Name -> PadsTy -> Q TH.Exp
genParseBody repN mdN ty = do
   repName     <- genRepName 
   mdName      <- genMdName 
   rhsE        <- parseE ty 
   let (repE,repP) = genPE repName
   let (mdE, mdP)  = genPE mdName
   let doParseS    = BindS (TupP [repP,mdP]) rhsE
   let frepE       = wrapRep repN ty repE 
   let resultE     = TupE [frepE,mdE]
   let finalS      = NoBindS (AppE (VarE 'return) resultE)
   return (DoE [doParseS, finalS])

{- Given a PadsTy ty, return the haskell expression that parses ty. -}
parseE :: PadsTy -> Q TH.Exp
parseE ty = case ty of
  Plit (PS.CharL c)    -> return (AppE (VarE(getParseName "PcharLit")) (LitE (TH.CharL   c)))   -- Note the type of this expression has a different pattern: (no rep).
  Plit (PS.StringL s)  -> return (AppE (VarE(getParseName "PstrLit"))  (LitE (TH.StringL s)))   -- Note the type of this expression has a different pattern: (no rep).
  Plit  PS.EorL        -> return       (VarE(getParseName "PeorLit"))                           -- Note the type of this expression has a different pattern: (no rep).
  Plit  PS.EofL        -> return       (VarE(getParseName "PeofLit"))                           -- Note the type of this expression has a different pattern: (no rep).
  Pname p_name   -> return (VarE (getParseName p_name))
  Ptuple tys     -> mkParseTuple tys
  Precord str fields   -> mkParseRecord str fields
  Punion  str branches -> mkParseUnion  str branches
  Plist ty sep term    -> mkParseList ty sep term
  Pline ty       -> mkParseLine ty
  Pmaybe ty      -> mkParseMaybe ty
  Papp ty argE   -> mkParseTyApp ty argE
  Ptrans tySrc tyDest exp -> mkParseTyTrans tySrc tyDest exp
  Ptypedef pat ty pred -> mkParseTyTypedef pat ty pred

{-
  do { (rep,md_orig) <- parseTy
       let pat = rep
       let ty_md = if pred then Base_md {numErrors = numErrors md_orig, errInfo = Nothing }
                          else Base_md {numErrors = 1 + numErrors md_orig, errInfo = Just "Typedef predicate failed." }
       return (rep, (b_md,md_orig))
-}

mkParseTyTypedef :: TH.Pat -> PadsTy -> TH.Exp -> Q TH.Exp
mkParseTyTypedef pat tyBase pred = do
  baseE <- parseE tyBase
  let baseEQ = return baseE
  let predQ  = return (TH.LamE [pat, TH.VarP (TH.mkName "rep"), TH.VarP (TH.mkName "md")] pred)    -- abstract on bound variables
  {- Why can't I define buildError and getLocOpt outside of the quasi-quote? -}
  [| do (b_rep, b_md @ Base_md{numErrors = b_errors, errInfo = b_errInfo} ) <- $baseEQ 
        let getLocOpt errInfo = case errInfo of 
                            Nothing -> Nothing
                            Just e -> E.position e
        let buildError pred n errInfo = if n == 0    then Nothing
                                  else if pred then Just (E.ErrInfo {msg = E.UnderlyingTypedefFail, position = getLocOpt errInfo})
                                  else              Just (E.ErrInfo {msg = E.PredicateFailure,      position = getLocOpt errInfo})
        let (predVal, totErrors) = if $predQ b_rep b_rep b_md    -- apply to bind values to bound variables.
                                      then (True, b_errors) else (False, 1+b_errors)
        let tdef_md = Base_md {numErrors = totErrors, errInfo = buildError predVal totErrors b_errInfo }

        return (b_rep, (tdef_md,b_md)) |]

mkParseTyTrans :: PadsTy -> PadsTy -> TH.Exp -> Q TH.Exp
mkParseTyTrans tySrc tyDest exp = do
  srcE <- parseE tySrc
  let srcEQ = return srcE
  let expQ  = return exp
  [| do begin_pos <- getPos
        src_result <- $srcEQ
        end_pos <- getPos
        let src_pos = S.pos_span begin_pos end_pos
        let (toDst,toSrc) = $expQ
        return (toDst src_pos src_result) |]

mkParseTyApp :: PadsTy -> TH.Exp -> Q TH.Exp
mkParseTyApp ty argE = do
  parseFnE <- parseE ty            -- XXX should add type checking to ensure that ty is expecting an argument
  return (AppE parseFnE argE) 

{-
The representation of Pline ty is the same as the representation for ty.
The meta-data structure of a Pline ty is the same as the meta-data structure for ty.
TODO: Report error if underlying type has no representation. 
-}

mkParseLine :: PadsTy -> Q TH.Exp
mkParseLine ty = do
   rhsE <- parseE ty
   return (AppE (VarE 'parseLine) rhsE)

mkParseMaybe :: PadsTy -> Q TH.Exp 
mkParseMaybe ty = do
  rhsE <- parseE ty
  case ty of
    Plit l    -> return (AppE (VarE 'parseMaybeNoRep) rhsE)
    otherwise -> return (AppE (VarE 'parseMaybeRep)   rhsE)

mkParseList :: PadsTy -> (Maybe PadsTy) -> (Maybe TermCond) -> Q TH.Exp
mkParseList ty sep term = do 
  rhsE <- parseE ty
  case (sep,term) of 
    (Nothing,Nothing) -> return (AppE (VarE 'parseListNoTermNoSep)   rhsE)

mkParseUnion :: String -> [(Maybe String, PadsTy, Maybe TH.Exp)] -> Q TH.Exp
mkParseUnion str branches = do
  parseEs     <- mkParseBranches str branches   
  return (AppE (VarE 'choiceP) (ListE parseEs))       -- choiceP [parse1, ..., parsen]

mkParseBranches :: String -> [(Maybe String, PadsTy, Maybe TH.Exp)] -> Q [TH.Exp]
mkParseBranches str branches = mapM (mkParseBranch str) branches

mkParseBranch :: String -> (Maybe String, PadsTy, Maybe TH.Exp) -> Q TH.Exp
mkParseBranch str (Nothing, padsTy, predM) = error ("Union ("++ str ++ ") branch is missing a name.")
mkParseBranch str (Just name, padsTy, predM) = do
   let repName  = getBranchNameL   name
   let mdName   = getBranchMDNameL name
   bmdName1     <- genBMdName 
   bmdName2     <- genBMdName 
   let (repE,  repP)  = genPE repName
   let ( mdE,  mdP)   = genPE mdName
   let (bmd1E, bmd1P) = genPE bmdName1
   let (bmd2E, bmd2P) = genPE bmdName2
   rhsE        <- parseE padsTy
   case (predM,padsTy) of
    (Just pred, Plit l) -> error ("Union "++ str ++ ": literal branch can't have a predicate.")
    (Nothing,   Plit l) -> let
       stmtPrs = BindS mdP rhsE                                                   -- md <- parse
       frepE   = TH.ConE (getBranchNameU   name)                                  -- . inject value into data type: Foo 
       imdE    = TH.AppE (TH.ConE (getBranchMDNameU name))  mdE                   -- . inject md into data type: Foo_md md
       fmdE    = TupE [mdE,  imdE]                                                -- . build final md: (md, Foo_md md)
       resultE = TupE [frepE,fmdE]                                                -- . build final result: (Foo, (md, Foo_md md))
       stmtRet = NoBindS (AppE (VarE 'mdReturn) resultE)                            -- return (Foo, (md, Foo_md md))
       in return (TH.DoE [stmtPrs,stmtRet])
    (Nothing, _) -> let
       stmtPrs = BindS (TupP [repP,mdP]) rhsE                                     -- (rep, md) <- parse
       stmtGmd = LetS [ValD bmd1P (NormalB (AppE (VarE 'get_md_header) mdE)) []]  -- let mbd1 = get_md_header md
       frepE   = TH.AppE (TH.ConE (getBranchNameU   name)) repE                   -- . inject value into data type: Foo rep
       imdE    = TH.AppE (TH.ConE (getBranchMDNameU name))  mdE                   -- . inject md into data type: Foo_md md
       fmdE    = TupE [bmd1E,imdE]                                                -- . build final md: (bmd1, Foo_md md)
       resultE = TupE [frepE,fmdE]                                                -- . build final result: 
       stmtRet = NoBindS (AppE (VarE 'mdReturn) resultE)                            -- return (Foo rep, (bmd1, Foo_md md))
       in return (TH.DoE [stmtPrs,stmtGmd,stmtRet])
    (Just pred,_) -> let
       stmtPrs = BindS (TupP [repP,mdP]) rhsE                                     -- (rep,md) <- parse
       stmtGmd = LetS [ValD bmd1P (NormalB (AppE (VarE 'get_md_header) mdE)) []]  -- let mbd1 = get_md_header md
       predTestE = TH.CondE pred bmd1E (AppE (VarE 'addPredFailureMD) bmd1E)      -- . build predicate test 
       stmtPred  = LetS [ValD bmd2P (NormalB predTestE)  []]                      -- let mbd2 = if pred then bmd1 else addPredFailureMD bmd1
       frepE   = TH.AppE (TH.ConE (getBranchNameU   name)) repE                   -- . inject value into data type: Foo rep
       imdE    = TH.AppE (TH.ConE (getBranchMDNameU name))  mdE                   -- . inject md into data type:    Foo_md md
       fmdE    = TupE [bmd2E,imdE]                                                -- . build final md:              (mbd2, Foo_md md)
       resultE = TupE [frepE,fmdE]                                                -- . build final result           (Foo rep, (md2, Foo_md md))
       stmtRet = NoBindS (AppE (VarE 'mdReturn) resultE)                            -- return (Foo rep, (md2, Foo_md md))
       in return (TH.DoE [stmtPrs,stmtGmd,stmtPred,stmtRet])                      
{- 
Invariants: literal can't have a field name or a predicate; no field name, no predicate
   stmts to parse each field of a record
   do
    (field_name, field_name_raw_md) <- parse_1             -- if field_name exists, not a literal, predicate
    let raw_bmd_1 = get_md_header field_name_raw_md
    let bmd_1 = if pred_1 then raw_bmd_1
                else addPredFailureMD raw_bmd_1
    let field_name_md = replace_md_header field_name_raw_md bmd_1
    ...
    (field_name, field_name_md) <- parse_field_name        -- if field_name exists, not a literal, no predicate
    let bmd_field_name = get_md_header field_name_md
    ... 
    md_i <- parse_i                                        -- no field name, literal field, no predicate
    let bmd_i = get_md_header md_i
    ...
    (rep_j,md_j) <- parse_j                                -- no field name, not literal, no predicate
    let bmd_j = get_md_header md_j

    let top_md = mergeBaseMDs [bmd_1,...bmd_n]
    let name_md = Name_md{name_1 = field_name_md, ... }
    return (rep,(top_md,name_md))
-}

mkParseRecord :: String -> [(Maybe String, PadsTy, Maybe TH.Exp)] -> Q TH.Exp
mkParseRecord str fields = do
  (repEs,mdEs,bmdEs, stmts) <- mkParseFields fields
  let tyName             = mkName str
  let top_md             = mkName "top_md"
  let (top_mdE, top_mdP) = genPE top_md
  let headerE            = AppE (VarE 'mergeBaseMDs) (ListE bmdEs)
  let mdS                = LetS [ValD top_mdP (NormalB headerE) []]
  let repE               = RecConE tyName repEs
  let inner_md_name      = getStructInnerMDName tyName   -- ty name is the same as the declared pads type name
  let mdE                = TupE [top_mdE, RecConE inner_md_name mdEs]
  let resultE            = TupE [repE,mdE]
  let finalS             = NoBindS (AppE (VarE 'return) resultE)
  return (DoE (stmts ++ [mdS,finalS]))


mkParseField :: (Maybe String, PadsTy, Maybe TH.Exp) -> Q ([TH.FieldExp], [TH.FieldExp], TH.Exp, [Stmt])
mkParseField (labelM, ty, predM) = do
   repName     <- case labelM of { Nothing -> genRepName; Just str -> return $ getFieldName   str}
   mdName      <- case labelM of { Nothing -> genMdName;  Just str -> return $ getFieldMDName str}
   bmdName     <- genBMdName 
   let (repE, repP) = genPE repName
   let ( mdE,  mdP) = genPE mdName
   let (bmdE, bmdP) = genPE bmdName
   rhsE        <- parseE ty
   case (labelM,ty,predM) of 
    (Nothing, _,  Just p)      ->  error "Predicates cannot modify unnamed fields in records."
    (Just str, Plit l, _)      ->  error "Named fields cannot have literal types in records."
    (Nothing,  Plit l, Nothing) ->  let                                         -- Parse unnamed, literal struct field
       stmt1 = BindS mdP rhsE                                                  -- No rep for literals
       stmt2 = LetS [ValD bmdP (NormalB (AppE (VarE 'get_md_header) mdE)) []]  -- Read out header of resulting parse descriptor
       in return([], [], bmdE, [stmt1,stmt2])                                  -- No rep or md to include in result
    (Nothing, _, Nothing)      ->  let                                         -- Parse unnamed, non-literal struct field
       stmt1 = BindS (TupP [repP,mdP]) rhsE                                    -- rep and md exist for non-literal types
       stmt2 = LetS [ValD bmdP (NormalB (AppE (VarE 'get_md_header) mdE)) []]  -- Read out header of resulting parse descriptor
       in return([], [], bmdE, [stmt1,stmt2])                                  -- No rep or md to include in result
    (Just str, ty, Nothing)    -> let                                          -- Parse named, non-literal struct field, no predicate
       stmt1 = BindS (TupP [repP,mdP]) rhsE                                    -- rep and md exist for non-literal types
       stmt2 = LetS [ValD bmdP (NormalB (AppE (VarE 'get_md_header) mdE)) []]  -- Read out header of resulting parse descriptor
       in return([(repName,repE)], [(mdName,mdE)], bmdE, [stmt1,stmt2])       -- Include named rep and md in result
    (Just str, ty, Just pred)    -> do                                         -- Parse named, non-literal struct field, predicate
      raw_mdName      <- genMdName
      raw_bmdName     <- genBMdName 
      let (rawMDE, rawMDP)   = genPE raw_mdName
      let (rawBMDE, rawBMDP) = genPE raw_bmdName
      let predTestE      = TH.CondE pred rawBMDE (AppE (VarE 'addPredFailureMD) rawBMDE)    -- if pred then rawBMD else addPredFailureMD rawBMD
      let replaceHeaderE = AppE (AppE (VarE 'replace_md_header) rawMDE) bmdE                -- replace_md_header rawMD bmd
      let stmt1 = BindS (TupP [repP,rawMDP]) rhsE
      let stmt2 = LetS [ValD rawBMDP (NormalB (AppE (VarE 'get_md_header) rawMDE)) []]
      let stmt3 = LetS [ValD bmdP (NormalB predTestE)  []] 
      let stmt4 = LetS [ValD mdP  (NormalB replaceHeaderE) []]
      return ([(repName,repE)], [(mdName,mdE)], bmdE, [stmt1,stmt2,stmt3,stmt4])       -- Include named rep and md in result

mkParseFields :: [(Maybe String, PadsTy, Maybe TH.Exp)] -> Q ([FieldExp], [FieldExp], [TH.Exp], [Stmt])
mkParseFields [] = return ([],[],[],[])
mkParseFields (field:fields) = do
  (rep_field,   md_field,  bmd_field,  stmts_field)  <- mkParseField  field
  (reps_fields, md_fields, bmd_fields, stmts_fields) <- mkParseFields fields
  return (rep_field++reps_fields, md_field++md_fields, bmd_field:bmd_fields, stmts_field++stmts_fields)

addPredFailureMD :: Base_md -> Base_md
addPredFailureMD (Base_md{numErrors, errInfo}) = 
  let errInfo' = case errInfo of
                  Nothing -> E.ErrInfo {msg = E.PredicateFailure, position = Nothing}
                  Just e ->  e
  in Base_md{numErrors = numErrors + 1, errInfo = Just errInfo'}
       
       
{- 
   stmts to parse each element of tuple:
   do
    (rep_1,md_1) <- parse_1
    let bmd_1 = get_md_header md_1
    ...
    (rep_n,md_n) <- parse_n
    let bmd_n = get_md_header md_n
    let top_md = mergeBaseMDs [bmd_1,...bmd_n]
    return (rep,(top_md,(md_1,...,md_n)))
-}

mkParseTuple :: [PadsTy] -> Q TH.Exp
mkParseTuple tys = do
  (repEs,mdEs,bmdEs, stmts) <- mkParseTupleB tys
  let top_md             = mkName "top_md"
  let (top_mdE, top_mdP) = genPE top_md
  let headerE            = AppE (VarE 'mergeBaseMDs) (ListE bmdEs)
  let mdS                = LetS [ValD top_mdP (NormalB headerE) []]
  let repE               = TupE repEs
  let mdE                = TupE [top_mdE, TupE mdEs]
  let resultE            = TupE [repE,mdE]
  let finalS             = NoBindS (AppE (VarE 'return) resultE)
  return (DoE (stmts ++ [mdS,finalS]))

mkParseTupleB :: [PadsTy] -> Q ([TH.Exp], [TH.Exp], [TH.Exp], [Stmt])
mkParseTupleB [] = return ([],[],[],[])
mkParseTupleB (ty:tys) = do
  (rep_ty,   md_ty,  bmd_ty,  stmt_ty)  <- mkParseTyB ty
  (reps_tys, md_tys, bmd_tys, stmt_tys) <- mkParseTupleB tys
  return (rep_ty++reps_tys, md_ty:md_tys, bmd_ty:bmd_tys, stmt_ty++stmt_tys)

{- Input:
     PadsTy: type we are generating parsing instructions for
   Output:
     [TH.Exp]: list of expressions that store parse results (can be empty)
      TH.Exp : expression that stores meta data result 
      TH.Exp : expression that stores base level meta data 
     [Stmt]  : list of statements to do parsing for PadsTy.

    Plit c:
     md <- parseE
     bmd <- get_md_header md
    others
     (rep,md) <- parseE
     bmd <- get_md_header md

-}
mkParseTyB :: PadsTy -> Q ([TH.Exp], TH.Exp, TH.Exp, [Stmt])
mkParseTyB ty = do
   repName     <- genRepName 
   mdName      <- genMdName  
   bmdName     <- genBMdName 
   let (repE, repP) = genPE repName
   let ( mdE,  mdP) = genPE mdName
   let (bmdE, bmdP) = genPE bmdName
   rhsE        <- parseE ty
   let (resultEs,stmt1) =  case ty of
        Plit l ->    ([],     BindS  mdP              rhsE)
        otherwise -> ([repE], BindS (TupP [repP,mdP]) rhsE)
   let stmt2    = LetS [ValD bmdP (NormalB (AppE (VarE 'get_md_header) mdE)) []]
   return (resultEs, mdE,bmdE,[stmt1,stmt2])
   


  

{- Helper functions -}
mkStr c = "'" ++ [c] ++ "'"

mapFstChar f [] = []
mapFstChar f (c:cs) = (f c) : cs

strToUpper = mapFstChar Char.toUpper
strToLower = mapFstChar Char.toLower

mergeMaybe m1 m2 = case (m1,m2) of
  (Nothing, Nothing) -> Nothing
  (Just d1, Just d2) -> Just (d1,d2)
  _ -> error "mergeMaybe given two maybes in different states."

flattenMaybeList xs = case xs of
  [] -> []
  (Nothing: xxs) -> flattenMaybeList xxs
  (Just x : xxs) -> x : flattenMaybeList xxs

mk_newTyD ty_name ty = NewtypeD [] ty_name [] con derives
    where con = NormalC ty_name [(NotStrict,ty)]           -- How should we determine whether a type should be Strict or not?
          derives = (map mkName ["Show", "Eq"]) ++  [''Typeable, ''Data]

mk_TySynD ty_name ty = TySynD ty_name [] ty

arrowTy ty1 ty2 = AppT (AppT ArrowT     ty1  ) ty2
tyListToTupleTy (ty:tys) = foldl AppT (AppT (TupleT (1 + length tys) ) ty) tys
tyListToListTy  tys      = foldl AppT ListT                                tys

{- XXX: need to add location information so can report location of error messages. -}
patToTy :: TH.Pat -> TH.Type
patToTy pat = case pat of
  LitP l      -> litToTy l
  VarP n      -> error ("Variable "++ (showName n) ++ " needs a type annotation.")
  TupP pats   -> tyListToTupleTy (map patToTy pats)
  InfixP p1 n p2 -> error ("Infix constructor "++ (showName n) ++ " application needs a type annotation.")
  TildeP p    -> patToTy p
  BangP  p    -> patToTy p
  AsP n p     -> patToTy p
  WildP       -> error "Wild card patterns are not supported in PADS declarations."
  RecP name fieldPats -> ConT name   {-  I think this is the correct represtentation of a line type. -}
  ListP pats  -> tyListToListTy (map patToTy pats)
  SigP p ty   -> ty
  
litToTy :: TH.Lit -> TH.Type
litToTy lit = 
  let name = case lit of
       Language.Haskell.TH.Syntax.CharL c       -> ''Char
       Language.Haskell.TH.Syntax.StringL s     -> ''String
       IntegerL i    -> ''Integer
       RationalL r   -> ''Rational
       IntPrimL  i   -> ''Integer
       WordPrimL i   -> ''Integer
       FloatPrimL f  -> ''Rational
       DoublePrimL d -> ''Rational
  in ConT name

patToExp :: TH.Pat -> TH.Exp
patToExp pat = case pat of
  LitP l      -> LitE l
  VarP n      -> VarE n
  TupP pats   -> TupE (map patToExp pats)
  InfixP p1 n p2 -> InfixE (Just (patToExp p1)) (VarE n) (Just (patToExp p2))
  TildeP p    -> patToExp p
  BangP  p    -> patToExp p
  AsP n p     -> VarE n
  WildP       -> error "Wild card patterns are not supported in PADS declarations. Can't convert to expression"
  RecP name fieldPats -> RecConE name (map fieldPatToExp fieldPats)   {-  I think this is the correct represtentation of a line type. -}
  ListP pats  -> ListE (map patToExp pats)
  SigP p ty   -> patToExp p

fieldPatToExp (n,p) = (n, patToExp p)


{- Name manipulation functions -}
genUniqueName base = newName base
genRepName = genUniqueName "rep" 
genMdName  = genUniqueName "md" 
genBMdName = genUniqueName "bmd"
getMDName pname = case M.lookup pname baseTypesMap of
         Nothing -> mkName ((strToUpper pname) ++ "_md")
         Just _ -> ''Base_md                  -- Built-in base type        
getStructInnerMDName name = 
  let str = show name
  in mkName (str++"_inner_md")
getFieldMDName str = mkName (str++"_md")
getFieldName str = mkName str

getBranchMDNameU str = mkName ((strToUpper str)++"_md")
getBranchNameU str = mkName (strToUpper str)

getBranchMDNameL str = mkName ((strToLower str)++"_md")
getBranchNameL   str = mkName  (strToLower str)

getTyName pname = mkName  (strToUpper pname)
{-
XXXgetTyName pname = case M.lookup pname baseTypesMap of
         Nothing  -> mkName  (strToUpper pname)
         Just name-> name -}
getParseName pname = mkName ((strToLower pname) ++ "_parseM")
getParseSName pname = mkName ((strToLower pname) ++ "_parseS")

genPE name = (VarE name, VarP name)

