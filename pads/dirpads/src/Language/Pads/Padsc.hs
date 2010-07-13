{-# LANGUAGE TypeSynonymInstances, GeneralizedNewtypeDeriving #-}
module Language.Pads.Padsc where

import Language.Pads.Syntax 
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

mergeBaseMDs = Data.List.foldl1 (\(Base_md {numErrors=num1,errInfo=i1}) (Base_md {numErrors=num2,errInfo=i2}) ->
                                  (Base_md {numErrors=num1 + num2,
                                            errInfo= Nothing })) 

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

class Data pads => Pads pads md | pads -> md  where
  def :: pads
  def = gdef
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
  ("Pstring",   (''String,  [''Char])),
  ("PstringFW", (''String,  [''Int])),
  ("PstringME", (''String,  [''S.RE])),
  ("PstringSE", (''String,  [''S.RE]))
 ]

baseTypesMap :: M.Map String (Name, [Name]) = M.fromList baseTypesList

{- XXX: These declarations should be generated from table above . -}
{- XXX: Default values should be defined to use parameter, so for example, we can generate a string of the proper length. -}
newtype Pint = Pint Int
  deriving (Eq, Show, Data, Typeable, Num)

instance Pads Pint Base_md where
  parsePP = pint_parseM

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

eitherP :: PadsParser a -> PadsParser a -> PadsParser a
eitherP p q = PadsParser $ \s -> 
   runPP p s `appendResult` runPP q s 

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

isEorP :: PadsParser Bool
isEorP = queryPads S.isEOR

ifEorP :: PadsParser ([rep],[md]) -> PadsParser ([rep],[md])
ifEorP p = do
  b <- isEorP
  if b then return ([],[]) else p

peakHeadP :: PadsParser Char 
peakHeadP = queryPads S.head

takeHeadP :: PadsParser Char
takeHeadP = primPads S.takeHead

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

digitListToInt :: [Char] -> Int
digitListToInt = foldl (\a d ->10*a + (Char.digitToInt d)) 0


recordBegin,recordEnd :: PadsParser (Maybe String)
recordBegin = primPads S.recordBegin
recordEnd = primPads S.recordEnd

doRecordEnd = do
  rendErr <- recordEnd
  case rendErr of
    Nothing -> return cleanBasePD
    Just err -> do p <- getPos
                   badReturn (mkErrBasePD (E.RecordError err) (Just p))

doRecordBegin = do
  rbegErr <- recordBegin
  case rbegErr of
    Nothing -> return cleanBasePD
    Just err -> do p <- getPos
                   badReturn (mkErrBasePD (E.RecordError err) (Just p))


parseRecord :: PadsMD md => PadsParser (r,md) -> PadsParser (r,md)
parseRecord p = do 
   bmd <- doRecordBegin
   (r,md) <- p
   emd <- doRecordEnd
   let hmd = get_md_header md
   let new_hd = mergeBaseMDs [bmd,hmd,emd]
   let new_md = replace_md_header md new_hd
   return (r,new_md)

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
          digits <- satisfy Char.isDigit
          if null digits then badReturn (def, mkErrBasePD (E.FoundWhenExpecting (mkStr c) "Pint") (Just initPos))
            else goodReturn (Pint $ digitListToInt digits, cleanBasePD)


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


  

{- Code generation routines -}
make_pads_declarations :: PadsDecl -> Q [Dec]
make_pads_declarations (PadsDecl (id, pat, padsTy)) = do
   let p_name = case id of
                   Id str -> str
                   AntiId str -> error "Did not expect antiquotation in defining context."
   let ty_name    = getTyName    p_name
   let md_ty_name = getMDName    p_name
   let parse_name = getParseName p_name
   let (ty,md_ty) = genRepMD padsTy       -- Generate reprsentation and meta-data types for padsTy
   let arg_info_opt = mergeMaybe pat (fmap patToTy pat)
   let ty_decl        ::  Dec  = mk_newTyD    ty_name    ty
   let md_ty_decl     ::  Dec  = mk_TySynD md_ty_name md_ty
   let padsInstance   :: [Dec] = genPadsInstance      ty_name md_ty parse_name  arg_info_opt
   parseM :: [Dec] <- genPadsParseM        parse_name ty_name md_ty_name padsTy arg_info_opt
   parseS :: [Dec] <- genPadsParseS p_name parse_name ty_name md_ty_name padsTy arg_info_opt
   return ([ty_decl, md_ty_decl]  ++ 
           padsInstance ++  
           parseM ++ 
           parseS )

{- Functions for generation representation and meta-data types -}
genRepMD ::  PadsTy -> (TH.Type, TH.Type)
genRepMD ty = case ty of
  Plit char    -> (ConT ''(),  ConT ''Base_md)
  Pname p_name -> (ConT (getTyName p_name), ConT (getMDName p_name))
  Ptuple tys   -> genRepMDTuple tys
  Precord ty   -> genRepMD ty
  Papp ty arg  -> genRepMD ty
  Ptrans tySrc tyDest exp -> genRepMD tyDest    -- rep and md for transform are rep and md for destination type


{- Generate a representation and meta-data types for a tuple -}
genRepMDTuple :: [PadsTy] -> (TH.Type, TH.Type)
genRepMDTuple tys = 
  let (tys', mds') = unzip (map genRepMD tys)
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

genPadsInstance ty_name md_ty parse_name mpat_info = 
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
   let frepE       = AppE  (ConE repN) repE
   let resultE     = TupE [frepE,mdE]
   let finalS      = NoBindS (AppE (VarE 'return) resultE)
   return (DoE [doParseS, finalS])

{- Given a PadsTy ty, return the haskell expression that parses ty. -}
parseE :: PadsTy -> Q TH.Exp
parseE ty = case ty of
  Plit c       -> return (AppE (VarE(getParseName "PcharLit")) (LitE (CharL c)))   -- Note the type of this expression has a different pattern: (no rep).
  Pname p_name -> return (VarE (getParseName p_name))
  Ptuple tys   -> mkParseTuple tys
  Precord ty   -> mkParseRecord ty
  Papp ty argE -> mkParseTyApp ty argE
  Ptrans tySrc tyDest exp -> mkParseTyTrans tySrc tyDest exp

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
The representation of Precord ty is the same as the representation for ty.
The meta-data structure of a Precord ty is the same as the meta-data structure for ty.
TODO: Report error if underlying type has no representation. 
-}

mkParseRecord :: PadsTy -> Q TH.Exp
mkParseRecord ty = do
   rhsE <- parseE ty
   return (AppE (VarE 'parseRecord) rhsE)

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
        Plit c ->    ([],     BindS  mdP              rhsE)
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
  RecP name fieldPats -> ConT name   {-  I think this is the correct represtentation of a record type. -}
  ListP pats  -> tyListToListTy (map patToTy pats)
  SigP p ty   -> ty
  
litToTy :: TH.Lit -> TH.Type
litToTy lit = 
  let name = case lit of
       CharL c       -> ''Char
       StringL s     -> ''String
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
  RecP name fieldPats -> RecConE name (map fieldPatToExp fieldPats)   {-  I think this is the correct represtentation of a record type. -}
  ListP pats  -> ListE (map patToExp pats)
  SigP p ty   -> patToExp p

fieldPatToExp (n,p) = (n, patToExp p)


{- Name manipulation functions -}
-- genUniqueName base (i::Int) = mkName (base++(show i))
genUniqueName base = newName base
genRepName = genUniqueName "rep" 
genMdName  = genUniqueName "md" 
genBMdName = genUniqueName "bmd"
getMDName pname = case M.lookup pname baseTypesMap of
         Nothing -> mkName ((strToUpper pname) ++ "_md")
         Just _ -> ''Base_md                  -- Built-in base type        
getTyName pname = mkName  (strToUpper pname)
{-
XXXgetTyName pname = case M.lookup pname baseTypesMap of
         Nothing  -> mkName  (strToUpper pname)
         Just name-> name -}
getParseName pname = mkName ((strToLower pname) ++ "_parseM")
getParseSName pname = mkName ((strToLower pname) ++ "_parseS")

genPE name = (VarE name, VarP name)

