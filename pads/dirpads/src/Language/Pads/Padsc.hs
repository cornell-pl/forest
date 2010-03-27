module Language.Pads.Padsc where

import Language.Pads.Syntax 
import Language.Haskell.TH as TH
import Language.Haskell.TH.Syntax
import Data.Data
import Data.List
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

instance Pretty Base_md where
  ppr Base_md {numErrors=num, errInfo = info} = text "Errors:" <+> PP.ppr num <+> 
                                                case info of Nothing -> empty
                                                             Just e -> PP.ppr e

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

class Pads rep md | rep -> md where
  def :: rep
  parsePP  :: PadsParser(rep,md)
  parseS   :: String -> ((rep, md), String) 
  parseS cs = let ((r,rest):alternates) = (runPP parsePP) (S.padsSourceFromString cs) in (r, S.padsSourceToString rest)  

instance Pads Int Base_md where
  def = gdef
  parsePP = pInt_parseM

{-
class Pads rep where
  def :: rep

instance Pads Int where
  def = gdef

instance Pads Char where
  def = gdef

instance Pads Float where
  def = gdef
-}



class PadsPD md where
  get_md_header :: md -> Base_md

instance PadsPD Base_md where
  get_md_header b_md = b_md

instance PadsPD a => PadsPD (a,b) where
  get_md_header (a,b) = get_md_header a




                                                         

{- Parsing Monad -}
newtype PadsParser a = PadsParser (S.Source -> [(a,S.Source)])

runPP :: PadsParser t -> S.Source -> [(t, S.Source)]
runPP (PadsParser p) = p

instance Monad PadsParser where
  return r = PadsParser (\bs -> [(r,bs)])
  p >>= f  = PadsParser (\bs -> concat [runPP (f a) bs' | (a,bs') <- runPP p bs ])

mkStr c = "'" ++ [c] ++ "'"

{- Base type library support -}
pInt_parseM :: PadsParser (Int,Base_md)
pInt_parseM = PadsParser (\source ->
    let p source a = if S.isEOR source then [((a,cleanBasePD),source)]
                     else let c = S.head source
                       in if Char.isDigit c then p (S.tail source)  (10*a + (Char.digitToInt c)) 
                          else [((a,cleanBasePD),source)]
    in if S.isEOF source then [((def, mkErrBasePD (E.FoundWhenExpecting "EOF" "Int") (S.getPos source)), source)]
       else if S.isEOR source then [((def, mkErrBasePD (E.FoundWhenExpecting "EOR" "Int") (S.getPos source)), source)]
       else if not $ Char.isDigit $ S.head source then 
                              [((def,mkErrBasePD (E.FoundWhenExpecting (mkStr (S.head source)) "Int") (S.getPos source)),source)]
       else p source 0 )


  
pCharLit_parseM :: Char -> PadsParser Base_md
pCharLit_parseM c  = PadsParser (\source ->
   if S.isEOF source then      [(mkErrBasePD (E.FoundWhenExpecting "EOF" (mkStr c)) (S.getPos source), source)]
   else if S.isEOR source then [(mkErrBasePD (E.FoundWhenExpecting "EOR" (mkStr c)) (S.getPos source), source)]
   else let c' = S.head source
         in if c == c' then [(cleanBasePD, S.tail source)]
            else let (foundIt, resSource,errPos) = S.scanTo c source 
                 in if foundIt then [(mkErrBasePD (E.ExtraBeforeLiteral (mkStr c)) errPos, resSource)] 
                    else            [(mkErrBasePD (E.MissingLiteral     (mkStr c)) errPos, resSource)] )





make_pads_declarations :: PadsDecl -> Q [Dec]
make_pads_declarations (PadsDecl (id, padsTy)) = do
   let p_name = case id of
                   Id str -> str
                   AntiId str -> error "Did not expect antiquotation in defining context."
   let ty_name    = getTyName    p_name
   let pd_ty_name = getMDName    p_name
   let parse_name = getParseName p_name
   let (ty,pd_ty,aux_ty_decls::[Dec]) = genTy padsTy
   let ty_decl :: Dec    = mk_newTyD ty_name ty
   let pd_ty_decl :: Dec = mk_newTyD pd_ty_name pd_ty
   let padsInstance :: [Dec]   = genPadsInstance ty_name pd_ty_name parse_name
   let padsPDInstance :: [Dec] = genPadsPDInstance pd_ty_name 
   let parseM :: [Dec] = genPadsParseM parse_name ty_name pd_ty_name padsTy 
   let parseS :: [Dec] = genPadsParseS p_name parse_name ty_name pd_ty_name padsTy 
   return ([ty_decl, pd_ty_decl] ++ aux_ty_decls ++ padsInstance ++ padsPDInstance ++ parseM ++ parseS)

genTy :: PadsTy -> (TH.Type, TH.Type, [Dec])
genTy ty = case ty of
  Pint      -> (ConT ''Int, ConT ''Base_md, [])
  Plit char -> (ConT ''(),  ConT ''Base_md, [])
--  Pname str -> (VarT (mkName str), [])
  Ptuple tys -> mkTupleTy tys


{- Convert a pads tuple type into a TH tuple type and any auxiliary declarations -}
mkTupleTy :: [PadsTy] -> (TH.Type, TH.Type, [Dec])
mkTupleTy tys = 
  let (tys', pds', declss) = unzip3 (map genTy tys)
      decls = concat declss
      r_tys = filter (\t -> t /= ConT ''()) tys'
      ty =  case r_tys of          -- Construct rep type for a tuple.
             []   -> ConT ''()     -- Tuple contained no non-singleton types, so its rep is the unit type.
             [ty] -> ty            -- Tuple contains one non-singleton type T, so its rep type is just T
             (r_ty:r_tys') ->      -- Rep is tuple of non-singleton types T.
                 foldl AppT (AppT (TupleT (length r_tys) ) r_ty) r_tys'
      pd_ty_nested = case pds' of  
                      []      -> ConT ''Base_md   -- Tuple contains no types, so its nested pd is just a base pd
                      [pd_ty] -> pd_ty            -- Tuple contains a single type, so its nested pd is just the pd of the type
                      (pd_ty:pd_tys') -> 
                         foldl AppT (AppT (TupleT (length pds') ) pd_ty) pd_tys'
     {- Pd of a tuple is a pair of a base pd and a tuple of pds for each element in the tuple. -}
      pd_ty = AppT (AppT (TupleT 2) (ConT ''Base_md)) pd_ty_nested     
  in
      (ty, pd_ty, decls)

genPadsInstance ty_name pd_ty_name parse_name = 
  let inst = AppT (AppT (ConT ''Pads) (ConT ty_name)) (ConT pd_ty_name)    -- Pads RepTy MDTy
      def = mkName "def"
      def_method = ValD (VarP def) (NormalB (VarE 'gdef)) []
      parsePP = mkName "parsePP"
      parsePP_method = ValD (VarP parsePP) (NormalB (VarE parse_name)) []
  in [InstanceD [] inst [def_method,parsePP_method]]


genPadsPDInstance pd_ty_name = 
  let inst = AppT (ConT ''PadsPD) (ConT pd_ty_name)
      get_md_header = mkName "get_md_header"
      md_hd = mkName "md_hd"
      md_rest = mkName "md_rest"
      get_hd_method = FunD get_md_header [Clause [ConP pd_ty_name [TupP [VarP md_hd,VarP md_rest]]] (NormalB (VarE md_hd)) []]
  in [InstanceD [] inst [get_hd_method]]


genPadsParseM :: Name -> Name -> Name -> PadsTy -> [Dec]
genPadsParseM parse_name rep_name pd_name padsTy = 
   let ty = AppT (ConT ''PadsParser) (AppT (AppT (TupleT 2) (ConT rep_name)) (ConT pd_name))
       sigD = SigD parse_name ty
       bodyE = genParseBody rep_name pd_name padsTy
       funD = ValD (VarP parse_name) (NormalB bodyE) []
   in [sigD, funD]

genPadsParseS :: String -> Name -> Name -> Name -> PadsTy -> [Dec]
genPadsParseS p_name parse_name rep_name pd_name padsTy = 
   let stringTy    = ConT ''String
       padsPairTy  =  AppT (AppT (TupleT 2) (ConT rep_name)) (ConT pd_name)
       resultTy    =  AppT (AppT (TupleT 2) padsPairTy) stringTy
       ty          = AppT (AppT ArrowT     stringTy  ) resultTy
       parseSName  = getParseSName p_name
       sigD = SigD parseSName ty
       bodySs = genParseBody rep_name pd_name padsTy
       funD = ValD (VarP parseSName) (NormalB (VarE 'parseS)) []
   in [sigD, funD]

genParseBody :: Name -> Name -> PadsTy -> TH.Exp
genParseBody repN mdN ty = 
   let repName     = genRepName 0
       mdName      = genMdName 0
       (repE,repP) = (VarE repName, VarP repName)
       (mdE, mdP)  = (VarE mdName,  VarP mdName)
       rhsE        = case ty of 
                       Ptuple tys -> mkParseTuple tys
       doParseS    = BindS (TupP [repP,mdP]) rhsE
       frepE       = AppE  (ConE repN) repE
       fmdE        = AppE  (ConE mdN ) mdE
       resultE     = TupE [frepE,fmdE]
       finalS      = NoBindS (AppE (VarE 'return) resultE)
   in 
      DoE [doParseS, finalS]

mkParseTuple :: [PadsTy] -> TH.Exp
mkParseTuple tys = 
  let (_,repEs,mdEs,bmdEs, stmts) = mkParseTupleB 0 tys
      top_md             = mkName "top_md"
      (top_mdE, top_mdP) = (VarE top_md, VarP top_md)
      headerE            = AppE (VarE 'mergeBaseMDs) (ListE bmdEs)
      mdS                = LetS [ValD top_mdP (NormalB headerE) []]
      repE               = TupE repEs
      mdE                = TupE [top_mdE, TupE mdEs]
      resultE            = TupE [repE,mdE]
      finalS             = NoBindS (AppE (VarE 'return) resultE)
  in
     DoE (stmts ++ [mdS,finalS])

mkParseTupleB :: Int -> [PadsTy] -> (Int, [TH.Exp], [TH.Exp], [TH.Exp], [Stmt])
mkParseTupleB init [] = (init, [],[],[],[])
mkParseTupleB init (ty:tys) = 
  let (i,rep_ty,   md_ty,  bmd_ty,  stmt_ty)  = mkParseTyB init ty
      (j,reps_tys, md_tys, bmd_tys, stmt_tys) = mkParseTupleB i tys
  in (j, rep_ty++reps_tys, md_ty++md_tys, bmd_ty ++ bmd_tys, stmt_ty++stmt_tys)

{- Input:
     Int: counter to generate new id
     PadsTy: type we are generating parsing instructions for
   Output:
     Int: new value of counter
     [TH.Exp]: list of expressions that store parse results (can be empty)
     [TH.Exp]: list of expressions that store meta data results (always !1)
     [TH.Exp]: list of expressions that store base level meta data (always !1) 
     [Stmt]  : list of statements to do parsing for PadsTy.
-}
mkParseTyB :: Int -> PadsTy -> (Int, [TH.Exp], [TH.Exp], [TH.Exp], [Stmt])
mkParseTyB next ty = 
   let repName     = genRepName next
       mdName      = genMdName  next
       bmdName     = genBMdName next
       (repE, repP)  = (VarE repName, VarP repName)
       ( mdE,  mdP)  = (VarE mdName,  VarP mdName)
       (bmdE, bmdP)  = (VarE bmdName, VarP bmdName)
       stmt2         = LetS [ValD bmdP (NormalB (AppE (VarE 'get_md_header) mdE)) []]
   in case ty of
       Pint ->       let prsName = getParseName "PInt"
                         stmt1    = BindS (TupP [repP,mdP]) (VarE prsName)
                     in (next+1, [repE], [mdE], [bmdE], [stmt1,stmt2])
       Plit c ->     let prsName = getParseName "pCharLit"
                         stmt1    = BindS mdP (AppE (VarE prsName) (LitE (CharL c)))
                     in (next+1, [], [mdE], [bmdE], [stmt1,stmt2])
       Ptuple tys -> let prsE = mkParseTuple tys
                         stmt1    = BindS (TupP [repP,mdP]) prsE
                     in (next+1, [repE], [mdE], [bmdE], [stmt1,stmt2])
       _ -> error "mkParseTyB: unimplemented case."

  

{- Helper functions -}
mapFstChar f [] = []
mapFstChar f (c:cs) = (f c) : cs

strToUpper = mapFstChar Char.toUpper
strToLower = mapFstChar Char.toLower

mk_newTyD ty_name ty = NewtypeD [] ty_name [] con derives
    where con = NormalC ty_name [(NotStrict,ty)]           -- How should we determine whether a type should be Strict or not?
          derives = (map mkName ["Show", "Eq"]) ++  [''Typeable, ''Data]


{- Name manipulation functions -}
genUniqueName base (i::Int) = mkName (base++(show i))
genRepName = genUniqueName "rep" 
genMdName  = genUniqueName "md" 
genBMdName = genUniqueName "bmd"
getMDName pname = mkName ((strToUpper pname) ++ "_md")
getTyName pname = mkName  (strToUpper pname)
getParseName pname = mkName ((strToLower pname) ++ "_parseM")
getParseSName pname = mkName ((strToLower pname) ++ "_parseS")

