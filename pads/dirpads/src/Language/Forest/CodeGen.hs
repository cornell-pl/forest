{-# LANGUAGE TemplateHaskell, NamedFieldPuns, ScopedTypeVariables, RecordWildCards #-}
module Language.Forest.CodeGen where


import Language.Forest.Syntax as PS
import Language.Forest.MetaData
--import Language.Forest.Generic
import qualified Language.Forest.Errors as E

import Language.Haskell.TH as TH
import Language.Haskell.TH.Syntax
import Language.Pads.Padsc
import Language.Pads.TH


import Data.Data
import Data.Char
import Data.Map 
import qualified Control.Exception as CE

--type Map = M.Map

fileload :: Pads pads md => FilePath -> IO (pads, (Forest_md, md))
fileload path = do
   fmd       <- getForestMD path
   (rep, md) <- parseFile path
   return (rep, (fmd, md))



{- Code generation routines -}
make_forest_declarations :: [ForestDecl] -> Q [Dec]
make_forest_declarations ds = fmap concat (mapM make_forest_declaration ds)

make_forest_declaration :: ForestDecl -> Q [Dec]
make_forest_declaration (ForestDecl (id, pat, forestTy)) = do
   let ty_name    = getTyName    id
   let md_ty_name = getMDName    id
   let load_name  = getLoadName  id
   let arg_info_opt = mergeMaybe pat (fmap patToTy pat)
   let (ty_decl, md_ty_decls, md_ty) = genRepMDDecl forestTy ty_name md_ty_name  -- Generate reprsentation and meta-data decls for padsTy
   loadM :: [Dec]                   <- genLoadM    load_name ty_name md_ty_name forestTy arg_info_opt
   return (   [ty_decl]    
           ++ md_ty_decls  
           ++ loadM
           )

genLoadM :: Name -> Name -> Name -> ForestTy -> Maybe (TH.Pat, TH.Type) -> Q [Dec]
genLoadM load_name rep_name pd_name forestTy mpat_info = do 
   let core_ty = arrowTy (ConT ''FilePath) (AppT (ConT ''IO) (AppT (AppT (TupleT 2) (ConT rep_name)) (ConT pd_name)))
   pathName    <- newName "path"
   let (pathE, pathP) = genPE pathName
   core_bodyE <- genLoadBody pathE rep_name pd_name forestTy
   let path_bodyE = LamE [pathP] core_bodyE
   let (bodyE,ty) = case mpat_info of
                     Nothing -> (path_bodyE, core_ty)
                     Just (pat,pat_ty) -> ( LamE [pat] path_bodyE,
                                            arrowTy pat_ty core_ty)
   let sigD = SigD load_name ty
   let funD = ValD (VarP load_name) (NormalB bodyE) []
   return [sigD, funD]

{-
 Generate body of loadM function, which has the form:
  do (rep,md) <- rhsE
     return (Rep rep, md)
-}
genLoadBody :: TH.Exp -> Name -> Name -> ForestTy -> Q TH.Exp
genLoadBody pathE repN mdN ty = do
   rhsE        <- loadE ty pathE
   case ty of 
     Directory _ -> return rhsE
     otherwise   -> do                  -- Add type constructor
          repName     <- genRepName 
          mdName      <- genMdName 
          let (repE,repP) = genPE repName
          let (mdE, mdP)  = genPE mdName
          let doLoadS     = BindS (TupP [repP,mdP]) rhsE
          let frepE       = AppE  (ConE repN) repE 
          let resultE     = TupE [frepE,mdE]
          let finalS      = NoBindS (AppE (VarE 'return) resultE)
          return (DoE [doLoadS, finalS])


loadE :: ForestTy -> TH.Exp -> Q TH.Exp
loadE ty pathE = case ty of
  Named f_name   -> return (AppE (VarE (getLoadName f_name)) pathE)
  File file_name -> return (AppE (VarE 'fileload) pathE)
  Directory dirTy -> loadDirectory dirTy pathE

loadDirectory :: DirectoryTy -> TH.Exp -> Q TH.Exp
loadDirectory ty pathE = case ty of
  Record id fields -> loadRecord id fields pathE

loadRecord :: String -> [Field] -> TH.Exp -> Q TH.Exp
loadRecord id fields pathE = do
  let dir_md             = mkName "dir_md"
  let (dir_mdE, dir_mdP) = genPE dir_md
  let top_md             = mkName "top_md"
  let (top_mdE, top_mdP) = genPE top_md
  let dir_mdS            = BindS  dir_mdP (AppE (VarE 'getForestMD) pathE)
  (repEs,mdEs,bmdEs, stmts) <- loadFields fields pathE
  let headerE            = AppE (AppE (VarE 'updateForestMDwith ) dir_mdE)  (ListE bmdEs)
  let mdS                = LetS [ValD top_mdP (NormalB headerE) []]
  let tyName             = mkName id
  let repE               = RecConE tyName repEs
  let inner_md_name      = getStructInnerMDName tyName   -- ty name is the same as the declared forest type name
  let mdE                = TupE [top_mdE, RecConE inner_md_name mdEs]
  let resultE            = TupE [repE,mdE]
  let finalS             = NoBindS (AppE (VarE 'return) resultE)
  return (DoE (dir_mdS : stmts ++ [mdS,finalS]))


loadFields :: [Field] -> TH.Exp -> Q ([FieldExp], [FieldExp], [TH.Exp], [Stmt])
loadFields [] pathE = return ([],[],[],[])
loadFields (field:fields) pathE = do
  (rep_field,   md_field,  bmd_field,  stmts_field)  <- loadField  field pathE
  (reps_fields, md_fields, bmd_fields, stmts_fields) <- loadFields fields pathE
  return (rep_field++reps_fields, md_field++md_fields, bmd_field:bmd_fields, stmts_field++stmts_fields)

loadField :: Field -> TH.Exp -> Q ([TH.FieldExp], [TH.FieldExp], TH.Exp, [Stmt])
loadField field = case field of
   Simple s -> loadSimple s
   Comp   c -> loadCompound c

loadSimple :: BasicField -> TH.Exp -> Q ([TH.FieldExp], [TH.FieldExp], TH.Exp, [Stmt])
loadSimple (internal, externalE, forestTy, predM) pathE = do
   let repName = mkName internal
   let mdName  = mkName (internal++"_md")
   bmdName <- newName (internal++"_bmd")
   let (repE, repP) = genPE repName
   let (mdE,  mdP ) = genPE mdName
   let (bmdE, bmdP) = genPE bmdName
   let newPathE     = appendStringM pathE (appendStringM (mkStrLitM "/") externalE)
   rhsE <- loadE forestTy newPathE
   case predM of 
     Nothing -> let
         stmt1 = BindS (TupP [repP,mdP]) rhsE                            
         stmt2 = LetS [ValD bmdP (NormalB (AppE (VarE 'get_fmd_header) mdE)) []]  -- Read out header of resulting parse descriptor        
         in return([(repName, repE)], [(mdName,mdE)], bmdE, [stmt1,stmt2]) 
     Just pred -> do
         final_mdName    <- newName (internal++"_final_md") 
         raw_bmdName     <- newName (internal++"_raw_bmd") 
         let (finalMDE, finalMDP) = genPE final_mdName
         let (rawBMDE, rawBMDP)   = genPE raw_bmdName
         let predTestE      = TH.CondE pred rawBMDE (AppE (VarE 'addPredFailureMD) rawBMDE)    -- if pred then rawBMD else addPredFailureMD rawBMD
         let replaceHeaderE = AppE (AppE (VarE 'replace_fmd_header) mdE) bmdE                -- replace_md_header mdE bmd
         let stmt1 = BindS (TupP [repP,mdP]) rhsE
         let stmt2 = LetS [ValD rawBMDP (NormalB (AppE (VarE 'get_fmd_header) mdE)) []]
         let stmt3 = LetS [ValD bmdP (NormalB predTestE)  []] 
         let stmt4 = LetS [ValD finalMDP  (NormalB replaceHeaderE) []]
         return ([(repName,repE)], [(mdName,finalMDE)], bmdE, [stmt1,stmt2,stmt3,stmt4])       -- Include named rep and md in result

newloadCompound :: CompField -> TH.Exp -> Q ([TH.FieldExp], [TH.FieldExp], TH.Exp, [Stmt])
newloadCompound (internal, externalE, forestTy, generatorP, generatorE, optPredE) pathE = do
   let repName = mkName internal
   let mdName  = mkName (internal++"_md")
   bmdName <- newName (internal++"_bmd")
   let (repE, repP) = genPE repName
   let (mdE,  mdP ) = genPE mdName
   let (bmdE, bmdP) = genPE bmdName
   let newPathE     = appendStringM pathE (appendStringM (mkStrLitM "/") externalE)
   rhsE <- loadE forestTy newPathE
   let compResultE = TupE[externalE, rhsE]  
   let CompE stmts = generatorE
   let compE = CompE ((init stmts) ++ [NoBindS compResultE])
   let buildMapsE = AppE (VarE 'insertRepMDs) compE
   let stmt = BindS (TupP [repP, mdP, bmdP])  buildMapsE
   return ([(repName,repE)], [(mdName,mdE)], bmdE, [stmt])       -- Include named rep and md in result

loadCompound :: CompField -> TH.Exp -> Q ([TH.FieldExp], [TH.FieldExp], TH.Exp, [Stmt])
loadCompound (internal, externalE, forestTy, generatorP, generatorE, optPredE) pathE = do
   let repName = mkName internal
   let mdName  = mkName (internal++"_md")
   bmdName <- newName (internal++"_bmd")
   let (repE, repP) = genPE repName
   let (mdE,  mdP ) = genPE mdName
   let (bmdE, bmdP) = genPE bmdName
   let newPathE     = appendStringM pathE (appendStringM (mkStrLitM "/") externalE)
   rhsE <- loadE forestTy newPathE
   let compResultE = TupE[externalE, rhsE]  
   let predSs = case optPredE of
                  Nothing -> [] 
                  Just predE -> [NoBindS predE]
   let compE = CompE ([BindS generatorP generatorE] ++ predSs ++ [ NoBindS compResultE])
   let buildMapsE = AppE (VarE 'insertRepMDs) compE
   let stmt = BindS (TupP [repP, mdP, bmdP])  buildMapsE
   return ([(repName,repE)], [(mdName,mdE)], bmdE, [stmt])       -- Include named rep and md in result


insertRepMDs :: ForestMD b => [(String, IO (a,b))] -> IO (Map String a, Map String b, Forest_md)
insertRepMDs inputs = do 
    let (paths, rep_mdIOs) = unzip inputs
    rep_mds <- sequence rep_mdIOs
    let (reps, mds) = unzip rep_mds
    let repList = zip paths reps
    let mdList = zip paths mds
    let bmdList = Prelude.map get_fmd_header mds
    let bmd = mergeForestMDs bmdList
    return (fromList repList, fromList mdList, bmd)





addPredFailureMD :: Forest_md -> Forest_md
addPredFailureMD (Forest_md{numErrors, errorMsg, fileInfo}) = 
  let errMsg' = case errorMsg of
                  Nothing -> E.PredicateFailure
                  Just e ->  e
  in Forest_md{numErrors = numErrors + 1, errorMsg = Just errMsg', fileInfo = fileInfo}


appendStringM str1E str2E = 
   AppE (AppE (VarE '(++)) str1E) str2E

mkStrLitM s = LitE (StringL s)





genRepMDDecl :: ForestTy -> Name -> Name -> (TH.Dec, [TH.Dec], TH.Type)
genRepMDDecl ty ty_name md_ty_name = case ty of
  Directory dirTy -> genRepMDDir dirTy ty_name md_ty_name
  File padsty     -> let (rep,md) = genRepMDTy (File padsty)
                     in  (mk_newTyD ty_name rep, [mk_TySynD md_ty_name md], md) 
  Named ty         -> let (rep,md) = genRepMDTy (Named ty)
                     in  (mk_newTyD ty_name rep, [mk_TySynD md_ty_name md], md) 

genRepMDDir :: DirectoryTy -> Name -> Name -> (TH.Dec, [TH.Dec], TH.Type)
genRepMDDir ty ty_name md_ty_name = case ty of
  Record _ fields -> genRepMDDeclRecord ty_name md_ty_name fields

{- Generate a representation and meta-data type for a directory with named fields. -}
genRepMDDeclRecord :: Name -> Name -> [Field] -> (TH.Dec, [TH.Dec], TH.Type)
genRepMDDeclRecord ty_name md_ty_name fields = 
  let (vsts', md_vsts') = unzip $ Prelude.map genRepMDField fields
      derives      = [''Show, ''Eq, ''Typeable, ''Data]
      ty_con       = TH.RecC ty_name vsts'
      ty_decl      = TH.DataD [] ty_name [] [ty_con] derives
      inner_md_name = getStructInnerMDName ty_name   -- ty name is the same as the declared pads type name
      imd_con       = TH.RecC inner_md_name md_vsts'
      imd_decl      = TH.DataD [] inner_md_name [] [imd_con] derives   -- declaration of line for nested components
      imd_ty        = TH.ConT inner_md_name
      md_ty         = tyListToTupleTy [ConT ''Forest_md, imd_ty]
      md_decl       = mk_TySynD md_ty_name md_ty
  in if length vsts' == 0 then 
        error ("Error: Directory " ++ (show ty_name) ++ " must contain at least one named field.")
     else 
        (ty_decl, [imd_decl,md_decl], md_ty)
 
type VST = (TH.Name, TH.Strict, TH.Type)
genRepMDField :: Field -> (VST, VST)
genRepMDField (Simple (internal, external, ty, predM)) = let
   (rep_ty, md_ty) = genRepMDTy ty
   in ((getFieldName   internal, TH.NotStrict, rep_ty),
       (getFieldMDName internal, TH.NotStrict, md_ty))
genRepMDField (Comp (internal, externalE, ty, generatorP, generatorE, optPredE)) = let
   (rng_rep_ty, rng_md_ty) = genRepMDTy ty
   (rep_ty, md_ty) = (mkStringMapTy rng_rep_ty, mkStringMapTy rng_md_ty)
   in ((getFieldName   internal, TH.NotStrict, rep_ty),
       (getFieldMDName internal, TH.NotStrict, md_ty))

mkStringMapTy ty = AppT (AppT (ConT ''Map) (ConT ''String)) ty

{- Generate type and meta-data representations. -}
genRepMDTy ::  ForestTy -> (TH.Type, TH.Type)
genRepMDTy ty = case ty of
    Directory _ -> error "Forest: Directory declarations must appear at the top level."
    File ty_name   -> (ConT (getTyName ty_name), tyListToTupleTy [ConT ''Forest_md, ConT (getMDName ty_name)])
    Named ty_name  -> (ConT (getTyName ty_name), ConT (getMDName ty_name))



{- Name manipulation functions -}
genUniqueName base = newName base
genRepName = genUniqueName "rep" 
genMdName  = genUniqueName "md" 
genBMdName = genUniqueName "bmd"
getMDName pname = mkName ((strToUpper pname) ++ "_md")

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

getLoadName pname = mkName ((strToLower pname) ++ "_load")


genPE name = (VarE name, VarP name)





