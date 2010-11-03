{-# LANGUAGE TemplateHaskell, NamedFieldPuns, ScopedTypeVariables, RecordWildCards, FlexibleInstances, MultiParamTypeClasses,
    UndecidableInstances  #-}
module Language.Forest.CodeGen where
import System.IO.Unsafe (unsafePerformIO)

import Language.Forest.Syntax as PS
import Language.Forest.MetaData
import Language.Forest.Generic
import qualified Language.Forest.Errors as E

import Language.Haskell.TH as TH
import Language.Haskell.TH.Syntax
import Language.Pads.Padsc
import Language.Pads.TH
import Language.Forest.ForestIO

import Data.Data
import Data.Char
import Data.Map 
import qualified Control.Exception as CE




{- Code generation routines -}
make_forest_declarations :: [ForestDecl] -> Q [Dec]
make_forest_declarations ds = fmap concat (mapM make_forest_declaration ds)

make_forest_declaration :: ForestDecl -> Q [Dec]
make_forest_declaration (ForestDecl (id, pat, forestTy)) = do
   let ty_name    = getTyName    id
   let md_ty_name = getMDName    id
   let load_name  = getLoadName  id
   let arg_info_opt = mergeMaybe pat (fmap patToTy pat)
   (ty_decl, md_ty_decls, md_ty) <- genRepMDDecl forestTy ty_name md_ty_name  -- Generate reprsentation and meta-data decls for padsTy
   let forestInstance       :: [Dec] = genFInst    load_name ty_name md_ty      forestTy arg_info_opt
   loadM :: [Dec]                   <- genLoadM    load_name ty_name md_ty_name forestTy arg_info_opt
   return (   [ty_decl]    
           ++ md_ty_decls  
           ++ forestInstance
           ++ loadM
           )

genFInst load_name ty_name md_ty forestTy mpat_info = 
      let (inst, load) = case mpat_info of
                          Nothing -> (AppT (AppT (ConT ''Forest) (ConT ty_name)) md_ty,   -- Forest RepTy MDTy
                                      mkName "load")
                          Just (p,arg_ty) -> 
                                     (AppT 
                                        (AppT (AppT (ConT ''Forest1) arg_ty) (ConT ty_name)) 
                                        md_ty,   -- Forest1 Arg RepTy MDTy
                                      mkName "load1")
          load_method = ValD (VarP load) (NormalB (VarE load_name)) []
      in [InstanceD [] inst [load_method]]


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

{-
loadE :: ForestTy -> TH.Exp -> Q TH.Exp
loadE ty pathE = do
   { actionE <- rawLoadE ty pathE
   ; return (AppE (AppE (VarE 'checkPath) pathE) actionE)
   }
-}

loadNonEmptyE :: ForestTy -> TH.Exp -> TH.Exp -> Q TH.Exp
loadNonEmptyE ty pathE fileE = do
  { actionE <- loadE ty pathE
  ; return (AppE (AppE (AppE (VarE 'checkNonEmpty) pathE) fileE) actionE)
  }


loadE :: ForestTy -> TH.Exp -> Q TH.Exp
loadE ty pathE = case ty of
  Named f_name   -> return (AppE (VarE (getLoadName f_name)) pathE)
  File (file_name, argEOpt) -> case argEOpt of 
                                Nothing ->     return (AppE (VarE 'fileload) pathE)
                                Just argE ->   return (AppE (AppE (VarE 'fileload1) argE) pathE)
  Gzip ty         -> loadGzip ty pathE
  Tar  ty         -> loadTar  ty pathE
  SymLink         -> loadSymLink pathE
  FConstraint p ty pred -> loadConstraint p ty pred pathE
  Fapp (Named f_name) argE  -> return (AppE (AppE (VarE (getLoadName f_name)) argE) pathE)   -- XXX should add type checking to ensure that ty is expecting an argument
  Directory dirTy -> loadDirectory dirTy pathE
  FMaybe forestTy -> loadMaybe forestTy pathE
  FComp cinfo     -> loadComp cinfo pathE


loadConstraint :: TH.Pat -> ForestTy -> TH.Exp -> TH.Exp -> Q TH.Exp
loadConstraint pat ty predE pathE = do
  { loadAction <- loadE ty pathE
  ; let predFnE = LamE [getRepMDPatFromPat pat] predE
  ; return (AppE (AppE (AppE (VarE 'doLoadConstraint) loadAction) pathE) predFnE)
  }

loadSymLink :: TH.Exp -> Q TH.Exp
loadSymLink pathE = do
  return (AppE (VarE 'doLoadSymLink) pathE)

loadGzip :: ForestTy -> TH.Exp -> Q TH.Exp
loadGzip ty pathE = do
  { newPathName <- newName "new_path"
  ; let (newPathE, newPathP) = genPE newPathName
  ; rawE <- loadE ty newPathE
  ; let rhsE =  LamE [newPathP] rawE
  ; return (AppE (AppE (VarE 'gzipload) rhsE) pathE)
  }

loadTar :: ForestTy -> TH.Exp -> Q TH.Exp
loadTar ty pathE = do
  { newPathName <- newName "new_path"
  ; let (newPathE, newPathP) = genPE newPathName
  ; rawE <- loadE ty newPathE
  ; let rhsE =  LamE [newPathP] rawE
  ; return (AppE (AppE (VarE 'tarload) rhsE) pathE)
  }

{-
loadTyApp :: ForestTy -> TH.Exp -> TH.Exp -> Q TH.Exp
loadTyApp ty pathE argE = do
  loadFnE <- loadE ty pathE            
  return (AppE loadFnE argE) 
-}

loadMaybe :: ForestTy -> TH.Exp -> Q TH.Exp
loadMaybe ty pathE = do 
   rhsE <- loadE ty pathE
   return (AppE (AppE(VarE 'doLoadMaybe) pathE) rhsE)

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
  let ifDirE             = DoE (stmts ++ [mdS, finalS])
  let chkDirS            = NoBindS(AppE (AppE (VarE 'checkIsDir ) dir_mdE)  ifDirE)
  let doDirE             = DoE [dir_mdS, chkDirS]
  return (AppE (AppE (VarE 'checkPath) pathE) doDirE)


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
loadSimple (internal, isForm, externalE, forestTy, predM) pathE = do
   let repName = mkName internal
   let mdName  = mkName (internal++"_md")
   bmdName <- newName (internal++"_bmd")
   filesName <- newName (internal++"_files")
   let (repE, repP) = genPE repName
   let (mdE,  mdP ) = genPE mdName
   let (bmdE, bmdP) = genPE bmdName
   let (filesE, filesP) = genPE filesName
   let (fileE, pathStmts) = if isForm then (externalE, [])
                            else (AppE (VarE 'pickFile) filesE,
                                  [BindS filesP (AppE (AppE (VarE 'getMatchingFiles) pathE) externalE)])
   let newPathE     = AppE (AppE (VarE 'concatPath) pathE) fileE
   rhsE <- loadNonEmptyE forestTy newPathE fileE
--   rhsE <- loadE forestTy newPathE 
   let stmt1 = BindS (TupP [repP,mdP]) rhsE                            
   case predM of 
     Nothing -> let
         stmt2 = LetS [ValD bmdP (NormalB (AppE (VarE 'get_fmd_header) mdE)) []]  -- Read out header of resulting parse descriptor        
         in return([(repName, repE)], [(mdName,mdE)], bmdE, pathStmts++[stmt1,stmt2]) 
     Just pred -> do
         final_mdName    <- newName (internal++"_final_md") 
         raw_bmdName     <- newName (internal++"_raw_bmd") 
         let (finalMDE, finalMDP) = genPE final_mdName
         let (rawBMDE, rawBMDP)   = genPE raw_bmdName
         let predTestE      = TH.CondE pred rawBMDE (AppE (VarE 'addPredFailureMD) rawBMDE)    -- if pred then rawBMD else addPredFailureMD rawBMD
         let replaceHeaderE = AppE (AppE (VarE 'replace_fmd_header) mdE) bmdE                -- replace_md_header mdE bmd
         let stmt2 = LetS [ValD rawBMDP (NormalB (AppE (VarE 'get_fmd_header) mdE)) []]
         let stmt3 = LetS [ValD bmdP (NormalB predTestE)  []] 
         let stmt4 = LetS [ValD finalMDP  (NormalB replaceHeaderE) []]
         return ([(repName,repE)], [(mdName,finalMDE)], bmdE, pathStmts++[stmt1,stmt2,stmt3,stmt4])       -- Include named rep and md in result


loadComp :: CompField -> TH.Exp -> Q TH.Exp
loadComp cinfo pathE = do
   { fmdName   <- newName ("fmd")
   ; let (fmdE, fmdP) = genPE fmdName
   ; (_,_,_,stmts) <- loadCompound cinfo pathE
   ; let dir_mdS = BindS  fmdP (AppE (VarE 'getForestMD) pathE)
   ; let resultE = TupE [VarE (mkName "this"), TupE[fmdE, VarE (mkName "this_md")]]
   ; let returnS = NoBindS (AppE (VarE 'return) resultE)
   ; let isGoodE = DoE ([dir_mdS]++stmts++[returnS])
   ; return (AppE (AppE (VarE 'checkPathIsDir) pathE) isGoodE)
   }

loadCompound :: CompField -> TH.Exp -> Q ([TH.FieldExp], [TH.FieldExp], TH.Exp, [Stmt])
loadCompound (CompField {internalName, tyConNameOpt, explicitName, externalE, descTy, generatorP, generatorG, predEOpt}) pathE = do
   let repName = mkName internalName
   let mdName  = mkName (internalName++"_md")
   bmdName   <- newName (internalName++"_bmd")
   filesName <- newName (internalName++"_files")
   metadatasName <- newName (internalName++"_metadatas")
   fmName        <- newName (internalName++"_fm")
   let (repE, repP) = genPE repName
   let (mdE,  mdP ) = genPE mdName
   let (bmdE, bmdP) = genPE bmdName
   let (filesE, filesP) = genPE filesName
   let (metadatasE, metadatasP) = genPE metadatasName
   let (fmE, fmP) = genPE fmName
   let newPathE     = AppE (AppE (VarE 'concatPath) pathE ) externalE
   rhsE <- loadNonEmptyE descTy newPathE externalE
   let compResultE = TupE[externalE, rhsE]
   let getFilesE regexpE = AppE (AppE (VarE 'getMatchingFiles) pathE) regexpE
   let genStmts = case generatorG of 
               Explicit expE    -> [LetS [ValD filesP (NormalB expE) []]]
               Matches  regexpE -> [BindS filesP (getFilesE regexpE)]
   let getRelFMDE = AppE (VarE 'getRelForestMD) pathE
   let (generatorP', generatorE, predStmts, compPredStmts) = case predEOpt of
                 Nothing ->    (generatorP, filesE, [], [])
                 Just predE -> (getAttPat explicitName externalE,
                                fmE, 
                                [ BindS metadatasP  (AppE (AppE (VarE 'mapM) getRelFMDE) filesE)
                                , LetS [(ValD fmP (NormalB (AppE (AppE (VarE 'zip) filesE) metadatasE) ) [])]],
                                [NoBindS predE])
   let compE = CompE ([BindS generatorP' generatorE]++ compPredStmts ++[NoBindS compResultE])
   buildMapsE <- case tyConNameOpt of
                     Nothing  -> do return (AppE (VarE 'insertRepMDsList) compE)
                     Just str -> do 
                                  { arity <- getTyConArity str   
                                  ; if arity == 1 then
                                       return (AppE (VarE 'insertRepMDsGeneric1) compE)
                                    else 
                                       return (AppE (VarE 'insertRepMDsGeneric2) compE)
                                  }
   let mapStmt = BindS (TildeP (TupP [repP, mdP, bmdP]))  buildMapsE
   return ([(repName,repE)], [(mdName,mdE)], bmdE, genStmts++predStmts++[mapStmt])       -- Include named rep and md in result

runGetTyConArity :: String ->  Int
runGetTyConArity str = unsafePerformIO (runQ (getTyConArity str))


getTyConArity :: String -> Q Int
getTyConArity str = do 
  { let tyConName = mkName str
  ; info <- qReify tyConName
  ; case info of
     TyConI dec -> return (getDecArity dec)
     otherwise -> error ("Forest: "++ str ++" should be a type constructor.")
  }

getDecArity :: TH.Dec -> Int
getDecArity dec = case dec of
  DataD cxt name args cons derives -> length args
  NewtypeD cxt name args con derives -> length args
  TySynD name args ty -> length args

getAttPat explicitName externalE = 
  case explicitName of 
      Just str -> TildeP (TupP[VarP (mkName str), VarP (mkName (str++"_att"))])
      Nothing -> getAttPatFromExp externalE

getAttPatFromExp externalE = case externalE of
  VarE name -> TildeP (TupP[VarP name, VarP (mkName ((nameBase name) ++"_att"))])
  otherwise -> error "Forest: Couldn't convert file expression to pattern; please supply an explicit name with 'name as exp' form"

getRepMDPat explicitName externalE = 
  case explicitName of 
      Just str -> TildeP (TupP[VarP (mkName str), VarP (mkName (str++"_md"))])
      Nothing -> getRepMDPatFromExp externalE

getRepMDPatFromExp externalE = case externalE of
  VarE name -> TildeP (TupP[VarP name, VarP (mkName ((nameBase name) ++"_md"))])
  otherwise -> error "Forest: Couldn't convert file expression to pattern; please supply an explicit name with 'name as exp' form"

getRepMDPatFromPat externalP = case externalP of
  VarP name -> TildeP (TupP[VarP name, VarP (mkName ((nameBase name) ++"_md"))])
  otherwise -> error "Forest: Couldn't convert constraint pattern to pattern for meta data.  Use simple pattern."



emptyMap :: Map a b
emptyMap = Data.Map.empty



filterWPred :: [(String, IO (a,b), ((a,b) -> Bool))] -> IO [ (String, (a,b))] 
filterWPred [] = return [] 
filterWPred (~(name, action, pred):rest) = do 
 { ~(rep,md) <- action
 ; if pred (rep,md) then do 
      { resRes <- filterWPred rest
      ; return ((name,(rep,md)) : resRes)  
      } 
   else filterWPred rest
 }




insertRepMDsList :: ForestMD b => [(String, IO (a,b))] -> IO ([(String, a)], [(String, b)], Forest_md)
insertRepMDsList inputs = do 
    let (paths, rep_mdIOs) = unzip inputs
    rep_mds <- sequence rep_mdIOs
    let (reps, mds) = unzip rep_mds
    let repList = zip paths reps
    let mdList = zip paths mds
    let bmdList = Prelude.map get_fmd_header mds
    let bmd = mergeForestMDs bmdList
    return (repList, mdList, bmd)


insertRepMDsGeneric1 :: (ForestMD b, BuildContainer1 c a, BuildContainer1 c b) => [(FilePath, IO (a,b))] -> IO (c (FilePath, a), c (FilePath, b), Forest_md)
insertRepMDsGeneric1 inputs = do 
    (repList, mdList, bmd) <- insertRepMDsList inputs
    return (buildContainer1 repList, buildContainer1 mdList, bmd)

insertRepMDsGeneric2 :: (ForestMD b, BuildContainer2 c a, BuildContainer2 c b) => [(FilePath, IO (a,b))] -> IO (c FilePath a, c FilePath b, Forest_md)
insertRepMDsGeneric2 inputs = do 
    (repList, mdList, bmd) <- insertRepMDsList inputs
    return (buildContainer2 repList, buildContainer2 mdList, bmd)

addPredFailureMD :: Forest_md -> Forest_md
addPredFailureMD (Forest_md{numErrors, errorMsg, fileInfo}) = 
  let errMsg' = case errorMsg of
                  Nothing -> E.PredicateFailure
                  Just e ->  e
  in Forest_md{numErrors = numErrors + 1, errorMsg = Just errMsg', fileInfo = fileInfo}


appendStringM str1E str2E = 
   AppE (AppE (VarE '(++)) str1E) str2E

mkStrLitM s = LitE (StringL s)

 



genRepMDDecl :: ForestTy -> Name -> Name -> Q (TH.Dec, [TH.Dec], TH.Type)
genRepMDDecl ty ty_name md_ty_name = case ty of
  Directory dirTy -> genRepMDDir dirTy ty_name md_ty_name
  others          -> do (rep,md) <- genRepMDTy others
                        return (mk_newTyD ty_name rep, [mk_TySynD md_ty_name md], md) 

{- Generate a representation and meta-data type for maybe. -}
genRepMDMaybe :: ForestTy -> Q (TH.Type, TH.Type)
genRepMDMaybe ty = do
  { (rep_orig, md_orig) <- genRepMDTy ty
  ; let rep_ty = AppT (ConT ''Maybe) rep_orig                 -- rep is Maybe ty where ty is rep of nested type
  ; let md'_ty = AppT (ConT ''Maybe) md_orig                  -- underyling md is Maybe of md of nested type
  ; let md_ty  = tyListToTupleTy [ConT ''Forest_md, md'_ty ]    -- md is a pair of a base md for the maybe and the underlying md.
  ; return (rep_ty, md_ty)
  }


genRepMDDir :: DirectoryTy -> Name -> Name -> Q (TH.Dec, [TH.Dec], TH.Type)
genRepMDDir ty ty_name md_ty_name = case ty of
  Record _ fields -> genRepMDDeclRecord ty_name md_ty_name fields

{- Generate a representation and meta-data type for a directory with named fields. -}
genRepMDDeclRecord :: Name -> Name -> [Field] -> Q (TH.Dec, [TH.Dec], TH.Type)
genRepMDDeclRecord ty_name md_ty_name fields = do
    { reps <- mapM genRepMDField fields
    ; let (vsts', md_vsts') = unzip reps
    ; let derives      = [''Show, ''Eq, ''Typeable, ''Data, ''Ord]
    ; let ty_con       = TH.RecC ty_name vsts'
    ; let ty_decl      = TH.DataD [] ty_name [] [ty_con] derives
    ; let inner_md_name = getStructInnerMDName ty_name   -- ty name is the same as the declared pads type name
    ; let imd_con       = TH.RecC inner_md_name md_vsts'
    ; let imd_decl      = TH.DataD [] inner_md_name [] [imd_con] derives   -- declaration of line for nested components
    ; let imd_ty        = TH.ConT inner_md_name
    ; let md_ty         = tyListToTupleTy [ConT ''Forest_md, imd_ty]
    ; let  md_decl       = mk_TySynD md_ty_name md_ty
    ;  if length vsts' == 0 then 
         error ("Error: Directory " ++ (show ty_name) ++ " must contain at least one named field.")
       else 
        return (ty_decl, [imd_decl,md_decl], md_ty)
    }
 
type VST = (TH.Name, TH.Strict, TH.Type)
genRepMDField :: Field -> Q (VST, VST)
genRepMDField (Simple (internal, isForm, external, ty, predM)) = do
   { (rep_ty, md_ty) <- genRepMDTy ty
   ; return ((getFieldName   internal, TH.NotStrict, rep_ty),
             (getFieldMDName internal, TH.NotStrict, md_ty))
   }

genRepMDField (Comp (info @ CompField {internalName, tyConNameOpt, descTy, ..})) = do
  { (rep_ty, md_ty) <- genRepMDComp info
  ; return ((getFieldName   internalName, TH.NotStrict, rep_ty),
            (getFieldMDName internalName, TH.NotStrict, md_ty))
  }

genRepMDComp (CompField {internalName, tyConNameOpt, descTy, ..}) = do
  { (rng_rep_ty, rng_md_ty) <- genRepMDTy descTy
  ; (rep_ty, md_ty) <- case tyConNameOpt of 
                        Nothing ->  return (mkStringListTy rng_rep_ty, mkStringListTy rng_md_ty)
                        Just str -> do { arity <- getTyConArity str
                                       ; case arity of 
                                          1 -> return (mkStringConTupleTy (mkName str) rng_rep_ty, mkStringConTupleTy (mkName str) rng_md_ty) 
                                          2 -> return (mkStringConCurryTy (mkName str) rng_rep_ty, mkStringConCurryTy (mkName str) rng_md_ty) 
                                       }
  ; return (rep_ty, md_ty)
  }

genRepMDCompTy info = do
  { (rep_ty, md'_ty) <- genRepMDComp info
  ; let md_ty  = tyListToTupleTy [ConT ''Forest_md, md'_ty ]    -- md is a pair of a base md for the maybe and the underlying md.
  ; return (rep_ty, md_ty)
  }

mkStringConTupleTy con ty = AppT (ConT con)  (tyListToTupleTy [ConT ''String, ty])
mkStringConCurryTy con ty = AppT (AppT (ConT con) (ConT ''String)) ty
mkStringListTy ty = AppT ListT (tyListToTupleTy [ConT ''String, ty])


{- Generate type and meta-data representations. -}
genRepMDTy ::  ForestTy -> Q (TH.Type, TH.Type)
genRepMDTy ty = case ty of
    Directory _          -> error "Forest: Directory declarations must appear at the top level."
    File (ty_name,arg)   -> return (ConT (getTyName ty_name), tyListToTupleTy [ConT ''Forest_md, ConT (getMDName ty_name)])
    Gzip ty              -> genRepMDTy ty
    Tar  ty              -> genRepMDTy ty
    SymLink              -> return (ConT ''FilePath, tyListToTupleTy [ConT ''Forest_md, ConT ''Base_md])
    Named ty_name        -> return (ConT (getTyName ty_name), ConT (getMDName ty_name))
    FConstraint p ty pred -> genRepMDTy ty
    FMaybe ty            -> genRepMDMaybe ty
    Fapp ty arg          -> genRepMDTy ty
    FComp cinfo          -> genRepMDCompTy cinfo


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




