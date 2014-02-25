{-# LANGUAGE TemplateHaskell, NamedFieldPuns, ScopedTypeVariables, RecordWildCards, FlexibleInstances, MultiParamTypeClasses,
    UndecidableInstances  #-}

{-
** *********************************************************************
*                                                                      *
*              This software is part of the pads package               *
*           Copyright (c) 2005-2011 AT&T Knowledge Ventures            *
*                      and is licensed under the                       *
*                        Common Public License                         *
*                      by AT&T Knowledge Ventures                      *
*                                                                      *
*                A copy of the License is available at                 *
*                    www.padsproj.org/License.html                     *
*                                                                      *
*  This program contains certain software code or other information    *
*  ("AT&T Software") proprietary to AT&T Corp. ("AT&T").  The AT&T     *
*  Software is provided to you "AS IS". YOU ASSUME TOTAL RESPONSIBILITY*
*  AND RISK FOR USE OF THE AT&T SOFTWARE. AT&T DOES NOT MAKE, AND      *
*  EXPRESSLY DISCLAIMS, ANY EXPRESS OR IMPLIED WARRANTIES OF ANY KIND  *
*  WHATSOEVER, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF*
*  MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE, WARRANTIES OF  *
*  TITLE OR NON-INFRINGEMENT.  (c) AT&T Corp.  All rights              *
*  reserved.  AT&T is a registered trademark of AT&T Corp.             *
*                                                                      *
*                   Network Services Research Center                   *
*                          AT&T Labs Research                          *
*                           Florham Park NJ                            *
*                                                                      *
*              Kathleen Fisher <kfisher@research.att.com>              *
*                                                                      *
************************************************************************
-}

module Language.Forest.CodeGen where
import System.IO.Unsafe (unsafePerformIO)

import Language.Forest.Syntax as PS
import Language.Forest.MetaData
import Language.Forest.Generic
import qualified Language.Forest.Errors as E
import Language.Forest.Writing
import System.Directory
import System.FilePath.Posix

import Language.Haskell.TH as TH
import Language.Haskell.TH.Syntax
import Language.Pads.Padsc
import Language.Pads.TH
import Language.Forest.ForestIO

import Data.Data
import Data.Char
import qualified Data.Map as Map
import qualified Data.List as List
import qualified Control.Exception as CE
import Control.Monad




{- Code generation routines -}
make_forest_declarations :: [ForestDecl] -> Q [Dec]
make_forest_declarations ds = fmap concat (mapM make_forest_declaration ds)

make_forest_declaration :: ForestDecl -> Q [Dec]
make_forest_declaration (ForestDecl (id, pat, forestTy)) = do
   let ty_name    = getTyName    id
   let md_ty_name = getMDName    id
   let load_name  = getLoadName  id
   let wn         = getWriteManifestName id
   let genM_name  = getGenManifestName id
   let arg_info_opt = mergeMaybe pat (fmap patToTy pat)
   (ty_decl, md_ty_decls, md_ty) <- genRepMDDecl forestTy ty_name md_ty_name  -- Generate reprsentation and meta-data decls for padsTy
   let forestInstance       :: [Dec] = genFInst    load_name wn genM_name ty_name md_ty      forestTy arg_info_opt
   loadM :: [Dec]                   <- genLoadM    load_name    ty_name md_ty_name forestTy arg_info_opt
   writeManifest :: [Dec]           <- genWriteManifest wn      ty_name md_ty_name forestTy arg_info_opt
   generateManifest :: [Dec]        <- genGenManifest genM_name load_name wn ty_name md_ty_name arg_info_opt
   return (   [ty_decl]    
           ++ md_ty_decls  
           ++ forestInstance
           ++ loadM
           ++ writeManifest
           ++ generateManifest
           )

genFInst load_name wn_name genM_name ty_name md_ty forestTy mpat_info = 
      let (inst, load, wn, genM) = case mpat_info of
                          Nothing -> (AppT (AppT (ConT ''Forest) (ConT ty_name)) md_ty,   -- Forest RepTy MDTy
                                      mkName "load",
                                      mkName "updateManifest",
                                      mkName "generateManifest")
                          Just (p,arg_ty) -> 
                                     (AppT 
                                        (AppT (AppT (ConT ''Forest1) arg_ty) (ConT ty_name)) 
                                        md_ty,   -- Forest1 Arg RepTy MDTy
                                      mkName "load1",
                                      mkName "updateManifest1",
                                      mkName "generateManifest1")
          load_method = ValD (VarP load) (NormalB (VarE load_name)) []
          update_manifest_method = ValD (VarP wn) (NormalB (VarE wn_name)) []
          generate_manifest_method = ValD (VarP genM) (NormalB (VarE genM_name)) []
      in [InstanceD [] inst [load_method, update_manifest_method, generate_manifest_method]]

genGenManifest :: Name -> Name -> Name -> Name -> Name -> Maybe (TH.Pat, TH.Type) -> Q[Dec]
genGenManifest genMName load_name wn rep_name md_name mpat_info = do 
  { let core_ty = arrowTy (AppT (AppT (TupleT 2) (ConT rep_name)) (ConT md_name)) 
                      (AppT (ConT ''IO) (ConT (mkName "Manifest")))
  ; (argE, argP) <- doGenPE "arg"
  ; (bodyE,ty) <- case mpat_info of
        Nothing -> do 
            { bodyE <- genGenManE (VarE load_name) (VarE wn)
            ; return (bodyE, core_ty)
            }
        Just(pat,pat_ty) -> do
            { core_bodyE <- genGenManE (AppE (VarE load_name) argE) (AppE (VarE wn) argE)
            ; return (LamE [argP] core_bodyE,
                      arrowTy pat_ty core_ty)
            }
  ; let sigD = SigD genMName ty
  ; let funD = ValD (VarP genMName) (NormalB bodyE) []
  ; return [sigD, funD]
  }


genGenManE :: TH.Exp -> TH.Exp -> Q TH.Exp
genGenManE loadE updateManE  = do
  { (man0E,man0P) <- doGenPE "manifest"
  ; (man1E,man1P) <- doGenPE "manifest"
  ; (man2E,man2P) <- doGenPE "manifest"
  ; (forestE,forestP) <- doGenPE "forest"
  ; let newManifestS = BindS man0P (VarE 'newManifest)
  ; let rawManifestS = BindS man1P (AppE (AppE updateManE forestE) man0E ) 
  ; let medManifestS = BindS man2P (AppE (VarE 'validateManifest) man1E ) 
  ; let finalS = NoBindS (AppE (AppE (AppE (VarE 'validateLists) loadE) updateManE) man2E)
  ; return (LamE [forestP] (DoE [newManifestS,rawManifestS,medManifestS,finalS]))
  }

genWriteManifest :: Name -> Name ->  Name -> ForestTy -> Maybe (TH.Pat, TH.Type) -> Q [Dec]
genWriteManifest    funName rep_name md_name forestTy mpat_info = do 
   core_bodyE <- writeE rep_name forestTy
   let core_ty = arrowTy (AppT (AppT (TupleT 2) (ConT rep_name)) (ConT md_name)) 
                (arrowTy (ConT (mkName "Manifest")) 
                (AppT (ConT ''IO) (ConT (mkName "Manifest"))))
   let (bodyE,ty) = case mpat_info of
                     Nothing -> (core_bodyE, core_ty)
                     Just (pat,pat_ty) -> ( LamE [pat] core_bodyE,
                                            arrowTy pat_ty core_ty)
   let sigD = SigD funName ty
   let funD = ValD (VarP funName) (NormalB bodyE) []
   return [sigD, funD]

writeE :: Name -> ForestTy -> Q TH.Exp
writeE repN ty = do
   repName  <- genRepName 
   mdName   <- genMdName
   (repE, repP)   <- doGenPE "rep"
   (mdE,  mdP)    <- doGenPE "md"
   (man1E, man1P) <- doGenPE "manifest"
   (man2E, man2P) <- doGenPE "manifest"
   let frepP       = wrapRepP repN ty repP 
   let setRootE = AppE (AppE (VarE 'setManifestRoot) mdE) man1E
   rhsE        <- writeE' (ty, repE, mdE, man2E)
--   let letE = LetE [ValD man2P (NormalB setRootE) []] rhsE
   let setRootS = BindS man2P setRootE
   let bodyE = DoE [setRootS, NoBindS rhsE]
   let writeFun = LamE [TupP [frepP, mdP]] (LamE [man1P] bodyE)
   return writeFun

{-
do { man2P <- setManifestRoot mdE man1E
   ; rhsE
   }
-}

wrapRepP :: Name -> ForestTy -> TH.Pat -> TH.Pat
wrapRepP repN fty repP = case fty of
  Directory _ -> repP
  otherwise   -> ConP repN [repP]



writeE' :: (ForestTy, TH.Exp, TH.Exp, TH.Exp) -> Q TH.Exp
writeE' (forestTy, repE, mdE, manE) = case forestTy of
  File fTy -> writeFileManifest fTy repE mdE manE
  Directory dTy -> writeDirManifest dTy repE mdE manE
  Named f_name -> return (AppE (AppE  (VarE (getWriteManifestName f_name))  (TupE [repE, mdE])) manE)
  SymLink -> return (AppE (AppE (VarE 'updateManifestWithLink) (AppE (VarE 'fst) mdE)) manE)
  FConstraint _ ty _ -> writeE' (ty, repE, mdE, manE)
  Fapp ty argE -> writeAppManifest ty argE repE mdE manE
  FMaybe ty -> writeMaybeManifest ty repE mdE manE
  FComp comp -> writeListManifest comp repE mdE manE
  Gzip fty -> writeGzipManifest fty repE mdE manE
  Tar fty -> writeTarManifest fty repE mdE manE
--  otherwise -> return (VarE 'undefined)

writeTarManifest :: ForestTy -> TH.Exp -> TH.Exp -> TH.Exp -> Q TH.Exp
writeTarManifest fty repE mdE manE = do
  { (irepE, irepP) <- doGenPE "rep"
  ; (imdE,  imdP)  <- doGenPE "md"
  ; (imanE, imanP) <- doGenPE "manifest"
  ; writeItemE <- writeE' (fty, irepE, imdE, imanE)
  ; let itemFnE = LamE [TupP[irepP,imdP], imanP] writeItemE
  ; return (AppE (AppE (AppE (AppE (VarE 'doWriteTarManifest) repE) mdE) itemFnE) manE)
  }


doWriteTarManifest rep (md @ (fmd,base)) doItem manifest = do 
  { freshManifest <- newManifest
  ; rawContentManifest <- doItem (rep,md) freshManifest
  ; contentManifest <- validateManifest rawContentManifest
  ; let tarStatus = collectManifestErrors contentManifest
  ; print "Finished generating manifest for tar"
  ; let localDirToTarName = takeBaseName(dropExtensions (fullpath (fileInfo fmd)))
  ; print ("name of directory to tar: " ++ localDirToTarName)
  ; let tarName = addExtension  localDirToTarName "tar"
  ; scratchDir <- getTempForestScratchDirectory
  ; let dirToTarName = combine scratchDir localDirToTarName
  ; createDirectoryIfMissing True dirToTarName
  ; canonScratchDir <- System.Directory.canonicalizePath scratchDir
  ; let clipPath = getClipPathFromTarManifest contentManifest canonScratchDir
  ; storeManifestAt' dirToTarName clipPath contentManifest
--  ; storeManifestAt dirToTarName contentManifest
  ; oldCurDir <- getCurrentDirectory
  ; setCurrentDirectory scratchDir
  ; doShellCmd ("tar -cvf " ++ tarName  ++ " " ++ localDirToTarName )
  ; doShellCmd ("mv " ++ tarName  ++ " " ++ (tempDir manifest) )
  ; setCurrentDirectory oldCurDir
  ; updateManifestWithTar tarName tarStatus fmd manifest   --XXX: need to pass errors in temp manifest on to higher-level manifest
  }


writeGzipManifest :: ForestTy -> TH.Exp -> TH.Exp -> TH.Exp -> Q TH.Exp
writeGzipManifest fty repE mdE manE = do
  { (irepE, irepP) <- doGenPE "rep"
  ; (imdE,  imdP)  <- doGenPE "md"
  ; (imanE, imanP) <- doGenPE "manifest"
  ; writeItemE <- writeE' (fty, irepE, imdE, imanE)
  ; let itemFnE = LamE [TupP[irepP,imdP], imanP] writeItemE
  ; return (AppE (AppE (AppE (AppE (VarE 'doWriteGzipManifest) repE) mdE) itemFnE) manE)
  }

doWriteGzipManifest rep (fmd,base) doItem manifest = do 
  { let fmd' = removeGzipSuffix fmd
  ; manifest' <- doItem (rep,(fmd',base)) manifest
  ; gzipManifestEntry fmd manifest'
  }

gSnd expE = AppE (VarE 'snd) expE
gFst expE = AppE (VarE 'fst) expE

writeListManifest :: CompField -> TH.Exp -> TH.Exp -> TH.Exp -> Q TH.Exp 
writeListManifest comp repE mdE manE = do
  { let fmdE = gFst mdE 
  ; let fileP = generatorP comp
  ; (fp_repEs, fp_mdEs) <- case tyConNameOpt comp of
        Nothing -> return (repE, gSnd mdE)
        Just str -> do { arity <- getTyConArity str
                       ; if arity == 1 then 
                              return (AppE (VarE 'toList1) repE,  AppE (VarE 'toList1) (gSnd mdE))
                         else 
                              return (AppE (VarE 'toList2) repE,  AppE (VarE 'toList2) (gSnd mdE))
                       }
  ; (irepE, irepP) <- doGenPE "rep"
  ; (imdE,  imdP)  <- doGenPE "md"
  ; (imanE, imanP) <- doGenPE "manifest"
  ; writeItemE <- writeE' (descTy comp, irepE, imdE, imanE)
  ; let getPathE = AppE (VarE 'takeFileName) (AppE (VarE 'get_fullpath) imdE)
  ; let writeItemBodyE = LetE[ ValD fileP (NormalB  getPathE ) []] writeItemE
  ; let itemFnE = LamE [TupP[irepP,imdP], imanP] writeItemBodyE
  ; return (AppE (AppE (AppE (AppE (AppE (VarE 'doWriteList) fmdE) fp_repEs) fp_mdEs) itemFnE) manE)
  }

--doWriteList :: Forest rep md  => 
--    Forest_md -> [(FilePath,rep)] -> [(FilePath,md)] ->  ((rep, md) -> Manifest -> IO Manifest ) -> Manifest -> IO Manifest
doWriteList fmd fp_reps fp_mds doItem manifest = do
   { let (files, reps) = List.unzip fp_reps
   ; let mds = snd (List.unzip fp_mds)
   ; let rep_mds = List.zip reps mds
   ; manifest' <- updateManifestWithComp fmd files manifest
   ; foldM (\manifest'' rm -> doItem rm manifest'')  manifest' rep_mds
   }


writeMaybeManifest :: ForestTy -> TH.Exp -> TH.Exp -> TH.Exp -> Q TH.Exp
writeMaybeManifest ty repE mdE manE = do 
  (jrepE, jrepP) <- doGenPE "rep"
  (jmdE,  jmdP)  <- doGenPE "md"
  (freshFmdE, freshFmdP) <- doGenPE "fmd"
  justE <- writeE' (ty, jrepE, jmdE, manE)
  let justB = NormalB justE
  let jrepPat = ConP 'Just [jrepP]
  let jmdPat =  TupP[freshFmdP, ConP 'Just [jmdP]]
  let justPat = TupP [jrepPat, jmdPat]
  let justmatch = Match justPat justB []
  let nrepPat = ConP 'Nothing []
  let nmdPat =  TupP[freshFmdP, ConP 'Nothing []]
  let nPat = TupP [nrepPat, nmdPat]
  let nmatchE = AppE (AppE (VarE 'updateManifestWithNone) freshFmdE) manE
  let nmatch = Match nPat (NormalB nmatchE) []
  let caseE = CaseE(TupE[repE,mdE]) [justmatch,nmatch]
  return caseE

writeAppManifest :: ForestTy -> TH.Exp -> TH.Exp -> TH.Exp -> TH.Exp -> Q TH.Exp
writeAppManifest ty argE repE mdE manE = case ty of
  Named f_name   -> return (AppE (AppE (AppE  (VarE (getWriteManifestName f_name))  argE) (TupE [repE, mdE])) manE)


writeDirManifest :: DirectoryTy -> TH.Exp -> TH.Exp -> TH.Exp -> Q TH.Exp
writeDirManifest (Record fty_name  fields) repE mdE manE = do
  { let (repEs, repPs) = getPEforFields getBranchNameL fields
  ; let (mdEs,   mdPs)  = getPEforFields getBranchMDNameL fields
  ; (freshFmdE, freshFmdP) <- doGenPE "fmd"
  ; (symManE, symManP) <- doGenPE "manifest_sym"
  ; let mdPat  = TupP[freshFmdP, RecP (getStructInnerMDName (mkName fty_name)) mdPs]
  ; let repPat = RecP (mkName fty_name) repPs
  ; let casePat = TupP [repPat, mdPat]
-- calculate the body
  ; let symCheckE = AppE (AppE (VarE 'updateManifestWithDir) freshFmdE) manE
  ; let checkS = BindS symManP symCheckE
  ; (freshManE, freshManP) <- doGenPE "manifest"
  ; let fields_rep_mds_manifest = List.zip4 fields repEs mdEs (repeat freshManE)
  ; expE <- mapM (writeField freshManP freshFmdE) fields_rep_mds_manifest
  ; let printItemsE = ListE expE
  ; let doFieldsS = NoBindS (AppE (AppE (VarE 'doWriteFields) symManE) printItemsE)
  ; let caseBody = NormalB (DoE [checkS, doFieldsS])
  ; let match = Match casePat caseBody []
  ; let caseE = CaseE (TupE [repE, mdE]) [match]
  ; return caseE
  }


doWriteFields  :: Monad m => a -> [a -> m a] -> m a
doWriteFields = foldM (\manifest' doItem -> doItem manifest') 

writeField :: TH.Pat -> TH.Exp -> (Field, TH.Exp, TH.Exp, TH.Exp) -> Q TH.Exp
writeField manP dirfmdE (field, repE, mdE, manE) = do
  { bodyE <- case field of
             Simple (_,_,_,fTy,_) ->  writeE' (fTy, repE, mdE, manE)
             Comp comp            ->  writeListManifest comp repE (TupE [dirfmdE, mdE]) manE  
  ; return (LamE [manP] bodyE)  
  }



getPEforFields :: (String -> Name) -> [Field] ->  ([TH.Exp], [TH.FieldPat])
getPEforFields mkFieldNm fields = 
  let eps =  List.map (getPEforField mkFieldNm) fields
      (es, fps) = List.unzip eps
  in (es, fps)

getPEforField :: (String -> Name) -> Field -> (TH.Exp, TH.FieldPat)
getPEforField mkFieldNm field = 
  let fieldStr = case field of
                   Simple (name, _, _, _, _) -> name
                   Comp r -> internalName r
      (varE, varP) = genPE (mkFieldNm fieldStr)
  in  (varE, (mkFieldNm fieldStr, varP))

writeFileManifest :: (String, Maybe TH.Exp) -> TH.Exp -> TH.Exp -> TH.Exp -> Q TH.Exp
writeFileManifest (pty_name, optArg) repE mdE manE = do
 { let funE = case optArg of 
               Nothing   -> VarE 'updateManifestPads
               Just argE -> AppE (VarE 'updateManifestPads1) argE
 ; let resultE = AppE (AppE funE (TupE [repE, mdE])) manE
 ; return resultE
 }

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
--  Named f_name   -> return (AppE (VarE (getLoadName f_name)) pathE)
  Named f_name   -> return (AppE (VarE 'load) pathE)
  File (file_name, argEOpt) -> case argEOpt of 
                                Nothing ->     return (AppE (VarE 'fileLoad) pathE)
                                Just argE ->   return (AppE (AppE (VarE 'fileLoad1) argE) pathE)
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
  ; let predFnE = modPredE pat predE
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
   let newPathE     = AppE (AppE (VarE 'concatPath)  pathE ) externalE
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

modPredE externalP predE = case externalP of
  VarP name -> let md_name = mkName ((nameBase name) ++"_md")  
                   attP    = VarP (mkName ((nameBase name) ++"_att"))
                   pat     = TildeP (TupP[VarP name, VarP md_name])
                   initE   = AppE (VarE 'get_fmd_header) (VarE md_name)
                   bodyE   = LetE [ValD attP (NormalB initE) []] predE      -- let name_att = fileInfo name_md in predE
               in LamE [pat] bodyE
                 
  otherwise -> error "Forest: Couldn't convert constraint pattern to pattern for meta data.  Use simple pattern."



emptyMap :: Map.Map a b
emptyMap = Map.empty



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
                  Nothing -> E.ForestPredicateFailure
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
genRepMDName = genUniqueName "rep_md" 
genManName = genUniqueName "manifest" 
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
getWriteManifestName pname = mkName ((strToLower pname) ++ "_updateManifest")
getGenManifestName pname = mkName ((strToLower pname) ++ "_generateManifest")

tyListToTupleTy (ty:tys) = foldl AppT (AppT (TupleT (1 + length tys) ) ty) tys
mk_newTyD ty_name ty = NewtypeD [] ty_name [] con derives
    where con = NormalC ty_name [(NotStrict,ty)]           -- How should we determine whether a type should be Strict or not?
          derives = (map mkName ["Show", "Eq"]) ++  [''Typeable, ''Data, ''Ord]

mk_TySynD ty_name ty = TySynD ty_name [] ty
