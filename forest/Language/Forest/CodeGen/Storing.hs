{-# LANGUAGE TupleSections, TemplateHaskell, NamedFieldPuns, ScopedTypeVariables, RecordWildCards, FlexibleInstances, MultiParamTypeClasses,
    UndecidableInstances, ViewPatterns  #-}

module Language.Forest.CodeGen.Storing where

import Language.Forest.CodeGen.Utils
import Control.Monad.Trans
import Control.Monad.Incremental

import Language.Forest.Syntax as PS
import Language.Forest.MetaData
import Language.Forest.Errors
import Language.Forest.Generic
import qualified Language.Forest.Errors as E
import Language.Forest.Manifest
import Language.Forest.FS.FSDelta
import System.Directory
import System.FilePath.Posix
import Control.Monad.Reader (Reader(..),ReaderT(..))
import qualified Control.Monad.Reader as Reader

import Language.Haskell.TH as TH
import Language.Haskell.TH.Syntax hiding (lift)
import Language.Pads.Padsc hiding (lift)
import Language.Pads.TH
import Language.Forest.IO
import Language.Forest.TH
import Language.Forest.FS.FSRep

import Data.Data
import Data.Maybe
import Data.Char
import Data.List
import qualified Data.Map as Map
import qualified Data.List as List
import qualified Control.Exception as CE
import Control.Monad
import Data.Set (Set(..))
import qualified Data.Set as Set
import Data.Map (Map(..))
import qualified Data.Map as Map
import Control.Monad.State (State(..),StateT(..))
import qualified Control.Monad.State as State

--writeFileManifest :: (String, Maybe TH.Exp) -> TH.Exp -> TH.Exp -> TH.Exp -> Q TH.Exp
--writeFileManifest (pty_name, optArg) repE mdE manE = do
-- { let funE = case optArg of 
--               Nothing   -> VarE 'updateManifestPads
--               Just argE -> AppE (VarE 'updateManifestPads1) argE
-- ; let resultE = AppE (AppE funE (TupE [repE, mdE])) manE
-- ; return resultE
-- }

--genGenManifest :: Name -> Name -> Name -> Name -> Maybe (TH.Pat, TH.Type) -> Q[Dec]
--genGenManifest genMName wn rep_name md_name mpat_info = do 
--  { let core_ty = arrowTy (AppT (AppT (TupleT 2) (ConT rep_name)) (ConT md_name)) 
--                      (AppT (ConT ''IO) (ConT (mkName "Manifest")))
--  ; (argE, argP) <- doGenPE "arg"
--  ; (bodyE,ty) <- case mpat_info of
--        Nothing -> do 
--            { bodyE <- genGenManE (VarE 'load) (VarE wn)
--            ; return (bodyE, core_ty)
--            }
--        Just(pat,pat_ty) -> do
--            { core_bodyE <- genGenManE (AppE (VarE 'load1) argE) (AppE (VarE wn) argE)
--            ; return (LamE [argP] core_bodyE,
--                      arrowTy pat_ty core_ty)
--            }
--  ; let sigD = SigD genMName ty
--  ; let funD = ValD (VarP genMName) (NormalB bodyE) []
--  ; return [sigD, funD]
--  }
--
--genGenManE :: TH.Exp -> TH.Exp -> Q TH.Exp
--genGenManE loadE updateManE  = do
--  { (man0E,man0P) <- doGenPE "manifest"
--  ; (man1E,man1P) <- doGenPE "manifest"
--  ; (man2E,man2P) <- doGenPE "manifest"
--  ; (forestE,forestP) <- doGenPE "forest"
--  ; let newManifestS = BindS man0P (VarE 'newManifest)
--  ; let rawManifestS = BindS man1P (AppE (AppE updateManE forestE) man0E ) 
--  ; let medManifestS = BindS man2P (AppE (VarE 'validateManifest) man1E ) 
--  ; let finalS = NoBindS (AppE (AppE (AppE (VarE 'validateLists) loadE) updateManE) man2E)
--  ; return (LamE [forestP] (DoE [newManifestS,rawManifestS,medManifestS,finalS]))
--  }
--
--genWriteManifest :: Name -> Name ->  Name -> ForestTy -> Maybe (TH.Pat, TH.Type) -> Q [Dec]
--genWriteManifest    funName rep_name md_name forestTy mpat_info = do 
--   core_bodyE <- writeE rep_name forestTy
--   let core_ty = arrowTy (AppT (AppT (TupleT 2) (ConT rep_name)) (ConT md_name)) 
--                (arrowTy (ConT (mkName "Manifest")) 
--                (AppT (ConT ''IO) (ConT (mkName "Manifest"))))
--   let (bodyE,ty) = case mpat_info of
--                     Nothing -> (core_bodyE, core_ty)
--                     Just (pat,pat_ty) -> ( LamE [pat] core_bodyE,
--                                            arrowTy pat_ty core_ty)
--   let sigD = SigD funName ty
--   let funD = ValD (VarP funName) (NormalB bodyE) []
--   return [sigD, funD]
--
--writeE :: Name -> ForestTy -> Q TH.Exp
--writeE repN ty = do
--   repName  <- genRepName 
--   mdName   <- genMdName
--   (repE, repP)   <- doGenPE "rep"
--   (mdE,  mdP)    <- doGenPE "md"
--   (man1E, man1P) <- doGenPE "manifest"
--   (man2E, man2P) <- doGenPE "manifest"
--   let frepP       = wrapRepP repN ty repP 
--   let setRootE = AppE (AppE (VarE 'setManifestRoot) mdE) man1E
--   rhsE        <- writeE' (ty, repE, mdE, man2E)
----   let letE = LetE [ValD man2P (NormalB setRootE) []] rhsE
--   let setRootS = BindS man2P setRootE
--   let bodyE = DoE [setRootS, NoBindS rhsE]
--   let writeFun = LamE [TupP [frepP, mdP]] (LamE [man1P] bodyE)
--   return writeFun

{-
do { man2P <- setManifestRoot mdE man1E
   ; rhsE
   }
-}

--writeE' :: (ForestTy, TH.Exp, TH.Exp, TH.Exp) -> Q TH.Exp
--writeE' (forestTy, repE, mdE, manE) = case forestTy of
--  File fTy -> writeFileManifest fTy repE mdE manE
--  Directory dTy -> writeDirManifest dTy repE mdE manE
--  Named f_name -> return (AppE (AppE  (VarE (getWriteManifestName f_name))  (TupE [repE, mdE])) manE)
--  SymLink -> return (AppE (AppE (VarE 'updateManifestWithLink) (AppE (VarE 'fst) mdE)) manE)
--  FConstraint _ ty _ -> writeE' (ty, repE, mdE, manE)
--  Fapp ty argE -> writeAppManifest ty argE repE mdE manE
--  FMaybe ty -> writeMaybeManifest ty repE mdE manE
--  FComp comp -> writeListManifest comp repE mdE manE
--  Gzip fty -> writeGzipManifest fty repE mdE manE
--  Tar fty -> writeTarManifest fty repE mdE manE

--writeTarManifest :: ForestTy -> TH.Exp -> TH.Exp -> TH.Exp -> Q TH.Exp
--writeTarManifest fty repE mdE manE = do
--	(irepE, irepP) <- doGenPE "rep"
--	(imdE,  imdP)  <- doGenPE "md"
--	(imanE, imanP) <- doGenPE "manifest"
--	writeItemE <- writeE' (fty, irepE, imdE, imanE)
--	let itemFnE = LamE [TupP[irepP,imdP], imanP] writeItemE
--	return (AppE (AppE (AppE (AppE (VarE 'doWriteTarManifest) repE) mdE) itemFnE) manE)
--
--doWriteTarManifest rep (md @ (fmd,base)) doItem manifest = do 
--	freshManifest <- newManifest
--	rawContentManifest <- doItem (rep,md) freshManifest
--	contentManifest <- validateManifest rawContentManifest
--	let tarStatus = collectManifestErrors contentManifest
--	print "Finished generating manifest for tar"
--	let localDirToTarName = takeBaseName(dropExtensions (fullpath (fileInfo fmd)))
--	print ("name of directory to tar: " ++ localDirToTarName)
--	let tarName = addExtension  localDirToTarName "tar"
--	scratchDir <- getTempForestScratchDirectory
--	let dirToTarName = combine scratchDir localDirToTarName
--	createDirectoryIfMissing True dirToTarName
--	canonScratchDir <- System.Directory.canonicalizePath scratchDir
--	let clipPath = getClipPathFromTarManifest contentManifest canonScratchDir
--	storeManifestAt' dirToTarName clipPath contentManifest
--	oldCurDir <- getCurrentDirectory
--	setCurrentDirectory scratchDir
--	doShellCmd ("tar -cvf " ++ tarName  ++ " " ++ localDirToTarName )
--	doShellCmd ("mv " ++ tarName  ++ " " ++ (tempDir manifest) )
--	setCurrentDirectory oldCurDir
--	updateManifestWithTar tarName tarStatus fmd manifest   --XXX: need to pass errors in temp manifest on to higher-level manifest

--writeGzipManifest :: ForestTy -> TH.Exp -> TH.Exp -> TH.Exp -> Q TH.Exp
--writeGzipManifest fty repE mdE manE = do
--	(irepE, irepP) <- doGenPE "rep"
--	(imdE,  imdP)  <- doGenPE "md"
--	(imanE, imanP) <- doGenPE "manifest"
--	writeItemE <- writeE' (fty, irepE, imdE, imanE)
--	let itemFnE = LamE [TupP[irepP,imdP], imanP] writeItemE
--	return (AppE (AppE (AppE (AppE (VarE 'doWriteGzipManifest) repE) mdE) itemFnE) manE)
--
--doWriteGzipManifest rep (fmd,base) doItem manifest = do 
--	let fmd' = removeGzipSuffix fmd
--	manifest' <- doItem (rep,(fmd',base)) manifest
--	gzipManifestEntry fmd manifest'
--	
--gSnd expE = AppE (VarE 'snd) expE
--gFst expE = AppE (VarE 'fst) expE

--writeListManifest :: CompField -> TH.Exp -> TH.Exp -> TH.Exp -> Q TH.Exp 
--writeListManifest comp repE mdE manE = do
--	let fmdE = gFst mdE 
--	let fileP = generatorP comp
--	(fp_repEs, fp_mdEs) <- case tyConNameOpt comp of
--	    Nothing -> return (repE, gSnd mdE)
--	    Just str -> do { arity <- getTyConArity str
--	                   ; if arity == 1 then 
--	                          return (AppE (VarE 'toList1) repE,  AppE (VarE 'toList1) (gSnd mdE))
--	                     else 
--	                          return (AppE (VarE 'toList2) repE,  AppE (VarE 'toList2) (gSnd mdE))
--	                   }
--	(irepE, irepP) <- doGenPE "rep"
--	(imdE,  imdP)  <- doGenPE "md"
--	(imanE, imanP) <- doGenPE "manifest"
--	writeItemE <- writeE' (descTy comp, irepE, imdE, imanE)
--	let getPathE = AppE (VarE 'takeFileName) (AppE (VarE 'get_fullpath) imdE)
--	let writeItemBodyE = LetE[ ValD fileP (NormalB  getPathE ) []] writeItemE
--	let itemFnE = LamE [TupP[irepP,imdP], imanP] writeItemBodyE
--	return (AppE (AppE (AppE (AppE (AppE (VarE 'doWriteList) fmdE) fp_repEs) fp_mdEs) itemFnE) manE)
--	
----doWriteList :: Forest rep md  => 
----    Forest_md -> [(FilePath,rep)] -> [(FilePath,md)] ->  ((rep, md) -> Manifest -> IO Manifest ) -> Manifest -> IO Manifest
--doWriteList fmd fp_reps fp_mds doItem manifest = do
--	let (files, reps) = List.unzip fp_reps
--	let mds = snd (List.unzip fp_mds)
--	let rep_mds = List.zip reps mds
--	manifest' <- updateManifestWithComp fmd files manifest
--	foldM (\manifest'' rm -> doItem rm manifest'')  manifest' rep_mds

--writeMaybeManifest :: ForestTy -> TH.Exp -> TH.Exp -> TH.Exp -> Q TH.Exp
--writeMaybeManifest ty repE mdE manE = do 
--  (jrepE, jrepP) <- doGenPE "rep"
--  (jmdE,  jmdP)  <- doGenPE "md"
--  (freshFmdE, freshFmdP) <- doGenPE "fmd"
--  justE <- writeE' (ty, jrepE, jmdE, manE)
--  let justB = NormalB justE
--  let jrepPat = ConP 'Just [jrepP]
--  let jmdPat =  TupP[freshFmdP, ConP 'Just [jmdP]]
--  let justPat = TupP [jrepPat, jmdPat]
--  let justmatch = Match justPat justB []
--  let nrepPat = ConP 'Nothing []
--  let nmdPat =  TupP[freshFmdP, ConP 'Nothing []]
--  let nPat = TupP [nrepPat, nmdPat]
--  let nmatchE = AppE (AppE (VarE 'updateManifestWithNone) freshFmdE) manE
--  let nmatch = Match nPat (NormalB nmatchE) []
--  let caseE = CaseE(TupE[repE,mdE]) [justmatch,nmatch]
--  return caseE

--writeAppManifest :: ForestTy -> TH.Exp -> TH.Exp -> TH.Exp -> TH.Exp -> Q TH.Exp
--writeAppManifest ty argE repE mdE manE = case ty of
--  Named f_name   -> return (AppE (AppE (AppE  (VarE (getWriteManifestName f_name))  argE) (TupE [repE, mdE])) manE)


--writeDirManifest :: DirectoryTy -> TH.Exp -> TH.Exp -> TH.Exp -> Q TH.Exp
--writeDirManifest (Record fty_name  fields) repE mdE manE = do
--  { let (repEs, repPs) = getPEforFields getBranchNameL fields
--  ; let (mdEs,   mdPs)  = getPEforFields getBranchMDNameL fields
--  ; (freshFmdE, freshFmdP) <- doGenPE "fmd"
--  ; (symManE, symManP) <- doGenPE "manifest_sym"
--  ; let mdPat  = TupP[freshFmdP, RecP (getStructInnerMDName (mkName fty_name)) mdPs]
--  ; let repPat = RecP (mkName fty_name) repPs
--  ; let casePat = TupP [repPat, mdPat]
---- calculate the body
--  ; let symCheckE = AppE (AppE (VarE 'updateManifestWithDir) freshFmdE) manE
--  ; let checkS = BindS symManP symCheckE
--  ; (freshManE, freshManP) <- doGenPE "manifest"
--  ; let fields_rep_mds_manifest = List.zip4 fields repEs mdEs (repeat freshManE)
--  ; expE <- mapM (writeField freshManP freshFmdE) fields_rep_mds_manifest
--  ; let printItemsE = ListE expE
--  ; let doFieldsS = NoBindS (AppE (AppE (VarE 'doWriteFields) symManE) printItemsE)
--  ; let caseBody = NormalB (DoE [checkS, doFieldsS])
--  ; let match = Match casePat caseBody []
--  ; let caseE = CaseE (TupE [repE, mdE]) [match]
--  ; return caseE
--  }


--doWriteFields  :: Monad m => a -> [a -> m a] -> m a
--doWriteFields = foldM (\manifest' doItem -> doItem manifest') 
--
--writeField :: TH.Pat -> TH.Exp -> (Field, TH.Exp, TH.Exp, TH.Exp) -> Q TH.Exp
--writeField manP dirfmdE (field, repE, mdE, manE) = do
--  { bodyE <- case field of
--             Simple (_,_,_,fTy,_) ->  writeE' (fTy, repE, mdE, manE)
--             Comp comp            ->  writeListManifest comp repE (TupE [dirfmdE, mdE]) manE  
--  ; return (LamE [manP] bodyE)  
--  }
