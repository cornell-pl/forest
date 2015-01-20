{-# LANGUAGE TupleSections, TemplateHaskell, NamedFieldPuns, ScopedTypeVariables, RecordWildCards, FlexibleInstances, MultiParamTypeClasses,
    UndecidableInstances, ViewPatterns  #-}

module Language.Forest.IC.CodeGen.ZStoring where

import Language.Forest.IC.CodeGen.Utils
import Control.Monad.Trans
import Control.Monad.Incremental

import Language.Forest.IC.CodeGen.Default
import Language.Forest.IC.IO.Storing
import Language.Forest.IC.IO.ZStoring
import Language.Forest.Syntax as PS
import Language.Forest.IC.MetaData
import Language.Forest.Pure.MetaData (FileInfo(..),FileType(..),(:*:)(..),Arg(..))
import qualified Language.Forest.Pure.MetaData as Pure
import qualified Language.Forest.Pure.CodeGen.Utils as Pure
import Language.Forest.Errors
import Language.Forest.IC.Generic
import qualified Language.Forest.Errors as E
import Language.Forest.Manifest
import Language.Forest.IC.FS.FSDelta
import System.Directory
import System.FilePath.Posix
import Control.Monad.Reader (Reader(..),ReaderT(..))
import qualified Control.Monad.Reader as Reader

import Language.Haskell.TH as TH
import Language.Haskell.TH.Syntax hiding (lift)
import Language.Pads.Padsc hiding (lift)
import Language.Pads.TH
import Language.Forest.IO.Utils
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
import {-# SOURCE #-} Language.Forest.IC.CodeGen.DeltaLoading
import {-# SOURCE #-} Language.Forest.IC.CodeGen.ZDeltaLoading
import Language.Forest.IC.CodeGen.Loading
import Language.Forest.IC.CodeGen.ZLoading
import Language.Haskell.TH.Quote

genZManifestM :: Name -> ForestTy -> [(TH.Pat,TH.Type)] -> ZEnvQ TH.Exp
genZManifestM rep_name forestTy pat_infos = do
	fsName <- lift $ newName "fs"
	treeName    <- lift $ newName "tree"
	dtaName    <- lift $ newName "dta"
	manName    <- lift $ newName "man"
	let (treeE, treeP) = genPE treeName
	let (dtaE, dtaP) = genPE dtaName
	let (fsE, fsP) = genPE fsName
	let (manE,manP) = genPE manName
	
	case pat_infos of
		[] -> do
			core_bodyE <- genZManifestBody treeE dtaE manE rep_name forestTy
			return $ LamE [TupP [],treeP,dtaP,manP] core_bodyE
		otherwise -> do
			(targsP,argThunkNames) <- genZManifestArgsE (zip [1..] pat_infos) forestTy
			let argsT = Pure.forestTupleTy $ map (AppT (ConT ''Arg) . snd) pat_infos
			let proxyArgs = SigE (ConE 'Proxy) $ AppT (ConT ''Proxy) argsT
			let update (fs,env) = (fs,foldl updatePat env (zip argThunkNames pat_infos))
				where updatePat env (thunkName,(pat,ty)) = Map.fromSet (\var -> Just (thunkName,pat)) (patPVars pat) `Map.union` env --left-biased union
			(fs,_) <- Reader.ask
			Reader.local update $ do -- adds pattern variable deltas to the env.
				margsName <- lift $ newName "margs"
				newdtaName <- lift $ newName "newdta"
				newmanName <- lift $ newName "newman"
				let (margsE,margsP) = genPE margsName
				let (newmanE,newmanP) = genPE newmanName
				let (newdtaE,newdtaP) = genPE newdtaName
				core_bodyE <- genZManifestBody treeE newdtaE newmanE rep_name forestTy
				return $ LamE [margsP,treeP,VarP dtaName,manP] $ DoE [
					BindS targsP $ AppE (VarE 'inside) $ Pure.appE3 (VarE 'newArgs) (proxyN fs) proxyArgs margsE,
					NoBindS $ Pure.appE5 (VarE 'doZManifestArgs) proxyArgs margsE dtaE (LamE [newdtaP,newmanP] core_bodyE) manE]

genZManifestBody :: TH.Exp -> TH.Exp -> TH.Exp -> Name -> ForestTy -> ZEnvQ TH.Exp
genZManifestBody treeE dtaE manE repN ty = case ty of 
	Directory _ -> zmanifestE ty treeE dtaE manE
	FConstraint _ (Directory _) _ -> zmanifestE ty treeE dtaE manE
	otherwise   -> do -- Decompose the representation type constructor
		let dtaE' = AppE (VarE $ Pure.getUnTyName $ nameBase repN) dtaE
		zmanifestE ty treeE dtaE' manE

-- loads top-level arguments into the environment
genZManifestArgsE :: [(Int,(TH.Pat,TH.Type))] -> ForestTy -> ZEnvQ (Pat,[Name])
genZManifestArgsE [pat_info] forestTy = genZManifestArgE pat_info forestTy
genZManifestArgsE (pat_info:pat_infos) forestTy = do
	(pat,names1) <- genZManifestArgE pat_info forestTy
	(pats,names2) <- genZManifestArgsE pat_infos forestTy
	return (ConP '(:*:) [pat,pats],names1++names2)

genZManifestArgE :: (Int,(TH.Pat,TH.Type)) -> ForestTy -> ZEnvQ (Pat,[Name])
genZManifestArgE (i,(pat,pat_ty)) forestTy = do
	thunkName <- lift $ newName $ "t"++show i
	return (VarP thunkName,[thunkName])

zmanifestE :: ForestTy -> Exp -> Exp -> Exp -> ZEnvQ Exp
zmanifestE ty treeE dtaE manE = case ty of
	Named f_name               -> zmanifestWithArgsE [] treeE dtaE manE
	Fapp (Named f_name) argEs  -> zmanifestWithArgsE argEs treeE dtaE manE
	File (file_name, argEOpt) -> zmanifestFile file_name argEOpt treeE dtaE manE
	Archive archtype ty         -> zmanifestArchive archtype ty treeE dtaE manE
	FSymLink         -> zmanifestSymLink treeE dtaE manE
	FConstraint p ty pred -> zmanifestConstraint treeE p pred dtaE manE $ zmanifestE ty treeE
	Directory dirTy -> zmanifestDirectory dirTy treeE dtaE manE
	FMaybe forestTy -> zmanifestMaybe forestTy treeE dtaE manE
	FComp cinfo     -> zmanifestComp cinfo treeE dtaE manE

-- they are terminals in the spec
zmanifestWithArgsE :: [TH.Exp] -> TH.Exp -> TH.Exp -> TH.Exp -> ZEnvQ TH.Exp
zmanifestWithArgsE [] treeE dtaE manE = return $ Pure.appE4 (VarE 'zupdateManifestScratch) (TupE []) treeE dtaE manE
zmanifestWithArgsE argsE treeE dtaE manE = do
	argsE <- mapM (\e -> forceVarsZEnvQ e return) argsE
	let tupArgsE = foldl1' (Pure.appE2 (ConE '(:*:))) argsE
	return $ Pure.appE4 (VarE 'zupdateManifestScratch) tupArgsE treeE dtaE manE

zmanifestArchive :: [ArchiveType] -> ForestTy -> TH.Exp -> TH.Exp -> TH.Exp -> ZEnvQ TH.Exp
zmanifestArchive archtype ty treeE dtaE manE = do
	newTreeName <- lift $ newName "new_tree"
	let (newTreeE, newTreeP) = genPE newTreeName
	newmanName <- lift $ newName "man"
	let (newmanE,newmanP) = genPE newmanName
	newdtaName <- lift $ newName "dta"
	let (newdtaE,newdtaP) = genPE newdtaName
	
	manifestContentsE <- liftM (LamE [newTreeP,newdtaP,newmanP]) $ zmanifestE ty newTreeE newdtaE newmanE
	exts <- lift $ dataToExpQ (\_ -> Nothing) archtype
	return $ Pure.appE5 (VarE 'doZManifestArchive) exts treeE dtaE manifestContentsE manE

zmanifestSymLink :: TH.Exp -> TH.Exp -> TH.Exp -> ZEnvQ TH.Exp
zmanifestSymLink treeE dtaE manE = do
  return $ Pure.appE3 (VarE 'doZManifestSymLink) treeE dtaE manE

zmanifestConstraint :: TH.Exp -> TH.Pat -> TH.Exp -> TH.Exp -> TH.Exp -> (Exp -> Exp -> ZEnvQ TH.Exp) -> ZEnvQ TH.Exp
zmanifestConstraint treeE pat predE dtaE manE manifest = forceVarsZEnvQ predE $ \predE' -> do
	newmanName <- lift $ newName "man"
	let (newmanE,newmanP) = genPE newmanName
	newdtaName <- lift $ newName "dta"
	let (newdtaE,newdtaP) = genPE newdtaName
	
	let predFnE = zmodPredE pat predE'
	manifestAction <- liftM (LamE [newdtaP,newmanP]) $ manifest newdtaE newmanE
	return $ Pure.appE5 (VarE 'doZManifestConstraint) treeE predFnE dtaE manifestAction manE

zmanifestFile :: String -> Maybe TH.Exp -> TH.Exp -> TH.Exp -> TH.Exp -> ZEnvQ TH.Exp
zmanifestFile fileName Nothing treeE dtaE manE = do
	return $ Pure.appE3 (VarE 'doZManifestFile) treeE dtaE manE
zmanifestFile fileName (Just argE) treeE dtaE manE = do
	return $ Pure.appE4 (VarE 'doZManifestFile1) argE treeE dtaE manE

zmanifestMaybe :: ForestTy -> TH.Exp -> TH.Exp -> TH.Exp -> ZEnvQ TH.Exp
zmanifestMaybe ty treeE dtaE manE = do 
	newdtaName <- lift $ newName "newdta"
	newmanName <- lift $ newName "newman"
	newrepName <- lift $ newName "newrep"
	newpathName <- lift $ newName "newpath"
	let (newdtaE,newdtaP) = genPE newdtaName
	let (newmanE,newmanP) = genPE newmanName
	let (newrepE,newrepP) = genPE newrepName
	let (newpathE,newpathP) = genPE newpathName
	doContentsE <- liftM (LamE [newdtaP,newmanP]) $ zmanifestE ty treeE newdtaE newmanE
	return $ Pure.appE4 (VarE 'doZManifestMaybe) treeE dtaE doContentsE manE

zmanifestDirectory :: DirectoryTy -> TH.Exp -> TH.Exp -> TH.Exp -> ZEnvQ TH.Exp
zmanifestDirectory dirTy@(Record id fields) treeE dtaE manE = do
	newdtaName <- lift $ newName "newdta"
	newmanName <- lift $ newName "newman"
	pathName <- lift $ newName "path"
	let (newdtaE,newdtaP) = genPE newdtaName
	let (newmanE,newmanP) = genPE newmanName
	let (pathE,pathP) = genPE pathName
	doDirE <- liftM (LamE [pathP,newdtaP,newmanP]) $ zmanifestDirectoryContents dirTy treeE pathE newdtaE newmanE
	collectMDs <- lift $ zgenMergeFieldsMDErrors fields	
	return $ Pure.appE5 (VarE 'doZManifestDirectory) treeE collectMDs dtaE doDirE manE

zmanifestDirectoryContents :: DirectoryTy -> TH.Exp -> TH.Exp -> TH.Exp -> TH.Exp -> ZEnvQ Exp
zmanifestDirectoryContents (Record id fields) treeE parentPathE dtaE manE = do
	liftM DoE $ zmanifestFields fields treeE parentPathE dtaE manE

zmanifestFields :: [Field] -> TH.Exp -> TH.Exp -> TH.Exp -> TH.Exp -> ZEnvQ [Stmt]
zmanifestFields [] treeE parentPathE dtaE manE = return [NoBindS $ Pure.returnExp manE]
zmanifestFields (field:fields) treeE parentPathE dtaE man0E = do
	man1Name <- lift $ newName "man"
	let (man1E,man1P) = genPE man1Name
	(rep_field,stmts_field) <- zmanifestField field treeE parentPathE dtaE man0E man1P
	let update (fs,env) = (fs,Map.insert rep_field Nothing env)
	stmts_fields <- Reader.local update $ zmanifestFields fields treeE parentPathE dtaE man1E
	return $ stmts_field++stmts_fields

zmanifestField :: Field -> TH.Exp -> TH.Exp -> TH.Exp -> TH.Exp -> Pat -> ZEnvQ (Name,[Stmt])
zmanifestField field treeE parentPathE dtaE man0E man1P = case field of
	Simple s -> zmanifestSimple s treeE parentPathE dtaE man0E man1P
	Comp   c -> zmanifestCompound True c treeE parentPathE dtaE man0E man1P

zmanifestComp :: CompField -> TH.Exp -> TH.Exp -> TH.Exp -> ZEnvQ TH.Exp
zmanifestComp cinfo treeE dtaE manE = do
	newdtaName <- lift $ newName "newdta"
	newmanName <- lift $ newName "newman"
	pathName <- lift $ newName "path"
	let (newdtaE,newdtaP) = genPE newdtaName
	let (newmanE,newmanP) = genPE newmanName
	let (pathE,pathP) = genPE pathName
	
	let collectMDs = zgenMergeFieldMDErrors (Comp cinfo)
	doCompE <- liftM (LamE [pathP,newdtaP,newmanP]) $ zmanifestCompContents cinfo treeE pathE newdtaE newmanE
	return $ Pure.appE5 (VarE 'doZManifestDirectory) treeE collectMDs dtaE doCompE manE

zmanifestCompContents :: CompField -> TH.Exp -> TH.Exp -> TH.Exp -> TH.Exp -> ZEnvQ TH.Exp
zmanifestCompContents cinfo treeE parentPathE dtaE manE = do
	man1Name <- lift $ newName "man1"
	let (man1E,man1P) = genPE man1Name
	(_,stmts) <- zmanifestCompound False cinfo treeE parentPathE dtaE manE man1P
	let doCompE = DoE $ stmts ++ [NoBindS $ Pure.returnExp man1E]
	return doCompE

zmanifestSimple :: BasicField -> TH.Exp -> TH.Exp -> TH.Exp -> TH.Exp -> Pat -> ZEnvQ (Name,[Stmt])
zmanifestSimple (internal, isForm, externalE, forestTy, predM) treeE parentPathE dtaE man0E man1P = do
	-- variable declarations
	let repName = mkName internal
	let (repE,repP) = genPE repName
	newmanName <- lift $ newName "man"
	let (newmanE,newmanP) = genPE newmanName
	newdtaName <- lift $ newName "dta"
	let (newdtaE,newdtaP) = genPE newdtaName
	
	let innerRepE = AppE repE dtaE
	
	-- we need to name the variables after the field names
	varName <- lift $ newName "var"
	let (varE,varP) = genPE varName
	let letRepS = [LetS [ValD varP (NormalB innerRepE) []],LetS [ValD repP (NormalB varE) []]]
	
	manifestFocusE <- do
		manifestContentE <- liftM (LamE [newdtaP,newmanP]) $ zmanifestE forestTy treeE newdtaE newmanE
		case predM of
			Nothing -> return $ Pure.appE6 (VarE 'doZManifestSimple) parentPathE externalE treeE innerRepE manifestContentE man0E
			Just pred -> return $ Pure.appE7 (VarE 'doZManifestSimpleWithConstraint) parentPathE externalE treeE (zmodPredE (VarP repName) pred) innerRepE manifestContentE man0E
	let bindManS = BindS man1P manifestFocusE
	return (repName,bindManS:letRepS)

zmanifestCompound :: Bool -> CompField -> TH.Exp -> TH.Exp -> TH.Exp -> TH.Exp -> TH.Pat -> ZEnvQ (Name,[Stmt])
zmanifestCompound isNested (CompField internal tyConNameOpt explicitName externalE descTy generatorP generatorG predM) treeE parentPathE dtaE man0E man1P = do
	-- variable declarations
	let repName = mkName internal
	let (repE,repP) = genPE repName
	newmanName <- lift $ newName "man"
	let (newmanE,newmanP) = genPE newmanName
	newdtaName <- lift $ newName "dta"
	let (newdtaE,newdtaP) = genPE newdtaName
	
	let innerRepE = if isNested then AppE repE dtaE else dtaE
	
	-- we need to name the variables after the field names
--	let letRepS = LetS [ValD repP (NormalB innerRepE) []]
--	let letMdS = LetS [ValD mdP (NormalB innerMdE) []]
	varName <- lift $ newName "var"
	let (varE,varP) = genPE varName
	let letRepS = [LetS [ValD varP (NormalB innerRepE) []],LetS [ValD repP (NormalB varE) []]]
	
	let genE = case generatorG of
		Explicit expE -> expE
		Matches regexpE -> regexpE

	forceVarsZEnvQ genE $ \genE -> do
		-- optional filtering
		let fileName = Pure.getCompName explicitName externalE
		let fileNameAtt = mkName $ nameBase fileName++"_att"
		let fileNameAttThunk = mkName $ nameBase fileName++"_att_thunk"
		
		-- build representation and metadata containers from a list
		destroyContainerE <- lift $ Pure.tyConNameOptToList tyConNameOpt
		
		let update (fs,env) = (fs,Map.insert fileName Nothing $ Map.insert fileNameAtt (Just (fileNameAttThunk,VarP fileNameAtt)) env)  --force the @FileInfo@ thunk
		
		-- container loading
		Reader.local update $ case predM of
			Nothing -> do
				manifestSingleE <- liftM (LamE [VarP fileName,VarP fileNameAttThunk,newdtaP,newmanP]) $ zmanifestE descTy treeE newdtaE newmanE
				let manifestContainerE = Pure.appE7 (VarE 'doZManifestCompound) parentPathE genE treeE destroyContainerE innerRepE manifestSingleE man0E
				let bindManS = BindS man1P manifestContainerE
				return (repName,bindManS:letRepS)
				
			Just predE -> forceVarsZEnvQ predE $ \predE -> do
				manifestSingleE <- liftM (LamE [VarP fileName,VarP fileNameAttThunk,newdtaP,newmanP]) $ zmanifestE descTy treeE newdtaE newmanE
				let manifestContainerE = Pure.appE8 (VarE 'doZManifestCompoundWithConstraint) parentPathE genE treeE destroyContainerE (modPredEComp (VarP fileName) predE) innerRepE manifestSingleE man0E
				let bindManS = BindS man1P manifestContainerE
				return (repName,bindManS:letRepS)
