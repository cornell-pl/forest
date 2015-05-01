{-# LANGUAGE TupleSections, TemplateHaskell, NamedFieldPuns, ScopedTypeVariables, RecordWildCards, FlexibleInstances, MultiParamTypeClasses,
    UndecidableInstances, ViewPatterns  #-}

module Language.Forest.IC.CodeGen.ZStoring where

import Language.Forest.IC.CodeGen.Utils
import Control.Monad.Trans
import Control.Monad.Incremental
import Language.Forest.IC.CodeGen.ZDefault

import {-# SOURCE #-} Language.Forest.IC.CodeGen.ZDeltaStoring
import Data.WithClass.MData
--import Language.Forest.IC.CodeGen.Default
--import Language.Forest.IC.IO.Storing
import Language.Forest.IC.IO.ZStoring
import Language.Forest.Syntax as PS
import Language.Forest.IC.MetaData
import Language.Forest.IC.ICRep
--import qualified Language.Forest.MetaData as Pure
--import qualified Language.Forest.CodeGen.Utils as Pure
import Language.Forest.Errors
import Language.Forest.IC.Generic
import qualified Language.Forest.Errors as E
import Language.Forest.Manifest
import Language.Forest.FS.FSDelta
import System.Directory
import System.FilePath.Posix
import Control.Monad.Reader (Reader(..),ReaderT(..))
import qualified Control.Monad.Reader as Reader
--import qualified Language.Forest.MetaData as Pure
import Language.Forest.IC.BX as BX

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
--import {-# SOURCE #-} Language.Forest.IC.CodeGen.DeltaLoading
import {-# SOURCE #-} Language.Forest.IC.CodeGen.ZDeltaLoading
--import Language.Forest.IC.CodeGen.Loading
import Language.Forest.IC.CodeGen.ZLoading
import Language.Haskell.TH.Quote

genZManifestM :: Bool -> Name -> ForestTy -> [(TH.Pat,TH.Type)] -> ZEnvQ Exp
genZManifestM isTop rep_name forestTy pat_infos = do
	fsName <- lift $ newName "fs"
	treeName    <- lift $ newName "tree"
	dtaName    <- lift $ newName "dta"
	manName    <- lift $ newName "man"
	pathName <- lift $ newName "path"
	argsName <- lift $ newName "args"
	let (pathE,pathP) = genPE pathName
	let (treeE, treeP) = genPE treeName
	let (dtaE, dtaP) = genPE dtaName
	let (fsE, fsP) = genPE fsName
	let (manE,manP) = genPE manName
	
	case pat_infos of
		[] -> do
			core_bodyE <- genZManifestBody isTop pathE treeE dtaE manE rep_name forestTy
			return $ LamE [VarP argsName,TupP [],pathP,treeP,dtaP,manP] core_bodyE
		otherwise -> do
			(targsP,argThunkNames) <- genZManifestArgsE (zip [1..] pat_infos) forestTy
			let argsT = forestTupleTy $ map (AppT (ConT ''Arg) . snd) pat_infos
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
				core_bodyE <- genZManifestBody isTop pathE treeE newdtaE newmanE rep_name forestTy
				return $ LamE [VarP argsName,margsP,pathP,treeP,VarP dtaName,manP] $ DoE [
					BindS targsP $ AppE (VarE 'lift) $ AppE (VarE 'inside) $ appE3 (VarE 'newArgs) (proxyN fs) proxyArgs margsE,
					NoBindS $ appE5 (VarE 'doZManifestArgs) proxyArgs margsE dtaE (LamE [newdtaP,newmanP] core_bodyE) manE]

genZManifestBody :: Bool -> Exp -> Exp -> Exp -> Exp -> Name -> ForestTy -> ZEnvQ Exp
genZManifestBody isTop pathE treeE dtaE manE repN ty = case ty of 
	Directory _ -> zmanifestE isTop ty pathE treeE dtaE manE
	FConstraint _ (Directory _) _ -> zmanifestE isTop ty pathE treeE dtaE manE
	otherwise   -> do -- Decompose the representation type constructor
		let dtaE' = AppE (VarE $ getUnTyName $ nameBase repN) dtaE
		zmanifestE isTop ty pathE treeE dtaE' manE

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

zmanifestE :: Bool -> ForestTy -> Exp -> Exp -> Exp -> Exp -> ZEnvQ Exp
zmanifestE isTop ty pathE treeE dtaE manE = case ty of
	Named f_name               -> zmanifestWithArgsE isTop f_name [] pathE treeE dtaE manE
	Fapp (Named f_name) argEs  -> zmanifestWithArgsE isTop f_name argEs pathE treeE dtaE manE
	FFile (file_name, argEOpt) -> zmanifestFile isTop file_name argEOpt pathE treeE dtaE manE
	Archive archtype ty         -> zmanifestArchive isTop archtype ty pathE treeE dtaE manE
	FSymLink         -> zmanifestSymLink isTop pathE treeE dtaE manE
	FConstraint p ty pred -> zmanifestConstraint isTop treeE p pred dtaE manE $ zmanifestE False ty pathE treeE
	Directory dirTy -> zmanifestDirectory isTop dirTy pathE treeE dtaE manE
	FMaybe forestTy -> zmanifestMaybe isTop forestTy pathE treeE dtaE manE
	FComp cinfo     -> zmanifestComp isTop cinfo pathE treeE dtaE manE

-- they are terminals in the spec
zmanifestWithArgsE :: Bool -> String -> [Exp] -> Exp -> Exp -> Exp -> Exp -> ZEnvQ Exp
zmanifestWithArgsE isTop ty_name [] pathE treeE dtaE manE = do
	let proxyE = AppE (VarE 'proxyOf) $ TupE []
	let manifest = if isTop then VarE 'doZManifestNamed else VarE 'zupdateManifestScratchGeneric
	return $ appE6 manifest proxyE (TupE []) pathE treeE dtaE manE
zmanifestWithArgsE isTop ty_name argsE pathE treeE dtaE manE = do
	argsE <- mapM (\e -> forceVarsZEnvQ e return) argsE
	let tupArgsE = foldl1' (appE2 (ConE '(:*:))) argsE
	let manifest = if isTop then VarE 'doZManifestNamed else VarE 'zupdateManifestScratchGeneric
	return $ appE6 manifest (VarE $ mkName $ "proxyZArgs_"++ty_name) tupArgsE pathE treeE dtaE manE

zmanifestArchive :: Bool -> [ArchiveType] -> ForestTy -> Exp -> Exp -> Exp -> Exp -> ZEnvQ Exp
zmanifestArchive isTop archtype ty pathE treeE dtaE manE = do
	newPathName <- lift $ newName "new_path"
	let (newPathE,newPathP) = genPE newPathName
	newPathName' <- lift $ newName "new_path'"
	let (newPathE',newPathP') = genPE newPathName
	newTreeName <- lift $ newName "new_tree"
	let (newTreeE, newTreeP) = genPE newTreeName
	newTreeName' <- lift $ newName "new_tree'"
	let (newTreeE', newTreeP') = genPE newTreeName'
	newmanName <- lift $ newName "man"
	let (newmanE,newmanP) = genPE newmanName
	newdtaName <- lift $ newName "dta"
	let (newdtaE,newdtaP) = genPE newdtaName
	newmanName <- lift $ newName "man"
	let (newmanE,newmanP) = genPE newmanName
	newDfName <- lift $ newName "df"
	let (newDfE,newDfP) = genPE newDfName
	newdvName <- lift $ newName "dv"
	let (newdvE,newdvP) = genPE newdvName
	
	manifestContentsE <- liftM (LamE [newPathP,newTreeP,newdtaP,newmanP]) $ zmanifestE False ty newPathE newTreeE newdtaE newmanE
	manifestContentDeltaE <- runZDeltaQ $ liftM (LamE [newPathP,newPathP',newTreeP,newDfP,newTreeP',newdtaP,newdvP,newmanP]) $ zmanifestDeltaE False ty newPathE newPathE' newTreeE newDfE newTreeE' newdtaE newdvE newmanE
	exts <- lift $ dataToExpQ (\_ -> Nothing) archtype
	isClosedE <- lift $ dataToExpQ (\_ -> Nothing) $ isClosedForestTy ty
	if isTop
		then return $ appE9 (VarE 'doZManifestArchive) isClosedE exts pathE treeE dtaE manifestContentsE manifestContentDeltaE (zdiffE ty) manE
		else return $ appE6 (VarE 'doZManifestArchiveInner) exts pathE treeE dtaE manifestContentsE manE

zmanifestSymLink :: Bool -> Exp -> Exp -> Exp -> Exp -> ZEnvQ Exp
zmanifestSymLink isTop pathE treeE dtaE manE = do
	if isTop
		then return $ appE4 (VarE 'doZManifestSymLink) pathE treeE dtaE manE
		else return $ appE4 (VarE 'doZManifestSymLinkInner) pathE treeE dtaE manE

zmanifestConstraint :: Bool -> Exp -> TH.Pat -> Exp -> Exp -> Exp -> (Exp -> Exp -> ZEnvQ Exp) -> ZEnvQ Exp
zmanifestConstraint isTop treeE pat predE dtaE manE manifest = forceVarsZEnvQ predE $ \predE' -> do
	newmanName <- lift $ newName "man"
	let (newmanE,newmanP) = genPE newmanName
	newdtaName <- lift $ newName "dta"
	let (newdtaE,newdtaP) = genPE newdtaName
	
	let predFnE = zmodPredE pat predE'
	manifestAction <- liftM (LamE [newdtaP,newmanP]) $ manifest newdtaE newmanE
	if isTop
		then return $ appE4 (VarE 'doZManifestConstraint) predFnE dtaE manifestAction manE
		else return $ appE4 (VarE 'doZManifestConstraintInner) predFnE dtaE manifestAction manE

zmanifestFile :: Bool -> String -> Maybe Exp -> Exp -> Exp -> Exp -> Exp -> ZEnvQ Exp
zmanifestFile isTop fileName Nothing pathE treeE dtaE manE = do
	if isTop
		then return $ appE5 (VarE 'doZManifestFile1) (returnExp $ TupE []) pathE treeE dtaE manE
		else return $ appE5 (VarE 'doZManifestFileInner1) (returnExp $ TupE []) pathE treeE dtaE manE
zmanifestFile isTop fileName (Just argE) pathE treeE dtaE manE = do
	if isTop
		then return $ appE5 (VarE 'doZManifestFile1) argE pathE treeE dtaE manE
		else return $ appE5 (VarE 'doZManifestFileInner1) argE pathE treeE dtaE manE

zmanifestMaybe :: Bool -> ForestTy -> Exp -> Exp -> Exp -> Exp -> ZEnvQ Exp
zmanifestMaybe isTop ty pathE treeE dtaE manE = do 
	newdtaName <- lift $ newName "newdta"
	newmanName <- lift $ newName "newman"
	newrepName <- lift $ newName "newrep"
	newpathName <- lift $ newName "newpath"
	let (newdtaE,newdtaP) = genPE newdtaName
	let (newmanE,newmanP) = genPE newmanName
	let (newrepE,newrepP) = genPE newrepName
	let (newpathE,newpathP) = genPE newpathName
	doContentsE <- liftM (LamE [newdtaP,newmanP]) $ zmanifestE False ty pathE treeE newdtaE newmanE
	if isTop
		then return $ appE5 (VarE 'doZManifestMaybe) pathE treeE dtaE doContentsE manE
		else return $ appE5 (VarE 'doZManifestMaybeInner) pathE treeE dtaE doContentsE manE

zmanifestDirectory :: Bool -> DirectoryTy -> Exp -> Exp -> Exp -> Exp -> ZEnvQ Exp
zmanifestDirectory isTop dirTy@(Record id fields) pathE treeE dtaE manE = do
	newdtaName <- lift $ newName "newdta"
	newmanName <- lift $ newName "newman"
	newpathName <- lift $ newName "newpath"
	let (newdtaE,newdtaP) = genPE newdtaName
	let (newmanE,newmanP) = genPE newmanName
	let (newpathE,newpathP) = genPE newpathName
	doDirE <- liftM (LamE [newdtaP,newmanP]) $ zmanifestDirectoryContents dirTy treeE pathE newdtaE newmanE
	collectMDs <- lift $ zgenMergeFieldsMDErrors fields	
	if isTop
		then return $ appE6 (VarE 'doZManifestDirectory) pathE treeE collectMDs dtaE doDirE manE
		else return $ appE6 (VarE 'doZManifestDirectoryInner) pathE treeE collectMDs dtaE doDirE manE

zmanifestDirectoryContents :: DirectoryTy -> Exp -> Exp -> Exp -> Exp -> ZEnvQ Exp
zmanifestDirectoryContents (Record id fields) treeE parentPathE dtaE manE = do
	liftM DoE $ zmanifestFields fields treeE parentPathE dtaE manE

zmanifestFields :: [Field] -> Exp -> Exp -> Exp -> Exp -> ZEnvQ [Stmt]
zmanifestFields [] treeE parentPathE dtaE manE = return [NoBindS $ returnExp manE]
zmanifestFields (field:fields) treeE parentPathE dtaE man0E = do
	man1Name <- lift $ newName "man"
	let (man1E,man1P) = genPE man1Name
	(rep_field,stmts_field) <- zmanifestField field treeE parentPathE dtaE man0E man1P
	let update (fs,env) = (fs,Map.insert rep_field Nothing env)
	stmts_fields <- Reader.local update $ zmanifestFields fields treeE parentPathE dtaE man1E
	return $ stmts_field++stmts_fields

zmanifestField :: Field -> Exp -> Exp -> Exp -> Exp -> Pat -> ZEnvQ (Name,[Stmt])
zmanifestField field treeE parentPathE dtaE man0E man1P = case field of
	Simple s -> zmanifestSimple s treeE parentPathE dtaE man0E man1P
	Comp   c -> zmanifestCompound True c treeE parentPathE dtaE man0E man1P

zmanifestComp :: Bool -> CompField -> Exp -> Exp -> Exp -> Exp -> ZEnvQ Exp
zmanifestComp isTop cinfo pathE treeE dtaE manE = do
	newdtaName <- lift $ newName "newdta"
	newmanName <- lift $ newName "newman"
	newpathName <- lift $ newName "newpath"
	let (newdtaE,newdtaP) = genPE newdtaName
	let (newmanE,newmanP) = genPE newmanName
	let (newpathE,newpathP) = genPE newpathName
	
	let collectMDs = zgenMergeFieldMDErrors (Comp cinfo)
	doCompE <- liftM (LamE [newdtaP,newmanP]) $ zmanifestCompContents cinfo treeE pathE newdtaE newmanE
	if isTop
		then return $ appE6 (VarE 'doZManifestDirectory) pathE treeE collectMDs dtaE doCompE manE
		else return $ appE6 (VarE 'doZManifestDirectoryInner) pathE treeE collectMDs dtaE doCompE manE

zmanifestCompContents :: CompField -> Exp -> Exp -> Exp -> Exp -> ZEnvQ Exp
zmanifestCompContents cinfo treeE parentPathE dtaE manE = do
	man1Name <- lift $ newName "man1"
	let (man1E,man1P) = genPE man1Name
	(_,stmts) <- zmanifestCompound False cinfo treeE parentPathE dtaE manE man1P
	let doCompE = DoE $ stmts ++ [NoBindS $ returnExp man1E]
	return doCompE

zmanifestSimple :: BasicField -> Exp -> Exp -> Exp -> Exp -> Pat -> ZEnvQ (Name,[Stmt])
zmanifestSimple (internal, isForm, externalE, forestTy, predM) treeE parentPathE dtaE man0E man1P = do
	-- variable declarations
	let repName = mkName internal
	let (repE,repP) = genPE repName
	newmanName <- lift $ newName "man"
	let (newmanE,newmanP) = genPE newmanName
	newdtaName <- lift $ newName "dta"
	let (newdtaE,newdtaP) = genPE newdtaName
	newpathName <- lift $ newName "newpath"
	let (newpathE,newpathP) = genPE newpathName
	
	let innerRepE = AppE repE dtaE
	
	-- we need to name the variables after the field names
	varName <- lift $ newName "var"
	let (varE,varP) = genPE varName
	let letRepS = [BindS varP (AppE (VarE 'lift) $ AppE (VarE 'inside) $ appE2 (VarE 'BX.getM) (VarE 'lens_content) $ returnExp innerRepE),LetS [ValD repP (NormalB varE) []]]
	
	manifestFocusE <- do
		manifestContentE <- liftM (LamE [newpathP,newdtaP,newmanP]) $ zmanifestE False forestTy newpathE treeE newdtaE newmanE
		case predM of
			Nothing -> return $ appE6 (VarE 'doZManifestSimple) parentPathE externalE treeE innerRepE manifestContentE man0E
			Just pred -> return $ appE7 (VarE 'doZManifestSimpleWithConstraint) parentPathE externalE treeE (zmodPredE (VarP repName) pred) innerRepE manifestContentE man0E
	let bindManS = BindS man1P manifestFocusE
	return (repName,bindManS:letRepS)

zmanifestCompound :: Bool -> CompField -> Exp -> Exp -> Exp -> Exp -> TH.Pat -> ZEnvQ (Name,[Stmt])
zmanifestCompound isNested (CompField internal tyConNameOpt explicitName externalE descTy generatorP generatorTy generatorG predM) treeE parentPathE dtaE man0E man1P = do
	-- variable declarations
	let repName = mkName internal
	let (repE,repP) = genPE repName
	newmanName <- lift $ newName "man"
	let (newmanE,newmanP) = genPE newmanName
	newdtaName <- lift $ newName "dta"
	let (newdtaE,newdtaP) = genPE newdtaName
	newpathName <- lift $ newName "newpath"
	let (newpathE,newpathP) = genPE newpathName
	
	let innerRepE = AppE (VarE 'lift) $ AppE (VarE 'inside) $ appE2 (VarE 'BX.getM) (VarE 'lens_content) $ returnExp $ if isNested then AppE repE dtaE else dtaE
	
	-- we need to name the variables after the field names
--	let letRepS = LetS [ValD repP (NormalB innerRepE) []]
--	let letMdS = LetS [ValD mdP (NormalB innerMdE) []]
	varName <- lift $ newName "var"
	let (varE,varP) = genPE varName
	let letRepS = [BindS varP innerRepE,LetS [ValD repP (NormalB varE) []]]
	
	let genE = case generatorG of
		Explicit expE -> expE
		Matches regexpE -> regexpE

	let keyArgE = case generatorTy of
		Just (key_ty_name,Just argE) -> argE
		otherwise -> returnExp $ TupE []

	forceVarsZEnvQ keyArgE $ \keyArgE -> forceVarsZEnvQ genE $ \genE -> do
		-- optional filtering
		let fileName = getCompName explicitName externalE
		let fileNameAtt = mkName $ nameBase fileName++"_att"
		let fileNameAttThunk = mkName $ nameBase fileName++"_att_thunk"
		
		-- build representation and metadata containers from a list
		destroyContainerE <- lift $ tyConNameOptToList tyConNameOpt
		
		let update (fs,env) = (fs,Map.insert fileName Nothing $ Map.insert fileNameAtt (Just (fileNameAttThunk,VarP fileNameAtt)) env)  --force the @FileInfo@ thunk
		
		-- container loading
		Reader.local update $ case predM of
			Nothing -> do
				manifestSingleE <- liftM (LamE [VarP fileName,VarP fileNameAttThunk,newpathP,newdtaP,newmanP]) $ zmanifestE False descTy newpathE treeE newdtaE newmanE
				let manifestContainerE = appE8 (VarE 'doZManifestCompound) parentPathE genE treeE keyArgE destroyContainerE varE manifestSingleE man0E
				let bindManS = BindS man1P manifestContainerE
				return (repName,letRepS++[bindManS])
				
			Just predE -> forceVarsZEnvQ predE $ \predE -> do
				manifestSingleE <- liftM (LamE [VarP fileName,VarP fileNameAttThunk,newpathP,newdtaP,newmanP]) $ zmanifestE False descTy newpathE treeE newdtaE newmanE
				let manifestContainerE = appE9 (VarE 'doZManifestCompoundWithConstraint) parentPathE genE treeE keyArgE destroyContainerE (modPredEComp (VarP fileName) predE) varE manifestSingleE man0E
				let bindManS = BindS man1P manifestContainerE
				return (repName,letRepS++[bindManS])
