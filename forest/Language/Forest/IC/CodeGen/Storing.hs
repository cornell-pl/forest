{-# LANGUAGE TupleSections, TemplateHaskell, NamedFieldPuns, ScopedTypeVariables, RecordWildCards, FlexibleInstances, MultiParamTypeClasses,
    UndecidableInstances, ViewPatterns  #-}

module Language.Forest.IC.CodeGen.Storing where

import Language.Forest.IC.CodeGen.Utils
import Control.Monad.Trans
import Control.Monad.Incremental

import Language.Forest.IC.CodeGen.Default
import Language.Forest.IC.IO.Storing
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
import Language.Forest.IC.CodeGen.Loading
import Language.Haskell.TH.Quote

genManifestM :: Name -> Name -> ForestTy -> [(TH.Pat,TH.Type)] -> EnvQ TH.Exp
genManifestM rep_name md_name forestTy pat_infos = return $ VarE 'undefined --do
--	fsName <- lift $ newName "fs"
--	treeName    <- lift $ newName "tree"
--	dtaName    <- lift $ newName "dta"
--	manName    <- lift $ newName "man"
--	let (treeE, treeP) = genPE treeName
--	let (dtaE, dtaP) = genPE dtaName
--	let (fsE, fsP) = genPE fsName
--	let (manE,manP) = genPE manName
--	
--	case pat_infos of
--		[] -> do
--			core_bodyE <- genManifestBody treeE dtaE manE rep_name forestTy
--			return $ LamE [TupP [],treeP,dtaP,manP] core_bodyE
--		otherwise -> do
--			(targsP,argThunkNames) <- genManifestArgsE (zip [1..] pat_infos) forestTy
--			let argsT = Pure.forestTupleTy $ map (AppT (ConT ''Arg) . snd) pat_infos
--			let proxyArgs = SigE (ConE 'Proxy) $ AppT (ConT ''Proxy) argsT
--			let update env = foldl updatePat env (zip argThunkNames pat_infos)
--				where updatePat env (thunkName,(pat,ty)) = Map.fromSet (\var -> Just (thunkName,pat)) (patPVars pat) `Map.union` env --left-biased union
--			Reader.local update $ do -- adds pattern variable deltas to the env.
--				margsName <- lift $ newName "margs"
--				newdtaName <- lift $ newName "newdta"
--				newmanName <- lift $ newName "newman"
--				let (margsE,margsP) = genPE margsName
--				let (newmanE,newmanP) = genPE newmanName
--				let (newdtaE,newdtaP) = genPE newdtaName
--				core_bodyE <- genManifestBody treeE newdtaE newmanE rep_name forestTy
--				return $ LamE [margsP,treeP,AsP dtaName (TupP [WildP,TupP [WildP,targsP]]),manP] $
--					Pure.appE5 (VarE 'doManifestArgs) proxyArgs margsE dtaE (LamE [newdtaP,newmanP] core_bodyE) manE

genManifestBody :: TH.Exp -> TH.Exp -> TH.Exp -> Name -> ForestTy -> EnvQ TH.Exp
genManifestBody treeE dtaE manE repN ty = case ty of 
	Directory _ -> manifestE ty treeE dtaE manE
	FConstraint _ (Directory _) _ -> manifestE ty treeE dtaE manE
	otherwise   -> do -- Decompose the representation type constructor
		let dtaE' = AppE (InfixE (Just $ VarE $ Pure.getUnTyName $ nameBase repN) (VarE '(><)) (Just $ VarE 'id)) dtaE
		manifestE ty treeE dtaE' manE

-- loads top-level arguments into the environment
genManifestArgsE :: [(Int,(TH.Pat,TH.Type))] -> ForestTy -> EnvQ (Pat,[Name])
genManifestArgsE [pat_info] forestTy = genManifestArgE pat_info forestTy
genManifestArgsE (pat_info:pat_infos) forestTy = do
	(pat,names1) <- genManifestArgE pat_info forestTy
	(pats,names2) <- genManifestArgsE pat_infos forestTy
	return (ConP '(:*:) [pat,pats],names1++names2)

genManifestArgE :: (Int,(TH.Pat,TH.Type)) -> ForestTy -> EnvQ (Pat,[Name])
genManifestArgE (i,(pat,pat_ty)) forestTy = do
	thunkName <- lift $ newName $ "t"++show i
	return (VarP thunkName,[thunkName])

manifestE :: ForestTy -> Exp -> Exp -> Exp -> EnvQ Exp
manifestE ty treeE dtaE manE = case ty of
	Named f_name               -> manifestWithArgsE [] treeE dtaE manE
	Fapp (Named f_name) argEs  -> manifestWithArgsE argEs treeE dtaE manE
	FFile (file_name, argEOpt) -> manifestFile file_name argEOpt treeE dtaE manE
	Archive archtype ty         -> manifestArchive archtype ty treeE dtaE manE
	FSymLink         -> manifestSymLink treeE dtaE manE
	FConstraint p ty pred -> manifestConstraint treeE p pred dtaE manE $ manifestE ty treeE
	Directory dirTy -> manifestDirectory dirTy treeE dtaE manE
	FMaybe forestTy -> manifestMaybe forestTy treeE dtaE manE
	FComp cinfo     -> manifestComp cinfo treeE dtaE manE

-- they are terminals in the spec
manifestWithArgsE :: [TH.Exp] -> TH.Exp -> TH.Exp -> TH.Exp -> EnvQ TH.Exp
manifestWithArgsE [] treeE dtaE manE = return $ Pure.appE4 (VarE 'updateManifestScratch) (TupE []) treeE dtaE manE
manifestWithArgsE argsE treeE dtaE manE = do
	argsE <- mapM (\e -> forceVarsEnvQ e return) argsE
	let tupArgsE = foldl1' (Pure.appE2 (ConE '(:*:))) argsE
	return $ Pure.appE4 (VarE 'updateManifestScratch) tupArgsE treeE dtaE manE

manifestArchive :: [ArchiveType] -> ForestTy -> TH.Exp -> TH.Exp -> TH.Exp -> EnvQ TH.Exp
manifestArchive archtype ty treeE dtaE manE = do
	newTreeName <- lift $ newName "new_tree"
	let (newTreeE, newTreeP) = genPE newTreeName
	newmanName <- lift $ newName "man"
	let (newmanE,newmanP) = genPE newmanName
	newdtaName <- lift $ newName "dta"
	let (newdtaE,newdtaP) = genPE newdtaName
	
	manifestContentsE <- liftM (LamE [newTreeP,newdtaP,newmanP]) $ manifestE ty newTreeE newdtaE newmanE
	exts <- lift $ dataToExpQ (\_ -> Nothing) archtype
	return $ Pure.appE5 (VarE 'doManifestArchive) exts treeE dtaE manifestContentsE manE

manifestSymLink :: TH.Exp -> TH.Exp -> TH.Exp -> EnvQ TH.Exp
manifestSymLink treeE dtaE manE = do
  return $ Pure.appE3 (VarE 'doManifestSymLink) treeE dtaE manE

manifestConstraint :: TH.Exp -> TH.Pat -> TH.Exp -> TH.Exp -> TH.Exp -> (Exp -> Exp -> EnvQ TH.Exp) -> EnvQ TH.Exp
manifestConstraint treeE pat predE dtaE manE manifest = forceVarsEnvQ predE $ \predE' -> do
	newmanName <- lift $ newName "man"
	let (newmanE,newmanP) = genPE newmanName
	newdtaName <- lift $ newName "dta"
	let (newdtaE,newdtaP) = genPE newdtaName
	
	let predFnE = modPredE pat predE'
	manifestAction <- liftM (LamE [newdtaP,newmanP]) $ manifest newdtaE newmanE
	return $ Pure.appE5 (VarE 'doManifestConstraint) treeE predFnE dtaE manifestAction manE

manifestFile :: String -> Maybe TH.Exp -> TH.Exp -> TH.Exp -> TH.Exp -> EnvQ TH.Exp
manifestFile fileName Nothing treeE dtaE manE = do
	return $ Pure.appE3 (VarE 'doManifestFile) treeE dtaE manE
manifestFile fileName (Just argE) treeE dtaE manE = do
	return $ Pure.appE4 (VarE 'doManifestFile1) argE treeE dtaE manE

manifestMaybe :: ForestTy -> TH.Exp -> TH.Exp -> TH.Exp -> EnvQ TH.Exp
manifestMaybe ty treeE dtaE manE = do 
	newdtaName <- lift $ newName "newdta"
	newmanName <- lift $ newName "newman"
	newrepName <- lift $ newName "newrep"
	newpathName <- lift $ newName "newpath"
	let (newdtaE,newdtaP) = genPE newdtaName
	let (newmanE,newmanP) = genPE newmanName
	let (newrepE,newrepP) = genPE newrepName
	let (newpathE,newpathP) = genPE newpathName
	doContentsE <- liftM (LamE [newdtaP,newmanP]) $ manifestE ty treeE newdtaE newmanE
	defaultContentsE <- liftM (LamE [newrepP,newpathP]) $ lift $ defaultE ty newrepE newpathE
	return $ Pure.appE5 (VarE 'doManifestMaybe) treeE dtaE doContentsE defaultContentsE manE

manifestDirectory :: DirectoryTy -> TH.Exp -> TH.Exp -> TH.Exp -> EnvQ TH.Exp
manifestDirectory dirTy@(Record id fields) treeE dtaE manE = do
	newdtaName <- lift $ newName "newdta"
	newmanName <- lift $ newName "newman"
	pathName <- lift $ newName "path"
	let (newdtaE,newdtaP) = genPE newdtaName
	let (newmanE,newmanP) = genPE newmanName
	let (pathE,pathP) = genPE pathName
	doDirE <- liftM (LamE [pathP,newdtaP,newmanP]) $ manifestDirectoryContents dirTy treeE pathE newdtaE newmanE
	collectMDs <- lift $ genMergeFieldsMDErrors fields	
	return $ Pure.appE5 (VarE 'doManifestDirectory) treeE collectMDs dtaE doDirE manE

manifestDirectoryContents :: DirectoryTy -> TH.Exp -> TH.Exp -> TH.Exp -> TH.Exp -> EnvQ Exp
manifestDirectoryContents (Record id fields) treeE parentPathE dtaE manE = do
	liftM DoE $ manifestFields fields treeE parentPathE dtaE manE

manifestFields :: [Field] -> TH.Exp -> TH.Exp -> TH.Exp -> TH.Exp -> EnvQ [Stmt]
manifestFields [] treeE parentPathE dtaE manE = return [NoBindS $ Pure.returnExp manE]
manifestFields (field:fields) treeE parentPathE dtaE man0E = do
	man1Name <- lift $ newName "man"
	let (man1E,man1P) = genPE man1Name
	(rep_field,md_field,stmts_field) <- manifestField field treeE parentPathE dtaE man0E man1P
	let update (mode,fs,env) = (mode,fs,Map.insert rep_field Nothing $ Map.insert md_field Nothing env)
	stmts_fields <- Reader.local update $ manifestFields fields treeE parentPathE dtaE man1E
	return $ stmts_field++stmts_fields

manifestField :: Field -> TH.Exp -> TH.Exp -> TH.Exp -> TH.Exp -> Pat -> EnvQ (Name,Name,[Stmt])
manifestField field treeE parentPathE dtaE man0E man1P = case field of
	Simple s -> manifestSimple s treeE parentPathE dtaE man0E man1P
	Comp   c -> manifestCompound True c treeE parentPathE dtaE man0E man1P

manifestComp :: CompField -> TH.Exp -> TH.Exp -> TH.Exp -> EnvQ TH.Exp
manifestComp cinfo treeE dtaE manE = do
	newdtaName <- lift $ newName "newdta"
	newmanName <- lift $ newName "newman"
	pathName <- lift $ newName "path"
	let (newdtaE,newdtaP) = genPE newdtaName
	let (newmanE,newmanP) = genPE newmanName
	let (pathE,pathP) = genPE pathName
	
	let collectMDs = genMergeFieldMDErrors (Comp cinfo)
	doCompE <- liftM (LamE [pathP,newdtaP,newmanP]) $ manifestCompContents cinfo treeE pathE newdtaE newmanE
	return $ Pure.appE5 (VarE 'doManifestDirectory) treeE collectMDs dtaE doCompE manE

manifestCompContents :: CompField -> TH.Exp -> TH.Exp -> TH.Exp -> TH.Exp -> EnvQ TH.Exp
manifestCompContents cinfo treeE parentPathE dtaE manE = do
	man1Name <- lift $ newName "man1"
	let (man1E,man1P) = genPE man1Name
	(_,_,stmts) <- manifestCompound False cinfo treeE parentPathE dtaE manE man1P
	let doCompE = DoE $ stmts ++ [NoBindS $ Pure.returnExp man1E]
	return doCompE

manifestSimple :: BasicField -> TH.Exp -> TH.Exp -> TH.Exp -> TH.Exp -> Pat -> EnvQ (Name,Name,[Stmt])
manifestSimple (internal, isForm, externalE, forestTy, predM) treeE parentPathE dtaE man0E man1P = do
	-- variable declarations
	let repName = mkName internal
	let mdName  = mkName (internal++"_md")
	let (repE,repP) = genPE repName
	let (mdE,mdP) = genPE mdName
	newmanName <- lift $ newName "man"
	let (newmanE,newmanP) = genPE newmanName
	newdtaName <- lift $ newName "dta"
	let (newdtaE,newdtaP) = genPE newdtaName
	
	let innerRepE = AppE repE $ AppE (VarE 'fst) dtaE
	let innerMdE = AppE mdE $ AppE (VarE 'snd) dtaE
	let innerdtaE = TupE [innerRepE,innerMdE]
	
	-- we need to name the variables after the field names
	varName <- lift $ newName "var"
	let (varE,varP) = genPE varName
	let letRepS = [LetS [ValD varP (NormalB innerRepE) []],LetS [ValD repP (NormalB varE) []]]
	let letMdS = [LetS [ValD varP (NormalB innerMdE) []],LetS [ValD repP (NormalB varE) []]]
	
	manifestFocusE <- do
		manifestContentE <- liftM (LamE [newdtaP,newmanP]) $ manifestE forestTy treeE newdtaE newmanE
		case predM of
			Nothing -> return $ Pure.appE6 (VarE 'doManifestSimple) parentPathE externalE treeE innerdtaE manifestContentE man0E
			Just pred -> return $ Pure.appE7 (VarE 'doManifestSimpleWithConstraint) parentPathE externalE treeE (modPredE (VarP repName) pred) innerdtaE manifestContentE man0E
	let bindManS = BindS man1P manifestFocusE
	return (repName,mdName,bindManS:letRepS++letMdS)

manifestCompound :: Bool -> CompField -> TH.Exp -> TH.Exp -> TH.Exp -> TH.Exp -> TH.Pat -> EnvQ (Name,Name,[Stmt])
manifestCompound isNested (CompField internal tyConNameOpt explicitName externalE descTy generatorP generatorG predM) treeE parentPathE dtaE man0E man1P = do
	-- variable declarations
	let repName = mkName internal
	let mdName  = mkName (internal++"_md")
	let (repE,repP) = genPE repName
	let (mdE,mdP) = genPE mdName
	newmanName <- lift $ newName "man"
	let (newmanE,newmanP) = genPE newmanName
	newdtaName <- lift $ newName "dta"
	let (newdtaE,newdtaP) = genPE newdtaName
	
	let innerRepE = if isNested then AppE repE $ AppE (VarE 'fst) dtaE else AppE (VarE 'fst) dtaE
	let innerMdE = if isNested then AppE mdE $ AppE (VarE 'snd) dtaE else AppE (VarE 'snd) dtaE
	let innerdtaE = TupE [innerRepE,innerMdE]
	
	-- we need to name the variables after the field names
--	let letRepS = LetS [ValD repP (NormalB innerRepE) []]
--	let letMdS = LetS [ValD mdP (NormalB innerMdE) []]
	varName <- lift $ newName "var"
	let (varE,varP) = genPE varName
	let letRepS = [LetS [ValD varP (NormalB innerRepE) []],LetS [ValD repP (NormalB varE) []]]
	let letMdS = [LetS [ValD varP (NormalB innerMdE) []],LetS [ValD repP (NormalB varE) []]]
	
	let genE = case generatorG of
		Explicit expE -> expE
		Matches regexpE -> regexpE

	forceVarsEnvQ genE $ \genE -> do
		-- optional filtering
		let fileName = Pure.getCompName explicitName externalE
		let fileNameAtt = mkName $ nameBase fileName++"_att"
		let fileNameAttThunk = mkName $ nameBase fileName++"_att_thunk"
		
		-- build representation and metadata containers from a list
		destroyContainerE <- lift $ Pure.tyConNameOptToList tyConNameOpt
		
		let update (mode,fs,env) = (mode,fs,Map.insert fileName Nothing $ Map.insert fileNameAtt (Just (fileNameAttThunk,VarP fileNameAtt)) env)  --force the @FileInfo@ thunk
		
		-- container loading
		Reader.local update $ case predM of
			Nothing -> do
				manifestSingleE <- liftM (LamE [VarP fileName,VarP fileNameAttThunk,newdtaP,newmanP]) $ manifestE descTy treeE newdtaE newmanE
				let manifestContainerE = Pure.appE8 (VarE 'doManifestCompound) parentPathE genE treeE destroyContainerE destroyContainerE innerdtaE manifestSingleE man0E
				let bindManS = BindS man1P manifestContainerE
				return (repName,mdName,bindManS:letRepS++letMdS)
				
			Just predE -> forceVarsEnvQ predE $ \predE -> do
				manifestSingleE <- liftM (LamE [VarP fileName,VarP fileNameAttThunk,newdtaP,newmanP]) $ manifestE descTy treeE newdtaE newmanE
				let manifestContainerE = Pure.appE9 (VarE 'doManifestCompoundWithConstraint) parentPathE genE treeE destroyContainerE destroyContainerE (modPredEComp (VarP fileName) predE) innerdtaE manifestSingleE man0E
				let bindManS = BindS man1P manifestContainerE
				return (repName,mdName,bindManS:letRepS++letMdS)
