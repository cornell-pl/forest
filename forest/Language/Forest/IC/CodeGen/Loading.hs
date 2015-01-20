{-# LANGUAGE TupleSections, TemplateHaskell, NamedFieldPuns, ScopedTypeVariables, RecordWildCards, FlexibleInstances, MultiParamTypeClasses,
    UndecidableInstances, ViewPatterns  #-}

module Language.Forest.IC.CodeGen.Loading where

import {-# SOURCE #-} Language.Forest.IC.CodeGen.DeltaLoading
import Language.Forest.IC.IO.Loading

import Prelude hiding (const,read)
import Language.Forest.IC.CodeGen.Utils
import Language.Forest.IC.ICRep
import Control.Monad.Trans
import Control.Monad.Incremental as Inc
import Language.Haskell.TH.Quote
import qualified Language.Forest.Pure.CodeGen.Utils as Pure
import Data.WithClass.MData

import Language.Forest.Syntax as PS
import Language.Forest.IC.MetaData
import Language.Forest.Pure.MetaData (FileInfo(..),FileType(..),(:*:)(..))
import Language.Forest.Errors
import Language.Forest.IC.Generic
import qualified Language.Forest.Errors as E
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

-- receives an expression with thunks, and returns an expression that forces the thunks to get the corresponding values
forceVarsEnvQ :: Exp -> (Exp -> EnvQ a) -> EnvQ a
forceVarsEnvQ e f = do
	let expvars = expVars e
	let forceVar var f = \e -> do
		(mode,fs,envvars) <- Reader.ask
		case Map.lookup var envvars of
			Just (Just (thunk,pat)) -> do
				
				let update (mode,fs,env) = (mode,fs,Set.foldr (\var env -> Map.insert var Nothing env) env (patPVars pat))
				Reader.local update $ f $ UInfixE (AppE (VarE 'Inc.read) (VarE thunk)) (VarE '(>>=)) (LamE [pat] e)
			otherwise -> f e
	(Set.foldr forceVar f expvars) e

genLoadM :: Name -> Name -> ForestTy -> [(TH.Pat,TH.Type)] -> EnvQ TH.Exp
genLoadM rep_name md_name forestTy pat_infos = do
	fsName <- lift $ newName "fs"
	pathName    <- lift $ newName "path"
	filterPathName    <- lift $ newName "filterPath"
	treeName    <- lift $ newName "tree"
--	oldtreeName    <- lift $ newName "oldtree"
	argsName <- lift $ newName "args"
	getMDName    <- lift $ newName "getMD"
--	dfName <- lift $ newName "df"
	let (pathE, pathP) = genPE pathName
	let (filterPathE, filterPathP) = genPE filterPathName
	let (treeE, treeP) = genPE treeName
--	let (oldtreeE, oldtreeP) = genPE oldtreeName
--	let (dfE, dfP) = genPE dfName
	let (getMDE, getMDP) = genPE getMDName
	let (fsE, fsP) = genPE fsName
	
	case pat_infos of
		[] -> do
			(mode,fs,_) <- Reader.ask
			core_bodyE <- genLoadBody filterPathE pathE treeE getMDE rep_name forestTy
			return $ LamE [VarP mode,VarP argsName,TupP [],filterPathP,pathP,treeP,getMDP] core_bodyE
		otherwise -> do
			(argsP,stmts,argsThunksE,argThunkNames) <- genLoadArgsE (zip [1..] pat_infos) treeE forestTy
			let update (mode,fs,env) = (mode,fs,foldl updatePat env (zip argThunkNames pat_infos))
				where updatePat env (thunkName,(pat,ty)) = Map.fromSet (\var -> Just (thunkName,pat)) (patPVars pat) `Map.union` env --left-biased union
			Reader.local update $ do -- adds pattern variable deltas to the env.
				(mode,fs,_) <- Reader.ask
				core_bodyE <- genLoadBody filterPathE pathE treeE getMDE rep_name forestTy
				let core_bodyE' = Pure.appE4 (VarE 'doLoadArgs) (VarE mode) (VarE argsName) argsThunksE core_bodyE
				return $ LamE [VarP mode,VarP argsName,argsP,filterPathP,pathP,treeP,getMDP] $ --DoE $ stmts ++ [NoBindS core_bodyE]
					DoE $ stmts ++ [NoBindS core_bodyE']

{-
 Generate body of loadM function, which has the form:
  do (rep,md) <- rhsE
     return (Rep rep, md)
-}
genLoadBody :: TH.Exp -> TH.Exp -> TH.Exp -> TH.Exp -> Name -> ForestTy -> EnvQ TH.Exp
genLoadBody filterPathE pathE treeE getMDE repN ty = case ty of 
	Directory _ -> loadE ty filterPathE pathE treeE getMDE
	FConstraint _ (Directory _) _ -> loadE ty filterPathE pathE treeE getMDE
	otherwise   -> do -- Add type constructor
		rhsE <- loadE ty filterPathE pathE treeE getMDE
		return $ Pure.appE2 (VarE 'liftM) (InfixE (Just $ ConE repN) (VarE '(><)) (Just $ VarE 'id)) rhsE 

-- adds top-level arguments to the metadata
genLoadArgsE :: [(Int,(TH.Pat,TH.Type))] -> TH.Exp -> ForestTy -> EnvQ (TH.Pat,[Stmt],TH.Exp,[Name])
genLoadArgsE [pat_info] treeE forestTy = genLoadArgE pat_info treeE forestTy
genLoadArgsE (pat_info:pat_infos) treeE forestTy = do
	(pat,stmt,expr,names1) <- genLoadArgE pat_info treeE forestTy
	(pats,stmts,exprs,names2) <- genLoadArgsE pat_infos treeE forestTy
	return (ConP '(:*:) [pat,pats],stmt++stmts,UInfixE expr (ConE '(:*:)) exprs,names1++names2)

--we do not force thunks, and leave that for whenever their values are required inside expressions
genLoadArgE :: (Int,(TH.Pat,TH.Type)) -> TH.Exp -> ForestTy -> EnvQ (TH.Pat,[Stmt],TH.Exp,[Name])
genLoadArgE (i,(pat,pat_ty)) treeE forestTy = do
	mName <- lift $ newName $ "m"++show i
	let (mE,mP) = genPE mName
	thunkName <- lift $ newName $ "t"++show i
	let (thunkE,thunkP) = genPE thunkName
	let thunkS = BindS thunkP $ AppE (VarE 'icThunk) mE
	return (mP,[thunkS],thunkE,[thunkName])

loadE :: ForestTy -> TH.Exp -> TH.Exp -> TH.Exp -> TH.Exp -> EnvQ TH.Exp
loadE ty filterPathE pathE treeE getMDE = case ty of
	Named ty_name               -> loadWithArgsE ty_name [] filterPathE pathE treeE getMDE
	Fapp (Named ty_name) argEs  -> loadWithArgsE ty_name argEs filterPathE pathE treeE getMDE
	File (file_name, argEOpt) -> loadFile file_name argEOpt filterPathE pathE treeE getMDE
	Archive archtype ty         -> loadArchive archtype ty filterPathE pathE treeE getMDE
	FSymLink         -> loadSymLink filterPathE pathE treeE getMDE
	FConstraint p ty pred -> loadConstraint treeE p pred $ loadE ty filterPathE pathE treeE getMDE
	Directory dirTy -> loadDirectory dirTy filterPathE pathE treeE getMDE
	FMaybe forestTy -> loadMaybe forestTy filterPathE pathE treeE getMDE
	FComp cinfo     -> loadComp cinfo filterPathE pathE treeE getMDE

proxyFileName :: String -> TH.Exp
proxyFileName name = SigE (ConE 'Proxy) (AppT (ConT ''Proxy) $ ConT $ mkName name)

loadFile :: String -> Maybe TH.Exp -> TH.Exp -> TH.Exp -> TH.Exp -> TH.Exp -> EnvQ TH.Exp
loadFile fileName Nothing pathFilterE pathE treeE getMDE = do
	let proxy = proxyFileName fileName
	return $ Pure.appE5 (VarE 'doLoadFile) proxy pathFilterE pathE treeE getMDE
loadFile fileName (Just argE) pathFilterE pathE treeE getMDE = do
	let proxy = proxyFileName fileName
	return $ Pure.appE6 (VarE 'doLoadFile1) proxy argE pathFilterE pathE treeE getMDE

-- these are terminals in the spec
loadWithArgsE :: String -> [TH.Exp] -> TH.Exp -> TH.Exp -> TH.Exp -> TH.Exp -> EnvQ TH.Exp
loadWithArgsE ty_name [] filterPathE pathE treeE getMDE = do
	let proxyE = AppE (VarE 'proxyOf) $ TupE []
	(mode,fs,_) <- Reader.ask
	return $ Pure.appE7 (VarE 'loadScratchMemo) (VarE mode) proxyE (TupE []) filterPathE pathE treeE getMDE
loadWithArgsE ty_name argEs filterPathE pathE treeE getMDE = do
	(mode,fs,_) <- Reader.ask
	argsE <- mapM (\e -> forceVarsEnvQ e return) argEs
	let tupArgsE = foldl1' (Pure.appE2 (ConE '(:*:))) argsE
	return $ Pure.appE7 (VarE 'loadScratchMemo) (VarE mode) (VarE $ mkName $ "proxyArgs_"++ty_name) tupArgsE filterPathE pathE treeE getMDE

loadConstraint :: TH.Exp -> TH.Pat -> TH.Exp -> EnvQ TH.Exp -> EnvQ TH.Exp
loadConstraint treeE pat predE load = forceVarsEnvQ predE $ \predE' -> do
	(mode,fs,_) <- Reader.ask
	let predFnE = modPredE pat predE'
	loadAction <- load
	return $ Pure.appE4 (VarE 'doLoadConstraint) (VarE mode) treeE predFnE loadAction

loadSymLink :: TH.Exp -> TH.Exp -> TH.Exp -> TH.Exp -> EnvQ TH.Exp
loadSymLink filterPathE pathE treeE getMDE = do
  return $ Pure.appE4 (VarE 'doLoadSymLink) filterPathE pathE treeE getMDE

loadArchive :: [ArchiveType] -> ForestTy -> TH.Exp -> TH.Exp -> TH.Exp -> TH.Exp -> EnvQ TH.Exp
loadArchive archtype ty filterPathE pathE treeE getMDE = do
	newPathName <- lift $ newName "new_path"
	newdfName <- lift $ newName "new_df"
	newTreeName <- lift $ newName "new_tree"
	newTreeName' <- lift $ newName "new_tree'"
	newGetMDName <- lift $ newName "newGetMD"
	newDPathName <- lift $ newName "new_dpath"
	newRepMdName <- lift $ newName "new_repmd"
	let (newPathE, newPathP) = genPE newPathName
	let (newdfE, newdfP) = genPE newdfName
	let (newTreeE, newTreeP) = genPE newTreeName
	let (newTreeE', newTreeP') = genPE newTreeName'
	let (newGetMDE, newGetMDP) = genPE newGetMDName
	let (newDPathE, newDPathP) = genPE newDPathName
	let (newRepMdE, newRepMdP) = genPE newRepMdName
	rhsE <- liftM (LamE [newPathP,newGetMDP,newTreeP']) $ loadE ty filterPathE newPathE newTreeE' newGetMDE
	rhsDE <- liftM (LamE [newPathP,newDPathP,newRepMdP,newTreeP,newdfP,newTreeP']) $ runDeltaQ (loadDeltaE ty newPathE newTreeE newRepMdE newDPathE newdfE newTreeE')
	exts <- lift $ dataToExpQ (\_ -> Nothing) archtype
	return $ Pure.appE7 (VarE 'doLoadArchive) exts filterPathE pathE treeE getMDE rhsE rhsDE

loadMaybe :: ForestTy -> TH.Exp -> TH.Exp -> TH.Exp -> TH.Exp -> EnvQ TH.Exp
loadMaybe ty filterPathE pathE treeE getMDE = do 
   rhsE <- loadE ty filterPathE pathE treeE getMDE
   return $ Pure.appE4 (VarE 'doLoadMaybe) filterPathE pathE treeE rhsE

loadDirectory :: DirectoryTy -> TH.Exp -> TH.Exp -> TH.Exp -> TH.Exp -> EnvQ TH.Exp
loadDirectory dirTy@(Record id fields) filterPathE pathE treeE getMDE = do
	doDirE <- loadDirectoryContents dirTy filterPathE pathE treeE getMDE
	collectMDs <- lift $ genMergeFieldsMDErrors fields	
	return $ Pure.appE5 (VarE 'doLoadDirectory) pathE treeE collectMDs getMDE doDirE

loadDirectoryContents :: DirectoryTy -> TH.Exp -> TH.Exp -> TH.Exp -> TH.Exp -> EnvQ TH.Exp
loadDirectoryContents (Record id fields) filterPathE pathE treeE getMDE = do
	(repEs,mdEs,stmts) <- loadFields fields filterPathE pathE treeE getMDE
	let tyName = mkName id
	let repE = Pure.appConE (Pure.getStructInnerName tyName) $ map VarE repEs
	let mdE = Pure.appConE (Pure.getStructInnerMDName tyName) $ map VarE mdEs
	let resultE = TupE [repE,mdE]
	let finalS = NoBindS $ AppE (VarE 'return) resultE
	let doDirE = DoE $ stmts ++ [finalS]
	return doDirE

loadFields :: [Field] -> TH.Exp -> TH.Exp -> TH.Exp -> TH.Exp -> EnvQ ([Name],[Name],[Stmt])
loadFields [] filterPathE pathE treeE getMDE = return ([],[],[])
loadFields (field:fields) filterPathE pathE treeE getMDE = do
	(rep_field,   md_field, stmts_field)  <- loadField field filterPathE pathE treeE getMDE
	let update (mode,fs,env) = (mode,fs,Map.insert rep_field Nothing $ Map.insert md_field Nothing env)
	(reps_fields, md_fields, stmts_fields) <- Reader.local update $ loadFields fields filterPathE pathE treeE getMDE
	return (rep_field:reps_fields, md_field:md_fields, stmts_field++stmts_fields)

loadField :: Field -> TH.Exp -> TH.Exp -> TH.Exp -> TH.Exp -> EnvQ (Name,Name, [Stmt])
loadField field filterPathE pathE treeE getMDE = case field of
	Simple s -> loadSimple s filterPathE pathE treeE getMDE
	Comp   c -> loadCompound True c filterPathE pathE treeE getMDE

loadSimple :: BasicField -> TH.Exp -> TH.Exp -> TH.Exp -> TH.Exp -> EnvQ (Name,Name, [Stmt])
loadSimple (internal, isForm, externalE, forestTy, predM) filterPathE pathE treeE getMDE = do
	-- variable declarations
	let repName = mkName internal
	let mdName  = mkName (internal++"_md")
	let (repE,repP) = genPE repName
	let (mdE,mdP) = genPE mdName
	newpathName <- lift $ newName "newpath"
	let (newpathE,newpathP) = genPE newpathName
	newGetMdName <- lift $ newName "newGetMD"
	let (newGetMdE,newGetMdP) = genPE newGetMdName
	newdfName <- lift $ newName "newdf"
	let (newdfE,newdfP) = genPE newdfName
	
	loadFocusE <- do
		(mode,fs,_) <- Reader.ask
		loadContentE <- liftM (LamE [newpathP,newGetMdP]) $ loadE forestTy filterPathE newpathE treeE newGetMdE
		case predM of
			Nothing -> return $ Pure.appE5 (VarE 'doLoadSimple) filterPathE pathE externalE treeE loadContentE
			Just pred -> do
				return $ Pure.appE7 (VarE 'doLoadSimpleWithConstraint) (VarE mode) filterPathE pathE externalE treeE (modPredE (VarP repName) pred) loadContentE
	let stmt1 = BindS (TildeP $ TupP [repP,mdP]) loadFocusE
	return (repName,mdName,[stmt1])

-- | Load a top-level declared comprehension
loadComp :: CompField -> TH.Exp -> TH.Exp -> TH.Exp -> TH.Exp -> EnvQ TH.Exp
loadComp cinfo filterPathE pathE treeE getMDE = do
	let collectMDs = genMergeFieldMDErrors (Comp cinfo)
	doCompE <- loadCompContents cinfo filterPathE pathE treeE getMDE
	return $ Pure.appE5 (VarE 'doLoadDirectory) pathE treeE collectMDs getMDE doCompE
	
-- | Load a top-level declared comprehension
loadCompContents :: CompField -> TH.Exp -> TH.Exp -> TH.Exp -> TH.Exp -> EnvQ TH.Exp
loadCompContents cinfo filterPathE pathE treeE getMDE = do
	(_,_,stmts) <- loadCompound False cinfo filterPathE pathE treeE getMDE
	let doCompE = DoE $ init stmts ++ [Pure.unBindS (last stmts)]
	return doCompE

-- | Load a comprehension inlined inside a @Directory@
-- if a comprehension is nested inside a directory, we created a top-level metadata thunk for it, otherwise the thunk already exists
loadCompound :: Bool -> CompField -> TH.Exp -> TH.Exp -> TH.Exp -> TH.Exp -> EnvQ (Name,Name,[Stmt])
loadCompound isNested (CompField internal tyConNameOpt explicitName externalE descTy generatorP generatorG predM) filterPathE pathE treeE getMDE = do
	-- variable declarations
	let repName = mkName internal
	let mdName  = mkName (internal++"_md")
	newpathName <- lift $ newName "newpath"
	newdfName <- lift $ newName "newdf"
	newGetMDName <- lift $ newName "newgetMD"
	let (repE,repP) = genPE repName
	let (mdE,mdP) = genPE mdName
	let (newpathE,newpathP) = genPE newpathName
	let (newGetMDE,newGetMDP) = genPE newGetMDName	
	let (newdfE,newdfP) = genPE newdfName
	
	let genE = case generatorG of
		Explicit expE -> expE
		Matches regexpE -> regexpE
	
	forceVarsEnvQ genE $ \genE -> do
		-- optional filtering
		let fileName = Pure.getCompName explicitName externalE
		let fileNameAtt = mkName $ nameBase fileName++"_att"
		let fileNameAttThunk = mkName $ nameBase fileName++"_att_thunk"
		
		-- build representation and metadata containers from a list
		buildContainerE <- lift $ Pure.tyConNameOptBuild tyConNameOpt
		
		let update (mode,fs,env) = (mode,fs,Map.insert fileName Nothing $ Map.insert fileNameAtt (Just (fileNameAttThunk,VarP fileNameAtt)) env)  --force the @FileInfo@ thunk
		
		-- container loading
		(mode,fs,_) <- Reader.ask
		Reader.local update $ case predM of
			Nothing -> do
				loadSingleE <- liftM (LamE [VarP fileName,VarP fileNameAttThunk,newpathP,newGetMDP]) $ loadE descTy filterPathE newpathE treeE newGetMDE
				let loadContainerE = Pure.appE8 (VarE 'doLoadCompound) (VarE mode) filterPathE pathE genE treeE buildContainerE buildContainerE loadSingleE
				let loadContainerS = BindS (TupP [VarP repName,VarP mdName]) loadContainerE
				return (repName,mdName,[loadContainerS])
				
			Just predE -> forceVarsEnvQ predE $ \predE -> do
				loadSingleE <- liftM (LamE [VarP fileName,VarP fileNameAttThunk,newpathP,newGetMDP]) $ loadE descTy filterPathE newpathE treeE newGetMDE
				let loadContainerE = Pure.appE9 (VarE 'doLoadCompoundWithConstraint) (VarE mode) filterPathE pathE genE treeE (modPredEComp (VarP fileName) predE) buildContainerE buildContainerE loadSingleE
				let loadContainerS = BindS (TupP [VarP repName,VarP mdName]) loadContainerE
				return (repName,mdName,[loadContainerS])



