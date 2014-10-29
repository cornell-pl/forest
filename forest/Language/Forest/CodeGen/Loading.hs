{-# LANGUAGE TupleSections, TemplateHaskell, NamedFieldPuns, ScopedTypeVariables, RecordWildCards, FlexibleInstances, MultiParamTypeClasses,
    UndecidableInstances, ViewPatterns  #-}

module Language.Forest.CodeGen.Loading where

import {-# SOURCE #-} Language.Forest.CodeGen.DeltaLoading

import Prelude hiding (const,read)
import Language.Forest.CodeGen.Utils
import Control.Monad.Trans
import Control.Monad.Incremental
import Language.Haskell.TH.Quote

import Language.Forest.Syntax as PS
import Language.Forest.MetaData
import Language.Forest.Errors
import Language.Forest.Generic
import qualified Language.Forest.Errors as E
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

-- receives an expression with thunks, and returns an expression that forces the thunks to get the corresponding values
forceVarsEnvQ :: Exp -> (Exp -> EnvQ a) -> EnvQ a
forceVarsEnvQ e f = do
	let expvars = expVars e
	let forceVar var f = \e -> do
		envvars <- Reader.ask
		case Map.lookup var envvars of
			Just (Just (thunk,pat)) -> do
				
				let update env = Set.foldr (\var env -> Map.insert var Nothing env) env (patPVars pat)
				Reader.local update $ f $ UInfixE (AppE (VarE 'read) (VarE thunk)) (VarE '(>>=)) (LamE [pat] e)
			otherwise -> f e
	(Set.foldr forceVar f expvars) e

genLoadM :: Name -> Name -> ForestTy -> [(TH.Pat,TH.Type)] -> EnvQ TH.Exp
genLoadM rep_name md_name forestTy pat_infos = do
	fsName <- lift $ newName "fs"
	pathName    <- lift $ newName "path"
	treeName    <- lift $ newName "tree"
	oldtreeName    <- lift $ newName "oldtree"
	getMDName    <- lift $ newName "getMD"
	dfName <- lift $ newName "df"
	let (pathE, pathP) = genPE pathName
	let (treeE, treeP) = genPE treeName
	let (oldtreeE, oldtreeP) = genPE oldtreeName
	let (dfE, dfP) = genPE dfName
	let (getMDE, getMDP) = genPE getMDName
	let (fsE, fsP) = genPE fsName
	
	case pat_infos of
		[] -> do
			core_bodyE <- genLoadBody pathE oldtreeE dfE treeE getMDE rep_name forestTy
			return $ LamE [TupP [],pathP,oldtreeP,dfP,treeP,getMDP] core_bodyE
		otherwise -> do
			(argsP,stmts,argsThunksE,argThunkNames) <- genLoadArgsE (zip [1..] pat_infos) treeE forestTy
			let update env = foldl updatePat env (zip argThunkNames pat_infos)
				where updatePat env (thunkName,(pat,ty)) = Map.fromSet (\var -> Just (thunkName,pat)) (patPVars pat) `Map.union` env --left-biased union
			Reader.local update $ do -- adds pattern variable deltas to the env.
				let addArgsE = AppE (VarE 'rtupM) argsThunksE
				core_bodyE <- genLoadBody pathE oldtreeE dfE treeE getMDE rep_name forestTy
				return $ LamE [argsP,pathP,oldtreeP,dfP,treeP,getMDP] $ --DoE $ stmts ++ [NoBindS core_bodyE]
					DoE $ stmts ++ [NoBindS $ UInfixE (InfixE (Just $ VarE 'return) (VarE '(>><<)) $ Just addArgsE) (VarE '(=<<)) core_bodyE]

{-
 Generate body of loadM function, which has the form:
  do (rep,md) <- rhsE
     return (Rep rep, md)
-}
genLoadBody :: TH.Exp -> TH.Exp -> TH.Exp -> TH.Exp -> TH.Exp -> Name -> ForestTy -> EnvQ TH.Exp
genLoadBody pathE oldtreeE dfE treeE getMDE repN ty = case ty of 
	Directory _ -> loadE ty pathE oldtreeE dfE treeE getMDE
	FConstraint _ (Directory _) _ -> loadE ty pathE oldtreeE dfE treeE getMDE
	otherwise   -> do -- Add type constructor
		rhsE <- loadE ty pathE oldtreeE dfE treeE getMDE
		return $ appE2 (VarE 'liftM) (InfixE (Just $ ConE repN) (VarE '(><)) (Just $ VarE 'id)) rhsE 

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
	let thunkS = BindS thunkP $ AppE (VarE 'thunk) mE
	return (mP,[thunkS],thunkE,[thunkName])

loadE :: ForestTy -> TH.Exp -> TH.Exp -> TH.Exp -> TH.Exp -> TH.Exp -> EnvQ TH.Exp
loadE ty pathE oldtreeE dfE treeE getMDE = case ty of
	Named f_name               -> loadWithArgsE [] pathE oldtreeE dfE treeE getMDE
	Fapp (Named f_name) argEs  -> loadWithArgsE argEs pathE oldtreeE dfE treeE getMDE
	File (file_name, argEOpt) -> loadFile file_name argEOpt pathE oldtreeE dfE treeE getMDE
	Archive archtype ty         -> loadArchive archtype ty pathE oldtreeE dfE treeE getMDE
	SymLink         -> loadSymLink pathE oldtreeE dfE treeE getMDE
	FConstraint p ty pred -> loadConstraint treeE p pred $ loadE ty pathE oldtreeE dfE treeE getMDE
	Directory dirTy -> loadDirectory dirTy pathE oldtreeE dfE treeE getMDE
	FMaybe forestTy -> loadMaybe forestTy pathE oldtreeE dfE treeE getMDE
	FComp cinfo     -> loadComp cinfo pathE oldtreeE dfE treeE getMDE

proxyFileName :: String -> TH.Exp
proxyFileName name = SigE (ConE 'Proxy) (AppT (ConT ''Proxy) $ ConT $ mkName name)

loadFile :: String -> Maybe TH.Exp -> TH.Exp -> TH.Exp -> TH.Exp -> TH.Exp -> TH.Exp -> EnvQ TH.Exp
loadFile fileName Nothing pathE oldtreeE dfE treeE getMDE = do
	let proxy = proxyFileName fileName
	return $ appE6 (VarE 'doLoadFile) proxy pathE oldtreeE dfE treeE getMDE
loadFile fileName (Just argE) pathE oldtreeE dfE treeE getMDE = do
	let proxy = proxyFileName fileName
	return $ appE7 (VarE 'doLoadFile1) proxy argE pathE oldtreeE dfE treeE getMDE

-- these are terminals in the spec
loadWithArgsE :: [TH.Exp] -> TH.Exp -> TH.Exp -> TH.Exp -> TH.Exp -> TH.Exp -> EnvQ TH.Exp
loadWithArgsE [] pathE oldtreeE dfE treeE getMDE = return $ appE6 (VarE 'loadScratch) (TupE []) pathE oldtreeE dfE treeE getMDE
loadWithArgsE argsE pathE oldtreeE dfE treeE getMDE = do
	argsE <- mapM (\e -> forceVarsEnvQ e return) argsE
	let tupArgsE = foldl1' (appE2 (ConE '(:*:))) argsE
	return $ appE6 (VarE 'loadScratch) tupArgsE pathE oldtreeE dfE treeE getMDE

loadConstraint :: TH.Exp -> TH.Pat -> TH.Exp -> EnvQ TH.Exp -> EnvQ TH.Exp
loadConstraint treeE pat predE load = forceVarsEnvQ predE $ \predE' -> do
	let predFnE = modPredE pat predE'
	loadAction <- load
	return $ appE3 (VarE 'doLoadConstraint) treeE predFnE loadAction

loadSymLink :: TH.Exp -> TH.Exp -> TH.Exp -> TH.Exp -> TH.Exp -> EnvQ TH.Exp
loadSymLink pathE oldtreeE dfE treeE getMDE = do
  return $ appE5 (VarE 'doLoadSymLink) pathE oldtreeE dfE treeE getMDE

loadArchive :: [ArchiveType] -> ForestTy -> TH.Exp -> TH.Exp -> TH.Exp -> TH.Exp -> TH.Exp -> EnvQ TH.Exp
loadArchive archtype ty pathE oldtreeE dfE treeE getMDE = do
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
	rhsE <- liftM (LamE [newPathP,newGetMDP,newTreeP,newdfP,newTreeP']) $ loadE ty newPathE newTreeE newdfE newTreeE' newGetMDE
	rhsDE <- liftM (LamE [newPathP,newDPathP,newRepMdP,newTreeP,newdfP,newTreeP']) $ runDeltaQ (loadDeltaE ty newPathE newTreeE newRepMdE newDPathE newdfE newTreeE')
	exts <- lift $ dataToExpQ (\_ -> Nothing) archtype
	return $ appE8 (VarE 'doLoadArchive) exts pathE oldtreeE dfE treeE getMDE rhsE rhsDE

loadMaybe :: ForestTy -> TH.Exp -> TH.Exp -> TH.Exp -> TH.Exp -> TH.Exp -> EnvQ TH.Exp
loadMaybe ty pathE oldtreeE dfE treeE getMDE = do 
   rhsE <- loadE ty pathE oldtreeE dfE treeE getMDE
   return $ appE4 (VarE 'doLoadMaybe) pathE dfE treeE rhsE

loadDirectory :: DirectoryTy -> TH.Exp -> TH.Exp -> TH.Exp -> TH.Exp -> TH.Exp -> EnvQ TH.Exp
loadDirectory dirTy@(Record id fields) pathE oldtreeE dfE treeE getMDE = do
	doDirE <- loadDirectoryContents dirTy pathE oldtreeE dfE treeE getMDE
	collectMDs <- lift $ genMergeFieldsMDErrors fields	
	return $ appE5 (VarE 'doLoadDirectory) pathE treeE collectMDs getMDE doDirE

loadDirectoryContents :: DirectoryTy -> TH.Exp -> TH.Exp -> TH.Exp -> TH.Exp -> TH.Exp -> EnvQ TH.Exp
loadDirectoryContents (Record id fields) pathE oldtreeE dfE treeE getMDE = do
	(repEs,mdEs,stmts) <- loadFields fields pathE oldtreeE dfE treeE getMDE
	let tyName = mkName id
	let repE = appConE (getStructInnerName tyName) $ map VarE repEs
	let mdE = appConE (getStructInnerMDName tyName) $ map VarE mdEs
	let resultE = TupE [repE,mdE]
	let finalS = NoBindS $ AppE (VarE 'return) resultE
	let doDirE = DoE $ stmts ++ [finalS]
	return doDirE

loadFields :: [Field] -> TH.Exp -> TH.Exp -> TH.Exp -> TH.Exp -> TH.Exp -> EnvQ ([Name],[Name],[Stmt])
loadFields [] pathE oldtreeE dfE treeE getMDE = return ([],[],[])
loadFields (field:fields) pathE oldtreeE dfE treeE getMDE = do
	(rep_field,   md_field, stmts_field)  <- loadField field pathE oldtreeE dfE treeE getMDE
	let update = Map.insert rep_field Nothing . Map.insert md_field Nothing
	(reps_fields, md_fields, stmts_fields) <- Reader.local update $ loadFields fields pathE oldtreeE dfE treeE getMDE
	return (rep_field:reps_fields, md_field:md_fields, stmts_field++stmts_fields)

loadField :: Field -> TH.Exp -> TH.Exp -> TH.Exp -> TH.Exp -> TH.Exp -> EnvQ (Name,Name, [Stmt])
loadField field pathE oldtreeE dfE treeE getMDE = case field of
	Simple s -> loadSimple s pathE oldtreeE dfE treeE getMDE
	Comp   c -> loadCompound True c pathE oldtreeE dfE treeE getMDE

loadSimple :: BasicField -> TH.Exp -> TH.Exp -> TH.Exp -> TH.Exp -> TH.Exp -> EnvQ (Name,Name, [Stmt])
loadSimple (internal, isForm, externalE, forestTy, predM) pathE oldtreeE dfE treeE getMDE = do
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
		loadContentE <- liftM (LamE [newpathP,newdfP,newGetMdP]) $ loadE forestTy newpathE oldtreeE newdfE treeE newGetMdE
		case predM of
			Nothing -> return $ appE6 (VarE 'doLoadSimple) pathE externalE oldtreeE dfE treeE loadContentE
			Just pred -> return $ appE7 (VarE 'doLoadSimpleWithConstraint) pathE externalE oldtreeE dfE treeE (modPredE (VarP repName) pred) loadContentE
	let stmt1 = BindS (TildeP $ TupP [repP,mdP]) loadFocusE
	return (repName,mdName,[stmt1])

-- | Load a top-level declared comprehension
loadComp :: CompField -> TH.Exp -> TH.Exp -> TH.Exp -> TH.Exp -> TH.Exp -> EnvQ TH.Exp
loadComp cinfo pathE oldtreeE dfE treeE getMDE = do
	let collectMDs = genMergeFieldMDErrors (Comp cinfo)
	doCompE <- loadCompContents cinfo pathE oldtreeE dfE treeE getMDE
	return $ appE5 (VarE 'doLoadDirectory) pathE treeE collectMDs getMDE doCompE
	
-- | Load a top-level declared comprehension
loadCompContents :: CompField -> TH.Exp -> TH.Exp -> TH.Exp -> TH.Exp -> TH.Exp -> EnvQ TH.Exp
loadCompContents cinfo pathE oldtreeE dfE treeE getMDE = do
	(_,_,stmts) <- loadCompound False cinfo pathE oldtreeE dfE treeE getMDE
	let doCompE = DoE $ init stmts ++ [unBindS (last stmts)]
	return doCompE

-- | Load a comprehension inlined inside a @Directory@
-- if a comprehension is nested inside a directory, we created a top-level metadata thunk for it, otherwise the thunk already exists
loadCompound :: Bool -> CompField -> TH.Exp -> TH.Exp -> TH.Exp -> TH.Exp -> TH.Exp -> EnvQ (Name,Name,[Stmt])
loadCompound isNested (CompField internal tyConNameOpt explicitName externalE descTy generatorP generatorG predM) pathE oldtreeE dfE treeE getMDE = do
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
		let fileName = getCompName explicitName externalE
		let fileNameAtt = mkName $ nameBase fileName++"_att"
		let fileNameAttThunk = mkName $ nameBase fileName++"_att_thunk"
		
		-- build representation and metadata containers from a list
		buildContainerE <- lift $ tyConNameOptBuild tyConNameOpt
		
		let update = Map.insert fileName Nothing . Map.insert fileNameAtt (Just (fileNameAttThunk,VarP fileNameAtt))  --force the @FileInfo@ thunk
		
		-- container loading
		Reader.local update $ case predM of
			Nothing -> do
				loadSingleE <- liftM (LamE [VarP fileName,VarP fileNameAttThunk,newpathP,newdfP,newGetMDP]) $ loadE descTy newpathE oldtreeE newdfE treeE newGetMDE
				let loadContainerE = appE8 (VarE 'doLoadCompound) pathE genE oldtreeE dfE treeE buildContainerE buildContainerE loadSingleE
				let loadContainerS = BindS (TupP [VarP repName,VarP mdName]) loadContainerE
				return (repName,mdName,[loadContainerS])
				
			Just predE -> forceVarsEnvQ predE $ \predE -> do
				loadSingleE <- liftM (LamE [VarP fileName,VarP fileNameAttThunk,newpathP,newdfP,newGetMDP]) $ loadE descTy newpathE oldtreeE newdfE treeE newGetMDE
				let loadContainerE = appE9 (VarE 'doLoadCompoundWithConstraint) pathE genE oldtreeE dfE treeE (modPredEComp (VarP fileName) predE) buildContainerE buildContainerE loadSingleE
				let loadContainerS = BindS (TupP [VarP repName,VarP mdName]) loadContainerE
				return (repName,mdName,[loadContainerS])



