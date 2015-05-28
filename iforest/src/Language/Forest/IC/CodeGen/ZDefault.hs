{-# LANGUAGE TupleSections, TemplateHaskell, NamedFieldPuns, ScopedTypeVariables, RecordWildCards, FlexibleInstances, MultiParamTypeClasses,
    UndecidableInstances, ViewPatterns  #-}

module Language.Forest.IC.CodeGen.ZDefault where

import Language.Forest.IC.BX as BX
import Prelude hiding (const,read)
import Language.Forest.IC.CodeGen.Utils
--import Language.Forest.IC.IO.Default
import {-# SOURCE #-} Language.Forest.IC.CodeGen.ZDeltaLoading
import Control.Monad.Trans
import Language.Haskell.TH.Quote
import Language.Forest.IC.Generic
import Language.Forest.Syntax as PS
--import Language.Forest.MetaData
import Language.Forest.Errors
--import Language.Forest.Generic as Forest
import qualified Language.Forest.Errors as E
import Language.Forest.IO.Utils
import System.Directory
import System.FilePath.Posix
import Control.Monad.Reader (Reader(..),ReaderT(..))
import qualified Control.Monad.Reader as Reader
--import qualified Language.Forest.CodeGen.Utils as Pure
--import Language.Forest.MetaData as Pure
import Language.Forest.IC.ICRep

import Language.Haskell.TH as TH
import Language.Haskell.TH.Syntax hiding (lift)
import Language.Pads.Padsc hiding (lift)
import Language.Pads.TH
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
import Language.Forest.IC.IO.ZDefault
import Control.Monad.Incremental as Inc
import Data.WithClass.MData

-- receives an expression with thunks, and returns an expression that forces the thunks to get the corresponding values
forceVarsZEnvQ :: Exp -> (Exp -> ZEnvQ a) -> ZEnvQ a
forceVarsZEnvQ e f = do
	let expvars = expVars e
	let forceVar var f = \e -> do
		(fs,envvars) <- Reader.ask
		case Map.lookup var envvars of
			Just (Just (thunk,pat)) -> do
				
				let update (fs,env) = (fs,Set.foldr (\var env -> Map.insert var Nothing env) env (patPVars pat))
				Reader.local update $ f $ UInfixE (AppE (VarE 'Inc.read) (VarE thunk)) (VarE '(>>=)) (LamE [pat] e)
			otherwise -> f e
	(Set.foldr forceVar f expvars) e

genZDefaultM :: Bool -> Name -> ForestTy -> [(TH.Pat,TH.Type)] -> ZEnvQ Exp
genZDefaultM isTop rep_name forestTy pat_infos = do
	(fsName,_) <- Reader.ask
	pathName    <- lift $ newName "path"
	treeName    <- lift $ newName "tree"
	argsName <- lift $ newName "args"
	let (pathE, pathP) = genPE pathName
	let (treeE, treeP) = genPE treeName
	let (fsE, fsP) = genPE fsName
	
	case pat_infos of
		[] -> do
			core_bodyE <- genZDefaultBody isTop pathE treeE rep_name forestTy
			return $ LamE [VarP argsName,SigP (ConP 'Proxy []) (AppT (ConT ''Proxy) (VarT fsName)),TupP [],pathP,treeP] core_bodyE
		otherwise -> do
			(argsP,stmts,argsThunksE,argThunkNames) <- genZDefaultArgsE (zip [1..] pat_infos) forestTy
			let update (fs,env) = (fs,foldl updatePat env (zip argThunkNames pat_infos))
				where updatePat env (thunkName,(pat,ty)) = Map.fromSet (\var -> Just (thunkName,pat)) (patPVars pat) `Map.union` env --left-biased union
			Reader.local update $ do -- adds pattern variable deltas to the env.
				(fs,_) <- Reader.ask
				core_bodyE <- genZDefaultBody isTop pathE treeE rep_name forestTy
				return $ LamE [VarP argsName,SigP (ConP 'Proxy []) (AppT (ConT ''Proxy) (VarT fsName)),argsP,pathP,treeP] $
					DoE $ stmts ++ [NoBindS core_bodyE]

{-
 Generate body of loadM function, which has the form:
  do (rep,md) <- rhsE
     return (Rep rep, md)
-}
genZDefaultBody :: Bool -> Exp -> Exp -> Name -> ForestTy -> ZEnvQ Exp
genZDefaultBody isTop pathE treeE repN ty = case ty of 
	Directory _ -> zdefaultE isTop ty pathE treeE 
	FConstraint _ (Directory _) _ -> zdefaultE isTop ty pathE treeE
	otherwise   -> do -- Add type constructor
		rhsE <- zdefaultE isTop ty pathE treeE
		return $ appE2 (VarE 'liftM) (ConE repN) rhsE

-- adds top-level arguments to the metadata
genZDefaultArgsE :: [(Int,(TH.Pat,TH.Type))] -> ForestTy -> ZEnvQ (TH.Pat,[Stmt],Exp,[Name])
genZDefaultArgsE [pat_info] forestTy = genZDefaultArgE pat_info forestTy
genZDefaultArgsE (pat_info:pat_infos) forestTy = do
	(pat,stmt,expr,names1) <- genZDefaultArgE pat_info forestTy
	(pats,stmts,exprs,names2) <- genZDefaultArgsE pat_infos forestTy
	return (ConP '(:*:) [pat,pats],stmt++stmts,UInfixE expr (ConE '(:*:)) exprs,names1++names2)

--we do not force thunks, and leave that for whenever their values are required inside expressions
genZDefaultArgE :: (Int,(TH.Pat,TH.Type)) -> ForestTy -> ZEnvQ (TH.Pat,[Stmt],Exp,[Name])
genZDefaultArgE (i,(pat,pat_ty)) forestTy = do
	mName <- lift $ newName $ "m"++show i
	let (mE,mP) = genPE mName
	thunkName <- lift $ newName $ "t"++show i
	let (thunkE,thunkP) = genPE thunkName
	let thunkS = BindS thunkP $ AppE (VarE 'icThunk) mE
	return (mP,[thunkS],thunkE,[thunkName])

zdefaultE :: Bool -> ForestTy -> Exp -> Exp -> ZEnvQ Exp
zdefaultE isTop ty pathE treeE = case ty of
	Named f_name               -> zdefaultWithArgsE isTop f_name [] pathE treeE 
	Fapp (Named f_name) argEs  -> zdefaultWithArgsE isTop f_name argEs pathE treeE 
	FFile (file_name, argEOpt) -> zdefaultFile isTop file_name argEOpt pathE treeE
	Archive archtype ty         -> zdefaultArchive isTop archtype ty pathE treeE 
	FSymLink         -> zdefaultSymLink isTop pathE treeE 
	FConstraint p ty pred -> zdefaultConstraint isTop p pred treeE $ zdefaultE False ty pathE treeE
	Directory dirTy -> zdefaultDirectory isTop dirTy pathE treeE
	FMaybe forestTy -> zdefaultMaybe isTop forestTy pathE treeE
	FComp cinfo     -> zdefaultComp isTop cinfo pathE treeE

zdefaultConstraint :: Bool -> TH.Pat -> Exp -> Exp -> ZEnvQ Exp -> ZEnvQ Exp
zdefaultConstraint isTop pat predE treeE load = forceVarsZEnvQ predE $ \predE' -> do
	(fs,_) <- Reader.ask
	let predFnE = zmodPredE pat predE'
	loadAction <- load
	if isTop
		then return $ appE3 (VarE 'doZDefaultConstraint) predFnE treeE loadAction
		else return $ appE3 (VarE 'doZDefaultConstraintInner) predFnE treeE loadAction

zdefaultFile :: Bool -> String -> Maybe Exp -> Exp -> Exp -> ZEnvQ Exp
zdefaultFile isTop fileName Nothing pathE treeE = do
	(fs,_) <- Reader.ask
	if isTop
		then return $ appE4 (VarE 'doZDefaultFile1) (proxyN fs) (returnExp $ TupE []) pathE treeE
		else return $ appE3 (VarE 'doZDefaultFileInner1) (returnExp $ TupE []) pathE treeE
zdefaultFile isTop fileName (Just argE) pathE treeE = do
	(fs,_) <- Reader.ask
	if isTop
		then return $ appE4 (VarE 'doZDefaultFile1) (proxyN fs) argE pathE treeE
		else return $ appE3 (VarE 'doZDefaultFileInner1) argE pathE treeE

zdefaultWithArgsE :: Bool -> String -> [Exp] -> Exp -> Exp -> ZEnvQ Exp
zdefaultWithArgsE isTop ty_name [] pathE treeE = do
	(fs,_) <- Reader.ask
	let proxyE = SigE (ConE 'Proxy) $ AppT (ConT ''Proxy) $ AppT (ConT $ mkName ty_name) (VarT fs)
	let load = if isTop then VarE 'doZDefaultNamed else VarE 'zdefaultScratchGeneric
	return $ appE5 load proxyE (proxyN fs) (TupE []) pathE treeE
zdefaultWithArgsE isTop ty_name argEs pathE treeE = do
	(fs,_) <- Reader.ask
	let proxyE = SigE (ConE 'Proxy) $ AppT (ConT ''Proxy) $ AppT (ConT $ mkName ty_name) (VarT fs)
	argsE <- mapM (\e -> forceVarsZEnvQ e return) argEs
	let tupArgsE = foldl1' (appE2 (ConE '(:*:))) argsE
	let load = if isTop then VarE 'doZDefaultNamed else VarE 'zdefaultScratchGeneric
	return $ appE5 load proxyE (proxyN fs) tupArgsE pathE treeE

zdefaultSymLink :: Bool -> Exp -> Exp -> ZEnvQ Exp
zdefaultSymLink isTop pathE treeE = do
	if isTop
		then return $ appE2 (VarE 'doZDefaultSymLink) pathE treeE
		else return $ appE2 (VarE 'doZDefaultSymLinkInner) pathE treeE

zdefaultArchive :: Bool -> [ArchiveType] -> ForestTy -> Exp -> Exp -> ZEnvQ Exp
zdefaultArchive isTop archtype ty pathE treeE = do
	newPathName <- lift $ newName "new_path"
	let (newPathE, newPathP) = genPE newPathName
	rhsE <- liftM (LamE [newPathP]) $ zdefaultE False ty newPathE treeE
	exts <- lift $ dataToExpQ (\_ -> Nothing) archtype
	isClosedE <- lift $ dataToExpQ (\_ -> Nothing) $ isClosedForestTy ty
	if isTop
		then return $ appE4 (VarE 'doZDefaultArchive) isClosedE pathE treeE rhsE
		else return $ appE3 (VarE 'doZDefaultArchiveInner) pathE treeE rhsE

zdefaultMaybe :: Bool -> ForestTy -> Exp -> Exp -> ZEnvQ Exp
zdefaultMaybe isTop ty pathE treeE = do 
	rhsE <- zdefaultE False ty pathE treeE
	if isTop
		then return $ appE2 (VarE 'doZDefaultMaybe) pathE treeE
		else return $ appE2 (VarE 'doZDefaultMaybeInner) pathE treeE

zdefaultDirectory :: Bool -> DirectoryTy -> Exp -> Exp -> ZEnvQ Exp
zdefaultDirectory isTop dirTy@(Record id fields) pathE treeE = do
	doDirE <- zdefaultDirectoryContents dirTy pathE treeE
	collectMDs <- lift $ zgenMergeFieldsMDErrors fields	
	if isTop
		then return $ appE4 (VarE 'doZDefaultDirectory) pathE treeE collectMDs doDirE
		else return $ appE4 (VarE 'doZDefaultDirectory') pathE treeE collectMDs doDirE

zdefaultDirectoryContents :: DirectoryTy -> Exp -> Exp -> ZEnvQ Exp
zdefaultDirectoryContents (Record id fields) pathE treeE = do
	(repNs,repEs,stmts) <- zdefaultFields fields pathE treeE
	let tyName = mkName id
	let repE = appConE (getStructInnerName tyName) $ map VarE repNs
	let resultE = TupE [repE]
	let finalS = NoBindS $ AppE (VarE 'return) resultE
	let doDirE = DoE $ stmts ++ [finalS]
	return doDirE

zdefaultFields :: [Field] -> Exp -> Exp -> ZEnvQ ([Name],[Name],[Stmt])
zdefaultFields [] pathE treeE = return ([],[],[])
zdefaultFields (field:fields) pathE treeE = do
	(rep_name,rep_field, stmts_field)  <- zdefaultField field pathE treeE 
	let update (fs,env) = (fs,Map.insert rep_field Nothing env)
	(reps_names,reps_fields, stmts_fields) <- Reader.local update $ zdefaultFields fields pathE treeE
	return (rep_name:reps_names,rep_field:reps_fields, stmts_field++stmts_fields)

zdefaultField :: Field -> Exp -> Exp -> ZEnvQ (Name,Name, [Stmt])
zdefaultField field pathE treeE = case field of
	Simple s -> zdefaultSimple s pathE treeE 
	Comp   c -> zdefaultCompound True c pathE treeE 

zdefaultSimple :: BasicField -> Exp -> Exp -> ZEnvQ (Name,Name, [Stmt])
zdefaultSimple (internal, isForm, externalE, forestTy, predM) pathE treeE = do
	-- variable declarations
	let repName = mkName internal
	let (repE,repP) = genPE repName
	newpathName <- lift $ newName "newpath"
	let (newpathE,newpathP) = genPE newpathName
	newGetMdName <- lift $ newName "newGetMD"
	let (newGetMdE,newGetMdP) = genPE newGetMdName
	newdfName <- lift $ newName "newdf"
	let (newdfE,newdfP) = genPE newdfName
	xName <- lift $ newName "x"
	let (xE,xP) = genPE xName
	
	loadFocusE <- do
		(fs,_) <- Reader.ask
		loadContentE <- liftM (LamE [newpathP]) $ zdefaultE False forestTy newpathE treeE
		case predM of
			Nothing -> return $ appE4 (VarE 'doZDefaultSimple) pathE treeE externalE loadContentE
			Just pred -> do
				return $ appE5 (VarE 'doZDefaultSimpleWithConstraint) pathE treeE externalE (zmodPredE (VarP repName) pred) loadContentE
	let stmt1 = BindS (TildeP $ TupP [xP]) loadFocusE
	let stmt2 = BindS (VarP repName) (AppE (VarE 'inside) $ appE3 (VarE 'BX.getMTree) (VarE 'lens_content) treeE $ returnExp xE)
	return (xName,repName,[stmt1,stmt2])

-- | Load a top-level declared comprehension
zdefaultComp :: Bool -> CompField -> Exp -> Exp -> ZEnvQ Exp
zdefaultComp isTop cinfo pathE treeE = do
	doCompE <- zdefaultCompContents cinfo pathE treeE
	let collectMDs = zgenMergeFieldMDErrors (Comp cinfo)
	if isTop
		then return $ appE4 (VarE 'doZDefaultDirectory) pathE treeE collectMDs doCompE
		else return $ appE4 (VarE 'doZDefaultDirectory') pathE treeE collectMDs doCompE
	
-- | Load a top-level declared comprehension
zdefaultCompContents :: CompField -> Exp -> Exp -> ZEnvQ Exp
zdefaultCompContents cinfo pathE treeE = do
	(xName,_,stmts) <- zdefaultCompound False cinfo pathE treeE
	let doCompE = DoE $ stmts ++ [NoBindS $ returnExp $ VarE xName]
	return doCompE

-- | Load a comprehension inlined inside a @Directory@
-- if a comprehension is nested inside a directory, we created a top-level metadata thunk for it, otherwise the thunk already exists
zdefaultCompound :: Bool -> CompField -> Exp -> Exp -> ZEnvQ (Name,Name,[Stmt])
zdefaultCompound isNested (CompField internal tyConNameOpt explicitName externalE descTy generatorP generatorTy generatorG predM) pathE treeE = do
	-- variable declarations
	let repName = mkName internal
	let mdName  = mkName (internal++"_md")
	newpathName <- lift $ newName "newpath"
	newdfName <- lift $ newName "newdf"
	newGetMDName <- lift $ newName "newgetMD"
	let (repE,repP) = genPE repName
	let (newpathE,newpathP) = genPE newpathName
	let (newGetMDE,newGetMDP) = genPE newGetMDName	
	let (newdfE,newdfP) = genPE newdfName
	xName <- lift $ newName "x"
	let (xE,xP) = genPE xName
	
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
		buildContainerE <- lift $ tyConNameOptBuild tyConNameOpt
		
		let update (fs,env) = (fs,Map.insert fileName Nothing $ Map.insert fileNameAtt (Just (fileNameAttThunk,VarP fileNameAtt)) env)  --force the @FileInfo@ thunk
		
		-- container loading
		(fs,_) <- Reader.ask
		Reader.local update $ case predM of
			Nothing -> do
				loadSingleE <- liftM (LamE [VarP fileName,VarP fileNameAttThunk,newpathP]) $ zdefaultE False descTy newpathE treeE
				let loadContainerE = appE6 (VarE 'doZDefaultCompound) pathE treeE genE keyArgE buildContainerE loadSingleE
				let loadContainerS = BindS (TupP [xP]) loadContainerE
				let stmt2 = BindS (VarP repName) (AppE (VarE 'inside) $ appE3 (VarE 'BX.getMTree) (VarE 'lens_content) treeE $ returnExp xE)
				return (xName,repName,[loadContainerS,stmt2])
				
			Just predE -> forceVarsZEnvQ predE $ \predE -> do
				loadSingleE <- liftM (LamE [VarP fileName,VarP fileNameAttThunk,newpathP]) $ zdefaultE False descTy newpathE treeE
				let loadContainerE = appE7 (VarE 'doZDefaultCompoundWithConstraint) pathE treeE genE (modPredEComp (VarP fileName) predE) keyArgE buildContainerE loadSingleE
				let loadContainerS = BindS (TupP [xP]) loadContainerE
				let stmt2 = BindS (VarP repName) (AppE (VarE 'inside) $ appE3 (VarE 'BX.getMTree) (VarE 'lens_content) treeE $ returnExp xE)
				return (xName,repName,[loadContainerS,stmt2])

				
		


