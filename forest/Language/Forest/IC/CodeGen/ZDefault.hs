{-# LANGUAGE TupleSections, TemplateHaskell, NamedFieldPuns, ScopedTypeVariables, RecordWildCards, FlexibleInstances, MultiParamTypeClasses,
    UndecidableInstances, ViewPatterns  #-}

module Language.Forest.IC.CodeGen.ZDefault where

import Prelude hiding (const,read)
import Language.Forest.IC.CodeGen.Utils
import Language.Forest.IC.IO.Default
import {-# SOURCE #-} Language.Forest.IC.CodeGen.ZDeltaLoading
import Control.Monad.Trans
import Language.Haskell.TH.Quote
import Language.Forest.IC.Generic
import Language.Forest.Syntax as PS
import Language.Forest.Pure.MetaData
import Language.Forest.Errors
import Language.Forest.Pure.Generic as Forest
import qualified Language.Forest.Errors as E
import Language.Forest.IO.Utils
import System.Directory
import System.FilePath.Posix
import Control.Monad.Reader (Reader(..),ReaderT(..))
import qualified Control.Monad.Reader as Reader
import qualified Language.Forest.Pure.CodeGen.Utils as Pure
import Language.Forest.Pure.MetaData as Pure
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

genZDefaultM :: Name -> ForestTy -> [(TH.Pat,TH.Type)] -> ZEnvQ Exp
genZDefaultM rep_name forestTy pat_infos = do
	fsName <- lift $ newName "fs"
	pathName    <- lift $ newName "path"
	argsName <- lift $ newName "args"
	let (pathE, pathP) = genPE pathName
	let (fsE, fsP) = genPE fsName
	
	case pat_infos of
		[] -> do
			core_bodyE <- genZDefaultBody pathE rep_name forestTy
			return $ LamE [VarP argsName,TupP [],pathP] core_bodyE
		otherwise -> do
			(argsP,stmts,argsThunksE,argThunkNames) <- genZDefaultArgsE (zip [1..] pat_infos) forestTy
			let update (fs,env) = (fs,foldl updatePat env (zip argThunkNames pat_infos))
				where updatePat env (thunkName,(pat,ty)) = Map.fromSet (\var -> Just (thunkName,pat)) (patPVars pat) `Map.union` env --left-biased union
			Reader.local update $ do -- adds pattern variable deltas to the env.
				(fs,_) <- Reader.ask
				core_bodyE <- genZDefaultBody pathE rep_name forestTy
				return $ LamE [VarP argsName,argsP,pathP] $
					DoE $ stmts ++ [NoBindS core_bodyE]

{-
 Generate body of loadM function, which has the form:
  do (rep,md) <- rhsE
     return (Rep rep, md)
-}
genZDefaultBody :: Exp -> Name -> ForestTy -> ZEnvQ Exp
genZDefaultBody pathE repN ty = case ty of 
	Directory _ -> zdefaultE True ty pathE
	FConstraint _ (Directory _) _ -> zdefaultE True ty pathE
	otherwise   -> do -- Add type constructor
		rhsE <- zdefaultE True ty pathE
		return $ Pure.appE2 (VarE 'liftM) (ConE repN) rhsE

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

zdefaultE :: Bool -> ForestTy -> Exp -> ZEnvQ Exp
zdefaultE isTop ty pathE = case ty of
	Named f_name               -> zdefaultWithArgsE f_name [] pathE
	Fapp (Named f_name) argEs  -> zdefaultWithArgsE f_name argEs pathE
	FFile (file_name, argEOpt) -> zdefaultFile file_name argEOpt pathE
	Archive archtype ty         -> zdefaultArchive isTop archtype ty pathE
	FSymLink         -> zdefaultSymLink pathE
	FConstraint p ty pred -> zdefaultConstraint isTop p pred $ zdefaultE False ty pathE
	Directory dirTy -> zdefaultDirectory isTop dirTy pathE 
	FMaybe forestTy -> zdefaultMaybe isTop forestTy pathE 
	FComp cinfo     -> zdefaultComp isTop cinfo pathE 

zdefaultConstraint :: Bool -> TH.Pat -> Exp -> ZEnvQ Exp -> ZEnvQ Exp
zdefaultConstraint isTop pat predE load = forceVarsZEnvQ predE $ \predE' -> do
	(fs,_) <- Reader.ask
	let predFnE = zmodPredE pat predE'
	loadAction <- load
	if isTop
		then return $ Pure.appE2 (VarE 'doZDefaultConstraint) predFnE loadAction
		else return $ Pure.appE2 (VarE 'doZDefaultConstraintInner) predFnE loadAction

zdefaultFile :: String -> Maybe Exp -> Exp -> ZEnvQ Exp
zdefaultFile fileName Nothing pathE = do
	return $ Pure.appE2 (VarE 'doZDefaultFile1) (AppE (ConE 'Pure.Arg) $ TupE []) pathE
zdefaultFile fileName (Just argE) pathE = do
	return $ Pure.appE2 (VarE 'doZDefaultFile1) argE pathE

zdefaultWithArgsE :: String -> [Exp] -> Exp -> ZEnvQ Exp
zdefaultWithArgsE ty_name [] pathE = do
	let proxyE = AppE (VarE 'proxyOf) $ TupE []
	(fs,_) <- Reader.ask
	return $ Pure.appE3 (VarE 'zdefaultScratchMemo) proxyE (TupE []) pathE
zdefaultWithArgsE ty_name argEs pathE = do
	(fs,_) <- Reader.ask
	argsE <- mapM (\e -> forceVarsZEnvQ e return) argEs
	let tupArgsE = foldl1' (Pure.appE2 (ConE '(:*:))) argsE
	return $ Pure.appE3 (VarE 'zdefaultScratchMemo) (VarE $ mkName $ "proxyZArgs_"++ty_name) tupArgsE pathE

zdefaultSymLink :: Exp -> ZEnvQ Exp
zdefaultSymLink pathE = do
  return $ AppE (VarE 'doZDefaultSymLink) pathE

zdefaultArchive :: Bool -> [ArchiveType] -> ForestTy -> Exp -> ZEnvQ Exp
zdefaultArchive isTop archtype ty pathE = do
	newPathName <- lift $ newName "new_path"
	let (newPathE, newPathP) = genPE newPathName
	rhsE <- liftM (LamE [newPathP]) $ zdefaultE False ty newPathE
	exts <- lift $ dataToExpQ (\_ -> Nothing) archtype
	if isTop
		then return $ Pure.appE2 (VarE 'doZDefaultArchive) pathE rhsE
		else return $ Pure.appE2 (VarE 'doZDefaultArchiveInner) pathE rhsE

zdefaultMaybe :: Bool -> ForestTy -> Exp -> ZEnvQ Exp
zdefaultMaybe isTop ty pathE = do 
	rhsE <- zdefaultE False ty pathE
	if isTop
		then return $ AppE (VarE 'doZDefaultMaybe) pathE
		else return $ AppE (VarE 'doZDefaultMaybeInner) pathE

zdefaultDirectory :: Bool -> DirectoryTy -> Exp -> ZEnvQ Exp
zdefaultDirectory isTop dirTy@(Record id fields) pathE = do
	doDirE <- zdefaultDirectoryContents dirTy pathE
	collectMDs <- lift $ zgenMergeFieldsMDErrors fields	
	if isTop
		then return $ Pure.appE3 (VarE 'doZDefaultDirectory) pathE collectMDs doDirE
		else return $ Pure.appE3 (VarE 'doZDefaultDirectory') pathE collectMDs doDirE

zdefaultDirectoryContents :: DirectoryTy -> Exp -> ZEnvQ Exp
zdefaultDirectoryContents (Record id fields) pathE = do
	(repEs,stmts) <- zdefaultFields fields pathE
	let tyName = mkName id
	let repE = Pure.appConE (Pure.getStructInnerName tyName) $ map VarE repEs
	let resultE = TupE [repE]
	let finalS = NoBindS $ AppE (VarE 'return) resultE
	let doDirE = DoE $ stmts ++ [finalS]
	return doDirE

zdefaultFields :: [Field] -> Exp -> ZEnvQ ([Name],[Stmt])
zdefaultFields [] pathE = return ([],[])
zdefaultFields (field:fields) pathE = do
	(rep_field, stmts_field)  <- zdefaultField field pathE
	let update (fs,env) = (fs,Map.insert rep_field Nothing env)
	(reps_fields, stmts_fields) <- Reader.local update $ zdefaultFields fields pathE
	return (rep_field:reps_fields, stmts_field++stmts_fields)

zdefaultField :: Field -> Exp -> ZEnvQ (Name, [Stmt])
zdefaultField field pathE = case field of
	Simple s -> zdefaultSimple s pathE
	Comp   c -> zdefaultCompound True c pathE

zdefaultSimple :: BasicField -> Exp -> ZEnvQ (Name, [Stmt])
zdefaultSimple (internal, isForm, externalE, forestTy, predM) pathE = do
	-- variable declarations
	let repName = mkName internal
	let (repE,repP) = genPE repName
	newpathName <- lift $ newName "newpath"
	let (newpathE,newpathP) = genPE newpathName
	newGetMdName <- lift $ newName "newGetMD"
	let (newGetMdE,newGetMdP) = genPE newGetMdName
	newdfName <- lift $ newName "newdf"
	let (newdfE,newdfP) = genPE newdfName
	
	loadFocusE <- do
		(fs,_) <- Reader.ask
		loadContentE <- liftM (LamE [newpathP]) $ zdefaultE False forestTy newpathE 
		case predM of
			Nothing -> return $ Pure.appE3 (VarE 'doZDefaultSimple) pathE externalE loadContentE
			Just pred -> do
				return $ Pure.appE4 (VarE 'doZDefaultSimpleWithConstraint) pathE externalE (zmodPredE (VarP repName) pred) loadContentE
	let stmt1 = BindS (TildeP $ TupP [repP]) loadFocusE
	return (repName,[stmt1])

-- | Load a top-level declared comprehension
zdefaultComp :: Bool -> CompField -> Exp -> ZEnvQ Exp
zdefaultComp isTop cinfo pathE = do
	doCompE <- zdefaultCompContents cinfo pathE
	let collectMDs = zgenMergeFieldMDErrors (Comp cinfo)
	if isTop
		then return $ Pure.appE3 (VarE 'doZDefaultDirectory) pathE collectMDs doCompE
		else return $ Pure.appE3 (VarE 'doZDefaultDirectory') pathE collectMDs doCompE
	
-- | Load a top-level declared comprehension
zdefaultCompContents :: CompField -> Exp -> ZEnvQ Exp
zdefaultCompContents cinfo pathE = do
	(_,stmts) <- zdefaultCompound False cinfo pathE
	let doCompE = DoE $ init stmts ++ [Pure.unBindS (last stmts)]
	return doCompE

-- | Load a comprehension inlined inside a @Directory@
-- if a comprehension is nested inside a directory, we created a top-level metadata thunk for it, otherwise the thunk already exists
zdefaultCompound :: Bool -> CompField -> Exp -> ZEnvQ (Name,[Stmt])
zdefaultCompound isNested (CompField internal tyConNameOpt explicitName externalE descTy generatorP generatorG predM) pathE = do
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
	
	let genE = case generatorG of
		Explicit expE -> expE
		Matches regexpE -> regexpE
	
	forceVarsZEnvQ genE $ \genE -> do
		-- optional filtering
		let fileName = Pure.getCompName explicitName externalE
		let fileNameAtt = mkName $ nameBase fileName++"_att"
		let fileNameAttThunk = mkName $ nameBase fileName++"_att_thunk"
		
		-- build representation and metadata containers from a list
		buildContainerE <- lift $ Pure.tyConNameOptBuild tyConNameOpt
		
		let update (fs,env) = (fs,Map.insert fileName Nothing $ Map.insert fileNameAtt (Just (fileNameAttThunk,VarP fileNameAtt)) env)  --force the @FileInfo@ thunk
		
		-- container loading
		(fs,_) <- Reader.ask
		Reader.local update $ case predM of
			Nothing -> do
				loadSingleE <- liftM (LamE [VarP fileName,VarP fileNameAttThunk,newpathP]) $ zdefaultE False descTy newpathE
				let loadContainerE = Pure.appE4 (VarE 'doZDefaultCompound) pathE genE buildContainerE loadSingleE
				let loadContainerS = BindS (TupP [VarP repName]) loadContainerE
				return (repName,[loadContainerS])
				
			Just predE -> forceVarsZEnvQ predE $ \predE -> do
				loadSingleE <- liftM (LamE [VarP fileName,VarP fileNameAttThunk,newpathP]) $ zdefaultE False descTy newpathE
				let loadContainerE = Pure.appE5 (VarE 'doZDefaultCompoundWithConstraint) pathE genE (modPredEComp (VarP fileName) predE) buildContainerE loadSingleE
				let loadContainerS = BindS (TupP [VarP repName]) loadContainerE
				return (repName,[loadContainerS])

				
		


