{-# LANGUAGE TupleSections, TemplateHaskell, NamedFieldPuns, ScopedTypeVariables, RecordWildCards, FlexibleInstances, MultiParamTypeClasses,
    UndecidableInstances, ViewPatterns  #-}

module Language.Forest.IC.CodeGen.ZLoading where

import Language.Forest.IC.CodeGen.Loading
import Language.Forest.IC.CodeGen.ZDefault
import {-# SOURCE #-} Language.Forest.IC.CodeGen.ZDeltaLoading
import Language.Forest.IC.IO.Loading
import Language.Forest.IC.IO.ZLoading
import qualified Language.Forest.Pure.MetaData as Pure

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

genZLoadM :: Name -> ForestTy -> [(TH.Pat,TH.Type)] -> ZEnvQ Exp
genZLoadM rep_name forestTy pat_infos = do
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
			core_bodyE <- genZLoadBody filterPathE pathE treeE getMDE rep_name forestTy
			return $ LamE [VarP argsName,TupP [],filterPathP,pathP,treeP,getMDP] core_bodyE
		otherwise -> do
			(argsP,stmts,argsThunksE,argThunkNames) <- genZLoadArgsE (zip [1..] pat_infos) treeE forestTy
			let update (fs,env) = (fs,foldl updatePat env (zip argThunkNames pat_infos))
				where updatePat env (thunkName,(pat,ty)) = Map.fromSet (\var -> Just (thunkName,pat)) (patPVars pat) `Map.union` env --left-biased union
			Reader.local update $ do -- adds pattern variable deltas to the env.
				(fs,_) <- Reader.ask
				core_bodyE <- genZLoadBody filterPathE pathE treeE getMDE rep_name forestTy
				return $ LamE [VarP argsName,argsP,filterPathP,pathP,treeP,getMDP] $ --DoE $ stmts ++ [NoBindS core_bodyE]
					DoE $ stmts ++ [NoBindS core_bodyE]

{-
 Generate body of loadM function, which has the form:
  do (rep,md) <- rhsE
     return (Rep rep, md)
-}
genZLoadBody :: Exp -> Exp -> Exp -> Exp -> Name -> ForestTy -> ZEnvQ Exp
genZLoadBody filterPathE pathE treeE getMDE repN ty = case ty of 
	Directory _ -> zloadE True ty filterPathE pathE treeE getMDE
	FConstraint _ (Directory _) _ -> zloadE True ty filterPathE pathE treeE getMDE
	otherwise   -> do -- Add type constructor
		rhsE <- zloadE True ty filterPathE pathE treeE getMDE
		return $ Pure.appE2 (VarE 'liftM) (ConE repN) rhsE 

-- adds top-level arguments to the metadata
genZLoadArgsE :: [(Int,(TH.Pat,TH.Type))] -> Exp -> ForestTy -> ZEnvQ (TH.Pat,[Stmt],Exp,[Name])
genZLoadArgsE [pat_info] treeE forestTy = genZLoadArgE pat_info treeE forestTy
genZLoadArgsE (pat_info:pat_infos) treeE forestTy = do
	(pat,stmt,expr,names1) <- genZLoadArgE pat_info treeE forestTy
	(pats,stmts,exprs,names2) <- genZLoadArgsE pat_infos treeE forestTy
	return (ConP '(:*:) [pat,pats],stmt++stmts,UInfixE expr (ConE '(:*:)) exprs,names1++names2)

--we do not force thunks, and leave that for whenever their values are required inside expressions
genZLoadArgE :: (Int,(TH.Pat,TH.Type)) -> Exp -> ForestTy -> ZEnvQ (TH.Pat,[Stmt],Exp,[Name])
genZLoadArgE (i,(pat,pat_ty)) treeE forestTy = do
	mName <- lift $ newName $ "m"++show i
	let (mE,mP) = genPE mName
	thunkName <- lift $ newName $ "t"++show i
	let (thunkE,thunkP) = genPE thunkName
	let thunkS = BindS thunkP $ AppE (VarE 'icThunk) mE
	return (mP,[thunkS],thunkE,[thunkName])

zloadE :: Bool -> ForestTy -> Exp -> Exp -> Exp -> Exp -> ZEnvQ Exp
zloadE isTop ty filterPathE pathE treeE getMDE = case ty of
	Named ty_name               -> zloadWithArgsE ty_name [] filterPathE pathE treeE getMDE
	Fapp (Named ty_name) argEs  -> zloadWithArgsE ty_name argEs filterPathE pathE treeE getMDE
	FFile (file_name, argEOpt) -> zloadFile file_name argEOpt filterPathE pathE treeE getMDE
	Archive archtype ty         -> zloadArchive isTop archtype ty filterPathE pathE treeE getMDE
	FSymLink         -> zloadSymLink pathE treeE getMDE
	FConstraint p ty pred -> zloadConstraint treeE p pred $ zloadE isTop ty filterPathE pathE treeE getMDE
	Directory dirTy -> zloadDirectory isTop dirTy filterPathE pathE treeE getMDE
	FMaybe forestTy -> zloadMaybe isTop forestTy filterPathE pathE treeE getMDE
	FComp cinfo     -> zloadComp isTop cinfo filterPathE pathE treeE getMDE

zloadFile :: String -> Maybe Exp -> Exp -> Exp -> Exp -> Exp -> ZEnvQ Exp
zloadFile fileName Nothing pathFilterE pathE treeE getMDE = do
	let proxy = proxyFileName fileName
	return $ Pure.appE6 (VarE 'doZLoadFile1) proxy (AppE (ConE 'Pure.Arg) $ TupE []) pathFilterE pathE treeE getMDE
zloadFile fileName (Just argE) pathFilterE pathE treeE getMDE = do
	let proxy = proxyFileName fileName
	return $ Pure.appE6 (VarE 'doZLoadFile1) proxy argE pathFilterE pathE treeE getMDE

-- these are terminals in the spec
zloadWithArgsE :: String -> [Exp] -> Exp -> Exp -> Exp -> Exp -> ZEnvQ Exp
zloadWithArgsE ty_name [] filterPathE pathE treeE getMDE = do
	let proxyE = AppE (VarE 'proxyOf) $ TupE []
	(fs,_) <- Reader.ask
	return $ Pure.appE6 (VarE 'zloadScratchMemo) proxyE (TupE []) filterPathE pathE treeE getMDE
zloadWithArgsE ty_name argEs filterPathE pathE treeE getMDE = do
	(fs,_) <- Reader.ask
	argsE <- mapM (\e -> forceVarsZEnvQ e return) argEs
	let tupArgsE = foldl1' (Pure.appE2 (ConE '(:*:))) argsE
	return $ Pure.appE6 (VarE 'zloadScratchMemo) (VarE $ mkName $ "proxyZArgs_"++ty_name) tupArgsE filterPathE pathE treeE getMDE

zloadConstraint :: Exp -> TH.Pat -> Exp -> ZEnvQ Exp -> ZEnvQ Exp
zloadConstraint treeE pat predE load = forceVarsZEnvQ predE $ \predE' -> do
	(fs,_) <- Reader.ask
	let predFnE = zmodPredE pat predE'
	loadAction <- load
	return $ Pure.appE3 (VarE 'doZLoadConstraint) treeE predFnE loadAction

zloadSymLink :: Exp -> Exp -> Exp -> ZEnvQ Exp
zloadSymLink pathE treeE repmdE = do
  return $ Pure.appE3 (VarE 'doZLoadSymLink) pathE treeE repmdE

zloadArchive :: Bool -> [ArchiveType] -> ForestTy -> Exp -> Exp -> Exp -> Exp -> ZEnvQ Exp
zloadArchive isTop archtype ty filterPathE pathE treeE getMDE = do
	newPathName <- lift $ newName "new_path"
	newdfName <- lift $ newName "new_df"
	newdvName <- lift $ newName "new_dv"
	newTreeName <- lift $ newName "new_tree"
	newTreeName' <- lift $ newName "new_tree'"
	newGetMDName <- lift $ newName "newGetMD"
	newDPathName <- lift $ newName "new_dpath"
	newRepMdName <- lift $ newName "new_repmd"
	let (newPathE, newPathP) = genPE newPathName
	let (newdfE, newdfP) = genPE newdfName
	let (newdvE, newdvP) = genPE newdvName
	let (newTreeE, newTreeP) = genPE newTreeName
	let (newTreeE', newTreeP') = genPE newTreeName'
	let (newGetMDE, newGetMDP) = genPE newGetMDName
	let (newDPathE, newDPathP) = genPE newDPathName
	let (newRepMdE, newRepMdP) = genPE newRepMdName
	rhsE <- liftM (LamE [newPathP,newGetMDP,newTreeP']) $ zloadE False ty filterPathE newPathE newTreeE' newGetMDE
	defE <- liftM (LamE [newPathP]) $ zdefaultE False ty newPathE
	rhsDE <- liftM (LamE [newPathP,newDPathP,newRepMdP,newTreeP,newdfP,newTreeP',newdvP]) $ runZDeltaQ (zloadDeltaE False ty newPathE newTreeE newRepMdE newDPathE newdfE newTreeE' newdvE)
	exts <- lift $ dataToExpQ (\_ -> Nothing) archtype
	isClosedE <- lift $ dataToExpQ (\_ -> Nothing) $ isClosedForestTy ty
	if isTop
		then return $ Pure.appE10 (VarE 'doZLoadArchive) isClosedE (ConE 'Proxy) exts filterPathE pathE treeE getMDE rhsE defE rhsDE
		else return $ Pure.appE7 (VarE 'doZLoadArchiveInner) exts filterPathE pathE treeE getMDE rhsE defE

zloadMaybe :: Bool -> ForestTy -> Exp -> Exp -> Exp -> Exp -> ZEnvQ Exp
zloadMaybe isTop ty filterPathE pathE treeE getMDE = do 
	rhsE <- zloadE False ty filterPathE pathE treeE getMDE
	if isTop
		then return $ Pure.appE4 (VarE 'doZLoadMaybe) filterPathE pathE treeE rhsE
		else return $ Pure.appE4 (VarE 'doZLoadMaybeInner) filterPathE pathE treeE rhsE

zloadDirectory :: Bool -> DirectoryTy -> Exp -> Exp -> Exp -> Exp -> ZEnvQ Exp
zloadDirectory True dirTy@(Record id fields) filterPathE pathE treeE getMDE = do
	doDirE <- zloadDirectoryContents dirTy filterPathE pathE treeE getMDE
	defE <- zdefaultDirectoryContents dirTy pathE
	collectMDs <- lift $ zgenMergeFieldsMDErrors fields	
	return $ Pure.appE6 (VarE 'doZLoadDirectory) pathE treeE collectMDs getMDE doDirE defE

zloadDirectoryContents :: DirectoryTy -> Exp -> Exp -> Exp -> Exp -> ZEnvQ Exp
zloadDirectoryContents (Record id fields) filterPathE pathE treeE getMDE = do
	(repEs,stmts) <- zloadFields fields filterPathE pathE treeE getMDE
	let tyName = mkName id
	let repE = Pure.appConE (Pure.getStructInnerName tyName) $ map VarE repEs
	let resultE = TupE [repE]
	let finalS = NoBindS $ AppE (VarE 'return) resultE
	let doDirE = DoE $ stmts ++ [finalS]
	return doDirE

zloadFields :: [Field] -> Exp -> Exp -> Exp -> Exp -> ZEnvQ ([Name],[Stmt])
zloadFields [] filterPathE pathE treeE getMDE = return ([],[])
zloadFields (field:fields) filterPathE pathE treeE getMDE = do
	(rep_field, stmts_field)  <- zloadField field filterPathE pathE treeE getMDE
	let update (fs,env) = (fs,Map.insert rep_field Nothing env)
	(reps_fields, stmts_fields) <- Reader.local update $ zloadFields fields filterPathE pathE treeE getMDE
	return (rep_field:reps_fields, stmts_field++stmts_fields)

zloadField :: Field -> Exp -> Exp -> Exp -> Exp -> ZEnvQ (Name, [Stmt])
zloadField field filterPathE pathE treeE getMDE = case field of
	Simple s -> zloadSimple s filterPathE pathE treeE getMDE
	Comp   c -> zloadCompound True c filterPathE pathE treeE getMDE

zloadSimple :: BasicField -> Exp -> Exp -> Exp -> Exp -> ZEnvQ (Name, [Stmt])
zloadSimple (internal, isForm, externalE, forestTy, predM) filterPathE pathE treeE getMDE = do
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
		loadContentE <- liftM (LamE [newpathP,newGetMdP]) $ zloadE False forestTy filterPathE newpathE treeE newGetMdE
		case predM of
			Nothing -> return $ Pure.appE5 (VarE 'doZLoadSimple) filterPathE pathE externalE treeE loadContentE
			Just pred -> do
				return $ Pure.appE6 (VarE 'doZLoadSimpleWithConstraint) filterPathE pathE externalE treeE (zmodPredE (VarP repName) pred) loadContentE
	let stmt1 = BindS (TildeP $ TupP [repP]) loadFocusE
	return (repName,[stmt1])

-- | Load a top-level declared comprehension
zloadComp :: Bool -> CompField -> Exp -> Exp -> Exp -> Exp -> ZEnvQ Exp
zloadComp True cinfo filterPathE pathE treeE getMDE = do
	let collectMDs = zgenMergeFieldMDErrors (Comp cinfo)
	doCompE <- zloadCompContents cinfo filterPathE pathE treeE getMDE
	defE <- zdefaultCompContents cinfo pathE  
	return $ Pure.appE6 (VarE 'doZLoadDirectory) pathE treeE collectMDs getMDE doCompE defE
	
-- | Load a top-level declared comprehension
zloadCompContents :: CompField -> Exp -> Exp -> Exp -> Exp -> ZEnvQ Exp
zloadCompContents cinfo filterPathE pathE treeE getMDE = do
	(_,stmts) <- zloadCompound False cinfo filterPathE pathE treeE getMDE
	let doCompE = DoE $ init stmts ++ [Pure.unBindS (last stmts)]
	return doCompE

-- | Load a comprehension inlined inside a @Directory@
-- if a comprehension is nested inside a directory, we created a top-level metadata thunk for it, otherwise the thunk already exists
zloadCompound :: Bool -> CompField -> Exp -> Exp -> Exp -> Exp -> ZEnvQ (Name,[Stmt])
zloadCompound isNested (CompField internal tyConNameOpt explicitName externalE descTy generatorP generatorG predM) filterPathE pathE treeE getMDE = do
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
				loadSingleE <- liftM (LamE [VarP fileName,VarP fileNameAttThunk,newpathP,newGetMDP]) $ zloadE False descTy filterPathE newpathE treeE newGetMDE
				let loadContainerE = Pure.appE6 (VarE 'doZLoadCompound) filterPathE pathE genE treeE buildContainerE loadSingleE
				let loadContainerS = BindS (TupP [VarP repName]) loadContainerE
				return (repName,[loadContainerS])
				
			Just predE -> forceVarsZEnvQ predE $ \predE -> do
				loadSingleE <- liftM (LamE [VarP fileName,VarP fileNameAttThunk,newpathP,newGetMDP]) $ zloadE False descTy filterPathE newpathE treeE newGetMDE
				let loadContainerE = Pure.appE7 (VarE 'doZLoadCompoundWithConstraint) filterPathE pathE genE treeE (modPredEComp (VarP fileName) predE) buildContainerE loadSingleE
				let loadContainerS = BindS (TupP [VarP repName]) loadContainerE
				return (repName,[loadContainerS])

