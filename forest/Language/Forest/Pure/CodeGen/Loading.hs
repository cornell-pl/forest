{-# LANGUAGE TupleSections, TemplateHaskell, NamedFieldPuns, ScopedTypeVariables, RecordWildCards, FlexibleInstances, MultiParamTypeClasses,
    UndecidableInstances, ViewPatterns  #-}

module Language.Forest.Pure.CodeGen.Loading where

import Prelude hiding (const,read)
import Language.Forest.Pure.CodeGen.Utils
import Language.Forest.Pure.IO.Loading
import Control.Monad.Trans
import Language.Haskell.TH.Quote

import Language.Forest.Syntax as PS
import Language.Forest.Pure.MetaData
import Language.Forest.Errors
import Language.Forest.Pure.Generic
import qualified Language.Forest.Errors as E
import Language.Forest.IO.Utils
import System.Directory
import System.FilePath.Posix
import Control.Monad.Reader (Reader(..),ReaderT(..))
import qualified Control.Monad.Reader as Reader

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

genLoadM :: Name -> Name -> ForestTy -> [(TH.Pat,TH.Type)] -> Q TH.Exp
genLoadM rep_name md_name forestTy pat_infos = do
	fsName <- newName "fs"
	pathName    <- newName "path"
	treeName    <- newName "tree"
	getMDName    <- newName "getMD"
	let (pathE, pathP) = genPE pathName
	let (treeE, treeP) = genPE treeName
	let (getMDE, getMDP) = genPE getMDName
	let (fsE, fsP) = genPE fsName
	
	case pat_infos of
		[] -> do
			core_bodyE <- genLoadBody pathE treeE getMDE rep_name forestTy
			return $ LamE [TupP [],pathP,treeP,getMDP] core_bodyE
		otherwise -> do
			(argsP) <- genLoadArgsE (zip [1..] pat_infos) treeE forestTy
			core_bodyE <- genLoadBody pathE treeE getMDE rep_name forestTy
			return $ LamE [argsP,pathP,treeP,getMDP] $ DoE [NoBindS core_bodyE]

{-
 Generate body of loadM function, which has the form:
  do (rep,md) <- rhsE
     return (Rep rep, md)
-}
genLoadBody :: TH.Exp -> TH.Exp -> TH.Exp -> Name -> ForestTy -> Q TH.Exp
genLoadBody pathE treeE getMDE repN ty = case ty of 
	Directory _ -> loadE ty pathE treeE getMDE
	FConstraint _ (Directory _) _ -> loadE ty pathE treeE getMDE
	otherwise   -> do -- Add type constructor
		rhsE <- loadE ty pathE treeE getMDE
		return $ appE2 (VarE 'liftM) (InfixE (Just $ ConE repN) (VarE '(><)) (Just $ VarE 'id)) rhsE 

-- adds top-level arguments to the metadata
genLoadArgsE :: [(Int,(TH.Pat,TH.Type))] -> TH.Exp -> ForestTy -> Q (TH.Pat)
genLoadArgsE [pat_info] treeE forestTy = genLoadArgE pat_info treeE forestTy
genLoadArgsE (pat_info:pat_infos) treeE forestTy = do
	(pat) <- genLoadArgE pat_info treeE forestTy
	(pats) <- genLoadArgsE pat_infos treeE forestTy
	return (ConP '(:*:) [pat,pats])

--we do not force thunks, and leave that for whenever their values are required inside expressions
genLoadArgE :: (Int,(TH.Pat,TH.Type)) -> TH.Exp -> ForestTy -> Q TH.Pat
genLoadArgE (i,(pat,pat_ty)) treeE forestTy = return pat

loadE :: ForestTy -> TH.Exp -> TH.Exp -> TH.Exp -> Q TH.Exp
loadE ty pathE treeE getMDE = case ty of
	Named f_name               -> loadWithArgsE [] pathE treeE getMDE
	Fapp (Named f_name) argEs  -> loadWithArgsE argEs pathE treeE getMDE
	File (file_name, argEOpt) -> loadFile file_name argEOpt pathE treeE getMDE
	Archive archtype ty         -> loadArchive archtype ty pathE treeE getMDE
	SymLink         -> loadSymLink pathE treeE getMDE
	FConstraint p ty pred -> loadConstraint treeE p pred $ loadE ty pathE treeE getMDE
	Directory dirTy -> loadDirectory dirTy pathE treeE getMDE
	FMaybe forestTy -> loadMaybe forestTy pathE treeE getMDE
	FComp cinfo     -> loadComp cinfo pathE treeE getMDE

proxyFileName :: String -> TH.Exp
proxyFileName name = SigE (ConE 'Proxy) (AppT (ConT ''Proxy) $ ConT $ mkName name)

loadFile :: String -> Maybe TH.Exp -> TH.Exp -> TH.Exp -> TH.Exp -> Q TH.Exp
loadFile fileName Nothing pathE treeE getMDE = do
	let proxy = proxyFileName fileName
	return $ appE4 (VarE 'doLoadFile) proxy pathE treeE getMDE
loadFile fileName (Just argE) pathE treeE getMDE = do
	let proxy = proxyFileName fileName
	return $ appE5 (VarE 'doLoadFile1) proxy argE pathE treeE getMDE

-- these are terminals in the spec
loadWithArgsE :: [TH.Exp] -> TH.Exp -> TH.Exp -> TH.Exp -> Q TH.Exp
loadWithArgsE [] pathE treeE getMDE = return $ appE4 (VarE 'loadWithTree) (TupE []) pathE treeE getMDE
loadWithArgsE argsE pathE treeE getMDE = do
	let tupArgsE = foldl1' (appE2 (ConE '(:*:))) argsE
	return $ appE4 (VarE 'loadWithTree) tupArgsE pathE treeE getMDE

loadConstraint :: TH.Exp -> TH.Pat -> TH.Exp -> Q TH.Exp -> Q TH.Exp
loadConstraint treeE pat predE load = do
	let predFnE = modPredE pat predE
	loadAction <- load
	return $ appE3 (VarE 'doLoadConstraint) treeE predFnE loadAction

loadSymLink :: TH.Exp -> TH.Exp -> TH.Exp -> Q TH.Exp
loadSymLink pathE treeE getMDE = do
  return $ appE3 (VarE 'doLoadSymLink) pathE treeE getMDE

loadArchive :: [ArchiveType] -> ForestTy -> TH.Exp -> TH.Exp -> TH.Exp -> Q TH.Exp
loadArchive archtype ty pathE treeE getMDE = do
	newPathName <- newName "new_path"
	newTreeName <- newName "new_tree"
	newGetMDName <- newName "newGetMD"
	newRepMdName <- newName "new_repmd"
	let (newPathE, newPathP) = genPE newPathName
	let (newTreeE, newTreeP) = genPE newTreeName
	let (newGetMDE, newGetMDP) = genPE newGetMDName
	let (newRepMdE, newRepMdP) = genPE newRepMdName
	rhsE <- liftM (LamE [newPathP,newGetMDP,newTreeP]) $ loadE ty newPathE newTreeE newGetMDE
	exts <- dataToExpQ (\_ -> Nothing) archtype
	return $ appE5 (VarE 'doLoadArchive) exts pathE treeE getMDE rhsE

loadMaybe :: ForestTy -> TH.Exp -> TH.Exp -> TH.Exp -> Q TH.Exp
loadMaybe ty pathE treeE getMDE = do 
   rhsE <- loadE ty pathE treeE getMDE
   return $ appE3 (VarE 'doLoadMaybe) pathE treeE rhsE

loadDirectory :: DirectoryTy -> TH.Exp -> TH.Exp -> TH.Exp -> Q TH.Exp
loadDirectory dirTy@(Record id fields) pathE treeE getMDE = do
	doDirE <- loadDirectoryContents dirTy pathE treeE getMDE
	collectMDs <- genMergeFieldsMDErrors fields	
	return $ appE5 (VarE 'doLoadDirectory) pathE treeE collectMDs getMDE doDirE

loadDirectoryContents :: DirectoryTy -> TH.Exp -> TH.Exp -> TH.Exp -> Q TH.Exp
loadDirectoryContents (Record id fields) pathE treeE getMDE = do
	(repEs,mdEs,stmts) <- loadFields fields pathE treeE getMDE
	let tyName = mkName id
	let repE = appConE (getStructInnerName tyName) $ map VarE repEs
	let mdE = appConE (getStructInnerMDName tyName) $ map VarE mdEs
	let resultE = TupE [repE,mdE]
	let finalS = NoBindS $ AppE (VarE 'return) resultE
	let doDirE = DoE $ stmts ++ [finalS]
	return doDirE

loadFields :: [Field] -> TH.Exp -> TH.Exp -> TH.Exp -> Q ([Name],[Name],[Stmt])
loadFields [] pathE treeE getMDE = return ([],[],[])
loadFields (field:fields) pathE treeE getMDE = do
	(rep_field,   md_field, stmts_field)  <- loadField field pathE treeE getMDE
	(reps_fields, md_fields, stmts_fields) <- loadFields fields pathE treeE getMDE
	return (rep_field:reps_fields, md_field:md_fields, stmts_field++stmts_fields)

loadField :: Field -> TH.Exp -> TH.Exp -> TH.Exp -> Q (Name,Name, [Stmt])
loadField field pathE treeE getMDE = case field of
	Simple s -> loadSimple s pathE treeE getMDE
	Comp   c -> loadCompound True c pathE treeE getMDE

loadSimple :: BasicField -> TH.Exp -> TH.Exp -> TH.Exp -> Q (Name,Name, [Stmt])
loadSimple (internal, isForm, externalE, forestTy, predM) pathE treeE getMDE = do
	-- variable declarations
	let repName = mkName internal
	let mdName  = mkName (internal++"_md")
	let (repE,repP) = genPE repName
	let (mdE,mdP) = genPE mdName
	newpathName <- newName "newpath"
	let (newpathE,newpathP) = genPE newpathName
	newGetMdName <- newName "newGetMD"
	let (newGetMdE,newGetMdP) = genPE newGetMdName
	
	loadFocusE <- do
		loadContentE <- liftM (LamE [newpathP,newGetMdP]) $ loadE forestTy newpathE treeE newGetMdE
		case predM of
			Nothing -> return $ appE4 (VarE 'doLoadSimple) pathE externalE treeE loadContentE
			Just pred -> return $ appE5 (VarE 'doLoadSimpleWithConstraint) pathE externalE treeE (modPredE (VarP repName) pred) loadContentE
	let stmt1 = BindS (TildeP $ TupP [repP,mdP]) loadFocusE
	return (repName,mdName,[stmt1])

-- | Load a top-level declared comprehension
loadComp :: CompField -> TH.Exp -> TH.Exp -> TH.Exp -> Q TH.Exp
loadComp cinfo pathE treeE getMDE = do
	let collectMDs = genMergeFieldMDErrors (Comp cinfo)
	doCompE <- loadCompContents cinfo pathE treeE getMDE
	return $ appE5 (VarE 'doLoadDirectory) pathE treeE collectMDs getMDE doCompE
	
-- | Load a top-level declared comprehension
loadCompContents :: CompField -> TH.Exp -> TH.Exp -> TH.Exp -> Q TH.Exp
loadCompContents cinfo pathE treeE getMDE = do
	(_,_,stmts) <- loadCompound False cinfo pathE treeE getMDE
	let doCompE = DoE $ init stmts ++ [unBindS (last stmts)]
	return doCompE

-- | Load a comprehension inlined inside a @Directory@
-- if a comprehension is nested inside a directory, we created a top-level metadata thunk for it, otherwise the thunk already exists
loadCompound :: Bool -> CompField -> TH.Exp -> TH.Exp -> TH.Exp -> Q (Name,Name,[Stmt])
loadCompound isNested (CompField internal tyConNameOpt explicitName externalE descTy generatorP generatorG predM) pathE treeE getMDE = do
	-- variable declarations
	let repName = mkName internal
	let mdName  = mkName (internal++"_md")
	newpathName <- newName "newpath"
	newGetMDName <- newName "newgetMD"
	let (repE,repP) = genPE repName
	let (mdE,mdP) = genPE mdName
	let (newpathE,newpathP) = genPE newpathName
	let (newGetMDE,newGetMDP) = genPE newGetMDName	
	
	let genE = case generatorG of
		Explicit expE -> expE
		Matches regexpE -> regexpE
	
	
	-- optional filtering
	let fileName = getCompName explicitName externalE
	let fileNameAtt = mkName $ nameBase fileName++"_att"
	
	-- build representation and metadata containers from a list
	buildContainerE <- tyConNameOptBuild tyConNameOpt
	
	-- container loading
	case predM of
		Nothing -> do
			loadSingleE <- liftM (LamE [VarP fileName,VarP fileNameAtt,newpathP,newGetMDP]) $ loadE descTy newpathE treeE newGetMDE
			let loadContainerE = appE6 (VarE 'doLoadCompound) pathE genE treeE buildContainerE buildContainerE loadSingleE
			let loadContainerS = BindS (TupP [VarP repName,VarP mdName]) loadContainerE
			return (repName,mdName,[loadContainerS])
				
		Just predE -> do
			loadSingleE <- liftM (LamE [VarP fileName,VarP fileNameAtt,newpathP,newGetMDP]) $ loadE descTy newpathE treeE newGetMDE
			let loadContainerE = appE7 (VarE 'doLoadCompoundWithConstraint) pathE genE treeE (modPredEComp (VarP fileName) predE) buildContainerE buildContainerE loadSingleE
			let loadContainerS = BindS (TupP [VarP repName,VarP mdName]) loadContainerE
			return (repName,mdName,[loadContainerS])



