{-# LANGUAGE TupleSections, TemplateHaskell, NamedFieldPuns, ScopedTypeVariables, RecordWildCards, FlexibleInstances, MultiParamTypeClasses,
    UndecidableInstances, ViewPatterns  #-}

module Language.Forest.IC.CodeGen.Default where

import Prelude hiding (const,read)
import Language.Forest.IC.CodeGen.Utils
import Language.Forest.IC.IO.Default
import Control.Monad.Trans
import Language.Haskell.TH.Quote

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

genDefaultM :: Name -> Name -> ForestTy -> [(TH.Pat,TH.Type)] -> Q TH.Exp
genDefaultM rep_name md_name forestTy pat_infos = return $ VarE 'undefined
--	pathName    <- newName "path"
--	repName    <- newName "rep"
--	let (pathE, pathP) = genPE pathName
--	let (repE, repP) = genPE repName
--	
--	case pat_infos of
--		[] -> do
--			core_bodyE <- genDefaultBody repE pathE rep_name forestTy
--			return $ LamE [TupP [],repP,pathP] core_bodyE
--		otherwise -> do
--			(argsP) <- genDefaultArgsE (zip [1..] pat_infos) forestTy
--			core_bodyE <- genDefaultBody repE pathE rep_name forestTy
--			return $ LamE [argsP,repP,pathP] $ DoE [NoBindS core_bodyE]

{-
 Generate body of loadM function, which has the form:
  do (rep,md) <- rhsE
     return (Rep rep, md)
-}
--genDefaultBody :: TH.Exp -> TH.Exp -> Name -> ForestTy -> Q TH.Exp
--genDefaultBody repE pathE repN ty = case ty of 
--	Directory _ -> defaultE ty repE pathE
--	FConstraint _ (Directory _) _ -> defaultE ty repE pathE
--	otherwise   -> do -- Add type constructor
--		let repE' = AppE (VarE $ getUnTyName $ nameBase repN) repE
--		defaultE ty repE' pathE
--
---- adds top-level arguments to the metadata
--genDefaultArgsE :: [(Int,(TH.Pat,TH.Type))] -> ForestTy -> Q (TH.Pat)
--genDefaultArgsE [pat_info] forestTy = genDefaultArgE pat_info forestTy
--genDefaultArgsE (pat_info:pat_infos) forestTy = do
--	(pat) <- genDefaultArgE pat_info forestTy
--	(pats) <- genDefaultArgsE pat_infos forestTy
--	return (ConP '(:*:) [pat,pats])
--
----we do not force thunks, and leave that for whenever their values are required inside expressions
--genDefaultArgE :: (Int,(TH.Pat,TH.Type)) -> ForestTy -> Q TH.Pat
--genDefaultArgE (i,(pat,pat_ty)) forestTy = return pat
--
defaultE :: ForestTy -> TH.Exp -> TH.Exp -> Q TH.Exp
defaultE ty repE pathE = return $ VarE 'undefined --case ty of
--	Named f_name               -> defaultWithArgsE [] repE pathE
--	Fapp (Named f_name) argEs  -> defaultWithArgsE argEs repE pathE
--	File (file_name, argEOpt) -> defaultFile file_name argEOpt repE pathE
--	Archive archtype ty         -> defaultArchive archtype ty repE pathE
--	SymLink         -> defaultSymLink repE pathE
--	FConstraint p ty pred -> defaultE ty repE pathE
--	Directory dirTy -> defaultDirectory dirTy repE pathE 
--	FMaybe forestTy -> defaultMaybe forestTy repE pathE 
--	FComp cinfo     -> defaultComp cinfo repE pathE 
--
--proxyFileName :: String -> TH.Exp
--proxyFileName name = SigE (ConE 'Proxy) (AppT (ConT ''Proxy) $ ConT $ mkName name)
--
--defaultFile :: String -> Maybe TH.Exp -> TH.Exp -> TH.Exp -> Q TH.Exp
--defaultFile fileName Nothing repE pathE = do
--	return $ appE2 (VarE 'doDefaultFile) repE pathE
--defaultFile fileName (Just argE) repE pathE = do
--	return $ appE3 (VarE 'doDefaultFile1) argE repE pathE
--
---- these are terminals in the spec
--defaultWithArgsE :: [TH.Exp] -> TH.Exp -> TH.Exp -> Q TH.Exp
--defaultWithArgsE [] repE pathE = return $ appE3 (VarE 'Forest.defaultMd) (TupE []) repE pathE
--defaultWithArgsE argsE repE pathE = do
--	let tupArgsE = foldl1' (appE2 (ConE '(:*:))) argsE
--	return $ appE3 (VarE 'Forest.defaultMd) tupArgsE repE pathE
--
--defaultSymLink :: TH.Exp -> TH.Exp -> Q TH.Exp
--defaultSymLink repE pathE = do
--  return $ appE2 (VarE 'doDefaultSymLink) repE pathE
--
--defaultArchive :: [ArchiveType] -> ForestTy -> TH.Exp -> TH.Exp -> Q TH.Exp
--defaultArchive archtype ty repE pathE = do
--	newPathName <- newName "new_path"
--	newRepName <- newName "new_rep"
--	let (newPathE, newPathP) = genPE newPathName
--	let (newRepE, newRepP) = genPE newRepName
--	rhsE <- liftM (LamE [newRepP,newPathP]) $ defaultE ty newRepE newPathE
--	exts <- dataToExpQ (\_ -> Nothing) archtype
--	return $ appE4 (VarE 'doDefaultArchive) exts repE pathE rhsE
--
--defaultMaybe :: ForestTy -> TH.Exp -> TH.Exp -> Q TH.Exp
--defaultMaybe ty repE pathE = do 
--	newRepName <- newName "new_rep"
--	let (newRepE, newRepP) = genPE newRepName
--	rhsE <- liftM (LamE [newRepP]) $ defaultE ty newRepE pathE
--	return $ appE3 (VarE 'doDefaultMaybe) repE pathE rhsE
--
--defaultDirectory :: DirectoryTy -> TH.Exp -> TH.Exp -> Q TH.Exp
--defaultDirectory dirTy@(Record id fields) repE pathE = do
--	doDirE <- defaultDirectoryContents dirTy repE pathE
--	return $ appE3 (VarE 'doDefaultDirectory) repE pathE doDirE
--
--defaultDirectoryContents :: DirectoryTy -> TH.Exp -> TH.Exp -> Q TH.Exp
--defaultDirectoryContents (Record id fields) repE pathE = do
--	(mdEs,stmts) <- defaultFields fields repE pathE
--	let tyName = mkName id
--	let mdE = appConE (getStructInnerMDName tyName) $ map VarE mdEs
--	let resultE = TupE [mdE]
--	let finalS = NoBindS $ AppE (VarE 'return) resultE
--	let doDirE = DoE $ stmts ++ [finalS]
--	return doDirE
--
--defaultFields :: [Field] -> TH.Exp -> TH.Exp -> Q ([Name],[Stmt])
--defaultFields [] repE pathE = return ([],[])
--defaultFields (field:fields) repE pathE = do
--	(md_field, stmts_field)  <- defaultField field repE pathE
--	(md_fields, stmts_fields) <- defaultFields fields repE pathE
--	return (md_field:md_fields, stmts_field++stmts_fields)
--
--defaultField :: Field -> TH.Exp -> TH.Exp -> Q (Name, [Stmt])
--defaultField field repE pathE = case field of
--	Simple s -> defaultSimple s repE pathE
--	Comp   c -> defaultCompound True c repE pathE
--
--defaultSimple :: BasicField -> TH.Exp -> TH.Exp -> Q (Name,[Stmt])
--defaultSimple (internal, isForm, externalE, forestTy, predM) parentRepE parentPathE = do
--	-- variable declarations
--	let repName = mkName internal
--	let mdName  = mkName (internal++"_md")
--	let (repE,repP) = genPE repName
--	let (mdE,mdP) = genPE mdName
--	newpathName <- newName "newpath"
--	let (newpathE,newpathP) = genPE newpathName
--	
--	let innerRepE = AppE repE parentRepE
--	
--	defaultContentE <- liftM (LamE [newpathP]) $ defaultE forestTy innerRepE newpathE
--	let defaultFocusE = appE4 (VarE 'doDefaultFocus) innerRepE parentPathE externalE defaultContentE
--	
--	let stmt1 = BindS (TildeP $ TupP [mdP]) defaultFocusE
--	return (mdName,[stmt1])
--
---- | Load a top-level declared comprehension
--defaultComp :: CompField -> TH.Exp -> TH.Exp -> Q TH.Exp
--defaultComp cinfo repE pathE = do
--	doCompE <- defaultCompContents cinfo repE pathE
--	return $ appE3 (VarE 'doDefaultDirectory) repE pathE doCompE
--	
---- | Load a top-level declared comprehension
--defaultCompContents :: CompField -> TH.Exp -> TH.Exp -> Q TH.Exp
--defaultCompContents cinfo repE pathE = do
--	(_,stmts) <- defaultCompound False cinfo repE pathE
--	let doCompE = DoE $ init stmts ++ [unBindS (last stmts)]
--	return doCompE
--    
--
---- if a comprehension is nested inside a directory, we created a top-level metadata thunk for it, otherwise the thunk already exists
--defaultCompound :: Bool -> CompField -> TH.Exp -> TH.Exp -> Q (Name,[Stmt])
--defaultCompound isNested (CompField internal tyConNameOpt explicitName externalE descTy generatorP generatorG predM) parentRepE pathE = do
--	-- variable declarations
--	let repName = mkName internal
--	let mdName  = mkName (internal++"_md")
--	newpathName <- newName "newpath"
--	let (repE,repP) = genPE repName
--	let (mdE,mdP) = genPE mdName
--	let (newpathE,newpathP) = genPE newpathName
--	newrepName <- newName "newrep"
--	let (newrepE,newrepP) = genPE newrepName
--	
--	let innerRepE = if isNested
--		then AppE repE parentRepE
--		else parentRepE
--	
--	-- optional filtering
--	let fileName = getCompName explicitName externalE
--	let fileNameAtt = mkName $ nameBase fileName++"_att"
--	
--	-- build representation and metadata containers from a list
--	buildContainerE <- tyConNameOptBuild tyConNameOpt
--	destroyContainerE <- tyConNameOptToList tyConNameOpt
--	
--
--	loadSingleE <- liftM (LamE [VarP fileName,VarP fileNameAtt,newrepP,newpathP]) $ defaultE descTy newrepE newpathE
--	let loadContainerE = appE5 (VarE 'doDefaultCompound) innerRepE pathE destroyContainerE buildContainerE loadSingleE
--	let loadContainerS = BindS (TupP [VarP mdName]) loadContainerE
--	return (mdName,[loadContainerS])
				
		


