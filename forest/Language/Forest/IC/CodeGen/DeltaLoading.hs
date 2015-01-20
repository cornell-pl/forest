{-# LANGUAGE TupleSections, TemplateHaskell, NamedFieldPuns, ScopedTypeVariables, RecordWildCards, FlexibleInstances, MultiParamTypeClasses,
    UndecidableInstances, ViewPatterns  #-}

module Language.Forest.IC.CodeGen.DeltaLoading where

import Language.Forest.IC.ICRep
import Prelude hiding (const,read)
import Language.Forest.IC.CodeGen.Utils
import Control.Monad.Incremental as Inc
import Language.Forest.IC.CodeGen.Loading
import Language.Haskell.TH.Quote
import Language.Forest.IC.IO.DeltaLoading
import qualified Language.Forest.Pure.CodeGen.Utils as Pure

import Language.Forest.Syntax as PS
import Language.Forest.Pure.MetaData
import Language.Forest.Errors
import Language.Forest.IC.Generic hiding (SymLink)
import qualified Language.Forest.Errors as E
import Language.Forest.IC.FS.FSDelta
import Data.WithClass.MData
import System.Directory
import System.FilePath.Posix

import Language.Haskell.TH as TH
import Language.Haskell.TH.Syntax hiding (lift)
import Language.Pads.Padsc hiding (lift)
import Language.Pads.TH
import Language.Forest.IO.Utils
import Language.Forest.TH
import Language.Forest.FS.FSRep
import Language.Forest.IC.ValueDelta

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
import Control.Monad.Reader (Reader(..),ReaderT(..))
import qualified Control.Monad.Reader as Reader
import Control.Monad.Trans.Class

-- for each variable name, we store (a boolean that indicates whether its value has NOT changed (changes are ALWAYS stable), the name of a thunk that holds its value, an optional pattern to match against the thunk's value when the variable is used)
type DeltaEnv = Map Name (TH.Exp,Maybe (Name,Pat))

-- the first boolean denotes the IC mode
type DeltaQ a = ReaderT (Name,Name,DeltaEnv) Q a

--environments store maps from variables to (the name of the thunk that holds its value,a pattern to match against the thunk's value)
type Env = Map Name (Maybe (Name,Pat))
-- the first boolean denotes the IC mode
type EnvQ = ReaderT (Name,Name,Env) Q

runDeltaQ :: DeltaQ a -> EnvQ a
runDeltaQ m = do
	(mode,fs,env) <- Reader.ask
	lift $ Reader.runReaderT m (mode,fs,Map.map (ConE 'False,) env) -- all non-delta variables have changed

runEnvQ :: EnvQ a -> DeltaQ a
runEnvQ m = do
	(mode,fs,env) <- Reader.ask
	lift $ Reader.runReaderT m (mode,fs,Map.map snd env)

-- forces thunk variables in an arbitrary expression
forceVarsDeltaQ :: Exp -> (Exp -> DeltaQ a) -> DeltaQ a
forceVarsDeltaQ e f = do
	let expvars = expVars e
	let forceVar var f = \e -> do
		(_,fs,envvars) <- Reader.ask
		case Map.lookup var envvars of
			Just (dv,Just (thunk,pat)) -> do
				
				let update (mode,fs,env) = (mode,fs,Set.foldr replaceVar env (patPVars pat)) where
					replaceVar var env = Map.insert var (dv,Nothing) env
				Reader.local update $ f $ UInfixE (AppE (VarE 'Inc.read) (VarE thunk)) (VarE '(>>=)) (LamE [pat] e)
			otherwise -> f e
	(Set.foldr forceVar f expvars) e

genLoadDeltaM :: (Name,Name) -> Name -> ForestTy -> [(TH.Pat, TH.Type)] -> DeltaQ TH.Exp
genLoadDeltaM (untyRep,tyRep) pd_name forestTy pat_infos = do
	dargsName     <- case pat_infos of
		[] -> return Nothing
		otherwise -> State.lift $ liftM Just $ newName "dargs"
	argsName <- lift $ newName "loadargs"
	margsName     <- case pat_infos of
		[] -> return Nothing
		otherwise -> State.lift $ liftM Just $ newName "margs"
	pathName    <- State.lift $ newName "path"
	dpathName    <- State.lift $ newName "dpath"
	treeName    <- State.lift $ newName "tree"
	treeName'    <- State.lift $ newName "tree'"
	repmdName     <- State.lift $ newName "repmd"
	dfName      <- State.lift $ newName "df"
	proxyName      <- State.lift $ newName "proxy"
	dargNames <- lift $ Pure.genForestTupleNames (length pat_infos) "d"
	margNames <- lift $ Pure.genForestTupleNames (length pat_infos) "m"
	(mode,fs,_) <- Reader.ask
	case pat_infos of -- add type argument variables to the environment
		[] -> do
			core_bodyE <- genLoadDeltaBody (VarE proxyName) (liftM VarE dargsName) pathName treeName repmdName dpathName dfName treeName' (untyRep,tyRep) forestTy
			return $ LamE [VarP mode,VarP proxyName,WildP,VarP pathName,VarP treeName,VarP repmdName,VarP dpathName,VarP dfName,VarP treeName'] core_bodyE
		otherwise -> do
			thunkNames <- lift $ Pure.genForestTupleNames (length pat_infos) "u"
			let update (mode,fs,env) = (mode,fs,foldl updateArg env (zip (zip thunkNames dargNames) pat_infos)) where
				updateArg env ((thunkName,dargName),(pat,ty)) = Map.fromSet insertVar (patPVars pat) `Map.union` env -- left-biased union
					where insertVar var = (AppE (VarE 'isEmptySValueDelta) (VarE dargName),Just (thunkName,pat))
				
			Reader.local update $ do -- adds pattern variable deltas to the env.
				
				newrepmdName <- State.lift $ newName "newrepmd"
				core_bodyE <- genLoadDeltaBody (VarE proxyName) (liftM VarE dargsName) pathName treeName newrepmdName dpathName dfName treeName' (untyRep,tyRep) forestTy
				let dargsP = AsP (fromJust dargsName) $ Pure.forestTupleP $ map VarP dargNames
				let argsP = AsP argsName (ViewP (Pure.appE3 (VarE 'getLoadDeltaArgs) (VarE mode) (proxyN fs) (VarE proxyName)) dargsP)
				let pats = [VarP mode,VarP proxyName,argsP,VarP pathName,VarP treeName,VarP repmdName,VarP dpathName,VarP dfName,VarP treeName']
				let recBodyE = LamE [Pure.forestTupleP $ map (VarP) thunkNames,VarP newrepmdName] $ DoE [NoBindS core_bodyE]
				return $ LamE pats $ Pure.appE6 (VarE 'doLoadDeltaArgs) (VarE mode) (VarE proxyName) (VarE argsName) (VarE repmdName) (VarE treeName') recBodyE

genLoadDeltaBody :: TH.Exp -> Maybe TH.Exp -> Name -> Name -> Name -> Name -> Name -> Name -> (Name,Name) -> ForestTy -> DeltaQ TH.Exp
genLoadDeltaBody proxyE argE pathName treeName repmdName dpathName dfName treeName' (untyRep,tyRep) forestTy = do
	let pathE = VarE pathName
	let treeE = VarE treeName
	let repmdE = VarE repmdName
	let dpathE = VarE dpathName
	let dfE = VarE dfName
	let treeE' = VarE treeName'
	case forestTy of
		Directory _ -> loadDeltaE forestTy pathE treeE repmdE dpathE dfE treeE'
		FConstraint _ (Directory _) _ -> loadDeltaE forestTy pathE treeE repmdE dpathE dfE treeE'
		otherwise -> do -- add type constructor
			let unfoldE = InfixE (Just $ (InfixE (Just $ VarE untyRep) (VarE '(><)) (Just $ VarE 'id))) (VarE '(><)) (Just $ VarE 'id)
			rhsE <- loadDeltaE forestTy pathE treeE (AppE unfoldE repmdE) dpathE dfE treeE'
			let isoE = VarE 'mapSValueDelta -- AppE (VarE 'isoSValueDelta) (Pure.appE2 (ConE 'Iso) (ConE tyRep) (VarE untyRep))
			return $ Pure.appE2 (VarE 'liftM) (InfixE (Just isoE) (VarE '(><)) (Just $ VarE 'id)) rhsE

loadDeltaE :: ForestTy -> TH.Exp -> TH.Exp -> TH.Exp -> TH.Exp -> TH.Exp -> TH.Exp -> DeltaQ TH.Exp
loadDeltaE forestTy pathE treeE repmdE dpathE dfE treeE' = case forestTy of
	Named ty_name -> loadDeltaNamed ty_name [] pathE treeE repmdE dpathE dfE treeE'
	Fapp (Named ty_name) argEs -> loadDeltaNamed ty_name argEs pathE treeE repmdE dpathE dfE treeE'
	File (file_name, argEOpt) -> checkUnevaluated "file" treeE' repmdE
		(loadFile file_name argEOpt pathFilterE pathE' treeE')
		(loadDeltaFile forestTy argEOpt pathE treeE dpathE dfE treeE')
	Archive archtype ty -> checkStop (archiveExtension archtype) forestTy pathE dpathE repmdE dfE treeE'
		(loadArchive archtype ty pathFilterE pathE' treeE')
		(loadDeltaArchive archtype ty pathE dpathE treeE dfE treeE')
	FSymLink -> checkUnevaluated "symlink" treeE' repmdE
		(loadSymLink pathE' treeE treeE')
		(loadDeltaSymLink pathE dpathE treeE dfE treeE')
	FConstraint pat descTy predE -> loadDeltaConstraint pat repmdE predE $ \newrepmdE -> loadDeltaE descTy pathE treeE newrepmdE dpathE dfE treeE'	
	(Directory dirTy) -> checkStop "directory" forestTy pathE dpathE repmdE dfE treeE'
		(loadDirectory dirTy pathFilterE pathE' treeE')
		(loadDeltaDirectory dirTy pathE treeE dpathE dfE treeE')
	FMaybe descTy -> checkStop "maybe" forestTy pathE dpathE repmdE dfE treeE'
		(loadMaybe descTy pathFilterE pathE' treeE')
		(loadDeltaMaybe descTy treeE pathE dpathE dfE treeE')
	FComp cinfo     -> checkStop "compound" forestTy pathE dpathE repmdE dfE treeE'
		(loadComp cinfo pathFilterE pathE' treeE')
		(loadDeltaComp cinfo pathE treeE dpathE dfE treeE')
  where pathE' = dpathE
        getMDE = AppE (VarE 'snd) repmdE
        pathFilterE = Pure.appE2 (VarE 'fsTreeDeltaPathFilter) dfE dpathE

-- terminals in the spec
loadDeltaNamed :: String -> [TH.Exp] -> TH.Exp -> TH.Exp -> TH.Exp -> TH.Exp -> TH.Exp -> TH.Exp -> DeltaQ TH.Exp
loadDeltaNamed ty_name [] pathE treeE repmdE dpathE dfE treeE' = do
	(mode,fs,_) <- Reader.ask
	let unit = Pure.appE2 (VarE 'mkEmptyLoadDeltaArgs) (VarE mode) (proxyN fs)
	return $ Pure.appE8 (VarE $ mkName $ "loadDelta_"++ty_name) (VarE mode) unit pathE treeE repmdE dpathE dfE treeE'
loadDeltaNamed ty_name argEs pathE treeE repmdE dpathE dfE treeE' = do
	(mode,fs,_) <- Reader.ask
	let makeArgDelta e = do
		b <- lift $ newName "b"
		condE <- isEmptyDeltaEnvExp e
		return ((condE,b),AppE (VarE 'makeSValueDelta) (VarE b))
		
	zips <- mapM makeArgDelta argEs
	
	let (conds,dargEs) = unzip zips
	argEs <- mapM (\e -> forceVarsDeltaQ e return) argEs
	let loadArgs = Pure.appE5 (VarE 'mkLoadDeltaArgs) (VarE mode) (proxyN fs) (VarE $ mkName $ "proxyArgs_"++ty_name) (Pure.forestTupleE argEs) (Pure.forestTupleE dargEs)
		
	let runCondsS = map (\(e,b) -> LetS [ValD (VarP b) (NormalB e) []]) conds 
	return $ DoE $ runCondsS ++ [NoBindS $ Pure.appE8 (VarE $ mkName $ "loadDelta_"++ty_name) (VarE mode) loadArgs pathE treeE repmdE dpathE dfE treeE']

loadDeltaArchive :: [ArchiveType] -> ForestTy -> Exp -> Exp -> Exp -> Exp -> Exp -> Exp -> DeltaQ Exp
loadDeltaArchive archtype ty pathE dpathE treeE dfE treeE' repmdE = do
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
	let pathFilterE = Pure.appE2 (VarE 'fsTreeDeltaPathFilter) dfE dpathE
	rhsE <- liftM (LamE [newPathP,newGetMDP,newTreeP,newdfP,newTreeP']) $ runEnvQ $ loadE ty pathFilterE newPathE newTreeE' newGetMDE
	rhsDE <- liftM (LamE [newPathP,newDPathP,newRepMdP,newTreeP,newdfP,newTreeP']) $ loadDeltaE ty newPathE newTreeE newRepMdE newDPathE newdfE newTreeE'
	exts <- lift $ dataToExpQ (\_ -> Nothing) archtype
	return $ Pure.appE9 (VarE 'doLoadDeltaArchive) exts pathE dpathE treeE dfE treeE' repmdE rhsE rhsDE

loadDeltaSymLink :: TH.Exp -> TH.Exp -> TH.Exp -> TH.Exp -> TH.Exp -> TH.Exp -> DeltaQ TH.Exp
loadDeltaSymLink pathE dpathE treeE dfE treeE' repmdE = return $ Pure.appE6 (VarE 'doLoadDeltaSymLink) pathE dpathE treeE dfE treeE' repmdE

loadDeltaMaybe :: ForestTy -> TH.Exp -> TH.Exp -> TH.Exp -> TH.Exp -> TH.Exp -> TH.Exp -> DeltaQ TH.Exp
loadDeltaMaybe forestTy treeE pathE dpathE dfE treeE' repmdE = do 
	newrepmdName <- lift $ newName "newrepmd"
	let (newrepmdE,newrepmdP) = genPE newrepmdName
	newGetMDName <- lift $ newName "newgetMD"
	let (newGetMDE,newGetMDP) = genPE newGetMDName
	let pathE' = dpathE
	let pathFilterE = Pure.appE2 (VarE 'fsTreeDeltaPathFilter) dfE dpathE
	loadContentNoDeltaE <- liftM (LamE [newGetMDP]) $ runEnvQ $ loadE forestTy pathFilterE pathE' treeE' newGetMDE
	loadContentDeltaE <- liftM (LamE [newrepmdP]) $ loadDeltaE forestTy pathE treeE newrepmdE dpathE dfE treeE'
	return $ Pure.appE8 (VarE 'doLoadDeltaMaybe) pathE repmdE dpathE treeE dfE treeE' loadContentNoDeltaE loadContentDeltaE

loadDeltaFile :: ForestTy -> Maybe TH.Exp -> TH.Exp -> TH.Exp -> TH.Exp -> TH.Exp -> TH.Exp -> TH.Exp -> DeltaQ TH.Exp
loadDeltaFile ty Nothing pathE treeE dpathE dfE treeE' repmdE = do
	return $ Pure.appE6 (VarE 'doLoadDeltaFile) pathE dpathE treeE dfE treeE' repmdE
loadDeltaFile ty (Just argE') pathE treeE dpathE dfE treeE' repmdE = do
	condE <- isEmptyDeltaEnvForestTy ty -- note that the variables from the argument delta are included in the delta environment
	return $ Pure.appE8 (VarE 'doLoadDeltaFile1) condE argE' pathE dpathE treeE dfE treeE' repmdE
	
loadDeltaDirectory :: DirectoryTy -> TH.Exp -> TH.Exp -> TH.Exp -> TH.Exp -> TH.Exp -> TH.Exp -> DeltaQ TH.Exp
loadDeltaDirectory dirTy@(Record id fields) pathE treeE dpathE dfE treeE' repmdE = do
	innerGetMDName <- lift $ newName $ "getMD"++id
	let (innerGetMDE,innerGetMDP) = genPE innerGetMDName
	let getMDE = AppE (VarE 'snd) repmdE
	let pathE' = dpathE
	
	innerRepMDName <- lift $ newName $ "repmd"++id
	let (innerRepMDE,innerRepMDP) = genPE innerRepMDName
	(drepEs,dmdEs,stmts) <- loadFieldsDelta fields pathE treeE innerRepMDE dpathE dfE treeE'
	let tyName = mkName id
	let drepE = mergeFieldDeltas drepEs
	let dmdE = mergeFieldDeltas dmdEs
	
--	let repE = appConE (getStructInnerName tyName) $ map snd repEs
--	let mdE = appConE (getStructInnerMDName tyName) $ map snd mdEs
--	let resultE = TupE [repE,mdE]
	let finalS = NoBindS $ Pure.returnExp $ TupE [drepE,dmdE] --Pure.appE2 (VarE 'tupM) drepE dmdE
	let pathFilterE = Pure.appE2 (VarE 'fsTreeDeltaPathFilter) dfE dpathE
	doDirNoDeltaE <- liftM (LamE [innerGetMDP]) $ runEnvQ $ loadDirectoryContents dirTy pathFilterE pathE' treeE' innerGetMDE
	let doDirDeltaE = LamE [innerRepMDP] $ DoE $ stmts ++ [finalS]
	collectMDs <- lift $ genMergeFieldsMDErrors fields	
	return $ Pure.appE9 (VarE 'doLoadDeltaDirectory) pathE repmdE dpathE treeE dfE treeE' collectMDs doDirNoDeltaE doDirDeltaE

-- add field variables to the environment
loadFieldsDelta :: [Field] -> TH.Exp -> TH.Exp -> TH.Exp -> TH.Exp -> TH.Exp -> TH.Exp -> DeltaQ ([Name],[Name],[Stmt])
loadFieldsDelta [] pathE treeE repmdE dpathE dfE treeE' = return ([],[],[])
loadFieldsDelta (field:fields) pathE treeE repmdE dpathE dfE treeE' = do
	(rep_name,rep_field,md_name,md_field, stmts_field)  <- loadFieldDelta field pathE treeE repmdE dpathE dfE treeE'
	-- updates the delta environment similarly to Forest <x:s1,s2> specifications; adds representation (this) and metadata (this_md) variables to the environment
	let update (mode,fs,env) = (mode,fs,Map.insert rep_name (AppE (VarE 'isEmptySValueDelta) $ VarE rep_field,Nothing) $ Map.insert md_name (AppE (VarE 'isEmptySValueDelta) $ VarE md_field,Nothing) env)
	(reps_fields, md_fields, stmts_fields) <- Reader.local update $ loadFieldsDelta fields pathE treeE repmdE dpathE dfE treeE'
	return (rep_field:reps_fields, md_field:md_fields, stmts_field++stmts_fields)

loadFieldDelta :: Field -> TH.Exp -> TH.Exp -> TH.Exp -> TH.Exp -> TH.Exp -> TH.Exp -> DeltaQ (Name,Name,Name,Name, [Stmt])
loadFieldDelta field pathE treeE repmdE dpathE dfE treeE' = case field of
	Simple s -> loadDeltaSimple s pathE treeE repmdE dpathE dfE treeE'
	Comp   c -> loadDeltaCompound True c pathE treeE repmdE dpathE dfE treeE'

loadDeltaSimple :: BasicField -> TH.Exp -> TH.Exp -> TH.Exp -> TH.Exp -> TH.Exp -> TH.Exp -> DeltaQ (Name,Name,Name,Name,[Stmt])
loadDeltaSimple (internal, isForm, externalE, forestTy, predM) pathE treeE repmdE dpathE dfE treeE' = do
	-- variable declarations
	let repName = mkName internal
	let mdName  = mkName (internal++"_md")
	let drepName = mkName $ "d"++internal
	let dmdName  = mkName ("d"++internal++"_md")
	let (drepE,drepP) = genPE drepName
	let (dmdE,dmdP) = genPE dmdName
	newpathName <- lift $ newName "newpath"
	newdpathName <- lift $ newName "newdpath"
	fieldrepmdName <- lift $ newName $ internal++"repmd"
	let (newpathE,newpathP) = genPE newpathName
	let (newdpathE,newdpathP) = genPE newdpathName
	let (fieldrepmdE,fieldrepmdP) = genPE fieldrepmdName
	newdfName <- lift $ newName "newdf"
	let (newdfE,newdfP) = genPE newdfName
	
	xName <- lift $ newName "x"
	let (xE,xP) = genPE xName
	yName <- lift $ newName "y"
	let (yE,yP) = genPE yName
	
	let fieldStmt1 = LetS [
		 ValD xP (NormalB $ AppE (VarE repName) $ AppE (InfixE (Just $ VarE 'fst) (VarE '(.)) (Just $ VarE 'fst)) repmdE) []
		,ValD yP (NormalB $ AppE (VarE mdName) $ AppE (InfixE (Just $ VarE 'snd) (VarE '(.)) (Just $ VarE 'fst)) repmdE) []
		]
	let fieldStmt2 = LetS [
		ValD (VarP repName) (NormalB xE) []
		,ValD (VarP mdName) (NormalB yE) []
		]
	
	let innerrepmdE = TupE [TupE [ xE, yE],AppE (VarE 'snd) repmdE]
	
	-- actual loading
--	lensRepE <- lift $ buildFieldLens repName
--	lensMdE <- lift $ buildFieldLens mdName
	
--	let innerrepmdE = AppE (UInfixE (AppE (VarE 'Inc.get) lensRepE) (VarE '(><)) (AppE (VarE 'Inc.get) lensMdE)) repmdE
	(mode,fs,_) <- Reader.ask
	let pathFilterE = Pure.appE2 (VarE 'fsTreeDeltaPathFilter) dfE dpathE
	loadContentNoDeltaE <- liftM (LamE [newpathP,fieldrepmdP]) $ runEnvQ $ loadE forestTy pathFilterE newpathE treeE' fieldrepmdE
	loadContentDeltaE <- liftM (LamE [fieldrepmdP,newpathP,newdpathP,newdfP]) $ loadDeltaE forestTy newpathE treeE fieldrepmdE newdpathE newdfE treeE'
	loadE <- case predM of
		Nothing -> return $ Pure.appE9 (VarE 'doLoadDeltaSimple) pathE dpathE externalE treeE dfE treeE' innerrepmdE loadContentNoDeltaE loadContentDeltaE
		Just predE -> do
			boolE <- isEmptyDeltaEnvExp predE
			return $ Pure.appE12 (VarE 'doLoadDeltaSimpleWithConstraint) (VarE mode) boolE pathE dpathE externalE treeE dfE treeE' innerrepmdE (modPredE (VarP repName) predE) loadContentNoDeltaE loadContentDeltaE
	let loadStmt = BindS (TupP [VarP drepName,VarP dmdName]) loadE
	
	return (repName,drepName,mdName,dmdName,[fieldStmt1,fieldStmt2,loadStmt])

loadDeltaComp :: CompField -> TH.Exp -> TH.Exp -> TH.Exp -> TH.Exp -> TH.Exp -> TH.Exp -> DeltaQ TH.Exp
loadDeltaComp cinfo pathE treeE dpathE dfE treeE' repmdE = do
	newrepmdName <- lift $ newName "newrepmd"
	newGetMDName <- lift $ newName "newGetMD"
	let (newrepmdE,newrepmdP) = genPE newrepmdName
	let (newGetMDE,newGetMDP) = genPE newGetMDName
	let getMDE = AppE (VarE 'snd) repmdE
	let pathE' = dpathE
	(_,_,_,_,stmts) <- loadDeltaCompound False cinfo pathE treeE newrepmdE dpathE dfE treeE'
	let collectMDs = genMergeFieldMDErrors (Comp cinfo)
	let pathFilterE = Pure.appE2 (VarE 'fsTreeDeltaPathFilter) dfE dpathE
	doCompNoDeltaE <- liftM (LamE [newGetMDP]) $ runEnvQ $ loadCompContents cinfo pathFilterE pathE' treeE' newGetMDE
	let doCompDeltaE = LamE [newrepmdP] $ DoE $ init stmts ++ [Pure.unBindS $ last stmts]
	return $ Pure.appE9 (VarE 'doLoadDeltaDirectory) pathE repmdE dpathE treeE dfE treeE' collectMDs doCompNoDeltaE doCompDeltaE

loadDeltaCompound :: Bool -> CompField -> TH.Exp -> TH.Exp -> TH.Exp -> TH.Exp -> TH.Exp -> TH.Exp -> DeltaQ (Name,Name,Name,Name, [Stmt])
loadDeltaCompound insideDirectory ty@(CompField internal tyConNameOpt explicitName externalE descTy generatorP generatorG predM) pathE treeE repmdE dpathE dfE treeE' = do
	-- variable declarataions
	let repName = mkName internal
	let mdName = mkName $ internal++"_md"
	let drepName = mkName $ "d"++internal
	let dmdName  = mkName $ "d"++internal++"_md"
	let (drepE,drepP) = genPE drepName
	let (dmdE,dmdP) = genPE dmdName
	newpathName <- lift $ newName "newpath"
	newdpathName <- lift $ newName "newdpath"
	fieldrepmdName <- lift $ newName $ internal++"repmd"
	let (newpathE,newpathP) = genPE newpathName
	let (newdpathE,newdpathP) = genPE newdpathName
	let (fieldrepmdE,fieldrepmdP) = genPE fieldrepmdName
	newdfName <- lift $ newName "newdf"
	let (newdfE,newdfP) = genPE newdfName
	
	xName <- lift $ newName "x"
	let (xE,xP) = genPE xName
	yName <- lift $ newName "y"
	let (yE,yP) = genPE yName
	
	let fieldStmt1 = LetS [
		 ValD xP (NormalB $ AppE (VarE repName) $ AppE (InfixE (Just $ VarE 'fst) (VarE '(.)) (Just $ VarE 'fst)) repmdE) []
		,ValD yP (NormalB $ AppE (VarE mdName) $ AppE (InfixE (Just $ VarE 'snd) (VarE '(.)) (Just $ VarE 'fst)) repmdE) []
		]
	let fieldStmt2 = LetS [
		ValD (VarP repName) (NormalB xE) []
		,ValD (VarP mdName) (NormalB yE) []
		]
	let fieldStmts = if insideDirectory then [fieldStmt1,fieldStmt2] else []
	
	let innerrepmdE = if insideDirectory then TupE [TupE [VarE repName,VarE mdName],AppE (VarE 'snd) repmdE] else repmdE
	
	let genE = case generatorG of
		Explicit expE -> expE
		Matches regexpE -> regexpE
	
	forceVarsDeltaQ genE $ \genE -> do
		-- optional filtering
		let fileName = Pure.getCompName explicitName externalE
--		let filterE = returnExp2 $ mkOptConstraintFunComp fileName predM
		
		-- update the environment: add the current filename and filename_att to the delta environment
		let fileNameAtt = mkName $ nameBase fileName++"_att"
		let fileNameAttThunk = mkName $ nameBase fileName++"_att_thunk"
		let dfileName = mkName $ "d"++nameBase fileName
		let dfileNameAtt = mkName $ "d"++nameBase fileName++"_att"
		let (fileNameE,fileNameP) = genPE fileName
		let (fileNameAttE,fileNameAttP) = genPE fileNameAtt
		let (dfileNameE,dfileNameP) = genPE dfileName
		let (dfileNameAttE,dfileNameAttP) = genPE dfileNameAtt
		
		-- lenses and isomorphisms
--		(lensRepE,lensMdE) <- if insideDirectory
--			then do
--				lensRepE <- lift $ buildFieldLens repName
--				lensMdE <- lift $ buildFieldLens mdName
--				return (lensRepE,lensMdE)
--			else return (idLensE,idLensE)
				
		isoE <- lift $ tyConNameOptIso tyConNameOpt
		
		-- actual loading
		(mode,fs,_) <- Reader.ask
		let update (mode,fs,env) = (mode,fs,Map.insert fileName (Pure.appE2 (VarE 'Pure.isSameFileName) fileNameE dfileNameE,Nothing) $ Map.insert fileNameAtt (AppE (VarE 'isEmptySValueDelta) dfileNameAttE,Just (fileNameAttThunk,VarP fileNameAtt)) env)
		let pathFilterE = Pure.appE2 (VarE 'fsTreeDeltaPathFilter) dfE dpathE
		Reader.local update $ case predM of
			Nothing -> do
				loadElementE <- liftM (LamE [fileNameP,VarP fileNameAttThunk,newpathP,fieldrepmdP]) $ runEnvQ $ loadE descTy pathFilterE newpathE treeE' fieldrepmdE
				loadElementDeltaE <- liftM (LamE [fileNameP,dfileNameP,VarP fileNameAttThunk,dfileNameAttP,fieldrepmdP,newpathP,newdpathP,newdfP]) $ loadDeltaE descTy newpathE treeE fieldrepmdE newdpathE newdfE treeE'
				let loadActionE = Pure.appE12 (VarE 'doLoadDeltaCompound) (VarE mode) isoE isoE pathE dpathE genE treeE dfE treeE' innerrepmdE loadElementE loadElementDeltaE
				let deltasE = BindS (TupP [drepP,dmdP]) $ loadActionE
				return (repName,drepName,mdName,dmdName,fieldStmts++[deltasE])
			Just predE -> forceVarsDeltaQ predE $ \predE -> do
				loadElementE <- liftM (LamE [fileNameP,VarP fileNameAttThunk,newpathP,fieldrepmdP]) $ runEnvQ $ loadE descTy pathFilterE newpathE treeE' fieldrepmdE
				loadElementDeltaE <- liftM (LamE [fileNameP,dfileNameP,VarP fileNameAttThunk,dfileNameAttP,fieldrepmdP,newpathP,newdpathP,newdfP]) $ loadDeltaE descTy newpathE treeE fieldrepmdE newdpathE newdfE treeE'
				let loadActionE = Pure.appE13 (VarE 'doLoadDeltaCompoundWithConstraint) (VarE mode) isoE isoE pathE dpathE genE treeE dfE treeE' innerrepmdE (modPredEComp (VarP fileName) predE) loadElementE loadElementDeltaE
				let deltasE = BindS (TupP [drepP,dmdP]) $ loadActionE
				return (repName,drepName,mdName,dmdName,fieldStmts++[deltasE])

loadDeltaConstraint :: TH.Pat -> TH.Exp -> TH.Exp -> (TH.Exp -> DeltaQ TH.Exp) -> DeltaQ TH.Exp
loadDeltaConstraint pat repmdE predE load = forceVarsDeltaQ predE $ \predE -> do
	(mode,fs,_) <- Reader.ask
	newRepMdName <- lift $ newName "newrepmd"
	let (newRepMdE,newRepMdP) = genPE newRepMdName
	
	let predFnE = modPredE pat predE
	loadAction <- load newRepMdE
	boolE <- isEmptyDeltaEnvExp predE
	return $ Pure.appE5 (VarE 'doLoadDeltaConstraint) (VarE mode) boolE repmdE predFnE $ LamE [newRepMdP] loadAction

-- tests if the environment variables used by given the forest specification have not changed
--isEmptyDeltaEnv :: (a -> Set Name) -> a -> DeltaQ TH.Exp
--isEmptyDeltaEnv getVars ty = do
--	env <- Reader.ask
--	Set.foldr (\var e -> e >>= \b1 -> isEmptyDeltaVar var >>= \b2 -> return $ UInfixE b1 (VarE '(&&)) b2) (return $ ConE 'True) (getVars ty)
--
--isEmptyDeltaVar :: Name -> DeltaQ TH.Exp
--isEmptyDeltaVar var = do
--	env <- Reader.ask
--	case Map.lookup var env of
--		Nothing -> error $ "delta for variable " ++ show var ++ " not found"
--		Just (dv,_) -> return dv

isEmptyDeltaEnv :: (a -> Set Name) -> a -> DeltaQ TH.Exp
isEmptyDeltaEnv getVars ty = do
	(mode,fs,env) <- Reader.ask
	let tyVars = getVars ty
	let envVars = map (fst . snd) $ Map.toList $ Map.filterWithKey (\v dv -> v `Set.member` tyVars) env
	return $ AppE (VarE 'and) (ListE envVars)

isEmptyDeltaEnvForestTy :: ForestTy -> DeltaQ TH.Exp
isEmptyDeltaEnvForestTy = isEmptyDeltaEnv forestTyVars

isEmptyDeltaEnvField :: Field -> DeltaQ TH.Exp
isEmptyDeltaEnvField = isEmptyDeltaEnv fieldPVars
	
isEmptyDeltaEnvExp :: TH.Exp -> DeltaQ TH.Exp
isEmptyDeltaEnvExp = isEmptyDeltaEnv expVars

checkNoChange :: ForestTy -> TH.Exp -> TH.Exp -> DeltaQ TH.Exp -> DeltaQ TH.Exp
checkNoChange ty dpathEs dfE m = do
	cond1 <- isEmptyDeltaEnvForestTy ty
	let cond2 = AppE (VarE 'isEmptySValueDelta) dpathEs
	let cond3 = AppE (VarE 'isEmptyFSTreeDelta) dfE
	x <- m
	return $ Pure.appE5 (VarE 'skipUpdateIf3) (LitE $ StringL "checkNoChange") cond1 cond2 cond3 x

checkUnevaluated :: String -> TH.Exp -> TH.Exp -> (TH.Exp -> EnvQ TH.Exp) -> (TH.Exp -> DeltaQ TH.Exp) -> DeltaQ TH.Exp
checkUnevaluated str treeE' repmdE load loadD = do
	newRepMDName <- lift $ newName "repmd"
	let (newRepMDE,newRepMDP) = genPE newRepMDName
	newGetMDName <- lift $ newName "getMd"
	let (newGetMDE,newGetMDP) = genPE newGetMDName
	z <- runEnvQ $ load newGetMDE
	x <- loadD newRepMDE
	strE <- lift $ liftString str
	return $ Pure.appE5 (VarE 'skipUnevaluated) strE treeE' repmdE (LamE [newGetMDP] z) (LamE [newRepMDP] x)

checkStop :: String -> ForestTy -> TH.Exp -> TH.Exp -> TH.Exp -> TH.Exp -> TH.Exp -> (TH.Exp -> EnvQ TH.Exp) -> (TH.Exp -> DeltaQ TH.Exp) -> DeltaQ TH.Exp
checkStop str ty pathE dpathE repmdE dfE treeE' load loadD = do
	newRepMDName <- lift $ newName "repmd"
	let (newRepMDE,newRepMDP) = genPE newRepMDName
	newGetMDName <- lift $ newName "getMd"
	let (newGetMDE,newGetMDP) = genPE newGetMDName
	cond1 <- isEmptyDeltaEnvForestTy ty
	z <- runEnvQ $ load newGetMDE
	x <- loadD newRepMDE
	strE <- lift $ liftString str
	return $ Pure.appE9 (VarE 'stopIf) (UInfixE strE (VarE '(++)) dpathE) cond1 pathE dpathE dfE treeE' repmdE (LamE [newGetMDP] z) (LamE [newRepMDP] x)





