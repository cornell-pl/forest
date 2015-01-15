{-# LANGUAGE TupleSections, TemplateHaskell, NamedFieldPuns, ScopedTypeVariables, RecordWildCards, FlexibleInstances, MultiParamTypeClasses,
    UndecidableInstances, ViewPatterns  #-}

module Language.Forest.IC.CodeGen.ZDeltaLoading where

import Language.Forest.IC.ICRep
import Prelude hiding (const,read)
import Language.Forest.IC.CodeGen.Utils
import Control.Monad.Incremental as Inc
import Language.Forest.IC.CodeGen.Loading
import Language.Forest.IC.CodeGen.ZLoading
import Language.Haskell.TH.Quote
import Language.Forest.IC.IO.DeltaLoading
import Language.Forest.IC.IO.ZDeltaLoading
import qualified Language.Forest.Pure.CodeGen.Utils as Pure

import Language.Forest.Syntax as PS
import Language.Forest.Pure.MetaData
import Language.Forest.Errors
import Language.Forest.IC.Generic
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
type ZDeltaEnv = Map Name (TH.Exp,Maybe (Name,Pat))

-- the first boolean denotes the IC mode
type ZDeltaQ a = ReaderT (Name,ZDeltaEnv) Q a

--environments store maps from variables to (the name of the thunk that holds its value,a pattern to match against the thunk's value)
type ZEnv = Map Name (Maybe (Name,Pat))
-- the first boolean denotes the IC mode
type ZEnvQ = ReaderT (Name,ZEnv) Q

runZDeltaQ :: ZDeltaQ a -> ZEnvQ a
runZDeltaQ m = do
	(fs,env) <- Reader.ask
	lift $ Reader.runReaderT m (fs,Map.map (ConE 'False,) env) -- all non-delta variables have changed

runZEnvQ :: ZEnvQ a -> ZDeltaQ a
runZEnvQ m = do
	(fs,env) <- Reader.ask
	lift $ Reader.runReaderT m (fs,Map.map snd env)

-- forces thunk variables in an arbitrary expression
forceVarsZDeltaQ :: Exp -> (Exp -> ZDeltaQ a) -> ZDeltaQ a
forceVarsZDeltaQ e f = do
	let expvars = expVars e
	let forceVar var f = \e -> do
		(fs,envvars) <- Reader.ask
		case Map.lookup var envvars of
			Just (dv,Just (thunk,pat)) -> do
				
				let update (fs,env) = (fs,Set.foldr replaceVar env (patPVars pat)) where
					replaceVar var env = Map.insert var (dv,Nothing) env
				Reader.local update $ f $ UInfixE (AppE (VarE 'Inc.read) (VarE thunk)) (VarE '(>>=)) (LamE [pat] e)
			otherwise -> f e
	(Set.foldr forceVar f expvars) e

genZLoadDeltaM :: (Name,Name) -> ForestTy -> [(TH.Pat, TH.Type)] -> ZDeltaQ TH.Exp
genZLoadDeltaM (untyRep,tyRep) forestTy pat_infos = do
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
	dvName      <- State.lift $ newName "dv"
	proxyName      <- State.lift $ newName "proxy"
	dargNames <- lift $ Pure.genForestTupleNames (length pat_infos) "d"
	margNames <- lift $ Pure.genForestTupleNames (length pat_infos) "m"
	(fs,_) <- Reader.ask
	case pat_infos of -- add type argument variables to the environment
		[] -> do
			core_bodyE <- genZLoadDeltaBody (VarE proxyName) (liftM VarE dargsName) pathName treeName repmdName dpathName dfName treeName' dvName (untyRep,tyRep) forestTy
			return $ LamE [VarP proxyName,WildP,VarP pathName,VarP treeName,VarP repmdName,VarP dpathName,VarP dfName,VarP treeName',VarP dvName] core_bodyE
		otherwise -> do
			thunkNames <- lift $ Pure.genForestTupleNames (length pat_infos) "u"
			let update (fs,env) = (fs,foldl updateArg env (zip (zip thunkNames dargNames) pat_infos)) where
				updateArg env ((thunkName,dargName),(pat,ty)) = Map.fromSet insertVar (patPVars pat) `Map.union` env -- left-biased union
					where insertVar var = (AppE (VarE 'isEmptySValueDelta) (VarE dargName),Just (thunkName,pat))
				
			Reader.local update $ do -- adds pattern variable deltas to the env.
				
				newrepmdName <- State.lift $ newName "newrepmd"
				core_bodyE <- genZLoadDeltaBody (VarE proxyName) (liftM VarE dargsName) pathName treeName newrepmdName dpathName dfName treeName' dvName (untyRep,tyRep) forestTy
				let dargsP = AsP (fromJust dargsName) $ Pure.forestTupleP $ map VarP dargNames
				let argsP = AsP argsName (ViewP (VarE 'snd) dargsP)
				let pats = [VarP proxyName,argsP,VarP pathName,VarP treeName,VarP repmdName,VarP dpathName,VarP dfName,VarP treeName',VarP dvName]
				let recBodyE = LamE [Pure.forestTupleP $ map (VarP) thunkNames,VarP newrepmdName] $ DoE [NoBindS core_bodyE]
				return $ LamE pats $ Pure.appE5 (VarE 'doZLoadDeltaArgs) (VarE proxyName) (VarE argsName) (VarE repmdName) (VarE treeName') recBodyE

genZLoadDeltaBody :: TH.Exp -> Maybe TH.Exp -> Name -> Name -> Name -> Name -> Name -> Name -> Name -> (Name,Name) -> ForestTy -> ZDeltaQ TH.Exp
genZLoadDeltaBody proxyE argE pathName treeName repmdName dpathName dfName treeName' dvName (untyRep,tyRep) forestTy = do
	let pathE = VarE pathName
	let treeE = VarE treeName
	let repmdE = VarE repmdName
	let dpathE = VarE dpathName
	let dfE = VarE dfName
	let dvE = VarE dvName
	let treeE' = VarE treeName'
	(fs,_) <- Reader.ask
	case forestTy of
		Directory _ -> zloadDeltaE forestTy pathE treeE repmdE dpathE dfE treeE' dvE
		FConstraint _ (Directory _) _ -> zloadDeltaE forestTy pathE treeE repmdE dpathE dfE treeE' dvE
		otherwise -> do -- add type constructor
			let unfoldE = InfixE (Just $ VarE untyRep) (VarE '(><)) (Just $ VarE 'id)
			rhsE <- zloadDeltaE forestTy pathE treeE (AppE unfoldE repmdE) dpathE dfE treeE' (Pure.appE2 (VarE 'mapValueDelta) (proxyN fs) dvE)
			let isoE = VarE 'mapSValueDelta -- AppE (VarE 'isoSValueDelta) (Pure.appE2 (ConE 'Iso) (ConE tyRep) (VarE untyRep))
			return $ Pure.appE2 (VarE 'liftM) isoE rhsE

zloadDeltaE :: ForestTy -> TH.Exp -> TH.Exp -> TH.Exp -> TH.Exp -> TH.Exp -> TH.Exp -> TH.Exp -> ZDeltaQ TH.Exp
zloadDeltaE forestTy pathE treeE repmdE dpathE dfE treeE' dvE = case forestTy of
	Named ty_name -> zloadDeltaNamed ty_name [] pathE treeE repmdE dpathE dfE treeE' dvE
	Fapp (Named ty_name) argEs -> zloadDeltaNamed ty_name argEs pathE treeE repmdE dpathE dfE treeE' dvE
	File (file_name, argEOpt) -> zcheckUnevaluated "file" treeE' repmdE
		(zloadFile file_name argEOpt pathFilterE pathE' treeE')
		(zloadDeltaFile forestTy argEOpt pathE treeE dpathE dfE treeE' dvE)
	Archive archtype ty -> zcheckStop (archiveExtension archtype) forestTy pathE dpathE repmdE dfE treeE' dvE
		(zloadArchive archtype ty pathFilterE pathE' treeE')
		(zloadDeltaArchive archtype ty pathE dpathE treeE dfE treeE')
	SymLink -> zcheckUnevaluated "symlink" treeE' repmdE
		(zloadSymLink pathE' treeE')
		(zloadDeltaSymLink pathE dpathE treeE dfE treeE' dvE)
	FConstraint pat descTy predE -> zloadDeltaConstraint pat repmdE predE dvE $ \newdvE newrepmdE -> zloadDeltaE descTy pathE treeE newrepmdE dpathE dfE treeE' newdvE	
	(Directory dirTy) -> zcheckStop "directory" forestTy pathE dpathE repmdE dfE treeE' dvE
		(zloadDirectory dirTy pathFilterE pathE' treeE')
		(zloadDeltaDirectory dirTy pathE treeE dpathE dfE treeE')
	FMaybe descTy -> zcheckStop "maybe" forestTy pathE dpathE repmdE dfE treeE' dvE
		(zloadMaybe descTy pathFilterE pathE' treeE')
		(zloadDeltaMaybe descTy treeE pathE dpathE dfE treeE')
	FComp cinfo     -> zcheckStop "compound" forestTy pathE dpathE repmdE dfE treeE' dvE
		(zloadComp cinfo pathFilterE pathE' treeE')
		(zloadDeltaComp cinfo pathE treeE dpathE dfE treeE')
  where pathE' = dpathE
        getMDE = AppE (VarE 'snd) repmdE
        pathFilterE = Pure.appE2 (VarE 'fsTreeDeltaPathFilter) dfE dpathE

-- terminals in the spec
zloadDeltaNamed :: String -> [TH.Exp] -> TH.Exp -> TH.Exp -> TH.Exp -> TH.Exp -> TH.Exp -> TH.Exp -> TH.Exp -> ZDeltaQ TH.Exp
zloadDeltaNamed ty_name [] pathE treeE repmdE dpathE dfE treeE' dvE = do
	(fs,_) <- Reader.ask
	let unit = TupE [TupE [],TupE []]
	return $ Pure.appE9 (VarE 'zloadDelta) (ConE 'Proxy) unit pathE treeE repmdE dpathE dfE treeE' dvE
zloadDeltaNamed ty_name argEs pathE treeE repmdE dpathE dfE treeE' dvE = do
	(fs,_) <- Reader.ask
	let makeArgDelta e = do
		b <- lift $ newName "b"
		condE <- isEmptyZDeltaEnvExp e
		return ((condE,b),AppE (VarE 'makeSValueDelta) (VarE b))
		
	zips <- mapM makeArgDelta argEs
	
	let (conds,dargEs) = unzip zips
	argEs <- mapM (\e -> forceVarsZDeltaQ e return) argEs
	let loadArgs = TupE [(Pure.forestTupleE argEs) , (Pure.forestTupleE dargEs) ]
		
	let runCondsS = map (\(e,b) -> LetS [ValD (VarP b) (NormalB e) []]) conds 
	return $ DoE $ runCondsS ++ [NoBindS $ Pure.appE9 (VarE 'zloadDelta) (ConE 'Proxy) loadArgs pathE treeE repmdE dpathE dfE treeE' dvE]

zloadDeltaArchive :: [ArchiveType] -> ForestTy -> Exp -> Exp -> Exp -> Exp -> Exp -> Exp -> Exp -> ZDeltaQ Exp
zloadDeltaArchive archtype ty pathE dpathE treeE dfE treeE' dvE repmdE = do
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
	let pathFilterE = Pure.appE2 (VarE 'fsTreeDeltaPathFilter) dfE dpathE
	rhsE <- liftM (LamE [newPathP,newGetMDP,newTreeP']) $ runZEnvQ $ zloadE ty pathFilterE newPathE newTreeE' newGetMDE
	rhsDE <- liftM (LamE [newPathP,newDPathP,newRepMdP,newTreeP,newdfP,newTreeP',newdvP]) $ zloadDeltaE ty newPathE newTreeE newRepMdE newDPathE newdfE newTreeE' newdvE
	exts <- lift $ dataToExpQ (\_ -> Nothing) archtype
	isClosedE <- lift $ dataToExpQ (\_ -> Nothing) $ isClosedForestTy ty
	return $ Pure.appE11 (VarE 'doZLoadDeltaArchive) isClosedE exts pathE dpathE treeE dfE treeE' dvE repmdE rhsE rhsDE

zloadDeltaSymLink :: TH.Exp -> TH.Exp -> TH.Exp -> TH.Exp -> Exp -> TH.Exp -> TH.Exp -> ZDeltaQ TH.Exp
zloadDeltaSymLink pathE dpathE treeE dfE treeE' dvE repmdE = return $ Pure.appE7 (VarE 'doZLoadDeltaSymLink) pathE dpathE treeE dfE treeE' dvE repmdE

zloadDeltaMaybe :: ForestTy -> TH.Exp -> TH.Exp -> TH.Exp -> Exp -> TH.Exp -> TH.Exp -> TH.Exp -> ZDeltaQ TH.Exp
zloadDeltaMaybe forestTy treeE pathE dpathE dfE treeE' dvE repmdE = do 
	newrepmdName <- lift $ newName "newrepmd"
	let (newrepmdE,newrepmdP) = genPE newrepmdName
	newGetMDName <- lift $ newName "newgetMD"
	let (newGetMDE,newGetMDP) = genPE newGetMDName
	newdvName <- lift $ newName "newdv"
	let (newdvE,newdvP) = genPE newdvName
	let pathE' = dpathE
	let pathFilterE = Pure.appE2 (VarE 'fsTreeDeltaPathFilter) dfE dpathE
	loadContentNoDeltaE <- liftM (LamE [newGetMDP]) $ runZEnvQ $ zloadE forestTy pathFilterE pathE' treeE' newGetMDE
	loadContentDeltaE <- liftM (LamE [newdvP,newrepmdP]) $ zloadDeltaE forestTy pathE treeE newrepmdE dpathE dfE treeE' newdvE
	return $ Pure.appE9 (VarE 'doZLoadDeltaMaybe) pathE repmdE dpathE treeE dfE treeE' dvE loadContentNoDeltaE loadContentDeltaE

zloadDeltaFile :: ForestTy -> Maybe TH.Exp -> TH.Exp -> TH.Exp -> Exp -> TH.Exp -> TH.Exp -> TH.Exp -> TH.Exp -> ZDeltaQ TH.Exp
zloadDeltaFile ty Nothing pathE treeE dpathE dfE treeE' dvE repmdE = do
	return $ Pure.appE7 (VarE 'doZLoadDeltaFile) pathE dpathE treeE dfE treeE' dvE repmdE
zloadDeltaFile ty (Just argE') pathE treeE dpathE dfE treeE' dvE repmdE = do
	condE <- isEmptyZDeltaEnvForestTy ty -- note that the variables from the argument delta are included in the delta environment
	return $ Pure.appE9 (VarE 'doZLoadDeltaFile1) condE argE' pathE dpathE treeE dfE treeE' dvE repmdE
	
zloadDeltaDirectory :: DirectoryTy -> TH.Exp -> TH.Exp -> TH.Exp -> TH.Exp -> TH.Exp -> Exp -> TH.Exp -> ZDeltaQ TH.Exp
zloadDeltaDirectory dirTy@(Record id fields) pathE treeE dpathE dfE treeE' dvE repmdE = do
	innerGetMDName <- lift $ newName $ "getMD"++id
	let (innerGetMDE,innerGetMDP) = genPE innerGetMDName
	let getMDE = AppE (VarE 'snd) repmdE
	let pathE' = dpathE
	
	innerRepMDName <- lift $ newName $ "repmd"++id
	innerdvName <- lift $ newName $ "dv"++id
	let (innerdvE,innerdvP) = genPE innerdvName
	let (innerRepMDE,innerRepMDP) = genPE innerRepMDName
	(drepEs,stmts) <- zloadFieldsDelta fields pathE treeE innerRepMDE dpathE dfE treeE' innerdvE
	let tyName = mkName id
	let drepE = mergeFieldDeltas drepEs
	
--	let repE = appConE (getStructInnerName tyName) $ map snd repEs
--	let mdE = appConE (getStructInnerMDName tyName) $ map snd mdEs
--	let resultE = TupE [repE,mdE]
	let finalS = NoBindS $ Pure.returnExp $ TupE [drepE] --Pure.appE2 (VarE 'tupM) drepE dmdE
	let pathFilterE = Pure.appE2 (VarE 'fsTreeDeltaPathFilter) dfE dpathE
	doDirNoDeltaE <- liftM (LamE [innerGetMDP]) $ runZEnvQ $ zloadDirectoryContents dirTy pathFilterE pathE' treeE' innerGetMDE
	let doDirDeltaE = LamE [innerdvP,innerRepMDP] $ DoE $ stmts ++ [finalS]
	collectMDs <- lift $ zgenMergeFieldsMDErrors fields	
	return $ Pure.appE10 (VarE 'doZLoadDeltaDirectory) pathE repmdE dpathE treeE dfE treeE' dvE collectMDs doDirNoDeltaE doDirDeltaE

-- add field variables to the environment
zloadFieldsDelta :: [Field] -> TH.Exp -> TH.Exp -> TH.Exp -> TH.Exp -> TH.Exp -> Exp -> TH.Exp -> ZDeltaQ ([Name],[Stmt])
zloadFieldsDelta [] pathE treeE repmdE dpathE dfE treeE' dvE = return ([],[])
zloadFieldsDelta (field:fields) pathE treeE repmdE dpathE dfE treeE' dvE = do
	(rep_name,rep_field, stmts_field)  <- zloadFieldDelta field pathE treeE repmdE dpathE dfE treeE' dvE
	-- updates the delta environment similarly to Forest <x:s1,s2> specifications; adds representation (this) and metadata (this_md) variables to the environment
	let update (fs,env) = (fs,Map.insert rep_name (AppE (VarE 'isEmptySValueDelta) $ VarE rep_field,Nothing) env)
	(reps_fields, stmts_fields) <- Reader.local update $ zloadFieldsDelta fields pathE treeE repmdE dpathE dfE treeE' dvE
	return (rep_field:reps_fields, stmts_field++stmts_fields)

zloadFieldDelta :: Field -> TH.Exp -> TH.Exp -> TH.Exp -> TH.Exp -> TH.Exp -> TH.Exp -> Exp -> ZDeltaQ (Name,Name, [Stmt])
zloadFieldDelta field pathE treeE repmdE dpathE dfE treeE' dvE = case field of
	Simple s -> zloadDeltaSimple s pathE treeE repmdE dpathE dfE treeE' dvE
	Comp   c -> zloadDeltaCompound True c pathE treeE repmdE dpathE dfE treeE' dvE

zloadDeltaSimple :: BasicField -> TH.Exp -> TH.Exp -> TH.Exp -> TH.Exp -> TH.Exp -> TH.Exp -> Exp -> ZDeltaQ (Name,Name,[Stmt])
zloadDeltaSimple (internal, isForm, externalE, forestTy, predM) pathE treeE repmdE dpathE dfE treeE' dvE = do
	-- variable declarations
	let repName = mkName internal
	let drepName = mkName $ "d"++internal
	let (drepE,drepP) = genPE drepName
	newpathName <- lift $ newName "newpath"
	newdpathName <- lift $ newName "newdpath"
	fieldrepmdName <- lift $ newName $ internal++"repmd"
	let (newpathE,newpathP) = genPE newpathName
	let (newdpathE,newdpathP) = genPE newdpathName
	let (fieldrepmdE,fieldrepmdP) = genPE fieldrepmdName
	newdfName <- lift $ newName "newdf"
	let (newdfE,newdfP) = genPE newdfName
	newdvName <- lift $ newName "newdv"
	let (newdvE,newdvP) = genPE newdvName
	
	xName <- lift $ newName "x"
	let (xE,xP) = genPE xName
	yName <- lift $ newName "y"
	let (yE,yP) = genPE yName
	
	let fieldStmt1 = LetS [
		 ValD xP (NormalB $ AppE (VarE repName) $ AppE (VarE 'fst) repmdE) []
		]
	let fieldStmt2 = LetS [
		ValD (VarP repName) (NormalB xE) []
		]
	
	let innerrepmdE = TupE [xE,AppE (VarE 'snd) repmdE]

	(fs,_) <- Reader.ask
	let fielddvE = Pure.appE2 (VarE 'mapValueDelta) (proxyN fs) dvE
	let pathFilterE = Pure.appE2 (VarE 'fsTreeDeltaPathFilter) dfE dpathE
	loadContentNoDeltaE <- liftM (LamE [newpathP,fieldrepmdP]) $ runZEnvQ $ zloadE forestTy pathFilterE newpathE treeE' fieldrepmdE
	loadContentDeltaE <- liftM (LamE [fieldrepmdP,newpathP,newdpathP,newdfP,newdvP]) $ zloadDeltaE forestTy newpathE treeE fieldrepmdE newdpathE newdfE treeE' newdvE
	loadE <- case predM of
		Nothing -> return $ Pure.appE10 (VarE 'doZLoadDeltaSimple) pathE dpathE externalE treeE dfE treeE' fielddvE innerrepmdE loadContentNoDeltaE loadContentDeltaE
		Just predE -> do
			boolE <- isEmptyZDeltaEnvExp predE
			return $ Pure.appE12 (VarE 'doZLoadDeltaSimpleWithConstraint) boolE pathE dpathE externalE treeE dfE treeE' fielddvE innerrepmdE (zmodPredE (VarP repName) predE) loadContentNoDeltaE loadContentDeltaE
	let loadStmt = BindS (TupP [VarP drepName]) loadE
	
	return (repName,drepName,[fieldStmt1,fieldStmt2,loadStmt])

zloadDeltaComp :: CompField -> TH.Exp -> TH.Exp -> TH.Exp -> TH.Exp -> TH.Exp -> Exp -> TH.Exp -> ZDeltaQ TH.Exp
zloadDeltaComp cinfo pathE treeE dpathE dfE treeE' dvE repmdE = do
	newrepmdName <- lift $ newName "newrepmd"
	newGetMDName <- lift $ newName "newGetMD"
	newdvName <- lift $ newName "newdv"
	let (newdvE,newdvP) = genPE newdvName
	let (newrepmdE,newrepmdP) = genPE newrepmdName
	let (newGetMDE,newGetMDP) = genPE newGetMDName
	let getMDE = AppE (VarE 'snd) repmdE
	let pathE' = dpathE
	(_,_,stmts) <- zloadDeltaCompound False cinfo pathE treeE newrepmdE dpathE dfE treeE' newdvE
	let collectMDs = zgenMergeFieldMDErrors (Comp cinfo)
	let pathFilterE = Pure.appE2 (VarE 'fsTreeDeltaPathFilter) dfE dpathE
	doCompNoDeltaE <- liftM (LamE [newGetMDP]) $ runZEnvQ $ zloadCompContents cinfo pathFilterE pathE' treeE' newGetMDE
	let doCompDeltaE = LamE [newdvP,newrepmdP] $ DoE $ init stmts ++ [Pure.unBindS $ last stmts]
	return $ Pure.appE10 (VarE 'doZLoadDeltaDirectory) pathE repmdE dpathE treeE dfE treeE' dvE collectMDs doCompNoDeltaE doCompDeltaE

zloadDeltaCompound :: Bool -> CompField -> TH.Exp -> TH.Exp -> TH.Exp -> TH.Exp -> TH.Exp -> TH.Exp -> Exp -> ZDeltaQ (Name,Name, [Stmt])
zloadDeltaCompound insideDirectory ty@(CompField internal tyConNameOpt explicitName externalE descTy generatorP generatorG predM) pathE treeE repmdE dpathE dfE treeE' dvE = do
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
	newdvName <- lift $ newName "newdv"
	let (newdvE,newdvP) = genPE newdvName
	
	xName <- lift $ newName "x"
	let (xE,xP) = genPE xName
	yName <- lift $ newName "y"
	let (yE,yP) = genPE yName
	
	let fieldStmt1 = LetS [
		 ValD xP (NormalB $ AppE (VarE repName) $ AppE (VarE 'fst) repmdE) []
		]
	let fieldStmt2 = LetS [
		ValD (VarP repName) (NormalB xE) []
		]
	let fieldStmts = if insideDirectory then [fieldStmt1,fieldStmt2] else []
	
	let innerrepmdE = if insideDirectory then TupE [VarE repName,AppE (VarE 'snd) repmdE] else repmdE
	
	let genE = case generatorG of
		Explicit expE -> expE
		Matches regexpE -> regexpE
	
	forceVarsZDeltaQ genE $ \genE -> do
		-- optional filtering
		let fileName = Pure.getCompName explicitName externalE
		
		-- update the environment: add the current filename and filename_att to the delta environment
		let fileNameAtt = mkName $ nameBase fileName++"_att"
		let fileNameAttThunk = mkName $ nameBase fileName++"_att_thunk"
		let dfileName = mkName $ "d"++nameBase fileName
		let dfileNameAtt = mkName $ "d"++nameBase fileName++"_att"
		let (fileNameE,fileNameP) = genPE fileName
		let (fileNameAttE,fileNameAttP) = genPE fileNameAtt
		let (dfileNameE,dfileNameP) = genPE dfileName
		let (dfileNameAttE,dfileNameAttP) = genPE dfileNameAtt
				
		isoE <- lift $ tyConNameOptIso tyConNameOpt
		
		-- actual loading
		(fs,_) <- Reader.ask
		let update (fs,env) = (fs,Map.insert fileName (Pure.appE2 (VarE 'Pure.isSameFileName) fileNameE dfileNameE,Nothing) $ Map.insert fileNameAtt (AppE (VarE 'isEmptySValueDelta) dfileNameAttE,Just (fileNameAttThunk,VarP fileNameAtt)) env)
		let pathFilterE = Pure.appE2 (VarE 'fsTreeDeltaPathFilter) dfE dpathE
		let fielddvE = Pure.appE2 (VarE 'mapValueDelta) (proxyN fs) dvE
		Reader.local update $ case predM of
			Nothing -> do
				loadElementE <- liftM (LamE [fileNameP,VarP fileNameAttThunk,newpathP,fieldrepmdP]) $ runZEnvQ $ zloadE descTy pathFilterE newpathE treeE' fieldrepmdE
				loadElementDeltaE <- liftM (LamE [fileNameP,dfileNameP,VarP fileNameAttThunk,dfileNameAttP,fieldrepmdP,newpathP,newdpathP,newdfP,newdvP]) $ zloadDeltaE descTy newpathE treeE fieldrepmdE newdpathE newdfE treeE' newdvE
				let loadActionE = Pure.appE11 (VarE 'doZLoadDeltaCompound) isoE pathE dpathE genE treeE dfE treeE' fielddvE innerrepmdE loadElementE loadElementDeltaE
				let deltasE = BindS (TupP [drepP]) $ loadActionE
				return (repName,drepName,fieldStmts++[deltasE])
			Just predE -> forceVarsZDeltaQ predE $ \predE -> do
				loadElementE <- liftM (LamE [fileNameP,VarP fileNameAttThunk,newpathP,fieldrepmdP]) $ runZEnvQ $ zloadE descTy pathFilterE newpathE treeE' fieldrepmdE
				loadElementDeltaE <- liftM (LamE [fileNameP,dfileNameP,VarP fileNameAttThunk,dfileNameAttP,fieldrepmdP,newpathP,newdpathP,newdfP,newdvP]) $ zloadDeltaE descTy newpathE treeE fieldrepmdE newdpathE newdfE treeE' newdvE
				let loadActionE = Pure.appE12 (VarE 'doZLoadDeltaCompoundWithConstraint) isoE pathE dpathE genE treeE dfE treeE' fielddvE innerrepmdE (modPredEComp (VarP fileName) predE) loadElementE loadElementDeltaE
				let deltasE = BindS (TupP [drepP]) $ loadActionE
				return (repName,drepName,fieldStmts++[deltasE])

zloadDeltaConstraint :: TH.Pat -> TH.Exp -> TH.Exp -> Exp -> (Exp -> TH.Exp -> ZDeltaQ TH.Exp) -> ZDeltaQ TH.Exp
zloadDeltaConstraint pat repmdE predE dvE load = forceVarsZDeltaQ predE $ \predE -> do
	(fs,_) <- Reader.ask
	newRepMdName <- lift $ newName "newrepmd"
	newdvName <- lift $ newName "newdv"
	let (newRepMdE,newRepMdP) = genPE newRepMdName
	let (newdvE,newdvP) = genPE newdvName
	
	let predFnE = zmodPredE pat predE
	loadAction <- load newdvE newRepMdE
	boolE <- isEmptyZDeltaEnvExp predE
	return $ Pure.appE5 (VarE 'doZLoadDeltaConstraint) boolE dvE repmdE predFnE $ LamE [newdvP,newRepMdP] loadAction

isEmptyZDeltaEnv :: (a -> Set Name) -> a -> ZDeltaQ TH.Exp
isEmptyZDeltaEnv getVars ty = do
	(fs,env) <- Reader.ask
	let tyVars = getVars ty
	let envVars = map (fst . snd) $ Map.toList $ Map.filterWithKey (\v dv -> v `Set.member` tyVars) env
	return $ AppE (VarE 'and) (ListE envVars)

isEmptyZDeltaEnvForestTy :: ForestTy -> ZDeltaQ TH.Exp
isEmptyZDeltaEnvForestTy = isEmptyZDeltaEnv forestTyVars

isEmptyZDeltaEnvField :: Field -> ZDeltaQ TH.Exp
isEmptyZDeltaEnvField = isEmptyZDeltaEnv fieldPVars
	
isEmptyZDeltaEnvExp :: TH.Exp -> ZDeltaQ TH.Exp
isEmptyZDeltaEnvExp = isEmptyZDeltaEnv expVars

zcheckNoChange :: ForestTy -> TH.Exp -> TH.Exp -> ZDeltaQ TH.Exp -> ZDeltaQ TH.Exp
zcheckNoChange ty dpathEs dfE m = do
	cond1 <- isEmptyZDeltaEnvForestTy ty
	let cond2 = AppE (VarE 'isEmptySValueDelta) dpathEs
	let cond3 = AppE (VarE 'isEmptyFSTreeDelta) dfE
	x <- m
	return $ Pure.appE5 (VarE 'zskipUpdateIf3) (LitE $ StringL "checkNoChange") cond1 cond2 cond3 x

zcheckUnevaluated :: String -> TH.Exp -> TH.Exp -> (TH.Exp -> ZEnvQ TH.Exp) -> (TH.Exp -> ZDeltaQ TH.Exp) -> ZDeltaQ TH.Exp
zcheckUnevaluated str treeE' repmdE load loadD = do
	newRepMDName <- lift $ newName "repmd"
	let (newRepMDE,newRepMDP) = genPE newRepMDName
	newGetMDName <- lift $ newName "getMd"
	let (newGetMDE,newGetMDP) = genPE newGetMDName
	z <- runZEnvQ $ load newGetMDE
	x <- loadD newRepMDE
	strE <- lift $ liftString str
	return $ Pure.appE5 (VarE 'zskipUnevaluated) strE treeE' repmdE (LamE [newGetMDP] z) (LamE [newRepMDP] x)

zcheckStop :: String -> ForestTy -> TH.Exp -> TH.Exp -> TH.Exp -> TH.Exp -> Exp -> TH.Exp -> (TH.Exp -> ZEnvQ TH.Exp) -> (TH.Exp -> TH.Exp -> ZDeltaQ TH.Exp) -> ZDeltaQ TH.Exp
zcheckStop str ty pathE dpathE repmdE dfE treeE' dvE load loadD = do
	newRepMDName <- lift $ newName "repmd"
	let (newRepMDE,newRepMDP) = genPE newRepMDName
	newGetMDName <- lift $ newName "getMd"
	let (newGetMDE,newGetMDP) = genPE newGetMDName
	newdvName <- lift $ newName "dv"
	let (newdvE,newdvP) = genPE newdvName
	cond1 <- isEmptyZDeltaEnvForestTy ty
	z <- runZEnvQ $ load newGetMDE
	x <- loadD newdvE newRepMDE
	strE <- lift $ liftString str
	return $ Pure.appE10 (VarE 'zstopIf) (UInfixE strE (VarE '(++)) dpathE) cond1 pathE dpathE dfE treeE' dvE repmdE (LamE [newGetMDP] z) (LamE [newdvP,newRepMDP] x)





