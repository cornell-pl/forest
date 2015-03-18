{-# LANGUAGE TupleSections, TemplateHaskell, NamedFieldPuns, ScopedTypeVariables, RecordWildCards, FlexibleInstances, MultiParamTypeClasses,
    UndecidableInstances, ViewPatterns  #-}

module Language.Forest.IC.CodeGen.ZDeltaLoading where

import Language.Forest.IC.ICRep
import Language.Forest.IC.CodeGen.ZDefault
import Prelude hiding (const,read)
import Language.Forest.IC.CodeGen.Utils
import Control.Monad.Incremental as Inc
import Language.Forest.IC.CodeGen.Loading
import Language.Forest.IC.CodeGen.ZLoading
import Language.Haskell.TH.Quote
import Language.Forest.IC.IO.DeltaLoading
import Language.Forest.IC.IO.ZDeltaLoading
import qualified Language.Forest.Pure.CodeGen.Utils as Pure
import Safe

import Language.Forest.Syntax as PS
import Language.Forest.Pure.MetaData
import Language.Forest.Errors
import Language.Forest.IC.Generic hiding (SymLink)
import qualified Language.Forest.Errors as E
import Language.Forest.FS.FSDelta
import Data.WithClass.MData
import System.Directory
import System.FilePath.Posix
import Language.Forest.IC.BX as BX

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
import qualified Language.Forest.Pure.MetaData as Pure

-- for each variable name, we store (a boolean that indicates whether its value has NOT changed (changes are ALWAYS stable), the name of a thunk that holds its value, an optional pattern to match against the thunk's value when the variable is used)
type ZDeltaEnv = Map Name (Exp,Maybe (Name,Pat))

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

genZLoadDeltaM :: (Name,Name) -> ForestTy -> [(TH.Pat, TH.Type)] -> ZDeltaQ Exp
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
					where insertVar var = (AppE (VarE 'isEmptyDelta) (VarE dargName),Just (thunkName,pat))
				
			Reader.local update $ do -- adds pattern variable deltas to the env.
				
				newrepmdName <- State.lift $ newName "newrepmd"
				core_bodyE <- genZLoadDeltaBody (VarE proxyName) (liftM VarE dargsName) pathName treeName newrepmdName dpathName dfName treeName' dvName (untyRep,tyRep) forestTy
				let dargsP = AsP (fromJustNote "genZLoadDeltaM" dargsName) $ Pure.forestTupleP $ map VarP dargNames
				let argsP = AsP argsName (ViewP (VarE 'snd) dargsP)
				let pats = [VarP proxyName,argsP,VarP pathName,VarP treeName,VarP repmdName,VarP dpathName,VarP dfName,VarP treeName',VarP dvName]
				let recBodyE = LamE [Pure.forestTupleP $ map (VarP) thunkNames,VarP newrepmdName] $ DoE [NoBindS core_bodyE]
				return $ LamE pats $ Pure.appE5 (VarE 'doZLoadDeltaArgs) (VarE proxyName) (VarE argsName) (VarE repmdName) (VarE treeName') recBodyE

genZLoadDeltaBody :: Exp -> Maybe Exp -> Name -> Name -> Name -> Name -> Name -> Name -> Name -> (Name,Name) -> ForestTy -> ZDeltaQ Exp
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
		Directory _ -> zloadDeltaE True forestTy pathE treeE repmdE dpathE dfE treeE' dvE
		FConstraint _ (Directory _) _ -> zloadDeltaE True forestTy pathE treeE repmdE dpathE dfE treeE' dvE
		otherwise -> do -- add type constructor
			let unfoldE = InfixE (Just $ VarE untyRep) (VarE '(><)) (Just $ VarE 'id)
			rhsE <- zloadDeltaE True forestTy pathE treeE (AppE unfoldE repmdE) dpathE dfE treeE' (Pure.appE2 (VarE 'mapValueDelta) (proxyN fs) dvE)
			let isoE = VarE 'mapSValueDelta -- AppE (VarE 'isoSValueDelta) (Pure.appE2 (ConE 'Iso) (ConE tyRep) (VarE untyRep))
			return $ Pure.appE2 (VarE 'liftM) isoE rhsE



hasTopThunkForestTy :: ForestTy -> Bool
hasTopThunkForestTy (Named _) = True
hasTopThunkForestTy (Fapp (Named _) _) = True
hasTopThunkForestTy (FFile _) = False
hasTopThunkForestTy (Archive _ _) = False
hasTopThunkForestTy (FSymLink) = False
hasTopThunkForestTy (FConstraint _ ty _) = hasTopThunkForestTy ty
hasTopThunkForestTy (Directory _) = False
hasTopThunkForestTy (FMaybe _) = False
hasTopThunkForestTy (FComp _) = False

zdiffE :: ForestTy -> Exp
zdiffE ty = VarE $ if hasTopThunkForestTy ty then 'diffValueThunk else 'diffValueAny

zloadDeltaE :: Bool -> ForestTy -> Exp -> Exp -> Exp -> Exp -> Exp -> Exp -> Exp -> ZDeltaQ Exp
zloadDeltaE isTop forestTy pathE treeE repmdE dpathE dfE treeE' dvE = do
	(fs,_) <- Reader.ask
	let pathFilterE = Pure.appE3 (VarE 'fsTreeDPathFilter) (proxyN fs) dfE dpathE
	case forestTy of
		Named ty_name -> zloadDeltaNamed ty_name [] pathE treeE repmdE dpathE dfE treeE' dvE
		Fapp (Named ty_name) argEs -> zloadDeltaNamed ty_name argEs pathE treeE repmdE dpathE dfE treeE' dvE
		FFile (file_name, argEOpt) -> if isTop
			then zcheckLoadUnevaluated "file" dpathE treeE' repmdE
				(zloadFile True file_name argEOpt pathFilterE pathE' treeE')
				(zloadDeltaFile True forestTy argEOpt pathE treeE dpathE dfE treeE' dvE)
			else zloadDeltaFile False forestTy argEOpt pathE treeE dpathE dfE treeE' dvE repmdE
		Archive archtype ty -> if isTop
			then zcheckLoadStop (archiveExtension archtype) forestTy pathE dpathE repmdE dfE treeE' dvE
				(zloadArchive True archtype ty pathFilterE pathE' treeE')
				(zloadDeltaArchive True archtype ty pathE dpathE treeE dfE treeE')
			else zloadDeltaArchive False archtype ty pathE dpathE treeE dfE treeE' dvE repmdE
		FSymLink -> if isTop
			then zcheckLoadUnevaluated "symlink" dpathE treeE' repmdE
				(zloadSymLink True pathE' treeE')
				(zloadDeltaSymLink True pathE dpathE treeE dfE treeE' dvE)
			else zloadDeltaSymLink False pathE dpathE treeE dfE treeE' dvE repmdE
		FConstraint pat descTy predE -> if isTop
			then zcheckLoadStop "constraint" forestTy pathE dpathE repmdE dfE treeE' dvE
				(\getMDE -> zloadConstraint True treeE' pat predE $ zloadE False descTy pathFilterE pathE' treeE' getMDE)
				(\dvE repmdE -> zloadDeltaConstraint True descTy pat treeE treeE' repmdE predE dvE
					(zloadE False descTy pathFilterE pathE' treeE' getMDE)
					(\newdvE newrepmdE -> zloadDeltaE False descTy pathE treeE newrepmdE dpathE dfE treeE' newdvE))
			else zloadDeltaConstraint False descTy pat treeE treeE' repmdE predE dvE
				(zloadE False descTy pathFilterE pathE' treeE' getMDE)
				(\newdvE newrepmdE -> zloadDeltaE False descTy pathE treeE newrepmdE dpathE dfE treeE' newdvE)
		(Directory dirTy) -> if isTop
			then zcheckLoadStop "directory" forestTy pathE dpathE repmdE dfE treeE' dvE
				(zloadDirectory True dirTy pathFilterE pathE' treeE')
				(zloadDeltaDirectory True dirTy pathE treeE dpathE dfE treeE')
			else zloadDeltaDirectory False dirTy pathE treeE dpathE dfE treeE' dvE repmdE
		FMaybe descTy -> if isTop
			then zcheckLoadStop "maybe" forestTy pathE dpathE repmdE dfE treeE' dvE
				(zloadMaybe True descTy pathFilterE pathE' treeE')
				(zloadDeltaMaybe True descTy treeE pathE dpathE dfE treeE')
			else zloadDeltaMaybe False descTy treeE pathE dpathE dfE treeE' dvE repmdE
		FComp cinfo     -> if isTop
			then zcheckLoadStop "compound" forestTy pathE dpathE repmdE dfE treeE' dvE
				(zloadComp True cinfo pathFilterE pathE' treeE')
				(zloadDeltaComp True cinfo pathE treeE dpathE dfE treeE')
			else zloadDeltaComp False cinfo pathE treeE dpathE dfE treeE' dvE repmdE
  where pathE' = dpathE
        getMDE = AppE (VarE 'snd) repmdE

-- terminals in the spec
zloadDeltaNamed :: String -> [Exp] -> Exp -> Exp -> Exp -> Exp -> Exp -> Exp -> Exp -> ZDeltaQ Exp
zloadDeltaNamed ty_name [] pathE treeE repmdE dpathE dfE treeE' dvE = do
	(fs,_) <- Reader.ask
	let unit = TupE [TupE [],TupE []]
	return $ Pure.appE9 (VarE 'zloadDeltaMemo) (ConE 'Proxy) unit pathE treeE repmdE dpathE dfE treeE' dvE
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
	return $ DoE $ runCondsS ++ [NoBindS $ Pure.appE9 (VarE 'zloadDeltaMemo) (ConE 'Proxy) loadArgs pathE treeE repmdE dpathE dfE treeE' dvE]

zloadDeltaArchive :: Bool -> [ArchiveType] -> ForestTy -> Exp -> Exp -> Exp -> Exp -> Exp -> Exp -> Exp -> ZDeltaQ Exp
zloadDeltaArchive isTop archtype ty pathE dpathE treeE dfE treeE' dvE repmdE = do
	(fs,_) <- Reader.ask
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
	let pathFilterE = Pure.appE3 (VarE 'fsTreeDPathFilter) (proxyN fs) dfE dpathE
	rhsE <- liftM (LamE [newPathP,newGetMDP,newTreeP']) $ runZEnvQ $ zloadE False ty pathFilterE newPathE newTreeE' newGetMDE
	defE <- liftM (LamE [newPathP]) $ runZEnvQ $ zdefaultE False ty newPathE
	rhsDE <- liftM (LamE [newPathP,newDPathP,newRepMdP,newTreeP,newdfP,newTreeP',newdvP]) $ zloadDeltaE False ty newPathE newTreeE newRepMdE newDPathE newdfE newTreeE' newdvE
	exts <- lift $ dataToExpQ (\_ -> Nothing) archtype
	isClosedE <- lift $ dataToExpQ (\_ -> Nothing) $ isClosedForestTy ty
	if isTop
		then return $ Pure.appE13 (VarE 'doZLoadDeltaArchive) isClosedE exts pathE dpathE treeE dfE treeE' dvE repmdE rhsE defE rhsDE (zdiffE ty)
		else return $ Pure.appE13 (VarE 'doZLoadDeltaArchiveInner) isClosedE exts pathE dpathE treeE dfE treeE' dvE repmdE rhsE defE rhsDE (zdiffE ty)

zloadDeltaSymLink :: Bool -> Exp -> Exp -> Exp -> Exp -> Exp -> Exp -> Exp -> ZDeltaQ Exp
zloadDeltaSymLink isTop pathE dpathE treeE dfE treeE' dvE repmdE = do
	if isTop
		then return $ Pure.appE7 (VarE 'doZLoadDeltaSymLink) pathE dpathE treeE dfE treeE' dvE repmdE
		else return $ Pure.appE7 (VarE 'doZLoadDeltaSymLinkInner) pathE dpathE treeE dfE treeE' dvE repmdE

zloadDeltaMaybe :: Bool -> ForestTy -> Exp -> Exp -> Exp -> Exp -> Exp -> Exp -> Exp -> ZDeltaQ Exp
zloadDeltaMaybe isTop forestTy treeE pathE dpathE dfE treeE' dvE repmdE = do 
	(fs,_) <- Reader.ask
	newrepmdName <- lift $ newName "newrepmd"
	let (newrepmdE,newrepmdP) = genPE newrepmdName
	newGetMDName <- lift $ newName "newgetMD"
	let (newGetMDE,newGetMDP) = genPE newGetMDName
	newdvName <- lift $ newName "newdv"
	let (newdvE,newdvP) = genPE newdvName
	let pathE' = dpathE
	let pathFilterE = Pure.appE3 (VarE 'fsTreeDPathFilter) (proxyN fs) dfE dpathE
	loadContentNoDeltaE <- liftM (LamE [newGetMDP]) $ runZEnvQ $ zloadE False forestTy pathFilterE pathE' treeE' newGetMDE
	loadContentDeltaE <- liftM (LamE [newdvP,newrepmdP]) $ zloadDeltaE False forestTy pathE treeE newrepmdE dpathE dfE treeE' newdvE
	if isTop
		then return $ Pure.appE10 (VarE 'doZLoadDeltaMaybe) pathE repmdE dpathE treeE dfE treeE' dvE loadContentNoDeltaE loadContentDeltaE (zdiffE forestTy)
		else return $ Pure.appE10 (VarE 'doZLoadDeltaMaybeInner) pathE repmdE dpathE treeE dfE treeE' dvE loadContentNoDeltaE loadContentDeltaE (zdiffE forestTy)

zloadDeltaFile :: Bool -> ForestTy -> Maybe Exp -> Exp -> Exp -> Exp -> Exp -> Exp -> Exp -> Exp -> ZDeltaQ Exp
zloadDeltaFile isTop ty Nothing pathE treeE dpathE dfE treeE' dvE repmdE = do
	condE <- lift $ dataToExpQ (\_ -> Nothing) True
	if isTop
		then return $ Pure.appE9 (VarE 'doZLoadDeltaFile1) condE (Pure.returnExp $ TupE []) pathE dpathE treeE dfE treeE' dvE repmdE
		else return $ Pure.appE9 (VarE 'doZLoadDeltaFileInner1) condE (Pure.returnExp $ TupE []) pathE dpathE treeE dfE treeE' dvE repmdE
zloadDeltaFile isTop ty (Just argE') pathE treeE dpathE dfE treeE' dvE repmdE = do
	condE <- isEmptyZDeltaEnvForestTy ty -- note that the variables from the argument delta are included in the delta environment
	if isTop
		then return $ Pure.appE9 (VarE 'doZLoadDeltaFile1) condE argE' pathE dpathE treeE dfE treeE' dvE repmdE
		else return $ Pure.appE9 (VarE 'doZLoadDeltaFileInner1) condE argE' pathE dpathE treeE dfE treeE' dvE repmdE
	
zloadDeltaDirectory :: Bool -> DirectoryTy -> Exp -> Exp -> Exp -> Exp -> Exp -> Exp -> Exp -> ZDeltaQ Exp
zloadDeltaDirectory isTop dirTy@(Record id fields) pathE treeE dpathE dfE treeE' dvE repmdE = do
	(fs,_) <- Reader.ask
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
	let drepE = mergeFieldNSDeltas drepEs
	
--	let repE = appConE (getStructInnerName tyName) $ map snd repEs
--	let mdE = appConE (getStructInnerMDName tyName) $ map snd mdEs
--	let resultE = TupE [repE,mdE]
	let finalS = NoBindS $ Pure.returnExp $ TupE [drepE] --Pure.appE2 (VarE 'tupM) drepE dmdE
	let pathFilterE = Pure.appE3 (VarE 'fsTreeDPathFilter) (proxyN fs) dfE dpathE
	doDirNoDeltaE <- liftM (LamE [innerGetMDP]) $ runZEnvQ $ zloadDirectoryContents dirTy pathFilterE pathE' treeE' innerGetMDE
	defE <- runZEnvQ $ zdefaultDirectoryContents dirTy pathE'
	let doDirDeltaE = LamE [innerdvP,innerRepMDP] $ DoE $ stmts ++ [finalS]
	let diffE = LamE [WildP,WildP] $ Pure.returnExp $ Pure.appE2 (VarE 'mapValueDelta) (proxyN fs) dvE
	collectMDs <- lift $ zgenMergeFieldsMDErrors fields	
	if isTop
		then return $ Pure.appE12 (VarE 'doZLoadDeltaDirectory) pathE repmdE dpathE treeE dfE treeE' dvE collectMDs doDirNoDeltaE defE doDirDeltaE diffE
		else return $ Pure.appE12 (VarE 'doZLoadDeltaDirectoryInner) pathE repmdE dpathE treeE dfE treeE' dvE collectMDs doDirNoDeltaE defE doDirDeltaE diffE

-- add field variables to the environment
zloadFieldsDelta :: [Field] -> Exp -> Exp -> Exp -> Exp -> Exp -> Exp -> Exp -> ZDeltaQ ([Name],[Stmt])
zloadFieldsDelta [] pathE treeE repmdE dpathE dfE treeE' dvE = return ([],[])
zloadFieldsDelta (field:fields) pathE treeE repmdE dpathE dfE treeE' dvE = do
	(rep_name,rep_field, stmts_field)  <- zloadFieldDelta field pathE treeE repmdE dpathE dfE treeE' dvE
	-- updates the delta environment similarly to Forest <x:s1,s2> specifications; adds representation (this) and metadata (this_md) variables to the environment
	let isEmpty = VarE 'isEmptyDelta
	let update (fs,env) = (fs,Map.insert rep_name (AppE isEmpty (VarE rep_field),Nothing) env)
	(reps_fields, stmts_fields) <- Reader.local update $ zloadFieldsDelta fields pathE treeE repmdE dpathE dfE treeE' dvE
	return (rep_field:reps_fields, stmts_field++stmts_fields)

zloadFieldDelta :: Field -> Exp -> Exp -> Exp -> Exp -> Exp -> Exp -> Exp -> ZDeltaQ (Name,Name, [Stmt])
zloadFieldDelta field pathE treeE repmdE dpathE dfE treeE' dvE = case field of
	Simple s -> zloadDeltaSimple s pathE treeE repmdE dpathE dfE treeE' dvE
	Comp   c -> zloadDeltaCompound True c pathE treeE repmdE dpathE dfE treeE' dvE

zloadDeltaSimple :: BasicField -> Exp -> Exp -> Exp -> Exp -> Exp -> Exp -> Exp -> ZDeltaQ (Name,Name,[Stmt])
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
	
	lensName <- lift $ newName "lens"
	let (lensE,lensP) = genPE lensName
	
	xName <- lift $ newName "x"
	let (xE,xP) = genPE xName
	yName <- lift $ newName "y"
	let (yE,yP) = genPE yName
	
	lensRepE <- lift $ buildFieldLens repName
	
	let fieldStmt1 = LetS [
		 ValD xP (NormalB $ AppE (VarE repName) $ AppE (VarE 'fst) repmdE) []
		,ValD lensP (NormalB lensRepE) []
		]
	let fieldStmt2 = BindS (VarP repName) (AppE (VarE 'inside) $ Pure.appE2 (VarE 'BX.getM) (VarE 'lens_content) $ Pure.returnExp xE)
	
--	let innerrepmdE = TupE [xE,AppE (VarE 'snd) repmdE]

	(fs,_) <- Reader.ask
	let pathFilterE = Pure.appE3 (VarE 'fsTreeDPathFilter) (proxyN fs) dfE dpathE
--	loadContentNoDeltaE <- liftM (LamE [newpathP,fieldrepmdP]) $ runZEnvQ $ zloadE False forestTy pathFilterE newpathE treeE' fieldrepmdE
	loadContentDeltaE <- liftM (LamE [fieldrepmdP,newpathP,newdpathP,newdfP,newdvP]) $ zloadDeltaE False forestTy newpathE treeE fieldrepmdE newdpathE newdfE treeE' newdvE
	loadE <- case predM of
		Nothing -> return $ Pure.appE11 (VarE 'doZLoadDeltaSimple) lensE pathE dpathE externalE treeE dfE treeE' dvE repmdE loadContentDeltaE (zdiffE forestTy)
		Just predE -> do
			boolE <- isEmptyZDeltaEnvExp predE
			return $ Pure.appE13 (VarE 'doZLoadDeltaSimpleWithConstraint) lensE boolE pathE dpathE externalE treeE dfE treeE' dvE repmdE (zmodPredE (VarP repName) predE) loadContentDeltaE (zdiffE forestTy)
	let loadStmt = BindS (TupP [VarP drepName]) loadE
	
	return (repName,drepName,[fieldStmt1,fieldStmt2,loadStmt])

zloadDeltaComp :: Bool -> CompField -> Exp -> Exp -> Exp -> Exp -> Exp -> Exp -> Exp -> ZDeltaQ Exp
zloadDeltaComp isTop cinfo pathE treeE dpathE dfE treeE' dvE repmdE = do
	(fs,_) <- Reader.ask
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
	let pathFilterE = Pure.appE3 (VarE 'fsTreeDPathFilter) (proxyN fs) dfE dpathE
	doCompNoDeltaE <- liftM (LamE [newGetMDP]) $ runZEnvQ $ zloadCompContents cinfo pathFilterE pathE' treeE' newGetMDE
	defE <- runZEnvQ $ zdefaultCompContents cinfo pathE'
	let diffE = LamE [WildP,WildP] $ Pure.returnExp $ Pure.appE2 (VarE 'mapValueDelta) (proxyN fs) dvE
	let doCompDeltaE = LamE [newdvP,newrepmdP] $ DoE $ init stmts ++ [Pure.unBindS $ last stmts]
	if isTop
		then return $ Pure.appE12 (VarE 'doZLoadDeltaDirectory) pathE repmdE dpathE treeE dfE treeE' dvE collectMDs doCompNoDeltaE defE doCompDeltaE diffE
		else return $ Pure.appE12 (VarE 'doZLoadDeltaDirectoryInner) pathE repmdE dpathE treeE dfE treeE' dvE collectMDs doCompNoDeltaE defE doCompDeltaE diffE

zloadDeltaCompound :: Bool -> CompField -> Exp -> Exp -> Exp -> Exp -> Exp -> Exp -> Exp -> ZDeltaQ (Name,Name, [Stmt])
zloadDeltaCompound insideDirectory ty@(CompField internal tyConNameOpt explicitName externalE descTy generatorP generatorTy generatorG predM) pathE treeE repmdE dpathE dfE treeE' dvE = do
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
	lensName <- lift $ newName "lens"
	let (lensE,lensP) = genPE lensName
	
	lensRepE <- if insideDirectory
		then lift $ buildFieldLens repName
		else return idLensE
	
	let fieldStmt1 = LetS [
		 ValD xP (NormalB $ AppE (VarE repName) $ AppE (VarE 'fst) repmdE) []
		,ValD lensP (NormalB lensRepE) []
		]
	let fieldStmt2 = BindS (VarP repName) (AppE (VarE 'inside) $ Pure.appE2 (VarE 'BX.getM) (VarE 'lens_content) $ Pure.returnExp xE)
	let fieldStmts = if insideDirectory then [fieldStmt1,fieldStmt2] else [LetS [ValD lensP (NormalB lensRepE) []]]
	
	let genE = case generatorG of
		Explicit expE -> expE
		Matches regexpE -> regexpE
	
	let keyArgE = case generatorTy of
		Just (key_ty_name,Just argE) -> argE
		otherwise -> Pure.returnExp $ TupE []
	
	forceVarsZDeltaQ keyArgE $ \keyArgE -> forceVarsZDeltaQ genE $ \genE -> do
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
		let update (fs,env) = (fs,Map.insert fileName (Pure.appE2 (VarE '(==)) fileNameE dfileNameE,Nothing) $ Map.insert fileNameAtt (AppE (VarE 'isEmptyDelta) dfileNameAttE,Just (fileNameAttThunk,VarP fileNameAtt)) env)
		let pathFilterE = Pure.appE3 (VarE 'fsTreeDPathFilter) (proxyN fs) dfE dpathE
		Reader.local update $ case predM of
			Nothing -> do
				loadElementE <- liftM (LamE [fileNameP,VarP fileNameAttThunk,newpathP,fieldrepmdP]) $ runZEnvQ $ zloadE False descTy pathFilterE newpathE treeE' fieldrepmdE
				loadElementDeltaE <- liftM (LamE [fileNameP,dfileNameP,VarP fileNameAttThunk,dfileNameAttP,fieldrepmdP,newpathP,newdpathP,newdfP,newdvP]) $ zloadDeltaE False descTy newpathE treeE fieldrepmdE newdpathE newdfE treeE' newdvE
				let loadActionE = Pure.appE14 (VarE 'doZLoadDeltaCompound) lensE isoE keyArgE pathE dpathE genE treeE dfE treeE' dvE repmdE loadElementE loadElementDeltaE (zdiffE descTy)
				let deltasE = BindS (TupP [drepP]) $ loadActionE
				return (repName,drepName,fieldStmts++[deltasE])
			Just predE -> forceVarsZDeltaQ predE $ \predE -> do
				loadElementE <- liftM (LamE [fileNameP,VarP fileNameAttThunk,newpathP,fieldrepmdP]) $ runZEnvQ $ zloadE False descTy pathFilterE newpathE treeE' fieldrepmdE
				loadElementDeltaE <- liftM (LamE [fileNameP,dfileNameP,VarP fileNameAttThunk,dfileNameAttP,fieldrepmdP,newpathP,newdpathP,newdfP,newdvP]) $ zloadDeltaE False descTy newpathE treeE fieldrepmdE newdpathE newdfE treeE' newdvE
				let loadActionE = Pure.appE15 (VarE 'doZLoadDeltaCompoundWithConstraint) lensE isoE keyArgE pathE dpathE genE treeE dfE treeE' dvE repmdE (modPredEComp (VarP fileName) predE) loadElementE loadElementDeltaE (zdiffE descTy)
				let deltasE = BindS (TupP [drepP]) $ loadActionE
				return (repName,drepName,fieldStmts++[deltasE])

zloadDeltaConstraint :: Bool -> ForestTy -> TH.Pat -> Exp -> Exp -> Exp -> Exp -> Exp -> ZEnvQ Exp -> (Exp -> Exp -> ZDeltaQ Exp) -> ZDeltaQ Exp
zloadDeltaConstraint isTop descTy pat treeE treeE' repmdE predE dvE load loadD = forceVarsZDeltaQ predE $ \predE -> do
	(fs,_) <- Reader.ask
	newRepMdName <- lift $ newName "newrepmd"
	newdvName <- lift $ newName "newdv"
	let (newRepMdE,newRepMdP) = genPE newRepMdName
	let (newdvE,newdvP) = genPE newdvName
	
	let predFnE = zmodPredE pat predE
	loadE <- runZEnvQ $ load
	loadDeltaE <- liftM (LamE [newdvP,newRepMdP]) $ loadD newdvE newRepMdE
	boolE <- isEmptyZDeltaEnvExp predE
	if isTop
		then return $ Pure.appE9 (VarE 'doZLoadDeltaConstraint) boolE treeE treeE' dvE repmdE predFnE loadE loadDeltaE (zdiffE descTy)
		else return $ Pure.appE7 (VarE 'doZLoadDeltaConstraintInner) boolE treeE dvE repmdE predFnE loadDeltaE (zdiffE descTy)

isEmptyZDeltaEnv :: (a -> Set Name) -> a -> ZDeltaQ Exp
isEmptyZDeltaEnv getVars ty = do
	(fs,env) <- Reader.ask
	let tyVars = getVars ty
	let envVars = map (fst . snd) $ Map.toList $ Map.filterWithKey (\v dv -> v `Set.member` tyVars) env
	return $ AppE (VarE 'and) (ListE envVars)

isEmptyZDeltaEnvForestTy :: ForestTy -> ZDeltaQ Exp
isEmptyZDeltaEnvForestTy = isEmptyZDeltaEnv forestTyVars

isEmptyZDeltaEnvField :: Field -> ZDeltaQ Exp
isEmptyZDeltaEnvField = isEmptyZDeltaEnv fieldPVars
	
isEmptyZDeltaEnvExp :: Exp -> ZDeltaQ Exp
isEmptyZDeltaEnvExp = isEmptyZDeltaEnv expVars

zcheckLoadUnevaluated :: String -> Exp -> Exp -> Exp -> (Exp -> ZEnvQ Exp) -> (Exp -> ZDeltaQ Exp) -> ZDeltaQ Exp
zcheckLoadUnevaluated str pathE' treeE' repmdE load loadD = do
	newRepMDName <- lift $ newName "repmd"
	let (newRepMDE,newRepMDP) = genPE newRepMDName
	newGetMDName <- lift $ newName "getMd"
	let (newGetMDE,newGetMDP) = genPE newGetMDName
	z <- runZEnvQ $ load newGetMDE
	x <- loadD newRepMDE
	strE <- lift $ liftString str
	return $ Pure.appE6 (VarE 'zskipLoadUnevaluated) strE pathE' treeE' repmdE (LamE [newGetMDP] z) (LamE [newRepMDP] x)

zcheckLoadStop :: String -> ForestTy -> Exp -> Exp -> Exp -> Exp -> Exp -> Exp -> (Exp -> ZEnvQ Exp) -> (Exp -> Exp -> ZDeltaQ Exp) -> ZDeltaQ Exp
zcheckLoadStop str ty pathE dpathE repmdE dfE treeE' dvE load loadD = do
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
	return $ Pure.appE10 (VarE 'zstopLoadIf) (UInfixE strE (VarE '(++)) dpathE) cond1 pathE dpathE dfE treeE' dvE repmdE (LamE [newGetMDP] z) (LamE [newdvP,newRepMDP] x)





