{-# LANGUAGE TupleSections, TemplateHaskell, NamedFieldPuns, ScopedTypeVariables, RecordWildCards, FlexibleInstances, MultiParamTypeClasses,
    UndecidableInstances, ViewPatterns  #-}

module Language.Forest.IC.CodeGen.ZDeltaStoring where

import Language.Forest.IC.ICRep
import Language.Forest.IC.CodeGen.ZDefault
import Prelude hiding (const,read)
import Language.Forest.IC.CodeGen.Utils
import Control.Monad.Incremental as Inc
import Language.Forest.IC.CodeGen.ZStoring
import Language.Haskell.TH.Quote
import Language.Forest.IC.IO.ZDeltaLoading
import Language.Forest.IC.CodeGen.ZDeltaLoading
import Language.Forest.IC.CodeGen.ZLoading
import Language.Forest.IC.IO.ZDeltaStoring
import qualified Language.Forest.Pure.CodeGen.Utils as Pure

import Language.Forest.Syntax as PS
import Language.Forest.Pure.MetaData
import qualified Language.Forest.IC.MetaData as IC
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
import Safe

genZManifestDeltaM :: Name -> ForestTy -> [(TH.Pat, TH.Type)] -> ZDeltaQ Exp
genZManifestDeltaM rep_name forestTy pat_infos = do
	dargsName     <- case pat_infos of
		[] -> return Nothing
		otherwise -> State.lift $ liftM Just $ newName "dargs"
	argsName <- lift $ newName "loadargs"
	margsName     <- case pat_infos of
		[] -> return Nothing
		otherwise -> State.lift $ liftM Just $ newName "margs"
	pathName    <- State.lift $ newName "path"
	pathName'    <- State.lift $ newName "dpath"
	treeName    <- State.lift $ newName "tree"
	treeName'    <- State.lift $ newName "tree'"
	repName     <- State.lift $ newName "rep"
	dfName      <- State.lift $ newName "df"
	dvName      <- State.lift $ newName "dv"
	manName      <- State.lift $ newName "man"
	proxyName      <- State.lift $ newName "proxy"
	dargNames <- lift $ Pure.genForestTupleNames (length pat_infos) "d"
	margNames <- lift $ Pure.genForestTupleNames (length pat_infos) "m"
	(fs,_) <- Reader.ask
	case pat_infos of -- add type argument variables to the environment
		[] -> do
			core_bodyE <- genZManifestDeltaBody (VarE proxyName) (liftM VarE dargsName) pathName pathName' treeName dfName treeName' repName dvName manName rep_name forestTy
			return $ LamE [VarP proxyName,WildP,VarP pathName,VarP pathName',VarP treeName,VarP dfName,VarP treeName',VarP repName,VarP dvName,VarP manName] core_bodyE
		otherwise -> do
			thunkNames <- lift $ Pure.genForestTupleNames (length pat_infos) "u"
			let update (fs,env) = (fs,foldl updateArg env (zip (zip thunkNames dargNames) pat_infos)) where
				updateArg env ((thunkName,dargName),(pat,ty)) = Map.fromSet insertVar (patPVars pat) `Map.union` env -- left-biased union
					where insertVar var = (AppE (VarE 'isEmptyDelta) (VarE dargName),Just (thunkName,pat))
				
			Reader.local update $ do -- adds pattern variable deltas to the env.
				
				newrepName <- State.lift $ newName "newrep"
				newdvName <- State.lift $ newName "newdv"
				newmanName <- State.lift $ newName "newman"
				core_bodyE <- genZManifestDeltaBody (VarE proxyName) (liftM VarE dargsName) pathName pathName' treeName dfName treeName' newrepName newdvName newmanName rep_name forestTy
				let dargsP = AsP (fromJustNote "genZmanifestDeltaM" dargsName) $ Pure.forestTupleP $ map VarP dargNames
				let argsP = AsP argsName (ViewP (VarE 'snd) dargsP)
				let pats = [VarP proxyName,argsP,VarP pathName,VarP pathName',VarP treeName,VarP dfName,VarP treeName',VarP repName,VarP dvName,VarP manName]
				let recBodyE = LamE [Pure.forestTupleP $ map (VarP) thunkNames,VarP newrepName,VarP newdvName,VarP newmanName] $ DoE [NoBindS core_bodyE]
				return $ LamE pats $ Pure.appE6 (VarE 'doZDeltaManifestArgs) (VarE proxyName) (VarE argsName) (VarE repName) (VarE dvName) recBodyE (VarE manName)

genZManifestDeltaBody :: Exp -> Maybe Exp -> Name -> Name -> Name -> Name -> Name -> Name -> Name -> Name -> Name -> ForestTy -> ZDeltaQ Exp
genZManifestDeltaBody proxyE argE pathName pathName' treeName dfName treeName' repName dvName manName repN forestTy = do
	let pathE = VarE pathName
	let treeE = VarE treeName
	let repE = VarE repName
	let pathE' = VarE pathName'
	let dfE = VarE dfName
	let dvE = VarE dvName
	let manE = VarE manName
	let treeE' = VarE treeName'
	(fs,_) <- Reader.ask
	case forestTy of
		Directory _ -> zmanifestDeltaE True forestTy pathE pathE' treeE dfE treeE' repE dvE manE
		FConstraint _ (Directory _) _ -> zmanifestDeltaE True forestTy pathE pathE' treeE dfE treeE' repE dvE manE
		otherwise -> do -- Decompose the representation type constructor
			let repE' = AppE (VarE $ Pure.getUnTyName $ nameBase repN) repE
			let dvE' = Pure.appE2 (VarE 'mapValueDelta) (proxyN fs) dvE
			zmanifestDeltaE True forestTy pathE pathE' treeE dfE treeE' repE' dvE' manE

zmanifestDeltaE :: Bool -> ForestTy -> Exp -> Exp -> Exp -> Exp -> Exp -> Exp -> Exp -> Exp -> ZDeltaQ Exp
zmanifestDeltaE isTop forestTy pathE pathE' treeE dfE treeE' repE dvE manE = case forestTy of
	Named ty_name -> zmanifestDeltaNamed ty_name [] pathE pathE' treeE dfE treeE' repE dvE manE
	Fapp (Named ty_name) argEs -> zmanifestDeltaNamed ty_name argEs pathE pathE' treeE dfE treeE' repE dvE manE
	FFile (file_name, argEOpt) -> if isTop
		then zcheckManifestStop forestTy pathE pathE' dfE treeE' repE dvE manE
			(\newtreeE -> zloadFile True file_name argEOpt filterPathE pathE' newtreeE getMDE)
			(\newdfE newtreeE newdvE newrepmdE -> zloadDeltaFile True forestTy argEOpt (Pure.returnExp pathE) treeE' pathE' newdfE newtreeE newdvE newrepmdE)
			(zmanifestDeltaFile True forestTy argEOpt pathE pathE' treeE dfE treeE')
		else zmanifestDeltaFile False forestTy argEOpt pathE pathE' treeE dfE treeE' repE dvE manE
	Archive archtype ty -> if isTop
		then zcheckManifestStop forestTy pathE pathE' dfE treeE' repE dvE manE
			(\newtreeE -> zloadArchive True archtype ty filterPathE pathE' newtreeE getMDE)	
			(\newdfE newtreeE newdvE newrepmdE -> zloadDeltaArchive True archtype ty (Pure.returnExp pathE) pathE' treeE' newdfE newtreeE newdvE newrepmdE)
			(zmanifestDeltaArchive True archtype ty pathE pathE' treeE dfE treeE')
		else zmanifestDeltaArchive False archtype ty pathE pathE' treeE dfE treeE' repE dvE manE
	FSymLink -> if isTop
		then zcheckManifestStop forestTy pathE pathE' dfE treeE' repE dvE manE
			(\newtreeE -> zloadSymLink True pathE' newtreeE getMDE)	
			(\newdfE newtreeE newdvE newrepmdE -> zloadDeltaSymLink True (Pure.returnExp pathE) pathE' treeE' newdfE newtreeE newdvE newrepmdE)
			(zmanifestDeltaSymLink True pathE pathE' treeE dfE treeE')
		else zmanifestDeltaSymLink False pathE pathE' treeE dfE treeE' repE dvE manE
	FConstraint pat descTy predE -> if isTop
		then zcheckManifestStop forestTy pathE pathE' dfE treeE' repE dvE manE
			(\newtreeE -> zloadConstraint True newtreeE pat predE $ zloadE False descTy filterPathE pathE' newtreeE getMDE)
			(\newdfE newtreeE newdvE newrepmdE -> zloadDeltaE True forestTy (Pure.returnExp pathE) treeE newrepmdE pathE' newdfE newtreeE newdvE)
			(\newRepE newdvE newManE -> zmanifestDeltaConstraint True descTy pat predE treeE newRepE newdvE newManE
				(\newrepE newmanE -> zmanifestE False descTy pathE' treeE' newrepE newmanE)
				(\newrepE newdvE newmanE -> zmanifestDeltaE False descTy pathE pathE' treeE dfE treeE' newrepE newdvE newmanE))
		else zmanifestDeltaConstraint False descTy pat predE treeE repE dvE manE
			(\newrepE newmanE -> zmanifestE False descTy pathE' treeE' newrepE newmanE)
			(\newrepE newdvE newmanE -> zmanifestDeltaE False descTy pathE pathE' treeE dfE treeE' newrepE newdvE newmanE)
	(Directory dirTy) -> if isTop
		then zcheckManifestStop forestTy pathE pathE' dfE treeE' repE dvE manE
			(\newtreeE -> zloadDirectory True dirTy filterPathE pathE' newtreeE getMDE)	
			(\newdfE newtreeE newdvE newrepmdE -> zloadDeltaDirectory True dirTy (Pure.returnExp pathE) treeE' pathE' newdfE newtreeE newdvE newrepmdE)
			(zmanifestDeltaDirectory True dirTy pathE pathE' treeE dfE treeE')
		else zmanifestDeltaDirectory False dirTy pathE pathE' treeE dfE treeE' repE dvE manE
	FMaybe descTy -> if isTop
		then zcheckManifestStop forestTy pathE pathE' dfE treeE' repE dvE manE
			(\newtreeE -> zloadMaybe True descTy filterPathE pathE' newtreeE getMDE)	
			(\newdfE newtreeE newdvE newrepmdE -> zloadDeltaMaybe True descTy treeE' (Pure.returnExp pathE) pathE' newdfE newtreeE newdvE newrepmdE)
			(zmanifestDeltaMaybe True descTy pathE pathE' treeE dfE treeE')
		else zmanifestDeltaMaybe False descTy pathE pathE' treeE dfE treeE' repE dvE manE
	FComp cinfo     -> if isTop
		then  zcheckManifestStop forestTy pathE pathE' dfE treeE' repE dvE manE
			(\newtreeE -> zloadComp True cinfo filterPathE pathE' newtreeE getMDE)	
			(\newdfE newtreeE newdvE newrepmdE -> zloadDeltaComp True cinfo (Pure.returnExp pathE) treeE' pathE' newdfE newtreeE newdvE newrepmdE)
			(zmanifestDeltaComp True cinfo pathE pathE' treeE dfE treeE')
		else zmanifestDeltaComp False cinfo pathE pathE' treeE dfE treeE' repE dvE manE
  where
	getMDE = VarE 'IC.getForestMDInTree
	filterPathE = Pure.constExp $ Pure.returnExp pathE'

-- terminals in the spec
zmanifestDeltaNamed :: String -> [Exp] -> Exp -> Exp -> Exp -> Exp -> Exp -> Exp -> Exp -> Exp -> ZDeltaQ Exp
zmanifestDeltaNamed ty_name [] pathE pathE' treeE dfE treeE' repE dvE manE = do
	(fs,_) <- Reader.ask
	let unit = TupE [TupE [],TupE []]
	return $ Pure.appE10 (VarE 'zupdateManifestDeltaMemo) (ConE 'Proxy) unit pathE pathE' treeE dfE treeE' repE dvE manE
zmanifestDeltaNamed ty_name argEs pathE pathE' treeE dfE treeE' repE dvE manE = do
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
	return $ DoE $ runCondsS ++ [NoBindS $ Pure.appE10 (VarE 'zupdateManifestDeltaMemo) (ConE 'Proxy) loadArgs pathE pathE' treeE dfE treeE' repE dvE manE]

zmanifestDeltaArchive :: Bool -> [ArchiveType] -> ForestTy -> Exp -> Exp -> Exp -> Exp -> Exp -> Exp -> Exp -> Exp -> ZDeltaQ Exp
zmanifestDeltaArchive isTop archtype ty pathE pathE' treeE dfE treeE' repE dvE manE = do
	newPathName <- lift $ newName "new_path"
	newdfName <- lift $ newName "new_df"
	newdvName <- lift $ newName "new_dv"
	newTreeName <- lift $ newName "new_tree"
	newTreeName' <- lift $ newName "new_tree'"
	newDPathName <- lift $ newName "new_dpath"
	newRepName <- lift $ newName "new_rep"
	newManName <- lift $ newName "new_man"
	let (newManE,newManP) = genPE newManName
	let (newPathE, newPathP) = genPE newPathName
	let (newdfE, newdfP) = genPE newdfName
	let (newdvE, newdvP) = genPE newdvName
	let (newTreeE, newTreeP) = genPE newTreeName
	let (newTreeE', newTreeP') = genPE newTreeName'
	let (newPathE', newPathP') = genPE newDPathName
	let (newRepE, newRepP) = genPE newRepName
	rhsE <- liftM (LamE [newPathP',newTreeP',newRepP,newManP]) $ runZEnvQ $ zmanifestE False ty newPathE' newTreeE' newRepE newManE
	rhsDE <- liftM (LamE [newPathP,newPathP',newTreeP,newdfP,newTreeP',newRepP,newdvP,newManP]) $ zmanifestDeltaE False ty newPathE newPathE' newTreeE newdfE newTreeE' newRepE newdvE newManE
	exts <- lift $ dataToExpQ (\_ -> Nothing) archtype
	isClosedE <- lift $ dataToExpQ (\_ -> Nothing) $ isClosedForestTy ty
	if isTop
		then return $ Pure.appE13 (VarE 'doZDeltaManifestArchive) isClosedE exts pathE pathE' treeE dfE treeE' repE dvE rhsE rhsDE (zdiffE ty) manE
		else return $ Pure.appE13 (VarE 'doZDeltaManifestArchiveInner) isClosedE exts pathE pathE' treeE dfE treeE' repE dvE rhsE rhsDE (zdiffE ty) manE

zmanifestDeltaSymLink :: Bool -> Exp -> Exp -> Exp -> Exp -> Exp -> Exp -> Exp -> Exp -> ZDeltaQ Exp
zmanifestDeltaSymLink isTop pathE pathE' treeE dfE treeE' repE dvE manE =
	if isTop
		then return $ Pure.appE8 (VarE 'doZDeltaManifestSymLink) pathE pathE' treeE dfE treeE' repE dvE manE
		else return $ Pure.appE8 (VarE 'doZDeltaManifestSymLinkInner) pathE pathE' treeE dfE treeE' repE dvE manE

zmanifestDeltaMaybe :: Bool -> ForestTy -> Exp -> Exp -> Exp -> Exp -> Exp -> Exp -> Exp -> Exp -> ZDeltaQ Exp
zmanifestDeltaMaybe isTop forestTy pathE pathE' treeE dfE treeE' repE dvE manE = do 
	newrepName <- lift $ newName "newrep"
	let (newrepE,newrepP) = genPE newrepName
	newdvName <- lift $ newName "newdv"
	let (newdvE,newdvP) = genPE newdvName
	newManName <- lift $ newName "newman"
	let (newManE,newManP) = genPE newManName
	manifestContentE <- liftM (LamE [newrepP,newManP]) $ runZEnvQ $ zmanifestE False forestTy pathE' treeE' newrepE newManE
	manifestContentDeltaE <- liftM (LamE [newrepP,newdvP,newManP]) $ zmanifestDeltaE False forestTy pathE pathE' treeE dfE treeE' newrepE newdvE newManE
	if isTop
		then return $ Pure.appE11 (VarE 'doZDeltaManifestMaybe) pathE pathE' treeE dfE treeE' repE dvE manifestContentE manifestContentDeltaE (zdiffE forestTy) manE
		else return $ Pure.appE11 (VarE 'doZDeltaManifestMaybeInner) pathE pathE' treeE dfE treeE' repE dvE manifestContentE manifestContentDeltaE (zdiffE forestTy) manE

zmanifestDeltaFile :: Bool -> ForestTy -> Maybe Exp -> Exp -> Exp -> Exp -> Exp -> Exp -> Exp -> Exp -> Exp -> ZDeltaQ Exp
zmanifestDeltaFile isTop ty Nothing pathE pathE' treeE dfE treeE' repE dvE manE = do
	condE <- lift $ dataToExpQ (\_ -> Nothing) True
	if isTop
		then return $ Pure.appE10 (VarE 'doZDeltaManifestFile1) condE (Pure.returnExp $ TupE []) pathE pathE' treeE dfE treeE' repE dvE manE
		else return $ Pure.appE10 (VarE 'doZDeltaManifestFileInner1) condE (Pure.returnExp $ TupE []) pathE pathE' treeE dfE treeE' repE dvE manE
zmanifestDeltaFile isTop ty (Just argE') pathE pathE' treeE dfE treeE' repE dvE manE = do
	condE <- isEmptyZDeltaEnvForestTy ty -- note that the variables from the argument delta are included in the delta environment
	if isTop
		then return $ Pure.appE10 (VarE 'doZDeltaManifestFile1) condE argE' pathE pathE' treeE dfE treeE' repE dvE manE
		else return $ Pure.appE10 (VarE 'doZDeltaManifestFileInner1) condE argE' pathE pathE' treeE dfE treeE' repE dvE manE
	
zmanifestDeltaDirectory :: Bool -> DirectoryTy -> Exp -> Exp -> Exp -> Exp -> Exp -> Exp -> Exp -> Exp -> ZDeltaQ Exp
zmanifestDeltaDirectory isTop dirTy@(Record id fields) pathE pathE' treeE dfE treeE' repE dvE manE = do
	(fs,_) <- Reader.ask
	innerRepName <- lift $ newName $ "rep"++id
	innerdvName <- lift $ newName $ "dv"++id
	innerManName <- lift $ newName $ "man"++id
	let (innerdvE,innerdvP) = genPE innerdvName
	let (innerRepE,innerRepP) = genPE innerRepName
	let (innerManE,innerManP) = genPE innerManName
	stmts <- zmanifestFieldsDelta fields pathE pathE' treeE dfE treeE' innerRepE innerdvE innerManE
	let tyName = mkName id
	
	doDirNoDeltaE <- liftM (LamE [innerRepP,innerManP]) $ runZEnvQ $ zmanifestDirectoryContents dirTy treeE pathE' innerRepE innerManE
	let doDirDeltaE = LamE [innerRepP,innerdvP,innerManP] $ DoE stmts
	let diffE = LamE [WildP,WildP] $ Pure.returnExp $ Pure.appE2 (VarE 'mapValueDelta) (proxyN fs) dvE
	collectMDs <- lift $ zgenMergeFieldsMDErrors fields	
	if isTop
		then return $ Pure.appE12 (VarE 'doZDeltaManifestDirectory) pathE pathE' treeE dfE treeE' repE dvE collectMDs doDirNoDeltaE doDirDeltaE diffE manE
		else return $ Pure.appE12 (VarE 'doZDeltaManifestDirectoryInner) pathE pathE' treeE dfE treeE' repE dvE collectMDs doDirNoDeltaE doDirDeltaE diffE manE

-- add field variables to the environment
zmanifestFieldsDelta :: [Field] -> Exp -> Exp -> Exp -> Exp -> Exp -> Exp -> Exp -> Exp -> ZDeltaQ [Stmt]
zmanifestFieldsDelta [] pathE pathE' treeE dfE treeE' repE dvE manE = return [NoBindS $ Pure.returnExp manE]
zmanifestFieldsDelta (field:fields) pathE pathE' treeE dfE treeE' repE dvE manE = do
	nextmanName <- lift $ newName "nextman"
	let (nextmanE,nextmanP) = genPE nextmanName
	(drep_name,rep_field, stmts_field)  <- zmanifestFieldDelta field pathE pathE' treeE dfE treeE' repE dvE manE nextmanP
	-- updates the delta environment similarly to Forest <x:s1,s2> specifications; adds representation (this) and metadata (this_md) variables to the environment
	let isEmpty = VarE 'isIdValueDelta
	let update (fs,env) = (fs,Map.insert rep_field (AppE isEmpty (VarE drep_name),Nothing) env)
	stmts_fields <- Reader.local update $ zmanifestFieldsDelta fields pathE pathE' treeE dfE treeE' repE dvE manE
	return (stmts_field++stmts_fields)

zmanifestFieldDelta :: Field -> Exp -> Exp -> Exp -> Exp -> Exp -> Exp -> Exp -> Exp -> Pat -> ZDeltaQ (Name,Name, [Stmt])
zmanifestFieldDelta field pathE pathE' treeE dfE treeE' repE dvE manE nextmanP = case field of
	Simple s -> zmanifestDeltaSimple s pathE pathE' treeE dfE treeE' repE dvE manE nextmanP
	Comp   c -> zmanifestDeltaCompound True c pathE pathE' treeE dfE treeE' repE dvE manE nextmanP

zmanifestDeltaSimple :: BasicField -> Exp -> Exp -> Exp -> Exp -> Exp -> Exp -> Exp -> Exp -> Pat -> ZDeltaQ (Name,Name,[Stmt])
zmanifestDeltaSimple (internal, isForm, externalE, forestTy, predM) pathE pathE' treeE dfE treeE' repE dvE manE nextmanP = do
	-- variable declarations
	let repName = mkName internal
	let drepName = mkName $ "d"++internal
	let (drepE,drepP) = genPE drepName
	newpathName <- lift $ newName "newpath"
	newpathName' <- lift $ newName "newpath'"
	fieldrepName <- lift $ newName $ internal++"rep"
	let (newpathE,newpathP) = genPE newpathName
	let (newpathE',newpathP') = genPE newpathName'
	let (fieldrepE,fieldrepP) = genPE fieldrepName
	newdfName <- lift $ newName "newdf"
	let (newdfE,newdfP) = genPE newdfName
	newdvName <- lift $ newName "newdv"
	let (newdvE,newdvP) = genPE newdvName
	newManName <- lift $ newName "newman"
	let (newManE,newManP) = genPE newManName
	xName <- lift $ newName "x"
	let (xE,xP) = genPE xName
	
	lensRepE <- lift $ buildFieldLens repName
	let innerRepE = AppE (VarE 'lift) $ AppE (VarE 'inside) $ Pure.appE2 (VarE 'BX.getM) (VarE 'lens_content) $ Pure.returnExp $ AppE (VarE repName) repE
	
	let fieldStmt1 = BindS xP innerRepE
	let fieldStmt2 = LetS [
		ValD (VarP repName) (NormalB xE) []
		, ValD (VarP drepName) (NormalB dvE) []
		]
	let fieldStmts = [fieldStmt1,fieldStmt2]

	(fs,_) <- Reader.ask
	manifestContentDeltaE <- liftM (LamE [fieldrepP,newdvP,newpathP,newpathP',newdfP,newManP]) $ zmanifestDeltaE False forestTy newpathE newpathE' treeE newdfE treeE' fieldrepE newdvE newManE
	manifestE <- case predM of
		Nothing -> return $ Pure.appE12 (VarE 'doZDeltaManifestSimple) lensRepE pathE pathE' externalE treeE dfE treeE' repE dvE manifestContentDeltaE (zdiffE forestTy) manE
		Just predE -> do
			boolE <- isEmptyZDeltaEnvExp predE
			return $ Pure.appE14 (VarE 'doZDeltaManifestSimpleWithConstraint) lensRepE boolE (zmodPredE (VarP repName) predE) pathE pathE' externalE treeE dfE treeE' repE dvE manifestContentDeltaE (zdiffE forestTy) manE
	let manifestStmt = BindS (TupP [nextmanP]) manifestE
	
	return (drepName,repName,[manifestStmt]++fieldStmts)

zmanifestDeltaComp :: Bool -> CompField -> Exp -> Exp -> Exp -> Exp -> Exp -> Exp -> Exp -> Exp -> ZDeltaQ Exp
zmanifestDeltaComp isTop cinfo pathE pathE' treeE dfE treeE' repE dvE manE = do
	(fs,_) <- Reader.ask
	newrepName <- lift $ newName "newrep"
	newdvName <- lift $ newName "newdv"
	newManName <- lift $ newName "newman"
	let (newManE,newManP) = genPE newManName
	let (newdvE,newdvP) = genPE newdvName
	let (newrepE,newrepP) = genPE newrepName
	doDeltaE <- zmanifestDeltaCompContents cinfo pathE pathE' treeE dfE treeE' newrepE newdvE newManE
	let collectMDs = zgenMergeFieldMDErrors (Comp cinfo)
	doCompNoDeltaE <- liftM (LamE [newrepP,newManP]) $ runZEnvQ $ zmanifestCompContents cinfo treeE' pathE' newrepE newManE
	let doCompDeltaE = LamE [newrepP,newdvP,newManP] $ doDeltaE
	let diffE = LamE [WildP,WildP] $ Pure.returnExp $ Pure.appE2 (VarE 'mapValueDelta) (proxyN fs) dvE
	if isTop
		then return $ Pure.appE12 (VarE 'doZDeltaManifestDirectory) pathE pathE' treeE dfE treeE' repE dvE collectMDs doCompNoDeltaE doCompDeltaE diffE manE
		else return $ Pure.appE12 (VarE 'doZDeltaManifestDirectoryInner) pathE pathE' treeE dfE treeE' repE dvE collectMDs doCompNoDeltaE doCompDeltaE diffE manE

zmanifestDeltaCompContents :: CompField -> Exp -> Exp -> Exp -> Exp -> Exp -> Exp -> Exp -> Exp -> ZDeltaQ Exp
zmanifestDeltaCompContents cinfo pathE pathE' treeE dfE treeE' repE dvE manE = do
	man1Name <- lift $ newName "man1"
	let (man1E,man1P) = genPE man1Name
	(_,_,stmts) <- zmanifestDeltaCompound False cinfo pathE pathE' treeE dfE treeE' repE dvE manE man1P
	let doCompE = DoE $ stmts ++ [NoBindS $ Pure.returnExp man1E]
	return doCompE

zmanifestDeltaCompound :: Bool -> CompField -> Exp -> Exp -> Exp -> Exp -> Exp -> Exp -> Exp -> Exp -> Pat -> ZDeltaQ (Name,Name, [Stmt])
zmanifestDeltaCompound insideDirectory ty@(CompField internal tyConNameOpt explicitName externalE descTy generatorP generatorTy generatorG predM) pathE pathE' treeE dfE treeE' repE dvE manE nextmanP = do
	-- variable declarataions
	newManName <- lift $ newName "newman"
	let (newManE,newManP) = genPE newManName
	let repName = mkName internal
	let mdName = mkName $ internal++"_md"
	let drepName = mkName $ "d"++internal
	let dmdName  = mkName $ "d"++internal++"_md"
	let (drepE,drepP) = genPE drepName
	let (dmdE,dmdP) = genPE dmdName
	newpathName <- lift $ newName "newpath"
	newpathName' <- lift $ newName "newdpath"
	fieldrepName <- lift $ newName $ internal++"rep"
	let (newpathE,newpathP) = genPE newpathName
	let (newpathE',newpathP') = genPE newpathName'
	let (fieldrepE,fieldrepP) = genPE fieldrepName
	newdfName <- lift $ newName "newdf"
	let (newdfE,newdfP) = genPE newdfName
	newdvName <- lift $ newName "newdv"
	let (newdvE,newdvP) = genPE newdvName
	xName <- lift $ newName "x"
	let (xE,xP) = genPE xName
	
	lensRepE <- if insideDirectory
		then lift $ buildFieldLens repName
		else return idLensE
	let innerRepE = AppE (VarE 'lift) $ AppE (VarE 'inside) $ Pure.appE2 (VarE 'BX.getM) (VarE 'lens_content) $ Pure.returnExp $ if insideDirectory then AppE (VarE repName) repE else repE
	
	let fieldStmt1 = BindS xP innerRepE
	let fieldStmt2 = LetS [
		ValD (VarP repName) (NormalB xE) []
		, ValD (VarP drepName) (NormalB dvE) []
		]
	let fieldStmts = [fieldStmt1,fieldStmt2]
	
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
		Reader.local update $ case predM of
			Nothing -> do
				manifestElementDeltaE <- liftM (LamE [fileNameP,dfileNameP,VarP fileNameAttThunk,dfileNameAttP,fieldrepP,newdvP,newpathP,newpathP',newdfP,newManP]) $ zmanifestDeltaE False descTy newpathE newpathE' treeE newdfE treeE' fieldrepE newdvE newManE
				let manifestActionE = Pure.appE14 (VarE 'doZDeltaManifestCompound) lensRepE isoE keyArgE pathE pathE' genE treeE dfE treeE' repE dvE manifestElementDeltaE (zdiffE descTy) manE
				let deltasE = BindS (TupP [nextmanP]) $ manifestActionE
				return (drepName,repName,[deltasE]++fieldStmts)
			Just predE -> forceVarsZDeltaQ predE $ \predE -> do
				manifestElementDeltaE <- liftM (LamE [fileNameP,dfileNameP,VarP fileNameAttThunk,dfileNameAttP,fieldrepP,newdvP,newpathP,newpathP',newdfP,newManP]) $ zmanifestDeltaConstraintCompound predE treeE fieldrepE newdvE newManE $ zmanifestDeltaE False descTy newpathE newpathE' treeE newdfE treeE'
				boolE <- isEmptyZDeltaEnvForestTy descTy
				let manifestActionE = Pure.appE15 (VarE 'doZDeltaManifestCompoundWithConstraint) lensRepE isoE keyArgE pathE pathE' genE treeE dfE treeE' repE dvE (modPredEComp (VarP fileName) predE) manifestElementDeltaE (zdiffE descTy) manE
				let deltasE = BindS (TupP [nextmanP]) $ manifestActionE
				return (drepName,repName,[deltasE]++fieldStmts)

zmanifestDeltaConstraintCompound :: Exp -> Exp -> Exp -> Exp -> Exp -> (Exp -> Exp -> Exp -> ZDeltaQ Exp) -> ZDeltaQ Exp
zmanifestDeltaConstraintCompound predE treeE repE dvE manE manifest = forceVarsZDeltaQ predE $ \predE -> do
	(fs,_) <- Reader.ask
	newRepName <- lift $ newName "newrep"
	newdvName <- lift $ newName "newdv"
	let (newRepE,newRepP) = genPE newRepName
	let (newdvE,newdvP) = genPE newdvName
	newManName <- lift $ newName "newman"
	let (newManE,newManP) = genPE newManName
	
	manifestAction <- manifest newRepE newdvE newManE
	boolE <- isEmptyZDeltaEnvExp predE
	return $ Pure.appE6 (VarE 'doZDeltaManifestConstraintCompound) boolE predE repE dvE (LamE [newRepP,newdvP,newManP] manifestAction) manE

zmanifestDeltaConstraint :: Bool -> ForestTy -> TH.Pat -> Exp -> Exp -> Exp -> Exp -> Exp -> (Exp -> Exp -> ZEnvQ Exp) -> (Exp -> Exp -> Exp -> ZDeltaQ Exp) -> ZDeltaQ Exp
zmanifestDeltaConstraint isTop ty pat predE treeE repE dvE manE manifest manifestD = forceVarsZDeltaQ predE $ \predE -> do
	(fs,_) <- Reader.ask
	newRepName <- lift $ newName "newrep"
	newdvName <- lift $ newName "newdv"
	let (newRepE,newRepP) = genPE newRepName
	let (newdvE,newdvP) = genPE newdvName
	newManName <- lift $ newName "newman"
	let (newManE,newManP) = genPE newManName
	
	let predFnE = zmodPredE pat predE
	manifestE <- runZEnvQ $ liftM (LamE [newRepP,newManP]) $ manifest newRepE newManE
	manifestDeltaE <- liftM (LamE [newRepP,newdvP,newManP]) $ manifestD newRepE newdvE newManE
	boolE <- isEmptyZDeltaEnvExp predE
	if isTop
		then return $ Pure.appE9 (VarE 'doZDeltaManifestConstraint) boolE predFnE treeE repE dvE manifestE manifestDeltaE (zdiffE ty) manE
		else return $ Pure.appE7 (VarE 'doZDeltaManifestConstraintInner) boolE predFnE treeE repE dvE manifestDeltaE manE

zcheckManifestStop :: ForestTy -> Exp -> Exp -> Exp -> Exp -> Exp -> Exp -> Exp -> (Exp -> ZEnvQ Exp) -> (Exp -> Exp -> Exp -> Exp -> ZDeltaQ Exp) -> (Exp -> Exp -> Exp -> ZDeltaQ Exp) -> ZDeltaQ Exp
zcheckManifestStop ty pathE pathE' dfE treeE' repE dvE manE load loadD manifestD = do
	newTreeName <- lift $ newName "newtree"
	let (newTreeE,newTreeP) = genPE newTreeName
	newRepName <- lift $ newName "rep"
	let (newRepE,newRepP) = genPE newRepName
	newManName <- lift $ newName "man"
	let (newManE,newManP) = genPE newManName
	newdvName <- lift $ newName "dv"
	let (newdvE,newdvP) = genPE newdvName
	newDfName <- lift $ newName "df"
	let (newDfE,newDfP) = genPE newDfName
	cond1 <- isEmptyZDeltaEnvForestTy ty
	loadE <- runZEnvQ $ liftM (LamE [newTreeP]) $ load newTreeE
	loadDeltaE <- liftM (LamE [newDfP,newTreeP,newdvP,newRepP]) $ loadD newDfE newTreeE newdvE newRepE
	manifestDeltaE <- liftM (LamE [newRepP,newdvP,newManP]) $ manifestD newRepE newdvE newManE
	return $ Pure.appE11 (VarE 'zskipManifestIf) cond1 pathE pathE' dfE treeE' repE dvE loadE loadDeltaE manifestDeltaE manE




