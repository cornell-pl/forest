{-# LANGUAGE Rank2Types, DataKinds, KindSignatures, TupleSections, FlexibleContexts, ScopedTypeVariables, GADTs, FlexibleInstances,MultiParamTypeClasses,UndecidableInstances, ViewPatterns #-}

module Language.Forest.IC.IO.DeltaLoading where
	
import Language.Forest.Syntax
import Language.Forest.IC.IO.Loading
import Language.Forest.IO.Utils
import Language.Forest.IC.ValueDelta
import Language.Pads.Padsc
import Language.Forest.IC.MetaData
import Language.Forest.IC.Generic
import Language.Forest.Errors
import Language.Forest.IC.ICRep
import Language.Forest.IC.FS.FSDelta
import qualified Language.Forest.Pure.MetaData as Pure
import Language.Forest.Pure.MetaData (FileInfo(..),FileType(..),(:*:)(..),Arg(..))
import Data.List.Diff
import Data.List as List
import Data.IORef
import Prelude hiding (const,read,mod)
import qualified Prelude

import qualified System.FilePath.Posix
import System.FilePath.Glob
import System.Posix.Env
import System.Posix.Files
import System.Process
import System.Exit
import System.Directory
import System.IO
import System.IO.Unsafe
import Text.Regex
import System.FilePath.Posix
import Control.Monad
import Data.List
import Language.Forest.FS.FSRep
--import Control.Monad.IO.Class
import Data.WithClass.MData
import System.FilePath.Canonical
--import Data.Functor.Identity
import Data.Either
--import Language.Forest.Show
import Language.Forest.IC.Draw
import Data.Unique
import Control.Monad.Incremental as Inc

import qualified Control.Exception as CE

import Data.Data
import Data.Maybe
import System.Random
import Data.Proxy
import Data.Map (Map(..))
import qualified Data.Map as Map
import Language.Forest.IC.IO.Memo
import Language.Forest.IC.BX as BX

doLoadDeltaFile :: (ForestInput fs FSThunk Inside,MData NoCtx (ForestI fs) pads,MData NoCtx (ForestI fs) md,Eq pads,Eq md,MData NoCtx (ForestO fs) pads,ICRep fs,Pads pads md,MData NoCtx (ForestO fs) md) =>
	ForestI fs FilePath -> FilePath -> FSTree fs -> FSTreeDeltaNodeMay -> FSTree fs -> OldData fs (ForestFSThunkI fs pads) (ForestFSThunkI fs (Forest_md fs,ForestFSThunkI fs md))
	-> ForestO fs (SValueDelta (ForestFSThunkI fs pads),SValueDelta (ForestFSThunkI fs (Forest_md fs,ForestFSThunkI fs md)))
doLoadDeltaFile mpath path' oldtree df tree' ((rep_thunk::ForestFSThunkI fs pads,md_thunk),getMD) = do
	path <- inside mpath
	case (path == path',df) of
		(True,isEmptyFSTreeDeltaNodeMay -> True) -> debug "constant0 unchanged" $ return (Id,Id)
		(True,Just (FSTreeChg _ _)) -> debug "constant0 attrs" $ do
			modify md_thunk $ \(_,md) -> getMD path' tree' >>= \fmd' -> return (fmd',md)
			return (Id,Delta)
		otherwise -> debug "constant0 changed" $ do
			(rep_thunk',md_thunk') <- inside $ doLoadFile (Proxy::Proxy pads) path' oldtree df tree' getMD
			overwrite rep_thunk $ Inc.get rep_thunk'
			overwrite md_thunk $ Inc.get md_thunk'
			return (Delta,Delta)

doLoadDeltaFile1 :: (ForestInput fs FSThunk Inside,MData NoCtx (ForestI fs) pads,MData NoCtx (ForestI fs) md,ForestMD fs md,Eq arg,Typeable arg,Eq pads,Eq md,MData NoCtx (ForestO fs) pads,MData NoCtx (ForestO fs) md,ICRep fs,Pads1 arg pads md)
	=> Bool -> arg -> ForestI fs FilePath -> FilePath -> FSTree fs -> FSTreeDeltaNodeMay -> FSTree fs -> OldData fs (ForestFSThunkI fs pads) (ForestFSThunkI fs (Forest_md fs,ForestFSThunkI fs md))
	-> ForestO fs (SValueDelta (ForestFSThunkI fs pads),SValueDelta (ForestFSThunkI fs (Forest_md fs,ForestFSThunkI fs md)))
doLoadDeltaFile1 isEmptyDArg arg' mpath path' oldtree df tree' ((rep_thunk::ForestFSThunkI fs pads,md_thunk),getMD) = do
	path <- inside mpath
	case (isEmptyDArg,path == path',df) of
		(True,True,isEmptyFSTreeDeltaNodeMay -> True) -> debug "constant1 unchanged" $ return (Id,Id)
		(True,True,Just (FSTreeChg _ _)) -> debug "constant1 attrs" $ do
			modify md_thunk $ \(_,md) -> getMD path' tree' >>= \fmd' -> return (fmd',md)
			return (Id,Delta)
		otherwise -> debug "constant1 changed" $ do
			(rep_thunk',md_thunk') <- inside $ doLoadFile1 (Proxy::Proxy pads) arg' path' oldtree df tree' getMD
			overwrite rep_thunk $ Inc.get rep_thunk'
			overwrite md_thunk $ Inc.get md_thunk'
			return (Delta,Delta)

doLoadDeltaArchive :: (ForestInput fs FSThunk Inside,MData NoCtx (ForestI fs) rep,MData NoCtx (ForestI fs) md,ForestMD fs md,MData NoCtx (ForestO fs) rep,MData NoCtx (ForestO fs) md,Eq rep,Eq md,ICRep fs) =>
	[ArchiveType] -> ForestI fs FilePath -> FilePath -> FSTree fs -> FSTreeDeltaNodeMay -> FSTree fs -> OldData fs (ForestFSThunkI fs rep) (ForestFSThunkI fs (Forest_md fs,md))
	-> (FilePath -> GetForestMD fs -> FSTree fs -> FSTreeDeltaNodeMay -> FSTree fs -> ForestI fs (rep,md))
	-> (ForestI fs FilePath -> FilePath -> OldData fs rep md -> FSTree fs -> FSTreeDeltaNodeMay -> FSTree fs -> ForestO fs (SValueDelta rep,SValueDelta md))
	-> ForestO fs (SValueDelta (ForestFSThunkI fs rep),SValueDelta (ForestFSThunkI fs (Forest_md fs,md)))
doLoadDeltaArchive exts mpath path' oldtree df tree' ((rep_thunk,md_thunk),getMD) load loadD = do
	path <- inside mpath
	case (path == path',df) of
		(True,isEmptyFSTreeDeltaNodeMay -> True) -> return (Id,Id)
		(True,Just (FSTreeChg _ _)) -> do
			modify md_thunk $ \(_,imd') -> do
				fmd' <- getMD path' tree'
				fmd'' <- updateForestMDErrorsInsideWith fmd' $ liftM (:[]) $ get_errors imd'
				return (fmd'',imd')
			return (Id,Delta)
		otherwise -> do
			let df' = Just $ FSTreeNew Map.empty (Just path) path -- since @doLoadArchive@ computes a diff anyway, we can always try to reuse any memoized data on the old path
			(rep',md') <- inside $ doLoadArchive exts path' oldtree df' tree' getMD load loadD
			overwrite rep_thunk $ Inc.get rep'
			overwrite md_thunk $ Inc.get md'
			return (Delta,Delta)

doLoadDeltaSymLink :: (ForestInput fs FSThunk Inside,ICRep fs)
	=> ForestI fs FilePath -> FilePath -> FSTree fs -> FSTreeDeltaNodeMay -> FSTree fs
	-> OldData fs (ForestFSThunkI fs FilePath) (ForestFSThunkI fs (Forest_md fs,Base_md))
	-> ForestO fs (SValueDelta (ForestFSThunkI fs FilePath),SValueDelta (ForestFSThunkI fs (Forest_md fs,Base_md)))
doLoadDeltaSymLink mpath path' oldtree df tree' ((rep_thunk,md_thunk),getMD) = do
	path <- inside mpath
	case (path == path',df) of
		(True,isEmptyFSTreeDeltaNodeMay -> True) -> debug "symlink unchanged" $ return (Id,Id)
		(True,Just (FSTreeChg _ _)) -> do
			modify md_thunk $ \(_,md) -> getMD path' tree' >>= \fmd' -> return (fmd',md)
			return (Id,Delta)
		otherwise -> debug "symlink changed" $ do
			(rep',md') <- inside $ doLoadSymLink' path' df tree' getMD
			set rep_thunk rep'
			set md_thunk md'
			return (Delta,Delta)

doLoadDeltaConstraint :: (ForestOutput fs ICThunk Inside,ForestInput fs FSThunk Outside,md ~ ForestFSThunkI fs imd,Eq imd,ForestMD fs imd) =>
	OldData fs rep (md,ForestICThunkI fs Bool)
	-> (OldData fs rep md -> ForestO fs (SValueDelta rep,SValueDelta md))
	-> ForestO fs (SValueDelta rep,SValueDelta (md,ForestICThunkI fs Bool))
doLoadDeltaConstraint ((rep,(md,cond_thunk)),getMD) loadD = debug ("doLoadDeltaConstraint: ") $ do
	(direp,dimd) <- loadD ((rep,md),getMD)
	case (direp,dimd) of
		(Id,Id) -> return (Id,Id)
		otherwise -> do
			isUnevaluated <- inside $ isUnevaluatedFSThunk md
			if isUnevaluated -- just an optimization to preserve the evaluation status of the thunk
				then do
					modify_errors_under md $ \err ->
						force cond_thunk >>= \cond -> if cond then return err else return $ Pure.updateForestErr err [Pure.constraintViolationForestErr]
					debug ("doLoadDeltaConstraintReturn1: ") $ return (direp,Delta)
				else do
					forceOutside cond_thunk >>= \cond -> if cond
						then debug ("doLoadDeltaConstraintReturn2: ") $ return (direp,mapSValueDelta dimd)
						else do
							modify_errors md $ \err -> return $ Pure.updateForestErr err [Pure.constraintViolationForestErr]
							debug ("doLoadDeltaConstraintReturn3: ") $ return (direp,Delta)

-- updates the thunks that keep track of the arguments of a top-level declaration
doLoadDeltaArgs :: (ForestArgs fs args,Eq rep,Eq md,ICRep fs) =>
	Proxy args -> SValueDeltas (ForestICThunksI fs args) -> OldData fs rep (md,ForestICThunksI fs args) -> FSTree fs
	-> (ForestICThunksI fs args -> OldData fs rep md -> ForestO fs (SValueDelta rep,SValueDelta md))
	-> ForestO fs (SValueDelta rep,SValueDelta (md,ForestICThunksI fs args))
doLoadDeltaArgs proxy dargs (((rep,(md,arg_thunks)),getMD)) (tree' :: FSTree fs) loadD = debug ("doLoadDeltaArgs") $ do
	(drep,dmd) <- loadD arg_thunks ((rep,md),getMD)
	return (mapSValueDelta drep,mapSValueDelta dmd `prodSValueDelta` andSValueDeltas (Proxy :: Proxy fs) proxy dargs) 

-- This is the only function that accepts inner non-stable @SValueDelta@s to account for nested comprehensions. In all other cases @SValueDelta@s shall always be stable
doLoadDeltaDirectory :: (MData NoCtx (ForestI fs) md,MData NoCtx (ForestI fs) rep,Eq rep,Eq md,ICRep fs,MData NoCtx (ForestO fs) rep,MData NoCtx (ForestO fs) md) =>
	ForestI fs FilePath -> OldData fs (ForestFSThunkI fs rep) (ForestFSThunkI fs (Forest_md fs,md)) -> FilePath -> FSTree fs -> FSTreeDeltaNodeMay -> FSTree fs
	-> (md -> ForestI fs Forest_err)
	-> (GetForestMD fs -> ForestI fs (rep,md))
	-> (OldData fs rep md -> ForestO fs (NSValueDelta rep,NSValueDelta md))
	-> ForestO fs (SValueDelta (ForestFSThunkI fs rep),SValueDelta (ForestFSThunkI fs (Forest_md fs,md)))
doLoadDeltaDirectory mpath ((rep_thunk,md_thunk),getMD) path' oldtree df tree' collectMDErrors load loadD = do
	path <- inside mpath
	exists <- liftM (doesDirectoryExistInMD path) $ get_fmd_header md_thunk -- to avoid having the old tree materialized, this should be fine with regards to @stopUnevaluated@
	exists' <- forestM $ doesDirectoryExistInTree path' tree'
	debug ("doLoadDeltaDirectory: " ++ show (path,exists,path',exists')) $ case (exists,exists') of
		(False,False) -> case (path == path',isEmptyTopFSTreeDeltaNodeMay df) of
			(True,True) -> return (Id,Id)
			otherwise -> do
				modify md_thunk $ \(_,md1) -> do
					fmd' <- defaultForest_mdWithErrors $ Pure.missingDirForestErr path'
					return (fmd',md1)
				return (Id,Delta)
		(True,False) -> do
			overwrite rep_thunk $ forestdefault
			modify md_thunk $ \(_,md1) -> do
				fmd' <- defaultForest_mdWithErrors $ Pure.missingDirForestErr path'
				return (fmd',md1)
			return (Delta,Delta)
		(True,True) -> do -- this is the ONLY place where non-stable @SValueDelta@s appear!
			irep <- inside $ Inc.get rep_thunk
			md@(fmd,imd) <- inside $ Inc.get md_thunk
			(direp,dimd) <- loadD ((irep,imd),getMD) -- load recursively
			drep <- case direp of
				StableVD d -> return $ liftSValueDelta d
				Modify f -> do
					let irep' = applyNSValueDelta direp irep
					set rep_thunk (f irep') >> return Delta -- modify the directory thunk, since underlying non-stable changes did not occur inside a modifiable
			dmd <- case (path == path',isEmptyTopFSTreeDeltaNodeMay df) of 
				(True,True) -> do -- if the current path stayed the same and no attributes changed, we can reuse its @Forest_md@ but replace its errors; all the changes are stable\
					case dimd of
						StableVD Id -> return Id
						StableVD Delta -> debug ("dir1") $ do
							replaceForestMDErrorsWith fmd $ liftM (:[]) $ collectMDErrors imd
							return Delta
						Modify _ -> error $ "this case should not happen! " ++ show (path',df)
				otherwise -> debug ("dir2") $ do -- otherwise we compute a new @Forest_md@ and add new errors; all the changes are stable
					fmd' <- inside $ getMD path' tree'
					let imd' = applyNSValueDelta dimd imd
					updateForestMDErrorsWith fmd' $ liftM (:[]) $ collectMDErrors imd' -- this forces the value after the update
					set md_thunk (fmd',imd')
					return Delta
			debug ("doLoadDeltaDirectoryReturn: "++show (path,exists,path',exists')++show (direp,dimd)) $ return (drep,dmd) 
		(False,True) -> do -- load from scratch
			(loadRep,loadMd) <- inside $ doLoadDirectory' path' tree' collectMDErrors getMD $ load getMD
			overwrite rep_thunk loadRep
			overwrite md_thunk loadMd
			return (Delta,Delta)

doLoadDeltaMaybe :: (MData NoCtx (ForestO fs) md,Eq rep,Eq md,MData NoCtx (ForestO fs) rep,ForestMD fs md,MData NoCtx (ForestI fs) rep) =>
	ForestI fs FilePath -> OldData fs (ForestFSThunkI fs (Maybe rep)) (ForestFSThunkI fs (Forest_md fs,Maybe md)) -> FilePath -> FSTree fs -> FSTreeDeltaNodeMay -> FSTree fs
	-> (GetForestMD fs -> ForestI fs (rep,md))
	-> (OldData fs rep md -> ForestO fs (SValueDelta rep,SValueDelta md))
	-> ForestO fs (SValueDelta (ForestFSThunkI fs (Maybe rep)),SValueDelta (ForestFSThunkI fs (Forest_md fs,Maybe md)))
doLoadDeltaMaybe mpath ((rep_thunk,md_thunk),getMD) path' oldtree df tree' load loadD = do
	path <- inside mpath
	exists <- liftM (doesExistInMD path) $ get_fmd_header md_thunk -- to avoid having the old tree materialized. this evaluates the inner thunk as well, so compromises @stopUnevaluated@
	exists' <- forestM $ doesExistInTree path' tree'
	case (exists,exists') of
		(False,False) -> do
			case (path == path',isEmptyTopFSTreeDeltaNodeMay df) of
				(True,True) -> return (Id,Id)
				otherwise -> do
					modify md_thunk $ \(_,md1) -> cleanForestMDwithFile path' >>= \fmd' -> return (fmd',md1)
					return (Id,Delta)
		(True,False) -> do
			overwrite rep_thunk forestdefault
			overwrite md_thunk $ cleanForestMDwithFile path' >>= \fmd' -> return (fmd',Nothing)
			return (Delta,Delta)
		(True,True) -> do
			rep <- inside $ Inc.get rep_thunk
			md@(fmd,imd) <- inside $ Inc.get md_thunk
			repimd <- mergeJustDefault (rep,imd)
			(direp,dimd) <- loadD (repimd,getMD) -- load recursively
			overwrite md_thunk $ do
				fmd' <- maybe forestdefault get_fmd_header imd
				return (fmd',imd) -- always update the metadata with the inner @Forest_md@ header
			return (mapSValueDelta direp,Delta)
		(False,True) -> do
			(loadRep,loadMd) <- inside $ doLoadMaybe' path' df tree' $ load getMD
			overwrite rep_thunk loadRep
			overwrite md_thunk loadMd
			return (Delta,Delta)

-- note that in the implementation we do not incrementalize the path-focusing expressions as they (typically) depend on the FS
-- changes the current path, the original data already corresponds to the focused path
doLoadDeltaFocus :: (Eq rep,Eq md,Eq irep,ForestRep rep (ForestFSThunkI fs irep),Matching a,ForestMD fs md) =>
	ForestI fs FilePath -> FilePath -> OldData fs rep md -> a -> FSTree fs -> FSTreeDeltaNodeMay -> FSTree fs
	-> (FilePath -> FSTreeDeltaNodeMay -> GetForestMD fs -> ForestI fs (rep,md))
	-> (ForestI fs FilePath -> FilePath -> FSTreeDeltaNodeMay -> ForestO fs (SValueDelta rep,SValueDelta md))
	-> ForestO fs (SValueDelta rep,SValueDelta md)
doLoadDeltaFocus mpath path' olddata matching oldtree df tree' load loadD = do
	let staticLoad getMD = doLoadFocus path' matching oldtree df tree' getMD load
	let deltaLoad (repmd@(rep,md),getMD) = do
		files' <- forestM $ Pure.getMatchingFilesInTree path' matching tree'
		let mcnewpath = get_fullpath md -- the old (original) path has been canonalized. this DOES NOT evaluate the metadata thunk, to avoid compromising @skipUnevaluated@
		case files' of
			[file'] -> doLoadDeltaNewPath mcnewpath path' file' oldtree df tree' loadD
			files' -> do
				file' <- inside $ pickFileDelta mcnewpath path' files' oldtree df tree'
				doLoadDeltaNewPath mcnewpath path' file' oldtree df tree' $ \mnewpath newdpath df -> do
					(drep,dmd) <- loadD mnewpath newdpath df -- changes the delta
					dmd' <- case dmd of
						Id -> return Id
						otherwise -> if length files' == 0
							then return dmd -- when there is no match an error will pop from the recursive load
							else do
								newpath <- inside mnewpath
								addMultipleMatchesErrorMD newpath files' md >> return Delta
					return (drep,dmd')
	deltaLoad olddata
--	skipUnevaluated "doLoadDeltaFocus" tree' olddata staticLoad deltaLoad -- the call to @skipUnevaluated@ here is to hack around the forceful evaluation caused by the need to use the original path
	
-- tries to pick a file that has been loaded before
-- XXX: revise this if moves are supported!
--XXX: the way things are implemented now we should not evaluate the old path, what would disrupt @skipUnevaluated@
pickFileDelta :: ICRep fs => ForestI fs FilePath -> FilePath -> [FileName] -> FSTree fs -> FSTreeDeltaNodeMay -> FSTree fs -> ForestI fs FileName
pickFileDelta moldpath path' files' oldtree df tree' = do
--	newpaths <- mapM (\f -> liftM (,f) $ canonalizePathInTree (path' </> f) tree') files'
--	case List.lookup oldpath newpaths of
--		Just file -> return file
--		Nothing ->
	inside $ pickFileNoDelta path' files' df tree'

-- changes the delta
doLoadDeltaNewPath :: (ICRep fs) => ForestI fs FilePath -> FilePath -> FilePath -> FSTree fs -> FSTreeDeltaNodeMay -> FSTree fs
	-> (ForestI fs FilePath -> FilePath -> FSTreeDeltaNodeMay -> ForestO fs (SValueDelta rep,SValueDelta md))
	-> ForestO fs (SValueDelta rep,SValueDelta md)
doLoadDeltaNewPath moldpath path' file' oldtree df tree' loadD = debug ("doLoadDeltaNewPath: "++show (path',file',df)) $ do
	newpath' <- forestM $ stepPathInTree tree' path' file'
	let newdf = focusFSTreeDeltaNodeMayByRelativePath df file' -- focusing the tree deltas is important for the skipping conditions to fire for unchanged branches of the FS
	debug ("changed FSDelta: " ++ show newdf) $ loadD moldpath newpath' newdf -- load recursively

doLoadDeltaSimple :: (Eq rep',Eq md',Eq irep,ForestRep rep' (ForestFSThunkI fs irep),Matching a,MData NoCtx (ForestO fs) rep',md' ~ ForestFSThunkI fs imd',Eq imd',ForestMD fs imd') =>
	Lens rep rep' -> Lens md md' -> ForestI fs FilePath -> FilePath -> ForestI fs a -> FSTree fs -> FSTreeDeltaNodeMay -> FSTree fs -> OldData fs rep md
	-> (FilePath -> FSTreeDeltaNodeMay -> GetForestMD fs -> ForestI fs (rep',md'))
	-> (OldData fs rep' md' -> ForestI fs FilePath -> FilePath -> FSTreeDeltaNodeMay -> ForestO fs (SValueDelta rep',SValueDelta md'))
	-> ForestO fs (NSValueDelta rep,NSValueDelta md)
doLoadDeltaSimple lensRep lensMd mpath path' matchingM oldtree df tree' (repmd@(rep,md),getMD) load loadD = debug ("doLoadDeltaSimple: "++show (path')) $ do
	matching <- inside $ matchingM
	let irepmd@(irep,imd) = (BX.get lensRep >< BX.get lensMd) repmd
	let idata = (irepmd,getForestMDInTree) -- we need to discard any previously loaded forest metadata
	(direp,dimd) <- doLoadDeltaFocus mpath path' idata matching oldtree df tree' load $ loadD idata 
	return (nonstableValueDelta $ liftSValueDelta direp,nonstableValueDelta $ liftSValueDelta dimd)

doLoadDeltaSimpleWithConstraint :: (ForestOutput fs ICThunk Inside,Eq rep',Eq md',Eq irep,ForestRep rep' (ForestFSThunkI fs irep),Matching a,MData NoCtx (ForestO fs) rep',md' ~ ForestFSThunkI fs imd',Eq imd',ForestMD fs imd') =>
	Lens rep rep' -> Lens md (md',ForestICThunkI fs Bool) -> ForestI fs FilePath -> FilePath -> ForestI fs a -> FSTree fs -> FSTreeDeltaNodeMay -> FSTree fs -> OldData fs rep md
	-> ((rep',md') -> ForestI fs Bool)
	-> (FilePath -> FSTreeDeltaNodeMay -> GetForestMD fs -> ForestI fs (rep',md'))
	-> (OldData fs rep' md' -> ForestI fs FilePath -> FilePath -> FSTreeDeltaNodeMay -> ForestO fs (SValueDelta rep',SValueDelta md'))
	-> ForestO fs (NSValueDelta rep,NSValueDelta md)
doLoadDeltaSimpleWithConstraint lensRep lensMd mpath path' matchingM oldtree df tree' ((rep,md),getMD) pred load loadD = debug ("doLoadDeltaSimpleWithConstraint: "++show (path')) $ do
	matching <- inside $ matchingM
	let irepmd@(irep,imd) = (BX.get lensRep >< BX.get lensMd) (rep,md)
	let idata = ((irep,imd),getForestMDInTree) -- we need to discard any previously loaded forest metadata
	(direp,dimd) <- doLoadDeltaConstraint idata $ \idata -> doLoadDeltaFocus mpath path' idata matching oldtree df tree' load $ loadD idata 
	return (nonstableValueDelta $ liftSValueDelta direp,nonstableValueDelta $ liftSValueDelta dimd)

doLoadDeltaCompound :: 	(Eq rep',Eq md',Eq irep,ForestRep rep' (ForestFSThunkI fs irep),Matching a,ICRep fs,ForestMD fs md',MData NoCtx (ForestO fs) list_rep',MData NoCtx (ForestO fs) list_imd
	  ,list_rep' ~ [(FileName,rep')],list_imd ~ [(FileName,imd)], imd ~ (md',ForestFSThunkI fs FileInfo)) =>
		Lens rep container_rep' -> Lens md container_md' -> Iso container_rep' list_rep' -> Iso container_md' list_imd
		-> ForestI fs FilePath -> FilePath -> ForestI fs a -> FSTree fs -> FSTreeDeltaNodeMay -> FSTree fs -> OldData fs rep md
		-> (FileName -> ForestFSThunkI fs FileInfo -> FilePath -> FSTreeDeltaNodeMay -> GetForestMD fs -> ForestI fs (rep',md'))
		-> (FileName -> FileName -> ForestFSThunkI fs FileInfo -> SValueDelta (ForestICThunkI fs FileInfo) -> OldData fs rep' md' -> ForestI fs FilePath -> FilePath -> FSTreeDeltaNodeMay -> ForestO fs (SValueDelta rep',SValueDelta md'))
		-> ForestO fs (NSValueDelta rep,NSValueDelta md)
doLoadDeltaCompound lensRep lensMd isoRep isoMd mpath path' matchingM oldtree df tree' ((rep,md),getMD) load loadD = debug ("doLoadDeltaCompound: "++show (path')) $ do
	path <- inside mpath
	matching <- inside $ matchingM -- matching expressions are always recomputed; since they depend on the FS we cannot incrementally reuse them
	let crep = BX.get lensRep rep
	let cmd = BX.get lensMd md
	let oldreplist = to isoRep crep
	let oldmdlist = to isoMd cmd 
	let oldfiles = map fst oldreplist
	let oldrepmds = map snd $ mergeKeyedLists oldreplist oldmdlist -- if the original data is inconsistent, this will produce mangled results
	newfiles <- forestM $ Pure.getMatchingFilesInTree path matching tree'
	(newrepmds,noDeletions) <- forestM $ forestIO $ alignWithLookup newfiles oldfiles oldrepmds -- alignment by filename; newfiles first for the association list to be sorted by newfile positions
	
	-- load each file in the new tree (note that filtering is only done later)
	let loadEachFile (newfile,mbrepmd) = doLoadDeltaCompoundFile mpath path' newfile mbrepmd oldtree df tree' load loadD
	(mergeCompoundSValueDeltas -> (repchanges,repkind),mergeCompoundSValueDeltas -> (mdchanges,mdkind)) <- liftM unzip $ mapM loadEachFile newrepmds
	
	-- return the new container values and deltas on the directory
	let drep = case (noDeletions,repkind) of
			(True,NoOp) -> StableVD Id -- this is OK because the alignment algorithm is stable
			(True,Stable) -> StableVD Delta
			otherwise -> let newrep' = from isoRep repchanges in (Modify $ \s -> put lensRep s newrep')
	let dmd = case (noDeletions,mdkind) of
			(True,NoOp) -> StableVD Id -- this is OK because the alignment algorithm is stable
			(True,Stable) -> StableVD Delta
			otherwise -> let newmd' = from isoMd mdchanges in (Modify $ \s -> put lensMd s newmd')
	return $ debug ("doLoadDeltaCompoundReturn: "++show path') (drep,dmd)

-- returns the new values and a boolean indicating whether it has changed
doLoadDeltaCompoundFile :: (Eq rep,Eq md,Eq irep,ForestRep rep (ForestFSThunkI fs irep),ForestMD fs md, imd ~ (md,ForestFSThunkI fs FileInfo)) => 
	ForestI fs FilePath -> FilePath -> FileName -> Maybe (rep,imd) -> FSTree fs -> FSTreeDeltaNodeMay -> FSTree fs
	-> (FileName -> ForestFSThunkI fs FileInfo -> FilePath -> FSTreeDeltaNodeMay -> GetForestMD fs -> ForestI fs (rep,md))
	-> (FileName -> FileName -> ForestFSThunkI fs FileInfo -> SValueDelta (ForestICThunkI fs FileInfo) -> OldData fs rep md -> ForestI fs FilePath -> FilePath -> FSTreeDeltaNodeMay -> ForestO fs (SValueDelta rep,SValueDelta md))
	-> ForestO fs ((Maybe (FileName,rep),SValueDeltaKind),(Maybe (FileName,(md,ForestFSThunkI fs FileInfo)),SValueDeltaKind))
doLoadDeltaCompoundFile mpath path' file' Nothing oldtree df tree' load loadD = debug ("doLoadDeltaCompoundFileNothing: "++show (path',file')) $ do
	-- try to reuse the original metadata (in this case it is not reused)
	(newGetMD,info) <- inside $ getRelForestMDInTree path' tree' file' >>= \fmd -> return (const2 $ return fmd,fileInfo fmd)
	-- filter new files according to the predicate
	fileInfo_thunk <- inside $ ref info
	(rep',md') <- inside $ doLoadFocus path' file' oldtree df tree' newGetMD $ \newpath newdf newGetMD -> do
		load file' fileInfo_thunk newpath newdf newGetMD
	return ((Just (file',rep'),NonStable),(Just (file',(md',fileInfo_thunk)),NonStable))
doLoadDeltaCompoundFile mpath path' file' (Just (rep,(md,fileInfo_thunk))) oldtree df tree' load loadD = debug ("doLoadDeltaCompoundFile: "++show (path',file')) $ do
	path <- inside mpath
	-- try to reuse the original metadata
	(newGetMD,dFileInfo) <- do
		isUnevaluated <- isUnevaluatedMDThunk md -- just an optimization to preserve the evaluation status of the thunk and avoid the unnecessary loading of old data
		case (isUnevaluated,path == path',isEmptyTopFSTreeDeltaNodeMay $ focusFSTreeDeltaNodeMayByRelativePath df file') of
			(False,True,True) -> get_fmd_header md >>= \fmd -> return (getForestMDInTree,Id) -- we keep the original data
			otherwise -> getRelForestMDInTree path' tree' file' >>= \fmd -> set fileInfo_thunk (fileInfo fmd) >> return (const2 $ return fmd,Delta)
	-- the filename has not changed as long as there was a match in the alignment operation, i.e., doLoadDeltaCompoundFile is called with original data
	(drep,dmd) <- doLoadDeltaFocus mpath path' ((rep,md),newGetMD) file' oldtree df tree' (load file' fileInfo_thunk) $ loadD file' file' fileInfo_thunk dFileInfo ((rep,md),newGetMD) -- load recursively under a different focus
	-- apply the field deltas so that the post-values can be made available in the environment
	let (rep',repkind) = (applySValueDelta drep rep,valueDeltaKind drep) 
	let (md',mdkind) = (applySValueDelta dmd md,valueDeltaKind dmd `andSValueDeltaKinds` (valueDeltaKind dFileInfo))
	return ((Just (file',rep'),repkind),(Just (file',(md',fileInfo_thunk)),mdkind))

doLoadDeltaCompoundWithConstraint :: (ForestOutput fs ICThunk Inside,Eq rep',Eq md',Eq irep,ForestRep rep' (ForestFSThunkI fs irep),Matching a,ICRep fs,ForestMD fs md',MData NoCtx (ForestO fs) list_rep', imd ~ (md',(ForestFSThunkI fs FileInfo,ForestICThunkI fs Bool))
	  ,list_rep' ~ [(FileName,rep')],list_imd ~ [(FileName,imd)]) =>
		Lens rep container_rep' -> Lens md container_md' -> Iso container_rep' list_rep' -> Iso container_md' list_imd
		-> ForestI fs FilePath -> FilePath -> ForestI fs a -> FSTree fs -> FSTreeDeltaNodeMay -> FSTree fs -> OldData fs rep md
		-> (FileName -> ForestFSThunkI fs FileInfo -> ForestI fs Bool)
		-> (FileName -> ForestFSThunkI fs FileInfo -> FilePath -> FSTreeDeltaNodeMay -> GetForestMD fs -> ForestI fs (rep',md'))
		-> (FileName -> FileName -> ForestFSThunkI fs FileInfo -> SValueDelta (ForestICThunkI fs FileInfo) -> OldData fs rep' md' -> ForestI fs FilePath -> FilePath -> FSTreeDeltaNodeMay -> ForestO fs (SValueDelta rep',SValueDelta md'))
		-> ForestO fs (NSValueDelta rep,NSValueDelta md)
doLoadDeltaCompoundWithConstraint lensRep lensMd isoRep isoMd mpath path' matchingM oldtree df tree' ((rep,md),getMD) pred load loadD = debug ("doLoadDeltaCompoundC: "++show (path')) $ do
	path <- inside mpath
	matching <- inside matchingM
	let crep = BX.get lensRep rep
	let cmd = BX.get lensMd md
	let oldreplist = to isoRep crep
	let oldmdlist = to isoMd cmd 
	let oldfiles = map fst oldreplist
	let oldrepmds = map snd $ mergeKeyedLists oldreplist oldmdlist -- if the original data is inconsistent, this will produce mangled results
	newfiles <- forestM $ Pure.getMatchingFilesInTree path matching tree'
	(newrepmds,noDeletions) <- forestM $ forestIO $ alignWithLookup newfiles oldfiles oldrepmds -- alignment by filename; newfiles first for the association list to be sorted by newfile positions
	
	-- load each file in the new tree (note that filtering is only done later)
	let loadEachFile (newfile,mbrepmd) = doLoadDeltaCompoundFileWithConstraint mpath path' newfile mbrepmd oldtree df tree' pred load loadD
	(mergeCompoundSValueDeltas -> (repchanges,repkind),mergeCompoundSValueDeltas -> (mdchanges,mdkind)) <- liftM unzip $ mapM loadEachFile newrepmds
	
	-- return the new container values and deltas on the directory
	let drep = case (noDeletions,repkind) of
			(True,NoOp) -> StableVD Id -- this is OK because the alignment algorithm is stable
			(True,Stable) -> StableVD Delta
			otherwise -> let newrep' = from isoRep repchanges in (Modify $ \s -> put lensRep s newrep')
	let dmd = case (noDeletions,mdkind) of
			(True,NoOp) -> StableVD Id -- this is OK because the alignment algorithm is stable
			(True,Stable) -> StableVD Delta
			otherwise -> let newmd' = from isoMd mdchanges in (Modify $ \s -> put lensMd s newmd')
	return $ debug ("doLoadDeltaCompoundCReturn: "++show (path')) (drep,dmd)

-- returns the new values and a boolean indicating whether it has changed
doLoadDeltaCompoundFileWithConstraint :: (ForestOutput fs ICThunk Inside,Eq rep,Eq md,Eq irep,ForestRep rep (ForestFSThunkI fs irep),ForestMD fs md, imd ~ (md,(ForestFSThunkI fs FileInfo,ForestICThunkI fs Bool))) => 
	ForestI fs FilePath -> FilePath -> FileName -> Maybe (rep,imd) -> FSTree fs -> FSTreeDeltaNodeMay -> FSTree fs
	-> (FileName -> ForestFSThunkI fs FileInfo -> ForestI fs Bool)
	-> (FileName -> ForestFSThunkI fs FileInfo -> FilePath -> FSTreeDeltaNodeMay -> GetForestMD fs -> ForestI fs (rep,md))
	-> (FileName -> FileName -> ForestFSThunkI fs FileInfo -> SValueDelta (ForestICThunkI fs FileInfo) -> OldData fs rep md -> ForestI fs FilePath -> FilePath -> FSTreeDeltaNodeMay -> ForestO fs (SValueDelta rep,SValueDelta md))
	-> ForestO fs ((Maybe (FileName,rep),SValueDeltaKind),(Maybe (FileName,imd),SValueDeltaKind))
doLoadDeltaCompoundFileWithConstraint mpath path' file' Nothing oldtree df tree' pred load loadD = debug ("doLoadDeltaCompoundFileNothing: "++show (path',file')) $ do
	path <- inside mpath
	-- try to reuse the original metadata (in this case it is not reused)
	(newGetMD,info) <- inside $ getRelForestMDInTree path' tree' file' >>= \fmd -> return (const2 $ return fmd,fileInfo fmd)
	fileInfo_thunk <- inside $ ref info
	-- filter new files according to the predicate
	cond_thunk <- inside $ thunk $ pred file' fileInfo_thunk
	cond <- forceOutside cond_thunk
	if cond
		then do
			(rep',md') <- inside $ doLoadFocus path' file' oldtree df tree' newGetMD $ \newpath newdf newGetMD -> do
				load file' fileInfo_thunk newpath newdf newGetMD
			return ((Just (file',rep'),NonStable),(Just (file',(md',(fileInfo_thunk,cond_thunk))),NonStable))
		else return ((Nothing,NonStable),(Nothing,NonStable))
doLoadDeltaCompoundFileWithConstraint mpath path' file' (Just (rep,(md,(fileInfo_thunk,cond_thunk)))) oldtree df tree' pred load loadD = debug ("doLoadDeltaCompoundFile: "++show (path',file')) $ do
	path <- inside mpath
	-- try to reuse the original metadata
	(newGetMD,dFileInfo) <- do
		isUnevaluated <- isUnevaluatedMDThunk md -- just an optimization to preserve the evaluation status of the thunk and avoid the unnecessary loading of old data
		case (isUnevaluated,path == path',isEmptyTopFSTreeDeltaNodeMay $ focusFSTreeDeltaNodeMayByRelativePath df file') of
			(False,True,True) -> get_fmd_header md >>= \fmd -> return (getForestMDInTree,Id) -- we keep the original data
			otherwise -> getRelForestMDInTree path' tree' file' >>= \fmd -> set fileInfo_thunk (fileInfo fmd) >> return (const2 $ return fmd,Delta)
	-- filter new files according to the predicate
	cond <- forceOutside cond_thunk -- this thunk should be automatically updated from the environment changes
	if cond
		then do -- the filename has not changed as long as there was a match in the alignment operation, i.e., doLoadDeltaCompoundFile is called with original data
			(drep,dmd) <- doLoadDeltaFocus mpath path' ((rep,md),newGetMD) file' oldtree df tree' (load file' fileInfo_thunk) $ loadD file' file' fileInfo_thunk dFileInfo ((rep,md),newGetMD) -- load recursively under a different focus
			-- apply the field deltas so that the post-values can be made available in the environment
			let (rep',repkind) = (applySValueDelta drep rep,valueDeltaKind drep) 
			let (md',mdkind) = (applySValueDelta dmd md,valueDeltaKind dmd `andSValueDeltaKinds` (valueDeltaKind dFileInfo))
			return ((Just (file',rep'),repkind),(Just (file',(md',(fileInfo_thunk,cond_thunk))),mdkind))
		else return ((Nothing,NonStable),(Nothing,NonStable))

-- we try @skipUnevaluated@ first to avoid forcing unecessary original data
stopIf :: (Eq rep,Eq md,Eq irep,ForestMD fs md,ICRep fs,rep ~ ForestFSThunkI fs irep,StableMD fs md) =>
	String -> Bool -> ForestI fs FilePath -> FilePath -> FSTreeDeltaNodeMay -> FSTree fs -> OldData fs rep md
	-> (GetForestMD fs -> ForestI fs (rep,md)) -- static loading function
	-> (OldData fs rep md -> ForestO fs (SValueDelta rep,SValueDelta md)) -- delta loading function
	-> ForestO fs (SValueDelta rep,SValueDelta md)
stopIf str isEmptyEnv mpath path' df tree' repmd load loadD = skipUnevaluated str tree' repmd load $ \newrepmd -> do
	path <- inside mpath
	skipUpdateIf3 str isEmptyEnv (path == path') (isEmptyFSTreeDeltaNodeMay df) $ loadD newrepmd

skipUpdateIf3 :: ICRep fs => String -> Bool -> Bool -> Bool -> ForestO fs (SValueDelta rep,SValueDelta md) -> ForestO fs (SValueDelta rep,SValueDelta md)
skipUpdateIf3 str b1 b2 b3 io = if b1 && b2 && b3
	then debug ("skippedUpdate "++str) $ return (Id,Id)
	else io
	
skipUnevaluated :: (Eq rep,Eq md,ICRep fs,Eq irep,ForestRep rep (ForestFSThunkI fs irep),ForestMD fs md,StableMD fs md) =>
	String -> FSTree fs -> OldData fs rep md
	-> (GetForestMD fs -> ForestI fs (rep,md)) -- static loading function
	-> (OldData fs rep md -> ForestO fs (SValueDelta rep,SValueDelta md)) -- delta loading function
	-> ForestO fs (SValueDelta rep,SValueDelta md)
skipUnevaluated str tree' olddata@((rep,md),getMD) load loadD = do
	let isoRep = iso_rep_thunk
	let rep_thunk = to isoRep rep
	cond1 <- inside $ isUnforcedFSThunk rep_thunk
	cond2 <- inside $ isUnforcedMDThunk md
	if cond1 && cond2
		then do -- if the thunks have not been forced, then we can return a NoOp update because we know that their values have not been necessary, so no other part of the specification needs to incrementally repaired
			loadThunk <- inside $ newHSThunk $ load getMD -- load recursively without original data
			overwrite rep_thunk $ Inc.get =<< liftM (to isoRep) (getRep $ read loadThunk)
			overwriteMD md $ getMd $ read loadThunk
			debug ("skippedUnevaluated "++str++" ") $ return (Id,Id) -- we modify the original thunks, but return Id updates
		else do
			{-debug ("NOT UNEVALUATED "++str++" "++show cond1++" "++show cond2) $-} loadD olddata
