{-# LANGUAGE Rank2Types, DataKinds, KindSignatures, TupleSections, FlexibleContexts, ScopedTypeVariables, GADTs, FlexibleInstances,MultiParamTypeClasses,UndecidableInstances, ViewPatterns #-}

module Language.Forest.IC.IO.ZDeltaLoading where
	
import Language.Forest.Syntax
import Language.Forest.IC.IO.ZLoading
import Language.Forest.IC.IO.Loading
import Language.Forest.IC.IO.DeltaLoading
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
import Language.Forest.IC.FS.Diff
import Language.Forest.IO.Shell
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

doZLoadDeltaFile :: (ZippedICMemo fs,ForestInput fs FSThunk Inside,MData NoCtx (ForestI fs) pads,MData NoCtx (ForestI fs) md,Eq pads,Eq md,MData NoCtx (ForestO fs) pads,ICRep fs,Pads pads md,MData NoCtx (ForestO fs) md) =>
	ForestI fs FilePath -> FilePath -> FSTree fs -> FSTreeDeltaNodeMay -> FSTree fs -> ValueDelta fs (ForestFSThunkI fs (Forest_md fs,(pads,md))) -> (ForestFSThunkI fs (Forest_md fs,(pads,md)),GetForestMD fs)
	-> ForestO fs (SValueDelta (ForestFSThunkI fs (Forest_md fs,(pads,md))))
doZLoadDeltaFile mpath path' oldtree df tree' dv (rep_thunk,getMD) = do
	path <- inside mpath
	case (path == path',isIdValueDelta dv,df) of
		(True,True,isEmptyFSTreeDeltaNodeMay -> True) -> debug "constant0 unchanged" $ return Id
		(True,True,Just (FSTreeChg _ _)) -> debug "constant0 attrs" $ do
			modify rep_thunk $ \(_,rep) -> getMD path' tree' >>= \fmd' -> return (fmd',rep)
			return Delta
		otherwise -> debug "constant0 changed" $ do
			rep_thunk' <- inside $ doZLoadFile Proxy (fsTreeDeltaPathFilter df path') path' tree' getMD
			overwrite rep_thunk $ Inc.get rep_thunk'
			return Delta

doZLoadDeltaFile1 :: (ZippedICMemo fs,MData NoCtx (Inside (IncForest fs) IORef IO) arg,ICMemo fs,ForestIs fs arg ~ ForestI fs arg,ForestInput fs FSThunk Inside,MData NoCtx (ForestI fs) pads,MData NoCtx (ForestI fs) md,ForestMD fs md,Eq arg,Typeable arg,Eq pads,Eq md,MData NoCtx (ForestO fs) pads,MData NoCtx (ForestO fs) md,ICRep fs,Pads1 arg pads md)
	=> Bool -> arg -> ForestI fs FilePath -> FilePath -> FSTree fs -> FSTreeDeltaNodeMay -> FSTree fs -> ValueDelta fs (ForestFSThunkI fs (Forest_md fs,(pads,md))) -> (ForestFSThunkI fs (Forest_md fs,(pads,md)),GetForestMD fs)
	-> ForestO fs (SValueDelta (ForestFSThunkI fs (Forest_md fs,(pads,md))))
doZLoadDeltaFile1 isEmptyDArg arg' mpath path' oldtree df tree' dv (rep_thunk,getMD) = do
	path <- inside mpath
	case (isEmptyDArg,path == path',isIdValueDelta dv,df) of
		(True,True,True,(isEmptyFSTreeDeltaNodeMay -> True)) -> debug "constant1 unchanged" $ return Id
		(True,True,True,Just (FSTreeChg _ _)) -> debug "constant1 attrs" $ do
			modify rep_thunk $ \(_,rep) -> getMD path' tree' >>= \fmd' -> return (fmd',rep)
			return Delta
		otherwise -> debug "constant1 changed" $ do
			rep_thunk' <- inside $ doZLoadFile1 (Proxy::Proxy pads) arg' (fsTreeDeltaPathFilter df path') path' tree' getMD
			overwrite rep_thunk $ Inc.get rep_thunk'
			return Delta

doZLoadDeltaArchive :: (ForestRep rep (ForestFSThunkI fs content0),ZippedICMemo fs
		,ForestInput fs FSThunk Inside,MData NoCtx (ForestI fs) rep,ForestMD fs rep,MData NoCtx (ForestO fs) rep,Eq rep) =>
	Bool -> [ArchiveType] -> ForestI fs FilePath -> FilePath -> FSTree fs -> FSTreeDeltaNodeMay -> FSTree fs -> ValueDelta fs (ForestFSThunkI fs (Forest_md fs,rep))
	-> (ForestFSThunkI fs (Forest_md fs,rep),GetForestMD fs)
	-> (FilePath -> GetForestMD fs -> FSTree fs -> ForestI fs rep)
	-> (ForestI fs FilePath -> FilePath -> (rep,GetForestMD fs) -> FSTree fs -> FSTreeDeltaNodeMay -> FSTree fs -> ValueDelta fs rep -> ForestO fs (SValueDelta rep))
	-> ForestO fs (SValueDelta (ForestFSThunkI fs (Forest_md fs,rep)))
doZLoadDeltaArchive isClosed exts mpath path' oldtree df tree' dv (rep_thunk,getMD) load loadD = do
	path <- inside mpath
	case (path == path',isIdValueDelta dv,df) of
		(True,True,isEmptyFSTreeDeltaNodeMay -> True) -> return Id
		(True,True,Just (FSTreeChg _ _)) -> do
			modify rep_thunk $ \(_,irep') -> do
				fmd' <- getMD path' tree'
				fmd'' <- updateForestMDErrorsInsideWith fmd' $ liftM (:[]) $ get_errors irep'
				return (fmd'',irep')
			return Delta
		
		-- if the archive file has been moved, try to reuse originally loaded archive data
		(_,_,Just (FSTreeNew _ (Just from) _)) -> do
			rep' <- inside $ doZLoadArchive isClosed (Proxy::Proxy rep) exts (fsTreeDeltaPathFilter df path') path' tree' getMD load loadD
			overwrite rep_thunk $ Inc.get rep'
			return Delta
		
		-- compute a diff for the archive's content, and continue
		otherwise -> do	
			
			rep@(fmd,irep) <- Inc.getOutside rep_thunk
			
			-- compute the difference for the archive's content
			avfsTree' <- forestM $ virtualTree tree'
			avfsOldTree <- forestM $ virtualTree oldtree
			let pathC = cardinalPath path
			let pathC' = cardinalPath path'
			archiveDf <- forestM $ focusDiffFSTree oldtree pathC tree' pathC'
			
			archiveDv <- inside $ diffValue oldtree irep
			direp <- loadD (return pathC) pathC' (irep,getForestMDInTree) avfsOldTree archiveDf avfsTree' archiveDv
			fmd' <- inside $ getMD path tree'
			updateForestMDErrorsWith fmd' $ liftM (:[]) $ get_errors irep -- like a directory
			set rep_thunk (fmd',irep)
			return Delta

doZLoadDeltaSymLink :: (ForestInput fs FSThunk Inside,ICRep fs)
	=> ForestI fs FilePath -> FilePath -> FSTree fs -> FSTreeDeltaNodeMay -> FSTree fs -> ValueDelta fs (ForestFSThunkI fs (Forest_md fs,(FilePath,Base_md)))
	-> (ForestFSThunkI fs (Forest_md fs,(FilePath,Base_md)),GetForestMD fs)
	-> ForestO fs (SValueDelta (ForestFSThunkI fs (Forest_md fs,(FilePath,Base_md))))
doZLoadDeltaSymLink mpath path' oldtree df tree' dv (rep_thunk,getMD) = do
	path <- inside mpath
	case (path == path',isIdValueDelta dv,df) of
		(True,True,isEmptyFSTreeDeltaNodeMay -> True) -> debug "symlink unchanged" $ return Id
		(True,True,Just (FSTreeChg _ _)) -> do
			modify rep_thunk $ \(_,rep) -> getMD path' tree' >>= \fmd' -> return (fmd',rep)
			return Delta
		otherwise -> debug "symlink changed" $ do
			rep' <- inside $ doZLoadSymLink' path' tree' getMD
			set rep_thunk rep'
			return Delta

doZLoadDeltaConstraint :: (Eq irep,Typeable irep,ForestMD fs irep,ForestMD fs rep,ForestOutput fs ICThunk Inside,ForestInput fs FSThunk Outside,ForestRep rep (ForestFSThunkI fs irep)) =>
	Bool -> ValueDelta fs rep -> (rep,GetForestMD fs) -> (rep -> ForestI fs Bool)
	-> (ValueDelta fs rep -> (rep,GetForestMD fs) -> ForestO fs (SValueDelta rep))
	-> ForestO fs (SValueDelta rep)
doZLoadDeltaConstraint emptyDArgs dv (rep,getMD) pred loadD = debug ("doLoadDeltaConstraint: ") $ do
	direp <- loadD dv (rep,getMD)
	case (emptyDArgs,isIdValueDelta dv,direp) of
		(True,True,Id) -> return Id
		otherwise -> do
			let rep_t = to iso_rep_thunk rep
			isUnevaluated <- inside $ isUnevaluatedFSThunk rep_t
			if isUnevaluated -- just an optimization to preserve the evaluation status of the thunk
				then do
					modify_errors_under rep_t $ \err -> do
						cond <- pred rep
						if cond then return err else return $ Pure.updateForestErr err [Pure.constraintViolationForestErr]
					debug ("doLoadDeltaConstraintReturn1: ") $ return Delta
				else do
					cond <- inside $ pred rep
					if cond
						then debug ("doLoadDeltaConstraintReturn2: ") $ return $ mapSValueDelta direp
						else do
							modify_errors rep $ \err -> return $ Pure.updateForestErr err [Pure.constraintViolationForestErr]
							debug ("doLoadDeltaConstraintReturn3: ") $ return Delta

-- updates the thunks that keep track of the arguments of a top-level declaration
doZLoadDeltaArgs :: (ForestArgs fs args,Eq rep,ICRep fs) =>
	Proxy args -> LoadDeltaArgs ICData fs args -> (rep,GetForestMD fs) -> FSTree fs
	-> (ForestICThunksI fs args -> (rep,GetForestMD fs) -> ForestO fs (SValueDelta rep))
	-> ForestO fs (SValueDelta rep)
doZLoadDeltaArgs proxy (margs,_) ((rep,getMD)) (tree' :: FSTree fs) loadD = debug ("doLoadDeltaArgs") $ do
	arg_thunks <- inside $ newArgs (Proxy :: Proxy fs) proxy margs -- creates new thunks to hold the new expressions
	(drep) <- loadD arg_thunks ((rep),getMD)
	return (mapSValueDelta drep) 

-- This is the only function that accepts inner non-stable @SValueDelta@s to account for nested comprehensions. In all other cases @SValueDelta@s shall always be stable
doZLoadDeltaDirectory :: (MData NoCtx (ForestI fs) rep,Eq rep,ICRep fs,MData NoCtx (ForestO fs) rep) =>
	ForestI fs FilePath -> (ForestFSThunkI fs (Forest_md fs,rep),GetForestMD fs) -> FilePath -> FSTree fs -> FSTreeDeltaNodeMay -> FSTree fs
	-> ValueDelta fs (ForestFSThunkI fs (Forest_md fs,rep))
	-> (rep -> ForestI fs Forest_err)
	-> (GetForestMD fs -> ForestI fs rep)
	-> (ValueDelta fs rep -> (rep,GetForestMD fs) -> ForestO fs (NSValueDelta rep))
	-> ForestO fs (SValueDelta (ForestFSThunkI fs (Forest_md fs,rep)))
doZLoadDeltaDirectory mpath (rep_thunk,getMD) path' oldtree df tree' dv collectMDErrors load loadD = do
	path <- inside mpath
	exists <- liftM (doesDirectoryExistInMD path) $ get_fmd_header rep_thunk -- to avoid having the old tree materialized, this should be fine with regards to @stopUnevaluated@
	exists' <- forestM $ doesDirectoryExistInTree path' tree'
	debug ("doLoadDeltaDirectory: " ++ show (path,exists,path',exists')) $ case (exists,exists') of
		(False,False) -> case (path == path',isIdValueDelta dv,isEmptyTopFSTreeDeltaNodeMay df) of
			(True,True,True) -> return Id
			otherwise -> do
				modify rep_thunk $ \(_,rep1) -> do
					fmd' <- defaultForest_mdWithErrors $ Pure.missingDirForestErr path'
					return (fmd',rep1)
				return Delta
		(True,False) -> do
			modify rep_thunk $ \(_,rep1) -> do
				fmd' <- defaultForest_mdWithErrors $ Pure.missingDirForestErr path'
				return (fmd',rep1)
			return Delta
		(True,True) -> do -- this is the ONLY place where non-stable @NSValueDelta@s appear!
			rep@(fmd,irep) <- inside $ Inc.get rep_thunk
			let idv = mapValueDelta Proxy dv
			direp <- loadD idv (irep,getMD) -- load recursively
			drep <- case (path == path',isIdValueDelta dv,isEmptyTopFSTreeDeltaNodeMay df) of 
				(True,True,True) -> do -- if the current path stayed the same and no attributes changed, we can reuse its @Forest_md@ but replace its errors; all the changes are stable
					case direp of
						StableVD Id -> return Id
						StableVD Delta -> debug ("dir1") $ do
							replaceForestMDErrorsWith fmd $ liftM (:[]) $ collectMDErrors irep
							return Delta
						Modify f -> do
							fmd' <- inside $ getMD path' tree'
							let irep' = applyNSValueDelta direp irep
							updateForestMDErrorsWith fmd' $ liftM (:[]) $ collectMDErrors irep' -- this forces the value after the update
							set rep_thunk (fmd',f irep') >> return Delta -- modify the directory thunk, since underlying non-stable changes did not occur inside a modifiable
				otherwise -> debug ("dir2") $ do -- otherwise we compute a new @Forest_md@ and add new errors; all the changes are stable
					fmd' <- inside $ getMD path' tree'
					let irep' = applyNSValueDelta direp irep
					updateForestMDErrorsWith fmd' $ liftM (:[]) $ collectMDErrors irep' -- this forces the value after the update
					set rep_thunk (fmd',irep')
					return Delta
			debug ("doLoadDeltaDirectoryReturn: "++show (path,exists,path',exists')++show direp) $ return drep
		(False,True) -> do -- load from scratch
			overwrite rep_thunk $ doZLoadDirectory' path' tree' collectMDErrors getMD $ load getMD
			return Delta

doZLoadDeltaMaybe :: (ForestRep rep (ForestFSThunk fs Inside irep),Eq rep,MData NoCtx (ForestO fs) rep,ForestMD fs rep,MData NoCtx (ForestI fs) rep) =>
	ForestI fs FilePath -> (ForestFSThunkI fs (Forest_md fs,Maybe rep),GetForestMD fs) -> FilePath -> FSTree fs -> FSTreeDeltaNodeMay -> FSTree fs
	-> ValueDelta fs (ForestFSThunkI fs (Forest_md fs,Maybe rep))
	-> (GetForestMD fs -> ForestI fs rep)
	-> (ValueDelta fs rep -> (rep,GetForestMD fs) -> ForestO fs (SValueDelta rep))
	-> ForestO fs (SValueDelta (ForestFSThunkI fs (Forest_md fs,Maybe rep)))
doZLoadDeltaMaybe mpath (rep_thunk,getMD) path' oldtree df tree' dv load loadD = do
	path <- inside mpath
	exists <- liftM (doesExistInMD path) $ get_fmd_header rep_thunk -- to avoid having the old tree materialized. this evaluates the inner thunk as well, so compromises @stopUnevaluated@
	exists' <- forestM $ doesExistInTree path' tree'
	case (exists,exists') of
		(False,False) -> do
			case (path == path',isIdValueDelta dv,isEmptyTopFSTreeDeltaNodeMay df) of
				(True,True,True) -> return Id
				otherwise -> do
					modify rep_thunk $ \(_,rep1) -> cleanForestMDwithFile path' >>= \fmd' -> return (fmd',rep1)
					return Delta
		(True,False) -> do
			overwrite rep_thunk $ cleanForestMDwithFile path' >>= \fmd' -> return (fmd',Nothing)
			return Delta
		(True,True) -> do
			rep@(fmd,irep) <- inside $ Inc.get rep_thunk
			irep' <- maybe forestdefault return irep
			idv <- inside $ diffValue oldtree irep'
			direp <- loadD idv (irep',getMD) -- load recursively
			overwrite rep_thunk $ do
				fmd' <- maybe forestdefault get_fmd_header irep
				return (fmd',irep) -- always update the metadata with the inner @Forest_md@ header
			return Delta
		(False,True) -> do
			overwrite rep_thunk $ doZLoadMaybe' (fsTreeDeltaPathFilter df path') path' tree' $ load getMD
			return Delta

-- note that in the implementation we do not incrementalize the path-focusing expressions as they (typically) depend on the FS
-- changes the current path, the original data already corresponds to the focused path
doZLoadDeltaFocus :: (Typeable irep,Eq irep,ForestRep rep (ForestFSThunkI fs irep),Typeable rep,StableMD fs rep,Eq rep,Matching fs a,ForestMD fs rep) =>
	ForestI fs FilePath -> FilePath -> (rep,GetForestMD fs) -> a -> FSTree fs -> FSTreeDeltaNodeMay -> FSTree fs
	-> ValueDelta fs rep
	-> (FilePath -> GetForestMD fs -> ForestI fs rep)
	-> (ForestI fs FilePath -> FilePath -> FSTreeDeltaNodeMay -> ValueDelta fs rep -> ForestO fs (SValueDelta rep))
	-> ForestO fs (SValueDelta rep)
doZLoadDeltaFocus mpath path' olddata matching oldtree df tree' dv load loadD = do
	let staticLoad getMD = doZLoadFocus (fsTreeDeltaPathFilter df path') path' matching tree' getMD load
	let deltaLoad (rep,getMD) = do
		files' <- forestM $ getMatchingFilesInTree path' matching tree'
		let mcnewpath = get_fullpath rep -- the old (original) path has been canonalized. this DOES NOT evaluate the metadata thunk, to avoid compromising @skipUnevaluated@
		case files' of
			[file'] -> doZLoadDeltaNewPath mcnewpath path' file' oldtree df tree' $ \x y z -> loadD x y z dv
			files' -> do
				file' <- inside $ pickFileDelta mcnewpath path' files' oldtree df tree'
				doZLoadDeltaNewPath mcnewpath path' file' oldtree df tree' $ \mnewpath newdpath df -> do
					drep <- loadD mnewpath newdpath df dv -- changes the delta
					drep' <- case drep of
						Id -> return Id
						otherwise -> if length files' == 0
							then return drep -- when there is no match an error will pop from the recursive load
							else do
								newpath <- inside mnewpath
								addMultipleMatchesErrorMD newpath files' rep >> return Delta
					return drep'
	deltaLoad olddata
	zskipUnevaluated "doLoadDeltaFocus" tree' olddata staticLoad deltaLoad -- the call to @skipUnevaluated@ here is to hack around the forceful evaluation caused by the need to use the original path

-- changes the delta
doZLoadDeltaNewPath :: (ICRep fs) => ForestI fs FilePath -> FilePath -> FilePath -> FSTree fs -> FSTreeDeltaNodeMay -> FSTree fs
	-> (ForestI fs FilePath -> FilePath -> FSTreeDeltaNodeMay -> ForestO fs (SValueDelta rep))
	-> ForestO fs (SValueDelta rep)
doZLoadDeltaNewPath moldpath path' file' oldtree df tree' loadD = debug ("doLoadDeltaNewPath: "++show (path',file',df)) $ do
	newpath' <- forestM $ stepPathInTree tree' path' file'
	let newdf = focusFSTreeDeltaNodeMayByRelativePath df file' -- focusing the tree deltas is important for the skipping conditions to fire for unchanged branches of the FS
	debug ("changed FSDelta: " ++ show newdf) $ loadD moldpath newpath' newdf -- load recursively

doZLoadDeltaSimple :: (Typeable irep,Eq irep,ForestRep rep' (ForestFSThunkI fs irep),StableMD fs rep',ForestMD fs rep',Eq rep',Matching fs a,MData NoCtx (ForestO fs) rep') =>
	Lens dir_rep rep'
	-> ForestI fs FilePath -> FilePath -> ForestI fs a -> FSTree fs -> FSTreeDeltaNodeMay -> FSTree fs -> ValueDelta fs dir_rep -> (dir_rep,GetForestMD fs)
	-> (FilePath -> GetForestMD fs -> ForestI fs rep')
	-> ((rep',GetForestMD fs) -> ForestI fs FilePath -> FilePath -> FSTreeDeltaNodeMay -> ValueDelta fs rep' -> ForestO fs (SValueDelta rep'))
	-> ForestO fs (NSValueDelta dir_rep)
doZLoadDeltaSimple lens mpath path' matchingM oldtree df tree' dv (dir_rep,getMD) load loadD = debug ("doLoadDeltaSimple: "++show path') $ do
	matching <- inside $ matchingM
	let irep = BX.get lens dir_rep
	let idata = (irep,getForestMDInTree) -- we need to discard any previously loaded forest metadata
	direp <- doZLoadDeltaFocus mpath path' idata matching oldtree df tree' (mapValueDelta Proxy dv) load $ loadD idata 
	return $ nonstableValueDelta $ liftSValueDelta direp

doZLoadDeltaSimpleWithConstraint :: (Typeable irep,ForestMD fs rep',ForestMD fs irep,StableMD fs rep',ForestOutput fs ICThunk Inside,Eq rep',Eq irep,ForestRep rep' (ForestFSThunkI fs irep),Matching fs a,MData NoCtx (ForestO fs) rep') =>
	Lens dir_rep rep'
	-> Bool -> ForestI fs FilePath -> FilePath -> ForestI fs a -> FSTree fs -> FSTreeDeltaNodeMay -> FSTree fs -> ValueDelta fs dir_rep -> (dir_rep,GetForestMD fs)
	-> (rep' -> ForestI fs Bool)
	-> (FilePath -> GetForestMD fs -> ForestI fs rep')
	-> ((rep',GetForestMD fs) -> ForestI fs FilePath -> FilePath -> FSTreeDeltaNodeMay -> ValueDelta fs rep' -> ForestO fs (SValueDelta rep'))
	-> ForestO fs (NSValueDelta dir_rep)
doZLoadDeltaSimpleWithConstraint lens emptyDArgs mpath path' matchingM oldtree df tree' dv (dir_rep,getMD) pred load loadD = debug ("doLoadDeltaSimpleWithConstraint: "++show (path')) $ do
	matching <- inside $ matchingM
	let irep = BX.get lens dir_rep
	let idata = (irep,getForestMDInTree) -- we need to discard any previously loaded forest metadata
	direp <- doZLoadDeltaConstraint emptyDArgs (mapValueDelta Proxy dv) idata pred $ \dv idata -> doZLoadDeltaFocus mpath path' idata matching oldtree df tree' dv load $ loadD idata 
	return $ nonstableValueDelta $ liftSValueDelta direp

doZLoadDeltaCompound :: (Typeable rep',Typeable irep,StableMD fs rep',ForestMD fs rep',Typeable container_rep',Eq container_rep',Eq rep',Eq irep,ForestRep rep' (ForestFSThunkI fs irep),Matching fs a,ICRep fs,MData NoCtx (ForestO fs) list_rep',list_rep' ~ [(FileName,rep')]) =>
		Lens dir_rep container_rep' -> Iso container_rep' list_rep'
		-> ForestI fs FilePath -> FilePath -> ForestI fs a -> FSTree fs -> FSTreeDeltaNodeMay -> FSTree fs -> ValueDelta fs dir_rep -> (dir_rep,GetForestMD fs)
		-> (FileName -> ForestFSThunkI fs FileInfo -> FilePath -> GetForestMD fs -> ForestI fs rep')
		-> (FileName -> FileName -> ForestFSThunkI fs FileInfo -> SValueDelta (ForestICThunkI fs FileInfo) -> (rep',GetForestMD fs) -> ForestI fs FilePath -> FilePath -> FSTreeDeltaNodeMay -> ValueDelta fs rep' -> ForestO fs (SValueDelta rep'))
		-> ForestO fs (NSValueDelta dir_rep)
doZLoadDeltaCompound lens isoRep mpath path' matchingM oldtree df tree' dv (dir_rep,getMD) load loadD = debug ("doLoadDeltaCompound: "++show (path')) $ do
	path <- inside mpath
	matching <- inside $ matchingM -- matching expressions are always recomputed; since they depend on the FS we cannot incrementally reuse them
	let crep = BX.get lens dir_rep
	let oldreplist = to isoRep crep
	let oldfiles = map fst oldreplist
	let oldreps = map snd oldreplist
	newfiles <- forestM $ getMatchingFilesInTree path matching tree'
	(newreps,noDeletions) <- forestM $ forestIO $ alignWithLookup newfiles oldfiles oldreps -- alignment by filename; newfiles first for the association list to be sorted by newfile positions
	
	-- load each file in the new tree (note that filtering is only done later)
	let loadEachFile (newfile,mbrep) = do
		idv <- maybe (return chgValueDelta) (inside . diffValue oldtree) mbrep
		doZLoadDeltaCompoundFile mpath path' newfile mbrep oldtree df tree' idv load loadD
	(mergeCompoundSValueDeltas -> (repchanges,repkind)) <- mapM loadEachFile newreps
	
	-- return the new container values and deltas on the directory
	drep <- case (noDeletions,isIdValueDelta dv,repkind) of
			(True,True,NoOp) -> return $ StableVD Id -- this is OK because the alignment algorithm is stable
			(True,True,Stable) -> return $ StableVD Delta
			otherwise -> return $ Modify $ \s -> put lens s (from isoRep repchanges)
	return $ debug ("doLoadDeltaCompoundReturn: "++show path') drep

-- returns the new values and a boolean indicating whether it has changed
doZLoadDeltaCompoundFile :: (Typeable rep,Typeable irep,StableMD fs rep,Eq rep,Eq irep,ForestRep rep (ForestFSThunkI fs irep),ForestMD fs rep) => 
	ForestI fs FilePath -> FilePath -> FileName -> Maybe rep -> FSTree fs -> FSTreeDeltaNodeMay -> FSTree fs -> ValueDelta fs rep
	-> (FileName -> ForestFSThunkI fs FileInfo -> FilePath -> GetForestMD fs -> ForestI fs rep)
	-> (FileName -> FileName -> ForestFSThunkI fs FileInfo -> SValueDelta (ForestICThunkI fs FileInfo) -> (rep,GetForestMD fs) -> ForestI fs FilePath -> FilePath -> FSTreeDeltaNodeMay -> ValueDelta fs rep -> ForestO fs (SValueDelta rep))
	-> ForestO fs ((Maybe (FileName,rep),SValueDeltaKind))
doZLoadDeltaCompoundFile mpath path' file' Nothing oldtree df tree' dv load loadD = debug ("doLoadDeltaCompoundFileNothing: "++show (path',file')) $ do
	-- try to reuse the original metadata (in this case it is not reused)
	(newGetMD,info) <- inside $ getRelForestMDInTree path' tree' file' >>= \fmd -> return (const2 $ return fmd,fileInfo fmd)
	-- filter new files according to the predicate
	fileInfo_thunk <- inside $ ref info
	rep' <- inside $ doZLoadFocus (fsTreeDeltaPathFilter df path') path' file' tree' newGetMD $ \newpath newGetMD -> do
		load file' fileInfo_thunk newpath newGetMD
	return ((Just (file',rep'),NonStable))
doZLoadDeltaCompoundFile mpath path' file' (Just rep) oldtree df tree' dv load loadD = debug ("doLoadDeltaCompoundFile: "++show (path',file')) $ do
	fileInfo_thunk <- getRelForestMDInTree path' tree' file' >>= \fmd -> inside $ ref $ fileInfo fmd
	path <- inside mpath
	-- try to reuse the original metadata
	(newGetMD,dFileInfo) <- do
		isUnevaluated <- isUnevaluatedMDThunk rep -- just an optimization to preserve the evaluation status of the thunk and avoid the unnecessary loading of old data
		case (isUnevaluated,path == path',isIdValueDelta dv,isEmptyTopFSTreeDeltaNodeMay $ focusFSTreeDeltaNodeMayByRelativePath df file') of
			(False,True,True,True) -> get_fmd_header rep >>= \fmd -> return (getForestMDInTree,Id) -- we keep the original data
			otherwise -> getRelForestMDInTree path' tree' file' >>= \fmd -> set fileInfo_thunk (fileInfo fmd) >> return (const2 $ return fmd,Delta)
	-- the filename has not changed as long as there was a match in the alignment operation, i.e., doLoadDeltaCompoundFile is called with original data
	drep <- doZLoadDeltaFocus mpath path' (rep,newGetMD) file' oldtree df tree' dv (load file' fileInfo_thunk) $ loadD file' file' fileInfo_thunk dFileInfo (rep,newGetMD) -- load recursively under a different focus
	-- apply the field deltas so that the post-values can be made available in the environment
	let (rep',repkind) = (applySValueDelta drep rep,valueDeltaKind drep `andSValueDeltaKinds` (valueDeltaKind dFileInfo))
	return (Just (file',rep'),repkind)

doZLoadDeltaCompoundWithConstraint :: (Typeable rep',Typeable irep,StableMD fs rep',ForestMD fs rep',Typeable container_rep',Eq container_rep',ForestOutput fs ICThunk Inside,Eq rep',Eq irep,ForestRep rep' (ForestFSThunkI fs irep),Matching fs a,ICRep fs,MData NoCtx (ForestO fs) list_rep',list_rep' ~ [(FileName,rep')]) =>
		Lens dir_rep container_rep' -> Iso container_rep' list_rep'
		-> ForestI fs FilePath -> FilePath -> ForestI fs a -> FSTree fs -> FSTreeDeltaNodeMay -> FSTree fs -> ValueDelta fs dir_rep -> (dir_rep,GetForestMD fs)
		-> (FileName -> ForestFSThunkI fs FileInfo -> ForestI fs Bool)
		-> (FileName -> ForestFSThunkI fs FileInfo -> FilePath -> GetForestMD fs -> ForestI fs rep')
		-> (FileName -> FileName -> ForestFSThunkI fs FileInfo -> SValueDelta (ForestICThunkI fs FileInfo) -> (rep',GetForestMD fs) -> ForestI fs FilePath -> FilePath -> FSTreeDeltaNodeMay -> ValueDelta fs rep' -> ForestO fs (SValueDelta rep'))
		-> ForestO fs (NSValueDelta dir_rep)
doZLoadDeltaCompoundWithConstraint lens isoRep mpath path' matchingM oldtree df tree' dv (dir_rep,getMD) pred load loadD = debug ("doLoadDeltaCompoundC: "++show (path')) $ do
	path <- inside mpath
	matching <- inside matchingM
	let crep = BX.get lens dir_rep
	let oldreplist = to isoRep crep
	let oldfiles = map fst oldreplist
	let oldreps = map snd oldreplist
	newfiles <- forestM $ getMatchingFilesInTree path matching tree'
	(newreps,noDeletions) <- forestM $ forestIO $ alignWithLookup newfiles oldfiles oldreps -- alignment by filename; newfiles first for the association list to be sorted by newfile positions
	
	-- load each file in the new tree (note that filtering is only done later)
	let loadEachFile (newfile,mbrep) = do
		idv <- maybe (return chgValueDelta) (inside . diffValue oldtree) mbrep
		doZLoadDeltaCompoundFileWithConstraint mpath path' newfile mbrep oldtree df tree' idv pred load loadD
	(mergeCompoundSValueDeltas -> (repchanges,repkind)) <- mapM loadEachFile newreps
	
	-- return the new container values and deltas on the directory
	drep <- case (noDeletions,isIdValueDelta dv,repkind) of
			(True,True,NoOp) -> return $ StableVD Id -- this is OK because the alignment algorithm is stable
			(True,True,Stable) -> return $ StableVD Delta
			otherwise -> return $ Modify $ \s -> put lens s (from isoRep repchanges)
	return $ debug ("doLoadDeltaCompoundCReturn: "++show (path')) drep

-- returns the new values and a boolean indicating whether it has changed
doZLoadDeltaCompoundFileWithConstraint :: (Typeable rep,Typeable irep,StableMD fs rep,ForestMD fs rep,ForestOutput fs ICThunk Inside,Eq rep,Eq irep,ForestRep rep (ForestFSThunkI fs irep)) => 
	ForestI fs FilePath -> FilePath -> FileName -> Maybe rep -> FSTree fs -> FSTreeDeltaNodeMay -> FSTree fs -> ValueDelta fs rep
	-> (FileName -> ForestFSThunkI fs FileInfo -> ForestI fs Bool)
	-> (FileName -> ForestFSThunkI fs FileInfo -> FilePath -> GetForestMD fs -> ForestI fs rep)
	-> (FileName -> FileName -> ForestFSThunkI fs FileInfo -> SValueDelta (ForestICThunkI fs FileInfo) -> (rep,GetForestMD fs) -> ForestI fs FilePath -> FilePath -> FSTreeDeltaNodeMay -> ValueDelta fs rep -> ForestO fs (SValueDelta rep))
	-> ForestO fs (Maybe (FileName,rep),SValueDeltaKind)
doZLoadDeltaCompoundFileWithConstraint mpath path' file' Nothing oldtree df tree' dv pred load loadD = debug ("doLoadDeltaCompoundFileNothing: "++show (path',file')) $ do
	path <- inside mpath
	-- try to reuse the original metadata (in this case it is not reused)
	(newGetMD,info) <- inside $ getRelForestMDInTree path' tree' file' >>= \fmd -> return (const2 $ return fmd,fileInfo fmd)
	fileInfo_thunk <- inside $ ref info
	-- filter new files according to the predicate
	cond <- inside $ pred file' fileInfo_thunk
	if cond
		then do
			rep' <- inside $ doZLoadFocus (fsTreeDeltaPathFilter df path') path' file' tree' newGetMD $ \newpath newGetMD -> do
				load file' fileInfo_thunk newpath newGetMD
			return (Just (file',rep'),NonStable)
		else return (Nothing,NonStable)
doZLoadDeltaCompoundFileWithConstraint mpath path' file' (Just rep) oldtree df tree' dv pred load loadD = debug ("doLoadDeltaCompoundFile: "++show (path',file')) $ do
	path <- inside mpath
	fmd <- getRelForestMDInTree path' tree' file'
	fileInfo_thunk <- inside $ ref $ fileInfo fmd
	-- try to reuse the original metadata
	(newGetMD,dFileInfo) <- do
		isUnevaluated <- isUnevaluatedMDThunk rep -- just an optimization to preserve the evaluation status of the thunk and avoid the unnecessary loading of old data
		case (isUnevaluated,path == path',isIdValueDelta dv,isEmptyTopFSTreeDeltaNodeMay $ focusFSTreeDeltaNodeMayByRelativePath df file') of
			(False,True,True,True) -> get_fmd_header rep >>= \fmd -> return (getForestMDInTree,Id) -- we keep the original data
			otherwise -> getRelForestMDInTree path' tree' file' >>= \fmd -> set fileInfo_thunk (fileInfo fmd) >> return (const2 $ return fmd,Delta)
	-- filter new files according to the predicate
	cond <- inside $ pred file' fileInfo_thunk
	if cond
		then do -- the filename has not changed as long as there was a match in the alignment operation, i.e., doLoadDeltaCompoundFile is called with original data
			drep <- doZLoadDeltaFocus mpath path' (rep,newGetMD) file' oldtree df tree' dv (load file' fileInfo_thunk) $ loadD file' file' fileInfo_thunk dFileInfo (rep,newGetMD) -- load recursively under a different focus
			-- apply the field deltas so that the post-values can be made available in the environment
			let (rep',repkind) = (applySValueDelta drep rep,valueDeltaKind drep `andSValueDeltaKinds` (valueDeltaKind dFileInfo)) 
			return (Just (file',rep'),repkind)
		else return (Nothing,NonStable)

-- we try @skipUnevaluated@ first to avoid forcing unecessary original data
zstopIf :: (Typeable irep,Typeable rep,Eq rep,Eq irep,ForestMD fs rep,ICRep fs,ForestRep rep (ForestFSThunkI fs irep),StableMD fs rep) =>
	String -> Bool -> ForestI fs FilePath -> FilePath -> FSTreeDeltaNodeMay -> FSTree fs -> ValueDelta fs rep -> (rep,GetForestMD fs)
	-> (GetForestMD fs -> ForestI fs rep) -- static loading function
	-> (ValueDelta fs rep -> (rep,GetForestMD fs) -> ForestO fs (SValueDelta rep)) -- delta loading function
	-> ForestO fs (SValueDelta rep)
zstopIf str isEmptyEnv mpath path' df tree' dv repmd load loadD = zskipUnevaluated str tree' repmd load $ \newrepmd -> do
	path <- inside mpath
	zskipUpdateIf3 str (isEmptyEnv && isIdValueDelta dv) (path == path') (isEmptyFSTreeDeltaNodeMay df) $ loadD dv newrepmd

zskipUpdateIf3 :: ICRep fs => String -> Bool -> Bool -> Bool -> ForestO fs (SValueDelta rep) -> ForestO fs (SValueDelta rep)
zskipUpdateIf3 str b1 b2 b3 io = if b1 && b2 && b3
	then debug ("skippedUpdate "++str) $ return Id
	else io
	
zskipUnevaluated :: (Typeable rep,Typeable irep,Eq rep,ICRep fs,Eq irep,ForestRep rep (ForestFSThunkI fs irep),ForestMD fs rep,StableMD fs rep) =>
	String -> FSTree fs -> (rep,GetForestMD fs)
	-> (GetForestMD fs -> ForestI fs rep) -- static loading function
	-> ((rep,GetForestMD fs) -> ForestO fs (SValueDelta rep)) -- delta loading function
	-> ForestO fs (SValueDelta rep)
zskipUnevaluated str tree' olddata@(rep,getMD) load loadD = do
	let isoRep = iso_rep_thunk
	let rep_thunk = to isoRep rep
	cond1 <- inside $ isUnforcedFSThunk rep_thunk
	if cond1
		then do -- if the thunks have not been forced, then we can return a NoOp update because we know that their values have not been necessary, so no other part of the specification needs to incrementally repaired
			overwrite rep_thunk $ Inc.get =<< liftM (to isoRep) (load getMD)
			debug ("skippedUnevaluated "++str++" ") $ return Delta
		else do
			{-debug ("NOT UNEVALUATED "++str++" "++show cond1++" "++show cond2) $-} loadD olddata
