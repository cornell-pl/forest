{-# LANGUAGE Rank2Types, DataKinds, KindSignatures, TupleSections, FlexibleContexts, ScopedTypeVariables, GADTs, FlexibleInstances,MultiParamTypeClasses,UndecidableInstances, ViewPatterns #-}

module Language.Forest.IC.IO.ZDeltaLoading where
	
import Language.Forest.Syntax
import Language.Forest.IC.Default
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

doZLoadDeltaFile1 :: (rept ~ ForestFSThunkI fs ((Forest_md fs,md),pads),ZippedICMemo fs,MData NoCtx (ForestI fs) arg,ForestInput fs FSThunk Inside,Eq arg,Typeable arg,Eq pads,Eq md,ICRep fs,Pads1 arg pads md)
	=> Bool -> Pure.Arg arg -> ForestI fs FilePath -> FilePath -> FSTree fs -> FSTreeDeltaNodeMay -> FSTree fs -> ValueDelta fs rept -> (rept,GetForestMD fs)
	-> ForestO fs (SValueDelta rept)
doZLoadDeltaFile1 isEmptyDArg (Pure.Arg arg' :: Pure.Arg arg) mpath path' oldtree df tree' dv (rep_thunk,getMD) = do
	let argProxy = Proxy :: Proxy (Pure.Arg arg)
	path <- inside mpath
	case (isEmptyDArg,path == path',isIdValueDelta dv,df) of
		(True,True,True,(isEmptyFSTreeDeltaNodeMay -> True)) -> debug "constant1 unchanged" $ do
			inside $ addZippedMemo path' argProxy (return arg') rep_thunk (Just tree')
			return Id
		(True,True,True,Just (FSTreeChg _ _)) -> debug "constant1 attrs" $ do
			modify rep_thunk $ \((_,bmd),rep) -> getMD path' tree' >>= \fmd' -> return ((fmd',bmd),rep)
			inside $ addZippedMemo path' argProxy (return arg') rep_thunk (Just tree')
			return Delta
		otherwise -> debug "constant1 changed" $ do
			rep_thunk' <- inside $ doZLoadFile1 (Proxy::Proxy pads) (Arg arg') (fsTreeDeltaPathFilter df path') path' tree' getMD
			overwrite rep_thunk $ Inc.get rep_thunk'
			inside $ addZippedMemo path' argProxy (return arg') rep_thunk (Just tree')
			return Delta

doZLoadDeltaArchiveInner :: (DeltaClass d,Typeable rep,ForestRep rep (ForestFSThunkI fs content0),ZippedICMemo fs
		,ForestInput fs FSThunk Inside,ForestMD fs rep,Eq rep) =>
	Bool -> [ArchiveType] -> ForestI fs FilePath -> FilePath -> FSTree fs -> FSTreeDeltaNodeMay -> FSTree fs -> ValueDelta fs rep
	-> (rep,GetForestMD fs)
	-> (FilePath -> GetForestMD fs -> FSTree fs -> ForestI fs rep)
	-> (FilePath -> ForestI fs rep)
	-> (ForestI fs FilePath -> FilePath -> (rep,GetForestMD fs) -> FSTree fs -> FSTreeDeltaNodeMay -> FSTree fs -> ValueDelta fs rep -> ForestO fs (d rep))
	-> ForestO fs (NSValueDelta rep)
doZLoadDeltaArchiveInner isClosed exts mpath path' oldtree df tree' dv (rep,getMD) loadGood loadBad loadD = do
	let argsProxy = Proxy :: Proxy ()
	path <- inside mpath
	case (path == path',isIdValueDelta dv,df) of
		(True,True,isEmptyFSTreeDeltaNodeMay -> True) -> return $ fromSValueDelta Id
		(True,True,Just (FSTreeChg _ _)) -> return $ fromSValueDelta Id
		
		-- if the archive file has been moved, try to reuse originally loaded archive data
		(_,_,Just (FSTreeNew _ (Just from) _)) -> do
			rep' <- inside $ doZLoadArchiveInner exts (fsTreeDeltaPathFilter df path') path' tree' getMD loadGood loadBad
			return $ Modify $ Prelude.const rep'
		
		-- compute a diff for the archive's content, and continue
		otherwise -> do
				
			-- compute the difference for the archive's content
			avfsTree' <- forestM $ virtualTree tree'
			avfsOldTree <- forestM $ virtualTree oldtree
			let pathC = cardinalPath path
			let pathC' = cardinalPath path'
			archiveDf <- forestM $ focusDiffFSTree oldtree pathC tree' pathC'
			
			drep <- loadD (return pathC) pathC' (rep,getForestMDInTree) avfsOldTree archiveDf avfsTree' dv
			fmd' <- inside $ getMD path tree'
			let rep' = applyDelta drep rep
			
			return $ Modify $ Prelude.const rep'

doZLoadDeltaArchive :: (DeltaClass d,Typeable rep,ForestRep rep (ForestFSThunkI fs content0),ZippedICMemo fs
		,ForestInput fs FSThunk Inside,ForestMD fs rep,Eq rep) =>
	Bool -> [ArchiveType] -> ForestI fs FilePath -> FilePath -> FSTree fs -> FSTreeDeltaNodeMay -> FSTree fs -> ValueDelta fs (ForestFSThunkI fs (Forest_md fs,rep))
	-> (ForestFSThunkI fs (Forest_md fs,rep),GetForestMD fs)
	-> (FilePath -> GetForestMD fs -> FSTree fs -> ForestI fs rep)
	-> (FilePath -> ForestI fs rep)
	-> (ForestI fs FilePath -> FilePath -> (rep,GetForestMD fs) -> FSTree fs -> FSTreeDeltaNodeMay -> FSTree fs -> ValueDelta fs rep -> ForestO fs (d rep))
	-> (FSTree fs -> rep -> ForestI fs (ValueDelta fs rep))
	-> ForestO fs (SValueDelta (ForestFSThunkI fs (Forest_md fs,rep)))
doZLoadDeltaArchive isClosed exts mpath path' oldtree df tree' dv (rep_thunk,getMD) loadGood loadBad loadD diffValue = do
	let argsProxy = Proxy :: Proxy ()
	path <- inside mpath
	case (path == path',isIdValueDelta dv,df) of
		(True,True,isEmptyFSTreeDeltaNodeMay -> True) -> do
			when isClosed $ inside $ addZippedMemo path' argsProxy () rep_thunk (Just tree')
			return Id
		(True,True,Just (FSTreeChg _ _)) -> do
			modify rep_thunk $ \(_,irep') -> do
				fmd' <- getMD path' tree'
				fmd'' <- updateForestMDErrorsInsideWith fmd' $ liftM (:[]) $ get_errors irep'
				return (fmd'',irep')
			when isClosed $ inside $ addZippedMemo path' argsProxy () rep_thunk (Just tree')
			return Delta
		
		-- if the archive file has been moved, try to reuse originally loaded archive data
		(_,_,Just (FSTreeNew _ (Just from) _)) -> do
			rep' <- inside $ doZLoadArchive isClosed (Proxy::Proxy rep) exts (fsTreeDeltaPathFilter df path') path' tree' getMD loadGood loadBad loadD diffValue
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
			direp <- liftM toNSValueDelta $ loadD (return pathC) pathC' (irep,getForestMDInTree) avfsOldTree archiveDf avfsTree' archiveDv
			case (path==path',direp) of
				(True,StableVD d) -> do
					replaceForestMDErrorsWith fmd $ liftM (:[]) $ get_errors irep
				otherwise -> do
					fmd' <- inside $ getMD path tree'
					let irep' = applyNSValueDelta direp irep
					updateForestMDErrorsWith fmd' $ liftM (:[]) $ get_errors irep' -- like a directory
					set rep_thunk (fmd',irep')
			
			when isClosed $ inside $ addZippedMemo path' argsProxy () rep_thunk (Just tree')
			return Delta
				

doZLoadDeltaSymLink :: (ForestInput fs FSThunk Inside,ICRep fs)
	=> ForestI fs FilePath -> FilePath -> FSTree fs -> FSTreeDeltaNodeMay -> FSTree fs -> ValueDelta fs (SymLink fs)
	-> (SymLink fs,GetForestMD fs)
	-> ForestO fs (SValueDelta (SymLink fs))
doZLoadDeltaSymLink mpath path' oldtree df tree' dv (SymLink rep_thunk,getMD) = do
	path <- inside mpath
	case (path == path',isIdValueDelta dv,df) of
		(True,True,isEmptyFSTreeDeltaNodeMay -> True) -> debug "symlink unchanged" $ return Id
		(True,True,Just (FSTreeChg _ _)) -> do
			modify rep_thunk $ \((_,bmd),rep) -> getMD path' tree' >>= \fmd' -> return ((fmd',bmd),rep)
			return Delta
		otherwise -> debug "symlink changed" $ do
			rep' <- inside $ doZLoadSymLink' path' tree' getMD
			set rep_thunk rep'
			return Delta

doZLoadDeltaConstraint :: (DeltaClass d,ForestMD fs rep,ForestOutput fs ICThunk Inside,ForestInput fs FSThunk Outside) =>
	Bool -> ValueDelta fs rep -> (rep,GetForestMD fs) -> (rep -> ForestI fs Bool)
	-> (ValueDelta fs rep -> (rep,GetForestMD fs) -> ForestO fs (d rep))
	-> ForestO fs (d rep)
doZLoadDeltaConstraint emptyDArgs dv (rep,getMD) pred loadD = debug ("doLoadDeltaConstraint: ") $ do
	direp <- loadD dv (rep,getMD)
	case (emptyDArgs,isIdValueDelta dv,isEmptyDelta direp) of
		(True,True,True) -> return direp
		otherwise -> do
			cond <- inside $ pred rep
			if cond
				then debug ("doLoadDeltaConstraintReturn2: ") $ return direp
				else do
					modify_errors rep $ \err -> return $ Pure.updateForestErr err [Pure.constraintViolationForestErr]
					if isStableDelta direp
						then return $ fromSValueDelta Delta 
						else return direp

-- updates the thunks that keep track of the arguments of a top-level declaration
doZLoadDeltaArgs :: (ForestArgs fs args,Eq rep,ICRep fs) =>
	Proxy args -> LoadDeltaArgs ICData fs args -> (rep,GetForestMD fs) -> FSTree fs
	-> (ForestICThunksI fs args -> (rep,GetForestMD fs) -> ForestO fs (SValueDelta rep))
	-> ForestO fs (SValueDelta rep)
doZLoadDeltaArgs proxy (margs,_) ((rep,getMD)) (tree' :: FSTree fs) loadD = debug ("doLoadDeltaArgs") $ do
	arg_thunks <- inside $ newArgs (Proxy :: Proxy fs) proxy margs -- creates new thunks to hold the new expressions
	(drep) <- loadD arg_thunks ((rep),getMD)
	return (mapSValueDelta drep) 

doZLoadDeltaDirectory :: (DeltaClass d,Typeable rep,Eq rep,ICRep fs) =>
	ForestI fs FilePath -> (ForestFSThunkI fs (Forest_md fs,rep),GetForestMD fs) -> FilePath -> FSTree fs -> FSTreeDeltaNodeMay -> FSTree fs
	-> ValueDelta fs (ForestFSThunkI fs (Forest_md fs,rep))
	-> (rep -> ForestI fs Forest_err)
	-> (GetForestMD fs -> ForestI fs rep)
	-> ForestI fs rep
	-> (ValueDelta fs rep -> (rep,GetForestMD fs) -> ForestO fs (d rep))
	-> ForestO fs (SValueDelta (ForestFSThunkI fs (Forest_md fs,rep)))
doZLoadDeltaDirectory mpath (rep_thunk,getMD) path' oldtree df tree' dv collectMDErrors loadGood loadBad loadD = do
	path <- inside mpath
	exists <- forestM $ doesDirectoryExistInTree path oldtree
	exists' <- forestM $ doesDirectoryExistInTree path' tree'
	debug ("doLoadDeltaDirectory: " ++ show (path,exists,path',exists')) $ case (exists,exists') of
		(False,False) -> case (path == path',isIdValueDelta dv,isEmptyTopFSTreeDeltaNodeMay df) of
			(True,True,True) -> return Id
			otherwise -> do
				modify rep_thunk $ \(_,rep1) -> do
					fmd' <- missingDirForestMD path'
					return (fmd',rep1)
				return Delta
		(True,False) -> do
			modify rep_thunk $ \(_,rep1) -> do
				fmd' <- missingDirForestMD path'
				return (fmd',rep1)
			return Delta
		(True,True) -> do
			rep@(fmd,irep) <- Inc.getOutside rep_thunk
			let idv = mapValueDelta Proxy dv
			direp <- liftM toNSValueDelta $ loadD idv (irep,getMD) -- load recursively
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
							set rep_thunk (fmd',f irep')
							return Delta -- modify the directory thunk, since underlying non-stable changes did not occur inside a modifiable
				otherwise -> do -- otherwise we compute a new @Forest_md@ and add new errors; all the changes are stable
					case (path==path',direp) of
						(True,StableVD d) -> do
							replaceForestMDErrorsWith fmd $ liftM (:[]) $ collectMDErrors irep
							return Delta
						otherwise -> do
							fmd' <- inside $ getMD path' tree'
							let irep' = applyNSValueDelta direp irep
							updateForestMDErrorsWith fmd' $ liftM (:[]) $ collectMDErrors irep' -- this forces the value after the update
							set rep_thunk (fmd',irep')
							return Delta
			debug ("doLoadDeltaDirectoryReturn: "++show (path,exists,path',exists')++show direp) $ return drep
		(False,True) -> do -- load from scratch
			overwrite rep_thunk $ doZLoadDirectory' path' tree' collectMDErrors getMD (loadGood getMD) loadBad
			return Delta

doZLoadDeltaMaybeInner :: (DeltaClass d,Typeable rep,Eq rep,ForestMD fs rep) =>
	ForestI fs FilePath -> (Maybe rep,GetForestMD fs) -> FilePath -> FSTree fs -> FSTreeDeltaNodeMay -> FSTree fs
	-> ValueDelta fs (Maybe rep)
	-> (GetForestMD fs -> ForestI fs rep)
	-> (ValueDelta fs rep -> (rep,GetForestMD fs) -> ForestO fs (d rep))
	-> (FSTree fs -> rep -> ForestI fs (ValueDelta fs rep))
	-> ForestO fs (NSValueDelta (Maybe rep))
doZLoadDeltaMaybeInner mpath (mb_rep,getMD) path' oldtree df tree' dv load loadD diffValue = do
	path <- inside mpath
	exists <- forestM $ doesExistInTree path oldtree
	exists' <- forestM $ doesExistInTree path' tree'
	case (exists,mb_rep,exists') of
		(False,Nothing,False) -> do
			case (path == path',isIdValueDelta dv,isEmptyTopFSTreeDeltaNodeMay df) of
				(True,True,True) -> return $ StableVD Id
				otherwise -> do
					return $ Modify $ Prelude.const Nothing
		(_,_,False) -> do
			return $ Modify $ Prelude.const Nothing
		(True,Just irep,True) -> do
			idv <- inside $ diffValue oldtree irep
			direp <- liftM toNSValueDelta $ loadD idv (irep,getMD) -- load recursively
			return $ maybeNSValueDelta direp
		(_,_,True) -> do
			mb_rep' <- inside $ doZLoadMaybeInner (fsTreeDeltaPathFilter df path') path' tree' $ load getMD
			return $ Modify $ Prelude.const mb_rep'

doZLoadDeltaMaybe :: (DeltaClass d,Typeable rep,Eq rep,ForestMD fs rep) =>
	ForestI fs FilePath -> (ForestFSThunkI fs (Forest_md fs,Maybe rep),GetForestMD fs) -> FilePath -> FSTree fs -> FSTreeDeltaNodeMay -> FSTree fs
	-> ValueDelta fs (ForestFSThunkI fs (Forest_md fs,Maybe rep))
	-> (GetForestMD fs -> ForestI fs rep)
	-> (ValueDelta fs rep -> (rep,GetForestMD fs) -> ForestO fs (d rep))
	-> (FSTree fs -> rep -> ForestI fs (ValueDelta fs rep))
	-> ForestO fs (SValueDelta (ForestFSThunkI fs (Forest_md fs,Maybe rep)))
doZLoadDeltaMaybe mpath (rep_thunk,getMD) path' oldtree df tree' dv load loadD diffValue = do
	path <- inside mpath
	exists <- forestM $ doesExistInTree path oldtree
	exists' <- forestM $ doesExistInTree path' tree'
	rep@(fmd,mb_rep) <- Inc.getOutside rep_thunk
	case (exists,mb_rep,exists') of
		(False,Nothing,False) -> do
			case (path == path',isIdValueDelta dv,isEmptyTopFSTreeDeltaNodeMay df) of
				(True,True,True) -> return Id
				otherwise -> do
					modify rep_thunk $ \(_,rep1) -> cleanForestMDwithFile path' >>= \fmd' -> return (fmd',rep1)
					return Delta
		(_,_,False) -> do
			overwrite rep_thunk $ cleanForestMDwithFile path' >>= \fmd' -> return (fmd',Nothing)
			return Delta
		(True,Just irep,True) -> do
			idv <- inside $ diffValue oldtree irep
			direp <- liftM toNSValueDelta $ loadD idv (irep,getMD) -- load recursively
			case (path==path',direp) of
				(True,StableVD direp) -> do -- we don't need to update the maybe thunk
					replaceForestMDErrorsWith fmd $ liftM (:[]) $ get_errors rep
					return Delta
				otherwise -> do
					let irep' = applyNSValueDelta direp irep
					fmd' <- cleanForestMDwithFile path
					updateForestMDErrorsWith fmd' $ liftM (:[]) $ get_errors rep
					overwrite rep_thunk $ return (fmd',Just irep') -- always update the metadata with the inner @Forest_md@ header
					return Delta
		(_,_,True) -> do
			overwrite rep_thunk $ doZLoadMaybe' (fsTreeDeltaPathFilter df path') path' tree' $ load getMD
			return Delta

-- note that in the implementation we do not incrementalize the path-focusing expressions as they (typically) depend on the FS
-- changes the current path, the original data already corresponds to the focused path
doZLoadDeltaFocus :: (DeltaClass d,Eq rep,Matching fs a,ForestMD fs rep) =>
	ForestI fs FilePath -> FilePath -> (rep,GetForestMD fs) -> a -> FSTree fs -> FSTreeDeltaNodeMay -> FSTree fs
	-> ValueDelta fs rep
	-> (ForestI fs FilePath -> FilePath -> FSTreeDeltaNodeMay -> ValueDelta fs rep -> ForestO fs (d rep))
	-> ForestO fs (d rep)
doZLoadDeltaFocus mpath path' (rep,getMD) matching oldtree df tree' dv loadD = do
	
	let mfile = do
		path <- inside mpath
		files <- forestM $ getMatchingFilesInTree path matching oldtree
		return $ pickFile files
	
	files' <- forestM $ getMatchingFilesInTree path' matching tree'
	case files' of
		[file'] -> doZLoadDeltaNewPath mpath path' mfile file' oldtree df tree' $ \x y z -> loadD x y z dv
		files' -> do
			doZLoadDeltaNewPath mpath path' mfile (pickFile files') oldtree df tree' $ \mnewpath newdpath df -> do
				drep <- loadD mnewpath newdpath df dv -- changes the delta
				if length files' == 0
					then return drep
					else if isEmptyDelta drep
						then return drep
						else do
							newpath <- inside mnewpath
							addMultipleMatchesErrorMD newpath files' rep
							return drep

-- changes the delta
doZLoadDeltaNewPath :: (DeltaClass d,ICRep fs) => ForestI fs FilePath -> FilePath -> ForestI fs FileName -> FileName -> FSTree fs -> FSTreeDeltaNodeMay -> FSTree fs
	-> (ForestI fs FilePath -> FilePath -> FSTreeDeltaNodeMay -> ForestO fs (d rep))
	-> ForestO fs (d rep)
doZLoadDeltaNewPath mpath path' mfile file' oldtree df tree' loadD = debug ("doLoadDeltaNewPath: "++show (path',file',df)) $ do
	let mnewpath = do
		path <- mpath
		file <- mfile
		forestM $ stepPathInTree oldtree path file
	newpath' <- forestM $ stepPathInTree tree' path' file'
	let newdf = focusFSTreeDeltaNodeMayByRelativePath df file' -- focusing the tree deltas is important for the skipping conditions to fire for unchanged branches of the FS
	debug ("changed FSDelta: " ++ show newdf) $ loadD mnewpath newpath' newdf -- load recursively

doZLoadDeltaSimple :: (DeltaClass d,ForestMD fs rep',Eq rep',Matching fs a,MData NoCtx (ForestO fs) rep') =>
	Lens dir_rep rep'
	-> ForestI fs FilePath -> FilePath -> ForestI fs a -> FSTree fs -> FSTreeDeltaNodeMay -> FSTree fs -> ValueDelta fs dir_rep -> (dir_rep,GetForestMD fs)
	-> ((rep',GetForestMD fs) -> ForestI fs FilePath -> FilePath -> FSTreeDeltaNodeMay -> ValueDelta fs rep' -> ForestO fs (d rep'))
	-> ForestO fs (NSValueDelta dir_rep)
doZLoadDeltaSimple lens mpath path' matchingM oldtree df tree' dv (dir_rep,getMD) loadD = debug ("doLoadDeltaSimple: "++show path') $ do
	matching <- inside $ matchingM
	let irep = BX.get lens dir_rep
	let idata = (irep,getForestMDInTree) -- we need to discard any previously loaded forest metadata
	direp <- liftM toNSValueDelta $ doZLoadDeltaFocus mpath path' idata matching oldtree df tree' (mapValueDelta Proxy dv) (loadD idata)
	case direp of
		StableVD d -> return $ StableVD $ liftSValueDelta d
		otherwise -> return $ Modify $ \s -> BX.put lens s $ applyNSValueDelta direp irep

doZLoadDeltaSimpleWithConstraint :: (DeltaClass d,ForestMD fs rep',ForestOutput fs ICThunk Inside,Eq rep',Matching fs a,MData NoCtx (ForestO fs) rep') =>
	Lens dir_rep rep'
	-> Bool -> ForestI fs FilePath -> FilePath -> ForestI fs a -> FSTree fs -> FSTreeDeltaNodeMay -> FSTree fs -> ValueDelta fs dir_rep -> (dir_rep,GetForestMD fs)
	-> (rep' -> ForestI fs Bool)
	-> ((rep',GetForestMD fs) -> ForestI fs FilePath -> FilePath -> FSTreeDeltaNodeMay -> ValueDelta fs rep' -> ForestO fs (d rep'))
	-> ForestO fs (NSValueDelta dir_rep)
doZLoadDeltaSimpleWithConstraint lens emptyDArgs mpath path' matchingM oldtree df tree' dv (dir_rep,getMD) pred loadD = debug ("doLoadDeltaSimpleWithConstraint: "++show (path')) $ do
	matching <- inside $ matchingM
	let irep = BX.get lens dir_rep
	let idata = (irep,getForestMDInTree) -- we need to discard any previously loaded forest metadata
	direp <- liftM toNSValueDelta $ doZLoadDeltaConstraint emptyDArgs (mapValueDelta Proxy dv) idata pred $ \dv idata -> doZLoadDeltaFocus mpath path' idata matching oldtree df tree' dv (loadD idata)
	case direp of
		StableVD d -> do
			return $ StableVD $ liftSValueDelta d
		otherwise -> return $ Modify $ \s -> BX.put lens s $ applyNSValueDelta direp irep
	

doZLoadDeltaCompound :: (DeltaClass d,Typeable rep',ForestMD fs rep',Typeable container_rep',Eq container_rep',Eq rep',Matching fs a,ICRep fs,MData NoCtx (ForestO fs) list_rep',list_rep' ~ [(FileName,rep')]) =>
		Lens dir_rep container_rep' -> Iso container_rep' list_rep'
		-> ForestI fs FilePath -> FilePath -> ForestI fs a -> FSTree fs -> FSTreeDeltaNodeMay -> FSTree fs -> ValueDelta fs dir_rep -> (dir_rep,GetForestMD fs)
		-> (FileName -> ForestFSThunkI fs FileInfo -> FilePath -> GetForestMD fs -> ForestI fs rep')
		-> (FileName -> FileName -> ForestFSThunkI fs FileInfo -> SValueDelta (ForestICThunkI fs FileInfo) -> (rep',GetForestMD fs) -> ForestI fs FilePath -> FilePath -> FSTreeDeltaNodeMay -> ValueDelta fs rep' -> ForestO fs (d rep'))
		-> (FSTree fs -> rep' -> ForestI fs (ValueDelta fs rep'))
		-> ForestO fs (NSValueDelta dir_rep)
doZLoadDeltaCompound lens isoRep mpath path' matchingM oldtree df tree' dv (dir_rep,getMD) load loadD diffValue = debug ("doLoadDeltaCompound: "++show (path')) $ do
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
doZLoadDeltaCompoundFile :: (DeltaClass d,Typeable rep,Eq rep,ForestMD fs rep) => 
	ForestI fs FilePath -> FilePath -> FileName -> Maybe rep -> FSTree fs -> FSTreeDeltaNodeMay -> FSTree fs -> ValueDelta fs rep
	-> (FileName -> ForestFSThunkI fs FileInfo -> FilePath -> GetForestMD fs -> ForestI fs rep)
	-> (FileName -> FileName -> ForestFSThunkI fs FileInfo -> SValueDelta (ForestICThunkI fs FileInfo) -> (rep,GetForestMD fs) -> ForestI fs FilePath -> FilePath -> FSTreeDeltaNodeMay -> ValueDelta fs rep -> ForestO fs (d rep))
	-> ForestO fs ((Maybe (FileName,rep),ValueDeltaKind))
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
	drep <- liftM toNSValueDelta $ doZLoadDeltaFocus mpath path' (rep,newGetMD) file' oldtree df tree' dv $ loadD file' file' fileInfo_thunk dFileInfo (rep,newGetMD) -- load recursively under a different focus
	-- apply the field deltas so that the post-values can be made available in the environment
	let (rep',repkind) = (applyNSValueDelta drep rep,valueDeltaKind drep `andValueDeltaKinds` (valueDeltaKind dFileInfo))
	return (Just (file',rep'),repkind)

doZLoadDeltaCompoundWithConstraint :: (DeltaClass d,Typeable rep',ForestMD fs rep',Typeable container_rep',Eq container_rep',ForestOutput fs ICThunk Inside,Eq rep',Matching fs a,ICRep fs,MData NoCtx (ForestO fs) list_rep',list_rep' ~ [(FileName,rep')]) =>
		Lens dir_rep container_rep' -> Iso container_rep' list_rep'
		-> ForestI fs FilePath -> FilePath -> ForestI fs a -> FSTree fs -> FSTreeDeltaNodeMay -> FSTree fs -> ValueDelta fs dir_rep -> (dir_rep,GetForestMD fs)
		-> (FileName -> ForestFSThunkI fs FileInfo -> ForestI fs Bool)
		-> (FileName -> ForestFSThunkI fs FileInfo -> FilePath -> GetForestMD fs -> ForestI fs rep')
		-> (FileName -> FileName -> ForestFSThunkI fs FileInfo -> SValueDelta (ForestICThunkI fs FileInfo) -> (rep',GetForestMD fs) -> ForestI fs FilePath -> FilePath -> FSTreeDeltaNodeMay -> ValueDelta fs rep' -> ForestO fs (d rep'))
		-> (FSTree fs -> rep' -> ForestI fs (ValueDelta fs rep'))
		-> ForestO fs (NSValueDelta dir_rep)
doZLoadDeltaCompoundWithConstraint lens isoRep mpath path' matchingM oldtree df tree' dv (dir_rep,getMD) pred load loadD diffValue = debug ("doLoadDeltaCompoundC: "++show (path')) $ do
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
doZLoadDeltaCompoundFileWithConstraint :: (DeltaClass d,Typeable rep,ForestMD fs rep,ForestOutput fs ICThunk Inside,Eq rep) => 
	ForestI fs FilePath -> FilePath -> FileName -> Maybe rep -> FSTree fs -> FSTreeDeltaNodeMay -> FSTree fs -> ValueDelta fs rep
	-> (FileName -> ForestFSThunkI fs FileInfo -> ForestI fs Bool)
	-> (FileName -> ForestFSThunkI fs FileInfo -> FilePath -> GetForestMD fs -> ForestI fs rep)
	-> (FileName -> FileName -> ForestFSThunkI fs FileInfo -> SValueDelta (ForestICThunkI fs FileInfo) -> (rep,GetForestMD fs) -> ForestI fs FilePath -> FilePath -> FSTreeDeltaNodeMay -> ValueDelta fs rep -> ForestO fs (d rep))
	-> ForestO fs (Maybe (FileName,rep),ValueDeltaKind)
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
			drep <- liftM toNSValueDelta $ doZLoadDeltaFocus mpath path' (rep,newGetMD) file' oldtree df tree' dv $ loadD file' file' fileInfo_thunk dFileInfo (rep,newGetMD) -- load recursively under a different focus
			-- apply the field deltas so that the post-values can be made available in the environment
			let (rep',repkind) = (applyNSValueDelta drep rep,valueDeltaKind drep `andValueDeltaKinds` (valueDeltaKind dFileInfo)) 
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
