{-# LANGUAGE TupleSections, FlexibleContexts, ScopedTypeVariables, GADTs, FlexibleInstances,MultiParamTypeClasses,UndecidableInstances, ViewPatterns #-}

module Language.Forest.IC.IO.ZDeltaStoring where

import Control.Monad.Trans
import Language.Forest.IC.IO.ZStoring
import Control.Monad.Writer (Writer(..),WriterT(..))
import qualified Control.Monad.Writer as Writer
import Language.Forest.Manifest
import Prelude hiding (const,read,mod)
import qualified Prelude
import Language.Forest.IO.Utils
import Language.Forest.Syntax
import Language.Forest.IC.BX as BX
import Language.Forest.IC.FS.Diff
import Language.Forest.IC.ValueDelta
import Language.Forest.IC.ICRep
import Language.Pads.Padsc hiding (lift,numErrors)
import qualified Language.Pads.Padsc as Pads
import Language.Forest.IC.MetaData
import Language.Forest.IC.Default
import Language.Forest.IC.Generic
import Language.Forest.IC.IO.Storing
import Language.Forest.Errors
import Language.Forest.IC.FS.FSDelta
import Language.Forest.Pure.MetaData (FileInfo(..),FileType(..),(:*:)(..))
import qualified Language.Forest.Pure.MetaData as Pure
import Control.Monad.Incremental as Inc hiding (memo)
import Data.IORef
import Data.List as List
import Language.Forest.IO.Shell

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
import Data.Monoid
import Data.List
import Language.Forest.FS.FSRep
--import Control.Monad.IO.Class
import Data.WithClass.MData

import Language.Forest.IC.IO.Memo

import qualified Control.Exception as CE

import Data.Data
import Data.Maybe
import System.Random
import Data.Proxy

-- adds consistency checks for top-level specification arguments
--doZManifestArgs :: (ICRep fs,ForestArgs fs args) =>
--	Proxy args -> ForestIs fs args
--	-> rep
--	-> (rep -> Manifest fs -> MManifestForestO fs)
--	-> Manifest fs -> MManifestForestO fs
--doZManifestArgs proxy margs rep manifestContent man = do
--	manifestContent rep man

doZDeltaManifestFile1 :: (ZippedICMemo fs,MData NoCtx (ForestI fs) arg,ForestInput fs FSThunk Inside,Eq arg,Typeable arg,Eq pads,Eq md,ICRep fs,Pads1 arg pads md) =>
	Bool -> Pure.Arg arg -> FilePath -> FilePath -> FSTree fs -> FSTreeDeltaNodeMay -> FSTree fs -> ForestFSThunkI fs ((Forest_md fs,md),pads) -> ValueDelta fs (ForestFSThunkI fs ((Forest_md fs,md),pads)) -> Manifest fs -> MManifestForestO fs
doZDeltaManifestFile1 isEmptyDArg (Pure.Arg arg) path path' tree df tree' rep_t dv man = do
	let argProxy = Proxy :: Proxy (Pure.Arg arg)
	case (isEmptyDArg,path == path',isIdValueDelta dv,df) of
		(True,True,True,(isEmptyFSTreeDeltaNodeMay -> True)) -> do -- no conflict tests are issued
			Writer.tell $ inside . addZippedMemo path' argProxy (return arg) rep_t . Just
			return man -- nothing changed
		otherwise -> doZManifestFile1 (Pure.Arg arg) path' tree' rep_t man
	

--doZManifestArchive :: (ZippedICMemo fs,Typeable rep,ForestMD fs rep,Eq rep,ForestInput fs FSThunk Inside,ICRep fs) =>
--	Bool -> [ArchiveType] -> FilePath -> FSTree fs 
--	-> ForestFSThunkI fs (Forest_md fs,rep)
--	-> (FilePath -> FSTree fs -> rep -> Manifest fs -> MManifestForestO fs)
--	-> Manifest fs -> MManifestForestO fs
--doZManifestArchive isClosed archTy path tree rep_t manifestContents man = do
--	let argsProxy = Proxy :: Proxy ()
--	newman <- doZManifestArchive' archTy path tree rep_t (\t -> get t >>= \(fmd,rep) -> return (Just fmd,rep)) manifestContents man
--	when isClosed $ Writer.tell $ inside . addZippedMemo path argsProxy () rep_t . Just
--	return newman
--
--doZManifestArchiveInner :: (Typeable rep,ForestMD fs rep,Eq rep,ForestInput fs FSThunk Inside,ICRep fs) =>
--	[ArchiveType] -> FilePath -> FSTree fs 
--	-> rep
--	-> (FilePath -> FSTree fs -> rep -> Manifest fs -> MManifestForestO fs)
--	-> Manifest fs -> MManifestForestO fs
--doZManifestArchiveInner archTy path tree rep_t manifestContents man = doZManifestArchive' archTy path tree rep_t (\rep -> return (Nothing,rep)) manifestContents man

--doZDeltaManifestArchive :: (ICRep fs,toprep ~ ForestFSThunkI fs (Forest_md fs,rep)) =>
--	Bool -> [ArchiveType] -> FilePath -> FilePath -> FSTree fs -> FSTreeDeltaNodeMay -> FSTree fs
--	-> toprep -> ValueDelta fs toprep
--	-> (FilePath -> FilePath -> FSTree fs -> FSTreeDeltaNodeMay -> FSTree fs -> rep -> ValueDelta fs rep -> Manifest fs -> MManifestForestO fs)
--	-> Manifest fs -> MManifestForestO fs
--doZDeltaManifestArchive isClosed archTy path path' tree df tree' arch_rep arch_dv manifestD man = do
--	
--	case (path == path',isIdValueDelta dv,df) of
--		(True,True,isEmptyFSTreeDeltaNodeMay -> True) -> do
--	
--doZManifestArchive' :: (Typeable rep,ForestMD fs rep,Eq rep,ForestInput fs FSThunk Inside,ICRep fs) =>
--	[ArchiveType] -> FilePath -> FSTree fs 
--	-> toprep -> (toprep -> ForestI fs (Maybe (Forest_md fs),rep))
--	-> (FilePath -> FSTree fs -> rep -> Manifest fs -> MManifestForestO fs)
--	-> Manifest fs -> MManifestForestO fs
--doZManifestArchive' archTy path tree toprep getRep manifestContents man = do
--	(mb_fmd,rep) <- lift $ inside $ getRep toprep
--	canpath <- lift $ forestM $ canonalizeDirectoryInTree path tree
--	dskpath <- lift $ forestM $ pathInTree canpath tree
--	let arch_canpath = cardinalPath canpath
--	avfsTree <- lift $ forestM $ virtualTree tree
--	
--	archiveDir <- lift $ forestM $ forestIO $ getTempPath -- unlogged temporary directory, since we remove it ourselves
--	archiveManifest <- lift $ forestM $ newManifestWith arch_canpath tree
--	lift $ forestM $ forestIO $ decompressArchive archTy dskpath archiveDir -- decompress the original content, since some of it may be preserved in the new archive
--	
--	archiveManifest' <- manifestContents arch_canpath avfsTree rep archiveManifest
--	
--	-- NOTE: we only need to commit the writes that contribute to the new archive, inside the forest temp dir; if we chose otherwise we could unsafely commit to the filesystem!
--	man1 <- lift $ forestM $ storeManifestAt archiveDir archiveManifest' -- store the manifest at the temp dir, and return all the modifications outside the archive
--	archiveFile <- lift $ forestM tempPath
--	lift $ forestM $ forestIO $ compressArchive archTy archiveDir archiveFile -- compresses the new data into a new temp file
--	lift $ forestM $ forestIO $ removePath archiveDir -- purges all temporary archive data 
--	
--	let testm = do
--		status1 <- liftM (boolStatus $ NonExistingPath path) $ latestTree >>= doesExistInTree path
--		status2 <- case mb_fmd of
--			Nothing -> return Valid
--			Just fmd -> do
--				isValid <- forestO $ isValidMD fmd
--				let path_fmd = fullpath $ fileInfo fmd 
--				status21 <- liftM (boolStatus ConflictingMdValidity) $ forestO $ sameValidity fmd rep
--				status22 <- liftM (boolStatus $ ConflictingPath path path_fmd) $ latestTree >>= sameCanonicalFullPathInTree path_fmd path
--				status23 <- liftM (boolStatus $ ConflictingFileExtensions path archTy) $ return $ isValid <= ('.' : archiveExtension archTy == takeExtensions path)
--				return $ status21 `mappend` status22 `mappend` status23
--		return $ status1 `mappend` status2
--	let man2 = addTestToManifest testm man -- errors in the metadata must be consistent
--	
--	abspath <- lift $ forestM $ forestIO $ absolutePath path
--	let man3 = addFileToManifest' canpath abspath archiveFile man2
--	return $ mergeManifests man1 man3

--doZDeltaManifestDirectory :: (Typeable rep,Eq rep,ICRep fs,dirrep ~ ForestFSThunkI fs (Forest_md fs,rep)) => 
--	FilePath -> FilePath -> FSTree fs -> FSTreeDeltaNodeMay -> FSTree fs
--	-> dirrep -> ValueDelta fs dirrep
--	-> (rep -> ForestI fs Forest_err)
--	-> (rep -> Manifest fs -> MManifestForestO fs)
--	-> (rep -> ValueDelta fs rep -> Manifest fs -> MManifestForestO fs)
--	-> (FSTree fs -> rep -> ForestI fs (ValueDelta fs rep))
--	-> Manifest fs -> MManifestForestO fs
--doZDeltaManifestDirectory path path' tree df tree' dirrep_t dv collectMDErrors manifest manifestD diffValue man = do
--	exists <- lift $ forestM $ doesDirectoryExistInTree path tree
--	exists' <- lift $ forestM $ doesDirectoryExistInTree path' tree'
--	(fmd,rep) <- lift $ Inc.getOutside dirrep_t
--	err_t <- lift $ get_errors_thunk fmd
--	let path_fmd = fullpath $ fileInfo fmd
--	let exists_dir = doesDirectoryExistInMD path fmd
--
--	case (exists,exists_dir,exists') of
--		(False,False,False) -> case (path == path',isIdValueDelta dv,isEmptyTopFSTreeDeltaNodeMay df) of
--			(True,True,True) -> return man
--			otherwise -> do
--				let testm = do
--					status1 <- liftM (boolStatus $ ConflictingMdValidity) $ forestO $ inside (collectMDErrors rep) >>= sameValidity' fmd
--					status2 <- liftM (boolStatus $ ConflictingPath path path_fmd) $ latestTree >>= sameCanonicalFullPathInTree path_fmd path
--					return $ status1 `mappend` status2
--				return $ addTestToManifest testm man -- errors in the metadata must be consistent
--				
--		(True,True,True) -> do
--			top <- lift $ inside $ diffTopValueThunk tree dirrep_t -- if the current thunk hasn't changed
--			toperror <- lift $ inside $ diffTopValueThunk tree err_t -- if the error thunk hasn't changed
--			let testm = case (path==path',top && toperror) of
--				(True,True) -> return Valid
--				otherwise -> do
--					status11 <- liftM (boolStatus ConflictingMdValidity) $ forestO $ inside (collectMDErrors rep) >>= sameValidity' fmd
--					status12 <- liftM (boolStatus $ ConflictingPath path path_fmd) $ latestTree >>= sameCanonicalFullPathInTree path_fmd path
--					return $ status11 `mappend` status12
--			idv <- lift $ inside $ diffValue tree rep
--			manifestD rep idv $ addTestToManifest testm man
--		otherwise -> doZManifestDirectory path' tree' collectMDErrors dirrep_t manifest man

doZDeltaManifestSymLink :: ICRep fs => FilePath -> FilePath -> FSTree fs -> FSTreeDeltaNodeMay -> FSTree fs -> SymLink fs -> ValueDelta fs (SymLink fs) -> Manifest fs -> MManifestForestO fs
doZDeltaManifestSymLink path path' tree df tree' (SymLink rep_t) dv man = do
	case (path == path',isIdValueDelta dv,df) of
		(True,True,isEmptyFSTreeDeltaNodeMay -> True) -> debug "symlink unchanged" $ return man
		otherwise -> doZManifestSymLink path' tree' (SymLink rep_t) man

doZDeltaManifestConstraint :: (ForestMD fs rep,ICRep fs) =>
	Bool -> (rep -> ForestI fs Bool) -> rep -> ValueDelta fs rep
	-> (rep -> ValueDelta fs rep -> Manifest fs -> MManifestForestO fs)
	-> Manifest fs -> MManifestForestO fs
doZDeltaManifestConstraint emptyDArgs pred rep dv manifestD man = do
	man1 <- manifestD rep dv man
	case (emptyDArgs,isIdValueDelta dv) of
		(True,True) -> return man1
		otherwise -> do
			let testm = do -- constraint errors need to be accounted for
				liftM (boolStatus $ ConflictingMdValidity) $ forestO $ inside $ do
					cond <- pred rep
					valid <- liftM ((==0) . numErrors) $ get_errors rep
					-- valid reps need to satisfy the predicate
					return $ valid <= cond
			return $ addTestToManifest testm man1

-- assumes that, before any changes occured, the original error thunk was computing the sum of the errors of the inner representation.
doZDeltaManifestDirectory :: (Typeable rep,Eq rep,ICRep fs,dirrep ~ ForestFSThunkI fs (Forest_md fs,rep)) => 
	FilePath -> FilePath -> FSTree fs -> FSTreeDeltaNodeMay -> FSTree fs
	-> dirrep -> ValueDelta fs dirrep
	-> (rep -> ForestI fs Forest_err)
	-> (rep -> Manifest fs -> MManifestForestO fs)
	-> (rep -> ValueDelta fs rep -> Manifest fs -> MManifestForestO fs)
	-> (FSTree fs -> rep -> ForestI fs (ValueDelta fs rep))
	-> Manifest fs -> MManifestForestO fs
doZDeltaManifestDirectory path path' tree df tree' dirrep_t dv collectMDErrors manifest manifestD diffValue man = do
	exists <- lift $ forestM $ doesDirectoryExistInTree path tree
	exists' <- lift $ forestM $ doesDirectoryExistInTree path' tree'
	(fmd,rep) <- lift $ Inc.getOutside dirrep_t
	err_t <- lift $ get_errors_thunk fmd
	let path_fmd = fullpath $ fileInfo fmd
	let exists_dir = doesDirectoryExistInMD path fmd

	case (exists,exists_dir,exists') of
		(False,False,False) -> case (path == path',isIdValueDelta dv,isEmptyTopFSTreeDeltaNodeMay df) of
			(True,True,True) -> return man
			otherwise -> do
				let testm = do
					status1 <- liftM (boolStatus $ ConflictingMdValidity) $ forestO $ inside (collectMDErrors rep) >>= sameValidity' fmd
					status2 <- liftM (boolStatus $ ConflictingPath path path_fmd) $ latestTree >>= sameCanonicalFullPathInTree path_fmd path
					return $ status1 `mappend` status2
				return $ addTestToManifest testm man -- errors in the metadata must be consistent
				
		(True,True,True) -> do
			top <- lift $ inside $ diffTopValueThunk tree dirrep_t -- if the current thunk hasn't changed
			toperror <- lift $ inside $ diffTopValueThunk tree err_t -- if the error thunk hasn't changed
			let testm = case (path==path',top && toperror) of
				(True,True) -> return Valid
				otherwise -> do
					status11 <- liftM (boolStatus ConflictingMdValidity) $ forestO $ inside (collectMDErrors rep) >>= sameValidity' fmd
					status12 <- liftM (boolStatus $ ConflictingPath path path_fmd) $ latestTree >>= sameCanonicalFullPathInTree path_fmd path
					return $ status11 `mappend` status12
			idv <- lift $ inside $ diffValue tree rep
			manifestD rep idv $ addTestToManifest testm man
		otherwise -> doZManifestDirectory path' tree' collectMDErrors dirrep_t manifest man

-- assumes that, before any changes occured, the original error thunk was computing the sum of the errors of the inner representation.
doZDeltaManifestMaybe :: (Typeable rep,ForestMD fs rep,Eq rep,ICRep fs,mbrep ~ ForestFSThunkI fs (Forest_md fs,Maybe rep)) =>
	FilePath -> FilePath -> FSTree fs -> FSTreeDeltaNodeMay -> FSTree fs -> mbrep -> ValueDelta fs mbrep
	-> (rep -> Manifest fs -> MManifestForestO fs) -- inner store function
	-> (rep -> ValueDelta fs rep -> Manifest fs -> MManifestForestO fs) -- inner incremental store function
	-> (FSTree fs -> rep -> ForestI fs (ValueDelta fs rep))
	-> Manifest fs -> MManifestForestO fs
doZDeltaManifestMaybe path path' tree df tree' mbrep_t dv manifest manifestD diffValue man = do
	exists <- lift $ forestM $ doesExistInTree path tree
	exists' <- lift $ forestM $ doesExistInTree path' tree'
	(fmd,mb_rep) <- lift $ Inc.getOutside mbrep_t
	err_t <- lift $ get_errors_thunk fmd
	let path_fmd = fullpath $ fileInfo fmd
	
	case (exists,mb_rep,exists') of
		(False,Nothing,False) -> do
			case (path == path',isIdValueDelta dv,isEmptyTopFSTreeDeltaNodeMay df) of
				(True,True,True) -> do
					let testm = liftM (boolStatus $ ExistingPath path) $ latestTree >>= liftM not . doesExistInTree path
					return $ addTestToManifest testm man
				otherwise -> do
					let testm = do
						status1 <- liftM (boolStatus $ ExistingPath path) $ latestTree >>= liftM not . doesExistInTree path
						status2 <- liftM (boolStatus ConflictingRepMd) $ forestO $ inside $ liftM (==fmd) $ cleanForestMDwithFile path'
						return $ status1 `mappend` status2
					return $ addTestToManifest testm man
		(True,Just irep,True) -> do
			top <- lift $ inside $ diffTopValueThunk tree mbrep_t -- if the current thunk hasn't changed
			toperror <- lift $ inside $ diffTopValueThunk tree err_t -- if the error thunk hasn't changed
			let testm = do
				status1 <- case (path==path',top && toperror) of
					(True,True) -> return Valid
					otherwise -> do
						status11 <- liftM (boolStatus ConflictingMdValidity) $ forestO $ sameValidity fmd irep
						status12 <- liftM (boolStatus $ ConflictingPath path path_fmd) $ latestTree >>= sameCanonicalFullPathInTree path_fmd path
						return $ status11 `mappend` status12
				status2 <- liftM (boolStatus $ NonExistingPath path) $ latestTree >>= doesExistInTree path
				return $ status1 `mappend` status2
			idv <- lift $ inside $ diffValue tree irep
			manifestD irep idv $ addTestToManifest testm man
		otherwise -> doZManifestMaybe path' tree' mbrep_t manifest man
			
-- assumes that, before any changes occured, the original error thunk was computing the sum of the errors of the inner representation.
doZDeltaManifestMaybeInner :: (Typeable rep,ForestMD fs rep,Eq rep,ICRep fs) =>
	FilePath -> FilePath -> FSTree fs -> FSTreeDeltaNodeMay -> FSTree fs -> Maybe rep -> ValueDelta fs (Maybe rep)
	-> (rep -> Manifest fs -> MManifestForestO fs) -- inner store function
	-> (rep -> ValueDelta fs rep -> Manifest fs -> MManifestForestO fs) -- inner incremental store function
	-> (FSTree fs -> rep -> ForestI fs (ValueDelta fs rep))
	-> Manifest fs -> MManifestForestO fs
doZDeltaManifestMaybeInner path path' tree df tree' mb_rep dv manifest manifestD diffValue man = do
	exists <- lift $ forestM $ doesExistInTree path tree
	exists' <- lift $ forestM $ doesExistInTree path' tree'
	
	case (exists,mb_rep,exists') of
		(False,Nothing,False) -> do
			let testm = liftM (boolStatus $ ExistingPath path) $ latestTree >>= liftM not . doesExistInTree path
			return $ addTestToManifest testm man
		(True,Just irep,True) -> do
			let testm = liftM (boolStatus $ NonExistingPath path) $ latestTree >>= doesExistInTree path
			idv <- lift $ inside $ diffValue tree irep
			manifestD irep idv $ addTestToManifest testm man
		otherwise -> doZManifestMaybeInner path' tree' mb_rep manifest man			

doZDeltaManifestFocus :: (Matching fs a,ICRep fs) => 
	a -> FilePath -> FilePath -> FSTree fs -> FSTreeDeltaNodeMay -> FSTree fs
	-> (FilePath -> FilePath -> FSTreeDeltaNodeMay -> Manifest fs -> MManifestForestO fs)
	-> Manifest fs -> MManifestForestO fs
doZDeltaManifestFocus matching path path' tree df tree' manifestD man = do
	
	files <- lift $ forestM $ getMatchingFilesInTree path matching tree
	let name = pickFile files
	child_path <- lift $ forestM $ stepPathInTree tree path name
	
	files' <- lift $ forestM $ getMatchingFilesInTree path' matching tree'
	let name' = pickFile files'
	let testm = testFocus path' name' (\file tree -> return True) [name']
	child_path' <- lift $ forestM $ stepPathInTree tree' path' name'
	
	let newdf = focusFSTreeDeltaNodeMayByRelativePath df name'
	
	manifestD child_path child_path' newdf $ addTestToManifest testm man

doZDeltaManifestSimple :: (ICRep fs,Matching fs a) => 
	Lens dir_rep rep
	-> FilePath -> FilePath -> ForestI fs a -> FSTree fs -> FSTreeDeltaNodeMay -> FSTree fs
	-> dir_rep -> ValueDelta fs dir_rep
	-> (rep -> ValueDelta fs rep -> FilePath -> FilePath -> FSTreeDeltaNodeMay -> Manifest fs -> MManifestForestO fs)
	-> Manifest fs -> MManifestForestO fs
doZDeltaManifestSimple lens path path' matchingM tree df tree' dir_rep dir_dv manifestD man = do
	matching <- lift $ inside matchingM
	let rep = BX.get lens dir_rep
	let dv = mapValueDelta Proxy dir_dv
	doZDeltaManifestFocus matching path path' tree df tree' (manifestD rep dv) man

doZDeltaManifestSimpleWithConstraint :: (ForestMD fs rep,ICRep fs,Matching fs a) => 
	Lens dir_rep rep -> Bool -> (rep -> ForestI fs Bool)
	-> FilePath -> FilePath -> ForestI fs a -> FSTree fs -> FSTreeDeltaNodeMay -> FSTree fs
	-> dir_rep -> ValueDelta fs dir_rep
	-> (rep -> ValueDelta fs rep -> FilePath -> FilePath -> FSTreeDeltaNodeMay -> Manifest fs -> MManifestForestO fs)
	-> Manifest fs -> MManifestForestO fs
doZDeltaManifestSimpleWithConstraint lens emptyDArgs pred path path' matchingM tree df tree' dir_rep dir_dv manifestD man = do
	matching <- lift $ inside matchingM
	let rep = BX.get lens dir_rep
	let dv = mapValueDelta Proxy dir_dv
	doZDeltaManifestConstraint emptyDArgs pred rep dv (\rep dv -> doZDeltaManifestFocus matching path path' tree df tree' $ manifestD rep dv) man


---- to enforce consistency while allowing the list to change, we delete all files in the directory that do not match the values
--doZManifestCompound :: (ForestMD fs rep',Typeable container_rep,Eq container_rep,Matching fs a) =>
--	FilePath -> ForestI fs a -> FSTree fs
--	-> (container_rep -> [(FilePath,rep')])
--	-> container_rep
--	-> (FileName -> ForestFSThunkI fs FileInfo -> FilePath -> rep' -> Manifest fs -> MManifestForestO fs)
--	-> Manifest fs -> MManifestForestO fs
--doZManifestCompound parentPath matchingM tree toListRep c_rep manifestUnder man = do
--	matching <- lift $ inside matchingM
--	old_files <- lift $ forestM $ getMatchingFilesInTree parentPath matching tree
--	let (new_files,reps') = unzip $ toListRep c_rep
--	repinfos' <- lift $ inside $ mapM (\rep -> mod (get_fileInfo rep) >>= \fileInfo_t -> return (rep,fileInfo_t)) reps'
--	
--	let rem_files = old_files \\ new_files -- files to be removed
--	man1 <- lift $ forestM $ foldr (\rem_path man0M -> man0M >>= removePathFromManifestInTree rem_path tree) (return man) $ map (parentPath </>) rem_files -- remove deprecated files
--	
--	let manifestEach (n,(rep',fileInfo_t)) man0M = do
--		man0M >>= doZManifestFocus parentPath n tree rep' (manifestUnder n fileInfo_t)
--	let testm = testFocus parentPath matching (\file tree -> return True) new_files
--	liftM (addTestToManifest testm) $ foldr manifestEach (return man1) (zip new_files repinfos')
--	
--
--doZManifestCompoundWithConstraint :: (ForestMD fs rep',Typeable container_rep,Eq container_rep,Matching fs a) =>
--	FilePath -> ForestI fs a -> FSTree fs
--	-> (container_rep -> [(FilePath,rep')])
--	-> (FileName -> ForestFSThunkI fs FileInfo -> ForestI fs Bool)
--	-> container_rep
--	-> (FileName -> ForestFSThunkI fs FileInfo -> FilePath -> rep' -> Manifest fs -> MManifestForestO fs)
--	-> Manifest fs -> MManifestForestO fs
--doZManifestCompoundWithConstraint parentPath matchingM tree toListRep pred c_rep manifestUnder man = do
--	matching <- lift $ inside matchingM
--	old_files <- lift $ forestM $ getMatchingFilesInTree parentPath matching tree
--	
--	let (new_files,reps') = unzip $ toListRep c_rep
--	repinfos' <- lift $ inside $ mapM (\rep -> mod (get_fileInfo rep) >>= \fileInfo_t -> return (rep,fileInfo_t)) reps'
--	
--	let old_files' = old_files \\ new_files -- old files that are not in the view
--	old_metadatas' <- lift $ mapM (getRelForestMDInTree parentPath tree) old_files'
--	-- we need to check which old files satisfy the predicate
--	old_values' <- lift $ inside $ filterM (\(n,fmd) -> ref (fileInfo fmd) >>= pred n) $ zip old_files' old_metadatas'
--	let rem_files = map fst old_values'
--	-- and delete them
--	man1 <- lift $ forestM $ foldr (\rem_path man0M -> man0M >>= removePathFromManifestInTree rem_path tree) (return man) $ map (parentPath </>) rem_files -- remove deprecated files
--	
--	let manifestEach (n,(rep',fileInfo_t)) man0M = man0M >>= doZManifestConstraint tree (Prelude.const $ pred n fileInfo_t) rep'
--		(\rep' man -> doZManifestFocus parentPath n tree rep' (manifestUnder n fileInfo_t) man)
--	let testm = testFocus parentPath matching (\file tree -> forestO $ inside $ getRelForestMDInTree parentPath tree file >>= ref . fileInfo >>= pred file) new_files
--	liftM (addTestToManifest testm) $ foldr manifestEach (return man1) (zip new_files repinfos')


