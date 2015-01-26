{-# LANGUAGE TupleSections, FlexibleContexts, ScopedTypeVariables, GADTs, FlexibleInstances,MultiParamTypeClasses,UndecidableInstances, ViewPatterns #-}

module Language.Forest.IC.IO.ZStoring where

import Control.Monad.Trans
import Control.Monad.Writer (Writer(..),WriterT(..))
import qualified Control.Monad.Writer as Writer
import Language.Forest.Manifest
import Prelude hiding (const,read,mod)
import qualified Prelude
import Language.Forest.IO.Utils
import Language.Forest.Syntax
import Language.Forest.IC.FS.Diff
import Language.Forest.IC.ValueDelta
import Language.Forest.IC.ICRep
import Language.Pads.Padsc as Pads hiding (lift)
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
doZManifestArgs :: (ICRep fs,ForestArgs fs args) =>
	Proxy args -> ForestIs fs args
	-> rep
	-> (rep -> Manifest fs -> WManifestForestO fs)
	-> Manifest fs -> WManifestForestO fs
doZManifestArgs proxy margs rep manifestContent man = do
	manifestContent rep man

doZManifestFile1 :: (Typeable arg,ZippedICMemo fs,Eq pads,Eq md,ICRep fs,Pads1 arg pads md) => Pure.Arg arg -> FilePath -> FSTree fs -> ForestFSThunkI fs (Forest_md fs,(pads,md)) -> Manifest fs -> WManifestForestO fs
doZManifestFile1 (Pure.Arg arg :: Pure.Arg arg) path tree rep_t man = do
	let argProxy = Proxy :: Proxy (Pure.Arg arg)
	(fmd,rep@(pads,bmd)) <- lift $ inside $ get rep_t
	let path_fmd = fullpath $ fileInfo fmd
	valid <- lift $ isValidMD fmd
	newman <- if valid
		then do
			let testm = do
				status1 <- liftM (boolStatus $ ConflictingMdValidity) $ return $ Pads.numErrors (get_md_header $ snd rep) == 0
				status2 <- liftM (boolStatus $ ConflictingPath path path_fmd) $ latestTree >>= sameCanonicalFullPathInTree path_fmd path
				return $ status1 `mappend` status2
			lift $ forestM $ addFileToManifestInTree (printFile1 arg) path tree rep $ addTestToManifest testm man
		else do
			-- inconsistent unless the non-stored representation has default data
			let testDefault = do
				status1 <- liftM (boolStatus ConflictingRepMd) $ forestO $ do
				 	let exists = doesDirectoryExistInMD path fmd
					return $ (not exists) || (Pads.numErrors (get_md_header bmd) > 0)
				status2 <- liftM (boolStatus $ ConflictingPath path path_fmd) $ latestTree >>= sameCanonicalFullPathInTree path_fmd path
				return $ status1 `mappend` status2
				
			let manDefault = addTestToManifest testDefault man
			lift $ forestM $ removePathFromManifestInTree path tree manDefault
	Writer.tell $ inside . addZippedMemo path argProxy (return arg) rep_t . Just
	return newman

doZManifestArchive :: (ZippedICMemo fs,Typeable rep,ForestMD fs rep,Eq rep,ForestInput fs FSThunk Inside,ICRep fs) =>
	Bool -> [ArchiveType] -> FilePath -> FSTree fs 
	-> ForestFSThunkI fs (Forest_md fs,rep)
	-> (FilePath -> FSTree fs -> rep -> Manifest fs -> WManifestForestO fs)
	-> Manifest fs -> WManifestForestO fs
doZManifestArchive isClosed archTy path tree rep_t manifestContents man = do
	let argsProxy = Proxy :: Proxy ()
	newman <- doZManifestArchive' archTy path tree rep_t (\t -> get t >>= \(fmd,rep) -> return (Just fmd,rep)) manifestContents man
	when isClosed $ Writer.tell $ inside . addZippedMemo path argsProxy () rep_t . Just
	return newman

doZManifestArchiveInner :: (Typeable rep,ForestMD fs rep,Eq rep,ForestInput fs FSThunk Inside,ICRep fs) =>
	[ArchiveType] -> FilePath -> FSTree fs 
	-> rep
	-> (FilePath -> FSTree fs -> rep -> Manifest fs -> WManifestForestO fs)
	-> Manifest fs -> WManifestForestO fs
doZManifestArchiveInner archTy path tree rep_t manifestContents man = doZManifestArchive' archTy path tree rep_t (\rep -> return (Nothing,rep)) manifestContents man

doZManifestArchive' :: (Typeable rep,ForestMD fs rep,Eq rep,ForestInput fs FSThunk Inside,ICRep fs) =>
	[ArchiveType] -> FilePath -> FSTree fs 
	-> toprep -> (toprep -> ForestI fs (Maybe (Forest_md fs),rep))
	-> (FilePath -> FSTree fs -> rep -> Manifest fs -> WManifestForestO fs)
	-> Manifest fs -> WManifestForestO fs
doZManifestArchive' archTy path tree toprep getRep manifestContents man = do
	(mb_fmd,rep) <- lift $ inside $ getRep toprep
	canpath <- lift $ forestM $ canonalizeDirectoryInTree path tree
	dskpath <- lift $ forestM $ pathInTree canpath tree
	let arch_canpath = cardinalPath canpath
	avfsTree <- lift $ forestM $ virtualTree tree
	
	archiveDir <- lift $ forestM $ forestIO $ getTempPath -- unlogged temporary directory, since we remove it ourselves
	archiveManifest <- lift $ forestM $ newManifestWith arch_canpath tree
	lift $ forestM $ forestIO $ decompressArchive archTy dskpath archiveDir -- decompress the original content, since some of it may be preserved in the new archive
	
	archiveManifest' <- manifestContents arch_canpath avfsTree rep archiveManifest
	
	-- NOTE: we only need to commit the writes that contribute to the new archive, inside the forest temp dir; if we chose otherwise we could unsafely commit to the filesystem!
	man1 <- lift $ forestM $ storeManifestAt archiveDir archiveManifest' -- store the manifest at the temp dir, and return all the modifications outside the archive
	archiveFile <- lift $ forestM tempPath
	lift $ forestM $ forestIO $ compressArchive archTy archiveDir archiveFile -- compresses the new data into a new temp file
	lift $ forestM $ forestIO $ removePath archiveDir -- purges all temporary archive data 
	
	-- guarantee that the error thunk is being updated on inner changes 
	case mb_fmd of
		Nothing -> return ()
		Just fmd -> Writer.tell $ Prelude.const $ get_errors_thunk fmd >>= flip overwrite (get_errors rep)
	
	let testm = do
		status1 <- liftM (boolStatus $ NonExistingPath path) $ latestTree >>= doesExistInTree path
		status2 <- case mb_fmd of
			Nothing -> return Valid
			Just fmd -> do
				isValid <- forestO $ isValidMD fmd
				let path_fmd = fullpath $ fileInfo fmd 
				status21 <- liftM (boolStatus ConflictingMdValidity) $ forestO $ sameValidity fmd rep
				status22 <- liftM (boolStatus $ ConflictingPath path path_fmd) $ latestTree >>= sameCanonicalFullPathInTree path_fmd path
				status23 <- liftM (boolStatus $ ConflictingFileExtensions path archTy) $ return $ isValid <= ('.' : archiveExtension archTy == takeExtensions path)
				return $ status21 `mappend` status22 `mappend` status23
		return $ status1 `mappend` status2
	let man2 = addTestToManifest testm man -- errors in the metadata must be consistent
	
	abspath <- lift $ forestM $ forestIO $ absolutePath path
	let man3 = addFileToManifest' canpath abspath archiveFile man2
	return $ mergeManifests man1 man3

doZManifestSymLink :: ICRep fs =>
	FilePath -> FSTree fs
	-> SymLink fs
	-> Manifest fs -> WManifestForestO fs
doZManifestSymLink path tree (SymLink rep_t) man = do
	(fmd, (tgt,base_md)) <- lift $ inside $ get rep_t
	let path_fmd = fullpath $ fileInfo fmd
	
	case symLink (fileInfo fmd) of
		Just sym -> do
			let testm = do
				status1 <- liftM (boolStatus $ ConflictingLink path tgt $ Just sym) $ return $ sym == tgt && base_md == cleanBasePD
				status2 <- liftM (boolStatus $ ConflictingPath path path_fmd) $ latestTree >>= sameCanonicalFullPathInTree path_fmd path
				return $ status1 `mappend` status2
			lift $ forestM $ addLinkToManifestInTree path tree tgt $ addTestToManifest testm man
		Nothing -> do
			let testm = do
				status1 <- liftM (boolStatus $ ConflictingLink path tgt Nothing) $ return $ tgt == "" && base_md == cleanBasePD
				status2 <- liftM (boolStatus $ ConflictingPath path path_fmd) $ latestTree >>= sameCanonicalFullPathInTree path_fmd path
				return $ status1 `mappend` status2
			lift $ forestM $ removePathFromManifestInTree path tree $ addTestToManifest testm man

-- constraints are a Forest validation feature and don't influence filesystem consistency
doZManifestConstraint :: ICRep fs =>
	FSTree fs -> (rep -> ForestI fs Bool) -> rep
	-> (rep -> Manifest fs -> WManifestForestO fs)
	-> Manifest fs -> WManifestForestO fs
doZManifestConstraint tree pred rep manifestContent man = do
	manifestContent rep man

-- on storing we make sure that the error thunk is a computation on the representation, instead of an arbitrary user-defined value.
-- we need to preserve this invariant to enable incremental storing
-- field manifest functions take the directory path
doZManifestDirectory :: (Typeable rep,Eq rep,ICRep fs) => 
	FilePath -> FSTree fs -> (rep -> ForestI fs Forest_err)
	-> ForestFSThunkI fs (Forest_md fs,rep)
	-> (rep -> Manifest fs -> WManifestForestO fs)
	-> Manifest fs -> WManifestForestO fs
doZManifestDirectory path tree collectMDErrors rep_t manifestContent man = do
	(fmd,rep) <- lift $ inside $ get rep_t
	let path_fmd = fullpath $ fileInfo fmd
	
	-- guarantee that the error thunk is being updated on inner changes 
	Writer.tell $ Prelude.const $ get_errors_thunk fmd >>= flip overwrite (collectMDErrors rep)
	
	let exists = doesDirectoryExistInMD path fmd
	if exists
		then do
			man1 <- lift $ forestM $ addDirToManifestInTree path tree man -- adds a new directory
			let testm = do
				status1 <- liftM (boolStatus $ ConflictingMdValidity) $ forestO $ inside (collectMDErrors rep) >>= sameValidity' fmd
				status2 <- liftM (boolStatus $ ConflictingPath path path_fmd) $ latestTree >>= sameCanonicalFullPathInTree path_fmd path
				return $ status1 `mappend` status2
			let man2 = addTestToManifest testm man1 -- errors in the metadata must be consistent
			manifestContent rep man2
		else do
			man1 <- lift $ forestM $ removePathFromManifestInTree path tree man -- remove the directory
			let testm = do
				status1 <- liftM (boolStatus $ ConflictingMdValidity) $ forestO $ inside (collectMDErrors rep) >>= sameValidity' fmd
				status2 <- liftM (boolStatus $ ConflictingPath path path_fmd) $ latestTree >>= sameCanonicalFullPathInTree path_fmd path
				return $ status1 `mappend` status2
			let man2 = addTestToManifest testm man1 -- errors in the metadata must be consistent
			manifestContent rep man2

doZManifestMaybe :: (Typeable rep,ForestMD fs rep,Eq rep,ICRep fs) =>
	FilePath -> FSTree fs
	-> ForestFSThunkI fs (Forest_md fs,Maybe rep)
	-> (rep -> Manifest fs -> WManifestForestO fs)
	-> Manifest fs -> WManifestForestO fs
doZManifestMaybe path tree rep manifestContent man = doZManifestMaybe' path tree rep (\t -> get t >>= \(fmd,rep) -> return (Just fmd,rep)) manifestContent man

doZManifestMaybeInner :: (Typeable rep,ForestMD fs rep,Eq rep,ICRep fs) =>
	FilePath -> FSTree fs
	-> Maybe rep
	-> (rep -> Manifest fs -> WManifestForestO fs)
	-> Manifest fs -> WManifestForestO fs
doZManifestMaybeInner path tree rep manifestContent man = doZManifestMaybe' path tree rep (\rep -> return (Nothing,rep)) manifestContent man

doZManifestMaybe' :: (Typeable rep,ForestMD fs rep,Eq rep,ICRep fs) =>
	FilePath -> FSTree fs
	-> toprep -> (toprep -> ForestI fs (Maybe (Forest_md fs),Maybe rep))
	-> (rep -> Manifest fs -> WManifestForestO fs)
	-> Manifest fs -> WManifestForestO fs
doZManifestMaybe' path tree toprep getRep manifestContent man = do
	(mb_fmd,rep_mb) <- lift $ inside $ getRep toprep
	
	-- guarantee that the error thunk is being updated on inner changes 
	case mb_fmd of
		Nothing -> return ()
		Just fmd -> Writer.tell $ Prelude.const $ get_errors_thunk fmd >>= flip overwrite (maybe (return cleanForestErr) get_errors rep_mb)
	
	case rep_mb of
		Just rep -> do
			let testm = do
				status1 <- case mb_fmd of
					Nothing -> return Valid
					Just fmd -> do
						let path_fmd = fullpath $ fileInfo fmd
						status11 <- liftM (boolStatus ConflictingMdValidity) $ forestO $ sameValidity fmd rep
						status12 <- liftM (boolStatus $ ConflictingPath path path_fmd) $ latestTree >>= sameCanonicalFullPathInTree path_fmd path
						return $ status11 `mappend` status12
				status2 <- liftM (boolStatus $ NonExistingPath path) $ latestTree >>= doesExistInTree path
				return $ status1 `mappend` status2
			let man1 = addTestToManifest testm man
			manifestContent rep man1 -- the path will be added recursively
		Nothing -> do
			let testm = do
				status1 <- liftM (boolStatus $ ExistingPath path) $ latestTree >>= liftM not . doesExistInTree path
				status2 <- case mb_fmd of
					Nothing -> return Valid
					Just fmd -> liftM (boolStatus ConflictingRepMd) $ forestO $ inside $ liftM (==fmd) $ cleanForestMDwithFile path
				return $ status1 `mappend` status2
			return $ addTestToManifest testm man

doZManifestFocus :: (ICRep fs,Matching fs a) =>
	FilePath -> a -> FSTree fs -> rep
	-> (FilePath -> rep -> Manifest fs -> WManifestForestO fs)
	-> Manifest fs -> WManifestForestO fs
doZManifestFocus parentPath matching tree rep manifestUnder man = do
	files <- lift $ forestM $ getMatchingFilesInTree parentPath matching tree
	let name = pickFile files
	let testm = testFocus parentPath name (\file tree -> return True) [name]
	child_path <- lift $ forestM $ stepPathInTree tree parentPath name
	manifestUnder child_path rep $ addTestToManifest testm man

doZManifestSimple :: (ICRep fs,Matching fs a) =>
	FilePath -> ForestI fs a -> FSTree fs -> rep
	-> (FilePath -> rep -> Manifest fs -> WManifestForestO fs)
	-> Manifest fs -> WManifestForestO fs
doZManifestSimple parentPath matching tree dta manifestUnder man = lift (inside matching) >>= \m -> doZManifestFocus parentPath m tree dta manifestUnder man

doZManifestSimpleWithConstraint :: (ICRep fs,Matching fs a) =>
	FilePath -> ForestI fs a -> FSTree fs
	-> (rep -> ForestI fs Bool)
	-> rep
	-> (FilePath -> rep -> Manifest fs -> WManifestForestO fs)
	-> Manifest fs -> WManifestForestO fs
doZManifestSimpleWithConstraint parentPath matching tree pred dta manifestUnder = doZManifestConstraint tree pred dta $ \dta' man1 ->
	lift (inside matching) >>= \m -> doZManifestFocus parentPath m tree dta' manifestUnder man1

-- to enforce consistency while allowing the list to change, we delete all files in the directory that do not match the values
doZManifestCompound :: (ForestMD fs rep',Typeable container_rep,Eq container_rep,Matching fs a) =>
	FilePath -> ForestI fs a -> FSTree fs
	-> (container_rep -> [(FilePath,rep')])
	-> container_rep
	-> (FileName -> ForestFSThunkI fs FileInfo -> FilePath -> rep' -> Manifest fs -> WManifestForestO fs)
	-> Manifest fs -> WManifestForestO fs
doZManifestCompound parentPath matchingM tree toListRep c_rep manifestUnder man = do
	matching <- lift $ inside matchingM
	old_files <- lift $ forestM $ getMatchingFilesInTree parentPath matching tree
	let (new_files,reps') = unzip $ toListRep c_rep
	repinfos' <- lift $ inside $ mapM (\rep -> mod (get_fileInfo rep) >>= \fileInfo_t -> return (rep,fileInfo_t)) reps'
	
	let rem_files = old_files \\ new_files -- files to be removed
	man1 <- lift $ forestM $ foldr (\rem_path man0M -> man0M >>= removePathFromManifestInTree rem_path tree) (return man) $ map (parentPath </>) rem_files -- remove deprecated files
	
	let manifestEach (n,(rep',fileInfo_t)) man0M = do
		man0M >>= doZManifestFocus parentPath n tree rep' (manifestUnder n fileInfo_t)
	let testm = testFocus parentPath matching (\file tree -> return True) new_files
	liftM (addTestToManifest testm) $ foldr manifestEach (return man1) (zip new_files repinfos')
	

doZManifestCompoundWithConstraint :: (ForestMD fs rep',Typeable container_rep,Eq container_rep,Matching fs a) =>
	FilePath -> ForestI fs a -> FSTree fs
	-> (container_rep -> [(FilePath,rep')])
	-> (FileName -> ForestFSThunkI fs FileInfo -> ForestI fs Bool)
	-> container_rep
	-> (FileName -> ForestFSThunkI fs FileInfo -> FilePath -> rep' -> Manifest fs -> WManifestForestO fs)
	-> Manifest fs -> WManifestForestO fs
doZManifestCompoundWithConstraint parentPath matchingM tree toListRep pred c_rep manifestUnder man = do
	matching <- lift $ inside matchingM
	old_files <- lift $ forestM $ getMatchingFilesInTree parentPath matching tree
	
	let (new_files,reps') = unzip $ toListRep c_rep
	repinfos' <- lift $ inside $ mapM (\rep -> mod (get_fileInfo rep) >>= \fileInfo_t -> return (rep,fileInfo_t)) reps'
	
	let old_files' = old_files \\ new_files -- old files that are not in the view
	old_metadatas' <- lift $ mapM (getRelForestMDInTree parentPath tree) old_files'
	-- we need to check which old files satisfy the predicate
	old_values' <- lift $ inside $ filterM (\(n,fmd) -> ref (fileInfo fmd) >>= pred n) $ zip old_files' old_metadatas'
	let rem_files = map fst old_values'
	-- and delete them
	man1 <- lift $ forestM $ foldr (\rem_path man0M -> man0M >>= removePathFromManifestInTree rem_path tree) (return man) $ map (parentPath </>) rem_files -- remove deprecated files
	
	let manifestEach (n,(rep',fileInfo_t)) man0M = man0M >>= doZManifestConstraint tree (Prelude.const $ pred n fileInfo_t) rep'
		(\rep' man -> doZManifestFocus parentPath n tree rep' (manifestUnder n fileInfo_t) man)
	let testm = testFocus parentPath matching (\file tree -> forestO $ inside $ getRelForestMDInTree parentPath tree file >>= ref . fileInfo >>= pred file) new_files
	liftM (addTestToManifest testm) $ foldr manifestEach (return man1) (zip new_files repinfos')


