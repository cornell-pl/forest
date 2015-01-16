{-# LANGUAGE TupleSections, FlexibleContexts, ScopedTypeVariables, GADTs, FlexibleInstances,MultiParamTypeClasses,UndecidableInstances, ViewPatterns #-}

module Language.Forest.IC.IO.ZStoring where

import Language.Forest.Manifest
import Prelude hiding (const,read,mod)
import qualified Prelude
import Language.Forest.IO.Utils
import Language.Forest.Syntax
import Language.Forest.IC.FS.Diff
import Language.Forest.IC.ValueDelta
import Language.Forest.IC.ICRep
import Language.Pads.Padsc as Pads
import Language.Forest.IC.MetaData
import Language.Forest.IC.Generic
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
	-> (rep -> Manifest fs -> ForestO fs (Manifest fs))
	-> Manifest fs -> ForestO fs (Manifest fs)
doZManifestArgs proxy margs rep manifestContent man = do
	manifestContent rep man

doZManifestFile :: (MData NoCtx (ForestI fs) pads,MData NoCtx (ForestI fs) md,DeepTypeable pads,DeepTypeable md,Eq pads,Eq md,ICRep fs,Pads pads md) => FSTree fs -> ForestFSThunkI fs (Forest_md fs,(pads,md)) -> Manifest fs -> ForestO fs (Manifest fs)
doZManifestFile tree rep_t man = do
	(fmd,rep) <- inside $ get rep_t
	let path = fullpath $ fileInfo fmd
	valid <- isValidMD fmd
	if valid
		then do -- for valid data we write it to disk
			-- the Pads metadata must be valid
			let testm = liftM (boolStatus "Pads metadata is invalid") $ return $ Pads.numErrors (get_md_header $ snd rep) == 0
			forestM $ addFileToManifestInTree printFile path tree rep $ addTestToManifest testm man
		else do
			-- inconsistent unless the non-stored representation has default data
			let testDefault = liftM (boolStatus "non-default data for invalid file") $ forestO $ inside $ liftM (==rep) forestdefault
			let manDefault = addTestToManifest testDefault man
			isFile <- forestM $ doesFileExistInTree path tree 
			if isFile -- if the data is invalid and a file exists on disk, we remove it, otherwise we don't change anything
				then forestM $ removePathFromManifestInTree path tree manDefault
				else return manDefault

doZManifestFile1 :: (MData NoCtx (ForestI fs) pads,MData NoCtx (ForestI fs) md,DeepTypeable pads,DeepTypeable md,Eq pads,Eq md,ICRep fs,Pads1 arg pads md) => arg -> FSTree fs -> ForestFSThunkI fs (Forest_md fs,(pads,md)) -> Manifest fs -> ForestO fs (Manifest fs)
doZManifestFile1 arg tree rep_t man = do
	(fmd,rep) <- inside $ get rep_t
	let path = fullpath $ fileInfo fmd
	valid <- isValidMD fmd
	if valid
		then do -- for valid data we write it to disk
			-- the Pads metadata must be valid
			let testm = liftM (boolStatus "Pads metadata is invalid") $ return $ Pads.numErrors (get_md_header $ snd rep) == 0
			forestM $ addFileToManifestInTree (printFile1 arg) path tree rep $ addTestToManifest testm man
		else do
			-- inconsistent unless the non-stored representation has default data
			let testDefault = liftM (boolStatus "non-default data for invalid file") $ forestO $ inside $ liftM (==rep) forestdefault
			let manDefault = addTestToManifest testDefault man
			isFile <- forestM $ doesFileExistInTree path tree 
			if isFile -- if the data is invalid and a file exists on disk, we remove it, otherwise we don't change anything
				then forestM $ removePathFromManifestInTree path tree manDefault
				else return manDefault

doZManifestArchive :: (Typeable rep,ForestMD fs rep,Eq rep,ForestInput fs FSThunk Inside,ICRep fs) =>
	[ArchiveType] -> FSTree fs 
	-> ForestFSThunkI fs (Forest_md fs,rep)
	-> (FSTree fs -> rep -> Manifest fs -> ForestO fs (Manifest fs))
	-> Manifest fs -> ForestO fs (Manifest fs)
doZManifestArchive archTy tree rep_t manifestContents man = do
	(fmd,rep) <- inside $ get rep_t
	let path = fullpath $ fileInfo fmd 
	canpath <- forestM $ canonalizeDirectoryInTree path tree
	dskpath <- forestM $ pathInTree canpath tree
	let arch_canpath = cardinalPath canpath
	avfsTree <- forestM $ virtualTree tree
	
	archiveDir <- forestM $ forestIO $ getTempPath -- unlogged temporary directory, since we remove it ourselves
	archiveManifest <- forestM $ newManifestWith arch_canpath tree
	forestM $ forestIO $ decompressArchive archTy dskpath archiveDir -- decompress the original content, since some of it may be preserved in the new archive
	
	archiveManifest' <- manifestContents avfsTree rep archiveManifest
	
	-- NOTE: we only need to commit the writes that contribute to the new archive, inside the forest temp dir; if we chose otherwise we could unsafely commit to the filesystem!
	man1 <- forestM $ storeManifestAt archiveDir archiveManifest' -- store the manifest at the temp dir, and return all the modifications outside the archive
	archiveFile <- forestM tempPath
	forestM $ forestIO $ compressArchive archTy archiveDir archiveFile -- compresses the new data into a new temp file
	forestM $ forestIO $ removePath archiveDir -- purges all temporary archive data 
	
	let testm = do
		isValid <- forestO $ isValidMD fmd
		status1 <- liftM (boolStatus "inconsistent Archive: file extension does not match archive type") $ return $ isValid <= ('.' : archiveExtension archTy == takeExtensions path)
		status2 <- forestO $ liftM (boolStatus "inconsistent Archive: top-level and inner metadatas have different validity") $ sameValidity fmd rep
		return $ status1 `mappend` status2
	let man2 = addTestToManifest testm man -- errors in the metadata must be consistent
	
	let man3 = addFileToManifest' canpath path archiveFile man2
	return $ mergeManifests man1 man3

doZManifestSymLink :: ICRep fs =>
	FSTree fs
	-> ForestFSThunkI fs (Forest_md fs,(FilePath,Base_md))
	-> Manifest fs -> ForestO fs (Manifest fs)
doZManifestSymLink tree rep_t man = do
	(fmd,(tgt,base_md)) <- inside $ get rep_t
	let path = fullpath $ fileInfo fmd
	
	case symLink (fileInfo fmd) of
		Just sym -> do
			let testm = liftM (boolStatus "inconsistent SymLink: targets of metadata and content don't match") $ return $ sym == tgt && base_md == cleanBasePD
			forestM $ addLinkToManifestInTree path tree tgt $ addTestToManifest testm man
		Nothing -> do
			let testm = liftM (boolStatus "inconsistent SymLink: targets of metadata and content don't match") $ return $ tgt == "" && base_md == cleanBasePD
			return $ addTestToManifest testm man

-- constraints are a Forest validation feature and don't influence filesystem consistency
doZManifestConstraint :: ICRep fs =>
	FSTree fs -> (rep -> ForestI fs Bool) -> rep
	-> (rep -> Manifest fs -> ForestO fs (Manifest fs))
	-> Manifest fs -> ForestO fs (Manifest fs)
doZManifestConstraint tree pred rep manifestContent man = do
	manifestContent rep man

-- field manifest functions take the directory path
doZManifestDirectory :: (Typeable rep,Eq rep,ICRep fs) => 
	FSTree fs -> (rep -> ForestI fs Forest_err)
	-> ForestFSThunkI fs (Forest_md fs,rep)
	-> (FilePath -> rep -> Manifest fs -> ForestO fs (Manifest fs))
	-> Manifest fs -> ForestO fs (Manifest fs)
doZManifestDirectory tree collectMDErrors rep_t manifestContent man = do
	(fmd,rep) <- inside $ get rep_t
	let path = fullpath $ fileInfo fmd
	man1 <- forestM $ addDirToManifestInTree path tree man -- adds a new directory
	let testm = liftM (boolStatus "inconsistent Directory: top-level and inner metadatas have different validity") $ forestO $ inside (collectMDErrors rep) >>= sameValidity' fmd
	let man2 = addTestToManifest testm man1 -- errors in the metadata must be consistent
	manifestContent path rep man2

doZManifestMaybe :: (Typeable rep,ForestMD fs rep,Eq rep,ICRep fs) =>
	FSTree fs
	-> ForestFSThunkI fs (Forest_md fs,Maybe rep)
	-> (rep -> Manifest fs -> ForestO fs (Manifest fs))
	-> Manifest fs -> ForestO fs (Manifest fs)
doZManifestMaybe tree rep_t manifestContent man = do
	(fmd,rep_mb) <- inside $ get rep_t
	case rep_mb of
		Just rep -> do
			let testm = do
				status1 <- liftM (boolStatus "inconsistent Maybe: top-level and inner metadatas have different validity") $ forestO $ sameValidity fmd rep
				-- the file will be stored recursively, so we just need to guarantee that filepaths match
				status2 <- liftM (boolStatus "inconsistentMaybe: top-level and inner medatadas have different paths") $ latestTree >>= forestO . sameCanonicalFullPathInTree fmd rep
				return $ status1 `mappend` status2
			let man1 = addTestToManifest testm man
			manifestContent rep man1 -- the path will be added recursively
		Nothing -> do
			let path = fullpath $ fileInfo fmd
			let testm = liftM (boolStatus "Nothing value contains non-default metadata") $ forestO $ inside $ liftM (==fmd) $ cleanForestMDwithFile path
			let man1 = addTestToManifest testm man
			forestM $ removePathFromManifestInTree path tree man1 -- removes the path

doZManifestFocus :: (ForestMD fs rep,Matching a) =>
	FilePath -> a -> FSTree fs -> rep
	-> (rep -> Manifest fs -> ForestO fs (Manifest fs))
	-> Manifest fs -> ForestO fs (Manifest fs)
doZManifestFocus parentPath matching tree rep manifestUnder man = do
	
	-- outside the test so that they are performed over the current tree
	files <- forestM $ Pure.getMatchingFilesInTree parentPath matching tree
	path <- liftM (fullpath . fileInfo) $ get_fmd_header rep
	fmd <- get_fmd_header rep
	canPath <- forestM $ canonalizePathWithTree path tree
	canParentPath <- forestM $ canonalizePathWithTree parentPath tree
	
	let name = makeRelative canParentPath canPath
	isValid <- isValidMD fmd
	
	let testm = do
		-- the metadata path must be consistent with the matching expression
		-- implication because the value may be invalid due to other errors
		return $ boolStatus "inconsistent matching expression and focus path" $ isValid <= (length files == 1) && (isParentPathOf canParentPath canPath)
	manifestUnder rep $ addTestToManifest testm man

doZManifestSimple :: (ForestMD fs rep,Matching a) =>
	FilePath -> ForestI fs a -> FSTree fs -> rep
	-> (rep -> Manifest fs -> ForestO fs (Manifest fs))
	-> Manifest fs -> ForestO fs (Manifest fs)
doZManifestSimple parentPath matching tree dta manifestUnder man = inside matching >>= \m -> doZManifestFocus parentPath m tree dta manifestUnder man

doZManifestSimpleWithConstraint :: (ForestMD fs rep,Matching a) =>
	FilePath -> ForestI fs a -> FSTree fs
	-> (rep -> ForestI fs Bool)
	-> rep
	-> (rep -> Manifest fs -> ForestO fs (Manifest fs))
	-> Manifest fs -> ForestO fs (Manifest fs)
doZManifestSimpleWithConstraint parentPath matching tree pred dta manifestUnder = doZManifestConstraint tree pred dta $ \dta' man1 ->
	inside matching >>= \m -> doZManifestFocus parentPath m tree dta' manifestUnder man1

-- to enforce consistency while allowing the list to change, we delete all files in the directory that do not match the values
doZManifestCompound :: (Typeable container_rep,Eq container_rep,ForestMD fs rep',Matching a) =>
	FilePath -> ForestI fs a -> FSTree fs
	-> (container_rep -> [(FilePath,rep')])
	-> container_rep
	-> (FileName -> ForestFSThunkI fs FileInfo -> rep' -> Manifest fs -> ForestO fs (Manifest fs))
	-> Manifest fs -> ForestO fs (Manifest fs)
doZManifestCompound parentPath matchingM tree toListRep c_rep manifestUnder man = do
	matching <- inside matchingM
	old_files <- forestM $ Pure.getMatchingFilesInTree parentPath matching tree
	let (new_files,reps') = unzip $ toListRep c_rep
	repinfos' <- inside $ mapM (\rep -> mod (get_fileInfo rep) >>= \fileInfo_t -> return (rep,fileInfo_t)) reps'
	
	let rem_files = old_files \\ new_files -- files to be removed
	man1 <- forestM $ foldr (\rem_path man0M -> man0M >>= removePathFromManifestInTree rem_path tree) (return man) $ map (parentPath </>) rem_files -- remove deprecated files
	
	let manifestEach (n,(rep',fileInfo_t)) man0M = do
		man0M >>= doZManifestFocus parentPath n tree rep' (manifestUnder n fileInfo_t)
	foldr manifestEach (return man1) (zip new_files repinfos')

doZManifestCompoundWithConstraint :: (Typeable container_rep,Eq container_rep,ForestMD fs rep',Matching a) =>
	FilePath -> ForestI fs a -> FSTree fs
	-> (container_rep -> [(FilePath,rep')])
	-> (FileName -> ForestFSThunkI fs FileInfo -> ForestI fs Bool)
	-> container_rep
	-> (FileName -> ForestFSThunkI fs FileInfo -> rep' -> Manifest fs -> ForestO fs (Manifest fs))
	-> Manifest fs -> ForestO fs (Manifest fs)
doZManifestCompoundWithConstraint parentPath matchingM tree toListRep pred c_rep manifestUnder man = do
	matching <- inside matchingM
	old_files <- forestM $ Pure.getMatchingFilesInTree parentPath matching tree
	
	let (new_files,reps') = unzip $ toListRep c_rep
	repinfos' <- inside $ mapM (\rep -> mod (get_fileInfo rep) >>= \fileInfo_t -> return (rep,fileInfo_t)) reps'
	
	let old_files' = old_files \\ new_files -- old files that are not in the view
	old_metadatas' <- mapM (getRelForestMDInTree parentPath tree) old_files'
	-- we need to check which old files satisfy the predicate
	old_values' <- inside $ filterM (\(n,fmd) -> ref (fileInfo fmd) >>= pred n) $ zip old_files' old_metadatas'
	let rem_files = map fst old_values'
	-- and delete them
	man1 <- forestM $ foldr (\rem_path man0M -> man0M >>= removePathFromManifestInTree rem_path tree) (return man) $ map (parentPath </>) rem_files -- remove deprecated files
	
	let manifestEach (n,(rep',fileInfo_t)) man0M = man0M >>= doZManifestConstraint tree (Prelude.const $ pred n fileInfo_t) rep'
		(\rep' man -> doZManifestFocus parentPath n tree rep' (manifestUnder n fileInfo_t) man)
	foldr manifestEach (return man1) (zip new_files repinfos')


