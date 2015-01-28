{-# LANGUAGE TupleSections, FlexibleContexts, ScopedTypeVariables, GADTs, FlexibleInstances,MultiParamTypeClasses,UndecidableInstances, ViewPatterns #-}

module Language.Forest.Pure.IO.Storing where

import Language.Forest.Manifest
import Language.Forest.Pure.Generic as Forest
import Prelude hiding (const,read,mod)
import qualified Prelude
import Language.Forest.IO.Utils
import Language.Forest.Syntax
import Language.Pads.Padsc
import Language.Forest.Pure.MetaData
import Language.Forest.Errors
import Language.Forest.Pure.MetaData
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

import qualified Control.Exception as CE

import Data.Data
import Data.Maybe
import System.Random
import Data.Proxy

doManifestFile :: (Eq pads,Eq md,FSRep fs,Pads pads md) => FSTree fs -> (pads,(Forest_md,md)) -> Manifest fs -> ForestM fs (Manifest fs)
doManifestFile tree (rep,(fmd,md)) man = do
	let path = fullpath $ fileInfo fmd
	canpath <- canonalizePathWithTree path tree
	dskpath <- pathInTree canpath tree
	let valid = isValidMD fmd
	if valid
		then do -- for valid data we write it to disk
			addFileToManifest (\p -> printFile p (rep,md)) canpath path man
		else do
			isFile <- doesFileExistInTree path tree 
			if isFile -- if the data is invalid and a file exists on disk, we remove it, otherwise we don't change anything
				then return $ removePathFromManifest canpath path man
				else return man

doManifestFile1 :: (Eq pads,Eq md,FSRep fs,Pads1 arg pads md) => arg -> FSTree fs -> (pads,(Forest_md,md)) -> Manifest fs -> ForestM fs (Manifest fs)
doManifestFile1 arg tree (rep,(fmd,md)) man = do
	let path = fullpath $ fileInfo fmd
	canpath <- canonalizePathWithTree path tree
	dskpath <- pathInTree canpath tree
	let valid = isValidMD fmd
	if valid
		then do -- for valid data we write it to disk
			addFileToManifest (\p -> printFile1 arg p (rep,md)) canpath path man
		else do
			isFile <- doesFileExistInTree path tree 
			if isFile -- if the data is invalid and a file exists on disk, we remove it, otherwise we don't change anything
				then return $ removePathFromManifest canpath path man
				else return man

doManifestArchive :: (ForestMD md,Eq rep,Eq md,FSRep fs) =>
	[ArchiveType] -> FSTree fs 
	-> (rep,(Forest_md,md))
	-> (FSTree fs -> (rep,md) -> Manifest fs -> ForestM fs (Manifest fs))
	-> Manifest fs -> ForestM fs (Manifest fs)
doManifestArchive archTy tree (rep,(fmd,md)) manifestContents man = do
	let path = fullpath $ fileInfo fmd
	canpath <- canonalizePathWithTree path tree
	dskpath <- pathInTree canpath tree
	let arch_canpath = cardinalPath canpath -- the virtual path that points to the content of the archive
	
	avfsTree <- virtualTree tree 
	
	archiveDir <- forestIO $ getTempPath -- unlogged temporary directory, since we remove it ourselves
	archiveManifest <- newManifestWith arch_canpath tree -- the paths in the in-memory metadata should be consistent with the virtual path
	ori_file <- pathInTree canpath tree
	forestIO $ decompressArchive archTy ori_file archiveDir -- decompress the original content, since some may be preserved in the new archive
	
	archiveManifest' <- manifestContents avfsTree (rep,md) archiveManifest
	
	-- NOTE: we only need to commit the writes that contribute to the new archive, inside the forest temp dir; if we chose otherwise we could unsafely commit to the filesystem!
	man1 <- storeManifestAt archiveDir archiveManifest' -- store the manifest at the temp dir, and return all the modifications outside the archive
	archiveFile <- tempPath
	forestIO $ compressArchive archTy archiveDir archiveFile -- compresses the new data into a new temp file
	forestIO $ removePath archiveDir -- purges all temporary archive data 
	
	let testm = do
		let isValid = isValidMD fmd
		status1 <- return $ boolStatus (ConflictingFileExtensions path archTy) $ isValid <= ('.' : archiveExtension archTy == takeExtensions path)
		status2 <- return $ boolStatus ConflictingMdValidity $ sameValidity fmd md
		return $ status1 `mappend` status2
	let man2 = addTestToManifest testm man -- errors in the metadata must be consistent
	
	let man3 = addFileToManifest' canpath path archiveFile man2
	return $ mergeManifests man1 man3

doManifestSymLink :: FSRep fs =>
	FSTree fs
	-> (FilePath,(Forest_md, Base_md))
	-> Manifest fs -> ForestM fs (Manifest fs)
doManifestSymLink tree (tgt,(fmd,base_md)) man = do
	let path = fullpath $ fileInfo fmd
	
	canpath <- canonalizePathWithTree path tree
	dskpath <- pathInTree canpath tree
	
	let testm = do
		let sym = symLink $ fileInfo fmd
		liftM (boolStatus $ ConflictingLink path tgt sym) $ return $ sym == Just tgt
	let man1 = addTestToManifest testm man
	
	return $ addLinkToManifest canpath path tgt man1

-- users may have arbitrarily changed the data, so we can't trust that the thunk still computes the correct predicate
doManifestConstraint :: (ForestMD md,FSRep fs) => FSTree fs -> ((rep,md) -> Bool) -> (rep,md)
	-> ((rep,md) -> Manifest fs -> ForestM fs (Manifest fs))
	-> Manifest fs -> ForestM fs (Manifest fs)
doManifestConstraint tree pred (rep,md) manifestContent man = do
	let testm = do
		if pred (rep,md)
			then return Valid
			else return $ boolStatus (ConflictingMdValidity) $ not $ isValidMD md
	let man1 = addTestToManifest testm man
	manifestContent (rep,md) man

doManifestDirectory :: (Eq rep,Eq md,FSRep fs) => 
	FSTree fs -> (md -> Forest_err)
	-> (rep,(Forest_md,md))
	-> (FilePath -> (rep,md) -> Manifest fs -> ForestM fs (Manifest fs))
	-> Manifest fs -> ForestM fs (Manifest fs)
doManifestDirectory tree collectMDErrors (rep,(fmd,md)) manifestContent man = do
	let path = fullpath $ fileInfo fmd
	canpath <- canonalizePathWithTree path tree
	dskpath <- pathInTree canpath tree
	let man1 = addDirToManifest canpath path man -- adds a new directory
	let testm = return $ boolStatus ConflictingMdValidity $ sameValidity' fmd $ collectMDErrors md
	let man2 = addTestToManifest testm man1 -- errors in the metadata must be consistent
	manifestContent path (rep,md) man2

doManifestMaybe :: (Eq md,Eq rep,FSRep fs,ForestMD md) =>
	FSTree fs
	-> (Maybe rep,(Forest_md,Maybe md))
	-> ((rep,md) -> Manifest fs -> ForestM fs (Manifest fs))
	-> (rep -> FilePath -> ForestM fs md)
	-> Manifest fs -> ForestM fs (Manifest fs)
doManifestMaybe tree (rep_mb,(fmd,md_mb)) manifestContent defaultContent man = do
	case (rep_mb,md_mb) of
		(Just rep,Just md) -> do
			let testm = do
				status1 <- return $ boolStatus ConflictingMdValidity $ sameValidity fmd md
				-- the file will be stored recursively, so we just need to guarantee that filepaths match
				status2 <- liftM (boolStatus ConflictingMdValidity) $ sameCanonicalFullPathInTree fmd md tree
				return $ status1 `mappend` status2
			let man1 = addTestToManifest testm man
			manifestContent (rep,md) man1 -- the path will be added recursively
		(Nothing,Nothing) -> do
			let path = fullpath $ fileInfo fmd
			canpath <- canonalizePathWithTree path tree
			dskpath <- pathInTree canpath tree
			let testm = return $ boolStatus (ConflictingMdValidity) $ isValidMD fmd
			let man1 = addTestToManifest testm man
			return $ removePathFromManifest canpath path man1 -- removes the path
		(Just rep,Nothing) -> do 
			md <- defaultContent rep (fullpath $ fileInfo fmd)
			let testm = return (Invalid [ConflictingRepMd]) -- always invalid
			let man1 = addTestToManifest testm man
			manifestContent (rep,md) man1 -- the path will be added recursively
		(Nothing,Just md) -> do 
			let path = fullpath $ fileInfo fmd
			canpath <- canonalizePathWithTree path tree
			dskpath <- pathInTree canpath tree
			let testm = return (Invalid [ConflictingRepMd]) -- always invalid
			let man1 = addTestToManifest testm man
			return $ removePathFromManifest canpath path man1 -- removes the path

doManifestFocus :: (FSRep fs,ForestMD md,Matching fs a) =>
	FilePath -> a -> FSTree fs -> (rep,md)
	-> ((rep,md) -> Manifest fs -> ForestM fs (Manifest fs))
	-> Manifest fs -> ForestM fs (Manifest fs)
doManifestFocus parentPath matching tree dta@(rep,md) manifestUnder man = do
	let testm = do
		files <- getMatchingFilesInTree parentPath matching tree
		let path = fullpath $ fileInfo $ get_fmd_header md
		let fmd = get_fmd_header md
		let name = makeRelative parentPath path
		let isValid = isValidMD fmd
		-- implication because the value may be invalid due to other errors
		return $ boolStatus (ConflictingMatching parentPath (show matching) [name] files) ((not $ null (List.delete name files)) <= isValid)
	let man1 = addTestToManifest testm man
	manifestUnder dta man1

doManifestSimple :: (FSRep fs,ForestMD md',Matching fs a) =>
	FilePath -> a -> FSTree fs -> (rep',md')
	-> ((rep',md') -> Manifest fs -> ForestM fs (Manifest fs))
	-> Manifest fs -> ForestM fs (Manifest fs)
doManifestSimple parentPath matching tree dta manifestUnder man = doManifestFocus parentPath matching tree dta manifestUnder man

doManifestSimpleWithConstraint :: (ForestMD md',FSRep fs,Matching fs a) =>
	FilePath -> a -> FSTree fs
	-> ((rep',md') -> Bool)
	-> (rep',md')
	-> ((rep',md') -> Manifest fs -> ForestM fs (Manifest fs))
	-> Manifest fs -> ForestM fs (Manifest fs)
doManifestSimpleWithConstraint parentPath matching tree pred dta manifestUnder = doManifestConstraint tree pred dta $ \dta' man1 ->
	doManifestFocus parentPath matching tree dta' manifestUnder man1

-- to enforce consistency while allowing the list to change, we delete all files in the directory that do not match the values
doManifestCompound :: (FSRep fs,ForestMD md',Matching fs a) =>
	FilePath -> a -> FSTree fs
	-> (container_rep -> [(FilePath,rep')]) -> (container_md -> [(FilePath,md')])
	-> (container_rep,container_md)
	-> (FileName -> FileInfo -> (rep',md') -> Manifest fs -> ForestM fs (Manifest fs))
	-> Manifest fs -> ForestM fs (Manifest fs)
doManifestCompound parentPath matching tree toListRep toListMd (c_rep,c_md) manifestUnder man = do
	old_files <- getMatchingFilesInTree parentPath matching tree
	let reps' = map snd $ toListRep c_rep
	let (new_files,mds') = unzip $ toListMd c_md
	let dtas' = zip reps' mds'
	
	let rem_files = old_files \\ new_files -- files to be removed
	man1 <- foldr (\rem_path man0M -> canonalizePathWithTree rem_path tree >>= \canpath -> liftM (removePathFromManifest canpath rem_path) man0M) (return man) $ map (parentPath </>) rem_files -- remove deprecated files
	
	let manifestEach (n,dta'@(rep',md')) man0M = man0M >>= doManifestFocus parentPath n tree dta' (manifestUnder n $ fileInfo $ get_fmd_header md')
	foldr manifestEach (return man1) (zip new_files dtas')

doManifestCompoundWithConstraint :: (FSRep fs,ForestMD md',Matching fs a) =>
	FilePath -> a -> FSTree fs
	-> (container_rep -> [(FilePath,rep')]) -> (container_md -> [(FilePath,md')])
	-> (FileName -> FileInfo -> Bool)
	-> (container_rep,container_md)
	-> (FileName -> FileInfo -> (rep',md') -> Manifest fs -> ForestM fs (Manifest fs))
	-> Manifest fs -> ForestM fs (Manifest fs)
doManifestCompoundWithConstraint parentPath matching tree toListRep toListMd pred (c_rep,c_md) manifestUnder man = do
	old_files <- getMatchingFilesInTree parentPath matching tree
	
	let reps' = map snd $ toListRep c_rep
	let (new_files,mds') = unzip $ toListMd c_md
	let dtas' = zip reps' mds'
	
	let old_files' = old_files \\ new_files -- old files that are not in the view
	old_metadatas' <- mapM (getRelForestMDInTree parentPath tree) old_files'
	-- we need to check which old files satisfy the predicate
	let old_values' = filter (\(n,fmd) -> pred n $ fileInfo fmd) $ zip old_files' old_metadatas'
	let rem_files = map fst old_values'
	-- and delete them
	man1 <- foldr (\rem_path man0M -> canonalizePathWithTree rem_path tree >>= \canpath -> liftM (removePathFromManifest canpath rem_path) man0M) (return man) $ map (parentPath </>) rem_files -- remove deprecated files
	
	let manifestEach (n,(rep',md')) man0M = man0M >>= doManifestConstraint tree (\_ -> pred n $ fileInfo $ get_fmd_header md') (rep',md')
		(\dta' -> doManifestFocus parentPath n tree dta' (manifestUnder n $ fileInfo $ get_fmd_header md'))
	foldr manifestEach (return man1) (zip new_files dtas')


