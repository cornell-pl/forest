{-# LANGUAGE TupleSections, FlexibleContexts, ScopedTypeVariables, GADTs, FlexibleInstances,MultiParamTypeClasses,UndecidableInstances, ViewPatterns #-}



module Language.Forest.Pure.IO.Loading where

import Language.Forest.IO.Utils
import Language.Forest.Syntax
import Language.Pads.Padsc
import Language.Forest.Pure.MetaData
import Language.Forest.Errors
import Language.Forest.Pure.MetaData
import Language.Forest.IO.Shell
import Data.IORef

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

import qualified Control.Exception as CE

import Data.Data
import Data.Maybe
import System.Random
import Data.Proxy
import Control.Monad.Lazy


-- | lazy file loading
-- XXX: Pads errors do not contribute to the Forest error count
doLoadFile :: (MonadLazy (ForestM fs),Eq md,Eq pads,Data pads,Data md,FSRep fs,Pads pads md) => Proxy pads -> FilePath -> FSTree fs -> GetForestMD fs -> ForestM fs (pads,(Forest_md,md))
doLoadFile repProxy path tree getMD = debug ("doLoadFile " ++ show (path)) $ checkPath (Just False) path tree $ do
	(rep,md) <- pathInTree path tree >>= lazily . forestIO . parseFile
	fmd <- lazily $ getMD path tree
	return (rep,(fmd,md))


-- | lazy file loading
-- XXX: Pads specs currently accept a single optional argument and have no incremental loading, so a change in the argument's value requires recomputation
-- XXX: Pads errors do not contribute to the Forest error count
doLoadFile1 :: (MonadLazy (ForestM fs),ForestMD md,Eq arg,Eq pads,Eq md,Data pads,Data md,FSRep fs,Pads1 arg pads md) => Proxy pads -> arg -> FilePath -> FSTree fs -> GetForestMD fs -> ForestM fs (pads,(Forest_md,md))
doLoadFile1 repProxy arg path tree getMD = debug ("doLoadFile1 " ++ show (path)) $ checkPath (Just False) path tree $ do
	(rep,md) <- pathInTree path tree >>= lazily . forestIO . parseFile1 arg
	fmd <- lazily $ getMD path tree
	return (rep,(fmd,md))

-- | compressed archive (tar,gz,zip)
-- AVFS gives us some laziness guarantees; the archived is only decompressed on demand
doLoadArchive :: (MonadLazy (ForestM fs),ForestMD md,Data rep,FSRep fs,Data md) =>
	[ArchiveType] -> FilePath -> FSTree fs -> GetForestMD fs
	-> (FilePath -> GetForestMD fs -> FSTree fs -> ForestM fs (rep,md))
	-> ForestM fs (rep,(Forest_md,md))
doLoadArchive exts path tree getMD load = checkPath (Just False) path tree $ checkFileExtension (archiveExtension exts) path $ do
	fmd <- lazily $ getMD path tree
	let contentPath = cardinalPath path -- add a cardinal to the filepath to denote the contents of the archive, e.g., the path /a/b/file.tar.gz#/a.txt refers to file a.txt inside a tarball /a/b/file/tar.gz
	avfsTree <- virtualTree tree -- enter AVFS filepath mode
	
	(rep,md_arch) <- load contentPath getForestMDInTree avfsTree -- we change the current path to the temporary directory
	let fmd' = updateForestMDErrorsWith fmd [get_errors md_arch]
	return (rep,(fmd',md_arch))

doLoadSymLink :: (MonadLazy (ForestM fs),FSRep fs) => FilePath -> FSTree fs -> GetForestMD fs -> ForestM fs (FilePath,(Forest_md,Base_md))
doLoadSymLink path tree getMD = checkPath Nothing path tree $ do
	md <- lazily $ getMD path tree
	case symLink (fileInfo md) of
		Just sym -> return (sym,(md,cleanBasePD))
		Nothing -> do
			let md' = updateForestMDErrorsWith md [ioExceptionForestErr]
			return ("", (md',cleanBasePD))

doLoadConstraint :: (ForestMD md,FSRep fs,Eq md,Data rep) => FSTree fs -> ((rep,md) -> Bool) -> ForestM fs (rep,md) -> ForestM fs (rep,md)
doLoadConstraint tree pred load = do -- note that constraints do not consider the current path
	result@(rep,md) <- load
	let md' = replace_errors md $ \err ->
		if pred result
			then err
			else updateForestErr err [constraintViolationForestErr]
	return (rep,md')

-- changes the current path
doLoadFocus :: (FSRep fs,Matching a,ForestMD md) => FilePath -> a -> FSTree fs -> GetForestMD fs -> (FilePath -> GetForestMD fs -> ForestM fs (rep,md)) -> ForestM fs (rep,md)
doLoadFocus path matching tree getMD load = do
	files <- getMatchingFilesInTree path matching tree
	case files of
		[file] -> doLoadNewPath path file tree getMD load
		files -> doLoadNewPath path (pickFile files) tree getMD $ \newpath newgetMD -> do
			(rep,md) <- load newpath newgetMD
			md' <- if length files == 0
				then return md -- if there is no match then an error will pop from the recursive load
				else return $ addMultipleMatchesErrorMD newpath files md
			return (rep,md')

doLoadNewPath :: (FSRep fs) => FilePath -> FilePath -> FSTree fs -> GetForestMD fs -> (FilePath -> GetForestMD fs -> ForestM fs x) -> ForestM fs x
doLoadNewPath oldpath file tree getMD load = debug ("doLoadNewPath " ++ show (oldpath </> file)) $ do
	newpath <- stepPathInTree tree oldpath file -- changes the old path by a relative path, check the path traversal restrictions specific to each FS instantiation
	load newpath getMD

doLoadDirectory :: (Eq rep,Eq md,Data rep,FSRep fs,Data md)
	=> FilePath -> FSTree fs -> (md -> Forest_err) -> GetForestMD fs -> ForestM fs (rep,md) -> ForestM fs (rep,(Forest_md,md))
doLoadDirectory path tree collectMDErrors getMD ifGood = debug ("doLoadDirectory: "++show path) $ do
	checkPath (Just True) path tree $ do
		(rep,mds) <- ifGood
		fmd <- getMD path tree
		let fmd' = updateForestMDErrorsWith fmd [collectMDErrors mds]
		return (rep,(fmd',mds))

doLoadMaybe :: (FSRep fs,Eq rep,Eq md,ForestMD md) => FilePath -> FSTree fs -> ForestM fs (rep,md) -> ForestM fs (Maybe rep,(Forest_md,Maybe md))
doLoadMaybe path tree ifExists = do
	exists <- doesExistInTree path tree
	if exists
		then do
			(rep,md) <- ifExists
			let fmd = get_fmd_header md
			return (Just rep,(fmd,Just md))
		else do
			let fmd = cleanForestMDwithFile path
			return (Nothing,(fmd,Nothing))

-- since the focus changes we need to compute the (eventually) previously loaded metadata of the parent node
doLoadSimple :: (FSRep fs,Matching a,Data rep',ForestMD md') =>
	FilePath -> a -> FSTree fs
	-> (FilePath -> GetForestMD fs -> ForestM fs (rep',md'))
	-> ForestM fs (rep',md')
doLoadSimple path matching tree load = doLoadFocus path matching tree getForestMDInTree load

-- since the focus changes we need to compute the (eventually) previously loaded metadata of the parent node
doLoadSimpleWithConstraint :: (Eq md',FSRep fs,Matching a,Data rep',ForestMD md') =>
	FilePath -> a -> FSTree fs -> ((rep',md') -> Bool)
	-> (FilePath -> GetForestMD fs -> ForestM fs (rep',md'))
	-> ForestM fs (rep',md')
doLoadSimpleWithConstraint path matching tree pred load = doLoadConstraint tree pred $ doLoadFocus path matching tree getForestMDInTree load

doLoadCompound :: (FSRep fs,Matching a,Data rep',ForestMD md') =>
	FilePath -> a -> FSTree fs
	-> ([(FilePath,rep')] -> container_rep) -> ([(FilePath,md')] -> container_md)
	-> (FileName -> FileInfo -> FilePath -> GetForestMD fs -> ForestM fs (rep',md'))
	-> ForestM fs (container_rep,container_md)
doLoadCompound path matching tree buildContainerRep buildContainerMd load = debug ("doLoadCompound: "++show path) $ do
	files <- getMatchingFilesInTree path matching tree
	metadatas <- mapM (getRelForestMDInTree path tree) files
	let filesmetas = zip files metadatas
	let loadEach (n,n_md) = liftM (n,) $ doLoadFocus path n tree (const2 $ return n_md) $ \newpath newGetMD -> do
		(rep',md') <- load n (fileInfo n_md) newpath newGetMD
		return (rep',md')
	loadlist <- mapM loadEach filesmetas
	let replist = map (id >< fst) loadlist
	let mdlist = map (id >< snd) loadlist
	return (buildContainerRep replist,buildContainerMd mdlist)

doLoadCompoundWithConstraint :: (FSRep fs,Matching a,Data rep',ForestMD md') =>
	FilePath -> a -> FSTree fs
	-> (FilePath -> FileInfo -> Bool)
	-> ([(FilePath,rep')] -> container_rep) -> ([(FilePath,md')] -> container_md)
	-> (FileName -> FileInfo -> FilePath -> GetForestMD fs -> ForestM fs (rep',md'))
	-> ForestM fs (container_rep,container_md)
doLoadCompoundWithConstraint path matching tree pred buildContainerRep buildContainerMd load = debug ("doLoadCompound: "++show path) $ do
	files <- getMatchingFilesInTree path matching tree
	metadatas <- mapM (getRelForestMDInTree path tree) files
	let filesmetas = zip files metadatas
	let filesmetas' = filter (\(n,fmd) -> pred n $ fileInfo fmd) filesmetas
	let loadEach (n,n_md) = do
		(rep,md) <- doLoadFocus path n tree (const2 $ return n_md) $ load n (fileInfo n_md)
		return (n,(rep,md))
	loadlist <- mapM loadEach filesmetas'
	let replist = map (id >< fst) loadlist
	let mdlist = map (id >< snd) loadlist
	return (buildContainerRep replist,buildContainerMd mdlist)

-- ** auxiliary functions

checkPathData :: (FSRep fs,Data rep) => Bool -> FilePath -> FSTree fs -> ForestM fs rep -> ForestM fs rep
checkPathData isDir path tree ifExists = do
	exists <- if isDir then doesDirectoryExistInTree path tree else doesFileExistInTree path tree
	if exists then ifExists else return forestdefault
checkPathMeta :: (FSRep fs,Data md,ForestMD md) => Bool -> FilePath -> FSTree fs -> ForestM fs md -> ForestM fs md
checkPathMeta isDir path tree ifExists = do
	let (doesExist,missingErr) = if isDir then (doesDirectoryExistInTree,missingDirForestErr) else (doesFileExistInTree,missingPathForestErr)
	exists <- doesExist path tree
	if exists
		then ifExists
		else do
			let def_md = forestdefault
			let def_md' = replace_errors def_md $ Prelude.const $ missingErr path
			return def_md'
checkPath :: (FSRep fs,Data md,Data rep,ForestMD md) => Maybe Bool -> FilePath -> FSTree fs -> ForestM fs (rep,md) -> ForestM fs (rep,md)
checkPath cond path tree ifExists = do
	exists <- case cond of
		Nothing -> doesExistInTree path tree
		Just isDir -> if isDir then doesDirectoryExistInTree path tree else doesFileExistInTree path tree
	if exists
		then ifExists
		else do
			let def_rep = forestdefault
			let def_md = forestdefault
			let def_md' = replace_errors def_md $ Prelude.const $ missingPathForestErr path
			return (def_rep,def_md')
			
checkFileExtension :: (FSRep fs,Data md,Data rep,ForestMD md) => String -> FilePath -> ForestM fs (rep,md) -> ForestM fs (rep,md)
checkFileExtension ext path ifExists = do
	if isSuffixOf ext path
		then ifExists
		else do
			let def_rep = forestdefault
			let def_md = forestdefault
			let def_md' = replace_errors def_md $ Prelude.const $ wrongFileExtensionForestErr ext path
			return (def_rep,def_md')
