{-# LANGUAGE ConstraintKinds, TupleSections, FlexibleContexts, ScopedTypeVariables, GADTs, FlexibleInstances,MultiParamTypeClasses,UndecidableInstances, ViewPatterns #-}

module Language.Forest.IO.Utils where

import Language.Pads.Padsc hiding (numErrors)
import Language.Forest.MetaData
--import Language.Forest.Generic
import Language.Forest.Errors
import Language.Forest.FS.FSDelta
import Language.Forest.MetaData
--import Language.Forest.ListDiff
import Control.Monad.Incremental
import Language.Forest.Shell

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
import Data.List as List
import Language.Forest.FS.FSRep
--import Control.Monad.IO.Class
import Data.WithClass.MData
import Language.Forest.Shell
import Language.Forest.BX
import Language.Forest.Syntax

import qualified Control.Exception as CE

import Data.Data
import Data.Maybe
import System.Random
import Data.Set as Set
import Data.Proxy

prodM :: Monad m => m a -> m b -> m (a,b)
prodM mx my = do { x <- mx; y <- my; return (x,y) }

mergeJustDefault :: (FSRep fs,ForestLayer fs l,MData NoCtx (ForestL fs l) a,MData NoCtx (ForestL fs l) b) => (Maybe a,Maybe b) -> ForestL fs l (a,b)
mergeJustDefault (Just x,Just y) = return (x,y)
mergeJustDefault (Just x,Nothing) = liftM (x,) mdefault
mergeJustDefault (Nothing,Just y) = liftM (,y) mdefault
mergeJustDefault (Nothing,Nothing) = tupM mdefault mdefault

mergeJust2 (Just x,Just y) = Just (x,y)
mergeJust2 _ = Nothing

getRep m = do { ~(rep,md) <- m; return rep }
getMd m = do { ~(rep,md) <- m; return md }

getTempForestDirectory :: IO FilePath
getTempForestDirectory = do 
	tempDir <- getTemporaryDirectory 
	let forestDir = tempDir </> "Forest"
	createDirectoryIfMissing False forestDir
	return forestDir

getTempForestScratchDirectory :: (FSRep fs,ForestLayer fs l) => ForestL fs l FilePath
getTempForestScratchDirectory = do 
	tempDir <- getForestDirectory
	let forestScratchDir = tempDir </> "Scratch"
	forestIO $ createDirectoryIfMissing False forestScratchDir
	return forestScratchDir

getTempPath :: (FSRep fs,ForestLayer fs l) => ForestL fs l FilePath
getTempPath = do 
	fp <- getTempForestScratchDirectory
	(fp', handle) <- forestIO $ openTempFile fp "Forest" -- generates a unique randomly generated filename
	forestIO $ hClose handle
	forestIO $ remove fp' -- removes the file, as we only need its name; the file will be created later
	return fp'

getTempDir :: (FSRep fs,ForestLayer fs l) => ForestL fs l FilePath
getTempDir = do 
	fp <- getTempPath
	forestIO $ createDirectoryIfMissing False fp
	return fp

remove :: FilePath -> IO ()
remove path = do
	let cmd = "rm -rf " ++ path   
	exitCode <- system cmd
	return ()

pickFile :: [FilePath] -> FilePath
pickFile files = case files of
  [] -> "--NO FILE--"  -- an illegal file path
  f:fs -> f

mergeKeyedLists :: [(a,b)] -> [(a,c)] -> [(a,(b,c))]
mergeKeyedLists [] [] = []
mergeKeyedLists ((a,b):xs) ((_,c):ys) = (a,(b,c)) : mergeKeyedLists xs ys

mapFromJust :: [Maybe a] -> [a]
mapFromJust [] = []
mapFromJust (Nothing:xs) = mapFromJust xs
mapFromJust (Just x:xs) = x : mapFromJust xs

--invalidateForestMD :: Language.Forest.Errors.ErrMsg -> Forest_md fs -> Forest_md fs
--invalidateForestMD errMsg f_md = f_md { numErrors = numErrors f_md + 1, errorMsg = Just errMsg }

isParentPathOf :: FilePath -> FilePath -> Bool
isParentPathOf = isPrefixOf

commonParentPath :: FilePath -> FilePath -> FilePath
commonParentPath path1 path2 = joinPath $ List.map fst $ takeWhile (uncurry (==)) $ zip (splitDirectories path1) (splitDirectories path2)

getMatchingFilesInTreeM :: (ForestLayer fs l,Matching fs a) => FilePath -> ForestL fs l a -> FSTree fs -> ForestL fs l [FilePath]
getMatchingFilesInTreeM path matchingM tree = do
	matching <- matchingM
	getMatchingFilesInTree path matching tree

data GL = GL String

class FSRep fs => Matching fs a where
	getMatchingFiles           :: FilePath -> a -> IO [FilePath]
--	getMatchingFilesWithFilter :: FilePath -> (FilePath -> Bool) -> a -> IO [FilePath]
	getMatchingFilesInTree :: ForestLayer fs l => FilePath -> a -> FSTree fs -> ForestL fs l [FilePath]

instance (FSRep fs) => Matching fs [FilePath] where
	getMatchingFiles _ files = return files
--	getMatchingFilesWithFilter _ _ files = return files
	getMatchingFilesInTree _ files _ = return files

instance (FSRep fs) => Matching fs FilePath where
	getMatchingFiles _ file = return [file]
--	getMatchingFilesWithFilter _ _ file = return [file]
	getMatchingFilesInTree _ file _ = return [file]

instance (FSRep fs) => Matching fs RE where
 getMatchingFiles           = getMatchingFilesRE
-- getMatchingFilesWithFilter = getMatchingFilesWithFilterRE
 getMatchingFilesInTree = getMatchingFilesREInTree

instance (FSRep fs) => Matching fs GL where
 getMatchingFiles = getMatchingFilesGlob
-- getMatchingFilesWithFilter = getMatchingFilesWithFilterGlob
 getMatchingFilesInTree = getMatchingFilesGlobInTree

getMatchingFilesRE :: FilePath -> RE -> IO [FilePath]
getMatchingFilesRE path re = do 
	files <- getDirectoryContents path
	let matches = (filterByRegex re files)
	return matches

getMatchingFilesREInTree :: (FSRep fs,ForestLayer fs l) => FilePath -> RE -> FSTree fs -> ForestL fs l [FilePath]
getMatchingFilesREInTree path re tree = do
	files <- getDirectoryContentsInTree path tree
	let matches = (filterByRegex re files)
	return matches

--getMatchingFilesWithFilterRE :: FilePath -> (FilePath -> Bool) -> RE -> IO [FilePath]
--getMatchingFilesWithFilterRE path afilter re = do 
--	files <- getDirectoryContents path
--	let matches = (filterByRegex re files)
--	return (Prelude.filter afilter matches)

filterByRegex (RE regStr) candidates = 
  let re = mkRegexWithOpts ('^':regStr++"$") True True
      matchOne str = isJust (matchRegex re str)
  in Prelude.filter matchOne candidates

getMatchingFilesGlob :: FilePath -> GL -> IO [FilePath]
getMatchingFilesGlob path (GL glob) = do 
	let gl = compile glob
	files <- getDirectoryContents path
	let matches = (Prelude.filter (match gl) files)
	return matches

getMatchingFilesGlobInTree :: (FSRep fs,ForestLayer fs l) => FilePath -> GL -> FSTree fs -> ForestL fs l [FilePath]
getMatchingFilesGlobInTree path (GL glob) tree = do
	let gl = compile glob
	files <- getDirectoryContentsInTree path tree
	let matches = (Prelude.filter (match gl) files)
	return matches

--getMatchingFilesWithFilterGlob :: FilePath -> (FilePath -> Bool) -> GL -> IO [FilePath]
--getMatchingFilesWithFilterGlob path afilter (GL glob) = do 
--  { let gl = compile glob
--  ; files <- getDirectoryContents path
--  ; let matches = Prelude.filter (compFilter (match gl) afilter) files
--  ; return matches
--  }

compFilter f1 f2 item =  f1 item && f2 item 

getRelForestMDInTree :: (FSRep fs,ForestInput fs FSThunk Inside,ForestLayer fs l) => FilePath -> FSTree fs -> FilePath -> ForestL fs l (Forest_md fs)
getRelForestMDInTree path tree file = getForestMDInTree (path </> file) tree

getMatchingFilesGlob' :: FilePath -> GL -> IO [FilePath]
getMatchingFilesGlob' path (GL glob) = do 
  { let gl = compile glob
  ; ([matches], unmatches) <- globDir [gl] path
  ; return matches
  }

--envVar str = fromJust (unsafePerformIO $ (System.Posix.Env.getEnv str))

infixl 6 ><
(><) :: (a -> c) -> (b -> d) -> (a,b) -> (c,d)
(><) f g (x,y) = (f x,g y)

infixl 6 >><<
(>><<) :: Monad m => (a -> m c) -> (b -> m d) -> (a,b) -> m (c,d)
(>><<) f g (x,y) = do { x' <- f x; y' <- g y; return (x',y') }

tupM :: Monad m => m a -> m b -> m (a,b)
tupM ma mb = do { a <- ma; b <- mb; return (a,b) }

rtupM :: Monad m => b -> a -> m (a,b)
rtupM b a = return (a,b)

mergeLists :: [([a],[b])] -> ([a],[b])
mergeLists l = let (xxs,yys) = unzip l in (concat xxs,concat yys)

foldM1 :: Monad m => (a -> a -> m a) -> [a] -> m a
foldM1 f [] = error "foldM1: empty list"
foldM1 f [x] = return x
foldM1 f (x:y:xs) = f x y >>= \z -> foldM1 f (z:xs)

const2 = Prelude.const . Prelude.const


-- archive manipulation

-- | archive extensions, original compressed file, temporary output directory
decompressArchive :: FSRep fs => [ArchiveType] -> FilePath -> FilePath -> ForestO fs ()
decompressArchive [x] oriFile archiveDir = decompressArchive' x oriFile archiveDir
decompressArchive (snocMay -> Just (xs,Gzip)) oriFile archiveDir = do
	tmpFile <- getTempPath
	forestIO $ runShellCommand_ $ "gunzip " ++ oriFile ++ " -c > " ++ tmpFile
	decompressArchive xs tmpFile archiveDir
	forestIO $ removePath tmpFile
	return ()
decompressArchive (snocMay -> Just (xs,Bzip)) oriFile archiveDir = do
	tmpFile <- getTempPath
	forestIO $ runShellCommand_ $ "bzip2 -kcd " ++ oriFile ++ " > " ++ archiveDir
	decompressArchive xs tmpFile archiveDir
	forestIO $ removePath tmpFile
	return ()
decompressArchive xs _ _ = error $ "decompression not implemented " ++ show xs

decompressArchive' :: FSRep fs => ArchiveType -> FilePath -> FilePath -> ForestO fs ()
decompressArchive' Gzip oriFile archiveDir = forestIO $ runShellCommand_ ("gunzip " ++ oriFile ++ " -c > " ++ archiveDir) >> return ()
decompressArchive' Tar oriFile archiveDir = do
	oldCurDir <- forestIO $ getCurrentDirectory
	forestIO $ setCurrentDirectory archiveDir
	forestIO $ runShellCommand_ $ "tar -xf " ++ oriFile
	forestIO $ setCurrentDirectory oldCurDir
decompressArchive' Zip oriFile archiveDir = error $ "decompression not implemented " ++ show Zip
decompressArchive' Bzip oriFile archiveDir = forestIO $ runShellCommand_ ("bzip2 -kcd " ++ oriFile ++ " > " ++ archiveDir) >> return ()
decompressArchive' Rar oriFile archiveDir = error $ "decompression not implemented " ++ show Rar

-- | archive extensions, content directory, output compressed file
compressArchive :: FSRep fs => [ArchiveType] -> FilePath -> FilePath -> ForestO fs ()
compressArchive [x] archiveDir archiveFile = compressArchive' x archiveDir archiveFile
compressArchive (snocMay -> Just (xs,Gzip)) archiveDir archiveFile = do
	tmpFile <- getTempPath
	compressArchive xs archiveDir tmpFile
	forestIO $ runShellCommand_ $ "gzip -kc " ++ tmpFile ++ " > " ++ archiveFile
	forestIO $ removePath tmpFile
	return ()
compressArchive (snocMay -> Just (xs,Bzip)) archiveDir archiveFile = do
	tmpFile <- getTempPath
	compressArchive xs archiveDir tmpFile
	forestIO $ runShellCommand_ $ "bzip2 -kc " ++ tmpFile ++ " > " ++ archiveFile
	forestIO $ removePath tmpFile
	return ()
compressArchive xs _ _ = error $ "compression not implemented " ++ show xs

compressArchive' :: FSRep fs => ArchiveType -> FilePath -> FilePath -> ForestO fs ()
compressArchive' Gzip archiveDir archiveFile = error $ "compression not implemented " ++ show Gzip
compressArchive' Tar archiveDir archiveFile = error $ "compression not implemented " ++ show Tar
compressArchive' Zip archiveDir archiveFile = error $ "compression not implemented " ++ show Zip
compressArchive' Bzip archiveDir archiveFile = forestIO $ runShellCommand_ ("bzip2 -kc " ++ archiveDir ++ " > " ++ archiveFile) >> return ()
compressArchive' Rar archiveDir archiveFile = error $ "compression not implemented " ++ show Rar

snocMay :: [a] -> Maybe ([a],a)
snocMay [] = Nothing
snocMay xs = Just (init xs,last xs)
