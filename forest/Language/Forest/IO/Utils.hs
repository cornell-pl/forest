{-# LANGUAGE ConstraintKinds, TupleSections, FlexibleContexts, ScopedTypeVariables, GADTs, FlexibleInstances,MultiParamTypeClasses,UndecidableInstances, ViewPatterns #-}

module Language.Forest.IO.Utils where

import Language.Pads.Padsc hiding (numErrors)
import Language.Forest.Errors
import Language.Forest.IO.Shell

import qualified System.FilePath.Posix
import System.FilePath.Canonical
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
import Data.WithClass.MData
import Language.Forest.Syntax

import qualified Control.Exception as CE

import Data.Data
import Data.Maybe
import System.Random
import Data.Set as Set
import Data.Proxy

getDirectoryContentsTry :: FilePath -> IO [FilePath]
getDirectoryContentsTry path = do
	e <- CE.try $ getDirectoryContents path
	case e of
		Left (e::CE.SomeException) -> return []
		Right contents -> return contents

type FileName = FilePath

prodM :: Monad m => m a -> m b -> m (a,b)
prodM mx my = do { x <- mx; y <- my; return (x,y) }

mergeJust2 (Just x,Just y) = Just (x,y)
mergeJust2 _ = Nothing

getTempForestDirectory :: IO FilePath
getTempForestDirectory = do 
	tempDir <- getTemporaryDirectory 
	let forestDir = tempDir </> "Forest"
	createDirectoryIfMissing False forestDir
	return forestDir

getTempForestScratchDirectory :: IO FilePath
getTempForestScratchDirectory = do 
	tempDir <- getTempForestDirectory
	let forestScratchDir = tempDir </> "Scratch"
	createDirectoryIfMissing False forestScratchDir
	return forestScratchDir

getTempPath :: IO FilePath
getTempPath = do 
	fp <- getTempForestScratchDirectory
	(fp', handle) <- openTempFile fp "Forest" -- generates a unique randomly generated filename
	hClose handle
	remove fp' -- removes the file, as we only need its name; the file will be created later
	return fp'

getTempDir :: IO FilePath
getTempDir = do 
	fp <- getTempPath
	createDirectoryIfMissing False fp
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

absolutePath :: FilePath -> IO FilePath
absolutePath path = if isRelative path then liftM (</> path) getCurrentDirectory else return path

commonParentPath :: FilePath -> FilePath -> FilePath
commonParentPath path1 path2 = joinPath $ List.map fst $ takeWhile (uncurry (==)) $ zip (splitDirectories path1) (splitDirectories path2)

data GL = GL String deriving (Eq,Show)

filterByRegex (RE regStr) candidates = 
  let re = mkRegexWithOpts ('^':regStr++"$") True True
      matchOne str = isJust (matchRegex re str)
  in Prelude.filter matchOne candidates

compFilter f1 f2 item =  f1 item && f2 item 

--getMatchingFilesGlob' :: FilePath -> GL -> IO [FilePath]
--getMatchingFilesGlob' path (GL glob) = do 
--  { let gl = compile glob
--  ; ([matches], unmatches) <- globDir [gl] path
--  ; return matches
--  }

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
decompressArchive :: [ArchiveType] -> FilePath -> FilePath -> IO ()
decompressArchive [x] oriFile archiveDir = decompressArchive' x oriFile archiveDir
decompressArchive (snocMay -> Just (xs,Gzip)) oriFile archiveDir = do
	tmpFile <- getTempPath
	runShellCommand_ $ "gunzip " ++ oriFile ++ " -c > " ++ tmpFile
	decompressArchive xs tmpFile archiveDir
	removePath tmpFile
	return ()
decompressArchive (snocMay -> Just (xs,Bzip)) oriFile archiveDir = do
	tmpFile <- getTempPath
	runShellCommand_ $ "bzip2 -kcd " ++ oriFile ++ " > " ++ archiveDir
	decompressArchive xs tmpFile archiveDir
	removePath tmpFile
	return ()
decompressArchive xs _ _ = error $ "decompression not implemented " ++ show xs

decompressArchive' :: ArchiveType -> FilePath -> FilePath -> IO ()
decompressArchive' Gzip oriFile archiveDir = runShellCommand_ ("gunzip " ++ oriFile ++ " -c > " ++ archiveDir) >> return ()
decompressArchive' Tar oriFile archiveDir = do
	oldCurDir <- getCurrentDirectory
	setCurrentDirectory archiveDir
	runShellCommand_ $ "tar -xf " ++ oriFile
	setCurrentDirectory oldCurDir
decompressArchive' Zip oriFile archiveDir = error $ "decompression not implemented " ++ show Zip
decompressArchive' Bzip oriFile archiveDir = runShellCommand_ ("bzip2 -kcd " ++ oriFile ++ " > " ++ archiveDir) >> return ()
decompressArchive' Rar oriFile archiveDir = error $ "decompression not implemented " ++ show Rar

-- | archive extensions, content directory, output compressed file
compressArchive :: [ArchiveType] -> FilePath -> FilePath -> IO ()
compressArchive [x] archiveDir archiveFile = compressArchive' x archiveDir archiveFile
compressArchive (snocMay -> Just (xs,Gzip)) archiveDir archiveFile = do
	tmpFile <- getTempPath
	compressArchive xs archiveDir tmpFile
	runShellCommand_ $ "gzip -kc " ++ tmpFile ++ " > " ++ archiveFile
	removePath tmpFile
	return ()
compressArchive (snocMay -> Just (xs,Bzip)) archiveDir archiveFile = do
	tmpFile <- getTempPath
	compressArchive xs archiveDir tmpFile
	runShellCommand_ $ "bzip2 -kc " ++ tmpFile ++ " > " ++ archiveFile
	removePath tmpFile
	return ()
compressArchive xs _ _ = error $ "compression not implemented " ++ show xs

compressArchive' :: ArchiveType -> FilePath -> FilePath -> IO ()
compressArchive' Gzip archiveDir archiveFile = error $ "compression not implemented " ++ show Gzip
compressArchive' Tar archiveDir archiveFile = error $ "compression not implemented " ++ show Tar
compressArchive' Zip archiveDir archiveFile = error $ "compression not implemented " ++ show Zip
compressArchive' Bzip archiveDir archiveFile = runShellCommand_ ("bzip2 -kc " ++ archiveDir ++ " > " ++ archiveFile) >> return ()
compressArchive' Rar archiveDir archiveFile = error $ "compression not implemented " ++ show Rar

snocMay :: [a] -> Maybe ([a],a)
snocMay [] = Nothing
snocMay xs = Just (init xs,last xs)

readOptSymLink :: FilePath -> IO (Maybe FilePath)
readOptSymLink fp = do 
	link <- readSymbolicLink fp
	return (Just link)

isAscii :: FilePath -> IO Bool
isAscii fp =  do
	let cmd = "file -i -L " ++ show fp
	result <- runShellCommand cmd
	return ("ascii" `List.isSuffixOf` result)

canonalizePath :: FilePath -> IO FilePath
canonalizePath path = liftM canonicalFilePath $ canonical path

getRep m = do { ~(rep,md) <- m; return rep }
getMd m = do { ~(rep,md) <- m; return md }