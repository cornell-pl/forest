{-# LANGUAGE TupleSections, FlexibleContexts, ScopedTypeVariables, GADTs, FlexibleInstances,MultiParamTypeClasses,UndecidableInstances, ViewPatterns #-}

module Language.Forest.Pure.IO.Default where

import Language.Forest.IO.Utils
import Language.Forest.Syntax
import Language.Pads.Padsc as Pads
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


doDefaultFile :: (FSRep fs,Pads pads md) => pads -> FilePath -> ForestM fs (Forest_md,md)
doDefaultFile rep path = do
	let md = Pads.defaultMd rep
	return (cleanForestMDwithFile path,md)

doDefaultFile1 :: (FSRep fs,Pads1 arg pads md) => arg -> pads -> FilePath -> ForestM fs (Forest_md,md)
doDefaultFile1 arg rep path = do
	let md = Pads.defaultMd1 arg rep
	return (cleanForestMDwithFile path,md)

doDefaultArchive :: FSRep fs => [ArchiveType] -> rep -> FilePath -> (rep -> FilePath -> ForestM fs md) -> ForestM fs (Forest_md,md)
doDefaultArchive exts rep path doContent = do
	md <- doContent rep (cardinalPath path)
	return (cleanForestMDwithFile path,md)

doDefaultSymLink :: FSRep fs => FilePath -> FilePath -> ForestM fs (Forest_md,Base_md)
doDefaultSymLink tgt path = do
	let md = cleanForestMDwithFile path
	let md' = md { fileInfo = (fileInfo md) { symLink = Just tgt } }
	return (md',cleanBasePD)
	
doDefaultFocus :: (FSRep fs,Matching fs a) => rep -> FilePath -> a -> (FilePath -> ForestM fs md) -> ForestM fs md
doDefaultFocus rep path matching doFocus = do
	files <- defaultMatch Proxy matching
	let file = pickFile files
	doFocus (path </> file) 

doDefaultDirectory :: FSRep fs => rep -> FilePath -> ForestM fs md -> ForestM fs (Forest_md,md)
doDefaultDirectory rep path doDir = do
	md <- doDir
	return (cleanForestMDwithFile path,md)

doDefaultMaybe :: FSRep fs => Maybe rep -> FilePath -> (rep -> ForestM fs md) -> ForestM fs (Forest_md,Maybe md)
doDefaultMaybe Nothing path doMaybe = return (cleanForestMDwithFile path,Nothing)
doDefaultMaybe (Just rep) path doMaybe = do
	md <- doMaybe rep
	return (cleanForestMDwithFile path,Just md)

doDefaultCompound :: FSRep fs => container_rep -> FilePath -> (container_rep -> [(FileName,rep)]) -> ([(FilePath,md)] -> container_md) -> (FileName -> FileInfo -> rep -> FilePath -> ForestM fs md) -> ForestM fs container_md
doDefaultCompound c_rep path toListContainerRep buildContainerMd doDir = do
	let reps = toListContainerRep c_rep
	
	let doEach (file,rep) = liftM (file,) $ doDefaultFocus rep path file (doDir file fileInfo_def rep)
	liftM buildContainerMd $ mapM doEach reps

