{-# LANGUAGE TupleSections, FlexibleContexts, ScopedTypeVariables, GADTs, FlexibleInstances,MultiParamTypeClasses,UndecidableInstances, ViewPatterns #-}

module Language.Forest.IO.Storing where

import Language.Forest.Manifest
import Prelude hiding (const,read,mod)
import qualified Prelude
import Language.Forest.IO.Utils
import Language.Forest.Syntax
import Language.Forest.FS.Diff
import Language.Forest.ValueDelta
import Language.Pads.Padsc
import Language.Forest.MetaData
import Language.Forest.Generic
import Language.Forest.Errors
import Language.Forest.FS.FSDelta
import Language.Forest.MetaData
import Control.Monad.Incremental hiding (memo)
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
import Data.Monoid
import Data.List
import Language.Forest.FS.FSRep
--import Control.Monad.IO.Class
import Data.WithClass.MData

import Language.Forest.IO.Memo

import qualified Control.Exception as CE

import Data.Data
import Data.Maybe
import System.Random
import Data.Proxy

-- adds consistency checks for top-level specification arguments
doManifestArgs :: ForestArgs fs args =>
	Proxy args -> ForestIs fs args
	-> (rep,(md,ForestICThunksI fs args))
	-> ((rep,md) -> Manifest fs -> ForestO fs (Manifest fs))
	-> Manifest fs -> ForestO fs (Manifest fs)
doManifestArgs proxy margs (rep,(md,targs)) manifestContent (man::Manifest fs) = do
	let man1 = addTestToManifest (checkArgs (Proxy::Proxy fs) proxy margs targs) man
	manifestContent (rep,md) man1

doManifestFile :: (Eq pads,Eq md,FSRep fs,Pads pads md) => FSTree fs -> (ForestFSThunkI fs pads,ForestFSThunkI fs (Forest_md fs,ForestFSThunkI fs md)) -> Manifest fs -> ForestO fs (Manifest fs)
doManifestFile tree (rep_t,md_t) man = do
	rep <- inside $ fsforce rep_t
	(fmd,md_t') <- inside $ fsforce md_t
	md <- inside $ fsforce md_t'
	let path = fullpath $ fileInfo fmd
	dskpath <- canonalizePathInTree path tree
	canpath <- pathFromTree dskpath tree
	valid <- isValidMD fmd
	if valid
		then do -- for valid data we write it to disk
			addFileToManifest printFile canpath path (rep,md) man
		else do
			isFile <- doesFileExistInTree path tree 
			if isFile -- if the data is invalid and a file exists on disk, we remove it, otherwise we don't change anything
				then return $ removeFileFromManifest canpath path man
				else return man

doManifestFile1 :: (Eq pads,Eq md,FSRep fs,Pads1 arg pads md) => arg -> FSTree fs -> (ForestFSThunkI fs pads,ForestFSThunkI fs (Forest_md fs,ForestFSThunkI fs md)) -> Manifest fs -> ForestO fs (Manifest fs)
doManifestFile1 arg tree (rep_t,md_t) man = do
	rep <- inside $ fsforce rep_t
	(fmd,md_t') <- inside $ fsforce md_t
	md <- inside $ fsforce md_t'
	let path = fullpath $ fileInfo fmd
	dskpath <- canonalizePathInTree path tree
	canpath <- pathFromTree dskpath tree
	valid <- isValidMD fmd
	if valid
		then do -- for valid data we write it to disk
			addFileToManifest (printFile1 arg) canpath path (rep,md) man
		else do
			isFile <- doesFileExistInTree path tree 
			if isFile -- if the data is invalid and a file exists on disk, we remove it, otherwise we don't change anything
				then return $ removeFileFromManifest canpath path man
				else return man

-- users may have arbitrarily changed the data, so we can't trust that the thunk still computes the correct predicate
doManifestConstraint :: FSRep fs => FSTree fs -> ((rep,md) -> ForestI fs Bool) -> (rep,(md,ForestICThunkI fs Bool))
	-> ((rep,md) -> Manifest fs -> ForestO fs (Manifest fs))
	-> Manifest fs -> ForestO fs (Manifest fs)
doManifestConstraint tree pred (rep,(md,pred_t)) manifestContent man = do
	let testm = do
		oldb <- inside $ force pred_t
		newb <- inside $ pred (rep,md)
		return $ boolStatus "Predicate validation mismatch" (oldb == newb)
	let man1 = addTestToManifest testm man
	manifestContent (rep,md) man1

doManifestDirectory :: (Eq rep,Eq md,ForestMD fs md,FSRep fs) => 
	FSTree fs -> (md -> ForestI fs Forest_err)
	-> (ForestFSThunkI fs rep,ForestFSThunkI fs (Forest_md fs,md))
	-> ((rep,md) -> Manifest fs -> ForestO fs (Manifest fs))
	-> Manifest fs -> ForestO fs (Manifest fs)
doManifestDirectory tree collectMDErrors (rep_t,md_t) manifestContent man = do
	rep <- inside $ fsforce rep_t
	(fmd,md) <- inside $ fsforce md_t
	let path = fullpath $ fileInfo fmd
	dskpath <- canonalizePathInTree path tree
	canpath <- pathFromTree dskpath tree
	let man1 = addDirToManifest canpath path man
	let testm = liftM (boolStatus "inconsistent Directory: top-level and inner metadatas have different validity") $ sameValidity fmd md
	let man2 = addTestToManifest testm man1 -- errors in the metadata must be consistent
	manifestContent (rep,md) man2

doManifestMaybe :: (Eq md,Eq rep,Forest fs args rep md,FSRep fs) =>
	FSTree fs
	-> (ForestFSThunkI fs (Maybe rep),ForestFSThunkI fs (Forest_md fs,Maybe md))
	-> ((rep,md) -> Manifest fs -> ForestO fs (Manifest fs))
	-> Manifest fs -> ForestO fs (Manifest fs)
doManifestMaybe tree (rep_t,md_t) manifestContent man = do
	rep_mb <- inside $ fsforce rep_t
	(fmd,md_mb) <- inside $ fsforce md_t
	case (rep_mb,md_mb) of
		(Just rep,Just md) -> do
			let testm = do
				status1 <- liftM (boolStatus "inconsistent Maybe: top-level and inner metadatas have different validity") $ sameValidity fmd md
				-- the file will be stored recursively, so we just need to guarantee that filepaths match
				status2 <- liftM (boolStatus "inconsistentMaybe: top-level and inner medatadas have different paths") $ sameCanonicalFullPathInTree fmd md tree
				return $ status1 `mappend` status2
			let man1 = addTestToManifest testm man
			manifestContent (rep,md) man1
		(Nothing,Nothing) -> do
			let path = fullpath $ fileInfo fmd
			dskpath <- canonalizePathInTree path tree
			canpath <- pathFromTree dskpath tree
			let testm = liftM (boolStatus "Nothing value contains invalid metadata") $ isValidMD fmd
			let man1 = addTestToManifest testm man
			return $ removeFileFromManifest canpath path man1
		(Just rep,Nothing) -> do 
			md <- inside $ defaultMd rep "" --XXX: can we provide a better filepath?
			let testm = return (Invalid "inconsistent Maybe values: missing metadata") -- always invalid
			let man1 = addTestToManifest testm man
			manifestContent (rep,md) man1
		(Nothing,Just md) -> do 
			let path = fullpath $ fileInfo fmd
			dskpath <- canonalizePathInTree path tree
			canpath <- pathFromTree dskpath tree
			let testm = return (Invalid $ "inconsnstent Maybe values: missing data") -- always invalid
			let man1 = addTestToManifest testm man
			return $ removeFileFromManifest canpath path man1

