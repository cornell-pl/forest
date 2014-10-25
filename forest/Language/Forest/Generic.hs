{-# LANGUAGE GADTs, ConstraintKinds, TemplateHaskell, DeriveDataTypeable, UndecidableInstances, TypeOperators, TypeFamilies, DataKinds, KindSignatures, MultiParamTypeClasses, FunctionalDependencies, ScopedTypeVariables, FlexibleContexts, FlexibleInstances, NamedFieldPuns #-}
{-
** *********************************************************************
*                                                                      *
*              This software is part of the pads package               *
*           Copyright (c) 2005-2011 AT&T Knowledge Ventures            *
*                      and is licensed under the                       *
*                        Common Public License                         *
*                      by AT&T Knowledge Ventures                      *
*                                                                      *
*                A copy of the License is available at                 *
*                    www.padsproj.org/License.html                     *
*                                                                      *
*  This program contains certain software code or other information    *
*  ("AT&T Software") proprietary to AT&T Corp. ("AT&T").  The AT&T     *
*  Software is provided to you "AS IS". YOU ASSUME TOTAL RESPONSIBILITY*
*  AND RISK FOR USE OF THE AT&T SOFTWARE. AT&T DOES NOT MAKE, AND      *
*  EXPRESSLY DISCLAIMS, ANY EXPRESS OR IMPLIED WARRANTIES OF ANY KIND  *
*  WHATSOEVER, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF*
*  MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE, WARRANTIES OF  *
*  TITLE OR NON-INFRINGEMENT.  (c) AT&T Corp.  All rights              *
*  reserved.  AT&T is a registered trademark of AT&T Corp.             *
*                                                                      *
*                   Network Services Research Center                   *
*                          AT&T Labs Research                          *
*                           Florham Park NJ                            *
*                                                                      *
*              Kathleen Fisher <kfisher@research.att.com>              *
*                                                                      *
************************************************************************
-}


module Language.Forest.Generic   where

import Data.WithClass.MData
import Language.Pads.Generic
import Control.Monad.Incremental
import Language.Forest.MetaData
import Language.Forest.Manifest
import Language.Forest.FS.FSDelta
import System.FilePath.Posix
import Data.Map hiding (map)
import Data.Set hiding (map)
import qualified Data.List as L
import Language.Forest.FS.FSRep
import Data.Proxy
import Control.Monad
import Language.Forest.ValueDelta
import Data.DeriveTH                 
import Data.WithClass.Derive.MData
import Data.Derive.MDataWithClassForest
import Data.WithClass.MGenerics
import Data.IORef
--import Control.Monad.IO.Class

-- * Transactional Forest interface

-- | The Forest transaction monad
-- stores for example:
-- 1) the FSTree at which the transaction started + a log of *pending* modifications on the original FSTree
-- 2) the original environment + a log of *pending* modification on the environment modifiables
type TFS fs = ForestO fs

class (FSRep fs) => TransactionalForest fs where
	
	-- if we allow incremental data reuse among transactions, we can't allow Forest data to escape the monad
	atomically :: Pure a => Transaction fs a -> IO a
	
	atomically_ :: Transaction fs () -> IO ()
	atomically_ = atomically

-- | A constraint to distinguish Forest data that can't escape transactions from regular data
type Pure a = Purity a ~ TRUE

-- | A type-level predicate to determine purity
type family Purity a :: BOOL
type instance Purity () = TRUE
-- TODO: define the remaining instances... and an automatic generator

-- type-level booleans
data BOOL = TRUE | FALSE

-- for transaction repair we will need a DSL for transactions, so that we can inspect their structure and replace load/store operations by incremental variants
data Transaction fs a where
	Load :: Forest fs args rep md => ForestIs fs args -> FilePath -> Transaction fs (rep,md)
	Manifest :: Forest fs args rep md => (rep,md) -> Transaction fs Manifest
	Store :: FSRep fs => Manifest -> Transaction fs ()
	Lift :: FSRep fs => ForestO fs a -> Transaction fs a -- we can mutate values inside transactions; Forest monads shall not allow arbitrary, just interacting with thunks in the same way as @TVar@s
	Bind :: Transaction fs a -> (a -> Transaction fs b) -> Transaction fs b

-- * Incremental Forest interface

-- (root path,current tree)
type LoadInfo fs = ForestFSThunk fs Inside (FilePath,FSTree fs) -- the args don't need to be remembered because in the current impl they are already being stored together with the md. we store the root path to avoid forcing the metadata thunk

-- we follow a closed-world assumption: the computations of the top-level arguments of a specification cannot be modified; nevertheless, they may depend on modifiables and their values are updated accordingly.
class (FSRep fs) => IncrementalForest fs args rep md | rep -> md, rep -> args where
	
	-- loads a specification at the most recent FS snapshot
	load :: ForestIs fs args -> FilePath -> ForestO fs ((rep,md),LoadInfo fs)
	load args path = latestTree >>= \t -> loadTree t args path
	
	-- loads a specification at a given FS snapshot
	loadTree :: FSTree fs -> ForestIs fs args -> FilePath -> ForestO fs ((rep,md),LoadInfo fs)
	
	-- incrementally reloads a specification given older data and the most recent FS snapshot
	reload :: FilePath -> ((rep,md),LoadInfo fs) -> ForestO fs ()
	reload path info = latestTree >>= \t -> reloadTree t Proxy path info
	
	-- incrementally reloads a specification given older data and a newer FS snapshot
	reloadTree :: FSTree fs -> Proxy args -> FilePath -> ((rep,md),LoadInfo fs) -> ForestO fs ()

instance (Forest fs args rep md) => IncrementalForest fs args rep md where
	
	loadTree tree margs path = do
		(rep,md) <- inside $ loadNoDelta margs path tree Nothing tree getForestMDInTree -- for batch loading we use the same tree and assume that nothing changed
		loadInfo <- inside $ fsref (path,tree)
		return ((rep,md),loadInfo)
	
	reloadTree newTree proxy path ((rep,md),loadInfo) = debug ("reloading") $ do
		(originalPath,originalTree) <- inside $ fsforce loadInfo
		treeDelta <- debug ("loadedInfo") $ changesBetween originalTree newTree -- assume that the @FSTreeDelta@ is absolute
		let relTreeDelta = focusFSTreeDeltaByRelativePathMay treeDelta path
		let dargs = (deltaArgs (Proxy::Proxy fs) proxy) -- we assume that top-level arguments always change
		(drep,dmd) <- debug ("initial FSDelta: "++ show path ++" --> "++ show relTreeDelta) $ loadDelta proxy dargs (return originalPath) originalTree ((rep,md),getForestMDInTree) path relTreeDelta newTree -- we can discard the top-level deltas
		fsset loadInfo (path,newTree)

-- * Automatically generated ugly, core, "not to be seen" Forest class

-- A Class for the types of Forest specifications
class (ForestArgs fs args,MData NoCtx (ForestO fs) rep,ForestMD fs md) => Forest fs args rep md | rep -> md, rep -> args  where
	
	-- batch non-incremental load (takes an original tree and a delta to handle moves whenever the old and new values are not in sync)
	loadNoDelta :: ForestIs fs args -> FilePath -> FSTree fs -> FSTreeDeltaNodeMay -> FSTree fs -> GetForestMD fs -> ForestI fs (rep,md)
	
	-- | incremental load function that considers the original source data and returns a stable delta
	-- expects the old data to be consistent with the old path and the old tree
	-- invariant: the FSTreeDelta is always relative to the updated root filepath
	-- the original root path is given as a computation (possibly over the original data), so that we can delay its evaluation
	loadDelta :: Proxy args -> SValueDeltas (ForestICThunksI fs args) -> ForestI fs FilePath -> FSTree fs -> OldData fs rep md -> FilePath -> FSTreeDeltaNodeMay -> FSTree fs -> ForestO fs (SValueDelta rep,SValueDelta md)

	-- | Writes the data to a private Forest on-disk location and generates a manifest file
	generateManifestNoDelta :: FSTree fs -> (rep,md) -> ForestO fs Manifest

--	updateManifest :: Proxy fs -> (rep,md) -> Manifest -> IO Manifest

-- | Class to support variadic Forest functions
class FSRep fs => ForestArgs fs args where
	newArgs :: Proxy fs -> Proxy args -> ForestIs fs args -> ForestI fs (ForestICThunksI fs args)
	deltaArgs :: Proxy fs -> Proxy args -> SValueDeltas (ForestICThunksI fs args)
	andSValueDeltas :: Proxy fs -> Proxy args -> SValueDeltas (ForestICThunksI fs args) -> SValueDelta (ForestICThunksI fs args)
	
instance FSRep fs => ForestArgs fs () where
	newArgs fs args () = return ()
	deltaArgs fs args = ()
	andSValueDeltas fs args () = Id
	
instance (Eq a,FSRep fs) => ForestArgs fs (Arg a) where
	newArgs fs args m = thunk m
	deltaArgs fs args = Delta
	andSValueDeltas fs args d = d
	
	
instance (ForestArgs fs a,ForestArgs fs b) => ForestArgs (fs :: FS) (a :*: b) where
	newArgs fs (args :: Proxy (a1 :*: a2)) (m1 :*: m2) = do
		t1 <- newArgs fs (Proxy :: Proxy a1) m1
		t2 <- newArgs fs (Proxy :: Proxy a2) m2
		return (t1 :*: t2)
	deltaArgs fs (args :: Proxy (a1 :*: a2)) = (deltaArgs fs (Proxy :: Proxy a1) :*: deltaArgs fs (Proxy :: Proxy a2))
	andSValueDeltas fs (args :: Proxy (a1 :*: a2)) (d1 :*: d2) = andSValueDeltas fs (Proxy :: Proxy a1) d1 `timesSValueDelta` andSValueDeltas fs (Proxy :: Proxy a2) d2
	
type family ForestICThunks (fs :: FS) l args :: * where
	ForestICThunks fs l (a :*: b) = (ForestICThunks fs l a :*: ForestICThunks fs l b)
	ForestICThunks fs l (Arg a) = ForestICThunk fs l a
	ForestICThunks fs l () = ()
type ForestICThunksI fs args = ForestICThunks fs Inside args
type ForestICThunksO fs args = ForestICThunks fs Outside args

type family ForestLs (fs :: FS) l args :: * where
	ForestLs fs l (a :*: b) = (ForestLs fs l a :*: ForestLs fs l b)
	ForestLs fs l (Arg a) = ForestL fs l a
	ForestLs fs l () = ()
type ForestOs fs args = ForestLs fs Outside args
type ForestIs fs args = ForestLs fs Inside args
type family ForestFSThunks (fs :: FS) l args :: * where
	ForestFSThunks fs l (a :*: b) = (ForestFSThunks fs l a :*: ForestFSThunks fs l b)
	ForestFSThunks fs l (Arg a) = ForestFSThunk fs l a
	ForestFSThunks fs l () = ()
type ForestFSThunksI fs args = ForestFSThunks fs Inside args
type ForestFSThunksO fs args = ForestFSThunks fs Outside args
type family SValueDeltas  args :: * where
	SValueDeltas (a :*: b) = (SValueDeltas a :*: SValueDeltas b)
	SValueDeltas (Arg a) = SValueDelta a
	SValueDeltas (ICThunk fs l inc r m a) = SValueDelta (ICThunk fs l inc r m a)
	SValueDeltas () = ()
--type family SValueDeltasL fs l args :: * where
--	SValueDeltasL fs l (a :*: b) = (SValueDeltasL fs l a :*: SValueDeltasL fs l b)
--	SValueDeltasL fs l (Arg a) = SValueDeltaL fs l a
--	SValueDeltasL fs l () = ()
--type SValueDeltasO fs args = SValueDeltasL fs Outside args
--type SValueDeltasI fs args = SValueDeltasL fs Inside args

type OldData fs rep md = ((rep,md),GetForestMD fs)
	
--	generateManifest1 :: arg -> Proxy fs -> (rep,md) -> IO Manifest
--	updateManifest1 :: arg -> Proxy fs -> (rep,md) -> Manifest -> IO Manifest

--validateLists :: (FilePath -> IO a) -> (a -> Manifest -> IO Manifest) -> Manifest -> IO Manifest
--validateLists  load updateMan (m @ Manifest {tempDir, pathToRoot, entries, count}) = do
--	listValidDir <- getTempForestListDirectory
--	storeManifestAt listValidDir m
--	let fileName = case pathToRoot of { Nothing -> error "pathToRoot should not be null." ; Just p -> takeFileName p}
--	let pathToDir = combine listValidDir fileName
--	print ("loading from: "++pathToDir)
--	testResult <- load pathToDir
--	emptyManifest <- newManifest
--	testManifest <- updateMan testResult emptyManifest
--	return (detectConflictsInListComps m testManifest)
--
--checkStore
--  :: (FilePath -> IO a)
--     -> (a -> Manifest -> IO Manifest)
--     -> FilePath
--     -> Manifest
--     -> IO [(FilePath, [FilePath])]
--checkStore load updateMan destDir writeManifest = do
--	res <- load destDir
--	emptyManifest <- newManifest
--	targetManifest <- updateMan res emptyManifest
--	return (diffManifest targetManifest writeManifest)

--rawManifest1 :: Forest1 arg fs rep md => arg -> Proxy fs -> (rep, md) -> IO Manifest
--rawManifest1 arg fs forest = do
--	origManifest <- newManifest
--	updateManifest1 arg fs forest origManifest

listDirs :: MData NoCtx m md => md -> m [FilePath] 
listDirs = liftM (map fullpath) . listify proxyNoCtx (\(r::FileInfo) -> return $ (kind r) `elem` [DirectoryK])

listFiles :: MData NoCtx m md => md -> m [FilePath] 
listFiles = liftM (map fullpath) . listify proxyNoCtx (\(r::FileInfo) -> return $ (kind r) `elem` [AsciiK, BinaryK])

findFiles :: MData NoCtx m md => md -> (FileInfo -> Bool) -> m [FilePath]
findFiles md pred = liftM (map fullpath) $ listify proxyNoCtx (return . pred) md

listNonEmptyFiles :: MData NoCtx m md => md -> m [FilePath] 
listNonEmptyFiles = liftM (L.filter (\s->s/= "")) . listFiles

listPaths :: MData NoCtx m md => md -> m [FilePath] 
listPaths = liftM (map fullpath) . listify proxyNoCtx (\(_::FileInfo) -> return True)

listNonEmptyPaths :: MData NoCtx m md => md -> m [FilePath] 
listNonEmptyPaths = liftM (map fullpath) . listify proxyNoCtx (\(r::FileInfo) -> return $ (fullpath r) /= "")

listInfoEmptyFiles :: MData NoCtx m a => a -> m [FileInfo]
listInfoEmptyFiles = listify proxyNoCtx $ \(r::FileInfo) -> return $ (fullpath r) == ""

listInfoNonEmptyFiles :: MData NoCtx m a => a -> m [FileInfo]
listInfoNonEmptyFiles = listify proxyNoCtx $ \(r::FileInfo) -> return $ (fullpath r) /= ""

listMDNonEmptyFiles :: (FSRep fs,MData NoCtx (ForestO fs) a) => a -> ForestO fs [Forest_md fs]
listMDNonEmptyFiles = listify proxyNoCtx (return . aux) where
	aux :: Forest_md fs -> Bool
	aux r = (fullpath $ fileInfo r) /= ""

mapInfoFiles :: MData NoCtx m a => a -> m (Map FilePath FileInfo)
mapInfoFiles md = do
  fileInfos <- listInfoNonEmptyFiles md
  let keyedInfos = map (\finfo -> (fullpath finfo, finfo)) fileInfos
  return $ Data.Map.fromList keyedInfos


