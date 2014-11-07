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


module Language.Forest.IC.Generic   where

import Data.Monoid
import Data.WithClass.MData
import Language.Pads.Generic
import Control.Monad.Incremental
import Language.Forest.Pure.MetaData (FileInfo(..),FileType(..),Arg(..),(:*:)(..))
import qualified Language.Forest.Pure.MetaData as Pure
import Language.Forest.Manifest
import Language.Forest.IC.FS.FSDelta
import System.FilePath.Posix
import Data.Map hiding (map)
import Data.Set hiding (map)
import qualified Data.List as L
import Language.Forest.FS.FSRep
import Language.Forest.IC.ICRep
import Data.Proxy
import Control.Monad
import Language.Forest.IC.ValueDelta
import Data.DeriveTH                 
import Data.WithClass.Derive.MData
import Data.WithClass.MGenerics
import Data.IORef
import Language.Forest.IC.MetaData


-- * Incremental Forest interface

-- (root path,current tree)
type LoadInfo fs = ForestFSThunk fs Inside (FilePath,FSTree fs) -- the args don't need to be remembered because in the current impl they are already being stored together with the md. we store the root path to avoid forcing the metadata thunk

-- we follow a closed-world assumption: the computations of the top-level arguments of a specification cannot be modified; nevertheless, they may depend on modifiables and their values are updated accordingly.

-- * Automatically generated "not to be seen" Forest class

-- A Class for the types of Forest specifications
class (ForestArgs fs args,MData NoCtx (ForestO fs) rep,ForestMD fs md) => ICForest fs args rep md | rep -> md, rep -> args  where
	
	-- loads a specification at the most recent FS snapshot
	load :: ForestIs fs args -> FilePath -> ForestO fs ((rep,md),LoadInfo fs)
	load args path = forestM latestTree >>= \t -> loadTree t args path
	
	-- loads a specification at a given FS snapshot
	loadTree :: FSTree fs -> ForestIs fs args -> FilePath -> ForestO fs ((rep,md),LoadInfo fs)
	loadTree tree margs path = do
		(rep,md) <- inside $ loadScratch margs path tree Nothing tree getForestMDInTree -- for batch loading we use the same tree and assume that nothing changed
		loadInfo <- inside $ ref (path,tree)
		return ((rep,md),loadInfo)
	
	-- incrementally reloads a specification given older data and the most recent FS snapshot
	reload :: FilePath -> ((rep,md),LoadInfo fs) -> ForestO fs ()
	reload path info = forestM latestTree >>= \t -> reloadTree t Proxy path info
	
	-- incrementally reloads a specification given older data and a newer FS snapshot
	reloadTree :: FSTree fs -> Proxy args -> FilePath -> ((rep,md),LoadInfo fs) -> ForestO fs ()
	reloadTree newTree proxy path ((rep,md),loadInfo) = debug ("reloading") $ do
		(originalPath,originalTree) <- inside $ get loadInfo
		treeDelta <- debug ("loadedInfo") $ changesBetween originalTree newTree -- assume that the @FSTreeDelta@ is absolute
		let relTreeDelta = focusFSTreeDeltaByRelativePathMay treeDelta path
		let dargs = (deltaArgs (Proxy::Proxy fs) proxy) -- we assume that top-level arguments always change, since we don't track their modifications
		(drep,dmd) <- debug ("initial FSDelta: "++ show path ++" --> "++ show relTreeDelta) $ loadDelta proxy dargs (return originalPath) originalTree ((rep,md),getForestMDInTree) path relTreeDelta newTree -- we can discard the top-level deltas
		set loadInfo (path,newTree)
	
	
	-- batch non-incremental load (takes an original tree and a delta to handle moves whenever the old and new values are not in sync)
	loadScratch :: ForestIs fs args -> FilePath -> FSTree fs -> FSTreeDeltaNodeMay -> FSTree fs -> GetForestMD fs -> ForestI fs (rep,md)
	
	-- | incremental load function that considers the original source data and returns a stable delta
	-- expects the old data to be consistent with the old path and the old tree
	-- invariant: the FSTreeDelta is always relative to the updated root filepath
	-- the original root path is given as a computation (possibly over the original data), so that we can delay its evaluation
	loadDelta :: Proxy args -> SValueDeltas (ForestICThunksI fs args) -> ForestI fs FilePath -> FSTree fs -> OldData fs rep md -> FilePath -> FSTreeDeltaNodeMay -> FSTree fs -> ForestO fs (SValueDelta rep,SValueDelta md)

	-- | Writes the data to a private Forest on-disk location and generates a manifest file
	generateManifestScratch :: ForestIs fs args -> FSTree fs -> (rep,md) -> ForestO fs (Manifest fs)
	generateManifestScratch args tree dta = forestM (newManifestWith "/" tree) >>= updateManifestScratch args tree dta
	
	updateManifestScratch :: ForestIs fs args -> FSTree fs -> (rep,md) -> Manifest fs -> ForestO fs (Manifest fs)

	-- | generates default metadata based on the specification
	defaultMd :: ForestIs fs args -> rep -> FilePath -> ForestI fs md

-- | Class to support variadic Forest functions
class FSRep fs => ForestArgs fs args where
	newArgs :: Proxy fs -> Proxy args -> ForestIs fs args -> ForestI fs (ForestICThunksI fs args)
	deltaArgs :: Proxy fs -> Proxy args -> SValueDeltas (ForestICThunksI fs args)
	andSValueDeltas :: Proxy fs -> Proxy args -> SValueDeltas (ForestICThunksI fs args) -> SValueDelta (ForestICThunksI fs args)
	checkArgs :: Proxy fs -> Proxy args -> ForestIs fs args -> ForestICThunksI fs args -> ForestO fs Status
	
instance ICRep fs => ForestArgs fs () where
	newArgs fs args () = return ()
	deltaArgs fs args = ()
	andSValueDeltas fs args () = Id
	checkArgs _ _ _ _ = return Valid
	
instance (Eq a,ICRep fs) => ForestArgs fs (Arg a) where
	newArgs fs args m = thunk m
	deltaArgs fs args = Delta
	andSValueDeltas fs args d = d
	checkArgs _ _ marg targ = do
		arg <- inside $ marg
		arg' <- inside $ force targ
		return $ boolStatus "top-level argument mismatch" (arg == arg')
	
instance (ICRep fs,ForestArgs fs a,ForestArgs fs b) => ForestArgs (fs :: FS) (a :*: b) where
	newArgs fs (args :: Proxy (a1 :*: a2)) (m1 :*: m2) = do
		t1 <- newArgs fs (Proxy :: Proxy a1) m1
		t2 <- newArgs fs (Proxy :: Proxy a2) m2
		return (t1 :*: t2)
	deltaArgs fs (args :: Proxy (a1 :*: a2)) = (deltaArgs fs (Proxy :: Proxy a1) :*: deltaArgs fs (Proxy :: Proxy a2))
	andSValueDeltas fs (args :: Proxy (a1 :*: a2)) (d1 :*: d2) = andSValueDeltas fs (Proxy :: Proxy a1) d1 `timesSValueDelta` andSValueDeltas fs (Proxy :: Proxy a2) d2
	checkArgs fs (args :: Proxy (a1 :*: a2)) (marg1 :*: marg2) (targ1 :*: targ2) = do
		status1 <- checkArgs fs (Proxy :: Proxy a1) marg1 targ1
		status2 <- checkArgs fs (Proxy :: Proxy a2) marg2 targ2
		return $ status1 `mappend` status2
	
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

type OldData fs rep md = ((rep,md),GetForestMD fs)


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

