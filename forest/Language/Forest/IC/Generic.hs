{-# LANGUAGE TupleSections, OverlappingInstances, StandaloneDeriving, GADTs, ConstraintKinds, TemplateHaskell, DeriveDataTypeable, UndecidableInstances, TypeOperators, TypeFamilies, DataKinds, KindSignatures, MultiParamTypeClasses, FunctionalDependencies, ScopedTypeVariables, FlexibleContexts, FlexibleInstances, NamedFieldPuns #-}
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

import Language.Pads.Padsc hiding (gmapT)
import Prelude hiding (mod)
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
import Data.WithClass.Derive.DeepTypeable
import Data.WithClass.MGenerics
import Data.IORef
import Language.Forest.IC.MetaData
import Data.DeepTypeable
import Language.Haskell.TH.Syntax
import Control.Exception

type FTM fs = ForestO fs
type FTV fs a = ForestFSThunkI fs a

type FTK fs args rep content = (Eq content,Typeable content,Typeable (ForestIs TxVarFS args),Typeable rep,Eq rep,ZippedICForest fs args rep,ForestRep rep (FTV fs content))

class TxICForest fs where

	atomically :: FTM fs b -> IO b
	
	retry :: FTM fs a
	
	orElse :: FTM fs a -> FTM fs a -> FTM fs a
	
	throw :: Exception e => e -> FTM fs a
	catch :: Exception e => FTM fs a -> (e -> FTM fs a) -> FTM fs a
	
	new :: FTK fs args rep content => args -> FilePath -> FTM fs rep
	
	read :: FTK fs args rep content => rep -> FTM fs content
	
	-- tries to modify a variable
	-- the write only occurs if validation succeeds
	-- if the new value is not a consistent view of the FS, an alternative action is run otherwise
	writeOrElse :: FTK fs args rep content => rep -> content -> b -> ([Message] -> FTM fs b) -> FTM fs b
	
tryWrite :: (TxICForest fs,FTK fs args rep content) => rep -> content -> FTM fs ()
tryWrite t v = writeOrElse t v () (Prelude.const $ return ())
	
writeOrRetry :: (TxICForest fs,FTK fs args rep content) => rep -> content -> b -> FTM fs b
writeOrRetry t v b = writeOrElse t v b (Prelude.const retry)

writeOrShow :: (TxICForest fs,FTK fs args rep content) => rep -> content -> FTM fs String
writeOrShow t v = writeOrElse t v "" (return . show)

-- * Zipped Incremental Forest interface

class (ICRep fs,ZippedICMemo fs,ForestArgs fs args,MData NoCtx (ForestO fs) rep) => ZippedICForest fs args rep | rep -> args  where
	
	zload :: (ForestRep rep (ForestFSThunkI fs content)) => ForestIs fs args -> FilePath -> ForestI fs rep
	zload args path = forestM latestTree >>= \tree -> zloadScratchMemo Proxy args return path tree getForestMDInTree
	
	zloadScratch :: Proxy args -> ForestIs fs args -> FilePathFilter fs -> FilePath -> FSTree fs -> GetForestMD fs -> ForestI fs rep
	
	zloadScratchMemo :: (ForestRep rep (ForestFSThunkI fs content)) => Proxy args -> ForestIs fs args -> FilePathFilter fs -> FilePath -> FSTree fs -> GetForestMD fs -> ForestI fs rep
	zloadScratchMemo proxy args pathfilter path tree getMD = do
		let fs = Proxy :: Proxy fs
		let proxyRep = Proxy
		oldpath <- pathfilter path
		mb <- findZippedMemo proxy path proxyRep
		case mb of
			Just (memo_tree,memo_args,memo_rep) -> do
				df <- forestM $ diffFS memo_tree tree path
				dv <- diffValue memo_tree memo_rep
				let deltas = (args,deltaArgs fs proxy)
				-- XXX: how safe is this?
				unsafeWorld $ zloadDelta proxy deltas (return oldpath) memo_tree (memo_rep,getMD) path df tree dv
				unless (oldpath==path) $ remZippedMemo fs oldpath proxyRep
				addZippedMemo path proxy args memo_rep tree
				return memo_rep
			Nothing -> zloadScratch proxy args pathfilter path tree getMD
	
	zloadDelta :: Proxy args -> LoadDeltaArgs ICData fs args -> ForestI fs FilePath -> FSTree fs -> (rep,GetForestMD fs) -> FilePath -> FSTreeDeltaNodeMay -> FSTree fs -> ValueDelta fs rep -> ForestO fs (SValueDelta rep)
	
	zupdateManifestScratch :: ForestIs fs args -> FSTree fs -> rep -> Manifest fs -> ForestO fs (Manifest fs)
	
	zupdateManifestDelta :: Proxy args -> LoadDeltaArgs ICData fs args -> FSTree fs -> rep -> FSTreeDeltaNodeMay -> FSTree fs -> ValueDelta fs rep -> Manifest fs -> ForestO fs (Manifest fs)
	
	zmanifest :: ForestIs fs args -> rep -> ForestO fs (Manifest fs)
	zmanifest = zmanifest' Proxy
	
	zmanifest' :: Proxy args -> ForestIs fs args -> rep -> ForestO fs (Manifest fs)
	zmanifest' proxy args rep = forestM latestTree >>= \tree -> forestM (newManifestWith "/" tree) >>= zupdateManifestScratch args tree rep

-- * Incremental Forest interface

-- (root path,current tree)
type family LoadInfo (ic :: ICMode) (fs :: FS) (args :: *) :: * where
	LoadInfo ICExpr fs args = ForestFSThunk fs Inside (FilePath,FSTree fs) -- we store the root path to avoid forcing the metadata thunk
	LoadInfo ICData fs args = (ForestICThunksI fs args,ForestFSThunk fs Inside (FilePath,FSTree fs))

-- we follow a closed-world assumption: the computations of the top-level arguments of a specification cannot be modified; nevertheless, they may depend on modifiables and their values are updated accordingly.

-- * Automatically generated "not to be seen" Forest class

-- A Class for the types of Forest specifications
class (Typeable rep,Typeable md,ICMemo fs,ForestArgs fs args,MData NoCtx (ForestO fs) rep,ForestMD fs md) => ICForest (mode :: ICMode) fs args rep md | mode rep -> md, rep -> args  where
	
	load :: LiftedICMode mode -> ForestIs fs args -> FilePath -> ForestO fs (rep,md)
	load mode args path = liftM fst $ loadFwd mode args path
	
	-- loads a specification at the most recent FS snapshot
	loadFwd :: LiftedICMode mode -> ForestIs fs args -> FilePath -> ForestO fs ((rep,md),LoadInfo mode fs args)
	loadFwd mode args path = forestM latestTree >>= \t -> loadTree mode t args path
	
	-- loads a specification at a given FS snapshot
	loadTree :: LiftedICMode mode -> FSTree fs -> ForestIs fs args -> FilePath -> ForestO fs ((rep,md),LoadInfo mode fs args)
	loadTree mode tree margs path = do undefined
--		(rep,md) <- inside $ loadScratch mode Proxy margs path tree Nothing tree getForestMDInTree -- for batch loading we use the same tree and assume that nothing changed
--		loadInfo <- inside $ ref (path,tree)
--		return ((rep,md),loadInfo)
	
	-- incrementally reloads a specification given older data and the most recent FS snapshot
	reload :: LiftedICMode mode -> FilePath -> ((rep,md),LoadInfo mode fs args) -> ForestO fs ()
	reload mode path info = forestM latestTree >>= \t -> reloadTree mode t Proxy path info
	
	-- incrementally reloads a specification given older data and a newer FS snapshot
	reloadTree :: LiftedICMode mode -> FSTree fs -> Proxy args -> FilePath -> ((rep,md),LoadInfo mode fs args) -> ForestO fs ()
	reloadTree mode newTree proxy path ((rep,md),loadInfo) = debug ("reloading") $ do undefined
--		(originalPath,originalTree) <- inside $ get loadInfo
--		treeDelta <- debug ("loadedInfo") $ changesBetween originalTree newTree -- assume that the @FSTreeDelta@ is absolute
--		let relTreeDelta = focusFSTreeDeltaByRelativePathMay treeDelta path
--		let dargs = (deltaArgs (Proxy::Proxy fs) proxy) -- we assume that top-level arguments always change, since we don't track their modifications
--		(drep,dmd) <- debug ("initial FSDelta: "++ show path ++" --> "++ show relTreeDelta) $ loadDelta mode proxy dargs (return originalPath) originalTree ((rep,md),getForestMDInTree) path relTreeDelta newTree -- we can discard the top-level deltas
--		set loadInfo (path,newTree)
	
	
	-- batch non-incremental load (takes a a function on FilePaths to handle moves whenever the old and new values are not in sync)
	loadScratch :: LiftedICMode mode -> Proxy args -> ForestIs fs args -> FilePathFilter fs -> FilePath -> FSTree fs -> GetForestMD fs -> ForestI fs (rep,md)
	
	loadScratchMemo :: (Typeable (ForestLs fs Inside args)) =>
		LiftedICMode mode -> Proxy args -> ForestIs fs args -> FilePathFilter fs -> FilePath -> FSTree fs -> GetForestMD fs -> ForestI fs (rep,md)
	loadScratchMemo mode proxy args oldpathM path (tree :: FSTree fs) getMD = do
		let fs = Proxy :: Proxy fs
		let proxyRep = Proxy
		oldpath <- oldpathM path
		mb <- findMemo proxy path proxyRep
		case mb of
			Just (memo_tree,memo_args,memo_rep,memo_md) -> do
				df <- forestM $ diffFS memo_tree tree path
				let deltas = mkLoadDeltaArgs mode fs proxy args (deltaArgs fs proxy)
				-- XXX: how safe is this?
				unsafeWorld $ loadDelta mode proxy deltas (return oldpath) memo_tree ((memo_rep,memo_md),getMD) path df tree
				unless (oldpath==path) $ remMemo fs oldpath proxyRep
				addMemo path proxy args (memo_rep,memo_md) tree
				return (memo_rep,memo_md)
			Nothing -> loadScratch mode proxy args oldpathM path tree getMD
	
	-- | incremental load function that considers the original source data and returns a stable delta
	-- expects the old data to be consistent with the old path and the old tree
	-- invariant: the FSTreeDelta is always relative to the updated root filepath
	-- the original root path is given as a computation (possibly over the original data), so that we can delay its evaluation
	loadDelta :: LiftedICMode mode -> Proxy args -> LoadDeltaArgs mode fs args -> ForestI fs FilePath -> FSTree fs -> OldData fs rep md -> FilePath -> FSTreeDeltaNodeMay -> FSTree fs -> ForestO fs (SValueDelta rep,SValueDelta md)

	-- | Writes the data to a private Forest on-disk location and generates a manifest file
	generateManifestScratch :: LiftedICMode mode -> ForestIs fs args -> FSTree fs -> (rep,md) -> ForestO fs (Manifest fs)
	generateManifestScratch mode args tree dta = forestM (newManifestWith "/" tree) >>= updateManifestScratch mode args tree dta
	
	updateManifestScratch :: LiftedICMode mode -> ForestIs fs args -> FSTree fs -> (rep,md) -> Manifest fs -> ForestO fs (Manifest fs)

	-- | generates default metadata based on the specification
	defaultMd :: ForestIs fs args -> rep -> FilePath -> ForestI fs md



-- | Class to support variadic Forest functions
class (FSRep fs,Typeable args,Typeable (ForestIs fs args)) => ForestArgs fs args where
	newArgs :: Proxy fs -> Proxy args -> ForestIs fs args -> ForestI fs (ForestICThunksI fs args)
	deltaArgs :: Proxy fs -> Proxy args -> SValueDeltas (ForestICThunksI fs args)
	andSValueDeltas :: Proxy fs -> Proxy args -> SValueDeltas (ForestICThunksI fs args) -> SValueDelta (ForestICThunksI fs args)
	checkArgs :: Proxy fs -> Proxy args -> ForestIs fs args -> ForestICThunksI fs args -> ForestO fs Status
	monadArgs :: Proxy fs -> args -> ForestIs fs args
	
instance ICRep fs => ForestArgs fs () where
	newArgs fs args () = return ()
	deltaArgs fs args = ()
	andSValueDeltas fs args () = Id
	checkArgs _ _ _ _ = return Valid
	monadArgs _ () = ()
	
instance (Typeable a,Eq a,ICRep fs) => ForestArgs fs (Arg a) where
	newArgs fs args m = thunk m
	deltaArgs fs args = Delta
	andSValueDeltas fs args d = d
	checkArgs _ _ marg targ = do
		arg <- inside $ marg
		arg' <- inside $ force targ
		return $ boolStatus "top-level argument mismatch" (arg == arg')
	monadArgs fs (Arg arg) = return arg
	
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
	monadArgs fs (arg1 :*: arg2) = monadArgs fs arg1 :*: monadArgs fs arg2

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

type family LoadDeltaArgs (mode :: ICMode) (fs :: FS) args :: * where
	LoadDeltaArgs ICData fs args = (ForestIs fs args,SValueDeltas (ForestICThunksI fs args)) -- they are not in fact stable deltas, but we store the new computations separately
	LoadDeltaArgs ICExpr fs args = SValueDeltas (ForestICThunksI fs args)

mkEmptyLoadDeltaArgs :: LiftedICMode mode -> Proxy fs -> LoadDeltaArgs mode fs ()
mkEmptyLoadDeltaArgs LiftedICData fs = ((),())
mkEmptyLoadDeltaArgs LiftedICExpr fs = ()

mkLoadDeltaArgs :: LiftedICMode mode -> Proxy fs -> Proxy args -> ForestIs fs args -> SValueDeltas (ForestICThunksI fs args) -> LoadDeltaArgs mode fs args
mkLoadDeltaArgs LiftedICData fs args margs dargs = (margs,dargs)
mkLoadDeltaArgs LiftedICExpr fs args margs dargs = dargs

getLoadDeltaArgs :: LiftedICMode mode -> Proxy fs -> Proxy args -> LoadDeltaArgs mode fs args -> SValueDeltas (ForestICThunksI fs args)
getLoadDeltaArgs LiftedICData fs args (margs,dargs) = dargs
getLoadDeltaArgs LiftedICExpr fs args dargs = dargs

type family MDArgs (mode :: ICMode) md args :: * where
	MDArgs ICData md args = md
	MDArgs ICExpr md args = (md,args)

data LiftedICMode (mode :: ICMode) where
	LiftedICData :: LiftedICMode ICData
	LiftedICExpr :: LiftedICMode ICExpr

mkMDArgs :: LiftedICMode mode -> md -> args -> MDArgs mode md args
mkMDArgs LiftedICData md args = md
mkMDArgs LiftedICExpr md args = (md,args)

patMDArgs :: Monad m => LiftedICMode mode -> MDArgs mode md args -> (md -> m args) -> m (md,args)
patMDArgs LiftedICData md fargs = liftM (md,) (fargs md)
patMDArgs LiftedICExpr imd fargs = return imd

-- whether only forest data representations are incremental or even the expressions whitin a forest specifications are incrementally replayed
data ICMode = ICData | ICExpr deriving Typeable
deriving instance Typeable ICData
deriving instance Typeable ICExpr
$( derive makeDeepTypeable ''ICMode )

instance DeepTypeable ICData where
	typeTree (_::Proxy ICData) = MkTypeTree (mkName "Language.Forest.IC.Generic.ICData") [] []
instance DeepTypeable ICExpr where
	typeTree (_::Proxy ICExpr) = MkTypeTree (mkName "Language.Forest.IC.Generic.ICExpr") [] []

-- * strictly copying @FSThunk@s

class (FSRep fs,ForestLayer fs l) => CopyFSThunks fs l a where
	copyFSThunks :: Proxy fs -> Proxy l -> (FilePath -> FilePath) -> a -> ForestL fs l a

copyFSThunksProxy :: Proxy fs -> Proxy l -> Proxy (CopyFSThunksDict fs l)
copyFSThunksProxy fs l = Proxy

data CopyFSThunksDict fs l a = CopyFSThunksDict { copyFSThunksDict :: Proxy fs -> Proxy l -> (FilePath -> FilePath) -> a -> ForestL fs l a }

instance (CopyFSThunks fs l a) => Sat (CopyFSThunksDict fs l a) where
	dict = CopyFSThunksDict { copyFSThunksDict = copyFSThunks }

-- we make a strict copy by forcing the original thunk
instance (Typeable a,ForestLayer fs l,Eq a,ForestInput fs FSThunk l) => CopyFSThunks fs l (ForestFSThunk fs l a) where
	copyFSThunks _ _ f t = get t >>= ref 

-- change fullpaths
instance (ForestInput fs FSThunk Inside,ForestLayer fs l) => CopyFSThunks fs l (Forest_md fs) where
	copyFSThunks _ _ f fmd = do
		errors' <- inside $ get (errors fmd) >>= ref
		let fileInfo' = (fileInfo fmd) { fullpath = f (fullpath $ fileInfo fmd) }
		return $ Forest_md errors' fileInfo'

-- just traverse recursively, until there are no more @FSThunks@ inside the type
instance (ForestLayer fs l,MData (CopyFSThunksDict fs l) (ForestL fs l) a) => CopyFSThunks fs l a where
	 copyFSThunks fs l f x = do
		let hasFSThunk (MkTypeTree name _ _) = showName name == "Language.Forest.FS.FSRep.FSThunk"
		if hasDeepTypeable hasFSThunk (proxyOf x)
			then gmapT (copyFSThunksProxy fs l) (copyFSThunksDict dict fs l f) x
			else return x



