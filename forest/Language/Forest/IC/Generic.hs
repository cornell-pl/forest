{-# LANGUAGE ViewPatterns, TupleSections, OverlappingInstances, StandaloneDeriving, GADTs, ConstraintKinds, TemplateHaskell, DeriveDataTypeable, UndecidableInstances, TypeOperators, TypeFamilies, DataKinds, KindSignatures, MultiParamTypeClasses, FunctionalDependencies, ScopedTypeVariables, FlexibleContexts, FlexibleInstances, NamedFieldPuns #-}
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

import Control.Monad.RWS (RWS(..),RWST(..))
import qualified Control.Monad.RWS as RWS
import Control.Monad.Trans
import Control.Monad.Writer (Writer(..),WriterT(..))
import qualified Control.Monad.Writer as Writer
import Language.Forest.IC.Default
import Language.Pads.Padsc hiding (gmapT,lift,gmapQr)
import Prelude hiding (mod,read)
import Data.Monoid
import Data.WithClass.MData
import Language.Pads.Generic
import Control.Monad.Incremental hiding (read)
import Language.Forest.Pure.MetaData (FileInfo(..),FileType(..),Arg(..),(:*:)(..))
import qualified Language.Forest.Pure.MetaData as Pure
import Language.Forest.Manifest
import Language.Forest.FS.FSDelta
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
import Language.Haskell.TH.Syntax hiding (lift)
import Control.Exception (Exception(..))
import Language.Forest.Errors
import Language.Forest.IC.BX as BX
import Control.Monad.Incremental.Display

infoLens = (Lens fileInfo $ \fmd info' -> fmd { fileInfo = info'} ) 

-- hides @Forest_err@ values from the representation type of a variable
class ForestContent var content | var -> content where
	lens_content :: Lens var content
instance ForestContent rep c => ForestContent (Forest_md fs,rep) (FileInfo,c) where
	lens_content = prodLens infoLens lens_content
instance ForestContent rep c => ForestContent ((Forest_md fs,bmd),rep) ((FileInfo,bmd),c) where
	lens_content = prodLens (prodLens infoLens idLens) lens_content
instance ForestContent rep c => ForestContent (ForestFSThunkI fs Forest_err,rep) c where
	lens_content = sndLens `compLens` lens_content
instance ForestContent String String where
	lens_content = idLens
instance ForestContent (ForestFSThunkI fs rep) (ForestFSThunkI fs rep) where
	lens_content = idLens
instance ForestContent (Map a b) (Map a b) where
	lens_content = idLens
instance ForestContent (Set a) (Set a) where
	lens_content = idLens
instance ForestContent [a] [a] where
	lens_content = idLens
instance ForestContent (Maybe a) (Maybe a) where
	lens_content = idLens
instance ForestContent Text Text where
	lens_content = idLens

type FTM fs = ForestO fs
type FTV fs a = ForestFSThunkI fs a

type FTK fs args rep var content = (IncK (IncForest fs) var,IncK (IncForest fs) content,CopyFSThunks fs Outside rep,ZippedICForest fs args rep,ForestRep rep (FTV fs var),ForestContent var content)

class TxICForest fs where

	atomically :: FTM fs b -> IO b
	
	retry :: FTM fs a
	
	orElse :: FTM fs a -> FTM fs a -> FTM fs a
	
	throw :: Exception e => e -> FTM fs a
	catch :: Exception e => FTM fs a -> (e -> FTM fs a) -> FTM fs a
	
	new :: FTK fs args rep var content => ForestVs args -> FilePath -> FTM fs rep
	
	-- gets the arguments bound to a variable
	args :: FTK fs args rep var content => rep -> FTM fs (ForestVs args,FilePath)
	
	read :: (ForestLayer fs l,FTK fs args rep var content) => rep -> ForestL fs l content
	
	-- tries to modify a variable
	-- the write only occurs if validation succeeds
	-- if the new value is not a consistent view of the FS, an alternative action is run otherwise
	writeOrElse :: (Display Outside (IncForest fs) IORef IO rep,FTK fs args rep var content) => rep -> content -> b -> ([ManifestError] -> FTM fs b) -> FTM fs b
	
	-- read-only Forest error count
	validate :: (ForestLayer fs l,FTK fs args rep var content) => rep -> ForestL fs l Forest_err
	validate = get_errors
	
	-- * An attempt to mimic regular filesystem operations, but over Forest specifications
	
	-- recursively deletes a variable from the filesystem; should always succeed
	delete :: (Display Outside (IncForest fs) IORef IO rep,FTK fs args rep var content) => rep -> FTM fs ()
	-- recursively copies the content of a variable into another; it may fail if the copied data is not consistent with the arguments and filepath of the target variable
	copyOrElse :: (Display Outside (IncForest fs) IORef IO rep,FTK fs args rep var content) => rep -> rep -> b -> ([ManifestError] -> FTM fs b) -> FTM fs b
	
unsafeIOToFTM :: (TxICForest fs,ICRep fs) => IO a -> FTM fs a
unsafeIOToFTM = forestM . forestIO
	
tryWrite :: (Display Outside (IncForest fs) IORef IO rep,TxICForest fs,FTK fs args rep var content) => rep -> content -> FTM fs ()
tryWrite t v = writeOrElse t v () (Prelude.const $ return ())

writeOrRetry :: (Display Outside (IncForest fs) IORef IO rep,TxICForest fs,FTK fs args rep var content) => rep -> content -> b -> FTM fs b
writeOrRetry t v b = writeOrElse t v b (Prelude.const retry)

writeOrShow :: (Display Outside (IncForest fs) IORef IO rep,TxICForest fs,FTK fs args rep var content) => rep -> content -> FTM fs String
writeOrShow t v = writeOrElse t v "" (return . show)

writeOrThrow :: (Display Outside (IncForest fs) IORef IO rep,TxICForest fs,FTK fs args rep var content,Exception e) => rep -> content -> e -> FTM fs ()
writeOrThrow t v e = writeOrElse t v () (Prelude.const $ throw e)

-- * Data/Metadata only functions
-- we don't provide data/metadata only write functions because writeData >> writeMeta would not be the same as write, thus misleading.

readData :: (TxICForest fs,ForestLayer fs l,FTK fs args rep var (md_content,rep_content)) => rep -> ForestL fs l rep_content
readData = liftM snd . read
	
readMeta :: (TxICForest fs,ForestLayer fs l,FTK fs args rep var (md_content,rep_content)) => rep -> ForestL fs l md_content
readMeta = liftM fst . read

-- * Zipped Incremental Forest interface

class (ForestMD fs rep,IncK (IncForest fs) rep,Typeable (ForestIs fs args),IncK (IncForest fs) Forest_err,ICRep fs,ZippedICMemo fs,ForestArgs fs args,MData NoCtx (ForestO fs) rep) => ZippedICForest fs args rep | rep -> args  where
	
	zload :: (IncK (IncForest fs) content,Typeable content,ForestRep rep (ForestFSThunkI fs content)) => ForestIs fs args -> FilePath -> ForestI fs rep
	zload args path = forestM latestTree >>= \tree -> zloadScratchMemo Proxy args return path tree getForestMDInTree
	
	zloadScratch :: Proxy args -> ForestIs fs args -> FilePathFilter fs -> FilePath -> FSTree fs -> GetForestMD fs -> ForestI fs rep
	
	zloadScratchMemo :: (IncK (IncForest fs) content,Typeable content,ForestRep rep (ForestFSThunkI fs content)) => Proxy args -> ForestIs fs args -> FilePathFilter fs -> FilePath -> FSTree fs -> GetForestMD fs -> ForestI fs rep
	zloadScratchMemo proxy args pathfilter path tree getMD = do
		let fs = Proxy :: Proxy fs
		let proxyRep = Proxy
		oldpath <- pathfilter path
		mb <- findZippedMemo proxy oldpath proxyRep
		
		let load_scratch = do
			rep <- zloadScratch proxy args pathfilter path tree getMD
			inside $ addZippedMemo path proxy args rep (Just tree)
			return rep
		
		case mb of
			Just (memo_tree,memo_args,memo_rep) -> do
				mb_df <- forestM $ diffFS memo_tree tree path
				case mb_df of
					Just df -> unsafeWorld $ do -- XXX: how safe is this?
						dv <- diffValueThunk memo_tree memo_rep
						ds <- deltaArgs memo_tree proxy memo_args args
						let deltas = (args,ds)
						zloadDeltaMemo proxy deltas (return oldpath) memo_tree (memo_rep,getMD) path df tree dv
						return memo_rep
					Nothing -> load_scratch
			Nothing -> load_scratch
	
	zloadDelta :: Proxy args -> LoadDeltaArgs ICData fs args -> ForestI fs FilePath -> FSTree fs -> (rep,GetForestMD fs) -> FilePath -> FSTreeDeltaNodeMay -> FSTree fs -> ValueDelta fs rep -> ForestO fs (SValueDelta rep)
	
	zloadDeltaMemo :: (IncK (IncForest fs) content,Typeable content,ForestRep rep (ForestFSThunkI fs content)) => Proxy args -> LoadDeltaArgs ICData fs args -> ForestI fs FilePath -> FSTree fs -> (rep,GetForestMD fs) -> FilePath -> FSTreeDeltaNodeMay -> FSTree fs -> ValueDelta fs rep -> ForestO fs (SValueDelta rep)
	zloadDeltaMemo proxy (args,dargs) path tree (rep,getMD) path' df tree' dv = do
		drep <- zloadDelta proxy (args,dargs) path tree (rep,getMD) path' df tree' dv
		inside $ addZippedMemo path' proxy args rep (Just tree')
		return drep
	
	-- returns a manifest and a computation that adds consistent entries to a memotable (to be run only when there are no fatal manifest errors)
	zupdateManifestScratch :: Proxy args -> ForestIs fs args -> FilePath -> FSTree fs -> rep -> Manifest fs -> MManifestForestO fs
	
	zupdateManifestScratchMemo :: (IncK (IncForest fs) content,Eq rep,Typeable content,ForestRep rep (ForestFSThunkI fs content)) => Proxy args -> ForestIs fs args -> FilePath -> FSTree fs -> rep -> Manifest fs -> MManifestForestO fs
	zupdateManifestScratchMemo proxy args path tree rep man = do
		let fs = Proxy :: Proxy fs
		let proxyRep = Proxy
		mb <- lift $ inside $ findZippedMemo proxy path proxyRep
		
		let mani_scratch = do
			man1 <- zupdateManifestScratch proxy args path tree rep man
			Writer.tell $ inside . addZippedMemo path proxy args rep . Just
			return man1
		
		case mb of
			-- the rep needs to be the same
			Just (memo_tree,memo_args,(== rep) -> True) -> do
				mb_df <- lift $ forestM $ diffFS memo_tree tree path
				case mb_df of
					Just df -> do
						dv <- lift $ diffValueThunk memo_tree rep
						ds <- lift $ deltaArgs memo_tree proxy memo_args args
						let deltas = (args,ds)
						zupdateManifestDeltaMemo proxy deltas path path memo_tree df tree rep dv man
					Nothing -> mani_scratch
			Nothing -> mani_scratch
	
	zupdateManifestDelta :: Proxy args -> LoadDeltaArgs ICData fs args -> FilePath -> FilePath -> FSTree fs -> FSTreeDeltaNodeMay -> FSTree fs -> rep -> ValueDelta fs rep -> Manifest fs -> MManifestForestO fs
	
	zupdateManifestDeltaMemo :: (IncK (IncForest fs) content,Typeable content,ForestRep rep (ForestFSThunkI fs content)) => Proxy args -> LoadDeltaArgs ICData fs args -> FilePath -> FilePath -> FSTree fs -> FSTreeDeltaNodeMay -> FSTree fs -> rep -> ValueDelta fs rep -> Manifest fs -> MManifestForestO fs
	zupdateManifestDeltaMemo proxy (margs,dargs) path path' tree df tree' rep dv man = do
		man1 <- zupdateManifestDelta proxy (margs,dargs) path path' tree df tree' rep dv man
		Writer.tell $ inside . addZippedMemo path proxy margs rep . Just
		return man1
	
	zmanifest :: (IncK (IncForest fs) content,Eq rep,Typeable content,ForestRep rep (ForestFSThunkI fs content)) => ForestIs fs args -> FilePath -> rep -> MManifestForestO fs
	zmanifest = zmanifest' Proxy
	
	zmanifest' :: (IncK (IncForest fs) content,Eq rep,Typeable content,ForestRep rep (ForestFSThunkI fs content)) => Proxy args -> ForestIs fs args -> FilePath -> rep -> MManifestForestO fs
	zmanifest' proxy args path rep = lift (forestM latestTree) >>= \tree -> lift (forestM (newManifestWith "/" tree)) >>= zupdateManifestScratchMemo proxy args path tree rep

	zdefaultScratch :: Proxy args -> ForestIs fs args -> FilePath -> ForestI fs rep
	
	zdefaultScratchMemo :: (IncK (IncForest fs) content,Typeable content,ForestRep rep (ForestFSThunkI fs content)) => Proxy args -> ForestIs fs args -> FilePath -> ForestI fs rep
	zdefaultScratchMemo proxy args path = do
		rep <- zdefaultScratch proxy args path
		addZippedMemo path proxy args rep Nothing
		return rep

	zdefault :: (IncK (IncForest fs) content,Typeable content,ForestRep rep (ForestFSThunkI fs content)) => ForestIs fs args -> FilePath -> ForestI fs rep
	zdefault = zdefaultScratchMemo Proxy

-- returns a manifest and a sequence of memoization actions to be performed after store
type MForestO fs a = RWST Bool (FSTree fs -> ForestO fs ()) () (ForestO fs) a
type MManifestForestO fs = MForestO fs (Manifest fs)

instance Monad m => Monoid (m ()) where
	mempty = return ()
	mappend f g = f >> g

-- * Incremental Forest interface

-- (root path,current tree)
type family LoadInfo (ic :: ICMode) (fs :: FS) (args :: *) :: * where
	LoadInfo ICExpr fs args = ForestFSThunk fs Inside (FilePath,FSTree fs) -- we store the root path to avoid forcing the metadata thunk
	LoadInfo ICData fs args = (ForestICThunksI fs args,ForestFSThunk fs Inside (FilePath,FSTree fs))

-- we follow a closed-world assumption: the computations of the top-level arguments of a specification cannot be modified; nevertheless, they may depend on modifiables and their values are updated accordingly.

-- * Automatically generated "not to be seen" Forest class

-- A Class for the types of Forest specifications
class (IncK (IncForest fs) rep,IncK (IncForest fs) md,IncK (IncForest fs) (rep,md),ICMemo fs,ForestArgs fs args,MData NoCtx (ForestO fs) rep,ForestMD fs md) => ICForest (mode :: ICMode) fs args rep md | mode rep -> md, rep -> args  where
	
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
	
	loadScratchMemo :: (Typeable md,Typeable (ForestLs fs Inside args)) =>
		LiftedICMode mode -> Proxy args -> ForestIs fs args -> FilePathFilter fs -> FilePath -> FSTree fs -> GetForestMD fs -> ForestI fs (rep,md)
	loadScratchMemo mode proxy args oldpathM path (tree :: FSTree fs) getMD = do
		let fs = Proxy :: Proxy fs
		let proxyRep = Proxy
		oldpath <- oldpathM path
		mb <- findMemo proxy path proxyRep
		case mb of
			Just (memo_tree,memo_args,memo_rep,memo_md) -> do
				mb_df <- forestM $ diffFS memo_tree tree path
				case mb_df of
					Just df -> do
						-- XXX: how safe is this?
						unsafeWorld $ do
							ds <- deltaArgs memo_tree proxy memo_args args
							let deltas = mkLoadDeltaArgs mode fs proxy args ds
							loadDelta mode proxy deltas (return oldpath) memo_tree ((memo_rep,memo_md),getMD) path df tree
						unless (oldpath==path) $ remMemo fs oldpath proxyRep
						addMemo path proxy args (memo_rep,memo_md) tree
						return (memo_rep,memo_md)
					Nothing -> loadScratch mode proxy args oldpathM path tree getMD
			Nothing -> loadScratch mode proxy args oldpathM path tree getMD
	
	-- | incremental load function that considers the original source data and returns a stable delta
	-- expects the old data to be consistent with the old path and the old tree
	-- invariant: the FSTreeDelta is always relative to the updated root filepath
	-- the original root path is given as a computation (possibly over the original data), so that we can delay its evaluation
	loadDelta :: LiftedICMode mode -> Proxy args -> LoadDeltaArgs mode fs args -> ForestI fs FilePath -> FSTree fs -> OldData fs rep md -> FilePath -> FSTreeDeltaNodeMay -> FSTree fs -> ForestO fs (SValueDelta rep,SValueDelta md)

	-- | Writes the data to a private Forest on-disk location and generates a manifest file
	generateManifestScratch :: LiftedICMode mode -> ForestIs fs args -> FilePath -> FSTree fs -> (rep,md) -> ForestO fs (Manifest fs)
	generateManifestScratch mode args path tree dta = forestM (newManifestWith "/" tree) >>= updateManifestScratch mode args path tree dta
	
	updateManifestScratch :: LiftedICMode mode -> ForestIs fs args -> FilePath -> FSTree fs -> (rep,md) -> Manifest fs -> ForestO fs (Manifest fs)

	-- | generates default metadata based on the specification
	defaultMd :: ForestIs fs args -> rep -> FilePath -> ForestI fs md

-- performs a diff on all top-level forest thunks found in a value
-- returns true if no thunks are found
class ICRep fs => ForestDiff fs a where
	forestDiff :: FSTree fs -> a -> ForestO fs Bool

forestDiffProxy :: Proxy fs -> Proxy (ForestDiffDict fs)
forestDiffProxy _ = Proxy

data ForestDiffDict fs a = ForestDiffDict { forestDiffDict :: FSTree fs -> a -> ForestO fs Bool }

instance (ForestDiff fs a) => Sat (ForestDiffDict fs a) where
	dict = ForestDiffDict { forestDiffDict = forestDiff }

instance (ICRep fs,IncK (IncForest fs) a,Typeable a,ForestRep (ForestFSThunk fs l a) (ForestFSThunkI fs a)) => ForestDiff fs (ForestFSThunk fs l a) where
	forestDiff tree t = liftM isIdValueDelta $ diffValueThunk tree t

instance (ICRep fs,MData (ForestDiffDict fs) (ForestO fs) a) => ForestDiff fs a where
	forestDiff (tree :: FSTree fs) x = do
		let f t1 t2 = return $ t1 && t2
		gmapQr (forestDiffProxy (Proxy :: Proxy fs)) f True (forestDiffDict dict tree) x

-- | Class to support variadic Forest functions
class (FSRep fs,Typeable args,Typeable (ForestIs fs args)) => ForestArgs fs args where
	newArgs :: Proxy fs -> Proxy args -> ForestIs fs args -> ForestI fs (ForestICThunksI fs args)
	deltaArgs :: FSTree fs -> Proxy args -> ForestIs fs args -> ForestIs fs args -> ForestO fs (SValueDeltas (ForestICThunksI fs args))
	andSValueDeltas :: Proxy fs -> Proxy args -> SValueDeltas (ForestICThunksI fs args) -> SValueDelta (ForestICThunksI fs args)
	checkArgs :: Proxy fs -> Proxy args -> ForestIs fs args -> ForestICThunksI fs args -> ForestO fs Status
	monadArgs :: Proxy fs -> args -> ForestIs fs args
	vmonadArgs :: Proxy fs -> Proxy args -> ForestVs args -> ForestIs fs args
	vArgs :: Proxy fs -> Proxy args -> ForestIs fs args -> ForestI fs (ForestVs args)
	
instance ICRep fs => ForestArgs fs () where
	newArgs fs args () = return ()
	deltaArgs tree proxy args args' = return ()
	andSValueDeltas fs args () = Id
	checkArgs _ _ _ _ = return Valid
	monadArgs _ () = ()
	vmonadArgs _ _ () = ()
	vArgs _ _ () = return ()
	
instance (ForestDiff fs a,Data a,Eq a,IncK (IncForest fs) a,ICRep fs) => ForestArgs fs (Arg a) where
	newArgs tree args m = thunk m
	deltaArgs tree proxy marg marg'= do
		arg <- inside $ marg 
		arg' <- inside $ marg'
		isEmpty <- forestDiff tree arg'
		if (arg == arg' && isEmpty) then return Id else return Delta
	andSValueDeltas fs args d = d
	checkArgs _ _ marg targ = do
		arg <- inside $ marg
		arg' <- inside $ force targ
		return $ boolStatus (ConflictingArguments) (arg == arg')
	monadArgs fs (Arg arg) = return arg
	vmonadArgs fs args arg = return arg
	vArgs fs args m = m
	
instance (ICRep fs,ForestArgs fs a,ForestArgs fs b) => ForestArgs (fs :: FS) (a :*: b) where
	newArgs fs (args :: Proxy (a1 :*: a2)) (m1 :*: m2) = do
		t1 <- newArgs fs (Proxy :: Proxy a1) m1
		t2 <- newArgs fs (Proxy :: Proxy a2) m2
		return (t1 :*: t2)
	deltaArgs tree (proxy :: Proxy (a1 :*: a2)) (marg1 :*: marg2) (marg1' :*: marg2') = do
		d1 <- deltaArgs tree (Proxy :: Proxy a1) marg1 marg1'
		d2 <- deltaArgs tree (Proxy :: Proxy a2) marg2 marg2'
		return (d1 :*: d2)
	andSValueDeltas fs (args :: Proxy (a1 :*: a2)) (d1 :*: d2) = andSValueDeltas fs (Proxy :: Proxy a1) d1 `timesSValueDelta` andSValueDeltas fs (Proxy :: Proxy a2) d2
	checkArgs fs (args :: Proxy (a1 :*: a2)) (marg1 :*: marg2) (targ1 :*: targ2) = do
		status1 <- checkArgs fs (Proxy :: Proxy a1) marg1 targ1
		status2 <- checkArgs fs (Proxy :: Proxy a2) marg2 targ2
		return $ status1 `mappend` status2
	monadArgs fs (arg1 :*: arg2) = monadArgs fs arg1 :*: monadArgs fs arg2
	vmonadArgs fs (args :: Proxy (arg1 :*: arg2)) (arg1 :*: arg2) = vmonadArgs fs (Proxy :: Proxy arg1) arg1 :*: vmonadArgs fs (Proxy :: Proxy arg2) arg2
	vArgs fs (args :: Proxy (arg1 :*: arg2)) (marg1 :*: marg2) = do
		arg1 <- vArgs fs (Proxy :: Proxy arg1) marg1
		arg2 <- vArgs fs (Proxy :: Proxy arg2) marg2
		return (arg1 :*: arg2)

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





