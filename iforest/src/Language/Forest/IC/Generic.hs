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

import Control.Concurrent.Transactional
import Control.Monad.RWS (RWS(..),RWST(..))
import qualified Control.Monad.RWS as RWS
import Control.Monad.Reader (Reader(..),ReaderT(..))
import qualified Control.Monad.Reader as Reader
import Control.Monad.Trans
import Control.Monad.Writer (Writer(..),WriterT(..))
import qualified Control.Monad.Writer as Writer
import Language.Forest.IC.Default
import Language.Pads.Padsc hiding (gmapT,lift,gmapQr)
import Prelude hiding (mod,read)
import Data.Monoid
import Data.WithClass.MData
import Language.Pads.Generic
import Control.Monad.Incremental as Inc hiding (read)
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

-- hides @Forest_err@ values from the representation type of a variable
class ICRep fs => ForestContent (fs :: FS) var where
	type ForestContentTy fs var :: *
	lens_content :: LensM (ReaderT (FSTree fs) (ForestI fs)) var (ForestContentTy fs var)

instance ICRep fs => ForestContent fs Binary where
	type ForestContentTy fs Binary = Binary
	lens_content = idLensM

type FTK fs rep = (ZippedICForest fs rep,FTKK fs rep)

type FTKK fs rep = (CopyFSThunks fs Outside rep,Eq rep,IncK (IncForest fs) (ForestRepTy fs rep),IncK (IncForest fs) (ForestContentTy fs (ForestRepTy fs rep)),ForestRep fs rep,ForestContent fs (ForestRepTy fs rep),Typeable (ForestRepTy fs rep))

infoLensM :: ICRep fs => LensM (ReaderT (FSTree fs) (ForestI fs)) (Forest_md fs) (ForestFSThunkI fs FileInfo)
infoLensM = LensM get put where
	get ms = liftM fileInfo ms
	put ms mv = do
		fmd <- ms
		info' <- mv
		return $ fmd { fileInfo = info'}

instance (IncK (IncForest fs) FileInfo,ForestContent fs rep) => ForestContent fs (Forest_md fs,rep) where
	type ForestContentTy fs (Forest_md fs,rep) = (FileInfo,ForestContentTy fs rep)
	lens_content = prodLensM (infoLensM `compLensM` fsThunkLensI) lens_content
instance (IncK (IncForest fs) FileInfo,ForestContent fs rep) => ForestContent fs ((Forest_md fs,bmd),rep) where
	type ForestContentTy fs ((Forest_md fs,bmd),rep) = ((FileInfo,bmd),ForestContentTy fs rep)
	lens_content = prodLensM (prodLensM (infoLensM `compLensM` fsThunkLensI) idLensM) lens_content
instance (ForestContent fs rep) => ForestContent fs (ForestFSThunkI fs Forest_err,rep) where
	type ForestContentTy fs (ForestFSThunkI fs Forest_err,rep) = ForestContentTy fs rep
	lens_content = sndLensM `compLensM` lens_content
instance ICRep fs => ForestContent fs String where
	type ForestContentTy fs String = String
	lens_content = idLensM
instance ICRep fs => ForestContent fs (ForestFSThunkI fs rep) where
	type ForestContentTy fs (ForestFSThunkI fs rep) = (ForestFSThunkI fs rep)
	lens_content = idLensM
instance ICRep fs => ForestContent fs (Map a b) where
	type ForestContentTy fs (Map a b) = Map a b
	lens_content = idLensM
instance ICRep fs => ForestContent fs (Set a) where
	type ForestContentTy fs (Set a) = Set a
	lens_content = idLensM
instance ICRep fs => ForestContent fs [a] where
	type ForestContentTy fs [a] = [a]
	lens_content = idLensM
instance (ICRep fs,ForestContent fs a) => ForestContent fs (Maybe a) where
	type ForestContentTy fs (Maybe a) = Maybe (ForestContentTy fs a)
	lens_content = maybeLensM lens_content
instance ICRep fs => ForestContent fs Text where
	type ForestContentTy fs Text = Text
	lens_content = idLensM


class ICRep fs => ZippedICMemo fs where
	
	-- adds a consistent path ~ value entry to the consistency table
	-- if no tree is provided, no memoization is done and we just remember the arguments
	addZippedMemo :: (FTK fs rep) => FilePath -> ForestIs fs (ForestArgs rep) -> rep -> Maybe (FSTree fs) -> ForestI fs ()
	
	-- given a path finds an old entry = (old FSTree,outdated thunks)
	-- the old entry needs to match on the filepath, i.e., have been loaded with that path
	-- if repairing incrementally, we have to assume that the environment changed
	findZippedMemo :: (FTK fs rep) => FilePath -> Proxy rep -> ForestI fs (Maybe (FSTree fs,ForestIs fs (ForestArgs rep),rep))

type FTM fs = ForestO fs
type ReadOnlyFTM fs = ForestI fs
type FTV fs a = ForestFSThunkI fs a

class Forest fs where
	
	new :: FTK fs rep => ForestVs (ForestArgs rep) -> FilePath -> FTM fs rep
	
	-- gets the arguments bound to a variable
	args :: FTK fs rep => rep -> ReadOnlyFTM fs (ForestVs (ForestArgs rep),FilePath)
	
	read :: (ForestLayer fs l,FTK fs rep) => rep -> ForestL fs l (ForestContentTy fs (ForestRepTy fs rep))
	
	-- tries to modify a variable
	-- the write only occurs if validation succeeds
	-- if the new value is not a consistent view of the FS, an alternative action is run otherwise
	writeOrElse :: (FTK fs rep) => rep -> ForestContentTy fs (ForestRepTy fs rep) -> b -> ([ManifestError] -> FTM fs b) -> FTM fs b
	
	-- read-only Forest error count
	validate :: (ForestLayer fs l,FTK fs rep) => rep -> ForestL fs l Forest_err
	validate = get_errors
	
	-- * An attempt to mimic regular filesystem operations, but over Forest specifications
	
	-- recursively deletes a variable from the filesystem; should always succeed
	delete :: (FTK fs rep) => rep -> FTM fs ()
	-- recursively copies the content of a variable into another; it may fail if the copied data is not consistent with the arguments and filepath of the target variable
	copyOrElse :: (FTK fs rep) => rep -> rep -> b -> ([ManifestError] -> FTM fs b) -> FTM fs b
	
unsafeIOToFTM :: (Forest fs,ICRep fs) => IO a -> FTM fs a
unsafeIOToFTM = forestM . forestIO
	
tryWrite :: (Display Outside (IncForest fs) rep,Forest fs,FTK fs rep) => rep -> ForestContentTy fs (ForestRepTy fs rep) -> FTM fs ()
tryWrite t v = writeOrElse t v () (Prelude.const $ return ())

writeOrRetry :: (Transactional (IncForest fs),Display Outside (IncForest fs) rep,Forest fs,FTK fs rep) => rep -> ForestContentTy fs (ForestRepTy fs rep) -> b -> FTM fs b
writeOrRetry t v b = writeOrElse t v b (Prelude.const retry)

writeOrShow :: (Display Outside (IncForest fs) rep,Forest fs,FTK fs rep) => rep -> ForestContentTy fs (ForestRepTy fs rep) -> FTM fs String
writeOrShow t v = writeOrElse t v "" (return . show)

writeOrThrow :: (MonadThrow (Outside (IncForest fs)),Display Outside (IncForest fs) rep,Forest fs,FTK fs rep,Exception e) => rep -> ForestContentTy fs (ForestRepTy fs rep) -> e -> FTM fs ()
writeOrThrow t v e = writeOrElse t v () (Prelude.const $ throw e)

writeOrError :: (MonadThrow (Outside (IncForest fs)),Display Outside (IncForest fs) rep,Forest fs,FTK fs rep) => rep -> ForestContentTy fs (ForestRepTy fs rep) -> String -> FTM fs ()
writeOrError t v e = writeOrElse t v () (Prelude.const $ error e)

-- * Data/Metadata only functions
-- we don't provide data/metadata only write functions because writeData >> writeMeta would not be the same as write, thus misleading.

readData :: (ForestContentTy fs (ForestRepTy fs rep) ~ (md_content,rep_content),Forest fs,ForestLayer fs l,FTK fs rep) => rep -> ForestL fs l rep_content
readData = liftM snd . read
	
readMeta :: (ForestContentTy fs (ForestRepTy fs rep) ~ (md_content,rep_content),Forest fs,ForestLayer fs l,FTK fs rep) => rep -> ForestL fs l md_content
readMeta = liftM fst . read

-- * Zipped Incremental Forest interface

type family ForestArgs rep :: *

class (Eq rep,ForestMD fs rep,IncK (IncForest fs) rep,Typeable (ForestIs fs (ForestArgs rep)),IncK (IncForest fs) Forest_err,ICRep fs,ZippedICMemo fs,ForestArgsClass fs (ForestArgs rep),MData NoCtx (ForestO fs) rep) => ZippedICForest fs rep  where
	
	diffValue :: FSTree fs -> rep -> ForestO fs (ValueDelta fs rep)
	
	zload :: FTK fs rep => ForestIs fs (ForestArgs rep) -> FilePath -> ForestI fs rep
	zload args path = forestM latestTree >>= \tree -> zloadScratchMemo Proxy args return path tree getForestMDInTree
	
	zloadScratchGeneric :: Proxy rep -> ForestIs fs (ForestArgs rep) -> FilePathFilter fs -> FilePath -> FSTree fs -> GetForestMD fs -> ForestI fs rep
	
	zloadScratch :: Proxy rep -> ForestIs fs (ForestArgs rep) -> FilePathFilter fs -> FilePath -> FSTree fs -> GetForestMD fs -> ForestI fs rep
	
	zloadScratchMemo :: FTK fs rep => Proxy rep -> ForestIs fs (ForestArgs rep) -> FilePathFilter fs -> FilePath -> FSTree fs -> GetForestMD fs -> ForestI fs rep
	zloadScratchMemo (proxyRep :: Proxy rep) args pathfilter path tree getMD = do
		let fs = Proxy :: Proxy fs
		let proxy = Proxy :: Proxy (ForestArgs rep)
		oldpath <- pathfilter path
		mb <- findZippedMemo oldpath proxyRep
		
		let load_scratch = do
			rep <- zloadScratch proxyRep args pathfilter path tree getMD
			inside $ addZippedMemo path args rep (Just tree)
			return rep
		
		case mb of
			Just (memo_tree,memo_args,memo_rep) -> debug ("zloadDeltaMemo "++show path) $ do
				mb_df <- forestM $ diffFS memo_tree tree path
				case mb_df of
					Just df -> unsafeWorld $ do -- XXX: how safe is this?
						dv <- diffValueThunk memo_tree memo_rep
						ds <- deltaArgs memo_tree proxy memo_args args
						let deltas = (args,ds)
						zloadDeltaMemo deltas (return oldpath) memo_tree (memo_rep,getMD) path df tree dv
						return memo_rep
					Nothing -> load_scratch
			Nothing -> load_scratch
	
	zloadDeltaGeneric :: LoadDeltaArgs fs (ForestArgs rep) -> ForestI fs FilePath -> FSTree fs -> (rep,GetForestMD fs) -> FilePath -> FSTreeD fs -> FSTree fs -> ValueDelta fs rep -> ForestO fs (NSValueDelta rep)
	
	zloadDelta :: LoadDeltaArgs fs (ForestArgs rep) -> ForestI fs FilePath -> FSTree fs -> (rep,GetForestMD fs) -> FilePath -> FSTreeD fs -> FSTree fs -> ValueDelta fs rep -> ForestO fs (NSValueDelta rep)
	
	zloadDeltaMemo :: FTK fs rep => LoadDeltaArgs fs (ForestArgs rep) -> ForestI fs FilePath -> FSTree fs -> (rep,GetForestMD fs) -> FilePath -> FSTreeD fs -> FSTree fs -> ValueDelta fs rep -> ForestO fs (NSValueDelta rep)
	zloadDeltaMemo (args,dargs) path tree (rep,getMD) path' df tree' dv = do
		drep <- zloadDelta (args,dargs) path tree (rep,getMD) path' df tree' dv
		inside $ addZippedMemo path' args rep (Just tree')
		return drep
	
	zupdateManifestScratchGeneric :: ForestIs fs (ForestArgs rep) -> FilePath -> FSTree fs -> rep -> Manifest fs -> MManifestForestO fs
	
	-- returns a manifest and a computation that adds consistent entries to a memotable (to be run only when there are no fatal manifest errors)
	zupdateManifestScratch :: ForestIs fs (ForestArgs rep) -> FilePath -> FSTree fs -> rep -> Manifest fs -> MManifestForestO fs
	
	zupdateManifestScratchMemo :: FTK fs rep => ForestIs fs (ForestArgs rep) -> FilePath -> FSTree fs -> rep -> Manifest fs -> MManifestForestO fs
	zupdateManifestScratchMemo args path tree (rep::rep) man = do
		let fs = Proxy :: Proxy fs
		let proxy = Proxy :: Proxy (ForestArgs rep)
		let proxyRep = Proxy :: Proxy rep
		mb <- lift $ inside $ findZippedMemo path proxyRep
		
		let mani_scratch = do
			man1 <- zupdateManifestScratch args path tree rep man
			Writer.tell $ inside . addZippedMemo path args rep . Just
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
						zupdateManifestDeltaMemo deltas path path memo_tree df tree rep dv man
					Nothing -> mani_scratch
			Nothing -> mani_scratch
	
	zupdateManifestDeltaGeneric :: LoadDeltaArgs fs (ForestArgs rep) -> FilePath -> FilePath -> FSTree fs -> FSTreeD fs -> FSTree fs -> rep -> ValueDelta fs rep -> Manifest fs -> MManifestForestO fs
	
	zupdateManifestDelta :: LoadDeltaArgs fs (ForestArgs rep) -> FilePath -> FilePath -> FSTree fs -> FSTreeD fs -> FSTree fs -> rep -> ValueDelta fs rep -> Manifest fs -> MManifestForestO fs
	
	zupdateManifestDeltaMemo :: FTK fs rep => LoadDeltaArgs fs (ForestArgs rep) -> FilePath -> FilePath -> FSTree fs -> FSTreeD fs -> FSTree fs -> rep -> ValueDelta fs rep -> Manifest fs -> MManifestForestO fs
	zupdateManifestDeltaMemo (margs,dargs) path path' tree df tree' rep dv man = do
		man1 <- zupdateManifestDelta (margs,dargs) path path' tree df tree' rep dv man
		Writer.tell $ inside . addZippedMemo path margs rep . Just
		return man1
	
	zmanifest :: FTK fs rep => ForestIs fs (ForestArgs rep) -> FilePath -> rep -> MManifestForestO fs
	zmanifest args path rep = lift (forestM latestTree) >>= \tree -> lift (forestM (newManifestWith "/" tree)) >>= zupdateManifestScratchMemo args path tree rep

	zdefaultScratchGeneric :: Proxy rep -> Proxy fs -> ForestIs fs (ForestArgs rep) -> FilePath -> FSTree fs -> ForestI fs rep

	zdefaultScratch :: Proxy rep -> Proxy fs -> ForestIs fs (ForestArgs rep) -> FilePath -> FSTree fs -> ForestI fs rep
	
	zdefaultScratchMemo :: FTK fs rep => Proxy rep -> Proxy fs -> ForestIs fs (ForestArgs rep) -> FilePath -> FSTree fs -> ForestI fs rep
	zdefaultScratchMemo proxy fs args path tree = do
		rep <- zdefaultScratch proxy fs args path tree
		addZippedMemo path args rep Nothing
		return rep

	zdefault :: FTK fs rep => ForestIs fs (ForestArgs rep) -> FilePath -> ForestI fs rep
	zdefault args path = forestM latestTree >>= \tree -> zdefaultScratchMemo Proxy Proxy args path tree

-- returns a manifest and a sequence of memoization actions to be performed after store
type MForestO fs a = RWST Bool (FSTree fs -> ForestO fs ()) () (ForestO fs) a
type MManifestForestO fs = MForestO fs (Manifest fs)

instance Monad m => Monoid (m ()) where
	mempty = return ()
	mappend f g = f >> g

-- * Incremental Forest interface

type LoadInfo fs args = (ForestICThunksI fs args,ForestFSThunk fs Inside (FilePath,FSTree fs))

-- performs a diff on all top-level forest thunks found in a value
-- returns true if no thunks are found
class ICRep fs => ForestDiff fs a where
	forestDiff :: FSTree fs -> a -> ForestO fs Bool

forestDiffProxy :: Proxy fs -> Proxy (ForestDiffDict fs)
forestDiffProxy _ = Proxy

data ForestDiffDict fs a = ForestDiffDict { forestDiffDict :: FSTree fs -> a -> ForestO fs Bool }

instance (ForestDiff fs a) => Sat (ForestDiffDict fs a) where
	dict = ForestDiffDict { forestDiffDict = forestDiff }

instance (ICRep fs,IncK (IncForest fs) a,Typeable a,ForestRep fs (ForestFSThunk fs Inside a)) => ForestDiff fs (ForestFSThunk fs Inside a) where
	forestDiff tree t = liftM isIdValueDelta $ diffValueThunk tree t

instance (ICRep fs,MData (ForestDiffDict fs) (ForestO fs) a) => ForestDiff fs a where
	forestDiff (tree :: FSTree fs) x = do
		let f t1 t2 = return $ t1 && t2
		gmapQr (forestDiffProxy (Proxy :: Proxy fs)) f True (forestDiffDict dict tree) x

-- | Class to support variadic Forest functions
class (FSRep fs,Typeable args,Typeable (ForestIs fs args)) => ForestArgsClass fs args where
	newArgs :: Proxy fs -> Proxy args -> ForestIs fs args -> ForestI fs (ForestICThunksI fs args)
	deltaArgs :: FSTree fs -> Proxy args -> ForestIs fs args -> ForestIs fs args -> ForestO fs (SValueDeltas (ForestICThunksI fs args))
	andSValueDeltas :: Proxy fs -> Proxy args -> SValueDeltas (ForestICThunksI fs args) -> SValueDelta (ForestICThunksI fs args)
	checkArgs :: Proxy fs -> Proxy args -> ForestIs fs args -> ForestICThunksI fs args -> ForestO fs Status
	monadArgs :: Proxy fs -> args -> ForestIs fs args
	vmonadArgs :: Proxy fs -> Proxy args -> ForestVs args -> ForestIs fs args
	vArgs :: Proxy fs -> Proxy args -> ForestIs fs args -> ForestI fs (ForestVs args)
	
instance ICRep fs => ForestArgsClass fs () where
	newArgs fs args () = return ()
	deltaArgs tree proxy args args' = return ()
	andSValueDeltas fs args () = Id
	checkArgs _ _ _ _ = return Valid
	monadArgs _ () = ()
	vmonadArgs _ _ () = ()
	vArgs _ _ () = return ()
	
instance (ForestDiff fs a,Data a,Eq a,IncK (IncForest fs) a,ICRep fs) => ForestArgsClass fs (Arg a) where
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
	
instance (ICRep fs,ForestArgsClass fs a,ForestArgsClass fs b) => ForestArgsClass (fs :: FS) (a :*: b) where
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

type family SValueDeltas args :: * where
	SValueDeltas (a :*: b) = (SValueDeltas a :*: SValueDeltas b)
	SValueDeltas (Arg a) = SValueDelta a
	SValueDeltas (ICThunk fs l inc a) = SValueDelta (ICThunk fs l inc a)
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

listMDNonEmptyFiles :: (IncK (IncForest fs) FileInfo,IncK (IncForest fs) Forest_err,ICRep fs,FSRep fs,MData NoCtx (ForestO fs) a) => a -> ForestO fs [Forest_md fs]
listMDNonEmptyFiles = listify proxyNoCtx aux where
	aux r = get_info r >>= \i -> return $ fullpath i /= ""

mapInfoFiles :: MData NoCtx m a => a -> m (Map FilePath FileInfo)
mapInfoFiles md = do
  fileInfos <- listInfoNonEmptyFiles md
  let keyedInfos = map (\finfo -> (fullpath finfo, finfo)) fileInfos
  return $ Data.Map.fromList keyedInfos

type LoadDeltaArgs fs args = (ForestIs fs args,SValueDeltas (ForestICThunksI fs args)) -- they are not in fact stable deltas, but we store the new computations separately

mkEmptyLoadDeltaArgs ::  Proxy fs -> LoadDeltaArgs fs ()
mkEmptyLoadDeltaArgs  fs = ((),())


mkLoadDeltaArgs :: Proxy fs -> Proxy args -> ForestIs fs args -> SValueDeltas (ForestICThunksI fs args) -> LoadDeltaArgs  fs args
mkLoadDeltaArgs  fs args margs dargs = (margs,dargs)


getLoadDeltaArgs :: Proxy fs -> Proxy args -> LoadDeltaArgs  fs args -> SValueDeltas (ForestICThunksI fs args)
getLoadDeltaArgs  fs args (margs,dargs) = dargs


type MDArgs md args = md

mkMDArgs :: md -> args -> MDArgs md args
mkMDArgs md args = md

patMDArgs :: Monad m => MDArgs md args -> (md -> m args) -> m (md,args)
patMDArgs md fargs = liftM (md,) (fargs md)







