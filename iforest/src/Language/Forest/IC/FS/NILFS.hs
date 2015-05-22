{-# LANGUAGE TupleSections, TypeOperators, TemplateHaskell, ScopedTypeVariables, ConstraintKinds, FlexibleInstances, MultiParamTypeClasses, StandaloneDeriving, GeneralizedNewtypeDeriving, FlexibleContexts, DataKinds, TypeFamilies, Rank2Types, GADTs, ViewPatterns, DeriveDataTypeable #-}

module Language.Forest.IC.FS.NILFS (
	changeFS,ForestCfg(..),IncParams(..)
	) where

import Language.Forest.FS.Diff
import System.Mem.StableName.Exts as StableName
import qualified Prelude
import Prelude hiding (read,mod,const)
import Control.Monad.Incremental.Internal.Adapton.Memo
import Control.Monad.Incremental.Internal.Adapton.Algorithm as Adapton
import Control.Monad.Incremental.Internal.Adapton.Types as Adapton
import Control.Monad.Incremental.Internal.Adapton.Layers as Adapton
import Control.Applicative
import qualified Language.Forest.IC.MetaData as IC
--import Filesystem.Path.CurrentOS hiding (FilePath,concat,(</>))
import Language.Forest.FS.FSDelta
import Language.Forest.IO.Utils
import Data.Dynamic
--import System.FSNotify
import Language.Forest.FS.FSRep
import Language.Forest.IC.BX as BX
import Language.Forest.IC.ICRep
import Language.Forest.IC.Generic
import Language.Forest.FS.NILFS
import qualified Data.DList as DList
import Data.Strict.Tuple
import qualified Data.Strict.List as Strict
import Control.Monad
--import Control.Monad.IO.Class
import Control.Concurrent.MVar
import Control.Concurrent
import Control.Exception
import qualified Control.Concurrent.Weak.Map as CWeakMap
--import Control.Concurrent.Async

import System.Mem.Weak.Exts (WeakRef(..))
import System.Mem.Weak.Exts as Weak
--import Control.Lens hiding (runIdentity,inside)
import Data.Maybe
import Data.Hashable
import Data.List
import Data.WithClass.MData
--import Data.Functor.Identity
import Data.List.Split
--import Language.Forest.Pure.MetaData
import Data.Typeable
import System.Directory
import System.IO.Unsafe
import Data.List as List
import Data.IORef.Exts
--import Data.Dequeue as Queue
import Data.Unique
import Data.Time.Clock
import System.IO
import Data.Map (Map(..))
import qualified Data.Map as Map
import Data.Set (Set(..))
import qualified Data.Set as Set
import Data.String
import qualified Data.Text as Text
import Data.Text (Text(..))
import System.FilePath
import Safe
import Data.Global.TH as TH

import Control.Monad.Incremental.Adapton
import qualified Control.Monad.Incremental.Adapton as Adapton
import Control.Monad.Incremental as Inc
import System.Process
import Language.Forest.IO.Shell
--import Language.Forest.FS.FSNotify
import Control.Monad.Trans
import Control.Monad.Ref
import Control.Monad.Incremental as Inc
import Data.Time.LocalTime
import Language.Forest.FS.NILFS hiding (LiveSnapshots,findRootDevice,MountPoint,mountSnapshot,Snapshot,readNILFSTime)
import System.Mem.MemoTable (MemoTable(..))
import qualified System.Mem.MemoTable as MemoTable

import Control.Monad.Reader (Reader(..),ReaderT(..),MonadReader(..))
import qualified Control.Monad.Reader as Reader
import Data.Global.Dynamic as Dyn

{-# NOINLINE symlinks #-}
symlinks :: CWeakMap.WeakMap Snapshot FSTreeSym
symlinks = unsafePerformIO $ CWeakMap.empty

TH.declareIORef "canonicalTreeNILFS" [t| CanonicalTree NILFS |] [e| CanonicalTree Map.empty |]

{-# NOINLINE liveSnapshots #-}
liveSnapshots :: LiveSnapshots
liveSnapshots = unsafePerformIO $ CWeakMap.empty

{-# NOINLINE forestData #-}
forestData :: IORef ForestData
forestData = unsafePerformIO $ newIORef undefined

type instance IncK (IncForest NILFS) a = (Typeable a,Eq a)

instance Incremental (IncForest NILFS) where
	
	newtype IncParams (IncForest NILFS) = NILFSIncParams (ForestCfg NILFS)
	
	newtype Outside (IncForest NILFS) a = NILFSForestO { adaptonOuter :: IO a } deriving (Monad,Functor,Applicative)
	newtype Inside (IncForest NILFS) a = NILFSForestI { adaptonInner :: IO a } deriving (Monad,Functor,Applicative)
	
	world = NILFSForestO . adaptonInner
	unsafeWorld = NILFSForestI . adaptonOuter
	
	runIncrementalWithParams (NILFSIncParams cfg) m = runNILFSForest cfg m
	defaultIncParams = error "no default parameters, the user needs to explicitly provide some"
	
	unsafeIOToInc = inside . NILFSForestI

nilfsTreeTime :: FSTree NILFS -> UTCTime
nilfsTreeTime (NILFSTree _ t _) = t
nilfsTreeTime (VirtualNILFSTree _ t _) = t

nilfsTreeSnapshot :: FSTree NILFS -> Snapshot
nilfsTreeSnapshot (NILFSTree s t _) = s
nilfsTreeSnapshot (VirtualNILFSTree s t _) = s

instance Ord (FSTree NILFS) where
	compare t1 t2 = compare (nilfsTreeSnapshot t1) (nilfsTreeSnapshot t2)
instance Eq (FSTree NILFS) where
	t1 == t2 = (nilfsTreeSnapshot t1) == (nilfsTreeSnapshot t2)

-- Log-based filesystem with support for snapshots
-- FSTree operations can be treated as pure, since NILFS snapshots are stable throughout the run of a program; although in reality old anpshots may be purged?

type Stone = IORef ()

instance FSRep NILFS where
	
	-- the forestdir needs to be a directory outside of the logged filesystem
	data ForestCfg NILFS = NILFSForestConfig { rootPath :: FilePath, forestDir :: FilePath, incParams :: IncParams Adapton }

	runForest cfg (NILFSForestM m) = runNILFSForest cfg (outside $ NILFSForestI m)

	forestIO = NILFSForestM

	-- the same as the inner layer
	newtype ForestM NILFS a = NILFSForestM { runNILFSForestM :: IO a } deriving (Monad,Functor,Applicative)

	getForestDirectory = do
		(forestDir :!: ((rootPath,rootFolder),device)) <- forestIO $ readIORef' forestData
		return forestDir

	data FSTree NILFS = NILFSTree Snapshot UTCTime Stone | VirtualNILFSTree Snapshot UTCTime Stone

	type FSTreeD NILFS = FSTreeDeltaNodeMay -- a delta fixed-point focused on the current path

	virtualTree (NILFSTree ss time stone) = return $ VirtualNILFSTree ss time stone
	virtualTree (VirtualNILFSTree ss time stone) = return $ VirtualNILFSTree ss time stone

	latestTree = forestIO latestNILFSTree >>= \tree@(NILFSTree snap time stone) -> testAndMountSnapshot snap stone >> return tree

	pathInTree path tree@(NILFSTree snapshot time stone) = do
		(forestDir :!: ((rootPath,rootFolder),device)) <- forestIO $ readIORef' forestData
		mb <- forestIO $ CWeakMap.lookup liveSnapshots snapshot
		case mb of
			Just ref -> do
				(mountpoint) <- forestIO $ readIORef ref
				let result = mountpoint </> makeRelative rootPath path
				return $ {-debug ("pathInTree: "++show path ++ " " ++ show (snapshot,mountpoint,rootPath) ++ " -> " ++ show result) -} result
			Nothing -> mountSnapshotM snapshot stone >> pathInTree path tree
	pathInTree path tree@(VirtualNILFSTree snapshot time stone) = do
		(forestDir :!: ((rootPath,rootFolder),device)) <- forestIO $ readIORef' forestData
		mb <- forestIO $ CWeakMap.lookup liveSnapshots snapshot
		case mb of
			Just ref -> do
				(mountpoint) <- forestIO $ readIORef ref
				home <- forestIO $ getHomeDirectory
				let result = home </> ".avfs" </> makeRelative "/" (mountpoint </> makeRelative rootPath path)
				return $ {-debug ("pathInTree: "++show path ++ " " ++ show (snapshot,mountpoint,rootPath) ++ " -> " ++ show result) -} result
			Nothing -> mountSnapshotM snapshot stone >> pathInTree path tree
	stepPathInTree tree path rel = do
		cpath <- canonalizePathInTree path tree
		let newpath = path </> rel
		cnewpath <- canonalizePathInTree newpath tree
		if cpath `isParentPathOf` cnewpath
			then return newpath 
			else error $ "NILFS incremental loading forbids non-tree-like filesystems" ++ show cpath ++ " " ++ show newpath
			
	canonalizePathInTree = canonalizePathNILFS
	diffFS oldtree newtree path = do
		let cp1 = nilfsTreeSnapshot oldtree
		let cp2 = nilfsTreeSnapshot newtree
		(forestDir :!: nilfsData@((rootPath,rootFolder),device)) <- forestIO $ readIORef' forestData
		
		-- difference between NILFS snapshots
		(dsNILFS,tdNILFS) <- if cp1 == cp2
			then return (DList.empty,emptyFSTreeDelta) -- nothing to do
			else do
				mount1 <- mountSnapshot liveSnapshots nilfsData cp1 (MkWeak $ mkWeakKey oldtree)
				mount2 <- mountSnapshot liveSnapshots nilfsData cp2 (MkWeak $ mkWeakKey newtree)
				let device = Prelude.snd nilfsData
				forestIO $ diffNILFS mount2 device cp1 cp2
		syms <- getSymlinks (Just (cp1,dsNILFS)) cp2 (MkWeak $ mkWeakKey newtree)
		let fixtd = fixFSTreeDelta tdNILFS syms
		let df = focusFSTreeDeltaByRelativePathMay fixtd path
		return $ Just df

	showFSTree tree = return $ show tree
	
	compareFSTree t1 t2 = return $ compare t1 t2

	unsafeInterleaveForestM (NILFSForestM io) = NILFSForestM $ unsafeInterleaveIO io
	
	deletePath = error "no file operations"
	writeFile = error "no file operations"
	writeDir = error "no file operations"
	writeLink = error "no file operations"
	writePathMD = error "no file operations"
	
	tempPath = forestIO getTempPath
	
	isEmptyFSTreeD _ = isEmptyFSTreeDeltaNodeMay
	isEmptyTopFSTreeD _ = isEmptyTopFSTreeDeltaNodeMay
	isChgFSTreeD _ = isChgFSTreeDeltaNodeMay
	isMoveFSTreeD _ = isMoveFSTreeDeltaNodeMay
	emptyFSTreeD _ = Nothing
	
	focusDiffFSTreeD tree path tree' path' = do
		td <- focusDiffFSTreeDelta tree path tree' path'
		syms <- getSymlinks Nothing (fsTreeSnapshot tree') (MkWeak $ mkWeakKey tree')
		let fixtd = fixFSTreeDelta td syms
		return $ focusFSTreeDeltaByRelativePathMay fixtd "/"
	focusFSTreeD fs td path file pathfile = focusFSTreeDeltaNodeMayByRelativePath td file

instance WeakKey (FSTree NILFS) where
	mkWeakKey (NILFSTree _ _ stone) = mkWeakKey stone
	mkWeakKey (VirtualNILFSTree _ _ stone) = mkWeakKey stone

instance Show (FSTree NILFS) where
	show (NILFSTree snap time _) = "(NILFSTree " ++ show snap ++ " " ++ show time ++ ")"
	show (VirtualNILFSTree snap time _) = "(VirtualNILFSTree " ++ show snap ++ " " ++ show time ++ ")"

fsTreeSnapshot :: FSTree NILFS -> Snapshot
fsTreeSnapshot (NILFSTree snap time _) = snap
fsTreeSnapshot (VirtualNILFSTree snap time _) = snap

canonalizePathNILFS :: FilePath -> FSTree NILFS -> ForestM NILFS FilePath
canonalizePathNILFS path tree = do
	norm <- forestIO $ liftM normalise $ absolutePath path
	let dirs = splitDirectories norm	
	mb <- liftM (findCanonicalTree dirs) $ forestIO $ readIORef' canonicalTreeNILFS
	can <- case mb of
		Just (root,oldtree,suffix) -> do
			cmp <- compareFSTree oldtree tree
			if (cmp == EQ)
				then forestIO $ canonalizePath (root </> joinPath suffix)
				else forestIO $ canonalizePath norm
		otherwise -> forestIO $ canonalizePath norm
	forestIO $ modifyIORef canonicalTreeNILFS $ memoCanonicalTree norm can tree
	return can

instance ICRep NILFS where
	
	forestO = NILFSForestM . adaptonOuter
	forestM = inside . NILFSForestI . runNILFSForestM
	
	data FSThunk NILFS l inc a = NILFSFSThunk { adaptonThunk :: (L l (IncForest NILFS) a), nilfsFSThunkArgs :: IORef (Maybe NILFSArgs) } -- a set of snapshots on which the FSThunk depends and a lazy Adapton modifiable
	
	newtype ICThunk NILFS l inc a = NILFSU { adaptonU :: U l (IncForest NILFS) a }

	data HSThunk NILFS l inc a = NILFSThunk { unNILFSThunk :: T l inc a, nilfsHSThunkArgs :: IORef (Maybe NILFSArgs) }
	
	newtype ValueDelta NILFS a = ValueDeltaNILFS Bool -- True = identity

	diffValueThunk = diffValueThunkNILFS False
	diffTopValueThunk = diffValueThunkNILFS True	
	diffValueAny _ _ = return chgValueDelta
	
	isIdValueDelta (ValueDeltaNILFS d) = d
	idValueDelta = ValueDeltaNILFS True
	chgValueDelta = ValueDeltaNILFS False
	mapValueDelta _ (ValueDeltaNILFS d) = (ValueDeltaNILFS d)

	eqFSThunk t1 t2 = idNM (metaL $ adaptonThunk t1) == idNM (metaL $ adaptonThunk t2)
	eqICThunk t1 t2 = False

	isUnevaluatedFSThunk var = isUnevaluatedL (adaptonThunk var)
	compareFSThunk t1 t2 = (idNM $ metaL $ adaptonThunk t1) `compare` (idNM $ metaL $ adaptonThunk t1)
	compareICThunk t1 t2 = (idNM $ metaU $ adaptonU t1) `compare` (idNM $ metaU $ adaptonU t2)

diffValueThunkNILFS :: (IncK (IncForest NILFS) (ForestRepTy NILFS rep),Typeable (ForestRepTy NILFS rep),ForestRep NILFS rep) => Bool -> FSTree NILFS -> rep -> ForestO NILFS (ValueDelta NILFS rep)
diffValueThunkNILFS isTop oldtree rep = do
		let thunk = to (iso_rep_thunk proxyNILFS) rep
		mb_args <- forestM $ forestIO $ readIORef' $ nilfsFSThunkArgs thunk
		case mb_args of
			Just (_ :!: _ :!: Just newtree) -> do
				cmp <- forestM $ compareFSTree oldtree newtree
				return $ ValueDeltaNILFS $ cmp /= LT
			otherwise -> return $ ValueDeltaNILFS False
	

-- arguments passed to transactional variables
type NILFSArgs = (Dynamic :!: FilePath :!: Maybe (FSTree NILFS))

instance ForestLayer NILFS l => Thunk (HSThunk NILFS) l (IncForest NILFS) where
	new v = do
		t <- Inc.new v
		args <- forestM $ forestIO $ newIORef' Nothing
		return $ NILFSThunk t args
	read (NILFSThunk t args) = Inc.read t

instance (Output U l (IncForest NILFS),ForestLayer NILFS l) => Thunk (ICThunk NILFS) l (IncForest NILFS) where
	new = liftM NILFSU . thunk
	read (NILFSU t) = force t

instance (Output U Outside (IncForest NILFS),ForestLayer NILFS Outside) => Output (ICThunk NILFS) Outside (IncForest NILFS) where
	thunk = liftM NILFSU . thunk
	force (NILFSU t) = force t

instance (Output U Inside (IncForest NILFS),ForestLayer NILFS Inside) => Output (ICThunk NILFS) Inside (IncForest NILFS) where
	thunk = liftM NILFSU . thunk
	force (NILFSU t) = force t
	memo rec = liftM NILFSU . Inc.memo (\f -> rec (liftM NILFSU . f))
	gmemoQ ctx (f :: (GenericQMemoNILFSU ctx Inside (IncForest NILFS) b -> GenericQMemoNILFSU ctx Inside (IncForest NILFS) b)) =
		let memo_func :: GenericQMemoNILFSU ctx Inside (IncForest NILFS) b
		    memo_func = gmemoNonRecNILFSU ctx (f memo_func)
		in memo_func
	gmemoQAs ctx name (f :: (GenericQMemoNILFSU ctx Inside (IncForest NILFS) b -> GenericQMemoNILFSU ctx Inside (IncForest NILFS) b)) =
		let memo_func :: GenericQMemoNILFSU ctx Inside (IncForest NILFS) b
		    memo_func = gmemoNonRecNILFSUAs ctx name (f memo_func)
		in memo_func

type GenericQMemoNILFSU ctx l inc b = GenericQMemo ctx (ICThunk NILFS) l inc b
type NewGenericQMemoNILFSU ctx l inc b = NewGenericQMemo ctx (ICThunk NILFS) l inc b

-- we just repeat the code from adapton here as workaround. revise this!
gmemoNonRecNILFSU :: (Typeable ctx,Typeable b) => Proxy ctx -> GenericQMemoNILFSU ctx Inside (IncForest NILFS) b -> GenericQMemoNILFSU ctx Inside (IncForest NILFS) b
gmemoNonRecNILFSU ctx f = liftM NILFSU . gmemoNonRecU ctx (liftM adaptonU . f)

gmemoNonRecNILFSUAs :: (Memo name,Typeable ctx,Typeable b) => Proxy ctx -> name -> GenericQMemoNILFSU ctx Inside (IncForest NILFS) b -> GenericQMemoNILFSU ctx Inside (IncForest NILFS) b
gmemoNonRecNILFSUAs ctx name f = liftM NILFSU . gmemoNonRecUAs ctx name (liftM adaptonU . f)

instance ForestInput NILFS FSThunk Inside where
	fsRef tree v = do
		a <- Inc.ref v
		f <- forestM $ forestIO $ newIORef' Nothing
		return $ NILFSFSThunk a f
	fsThunk tree m = do
		t <- do
			a <- Inc.mod m
			f <- forestM $ forestIO $ newIORef' Nothing
			return $ NILFSFSThunk a f
		return t
	fsSet tree t v' = do
		Inc.set (adaptonThunk t) v'
	fsModify tree t f = do
		Inc.modify (adaptonThunk t) f
	fsOverwrite tree t m = do
		Inc.overwrite (adaptonThunk t) m
	fsForce t = Inc.get $ adaptonThunk t

instance ForestInput NILFS FSThunk Outside where
	fsRef tree v = do
		a <- Inc.ref v
		f <- forestM $ forestIO $ newIORef' Nothing
		return $ NILFSFSThunk a f
	fsThunk tree m = do
		t <- do
			a <- Inc.mod m
			f <- forestM $ forestIO $ newIORef' Nothing
			return $ NILFSFSThunk a f
		return t
	fsSet tree t v' = do
		Inc.set (adaptonThunk t) v'
	fsModify tree t f = do
		Inc.modify (adaptonThunk t) f
	fsOverwrite tree t m = do
		Inc.overwrite (adaptonThunk t) m
	fsForce t = Inc.get $ adaptonThunk t

idNILFSFSThunk :: ForestFSThunk NILFS l a -> ThunkId
idNILFSFSThunk t = Adapton.idNM $ Adapton.metaL $ adaptonThunk t

-- * inner Forest state

type ThunkId = Unique

-- the forest temporary directory, the root path being monitored, and its corresponding device
type ForestData = (FilePath :!: NILFSData)

latestNILFSCheckpoint :: IO (Snapshot,UTCTime,Bool)
latestNILFSCheckpoint = do
	(snapStr:dateStr:timeStr:isSnapStr:_) <- liftM words $ runShellCommand "lscp -r | sed -n '2 p'"
	snapTime <- readNILFSTime (dateStr++' ':timeStr)
	let isSnap = case isSnapStr of { "ss" -> True; "cp" -> False }
	
	return (Prelude.read snapStr,snapTime,isSnap)

-- mounts a NILFS snapshot, adding it to the live snapshots, and making sure that it is actually a NILFS snapshot (not a checkpoint)
mountSnapshotM :: Snapshot -> Stone -> ForestM NILFS ()
mountSnapshotM snapshot stone = do
	(forestDir :!: nilfsData) <- forestIO $ readIORef' forestData
	mountpoint <- mountSnapshot liveSnapshots nilfsData snapshot (MkWeak $ mkWeakKey stone)
	forestIO $ mkWeakKey stone () $ Just $ unmountSnapshot' liveSnapshots snapshot mountpoint
	return ()

testAndMountSnapshot :: Snapshot -> Stone -> ForestM NILFS ()
testAndMountSnapshot snapshot stone = do
	(forestDir :!: ((rootPath,rootFolder),device)) <- forestIO $ readIORef' forestData
	mb <- forestIO $ CWeakMap.lookup liveSnapshots snapshot
	case mb of
		Just ref -> return ()
		Nothing -> mountSnapshotM snapshot stone

			
unmountSnapshot' :: LiveSnapshots -> Snapshot -> MountPoint -> IO ()
unmountSnapshot' liveSnapshots snapshot mountpoint = do
	CWeakMap.delete liveSnapshots snapshot
	sudoShellCommand_ $ "umount "++mountpoint
	removeDirectory mountpoint
	sudoShellCommand_ $ "chcp cp "++show snapshot
	return ()

unmountAllSnapshots :: LiveSnapshots -> ForestData -> IO ()
unmountAllSnapshots liveSnapshots (forestDir :!: ((rootPath,rootFolder),device)) = do
	let allSnapshots = forestDir </> "Snapshots" </> "*"
	test <- doesDirectoryExistShell allSnapshots
	when test $ sudoShellCommand_ ("umount "++ allSnapshots) >> return ()
	return ()

-- * Connection to the event notifier

runNILFSForest :: ForestCfg NILFS -> ForestO NILFS a -> IO a
runNILFSForest cfg (NILFSForestO m) = do
	(writeIORef Adapton.adaptonParams $! incParams cfg)
	-- create temporary Forest directory
	
	createForestData cfg >>= writeIORef' forestData
	
	-- initialize FS monitor in a separate thread and run the computation (note that the monitor never ends by itself, so it always loses the race)
	finally (m) (removeForestData cfg)

createForestData :: ForestCfg NILFS -> IO ForestData
createForestData (NILFSForestConfig rootPathFolder forestDir incParams) = do
	mountAVFS
	createDirectoryIfMissing True forestDir
	createDirectoryIfMissing True (forestDir </> "Snapshots")
	root <- findRootDevice rootPathFolder
	return (forestDir :!: root)

-- catch any exception that this may generate
removeForestData :: ForestCfg NILFS -> IO ()
removeForestData (NILFSForestConfig rootPath forestDir adaptonParams) = flip finally (return ()) $ do
	forestData <- readIORef forestData
	putStrLn "removing forest data..."
	CWeakMap.unsafeMapM_ (\(snapshot,ref) -> readIORef ref >>= unmountSnapshot' liveSnapshots snapshot)  liveSnapshots
	unmountAllSnapshots liveSnapshots forestData-- just to make sure that there are no mounts left
	removeDirectoryRecursive forestDir
	unmountAVFS
	return ()

latestNILFSTree :: IO (FSTree NILFS)
latestNILFSTree = do
	(snapshot,snapshotTime,_) <- latestNILFSCheckpoint
	stone <- newIORef' ()
	return $ NILFSTree snapshot snapshotTime stone

makeNewNILFSCheckpoint :: Bool -> IO Int
makeNewNILFSCheckpoint isSS = do
	let mkSS = if isSS then " -s" else ""
	liftM (Prelude.read) $ sudoShellCommand $ "mkcp -p" ++ mkSS

proxyNILFS :: Proxy NILFS
proxyNILFS = Proxy

instance (Typeable l,Typeable a,Memo (L l Adapton a)) => Memo (FSThunk NILFS l (IncForest NILFS) a) where
	type Key (FSThunk NILFS l (IncForest NILFS) a) = Key (L l Adapton a)
	{-# INLINE memoKey #-}
	memoKey = memoKey . adaptonThunk
	memoWeak = memoWeak . adaptonThunk

instance Hashable (L l Adapton a) => Hashable (FSThunk NILFS l (IncForest NILFS) a) where
	hashWithSalt i = hashWithSalt i . adaptonThunk

instance (Typeable l,Typeable a,Memo (U l Adapton a)) => Memo (ICThunk NILFS l (IncForest NILFS) a) where
	type Key (ICThunk NILFS l (IncForest NILFS) a) = Key (U l Adapton a)
	{-# INLINE memoKey #-}
	memoKey = memoKey . adaptonU
	memoWeak = memoWeak . adaptonU

instance Hashable (U l Adapton a) => Hashable (ICThunk NILFS l (IncForest NILFS) a) where
	hashWithSalt i = hashWithSalt i . adaptonU

instance WeakRef (FSThunk NILFS l (IncForest NILFS)) where
	mkWeakRefKey t v f = mkWeakRefKey (Adapton.dataL $ adaptonThunk t) v f

getSymlinks :: Maybe (Snapshot,FSDeltas) -> Snapshot -> MkWeak -> ForestM NILFS FSTreeSym
getSymlinks mb_old snap mkWeak = do
	forestDir :!: nilfsData@((rootPath,rootFolder),_) <- forestIO $ readIORef' forestData
	let root = rootPath </> rootFolder
	path <- pathInSnapshot liveSnapshots nilfsData root snap mkWeak	
	let compute = case mb_old of
		Nothing -> computeSymlinks path
		Just (old_snap,td) -> do
			mb <- CWeakMap.lookup symlinks old_snap
			case mb of
				Nothing -> computeSymlinks path
				Just old_syms -> return $ appendListToFSTreeSym td old_syms
	forestIO $ CWeakMap.lookupOrInsertMkWeak symlinks snap compute (\v -> mkWeak)

computeSymlinks :: FilePath -> IO FSTreeSym
computeSymlinks path = do
	putStrLn $ "Finding all symbolic links under... " ++ show path
	syms <- findSymLinks path
	putStrLn $ "Symbolic links calculated for " ++ show path
	return syms

TH.declareIORef "callstackNILFS"  [t| CallStack (IncForest NILFS) |] [e| Strict.Nil |]

instance AdaptonImpl (IncForest NILFS) where
	callstack = callstackNILFS
	
instance (Layer Inside (IncForest NILFS)) => Thunk M Inside (IncForest NILFS) where
	new = modInnerM
	{-# INLINE new #-}
	newc = refInnerM
	{-# INLINE newc #-}
	read = getInnerM
	{-# INLINE read #-}

instance (Layer Outside (IncForest NILFS)) => Thunk M Outside (IncForest NILFS) where
	new = modOuterM
	{-# INLINE new #-}
	newc = refOuterM
	{-# INLINE newc #-}
	read = getOuterM
	{-# INLINE read #-}

instance (Layer Inside (IncForest NILFS)) => Input M Inside (IncForest NILFS) where
	ref = refInnerM
	{-# INLINE ref #-}
	get = getInnerM
	{-# INLINE get #-}
	set = setM
	{-# INLINE set #-}
	getOutside = getOuterM
	{-# INLINE getOutside #-}
	refOutside = refOuterM
	{-# INLINE refOutside #-}
	modOutside = \c -> outside c >>= refOutside
	{-# INLINE modOutside #-}

instance (Layer Outside (IncForest NILFS)) => Input M Outside (IncForest NILFS) where
	ref = refOuterM
	{-# INLINE ref #-}
	get = getOuterM
	{-# INLINE get #-}
	set = setM
	{-# INLINE set #-}
	refOutside = refOuterM
	{-# INLINE refOutside #-}
	modOutside = \c -> c >>= refOutside
	{-# INLINE modOutside #-}
	
instance (Layer Inside (IncForest NILFS)) => Thunk L Inside (IncForest NILFS) where
	new = modL
	{-# INLINE new #-}
	newc = refL
	{-# INLINE newc #-}
	read = getInnerL
	{-# INLINE read #-}

instance (Layer Outside (IncForest NILFS)) => Thunk L Outside (IncForest NILFS) where
	new = modL
	{-# INLINE new #-}
	newc = refL
	{-# INLINE newc #-}
	read = getOuterL
	{-# INLINE read #-}

instance (Layer Inside (IncForest NILFS)) => Input L Inside (IncForest NILFS) where
	ref = refL
	{-# INLINE ref #-}
	mod = modL
	{-# INLINE mod #-}
	get = getInnerL
	{-# INLINE get #-}
	set = setL
	{-# INLINE set #-}
	overwrite = overwriteL
	{-# INLINE overwrite #-}
	modify = modifyL
	{-# INLINE modify #-}
	getOutside = inside . getNoDependentsInnerL
	{-# INLINE getOutside #-}

instance (Layer Outside (IncForest NILFS)) => Input L Outside (IncForest NILFS) where
	ref = refL
	{-# INLINE ref #-}
	mod = modL
	{-# INLINE mod #-}
	get = getOuterL
	{-# INLINE get #-}
	set = setL
	{-# INLINE set #-}
	overwrite = overwriteL
	{-# INLINE overwrite #-}
	modify = modifyL
	{-# INLINE modify #-}	

instance (Layer Inside (IncForest NILFS)) => Thunk U Inside (IncForest NILFS) where
	new = thunkU
	{-# INLINE new #-}
	newc = constU
	{-# INLINE newc #-}
	read = forceInnerU
	{-# INLINE read #-}

instance (Layer Outside (IncForest NILFS)) => Thunk U Outside (IncForest NILFS) where
	new = thunkU
	{-# INLINE new #-}
	newc = constU
	{-# INLINE newc #-}
	read = forceOuterU
	{-# INLINE read #-}

instance (Layer Outside (IncForest NILFS)) => Output U Outside (IncForest NILFS) where
	thunk = thunkU
	{-# INLINE thunk #-}
	const = constU
	{-# INLINE const #-}
	force = forceOuterU
	{-# INLINE force #-}
	forceOutside = forceOuterU
	{-# INLINE forceOutside #-}

instance (Layer Inside (IncForest NILFS)) => Output U Inside (IncForest NILFS) where
	thunk = thunkU
	{-# INLINE thunk #-}
	const = constU
	{-# INLINE const #-}
	force = forceInnerU
	{-# INLINE force #-}
	forceOutside = world . forceNoDependentsU
	{-# INLINE forceOutside #-}
	memo = memoU
	{-# INLINE memo #-}
	memoAs = memoUAs
	{-# INLINE memoAs #-}
	gmemoQ = gmemoQU
	{-# INLINE gmemoQ #-}
	
-- * Read-only Forest interface (if instead of NILFS we used a transactional filesystem, we could perform writes consistently)

-- performs arbitrary filesystem changes, and creates a new snapshot at the end
changeFS :: IO a -> FTM NILFS a
changeFS io = forestM $ forestIO $ do
	a <- io
	(_ :!: nilfsData) <- readIORef forestData
	newCheckpoint nilfsData
	return a

instance Forest NILFS where
	new = newNILFS Proxy
	args = argsNILFS Proxy
	read = readNILFS Proxy
	writeOrElse = error "no support for writes"
	delete = error "no support for writes"
	copyOrElse = error "no support for writes"
	
newNILFS :: FTK NILFS rep => Proxy (ForestArgs rep) -> ForestVs (ForestArgs rep) -> FilePath -> FTM NILFS rep
newNILFS proxy args path = inside $ zload (vmonadArgs proxyNILFS proxy args) path

-- incrementally repair the thunk to the latest tree
loadNILFS :: (ForestLayer NILFS l,FTK NILFS rep) => Proxy (ForestArgs rep) -> rep -> ForestL NILFS l ()
loadNILFS proxy rep = do
	let t = to (iso_rep_thunk proxyNILFS) rep
	let uid = idNM $ metaL $ adaptonThunk t
	(txargs :!: path :!: mb_oldtree) <- liftM (fromJustNote "loadNILFS1") $ forestM $ getFTVArgsNILFS proxy rep
	tree <- forestM latestTree
	let oldtree = maybe tree id mb_oldtree
	df <- liftM (fromJustNote "loadNILFS2") $ forestM $ diffFS oldtree tree path
	-- load incrementally at the latest tree
	inside $ unsafeWorld $ debug ("loadNILFS " ++ show path ++" "++ show uid) $ do
		let dv = case mb_oldtree of
			Just _ -> ValueDeltaNILFS True -- users cannot manually change values
			Nothing -> ValueDeltaNILFS False -- the old snapshot is not consistent
		ds <- deltaArgs oldtree proxy txargs txargs
		let deltas = (txargs,ds)
		-- should be fine, since it does not update thunks that have been visited before (they are already at the latest tree)
		zloadDeltaMemo deltas (return path) oldtree (rep,IC.getForestMDInTree) path df tree dv
	return ()
			

-- read a transactional variable
readNILFS :: (ForestLayer NILFS l,FTK NILFS rep) => Proxy (ForestArgs rep) -> rep -> ForestL NILFS l (ForestContentTy NILFS (ForestRepTy NILFS rep))
readNILFS proxy rep = do
	loadNILFS proxy rep
	let t = to (iso_rep_thunk proxyNILFS) rep
	tree <- inside $ fsTree t
	inside $ runReaderT (BX.getM lens_content $ lift $ fsForce t) tree

argsNILFS :: (FTK NILFS rep) => Proxy (ForestArgs rep) -> rep -> ForestI NILFS (ForestVs (ForestArgs rep),FilePath)
argsNILFS proxy rep = do
	mb <- forestM $ getFTVArgsNILFS proxy rep
	case mb of
		Nothing -> error "should not happen"
		Just (margs :!: path :!: tree) -> do
			args <- vArgs Proxy proxy margs
			return (args,path)

getFTVArgsNILFS :: (FTK NILFS rep) => Proxy (ForestArgs rep) -> rep -> ForestM NILFS (Maybe (ForestIs NILFS (ForestArgs rep) :!: FilePath :!: Maybe (FSTree NILFS)))
getFTVArgsNILFS (proxy ::Proxy args) rep = forestIO $ do
	let var = to (iso_rep_thunk proxyNILFS) rep
	mb <- readIORef (nilfsFSThunkArgs var)
	case mb of
		Just ((fromDynamic -> Just args) :!: path :!: tree) -> return $ Just (args :!: path :!: tree)
		otherwise -> return Nothing

data DynamicNILFS where
	DynNILFS :: FTK NILFS rep => rep -> DynamicNILFS

type NILFSMemoTable = MemoTable (FilePath,TypeRep) (DynamicNILFS :!: FSTree NILFS)

{-# NOINLINE nilfsMemoTable #-}
nilfsMemoTable :: NILFSMemoTable
nilfsMemoTable = unsafePerformIO $ MemoTable.newSized (10^3)

instance ZippedICMemo NILFS where
	addZippedMemo path args rep mb_tree = do
		let var = to (iso_rep_thunk proxyNILFS) rep
		debug ("added memo "++show path ++ " "++ show (idNM $ metaL $ adaptonThunk var) ++" "++ show (typeOf rep)) $ do
			
			-- remember the arguments (this is how we connect transactional arguments to transactional variables)
			let txargs = (toDyn args :!: path :!: mb_tree)
			forestM $ forestIO $ writeIORef (nilfsFSThunkArgs var) $ Just txargs
			
			-- memoize the entry
			case mb_tree of
				Nothing -> return ()
				Just tree -> do
					
					let mkWeak = MkWeak $ mkWeakRefKey $ nilfsFSThunkArgs var
					forestM $ forestIO $ MemoTable.insertWithMkWeak nilfsMemoTable (path,typeOf rep) (DynNILFS rep :!: tree) mkWeak
	
	findZippedMemo path (proxy :: Proxy rep) = debug ("finding "++show path ++" "++ show (typeOf (undefined::rep))) $ do
		mb <- forestM $ forestIO $ MemoTable.lookup nilfsMemoTable (path,typeOf (undefined :: rep))
		case mb of
			Nothing -> return Nothing
			Just ((fromDynNILFS -> Just rep) :!: tree) -> do
				stree <- forestM $ showFSTree tree
				debug ("found memo " ++ show path ++ " " ++ stree) $ do
					let var = to (iso_rep_thunk proxyNILFS) rep
					Just ((fromDynamic -> Just txargs) :!: _ :!: _) <- forestM $ forestIO $ readIORef $ nilfsFSThunkArgs var
					return $ Just (tree,txargs,rep)


fromDynNILFS :: FTK NILFS rep => DynamicNILFS -> Maybe rep
fromDynNILFS (DynNILFS v) = cast v


-- for NILFS thunks, we also print IC information
instance (Input L l (IncForest NILFS),ForestInput NILFS FSThunk l,Eq a,MData (DrawDict (IncForest NILFS)) (ForestO NILFS) a) => Draw (IncForest NILFS) (ForestFSThunk NILFS l a) where
	draw inc = draw inc . adaptonThunk

-- for NILFS thunks, we also print IC information
instance (Output U l (IncForest 'NILFS),ForestOutput NILFS ICThunk l,Eq a,MData (DrawDict (IncForest NILFS)) (ForestO NILFS) a) => Draw (IncForest NILFS) (ForestICThunk NILFS l a) where
	draw inc = draw inc . adaptonU