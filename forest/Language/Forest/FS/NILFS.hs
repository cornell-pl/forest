{-# LANGUAGE ScopedTypeVariables, ConstraintKinds, FlexibleInstances, MultiParamTypeClasses, StandaloneDeriving, GeneralizedNewtypeDeriving, FlexibleContexts, DataKinds, TypeFamilies, Rank2Types, GADTs, ViewPatterns, DeriveDataTypeable #-}

module Language.Forest.FS.NILFS where

import Control.Monad.Incremental.Adapton.Memo
import Filesystem.Path.CurrentOS hiding (FilePath,concat,(</>))
import Language.Forest.FS.FSDelta
import Language.Forest.IO.Utils
--import System.FSNotify
import Language.Forest.FS.FSRep
import Control.Monad
--import Control.Monad.IO.Class
import Control.Concurrent.MVar
import Control.Concurrent
import Language.Forest.IO.Memo
import Control.Exception
--import Control.Concurrent.Async
import System.Mem.WeakRef
--import Control.Lens hiding (runIdentity,inside)
import Data.Maybe
import Data.Hashable
import Data.List
import Data.WithClass.MData
--import Data.Functor.Identity
import Data.List.Split
import Language.Forest.MetaData
import Data.Typeable
import System.Directory
import System.IO.Unsafe
import Data.List as List
import Data.IORef
--import Data.Dequeue as Queue
import Data.Unique
import Data.Time.Clock
import System.IO
import System.Mem.WeakTable (WeakTable(..))
import qualified System.Mem.WeakTable as WeakTable
import Data.Map (Map(..))
import qualified Data.Map as Map
import Data.Set (Set(..))
import qualified Data.Set as Set
import Data.String
import qualified Data.Text as Text
import Data.Text (Text(..))
import System.FilePath
import Safe

import Control.Monad.Incremental.Adapton (GenericQMemoU(..),L(..),U(..),Inner(..),Outer(..),Adapton,runInner,runOuter,memoU,gmemoQU,addFinalizerU)
import qualified Control.Monad.Incremental.Adapton as Adapton
import Control.Monad.Incremental as Inc
import System.Process
import Language.Forest.Shell
--import Language.Forest.FS.FSNotify
import Control.Monad.Trans
import Control.Monad.Ref
import Control.Monad.Incremental as Inc
import Data.Time.LocalTime
import Language.Forest.FS.NILFSDiff
import System.Mem.MemoTable
import Control.Monad.Lazy

instance LiftInc Inside Adapton (IncForest NILFS) IORef IO where
	liftInc = NILFSForestI
instance LiftInc Outside Adapton (IncForest NILFS) IORef IO where
	liftInc = NILFSForestO

instance Incremental (IncForest NILFS) IORef IO where
	
	newtype Outside (IncForest NILFS) IORef IO a = NILFSForestO { adaptonOuter :: Outer IORef IO a } deriving (Monad,MonadLazy)
	newtype Inside (IncForest NILFS) IORef IO a = NILFSForestI { adaptonInner :: Inner IORef IO a } deriving (Monad,MonadLazy)
	
	world = NILFSForestO . inside . adaptonInner
	
	runIncremental m = getNILFSCfg >>= \cfg -> runForest cfg m
	
instance InLayer Outside (IncForest NILFS) IORef IO where
	inL = NILFSForestO . Adapton.Outer
	{-# INLINE inL #-}
instance InLayer Inside (IncForest NILFS) IORef IO where
	inL = NILFSForestI . Adapton.Inner
	{-# INLINE inL #-}

nilfsTreeTime :: FSTree NILFS -> UTCTime
nilfsTreeTime (NILFSTree _ t) = t
nilfsTreeTime (VirtualNILFSTree _ t) = t

nilfsTreeSnapshot :: FSTree NILFS -> Snapshot
nilfsTreeSnapshot (NILFSTree s t) = s
nilfsTreeSnapshot (VirtualNILFSTree s t) = s

instance Ord (FSTree NILFS) where
	compare t1 t2 = compare (nilfsTreeSnapshot t1) (nilfsTreeSnapshot t2)
instance Eq (FSTree NILFS) where
	t1 == t2 = (nilfsTreeSnapshot t1) == (nilfsTreeSnapshot t2)

-- Log-based filesystem with support for snapshots
-- FSTree operations can be treated as pure, since NILFS snapshots are stable throughout the run of a program; although in reality old anpshots may be purged?
-- our current Forest state is stored in an IORef, so it is not thread-safe!
instance FSRep NILFS where
	
	-- the forestdir needs to be a directory outside of the logged filesystem
	data ForestCfg NILFS = NILFSForestConfig { rootPath :: FilePath, forestDir :: FilePath }
	
	runForest = runNILFSForest
	
	forestIO = inside . liftAdaptonNILFS . inL . liftIO where
		liftAdaptonNILFS :: Inside Adapton IORef IO a -> Inside (IncForest NILFS) IORef IO a
		liftAdaptonNILFS = liftInc
	
	unsafeInside = NILFSForestI . Adapton.Inner . runOuter . adaptonOuter
	
	getForestDirectory = forestIO $ liftM fst $ readIORef forestData
	
	data FSTree NILFS = NILFSTree Snapshot UTCTime | VirtualNILFSTree Snapshot UTCTime deriving (Show)

	virtualTree (NILFSTree ss time) = return $ VirtualNILFSTree ss time
	virtualTree (VirtualNILFSTree ss time) = return $ VirtualNILFSTree ss time

--	latestTreeIO fs = latestNILFSTree
	latestTree = forestIO $ latestNILFSTree >>= \tree@(NILFSTree snap time) -> testAndMountSnapshot snap >> return tree
	changesBetween = changesBetweenNILFS
	pathInTree path tree@(NILFSTree snapshot time) = do
		mb <- forestIO $ WeakTable.lookup liveSnapshots snapshot
		case mb of
			Just ref -> do
				(mountpoint,_) <- forestIO $ readIORef ref
				(forestDir,((rootPath,rootFolder),device)) <- forestIO $ readIORef forestData
				let result = mountpoint </> makeRelative rootPath path
				return $ {-debug ("pathInTree: "++show path ++ " " ++ show (snapshot,mountpoint,rootPath) ++ " -> " ++ show result) -} result
			Nothing -> forestIO (mountSnapshot snapshot) >> pathInTree path tree
	pathInTree path tree@(VirtualNILFSTree snapshot time) = do
		mb <- forestIO $ WeakTable.lookup liveSnapshots snapshot
		case mb of
			Just ref -> do
				(mountpoint,_) <- forestIO $ readIORef ref
				(forestDir,((rootPath,rootFolder),device)) <- forestIO $ readIORef forestData
				home <- forestIO $ getHomeDirectory
				let result = home </> ".avfs" </> makeRelative "/" (mountpoint </> makeRelative rootPath path)
				return $ {-debug ("pathInTree: "++show path ++ " " ++ show (snapshot,mountpoint,rootPath) ++ " -> " ++ show result) -} result
			Nothing -> forestIO (mountSnapshot snapshot) >> pathInTree path tree
	pathFromTree path tree@(NILFSTree snapshot time) = do
		mb <- forestIO $ WeakTable.lookup liveSnapshots snapshot
		case mb of
			Just ref -> do
				(mountpoint,_) <- forestIO $ readIORef ref
				(forestDir,((rootPath,rootFolder),device)) <- forestIO $ readIORef forestData
				return $ rootPath </> makeRelative mountpoint path
			Nothing -> forestIO (mountSnapshot snapshot) >> pathFromTree path tree
	pathFromTree path tree@(VirtualNILFSTree snapshot time) = do
		mb <- forestIO $ WeakTable.lookup liveSnapshots snapshot
		case mb of
			Just ref -> do
				(mountpoint,_) <- forestIO $ readIORef ref
				(forestDir,((rootPath,rootFolder),device)) <- forestIO $ readIORef forestData
				home <- forestIO $ getHomeDirectory
				return $ rootPath </> makeRelative (home </> ".avfs" </> makeRelative "/" mountpoint) path
			Nothing -> forestIO (mountSnapshot snapshot) >> pathFromTree path tree
	stepPathInTree tree path rel = do
		cpath <- canonalizePathWithTree path tree
		let newpath = path </> rel
		cnewpath <- canonalizePathWithTree newpath tree
		if cpath `isParentPathOf` cnewpath
			then return newpath 
			else error $ "NILFS incremental loading forbids non-tree-like filesystems" ++ show cpath ++ " " ++ show newpath
	
	data FSThunk NILFS l inc r m a = NILFSFSThunk { snapshots :: r [FSTree NILFS], adaptonThunk :: (L l (IncForest NILFS) r m a), unmemoNILFS :: r (IO ()) } -- a set of snapshots on which the FSThunk depends and a lazy Adapton modifiable
		
	eqFSThunk t1 t2 = adaptonThunk t1 == adaptonThunk t2
	fsthunkID = Adapton.idNM . Adapton.metaL . adaptonThunk
	isUnevaluatedFSThunk t = Adapton.isUnevaluatedL (adaptonThunk t)
	isUnforcedFSThunk t = Adapton.isUnforcedL (adaptonThunk t)
		
	showFSTree = show
	
--	compareTreeWithTime tree time = compare (nilfsTreeTime tree) time
	
	newtype ICThunk NILFS l inc r m a = NILFSU { adaptonU :: U l (IncForest NILFS) r m a }

	eqICThunk t1 t2 = adaptonU t1 == adaptonU t2
	icthunkID = Adapton.idNM . Adapton.metaU . adaptonU
	
	isUnevaluatedICThunk t = Adapton.isUnevaluatedU (adaptonU t)
	
	newtype HSThunk NILFS l inc r m a = NILFSThunk { unNILFSThunk :: T l inc r m a }

	memo = memoForest
	unmemo _ = unmemoForest
	lookupmemo = lookupmemoForest

instance ForestLayer NILFS l => Thunk (HSThunk NILFS) l (IncForest NILFS) IORef IO where
	new = liftM NILFSThunk . new
	read (NILFSThunk t) = Inc.read t

instance (Output U l (IncForest 'NILFS) IORef IO,ForestLayer NILFS l) => Thunk (ICThunk NILFS) l (IncForest NILFS) IORef IO where
	new = liftM NILFSU . thunk
	read (NILFSU t) = force t

instance (Output U Outside (IncForest 'NILFS) IORef IO,ForestLayer NILFS Outside) => Output (ICThunk NILFS) Outside (IncForest NILFS) IORef IO where
	thunk = liftM NILFSU . thunk
	force (NILFSU t) = force t

instance (Output U Inside (IncForest 'NILFS) IORef IO,ForestLayer NILFS Inside) => Output (ICThunk NILFS) Inside (IncForest NILFS) IORef IO where
	thunk = liftM NILFSU . thunk
	force (NILFSU t) = force t
	memo rec = liftM NILFSU . Inc.memo (\f -> rec (liftM NILFSU . f))
	gmemoQ ctx (f :: (GenericQMemoNILFSU ctx Inside (IncForest NILFS) IORef IO b -> GenericQMemoNILFSU ctx Inside (IncForest NILFS) IORef IO b)) =
		let memo_func :: GenericQMemoNILFSU ctx Inside (IncForest NILFS) IORef IO b
		    memo_func = gmemoNonRecNILFSU ctx (f memo_func)
		in memo_func

type GenericQMemoNILFSU ctx l inc r m b = GenericQMemo ctx (ICThunk NILFS) l inc r m b
type NewGenericQMemoNILFSU ctx l inc r m b = NewGenericQMemo ctx (ICThunk NILFS) l inc r m b

-- we just repeat the code from adapton here as workaround. revise this!
gmemoNonRecNILFSU :: Proxy ctx -> GenericQMemoNILFSU ctx Inside (IncForest NILFS) IORef IO b -> GenericQMemoNILFSU ctx Inside (IncForest NILFS) IORef IO b
gmemoNonRecNILFSU ctx f = unNewGenericQ (newGmemoNonRecNILFSU ctx (NewGenericQ f)) where
	newGmemoNonRecNILFSU ctx f = gmemoNonRecNILFSU' ctx f (unsafePerformIO $ debug "NewTable!!" $ WeakTable.newFor f)

gmemoNonRecNILFSU' :: Proxy ctx -> NewGenericQMemoNILFSU ctx Inside (IncForest NILFS) IORef IO b -> MemoTable (TypeRep,KeyDynamic) (U Inside (IncForest NILFS) IORef IO b) -> NewGenericQMemoNILFSU ctx Inside (IncForest NILFS) IORef IO b
gmemoNonRecNILFSU' ctx (NewGenericQ f) tbl = NewGenericQ $ \arg -> do
	let (mkWeak,k) = memoKeyCtx dict ctx $! arg
	let tyk = (typeRepOf arg,keyDynamicCtx dict ctx (proxyOf arg) k)
	lkp <- debug ("memo search "++show tyk) $ inL $ liftIO $ WeakTable.lookup tbl tyk
	case lkp of
		Nothing -> do
			NILFSU thunk <- f arg
			inL $ liftIO $ WeakTable.insertWithMkWeak tbl mkWeak tyk thunk
			debug (show tyk ++" => "++show thunk) $ return $ NILFSU thunk
		Just thunk -> debug ("memo hit "++show tyk ++ " " ++ show thunk) $ return $ NILFSU thunk

instance ForestInput NILFS FSThunk Inside where
	fsref v = do
		deps <- forestIO $ newIORef []
		a <- Inc.ref v
		f <- forestIO $ newIORef (return ())
		return $ NILFSFSThunk deps a f
	fsthunk trees m = do
		t <- do
			deps <- forestIO $ newIORef trees
			a <- Inc.mod m
			f <- forestIO $ newIORef (return ())
			return $ NILFSFSThunk deps a f
		forestIO $ addDependentThunk trees t
		return t
	fsset t v' = do
		oldtrees <- forestIO $ readIORef (snapshots t)
		forestIO $ removeDependentThunk oldtrees t
		Inc.set (adaptonThunk t) v'
		f <- forestIO $ readIORef $ unmemoNILFS t
		forestIO f
	fsmodify trees t f = do
		forestIO $ modifyIORef (snapshots t) $ List.union trees
		Inc.modify (adaptonThunk t) f
		f <- forestIO $ readIORef $ unmemoNILFS t
		forestIO f
		forestIO $ addDependentThunk trees t
	fsoverwrite trees t m = do
		oldtrees <- forestIO $ readIORef (snapshots t)
		forestIO $ removeDependentThunk oldtrees t
		forestIO $ writeIORef (snapshots t) trees
		Inc.overwrite (adaptonThunk t) m
		f <- forestIO $ readIORef $ unmemoNILFS t
		forestIO f
		forestIO $ addDependentThunk trees t
	fsforce t = do
		forestIO $ do
			oldtrees <- readIORef (snapshots t)
			removeDependentThunk oldtrees t
			writeIORef (snapshots t) []
		Inc.get $ adaptonThunk t
	
	addUnmemoFSThunk t f = inL $ liftIO $ modifyIORef (unmemoNILFS t) (>> f)

instance ForestInput NILFS FSThunk Outside

fsforceIO :: (Eq a,Input L l (IncForest NILFS) IORef IO) => ForestFSThunk NILFS l a -> IO a
fsforceIO t = do
--	putStrLn $ "forcing thunk " ++ show (hashUnique (fsthunkID t))
	oldtrees <- readIORef (snapshots t)
	removeDependentThunk oldtrees t
	
	writeIORef (snapshots t) []
	v <- runIncremental $ Inc.getOutside $ adaptonThunk t
--	putStrLn $ "finished forcing " ++ show (hashUnique $ Adapton.idNM $ Adapton.metaL $ adaptonThunk t)
	return v

idNILFSFSThunk :: ForestFSThunk NILFS l a -> ThunkId
idNILFSFSThunk t = Adapton.idNM $ Adapton.metaL $ adaptonThunk t

-- * inner Forest state

type Snapshot = Int
type ThunkId = Unique
type DependentThunks = Map ThunkId (IO ()) -- mapping from thunk ids to a computation that forces its value

-- map from snapshot ids to the filepath where its root directory is mounted on the disk and a record of unevaluated thunks depending on the snapshot
-- a weak table to avoid keeping thunks alive
type LiveSnapshots = WeakTable Snapshot (IORef (MountPoint,DependentThunks))

{-# NOINLINE liveSnapshots #-}
liveSnapshots :: LiveSnapshots
liveSnapshots = unsafePerformIO $ WeakTable.new

-- the forest temporary directory, the root path being monitored, and its corresponding device
{-# NOINLINE forestData #-}
forestData :: IORef (FilePath,((FilePath,FilePath),FilePath))
forestData = unsafePerformIO $ newIORef (error "no directory",((error "no root path",error "no root folder"),error "no device"))

type MountPoint = FilePath

latestNILFSCheckpoint :: IO (Snapshot,UTCTime,Bool)
latestNILFSCheckpoint = do
	(snapStr:dateStr:timeStr:isSnapStr:_) <- liftM words $ runShellCommand "lscp -r | sed -n '2 p'"
	snapTime <- readNILFSTime (dateStr++' ':timeStr)
	let isSnap = case isSnapStr of { "ss" -> True; "cp" -> False }
	
	return (Prelude.read snapStr,snapTime,isSnap)

readNILFSTime :: String -> IO UTCTime
readNILFSTime str = do
	timezone <- getCurrentTimeZone
	let localTime = Prelude.read str
	return $ localTimeToUTC timezone localTime

-- mounts a NILFS snapshot, adding it to the live snapshots, and making sure that it is actually a NILFS snapshot
mountSnapshot :: Snapshot -> IO ()
mountSnapshot snapshot = do
	(forestDir,((rootPath,rootFolder),device)) <- readIORef forestData
	let mountpoint = forestDir </> "Snapshots" </> show snapshot
	sudoShellCommand_ $ "chcp ss "++show snapshot
	createDirectoryIfMissing False mountpoint
	sudoShellCommand_ $ "mount.nilfs2 -r "++device++" "++mountpoint++" -o cp="++show snapshot
	ref <- newIORef (mountpoint,Map.empty)
	WeakTable.insert liveSnapshots snapshot ref

testAndMountSnapshot :: Snapshot -> IO ()
testAndMountSnapshot snapshot = do
	mb <- WeakTable.lookup liveSnapshots snapshot
	case mb of
		Just ref -> return ()
		Nothing -> mountSnapshot snapshot

-- unmounts a NILFS snapshot, forcing all dependent thunks before and making it a regular checkpoint at the end
lookupAndUnmountSnapshot :: Bool -> Snapshot -> IO ()
lookupAndUnmountSnapshot shallForceDeps snapshot = do
	mb <- WeakTable.lookup liveSnapshots snapshot
	case mb of
		Nothing -> return ()
		Just ref -> readIORef ref >>= unmountSnapshot' shallForceDeps snapshot
			
-- unmounts a NILFS snapshot, forcing all dependent thunks before and making it a regular checkpoint at the end
unmountSnapshot' :: Bool -> Snapshot -> (MountPoint,DependentThunks) -> IO ()
unmountSnapshot' shallForceDeps snapshot (mountpoint,deps) = do
	if shallForceDeps then Map.foldl' (>>) (return ()) deps else return ()
	WeakTable.delete liveSnapshots snapshot
	sudoShellCommand_ $ "umount "++mountpoint
	removeDirectory mountpoint
	sudoShellCommand_ $ "chcp cp "++show snapshot
	return ()

unmountAllSnapshots :: IO ()
unmountAllSnapshots = do
	(forestDir,((rootPath,rootFolder),device)) <- readIORef forestData
	let allSnapshots = forestDir </> "Snapshots" </> "*"
	test <- doesDirectoryExistShell allSnapshots
	when test $ sudoShellCommand_ ("umount "++ allSnapshots) >> return ()
	return ()

-- * Thunk operations

addDependentThunk :: (Eq a,Input L l (IncForest NILFS) IORef IO) => [FSTree NILFS] -> ForestFSThunk NILFS l a -> IO ()
addDependentThunk trees t = mapM_ (addDepThunk t) trees where
	addDepThunk :: (Eq a,Input L l (IncForest NILFS) IORef IO) => ForestFSThunk NILFS l a -> FSTree NILFS -> IO ()
	addDepThunk t tree@(NILFSTree snapshot time) = do
		mb <- WeakTable.lookup liveSnapshots snapshot
		case mb of
			Just ref -> modifyIORef ref $ \(mountpoint,deps) -> (mountpoint,Map.insert (idNILFSFSThunk t) (fsforceIO t >> return ()) deps)
			Nothing -> mountSnapshot snapshot >> addDepThunk t tree
	addDepThunk t tree@(VirtualNILFSTree snapshot time) = addDepThunk t (NILFSTree snapshot time)

removeDependentThunk :: Eq a => [FSTree NILFS] -> ForestFSThunk NILFS l a -> IO ()
removeDependentThunk trees t = mapM_  (remDepThunk t) trees where
	remDepThunk :: Eq a => ForestFSThunk NILFS l a -> FSTree NILFS -> IO ()
	remDepThunk t tree@(NILFSTree snapshot time) = do
		mb <- WeakTable.lookup liveSnapshots snapshot
		case mb of
			Just ref -> modifyIORef ref $ \(mountpoint,deps) -> (mountpoint,Map.delete (idNILFSFSThunk t) deps)
			Nothing -> return ()
	remDepThunk t tree@(VirtualNILFSTree snapshot time) = remDepThunk t (NILFSTree snapshot time)

-- * Connection to the event notifier

runNILFSForest :: ForestCfg NILFS -> ForestO NILFS a -> IO a
runNILFSForest cfg m = do
	-- create temporary Forest directory
	createForestData cfg
	
	-- initialize FS monitor in a separate thread and run the computation (note that the monitor never ends by itself, so it always loses the race)
	v <- finally (Adapton.runOuter $ adaptonOuter m) $ removeForestData cfg
--	man <- startManager
--	Right v <- race (runMonitor' proxyNILFS (rootPath cfg) man) (finally (Adapton.runOuter $ adaptonOuter m) $ removeForestData cfg)
	return v

-- | a separate thread that waits for calls and computes the difference between NILFS checkpoints on demand
--runNILFSMonitor :: IO ()
--runNILFSMonitor = do
--	(forestDir,((rootPath,rootFolder),device)) <- readIORef forestData
--	(cp1,cp2) <- readMVar diffCheckpoints
--	td <- diffNILFS rootPath device cp1 cp2
--	putMVar diffTree td
--	runNILFSMonitor -- recurse indefinitely (halting on @readMVar@)

createForestData :: ForestCfg NILFS -> IO ()
createForestData (NILFSForestConfig rootPathFolder forestDir) = do
	mountAVFS
	createDirectoryIfMissing True forestDir
	createDirectoryIfMissing True (forestDir </> "Snapshots")
	rootDevice <- pathDevice rootPathFolder
	rootPath <- devicePath rootDevice
	let rootFolder = makeRelative rootPath rootPathFolder
	
	writeIORef forestData (forestDir,((rootPath,rootFolder),rootDevice))

getNILFSCfg :: IO (ForestCfg NILFS)
getNILFSCfg = do
	(forestDir,((rootPath,rootFolder),rootDevice)) <- readIORef forestData
	return $ NILFSForestConfig (rootPath </> rootFolder) forestDir

-- catch any exception that this may generate
removeForestData :: ForestCfg NILFS -> IO ()
removeForestData (NILFSForestConfig rootPath forestDir) = flip finally (return ()) $ do
	putStrLn "removing forest data..."
	WeakTable.mapM_ (\(snapshot,ref) -> readIORef ref >>= unmountSnapshot' False snapshot) liveSnapshots
	unmountAllSnapshots -- just to make sure that there are no mounts left
	removeDirectoryRecursive forestDir
	unmountAVFS
	return ()

changesBetweenNILFS :: FSTree NILFS -> FSTree NILFS -> ForestO NILFS FSTreeDelta
changesBetweenNILFS tree tree' = do
--	inL $ liftIO $ putMVar diffCheckpoints (nilfsTreeSnapshot tree,nilfsTreeSnapshot tree')
--	td <- inL $ liftIO $ readMVar diffTree
	(forestDir,((rootPath,rootFolder),device)) <- inL $ liftIO $ readIORef forestData
	td <- inL $ liftIO $ diffNILFS rootPath device (nilfsTreeSnapshot tree) (nilfsTreeSnapshot tree')
	let report = "NILFS changes between " ++ show tree ++ " and " ++ show tree' ++ ": " ++ show td
	debug report $ return td

{-# NOINLINE diffCheckpoints #-}
diffCheckpoints :: MVar (Int,Int)
diffCheckpoints = unsafePerformIO $ newEmptyMVar

{-# NOINLINE diffTree #-}
diffTree :: MVar FSTreeDelta
diffTree = unsafePerformIO $ newEmptyMVar

latestNILFSTree :: IO (FSTree NILFS)
latestNILFSTree = do
	(snapshot,snapshotTime,_) <- latestNILFSCheckpoint
	return $ NILFSTree snapshot snapshotTime

makeNewNILFSCheckpoint :: Bool -> IO Int
makeNewNILFSCheckpoint isSS = do
	let mkSS = if isSS then " -s" else ""
	liftM (Prelude.read) $ sudoShellCommand $ "mkcp -p" ++ mkSS

proxyNILFS :: Proxy NILFS
proxyNILFS = Proxy

instance (WeakRef r,Memo (L l Adapton r m a)) => Memo (FSThunk NILFS l (IncForest NILFS) r m a) where
	type Key (FSThunk NILFS l (IncForest NILFS) r m a) = Key (L l Adapton r m a)
	{-# INLINE memoKey #-}
	memoKey = memoKey . adaptonThunk

instance Hashable (L l Adapton r m a) => Hashable (FSThunk NILFS l (IncForest NILFS) r m a) where
	hashWithSalt i = hashWithSalt i . adaptonThunk

instance (WeakRef r,Memo (U l Adapton r m a)) => Memo (ICThunk NILFS l (IncForest NILFS) r m a) where
	type Key (ICThunk NILFS l (IncForest NILFS) r m a) = Key (U l Adapton r m a)
	{-# INLINE memoKey #-}
	memoKey = memoKey . adaptonU

instance Hashable (U l Adapton r m a) => Hashable (ICThunk NILFS l (IncForest NILFS) r m a) where
	hashWithSalt i = hashWithSalt i . adaptonU

instance WeakRef (FSThunk NILFS l (IncForest NILFS) IORef IO) where
	mkWeakWithRefKey t v f = mkWeakWithRefKey (Adapton.dataL $ adaptonThunk t) v f
