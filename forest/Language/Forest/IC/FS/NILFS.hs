{-# LANGUAGE ScopedTypeVariables, ConstraintKinds, FlexibleInstances, MultiParamTypeClasses, StandaloneDeriving, GeneralizedNewtypeDeriving, FlexibleContexts, DataKinds, TypeFamilies, Rank2Types, GADTs, ViewPatterns, DeriveDataTypeable #-}

module Language.Forest.IC.FS.NILFS where

import qualified Prelude
import Prelude hiding (read,mod,const)
import Control.Monad.Incremental.Adapton.Memo
import Filesystem.Path.CurrentOS hiding (FilePath,concat,(</>))
import Language.Forest.IC.FS.FSDelta
import Language.Forest.IO.Utils
--import System.FSNotify
import Language.Forest.FS.FSRep
import Language.Forest.IC.ICRep
import Control.Monad
--import Control.Monad.IO.Class
import Control.Concurrent.MVar
import Control.Concurrent
import Language.Forest.IC.IO.Memo
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
import Language.Forest.Pure.MetaData
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
import Language.Forest.IO.Shell
--import Language.Forest.FS.FSNotify
import Control.Monad.Trans
import Control.Monad.Ref
import Control.Monad.Incremental as Inc
import Data.Time.LocalTime
import Language.Forest.IC.FS.NILFSDiff
import System.Mem.MemoTable
import Control.Monad.Lazy
import Control.Monad.Reader (Reader(..),ReaderT(..),MonadReader(..))
import qualified Control.Monad.Reader as Reader

instance LiftInc Inside Adapton (IncForest NILFS) IORef IO where
	liftInc = NILFSForestI . lift
instance LiftInc Outside Adapton (IncForest NILFS) IORef IO where
	liftInc = NILFSForestO . lift

instance Incremental (IncForest NILFS) IORef IO where
	
	newtype Outside (IncForest NILFS) IORef IO a = NILFSForestO { adaptonOuter :: ReaderT (LiveSnapshots,Bool,ForestData) (Outer IORef IO) a } deriving (Monad,MonadLazy,MonadReader (LiveSnapshots,Bool,ForestData))
	newtype Inside (IncForest NILFS) IORef IO a = NILFSForestI { adaptonInner :: ReaderT (LiveSnapshots,Bool,ForestData) (Inner IORef IO) a } deriving (Monad,MonadLazy,MonadReader (LiveSnapshots,Bool,ForestData))
	
	world = NILFSForestO . Reader.mapReaderT inside . adaptonInner
	unsafeWorld = NILFSForestI . Reader.mapReaderT (Adapton.Inner . runOuter) . adaptonOuter
	
	runIncremental m = error "use runForest instead"
	
instance InLayer Outside (IncForest NILFS) IORef IO where
	inL = NILFSForestO . lift . Adapton.Outer
	{-# INLINE inL #-}
instance InLayer Inside (IncForest NILFS) IORef IO where
	inL = NILFSForestI . lift . Adapton.Inner
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
	data ForestCfg NILFS = NILFSForestConfig { supportMoves :: Bool, rootPath :: FilePath, forestDir :: FilePath }

	runForest = error "use runIncrementalForest instead"

	forestIO = liftAdaptonNILFS . inL . liftIO where
		liftAdaptonNILFS :: Inside Adapton IORef IO a -> ForestM NILFS a
		liftAdaptonNILFS = NILFSForestM . lift

	-- the same as the inner layer
	newtype ForestM NILFS a = NILFSForestM { runNILFSForestM :: ReaderT (LiveSnapshots,Bool,ForestData) (Inner IORef IO) a } deriving (Monad,MonadLazy,MonadReader (LiveSnapshots,Bool,ForestData))

	getForestDirectory = do
		(liveSnapshots,moves,(forestDir,((rootPath,rootFolder),device))) <- Reader.ask
		return forestDir

	data FSTree NILFS = NILFSTree Snapshot UTCTime | VirtualNILFSTree Snapshot UTCTime deriving (Show)

	virtualTree (NILFSTree ss time) = return $ VirtualNILFSTree ss time
	virtualTree (VirtualNILFSTree ss time) = return $ VirtualNILFSTree ss time

	latestTree = forestIO latestNILFSTree >>= \tree@(NILFSTree snap time) -> testAndMountSnapshot snap >> return tree

	pathInTree path tree@(NILFSTree snapshot time) = do
		(liveSnapshots,moves,(forestDir,((rootPath,rootFolder),device))) <- Reader.ask
		mb <- forestIO $ WeakTable.lookup liveSnapshots snapshot
		case mb of
			Just ref -> do
				(mountpoint) <- forestIO $ readIORef ref
				let result = mountpoint </> makeRelative rootPath path
				return $ {-debug ("pathInTree: "++show path ++ " " ++ show (snapshot,mountpoint,rootPath) ++ " -> " ++ show result) -} result
			Nothing -> mountSnapshot snapshot >> pathInTree path tree
	pathInTree path tree@(VirtualNILFSTree snapshot time) = do
		(liveSnapshots,moves,(forestDir,((rootPath,rootFolder),device))) <- Reader.ask
		mb <- forestIO $ WeakTable.lookup liveSnapshots snapshot
		case mb of
			Just ref -> do
				(mountpoint) <- forestIO $ readIORef ref
				home <- forestIO $ getHomeDirectory
				let result = home </> ".avfs" </> makeRelative "/" (mountpoint </> makeRelative rootPath path)
				return $ {-debug ("pathInTree: "++show path ++ " " ++ show (snapshot,mountpoint,rootPath) ++ " -> " ++ show result) -} result
			Nothing -> mountSnapshot snapshot >> pathInTree path tree
	pathFromTree path tree@(NILFSTree snapshot time) = do
		(liveSnapshots,moves,(forestDir,((rootPath,rootFolder),device))) <- Reader.ask
		mb <- forestIO $ WeakTable.lookup liveSnapshots snapshot
		case mb of
			Just ref -> do
				(mountpoint) <- forestIO $ readIORef ref
				return $ rootPath </> makeRelative mountpoint path
			Nothing -> mountSnapshot snapshot >> pathFromTree path tree
	pathFromTree path tree@(VirtualNILFSTree snapshot time) = do
		(liveSnapshots,moves,(forestDir,((rootPath,rootFolder),device))) <- Reader.ask
		mb <- forestIO $ WeakTable.lookup liveSnapshots snapshot
		case mb of
			Just ref -> do
				(mountpoint) <- forestIO $ readIORef ref
				home <- forestIO $ getHomeDirectory
				return $ rootPath </> makeRelative (home </> ".avfs" </> makeRelative "/" mountpoint) path
			Nothing -> mountSnapshot snapshot >> pathFromTree path tree
	stepPathInTree tree path rel = do
		cpath <- canonalizePathWithTree path tree
		let newpath = path </> rel
		cnewpath <- canonalizePathWithTree newpath tree
		if cpath `isParentPathOf` cnewpath
			then return newpath 
			else error $ "NILFS incremental loading forbids non-tree-like filesystems" ++ show cpath ++ " " ++ show newpath

instance ICRep NILFS where
	
	runIncrementalForest = runNILFSForest
	
	forestM = inside . NILFSForestI . runNILFSForestM

	changesBetween = changesBetweenNILFS
	
	
	data FSThunk NILFS l inc r m a = NILFSFSThunk { adaptonThunk :: (L l (IncForest NILFS) r m a), unmemoNILFS :: r (IO ()) } -- a set of snapshots on which the FSThunk depends and a lazy Adapton modifiable
		
	isUnforcedFSThunk t = Adapton.isUnforcedL (adaptonThunk t)
	
	newtype ICThunk NILFS l inc r m a = NILFSU { adaptonU :: U l (IncForest NILFS) r m a }

	
	newtype HSThunk NILFS l inc r m a = NILFSThunk { unNILFSThunk :: T l inc r m a }

	memo path dta tree = do
		(liveSnapshots,moves,(forestDir,((rootPath,rootFolder),device))) <- Reader.ask
		if moves then memoForest path dta tree else return ()
	unmemo fs path rep = do
		(liveSnapshots,moves,(forestDir,((rootPath,rootFolder),device))) <- Reader.ask
		if moves then forestM (forestIO $ unmemoForest path rep) else return ()
	lookupmemo path rep = do
		(liveSnapshots,moves,(forestDir,((rootPath,rootFolder),device))) <- Reader.ask
		if moves then lookupmemoForest path rep else return Nothing

	addUnmemoFSThunk t f = inL $ liftIO $ modifyIORef (unmemoNILFS t) (>> f)

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

instance Thunk (FSThunk 'NILFS) Inside (IncForest 'NILFS) IORef IO where
	newc v = do
		a <- Inc.ref v
		f <- forestM $ forestIO $ newIORef (return ())
		return $ NILFSFSThunk a f
	new m = do
		t <- do
			a <- Inc.mod m
			f <- forestM $ forestIO $ newIORef (return ())
			return $ NILFSFSThunk a f
		return t
	read t = Inc.get $ adaptonThunk t

instance Input (FSThunk NILFS) Inside (IncForest NILFS) IORef IO where
	ref = newc
	mod = new
	set t v' = do
		Inc.set (adaptonThunk t) v'
		f <- forestM $ forestIO $ readIORef $ unmemoNILFS t
		forestM $ forestIO f
	modify t f = do
		Inc.modify (adaptonThunk t) f
		f <- forestM $ forestIO $ readIORef $ unmemoNILFS t
		forestM $ forestIO f
	overwrite t m = do
		Inc.overwrite (adaptonThunk t) m
		f <- forestM $ forestIO $ readIORef $ unmemoNILFS t
		forestM $ forestIO f
	get = read

instance Thunk (FSThunk 'NILFS) Outside (IncForest 'NILFS) IORef IO where

instance Input (FSThunk NILFS) Outside (IncForest NILFS) IORef IO where

idNILFSFSThunk :: ForestFSThunk NILFS l a -> ThunkId
idNILFSFSThunk t = Adapton.idNM $ Adapton.metaL $ adaptonThunk t

-- * inner Forest state

type Snapshot = Int
type ThunkId = Unique

-- map from snapshot ids to the filepath where its root directory is mounted on the disk and a record of unevaluated thunks depending on the snapshot
-- a weak table to avoid keeping thunks alive
type LiveSnapshots = WeakTable Snapshot (IORef MountPoint)

-- the forest temporary directory, the root path being monitored, and its corresponding device
type ForestData = (FilePath,((FilePath,FilePath),FilePath))

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

-- mounts a NILFS snapshot, adding it to the live snapshots, and making sure that it is actually a NILFS snapshot (not a checkpoint)
mountSnapshot :: Snapshot -> ForestM NILFS ()
mountSnapshot snapshot = do
	(liveSnapshots,moves,(forestDir,((rootPath,rootFolder),device))) <- Reader.ask
	let mountpoint = forestDir </> "Snapshots" </> show snapshot
	forestIO $ sudoShellCommand_ $ "chcp ss "++show snapshot
	forestIO $ createDirectoryIfMissing False mountpoint
	forestIO $ sudoShellCommand_ $ "mount.nilfs2 -r "++device++" "++mountpoint++" -o cp="++show snapshot
	ref <- forestIO $ newIORef (mountpoint)
	forestIO $ WeakTable.insert liveSnapshots snapshot ref

testAndMountSnapshot :: Snapshot -> ForestM NILFS ()
testAndMountSnapshot snapshot = do
	(liveSnapshots,moves,(forestDir,((rootPath,rootFolder),device))) <- Reader.ask
	mb <- forestIO $ WeakTable.lookup liveSnapshots snapshot
	case mb of
		Just ref -> return ()
		Nothing -> mountSnapshot snapshot

			
unmountSnapshot' :: LiveSnapshots -> Snapshot -> (MountPoint) -> IO ()
unmountSnapshot' liveSnapshots snapshot (mountpoint) = do
	WeakTable.delete liveSnapshots snapshot
	sudoShellCommand_ $ "umount "++mountpoint
	removeDirectory mountpoint
	sudoShellCommand_ $ "chcp cp "++show snapshot
	return ()

unmountAllSnapshots :: LiveSnapshots -> ForestData -> IO ()
unmountAllSnapshots liveSnapshots (forestDir,((rootPath,rootFolder),device)) = do
	let allSnapshots = forestDir </> "Snapshots" </> "*"
	test <- doesDirectoryExistShell allSnapshots
	when test $ sudoShellCommand_ ("umount "++ allSnapshots) >> return ()
	return ()

-- * Connection to the event notifier

runNILFSForest :: ForestCfg NILFS -> ForestO NILFS a -> IO a
runNILFSForest cfg (NILFSForestO m) = do
	-- create temporary Forest directory
	liveSnapshots <- WeakTable.new
	forestData <- createForestData cfg
	
	-- initialize FS monitor in a separate thread and run the computation (note that the monitor never ends by itself, so it always loses the race)
	finally (Adapton.runOuter $ Reader.runReaderT m (liveSnapshots,supportMoves cfg,forestData)) (removeForestData liveSnapshots forestData cfg)

createForestData :: ForestCfg NILFS -> IO ForestData
createForestData (NILFSForestConfig moves rootPathFolder forestDir) = do
	mountAVFS
	createDirectoryIfMissing True forestDir
	createDirectoryIfMissing True (forestDir </> "Snapshots")
	root <- findRootDevice rootPathFolder
	return (forestDir,root)

findRootDevice :: FilePath -> IO ((FilePath,FilePath),FilePath)
findRootDevice rootPathFolder = do
	rootDevice <- pathDevice rootPathFolder
	rootPath <- devicePath rootDevice
	let rootFolder = makeRelative rootPath rootPathFolder
	return ((rootPath,rootFolder),rootDevice)

-- catch any exception that this may generate
removeForestData :: LiveSnapshots -> ForestData -> ForestCfg NILFS -> IO ()
removeForestData liveSnapshots forestData (NILFSForestConfig moves rootPath forestDir) = flip finally (return ()) $ do
	putStrLn "removing forest data..."
	WeakTable.mapM_ (\(snapshot,ref) -> readIORef ref >>= unmountSnapshot' liveSnapshots snapshot) liveSnapshots
	unmountAllSnapshots liveSnapshots forestData-- just to make sure that there are no mounts left
	removeDirectoryRecursive forestDir
	unmountAVFS
	return ()

changesBetweenNILFS :: FSTree NILFS -> FSTree NILFS -> ForestO NILFS FSTreeDelta
changesBetweenNILFS tree tree' = do
	(liveSnapsots,moves,(forestDir,((rootPath,rootFolder),device))) <- Reader.ask
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
