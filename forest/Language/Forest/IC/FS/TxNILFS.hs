
{-# LANGUAGE OverlappingInstances, TypeOperators, ConstraintKinds, UndecidableInstances, TupleSections, FlexibleInstances, MultiParamTypeClasses, StandaloneDeriving, GeneralizedNewtypeDeriving, FlexibleContexts, DataKinds, TypeFamilies, Rank2Types, GADTs, ViewPatterns, DeriveDataTypeable, ScopedTypeVariables #-}

-- NILFS filesystem with optimistic concurrency support for transactions, with mutable transactional variables structures mapped to specifications, and incremental reuse amoung transactions

module Language.Forest.IC.FS.TxNILFS (
	TxICForest(..)
) where

import Language.Forest.Errors
import Language.Forest.Pure.MetaData (FileInfo)
import qualified Control.Concurrent.STM as STM
import Data.Monoid as Monoid
import Language.Forest.FS.NILFS
import Control.Monad.RWS (RWS(..),RWST(..))
import qualified Control.Monad.RWS as RWS
import Control.Monad.Writer (Writer(..),WriterT(..))
import qualified Control.Monad.Writer as Writer
import Language.Forest.IC.Default
import Control.Monad.Catch
import Control.Concurrent
import System.Process
import System.Mem.Weak as Weak
import Control.Exception as Exception
import Data.Strict.List
import Unsafe.Coerce
import Language.Forest.IC.ValueDelta
import Language.Forest.IC.BX as BX
import System.Mem.WeakKey
import Data.Foldable as Foldable
import Data.Concurrent.Deque.Class as Queue
import Data.Concurrent.Deque.Reference.DequeInstance
import qualified Control.Concurrent.Map as CMap
import qualified Control.Concurrent.WeakMap as CWeakMap
import System.Posix.Files
import Language.Forest.FS.Diff
import Language.Forest.IC.ICRep

import Language.Forest.FS.FSRep
import Control.Applicative
import System.IO.Unsafe
import Control.Monad
import Control.Monad.Lazy
import Data.Maybe
import Data.List
import Data.WithClass.MData
import Data.Typeable
import System.Directory
import Data.List as List
import Data.Dynamic
import System.Mem.WeakTable as WeakTable
import Data.IORef
import System.FilePath.Posix
import Language.Forest.IO.Utils
import Language.Forest.IC.MetaData
import Control.Concurrent.Lock (Lock(..))
import qualified Control.Concurrent.Lock as Lock
import Control.Concurrent.MVar
import Language.Forest.IO.Shell
import Language.Forest.IO.Utils
import Language.Forest.IC.Generic
import Language.Forest.FS.FSRep
import Control.Monad.Incremental as Inc
import Data.IORef
import Control.Monad.Ref
import Data.Map (Map(..))
import qualified Data.Map as Map
import Data.Set (Set(..))
import qualified Data.Set as Set
import Control.Monad.State (State(..),StateT(..),MonadState(..))
import Control.Monad.Reader (Reader(..),ReaderT(..),MonadReader(..))
import Control.Monad.Reader as Reader
import Language.Forest.FS.FSDelta
import Control.Monad.Trans
import qualified Control.Monad.State as State
import Language.Forest.Manifest
import Data.Time.Clock
import Data.Unique
import Safe
import Control.Monad.Catch as Catch
import System.Mem.Concurrent.WeakMap as WeakMap
import Data.DList as DList

type TxNILFSLayer l = (MonadReader TxNILFSEnv (ForestL TxNILFS l),ForestLayer TxNILFS l)

type instance IncK (IncForest TxNILFS) a = (Typeable a,Eq a,AddTxNILFSParent Inside a,AddTxNILFSParent Outside a)

proxyTxNILFS = Proxy :: Proxy TxNILFS

-- a list of the starting times of running transactions sorted from newest to oldest (we may two txs with the same starting time)
{-# NOINLINE runningTxsNILFS #-}
runningTxsNILFS :: MVar [UTCTime]
runningTxsNILFS = unsafePerformIO $ newMVar []

-- insert a new time in a list sorted from newest to oldest
addRunningTxNILFS time = modifyMVarMasked_ runningTxsNILFS (\xs -> return $ List.insertBy (\x y -> compare y x) time xs)
-- remove a time in a list sorted from newest to oldest
deleteRunningTxNILFS time = modifyMVarMasked_ runningTxsNILFS (\xs -> return $ List.delete time xs)

-- a map with commit times of committed transactions and their performed changes
-- we use a @FSTreeDelta@ because the set of written paths may be infinite (like all paths under a given directory)
{-# NOINLINE doneTxsNILFS #-}
doneTxsNILFS :: MVar (Map UTCTime TxNILFSWrites)
doneTxsNILFS = unsafePerformIO $ newMVar Map.empty

-- not a concurrent data structure, because we only commit to it under a lock
{-# NOINLINE memoTxNILFS #-}
memoTxNILFS :: TxNILFSCMemoTable
memoTxNILFS = unsafePerformIO $ CWeakMap.empty

-- ** Filesystem

-- latest tree,delta after the current tree,next tree,reads since the beginning, buffered variables, memoized forest transactional variables, temporary paths since the beginning
type TxNILFSLog = IORef (FSTree TxNILFS,FSDeltas,FSTree TxNILFS,TxNILFSReads,TxNILFSBuffTable,TxNILFSMemoTable,Set FilePath)

-- one per transaction
type TxNILFSBuffTable = WeakTable Unique DynTxNILFSThunk
-- one per transaction
type TxNILFSMemoTable = WeakTable (FilePath,TypeRep) (DynamicNILFS,MkWeak)
-- a global memotable
type TxNILFSCMemoTable = CWeakMap.WeakMap (FilePath,TypeRep) (DynamicNILFS,MkWeak)

data DynamicNILFS where
	DynNILFS :: FTK TxNILFS args rep var content => rep -> DynamicNILFS

fromDynNILFS :: FTK TxNILFS args rep var content => DynamicNILFS -> Maybe rep
fromDynNILFS (DynNILFS v) = cast v

-- nested logs for nested transactions
type TxNILFSLogs = SList TxNILFSLog

-- a chronological list of relative deltas for the different FS versions
type TxNILFSTreeDeltas = IORef (Map FSVersion FSDeltas)

-- (starttime,tx id,register of deltas,nested logs)
type TxNILFSEnv = (IORef UTCTime,Snapshot,TxNILFSTreeDeltas,TxNILFSLogs)

type TxNILFSReads = Set FilePath
type TxNILFSWrites = Set FilePath
type TxNILFSChanges = (TxNILFSReads,FSTreeDelta)
type TxNILFSChangesFlat = (TxNILFSReads,TxNILFSWrites)

acquireTxNILFSBuffTable :: TxNILFSBuffTable -> IO ()
acquireTxNILFSBuffTable = WeakTable.mapM_ acquireDynTxNILFSThunk

acquireDynTxNILFSThunk :: (Unique,DynTxNILFSThunk) -> IO ()
acquireDynTxNILFSThunk (uid,DynTxFSThunk _ _ t) = takeMVar (txNILFSFSThunk t) >> return ()
acquireDynTxNILFSThunk (uid,DynTxHSThunk _ _ t) = takeMVar (txNILFSHSThunk t) >> return ()
acquireDynTxNILFSThunk (uid,DynBuffTxNILFSThunk _ _ t) = Lock.acquire (txNILFSLock t)

commitTxNILFSBuffTable :: TxNILFSBuffTable -> IO ()
commitTxNILFSBuffTable tbl = WeakTable.mapM_ commitDynTxNILFSThunk tbl

commitDynTxNILFSThunk :: (Unique,DynTxNILFSThunk) -> IO ()
commitDynTxNILFSThunk (uid,DynTxFSThunk v _ t) = putMVar (txNILFSFSThunk t) v
commitDynTxNILFSThunk (uid,DynTxHSThunk v _ t) = putMVar (txNILFSHSThunk t) v
commitDynTxNILFSThunk (uid,DynBuffTxNILFSThunk v _ t) = do
	oriParents <- liftM buffTxNILFSParents $ readIORef (txNILFSBuff t)
	unionWithKey' oriParents (buffTxNILFSParents v)
	writeIORef (txNILFSBuff t) (v { buffTxNILFSParents = oriParents })
	Lock.release (txNILFSLock t)

commitTxNILFSMemoTable :: TxNILFSMemoTable -> IO ()
commitTxNILFSMemoTable = WeakTable.mapM_ commitTxNILFSMemoEntry where
	commitTxNILFSMemoEntry (k,v@(_,mkWeak)) = CWeakMap.insertWithMkWeak memoTxNILFS mkWeak k v 

-- repairs memoized entries to the latest fs tree
repairTxNILFSMemoTable :: TxNILFSLayer l => TxNILFSMemoTable -> ForestL TxNILFS l ()
repairTxNILFSMemoTable = WeakTable.mapMGeneric__ (forestM . forestIO) repairTxNILFSMemoEntry where
	repairTxNILFSMemoEntry :: TxNILFSLayer l => ((FilePath,TypeRep),(DynamicNILFS,MkWeak)) -> ForestL TxNILFS l ()
	repairTxNILFSMemoEntry ((path,tyRep),(DynNILFS rep,mkWeak)) = loadTxNILFS Proxy rep

bufferTxNILFSFSThunk :: (IncK (IncForest TxNILFS) a,AddTxNILFSParent l a,TxNILFSLayer l) => ForestFSThunk TxNILFS l a -> TxNILFSThunk l a -> ForestM TxNILFS ()
bufferTxNILFSFSThunk var thunk = do
	(starttime,txid,deltas,SCons txlog _) <- Reader.ask
	(tree,ds,newtree,reads,bufftbl,memotbl,tmps) <- forestIO $ readRef txlog
	let mkWeak = MkWeak $ mkWeakRefKey $ txNILFSFSThunkArgs var
	forestIO $ WeakTable.insertWithMkWeak bufftbl mkWeak (txNILFSFSThunkId var) $ DynTxFSThunk thunk mkWeak var
	
bufferTxNILFSHSThunk :: (IncK (IncForest TxNILFS) a,AddTxNILFSParent l a,TxNILFSLayer l) => ForestHSThunk TxNILFS l a -> TxNILFSThunk l a -> ForestM TxNILFS ()
bufferTxNILFSHSThunk var thunk = do
	(starttime,txid,deltas,SCons txlog _) <- Reader.ask
	(tree,ds,newtree,reads,bufftbl,memotbl,tmps) <- forestIO $ readRef txlog
	let mkWeak = MkWeak $ mkWeakRefKey $ txNILFSHSThunkArgs var
	forestIO $ WeakTable.insertWithMkWeak bufftbl mkWeak (txNILFSHSThunkId var) $ DynTxHSThunk thunk mkWeak var

-- reads the pointer from a transactional variable to a thunk
bufferedTxNILFSFSThunk :: (Typeable l,Typeable a) => ForestFSThunk TxNILFS l a -> ForestM TxNILFS (TxNILFSThunk l a)
bufferedTxNILFSFSThunk var = do
	(starttime,txid,deltas,txlogs) <- Reader.ask
	-- read from the concurrent global thunk
	let newBuff = do
		let mkWeak = MkWeak $ mkWeakRefKey $ txNILFSFSThunk var
		buff <- forestIO $ readMVar $ txNILFSFSThunk var
		return buff
	-- read from the local buffer
	let find txlog m = do
		(tree,ds,newtree,reads,bufftbl,memotbl,tmps) <- forestIO $ readRef txlog
		mb <- forestIO $ WeakTable.lookup bufftbl (txNILFSFSThunkId var)
		case mb of
			Nothing -> m
			Just (DynTxFSThunk (cast -> Just thunk) _ _) -> return thunk
	Foldable.foldr find newBuff txlogs

-- reads the pointer from a transactional variable to a thunk
bufferedTxNILFSHSThunk :: (Typeable l,Typeable a) => ForestHSThunk TxNILFS l a -> ForestM TxNILFS (TxNILFSThunk l a)
bufferedTxNILFSHSThunk var = do
	(starttime,txid,deltas,txlogs) <- Reader.ask
	-- read from the concurrent global thunk
	let newBuff = do
		let mkWeak = MkWeak $ mkWeakRefKey $ txNILFSHSThunk var
		buff <- forestIO $ readMVar $ txNILFSHSThunk var
		return buff
	-- read from the local buffer
	let find txlog m = do
		(tree,ds,newtree,reads,bufftbl,memotbl,tmps) <- forestIO $ readRef txlog
		mb <- forestIO $ WeakTable.lookup bufftbl (txNILFSHSThunkId var)
		case mb of
			Nothing -> m
			Just (DynTxHSThunk (cast -> Just thunk) _ _) -> return thunk
	Foldable.foldr find newBuff txlogs

getNextFSTree :: MonadReader TxNILFSEnv (ForestM TxNILFS) => ForestM TxNILFS (FSTree TxNILFS)
getNextFSTree = do
	(starttime,txid,deltas,SCons txlog _) <- Reader.ask
	(tree,ds,newtree,reads,bufftbl,memotbl,tmps) <- forestIO $ readRef txlog
	return newtree

incrementTxNILFSTree :: MonadReader TxNILFSEnv (ForestM TxNILFS) => ForestM TxNILFS ()
incrementTxNILFSTree = do
	(starttime,txid,deltas,SCons txlog _) <- Reader.ask
	(tree,ds,newtree,reads,bufftbl,memotbl,tmps) <- forestIO $ readRef txlog
	-- the new tree deltas are appended to the original deltas
	treeDelta <- forestIO $ fsTreeFSTreeDelta tree
	nextTreeDeltas <- forestIO $ readRef treeDelta >>= newRef
	newTreeSym <- forestIO $ fsTreeSym newtree
	nextSyms <- forestIO $ readIORef newTreeSym >>= newIORef
	-- create the new pending tree
	newTreeSnap <- forestIO $ fsTreeSnapshot newtree
	newTreeVersion <- forestIO $ fsTreeFSVersion newtree
	newTreeVirtual <- forestIO $ fsTreeVirtual newtree
	nexttree <- liftM FSTreeTxNILFS $ forestIO $ newIORef $ TxTree newTreeSnap (succ newTreeVersion) nextTreeDeltas newTreeVirtual nextSyms
	-- add the relative deltas
	tds <- forestIO $ readRef deltas
	forestIO $ writeRef deltas $ Map.insert newTreeVersion ds tds
	forestIO $ writeRef txlog (newtree,DList.empty,nexttree,reads,bufftbl,memotbl,tmps)

-- only needs to read from the top-level log
getTxNILFSChangesFlat :: ForestM TxNILFS TxNILFSChangesFlat
getTxNILFSChangesFlat = do
	(starttime,txid,deltas,SCons txlog _) <- Reader.ask
	(tree,ds,newtree,reads,bufftbl,memotbl,tmps) <- forestIO $ readRef txlog
	d <- forestIO $ fsTreeFSTreeDelta tree
	writes <- forestIO $ readRef d
	return (reads,fsTreeDeltaWrites "" writes)

-- reads from the current tree
-- only needs to read from the top-level log
getTxNILFSChanges :: ForestM TxNILFS TxNILFSChanges
getTxNILFSChanges = do
	(starttime,txid,deltas,SCons txlog _) <- Reader.ask
	(tree,ds,newtree,reads,bufftbl,memotbl,tmps) <- forestIO $ readIORef txlog
	d <- forestIO $ fsTreeFSTreeDelta tree
	writes <- forestIO $ readRef d
	return (reads,writes)

putTxNILFSRead :: FilePath -> ForestM TxNILFS ()
putTxNILFSRead path = do
	(starttime,txid,deltas,SCons txlog _) <- Reader.ask
	(tree,ds,newtree,reads,bufftbl,memotbl,tmps) <- forestIO $ readIORef txlog
	forestIO $ writeIORef txlog (tree,ds,newtree,Set.insert path reads,bufftbl,memotbl,tmps)

putTxNILFSTmp :: FilePath -> ForestM TxNILFS ()
putTxNILFSTmp tmp = do
	(starttime,txid,deltas,SCons txlog _) <- Reader.ask
	(tree,ds,newtree,reads,bufftbl,memotbl,tmps) <- forestIO $ readIORef txlog
	forestIO $ writeRef txlog (tree,ds,newtree,reads,bufftbl,memotbl,Set.insert tmp tmps)

-- writes to the next tree
-- duplicate the action on both the relative deltas (sequence of primitve FSDelta) and the absolute deltas (FSTreeDelta)
modifyTxNILFSTreeDeltas :: FSDelta -> ForestM TxNILFS ()
modifyTxNILFSTreeDeltas d = do
	(starttime,txid,deltas,SCons txlog _) <- Reader.ask
	(tree,ds,newtree,reads,bufftbl,memotbl,tmps) <- forestIO $ readIORef txlog
	delta <- forestIO $ fsTreeFSTreeDelta newtree
	forestIO $ modifyIORef delta (appendToFSTreeDelta d)
	sym <- forestIO $ fsTreeSym newtree
	forestIO $ modifyIORef sym (\(ds,ts) -> (DList.snoc ds d,appendToFSTreeSym d ts))
	forestIO $ writeIORef txlog (tree,DList.snoc ds d,newtree,reads,bufftbl,memotbl,tmps)

treeTxNILFSFSThunk :: (IncK (IncForest TxNILFS) a,Typeable l,Typeable a,TxNILFSLayer l) => ForestFSThunk TxNILFS l a -> ForestL TxNILFS l (FSTree TxNILFS)
treeTxNILFSFSThunk var = do
	thunk <- forestM $ bufferedTxNILFSFSThunk var
	treeTxNILFSThunk thunk

treeTxNILFSThunk :: (IncK (IncForest TxNILFS) a,Typeable l,Typeable a,TxNILFSLayer l) => TxNILFSThunk l a -> ForestL TxNILFS l (FSTree TxNILFS)
treeTxNILFSThunk thunk = do
	buff <- forestM $ bufferedTxNILFSThunk thunk
	return $ buffTxNILFSTree buff

-- transaction unique id
type TxId = Unique

instance MonadReader TxNILFSEnv (ForestM TxNILFS) where
	ask = TxNILFSForestM ask
	local f (TxNILFSForestM m) = TxNILFSForestM $ local f m

instance Show (FSTree TxNILFS) where
	show tree = "FSTreeTxNILFS"

type FSVersion = Int

instance FSRep TxNILFS where
	
	newtype ForestM TxNILFS a = TxNILFSForestM { runTxNILFSForestM :: ReaderT TxNILFSEnv IO a } deriving (Functor,Applicative,Monad,MonadLazy,MonadThrow,MonadCatch,MonadMask)
	
	data ForestCfg TxNILFS = TxNILFSForestCfg FilePath
	
	runForest _ m = error "please use atomically instead"
	
	forestIO = TxNILFSForestM . lift
	
	-- | returns the forest directory used to store auxiliary data
	getForestDirectory = liftM (</> "Forest") $ forestIO getTemporaryDirectory
	
	-- a modifiable reference to allow changing the FSTree on commits
	newtype FSTree TxNILFS = FSTreeTxNILFS { unFSTreeTxNILFS :: IORef FSTreeTxNILFS' }
	
	compareFSTree (FSTreeTxNILFS r1) (FSTreeTxNILFS r2) = do
		t1 <- forestIO $ readIORef r1
		t2 <- forestIO $ readIORef r2
		return $ compare t1 t2
	
	-- log the modifications
	-- writes come only from manifests, for which we have already canonized the paths
	deletePath path = modifyTxNILFSTreeDeltas (Rem path)
	writeFile path ondisk = modifyTxNILFSTreeDeltas (Add path ondisk)
	writeDir path = do
		ondisk <- tempPath
		forestIO $ createDirectory ondisk
		modifyTxNILFSTreeDeltas (Add path ondisk)
	writeLink path linkpath = modifyTxNILFSTreeDeltas (AddLink path linkpath)
	writePathMD path ondisk = modifyTxNILFSTreeDeltas (ChgAttrs path ondisk)
	
	-- registers a new temporary path
	tempPath = forestIO getTempPath >>= \path -> putTxNILFSTmp path >> return path
	-- change into AVFS mode
	virtualTree (FSTreeTxNILFS tree) = do
		t <- forestIO $ readIORef tree
		let t' = t { txTreeVirtual = True }
		liftM FSTreeTxNILFS $ forestIO $ newIORef t'
	latestTree = do
		(starttime,txid,deltas,SCons txlog _) <- Reader.ask
		(tree,ds,newtree,reads,bufftbl,memotbl,tmps) <- forestIO $ readIORef txlog
		return tree
	
	-- reads from the FS or from the modification log
	pathInTree path tree = do
		-- canonize the fullpath for reads
		canpath <- canonalizeDirectoryInTree path tree
		-- mark the path as read
		putTxNILFSRead canpath
		(_,td) <- getTxNILFSChanges
		let path_td = focusFSTreeDeltaByRelativePathMay td canpath
		ondisk <- case onDiskWriteMay path_td of
			Nothing -> pathInSnapshot liveSnapshots nilfsData canpath =<< forestIO (fsTreeSnapshot tree) -- read the content from the original NILFS snapshot
			Just ondisk -> return ondisk -- return the written ondisk temporary file
		virtualizeTxNILFS ondisk tree

	stepPathInTree tree path rel = do
		dir <- canonalizePathWithTree path tree
		return $ dir </> rel
	
	getDirectoryContentsInTree = getDirectoryContentsTxNILFS
		
	doesDirectoryExistInTree = doesExistTxNILFS doesDirectoryExist
	doesFileExistInTree = doesExistTxNILFS doesFileExist
	doesExistInTree = doesExistTxNILFS (\path -> doesDirectoryExist path >>= \isDir -> if isDir then return True else doesFileExist path)
	
	canonalizePathWithTree = canonalizePathTxNILFS

	type FSTreeD TxNILFS = FSTreeDelta -- a delta fixed-point focused on the current path

	isEmptyFSTreeD _ = isEmptyFSTreeDelta
	isEmptyTopFSTreeD _ = isEmptyTopFSTreeDelta
	isChgFSTreeD _ = isChgFSTreeDelta
	isMoveFSTreeD _ = isMoveFSTreeDelta

	focusDiffFSTreeD tree path tree' path' = do
		td <- focusDiffFSTreeDelta tree path tree' path'
		syms <- forestIO $ findSymLinks path'
		let fixtd = fixFSTreeDelta td syms
		return fixtd
		 
	focusFSTreeD fs td path file pathfile = focusFSTreeDeltaByRelativePath td pathfile

	diffFS oldtree newtree path = do
		(starttime,cp2,deltas,txlogs) <- Reader.ask
		ot <- forestIO $ readIORef (unFSTreeTxNILFS oldtree)
		nt <- forestIO $ readIORef (unFSTreeTxNILFS newtree)
		case (ot,nt) of
			(NILFSTree cp1 _,NILFSTree cp2 _) -> do
				-- difference between NILFS snapshots
				tdNILFS <- if cp1 == cp2
					then return emptyFSTreeDelta -- nothing to do
					else do
						mount1 <- mountSnapshot liveSnapshots nilfsData cp1
						mount2 <- mountSnapshot liveSnapshots nilfsData cp2
						let device = snd nilfsData
						forestIO $ diffNILFS mount2 device cp1 cp2
				syms <- forestIO $ readMVar symlinks
				let fixtd = fixFSTreeDelta tdNILFS syms
				let df = focusFSTreeDeltaByRelativePath fixtd path
				return $ Just df
			(NILFSTree cp1 _,TxTree ((==cp2) -> True) fs2 delta2 virtual2 sym2) -> do
				-- difference between NILFS snapshots
				tdNILFS <- if cp1 == cp2
					then return emptyFSTreeDelta -- nothing to do
					else do
						mount1 <- mountSnapshot liveSnapshots nilfsData cp1
						mount2 <- mountSnapshot liveSnapshots nilfsData cp2
						let device = snd nilfsData
						forestIO $ diffNILFS mount2 device cp1 cp2
				-- transaction-local difference
				(starttime,txid,deltas,txlogs) <- Reader.ask
				tds <- forestIO $ readRef deltas
				let dfs = take fs2 $ Map.elems tds -- deltas since the beginning of the tx
				syms <- liftM snd $ forestIO $ readRef sym2
				let fixtd = fixFSTreeDelta (appendListToFSTreeDelta (DList.concat dfs) tdNILFS) syms
				let df = focusFSTreeDeltaByRelativePath fixtd path
				return $ Just df
			(TxTree ((==cp2) -> True) fs1 delta1 virtual1 sym1,TxTree ((==cp2) -> True) fs2 delta2 virtual2 sym2) -> do
				-- transaction-local difference
				(starttime,txid,deltas,txlogs) <- Reader.ask
				tds <- forestIO $ readRef deltas
				let dfs = drop (pred fs1) $ take fs2 $ Map.elems tds -- deltas since the beginning of the tx
				syms <- liftM snd $ forestIO $ readRef sym2
				let fixtd = fixFSTreeDelta (compressFSDeltas (DList.concat dfs)) syms
				let df = focusFSTreeDeltaByRelativePath fixtd path
				return $ Just df
			otherwise -> return Nothing

data FSTreeTxNILFS' = TxTree {
		  txTreeSnapshot :: Snapshot -- original NILFS snapshot
		, txTreeFSVersion :: FSVersion -- tx-local fsversion
		, txTreeFSTreeDelta :: IORef FSTreeDelta -- FS deltas since beginning of the tx (over the physical FS)
		, txTreeVirtual :: Bool -- virtual flag
		, txTreeSym :: IORef (DList FSDelta,FSTreeSym) -- a sequence of symlink modifications since the start and the symlinks table at the time of this tree
		}
		| NILFSTree Snapshot Bool

instance Eq FSTreeTxNILFS' where
	(TxTree snap1 v1 _ vir1 _) == (TxTree snap2 v2 _ vir2 _) = (snap1,v1,vir1) == (snap2,v2,vir2)
	(NILFSTree snap1 vir1) == (NILFSTree snap2 vir2) = (snap1,vir1) == (snap2,vir2)

instance Ord FSTreeTxNILFS' where
	(TxTree snap1 v1 _ vir1 _) <= (TxTree snap2 v2 _ vir2 _) = (snap1,v1,vir1) <= (snap2,v2,vir2)
	(NILFSTree snap1 vir1) <= (NILFSTree snap2 vir2) = (snap1,vir1) <= (snap2,vir2)

fsTreeSnapshot :: FSTree TxNILFS -> IO Snapshot
fsTreeSnapshot (FSTreeTxNILFS tree) = readIORef tree >>= \t -> case t of
	TxTree snap fs delta virtual sym -> return snap
	NILFSTree snap virtual -> return snap
fsTreeFSVersion :: FSTree TxNILFS -> IO FSVersion
fsTreeFSVersion (FSTreeTxNILFS tree) = readIORef tree >>= \t -> case t of
	TxTree snap fs delta virtual sym -> return fs
	NILFSTree snap virtual -> error "no version"
fsTreeFSTreeDelta :: FSTree TxNILFS -> IO (IORef FSTreeDelta)
fsTreeFSTreeDelta (FSTreeTxNILFS tree) = readIORef tree >>= \t -> case t of
	TxTree snap fs delta virtual sym -> return delta
	NILFSTree snap virtual -> error "no delta"
fsTreeVirtual :: FSTree TxNILFS -> IO Bool
fsTreeVirtual (FSTreeTxNILFS tree) = readIORef tree >>= \t -> case t of
	TxTree snap fs delta virtual sym -> return virtual
	NILFSTree snap virtual -> return virtual
fsTreeSym :: FSTree TxNILFS -> IO (IORef (DList FSDelta,FSTreeSym))
fsTreeSym (FSTreeTxNILFS tree) = readIORef tree >>= \t -> case t of
	TxTree snap fs delta virtual sym -> return sym
	NILFSTree snap virtual -> error "no sym"

-- when we commit FS modifications we generate a new NILFS checkpoint and update the tx-local FSTree reference to a global NILFS tree
newNILFSTree :: FSTree TxNILFS -> ForestM TxNILFS ()
newNILFSTree tree@(FSTreeTxNILFS ref) = forestIO $ do
	cp <- newCheckpoint nilfsData
	tree <- readIORef ref
	case tree of
		TxTree snap fs delta virtual sym -> writeIORef ref $ NILFSTree cp virtual
		NILFSTree snap virtual -> return ()

virtualizeTxNILFS path tree = forestIO (fsTreeVirtual tree) >>= \virtual -> if virtual
	then liftM (</> ".avfs" </> makeRelative "/" path) (forestIO getHomeDirectory)
	else return path

getDirectoryContentsTxNILFS :: FilePath -> FSTree TxNILFS -> ForestM TxNILFS [FileName]
getDirectoryContentsTxNILFS path tree = do
	canpath <- canonalizeDirectoryInTree path tree
	putTxNILFSRead canpath
	(_,td) <- getTxNILFSChanges
	let path_td = focusFSTreeDeltaByRelativePathMay td canpath
	xs <- forestIO $ getContentsFSTreeDeltaNodeMay canpath path_td
	return xs

doesExistTxNILFS :: (FilePath -> IO Bool) -> FilePath -> FSTree TxNILFS -> ForestM TxNILFS Bool
doesExistTxNILFS test path tree = do
	canpath <- canonalizeDirectoryInTree path tree
	putTxNILFSRead canpath
	(_,td) <- getTxNILFSChanges
	let path_td = focusFSTreeDeltaByRelativePathMay td canpath
	case path_td of
		Just (FSTreeNew _ _ diskpath _) -> forestIO $ test diskpath
		otherwise -> forestIO $ test canpath

-- canonalizes a path, taking into account the buffered FSTreeDelta and logging reads of followed symlinks
canonalizePathTxNILFS :: FilePath -> FSTree TxNILFS -> ForestM TxNILFS FilePath
canonalizePathTxNILFS path tree = do
	norm <- forestIO $ liftM normalise $ absolutePath path
	let dirs = splitDirectories norm
	follow "" dirs
  where
	follow root [] = return root
	follow root (".":dirs) = follow root dirs
	follow root ("..":dirs) = follow (takeDirectory root) dirs
	follow root (dir:dirs) = do
		(_,td) <- getTxNILFSChanges
		let rootdir = root </> dir
		let td' = focusFSTreeDeltaByRelativePathMay td rootdir
		case td' of
			Just (FSTreeNewLink link _) -> do -- follow buffered links
				-- mark the symbolic link as read
				putTxNILFSRead rootdir
				canLink <- canonalizePathTxNILFS (root </> link) tree
				follow canLink dirs
			otherwise -> do -- follow FS links
				rootdir_dsk <- flip virtualizeTxNILFS tree =<< case onDiskWriteMay td' of
					Nothing -> pathInSnapshot liveSnapshots nilfsData rootdir =<< forestIO (fsTreeSnapshot tree)
					Just ondisk -> return $ rootdir
				e <- forestIO $ Exception.try $ liftM isSymbolicLink (getSymbolicLinkStatus rootdir_dsk)
				case e of
					Left (e::SomeException) -> follow rootdir dirs
					Right isLink -> do
						if isLink
							then do
								-- mark the symbolic link as read
								putTxNILFSRead rootdir
								e <- forestIO $ Exception.try $ readSymbolicLink rootdir_dsk
								case e of
									Left (e::SomeException) -> follow rootdir dirs
									Right p -> do
										canLink <- canonalizePathTxNILFS (root </> p) tree
										follow canLink dirs
							else follow rootdir dirs

-- ** Incrementality

instance MonadReader TxNILFSEnv (ForestO TxNILFS) where
	ask = TxNILFSForestO ask
	local f (TxNILFSForestO m) = TxNILFSForestO $ local f m
	
instance MonadReader TxNILFSEnv (ForestI TxNILFS) where
	ask = TxNILFSForestI ask
	local f (TxNILFSForestI m) = TxNILFSForestI $ local f m

instance Incremental (IncForest TxNILFS) IORef IO where
	
	newtype Outside (IncForest TxNILFS) IORef IO a = TxNILFSForestO { runTxNILFSForestO :: ReaderT TxNILFSEnv IO a } deriving (Functor,Applicative,Monad,MonadLazy,MonadThrow,MonadCatch,MonadMask)
	newtype Inside (IncForest TxNILFS) IORef IO a = TxNILFSForestI { runTxNILFSForestI :: ReaderT TxNILFSEnv IO a } deriving (Functor,Applicative,Monad,MonadLazy,MonadThrow,MonadCatch,MonadMask)

	world = TxNILFSForestO . runTxNILFSForestI
	unsafeWorld = TxNILFSForestI . runTxNILFSForestO

	runIncremental = error "please use atomically instead"

instance InLayer Outside (IncForest TxNILFS) IORef IO where
	inL = TxNILFSForestO . lift
	{-# INLINE inL #-}
instance InLayer Inside (IncForest TxNILFS) IORef IO where
	inL = TxNILFSForestI . lift
	{-# INLINE inL #-}
	
instance ICRep TxNILFS where

	forestM = inside . TxNILFSForestI . runTxNILFSForestM
	forestO = TxNILFSForestM . runTxNILFSForestO

	--MVar because transactions may concurrently commit updates to in-memory variables, and a tx may commit the load of a variable whose associated FS fragment has not been locked
	-- arguments never change
	data FSThunk TxNILFS l inc r m a = TxNILFSFSThunk { txNILFSFSThunkId :: Unique, txNILFSFSThunk :: MVar (TxNILFSThunk l a), txNILFSFSThunkArgs :: IORef (Maybe TxNILFSArgs) }

	data HSThunk TxNILFS l inc r m a = TxNILFSHSThunk { txNILFSHSThunkId :: Unique, txNILFSHSThunk :: MVar (TxNILFSThunk l a), txNILFSHSThunkArgs :: IORef (Maybe TxNILFSArgs) }
		
	newtype ICThunk TxNILFS l inc r m a = TxNILFSICThunk (l inc r m a)

	data ValueDelta TxNILFS a = ValueDeltaTxNILFS Bool -- True = identity
	
	diffValueThunk = diffValueThunkTxNILFS False
	diffTopValueThunk = diffValueThunkTxNILFS True	
	diffValueAny _ _ = return chgValueDelta
	
	isIdValueDelta (ValueDeltaTxNILFS d) = d
	idValueDelta = ValueDeltaTxNILFS True
	chgValueDelta = ValueDeltaTxNILFS False
	mapValueDelta _ (ValueDeltaTxNILFS d) = (ValueDeltaTxNILFS d)

	eqFSThunk t1 t2 = txNILFSFSThunkId t1 == txNILFSFSThunkId t2
	eqICThunk t1 t2 = False

	isUnevaluatedFSThunk var = do
		thunk <- forestM $ bufferedTxNILFSFSThunk var
		buff <- forestM $ bufferedTxNILFSThunk thunk
		case buffTxNILFSData buff of
			TxNILFSThunkComp force -> return True
			TxNILFSThunkForce a stone -> return False

diffValueThunkTxNILFS :: (IncK (IncForest TxNILFS) content,Typeable content,ForestRep rep (ForestFSThunkI TxNILFS content)) => Bool -> FSTree TxNILFS -> rep -> ForestO TxNILFS (ValueDelta TxNILFS rep)
diffValueThunkTxNILFS isTop oldtree rep = do
		thunk <- forestM $ bufferedTxNILFSFSThunk $ to iso_rep_thunk rep
		let uid = txNILFSId thunk
		(buff :: BuffTxNILFSThunk Inside content) <- forestM $ bufferedTxNILFSThunk thunk
		let newtree = if isTop then buffTxNILFSTree buff else buffTxNILFSDeltaTree buff
		cmp <- forestM $ compareFSTree oldtree newtree
		return $ ValueDeltaTxNILFS $ cmp == EQ
	
-- arguments passed to transactional variables
type TxNILFSArgs = (Dynamic,FilePath)

-- dynamic variables for buffered content
data DynTxNILFSThunk where
	-- transactional variable (points to an internal thunk)
	DynTxFSThunk :: (IncK (IncForest TxNILFS) a,TxNILFSLayer l) => TxNILFSThunk l a -> MkWeak -> FSThunk TxNILFS l (IncForest TxNILFS) IORef IO a ->  DynTxNILFSThunk
	DynTxHSThunk :: (IncK (IncForest TxNILFS) a,TxNILFSLayer l) => TxNILFSThunk l a -> MkWeak -> HSThunk TxNILFS l (IncForest TxNILFS) IORef IO a ->  DynTxNILFSThunk
	-- internal thunk (buffered content used for nested logs) 
	DynBuffTxNILFSThunk :: (IncK (IncForest TxNILFS) a,TxNILFSLayer l) => BuffTxNILFSThunk l a -> MkWeak -> TxNILFSThunk l a -> DynTxNILFSThunk

dynTxMkWeak :: DynTxNILFSThunk -> MkWeak
dynTxMkWeak (DynTxFSThunk _ mkWeak _) = mkWeak
dynTxMkWeak (DynTxHSThunk _ mkWeak _) = mkWeak
dynTxMkWeak (DynBuffTxNILFSThunk _ mkWeak _) = mkWeak

data WeakTxNILFSThunk where
	WeakTxNILFSThunk :: (IncK (IncForest TxNILFS) a,TxNILFSLayer l) => Weak (TxNILFSThunk l a) -> WeakTxNILFSThunk

data BuffTxNILFSThunk l a = BuffTxNILFSThunk {
	  buffTxNILFSData :: TxNILFSThunkData l a
	, buffTxNILFSTree :: FSTree TxNILFS -- latest change to the thunk
	, buffTxNILFSDeltaTree :: FSTree TxNILFS -- latest recursive change
	, buffTxNILFSParents :: TxNILFSParents -- parent thunks
	} deriving Typeable

-- inner thunks
-- buffered for nested transactions
data TxNILFSThunk (l :: * -> (* -> *) -> (* -> *) -> * -> *) a = TxNILFSThunk {
	  txNILFSId :: Unique
	, txNILFSBuff :: IORef (BuffTxNILFSThunk l a)
	, txNILFSLock :: Lock
	} deriving Typeable

data TxNILFSThunkData l a =
		TxNILFSThunkComp (l (IncForest TxNILFS) IORef IO a)
	| 	TxNILFSThunkForce a TxNILFSStone

-- a dummy reference to be used as key for weak references
type TxNILFSStone = IORef ()

type TxNILFSParents = WeakMap Unique (WeakTxNILFSThunk,MkWeak)

bufferTxNILFSThunk :: (IncK (IncForest TxNILFS) a,TxNILFSLayer l) => TxNILFSThunk l a -> BuffTxNILFSThunk l a -> ForestM TxNILFS ()
bufferTxNILFSThunk thunk buff = do
	let uid = txNILFSId thunk
	let mkWeak = MkWeak $ mkWeakRefKey $ txNILFSBuff thunk
	bufferTxNILFSThunk' thunk uid mkWeak buff

bufferTxNILFSThunk' :: (IncK (IncForest TxNILFS) a,TxNILFSLayer l) => TxNILFSThunk l a -> Unique -> MkWeak -> BuffTxNILFSThunk l a -> ForestM TxNILFS ()
bufferTxNILFSThunk' thunk uid mkWeak buff = bufferTxNILFSThunk'' uid (DynBuffTxNILFSThunk buff mkWeak thunk)

bufferTxNILFSThunk'' :: Unique -> DynTxNILFSThunk -> ForestM TxNILFS ()
bufferTxNILFSThunk'' uid dyn = do
	(starttime,txid,deltas,SCons txlog _) <- Reader.ask
	(tree,delta,newtree,reads,bufftbl,memotbl,tmps) <- forestIO $ readIORef txlog
	forestIO $ WeakTable.insertWithMkWeak bufftbl (dynTxMkWeak dyn) uid dyn

bufferedTxNILFSThunk :: (IncK (IncForest TxNILFS) a,TxNILFSLayer l) => TxNILFSThunk l a -> ForestM TxNILFS (BuffTxNILFSThunk l a)
bufferedTxNILFSThunk thunk = liftM fst $ bufferedTxNILFSThunk' thunk (txNILFSId thunk)

bufferedTxNILFSThunk' :: (IncK (IncForest TxNILFS) a,TxNILFSLayer l) => TxNILFSThunk l a -> Unique -> ForestM TxNILFS (BuffTxNILFSThunk l a,MkWeak)
bufferedTxNILFSThunk' thunk uid = do
	dyn <- bufferedTxNILFSThunk'' thunk uid
	case dyn of
		(DynBuffTxNILFSThunk (cast -> Just buff) mkWeak thunk) -> return (buff,mkWeak)

bufferedTxNILFSThunk'' :: (IncK (IncForest TxNILFS) a,TxNILFSLayer l) => TxNILFSThunk l a -> Unique -> ForestM TxNILFS DynTxNILFSThunk
bufferedTxNILFSThunk'' thunk uid = do
	(starttime,txid,deltas,txlogs) <- Reader.ask
	-- read from the concurrent global thunk
	let newBuff = do
		let mkWeak = MkWeak $ mkWeakRefKey $ txNILFSBuff thunk
		buff <- forestIO $ readIORef $ txNILFSBuff thunk
		newParents <- forestIO $ copyWithKey snd (buffTxNILFSParents buff)
		let buff' = buff { buffTxNILFSParents = newParents }
		return $ DynBuffTxNILFSThunk buff' mkWeak thunk
	-- read from the local buffer
	let find txlog m = do
		(tree,ds,newtree,reads,bufftbl,memotbl,tmps) <- forestIO $ readRef txlog
		mb <- forestIO $ WeakTable.lookup bufftbl uid
		case mb of
			Nothing -> m
			Just dyn -> return dyn
	Foldable.foldr find newBuff txlogs

dirtyTxNILFSParents :: FSTree TxNILFS -> TxNILFSParents -> ForestM TxNILFS ()
dirtyTxNILFSParents tree parents = WeakMap.mapM_' forestIO dirty parents where
	dirty (uid,(w,mk)) = do
		mb <- changeTxNILFSThunkW w $ \buff -> do
			let oldtree = buffTxNILFSDeltaTree buff
			cmp <- forestM $ compareFSTree oldtree tree
			let b = cmp > LT
			let buff' = if b then buff else buff { buffTxNILFSDeltaTree = tree }
			return (buff',(b,parents))
		case mb of
			Just (b,parents) -> unless b $ dirtyTxNILFSParents tree parents
			Nothing -> return ()

class AddTxNILFSParent l a where
	addTxNILFSParent :: Proxy l -> TxNILFSStone -> Unique -> FSTree TxNILFS -> a -> ForestL TxNILFS l (FSTree TxNILFS)

addTxNILFSParentProxy :: Proxy l -> Proxy (AddTxNILFSParentDict l)
addTxNILFSParentProxy _ = Proxy

data AddTxNILFSParentDict l a = AddTxNILFSParentDict { addTxNILFSParentDictDict :: Proxy l -> TxNILFSStone -> Unique -> FSTree TxNILFS -> a -> ForestL TxNILFS l (FSTree TxNILFS) }

instance (AddTxNILFSParent l a) => Sat (AddTxNILFSParentDict l a) where
	dict = AddTxNILFSParentDict { addTxNILFSParentDictDict = addTxNILFSParent }

instance (IncK (IncForest TxNILFS) a,Typeable l,Typeable a,TxNILFSLayer l) => AddTxNILFSParent l (ForestFSThunk TxNILFS l a) where
	addTxNILFSParent proxy stone uid z (var :: ForestFSThunk TxNILFS l a) = do
		(thunk :: TxNILFSThunk l a) <- forestM $ bufferedTxNILFSFSThunk var
		let add buff = do
			w <- forestM $ forestIO $ Weak.mkWeak thunk thunk Nothing
			let mkWeak = MkWeak $ mkWeakRefKey stone
			forestM $ forestIO $ WeakMap.insertWithMkWeak (buffTxNILFSParents buff) mkWeak uid (WeakTxNILFSThunk w,mkWeak)
			tree <- treeTxNILFSThunk thunk
			return (buff,tree)
		changeTxNILFSThunk thunk add
instance (IncK (IncForest TxNILFS) a,Typeable l,Typeable a,TxNILFSLayer l) => AddTxNILFSParent l (ForestHSThunk TxNILFS l a) where
	addTxNILFSParent proxy stone uid z (var :: ForestHSThunk TxNILFS l a) = do
		(thunk :: TxNILFSThunk l a) <- forestM $ bufferedTxNILFSHSThunk var
		let add buff = do
			w <- forestM $ forestIO $ Weak.mkWeak thunk thunk Nothing
			let mkWeak = MkWeak $ mkWeakRefKey stone
			forestM $ forestIO $ WeakMap.insertWithMkWeak (buffTxNILFSParents buff) mkWeak uid (WeakTxNILFSThunk w,mkWeak)
			tree <- treeTxNILFSThunk thunk
			return (buff,tree)
		changeTxNILFSThunk thunk add
instance (ForestLayer TxNILFS l) => AddTxNILFSParent l Forest_err where
	addTxNILFSParent proxy stone meta z x = return z
instance (ForestLayer TxNILFS l) => AddTxNILFSParent l FileInfo where
	addTxNILFSParent proxy stone meta z x = return z
instance (ForestLayer TxNILFS l,AddTxNILFSParent l (ForestFSThunkI TxNILFS Forest_err)) => AddTxNILFSParent l (Forest_md TxNILFS) where
	addTxNILFSParent proxy stone meta z fmd = addTxNILFSParent proxy stone meta z (errors fmd)
instance (ForestLayer TxNILFS l,MData (AddTxNILFSParentDict l) (ForestL TxNILFS l) a) => AddTxNILFSParent l a where
	addTxNILFSParent proxy stone meta z x = do
		let f t1 t2 = forestM $ maxFSTree t1 t2
		gmapQr (addTxNILFSParentProxy proxy) f z (addTxNILFSParentDictDict dict proxy stone meta z) x

newTxNILFSThunk :: (IncK (IncForest TxNILFS) a,TxNILFSLayer l) => l (IncForest TxNILFS) IORef IO a -> ForestM TxNILFS (TxNILFSThunk l a)
newTxNILFSThunk m = do
	tree <- latestTree
	uid <- forestIO $ newUnique
	stone <- forestIO $ newRef uid
	let dta = TxNILFSThunkComp m
	let delta = tree
	parents <- forestIO $ WeakMap.new
	buff <- forestIO $ newIORef $ BuffTxNILFSThunk dta tree delta parents
	lock <- forestIO $ Lock.new
	return $ TxNILFSThunk uid buff lock

readTxNILFSThunk :: (IncK (IncForest TxNILFS) a,AddTxNILFSParent l a,TxNILFSLayer l) => TxNILFSThunk l a -> FSTree TxNILFS -> ForestL TxNILFS l (a,FSTree TxNILFS)
readTxNILFSThunk t mtree = do
	let eval buff = case buffTxNILFSData buff of
		TxNILFSThunkComp force -> do
			a <- force
			stone <- forestM $ forestIO $ newIORef ()
			let dta' = TxNILFSThunkForce a stone
			let uid = txNILFSId t
			-- add current thunk as a parent of its content thunks
			deltatree <- addTxNILFSParent Proxy stone uid mtree a
			return (buff { buffTxNILFSData = dta', buffTxNILFSDeltaTree = deltatree },(a,deltatree))
		TxNILFSThunkForce a stone -> return (buff,(a,mtree))
	changeTxNILFSThunk t eval

changeTxNILFSThunk :: (IncK (IncForest TxNILFS) a,TxNILFSLayer l) => TxNILFSThunk l a -> (BuffTxNILFSThunk l a -> ForestL TxNILFS l (BuffTxNILFSThunk l a,b)) -> ForestL TxNILFS l b
changeTxNILFSThunk thunk f = do
	buff <- forestM $ bufferedTxNILFSThunk thunk
	(buff',b) <- f buff
	forestM $ bufferTxNILFSThunk thunk buff'
	return b

changeTxNILFSThunkW :: WeakTxNILFSThunk -> (forall a l . (IncK (IncForest TxNILFS) a,TxNILFSLayer l) => BuffTxNILFSThunk l a -> ForestL TxNILFS l (BuffTxNILFSThunk l a,b)) -> ForestM TxNILFS (Maybe b)
changeTxNILFSThunkW (WeakTxNILFSThunk w) f = do
	mb <- forestIO $ Weak.deRefWeak w
	case mb of
		Nothing -> return Nothing
		Just thunk -> liftM Just $ forestO $ outside $ changeTxNILFSThunk thunk f

-- strict variable write (value)
writeTxNILFSThunk :: (IncK (IncForest TxNILFS) a,AddTxNILFSParent l a,TxNILFSLayer l) => TxNILFSThunk l a -> a -> ForestO TxNILFS ()
writeTxNILFSThunk var a = do
	newtree <- forestM getNextFSTree
	let set buff = do
		stone <- forestM $ forestIO $ newIORef ()
		let dta' = TxNILFSThunkForce a stone
		let uid = txNILFSId var
		-- mark the written thunk as a parent to its content thunks
		_ <- addTxNILFSParent Proxy stone uid (newtree) a
		-- dirty the parents of the written thunk
		forestM $ dirtyTxNILFSParents newtree (buffTxNILFSParents buff)
		return (buff { buffTxNILFSData = dta', buffTxNILFSTree = newtree, buffTxNILFSDeltaTree = newtree },())
	outside $ changeTxNILFSThunk var set

-- lazy variable write (expression)
overwriteTxNILFSThunk :: (IncK (IncForest TxNILFS) a,AddTxNILFSParent l a,TxNILFSLayer l) => TxNILFSThunk l a -> l (IncForest TxNILFS) IORef IO a -> ForestO TxNILFS ()
overwriteTxNILFSThunk var ma = do
	newtree <- forestM getNextFSTree
	let set buff = do
		let dta' = TxNILFSThunkComp ma
		-- dirty the parents of the written thunk
		forestM $ dirtyTxNILFSParents newtree (buffTxNILFSParents buff)
  	-- assuming that this is only used by loadDelta at the latest tree
		return (buff { buffTxNILFSData = dta', buffTxNILFSTree = newtree, buffTxNILFSDeltaTree = newtree },())
	outside $ changeTxNILFSThunk var set
	

-- memoize internal thunks, not FSThunks
instance ZippedICMemo TxNILFS where

	addZippedMemo path proxy args rep mb_tree = do
		let var = to iso_rep_thunk rep
		(starttime,txid,deltas,SCons txlog _) <- Reader.ask
		(tree,ds,newtree,reads,bufftbl,memotbl,tmps) <- forestM $ forestIO $ readRef txlog
		
		-- remember the arguments (this is how we connect transactional arguments to transactional variables)
		let txargs = (toDyn args,path)
		forestM $ forestIO $ writeRef (txNILFSFSThunkArgs var) $ Just txargs
		
		-- memoize the entry
		case mb_tree of
			Nothing -> return ()
			Just tree -> do
				let mkWeak = MkWeak $ mkWeakRefKey $ txNILFSFSThunkArgs var
				forestM $ forestIO $ WeakTable.insertWithMkWeak memotbl mkWeak (path,typeOf rep) (DynNILFS rep,mkWeak)
	
	findZippedMemo args path (Proxy :: Proxy rep) = do
		(starttime,txid,deltas,SCons txlog _) <- Reader.ask
		(tree,ds,newtree,reads,bufftbl,memotbl,tmps) <- forestM $ forestIO $ readRef txlog
		mb <- forestM $ forestIO $ WeakTable.lookup memotbl (path,typeOf (undefined :: rep))
		case mb of
			Nothing -> return Nothing
			Just (fromDynNILFS -> Just rep,mkWeak) -> do
				let var = to iso_rep_thunk rep
				Just (fromDynamic -> Just txargs,_) <- forestM $ forestIO $ readRef $ txNILFSFSThunkArgs var
				thunk <- forestM $ bufferedTxNILFSFSThunk var
 				buff <- forestM $ bufferedTxNILFSThunk thunk
				return $ Just (buffTxNILFSTree buff,txargs,rep)

argsTxNILFS :: (FTK TxNILFS args rep var content) => Proxy args -> rep -> ForestO TxNILFS (ForestVs args,FilePath)
argsTxNILFS proxy rep = do
	mb <- forestM $ getFTVArgsNILFS proxy rep
	case mb of
		Nothing -> error "should not happen"
		Just (margs,path) -> liftM (,path) $ inside $ vArgs Proxy proxy margs

getFTVArgsNILFS :: (FTK TxNILFS args rep var content) => Proxy args -> rep -> ForestM TxNILFS (Maybe (ForestIs TxNILFS args,FilePath))
getFTVArgsNILFS (proxy ::Proxy args) rep = forestIO $ do
	let var = to iso_rep_thunk rep
	mb <- readIORef (txNILFSFSThunkArgs var)
	case mb of
		Just (fromDynamic -> Just args,path) -> return $ Just (args,path)
		otherwise -> return Nothing

instance Thunk (HSThunk TxNILFS) Inside (IncForest TxNILFS) IORef IO where
	new m = do
		uid <- forestM $ forestIO newUnique
		txargs <- forestM $ forestIO $ newIORef Nothing
		thunk <- forestM $ newTxNILFSThunk m
		mthunk <- forestM $ forestIO $ newMVar thunk
		let var = TxNILFSHSThunk uid mthunk txargs
		return var
	read var = do
		thunk <- forestM $ bufferedTxNILFSHSThunk var
		tree <- forestM latestTree
		liftM fst $ readTxNILFSThunk thunk tree
instance Thunk (HSThunk TxNILFS) Outside (IncForest TxNILFS) IORef IO where
	new m = do
		uid <- forestM $ forestIO newUnique
		txargs <- forestM $ forestIO $ newIORef Nothing
		thunk <- forestM $ newTxNILFSThunk m
		mthunk <- forestM $ forestIO $ newMVar thunk
		let var = TxNILFSHSThunk uid mthunk txargs
		return var
	read var = do
		thunk <- forestM $ bufferedTxNILFSHSThunk var
		tree <- forestM latestTree
		liftM fst $ readTxNILFSThunk thunk tree

instance TxNILFSLayer l => Thunk (ICThunk TxNILFS) l (IncForest TxNILFS) IORef IO where
	new m = return $ TxNILFSICThunk m
	read (TxNILFSICThunk m) = m
instance TxNILFSLayer l => Output (ICThunk TxNILFS) l (IncForest TxNILFS) IORef IO where
	thunk = Inc.new
	force = Inc.read

instance Thunk (FSThunk TxNILFS) Inside (IncForest TxNILFS) IORef IO where
	new m = do
		uid <- forestM $ forestIO newUnique
		txargs <- forestM $ forestIO $ newIORef Nothing
		thunk <- forestM $ newTxNILFSThunk m
		mthunk <- forestM $ forestIO $ newMVar thunk
		let var = TxNILFSFSThunk uid mthunk txargs
		return var
	-- read on the internal thunk, not on the latest filesystem
	read var = do
		thunk <- forestM $ bufferedTxNILFSFSThunk var
		tree <- forestM latestTree
		liftM fst $ readTxNILFSThunk thunk tree
instance Thunk (FSThunk TxNILFS) Outside (IncForest TxNILFS) IORef IO where
	new m = do
		uid <- forestM $ forestIO newUnique
		txargs <- forestM $ forestIO $ newIORef Nothing
		thunk <- forestM $ newTxNILFSThunk m
		mthunk <- forestM $ forestIO $ newMVar thunk
		let var = TxNILFSFSThunk uid mthunk txargs
		return var
	-- read on the internal thunk, not on the latest filesystem
	read var = do
		thunk <- forestM $ bufferedTxNILFSFSThunk var
		tree <- forestM latestTree
		liftM fst $ readTxNILFSThunk thunk tree

instance Input (FSThunk TxNILFS) Inside (IncForest TxNILFS) IORef IO where
	ref c = Inc.new (return c)
	mod = Inc.new
	get = Inc.read
	set (var :: ForestFSThunk TxNILFS Inside a) v = do
		(thunk :: TxNILFSThunk Inside a) <- forestM $ bufferedTxNILFSFSThunk var
		writeTxNILFSThunk thunk v
	overwrite (var :: ForestFSThunk TxNILFS Inside a) mv = do
		(thunk :: TxNILFSThunk Inside a) <- forestM $ bufferedTxNILFSFSThunk var
		overwriteTxNILFSThunk thunk mv
instance Input (FSThunk TxNILFS) Outside (IncForest TxNILFS) IORef IO where
	ref c = Inc.new (return c)
	mod = Inc.new
	get = Inc.read
	set (var :: ForestFSThunk TxNILFS Outside a) v = do
		(thunk :: TxNILFSThunk Outside a) <- forestM $ bufferedTxNILFSFSThunk var
		writeTxNILFSThunk thunk v
	overwrite (var :: ForestFSThunk TxNILFS Outside a) mv = do
		(thunk :: TxNILFSThunk Outside a) <- forestM $ bufferedTxNILFSFSThunk var
		overwriteTxNILFSThunk thunk mv

-- ** Transactions

-- the Forest transactional monad
type TxNILFSFTM = FTM TxNILFS
-- a Forest transactional variable
type TxNILFSFTV a = FTV TxNILFS a

instance TxICForest TxNILFS where
	
	atomically = atomicallyTxNILFS
	retry = retryTxNILFS
	orElse = orElseTxNILFS
	throw = throwTxNILFS 
	catch = catchTxNILFS 
	new = newTxNILFS Proxy
	args = argsTxNILFS Proxy
	read = readTxNILFS Proxy
	writeOrElse = writeOrElseTxNILFS
	delete = deleteTxNILFS Proxy
	copyOrElse = copyOrElseTxNILFS Proxy

deleteTxNILFS :: FTK TxNILFS args rep var content => Proxy args -> rep -> TxNILFSFTM ()
deleteTxNILFS proxy (rep :: rep) = do
	mb <- forestM $ getFTVArgsNILFS proxy rep
	case mb of
		Nothing -> error "tried to write to a variable that is not connected to the FS"
		Just (args,path) -> do
			def_rep :: rep <- inside $ zdefaultScratchMemo proxy args path
			content <- liftM (BX.get lens_content) $ Inc.getOutside $ to iso_rep_thunk def_rep
			writeOrElseTxNILFS rep content () (error "failed to write")

copyOrElseTxNILFS :: (FTK TxNILFS args rep var content) => Proxy args -> rep -> rep -> b -> ([ManifestError] -> TxNILFSFTM b) -> TxNILFSFTM b
copyOrElseTxNILFS proxy src tgt b f = do
	mb_src <- forestM $ getFTVArgsNILFS proxy src
	mb_tgt <- forestM $ getFTVArgsNILFS proxy tgt
	case (mb_src,mb_tgt) of
		(Just (args_src,path_src),Just (args_tgt,path_tgt)) -> do
			-- XXX: this may fail if FileInfo paths are canonized...
			let chgPath path = path_tgt </> (makeRelative path_src path)
			tgt' <- copyFSThunks proxyTxNILFS proxyOutside chgPath src
			content' <- liftM (BX.get lens_content) $ Inc.getOutside $ to iso_rep_thunk tgt'
			writeOrElseTxNILFS tgt content' b f
		otherwise -> error "tried to write to a variable that is not connected to the FS"

newTxNILFS :: FTK TxNILFS args rep var content => Proxy args -> ForestVs args -> FilePath -> TxNILFSFTM rep
newTxNILFS proxy args path = inside $ zload (vmonadArgs proxyTxNILFS proxy args) path

-- incrementally repair the thunk to the latest tree
loadTxNILFS :: (ForestLayer TxNILFS l,FTK TxNILFS args rep var content) => Proxy args -> rep -> ForestL TxNILFS l ()
loadTxNILFS proxy rep = do
	let t = to iso_rep_thunk rep
	(txargs,path) <- liftM fromJust $ forestM $ getFTVArgsNILFS proxy rep
	oldtree <- inside $ treeTxNILFSFSThunk t
	tree <- forestM latestTree
	df <- liftM fromJust $ forestM $ diffFS oldtree tree path
	-- load incrementally at the latest tree
	inside $ unsafeWorld $ do
		dv <- diffValueThunk oldtree rep
		ds <- deltaArgs oldtree proxy txargs txargs
		let deltas = (txargs,ds)
		zloadDeltaMemo proxy deltas (return path) oldtree (rep,getForestMDInTree) path df tree dv
	return ()

-- read a transactional variable
readTxNILFS :: (ForestLayer TxNILFS l,FTK TxNILFS args rep var content) => Proxy args -> rep -> ForestL TxNILFS l content
readTxNILFS proxy rep = do
	loadTxNILFS proxy rep
	let t = to iso_rep_thunk rep
	liftM (BX.get lens_content) $ inside $ Inc.get t

data ManifestConflictTx = ManifestConflictTx [ManifestError] deriving (Show,Eq,Typeable)
instance Exception ManifestConflictTx

writeOrElseTxNILFS :: FTK TxNILFS args rep var content => rep -> content -> b -> ([ManifestError] -> TxNILFSFTM b) -> TxNILFSFTM b
writeOrElseTxNILFS rep content b f = do

	let t = to iso_rep_thunk rep
		
	mb :: (Maybe (ForestIs TxNILFS args,FilePath)) <- forestM $ getFTVArgsNILFS Proxy rep
	case mb of
		Nothing -> error "writeOrElseTxNILFS: should not happen" -- the top-level arguments of a variable don't match the spec
		Just (txargs,path) -> do
			let proxy = Proxy :: Proxy args
			oldtree <- inside $ treeTxNILFSFSThunk t
			tree <- forestM latestTree
			df <- liftM fromJust $ forestM $ diffFS oldtree tree path
			dv <- diffValueThunk oldtree rep
			ds <- deltaArgs oldtree proxy txargs txargs
			let deltas = (txargs,ds)
			
			-- update the value to the latest filesystem (since some variables may be outdated due to modifications to other descriptions)
			-- this is necessary because the store function reads the content from the inner thunks, that are not necessarily in sync with the latest filesystem
			-- skipping this step would eventually overwrite user modifications with older values!!
			zloadDeltaMemo proxy deltas (return path) oldtree (rep,getForestMDInTree) path df tree dv
			
			let tryWrite = do
				-- write the new content
				modify t $ \s -> return $ BX.put lens_content s content
				
				-- compute new value deltas for the write
				newdv <- diffValueThunk tree rep
				newds <- deltaArgs tree proxy txargs txargs
				let newdeltas = (txargs,newds)
				
				-- store incrementally at the latest tree (there are no filesystem modifications, since have only c hanged the value since loadDelta)
				man <- forestM $ newManifestWith "/" tree
				(mani,_,memos) <- RWS.runRWST (zupdateManifestDeltaMemo proxy newdeltas path path tree (emptyFSTreeD proxyTxNILFS) tree rep newdv man) True ()
				-- we need to store the errors to the (buffered) FS before validating
				forestM $ storeManifest mani
				-- commit the pending modifications
				forestM $ incrementTxNILFSTree
				
				forestM $ forestIO $ putStrLn "Manifest!"
				forestM $ forestIO $ print mani
				errors <- forestM $ manifestErrors mani
				if List.null errors
					then forestM latestTree >>= memos >> return b
					else throwTxNILFS $ ManifestConflictTx errors
			catchTxNILFS tryWrite (\(ManifestConflictTx errors) -> f errors)
					
atomicallyTxNILFS :: TxNILFSFTM b -> IO b
atomicallyTxNILFS stm = initializeTxNILFS try where
	try = flip Catch.catches [Catch.Handler catchInvalid,Catch.Handler catchRetry,Catch.Handler catchSome] $ do
		-- run the tx
		x <- stm
		-- tries to commit the current tx, otherwise repairs it incrementally
		success <- validateAndCommitTopTxNILFS True
		if success
			then return x
			else debug "throw InvalidTx" $ throwM InvalidTx
	catchInvalid InvalidTx = debug "InvalidTx" $ do
		resetTxNILFS try
	catchRetry BlockedOnRetry = debug "BlockedOnRetry" $ do
		-- if the retry was invoked on an inconsistent state, we incrementally repair and run again, otherwise we place the tx in the waiting queue
		mbsuccess <- validateAndRetryTopTxNILFS
		case mbsuccess of
			Just lck -> do -- retried txs are always in a consistent state, because we apply all affecting updates before releasing the lock
				-- wait for the lock to be released (whenever some variables that it depends on are changed)
				-- we don't consume the contents of the mvar to avoid further puts to succeeed; a new MVar is created for each retry
				inL $ liftIO $ Lock.acquire lck
				resetTxNILFS try
			Nothing -> resetTxNILFS try
	catchSome (e::SomeException) = debug ("SomeException "++show e) $ do
		-- we still need to validate on exceptions, otherwise repair incrementally; transaction-local allocations still get committed
		success <- validateAndCommitTopTxNILFS False
		if success
			then throwM e
			else do
				resetTxNILFS try

retryTxNILFS :: TxNILFSFTM a
retryTxNILFS = inL $ liftIO $ throwIO BlockedOnRetry

orElseTxNILFS :: TxNILFSFTM a -> TxNILFSFTM a -> TxNILFSFTM a
orElseTxNILFS stm1 stm2 = do1 where
	try1 = do { x <- stm1; validateAndCommitNestedTxNILFS Nothing; return x }
	try2 = do { x <- stm2; validateAndCommitNestedTxNILFS Nothing; return x }
	do1 = startNestedTxNILFS $ try1 `Catch.catches` [Catch.Handler catchRetry1,Catch.Handler catchInvalid,Catch.Handler catchSome]
	do2 = dropChildTxNILFSLog $ startNestedTxNILFS $ try2 `Catch.catches` [Catch.Handler catchRetry2,Catch.Handler catchInvalid,Catch.Handler catchSome]
	catchRetry1 BlockedOnRetry = validateAndRetryNestedTxNILFS >> do2
	catchRetry2 BlockedOnRetry = validateAndRetryNestedTxNILFS >> throwM BlockedOnRetry
	catchInvalid (e::InvalidTx) = throwM e
	catchSome (e::SomeException) = validateAndCommitNestedTxNILFS (Just e) >> throwM e

throwTxNILFS :: Exception e => e -> TxNILFSFTM a
throwTxNILFS = Catch.throwM

catchTxNILFS :: Exception e => TxNILFSFTM a -> (e -> TxNILFSFTM a) -> TxNILFSFTM a
catchTxNILFS stm h = stm `Catch.catches` [Catch.Handler catchInvalid,Catch.Handler catchRetry,Catch.Handler catchSome] where
	catchInvalid (e::InvalidTx) = throwM e
	catchRetry (e::BlockedOnRetry) = throwM e
	catchSome (e::SomeException) = do
		validateCatchTxNILFS
		h $ fromJust $ fromException e

initializeTxNILFS :: TxNILFSFTM b -> IO b 
initializeTxNILFS (TxNILFSForestO m) = do
	starttime <- startTxNILFS >>= newIORef
	mountAVFS -- should succeed even if AVFS is already mounted
	forestDir <- getTemporaryDirectory
	createDirectoryIfMissing True forestDir
	createDirectoryIfMissing True (forestDir </> "Snapshots")
	(txid,_,_) <- latestCheckpoint
	-- current tree
	treeDelta <- newIORef $ emptyFSTreeDelta
	treeSyms <- newIORef =<< liftM (DList.empty,) (readMVar symlinks)
	tree <- liftM FSTreeTxNILFS $ newIORef $ NILFSTree txid False
	-- next tree
	newtreeDelta <- newIORef $ emptyFSTreeDelta
	newtreeSyms <- newIORef =<< liftM (DList.empty,) (readMVar symlinks)
	newtree <- liftM FSTreeTxNILFS $ newIORef $ TxTree txid 1 newtreeDelta False newtreeSyms
	-- tables
	bufftbl <- WeakTable.new
	memotbl <- WeakTable.new
	txlog <- newIORef (tree,DList.empty,newtree,Set.empty,bufftbl,memotbl,Set.empty)
	deltas <- newIORef Map.empty
	debug ("initializeTxNILFS") $ Reader.runReaderT m (starttime,txid,deltas,SCons txlog SNil)
	-- don't unmount AVFS, since multiple parallel txs may be using it

-- resets
resetTxNILFS :: TxNILFSFTM a -> TxNILFSFTM a
resetTxNILFS m = do
	now <- inL $ liftIO $ startTxNILFS >>= newIORef
	(_,txid,deltas,_) <- Reader.ask
	-- current tree
	tree <- forestM $ forestIO $ liftM FSTreeTxNILFS $ newIORef $ NILFSTree txid False
	-- next tree
	newtreeDelta <- forestM $ forestIO $ newIORef $ emptyFSTreeDelta
	newtreeSyms <- forestM $ forestIO $ newIORef =<< liftM (DList.empty,) (readMVar symlinks)
	newtree <- forestM $ forestIO $ liftM FSTreeTxNILFS $ newIORef $ TxTree txid 1 newtreeDelta False newtreeSyms
	-- tables
	bufftbl <- forestM $ forestIO $ WeakTable.new
	memotbl <- forestM $ forestIO $ WeakTable.new
	txlog <- forestM $ forestIO $ newIORef (tree,DList.empty,newtree,Set.empty,bufftbl,memotbl,Set.empty)
	forestM $ forestIO $ writeIORef deltas Map.empty
	debug ("resetTxNILFS") $ Reader.local (Prelude.const $ (now,txid,deltas,SCons txlog SNil)) m

-- we need to acquire a lock, but this should be minimal
startTxNILFS :: IO UTCTime
startTxNILFS = getCurrentTime >>= \t -> addRunningTxNILFS t >> return t

-- appends a freshly created txlog for the inner tx
-- the starting time reference is preserved
--we create a new fsversion reference
startNestedTxNILFS :: TxNILFSFTM a -> TxNILFSFTM a
startNestedTxNILFS m = do
	(starttime,txid,deltas,txlogs@(SCons txlog_parent _)) <- Reader.ask
	(tree,delta,newtree,reads,_,_,tmps) <- inL $ readIORef txlog_parent
	bufftbl' <- forestM $ forestIO $ WeakTable.new
	memotbl' <- forestM $ forestIO $ WeakTable.new
	txlog_child <- inL $ newIORef (tree,delta,newtree,Set.empty,bufftbl',memotbl',Set.empty)
	debug ("startNestedTxNILFS ") $ Reader.local (Prelude.const (starttime,txid,deltas,SCons txlog_child txlogs)) m

-- remember to empty the next tree and delete the new versions in case nested txs fail
dropChildTxNILFSLog :: TxNILFSFTM a -> TxNILFSFTM a
dropChildTxNILFSLog m = do
	(starttime,txid,deltas,(SCons txlog_child txlogs@(SCons txlog_parent _))) <- Reader.ask
	(tree_parent,_,newtree_parent,_,_,_,_) <- inL $ readIORef txlog_parent
	-- forget newly created nested versions
	tree_parentVersion <- forestM $ forestIO $ fsTreeFSVersion tree_parent
	forestM $ forestIO $ modifyIORef deltas $ Map.filterWithKey (\version _ -> version <= tree_parentVersion)
	-- reset the next tree
	newtree_parentDelta <- forestM $ forestIO $ fsTreeFSTreeDelta newtree_parent
	forestM $ forestIO $ writeIORef newtree_parentDelta emptyFSTreeDelta
	Reader.local (Prelude.const (starttime,txid,deltas,txlogs)) m

-- if an inner tx validation fails, then we throw an @InvalidTx@ exception to retry the whole atomic block
data InvalidTx = InvalidTx deriving (Typeable)
instance Show (InvalidTx) where
	show InvalidTx = "InvalidTx"
instance Exception InvalidTx
data BlockedOnRetry = BlockedOnRetry deriving (Show,Typeable)
instance Exception BlockedOnRetry

-- returns a bool stating whether the transaction was committed or needs to be incrementally repaired
-- no exceptions should be raised inside this block
validateAndCommitTopTxNILFS :: Bool -> TxNILFSFTM Bool
validateAndCommitTopTxNILFS doWrites = do
	-- update the memoized transactional variables to the latest fs tree (outside the locks)
	(starttime,txid,deltas,SCons txlog _) <- Reader.ask
	(tree,deltas,newtree,reads,bufftbl,memotbl,tmps) <- forestM $ forestIO $ readIORef txlog
	repairTxNILFSMemoTable memotbl
	-- perform the actual commit
	atomicTxNILFS "validateAndCommitTopTxNILFS" $ do
		txenv@(timeref,txid,deltas,txlogs@(SCons txlog SNil)) <- Reader.ask
		starttime <- inL $ readRef timeref
		success <- forestM $ validateTxsNILFS starttime txlogs
		case success of
			Left mode -> do
				if mode then forestM $ finishTopTxNILFS starttime txlog else forestM $ commitTopTxNILFS doWrites starttime txlog
				return True
			Right () -> do
				inL $ liftIO $ deleteRunningTxNILFS starttime
				return False

validateAndCommitNestedTxNILFS :: Maybe SomeException -> TxNILFSFTM ()
validateAndCommitNestedTxNILFS mbException = do
	txenv@(timeref,txid,deltas,txlogs@(SCons txlog1 (SCons txlog2 _))) <- Reader.ask
	starttime <- inL $ readRef timeref
	case mbException of
		Just e -> do -- throwing an exception exits the chain of txs one by one
			forestM $ commitNestedTxNILFS False deltas txlog1 txlog2 -- does not perform @Write@s
		Nothing -> do
			-- validates the current and enclosing txs up the tx tree
			success <- forestM $ validateTxsNILFS starttime txlogs
			case success of
				Left _ -> do
					forestM $ commitNestedTxNILFS True deltas txlog1 txlog2 -- performs @Write@s
				Right _ -> do
					inL $ liftIO $ deleteRunningTxNILFS starttime
					throwM InvalidTx

-- validates a transaction and places it into the waiting queue for retrying
validateAndRetryTopTxNILFS :: TxNILFSFTM (Maybe Lock)
validateAndRetryTopTxNILFS = atomicTxNILFS "validateAndRetryTopTxNILFS" $ do
	txenv@(timeref,txid,deltas,txlogs@(SCons txlog SNil)) <- Reader.ask
	starttime <- inL $ readRef timeref
	-- validates the current and enclosing txs up the tx tree
	success <- forestM $ validateTxsNILFS starttime txlogs
	case success of
		Left _ -> do
			lck <- inL $ liftIO $ Lock.newAcquired -- sets the tx lock as acquired; the tx will be resumed when the lock is released
			forestM $ commitTopTxNILFS False starttime txlog
			forestM $ retryTxNILFSLog lck timeref txlog -- wait on changes to retry (only registers waits, does not actually wait)
			return $ Just lck
		Right _ -> do
			inL $ liftIO $ deleteRunningTxNILFS starttime
			return Nothing

--registers waits for all the filepaths read by a txlog
--since we treat reads/writes separately, we don't need to wait on writes that have not been read
retryTxNILFSLog :: Lock -> IORef UTCTime -> TxNILFSLog -> ForestM TxNILFS ()
retryTxNILFSLog lck timeref txlog = do
	(tree,delta,newtree,reads,bufftbl,memotbl,tmps) <- forestIO $ readIORef txlog
	let retryPath path = do
		-- the reference to the lock lives as long as the transaction
		modifyMVar_ waitingTxsNILFS $ \xs -> do
			case Map.lookup path xs of
				Nothing -> newQ >>= \q -> return $ Map.insert path q xs
				Just q -> pushL q lck >> return xs
	forestIO $ Foldable.mapM_ retryPath reads

-- validates a nested transaction and merges its log with its parent
-- note that retrying discards the tx's writes
validateAndRetryNestedTxNILFS :: TxNILFSFTM ()
validateAndRetryNestedTxNILFS = do
	txenv@(timeref,txid,deltas,txlogs@(SCons txlog1 (SCons txlog2 _))) <- Reader.ask
	starttime <- inL $ readRef timeref
	success <- forestM $ validateTxsNILFS starttime txlogs
	case success of
		Left _ -> do
			forestM $ commitNestedTxNILFS False deltas txlog1 txlog2 -- does not perform @Write@s on @retry@
		Right _ -> do
			inL $ liftIO $ deleteRunningTxNILFS starttime
			throwM InvalidTx

-- validates the current log before catching an exception
validateCatchTxNILFS :: TxNILFSFTM ()
validateCatchTxNILFS = do
	txenv@(timeref,txid,deltas,txlogs) <- Reader.ask
	starttime <- inL $ readRef timeref
	success <- forestM $ validateTxsNILFS starttime txlogs
	case success of
		Left _ -> do
			-- in case the computation raises an exception, discard all its visible (write) effects
			forestM unbufferTxNILFSWrites
		Right _ -> do
			inL $ liftIO $ deleteRunningTxNILFS starttime
			throwM InvalidTx

-- we only unbuffer the child log
unbufferTxNILFSWrites :: ForestM TxNILFS ()
unbufferTxNILFSWrites = do
	(starttime,txid,deltas,txlogs@(SCons txlog1 _)) <- Reader.ask
	(tree1,delta1,newtree1,reads1,bufftbl1,memotbl1,tmps1) <- forestIO $ readRef txlog1
	bufftbl1' <- forestIO $ WeakTable.new
	memotbl1' <- forestIO $ WeakTable.new
	-- reset deltas
	forestIO $ writeRef deltas Map.empty
	-- current tree
	tree1' <- forestIO $ liftM FSTreeTxNILFS $ newIORef $ NILFSTree txid False
	-- next tree
	newtreeDelta1' <- forestIO $ newIORef $ emptyFSTreeDelta
	newtreeSym1' <- forestIO $ newIORef $ (DList.empty,Map.empty)
	newtree1' <- forestIO $ liftM FSTreeTxNILFS $ newIORef $ TxTree txid 1 newtreeDelta1' False newtreeSym1'
	forestIO $ writeRef txlog1 (tree1',DList.empty,newtree1',reads1,bufftbl1',memotbl1',tmps1)

-- on success, returns True=read-only or False=read-write
validateTxsNILFS :: UTCTime -> TxNILFSLogs -> ForestM TxNILFS (Either Bool ())
validateTxsNILFS starttime txlogs = do
	-- gets the transactions that committed after the current transaction's start time
	finished <- liftM (Map.toAscList . Map.filterWithKey (\k v -> k > starttime)) $ forestIO $ readMVar doneTxsNILFS
	before <- checkTxsNILFSBefore txlogs
	if before then return (Left True) else do
		after <- checkTxsNILFSAfter txlogs finished
		if after then return (Left False) else return (Right ())

checkTxsNILFSBefore :: TxNILFSLogs -> ForestM TxNILFS Bool
checkTxsNILFSBefore SNil = return True
checkTxsNILFSBefore env@(SCons txlog txlogs) = do
	b1 <- checkTxNILFSBefore txlog
	b2 <- checkTxsNILFSBefore txlogs
	return $ b1 && b2

-- checks if the current txlog is consistent with a sequence of concurrent post-modifications
checkTxNILFSBefore :: TxNILFSLog -> ForestM TxNILFS Bool
checkTxNILFSBefore txlog = do
	(tree,ds,newtree,reads,bufftbl,memotbl,tmps) <- forestIO $ readIORef txlog
	writes <- forestIO (readIORef =<< fsTreeFSTreeDelta tree)
	return $ writes == emptyFSTreeDelta -- tx has no writes

checkTxsNILFSAfter :: TxNILFSLogs -> [(UTCTime,TxNILFSWrites)] -> ForestM TxNILFS Bool
checkTxsNILFSAfter SNil finished = return True
checkTxsNILFSAfter env@(SCons txlog txlogs) finished = do
	b1 <- checkTxNILFSAfter txlog finished
	b2 <- checkTxsNILFSAfter txlogs finished
	return $ b1 && b2

-- checks if the current txlog is consistent with a sequence of concurrent pre-modifications
checkTxNILFSAfter :: TxNILFSLog -> [(UTCTime,TxNILFSWrites)] -> ForestM TxNILFS Bool
checkTxNILFSAfter txlog txchgs = do
	let txwrites = mconcat $ List.map snd txchgs
	(tree,ds,newtree,reads,bufftbl,memotbl,tmps) <- forestIO $ readIORef txlog
	return $ (reads `Set.intersection` txwrites) == Set.empty -- no write-read conflicts

-- finish a read-only transaction without commmitting modifications
-- read-only txs are not added to the @doneTxs@
finishTopTxNILFS :: UTCTime -> TxNILFSLog -> ForestM TxNILFS ()
finishTopTxNILFS starttime txlog = do
	-- deletes this transaction from the running list and gets the earliest running tx 
	mbearliestTx <- forestIO $ modifyMVarMasked runningTxsNILFS (\xs -> return (List.delete starttime xs,lastMay xs))
	-- finishes the current tx and deletes txs that finished before the start of the earliest running tx
	case mbearliestTx of
		Just earliestTx -> forestIO $ modifyMVarMasked_ doneTxsNILFS (\m -> let m' = Map.filterWithKey (\t _ -> t > earliestTx) m in m' `seq` return m')
		Nothing -> return ()

-- finish a read-write transaction by committing its modifications
commitTopTxNILFS :: Bool -> UTCTime -> TxNILFSLog -> ForestM TxNILFS ()
commitTopTxNILFS doWrites starttime txlog = do
	-- deletes this transaction from the running list and gets the earliest running tx 
	mbearliestTx <- forestIO $ modifyMVarMasked runningTxsNILFS (\xs -> return (List.delete starttime xs,lastMay xs))
	-- commits the log and gets a sequence of performed writes
	(reads,_) <- getTxNILFSChanges
	writes <- commitTxNILFSLog starttime doWrites txlog
	-- finishes the current tx and deletes txs that finished before the start of the earliest running tx
	let addDone time m = Map.insert time writes m
	now <- case mbearliestTx of
		Just earliestTx -> forestIO $ modifyMVarMasked doneTxsNILFS (\m -> getCurrentTime >>= \now -> let m' = Map.filterWithKey (\t _ -> t > earliestTx) (addDone now m) in m' `seq` return (m',now))
		Nothing -> forestIO $ modifyMVarMasked doneTxsNILFS (\m -> getCurrentTime >>= \now -> let m' = addDone now m in m' `seq` return (m',now))
	-- wakes up the transactions after updating their buffered content
	wakeUpWaitsTxNILFS writes

-- makes the parent log sensitive to the variables used in the nested branch
commitNestedTxNILFS :: Bool -> TxNILFSTreeDeltas -> TxNILFSLog -> TxNILFSLog -> ForestM TxNILFS ()
commitNestedTxNILFS doWrites deltas txlog_child txlog_parent = if doWrites
	then mergeTxNILFSLog txlog_child txlog_parent
	else extendTxNILFSLog deltas txlog_child txlog_parent

-- merges a nested txlog with its parent txlog
mergeTxNILFSLog :: TxNILFSLog -> TxNILFSLog -> ForestM TxNILFS ()
mergeTxNILFSLog txlog1 txlog2 = forestIO $ do
	(tree1,delta1,newtree1,reads1,bufftbl1,memotbl1,tmps1) <- readIORef txlog1
	(tree2,delta2,newtree2,reads2,bufftbl2,memotbl2,tmps2) <- readIORef txlog2
	
	WeakTable.mapM_ (\(uid,entry) -> WeakTable.insertWithMkWeak bufftbl2 (dynTxMkWeak entry) uid entry) bufftbl1
	WeakTable.mapM_ (\(uid,entry) -> WeakTable.insertWithMkWeak memotbl2 (snd entry) uid entry) memotbl1
	
	writeIORef txlog2 (tree1,delta1,newtree1,reads1 `Set.union` reads2,bufftbl2,memotbl2,tmps1 `Set.union` tmps2)
	
-- does not commit writes, just merges reads
extendTxNILFSLog :: TxNILFSTreeDeltas -> TxNILFSLog -> TxNILFSLog -> ForestM TxNILFS ()
extendTxNILFSLog deltas txlog1 txlog2 = forestIO $ do
	(tree1,delta1,newtree1,reads1,bufftbl1,memotbl1,tmps1) <- readIORef txlog1
	(tree2,delta2,newtree2,reads2,bufftbl2,memotbl2,tmps2) <- readIORef txlog2
	
	-- forget newly created nested versions
	version2 <- fsTreeFSVersion tree2
	modifyIORef deltas $ Map.filterWithKey (\version _ -> version <= version2)
	-- reset the next tree
	fsTreeFSTreeDelta newtree2 >>= \d -> writeIORef d emptyFSTreeDelta
	
	writeIORef txlog2 (tree2,delta2,newtree2,reads1 `Set.union` reads2,bufftbl2,memotbl2,tmps1 `Set.union` tmps2)

-- locks on which retrying transactions will wait
type WaitQueue = Deque Threadsafe Threadsafe SingleEnd SingleEnd Grow Safe Lock

-- a register of locks for retrying transactions
-- these paths should be canonical
{-# NOINLINE waitingTxsNILFS #-}
waitingTxsNILFS :: MVar (Map FilePath WaitQueue)
waitingTxsNILFS = unsafePerformIO $ newMVar Map.empty

-- wakes up the locks waiting on a given set of paths
wakeUpWaitsTxNILFS :: Set FilePath -> ForestM TxNILFS ()
wakeUpWaitsTxNILFS = Foldable.mapM_ wakeUpWaitTxNILFS

wakeUpWaitTxNILFS :: FilePath -> ForestM TxNILFS ()
wakeUpWaitTxNILFS path = forestIO $ do
	let wakeQueue q = do		
		mb <- tryPopR q
		case mb of
			Just lck -> tryReleaseTxNILFS lck >> wakeQueue q
			Nothing -> return ()
	mb <- modifyMVar waitingTxsNILFS (\xs -> return (Map.delete path xs,Map.lookup path xs))
	case mb of
		Nothing -> return ()
		Just q -> wakeQueue q

tryReleaseTxNILFS :: Lock -> IO ()
tryReleaseTxNILFS lck = do
	isLocked <- Lock.locked lck
	if isLocked then Lock.release lck else return ()

commitTxNILFSLog :: UTCTime -> Bool -> TxNILFSLog -> ForestM TxNILFS TxNILFSWrites
commitTxNILFSLog starttime doWrites txlog = do
	-- no pending changes (deltas == DList.empty)
	(tree,deltas,newtree,reads,bufftbl,memotbl,tmps) <- forestIO $ readIORef txlog
	
	wakes <- if doWrites
		then do
			-- acquire locks on the written memory addresses in sorted order, and commit their changes (memory modifications are done atomically)
			forestIO $ acquireTxNILFSBuffTable bufftbl
			forestIO $ commitTxNILFSBuffTable bufftbl
			-- commit the buffered memo table
			forestIO $ commitTxNILFSMemoTable memotbl
			-- get writes since the beginning of the tx
			writes <- forestIO (readIORef =<< fsTreeFSTreeDelta tree)
			-- commits filesystem modifications over the latest NILFS snapshot
			wakes <- forestIO $ commitFSTreeDelta "" writes
			-- get the symlink changes since the beginning of the tx
			(syms,_) <- forestIO (readIORef =<< fsTreeSym tree)
			-- update the symlinks table (uses a global lock)
			forestIO $ modifyMVar_ symlinks $ return . appendListToFSTreeSym syms
			-- requests a new NILFS snapshot for the latest tree
			newNILFSTree tree
			return wakes
		else return Set.empty
	-- removes temporary files
	forestIO $ Foldable.mapM_ (\tmp -> runShellCommand_ $ "rm -rf " ++ tmp) tmps
	return wakes

type FLock = STM.TMVar ()

newFLockIO = STM.newTMVarIO ()
waitOrRetryFLock :: FLock -> STM.STM ()
waitOrRetryFLock mv = STM.tryTakeTMVar mv >>= maybe STM.retry (STM.putTMVar mv)
acquireFLock = STM.takeTMVar
acquireOrRetryFLock :: FLock -> STM.STM ()
acquireOrRetryFLock = STM.tryTakeTMVar >=> maybe STM.retry return
releaseFLock mv = do
	b <- STM.tryPutTMVar mv ()
	when (not b) $ error "Control.Concurrent.Lock.release: Can't release unlocked Lock!"

fileLocks :: CWeakMap.WeakMap FilePath FLock
fileLocks = unsafePerformIO $ CWeakMap.empty

-- a reference to a lock lives as long as the lock itself, and the lock lives at least as long as it is acquired by a transaction
fileLock :: FilePath -> IO FLock
fileLock path = CWeakMap.lookupOrInsert fileLocks path newFLockIO (\v mb -> STM.mkWeakTMVar v (maybe (return ()) id mb))

atomicTxNILFS :: String -> FTM TxNILFS a -> FTM TxNILFS a
atomicTxNILFS msg m = do
	-- get the changes of the innermost txlog
	(reads,writes) <- forestM getTxNILFSChangesFlat
	-- wait on currently acquired read locks (to ensure that concurrent writes are seen by this tx's validation step)
	forestM $ forestIO $ print $ "entering atomic " ++ msg
	x <- withFileLocks reads writes m
	forestM $ forestIO $ print $ "left atomic " ++ msg
	return x

-- acquiring the locks in sorted order is essential to avoid deadlocks!
withFileLocks :: Set FilePath -> Set FilePath -> FTM TxNILFS a -> FTM TxNILFS a
withFileLocks reads writes m = do
	rcks <- inL $ liftIO $ State.mapM fileLock $ Set.toAscList reads
	wcks <- inL $ liftIO $ State.mapM fileLock $ Set.toAscList writes
		
	let waitAndAcquire wcks = inL $ liftIO $ STM.atomically $ do
		-- wait on read locks
		Foldable.mapM_ waitOrRetryFLock rcks
		-- acquire write locks or retry
		Foldable.mapM_ acquireOrRetryFLock wcks
		
	liftA2 Catch.bracket_ waitAndAcquire (inL . liftIO . STM.atomically . Foldable.mapM_ releaseFLock) wcks m

{-# NOINLINE nilfsData #-}
nilfsData :: NILFSData
nilfsData = unsafePerformIO $ do
	rootPath <- readIORef root
	findRootDevice rootPath

{-# NOINLINE liveSnapshots #-}
liveSnapshots :: LiveSnapshots
liveSnapshots = unsafePerformIO $ CWeakMap.empty

-- the root directory for transactional forest (it can't modify data not under this path)
{-# NOINLINE root #-}
root :: IORef FilePath
root = unsafePerformIO $ newIORef "/"

-- a global table of symbolic links that all transactions read from and keep updated
-- this is used to explore the locality in FS modifications; without knowing the existing symlinks, there is no locality
{-# NOINLINE symlinks #-}
symlinks :: MVar FSTreeSym
symlinks = unsafePerformIO $ do
	path <- readIORef root
	putStrLn $ "Finding all symbolic links under... " ++ show path
	syms <- findSymLinks path
	putStrLn $ "Symbolic links calculated for " ++ show path
	newMVar syms


