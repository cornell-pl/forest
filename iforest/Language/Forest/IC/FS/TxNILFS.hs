
{-# LANGUAGE TemplateHaskell, OverlappingInstances, TypeOperators, ConstraintKinds, UndecidableInstances, TupleSections, FlexibleInstances, MultiParamTypeClasses, StandaloneDeriving, GeneralizedNewtypeDeriving, FlexibleContexts, DataKinds, TypeFamilies, Rank2Types, GADTs, ViewPatterns, DeriveDataTypeable, ScopedTypeVariables #-}

-- NILFS filesystem with optimistic concurrency support for transactions, with mutable transactional variables structures mapped to specifications, and incremental reuse amoung transactions

module Language.Forest.IC.FS.TxNILFS (
	Forest(..),Transactional(..),MonadThrow(..),MonadCatch(..)
	,FSRep(..),ForestCfg(..),atomicallyTxNILFS
) where

import Language.Forest.FS.FileLock
import Control.Concurrent.Transactional
import Language.Forest.Errors
--import Language.Forest.Pure.MetaData (FileInfo)
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
import System.Mem.Weak.Exts as Weak
import Control.Exception as Exception
import qualified Data.Strict.List as Strict
import Unsafe.Coerce
import Language.Forest.IC.ValueDelta
import Language.Forest.IC.BX as BX
import System.Mem.MemoTable (MemoTable(..))
import qualified System.Mem.MemoTable as MemoTable

import Data.Foldable as Foldable
import Data.Concurrent.Deque.Class as Queue
import Data.Concurrent.Deque.Reference.DequeInstance
import qualified Control.Concurrent.Map.Exts as CMap
import qualified Control.Concurrent.Weak.Map as CWeakMap
import System.Posix.Files
import Language.Forest.FS.Diff
import Language.Forest.IC.ICRep

import Language.Forest.FS.FSRep
import Control.Applicative
import System.IO.Unsafe
import Control.Monad

import Data.Maybe
import Data.List
import Data.WithClass.MData
import Data.Typeable
import System.Directory
import Data.List as List
import Data.Dynamic
import Data.IORef.Exts
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
import Data.Strict.Tuple as Strict

import Data.Global.TH as TH

-- the root directory for transactional forest (it can't modify data that is not under this path)
TH.declareMVar "rootTxNILFS"  [t| FilePath |] [e| "/" |]

-- locks on which retrying transactions will wait
type WaitQueue = Deque Threadsafe Threadsafe SingleEnd SingleEnd Grow Safe Lock

-- a register of locks for retrying transactions
-- these paths should be canonical
TH.declareMVar "waitingTxsNILFS"  [t| (Map FilePath WaitQueue) |] [e| Map.empty |]

-- a list of the starting times of running transactions sorted from newest to oldest (we may two txs with the same starting time)
TH.declareMVar "runningTxsNILFS"  [t| [UTCTime] |] [e| [] |]

type TxNILFSWrites = Set FilePath

-- a map with commit times of committed transactions and their performed changes
-- we use a @FSTreeDelta@ because the set of written paths may be infinite (like all paths under a given directory)
TH.declareMVar "doneTxsNILFS"  [t| (Map UTCTime TxNILFSWrites) |] [e| Map.empty |]

TH.declareMVar "canonicalTxNILFS"  [t| (CanonicalTree TxNILFS) |] [e| CanonicalTree Map.empty |]

type TxNILFSLayer l = (ForestLayer TxNILFS l)

type instance IncK (IncForest TxNILFS) a = (Typeable a,Eq a,AddTxNILFSParent Inside a,AddTxNILFSParent Outside a)

proxyTxNILFS = Proxy :: Proxy TxNILFS

-- insert a new time in a list sorted from newest to oldest
addRunningTxNILFS time = modifyMVarMasked_ runningTxsNILFS (\xs -> return $ List.insertBy (\x y -> compare y x) time xs)
-- remove a time in a list sorted from newest to oldest
deleteRunningTxNILFS time = modifyMVarMasked_ runningTxsNILFS (\xs -> return $ List.delete time xs)

-- not a concurrent data structure, because we only commit to it under a lock
{-# NOINLINE memoTxNILFS #-}
memoTxNILFS :: TxNILFSCMemoTable
memoTxNILFS = unsafePerformIO $ CWeakMap.empty

-- ** Filesystem

-- latest tree,delta after the current tree,next tree,reads since the beginning, buffered variables, memoized forest transactional variables, temporary paths since the beginning
type TxNILFSLog = IORef (FSTree TxNILFS,FSDeltas,FSTree TxNILFS,TxNILFSReads,TxNILFSBuffTable,TxNILFSMemoTable,CanonicalTree TxNILFS,Set FilePath)

-- one per transaction
type TxNILFSBuffTable = MemoTable Unique DynTxNILFSThunk
-- one per transaction
type TxNILFSMemoTable = MemoTable (FilePath,TypeRep) (DynamicNILFS,MkWeak,FSTree TxNILFS)
-- a global memotable
type TxNILFSCMemoTable = CWeakMap.WeakMap (FilePath,TypeRep) (DynamicNILFS,MkWeak,FSTree TxNILFS)

data DynamicNILFS where
	DynNILFS :: FTK TxNILFS rep => rep -> DynamicNILFS

fromDynNILFS :: FTK TxNILFS rep => DynamicNILFS -> Maybe rep
fromDynNILFS (DynNILFS v) = cast v

-- nested logs for nested transactions
type TxNILFSLogs = Strict.List TxNILFSLog

-- a chronological list of relative deltas for the different FS versions
type TxNILFSTreeDeltas = IORef (Map FSVersion FSDeltas)

-- (starttime,tx id,register of deltas,nested logs)
newtype TxNILFSEnv = TxNILFSEnv (IORef UTCTime :!: Snapshot :!: TxNILFSTreeDeltas :!: TxNILFSLogs) deriving Typeable

type TxNILFSReads = Set FilePath
type TxNILFSChanges = (TxNILFSReads,FSTreeDelta)
type TxNILFSChangesFlat = (TxNILFSReads,TxNILFSWrites)

-- acquire locks in sorted order
acquireTxNILFSBuffTable :: TxNILFSBuffTable -> IO ()
acquireTxNILFSBuffTable tbl = MemoTable.foldM step Map.empty tbl >>= Foldable.mapM_ id
	where
	step :: Map Unique (IO ()) -> (Unique,DynTxNILFSThunk) -> IO (Map Unique (IO ()))
	step xs (uid,DynTxFSThunk _ _ t) = return $ Map.insert uid (takeMVar (txNILFSFSThunk t) >> return ()) xs
	step xs (uid,DynTxHSThunk _ _ t) = return $ Map.insert uid (takeMVar (txNILFSHSThunk t) >> return ()) xs
	step xs (uid,DynBuffTxNILFSThunk _ _ t) = return $ Map.insert uid (Lock.acquire (txNILFSLock t)) xs

commitTxNILFSBuffTable :: TxNILFSBuffTable -> IO ()
commitTxNILFSBuffTable tbl = MemoTable.mapM_ commitDynTxNILFSThunk tbl

commitDynTxNILFSThunk :: (Unique,DynTxNILFSThunk) -> IO ()
commitDynTxNILFSThunk (uid,DynTxFSThunk v _ t) = putMVar (txNILFSFSThunk t) v
commitDynTxNILFSThunk (uid,DynTxHSThunk v _ t) = putMVar (txNILFSHSThunk t) v
commitDynTxNILFSThunk (uid,DynBuffTxNILFSThunk v _ t) = do
	oriParents <- liftM buffTxNILFSParents $ readIORef (txNILFSBuff t)
	unionWithKey' oriParents (buffTxNILFSParents v)
	writeIORef (txNILFSBuff t) (v { buffTxNILFSParents = oriParents })
	Lock.release (txNILFSLock t)

commitTxNILFSMemoTable :: TxNILFSMemoTable -> IO ()
commitTxNILFSMemoTable = MemoTable.mapM_ commitTxNILFSMemoEntry where
	commitTxNILFSMemoEntry (k,v@(_,mkWeak,tree)) = CWeakMap.insertWithMkWeak memoTxNILFS mkWeak k v 

-- repairs memoized entries to the latest fs tree
repairTxNILFSMemoTable :: TxNILFSLayer l => TxNILFSMemoTable -> ForestL TxNILFS l ()
repairTxNILFSMemoTable tbl = do
	xs <- forestM $ forestIO $ MemoTable.toList tbl
	Foldable.mapM_ repairTxNILFSMemoEntry xs
  where
	repairTxNILFSMemoEntry :: TxNILFSLayer l => ((FilePath,TypeRep),(DynamicNILFS,MkWeak,FSTree TxNILFS)) -> ForestL TxNILFS l ()
	repairTxNILFSMemoEntry ((path,tyRep),(DynNILFS rep,mkWeak,tree)) = loadTxNILFS Proxy rep

findTxNILFSMemo :: FTK TxNILFS rep => FilePath -> Proxy rep -> ForestM TxNILFS (Maybe (rep,MkWeak,FSTree TxNILFS))
findTxNILFSMemo path (_ :: Proxy rep) = do
	TxNILFSEnv (starttime :!: txid :!: deltas :!: txlogs) <- TxNILFSForestM Reader.ask
	
	let find txlog m = do
		(tree,ds,newtree,reads,bufftbl,memotbl,cantree,tmps) <- forestIO $ readIORef txlog
		mb <- forestIO $ MemoTable.lookup memotbl (path,typeOf (undefined :: rep))
		case mb of
			Just (fromDynNILFS -> Just rep,mkWeak,tree) -> do
				return $ Just (rep,mkWeak,tree)
			otherwise -> m
	Foldable.foldr find (return Nothing) txlogs

bufferTxNILFSFSThunk :: (IncK (IncForest TxNILFS) a,AddTxNILFSParent l a,TxNILFSLayer l) => ForestFSThunk TxNILFS l a -> TxNILFSThunk l a -> ForestM TxNILFS ()
bufferTxNILFSFSThunk var thunk = do
	TxNILFSEnv (starttime :!: txid :!: deltas :!: Strict.Cons txlog _) <- TxNILFSForestM Reader.ask
	(tree,ds,newtree,reads,bufftbl,memotbl,cantree,tmps) <- forestIO $ readIORef txlog
	let mkWeak = MkWeak $ mkWeakRefKey $ txNILFSFSThunkArgs var
	forestIO $ MemoTable.insertWithMkWeak bufftbl (txNILFSFSThunkId var) (DynTxFSThunk thunk mkWeak var) mkWeak
	
bufferTxNILFSHSThunk :: (IncK (IncForest TxNILFS) a,AddTxNILFSParent l a,TxNILFSLayer l) => ForestHSThunk TxNILFS l a -> TxNILFSThunk l a -> ForestM TxNILFS ()
bufferTxNILFSHSThunk var thunk = do
	TxNILFSEnv (starttime :!: txid :!: deltas :!: Strict.Cons txlog _) <- TxNILFSForestM Reader.ask
	(tree,ds,newtree,reads,bufftbl,memotbl,cantree,tmps) <- forestIO $ readIORef txlog
	let mkWeak = MkWeak $ mkWeakRefKey $ txNILFSHSThunkArgs var
	forestIO $ MemoTable.insertWithMkWeak bufftbl (txNILFSHSThunkId var) (DynTxHSThunk thunk mkWeak var) mkWeak

-- reads the pointer from a transactional variable to a thunk
bufferedTxNILFSFSThunk :: (Typeable l,Typeable a) => ForestFSThunk TxNILFS l a -> ForestM TxNILFS (TxNILFSThunk l a)
bufferedTxNILFSFSThunk var = do
	TxNILFSEnv (starttime :!: txid :!: deltas :!: txlogs) <-TxNILFSForestM  Reader.ask
	-- read from the concurrent global thunk
	let newBuff = do
		buff <- forestIO $ readMVar $ txNILFSFSThunk var
		return buff
	-- read from the local buffer
	let find txlog m = do
		(tree,ds,newtree,reads,bufftbl,memotbl,cantree,tmps) <- forestIO $ readIORef txlog
		mb <- forestIO $ MemoTable.lookup bufftbl (txNILFSFSThunkId var)
		case mb of
			Nothing -> m
			Just (DynTxFSThunk (cast -> Just thunk) _ _) -> return thunk
	Foldable.foldr find newBuff txlogs

-- reads the pointer from a transactional variable to a thunk
bufferedTxNILFSHSThunk :: (Typeable l,Typeable a) => ForestHSThunk TxNILFS l a -> ForestM TxNILFS (TxNILFSThunk l a)
bufferedTxNILFSHSThunk var = do
	TxNILFSEnv (starttime :!: txid :!: deltas :!: txlogs) <- TxNILFSForestM Reader.ask
	-- read from the concurrent global thunk
	let newBuff = do
		buff <- forestIO $ readMVar $ txNILFSHSThunk var
		return buff
	-- read from the local buffer
	let find txlog m = do
		(tree,ds,newtree,reads,bufftbl,memotbl,cantree,tmps) <- forestIO $ readIORef txlog
		mb <- forestIO $ MemoTable.lookup bufftbl (txNILFSHSThunkId var)
		case mb of
			Nothing -> m
			Just (DynTxHSThunk (cast -> Just thunk) _ _) -> return thunk
	Foldable.foldr find newBuff txlogs

getNextFSTree :: ForestM TxNILFS (FSTree TxNILFS)
getNextFSTree = do
	TxNILFSEnv (starttime :!: txid :!: deltas :!: Strict.Cons txlog _) <- TxNILFSForestM Reader.ask
	(tree,ds,newtree,reads,bufftbl,memotbl,cantree,tmps) <- forestIO $ readIORef txlog
	return newtree

nextTxNILFSTree :: ForestM TxNILFS ()
nextTxNILFSTree = do
	TxNILFSEnv (starttime :!: txid :!: deltas :!: Strict.Cons txlog _) <- TxNILFSForestM Reader.ask
	(tree,ds,newtree,reads,bufftbl,memotbl,cantree,tmps) <- forestIO $ readIORef txlog
	
	-- old tree
	treeSnap <- forestIO $ fsTreeSnapshot tree
	treeVersion <- forestIO $ fsTreeFSVersion tree
	treeVirtual <- forestIO $ fsTreeVirtual tree
	
	-- next tree
	newtreeDelta <- forestIO $ readTreeFSTreeDelta tree >>= newIORef 
	newtreeSyms <- readTreeSym tree >>= forestIO . newIORef
	newtree <- forestIO $ liftM FSTreeTxNILFS $ newIORef $ TxTree treeSnap (succ treeVersion) newtreeDelta treeVirtual newtreeSyms
	
	forestIO $ writeIORef txlog (tree,DList.empty,newtree,reads,bufftbl,memotbl,cantree,tmps)

incrementTxNILFSTree :: ForestM TxNILFS ()
incrementTxNILFSTree = do
	TxNILFSEnv (starttime :!: txid :!: deltas :!: Strict.Cons txlog _) <- TxNILFSForestM Reader.ask
	(tree,ds,newtree,reads,bufftbl,memotbl,cantree,tmps) <- forestIO $ readIORef txlog
	-- the new tree deltas are appended to the original deltas
	newTreeVersion <- forestIO $ fsTreeFSVersion newtree
	-- add the relative deltas
	tds <- forestIO $ readIORef deltas
	forestIO $ writeIORef deltas $ Map.insert newTreeVersion ds tds
	forestIO $ writeIORef txlog (newtree,DList.empty,newtree,reads,bufftbl,memotbl,cantree,tmps)

-- only needs to read from the top-level log
getTxNILFSChangesFlat :: ForestM TxNILFS TxNILFSChangesFlat
getTxNILFSChangesFlat = do
	TxNILFSEnv (starttime :!: txid :!: deltas :!: Strict.Cons txlog _) <- TxNILFSForestM Reader.ask
	(tree,ds,newtree,reads,bufftbl,memotbl,cantree,tmps) <- forestIO $ readIORef txlog
	writes <- forestIO $ readTreeFSTreeDelta tree
	return (reads,fsTreeDeltaWrites "" writes)

-- reads from the current tree
-- only needs to read from the top-level log
getTxNILFSChanges :: ForestM TxNILFS TxNILFSChanges
getTxNILFSChanges = do
	TxNILFSEnv (starttime :!: txid :!: deltas :!: Strict.Cons txlog _) <- TxNILFSForestM Reader.ask
	(tree,ds,newtree,reads,bufftbl,memotbl,cantree,tmps) <- forestIO $ readIORef txlog
	writes <- forestIO $ readTreeFSTreeDelta tree
	return (reads,writes)

putTxNILFSRead :: FilePath -> ForestM TxNILFS ()
putTxNILFSRead path = do
	TxNILFSEnv (starttime :!: txid :!: deltas :!: Strict.Cons txlog _) <- TxNILFSForestM Reader.ask
	(tree,ds,newtree,reads,bufftbl,memotbl,cantree,tmps) <- forestIO $ readIORef txlog
	forestIO $ writeIORef txlog (tree,ds,newtree,Set.insert path reads,bufftbl,memotbl,cantree,tmps)

putTxNILFSTmp :: FilePath -> ForestM TxNILFS ()
putTxNILFSTmp tmp = do
	TxNILFSEnv (starttime :!: txid :!: deltas :!: Strict.Cons txlog _) <- TxNILFSForestM Reader.ask
	(tree,ds,newtree,reads,bufftbl,memotbl,cantree,tmps) <- forestIO $ readIORef txlog
	forestIO $ writeIORef txlog (tree,ds,newtree,reads,bufftbl,memotbl,cantree,Set.insert tmp tmps)

-- writes to the next tree
-- duplicate the action on both the relative deltas (sequence of primitve FSDelta) and the absolute deltas (FSTreeDelta)
modifyTxNILFSTreeDeltas :: FSDelta -> ForestM TxNILFS ()
modifyTxNILFSTreeDeltas d = do
	TxNILFSEnv (starttime :!: txid :!: deltas :!: Strict.Cons txlog _) <- TxNILFSForestM Reader.ask
	(tree,ds,newtree,reads,bufftbl,memotbl,cantree,tmps) <- forestIO $ readIORef txlog
	delta <- forestIO $ fsTreeFSTreeDelta newtree
	forestIO $ modifyIORef delta (appendToFSTreeDelta d)
	sym <- forestIO $ fsTreeSym newtree
	forestIO $ modifyIORef sym (appendToFSTreeSym d)
	forestIO $ writeIORef txlog (tree,DList.snoc ds d,newtree,reads,bufftbl,memotbl,cantree,tmps)

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

type FSVersion = Int

instance FSRep TxNILFS where
	
	runForest (TxNILFSForestCfg root) (TxNILFSForestM m) = atomicallyTxNILFS root (TxNILFSForestO m)
	
	newtype ForestM TxNILFS a = TxNILFSForestM { runTxNILFSForestM :: ReaderT TxNILFSEnv IO a } deriving (Functor,Applicative,Monad)
	
	data ForestCfg TxNILFS = TxNILFSForestCfg FilePath
	
	forestIO = TxNILFSForestM . lift
	
	-- | returns the forest directory used to store auxiliary data
	getForestDirectory = liftM (</> "Forest") $ forestIO getTemporaryDirectory
	
	-- a modifiable reference to allow changing the FSTree on commits
	newtype FSTree TxNILFS = FSTreeTxNILFS { unFSTreeTxNILFS :: IORef FSTreeTxNILFS' }
	
	showFSTree (FSTreeTxNILFS tree) = forestIO $ liftM show $ readIORef tree
	
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
		TxNILFSEnv (starttime :!: txid :!: deltas :!: Strict.Cons txlog _) <- TxNILFSForestM Reader.ask
		(tree,ds,newtree,reads,bufftbl,memotbl,cantree,tmps) <- forestIO $ readIORef txlog
		return tree
	
	-- reads from the FS or from the modification log
	pathInTree path tree = do
		-- canonize the fullpath for reads
		canpath <- canonalizeDirectoryInTree path tree
		-- mark the path as read
		putTxNILFSRead canpath
		td <- forestIO $ readTreeFSTreeDelta tree
		let path_td = focusFSTreeDeltaByRelativePathMay td canpath
		ondisk <- case onDiskWriteMay path_td of
			Nothing -> do
				snap <- forestIO (fsTreeSnapshot tree)
				pathInSnapshot liveSnapshots nilfsData canpath snap (MkWeak $ mkWeakKey tree) -- read the content from the original NILFS snapshot
			Just ondisk -> return ondisk -- return the written ondisk temporary file
		virtualizeTxNILFS ondisk tree

	stepPathInTree tree path rel = do
		dir <- canonalizePathInTree path tree
		return $ dir </> rel
	
	getDirectoryContentsInTree = getDirectoryContentsTxNILFS
		
	doesDirectoryExistInTree = doesExistTxNILFS DirExists
	doesFileExistInTree = doesExistTxNILFS FileExists
	doesExistInTree = doesExistTxNILFS AnyExists
	
	canonalizePathInTree = canonalizePathTxNILFS

	type FSTreeD TxNILFS = FSTreeDeltaNodeMay -- a delta fixed-point focused on the current path

	isEmptyFSTreeD _ = isEmptyFSTreeDeltaNodeMay
	isEmptyTopFSTreeD _ = isEmptyTopFSTreeDeltaNodeMay
	isChgFSTreeD _ = isChgFSTreeDeltaNodeMay
	isMoveFSTreeD _ = isMoveFSTreeDeltaNodeMay

	focusDiffFSTreeD tree path tree' path' = do
		td <- focusDiffFSTreeDelta tree path tree' path'
		syms <- forestIO $ findSymLinks path'
		let fixtd = fixFSTreeDelta td syms
		return $ focusFSTreeDeltaByRelativePathMay fixtd "/"
		 
	focusFSTreeD fs td path file pathfile = focusFSTreeDeltaNodeMayByRelativePath td file

	diffFS oldtree newtree path = do
		TxNILFSEnv (starttime :!: cp2 :!: deltas :!: txlogs) <- TxNILFSForestM Reader.ask
		ot <- forestIO $ readIORef (unFSTreeTxNILFS oldtree)
		nt <- forestIO $ readIORef (unFSTreeTxNILFS newtree)
		case (ot,nt) of
			(NILFSTree cp1 _,NILFSTree cp2 _) -> do
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
			(NILFSTree cp1 _,TxTree ((==cp2) -> True) fs2 delta2 virtual2 sym2) -> do
				-- difference between NILFS snapshots
				(dsNILFS,tdNILFS) <- if cp1 == cp2
					then return (DList.empty,emptyFSTreeDelta) -- nothing to do
					else do
						mount1 <- mountSnapshot liveSnapshots nilfsData cp1 (MkWeak $ mkWeakKey oldtree)
						mount2 <- mountSnapshot liveSnapshots nilfsData cp2 (MkWeak $ mkWeakKey newtree)
						let device = Prelude.snd nilfsData
						forestIO $ diffNILFS mount2 device cp1 cp2
				-- transaction-local difference
				TxNILFSEnv (starttime :!: txid :!: deltas :!: txlogs) <- TxNILFSForestM Reader.ask
				tds <- forestIO $ readIORef deltas
				let dfs = take fs2 $ Map.elems tds -- deltas since the beginning of the tx
				syms <- forestIO $ readIORef sym2
				let fixtd = fixFSTreeDelta (appendListToFSTreeDelta (DList.concat dfs) tdNILFS) syms
				let df = focusFSTreeDeltaByRelativePathMay fixtd path
				return $ Just df
			(TxTree ((==cp2) -> True) fs1 delta1 virtual1 sym1,TxTree ((==cp2) -> True) fs2 delta2 virtual2 sym2) -> do
				-- transaction-local difference
				TxNILFSEnv (starttime :!: txid :!: deltas :!: txlogs) <- TxNILFSForestM Reader.ask
				tds <- forestIO $ readIORef deltas
				let dfs = drop (pred fs1) $ take fs2 $ Map.elems tds -- deltas since the beginning of the tx
				syms <- forestIO $ readIORef sym2
				let fixtd = fixFSTreeDelta (compressFSDeltas (DList.concat dfs)) syms
				let df = focusFSTreeDeltaByRelativePathMay fixtd path
				return $ Just df
			otherwise -> return Nothing

instance WeakKey (FSTree TxNILFS) where
	mkWeakKey (FSTreeTxNILFS tree) = mkWeakKey tree

data FSTreeTxNILFS' = TxTree {
		  txTreeSnapshot :: Snapshot -- original NILFS snapshot
		, txTreeFSVersion :: FSVersion -- tx-local fsversion
		, txTreeFSTreeDelta :: IORef FSTreeDelta -- FS deltas since beginning of the tx (over the physical FS)
		, txTreeVirtual :: Bool -- virtual flag
		, txTreeSym :: IORef (FSTreeSym) -- a sequence of symlink modifications since the start and the symlinks table at the time of this tree
		}
		| NILFSTree Snapshot Bool

instance Eq FSTreeTxNILFS' where
	(TxTree snap1 v1 _ vir1 _) == (TxTree snap2 v2 _ vir2 _) = (snap1,v1,vir1) == (snap2,v2,vir2)
	(NILFSTree snap1 vir1) == (NILFSTree snap2 vir2) = (snap1,vir1) == (snap2,vir2)
	t1 == t2 = False

instance Ord FSTreeTxNILFS' where
	(TxTree snap1 v1 _ vir1 _) <= (TxTree snap2 v2 _ vir2 _) = (snap1,v1,vir1) <= (snap2,v2,vir2)
	(NILFSTree snap1 vir1) <= (NILFSTree snap2 vir2) = (snap1,vir1) <= (snap2,vir2)
	(NILFSTree snap1 vir1) <= (TxTree snap2 v2 _ vir2 _) = (snap1,0,vir1) <= (snap2,v2,vir2)
	(TxTree snap1 v1 _ vir1 _) <= (NILFSTree snap2 vir2) = (snap1,v1,vir1) <= (snap2,0,vir2)

instance Show FSTreeTxNILFS' where
	show (TxTree snap1 v1 _ vir1 _) = "(TxTree " ++ show snap1 ++" "++ show v1 ++" "++ show vir1 ++ ")"
	show (NILFSTree snap1 vir1) = "(NILFSTree " ++ show snap1 ++" "++ show vir1 ++ ")"

fsTreeSnapshot :: FSTree TxNILFS -> IO Snapshot
fsTreeSnapshot (FSTreeTxNILFS tree) = readIORef tree >>= \t -> case t of
	TxTree snap fs delta virtual sym -> return snap
	NILFSTree snap virtual -> return snap
fsTreeFSVersion :: FSTree TxNILFS -> IO FSVersion
fsTreeFSVersion (FSTreeTxNILFS tree) = readIORef tree >>= \t -> case t of
	TxTree snap fs delta virtual sym -> return fs
	NILFSTree snap virtual -> return 0
readTreeFSTreeDelta :: FSTree TxNILFS -> IO FSTreeDelta
readTreeFSTreeDelta (FSTreeTxNILFS tree) = readIORef tree >>= \t -> case t of
	TxTree snap fs delta virtual sym -> readIORef delta
	NILFSTree snap virtual -> return emptyFSTreeDelta
fsTreeFSTreeDelta :: FSTree TxNILFS -> IO (IORef FSTreeDelta)
fsTreeFSTreeDelta (FSTreeTxNILFS tree) = readIORef tree >>= \t -> case t of
	TxTree snap fs delta virtual sym -> return delta
	NILFSTree snap virtual -> error "no delta"
fsTreeVirtual :: FSTree TxNILFS -> IO Bool
fsTreeVirtual (FSTreeTxNILFS tree) = readIORef tree >>= \t -> case t of
	TxTree snap fs delta virtual sym -> return virtual
	NILFSTree snap virtual -> return virtual
readTreeSym :: FSTree TxNILFS -> ForestM TxNILFS (FSTreeSym)
readTreeSym (FSTreeTxNILFS tree) = forestIO (readIORef tree) >>= \t -> case t of
	NILFSTree ss _ -> getSymlinks Nothing ss (MkWeak $ mkWeakKey tree)
	TxTree snap fs delta virtual sym -> forestIO $ readIORef sym
fsTreeSym :: FSTree TxNILFS -> IO (IORef (FSTreeSym))
fsTreeSym (FSTreeTxNILFS tree) = readIORef tree >>= \t -> case t of
	TxTree snap fs delta virtual sym -> return sym
	NILFSTree snap virtual -> error "no sym"
isNILFSTree :: FSTree TxNILFS -> IO Bool
isNILFSTree (FSTreeTxNILFS tree) = readIORef tree >>= \t -> case t of
	TxTree _ _ _ _ _ -> return False
	NILFSTree _ _ -> return True

-- when we commit FS modifications we generate a new NILFS checkpoint and update the tx-local FSTree reference to a global NILFS tree
newNILFSTree :: FSTree TxNILFS -> ForestM TxNILFS Snapshot
newNILFSTree tree@(FSTreeTxNILFS ref) = forestIO $ do
	cp <- newCheckpoint nilfsData
	tree <- readIORef ref
	case tree of
		TxTree snap fs delta virtual symsref -> do
			syms <- readIORef symsref
			writeIORef ref $ NILFSTree cp virtual
			CWeakMap.insertWithMkWeak symlinks (MkWeak $ mkWeakKey ref) cp syms
		NILFSTree snap virtual -> return ()
	-- make the checkpoint a snapshot
	sudoShellCommand_ $ "chcp ss "++show cp
	return cp

virtualizeTxNILFS path tree = forestIO (fsTreeVirtual tree) >>= \virtual -> if virtual
	then liftM (</> ".avfs" </> makeRelative "/" path) (forestIO getHomeDirectory)
	else return path

getDirectoryContentsTxNILFS :: FilePath -> FSTree TxNILFS -> ForestM TxNILFS [FileName]
getDirectoryContentsTxNILFS path tree = do
	canpath <- canonalizePathInTree path tree
	putTxNILFSRead canpath
	td <- forestIO $ readTreeFSTreeDelta tree
	let path_td = focusFSTreeDeltaByRelativePathMay td canpath
	xs <- forestIO $ getContentsFSTreeDeltaNodeMay canpath path_td
	return xs

doesExistTxNILFS :: ExistFlag -> FilePath -> FSTree TxNILFS -> ForestM TxNILFS Bool
doesExistTxNILFS flag path tree = do
	canpath <- canonalizeDirectoryInTree path tree
	putTxNILFSRead canpath
	td <- forestIO $ readTreeFSTreeDelta tree
	let path_td = focusFSTreeDeltaByRelativePathMay td canpath
	case path_td of
		Just (FSTreeNop _) -> return $ flag /= FileExists
		Just (FSTreeChg _ _) -> return $ flag /= FileExists
		Just (FSTreeNew _ _ diskpath _) -> forestIO $ doesExistShellFlag flag diskpath
		otherwise -> forestIO $ doesExistShellFlag flag canpath
memoCanonicalPath :: FilePath -> FilePath -> FSTree TxNILFS -> ForestM TxNILFS ()
memoCanonicalPath src tgt fs = do
	TxNILFSEnv (starttime :!: txid :!: deltas :!: Strict.Cons txlog _) <- TxNILFSForestM Reader.ask
	(tree,ds,newtree,reads,bufftbl,memotbl,cantree,tmps) <- forestIO $ readIORef txlog
	let cantree' = memoCanonicalTree src tgt fs cantree
	forestIO $ writeIORef txlog (tree,ds,newtree,reads,bufftbl,memotbl,cantree',tmps)

-- returns a canonical prefix, at a given tree, and a non-canonized suffix
findCanonicalPath :: [FileName] -> ForestM TxNILFS (Maybe (FilePath,FSTree TxNILFS,[FileName]))
findCanonicalPath dirs = do
	TxNILFSEnv (starttime :!: txid :!: deltas :!: Strict.Cons txlog _) <- TxNILFSForestM Reader.ask
	(tree,ds,newtree,reads,bufftbl,memotbl,cantree,tmps) <- forestIO $ readIORef txlog
	return $ findCanonicalTree dirs cantree

-- canonalizes a path, taking into account the buffered FSTreeDelta and logging reads of followed symlinks
canonalizePathTxNILFS :: FilePath -> FSTree TxNILFS -> ForestM TxNILFS FilePath
canonalizePathTxNILFS path tree = do
	norm <- forestIO $ liftM normalise $ absolutePath path
	let dirs = splitDirectories norm	
	mb <- findCanonicalPath dirs
	syms <- readTreeSym tree
	can <- case mb of
		Just (root,oldtree,suffix) -> do
			cmp <- compareFSTree oldtree tree
			if (cmp == EQ)
				then follow (focusFSTreeSym root syms) root suffix
				else follow syms "" dirs
		otherwise -> follow syms "" dirs
	memoCanonicalPath norm can tree
	return can	
  where
	follow :: FSTreeSym -> FilePath -> [FileName] -> ForestM TxNILFS FilePath
	follow td root [] = return root
	follow td root (".":dirs) = follow td root dirs
	follow td root ("..":dirs) = follow td (takeDirectory root) dirs
	follow td root (dir:dirs) = do
		let rootdir = root </> dir
		case Map.lookup dir td of
			Just (FSTreeSymLink tgt) -> do -- follow link
				-- mark the symbolic link as read
				putTxNILFSRead rootdir
				canonalizePathTxNILFS (root </> tgt </> joinPath dirs) tree
			Nothing -> do
				follow (focusFSTreeSym dir td) rootdir dirs

-- ** Incrementality

class TxNILFSLayerImpl l where
	unTxNILFSLayer :: Proxy l -> l (IncForest TxNILFS) a -> ReaderT TxNILFSEnv IO a
	txNILFSLayer :: Proxy l -> ReaderT TxNILFSEnv IO a -> l (IncForest TxNILFS) a

instance TxNILFSLayerImpl Outside where
	unTxNILFSLayer _ = runTxNILFSForestO
	txNILFSLayer _ = TxNILFSForestO

instance TxNILFSLayerImpl Inside where
	unTxNILFSLayer _ = runTxNILFSForestI
	txNILFSLayer _ = TxNILFSForestI

instance Incremental (IncForest TxNILFS) where
	
	newtype Outside (IncForest TxNILFS) a = TxNILFSForestO { runTxNILFSForestO :: ReaderT TxNILFSEnv IO a } deriving (Functor,Applicative,Monad)
	newtype Inside (IncForest TxNILFS) a = TxNILFSForestI { runTxNILFSForestI :: ReaderT TxNILFSEnv IO a } deriving (Functor,Applicative,Monad)

	world = TxNILFSForestO . runTxNILFSForestI
	unsafeWorld = TxNILFSForestI . runTxNILFSForestO

	runIncremental = error "please use atomically instead"
	
	unsafeIOToInc = inside . TxNILFSForestI . lift
	
instance ICRep TxNILFS where

	forestM = inside . TxNILFSForestI . runTxNILFSForestM
	forestO = TxNILFSForestM . runTxNILFSForestO

	--MVar because transactions may concurrently commit updates to in-memory variables, and a tx may commit the load of a variable whose associated FS fragment has not been locked
	-- arguments never change
	data FSThunk TxNILFS l inc a = TxNILFSFSThunk { txNILFSFSThunkId :: Unique, txNILFSFSThunk :: MVar (TxNILFSThunk l a), txNILFSFSThunkArgs :: IORef (Maybe TxNILFSArgs) }

	data HSThunk TxNILFS l inc a = TxNILFSHSThunk { txNILFSHSThunkId :: Unique, txNILFSHSThunk :: MVar (TxNILFSThunk l a), txNILFSHSThunkArgs :: IORef (Maybe TxNILFSArgs) }
		
	-- no IC
	newtype ICThunk TxNILFS l inc a = TxNILFSICThunk (l inc a)

	newtype ValueDelta TxNILFS a = ValueDeltaTxNILFS Bool -- True = identity
	
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

diffValueThunkTxNILFS :: (IncK (IncForest TxNILFS) (ForestRepTy TxNILFS rep),Typeable (ForestRepTy TxNILFS rep),ForestRep TxNILFS rep) => Bool -> FSTree TxNILFS -> rep -> ForestO TxNILFS (ValueDelta TxNILFS rep)
diffValueThunkTxNILFS isTop oldtree rep = do
		thunk <- forestM $ bufferedTxNILFSFSThunk $ to (iso_rep_thunk proxyTxNILFS) rep
		let uid = txNILFSId thunk
		(buff :: BuffTxNILFSThunk Inside content) <- forestM $ bufferedTxNILFSThunk thunk
		let newtree = if isTop then buffTxNILFSTree buff else buffTxNILFSDeltaTree buff
		cmp <- forestM $ compareFSTree oldtree newtree
		return $ ValueDeltaTxNILFS $ cmp /= LT
	
-- arguments passed to transactional variables
type TxNILFSArgs = (Dynamic,FilePath)

-- dynamic variables for buffered content
data DynTxNILFSThunk where
	-- transactional variable (points to an internal thunk)
	DynTxFSThunk :: (IncK (IncForest TxNILFS) a,TxNILFSLayer l) => TxNILFSThunk l a -> MkWeak -> FSThunk TxNILFS l (IncForest TxNILFS) a ->  DynTxNILFSThunk
	DynTxHSThunk :: (IncK (IncForest TxNILFS) a,TxNILFSLayer l) => TxNILFSThunk l a -> MkWeak -> HSThunk TxNILFS l (IncForest TxNILFS) a ->  DynTxNILFSThunk
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
data TxNILFSThunk (l :: * -> * -> *) a = TxNILFSThunk {
	  txNILFSId :: Unique
	, txNILFSBuff :: IORef (BuffTxNILFSThunk l a)
	, txNILFSLock :: Lock
	} deriving Typeable

data TxNILFSThunkData l a =
		TxNILFSThunkComp (l (IncForest TxNILFS) a)
	| 	TxNILFSThunkForce a TxNILFSStone

-- a dummy reference to be used as key for weak references
type TxNILFSStone = IORef ()

type TxNILFSParents = WeakMap Unique (WeakTxNILFSThunk,MkWeak)

bufferTxNILFSThunk :: (IncK (IncForest TxNILFS) a,TxNILFSLayer l) => TxNILFSThunk l a -> BuffTxNILFSThunk l a -> ForestM TxNILFS ()
bufferTxNILFSThunk thunk buff = do
	let uid = txNILFSId thunk
	let mkWeak = MkWeak $ mkWeakRefKey $ txNILFSBuff thunk
	let dyn = DynBuffTxNILFSThunk buff mkWeak thunk
	TxNILFSEnv (starttime :!: txid :!: deltas :!: Strict.Cons txlog _) <- TxNILFSForestM Reader.ask
	(tree,delta,newtree,reads,bufftbl,memotbl,cantree,tmps) <- forestIO $ readIORef txlog
	forestIO $ MemoTable.insertWithMkWeak bufftbl uid dyn mkWeak

bufferedTxNILFSThunk :: (IncK (IncForest TxNILFS) a,TxNILFSLayer l) => TxNILFSThunk l a -> ForestM TxNILFS (BuffTxNILFSThunk l a)
bufferedTxNILFSThunk thunk = do
	TxNILFSEnv (starttime :!: txid :!: deltas :!: txlogs) <- TxNILFSForestM Reader.ask
	-- read from the concurrent global thunk
	let newBuff = do
		let mkWeak = MkWeak $ mkWeakRefKey $ txNILFSBuff thunk
		buff <- forestIO $ readIORef $ txNILFSBuff thunk
		newParents <- forestIO $ WeakMap.copy' (buffTxNILFSParents buff)
		let buff' = buff { buffTxNILFSParents = newParents }
		return buff'
	-- read from the local buffer
	let find txlog m = do
		(tree,ds,newtree,reads,bufftbl,memotbl,cantree,tmps) <- forestIO $ readIORef txlog
		mb <- forestIO $ MemoTable.lookup bufftbl (txNILFSId thunk)
		case mb of
			Nothing -> m
			Just (DynBuffTxNILFSThunk (cast -> Just buff) mkWeak thunk) -> return buff
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
instance (ForestLayer TxNILFS l,AddTxNILFSParent l (ForestFSThunkI TxNILFS Forest_err),AddTxNILFSParent l (ForestFSThunkI TxNILFS FileInfo)) => AddTxNILFSParent l (Forest_md TxNILFS) where
	addTxNILFSParent proxy stone meta z fmd = do
		z1 <- addTxNILFSParent proxy stone meta z (errors fmd)
		addTxNILFSParent proxy stone meta z1 (fileInfo fmd)
instance (ForestLayer TxNILFS l,MData (AddTxNILFSParentDict l) (ForestL TxNILFS l) a) => AddTxNILFSParent l a where
	addTxNILFSParent proxy stone meta z x = do
		let f t1 t2 = forestM $ maxFSTree t1 t2
		gmapQr (addTxNILFSParentProxy proxy) f z (addTxNILFSParentDictDict dict proxy stone meta z) x

newTxNILFSThunk :: (IncK (IncForest TxNILFS) a,TxNILFSLayer l) => l (IncForest TxNILFS) a -> ForestM TxNILFS (TxNILFSThunk l a)
newTxNILFSThunk m = do
	tree <- latestTree
	uid <- forestIO $ newUnique
	stone <- forestIO $ newIORef uid
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
			debug ("evaluated " ++ show (typeRep t) ++ " " ++ show uid) $ return (buff { buffTxNILFSData = dta', buffTxNILFSDeltaTree = deltatree },(a,deltatree))
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
overwriteTxNILFSThunk :: (IncK (IncForest TxNILFS) a,AddTxNILFSParent l a,TxNILFSLayer l) => TxNILFSThunk l a -> l (IncForest TxNILFS) a -> ForestO TxNILFS ()
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

	addZippedMemo path args rep mb_tree = do
		let var = to (iso_rep_thunk proxyTxNILFS) rep
		smb_tree <- case mb_tree of
			Nothing -> return "Nothing"
			Just tree -> forestM $ liftM ("Just "++) $ showFSTree tree
		debug ("added memo "++show path ++ smb_tree ++" "++ show (txNILFSFSThunkId var) ++" "++ show (typeOf rep)) $ do
			TxNILFSEnv (starttime :!: txid :!: deltas :!: Strict.Cons txlog _) <- TxNILFSForestI Reader.ask
			(tree,ds,newtree,reads,bufftbl,memotbl,cantree,tmps) <- forestM $ forestIO $ readIORef txlog
			
			-- remember the arguments (this is how we connect transactional arguments to transactional variables)
			let txargs = (toDyn args,path)
			forestM $ forestIO $ writeIORef (txNILFSFSThunkArgs var) $ Just txargs
			
			-- memoize the entry
			case mb_tree of
				Nothing -> return ()
				Just tree -> do
					let mkWeak = MkWeak $ mkWeakKey memotbl
					--let mkWeak = MkWeak $ mkWeakRefKey $ txNILFSFSThunkArgs var
					forestM $ forestIO $ MemoTable.insertWithMkWeak memotbl (path,typeOf rep) (DynNILFS rep,mkWeak,tree) mkWeak
	
	findZippedMemo path (proxy :: Proxy rep) = debug ("finding "++show path ++" "++ show (typeOf (undefined::rep))) $ do
		TxNILFSEnv (starttime :!: txid :!: deltas :!: Strict.Cons txlog _) <- TxNILFSForestI Reader.ask
		(tree,ds,newtree,reads,bufftbl,memotbl,cantree,tmps) <- forestM $ forestIO $ readIORef txlog
		mb <- forestM $ findTxNILFSMemo path proxy
		case mb of
			Nothing -> return Nothing
			Just (rep,mkWeak,tree) -> do
				stree <- forestM $ showFSTree tree
				debug ("found memo " ++ show path ++ " " ++ stree) $ do
					let var = to (iso_rep_thunk proxyTxNILFS) rep
					Just (fromDynamic -> Just txargs,_) <- forestM $ forestIO $ readIORef $ txNILFSFSThunkArgs var
					return $ Just (tree,txargs,rep)

argsTxNILFS :: (FTK TxNILFS rep) => Proxy (ForestArgs rep) -> rep -> ForestI TxNILFS (ForestVs (ForestArgs rep),FilePath)
argsTxNILFS proxy rep = do
	mb <- forestM $ getFTVArgsNILFS proxy rep
	case mb of
		Nothing -> error "should not happen"
		Just (margs,path) -> liftM (,path) $ vArgs Proxy proxy margs

getFTVArgsNILFS :: (FTK TxNILFS rep) => Proxy (ForestArgs rep) -> rep -> ForestM TxNILFS (Maybe (ForestIs TxNILFS (ForestArgs rep),FilePath))
getFTVArgsNILFS (proxy ::Proxy args) rep = forestIO $ do
	let var = to (iso_rep_thunk proxyTxNILFS) rep
	mb <- readIORef (txNILFSFSThunkArgs var)
	case mb of
		Just (fromDynamic -> Just args,path) -> return $ Just (args,path)
		otherwise -> return Nothing

instance Thunk (HSThunk TxNILFS) Inside (IncForest TxNILFS) where
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
		liftM Prelude.fst $ readTxNILFSThunk thunk tree
instance Thunk (HSThunk TxNILFS) Outside (IncForest TxNILFS) where
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
		liftM Prelude.fst $ readTxNILFSThunk thunk tree

instance TxNILFSLayer l => Thunk (ICThunk TxNILFS) l (IncForest TxNILFS) where
	new m = return $ TxNILFSICThunk m
	read (TxNILFSICThunk m) = m
instance TxNILFSLayer l => Output (ICThunk TxNILFS) l (IncForest TxNILFS) where
	thunk = Inc.new
	force = Inc.read

instance Thunk (FSThunk TxNILFS) Inside (IncForest TxNILFS) where
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
		v <- liftM Prelude.fst $ readTxNILFSThunk thunk tree
		b <- inside $ isUnevaluatedFSThunk var
		let uid = txNILFSFSThunkId var
		debug ("read " ++ show uid ++ " " ++ show (typeOf var) ++ show b) $ return v
instance Thunk (FSThunk TxNILFS) Outside (IncForest TxNILFS) where
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
		liftM Prelude.fst $ readTxNILFSThunk thunk tree

instance Input (FSThunk TxNILFS) Inside (IncForest TxNILFS) where
	ref c = Inc.new (return c)
	mod = Inc.new
	get = Inc.read
	set (var :: ForestFSThunk TxNILFS Inside a) v = do
		(thunk :: TxNILFSThunk Inside a) <- forestM $ bufferedTxNILFSFSThunk var
		writeTxNILFSThunk thunk v
	overwrite (var :: ForestFSThunk TxNILFS Inside a) mv = do
		(thunk :: TxNILFSThunk Inside a) <- forestM $ bufferedTxNILFSFSThunk var
		overwriteTxNILFSThunk thunk mv
instance Input (FSThunk TxNILFS) Outside (IncForest TxNILFS) where
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

instance Transactional (IncForest TxNILFS) where
	atomically = atomicallyTxNILFS "/"
	retry = retryTxNILFS
	orElse = orElseTxNILFS
instance MonadThrow (Outside (IncForest TxNILFS)) where
	throwM = throwTxNILFS 
instance MonadCatch (Outside (IncForest TxNILFS)) where
	catch = catchTxNILFS False

instance Forest TxNILFS where
	new = newTxNILFS Proxy
	args = argsTxNILFS Proxy
	read = readTxNILFS Proxy
	writeOrElse = writeOrElseTxNILFS
	delete = deleteTxNILFS Proxy
	copyOrElse = copyOrElseTxNILFS Proxy

deleteTxNILFS :: FTK TxNILFS rep => Proxy (ForestArgs rep) -> rep -> TxNILFSFTM ()
deleteTxNILFS proxy (rep :: rep) = do
	let proxyRep = proxyOf rep
	mb <- forestM $ getFTVArgsNILFS proxy rep
	case mb of
		Nothing -> error "tried to write to a variable that is not connected to the FS"
		Just (args,path) -> do
			def_rep :: rep <- inside $ zdefaultScratchMemo proxyRep proxyTxNILFS args path
			content <- inside $ BX.getM lens_content $ Inc.get $ to (iso_rep_thunk proxyTxNILFS) def_rep
			writeOrElseTxNILFS rep content () (error "failed to write")

copyOrElseTxNILFS :: (FTK TxNILFS rep) => Proxy (ForestArgs rep) -> rep -> rep -> b -> ([ManifestError] -> TxNILFSFTM b) -> TxNILFSFTM b
copyOrElseTxNILFS proxy src tgt b f = do
	mb_src <- forestM $ getFTVArgsNILFS proxy src
	mb_tgt <- forestM $ getFTVArgsNILFS proxy tgt
	case (mb_src,mb_tgt) of
		(Just (args_src,path_src),Just (args_tgt,path_tgt)) -> do
			-- XXX: this may fail if FileInfo paths are canonized...
			let chgPath path = path_tgt </> (makeRelative path_src path)
			tgt' <- copyFSThunks proxyTxNILFS proxyOutside chgPath src
			content' <- inside $ BX.getM lens_content $ Inc.get $ to (iso_rep_thunk proxyTxNILFS) tgt'
			writeOrElseTxNILFS tgt content' b f
		otherwise -> error "tried to write to a variable that is not connected to the FS"

newTxNILFS :: FTK TxNILFS rep => Proxy (ForestArgs rep) -> ForestVs (ForestArgs rep) -> FilePath -> TxNILFSFTM rep
newTxNILFS proxy args path = inside $ zload (vmonadArgs proxyTxNILFS proxy args) path

-- incrementally repair the thunk to the latest tree
loadTxNILFS :: (ForestLayer TxNILFS l,FTK TxNILFS rep) => Proxy (ForestArgs rep) -> rep -> ForestL TxNILFS l ()
loadTxNILFS proxy rep = do
	let t = to (iso_rep_thunk proxyTxNILFS) rep
	let uid = txNILFSFSThunkId t
	(txargs,path) <- liftM (fromJustNote "loadTxNILFS1") $ forestM $ getFTVArgsNILFS proxy rep
	oldtree <- inside $ treeTxNILFSFSThunk t
	tree <- forestM latestTree
	df <- liftM (fromJustNote "loadTxNILFS2") $ forestM $ diffFS oldtree tree path
	-- load incrementally at the latest tree
	inside $ unsafeWorld $ debug ("loadTxNILFS " ++ show path ++" "++ show uid) $ do
		dv <- diffValueThunk oldtree rep
		ds <- deltaArgs oldtree proxy txargs txargs
		let deltas = (txargs,ds)
		zloadDeltaMemo deltas (return path) oldtree (rep,getForestMDInTree) path df tree dv
	return ()

-- read a transactional variable
readTxNILFS :: (ForestLayer TxNILFS l,FTK TxNILFS rep) => Proxy (ForestArgs rep) -> rep -> ForestL TxNILFS l (ForestContentTy TxNILFS (ForestRepTy TxNILFS rep))
readTxNILFS proxy rep = do
	loadTxNILFS proxy rep
	let t = to (iso_rep_thunk proxyTxNILFS) rep
	inside $ BX.getM lens_content $ Inc.get t

data ManifestConflictTx = ManifestConflictTx [ManifestError] deriving (Show,Eq,Typeable)
instance Exception ManifestConflictTx

writeOrElseTxNILFS :: FTK TxNILFS rep => rep -> ForestContentTy TxNILFS (ForestRepTy TxNILFS rep) -> b -> ([ManifestError] -> TxNILFSFTM b) -> TxNILFSFTM b
writeOrElseTxNILFS (rep::rep) content b f = do
	let proxy = Proxy :: Proxy (ForestArgs rep)
	let t = to (iso_rep_thunk proxyTxNILFS) rep
		
	mb :: (Maybe (ForestIs TxNILFS (ForestArgs rep),FilePath)) <- forestM $ getFTVArgsNILFS Proxy rep
	case mb of
		Nothing -> error "writeOrElseTxNILFS: should not happen" -- the top-level arguments of a variable don't match the spec
		Just (txargs,path) -> do
			let proxy = Proxy :: Proxy (ForestArgs rep)
			oldtree <- inside $ treeTxNILFSFSThunk t
			tree <- forestM latestTree
			df <- liftM (fromJustNote "writeOrElseTxNILFS") $ forestM $ diffFS oldtree tree path
			dv <- diffValueThunk oldtree rep
			ds <- deltaArgs oldtree proxy txargs txargs
			let deltas = (txargs,ds)
			
			-- update the value to the latest filesystem (since some variables may be outdated due to modifications to other descriptions)
			-- this is necessary because the store function reads the content from the inner thunks, that are not necessarily in sync with the latest filesystem
			-- skipping this step would eventually overwrite user modifications with older values!!
			zloadDeltaMemo deltas (return path) oldtree (rep,getForestMDInTree) path df tree dv
			
			let tryWrite = do
				forestM nextTxNILFSTree
				-- write the new content
				modify t $ \s -> BX.putM lens_content (return s) (return content)
				
				-- compute new value deltas for the write
				newdv <- diffValueThunk tree rep
				newds <- deltaArgs tree proxy txargs txargs
				let newdeltas = (txargs,newds)
				
				-- store incrementally at the latest tree (there are no filesystem modifications, since have only c hanged the value since loadDelta)
				man <- forestM $ newManifestWith "/" tree
				(mani,_,memos) <- debug ("storing... " ++ show path) $ RWS.runRWST (zupdateManifestDeltaMemo newdeltas path path tree (emptyFSTreeD proxyTxNILFS) tree rep newdv man) True ()
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
			catchTxNILFS True tryWrite (\(ManifestConflictTx errors) -> f errors)
					
atomicallyTxNILFS :: FilePath -> TxNILFSFTM b -> IO b
atomicallyTxNILFS root stm = updateRootTxNILFS root >> initializeTxNILFS try where
	try = txNILFSLayer proxyOutside $ flip Catch.catches [Catch.Handler catchInvalid,Catch.Handler catchRetry,Catch.Handler catchSome] $ unTxNILFSLayer proxyOutside $ do
		-- run the tx
		x <- stm
		-- tries to commit the current tx, otherwise repairs it incrementally
		success <- validateAndCommitTopTxNILFS True
		if success
			then return x
			else debug "throw InvalidTx" $ txNILFSLayer proxyOutside $ throwM InvalidTx
	catchInvalid InvalidTx = unTxNILFSLayer proxyOutside $ debug "InvalidTx" $ do
		resetTxNILFS try
	catchRetry BlockedOnRetry = unTxNILFSLayer proxyOutside $ debug "BlockedOnRetry" $ do
		-- if the retry was invoked on an inconsistent state, we incrementally repair and run again, otherwise we place the tx in the waiting queue
		mbsuccess <- validateAndRetryTopTxNILFS
		case mbsuccess of
			Just lck -> do -- retried txs are always in a consistent state, because we apply all affecting updates before releasing the lock
				-- wait for the lock to be released (whenever some variables that it depends on are changed)
				-- we don't consume the contents of the mvar to avoid further puts to succeeed; a new MVar is created for each retry
				unsafeIOToInc $ Lock.acquire lck
				resetTxNILFS try
			Nothing -> resetTxNILFS try
	catchSome (e::SomeException) = unTxNILFSLayer proxyOutside $ debug ("SomeException "++show e) $ do
		-- we still need to validate on exceptions, otherwise repair incrementally; transaction-local allocations still get committed
		success <- validateAndCommitTopTxNILFS False
		if success
			then txNILFSLayer proxyOutside $ throwM e
			else do
				resetTxNILFS try

retryTxNILFS :: TxNILFSFTM a
retryTxNILFS = unsafeIOToInc $ throwIO BlockedOnRetry

orElseTxNILFS :: TxNILFSFTM a -> TxNILFSFTM a -> TxNILFSFTM a
orElseTxNILFS stm1 stm2 = do1 where
	try1 = do { x <- stm1; validateAndCommitNestedTxNILFS Nothing; return x }
	try2 = do { x <- stm2; validateAndCommitNestedTxNILFS Nothing; return x }
	do1 = startNestedTxNILFS $ txNILFSLayer proxyOutside $ (unTxNILFSLayer proxyOutside try1) `Catch.catches` [Catch.Handler catchRetry1,Catch.Handler catchInvalid,Catch.Handler catchSome]
	do2 = dropChildTxNILFSLog $ startNestedTxNILFS $ txNILFSLayer proxyOutside $ (unTxNILFSLayer proxyOutside try2) `Catch.catches` [Catch.Handler catchRetry2,Catch.Handler catchInvalid,Catch.Handler catchSome]
	catchRetry1 BlockedOnRetry = unTxNILFSLayer proxyOutside $ validateAndRetryNestedTxNILFS >> do2
	catchRetry2 BlockedOnRetry = unTxNILFSLayer proxyOutside validateAndRetryNestedTxNILFS >> throwM BlockedOnRetry
	catchInvalid (e::InvalidTx) = throwM e
	catchSome (e::SomeException) = unTxNILFSLayer proxyOutside (validateAndCommitNestedTxNILFS (Just e)) >> throwM e

throwTxNILFS :: Exception e => e -> TxNILFSFTM a
throwTxNILFS = txNILFSLayer proxyOutside . Catch.throwM

catchTxNILFS :: Exception e => Bool -> TxNILFSFTM a -> (e -> TxNILFSFTM a) -> TxNILFSFTM a
catchTxNILFS doWrites stm (h :: e -> TxNILFSFTM a) = txNILFSLayer proxyOutside $ (unTxNILFSLayer proxyOutside stm) `Catch.catches` [Catch.Handler catchInvalid,Catch.Handler catchRetry,Catch.Handler catchSome] where
	catchInvalid (e::InvalidTx) = throwM e
	catchRetry (e::BlockedOnRetry) = throwM e
	catchSome (e::e) = unTxNILFSLayer proxyOutside $ do
		validateCatchTxNILFS doWrites
		h e

initializeTxNILFS :: TxNILFSFTM b -> IO b 
initializeTxNILFS (TxNILFSForestO m) = do
	starttime <- startTxNILFS >>= newIORef
	mountAVFS -- should succeed even if AVFS is already mounted
	forestDir <- liftM (</> "Forest") getTemporaryDirectory
	createDirectoryIfMissing True forestDir
	createDirectoryIfMissing True (forestDir </> "Snapshots")
	(txid,_,_) <- latestCheckpoint
	-- current tree
	treeDelta <- newIORef $ emptyFSTreeDelta
	tree <- liftM FSTreeTxNILFS $ newIORef $ NILFSTree txid False
	-- tables
	bufftbl <- MemoTable.new
	memotbl <- MemoTable.new
	cantree <- readMVar canonicalTxNILFS
	txlog <- newIORef (tree,DList.empty,tree,Set.empty,bufftbl,memotbl,cantree,Set.empty)
	deltas <- newIORef Map.empty
	debug ("initializeTxNILFS") $ Reader.runReaderT m $ TxNILFSEnv (starttime :!: txid :!: deltas :!: Strict.Cons txlog Strict.Nil)
	-- don't unmount AVFS, since multiple parallel txs may be using it

-- resets
resetTxNILFS :: TxNILFSFTM a -> TxNILFSFTM a
resetTxNILFS m = do
	now <- unsafeIOToInc $ startTxNILFS >>= newIORef
	TxNILFSEnv (_ :!: txid :!: deltas :!: _) <- TxNILFSForestO Reader.ask
	-- current tree
	tree <- forestM $ forestIO $ liftM FSTreeTxNILFS $ newIORef' $ NILFSTree txid False
	
	-- tables
	bufftbl <- forestM $ forestIO $ MemoTable.new
	memotbl <- forestM $ forestIO $ MemoTable.new
	cantree <- forestM $ forestIO $ readMVar canonicalTxNILFS
	txlog <- forestM $ forestIO $ newIORef (tree,DList.empty,tree,Set.empty,bufftbl,memotbl,cantree,Set.empty)
	forestM $ forestIO $ writeIORef deltas Map.empty
	debug ("resetTxNILFS") $ TxNILFSForestO $ Reader.local (Prelude.const $ TxNILFSEnv (now :!: txid :!: deltas :!: Strict.Cons txlog Strict.Nil)) $ runTxNILFSForestO m

-- we need to acquire a lock, but this should be minimal
startTxNILFS :: IO UTCTime
startTxNILFS = getCurrentTime >>= \t -> addRunningTxNILFS t >> return t

-- appends a freshly created txlog for the inner tx
-- the starting time reference is preserved
--we create a new fsversion reference
startNestedTxNILFS :: TxNILFSFTM a -> TxNILFSFTM a
startNestedTxNILFS m = do
	TxNILFSEnv (starttime :!: txid :!: deltas :!: txlogs@(Strict.Cons txlog_parent _)) <- TxNILFSForestO Reader.ask
	(tree,delta,newtree,reads,_,_,cantree,tmps) <- unsafeIOToInc $ readIORef txlog_parent
	bufftbl' <- forestM $ forestIO $ MemoTable.new
	memotbl' <- forestM $ forestIO $ MemoTable.new
	txlog_child <- unsafeIOToInc $ newIORef (tree,delta,newtree,Set.empty,bufftbl',memotbl',cantree,Set.empty)
	debug ("startNestedTxNILFS ") $ TxNILFSForestO $ Reader.local (Prelude.const $ TxNILFSEnv (starttime:!: txid :!: deltas :!: Strict.Cons txlog_child txlogs)) $ runTxNILFSForestO m

-- remember to empty the next tree and delete the new versions in case nested txs fail
dropChildTxNILFSLog :: TxNILFSFTM a -> TxNILFSFTM a
dropChildTxNILFSLog m = do
	TxNILFSEnv (starttime :!: txid :!: deltas :!: (Strict.Cons txlog_child txlogs@(Strict.Cons txlog_parent _))) <- TxNILFSForestO Reader.ask
	(tree_parent,_,newtree_parent,_,_,_,_,_) <- unsafeIOToInc $ readIORef txlog_parent
	-- forget newly created nested versions
	tree_parentVersion <- forestM $ forestIO $ fsTreeFSVersion tree_parent
	forestM $ forestIO $ modifyIORef deltas $ Map.filterWithKey (\version _ -> version <= tree_parentVersion)
	-- reset the next tree
	newtree_parentDelta <- forestM $ forestIO $ fsTreeFSTreeDelta newtree_parent
	forestM $ forestIO $ writeIORef newtree_parentDelta emptyFSTreeDelta
	TxNILFSForestO $ Reader.local (Prelude.const $ TxNILFSEnv (starttime :!: txid :!: deltas :!: txlogs)) $ runTxNILFSForestO m

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
	TxNILFSEnv (starttime :!: txid :!: deltas :!: Strict.Cons txlog _) <- TxNILFSForestO Reader.ask
	(tree,deltas,newtree,reads,bufftbl,memotbl,cantree,tmps) <- forestM $ forestIO $ readIORef txlog
	repairTxNILFSMemoTable memotbl
	-- perform the actual commit
	atomicTxNILFS "validateAndCommitTopTxNILFS" $ do
		txenv@(TxNILFSEnv (timeref :!: txid :!: deltas :!: txlogs@(Strict.Cons txlog Strict.Nil))) <- TxNILFSForestO Reader.ask
		starttime <- unsafeIOToInc $ readIORef timeref
		success <- forestM $ validateTxsNILFS starttime txlogs
		case success of
			Left mode -> do
				if mode then forestM $ finishTopTxNILFS starttime txlog else forestM $ commitTopTxNILFS doWrites starttime txlog
				return True
			Right () -> do
				unsafeIOToInc $ deleteRunningTxNILFS starttime
				return False

validateAndCommitNestedTxNILFS :: Maybe SomeException -> TxNILFSFTM ()
validateAndCommitNestedTxNILFS mbException = do
	txenv@(TxNILFSEnv (timeref :!: txid :!: deltas :!: txlogs@(Strict.Cons txlog1 (Strict.Cons txlog2 _)))) <- TxNILFSForestO Reader.ask
	starttime <- unsafeIOToInc $ readIORef timeref
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
					unsafeIOToInc $ deleteRunningTxNILFS starttime
					txNILFSLayer proxyOutside $ throwM InvalidTx

-- validates a transaction and places it into the waiting queue for retrying
validateAndRetryTopTxNILFS :: TxNILFSFTM (Maybe Lock)
validateAndRetryTopTxNILFS = atomicTxNILFS "validateAndRetryTopTxNILFS" $ do
	txenv@(TxNILFSEnv (timeref :!: txid :!: deltas :!: txlogs@(Strict.Cons txlog Strict.Nil))) <- TxNILFSForestO Reader.ask
	starttime <- unsafeIOToInc $ readIORef timeref
	-- validates the current and enclosing txs up the tx tree
	success <- forestM $ validateTxsNILFS starttime txlogs
	case success of
		Left _ -> do
			lck <- unsafeIOToInc $ Lock.newAcquired -- sets the tx lock as acquired; the tx will be resumed when the lock is released
			forestM $ commitTopTxNILFS False starttime txlog
			forestM $ retryTxNILFSLog lck timeref txlog -- wait on changes to retry (only registers waits, does not actually wait)
			return $ Just lck
		Right _ -> do
			unsafeIOToInc $ deleteRunningTxNILFS starttime
			return Nothing

--registers waits for all the filepaths read by a txlog
--since we treat reads/writes separately, we don't need to wait on writes that have not been read
retryTxNILFSLog :: Lock -> IORef UTCTime -> TxNILFSLog -> ForestM TxNILFS ()
retryTxNILFSLog lck timeref txlog = do
	(tree,delta,newtree,reads,bufftbl,memotbl,cantree,tmps) <- forestIO $ readIORef txlog
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
	txenv@(TxNILFSEnv (timeref :!: txid :!: deltas :!: txlogs@(Strict.Cons txlog1 (Strict.Cons txlog2 _)))) <- TxNILFSForestO Reader.ask
	starttime <- unsafeIOToInc $ readIORef timeref
	success <- forestM $ validateTxsNILFS starttime txlogs
	case success of
		Left _ -> do
			forestM $ commitNestedTxNILFS False deltas txlog1 txlog2 -- does not perform @Write@s on @retry@
		Right _ -> do
			unsafeIOToInc $ deleteRunningTxNILFS starttime
			txNILFSLayer proxyOutside $ throwM InvalidTx

-- validates the current log before catching an exception
validateCatchTxNILFS :: Bool -> TxNILFSFTM ()
validateCatchTxNILFS doWrites = do
	txenv@(TxNILFSEnv (timeref :!: txid :!: deltas :!: txlogs)) <- TxNILFSForestO Reader.ask
	starttime <- unsafeIOToInc $ readIORef timeref
	success <- forestM $ validateTxsNILFS starttime txlogs
	case success of
		Left _ -> do
			-- in case the computation raises an exception, discard all its visible (write) effects
			unless doWrites $ forestM unbufferTxNILFSWrites
		Right _ -> do
			unsafeIOToInc $ deleteRunningTxNILFS starttime
			txNILFSLayer proxyOutside $ throwM InvalidTx

parentCanonicalTree :: TxNILFSLogs -> ForestM TxNILFS (CanonicalTree TxNILFS)
parentCanonicalTree Strict.Nil = forestIO $ readMVar canonicalTxNILFS
parentCanonicalTree (Strict.Cons txlog _) = do
	(tree,delta,newtree,reads,bufftbl,memotbl,cantree,tmps) <- forestIO $ readIORef txlog
	return cantree
	
parentSyms :: Snapshot -> MkWeak -> TxNILFSLogs -> ForestM TxNILFS (FSTreeSym)
parentSyms snap mkWeak Strict.Nil = getSymlinks Nothing snap mkWeak
parentSyms snap mkWeak (Strict.Cons txlog _) = do
	(tree,delta,newtree,reads,bufftbl,memotbl,cantree,tmps) <- forestIO $ readIORef txlog
	readTreeSym tree

parentDelta :: TxNILFSLogs -> ForestM TxNILFS FSTreeDelta
parentDelta Strict.Nil = return emptyFSTreeDelta
parentDelta (Strict.Cons txlog _) = do
	(tree,delta,newtree,reads,bufftbl,memotbl,cantree,tmps) <- forestIO $ readIORef txlog
	forestIO $ fsTreeFSTreeDelta tree >>= readIORef

parentVirtual :: TxNILFSLogs -> ForestM TxNILFS Bool
parentVirtual Strict.Nil = return False
parentVirtual (Strict.Cons txlog _) = do
	(tree,delta,newtree,reads,bufftbl,memotbl,cantree,tmps) <- forestIO $ readIORef txlog
	forestIO $ fsTreeVirtual tree

-- we only unbuffer the child log
unbufferTxNILFSWrites :: ForestM TxNILFS ()
unbufferTxNILFSWrites = do
	TxNILFSEnv (starttime :!: txid :!: deltas :!: txlogs@(Strict.Cons txlog1 txlogs_parent)) <- TxNILFSForestM Reader.ask
	(tree1,delta1,newtree1,reads1,bufftbl1,memotbl1,cantree1,tmps1) <- forestIO $ readIORef txlog1
	b <- forestIO $ isNILFSTree tree1
	unless b $ do
		bufftbl1' <- forestIO $ MemoTable.new
		memotbl1' <- forestIO $ MemoTable.new
		-- reset deltas
		forestIO $ writeIORef deltas Map.empty
		-- current tree
		treeVersion1 <- forestIO $ fsTreeFSVersion tree1
		treeDelta1' <- parentDelta txlogs_parent >>= forestIO . newIORef
		treeVirtual1' <- parentVirtual txlogs_parent
		treeSym1 <- readTreeSym tree1
		tree1Ref' <- forestIO $ newIORef undefined
		let tree1' = FSTreeTxNILFS tree1Ref'
		treeSym1' <- parentSyms txid (MkWeak $ mkWeakKey tree1Ref') txlogs_parent >>= forestIO . newIORef
		forestIO $ writeIORef' tree1Ref' $ TxTree txid treeVersion1 treeDelta1' treeVirtual1' treeSym1'
		cantree1' <- parentCanonicalTree txlogs_parent
		forestIO $ writeIORef txlog1 (tree1',DList.empty,tree1',reads1,bufftbl1',memotbl1',cantree1',tmps1)

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
checkTxsNILFSBefore Strict.Nil = return True
checkTxsNILFSBefore env@(Strict.Cons txlog txlogs) = do
	b1 <- checkTxNILFSBefore txlog
	b2 <- checkTxsNILFSBefore txlogs
	return $ b1 && b2

-- checks if the current txlog is consistent with a sequence of concurrent post-modifications
checkTxNILFSBefore :: TxNILFSLog -> ForestM TxNILFS Bool
checkTxNILFSBefore txlog = do
	(tree,ds,newtree,reads,bufftbl,memotbl,cantree,tmps) <- forestIO $ readIORef txlog
	writes <- forestIO (readTreeFSTreeDelta tree)
	return $ writes == emptyFSTreeDelta -- tx has no writes

checkTxsNILFSAfter :: TxNILFSLogs -> [(UTCTime,TxNILFSWrites)] -> ForestM TxNILFS Bool
checkTxsNILFSAfter Strict.Nil finished = return True
checkTxsNILFSAfter env@(Strict.Cons txlog txlogs) finished = do
	b1 <- checkTxNILFSAfter txlog finished
	b2 <- checkTxsNILFSAfter txlogs finished
	return $ b1 && b2

-- checks if the current txlog is consistent with a sequence of concurrent pre-modifications
checkTxNILFSAfter :: TxNILFSLog -> [(UTCTime,TxNILFSWrites)] -> ForestM TxNILFS Bool
checkTxNILFSAfter txlog txchgs = do
	let txwrites = mconcat $ List.map Prelude.snd txchgs
	(tree,ds,newtree,reads,bufftbl,memotbl,cantree,tmps) <- forestIO $ readIORef txlog
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
	(tree1,delta1,newtree1,reads1,bufftbl1,memotbl1,cantree1,tmps1) <- readIORef txlog1
	(tree2,delta2,newtree2,reads2,bufftbl2,memotbl2,cantree2,tmps2) <- readIORef txlog2
	
	MemoTable.mapM_ (\(uid,entry) -> MemoTable.insertWithMkWeak bufftbl2 uid entry (dynTxMkWeak entry)) bufftbl1
	MemoTable.mapM_ (\(uid,entry@(_,mkWeak,_)) -> MemoTable.insertWithMkWeak memotbl2 uid entry mkWeak) memotbl1
	
	writeIORef txlog2 (tree1,delta1,newtree1,reads1 `Set.union` reads2,bufftbl2,memotbl2,cantree1,tmps1 `Set.union` tmps2)
	
-- does not commit writes, just merges reads
extendTxNILFSLog :: TxNILFSTreeDeltas -> TxNILFSLog -> TxNILFSLog -> ForestM TxNILFS ()
extendTxNILFSLog deltas txlog1 txlog2 = forestIO $ do
	(tree1,delta1,newtree1,reads1,bufftbl1,memotbl1,cantree1,tmps1) <- readIORef txlog1
	(tree2,delta2,newtree2,reads2,bufftbl2,memotbl2,cantree2,tmps2) <- readIORef txlog2
	
	-- forget newly created nested versions
	version2 <- fsTreeFSVersion tree2
	modifyIORef deltas $ Map.filterWithKey (\version _ -> version <= version2)
	-- reset the next tree
	fsTreeFSTreeDelta newtree2 >>= \d -> writeIORef d emptyFSTreeDelta
	
	writeIORef txlog2 (tree2,delta2,newtree2,reads1 `Set.union` reads2,bufftbl2,memotbl2,cantree2,tmps1 `Set.union` tmps2)

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
	(tree,deltas,newtree,reads,bufftbl,memotbl,cantree,tmps) <- forestIO $ readIORef txlog
	
	wakes <- if doWrites
		then do
			-- acquire locks on the written memory addresses in sorted order, and commit their changes (memory modifications are done atomically)
			forestIO $ acquireTxNILFSBuffTable bufftbl
			forestIO $ commitTxNILFSBuffTable bufftbl
			-- commit the buffered memo table
			forestIO $ commitTxNILFSMemoTable memotbl
			-- commit the memoized canonical paths (don't merge but overwrite)
			forestIO $ swapMVar canonicalTxNILFS cantree
			
			-- get writes since the beginning of the tx
			writes <- forestIO (readTreeFSTreeDelta tree)
			-- commits filesystem modifications over the latest NILFS snapshot
			wakes <- forestIO $ commitFSTreeDelta "" writes
			-- requests a new NILFS snapshot for the latest tree
			ss <- newNILFSTree tree
			return wakes
		else return Set.empty
	-- removes temporary files
	forestIO $ Foldable.mapM_ (\tmp -> runShellCommand_ $ "rm -rf " ++ tmp) tmps
	return wakes

atomicTxNILFS :: String -> FTM TxNILFS a -> FTM TxNILFS a
atomicTxNILFS msg m = do
	-- get the changes of the innermost txlog
	(reads,writes) <- forestM getTxNILFSChangesFlat
	-- wait on currently acquired read locks (to ensure that concurrent writes are seen by this tx's validation step)
	forestM $ forestIO $ print $ "entering atomic " ++ msg
	x <- txNILFSLayer proxyOutside $ withFileLocks reads writes $ unTxNILFSLayer proxyOutside $ forestM (forestIO $ print $ "entered atomic " ++ msg) >> m
	forestM $ forestIO $ print $ "left atomic " ++ msg
	return x

{-# NOINLINE nilfsData #-}
nilfsData :: NILFSData
nilfsData = unsafePerformIO $ do
	rootPath <- readMVar rootTxNILFS
	findRootDevice rootPath

{-# NOINLINE liveSnapshots #-}
liveSnapshots :: LiveSnapshots
liveSnapshots = unsafePerformIO $ CWeakMap.empty

-- a global table of symbolic links that all transactions read from and keep updated
-- this is used to explore the locality in FS modifications; without knowing the existing symlinks, there is no locality
-- all the symlinks at a specific snapshot
{-# NOINLINE symlinks #-}
symlinks :: CWeakMap.WeakMap Snapshot FSTreeSym
symlinks = unsafePerformIO $ CWeakMap.empty

getSymlinks :: Maybe (Snapshot,FSDeltas) -> Snapshot -> MkWeak -> ForestM TxNILFS FSTreeSym
getSymlinks mb_old snap mkWeak = do
	root <- forestIO $ readMVar rootTxNILFS
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


updateRootTxNILFS :: FilePath -> IO ()
updateRootTxNILFS root = do
	swapMVar rootTxNILFS root
	return ()
