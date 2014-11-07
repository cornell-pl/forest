{-# LANGUAGE ConstraintKinds, UndecidableInstances, TupleSections, FlexibleInstances, MultiParamTypeClasses, StandaloneDeriving, GeneralizedNewtypeDeriving, FlexibleContexts, DataKinds, TypeFamilies, Rank2Types, GADTs, ViewPatterns, DeriveDataTypeable #-}

-- Regular filesystem with optimistic concurrency support for transactions, with purely functional data structures

module Language.Forest.Pure.FS.TxFS where

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
import Data.IORef
import System.FilePath.Posix
import Language.Forest.IO.Utils
import Language.Forest.Pure.MetaData
import Control.Concurrent.Lock (Lock(..))
import qualified Control.Concurrent.Lock as Lock
import Control.Concurrent.MVar
import Language.Forest.IO.Shell
import Language.Forest.IO.Utils
import Language.Forest.Pure.Generic
import Data.IORef
import Control.Monad.Ref
import Data.Map (Map(..))
import qualified Data.Map as Map
import Data.Set (Set(..))
import qualified Data.Set as Set
import Control.Monad.State (State(..),StateT(..),MonadState(..))
import Language.Forest.IC.FS.FSDelta
import Control.Monad.Trans
import qualified Control.Monad.State as State
import Language.Forest.Manifest
import Data.Time.Clock
import Safe

-- a list of the starting times of running transactions sorted from newest to oldest
{-# NOINLINE runningTransactions #-}
runningTransactions :: MVar [UTCTime]
runningTransactions = unsafePerformIO $ newMVar []

-- a map with commit times of committed transactions and their performed changes
{-# NOINLINE doneTransactions #-}
doneTransactions :: MVar (Map UTCTime TxFSWrites)
doneTransactions = unsafePerformIO $ newMVar Map.empty

startTxFSTransaction :: IO UTCTime
startTxFSTransaction = modifyMVar runningTransactions (\xs -> getCurrentTime >>= \t -> return (t:xs,t))

-- validates and commits a transaction as a single atomic operation
validateAndCommitTxFSTransaction :: UTCTime -> TxFSChangesFlat -> IO Bool
validateAndCommitTxFSTransaction starttime chgs@(reads,writes) = Lock.with noFSLock $ do
	-- gets the transactions that commited after the current transaction's start time
	finished <- liftM (map snd . Map.toAscList . Map.filterWithKey (\k v -> k > starttime)) $ readMVar doneTransactions
	let check = checkTxFSTransaction chgs finished
	if check
		then finishTransaction starttime writes
		else modifyMVar_ runningTransactions (return . List.delete starttime)
	return check

checkTxFSTransaction :: TxFSChangesFlat -> [TxFSWrites] -> Bool
checkTxFSTransaction chg [] = True
checkTxFSTransaction chg (d:ds) = checkTxFSTransaction' d chg && checkTxFSTransaction chg ds

-- checks if two logs of modifications are consistent
checkTxFSTransaction' :: TxFSWrites -> TxFSChangesFlat -> Bool
checkTxFSTransaction' (writesBefore) (readsAfter,writesAfter) = do
	readsAfter `Set.intersection` writesBefore == Set.empty -- no write-read conflicts
--	&& writesAfter `Set.intersection` writesBefore == Set.empty -- no write-write conflicts

-- adds a new commited transaction, and deletes done transactions that finished before the start of the earliest running transaction
finishTransaction :: UTCTime -> TxFSWrites -> IO ()
finishTransaction starttime writes = do
	mb <- modifyMVar runningTransactions (\xs -> return (List.delete starttime xs,lastMay xs))
	case mb of
		Just oldt -> modifyMVar_ doneTransactions (\m -> getCurrentTime >>= \now -> return $ Map.filterWithKey (\t _ -> t > oldt) $ Map.insert now writes m)
		Nothing -> modifyMVar_ doneTransactions (\m -> getCurrentTime >>= \now -> return $ Map.insert now writes m)
	error "perform actual commit!"

{-# NOINLINE noFSLock #-}
noFSLock :: Lock
noFSLock = unsafePerformIO $ Lock.new

instance TransactionalPureForest TxFS where

	atomically = atomicallyTxFS

-- we remember a set of read files
-- we use an asbolute tree delta (starting at the filesystem's root) to keep track of the filesystem modifications performed by the transaction, to be committed at the end
-- we keep a set of temporary files/directories used  by the transaction, that are purged after commit
type TxFSLog = (TxFSChanges,Set FilePath)

type TxFSReads = Set FilePath
type TxFSWrites = Set FilePath
-- changes just as a set of reads and a set of writes
type TxFSChangesFlat = (TxFSReads,TxFSWrites)
-- changes as a set of reads and a modification tree
type TxFSChanges = (TxFSReads,FSTreeDelta)

getTxFSChanges :: (MonadState TxFSLog (ForestM TxFS)) => ForestM TxFS TxFSChanges
getTxFSChanges = liftM fst State.get

putTxFSRead :: (MonadState TxFSLog (ForestM TxFS)) => FilePath -> ForestM TxFS ()
putTxFSRead path = State.modify $ \((paths,y),z) -> ((Set.insert path paths,y),z)

getTxFSTmp :: (MonadState TxFSLog (ForestM TxFS)) => ForestM TxFS (Set FilePath)
getTxFSTmp = liftM snd State.get

putTxFSTmp :: (MonadState TxFSLog (ForestM TxFS)) => FilePath -> ForestM TxFS ()
putTxFSTmp tmp = State.modify $ \(x,tmps) -> (x,Set.insert tmp tmps)

getTxFSTreeDeltas :: (MonadState TxFSLog (ForestM TxFS)) => ForestM TxFS FSTreeDelta
getTxFSTreeDeltas = State.get >>= \((_,td),_) -> return td

modifyTxFSTreeDeltas :: (MonadState TxFSLog (ForestM TxFS)) => (FSTreeDelta -> FSTreeDelta) -> ForestM TxFS ()
modifyTxFSTreeDeltas f = State.modify $ \((x,td),z) -> ((x,f td),z)

atomicallyTxFS :: ForestM TxFS a -> IO a
atomicallyTxFS t = do
	let try = runForest TxFSForestCfg $ do
		time <- forestIO $ startTxFSTransaction
		x <- t
		(reads,td) <- getTxFSChanges
		let writes = fsTreeDeltaWrites td
		success <- forestIO $ validateAndCommitTxFSTransaction time (reads,writes)
		getTxFSTmp >>= return . Set.foldr (\path m -> removePath path >> m) (return ()) -- remove all temporary data used by this run
		return (x,success)
	let tryAgain = do
		(x,success) <- try
		if success then return x else tryAgain -- if a transaction is invalid or fails, just restart it
	tryAgain

-- filesystems are always different
instance Eq (FSTree TxFS) where
	t1 == t2 = False

instance FSRep TxFS where
	
	newtype ForestM TxFS a = TxFSForestM { runTxFSForestM :: StateT TxFSLog IO a } deriving (Functor,Applicative,Monad,MonadState TxFSLog,MonadLazy)
	
	data ForestCfg TxFS = TxFSForestCfg
	
	runForest _ (TxFSForestM m) = do
		mountAVFS
		x <- State.evalStateT m ((Set.empty,Map.empty),Set.empty)
		unmountAVFS
		return x
	
	forestIO = TxFSForestM . lift
	
	-- | returns the forest directory used to store auxiliary data
	getForestDirectory = liftM (</> "Forest") $ forestIO getTemporaryDirectory
	
	-- there is no notion of filesystem versions, but we still need to distinguish AVFS mode from normal mode
	data FSTree TxFS = TxFSTree | VirtualTxFSTree deriving Show
	
	-- log the modifications
	deletePath path = modifyTxFSTreeDeltas $ appendToFSTreeDelta (Rem path)
	writeFile path ondisk = modifyTxFSTreeDeltas $ appendToFSTreeDelta (Add path ondisk)
	writeDir path = modifyTxFSTreeDeltas $ appendToFSTreeDelta (Add path path) --XXX:revise this...
	writeLink path ondisk = modifyTxFSTreeDeltas $ appendToFSTreeDelta (Add path ondisk)
	writePathMD path ondisk = modifyTxFSTreeDeltas $ appendToFSTreeDelta (ChgAttrs path ondisk)
	
	-- registers a new temporary path
	tempPath = forestIO getTempPath >>= \path -> putTxFSTmp path >> return path
	-- change into AVFS mode
	virtualTree _ = return VirtualTxFSTree
	latestTree = return TxFSTree
	
	-- reads from the FS or from the modification log
	pathInTree path TxFSTree = do
		(reads,td) <- getTxFSChanges
		let td' = focusFSTreeDeltaByRelativePathMay td path
		case onDiskWriteMay td' of
			Nothing -> putTxFSRead path >> return path
			Just ondisk -> return ondisk
	pathInTree path VirtualTxFSTree = do
		ondisk <- pathInTree path TxFSTree
		home <- forestIO $ getHomeDirectory
		let result = home </> ".avfs" </> makeRelative "/" ondisk
		return result
		
	stepPathInTree _ path rel = return $ path </> rel
	canonalizePathWithTree path _ = forestIO $ canonalizePath path









