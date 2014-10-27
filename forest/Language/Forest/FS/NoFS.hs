{-# LANGUAGE ConstraintKinds, UndecidableInstances, TupleSections, FlexibleInstances, MultiParamTypeClasses, StandaloneDeriving, GeneralizedNewtypeDeriving, FlexibleContexts, DataKinds, TypeFamilies, Rank2Types, GADTs, ViewPatterns, DeriveDataTypeable #-}

-- Regular filesystem with optimistic concurrency support for transactions, without any incrementality

module Language.Forest.FS.NoFS where

import Language.Forest.FS.FSRep
import System.IO.Unsafe
import Control.Monad
import Data.Maybe
import Data.List
import Data.WithClass.MData
import Data.Typeable
import System.Directory
import Data.List as List
import Data.IORef
import System.FilePath.Posix
import Language.Forest.IO.Utils
import Language.Forest.MetaData
import Control.Concurrent.Lock (Lock(..))
import qualified Control.Concurrent.Lock as Lock
import Control.Concurrent.MVar
import Control.Monad.Incremental as Inc
import Language.Forest.Shell
import Language.Forest.Generic
import Control.Monad.Incremental.LazyNonInc
import Data.IORef
import Control.Monad.Ref
import Data.Map (Map(..))
import qualified Data.Map as Map
import Data.Set (Set(..))
import qualified Data.Set as Set
import Control.Monad.State (State(..),StateT(..),MonadState(..))
import Language.Forest.FS.FSDelta
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
doneTransactions :: MVar (Map UTCTime NoFSWrites)
doneTransactions = unsafePerformIO $ newMVar Map.empty

startNoFSTransaction :: IO UTCTime
startNoFSTransaction = modifyMVar runningTransactions (\xs -> getCurrentTime >>= \t -> return (t:xs,t))

-- validates and commits a transaction as a single atomic operation
validateAndCommitNoFSTransaction :: UTCTime -> NoFSChangesFlat -> IO Bool
validateAndCommitNoFSTransaction starttime chgs@(reads,writes) = Lock.with noFSLock $ do
	-- gets the transactions that commited after the current transaction's start time
	finished <- liftM (map snd . Map.toAscList . Map.filterWithKey (\k v -> k > starttime)) $ readMVar doneTransactions
	let check = checkNoFSTransaction chgs finished
	if check
		then finishTransaction starttime writes
		else modifyMVar_ runningTransactions (return . List.delete starttime)
	return check

checkNoFSTransaction :: NoFSChangesFlat -> [NoFSWrites] -> Bool
checkNoFSTransaction chg [] = True
checkNoFSTransaction chg (d:ds) = checkNoFSTransaction' d chg && checkNoFSTransaction chg ds

-- checks if two logs of modifications are consistent
checkNoFSTransaction' :: NoFSWrites -> NoFSChangesFlat -> Bool
checkNoFSTransaction' (writesBefore) (readsAfter,writesAfter) = do
	readsAfter `Set.intersection` writesBefore == Set.empty -- no write-read conflicts
--	&& writesAfter `Set.intersection` writesBefore == Set.empty -- no write-write conflicts

-- adds a new commited transaction, and deletes done transactions that finished before the start of the earliest running transaction
finishTransaction :: UTCTime -> NoFSWrites -> IO ()
finishTransaction starttime writes = do
	mb <- modifyMVar runningTransactions (\xs -> return (List.delete starttime xs,lastMay xs))
	case mb of
		Just oldt -> modifyMVar_ doneTransactions (\m -> getCurrentTime >>= \now -> return $ Map.filterWithKey (\t _ -> t > oldt) $ Map.insert now writes m)
		Nothing -> return ()
	error "perform actual commit!"

{-# NOINLINE noFSLock #-}
noFSLock :: Lock
noFSLock = unsafePerformIO $ Lock.new

instance TransactionalForest NoFS where

	atomically = atomicallyNoFS

-- we remember a set of read files
-- we use an asbolute tree delta (starting at the filesystem's root) to keep track of the filesystem modifications performed by the transaction, to be committed at the end
-- we keep a set of temporary files/directories used  by the transaction, that are purged after commit
type NoFSLog = (NoFSChanges,Set FilePath)

type NoFSReads = Set FilePath
type NoFSWrites = Set FilePath
-- changes just as a set of reads and a set of writes
type NoFSChangesFlat = (NoFSReads,NoFSWrites)
-- changes as a set of reads and a modification tree
type NoFSChanges = (NoFSReads,FSTreeDelta)

getNoFSChanges :: (MonadState NoFSLog (ForestL NoFS l),ForestLayer NoFS l) => ForestL NoFS l NoFSChanges
getNoFSChanges = liftM fst State.get

putNoFSRead :: (MonadState NoFSLog (ForestL NoFS l),ForestLayer NoFS l) => FilePath -> ForestL NoFS l ()
putNoFSRead path = State.modify $ \((paths,y),z) -> ((Set.insert path paths,y),z)

getNoFSTmp :: (MonadState NoFSLog (ForestL NoFS l),ForestLayer NoFS l) => ForestL NoFS l (Set FilePath)
getNoFSTmp = liftM snd State.get

putNoFSTmp :: (MonadState NoFSLog (ForestL NoFS l),ForestLayer NoFS l) => FilePath -> ForestL NoFS l ()
putNoFSTmp tmp = State.modify $ \(x,tmps) -> (x,Set.insert tmp tmps)

getNoFSTreeDeltas :: (MonadState NoFSLog (ForestL NoFS l),ForestLayer NoFS l) => ForestL NoFS l FSTreeDelta
getNoFSTreeDeltas = State.get >>= \((_,td),_) -> return td

modifyNoFSTreeDeltas :: (MonadState NoFSLog (ForestL NoFS l),ForestLayer NoFS l) => (FSTreeDelta -> FSTreeDelta) -> ForestL NoFS l ()
modifyNoFSTreeDeltas f = State.modify $ \((x,td),z) -> ((x,f td),z)

atomicallyNoFS :: Transaction NoFS a -> IO a
atomicallyNoFS t = do
	let try = runIncremental NoFSIncrementalArgs $ do
		time <- forestIO $ startNoFSTransaction
		x <- intepretNoFSTransaction t
		(reads,td) <- getNoFSChanges
		let writes = fsTreeDeltaWrites td
		success <- forestIO $ validateAndCommitNoFSTransaction time (reads,writes)
		getNoFSTmp >>= return . Set.foldr (\path m -> removePath path >> m) (return ()) -- remove all temporary data used by this run
		return (x,success)
	let tryAgain = do
		(x,success) <- try
		if success then return x else tryAgain -- if a transaction is invalid or fails, just restart it
	tryAgain
	
intepretNoFSTransaction :: Transaction NoFS a -> TFS NoFS a
intepretNoFSTransaction (Load margs path) = inside $ loadScratch margs path NoFSTree Nothing NoFSTree getForestMDInTree 
intepretNoFSTransaction (Manifest margs dta) = generateManifestScratch margs NoFSTree dta
intepretNoFSTransaction (Store manifest) = storeManifest manifest

instance Incremental (IncForest NoFS) IORef IO where
	
	newtype Outside (IncForest NoFS) IORef IO a = NoFSForestO { runNoFSForestO :: StateT NoFSLog (Outside LazyNonInc IORef IO) a } deriving (Monad,MonadState NoFSLog)
	newtype Inside (IncForest NoFS) IORef IO a = NoFSForestI { runNoFSForestI :: StateT NoFSLog (Inside LazyNonInc IORef IO) a } deriving (Monad,MonadState NoFSLog)

	world = NoFSForestO . State.mapStateT inside . runNoFSForestI

	data IncrementalArgs (IncForest NoFS) = NoFSIncrementalArgs

	runIncremental _ m = runLazyNonIncOuter $ State.evalStateT (runNoFSForestO m) ((Set.empty,Map.empty),Set.empty)

instance InLayer Outside (IncForest NoFS) IORef IO where
	inL = NoFSForestO . lift . LazyNonIncOuter
	{-# INLINE inL #-}
instance InLayer Inside (IncForest NoFS) IORef IO where
	inL = NoFSForestI . lift . LazyNonIncInner
	{-# INLINE inL #-}

instance Eq (FSTree NoFS) where
	t1 == t2 = False

instance FSRep NoFS where
	
	forestIO = inside . liftLazyNonIncNoFS . inL . liftIO where
		liftLazyNonIncNoFS :: Inside LazyNonInc IORef IO a -> Inside (IncForest NoFS) IORef IO a
		liftLazyNonIncNoFS = liftInc
	
	unsafeInside = NoFSForestI . State.mapStateT (LazyNonIncInner . runLazyNonIncOuter) . runNoFSForestO
	
	-- | returns the forest directory used to store auxiliary data
	getForestDirectory = liftM (</> "Forest") $ forestIO getTemporaryDirectory
	
	-- there is no notion of filesystem versions, but we still need to distinguish AVFS mode from normal mode
	data FSTree NoFS = NoFSTree | VirtualNoFSTree
	
	-- log the modifications
	writePath path ondisk = modifyNoFSTreeDeltas $ appendToFSTreeDelta (Add path ondisk)
	writePathMD path ondisk = modifyNoFSTreeDeltas $ appendToFSTreeDelta (ChgAttrs path ondisk)
	
	-- registers a new temporary path
	tempPath = inside $ getTempPath >>= \path -> putNoFSTmp path >> return path
	-- change into AVFS mode
	virtualTree _ = return VirtualNoFSTree
	purgeTree _ = return ()
	latestTree = return NoFSTree
	
	changesBetween = error "No filesystem versioning support for NoFS"
	
	-- reads from the FS or from the modification log
	pathInTree path NoFSTree = inside $ do
		(reads,td) <- getNoFSChanges
		let td' = focusFSTreeDeltaByRelativePathMay td path
		case onDiskWriteMay td' of
			Nothing -> putNoFSRead path >> return path
			Just ondisk -> return ondisk
	pathInTree path VirtualNoFSTree = do
		ondisk <- pathInTree path NoFSTree
		home <- forestIO $ getHomeDirectory
		let result = home </> ".avfs" </> makeRelative "/" ondisk
		return result
	stepPathInTree _ path rel = return $ path </> rel
	
	showFSTree NoFSTree = "NoFSTree"
	showFSTree VirtualNoFSTree = "VirtualNoFSTree"
	
	newtype FSThunk NoFS l inc r m a = NoFSFSThunk { unNoFSFSThunk :: LazyNonIncL l (IncForest NoFS) r m a }

	eqFSThunk = error "no equality for NOFSThunk"
	fsthunkID = error "no identity for NoFSThunk"
	isUnevaluatedFSThunk = error "isUnevaluatedFSThunk"
	isUnforcedFSThunk = error "isUnforcedFSThunk"
	
	data ICThunk NoFS l inc r m a = NoFSICThunk { unNoFSICThunk :: LazyNonIncU l (IncForest NoFS) r m a }
	
	eqICThunk = error "eqICThunk"
	icthunkID = error "icthunkID"

	isUnevaluatedICThunk _ = return True

	data HSThunk NoFS l inc r m a = NoFSHSThunk { unNoFSHSThunk :: T l inc r m a }
	
	-- no memoization
	memo _ _ _ = return ()
	unmemo _ _ _ = return ()
	lookupmemo _ _ = return Nothing

instance ForestLayer NoFS l => Thunk (HSThunk NoFS) l (IncForest NoFS) IORef IO where
	new = liftM NoFSHSThunk . new
	read (NoFSHSThunk t) = Inc.read t

instance ForestLayer NoFS l => Thunk (ICThunk NoFS) l (IncForest NoFS) IORef IO where
	new = liftM NoFSICThunk . thunkLazyNonIncU
	read (NoFSICThunk t) = forceLazyNonIncU t

instance ForestLayer NoFS l => Output (ICThunk NoFS) l (IncForest NoFS) IORef IO where
	thunk = liftM NoFSICThunk . thunkLazyNonIncU
	force (NoFSICThunk t) = forceLazyNonIncU t

instance (Input LazyNonIncL l (IncForest NoFS) IORef IO,ForestLayer NoFS l,Input LazyNonIncL l LazyNonInc IORef IO) => ForestInput NoFS FSThunk l where
	fsref = liftM NoFSFSThunk . ref
	fsthunk _ = liftM NoFSFSThunk . Inc.mod
	fsset (NoFSFSThunk t) v = set t v
	fsmodify _ (NoFSFSThunk t) f = modify t f
	fsoverwrite _ (NoFSFSThunk t) m = overwrite t m
	fsforce (NoFSFSThunk t) = Inc.get t
	
	addUnmemoFSThunk t f = return ()

instance LiftInc Inside LazyNonInc (IncForest NoFS) IORef IO where
	liftInc = NoFSForestI . lift
instance LiftInc Outside LazyNonInc (IncForest NoFS) IORef IO where
	liftInc = NoFSForestO . lift










