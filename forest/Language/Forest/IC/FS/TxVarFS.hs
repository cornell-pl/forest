
{-# LANGUAGE ConstraintKinds, UndecidableInstances, TupleSections, FlexibleInstances, MultiParamTypeClasses, StandaloneDeriving, GeneralizedNewtypeDeriving, FlexibleContexts, DataKinds, TypeFamilies, Rank2Types, GADTs, ViewPatterns, DeriveDataTypeable, ScopedTypeVariables #-}

-- Regular filesystem with optimistic concurrency support for transactions, with mutable transactional variables structures mapped to specifications, and no incrementality

module Language.Forest.IC.FS.TxVarFS where

import Control.Monad.Catch
import Control.Concurrent
import System.Cmd
import Control.Exception as Exception
import Data.Strict.List
import Unsafe.Coerce
import Language.Forest.IC.ValueDelta
import Language.Forest.IC.BX
import System.Mem.WeakKey
--import System.Posix.FileLock as FileLock
import Data.Foldable as Foldable
import Data.Concurrent.Deque.Class as Queue
import Data.Concurrent.Deque.Reference.DequeInstance
import qualified Control.Concurrent.Map as CMap
import System.Posix.Files

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
import Language.Forest.IC.ICRep
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
import Language.Forest.IC.FS.FSDelta
import Control.Monad.Trans
import qualified Control.Monad.State as State
import Language.Forest.Manifest
import Data.Time.Clock
import Data.Unique
import Safe
import Control.Monad.Catch as Catch
import System.Mem.Concurrent.WeakMap as WeakMap

proxyTxVarFS = Proxy :: Proxy TxVarFS

-- a list of the starting times of running transactions sorted from newest to oldest (we may two txs with the same starting time)
{-# NOINLINE runningTxs #-}
runningTxs :: MVar [UTCTime]
runningTxs = unsafePerformIO $ newMVar []

-- insert a new time in a list sorted from newest to oldest
addRunningTx time = modifyMVarMasked_ runningTxs (\xs -> return $ List.insertBy (\x y -> compare y x) time xs)
-- remove a time in a list sorted from newest to oldest
deleteRunningTx time = modifyMVarMasked_ runningTxs (\xs -> return $ List.delete time xs)

-- a map with commit times of committed transactions and their performed changes
-- we use a @FSTreeDelta@ because the set of written paths may be infinite (like all paths under a given directory)
{-# NOINLINE doneTxs #-}
doneTxs :: MVar (Map UTCTime TxVarFSWrites)
doneTxs = unsafePerformIO $ newMVar Map.empty

-- ** Filesystem

-- current FS version
-- we remember a set of read files
-- we use an absolute tree delta (starting at the filesystem's root) to keep track of the filesystem modifications performed by the transaction, to be committed at the end
-- we keep a set of temporary files/directories used  by the transaction, that are purged after commit
type TxVarFSLog = IORef (FSVersion,TxVarFSChanges,Set FilePath)

-- nested logs for nested transactions
type TxVarFSLogs = SList TxVarFSLog

-- (starttime,nested logs)
type TxVarFSEnv = (IORef UTCTime,FSVersion,TxVarFSLogs)

type TxVarFSReads = Set FilePath
type TxVarFSWrites = Set FilePath
-- changes as a set of reads and a modification tree
type TxVarFSChanges = (TxVarFSReads,FSTreeDelta)
type TxVarFSChangesFlat = (TxVarFSReads,TxVarFSWrites)

-- the FSVersion is initialized differently for each top-level tx and modified on stores. initializing a nested tx does not modify the FSVersion
type FSVersion = Unique

-- only needs to read from the top-level log
getTxVarFSChangesFlat :: ForestM TxVarFS TxVarFSChangesFlat
getTxVarFSChangesFlat = do
	(starttime,startversion,SCons txlog _) <- Reader.ask
	(fsversion,(reads,writes),tmps) <- forestIO $ readRef txlog
	return (reads,fsTreeDeltaWrites "" writes)

getFSVersionTxVarFS :: ForestM TxVarFS FSVersion
getFSVersionTxVarFS = do
	(starttime,startversion,SCons txlog _) <- Reader.ask
	(fsversion,(reads,writes),tmps) <- forestIO $ readRef txlog
	return fsversion

incrementFSVersionTxVarFS :: ForestM TxVarFS ()
incrementFSVersionTxVarFS = do
	fsversion' <- forestIO newUnique
	setFSVersionTxVarFS fsversion'

setFSVersionTxVarFS :: FSVersion -> ForestM TxVarFS ()
setFSVersionTxVarFS new_fsversion = do
	(starttime,startversion,SCons txlog _) <- Reader.ask
	(fsversion,(reads,writes),tmps) <- forestIO $ readRef txlog
	forestIO $ writeRef txlog (new_fsversion,(reads,writes),tmps)

-- only needs to read from the top-level log
getTxVarFSChanges :: ForestM TxVarFS TxVarFSChanges
getTxVarFSChanges = do
	(starttime,startversion,SCons txlog _) <- Reader.ask
	(fsversion,(reads,writes),tmps) <- forestIO $ readRef txlog
	return (reads,writes)

putTxVarFSRead :: FilePath -> ForestM TxVarFS ()
putTxVarFSRead path = do
	(starttime,startversion,SCons txlog _) <- Reader.ask
	(fsversion,(reads,writes),tmps) <- forestIO $ readRef txlog
	forestIO $ writeRef txlog (fsversion,(Set.insert path reads,writes),tmps)

putTxVarFSTmp :: FilePath -> ForestM TxVarFS ()
putTxVarFSTmp tmp = do
	(starttime,startversion,SCons txlog _) <- Reader.ask
	(fsversion,(reads,writes),tmps) <- forestIO $ readRef txlog
	forestIO $ writeRef txlog (fsversion,(reads,writes),Set.insert tmp tmps)

modifyTxVarFSTreeDeltas :: (FSTreeDelta -> FSTreeDelta) -> ForestM TxVarFS ()
modifyTxVarFSTreeDeltas f = do
	(starttime,startversion,SCons txlog _) <- Reader.ask
	(fsversion,(reads,writes),tmps) <- forestIO $ readRef txlog
	forestIO $ writeRef txlog (fsversion,(reads,f writes),tmps)

instance Eq (FSTree TxVarFS) where
	t1 == t2 = False

instance FSRep TxVarFS where
	
	newtype ForestM TxVarFS a = TxVarFSForestM { runTxVarFSForestM :: ReaderT TxVarFSEnv IO a } deriving (Functor,Applicative,Monad,MonadReader TxVarFSEnv,MonadLazy,MonadThrow,MonadCatch,MonadMask)
	
	data ForestCfg TxVarFS = TxVarFSForestCfg
	
	runForest _ m = error "please use atomically instead"
	
	forestIO = TxVarFSForestM . lift
	
	-- | returns the forest directory used to store auxiliary data
	getForestDirectory = liftM (</> "Forest") $ forestIO getTemporaryDirectory
	
	data FSTree TxVarFS = TxVarFSTree | VirtualTxVarFSTree deriving Show
	
	-- log the modifications
	-- writes come from manifests only, that have already canonized the paths
	deletePath path = modifyTxVarFSTreeDeltas $ appendToFSTreeDelta (Rem path)
	writeFile path ondisk = modifyTxVarFSTreeDeltas $ appendToFSTreeDelta (Add path ondisk)
	writeDir path = do
		ondisk <- tempPath
		forestIO $ createDirectory ondisk
		modifyTxVarFSTreeDeltas $ appendToFSTreeDelta (Add path ondisk)
	writeLink path linkpath = modifyTxVarFSTreeDeltas $ appendToFSTreeDelta (AddLink path linkpath)
	writePathMD path ondisk = modifyTxVarFSTreeDeltas $ appendToFSTreeDelta (ChgAttrs path ondisk)
	
	-- registers a new temporary path
	tempPath = forestIO getTempPath >>= \path -> putTxVarFSTmp path >> return path
	-- change into AVFS mode
	virtualTree _ = return VirtualTxVarFSTree
	latestTree = return TxVarFSTree
	
	-- reads from the FS or from the modification log
	pathInTree path TxVarFSTree = do
		-- canonize the fullpath for reads
		canpath <- canonalizeDirectoryInTree path TxVarFSTree
		-- mark the path as read
		putTxVarFSRead canpath
		(_,td) <- getTxVarFSChanges
		let path_td = focusFSTreeDeltaByRelativePathMay td canpath
		case onDiskWriteMay path_td of
			Nothing -> do return canpath
			Just ondisk -> return ondisk
	pathInTree path VirtualTxVarFSTree = do
		ondisk <- pathInTree path TxVarFSTree
		home <- forestIO $ getHomeDirectory
		let result = home </> ".avfs" </> makeRelative "/" ondisk
		return result
		
	stepPathInTree _ path rel = return $ path </> rel
	
	getDirectoryContentsInTree dir _ = getDirectoryContentsTxVarFS dir
		
	doesDirectoryExistInTree path _ = doesExistTxVarFS doesDirectoryExist path
	doesFileExistInTree path _ = doesExistTxVarFS doesFileExist path
	doesExistInTree path _ = doesExistTxVarFS (\path -> doesDirectoryExist path >>= \isDir -> if isDir then return True else doesFileExist path) path
	
	canonalizePathWithTree path _ = canonalizePathTxVarFS path

getDirectoryContentsTxVarFS :: FilePath -> ForestM TxVarFS [FileName]
getDirectoryContentsTxVarFS path = do
	canpath <- canonalizeDirectoryInTree path TxVarFSTree
	putTxVarFSRead canpath
	(_,td) <- getTxVarFSChanges
	let path_td = focusFSTreeDeltaByRelativePathMay td canpath
	xs <- forestIO $ getContentsFSTreeDeltaNodeMay canpath path_td
	return xs

doesExistTxVarFS :: (FilePath -> IO Bool) -> FilePath -> ForestM TxVarFS Bool
doesExistTxVarFS test path = do
	canpath <- canonalizeDirectoryInTree path TxVarFSTree
	putTxVarFSRead canpath
	(_,td) <- getTxVarFSChanges
	let path_td = focusFSTreeDeltaByRelativePathMay td canpath
	case path_td of
		Just (FSTreeNew _ _ diskpath) -> forestIO $ test diskpath
		otherwise -> forestIO $ test canpath

-- canonalizes a path, taking into account the buffered FSTreeDelta and logging reads of followed symlinks
canonalizePathTxVarFS :: FilePath -> ForestM TxVarFS FilePath
canonalizePathTxVarFS path = do
	norm <- forestIO $ liftM normalise $ absolutePath path
	let dirs = splitDirectories norm
	follow "" dirs
  where
	follow root [] = return root
	follow root (".":dirs) = follow root dirs
	follow root ("..":dirs) = follow (takeDirectory root) dirs
	follow root (dir:dirs) = do
		(reads,td) <- getTxVarFSChanges
		let rootdir = root </> dir
		let td' = focusFSTreeDeltaByRelativePathMay td rootdir
		case td' of
			Just (FSTreeNewLink link _) -> do -- follow buffered links
				-- mark the symbolic link as read
				putTxVarFSRead rootdir
				canLink <- canonalizePathTxVarFS (root </> link)
				follow canLink dirs
			otherwise -> do -- follow FS links
				e <- forestIO $ Exception.try $ liftM isSymbolicLink (getSymbolicLinkStatus rootdir)
				case e of
					Left (e::SomeException) -> follow rootdir dirs
					Right isLink -> do
						if isLink
							then do
								-- mark the symbolic link as read
								putTxVarFSRead rootdir
								e <- forestIO $ Exception.try $ readSymbolicLink rootdir
								case e of
									Left (e::SomeException) -> follow rootdir dirs
									Right p -> do
										canLink <- canonalizePathTxVarFS (root </> p)
										follow canLink dirs
							else follow rootdir dirs

-- ** Incrementality

instance Incremental (IncForest TxVarFS) IORef IO where
	
	newtype Outside (IncForest TxVarFS) IORef IO a = TxVarFSForestO { runTxVarFSForestO :: ReaderT TxVarFSEnv IO a } deriving (Monad,MonadLazy,MonadReader TxVarFSEnv,MonadThrow,MonadCatch,MonadMask)
	newtype Inside (IncForest TxVarFS) IORef IO a = TxVarFSForestI { runTxVarFSForestI :: ReaderT TxVarFSEnv IO a } deriving (Monad,MonadLazy,MonadReader TxVarFSEnv,MonadThrow,MonadCatch,MonadMask)

	world = TxVarFSForestO . runTxVarFSForestI
	unsafeWorld = TxVarFSForestI . runTxVarFSForestO

	runIncremental = error "please use atomically instead"

instance InLayer Outside (IncForest TxVarFS) IORef IO where
	inL = TxVarFSForestO . lift
	{-# INLINE inL #-}
instance InLayer Inside (IncForest TxVarFS) IORef IO where
	inL = TxVarFSForestI . lift
	{-# INLINE inL #-}
	
instance ICRep TxVarFS where

	forestM = inside . TxVarFSForestI . runTxVarFSForestM
	forestO = TxVarFSForestM . runTxVarFSForestO

	-- stores a computation and a concurrent map from @FSVersion@s to computed values
	newtype FSThunk TxVarFS l inc r m a = TxVarFSThunk (IORef (Dynamic,FilePath),l inc r m a,WeakMap FSVersion a)

	newtype HSThunk TxVarFS l inc r m a = TxVarHSThunk (IORef (Dynamic,FilePath),l inc r m a,WeakMap FSVersion a)
	
	newtype ICThunk TxVarFS l inc r m a = TxVarICThunk (l inc r m a)

instance ZippedICMemo TxVarFS where

	addZippedMemo path proxy args rep tree = forestM $ forestIO $ do
		let (TxVarFSThunk (dyn,_,_)) = to iso_rep_thunk rep
		putStrLn $ "adding args " ++ show (typeOf rep) ++ " " ++ show (typeOf args)
		writeIORef dyn (toDyn args,path)
		
	findZippedMemo args path rep = return Nothing

getFTVArgs :: (FTK TxVarFS args rep content) => Proxy args -> rep -> ForestM TxVarFS (ForestIs TxVarFS args,FilePath)
getFTVArgs (proxy ::Proxy args) rep = forestIO $ do
	let (TxVarFSThunk (rdyn,_,_)) = to iso_rep_thunk rep
	(dyn,path) <- readIORef rdyn
	case fromDynamic dyn of
		Nothing -> error $ "should not happen " ++ show (typeOf rep) ++ " " ++ show (typeOf (undefined::args)) ++ " " ++  show (dynTypeRep dyn)
		Just args -> return (args,path)
	

instance ForestLayer TxVarFS l => Thunk (HSThunk TxVarFS) l (IncForest TxVarFS) IORef IO where
	new m = do
		rdyn <- forestM $ forestIO $ newIORef (toDyn (),"")
		tbl <- forestM $ forestIO $ WeakMap.new
		return $ TxVarHSThunk (rdyn,m,tbl)
	read (TxVarHSThunk (rdyn,m,tbl)) = do
		fsversion <- forestM getFSVersionTxVarFS
		mb <- forestM $ forestIO $ WeakMap.lookup tbl fsversion
		case mb of
			Nothing -> do
				v <- m
				forestM $ forestIO $ WeakMap.insertWithMkWeak tbl (MkWeak $ mkWeakRefKey rdyn) fsversion v
				return v
			Just v -> return v

instance ForestLayer TxVarFS l => Thunk (ICThunk TxVarFS) l (IncForest TxVarFS) IORef IO where
	new m = return $ TxVarICThunk m
	read (TxVarICThunk m) = m
instance ForestLayer TxVarFS l => Output (ICThunk TxVarFS) l (IncForest TxVarFS) IORef IO where
	thunk = Inc.new
	force = Inc.read

instance (ForestLayer TxVarFS l) => Thunk (FSThunk TxVarFS) l (IncForest TxVarFS) IORef IO where
	new m = do
		rdyn <- forestM $ forestIO $ newIORef (toDyn (),"")
		tbl <- forestM $ forestIO $ WeakMap.new
		return $ TxVarFSThunk (rdyn,m,tbl)
	read (TxVarFSThunk (rdyn,m,tbl)) = do
		fsversion <- forestM getFSVersionTxVarFS
		mb <- forestM $ forestIO $ WeakMap.lookup tbl fsversion
		case mb of
			Nothing -> do
				v <- m
				forestM $ forestIO $ WeakMap.insertWithMkWeak tbl (MkWeak $ mkWeakRefKey rdyn) fsversion v
				return v
			Just v -> return v

instance (ForestLayer TxVarFS l) => Input (FSThunk TxVarFS) l (IncForest TxVarFS) IORef IO where
	ref c = Inc.new (return c)
	mod = Inc.new
	get = Inc.read
	set (TxVarFSThunk (rdyn,m,tbl)) v = do
		forestM incrementFSVersionTxVarFS
		new_fsversion <- forestM getFSVersionTxVarFS
		forestM $ forestIO $ WeakMap.insertWithMkWeak tbl (MkWeak $ mkWeakRefKey rdyn) new_fsversion v

-- ** Transactions

-- the Forest transactional monad
type TxVarFTM = FTM TxVarFS
-- a Forest transactional variable
type TxVarFTV a = FTV TxVarFS a

instance TxICForest TxVarFS where
	
	atomically = atomicallyTxVarFS
	retry = retryTxVarFS
	orElse = orElseTxVarFS 
	throw = throwTxVarFS 
	catch = catchTxVarFS 
	new = newTxVarFS Proxy
	read = readTxVarFS
	writeOrElse = writeOrElseTxVarFS

newTxVarFS :: FTK TxVarFS args rep content => Proxy args -> ForestVs args -> FilePath -> TxVarFTM rep
newTxVarFS proxy args path = inside $ zload (vmonadArgs proxyTxVarFS proxy args) path

readTxVarFS :: FTK TxVarFS args rep content => rep -> TxVarFTM content
readTxVarFS rep = Inc.getOutside (to iso_rep_thunk rep)

writeOrElseTxVarFS :: FTK TxVarFS args rep content => rep -> content -> b -> ([ManifestError] -> TxVarFTM b) -> TxVarFTM b
writeOrElseTxVarFS rep content b f = do
	let t = to iso_rep_thunk rep
	(starttime,old_fsversion,SCons fslog_ref _) <- Reader.ask
	old_fslog <- forestM $ forestIO $ readIORef fslog_ref
	set t content -- automatically increments the FSVersion
	(args :: ForestIs TxVarFS args,path) <- forestM $ getFTVArgs Proxy rep
	mani <- zmanifest' (Proxy :: Proxy args) args path rep
	-- we need to store the errors to the (buffered) FS before validating
	forestM $ storeManifest mani
	forestM $ forestIO $ putStrLn "Manifest!"
	forestM $ forestIO $ print mani
	errors <- forestM $ manifestErrors mani
	if List.null errors
		then return b
		else do
			-- rollback the modifications
			forestM $ forestIO $ writeIORef fslog_ref old_fslog
			forestM $ setFSVersionTxVarFS old_fsversion
			f errors

atomicallyTxVarFS :: TxVarFTM b -> IO b
atomicallyTxVarFS stm = initializeTxVarFS try where
	try = flip Catch.catches [Catch.Handler catchInvalid,Catch.Handler catchRetry,Catch.Handler catchSome] $ do
		-- run the tx
		x <- stm
		-- tries to commit the current tx, otherwise repairs it incrementally
		success <- validateAndCommitTopTxVarFS True
		if success
			then return x
			else debug "throw InvalidTx" $ throwM InvalidTx
	catchInvalid InvalidTx = debug "InvalidTx" $ do
		forestM $ forestIO $ threadDelay 2000000
		resetTxVarFS try
	catchRetry BlockedOnRetry = debug "BlockedOnRetry" $ do
		-- if the retry was invoked on an inconsistent state, we incrementally repair and run again, otherwise we place the tx in the waiting queue
		mbsuccess <- validateAndRetryTopTxVarFS
		case mbsuccess of
			Just lck -> do -- retried txs are always in a consistent state, because we apply all affecting updates before releasing the lock
				-- wait for the lock to be released (whenever some variables that it depends on are changed)
				-- we don't consume the contents of the mvar to avoid further puts to succeeed; a new MVar is created for each retry
				inL $ liftIO $ Lock.acquire lck
				resetTxVarFS try
			Nothing -> resetTxVarFS try
	catchSome (e::SomeException) = debug ("SomeException "++show e) $ do
		-- we still need to validate on exceptions, otherwise repair incrementally; transaction-local allocations still get committed
		success <- validateAndCommitTopTxVarFS False
		if success
			then throwM e
			else do
				forestM $ forestIO $ threadDelay 2000000
				resetTxVarFS try


retryTxVarFS :: TxVarFTM a
retryTxVarFS = inL $ liftIO $ throwIO BlockedOnRetry

orElseTxVarFS :: TxVarFTM a -> TxVarFTM a -> TxVarFTM a
orElseTxVarFS stm1 stm2 = do1 where
	try1 = do { x <- stm1; validateAndCommitNestedTxVarFS Nothing; return x }
	try2 = do { x <- stm2; validateAndCommitNestedTxVarFS Nothing; return x }
	do1 = startNestedTxVarFS $ try1 `Catch.catches` [Catch.Handler catchRetry1,Catch.Handler catchInvalid,Catch.Handler catchSome]
	do2 = startNestedTxVarFS $ try2 `Catch.catches` [Catch.Handler catchRetry2,Catch.Handler catchInvalid,Catch.Handler catchSome]
	catchRetry1 BlockedOnRetry = validateAndRetryNestedTxVarFS >> do2
	catchRetry2 BlockedOnRetry = validateAndRetryNestedTxVarFS >> throwM BlockedOnRetry
	catchInvalid (e::InvalidTx) = throwM e
	catchSome (e::SomeException) = validateAndCommitNestedTxVarFS (Just e) >> throwM e

throwTxVarFS :: Exception e => e -> TxVarFTM a
throwTxVarFS = Catch.throwM

catchTxVarFS :: Exception e => TxVarFTM a -> (e -> TxVarFTM a) -> TxVarFTM a
catchTxVarFS stm h = stm `Catch.catches` [Catch.Handler catchInvalid,Catch.Handler catchRetry,Catch.Handler catchSome] where
	catchInvalid (e::InvalidTx) = throwM e
	catchRetry (e::BlockedOnRetry) = throwM e
	catchSome (e::SomeException) = do
		validateCatchTxVarFS
		h $ fromJust $ fromException e

initializeTxVarFS :: TxVarFTM b -> IO b 
initializeTxVarFS (TxVarFSForestO m) = do
	starttime <- startTxVarFS >>= newIORef
	mountAVFS -- should succeed even if AVFS is already mounted
	fsversion <- newUnique
	txlog <- newIORef (fsversion,(Set.empty,emptyFSTreeDelta),Set.empty)
	Reader.runReaderT m (starttime,fsversion,SCons txlog SNil)
	-- don't unmount AVFS, since multiple parallel txs may be using it

-- resets
resetTxVarFS :: TxVarFTM a -> TxVarFTM a
resetTxVarFS m = do
	now <- inL $ liftIO $ startTxVarFS >>= newIORef
	fsversion <- inL $ liftIO $ newUnique
	txlog <- inL $ liftIO $ newIORef (fsversion,(Set.empty,emptyFSTreeDelta),Set.empty)
	Reader.local (\_ -> (now , fsversion,SCons txlog SNil)) m

-- we need to acquire a lock, but this should be minimal
startTxVarFS :: IO UTCTime
startTxVarFS = getCurrentTime >>= \t -> addRunningTx t >> return t

-- appends a freshly created txlog for the inner tx
-- the starting time reference is preserved
--we create a new fsversion reference
startNestedTxVarFS :: TxVarFTM a -> TxVarFTM a
startNestedTxVarFS m = do
	(starttime,startversion,txlogs@(SCons txlog_parent _)) <- Reader.ask
	(fsversion,chgs,tmps) <- inL $ readRef txlog_parent
	txlog_child <- inL $ newRef (fsversion,chgs,tmps)
	Reader.local (Prelude.const (starttime,startversion,SCons txlog_child txlogs)) m

-- if an inner tx validation fails, then we throw an @InvalidTx@ exception to retry the whole atomic block
data InvalidTx = InvalidTx deriving (Typeable)
instance Show (InvalidTx) where
	show InvalidTx = "InvalidTx"
instance Exception InvalidTx
data BlockedOnRetry = BlockedOnRetry deriving (Show,Typeable)
instance Exception BlockedOnRetry

-- returns a bool stating whether the transaction was committed or needs to be incrementally repaired
-- no exceptions should be raised inside this block
validateAndCommitTopTxVarFS :: Bool -> TxVarFTM Bool
validateAndCommitTopTxVarFS doWrites = atomicTxVarFS $ do
	txenv@(timeref,startversion,txlogs@(SCons txlog SNil)) <- Reader.ask
	starttime <- inL $ readRef timeref
	success <- forestM $ validateTxsVarFS starttime txlogs
	if success
		then do
			forestM $ commitTopTxVarFS doWrites starttime txlog
			return True
		else do
			inL $ liftIO $ deleteRunningTx starttime
			return False

validateAndCommitNestedTxVarFS :: Maybe SomeException -> TxVarFTM ()
validateAndCommitNestedTxVarFS mbException = atomicTxVarFS $ do
	txenv@(timeref,startversion,txlogs@(SCons txlog1 (SCons txlog2 _))) <- Reader.ask
	starttime <- inL $ readRef timeref
	case mbException of
		Just e -> do -- throwing an exception exits the chain of txs one by one
			forestM $ commitNestedTxVarFS False txlog1 txlog2 -- does not perform @Write@s
		Nothing -> do
			-- validates the current and enclosing txs up the tx tree
			success <- forestM $ validateTxsVarFS starttime txlogs
			if success
				then do
					forestM $ commitNestedTxVarFS True txlog1 txlog2 -- performs @Write@s
				else do
					inL $ liftIO $ deleteRunningTx starttime
					throwM InvalidTx

-- validates a transaction and places it into the waiting queue for retrying
validateAndRetryTopTxVarFS :: TxVarFTM (Maybe Lock)
validateAndRetryTopTxVarFS = atomicTxVarFS $ do
	txenv@(timeref,startversion,txlogs@(SCons txlog SNil)) <- Reader.ask
	starttime <- inL $ readRef timeref
	-- validates the current and enclosing txs up the tx tree
	success <- forestM $ validateTxsVarFS starttime txlogs
	if success
		then do
			lck <- inL $ liftIO $ Lock.newAcquired -- sets the tx lock as acquired; the tx will be resumed when the lock is released
			forestM $ commitTopTxVarFS False starttime txlog
			forestM $ retryTxVarFSLog lck timeref txlog -- wait on changes to retry (only registers waits, does not actually wait)
			return $ Just lck
		else do
			inL $ liftIO $ deleteRunningTx starttime
			return Nothing

--registers waits for all the filepaths read by a txlog
--since we treat reads/writes separately, we don't need to wait on writes that have not been read
retryTxVarFSLog :: Lock -> IORef UTCTime -> TxVarFSLog -> ForestM TxVarFS ()
retryTxVarFSLog lck timeref txlog = do
	(fsversion,(reads,writes),tmps) <- forestIO $ readRef txlog
	let retryPath path = do
		-- the reference to the lock lives as long as the transaction
		modifyMVar_ waitingTxs $ \xs -> do
			case Map.lookup path xs of
				Nothing -> newQ >>= \q -> return $ Map.insert path q xs
				Just q -> pushL q lck >> return xs
	forestIO $ Foldable.mapM_ retryPath reads

-- validates a nested transaction and merges its log with its parent
-- note that retrying discards the tx's writes
validateAndRetryNestedTxVarFS :: TxVarFTM ()
validateAndRetryNestedTxVarFS = atomicTxVarFS $ do
	txenv@(timeref,startversion,txlogs@(SCons txlog1 (SCons txlog2 _))) <- Reader.ask
	starttime <- inL $ readRef timeref
	success <- forestM $ validateTxsVarFS starttime txlogs
	if success
		then do
			forestM $ commitNestedTxVarFS False txlog1 txlog2 -- does not perform @Write@s on @retry@
		else do
			inL $ liftIO $ deleteRunningTx starttime
			throwM InvalidTx

-- validates the current log before catching an exception
validateCatchTxVarFS :: TxVarFTM ()
validateCatchTxVarFS = atomicTxVarFS $ do
	txenv@(timeref,startversion,txlogs) <- Reader.ask
	starttime <- inL $ readRef timeref
	success <- forestM $ validateTxsVarFS starttime txlogs
	if success
		then do
			-- in case the computation raises an exception, discard all its visible (write) effects
			forestM $ unbufferTxVarFSWrites
		else do
			inL $ liftIO $ deleteRunningTx starttime
			throwM InvalidTx

unbufferTxVarFSWrites :: ForestM TxVarFS ()
unbufferTxVarFSWrites = do
	(starttime,startversion,txlogs) <- Reader.ask
	let unbufferTxVarFSLog txlog1 = do
		(fsversion1,(reads1,chgs1),tmps1) <- readRef txlog1
		writeRef txlog1 (startversion,(reads1,emptyFSTreeDelta),tmps1)
	forestIO $ Foldable.mapM_ unbufferTxVarFSLog txlogs

validateTxsVarFS :: UTCTime -> TxVarFSLogs -> ForestM TxVarFS Bool
validateTxsVarFS starttime txlogs = do
	-- gets the transactions that committed after the current transaction's start time
	finished <- liftM (Map.toAscList . Map.filterWithKey (\k v -> k > starttime)) $ forestIO $ readMVar doneTxs
	checkTxsVarFS txlogs finished

checkTxsVarFS :: TxVarFSLogs -> [(UTCTime,TxVarFSWrites)] -> ForestM TxVarFS Bool
checkTxsVarFS SNil finished = return True
checkTxsVarFS env@(SCons txlog txlogs) finished = do
	b1 <- checkTxVarFS txlog finished
	b2 <- checkTxsVarFS txlogs finished
	return $ b1 && b2

-- checks if the current txlog is consistent with a sequence of concurrent modifications
checkTxVarFS :: TxVarFSLog -> [(UTCTime,TxVarFSWrites)] -> ForestM TxVarFS Bool
checkTxVarFS txlog wrts = liftM List.and $ Prelude.mapM (checkTxVarFS' txlog) wrts where
	checkTxVarFS' txlog (txtime,paths) = do
		(starttime_ref,startversion,SCons txlog _) <- Reader.ask
		starttime <- forestIO $ readIORef starttime_ref
		forestIO $ putStrLn $ "checking " ++ show starttime ++ " against " ++ show txtime
		Foldable.foldrM (\path b -> liftM (b &&) $ checkTxVarFSWrite txlog path) True paths
	checkTxVarFSWrite txlog path = do
		-- we only check for write-read conflicts
		(reads,_) <- getTxVarFSChangesFlat
		return $ not $ path `Set.member` reads

commitTopTxVarFS :: Bool -> UTCTime -> TxVarFSLog -> ForestM TxVarFS ()
commitTopTxVarFS doWrites starttime txlog = do
	-- deletes this transaction from the running list and gets the earliest running tx 
	mbearliestTx <- forestIO $ modifyMVarMasked runningTxs (\xs -> return (List.delete starttime xs,lastMay xs))
	-- commits the log and gets a sequence of performed writes
	writes <- commitTxVarFSLog starttime doWrites txlog
	-- finishes the current tx and deletes txs that finished before the start of the earliest running tx
	-- we don't need to log transactions with empty commits (no @Eval@s or @Write@s)
	let addDone time m = if Set.null writes then m else Map.insert time writes m
	now <- case mbearliestTx of
		Just earliestTx -> forestIO $ modifyMVarMasked doneTxs (\m -> getCurrentTime >>= \now -> let m' = Map.filterWithKey (\t _ -> t > earliestTx) (addDone now m) in m' `seq` return (m',now))
		Nothing -> forestIO $ modifyMVarMasked doneTxs (\m -> getCurrentTime >>= \now -> let m' = addDone now m in m' `seq` return (m',now))
	-- wakes up the transactions after updating their buffered content
	wakeUpWaits writes

-- makes the parent log sensitive to the variables used in the nested branch
commitNestedTxVarFS :: Bool -> TxVarFSLog -> TxVarFSLog -> ForestM TxVarFS ()
commitNestedTxVarFS doWrites txlog_child txlog_parent = if doWrites
	then mergeTxVarFSLog txlog_child txlog_parent
	else extendTxVarFSLog txlog_child txlog_parent

-- merges a nested txlog with its parent txlog
mergeTxVarFSLog :: TxVarFSLog -> TxVarFSLog -> ForestM TxVarFS ()
mergeTxVarFSLog txlog1 txlog2 = forestIO $ do
	(fsversion,chgs,tmps) <- readRef txlog1
	writeRef txlog2 (fsversion,chgs,tmps)
	
-- does not commit writes
extendTxVarFSLog :: TxVarFSLog -> TxVarFSLog -> ForestM TxVarFS ()
extendTxVarFSLog txlog1 txlog2 = forestIO $ do
	(fsversion1,(reads1,writes1),tmps1) <- readRef txlog1
	(fsversion2,(reads2,writes2),tmps2) <- readRef txlog2
	writeRef txlog2 (fsversion2,(reads1,writes2),tmps1)

-- locks on which retrying transactions will wait
type WaitQueue = Deque Threadsafe Threadsafe SingleEnd SingleEnd Grow Safe Lock

-- a register of locks for retrying transactions
-- these paths should be canonical
{-# NOINLINE waitingTxs #-}
waitingTxs :: MVar (Map FilePath WaitQueue)
waitingTxs = unsafePerformIO $ newMVar Map.empty

-- wakes up the locks waiting on a given set of paths
wakeUpWaits :: Set FilePath -> ForestM TxVarFS ()
wakeUpWaits = Foldable.mapM_ wakeUpWait

wakeUpWait :: FilePath -> ForestM TxVarFS ()
wakeUpWait path = forestIO $ do
	let wakeQueue q = do		
		mb <- tryPopR q
		case mb of
			Just lck -> tryRelease lck >> wakeQueue q
			Nothing -> return ()
	mb <- modifyMVar waitingTxs (\xs -> return (Map.delete path xs,Map.lookup path xs))
	case mb of
		Nothing -> return ()
		Just q -> wakeQueue q

tryRelease :: Lock -> IO ()
tryRelease lck = do
	isLocked <- Lock.locked lck
	if isLocked then Lock.release lck else return ()

commitTxVarFSLog :: UTCTime -> Bool -> TxVarFSLog -> ForestM TxVarFS TxVarFSWrites
commitTxVarFSLog starttime doWrites txlog = do
	(fsversion,(reads,writes),tmps) <- forestIO $ readIORef txlog
	-- commits filesystem modifications
	wakes <- forestIO $ commitFSTreeDelta "" writes
	-- removes temporary files
	forestIO $ Foldable.mapM_ (\tmp -> runShellCommand_ $ "rm -rf " ++ tmp) tmps
	return wakes

globalLock :: Lock
globalLock = unsafePerformIO $ Lock.new

atomicTxVarFS :: TxVarFTM a -> TxVarFTM a
atomicTxVarFS m = do
	forestM $ forestIO $ print "entering atomic"
	x <- withLock globalLock m
	forestM $ forestIO $ print "left atomic"
	return x

-- acquiring the locks in sorted order is essential to avoid deadlocks!
withLock :: Lock -> TxVarFTM a -> TxVarFTM a
withLock = liftA2 Catch.bracket_ (forestM . forestIO . Lock.acquire) (forestM . forestIO . Lock.release)

---- acquiring the locks in sorted order is essential to avoid deadlocks!
--withFileLocksTxVarFS :: Set FilePath -> TxVarFTM a -> TxVarFTM a
--withFileLocksTxVarFS paths = Catch.bracket (inL $ Foldable.foldlM (\xs path -> liftM (:xs) $ FileLock.lock path WriteLock) [] paths) (inL . Foldable.mapM_ FileLock.unlock) . Prelude.const
--
--waitFileLock :: MonadIO m => FilePath -> m ()
--waitFileLock path = liftIO $ Control.Exception.mask_ $ FileLock.lock path WriteLock >>= FileLock.unlock

