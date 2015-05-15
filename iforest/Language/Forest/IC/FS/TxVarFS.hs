
{-# LANGUAGE TemplateHaskell, TypeOperators, ConstraintKinds, UndecidableInstances, TupleSections, FlexibleInstances, MultiParamTypeClasses, StandaloneDeriving, GeneralizedNewtypeDeriving, FlexibleContexts, DataKinds, TypeFamilies, Rank2Types, GADTs, ViewPatterns, DeriveDataTypeable, ScopedTypeVariables #-}

-- Regular filesystem with optimistic concurrency support for transactions, with mutable transactional variables structures mapped to specifications, and no incrementality

module Language.Forest.IC.FS.TxVarFS (
	Forest(..),Transactional(..),MonadThrow(..),MonadCatch(..)
	, proxyTxVarFS
	) where
	
import Control.Concurrent.Transactional
import Data.Global.TH as TH
import Control.Monad.Incremental.Display
import Control.Monad.RWS (RWS(..),RWST(..))
import qualified Control.Monad.RWS as RWS
import Control.Monad.Writer (Writer(..),WriterT(..))
import qualified Control.Monad.Writer as Writer
import Language.Forest.IC.Default
import Control.Monad.Catch
import Control.Concurrent
import System.Cmd
import System.Mem.Weak.Exts as Weak
import Control.Exception as Exception
import qualified Data.Strict.List as Strict
import Unsafe.Coerce
import Language.Forest.IC.ValueDelta
import Language.Forest.IC.BX as BX
import Language.Forest.FS.FileLock

--import System.Posix.FileLock as FileLock
import Data.Foldable as Foldable
import Data.Concurrent.Deque.Class as Queue
import Data.Concurrent.Deque.Reference.DequeInstance
import qualified Control.Concurrent.Map.Exts as CMap
import qualified Control.Concurrent.Weak.Map as CWeakMap
import System.Posix.Files

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
import Language.Forest.FS.FSDelta
import Control.Monad.Trans
import qualified Control.Monad.State as State
import Language.Forest.Manifest
import Data.Time.Clock
import Data.Unique
import Safe
import Control.Monad.Catch as Catch
import System.Mem.Concurrent.WeakMap as WeakMap

import qualified Control.Concurrent.STM as STM

type instance IncK (IncForest TxVarFS) a = (Typeable a,Eq a)

proxyTxVarFS = Proxy :: Proxy TxVarFS

-- a list of the starting times of running transactions sorted from newest to oldest (we may two txs with the same starting time)
TH.declareMVar "runningTxs"  [t| [UTCTime] |] [e| [] |]

-- insert a new time in a list sorted from newest to oldest
addRunningTx time = modifyMVarMasked_ runningTxs (\xs -> return $ List.insertBy (\x y -> compare y x) time xs)
-- remove a time in a list sorted from newest to oldest
deleteRunningTx time = modifyMVarMasked_ runningTxs (\xs -> return $ List.delete time xs)

-- ** Filesystem

-- current FS version
-- we remember a set of read files
-- we use an absolute tree delta (starting at the filesystem's root) to keep track of the filesystem modifications performed by the transaction, to be committed at the end
-- we keep a set of temporary files/directories used  by the transaction, that are purged after commit
type TxVarFSLog = IORef (FSVersion,TxVarFSChanges,Set FilePath)

-- nested logs for nested transactions
type TxVarFSLogs = Strict.List TxVarFSLog

-- (starttime,start fsversion,nested logs)
type TxVarFSEnv = (IORef UTCTime,FSVersion,TxVarFSLogs)

type TxVarFSReads = Set FilePath
type TxVarFSWrites = Set FilePath
-- changes as a set of reads and a modification tree
type TxVarFSChanges = (TxVarFSReads,FSTreeDelta)
type TxVarFSChangesFlat = (TxVarFSReads,TxVarFSWrites)

-- the FSVersion is initialized differently for each top-level tx and modified on stores. initializing a nested tx does not modify the FSVersion
type FSVersion = Unique

-- a map with commit times of committed transactions and their performed changes
-- we use a @FSTreeDelta@ because the set of written paths may be infinite (like all paths under a given directory)
TH.declareMVar "doneTxs"  [t| (Map UTCTime TxVarFSWrites) |] [e| Map.empty |]

-- locks on which retrying transactions will wait
type WaitQueue = Deque Threadsafe Threadsafe SingleEnd SingleEnd Grow Safe Lock

-- a register of locks for retrying transactions
-- these paths should be canonical
declareMVar "waitingTxs"  [t| (Map FilePath WaitQueue) |] [e| Map.empty |]

-- only needs to read from the top-level log
getTxVarFSChangesFlat :: ForestM TxVarFS TxVarFSChangesFlat
getTxVarFSChangesFlat = do
	(starttime,startversion,Strict.Cons txlog _) <- TxVarFSForestM Reader.ask
	(fsversion,(reads,writes),tmps) <- forestIO $ readRef txlog
	return (reads,fsTreeDeltaWrites "" writes)

getFSVersionTxVarFS :: ForestM TxVarFS FSVersion
getFSVersionTxVarFS = do
	(starttime,startversion,Strict.Cons txlog _) <- TxVarFSForestM Reader.ask
	(fsversion,(reads,writes),tmps) <- forestIO $ readRef txlog
	return fsversion

incrementFSVersionTxVarFS :: ForestM TxVarFS ()
incrementFSVersionTxVarFS = do
	fsversion' <- forestIO newUnique
	setFSVersionTxVarFS fsversion'

setFSVersionTxVarFS :: FSVersion -> ForestM TxVarFS ()
setFSVersionTxVarFS new_fsversion = do
	(starttime,startversion,Strict.Cons txlog _) <- TxVarFSForestM Reader.ask
	(fsversion,(reads,writes),tmps) <- forestIO $ readRef txlog
	forestIO $ writeRef txlog (new_fsversion,(reads,writes),tmps)

-- only needs to read from the top-level log
getTxVarFSChanges :: ForestM TxVarFS TxVarFSChanges
getTxVarFSChanges = do
	(starttime,startversion,Strict.Cons txlog _) <- TxVarFSForestM Reader.ask
	(fsversion,(reads,writes),tmps) <- forestIO $ readRef txlog
	return (reads,writes)

putTxVarFSRead :: FilePath -> ForestM TxVarFS ()
putTxVarFSRead path = do
	(starttime,startversion,Strict.Cons txlog _) <- TxVarFSForestM Reader.ask
	(fsversion,(reads,writes),tmps) <- forestIO $ readRef txlog
	forestIO $ writeRef txlog (fsversion,(Set.insert path reads,writes),tmps)

putTxVarFSTmp :: FilePath -> ForestM TxVarFS ()
putTxVarFSTmp tmp = do
	(starttime,startversion,Strict.Cons txlog _) <- TxVarFSForestM Reader.ask
	(fsversion,(reads,writes),tmps) <- forestIO $ readRef txlog
	forestIO $ writeRef txlog (fsversion,(reads,writes),Set.insert tmp tmps)

modifyTxVarFSTreeDeltas :: (FSTreeDelta -> FSTreeDelta) -> ForestM TxVarFS ()
modifyTxVarFSTreeDeltas f = do
	(starttime,startversion,Strict.Cons txlog _) <- TxVarFSForestM Reader.ask
	(fsversion,(reads,writes),tmps) <- forestIO $ readRef txlog
	forestIO $ writeRef txlog (fsversion,(reads,f writes),tmps)

instance Eq (FSTree TxVarFS) where
	t1 == t2 = False

instance FSRep TxVarFS where
	
	newtype ForestM TxVarFS a = TxVarFSForestM { runTxVarFSForestM :: ReaderT TxVarFSEnv IO a } deriving (Functor,Applicative,Monad)
	
	data ForestCfg TxVarFS = TxVarFSForestCfg
	
	runForest _ m = error "please use atomically instead"
	
	forestIO = TxVarFSForestM . lift
	
	-- | returns the forest directory used to store auxiliary data
	getForestDirectory = liftM (</> "Forest") $ forestIO getTemporaryDirectory
	
	data FSTree TxVarFS = TxVarFSTree | VirtualTxVarFSTree deriving Show
	
	showFSTree = return . show
	
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
		
	doesDirectoryExistInTree path _ = doesExistTxVarFS DirExists path
	doesFileExistInTree path _ = doesExistTxVarFS FileExists path
	doesExistInTree path _ = doesExistTxVarFS AnyExists path
	
	canonalizePathInTree path _ = canonalizePathTxVarFS path

getDirectoryContentsTxVarFS :: FilePath -> ForestM TxVarFS [FileName]
getDirectoryContentsTxVarFS path = do
	canpath <- canonalizePathInTree path TxVarFSTree
	putTxVarFSRead canpath
	(_,td) <- getTxVarFSChanges
	let path_td = focusFSTreeDeltaByRelativePathMay td canpath
	xs <- forestIO $ getContentsFSTreeDeltaNodeMay canpath path_td
	return xs

doesExistTxVarFS :: ExistFlag -> FilePath -> ForestM TxVarFS Bool
doesExistTxVarFS flag path = do
	canpath <- canonalizeDirectoryInTree path TxVarFSTree
	putTxVarFSRead canpath
	(_,td) <- getTxVarFSChanges
	let path_td = focusFSTreeDeltaByRelativePathMay td canpath
	case path_td of
		Just (FSTreeNop _) -> return $ flag /= FileExists
		Just (FSTreeChg _ _) -> return $ flag /= FileExists
		Just (FSTreeNew _ _ diskpath _) -> forestIO $ doesExistShellFlag flag diskpath
		otherwise -> forestIO $ doesExistShellFlag flag canpath

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

class TxVarFSLayerImpl l where
	unTxVarFSLayer :: Proxy l -> l (IncForest TxVarFS) a -> ReaderT TxVarFSEnv IO a
	txVarFSLayer :: Proxy l -> ReaderT TxVarFSEnv IO a -> l (IncForest TxVarFS) a

instance TxVarFSLayerImpl Outside where
	unTxVarFSLayer _ = runTxVarFSForestO
	txVarFSLayer _ = TxVarFSForestO

instance TxVarFSLayerImpl Inside where
	unTxVarFSLayer _ = runTxVarFSForestI
	txVarFSLayer _ = TxVarFSForestI

instance Incremental (IncForest TxVarFS) where
	
	newtype Outside (IncForest TxVarFS) a = TxVarFSForestO { runTxVarFSForestO :: ReaderT TxVarFSEnv IO a } deriving (Monad,Functor,Applicative)
	newtype Inside (IncForest TxVarFS) a = TxVarFSForestI { runTxVarFSForestI :: ReaderT TxVarFSEnv IO a } deriving (Monad,Functor,Applicative)

	world = TxVarFSForestO . runTxVarFSForestI
	unsafeWorld = TxVarFSForestI . runTxVarFSForestO

	runIncremental = error "please use atomically instead"
	
	unsafeIOToInc = inside . TxVarFSForestI . lift
	
instance ICRep TxVarFS where

	forestM = inside . TxVarFSForestI . runTxVarFSForestM
	forestO = TxVarFSForestM . runTxVarFSForestO

	-- stores a computation and a concurrent map from @FSVersion@s to computed values
	newtype FSThunk TxVarFS l inc a = TxVarFSThunk (IORef (Dynamic,FilePath),l inc a,WeakMap FSVersion a)

	-- a rough estimate
	isUnevaluatedFSThunk (TxVarFSThunk (args,m,entries)) = liftM Map.null $ forestM $ forestIO $ WeakMap.toMap entries

	newtype HSThunk TxVarFS l inc a = TxVarHSThunk (IORef (Dynamic,FilePath),l inc a,WeakMap FSVersion a)
	
	newtype ICThunk TxVarFS l inc a = TxVarICThunk (l inc a)

instance ZippedICMemo TxVarFS where

	addZippedMemo path args rep _ = forestM $ forestIO $ do
		let (TxVarFSThunk (dyn,_,_)) = to (iso_rep_thunk proxyTxVarFS) rep
		putStrLn $ "adding args " ++ show (typeOf rep) ++ " " ++ show (typeOf args)
		writeIORef dyn (toDyn args,path)
		
	findZippedMemo path rep = return Nothing

argsTxVarFS :: (ForestLayer TxVarFS l,FTK TxVarFS rep) => Proxy (ForestArgs rep) -> rep -> ForestL TxVarFS l (ForestVs (ForestArgs rep),FilePath)
argsTxVarFS proxy rep = do
	mb <- forestM $ getFTVArgs proxy rep
	case mb of
		Nothing -> error "should not happen"
		Just (margs,path) -> liftM (,path) $ inside $ vArgs Proxy proxy margs

getFTVArgs :: (FTK TxVarFS rep) => Proxy (ForestArgs rep) -> rep -> ForestM TxVarFS (Maybe (ForestIs TxVarFS (ForestArgs rep),FilePath))
getFTVArgs (proxy ::Proxy args) rep = forestIO $ do
	let (TxVarFSThunk (rdyn,_,_)) = to (iso_rep_thunk proxyTxVarFS) rep
	(dyn,path) <- readIORef rdyn
	case fromDynamic dyn of
		Nothing -> return Nothing --error $ "should not happen " ++ show (typeOf rep) ++ " " ++ show (typeOf (undefined::args)) ++ " " ++  show (dynTypeRep dyn)
		Just args -> return $ Just (args,path)
	

instance ForestLayer TxVarFS l => Thunk (HSThunk TxVarFS) l (IncForest TxVarFS) where
	new m = do
		rdyn <- forestM $ forestIO $ newIORef (toDyn (),"--NOFILE--")
		tbl <- forestM $ forestIO $ WeakMap.new
		return $ TxVarHSThunk (rdyn,m,tbl)
	read (TxVarHSThunk (rdyn,m,tbl)) = do
		fsversion <- forestM getFSVersionTxVarFS
		mb <- forestM $ forestIO $ WeakMap.lookup tbl fsversion
		case mb of
			Nothing -> do
				v <- m
				forestM $ forestIO $ WeakMap.insertWithMkWeak tbl (MkWeak $ Weak.mkWeakRefKey rdyn) fsversion v
				return v
			Just v -> return v

instance ForestLayer TxVarFS l => Thunk (ICThunk TxVarFS) l (IncForest TxVarFS) where
	new m = return $ TxVarICThunk m
	read (TxVarICThunk m) = m
instance ForestLayer TxVarFS l => Output (ICThunk TxVarFS) l (IncForest TxVarFS) where
	thunk = Inc.new
	force = Inc.read

instance (ForestLayer TxVarFS l) => Thunk (FSThunk TxVarFS) l (IncForest TxVarFS) where
	new m = do
		rdyn <- forestM $ forestIO $ newIORef (toDyn (),"--NOFILE--")
		tbl <- forestM $ forestIO $ WeakMap.new
		return $ TxVarFSThunk (rdyn,m,tbl)
	read (TxVarFSThunk (rdyn,m,tbl)) = do -- always reads from the latest file system version
		fsversion <- forestM getFSVersionTxVarFS
		mb <- forestM $ forestIO $ WeakMap.lookup tbl fsversion
		case mb of
			Nothing -> do
				v <- m
				forestM $ forestIO $ WeakMap.insertWithMkWeak tbl (MkWeak $ Weak.mkWeakRefKey rdyn) fsversion v
				return v
			Just v -> return v

instance (ForestLayer TxVarFS l) => Input (FSThunk TxVarFS) l (IncForest TxVarFS) where
	ref c = Inc.new (return c)
	mod = Inc.new
	get = Inc.read
	set (TxVarFSThunk (rdyn,m,tbl)) v = do
		fsversion <- forestM getFSVersionTxVarFS
		forestM $ forestIO $ WeakMap.insertWithMkWeak tbl (MkWeak $ Weak.mkWeakRefKey rdyn) fsversion v

-- ** Transactions

-- the Forest transactional monad
type TxVarFTM = FTM TxVarFS
-- a Forest transactional variable
type TxVarFTV a = FTV TxVarFS a

instance Transactional (IncForest TxVarFS) where
	atomically = atomicallyTxVarFS
	retry = retryTxVarFS
	orElse = orElseTxVarFS

instance MonadThrow (Outside (IncForest TxVarFS)) where
	throwM = throwTxVarFS 
instance MonadCatch (Outside (IncForest TxVarFS)) where
	catch = catchTxVarFS False	
	
instance Forest TxVarFS where
	new = newTxVarFS Proxy
	args = argsTxVarFS Proxy
	read = readTxVarFS Proxy
	writeOrElse = writeOrElseTxVarFS
	delete = deleteTxVarFS
	copyOrElse = copyOrElseTxVarFS Proxy

deleteTxVarFS :: (FTK TxVarFS rep) => rep -> TxVarFTM ()
deleteTxVarFS (rep :: rep) = do
	let proxyRep = Proxy :: Proxy rep
	let proxy = Proxy :: Proxy (ForestArgs rep)
	mb <- forestM $ getFTVArgs proxy rep
	case mb of
		Nothing -> error "tried to write to a variable that is not connected to the FS"
		Just (args,path) -> do
			def_rep :: rep <- inside $ zdefaultScratchMemo proxyRep proxyTxVarFS args path
			var <- Inc.getOutside $ to (iso_rep_thunk proxyTxVarFS) def_rep
--			str <- showInc def_rep
--			forestM $ forestIO $ print str
			writeOrElseTxVarFS' rep var () (error "failed to write")

copyOrElseTxVarFS :: (FTK TxVarFS rep) => Proxy (ForestArgs rep) -> rep -> rep -> b -> ([ManifestError] -> TxVarFTM b) -> TxVarFTM b
copyOrElseTxVarFS proxy src tgt b f = do
	mb_src <- forestM $ getFTVArgs proxy src
	mb_tgt <- forestM $ getFTVArgs proxy tgt
	case (mb_src,mb_tgt) of
		(Just (args_src,path_src),Just (args_tgt,path_tgt)) -> do
			-- XXX: this may fail if FileInfo paths are canonized...
			let chgPath path = path_tgt </> (makeRelative path_src path)
			tgt' <- copyFSThunks proxyTxVarFS proxyOutside chgPath src
			content' <- inside $ BX.getM lens_content $ Inc.get $ to (iso_rep_thunk proxyTxVarFS) tgt'
			writeOrElseTxVarFS tgt content' b f
		otherwise -> error "tried to write to a variable that is not connected to the FS"

newTxVarFS :: FTK TxVarFS rep => Proxy (ForestArgs rep) -> ForestVs (ForestArgs rep) -> FilePath -> TxVarFTM rep
newTxVarFS proxy args path = inside $ zload (vmonadArgs proxyTxVarFS proxy args) path

-- make sure that the computation that is run is a load function (because of default loading)
readTxVarFS :: (ForestLayer TxVarFS l,FTK TxVarFS rep) => Proxy (ForestArgs rep) -> rep -> ForestL TxVarFS l (ForestContentTy TxVarFS (ForestRepTy TxVarFS rep))
readTxVarFS proxy (rep :: rep) = do
	(txargs,path) <- inside $ argsTxVarFS proxy rep
	let (TxVarFSThunk (rdyn,m,tbl)) = to (iso_rep_thunk proxyTxVarFS) rep
	fsversion <- forestM getFSVersionTxVarFS
	mb <- forestM $ forestIO $ WeakMap.lookup tbl fsversion
	case mb of
		Nothing -> do
			(rep' :: rep) <- inside $ zload (vmonadArgs proxyTxVarFS proxy txargs) path
			v' <- inside $ Inc.get (to (iso_rep_thunk proxyTxVarFS) rep')
			forestM $ forestIO $ WeakMap.insertWithMkWeak tbl (MkWeak $ Weak.mkWeakRefKey rdyn) fsversion v'
			inside $ BX.getM lens_content (return v')
		Just v -> inside $ BX.getM lens_content (return v)

writeOrElseTxVarFS :: (FTK TxVarFS rep) => rep -> ForestContentTy TxVarFS (ForestRepTy TxVarFS rep) -> b -> ([ManifestError] -> TxVarFTM b) -> TxVarFTM b
writeOrElseTxVarFS rep content b f = do
	t <- inside $ BX.putM lens_content (Inc.get $ to (iso_rep_thunk proxyTxVarFS) rep) (return content)
	writeOrElseTxVarFS' rep t b f

-- does not change the inner computation; just sets the cached fsversion forward
writeOrElseTxVarFS' :: (FTK TxVarFS rep) => rep -> ForestRepTy TxVarFS rep -> b -> ([ManifestError] -> TxVarFTM b) -> TxVarFTM b
writeOrElseTxVarFS' (rep::rep) var b f = do
	let t = to (iso_rep_thunk proxyTxVarFS) rep
	(starttime,old_fsversion,Strict.Cons fslog_ref _) <- TxVarFSForestO Reader.ask
	old_fslog <- forestM $ forestIO $ readIORef fslog_ref
	forestM incrementFSVersionTxVarFS
	set t var
	
	let rollback errors = do -- rollback the modifications
		forestM $ forestIO $ writeIORef fslog_ref old_fslog
		forestM $ setFSVersionTxVarFS old_fsversion
		f errors
	
	mb :: (Maybe (ForestIs TxVarFS (ForestArgs rep),FilePath)) <- forestM $ getFTVArgs Proxy rep
	case mb of
		Nothing -> rollback [ConflictingArguments] -- the top-level arguments of a variable don't match the spec
		Just (args,path) -> do
--			str <- showInc rep
--			forestM $ forestIO $ putStrLn $ "mani  " ++ show str
			(mani,_,memos) <- RWS.runRWST (zmanifest args path rep) True ()
			-- we need to store the changes to the (buffered) FS before validating
			forestM $ storeManifest mani
			
			forestM $ forestIO $ putStrLn "Manifest!"
			forestM $ forestIO $ print mani
			errors <- forestM $ manifestErrors mani
			if List.null errors
				then forestM latestTree >>= memos >> return b
				else rollback errors
					

atomicallyTxVarFS :: TxVarFTM b -> IO b
atomicallyTxVarFS stm = initializeTxVarFS try where
	try = txVarFSLayer proxyOutside $ flip Catch.catches [Catch.Handler catchInvalid,Catch.Handler catchRetry,Catch.Handler catchSome] $ unTxVarFSLayer proxyOutside $ do
		-- run the tx
		x <- stm
		-- tries to commit the current tx, otherwise repairs it incrementally
		success <- validateAndCommitTopTxVarFS True
		if success
			then return x
			else debug "throw InvalidTx" $ txVarFSLayer proxyOutside $ throwM InvalidTx
	catchInvalid InvalidTx = unTxVarFSLayer proxyOutside $ debug "InvalidTx" $ do
		resetTxVarFS try
	catchRetry BlockedOnRetry = unTxVarFSLayer proxyOutside $ debug "BlockedOnRetry" $ do
		-- if the retry was invoked on an inconsistent state, we incrementally repair and run again, otherwise we place the tx in the waiting queue
		mbsuccess <- validateAndRetryTopTxVarFS
		case mbsuccess of
			Just lck -> do -- retried txs are always in a consistent state, because we apply all affecting updates before releasing the lock
				-- wait for the lock to be released (whenever some variables that it depends on are changed)
				-- we don't consume the contents of the mvar to avoid further puts to succeeed; a new MVar is created for each retry
				unsafeIOToInc $ Lock.acquire lck
				resetTxVarFS try
			Nothing -> resetTxVarFS try
	catchSome (e::SomeException) = unTxVarFSLayer proxyOutside $ debug ("SomeException "++show e) $ do
		-- we still need to validate on exceptions, otherwise repair incrementally; transaction-local allocations still get committed
		success <- validateAndCommitTopTxVarFS False
		if success
			then txVarFSLayer proxyOutside $ throwM e
			else do
				resetTxVarFS try


retryTxVarFS :: TxVarFTM a
retryTxVarFS = unsafeIOToInc $ throwIO BlockedOnRetry

orElseTxVarFS :: TxVarFTM a -> TxVarFTM a -> TxVarFTM a
orElseTxVarFS stm1 stm2 = do1 where
	try1 = do { x <- stm1; validateAndCommitNestedTxVarFS Nothing; return x }
	try2 = do { x <- stm2; validateAndCommitNestedTxVarFS Nothing; return x }
	do1 = startNestedTxVarFS $ txVarFSLayer proxyOutside $ (unTxVarFSLayer proxyOutside try1) `Catch.catches` [Catch.Handler catchRetry1,Catch.Handler catchInvalid,Catch.Handler catchSome]
	do2 = dropChildTxLog $ startNestedTxVarFS $ txVarFSLayer proxyOutside $ (unTxVarFSLayer proxyOutside try2) `Catch.catches` [Catch.Handler catchRetry2,Catch.Handler catchInvalid,Catch.Handler catchSome]
	catchRetry1 BlockedOnRetry = unTxVarFSLayer proxyOutside $ validateAndRetryNestedTxVarFS >> do2
	catchRetry2 BlockedOnRetry = unTxVarFSLayer proxyOutside validateAndRetryNestedTxVarFS >> throwM BlockedOnRetry
	catchInvalid (e::InvalidTx) = throwM e
	catchSome (e::SomeException) = unTxVarFSLayer proxyOutside (validateAndCommitNestedTxVarFS (Just e)) >> throwM e

throwTxVarFS :: Exception e => e -> TxVarFTM a
throwTxVarFS = txVarFSLayer proxyOutside . Catch.throwM

catchTxVarFS :: Exception e => Bool -> TxVarFTM a -> (e -> TxVarFTM a) -> TxVarFTM a
catchTxVarFS doWrites stm (h :: e -> TxVarFTM a) = txVarFSLayer proxyOutside ((unTxVarFSLayer proxyOutside stm) `Catch.catches` [Catch.Handler catchInvalid,Catch.Handler catchRetry,Catch.Handler catchSome]) where
	catchInvalid (e::InvalidTx) = throwM e
	catchRetry (e::BlockedOnRetry) = throwM e
	catchSome (e::e) = unTxVarFSLayer proxyOutside $ do
		validateCatchTxVarFS doWrites
		h e

initializeTxVarFS :: TxVarFTM b -> IO b 
initializeTxVarFS (TxVarFSForestO m) = do
	starttime <- startTxVarFS >>= newIORef
	mountAVFS -- should succeed even if AVFS is already mounted
	fsversion <- newUnique
	txlog <- newIORef (fsversion,(Set.empty,emptyFSTreeDelta),Set.empty)
	debug ("initializeTxVarFS") $ Reader.runReaderT m (starttime,fsversion,Strict.Cons txlog Strict.Nil)
	-- don't unmount AVFS, since multiple parallel txs may be using it

-- resets
resetTxVarFS :: TxVarFTM a -> TxVarFTM a
resetTxVarFS m = do
	now <- unsafeIOToInc $ startTxVarFS >>= newIORef
	fsversion <- unsafeIOToInc $ newUnique
	txlog <- unsafeIOToInc $ newIORef (fsversion,(Set.empty,emptyFSTreeDelta),Set.empty)
	debug ("resetTxVarFS") $ TxVarFSForestO $ Reader.local (\_ -> (now , fsversion,Strict.Cons txlog Strict.Nil)) $ runTxVarFSForestO m

-- we need to acquire a lock, but this should be minimal
startTxVarFS :: IO UTCTime
startTxVarFS = getCurrentTime >>= \t -> addRunningTx t >> return t

-- appends a freshly created txlog for the inner tx
-- the starting time reference is preserved
--we create a new fsversion reference
startNestedTxVarFS :: TxVarFTM a -> TxVarFTM a
startNestedTxVarFS m = do
	(starttime,startversion,txlogs@(Strict.Cons txlog_parent _)) <- TxVarFSForestO Reader.ask
	(fsversion,chgs,tmps) <- unsafeIOToInc $ readIORef txlog_parent
	txlog_child <- unsafeIOToInc $ newIORef (fsversion,chgs,tmps) -- copy the parent's status; we will append modifications to it
	debug ("startNestedTxVarFS ") $ TxVarFSForestO $ Reader.local (Prelude.const (starttime,startversion,Strict.Cons txlog_child txlogs)) $ runTxVarFSForestO m

dropChildTxLog :: TxVarFTM a -> TxVarFTM a
dropChildTxLog m = TxVarFSForestO $ Reader.local (\(starttime,startversion,Strict.Cons _ txlogs) -> (starttime,startversion,txlogs)) $ runTxVarFSForestO m

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
validateAndCommitTopTxVarFS doWrites = atomicTxVarFS "validateAndCommitTopTxVarFS" $ do
	txenv@(timeref,startversion,txlogs@(Strict.Cons txlog Strict.Nil)) <- TxVarFSForestO Reader.ask
	starttime <- unsafeIOToInc $ readIORef timeref
	success <- forestM $ validateTxsVarFS starttime txlogs
	if success
		then do
			forestM $ commitTopTxVarFS doWrites starttime txlog
			return True
		else do
			unsafeIOToInc $ deleteRunningTx starttime
			return False

validateAndCommitNestedTxVarFS :: Maybe SomeException -> TxVarFTM ()
validateAndCommitNestedTxVarFS mbException = do
	txenv@(timeref,startversion,txlogs@(Strict.Cons txlog1 (Strict.Cons txlog2 _))) <- TxVarFSForestO Reader.ask
	starttime <- unsafeIOToInc $ readIORef timeref
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
					unsafeIOToInc $ deleteRunningTx starttime
					txVarFSLayer proxyOutside $ throwM InvalidTx

-- validates a transaction and places it into the waiting queue for retrying
validateAndRetryTopTxVarFS :: TxVarFTM (Maybe Lock)
validateAndRetryTopTxVarFS = atomicTxVarFS "validateAndRetryTopTxVarFS" $ do
	txenv@(timeref,startversion,txlogs@(Strict.Cons txlog Strict.Nil)) <- TxVarFSForestO Reader.ask
	starttime <- unsafeIOToInc $ readIORef timeref
	-- validates the current and enclosing txs up the tx tree
	success <- forestM $ validateTxsVarFS starttime txlogs
	if success
		then do
			lck <- unsafeIOToInc $ Lock.newAcquired -- sets the tx lock as acquired; the tx will be resumed when the lock is released
			forestM $ commitTopTxVarFS False starttime txlog
			forestM $ retryTxVarFSLog lck timeref txlog -- wait on changes to retry (only registers waits, does not actually wait)
			return $ Just lck
		else do
			unsafeIOToInc $ deleteRunningTx starttime
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
validateAndRetryNestedTxVarFS = do
	txenv@(timeref,startversion,txlogs@(Strict.Cons txlog1 (Strict.Cons txlog2 _))) <- TxVarFSForestO Reader.ask
	starttime <- unsafeIOToInc $ readIORef timeref
	success <- forestM $ validateTxsVarFS starttime txlogs
	if success
		then do
			forestM $ commitNestedTxVarFS False txlog1 txlog2 -- does not perform @Write@s on @retry@
		else do
			unsafeIOToInc $ deleteRunningTx starttime
			txVarFSLayer proxyOutside $ throwM InvalidTx

-- validates the current log before catching an exception
validateCatchTxVarFS :: Bool -> TxVarFTM ()
validateCatchTxVarFS doWrites = do
	txenv@(timeref,startversion,txlogs) <- TxVarFSForestO Reader.ask
	starttime <- unsafeIOToInc $ readIORef timeref
	success <- forestM $ validateTxsVarFS starttime txlogs
	if success
		then do
			-- in case the computation raises an exception, discard all its visible (write) effects
			unless doWrites $ forestM $ unbufferTxVarFSWrites
		else do
			unsafeIOToInc $ deleteRunningTx starttime
			txVarFSLayer proxyOutside $ throwM InvalidTx

-- unbuffers only the top-level writes
unbufferTxVarFSWrites :: ForestM TxVarFS ()
unbufferTxVarFSWrites = do
	(starttime,startversion,Strict.Cons txlog _) <- TxVarFSForestM Reader.ask
	let unbufferTxVarFSLog txlog1 = do
		(fsversion1,(reads1,chgs1),tmps1) <- readRef txlog1
		writeRef txlog1 (startversion,(reads1,emptyFSTreeDelta),tmps1)
	forestIO $ unbufferTxVarFSLog txlog

validateTxsVarFS :: UTCTime -> TxVarFSLogs -> ForestM TxVarFS Bool
validateTxsVarFS starttime txlogs = do
	-- gets the transactions that committed after the current transaction's start time
	finished <- liftM (Map.toAscList . Map.filterWithKey (\k v -> k > starttime)) $ forestIO $ readMVar doneTxs
	checkTxsVarFS txlogs finished

checkTxsVarFS :: TxVarFSLogs -> [(UTCTime,TxVarFSWrites)] -> ForestM TxVarFS Bool
checkTxsVarFS Strict.Nil finished = return True
checkTxsVarFS env@(Strict.Cons txlog txlogs) finished = do
	b1 <- checkTxVarFS txlog finished
	b2 <- checkTxsVarFS txlogs finished
	return $ b1 && b2

-- checks if the current txlog is consistent with a sequence of concurrent modifications
checkTxVarFS :: TxVarFSLog -> [(UTCTime,TxVarFSWrites)] -> ForestM TxVarFS Bool
checkTxVarFS txlog wrts = liftM List.and $ Prelude.mapM (checkTxVarFS' txlog) wrts where
	checkTxVarFS' txlog (txtime,paths) = do
		(starttime_ref,startversion,Strict.Cons txlog _) <- TxVarFSForestM Reader.ask
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
	(fsversion1,chgs1,tmps1) <- readRef txlog1
	(fsversion2,(reads2,writes2),tmps2) <- readRef txlog2
	writeRef txlog2 (fsversion1,chgs1,tmps1)
	
-- does not commit writes
extendTxVarFSLog :: TxVarFSLog -> TxVarFSLog -> ForestM TxVarFS ()
extendTxVarFSLog txlog1 txlog2 = forestIO $ do
	(fsversion1,(reads1,writes1),tmps1) <- readRef txlog1
	(fsversion2,(reads2,writes2),tmps2) <- readRef txlog2
	writeRef txlog2 (fsversion1,(reads1,writes2),tmps1)

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
	wakes <- if doWrites
		then forestIO $ commitFSTreeDelta "" writes
		else return Set.empty
	-- removes temporary files
	forestIO $ Foldable.mapM_ (\tmp -> runShellCommand_ $ "rm -rf " ++ tmp) tmps
	return wakes

atomicTxVarFS :: String -> TxVarFTM a -> TxVarFTM a
atomicTxVarFS msg m = do
	-- get the changes of the innermost txlog
	(reads,writes) <- forestM getTxVarFSChangesFlat
	-- wait on currently acquired read locks (to ensure that concurrent writes are seen by this tx's validation step)
	forestM $ forestIO $ print $ "entering atomic " ++ msg
	x <- txVarFSLayer proxyOutside $ withFileLocks reads writes $ unTxVarFSLayer proxyOutside $ forestM (forestIO $ print $ "entered atomic " ++ msg) >> m
	forestM $ forestIO $ print $ "left atomic " ++ msg
	return x

debugChanges :: String -> TxVarFTM a -> TxVarFTM a
debugChanges str m = do
	r <- m
	(starttime,startversion,Strict.Cons txlog _) <- txVarFSLayer Proxy $ Reader.ask
	(fsversion,(reads,writes),tmps) <- forestM $ forestIO $ readRef txlog
	forestM $ forestIO $ putStrLn $ show fsversion ++ " changes!! "++ str ++ " "++ show reads ++ "\n" ++ show writes
	return r