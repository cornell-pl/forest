
{-# LANGUAGE TemplateHaskell, OverlappingInstances, TypeOperators, ConstraintKinds, UndecidableInstances, TupleSections, FlexibleInstances, MultiParamTypeClasses, StandaloneDeriving, GeneralizedNewtypeDeriving, FlexibleContexts, DataKinds, TypeFamilies, Rank2Types, GADTs, ViewPatterns, DeriveDataTypeable, ScopedTypeVariables #-}

-- Regular filesystem with optimistic concurrency support for transactions, with mutable transactional variables structures mapped to specifications, and incremental reuse within transactions

module Language.Forest.IC.FS.TxICFS (
	TransactionalForest(..),Transactional(..),MonadThrow(..),MonadCatch(..)
	,FSRep(..),ForestCfg(..),atomicallyTxICFS
) where

import Language.Forest.Errors
--import Language.Forest.Pure.MetaData (FileInfo)
import qualified Control.Concurrent.STM as STM
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
import Data.Strict.List
import Unsafe.Coerce
import Language.Forest.IC.ValueDelta
import Language.Forest.IC.BX as BX
import Control.Concurrent.Transactional

import System.Mem.MemoTable (MemoTable(..))
import qualified System.Mem.MemoTable as MemoTable
import Data.Foldable as Foldable
import Data.Concurrent.Deque.Class as Queue
import Data.Concurrent.Deque.Reference.DequeInstance
import qualified Control.Concurrent.Map.Exts as CMap
import qualified Control.Concurrent.Weak.Map as CWeakMap
import System.Posix.Files
import Language.Forest.FS.Diff

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
import Data.DList as DList
import Data.Global.TH as TH
import Data.Strict.Tuple as Strict

-- locks on which retrying transactions will wait
type WaitQueue = Deque Threadsafe Threadsafe SingleEnd SingleEnd Grow Safe Lock

-- a register of locks for retrying transactions
-- these paths should be canonical
TH.declareMVar "waitingTxsIC"  [t| (Map FilePath WaitQueue) |] [e| Map.empty |]

-- the root directory for transactional forest (it can't modify data not under this path)
TH.declareMVar "rootTxICFS"  [t| FilePath |] [e| "/" |]

-- a list of the starting times of running transactions sorted from newest to oldest (we may two txs with the same starting time)
declareMVar "runningTxsIC"  [t| [UTCTime] |] [e| [] |]

type TxICFSWrites = Set FilePath

-- a map with commit times of committed transactions and their performed changes
-- we use a @FSTreeDelta@ because the set of written paths may be infinite (like all paths under a given directory)
declareMVar "doneTxsIC"  [t| (Map UTCTime TxICFSWrites) |] [e| Map.empty |]

type TxICLayer l = (ForestLayer TxICFS l)

type instance IncK (IncForest TxICFS) a = (Typeable a,Eq a,AddTxICParent Inside a,AddTxICParent Outside a)

proxyTxICFS = Proxy :: Proxy TxICFS

-- insert a new time in a list sorted from newest to oldest
addRunningTxIC time = modifyMVarMasked_ runningTxsIC (\xs -> return $ List.insertBy (\x y -> compare y x) time xs)
-- remove a time in a list sorted from newest to oldest
deleteRunningTxIC time = modifyMVarMasked_ runningTxsIC (\xs -> return $ List.delete time xs)

-- ** Filesystem

-- latest tree,delta after the current tree,next tree,reads since the beginning, buffered variables, memoized forest transactional variables, temporary paths since the beginning
type TxICFSLog = IORef (FSTree TxICFS,FSDeltas,FSTree TxICFS,TxICFSReads,TxICBuffTable,TxICMemoTable,CanonicalTree TxICFS,Set FilePath)

type TxICBuffTable = MemoTable Unique DynTxICThunk
type TxICMemoTable = MemoTable (FilePath,TypeRep) (Dynamic,MkWeak,FSTree TxICFS)

-- nested logs for nested transactions
type TxICFSLogs = SList TxICFSLog

-- a chronological list of relative deltas for the different FS versions
type TxICFSTreeDeltas = IORef (Map FSVersion FSDeltas)

-- (starttime,tx id,register of deltas,nested logs)
newtype TxICFSEnv = TxICFSEnv (IORef UTCTime :!: TxId :!: TxICFSTreeDeltas :!: TxICFSLogs) deriving Typeable

type TxICFSReads = Set FilePath
type TxICFSChanges = (TxICFSReads,FSTreeDelta)
type TxICFSChangesFlat = (TxICFSReads,TxICFSWrites)

findTxICMemo :: Typeable rep => FilePath -> Proxy rep -> ForestM TxICFS (Maybe (rep,MkWeak,FSTree TxICFS))
findTxICMemo path (_ :: Proxy rep) = do
	TxICFSEnv (starttime :!: txid :!: deltas :!: txlogs) <- TxICFSForestM Reader.ask
	
	let find txlog m = do
		(tree,ds,newtree,reads,bufftbl,memotbl,cantree,tmps) <- forestIO $ readRef txlog
		mb <- forestIO $ MemoTable.lookup memotbl (path,typeOf (undefined :: rep))
		case mb of
			Just (fromDynamic -> Just rep,mkWeak,tree) -> do
				return $ Just (rep,mkWeak,tree)
			otherwise -> m
	Foldable.foldr find (return Nothing) txlogs

bufferTxICFSThunk :: (IncK (IncForest TxICFS) a,TxICLayer l) => ForestFSThunk TxICFS l a -> TxICThunk l a -> ForestM TxICFS ()
bufferTxICFSThunk var thunk = do
	TxICFSEnv (starttime :!: txid :!: deltas :!: SCons txlog _) <- TxICFSForestM Reader.ask
	(tree,ds,newtree,reads,bufftbl,memotbl,cantree,tmps) <- forestIO $ readRef txlog
	let mkWeak = MkWeak $ mkWeakRefKey $ txICFSThunkArgs var
	forestIO $ MemoTable.insertWithMkWeak bufftbl (txICFSThunkId var) (DynTxICThunk thunk mkWeak) mkWeak
	
bufferTxICHSThunk :: (IncK (IncForest TxICFS) a,TxICLayer l) => ForestHSThunk TxICFS l a -> TxICThunk l a -> ForestM TxICFS ()
bufferTxICHSThunk var thunk = do
	TxICFSEnv (starttime :!: txid :!: deltas :!: SCons txlog _) <- TxICFSForestM Reader.ask
	(tree,ds,newtree,reads,bufftbl,memotbl,cantree,tmps) <- forestIO $ readRef txlog
	let mkWeak = MkWeak $ mkWeakRefKey $ txICHSThunkArgs var
	forestIO $ MemoTable.insertWithMkWeak bufftbl (txICHSThunkId var) (DynTxICThunk thunk mkWeak) mkWeak

-- reads the pointer from a transactional variable to a thunk
bufferedTxICFSThunk :: (IncK (IncForest TxICFS) a,TxICLayer l) => ForestFSThunk TxICFS l a -> ForestM TxICFS (TxICThunk l a)
bufferedTxICFSThunk (var :: ForestFSThunk TxICFS l a) = do
	TxICFSEnv (starttime :!: txid :!: deltas :!: txlogs) <- TxICFSForestM Reader.ask
	
	let find txlog m = do
		(tree,ds,newtree,reads,bufftbl,memotbl,cantree,tmps) <- forestIO $ readRef txlog
		mb <- forestIO $ MemoTable.lookup bufftbl (txICFSThunkId var)
		case mb of
			Just (DynTxICThunk (cast -> Just thunk) _) -> return thunk
			Just dyn -> error $ "type mismatch: " ++ show (typeOf (undefined :: (TxICThunk l a))) ++" "++show dyn
			otherwise -> m
	
	let global = do
		tree <- latestTree
		mb <- forestIO $ readIORef (txICFSThunkArgs var)
		case mb of
			Just (_,comp) -> do
				t <- newTxICThunk comp tree
				bufferTxICFSThunk var t
				return t
			Nothing -> error $ "bufferedTxICFSThunk"
	
	Foldable.foldr find global txlogs

bufferedTxICHSThunk :: (IncK (IncForest TxICFS) a,TxICLayer l) => ForestHSThunk TxICFS l a -> ForestM TxICFS (TxICThunk l a)
bufferedTxICHSThunk (var :: ForestHSThunk TxICFS l a) = do
	TxICFSEnv (starttime :!: txid :!: deltas :!: txlogs) <- TxICFSForestM Reader.ask
	
	let find txlog m = do
		(tree,ds,newtree,reads,bufftbl,memotbl,cantree,tmps) <- forestIO $ readRef txlog
		mb <- forestIO $ MemoTable.lookup bufftbl (txICHSThunkId var)
		case mb of
			Just (DynTxICThunk (cast -> Just thunk) _) -> return thunk
			Just dyn -> error $ "type mismatch: " ++ show (typeOf (undefined :: (TxICThunk l a))) ++" "++show dyn
			otherwise -> m
	
	let global = do
		tree <- latestTree
		mb <- forestIO $ readIORef (txICHSThunkArgs var)
		case mb of
			Just (_,comp) -> do
				t <- newTxICThunk comp tree
				bufferTxICHSThunk var t
				return t
			Nothing -> error $ "bufferedTxICHSThunk"
	
	Foldable.foldr find global txlogs

getNextFSTree :: ForestM TxICFS (FSTree TxICFS)
getNextFSTree = do
	TxICFSEnv (starttime :!: txid :!: deltas :!: SCons txlog _) <- TxICFSForestM Reader.ask
	(tree,ds,newtree,reads,bufftbl,memotbl,cantree,tmps) <- forestIO $ readRef txlog
	return newtree

-- creates the next tree (on writes)
nextTxICFSTree :: ForestM TxICFS ()
nextTxICFSTree = do
	TxICFSEnv (starttime :!: txid :!: deltas :!: SCons txlog _) <- TxICFSForestM Reader.ask
	(tree,ds,_,reads,bufftbl,memotbl,cantree,tmps) <- forestIO $ readRef txlog
	
	-- next tree
	newtreeDelta <- forestIO $ readIORef (fsTreeFSTreeDelta tree) >>= newIORef 
	newtreeSyms <- forestIO $ readIORef (fsTreeSym tree) >>= newIORef
	let newtree = TxICFSTree (fsTreeTxId tree) (succ $ fsTreeFSVersion tree) newtreeDelta (fsTreeVirtual tree) newtreeSyms
	
	forestIO $ writeRef txlog (tree,DList.empty,newtree,reads,bufftbl,memotbl,cantree,tmps)
	

incrementTxICFSTree :: ForestM TxICFS ()
incrementTxICFSTree = do
	TxICFSEnv (starttime :!: txid :!: deltas :!: SCons txlog _) <- TxICFSForestM Reader.ask
	(tree,ds,newtree,reads,bufftbl,memotbl,cantree,tmps) <- forestIO $ readRef txlog
	-- add the relative deltas
	tds <- forestIO $ readRef deltas
	forestIO $ writeRef deltas $ Map.insert (fsTreeFSVersion newtree) ds tds
	forestIO $ writeRef txlog (newtree,DList.empty,newtree,reads,bufftbl,memotbl,cantree,tmps)

-- only needs to read from the top-level log
getTxICFSChangesFlat :: ForestM TxICFS TxICFSChangesFlat
getTxICFSChangesFlat = do
	TxICFSEnv (starttime :!: txid :!: deltas :!: SCons txlog _) <- TxICFSForestM Reader.ask
	(tree,ds,newtree,reads,bufftbl,memotbl,cantree,tmps) <- forestIO $ readRef txlog
	writes <- forestIO $ readRef (fsTreeFSTreeDelta tree)
	return (reads,fsTreeDeltaWrites "" writes)

-- reads from the current tree
-- only needs to read from the top-level log
getTxICFSChanges :: ForestM TxICFS TxICFSChanges
getTxICFSChanges = do
	TxICFSEnv (starttime :!: txid :!: deltas :!: SCons txlog _) <- TxICFSForestM Reader.ask
	(tree,ds,newtree,reads,bufftbl,memotbl,cantree,tmps) <- forestIO $ readIORef txlog
	writes <- forestIO $ readRef (fsTreeFSTreeDelta tree)
	return (reads,writes)
	
getTxICFSNextChanges :: ForestM TxICFS FSTreeDelta
getTxICFSNextChanges = do
	TxICFSEnv (starttime :!: txid :!: deltas :!: SCons txlog _) <- TxICFSForestM Reader.ask
	(tree,ds,newtree,reads,bufftbl,memotbl,cantree,tmps) <- forestIO $ readIORef txlog
	writes <- forestIO $ readRef (fsTreeFSTreeDelta newtree)
	return writes

putTxICFSRead :: FilePath -> ForestM TxICFS ()
putTxICFSRead path = do
	TxICFSEnv (starttime :!: txid :!: deltas :!: SCons txlog _) <- TxICFSForestM Reader.ask
	(tree,ds,newtree,reads,bufftbl,memotbl,cantree,tmps) <- forestIO $ readIORef txlog
	forestIO $ writeIORef txlog (tree,ds,newtree,Set.insert path reads,bufftbl,memotbl,cantree,tmps)

putTxICFSTmp :: FilePath -> ForestM TxICFS ()
putTxICFSTmp tmp = do
	TxICFSEnv (starttime :!: txid :!: deltas :!: SCons txlog _) <- TxICFSForestM Reader.ask
	(tree,ds,newtree,reads,bufftbl,memotbl,cantree,tmps) <- forestIO $ readIORef txlog
	forestIO $ writeRef txlog (tree,ds,newtree,reads,bufftbl,memotbl,cantree,Set.insert tmp tmps)

-- writes to the next tree
-- duplicate the action on both the relative deltas (sequence of primitve FSDelta) and the absolute deltas (FSTreeDelta)
modifyTxICFSTreeDeltas :: FSDelta -> ForestM TxICFS ()
modifyTxICFSTreeDeltas d = do
	TxICFSEnv (starttime :!: txid :!: deltas :!: SCons txlog _) <- TxICFSForestM Reader.ask
	(tree,ds,newtree,reads,bufftbl,memotbl,cantree,tmps) <- forestIO $ readIORef txlog
	forestIO $ modifyIORef (fsTreeFSTreeDelta newtree) (appendToFSTreeDelta d)
	forestIO $ modifyIORef (fsTreeSym newtree) (\(ds,ts) -> (DList.snoc ds d,appendToFSTreeSym d ts))
	forestIO $ writeIORef txlog (tree,DList.snoc ds d,newtree,reads,bufftbl,memotbl,cantree,tmps)

treeTxICFSThunk :: (IncK (IncForest TxICFS) a,Typeable l,Typeable a,TxICLayer l) => ForestFSThunk TxICFS l a -> ForestL TxICFS l (FSTree TxICFS)
treeTxICFSThunk var = do
	thunk <- forestM $ bufferedTxICFSThunk var
	treeTxICThunk thunk

treeTxICThunk :: (IncK (IncForest TxICFS) a,Typeable l,Typeable a,TxICLayer l) => TxICThunk l a -> ForestL TxICFS l (FSTree TxICFS)
treeTxICThunk thunk = do
	buff <- forestM $ bufferedTxICThunk thunk
	return $ buffTxICTree buff

-- transaction unique id
type TxId = Unique
-- a transaction-local version number that is incremented on writes
type FSVersion = Int
	
instance Show (FSTree TxICFS) where
	show tree = "(FSTreeTxICFS " ++ show (fsTreeTxId tree) ++" "++ show (fsTreeFSVersion tree) ++" "++ show (fsTreeVirtual tree) ++ ")"

-- a global table of symbolic links that all transactions read from and keep updated
-- this is used to explore the locality in FS modifications; without knowing the existing symlinks, there is no locality
{-# NOINLINE symlinks #-}
symlinks :: MVar FSTreeSym
symlinks = unsafePerformIO $ do
	syms <- computeSymlinks
	newMVar syms

updateSymlinks :: IO ()
updateSymlinks = do
	syms <- computeSymlinks
	swapMVar symlinks syms
	return ()

computeSymlinks :: IO FSTreeSym
computeSymlinks = do
	path <- readMVar rootTxICFS
	putStrLn $ "Finding all symbolic links under... " ++ show path
	syms <- findSymLinks path
	putStrLn $ "Symbolic links calculated for " ++ show path
	return syms

instance FSRep TxICFS where
	
	newtype ForestM TxICFS a = TxICFSForestM { runTxICFSForestM :: ReaderT TxICFSEnv IO a } deriving (Functor,Applicative,Monad)
	
	data ForestCfg TxICFS = TxICFSForestCfg FilePath
	
	forestIO = TxICFSForestM . lift
	
	-- | returns the forest directory used to store auxiliary data
	getForestDirectory = liftM (</> "Forest") $ forestIO getTemporaryDirectory
	
	data FSTree TxICFS = TxICFSTree {
		  fsTreeTxId :: TxId -- transaction id
		, fsTreeFSVersion :: FSVersion -- tx-local fsversion
		, fsTreeFSTreeDelta :: IORef FSTreeDelta -- FS deltas since beginning of the tx (over the physical FS)
		, fsTreeVirtual :: Bool -- virtual flag
		, fsTreeSym :: IORef (DList FSDelta,FSTreeSym) -- a sequence of symlink modifications since the start and the symlinks table at the time of this tree
		}
	
	showFSTree = return . show
	compareFSTree tree1 tree2 = return $ compare
		(fsTreeTxId tree1,fsTreeFSVersion tree1,fsTreeVirtual tree1)
		(fsTreeTxId tree2,fsTreeFSVersion tree2,fsTreeVirtual tree2)
	
	-- log the modifications
	-- writes come only from manifests, for which we have already canonized the paths
	deletePath path = modifyTxICFSTreeDeltas (Rem path)
	writeFile path ondisk = modifyTxICFSTreeDeltas (Add path ondisk)
	writeDir path = do
		ondisk <- tempPath
		forestIO $ createDirectory ondisk
		modifyTxICFSTreeDeltas (Add path ondisk)
	writeLink path linkpath = modifyTxICFSTreeDeltas (AddLink path linkpath)
	writePathMD path ondisk = modifyTxICFSTreeDeltas (ChgAttrs path ondisk)
	
	-- registers a new temporary path
	tempPath = forestIO getTempPath >>= \path -> putTxICFSTmp path >> return path
	-- change into AVFS mode
	virtualTree tree = return $ tree { fsTreeVirtual = True }
	latestTree = do
		TxICFSEnv (starttime :!: txid :!: deltas :!: SCons txlog _) <- TxICFSForestM Reader.ask
		(tree,ds,newtree,reads,bufftbl,memotbl,cantree,tmps) <- forestIO $ readIORef txlog
		return tree
	
	-- reads from the FS or from the modification log
	pathInTree path tree = do
		-- canonize the fullpath for reads
		canpath <- canonalizeDirectoryInTree path tree
		-- mark the path as read
		putTxICFSRead canpath
		td <- forestIO $ readIORef $ fsTreeFSTreeDelta tree
		let path_td = focusFSTreeDeltaByRelativePathMay td canpath
		let ondisk = case onDiskWriteMay path_td of
			Nothing -> canpath
			Just ondisk -> ondisk
		virtualizeTxICFS ondisk tree

	stepPathInTree tree path rel = do
		dir <- canonalizePathInTree path tree
		return $ dir </> rel
	
	getDirectoryContentsInTree = getDirectoryContentsTxICFS
		
	doesDirectoryExistInTree = doesExistTxICFS DirExists
	doesFileExistInTree = doesExistTxICFS FileExists
	doesExistInTree = doesExistTxICFS AnyExists
	
	canonalizePathInTree = canonalizePathTxICFS

	type FSTreeD TxICFS = FSTreeDeltaNodeMay -- a delta fixed-point focused on the current path

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
		if fsTreeTxId oldtree == fsTreeTxId newtree
			then do
				TxICFSEnv (starttime :!: txid :!: deltas :!: txlogs) <- TxICFSForestM Reader.ask
				tds <- forestIO $ readRef deltas
				let dfs = drop (fsTreeFSVersion oldtree) $ take (fsTreeFSVersion newtree) $ Map.elems tds
				syms <- liftM Prelude.snd $ forestIO $ readRef (fsTreeSym newtree)
				let fixtd = fixFSTreeDelta (compressFSDeltas $ DList.concat dfs) syms
				let df = focusFSTreeDeltaByRelativePathMay fixtd path
				return $ Just df
			-- no sharing among different txs
			else return Nothing

virtualizeTxICFS path tree = if fsTreeVirtual tree
	then liftM (</> ".avfs" </> makeRelative "/" path) (forestIO getHomeDirectory)
	else return path

getDirectoryContentsTxICFS :: FilePath -> FSTree TxICFS -> ForestM TxICFS [FileName]
getDirectoryContentsTxICFS path tree = do
	canpath <- canonalizePathInTree path tree
	putTxICFSRead canpath
	td <- forestIO $ readIORef $ fsTreeFSTreeDelta tree
	let path_td = focusFSTreeDeltaByRelativePathMay td canpath
	xs <- forestIO $ getContentsFSTreeDeltaNodeMay canpath path_td
	return xs

doesExistTxICFS :: ExistFlag -> FilePath -> FSTree TxICFS -> ForestM TxICFS Bool
doesExistTxICFS flag path tree = do
	canpath <- canonalizeDirectoryInTree path tree
	putTxICFSRead canpath
	td <- forestIO $ readIORef $ fsTreeFSTreeDelta tree
	let path_td = focusFSTreeDeltaByRelativePathMay td canpath
	case path_td of
		Just (FSTreeNop _) -> return $ flag /= FileExists
		Just (FSTreeChg _ _) -> return $ flag /= FileExists
		Just (FSTreeNew _ _ diskpath _) -> forestIO $ doesExistShellFlag flag diskpath
		otherwise -> forestIO $ doesExistShellFlag flag canpath

memoCanonicalPath :: FilePath -> FilePath -> FSTree TxICFS -> ForestM TxICFS ()
memoCanonicalPath src tgt fs = do
	TxICFSEnv (starttime :!: txid :!: deltas :!: SCons txlog _) <- TxICFSForestM Reader.ask
	(tree,ds,newtree,reads,bufftbl,memotbl,cantree,tmps) <- forestIO $ readIORef txlog
	let cantree' = memoCanonicalTree src tgt fs cantree
	forestIO $ writeIORef txlog (tree,ds,newtree,reads,bufftbl,memotbl,cantree',tmps)

-- returns a canonical prefix, at a given tree, and a non-canonized suffix
findCanonicalPath :: [FileName] -> ForestM TxICFS (Maybe (FilePath,FSTree TxICFS,[FileName]))
findCanonicalPath dirs = do
	TxICFSEnv (starttime :!: txid :!: deltas :!: SCons txlog _) <- TxICFSForestM Reader.ask
	(tree,ds,newtree,reads,bufftbl,memotbl,cantree,tmps) <- forestIO $ readIORef txlog
	return $ findCanonicalTree dirs cantree

-- canonalizes a path, taking into account the buffered FSTreeDelta and logging reads of followed symlinks
--canonalizePathTxICFS :: FilePath -> FSTree TxICFS -> ForestM TxICFS FilePath
--canonalizePathTxICFS path tree = do
--	norm <- forestIO $ liftM normalise $ absolutePath path
--	let dirs = splitDirectories norm	
--	mb <- findCanonicalPath dirs
--	can <- case mb of
--		Just (root,oldtree,suffix) -> do
--			mb <- diffFS oldtree tree ""
--			case mb of
--				Just df -> follow df root suffix
--				Nothing -> liftM Prelude.snd getTxICFSChanges >>= \df -> follow df "" dirs
--		Nothing -> liftM Prelude.snd getTxICFSChanges >>= \df -> follow df "" dirs
--	memoCanonicalPath norm can tree
--	return can	
--  where
--	follow :: FSTreeDelta -> FilePath -> [FileName] -> ForestM TxICFS FilePath
--	follow td root [] = return root
--	follow td root (".":dirs) = follow td root dirs
--	follow td root ("..":dirs) = follow td (takeDirectory root) dirs
--	follow td root (dir:dirs) = do
--		let rootdir = root </> dir
--		let td' = focusFSTreeDeltaByRelativePathMay td rootdir
--		case td' of
--			Just (FSTreeNewLink link _) -> do -- follow buffered links
--				-- mark the symbolic link as read
--				putTxICFSRead rootdir
--				canonalizePathTxICFS (root </> link </> joinPath dirs) tree
--			otherwise -> do -- follow FS links
--				rootdir_dsk <- virtualizeTxICFS rootdir tree
--				e <- forestIO $ Exception.try $ liftM isSymbolicLink (getSymbolicLinkStatus rootdir_dsk)
--				case e of
--					Left (e::SomeException) -> follow td rootdir dirs
--					Right isLink -> do
--						if isLink
--							then do
--								-- mark the symbolic link as read
--								putTxICFSRead rootdir
--								e <- forestIO $ Exception.try $ readSymbolicLink rootdir_dsk
--								case e of
--									Left (e::SomeException) -> follow td rootdir dirs
--									Right p -> canonalizePathTxICFS (root </> p </> joinPath dirs) tree
--							else follow td rootdir dirs

-- canonalize subpaths within the same tree (does it pay off to compute diffs just for canonization?)
canonalizePathTxICFS :: FilePath -> FSTree TxICFS -> ForestM TxICFS FilePath
canonalizePathTxICFS path tree = {-debug ("canonalizePathTxICFS " ++ show path) $ -} do
	norm <- forestIO $ liftM normalise $ absolutePath path
	let dirs = splitDirectories norm	
	mb <- findCanonicalPath dirs
	(_,syms) <- forestIO $ readIORef $ fsTreeSym tree
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
	follow :: FSTreeSym -> FilePath -> [FileName] -> ForestM TxICFS FilePath
	follow td root [] = return root
	follow td root (".":dirs) = follow td root dirs
	follow td root ("..":dirs) = follow td (takeDirectory root) dirs
	follow td root (dir:dirs) = do
		let rootdir = root </> dir
		case Map.lookup dir td of
			Just (FSTreeSymLink tgt) -> do -- follow link
				-- mark the symbolic link as read
				putTxICFSRead rootdir
				canonalizePathTxICFS (root </> tgt </> joinPath dirs) tree
			Nothing -> do
				follow (focusFSTreeSym dir td) rootdir dirs

-- ** Incrementality

class TxICFSLayerImpl l where
	unTxICFSLayer :: Proxy l -> l (IncForest TxICFS) a -> ReaderT TxICFSEnv IO a
	txICFSLayer :: Proxy l -> ReaderT TxICFSEnv IO a -> l (IncForest TxICFS) a

instance TxICFSLayerImpl Outside where
	unTxICFSLayer _ = runTxICFSForestO
	txICFSLayer _ = TxICFSForestO

instance TxICFSLayerImpl Inside where
	unTxICFSLayer _ = runTxICFSForestI
	txICFSLayer _ = TxICFSForestI

instance Incremental (IncForest TxICFS) where
	
	newtype Outside (IncForest TxICFS) a = TxICFSForestO { runTxICFSForestO :: ReaderT TxICFSEnv IO a } deriving (Functor,Applicative,Monad)
	newtype Inside (IncForest TxICFS) a = TxICFSForestI { runTxICFSForestI :: ReaderT TxICFSEnv IO a } deriving (Functor,Applicative,Monad)

	world = TxICFSForestO . runTxICFSForestI
	unsafeWorld = TxICFSForestI . runTxICFSForestO

	runIncremental = error "please use atomically instead"

	unsafeIOToInc = inside . TxICFSForestI . lift
	
instance ICRep TxICFS where

	runIncrementalForest (TxICFSForestCfg root) m = atomicallyTxICFS root m

	forestM = inside . TxICFSForestI . runTxICFSForestM
	forestO = TxICFSForestM . runTxICFSForestO

	-- arguments never change
	-- global variables may be shared across transactions, and the argument computation creates new data if no buffered data is found in the tx log
	data FSThunk TxICFS l inc a = TxICFSThunk { txICFSThunkId :: Unique, txICFSThunkArgs :: IORef (Maybe (TxICArgs,l inc a)) }

	data HSThunk TxICFS l inc a = TxICHSThunk { txICHSThunkId :: Unique, txICHSThunkArgs :: IORef (Maybe (TxICArgs,l inc a)) }
		
	newtype ICThunk TxICFS l inc a = TxICICThunk (l inc a)

	data ValueDelta TxICFS a = ValueDeltaTxICFS Bool -- True = identity
	
	diffValueThunk = diffValueThunkTxICFS False
	diffTopValueThunk = diffValueThunkTxICFS True	
	diffValueAny _ _ = return chgValueDelta
	
	isIdValueDelta (ValueDeltaTxICFS d) = d
	idValueDelta = ValueDeltaTxICFS True
	chgValueDelta = ValueDeltaTxICFS False
	mapValueDelta _ (ValueDeltaTxICFS d) = (ValueDeltaTxICFS d)

	eqFSThunk t1 t2 = txICFSThunkId t1 == txICFSThunkId t2
	eqICThunk t1 t2 = False

	isUnevaluatedFSThunk var = do
		thunk <- forestM $ bufferedTxICFSThunk var
		buff <- forestM $ bufferedTxICThunk thunk
		case buffTxICData buff of
			TxICThunkComp force -> return True
			TxICThunkForce a stone -> return False

-- tests for equality
diffValueThunkTxICFS :: (Typeable rep,IncK (IncForest TxICFS) content,Typeable content,ForestRep rep (ForestFSThunkI TxICFS content)) => Bool -> FSTree TxICFS -> rep -> ForestO TxICFS (ValueDelta TxICFS rep)
diffValueThunkTxICFS isTop oldtree rep = do
		thunk <- forestM $ bufferedTxICFSThunk $ to iso_rep_thunk rep
		uid <- forestM $ forestIO $ readRef $ txICId thunk
		(buff :: BuffTxICThunk Inside content) <- forestM $ bufferedTxICThunk thunk
		let newtree = if isTop then buffTxICTree buff else buffTxICDeltaTree buff
		cmp <- forestM $ compareFSTree oldtree newtree
		--debug ("diffValueThunkTxICFS " ++ show isTop ++" "++ show oldtree ++" "++ show newtree ++" "++ show (typeOf rep)) $
		return $ ValueDeltaTxICFS $ cmp /= LT
	
-- arguments passed to transactional variables
type TxICArgs = (Dynamic,FilePath)

-- dynamic variables for buffered content
data DynTxICThunk where
	-- transactional variable (points to an internal thunk)
	DynTxICThunk :: (IncK (IncForest TxICFS) a,ForestLayer TxICFS l) => TxICThunk l a -> MkWeak -> DynTxICThunk
	-- internal thunk (buffered content used for nested logs) 
	DynBuffTxICThunk :: (IncK (IncForest TxICFS) a,ForestLayer TxICFS l) => BuffTxICThunk l a -> MkWeak -> DynTxICThunk

instance Show DynTxICThunk where
	show (DynTxICThunk t _) = show (typeOf t)
	show (DynBuffTxICThunk t _) = show (typeOf t)

dynTxMkWeak :: DynTxICThunk -> MkWeak
dynTxMkWeak (DynTxICThunk _ mkWeak) = mkWeak
dynTxMkWeak (DynBuffTxICThunk _ mkWeak) = mkWeak

data BuffTxICThunk l a = BuffTxICThunk {
	  buffTxICData :: TxICThunkData l a
	, buffTxICTree :: FSTree TxICFS -- latest change to the thunk
	, buffTxICDeltaTree :: FSTree TxICFS -- latest recursive change
	, buffTxICParents :: TxICParents -- parent thunks
	} deriving Typeable

-- buffered for nested transactions
newtype TxICThunk (l :: * -> * -> *) a = TxICThunk { txICId :: IORef Unique } deriving Typeable

data TxICThunkData l a =
		TxICThunkComp (l (IncForest TxICFS) a)
	| 	TxICThunkForce a TxICStone -- the stone is used to keep weak memoized entries as long as the thunk is not recomputed

-- a dummy reference to be used as key for weak references
type TxICStone = IORef ()

type TxICParents = WeakMap Unique (WeakTxICThunk,MkWeak)

data WeakTxICThunk where
	WeakTxICThunk :: (IncK (IncForest TxICFS) a,TxICLayer l) => Weak (TxICThunk l a) -> WeakTxICThunk

bufferTxICThunk :: (IncK (IncForest TxICFS) a,ForestLayer TxICFS l) => TxICThunk l a -> BuffTxICThunk l a -> ForestM TxICFS ()
bufferTxICThunk thunk buff = do
	uid <- forestIO $ readIORef (txICId thunk)
	let mkWeak = MkWeak $ mkWeakRefKey $ txICId thunk
	let dyn = DynBuffTxICThunk buff mkWeak
	TxICFSEnv (starttime :!: txid :!: deltas :!: SCons txlog _) <- TxICFSForestM Reader.ask
	(tree,delta,newtree,reads,bufftbl,memotbl,cantree,tmps) <- forestIO $ readIORef txlog
	forestIO $ MemoTable.insertWithMkWeak bufftbl uid dyn mkWeak

bufferedTxICThunk :: (IncK (IncForest TxICFS) a,ForestLayer TxICFS l) => TxICThunk l a -> ForestM TxICFS (BuffTxICThunk l a)
bufferedTxICThunk thunk = do
	uid <- forestIO $ readIORef (txICId thunk)
	TxICFSEnv (starttime :!: txid :!: deltas :!: txlogs) <- TxICFSForestM Reader.ask
	let find mb txlog = case mb of
		Nothing -> do
			(tree,ds,newtree,reads,bufftbl,memotbl,cantree,tmps) <- forestIO $ readRef txlog
			forestIO $ MemoTable.lookup bufftbl uid
		otherwise -> return mb
	dyn <- liftM (fromJustNote $ "bufferedTxICThunk'' ") $ Foldable.foldlM find Nothing txlogs
	
	case dyn of
		(DynBuffTxICThunk (cast -> Just buff) mkWeak) -> return buff
		dyn -> error $ "type mismatch: " ++ show (typeOf thunk) ++" "++show dyn

dirtyTxICParents :: FSTree TxICFS -> TxICParents -> ForestM TxICFS ()
dirtyTxICParents tree parents = WeakMap.mapM_' forestIO dirty parents where
	dirty (uid,(w,mk)) = do
		mb <- changeTxICThunkW w $ \buff -> do
			let oldtree = buffTxICDeltaTree buff
			cmp <- forestM $ compareFSTree oldtree tree
			let b = cmp > LT
			let buff' = if b then buff else buff { buffTxICDeltaTree = tree }
			return (buff',(b,parents))
		case mb of
			Just (b,parents) -> unless b $ dirtyTxICParents tree parents
			Nothing -> return ()

class AddTxICParent l a where
	addTxICParent :: Proxy l -> TxICStone -> Unique -> FSTree TxICFS -> a -> ForestL TxICFS l (FSTree TxICFS)

addTxICParentProxy :: Proxy l -> Proxy (AddTxICParentDict l)
addTxICParentProxy _ = Proxy

data AddTxICParentDict l a = AddTxICParentDict { addTxICParentDictDict :: Proxy l -> TxICStone -> Unique -> FSTree TxICFS -> a -> ForestL TxICFS l (FSTree TxICFS) }

instance (AddTxICParent l a) => Sat (AddTxICParentDict l a) where
	dict = AddTxICParentDict { addTxICParentDictDict = addTxICParent }

instance (IncK (IncForest TxICFS) a,Typeable l,Typeable a,TxICLayer l) => AddTxICParent l (ForestFSThunk TxICFS l a) where
	addTxICParent proxy stone uid z (var :: ForestFSThunk TxICFS l a) = do
		(thunk :: TxICThunk l a) <- forestM $ bufferedTxICFSThunk var
		let add buff = do
			w <- forestM $ forestIO $ Weak.mkWeak thunk thunk Nothing
			let mkWeak = MkWeak $ mkWeakRefKey stone
			forestM $ forestIO $ WeakMap.insertWithMkWeak (buffTxICParents buff) mkWeak uid (WeakTxICThunk w,mkWeak)
			tree <- treeTxICThunk thunk
			return (buff,tree)
		changeTxICThunk thunk add
instance (IncK (IncForest TxICFS) a,Typeable l,Typeable a,TxICLayer l) => AddTxICParent l (ForestHSThunk TxICFS l a) where
	addTxICParent proxy stone uid z (var :: ForestHSThunk TxICFS l a) = do
		(thunk :: TxICThunk l a) <- forestM $ bufferedTxICHSThunk var
		let add buff = do
			w <- forestM $ forestIO $ Weak.mkWeak thunk thunk Nothing
			let mkWeak = MkWeak $ mkWeakRefKey stone
			forestM $ forestIO $ WeakMap.insertWithMkWeak (buffTxICParents buff) mkWeak uid (WeakTxICThunk w,mkWeak)
			tree <- treeTxICThunk thunk
			return (buff,tree)
		changeTxICThunk thunk add
instance (ForestLayer TxICFS l) => AddTxICParent l Forest_err where
	addTxICParent proxy stone meta z x = return z
instance (ForestLayer TxICFS l) => AddTxICParent l FileInfo where
	addTxICParent proxy stone meta z x = return z
instance (ForestLayer TxICFS l,AddTxICParent l (ForestFSThunkI TxICFS Forest_err),AddTxICParent l (ForestFSThunkI TxICFS FileInfo)) => AddTxICParent l (Forest_md TxICFS) where
	addTxICParent proxy stone meta z fmd = do
		z1 <- addTxICParent proxy stone meta z (errors fmd)
		addTxICParent proxy stone meta z1 (fileInfo fmd)
instance (ForestLayer TxICFS l,MData (AddTxICParentDict l) (ForestL TxICFS l) a) => AddTxICParent l a where
	addTxICParent proxy stone meta z x = do
		let f t1 t2 = forestM $ maxFSTree t1 t2
		gmapQr (addTxICParentProxy proxy) f z (addTxICParentDictDict dict proxy stone meta z) x

newTxICThunk :: (IncK (IncForest TxICFS) a,TxICLayer l) => l (IncForest TxICFS) a -> FSTree TxICFS -> ForestM TxICFS (TxICThunk l a)
newTxICThunk m tree = do
	uid <- forestIO $ newUnique
	stone <- forestIO $ newRef uid
	let dta = TxICThunkComp m
	let delta = tree
	parents <- forestIO $ WeakMap.new
	let thunk = TxICThunk stone
	let buff = BuffTxICThunk dta tree delta parents
	bufferTxICThunk thunk buff
	return thunk

readTxICThunk :: (IncK (IncForest TxICFS) a,AddTxICParent l a,TxICLayer l) => TxICThunk l a -> FSTree TxICFS -> ForestL TxICFS l (a,FSTree TxICFS)
readTxICThunk t tree = do
	let eval buff = case buffTxICData buff of
		TxICThunkComp force -> do
			a <- force
			stone <- forestM $ forestIO $ newIORef ()
			let dta' = TxICThunkForce a stone
			uid <- forestM $ forestIO $ readRef $ txICId t
			-- add current thunk as a parent of its content thunks
			deltatree <- addTxICParent Proxy stone uid tree a
			return (buff { buffTxICData = dta', buffTxICDeltaTree = deltatree },(a,deltatree))
		TxICThunkForce a stone -> return (buff,(a,tree))
	changeTxICThunk t eval

changeTxICThunk :: (IncK (IncForest TxICFS) a,TxICLayer l) => TxICThunk l a -> (BuffTxICThunk l a -> ForestL TxICFS l (BuffTxICThunk l a,b)) -> ForestL TxICFS l b
changeTxICThunk thunk f = do
	buff <- forestM $ bufferedTxICThunk thunk
	(buff',b) <- f buff
	forestM $ bufferTxICThunk thunk buff'
	return b

changeTxICThunkW :: WeakTxICThunk -> (forall a l . (IncK (IncForest TxICFS) a,TxICLayer l) => BuffTxICThunk l a -> ForestL TxICFS l (BuffTxICThunk l a,b)) -> ForestM TxICFS (Maybe b)
changeTxICThunkW (WeakTxICThunk w) f = do
	mb <- forestIO $ Weak.deRefWeak w
	case mb of
		Nothing -> return Nothing
		Just thunk -> liftM Just $ forestO $ outside $ changeTxICThunk thunk f

-- strict variable write (value)
writeTxICThunk :: (IncK (IncForest TxICFS) a,AddTxICParent l a,TxICLayer l) => TxICThunk l a -> a -> ForestO TxICFS ()
writeTxICThunk var a = do
	newtree <- forestM getNextFSTree
	let set buff = do
		stone <- forestM $ forestIO $ newIORef ()
		let dta' = TxICThunkForce a stone
		uid <- forestM $ forestIO $ readRef $ txICId var
		-- mark the written thunk as a parent to its content thunks
		_ <- addTxICParent Proxy stone uid newtree a
		-- dirty the parents of the written thunk
		forestM $ dirtyTxICParents newtree (buffTxICParents buff)
		return (buff { buffTxICData = dta', buffTxICTree = newtree, buffTxICDeltaTree = newtree },())
	outside $ changeTxICThunk var set

-- lazy variable write (expression)
overwriteTxICThunk :: (IncK (IncForest TxICFS) a,AddTxICParent l a,TxICLayer l) => TxICThunk l a -> l (IncForest TxICFS) a -> ForestO TxICFS ()
overwriteTxICThunk var ma = do
	newtree <- forestM getNextFSTree
	let set buff = do
		let dta' = TxICThunkComp ma
		-- dirty the parents of the written thunk
		forestM $ dirtyTxICParents newtree (buffTxICParents buff)
		-- assuming that this is only used by loadDelta at the latest tree
		return (buff { buffTxICData = dta', buffTxICTree = newtree, buffTxICDeltaTree = newtree },())
		-- whenever we want to know the latest modification for the written thunk, we have to force a read and check its content thunks
		--return (buff { buffTxICData = dta', buffTxICTree = newtree, buffTxICDeltaTree = join $ liftM Prelude.snd $ readTxICThunk var (return newtree) },())
	outside $ changeTxICThunk var set
	

-- memoize internal thunks, not FSThunks
instance ZippedICMemo TxICFS where

	addZippedMemo path proxy args (rep :: rep) mb_tree = debug ("added memo "++show path ++ show mb_tree) $ do
		let var = to iso_rep_thunk rep
		TxICFSEnv (starttime :!: txid :!: deltas :!: SCons txlog _) <- txICFSLayer Proxy Reader.ask
		(tree,ds,newtree,reads,bufftbl,memotbl,cantree,tmps) <- forestM $ forestIO $ readRef txlog
		
		-- remember the arguments (this is how we connect transactional arguments to transactional variables)
		let txargs = (toDyn args,path)
		
		let load = do
			latest <- forestM latestTree
			(t :: rep) <- zloadScratch proxy args return path latest getForestMDInTree
			Inc.get (to iso_rep_thunk t)
			
		
		forestM $ forestIO $ writeRef (txICFSThunkArgs var) $ Just (txargs,load)
		
		-- memoize the entry
		case mb_tree of
			Nothing -> return ()
			Just tree -> do
				let mkWeak = MkWeak $ Weak.mkWeakRefKey $ txICFSThunkArgs var
				forestM $ forestIO $ MemoTable.insertWithMkWeak memotbl (path,typeOf rep) (toDyn rep,mkWeak,tree) mkWeak
	
	findZippedMemo args path proxy = do
		TxICFSEnv (starttime :!: txid :!: deltas :!: SCons txlog _) <- TxICFSForestI Reader.ask
		(tree,ds,newtree,reads,bufftbl,memotbl,cantree,tmps) <- forestM $ forestIO $ readRef txlog
		mb <- forestM $ findTxICMemo path proxy
		case mb of
			Nothing -> return Nothing
			Just (rep,mkWeak,tree) -> debug ("found memo " ++ show path ++ " " ++ show tree) $ do
				let var = to iso_rep_thunk rep
				Just ((fromDynamic -> Just txargs,_),_) <- forestM $ forestIO $ readRef $ txICFSThunkArgs var
				return $ Just (tree,txargs,rep)

argsTxICFS :: (FTK TxICFS args rep var content) => Proxy args -> rep -> ForestO TxICFS (ForestVs args,FilePath)
argsTxICFS proxy rep = do
	mb <- forestM $ getFTVArgsIC proxy rep
	case mb of
		Nothing -> error "should not happen"
		Just (margs,path) -> liftM (,path) $ inside $ vArgs Proxy proxy margs

getFTVArgsIC :: (FTK TxICFS args rep var content) => Proxy args -> rep -> ForestM TxICFS (Maybe (ForestIs TxICFS args,FilePath))
getFTVArgsIC (proxy ::Proxy args) rep = forestIO $ do
	let var = to iso_rep_thunk rep
	mb <- readIORef (txICFSThunkArgs var)
	case mb of
		Just ((fromDynamic -> Just args,path),_) -> return $ Just (args,path)
		otherwise -> return Nothing
	

instance Thunk (HSThunk TxICFS) Inside (IncForest TxICFS) where
	new m = do
		tree <- forestM $ latestTree
		uid <- forestM $ forestIO newUnique
		txargs <- forestM $ forestIO $ newIORef Nothing
		let var = TxICHSThunk uid txargs
		thunk <- forestM $ newTxICThunk m tree
		forestM $ bufferTxICHSThunk var thunk
		return var
	read var = do
		thunk <- forestM $ bufferedTxICHSThunk var
		tree <- forestM $ latestTree
		liftM Prelude.fst $ readTxICThunk thunk tree
instance Thunk (HSThunk TxICFS) Outside (IncForest TxICFS) where
	new m = do
		tree <- forestM $ latestTree
		uid <- forestM $ forestIO newUnique
		txargs <- forestM $ forestIO $ newIORef Nothing
		let var = TxICHSThunk uid txargs
		thunk <- forestM $ newTxICThunk m tree
		forestM $ bufferTxICHSThunk var thunk
		return var
	read var = do
		thunk <- forestM $ bufferedTxICHSThunk var
		tree <- forestM $ latestTree
		liftM Prelude.fst $ readTxICThunk thunk tree

instance TxICLayer l => Thunk (ICThunk TxICFS) l (IncForest TxICFS) where
	new m = return $ TxICICThunk m
	read (TxICICThunk m) = m
instance TxICLayer l => Output (ICThunk TxICFS) l (IncForest TxICFS) where
	thunk = Inc.new
	force = Inc.read

instance Thunk (FSThunk TxICFS) Inside (IncForest TxICFS) where
	new m = do
		tree <- forestM $ latestTree
		uid <- forestM $ forestIO newUnique
		txargs <- forestM $ forestIO $ newIORef Nothing
		let var = TxICFSThunk uid txargs
		thunk <- forestM $ newTxICThunk m tree
		forestM $ bufferTxICFSThunk var thunk
		return var
	-- read on the internal thunk, not on the latest filesystem
	read var = do
		thunk <- forestM $ bufferedTxICFSThunk var
		tree <- forestM $ latestTree
		liftM Prelude.fst $ readTxICThunk thunk tree
instance Thunk (FSThunk TxICFS) Outside (IncForest TxICFS) where
	new m = do
		tree <- forestM $ latestTree
		uid <- forestM $ forestIO newUnique
		txargs <- forestM $ forestIO $ newIORef Nothing
		let var = TxICFSThunk uid txargs
		thunk <- forestM $ newTxICThunk m tree
		forestM $ bufferTxICFSThunk var thunk
		return var
	-- read on the internal thunk, not on the latest filesystem
	read var = do
		thunk <- forestM $ bufferedTxICFSThunk var
		tree <- forestM $ latestTree
		liftM Prelude.fst $ readTxICThunk thunk tree

instance Input (FSThunk TxICFS) Inside (IncForest TxICFS) where
	ref c = Inc.new (return c)
	mod = Inc.new
	get = Inc.read
	set (var :: ForestFSThunk TxICFS Inside a) v = do
		(thunk :: TxICThunk Inside a) <- forestM $ bufferedTxICFSThunk var
		writeTxICThunk thunk v
	overwrite (var :: ForestFSThunk TxICFS Inside a) mv = do
		(thunk :: TxICThunk Inside a) <- forestM $ bufferedTxICFSThunk var
		overwriteTxICThunk thunk mv
instance Input (FSThunk TxICFS) Outside (IncForest TxICFS) where
	ref c = Inc.new (return c)
	mod = Inc.new
	get = Inc.read
	set (var :: ForestFSThunk TxICFS Outside a) v = do
		(thunk :: TxICThunk Outside a) <- forestM $ bufferedTxICFSThunk var
		writeTxICThunk thunk v
	overwrite (var :: ForestFSThunk TxICFS Outside a) mv = do
		(thunk :: TxICThunk Outside a) <- forestM $ bufferedTxICFSThunk var
		overwriteTxICThunk thunk mv

-- ** Transactions

-- the Forest transactional monad
type TxICFTM = FTM TxICFS
-- a Forest transactional variable
type TxICFTV a = FTV TxICFS a

instance Transactional (IncForest TxICFS) where
	atomically = atomicallyTxICFS "/"
	retry = retryTxICFS
	orElse = orElseTxICFS
instance MonadThrow (Outside (IncForest TxICFS)) where
	throwM = throwTxICFS 
instance MonadCatch (Outside (IncForest TxICFS)) where
	catch = catchTxICFS False

instance TransactionalForest TxICFS where	
	new = newTxICFS Proxy
	args = argsTxICFS Proxy
	read = readTxICFS Proxy
	writeOrElse = writeOrElseTxICFS
	delete = deleteTxICFS Proxy
	copyOrElse = copyOrElseTxICFS Proxy

deleteTxICFS :: FTK TxICFS args rep var content => Proxy args -> rep -> TxICFTM ()
deleteTxICFS proxy (rep :: rep) = do
	mb <- forestM $ getFTVArgsIC proxy rep
	case mb of
		Nothing -> error "tried to write to a variable that is not connected to the FS"
		Just (args,path) -> do
			def_rep :: rep <- inside $ zdefaultScratchMemo proxy args path
			content <- inside $ BX.getM lens_content $ Inc.get $ to iso_rep_thunk def_rep
			writeOrElseTxICFS rep content () (error "failed to write")

copyOrElseTxICFS :: (FTK TxICFS args rep var content) => Proxy args -> rep -> rep -> b -> ([ManifestError] -> TxICFTM b) -> TxICFTM b
copyOrElseTxICFS proxy src tgt b f = do
	mb_src <- forestM $ getFTVArgsIC proxy src
	mb_tgt <- forestM $ getFTVArgsIC proxy tgt
	case (mb_src,mb_tgt) of
		(Just (args_src,path_src),Just (args_tgt,path_tgt)) -> do
			-- XXX: this may fail if FileInfo paths are canonized...
			let chgPath path = path_tgt </> (makeRelative path_src path)
			tgt' <- copyFSThunks proxyTxICFS proxyOutside chgPath src
			content' <- inside $ BX.getM lens_content $ Inc.get $ to iso_rep_thunk tgt'
			writeOrElseTxICFS tgt content' b f
		otherwise -> error "tried to write to a variable that is not connected to the FS"

newTxICFS :: FTK TxICFS args rep var content => Proxy args -> ForestVs args -> FilePath -> TxICFTM rep
newTxICFS proxy args path = debug ("newTxICFS " ++ show path) $ inside $ zload (vmonadArgs proxyTxICFS proxy args) path

-- read the transactional variable by incrementally repairing its internal thunk to the latest tree
readTxICFS :: (ForestLayer TxICFS l,FTK TxICFS args rep var content) => Proxy args -> rep -> ForestL TxICFS l content
readTxICFS proxy rep = do
	let t = to iso_rep_thunk rep
	(txargs,path) <- liftM (fromJustNote "readTxICFS1") $ forestM $ getFTVArgsIC proxy rep
	oldtree <- inside $ treeTxICFSThunk t
	tree <- forestM latestTree
	df <- liftM (fromJustNote "readTxICFS2") $ forestM $ diffFS oldtree tree path
	-- load incrementally at the latest tree
	debug ("readTxICFS " ++ show path) $ inside $ unsafeWorld $ do
		dv <- diffValueThunk oldtree rep
		ds <- deltaArgs oldtree proxy txargs txargs
		let deltas = (txargs,ds)
		zloadDeltaMemo proxy deltas (return path) oldtree (rep,getForestMDInTree) path df tree dv
	res <- inside $ BX.getM lens_content $ Inc.get t
	return res

data ManifestConflictTx = ManifestConflictTx [ManifestError] deriving (Show,Eq,Typeable)
instance Exception ManifestConflictTx

writeOrElseTxICFS :: FTK TxICFS args rep var content => rep -> content -> b -> ([ManifestError] -> TxICFTM b) -> TxICFTM b
writeOrElseTxICFS rep content b f = do

	let t = to iso_rep_thunk rep
		
	mb :: (Maybe (ForestIs TxICFS args,FilePath)) <- forestM $ getFTVArgsIC Proxy rep
	case mb of
		Nothing -> error "writeOrElseTxICFS: should not happen" -- the top-level arguments of a variable don't match the spec
		Just (txargs,path) -> debug ("writeOrElseTxICFS " ++ show path) $ do
			let proxy = Proxy :: Proxy args
			oldtree <- inside $ treeTxICFSThunk t
			tree <- forestM latestTree
			df <- liftM (fromJustNote "writeOrElseTxICFS") $ forestM $ diffFS oldtree tree path
			dv <- diffValueThunk oldtree rep
			ds <- deltaArgs oldtree proxy txargs txargs
			let deltas = (txargs,ds)
			
			-- update the value to the latest filesystem (since some variables may be outdated due to modifications to other descriptions)
			-- this is necessary because the store function reads the content from the inner thunks, that are not necessarily in sync with the latest filesystem
			-- skipping this step would eventually overwrite user modifications with older values!!
			zloadDeltaMemo proxy deltas (return path) oldtree (rep,getForestMDInTree) path df tree dv
			
			let tryWrite = do
				-- set the next tree with which writes will be consistent with
				forestM nextTxICFSTree
				-- write the new content
				modify t $ \s -> BX.putM lens_content (return s) (return content)
				
				-- compute new value deltas for the write
				newdv <- diffValueThunk tree rep
				newds <- deltaArgs tree proxy txargs txargs
				let newdeltas = (txargs,newds)
				
				-- store incrementally at the latest tree (there are no filesystem modifications, since only the value has changed since loadDelta)
				man <- forestM $ newManifestWith "/" tree
				(mani,_,memos) <- debug ("storing... " ++ show path) $ RWS.runRWST (zupdateManifestDeltaMemo proxy newdeltas path path tree (emptyFSTreeD proxyTxICFS) tree rep newdv man) True ()
				-- we need to store the errors to the (buffered) FS before validating
				forestM $ storeManifest mani
				-- commit the pending modifications
				forestM $ incrementTxICFSTree
				
				forestM $ forestIO $ putStrLn "Manifest!"
				forestM $ forestIO $ print mani
				errors <- forestM $ manifestErrors mani
				if List.null errors
					then forestM latestTree >>= memos >> return b
					else throwTxICFS $ ManifestConflictTx errors
			catchTxICFS True tryWrite (\(ManifestConflictTx errors) -> f errors)
					
atomicallyTxICFS :: FilePath -> TxICFTM b -> IO b
atomicallyTxICFS root stm = updateRootTxICFS root >> initializeTxICFS try where
	try = txICFSLayer proxyOutside $ flip Catch.catches [Catch.Handler catchInvalid,Catch.Handler catchRetry,Catch.Handler catchSome] $ unTxICFSLayer proxyOutside $ do
		-- run the tx
		x <- stm
		-- tries to commit the current tx, otherwise repairs it incrementally
		success <- validateAndCommitTopTxICFS True
		if success
			then return x
			else debug "throw InvalidTx" $ txICFSLayer proxyOutside $ throwM InvalidTx
	catchInvalid InvalidTx = unTxICFSLayer proxyOutside $ debug "InvalidTx" $ do
		resetTxICFS try
	catchRetry BlockedOnRetry = unTxICFSLayer proxyOutside $ debug "BlockedOnRetry" $ do
		-- if the retry was invoked on an inconsistent state, we incrementally repair and run again, otherwise we place the tx in the waiting queue
		mbsuccess <- validateAndRetryTopTxICFS
		case mbsuccess of
			Just lck -> do -- retried txs are always in a consistent state, because we apply all affecting updates before releasing the lock
				-- wait for the lock to be released (whenever some variables that it depends on are changed)
				-- we don't consume the contents of the mvar to avoid further puts to succeeed; a new MVar is created for each retry
				unsafeIOToInc $ Lock.acquire lck
				resetTxICFS try
			Nothing -> resetTxICFS try
	catchSome (e::SomeException) = unTxICFSLayer proxyOutside $ debug ("SomeException "++show e) $ do
		-- we still need to validate on exceptions, otherwise repair incrementally; transaction-local allocations still get committed
		success <- validateAndCommitTopTxICFS False
		if success
			then txICFSLayer proxyOutside $ throwM e
			else do
				resetTxICFS try

retryTxICFS :: TxICFTM a
retryTxICFS = unsafeIOToInc $ throwIO BlockedOnRetry

orElseTxICFS :: TxICFTM a -> TxICFTM a -> TxICFTM a
orElseTxICFS stm1 stm2 = do1 where
	try1 = do { x <- stm1; validateAndCommitNestedTxICFS Nothing; return x }
	try2 = do { x <- stm2; validateAndCommitNestedTxICFS Nothing; return x }
	do1 = startNestedTxICFS $ txICFSLayer proxyOutside $ (unTxICFSLayer proxyOutside try1) `Catch.catches` [Catch.Handler catchRetry1,Catch.Handler catchInvalid,Catch.Handler catchSome]
	do2 = dropChildTxICLog $ startNestedTxICFS $ txICFSLayer proxyOutside $ (unTxICFSLayer proxyOutside try2) `Catch.catches` [Catch.Handler catchRetry2,Catch.Handler catchInvalid,Catch.Handler catchSome]
	catchRetry1 BlockedOnRetry = unTxICFSLayer proxyOutside $ validateAndRetryNestedTxICFS >> do2
	catchRetry2 BlockedOnRetry = unTxICFSLayer proxyOutside validateAndRetryNestedTxICFS >> throwM BlockedOnRetry
	catchInvalid (e::InvalidTx) = throwM e
	catchSome (e::SomeException) = unTxICFSLayer proxyOutside (validateAndCommitNestedTxICFS (Just e)) >> throwM e

throwTxICFS :: Exception e => e -> TxICFTM a
throwTxICFS = txICFSLayer proxyOutside . Catch.throwM

catchTxICFS :: Exception e => Bool -> TxICFTM a -> (e -> TxICFTM a) -> TxICFTM a
catchTxICFS doWrites stm (h :: e -> TxICFTM a) = txICFSLayer proxyOutside $ (unTxICFSLayer proxyOutside stm) `Catch.catches` [Catch.Handler catchInvalid,Catch.Handler catchRetry,Catch.Handler catchSome] where
	catchInvalid (e::InvalidTx) = throwM e
	catchRetry (e::BlockedOnRetry) = throwM e
	catchSome (e::e) = unTxICFSLayer proxyOutside $ do
		validateCatchTxICFS doWrites
		h e

-- the next tree is initialized as the same as the current tree
initializeTxICFS :: TxICFTM b -> IO b 
initializeTxICFS (TxICFSForestO m) = do
	starttime <- startTxICFS >>= newIORef
	mountAVFS -- should succeed even if AVFS is already mounted
	putStrLn "mounted AVFS"
	txid <- newUnique
	-- current tree
	treeDelta <- newIORef $ emptyFSTreeDelta
	treeSyms <- newIORef =<< liftM (DList.empty,) (readMVar symlinks)
	let tree = TxICFSTree txid 0 treeDelta False treeSyms
	-- tables
	bufftbl <- MemoTable.new
	memotbl <- MemoTable.new
	txlog <- newIORef (tree,DList.empty,tree,Set.empty,bufftbl,memotbl,CanonicalTree Map.empty,Set.empty)
	deltas <- newIORef Map.empty
	debug ("initializeTxICFS") $ Reader.runReaderT m $ TxICFSEnv (starttime :!: txid :!: deltas :!: SCons txlog SNil)
	-- don't unmount AVFS, since multiple parallel txs may be using it

-- resets
resetTxICFS :: TxICFTM a -> TxICFTM a
resetTxICFS m = do
	now <- unsafeIOToInc $ startTxICFS >>= newIORef
	TxICFSEnv (_ :!: txid :!: deltas :!: _) <- TxICFSForestO Reader.ask
	-- current tree
	treeDelta <- forestM $ forestIO $ newIORef $ emptyFSTreeDelta
	treeSyms <- forestM $ forestIO $ newIORef =<< liftM (DList.empty,) (readMVar symlinks)
	let tree = TxICFSTree txid 0 treeDelta False treeSyms
	-- tables
	bufftbl <- forestM $ forestIO $ MemoTable.new
	memotbl <- forestM $ forestIO $ MemoTable.new
	txlog <- forestM $ forestIO $ newIORef (tree,DList.empty,tree,Set.empty,bufftbl,memotbl,CanonicalTree Map.empty,Set.empty)
	forestM $ forestIO $ writeIORef deltas Map.empty
	debug ("resetTxICFS") $ TxICFSForestO $ Reader.local (Prelude.const $ TxICFSEnv (now :!: txid :!: deltas :!: SCons txlog SNil)) $ runTxICFSForestO m

-- we need to acquire a lock, but this should be minimal
startTxICFS :: IO UTCTime
startTxICFS = getCurrentTime >>= \t -> addRunningTxIC t >> return t

-- appends a freshly created txlog for the inner tx
-- the starting time reference is preserved
--we create a new fsversion reference
startNestedTxICFS :: TxICFTM a -> TxICFTM a
startNestedTxICFS m = do
	TxICFSEnv (starttime :!: txid :!: deltas :!: txlogs@(SCons txlog_parent _)) <- TxICFSForestO Reader.ask
	(tree,delta,newtree,reads,_,_,cantree,tmps) <- unsafeIOToInc $ readIORef txlog_parent
	bufftbl' <- forestM $ forestIO $ MemoTable.new
	memotbl' <- forestM $ forestIO $ MemoTable.new
	txlog_child <- unsafeIOToInc $ newIORef (tree,delta,newtree,Set.empty,bufftbl',memotbl',cantree,Set.empty)
	debug ("startNestedTxICFS ") $ TxICFSForestO $ Reader.local (Prelude.const $ TxICFSEnv (starttime :!: txid :!: deltas :!: SCons txlog_child txlogs)) $ runTxICFSForestO m

-- remember to empty the next tree and delete the new versions in case nested txs fail
dropChildTxICLog :: TxICFTM a -> TxICFTM a
dropChildTxICLog m = do
	TxICFSEnv (starttime :!: txid :!: deltas :!: (SCons txlog_child txlogs@(SCons txlog_parent _))) <- TxICFSForestO Reader.ask
	(tree_parent,_,newtree_parent,_,_,_,_,_) <- unsafeIOToInc $ readIORef txlog_parent
	-- forget newly created nested versions
	forestM $ forestIO $ modifyIORef deltas $ Map.filterWithKey (\version _ -> version <= fsTreeFSVersion tree_parent)
	-- reset the next tree
	forestM $ forestIO $ writeIORef (fsTreeFSTreeDelta newtree_parent) emptyFSTreeDelta
	TxICFSForestO $ Reader.local (Prelude.const $ TxICFSEnv (starttime :!: txid :!: deltas :!: txlogs)) $ runTxICFSForestO m

-- if an inner tx validation fails, then we throw an @InvalidTx@ exception to retry the whole atomic block
data InvalidTx = InvalidTx deriving (Typeable)
instance Show (InvalidTx) where
	show InvalidTx = "InvalidTx"
instance Exception InvalidTx
data BlockedOnRetry = BlockedOnRetry deriving (Show,Typeable)
instance Exception BlockedOnRetry

-- returns a bool stating whether the transaction was committed or needs to be incrementally repaired
-- no exceptions should be raised inside this block
validateAndCommitTopTxICFS :: Bool -> TxICFTM Bool
validateAndCommitTopTxICFS doWrites = atomicTxICFS "validateAndCommitTopTxICFS" $ do
	txenv@(TxICFSEnv (timeref :!: txid :!: deltas :!: txlogs@(SCons txlog SNil))) <- TxICFSForestO Reader.ask
	starttime <- unsafeIOToInc $ readIORef timeref
	success <- forestM $ validateTxsICFS starttime txlogs
	if success
		then do
			forestM $ commitTopTxICFS doWrites starttime txlog
			return True
		else do
			unsafeIOToInc $ deleteRunningTxIC starttime
			return False

validateAndCommitNestedTxICFS :: Maybe SomeException -> TxICFTM ()
validateAndCommitNestedTxICFS mbException = do
	txenv@(TxICFSEnv (timeref :!: txid :!: deltas :!: txlogs@(SCons txlog1 (SCons txlog2 _)))) <- TxICFSForestO Reader.ask
	starttime <- unsafeIOToInc $ readIORef timeref
	case mbException of
		Just e -> do -- throwing an exception exits the chain of txs one by one
			forestM $ commitNestedTxICFS False deltas txlog1 txlog2 -- does not perform @Write@s
		Nothing -> do
			-- validates the current and enclosing txs up the tx tree
			success <- forestM $ validateTxsICFS starttime txlogs
			if success
				then do
					forestM $ commitNestedTxICFS True deltas txlog1 txlog2 -- performs @Write@s
				else do
					unsafeIOToInc $ deleteRunningTxIC starttime
					txICFSLayer proxyOutside $ throwM InvalidTx

-- validates a transaction and places it into the waiting queue for retrying
validateAndRetryTopTxICFS :: TxICFTM (Maybe Lock)
validateAndRetryTopTxICFS = atomicTxICFS "validateAndRetryTopTxICFS" $ do
	txenv@(TxICFSEnv (timeref :!: txid :!: deltas :!: txlogs@(SCons txlog SNil))) <- TxICFSForestO Reader.ask
	starttime <- unsafeIOToInc $ readIORef timeref
	-- validates the current and enclosing txs up the tx tree
	success <- forestM $ validateTxsICFS starttime txlogs
	if success
		then do
			lck <- unsafeIOToInc $ Lock.newAcquired -- sets the tx lock as acquired; the tx will be resumed when the lock is released
			forestM $ commitTopTxICFS False starttime txlog
			forestM $ retryTxICFSLog lck timeref txlog -- wait on changes to retry (only registers waits, does not actually wait)
			return $ Just lck
		else do
			unsafeIOToInc $ deleteRunningTxIC starttime
			return Nothing

--registers waits for all the filepaths read by a txlog
--since we treat reads/writes separately, we don't need to wait on writes that have not been read
retryTxICFSLog :: Lock -> IORef UTCTime -> TxICFSLog -> ForestM TxICFS ()
retryTxICFSLog lck timeref txlog = do
	(tree,delta,newtree,reads,bufftbl,memotbl,cantree,tmps) <- forestIO $ readIORef txlog
	let retryPath path = do
		-- the reference to the lock lives as long as the transaction
		modifyMVar_ waitingTxsIC $ \xs -> do
			case Map.lookup path xs of
				Nothing -> newQ >>= \q -> return $ Map.insert path q xs
				Just q -> pushL q lck >> return xs
	forestIO $ Foldable.mapM_ retryPath reads

-- validates a nested transaction and merges its log with its parent
-- note that retrying discards the tx's writes
validateAndRetryNestedTxICFS :: TxICFTM ()
validateAndRetryNestedTxICFS = do
	txenv@(TxICFSEnv (timeref :!: txid :!: deltas :!: txlogs@(SCons txlog1 (SCons txlog2 _)))) <- TxICFSForestO Reader.ask
	starttime <- unsafeIOToInc $ readIORef timeref
	success <- forestM $ validateTxsICFS starttime txlogs
	if success
		then do
			forestM $ commitNestedTxICFS False deltas txlog1 txlog2 -- does not perform @Write@s on @retry@
		else do
			unsafeIOToInc $ deleteRunningTxIC starttime
			txICFSLayer proxyOutside $ throwM InvalidTx

-- validates the current log before catching an exception
validateCatchTxICFS :: Bool -> TxICFTM ()
validateCatchTxICFS doWrites = do
	txenv@(TxICFSEnv (timeref :!: txid :!: deltas :!: txlogs)) <- TxICFSForestO Reader.ask
	starttime <- unsafeIOToInc $ readIORef timeref
	success <- forestM $ validateTxsICFS starttime txlogs
	if success
		then do
			-- in case the computation raises an exception, discard all its visible (write) effects
			unless doWrites $ forestM unbufferTxICFSWrites
		else do
			unsafeIOToInc $ deleteRunningTxIC starttime
			txICFSLayer proxyOutside $ throwM InvalidTx

parentCanonicalTree :: TxICFSLogs -> ForestM TxICFS (CanonicalTree TxICFS)
parentCanonicalTree SNil = return $ CanonicalTree Map.empty
parentCanonicalTree (SCons txlog _) = do
	(tree,delta,newtree,reads,bufftbl,memotbl,cantree,tmps) <- forestIO $ readRef txlog
	return cantree

parentSyms :: TxICFSLogs -> ForestM TxICFS (DList FSDelta,FSTreeSym)
parentSyms SNil = forestIO $ liftM (DList.empty,) $ readMVar symlinks
parentSyms (SCons txlog _) = do
	(tree,delta,newtree,reads,bufftbl,memotbl,cantree,tmps) <- forestIO $ readRef txlog
	forestIO $ readIORef $ fsTreeSym tree

parentDelta :: TxICFSLogs -> ForestM TxICFS FSTreeDelta
parentDelta SNil = return emptyFSTreeDelta
parentDelta (SCons txlog _) = do
	(tree,delta,newtree,reads,bufftbl,memotbl,cantree,tmps) <- forestIO $ readRef txlog
	forestIO $ readIORef $ fsTreeFSTreeDelta tree

parentVirtual :: TxICFSLogs -> ForestM TxICFS Bool
parentVirtual SNil = return False
parentVirtual (SCons txlog _) = do
	(tree,delta,newtree,reads,bufftbl,memotbl,cantree,tmps) <- forestIO $ readRef txlog
	return $ fsTreeVirtual tree

-- we only unbuffer the child log
unbufferTxICFSWrites :: ForestM TxICFS ()
unbufferTxICFSWrites = do
	TxICFSEnv (starttime :!: txid :!: deltas :!: txlogs@(SCons txlog1 txlogs_parent)) <- TxICFSForestM Reader.ask
	(tree1,delta1,newtree1,reads1,bufftbl1,memotbl1,cantree1,tmps1) <- forestIO $ readRef txlog1
	bufftbl1' <- forestIO $ MemoTable.new
	memotbl1' <- forestIO $ MemoTable.new
	
	-- reset deltas
	forestIO $ writeRef deltas Map.empty
	-- current tree
	treeDelta1' <- parentDelta txlogs_parent >>= forestIO . newIORef
	treeVirtual1' <- parentVirtual txlogs_parent
	treeSym1 <- forestIO $ readIORef $ fsTreeSym tree1
	treeSym1' <- case treeSym1 of
		((==DList.empty) -> True,syms) -> forestIO $ newIORef treeSym1
		otherwise -> parentSyms txlogs_parent >>= forestIO . newIORef
	let tree1' = TxICFSTree txid (fsTreeFSVersion tree1) treeDelta1' treeVirtual1' treeSym1'
	cantree1' <- parentCanonicalTree txlogs_parent
	forestIO $ writeRef txlog1 (tree1',DList.empty,tree1',reads1,bufftbl1',memotbl1',cantree1',tmps1)

validateTxsICFS :: UTCTime -> TxICFSLogs -> ForestM TxICFS Bool
validateTxsICFS starttime txlogs = do
	-- gets the transactions that committed after the current transaction's start time
	finished <- liftM (Map.toAscList . Map.filterWithKey (\k v -> k > starttime)) $ forestIO $ readMVar doneTxsIC
	checkTxsICFS txlogs finished

checkTxsICFS :: TxICFSLogs -> [(UTCTime,TxICFSWrites)] -> ForestM TxICFS Bool
checkTxsICFS SNil finished = return True
checkTxsICFS env@(SCons txlog txlogs) finished = do
	b1 <- checkTxICFS txlog finished
	b2 <- checkTxsICFS txlogs finished
	return $ b1 && b2

-- checks if the current txlog is consistent with a sequence of concurrent modifications
checkTxICFS :: TxICFSLog -> [(UTCTime,TxICFSWrites)] -> ForestM TxICFS Bool
checkTxICFS txlog wrts = liftM List.and $ Prelude.mapM (checkTxICFS' txlog) wrts where
	checkTxICFS' txlog (txtime,paths) = do
		TxICFSEnv (starttime_ref :!: txid :!: _ :!: SCons txlog _) <- TxICFSForestM Reader.ask
		starttime <- forestIO $ readIORef starttime_ref
		forestIO $ putStrLn $ "checking " ++ show starttime ++ " against " ++ show txtime
		Foldable.foldrM (\path b -> liftM (b &&) $ checkTxICFSWrite txlog path) True paths
	checkTxICFSWrite txlog path = do
		-- we only check for write-read conflicts
		(reads,_) <- getTxICFSChangesFlat
		return $ not $ path `Set.member` reads

commitTopTxICFS :: Bool -> UTCTime -> TxICFSLog -> ForestM TxICFS ()
commitTopTxICFS doWrites starttime txlog = do
	-- deletes this transaction from the running list and gets the earliest running tx 
	mbearliestTx <- forestIO $ modifyMVarMasked runningTxsIC (\xs -> return (List.delete starttime xs,lastMay xs))
	-- commits the log and gets a sequence of performed writes
	writes <- commitTxICFSLog starttime doWrites txlog
	-- finishes the current tx and deletes txs that finished before the start of the earliest running tx
	-- we don't need to log transactions with empty commits (no @Eval@s or @Write@s)
	let addDone time m = if Set.null writes then m else Map.insert time writes m
	now <- case mbearliestTx of
		Just earliestTx -> forestIO $ modifyMVarMasked doneTxsIC (\m -> getCurrentTime >>= \now -> let m' = Map.filterWithKey (\t _ -> t > earliestTx) (addDone now m) in m' `seq` return (m',now))
		Nothing -> forestIO $ modifyMVarMasked doneTxsIC (\m -> getCurrentTime >>= \now -> let m' = addDone now m in m' `seq` return (m',now))
	-- wakes up the transactions after updating their buffered content
	wakeUpWaitsTxIC writes

-- makes the parent log sensitive to the variables used in the nested branch
commitNestedTxICFS :: Bool -> TxICFSTreeDeltas -> TxICFSLog -> TxICFSLog -> ForestM TxICFS ()
commitNestedTxICFS doWrites deltas txlog_child txlog_parent = if doWrites
	then mergeTxICFSLog txlog_child txlog_parent
	else extendTxICFSLog deltas txlog_child txlog_parent

-- merges a nested txlog with its parent txlog
mergeTxICFSLog :: TxICFSLog -> TxICFSLog -> ForestM TxICFS ()
mergeTxICFSLog txlog1 txlog2 = forestIO $ do
	(tree1,delta1,newtree1,reads1,bufftbl1,memotbl1,cantree1,tmps1) <- readIORef txlog1
	(tree2,delta2,newtree2,reads2,bufftbl2,memotbl2,cantree2,tmps2) <- readIORef txlog2
	
	MemoTable.mapM_ (\(uid,entry) -> MemoTable.insertWithMkWeak bufftbl2 uid entry (dynTxMkWeak entry)) bufftbl1 
	MemoTable.mapM_ (\(uid,entry@(_,mkWeak,_)) -> MemoTable.insertWithMkWeak memotbl2 uid entry mkWeak) memotbl1
	
	writeIORef txlog2 (tree1,delta1,newtree1,reads1 `Set.union` reads2,bufftbl2,memotbl2,cantree1,tmps1 `Set.union` tmps2)
	
-- does not commit writes
extendTxICFSLog :: TxICFSTreeDeltas -> TxICFSLog -> TxICFSLog -> ForestM TxICFS ()
extendTxICFSLog deltas txlog1 txlog2 = forestIO $ do
	(tree1,delta1,newtree1,reads1,bufftbl1,memotbl1,cantree1,tmps1) <- readIORef txlog1
	(tree2,delta2,newtree2,reads2,bufftbl2,memotbl2,cantree2,tmps2) <- readIORef txlog2
	
	-- forget newly created nested versions
	modifyIORef deltas $ Map.filterWithKey (\version _ -> version <= fsTreeFSVersion tree2)
	-- reset the next tree
	writeIORef (fsTreeFSTreeDelta newtree2) emptyFSTreeDelta
	
	writeIORef txlog2 (tree2,delta2,newtree2,reads1 `Set.union` reads2,bufftbl2,memotbl2,cantree2,tmps1 `Set.union` tmps2)

-- wakes up the locks waiting on a given set of paths
wakeUpWaitsTxIC :: Set FilePath -> ForestM TxICFS ()
wakeUpWaitsTxIC = Foldable.mapM_ wakeUpWaitTxIC

wakeUpWaitTxIC :: FilePath -> ForestM TxICFS ()
wakeUpWaitTxIC path = forestIO $ do
	let wakeQueue q = do		
		mb <- tryPopR q
		case mb of
			Just lck -> tryReleaseTxIC lck >> wakeQueue q
			Nothing -> return ()
	mb <- modifyMVar waitingTxsIC (\xs -> return (Map.delete path xs,Map.lookup path xs))
	case mb of
		Nothing -> return ()
		Just q -> wakeQueue q

tryReleaseTxIC :: Lock -> IO ()
tryReleaseTxIC lck = do
	isLocked <- Lock.locked lck
	if isLocked then Lock.release lck else return ()

commitTxICFSLog :: UTCTime -> Bool -> TxICFSLog -> ForestM TxICFS TxICFSWrites
commitTxICFSLog starttime doWrites txlog = do
	(tree,delta,newtree,reads,bufftbl,memotbl,cantree,tmps) <- forestIO $ readIORef txlog
	wakes <- if doWrites
		then do
			-- get writes since the beginning of the tx
			writes <- forestIO $ readIORef (fsTreeFSTreeDelta tree)
			-- commits filesystem modifications
			wakes <- forestIO $ commitFSTreeDelta "" writes
			-- get the symlink changes since the beginning of the tx
			(syms,_) <- forestIO $ readIORef (fsTreeSym tree)
			-- update the symlinks table (uses a global lock)
			forestIO $ modifyMVar_ symlinks $ return . appendListToFSTreeSym syms
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

{-# NOINLINE fileLocks #-}
fileLocks :: CWeakMap.WeakMap FilePath FLock
fileLocks = unsafePerformIO $ CWeakMap.empty

-- a reference to a lock lives as long as the lock itself, and the lock lives at least as long as it is acquired by a transaction
fileLock :: FilePath -> IO FLock
fileLock path = CWeakMap.lookupOrInsert fileLocks path newFLockIO (\v mb -> STM.mkWeakTMVar v (maybe (return ()) id mb))

atomicTxICFS :: String -> TxICFTM a -> TxICFTM a
atomicTxICFS msg m = do
	-- get the changes of the innermost txlog
	(reads,writes) <- forestM getTxICFSChangesFlat
	-- wait on currently acquired read locks (to ensure that concurrent writes are seen by this tx's validation step)
	forestM $ forestIO $ print $ "entering atomic " ++ msg
	x <- withFileLocks reads writes m
	forestM $ forestIO $ print $ "left atomic " ++ msg
	return x

-- acquiring the locks in sorted order is essential to avoid deadlocks!
withFileLocks :: Set FilePath -> Set FilePath -> TxICFTM a -> TxICFTM a
withFileLocks reads writes m = txICFSLayer proxyOutside $ do
	rcks <- lift $ State.mapM fileLock $ Set.toAscList reads
	wcks <- lift $ State.mapM fileLock $ Set.toAscList writes
		
	let waitAndAcquire wcks = lift $ STM.atomically $ do
		-- wait on read locks
		Foldable.mapM_ waitOrRetryFLock rcks
		-- acquire write locks or retry
		Foldable.mapM_ acquireOrRetryFLock wcks
		
	liftA2 Catch.bracket_ waitAndAcquire (lift . STM.atomically . Foldable.mapM_ releaseFLock) wcks (unTxICFSLayer proxyOutside m)

updateRootTxICFS :: FilePath -> IO ()
updateRootTxICFS root = do
	oldroot <- readMVar rootTxICFS
	if (oldroot `isParentPathOf` root)
		then putStrLn $ "child root " ++ show root ++ " " ++ show oldroot
		else updateSymlinks
	swapMVar rootTxICFS root
	return ()

