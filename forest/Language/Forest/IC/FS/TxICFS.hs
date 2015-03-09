
{-# LANGUAGE OverlappingInstances, TypeOperators, ConstraintKinds, UndecidableInstances, TupleSections, FlexibleInstances, MultiParamTypeClasses, StandaloneDeriving, GeneralizedNewtypeDeriving, FlexibleContexts, DataKinds, TypeFamilies, Rank2Types, GADTs, ViewPatterns, DeriveDataTypeable, ScopedTypeVariables #-}

-- Regular filesystem with optimistic concurrency support for transactions, with mutable transactional variables structures mapped to specifications, and incremental reuse within transactions

module Language.Forest.IC.FS.TxICFS (
	TxICForest(..)
) where

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

type TxICLayer l = (MonadReader TxICFSEnv (ForestL TxICFS l),ForestLayer TxICFS l)

type instance IncK (IncForest TxICFS) a = (Typeable a,Eq a,AddTxICParent Inside a,AddTxICParent Outside a)

proxyTxICFS = Proxy :: Proxy TxICFS

-- a list of the starting times of running transactions sorted from newest to oldest (we may two txs with the same starting time)
{-# NOINLINE runningTxsIC #-}
runningTxsIC :: MVar [UTCTime]
runningTxsIC = unsafePerformIO $ newMVar []

-- insert a new time in a list sorted from newest to oldest
addRunningTxIC time = modifyMVarMasked_ runningTxsIC (\xs -> return $ List.insertBy (\x y -> compare y x) time xs)
-- remove a time in a list sorted from newest to oldest
deleteRunningTxIC time = modifyMVarMasked_ runningTxsIC (\xs -> return $ List.delete time xs)

-- a map with commit times of committed transactions and their performed changes
-- we use a @FSTreeDelta@ because the set of written paths may be infinite (like all paths under a given directory)
{-# NOINLINE doneTxsIC #-}
doneTxsIC :: MVar (Map UTCTime TxICFSWrites)
doneTxsIC = unsafePerformIO $ newMVar Map.empty

-- ** Filesystem

-- latest tree,delta after the current tree,next tree,reads since the beginning, buffered variables, memoized forest transactional variables, temporary paths since the beginning
type TxICFSLog = IORef (FSTree TxICFS,FSDeltas,FSTree TxICFS,TxICFSReads,TxICBuffTable,TxICMemoTable,Set FilePath)

type TxICBuffTable = WeakTable Unique DynTxICThunk
type TxICMemoTable = WeakTable (FilePath,TypeRep) (Dynamic,MkWeak)

-- nested logs for nested transactions
type TxICFSLogs = SList TxICFSLog

-- a chronological list of relative deltas for the different FS versions
type TxICFSTreeDeltas = IORef (Map FSVersion FSDeltas)

-- (starttime,tx id,register of deltas,nested logs)
type TxICFSEnv = (IORef UTCTime,TxId,TxICFSTreeDeltas,TxICFSLogs)

type TxICFSReads = Set FilePath
type TxICFSWrites = Set FilePath
type TxICFSChanges = (TxICFSReads,FSTreeDelta)
type TxICFSChangesFlat = (TxICFSReads,TxICFSWrites)

bufferTxICFSThunk :: (IncK (IncForest TxICFS) a,AddTxICParent l a,TxICLayer l) => ForestFSThunk TxICFS l a -> TxICThunk l a -> ForestM TxICFS ()
bufferTxICFSThunk var thunk = do
	(starttime,txid,deltas,SCons txlog _) <- Reader.ask
	(tree,ds,newtree,reads,bufftbl,memotbl,tmps) <- forestIO $ readRef txlog
	let mkWeak = MkWeak $ mkWeakRefKey $ txICFSThunkArgs var
	forestIO $ WeakTable.insertWithMkWeak bufftbl mkWeak (txICFSThunkId var) $ DynTxICThunk thunk mkWeak
	
bufferTxICHSThunk :: (IncK (IncForest TxICFS) a,AddTxICParent l a,TxICLayer l) => ForestHSThunk TxICFS l a -> TxICThunk l a -> ForestM TxICFS ()
bufferTxICHSThunk var thunk = do
	(starttime,txid,deltas,SCons txlog _) <- Reader.ask
	(tree,ds,newtree,reads,bufftbl,memotbl,tmps) <- forestIO $ readRef txlog
	let mkWeak = MkWeak $ mkWeakRefKey $ txICHSThunkArgs var
	forestIO $ WeakTable.insertWithMkWeak bufftbl mkWeak (txICHSThunkId var) $ DynTxICThunk thunk mkWeak

-- reads the pointer from a transactional variable to a thunk
bufferedTxICFSThunk :: (Typeable l,Typeable a) => ForestFSThunk TxICFS l a -> ForestM TxICFS (TxICThunk l a)
bufferedTxICFSThunk var = do
	(starttime,txid,deltas,txlogs) <- Reader.ask
	let find mb txlog = case mb of
		Nothing -> do
			(tree,ds,newtree,reads,bufftbl,memotbl,tmps) <- forestIO $ readRef txlog
			mb <- forestIO $ WeakTable.lookup bufftbl (txICFSThunkId var)
			case mb of
				Just (DynTxICThunk (cast -> Just thunk) _) -> return $ Just thunk
				Nothing -> return Nothing
		otherwise -> return mb
	liftM fromJust $ Foldable.foldlM find Nothing txlogs

bufferedTxICHSThunk :: (Typeable l,Typeable a) => ForestHSThunk TxICFS l a -> ForestM TxICFS (TxICThunk l a)
bufferedTxICHSThunk var = do
	(starttime,txid,deltas,txlogs) <- Reader.ask
	let find mb txlog = case mb of
		Nothing -> do
			(tree,ds,newtree,reads,bufftbl,memotbl,tmps) <- forestIO $ readRef txlog
			mb <- forestIO $ WeakTable.lookup bufftbl (txICHSThunkId var)
			case mb of
				Just (DynTxICThunk (cast -> Just thunk) _) -> return $ Just thunk
				Nothing -> return Nothing
		otherwise -> return mb
	liftM fromJust $ Foldable.foldlM find Nothing txlogs

getNextFSTree :: MonadReader TxICFSEnv (ForestM TxICFS) => ForestM TxICFS (FSTree TxICFS)
getNextFSTree = do
	(starttime,txid,deltas,SCons txlog _) <- Reader.ask
	(tree,ds,newtree,reads,bufftbl,memotbl,tmps) <- forestIO $ readRef txlog
	return newtree

incrementTxICFSTree :: MonadReader TxICFSEnv (ForestM TxICFS) => ForestM TxICFS ()
incrementTxICFSTree = do
	(starttime,txid,deltas,SCons txlog _) <- Reader.ask
	(tree,ds,newtree,reads,bufftbl,memotbl,tmps) <- forestIO $ readRef txlog
	-- the new tree deltas are appended to the original deltas
	nextTreeDeltas <- forestIO $ readRef (fsTreeFSTreeDelta tree) >>= newRef
	nextSyms <- forestIO $ readIORef (fsTreeSym newtree) >>= newIORef
	-- create the new pending tree
	let nexttree = TxICFSTree (fsTreeTxId newtree) (succ $ fsTreeFSVersion newtree) nextTreeDeltas (fsTreeVirtual newtree) nextSyms
	-- add the relative deltas
	tds <- forestIO $ readRef deltas
	forestIO $ writeRef deltas $ Map.insert (fsTreeFSVersion newtree) ds tds
	forestIO $ writeRef txlog (newtree,DList.empty,nexttree,reads,bufftbl,memotbl,tmps)

-- only needs to read from the top-level log
getTxICFSChangesFlat :: ForestM TxICFS TxICFSChangesFlat
getTxICFSChangesFlat = do
	(starttime,txid,deltas,SCons txlog _) <- Reader.ask
	(tree,ds,newtree,reads,bufftbl,memotbl,tmps) <- forestIO $ readRef txlog
	writes <- forestIO $ readRef (fsTreeFSTreeDelta tree)
	return (reads,fsTreeDeltaWrites "" writes)

-- reads from the current tree
-- only needs to read from the top-level log
getTxICFSChanges :: ForestM TxICFS TxICFSChanges
getTxICFSChanges = do
	(starttime,txid,deltas,SCons txlog _) <- Reader.ask
	(tree,ds,newtree,reads,bufftbl,memotbl,tmps) <- forestIO $ readIORef txlog
	writes <- forestIO $ readRef (fsTreeFSTreeDelta tree)
	return (reads,writes)

putTxICFSRead :: FilePath -> ForestM TxICFS ()
putTxICFSRead path = do
	(starttime,txid,deltas,SCons txlog _) <- Reader.ask
	(tree,ds,newtree,reads,bufftbl,memotbl,tmps) <- forestIO $ readIORef txlog
	forestIO $ writeIORef txlog (tree,ds,newtree,Set.insert path reads,bufftbl,memotbl,tmps)

putTxICFSTmp :: FilePath -> ForestM TxICFS ()
putTxICFSTmp tmp = do
	(starttime,txid,deltas,SCons txlog _) <- Reader.ask
	(tree,ds,newtree,reads,bufftbl,memotbl,tmps) <- forestIO $ readIORef txlog
	forestIO $ writeRef txlog (tree,ds,newtree,reads,bufftbl,memotbl,Set.insert tmp tmps)

-- writes to the next tree
-- duplicate the action on both the relative deltas (sequence of primitve FSDelta) and the absolute deltas (FSTreeDelta)
modifyTxICFSTreeDeltas :: FSDelta -> ForestM TxICFS ()
modifyTxICFSTreeDeltas d = do
	(starttime,txid,deltas,SCons txlog _) <- Reader.ask
	(tree,ds,newtree,reads,bufftbl,memotbl,tmps) <- forestIO $ readIORef txlog
	forestIO $ modifyIORef (fsTreeFSTreeDelta newtree) (appendToFSTreeDelta d)
	forestIO $ modifyIORef (fsTreeSym newtree) (\(ds,ts) -> (DList.snoc ds d,appendToFSTreeSym d ts))
	forestIO $ writeIORef txlog (tree,DList.snoc ds d,newtree,reads,bufftbl,memotbl,tmps)

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

instance Eq (FSTree TxICFS) where
	(TxICFSTree txid1 fsv1 _ virtual1 sym1) == (TxICFSTree txid2 fsv2 _ virtual2 sym2) = txid1 == txid2 && fsv1 == fsv2 && virtual1 == virtual2

instance MonadReader TxICFSEnv (ForestM TxICFS) where
	ask = TxICFSForestM ask
	local f (TxICFSForestM m) = TxICFSForestM $ local f m

instance Show (FSTree TxICFS) where
	show tree = "FSTreeTxICFS " ++ show (fsTreeTxId tree) ++" "++ show (fsTreeFSVersion tree) ++" "++ show (fsTreeVirtual tree)

instance Ord (FSTree TxICFS) where
	tree1 <= tree2 = (fsTreeTxId tree1,fsTreeFSVersion tree1,fsTreeVirtual tree1) <= (fsTreeTxId tree2,fsTreeFSVersion tree2,fsTreeVirtual tree2)

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

instance FSRep TxICFS where
	
	newtype ForestM TxICFS a = TxICFSForestM { runTxICFSForestM :: ReaderT TxICFSEnv IO a } deriving (Functor,Applicative,Monad,MonadLazy,MonadThrow,MonadCatch,MonadMask)
	
	data ForestCfg TxICFS = TxICFSForestCfg
	
	runForest _ m = error "please use atomically instead"
	
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
		(starttime,txid,deltas,SCons txlog _) <- Reader.ask
		(tree,ds,newtree,reads,bufftbl,memotbl,tmps) <- forestIO $ readIORef txlog
		return tree
	
	-- reads from the FS or from the modification log
	pathInTree path tree = do
		-- canonize the fullpath for reads
		canpath <- canonalizeDirectoryInTree path tree
		-- mark the path as read
		putTxICFSRead canpath
		(_,td) <- getTxICFSChanges
		let path_td = focusFSTreeDeltaByRelativePathMay td canpath
		let ondisk = case onDiskWriteMay path_td of
			Nothing -> canpath
			Just ondisk -> ondisk
		virtualizeTxICFS ondisk tree

	stepPathInTree tree path rel = do
		dir <- canonalizePathWithTree path tree
		return $ dir </> rel
	
	getDirectoryContentsInTree = getDirectoryContentsTxICFS
		
	doesDirectoryExistInTree = doesExistTxICFS doesDirectoryExist
	doesFileExistInTree = doesExistTxICFS doesFileExist
	doesExistInTree = doesExistTxICFS (\path -> doesDirectoryExist path >>= \isDir -> if isDir then return True else doesFileExist path)
	
	canonalizePathWithTree = canonalizePathTxICFS

	type FSTreeD TxICFS = FSTreeDelta -- a delta fixed-point focused on the current path

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
		if fsTreeTxId oldtree == fsTreeTxId newtree
			then do
				(starttime,txid,deltas,txlogs) <- Reader.ask
				tds <- forestIO $ readRef deltas
				let dfs = drop (fsTreeFSVersion oldtree) $ take (fsTreeFSVersion newtree) $ Map.elems tds
				syms <- liftM snd $ forestIO $ readRef (fsTreeSym newtree)
				let fixtd = fixFSTreeDelta (compressFSDeltas $ DList.concat dfs) syms
				let df = focusFSTreeDeltaByRelativePath fixtd path
				return $ Just df
			-- no sharing among different txs
			else return Nothing

virtualizeTxICFS path tree = if fsTreeVirtual tree
	then liftM (</> ".avfs" </> makeRelative "/" path) (forestIO getHomeDirectory)
	else return path

getDirectoryContentsTxICFS :: FilePath -> FSTree TxICFS -> ForestM TxICFS [FileName]
getDirectoryContentsTxICFS path tree = do
	canpath <- canonalizeDirectoryInTree path tree
	putTxICFSRead canpath
	(_,td) <- getTxICFSChanges
	let path_td = focusFSTreeDeltaByRelativePathMay td canpath
	xs <- forestIO $ getContentsFSTreeDeltaNodeMay canpath path_td
	return xs

doesExistTxICFS :: (FilePath -> IO Bool) -> FilePath -> FSTree TxICFS -> ForestM TxICFS Bool
doesExistTxICFS test path tree = do
	canpath <- canonalizeDirectoryInTree path tree
	putTxICFSRead canpath
	(_,td) <- getTxICFSChanges
	let path_td = focusFSTreeDeltaByRelativePathMay td canpath
	case path_td of
		Just (FSTreeNew _ _ diskpath _) -> forestIO $ test diskpath
		otherwise -> forestIO $ test canpath

-- canonalizes a path, taking into account the buffered FSTreeDelta and logging reads of followed symlinks
canonalizePathTxICFS :: FilePath -> FSTree TxICFS -> ForestM TxICFS FilePath
canonalizePathTxICFS path tree = do
	norm <- forestIO $ liftM normalise $ absolutePath path
	let dirs = splitDirectories norm
	follow "" dirs
  where
	follow root [] = return root
	follow root (".":dirs) = follow root dirs
	follow root ("..":dirs) = follow (takeDirectory root) dirs
	follow root (dir:dirs) = do
		(reads,td) <- getTxICFSChanges
		let rootdir = root </> dir
		let td' = focusFSTreeDeltaByRelativePathMay td rootdir
		case td' of
			Just (FSTreeNewLink link _) -> do -- follow buffered links
				-- mark the symbolic link as read
				putTxICFSRead rootdir
				canLink <- canonalizePathTxICFS (root </> link) tree
				follow canLink dirs
			otherwise -> do -- follow FS links
				rootdir_dsk <- virtualizeTxICFS rootdir tree
				e <- forestIO $ Exception.try $ liftM isSymbolicLink (getSymbolicLinkStatus rootdir_dsk)
				case e of
					Left (e::SomeException) -> follow rootdir dirs
					Right isLink -> do
						if isLink
							then do
								-- mark the symbolic link as read
								putTxICFSRead rootdir
								e <- forestIO $ Exception.try $ readSymbolicLink rootdir_dsk
								case e of
									Left (e::SomeException) -> follow rootdir dirs
									Right p -> do
										canLink <- canonalizePathTxICFS (root </> p) tree
										follow canLink dirs
							else follow rootdir dirs

-- ** Incrementality

instance MonadReader TxICFSEnv (ForestO TxICFS) where
	ask = TxICFSForestO ask
	local f (TxICFSForestO m) = TxICFSForestO $ local f m
	
instance MonadReader TxICFSEnv (ForestI TxICFS) where
	ask = TxICFSForestI ask
	local f (TxICFSForestI m) = TxICFSForestI $ local f m

instance Incremental (IncForest TxICFS) IORef IO where
	
	newtype Outside (IncForest TxICFS) IORef IO a = TxICFSForestO { runTxICFSForestO :: ReaderT TxICFSEnv IO a } deriving (Functor,Applicative,Monad,MonadLazy,MonadThrow,MonadCatch,MonadMask)
	newtype Inside (IncForest TxICFS) IORef IO a = TxICFSForestI { runTxICFSForestI :: ReaderT TxICFSEnv IO a } deriving (Functor,Applicative,Monad,MonadLazy,MonadThrow,MonadCatch,MonadMask)

	world = TxICFSForestO . runTxICFSForestI
	unsafeWorld = TxICFSForestI . runTxICFSForestO

	runIncremental = error "please use atomically instead"

instance InLayer Outside (IncForest TxICFS) IORef IO where
	inL = TxICFSForestO . lift
	{-# INLINE inL #-}
instance InLayer Inside (IncForest TxICFS) IORef IO where
	inL = TxICFSForestI . lift
	{-# INLINE inL #-}
	
instance ICRep TxICFS where

	forestM = inside . TxICFSForestI . runTxICFSForestM
	forestO = TxICFSForestM . runTxICFSForestO

	-- arguments never change
	data FSThunk TxICFS l inc r m a = TxICFSThunk { txICFSThunkId :: Unique, txICFSThunkArgs :: IORef (Maybe TxICArgs) }

	data HSThunk TxICFS l inc r m a = TxICHSThunk { txICHSThunkId :: Unique, txICHSThunkArgs :: IORef (Maybe TxICArgs) }
		
	newtype ICThunk TxICFS l inc r m a = TxICICThunk (l inc r m a)

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
diffValueThunkTxICFS :: (IncK (IncForest TxICFS) content,Typeable content,ForestRep rep (ForestFSThunkI TxICFS content)) => Bool -> FSTree TxICFS -> rep -> ForestO TxICFS (ValueDelta TxICFS rep)
diffValueThunkTxICFS isTop oldtree rep = do
		thunk <- forestM $ bufferedTxICFSThunk $ to iso_rep_thunk rep
		uid <- forestM $ forestIO $ readRef $ txICId thunk
		(buff :: BuffTxICThunk Inside content) <- forestM $ bufferedTxICThunk thunk
		if isTop
			then return $ ValueDeltaTxICFS $ oldtree == buffTxICTree buff
			else do
				let newtree = buffTxICDeltaTree buff
				return $ ValueDeltaTxICFS $ oldtree == newtree
	
-- arguments passed to transactional variables
type TxICArgs = (Dynamic,FilePath)

-- dynamic variables for buffered content
data DynTxICThunk where
	-- transactional variable (points to an internal thunk)
	DynTxICThunk :: (IncK (IncForest TxICFS) a,ForestLayer TxICFS l) => TxICThunk l a -> MkWeak -> DynTxICThunk
	-- internal thunk (buffered content used for nested logs) 
	DynBuffTxICThunk :: (IncK (IncForest TxICFS) a,ForestLayer TxICFS l) => BuffTxICThunk l a -> MkWeak -> DynTxICThunk

mapDynBuffTxICThunk :: (forall a l . (IncK (IncForest TxICFS) a,ForestLayer TxICFS l) => BuffTxICThunk l a -> ForestL TxICFS l (BuffTxICThunk l a,b)) -> DynTxICThunk -> ForestM TxICFS (DynTxICThunk,b)
mapDynBuffTxICThunk f (DynBuffTxICThunk buff mkWeak) = do
	(buff',b) <- forestO $ outside $ f buff
	return (DynBuffTxICThunk buff' mkWeak,b)

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
newtype TxICThunk (l :: * -> (* -> *) -> (* -> *) -> * -> *) a = TxICThunk { txICId :: IORef Unique } deriving Typeable

data TxICThunkData l a =
		TxICThunkComp (l (IncForest TxICFS) IORef IO a)
	| 	TxICThunkForce a TxICStone

-- a dummy reference to be used as key for weak references
type TxICStone = IORef ()

type TxICParents = WeakMap Unique ()

bufferTxICThunk :: (IncK (IncForest TxICFS) a,ForestLayer TxICFS l) => TxICThunk l a -> BuffTxICThunk l a -> ForestM TxICFS ()
bufferTxICThunk thunk buff = do
	uid <- forestIO $ readIORef (txICId thunk)
	let mkWeak = MkWeak $ mkWeakRefKey $ txICId thunk
	bufferTxICThunk' uid mkWeak buff

bufferTxICThunk' :: (IncK (IncForest TxICFS) a,ForestLayer TxICFS l) => Unique -> MkWeak -> BuffTxICThunk l a -> ForestM TxICFS ()
bufferTxICThunk' uid mkWeak buff = do
	bufferTxICThunk'' uid (DynBuffTxICThunk buff mkWeak)

bufferTxICThunk'' :: Unique -> DynTxICThunk -> ForestM TxICFS ()
bufferTxICThunk'' uid dyn = do
	(starttime,txid,deltas,SCons txlog _) <- Reader.ask
	(tree,delta,newtree,reads,bufftbl,memotbl,tmps) <- forestIO $ readIORef txlog
	forestIO $ WeakTable.insertWithMkWeak bufftbl (dynTxMkWeak dyn) uid dyn

bufferedTxICThunk :: (IncK (IncForest TxICFS) a,ForestLayer TxICFS l) => TxICThunk l a -> ForestM TxICFS (BuffTxICThunk l a)
bufferedTxICThunk thunk = do
	uid <- forestIO $ readIORef (txICId thunk)
	liftM fst $ bufferedTxICThunk' uid

bufferedTxICThunk' :: (IncK (IncForest TxICFS) a,ForestLayer TxICFS l) => Unique -> ForestM TxICFS (BuffTxICThunk l a,MkWeak)
bufferedTxICThunk' uid = do
	dyn <- bufferedTxICThunk'' uid
	case dyn of
		(DynBuffTxICThunk (cast -> Just buff) mkWeak) -> return (buff,mkWeak)

bufferedTxICThunk'' :: Unique -> ForestM TxICFS DynTxICThunk
bufferedTxICThunk'' uid = do
	(starttime,txid,deltas,txlogs) <- Reader.ask
	let find mb txlog = case mb of
		Nothing -> do
			(tree,ds,newtree,reads,bufftbl,memotbl,tmps) <- forestIO $ readRef txlog
			mb <- forestIO $ WeakTable.lookup bufftbl uid
			return mb
		otherwise -> return mb
	liftM fromJust $ Foldable.foldlM find Nothing txlogs

dirtyTxICParents :: FSTree TxICFS -> TxICParents -> ForestM TxICFS ()
dirtyTxICParents tree parents = WeakMap.mapM_' forestIO dirty parents where
	dirty (uid,_) = do
		(b,parents) <- changeTxICThunkM uid $ \buff -> do
			let oldtree = buffTxICDeltaTree buff
			let b = oldtree >= tree -- stop if the the current dirty tree is newer than the tree we are trying to dirty the node with
			let buff' = if b then buff else buff { buffTxICDeltaTree = tree }
			return (buff',(b,parents))
		unless b $ dirtyTxICParents tree parents

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
			forestM $ forestIO $ WeakMap.insertWithMkWeak (buffTxICParents buff) (MkWeak $ mkWeakRefKey stone) uid ()
			tree <- treeTxICThunk thunk
			return (buff,tree)
		changeTxICThunk thunk add
instance (IncK (IncForest TxICFS) a,Typeable l,Typeable a,TxICLayer l) => AddTxICParent l (ForestHSThunk TxICFS l a) where
	addTxICParent proxy stone uid z (var :: ForestHSThunk TxICFS l a) = do
		(thunk :: TxICThunk l a) <- forestM $ bufferedTxICHSThunk var
		let add buff = do
			forestM $ forestIO $ WeakMap.insertWithMkWeak (buffTxICParents buff) (MkWeak $ mkWeakRefKey stone) uid ()
			tree <- treeTxICThunk thunk
			return (buff,tree)
		changeTxICThunk thunk add
instance (MData (AddTxICParentDict l) (ForestL TxICFS l) a) => AddTxICParent l a where
	addTxICParent proxy stone meta z x = do
		let f t1 t2 = return $ max t1 t2
		gmapQr (addTxICParentProxy proxy) f z (addTxICParentDictDict dict proxy stone meta z) x

newTxICThunk :: (IncK (IncForest TxICFS) a,TxICLayer l) => l (IncForest TxICFS) IORef IO a -> FSTree TxICFS -> ForestM TxICFS (TxICThunk l a)
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

changeTxICThunkM :: Unique -> (forall a l . (IncK (IncForest TxICFS) a,ForestLayer TxICFS l) => BuffTxICThunk l a -> ForestL TxICFS l (BuffTxICThunk l a,b)) -> ForestM TxICFS b
changeTxICThunkM uid f = do
	dyn <- bufferedTxICThunk'' uid
	(dyn',b) <- mapDynBuffTxICThunk f dyn
	bufferTxICThunk'' uid dyn'
	return b

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
overwriteTxICThunk :: (IncK (IncForest TxICFS) a,AddTxICParent l a,TxICLayer l) => TxICThunk l a -> l (IncForest TxICFS) IORef IO a -> ForestO TxICFS ()
overwriteTxICThunk var ma = do
	newtree <- forestM getNextFSTree
	let set buff = do
		let dta' = TxICThunkComp ma
		-- dirty the parents of the written thunk
		forestM $ dirtyTxICParents newtree (buffTxICParents buff)
		-- assuming that this is only used by loadDelta at the latest tree
		return (buff { buffTxICData = dta', buffTxICTree = newtree, buffTxICDeltaTree = newtree },())
		-- whenever we want to know the latest modification for the written thunk, we have to force a read and check its content thunks
		--return (buff { buffTxICData = dta', buffTxICTree = newtree, buffTxICDeltaTree = join $ liftM snd $ readTxICThunk var (return newtree) },())
	outside $ changeTxICThunk var set
	

-- memoize internal thunks, not FSThunks
instance ZippedICMemo TxICFS where

	addZippedMemo path proxy args rep mb_tree = do
		let var = to iso_rep_thunk rep
		(starttime,txid,deltas,SCons txlog _) <- Reader.ask
		(tree,ds,newtree,reads,bufftbl,memotbl,tmps) <- forestM $ forestIO $ readRef txlog
		
		-- remember the arguments (this is how we connect transactional arguments to transactional variables)
		let txargs = (toDyn args,path)
		forestM $ forestIO $ writeRef (txICFSThunkArgs var) $ Just txargs
		
		-- memoize the entry
		case mb_tree of
			Nothing -> return ()
			Just tree -> do
				let mkWeak = MkWeak $ mkWeakRefKey $ txICFSThunkArgs var
				forestM $ forestIO $ WeakTable.insertWithMkWeak memotbl mkWeak (path,typeOf rep) (toDyn rep,mkWeak)
	
	findZippedMemo args path (Proxy :: Proxy rep) = do
		(starttime,txid,deltas,SCons txlog _) <- Reader.ask
		(tree,ds,newtree,reads,bufftbl,memotbl,tmps) <- forestM $ forestIO $ readRef txlog
		mb <- forestM $ forestIO $ WeakTable.lookup memotbl (path,typeOf (undefined :: rep))
		case mb of
			Nothing -> return Nothing
			Just (fromDynamic -> Just rep,mkWeak) -> do
				let var = to iso_rep_thunk rep
				Just (fromDynamic -> Just txargs,_) <- forestM $ forestIO $ readRef $ txICFSThunkArgs var
				thunk <- forestM $ bufferedTxICFSThunk var
 				buff <- forestM $ bufferedTxICThunk thunk
				return $ Just (buffTxICTree buff,txargs,rep)

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
		Just (fromDynamic -> Just args,path) -> return $ Just (args,path)
		otherwise -> return Nothing
	

instance Thunk (HSThunk TxICFS) Inside (IncForest TxICFS) IORef IO where
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
		liftM fst $ readTxICThunk thunk tree
instance Thunk (HSThunk TxICFS) Outside (IncForest TxICFS) IORef IO where
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
		liftM fst $ readTxICThunk thunk tree

instance TxICLayer l => Thunk (ICThunk TxICFS) l (IncForest TxICFS) IORef IO where
	new m = return $ TxICICThunk m
	read (TxICICThunk m) = m
instance TxICLayer l => Output (ICThunk TxICFS) l (IncForest TxICFS) IORef IO where
	thunk = Inc.new
	force = Inc.read

instance Thunk (FSThunk TxICFS) Inside (IncForest TxICFS) IORef IO where
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
		liftM fst $ readTxICThunk thunk tree
instance Thunk (FSThunk TxICFS) Outside (IncForest TxICFS) IORef IO where
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
		liftM fst $ readTxICThunk thunk tree

instance Input (FSThunk TxICFS) Inside (IncForest TxICFS) IORef IO where
	ref c = Inc.new (return c)
	mod = Inc.new
	get = Inc.read
	set (var :: ForestFSThunk TxICFS Inside a) v = do
		(thunk :: TxICThunk Inside a) <- forestM $ bufferedTxICFSThunk var
		writeTxICThunk thunk v
	overwrite (var :: ForestFSThunk TxICFS Inside a) mv = do
		(thunk :: TxICThunk Inside a) <- forestM $ bufferedTxICFSThunk var
		overwriteTxICThunk thunk mv
instance Input (FSThunk TxICFS) Outside (IncForest TxICFS) IORef IO where
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

instance TxICForest TxICFS where
	
	atomically = atomicallyTxICFS
	retry = retryTxICFS
	orElse = orElseTxICFS
	throw = throwTxICFS 
	catch = catchTxICFS 
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
			content <- liftM (BX.get lens_content) $ Inc.getOutside $ to iso_rep_thunk def_rep
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
			content' <- liftM (BX.get lens_content) $ Inc.getOutside $ to iso_rep_thunk tgt'
			writeOrElseTxICFS tgt content' b f
		otherwise -> error "tried to write to a variable that is not connected to the FS"

newTxICFS :: FTK TxICFS args rep var content => Proxy args -> ForestVs args -> FilePath -> TxICFTM rep
newTxICFS proxy args path = inside $ zload (vmonadArgs proxyTxICFS proxy args) path

-- read the transactional variable by incrementally repairing its internal thunk to the latest tree
readTxICFS :: (ForestLayer TxICFS l,FTK TxICFS args rep var content) => Proxy args -> rep -> ForestL TxICFS l content
readTxICFS proxy rep = do
	let t = to iso_rep_thunk rep
	(txargs,path) <- liftM fromJust $ forestM $ getFTVArgsIC proxy rep
	oldtree <- inside $ treeTxICFSThunk t
	tree <- forestM latestTree
	df <- liftM fromJust $ forestM $ diffFS oldtree tree path
	-- load incrementally at the latest tree
	inside $ unsafeWorld $ do
		dv <- diffValueThunk oldtree rep
		ds <- deltaArgs oldtree proxy txargs txargs
		let deltas = (txargs,ds)
		zloadDeltaMemo proxy deltas (return path) oldtree (rep,getForestMDInTree) path df tree dv
	liftM (BX.get lens_content) $ inside $ Inc.get t

data ManifestConflictTx = ManifestConflictTx [ManifestError] deriving (Show,Eq,Typeable)
instance Exception ManifestConflictTx

writeOrElseTxICFS :: FTK TxICFS args rep var content => rep -> content -> b -> ([ManifestError] -> TxICFTM b) -> TxICFTM b
writeOrElseTxICFS rep content b f = do

	let t = to iso_rep_thunk rep
		
	mb :: (Maybe (ForestIs TxICFS args,FilePath)) <- forestM $ getFTVArgsIC Proxy rep
	case mb of
		Nothing -> error "writeOrElseTxICFS: should not happen" -- the top-level arguments of a variable don't match the spec
		Just (txargs,path) -> do
			let proxy = Proxy :: Proxy args
			oldtree <- inside $ treeTxICFSThunk t
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
				
				-- store incrementally at the latest tree (there are no filesystem modifications, since only the value has changed since loadDelta)
				man <- forestM $ newManifestWith "/" tree
				(mani,_,memos) <- RWS.runRWST (zupdateManifestDeltaMemo proxy newdeltas path path tree (emptyFSTreeD proxyTxICFS) tree rep newdv man) True ()
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
			catchTxICFS tryWrite (\(ManifestConflictTx errors) -> f errors)
					
atomicallyTxICFS :: TxICFTM b -> IO b
atomicallyTxICFS stm = initializeTxICFS try where
	try = flip Catch.catches [Catch.Handler catchInvalid,Catch.Handler catchRetry,Catch.Handler catchSome] $ do
		-- run the tx
		x <- stm
		-- tries to commit the current tx, otherwise repairs it incrementally
		success <- validateAndCommitTopTxICFS True
		if success
			then return x
			else debug "throw InvalidTx" $ throwM InvalidTx
	catchInvalid InvalidTx = debug "InvalidTx" $ do
		resetTxICFS try
	catchRetry BlockedOnRetry = debug "BlockedOnRetry" $ do
		-- if the retry was invoked on an inconsistent state, we incrementally repair and run again, otherwise we place the tx in the waiting queue
		mbsuccess <- validateAndRetryTopTxICFS
		case mbsuccess of
			Just lck -> do -- retried txs are always in a consistent state, because we apply all affecting updates before releasing the lock
				-- wait for the lock to be released (whenever some variables that it depends on are changed)
				-- we don't consume the contents of the mvar to avoid further puts to succeeed; a new MVar is created for each retry
				inL $ liftIO $ Lock.acquire lck
				resetTxICFS try
			Nothing -> resetTxICFS try
	catchSome (e::SomeException) = debug ("SomeException "++show e) $ do
		-- we still need to validate on exceptions, otherwise repair incrementally; transaction-local allocations still get committed
		success <- validateAndCommitTopTxICFS False
		if success
			then throwM e
			else do
				resetTxICFS try

retryTxICFS :: TxICFTM a
retryTxICFS = inL $ liftIO $ throwIO BlockedOnRetry

orElseTxICFS :: TxICFTM a -> TxICFTM a -> TxICFTM a
orElseTxICFS stm1 stm2 = do1 where
	try1 = do { x <- stm1; validateAndCommitNestedTxICFS Nothing; return x }
	try2 = do { x <- stm2; validateAndCommitNestedTxICFS Nothing; return x }
	do1 = startNestedTxICFS $ try1 `Catch.catches` [Catch.Handler catchRetry1,Catch.Handler catchInvalid,Catch.Handler catchSome]
	do2 = dropChildTxICLog $ startNestedTxICFS $ try2 `Catch.catches` [Catch.Handler catchRetry2,Catch.Handler catchInvalid,Catch.Handler catchSome]
	catchRetry1 BlockedOnRetry = validateAndRetryNestedTxICFS >> do2
	catchRetry2 BlockedOnRetry = validateAndRetryNestedTxICFS >> throwM BlockedOnRetry
	catchInvalid (e::InvalidTx) = throwM e
	catchSome (e::SomeException) = validateAndCommitNestedTxICFS (Just e) >> throwM e

throwTxICFS :: Exception e => e -> TxICFTM a
throwTxICFS = Catch.throwM

catchTxICFS :: Exception e => TxICFTM a -> (e -> TxICFTM a) -> TxICFTM a
catchTxICFS stm h = stm `Catch.catches` [Catch.Handler catchInvalid,Catch.Handler catchRetry,Catch.Handler catchSome] where
	catchInvalid (e::InvalidTx) = throwM e
	catchRetry (e::BlockedOnRetry) = throwM e
	catchSome (e::SomeException) = do
		validateCatchTxICFS
		h $ fromJust $ fromException e

initializeTxICFS :: TxICFTM b -> IO b 
initializeTxICFS (TxICFSForestO m) = do
	starttime <- startTxICFS >>= newIORef
	mountAVFS -- should succeed even if AVFS is already mounted
	txid <- newUnique
	-- current tree
	treeDelta <- newIORef $ emptyFSTreeDelta
	treeSyms <- newIORef =<< liftM (DList.empty,) (readMVar symlinks)
	let tree = TxICFSTree txid 0 treeDelta False treeSyms
	-- next tree
	newtreeDelta <- newIORef $ emptyFSTreeDelta
	newtreeSyms <- newIORef =<< liftM (DList.empty,) (readMVar symlinks)
	let newtree = TxICFSTree txid 1 newtreeDelta False newtreeSyms
	-- tables
	bufftbl <- WeakTable.new
	memotbl <- WeakTable.new
	txlog <- newIORef (tree,DList.empty,newtree,Set.empty,bufftbl,memotbl,Set.empty)
	deltas <- newIORef Map.empty
	debug ("initializeTxICFS") $ Reader.runReaderT m (starttime,txid,deltas,SCons txlog SNil)
	-- don't unmount AVFS, since multiple parallel txs may be using it

-- resets
resetTxICFS :: TxICFTM a -> TxICFTM a
resetTxICFS m = do
	now <- inL $ liftIO $ startTxICFS >>= newIORef
	(_,txid,deltas,_) <- Reader.ask
	-- current tree
	treeDelta <- forestM $ forestIO $ newIORef $ emptyFSTreeDelta
	treeSyms <- forestM $ forestIO $ newIORef =<< liftM (DList.empty,) (readMVar symlinks)
	let tree = TxICFSTree txid 0 treeDelta False treeSyms
	-- next tree
	newtreeDelta <- forestM $ forestIO $ newIORef $ emptyFSTreeDelta
	newtreeSyms <- forestM $ forestIO $ newIORef =<< liftM (DList.empty,) (readMVar symlinks)
	let newtree = TxICFSTree txid 1 newtreeDelta False newtreeSyms
	-- tables
	bufftbl <- forestM $ forestIO $ WeakTable.new
	memotbl <- forestM $ forestIO $ WeakTable.new
	txlog <- forestM $ forestIO $ newIORef (tree,DList.empty,newtree,Set.empty,bufftbl,memotbl,Set.empty)
	forestM $ forestIO $ writeIORef deltas Map.empty
	debug ("resetTxICFS") $ Reader.local (Prelude.const $ (now,txid,deltas,SCons txlog SNil)) m

-- we need to acquire a lock, but this should be minimal
startTxICFS :: IO UTCTime
startTxICFS = getCurrentTime >>= \t -> addRunningTxIC t >> return t

-- appends a freshly created txlog for the inner tx
-- the starting time reference is preserved
--we create a new fsversion reference
startNestedTxICFS :: TxICFTM a -> TxICFTM a
startNestedTxICFS m = do
	(starttime,txid,deltas,txlogs@(SCons txlog_parent _)) <- Reader.ask
	(tree,delta,newtree,reads,_,_,tmps) <- inL $ readIORef txlog_parent
	bufftbl' <- forestM $ forestIO $ WeakTable.new
	memotbl' <- forestM $ forestIO $ WeakTable.new
	txlog_child <- inL $ newIORef (tree,delta,newtree,Set.empty,bufftbl',memotbl',Set.empty)
	debug ("startNestedTxICFS ") $ Reader.local (Prelude.const (starttime,txid,deltas,SCons txlog_child txlogs)) m

-- remember to empty the next tree and delete the new versions in case nested txs fail
dropChildTxICLog :: TxICFTM a -> TxICFTM a
dropChildTxICLog m = do
	(starttime,txid,deltas,(SCons txlog_child txlogs@(SCons txlog_parent _))) <- Reader.ask
	(tree_parent,_,newtree_parent,_,_,_,_) <- inL $ readIORef txlog_parent
	-- forget newly created nested versions
	forestM $ forestIO $ modifyIORef deltas $ Map.filterWithKey (\version _ -> version <= fsTreeFSVersion tree_parent)
	-- reset the next tree
	forestM $ forestIO $ writeIORef (fsTreeFSTreeDelta newtree_parent) emptyFSTreeDelta
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
validateAndCommitTopTxICFS :: Bool -> TxICFTM Bool
validateAndCommitTopTxICFS doWrites = atomicTxICFS "validateAndCommitTopTxICFS" $ do
	txenv@(timeref,txid,deltas,txlogs@(SCons txlog SNil)) <- Reader.ask
	starttime <- inL $ readRef timeref
	success <- forestM $ validateTxsICFS starttime txlogs
	if success
		then do
			forestM $ commitTopTxICFS doWrites starttime txlog
			return True
		else do
			inL $ liftIO $ deleteRunningTxIC starttime
			return False

validateAndCommitNestedTxICFS :: Maybe SomeException -> TxICFTM ()
validateAndCommitNestedTxICFS mbException = do
	txenv@(timeref,txid,deltas,txlogs@(SCons txlog1 (SCons txlog2 _))) <- Reader.ask
	starttime <- inL $ readRef timeref
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
					inL $ liftIO $ deleteRunningTxIC starttime
					throwM InvalidTx

-- validates a transaction and places it into the waiting queue for retrying
validateAndRetryTopTxICFS :: TxICFTM (Maybe Lock)
validateAndRetryTopTxICFS = atomicTxICFS "validateAndRetryTopTxICFS" $ do
	txenv@(timeref,txid,deltas,txlogs@(SCons txlog SNil)) <- Reader.ask
	starttime <- inL $ readRef timeref
	-- validates the current and enclosing txs up the tx tree
	success <- forestM $ validateTxsICFS starttime txlogs
	if success
		then do
			lck <- inL $ liftIO $ Lock.newAcquired -- sets the tx lock as acquired; the tx will be resumed when the lock is released
			forestM $ commitTopTxICFS False starttime txlog
			forestM $ retryTxICFSLog lck timeref txlog -- wait on changes to retry (only registers waits, does not actually wait)
			return $ Just lck
		else do
			inL $ liftIO $ deleteRunningTxIC starttime
			return Nothing

--registers waits for all the filepaths read by a txlog
--since we treat reads/writes separately, we don't need to wait on writes that have not been read
retryTxICFSLog :: Lock -> IORef UTCTime -> TxICFSLog -> ForestM TxICFS ()
retryTxICFSLog lck timeref txlog = do
	(tree,delta,newtree,reads,bufftbl,memotbl,tmps) <- forestIO $ readIORef txlog
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
	txenv@(timeref,txid,deltas,txlogs@(SCons txlog1 (SCons txlog2 _))) <- Reader.ask
	starttime <- inL $ readRef timeref
	success <- forestM $ validateTxsICFS starttime txlogs
	if success
		then do
			forestM $ commitNestedTxICFS False deltas txlog1 txlog2 -- does not perform @Write@s on @retry@
		else do
			inL $ liftIO $ deleteRunningTxIC starttime
			throwM InvalidTx

-- validates the current log before catching an exception
validateCatchTxICFS :: TxICFTM ()
validateCatchTxICFS = do
	txenv@(timeref,txid,deltas,txlogs) <- Reader.ask
	starttime <- inL $ readRef timeref
	success <- forestM $ validateTxsICFS starttime txlogs
	if success
		then do
			-- in case the computation raises an exception, discard all its visible (write) effects
			forestM unbufferTxICFSWrites
		else do
			inL $ liftIO $ deleteRunningTxIC starttime
			throwM InvalidTx

-- we only unbuffer the child log
unbufferTxICFSWrites :: ForestM TxICFS ()
unbufferTxICFSWrites = do
	(starttime,txid,deltas,txlogs@(SCons txlog1 _)) <- Reader.ask
	(tree1,delta1,newtree1,reads1,bufftbl1,memotbl1,tmps1) <- forestIO $ readRef txlog1
	bufftbl1' <- forestIO $ WeakTable.new
	memotbl1' <- forestIO $ WeakTable.new
	-- reset deltas
	forestIO $ writeRef deltas Map.empty
	-- current tree
	treeDelta1' <- forestIO $ newIORef $ emptyFSTreeDelta
	treeSym1' <- forestIO $ newIORef $ (DList.empty,Map.empty)
	let tree1' = TxICFSTree txid 0 treeDelta1' False treeSym1'
	-- next tree
	newtreeDelta1' <- forestIO $ newIORef $ emptyFSTreeDelta
	newtreeSym1' <- forestIO $ newIORef $ (DList.empty,Map.empty)
	let newtree1' = TxICFSTree txid 1 newtreeDelta1' False newtreeSym1'
	forestIO $ writeRef txlog1 (tree1',DList.empty,newtree1',reads1,bufftbl1',memotbl1',tmps1)

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
		(starttime_ref,txid,_,SCons txlog _) <- Reader.ask
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
	(tree1,delta1,newtree1,reads1,bufftbl1,memotbl1,tmps1) <- readIORef txlog1
	(tree2,delta2,newtree2,reads2,bufftbl2,memotbl2,tmps2) <- readIORef txlog2
	
	WeakTable.mapM_ (\(uid,entry) -> WeakTable.insertWithMkWeak bufftbl2 (dynTxMkWeak entry) uid entry) bufftbl1
	WeakTable.mapM_ (\(uid,entry) -> WeakTable.insertWithMkWeak memotbl2 (snd entry) uid entry) memotbl1
	
	writeIORef txlog2 (tree1,delta1,newtree1,reads1 `Set.union` reads2,bufftbl2,memotbl2,tmps1 `Set.union` tmps2)
	
-- does not commit writes
extendTxICFSLog :: TxICFSTreeDeltas -> TxICFSLog -> TxICFSLog -> ForestM TxICFS ()
extendTxICFSLog deltas txlog1 txlog2 = forestIO $ do
	(tree1,delta1,newtree1,reads1,bufftbl1,memotbl1,tmps1) <- readIORef txlog1
	(tree2,delta2,newtree2,reads2,bufftbl2,memotbl2,tmps2) <- readIORef txlog2
	
	-- forget newly created nested versions
	modifyIORef deltas $ Map.filterWithKey (\version _ -> version <= fsTreeFSVersion tree2)
	-- reset the next tree
	writeIORef (fsTreeFSTreeDelta newtree2) emptyFSTreeDelta
	
	writeIORef txlog2 (tree2,delta2,newtree2,reads1 `Set.union` reads2,bufftbl2,memotbl2,tmps1 `Set.union` tmps2)

-- locks on which retrying transactions will wait
type WaitQueue = Deque Threadsafe Threadsafe SingleEnd SingleEnd Grow Safe Lock

-- a register of locks for retrying transactions
-- these paths should be canonical
{-# NOINLINE waitingTxsIC #-}
waitingTxsIC :: MVar (Map FilePath WaitQueue)
waitingTxsIC = unsafePerformIO $ newMVar Map.empty

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
	(tree,delta,newtree,reads,bufftbl,memotbl,tmps) <- forestIO $ readIORef txlog
	wakes <- if doWrites
		then do
			-- get writes since the beginning of the tx
			writes <- forestIO $ readIORef (fsTreeFSTreeDelta newtree)
			-- commits filesystem modifications
			wakes <- forestIO $ commitFSTreeDelta "" writes
			-- get the symlink changes since the beginning of the tx
			(syms,_) <- forestIO $ readIORef (fsTreeSym newtree)
			-- update the symlinks table (uses a global lock)
			forestIO $ modifyMVar_ symlinks $ return . appendListToFSTreeSym syms
			return wakes
		else return Set.empty
	-- removes temporary files
	forestIO $ Foldable.mapM_ (\tmp -> runShellCommand_ $ "rm -rf " ++ tmp) tmps
	return wakes

type FLock = MVar ()

newFLock = newMVar ()
waitFLock mv = Exception.mask_ $ takeMVar mv >> putMVar mv ()
acquireFLock = takeMVar
releaseFLock mv = do
	b <- tryPutMVar mv ()
	when (not b) $ error "Control.Concurrent.Lock.release: Can't release unlocked Lock!"

fileLocks :: CWeakMap.WeakMap FilePath FLock
fileLocks = unsafePerformIO $ CWeakMap.empty

-- a reference to a lock lives as long as the lock itself, and the lock lives at least as long as it is acquired by a transaction
fileLock :: FilePath -> IO FLock
fileLock path = CWeakMap.lookupOrInsert fileLocks path newFLock (\v -> MkWeak $ mkWeakKey v)

atomicTxICFS :: String -> TxICFTM a -> TxICFTM a
atomicTxICFS msg m = do
	-- get the changes of the innermost txlog
	(reads,writes) <- forestM getTxICFSChangesFlat
	-- wait on currently acquired read locks (to ensure that concurrent writes are seen by this tx's validation step)
	waitFileLocks reads
	forestM $ forestIO $ print $ "entering atomic " ++ msg
	x <- withFileLocks writes m
	forestM $ forestIO $ print $ "left atomic " ++ msg
	return x

waitFileLocks :: Set FilePath -> TxICFTM ()
waitFileLocks = inL . liftIO . Foldable.mapM_ (waitFLock <=< fileLock) . Set.toAscList

-- acquiring the locks in sorted order is essential to avoid deadlocks!
withFileLocks :: Set FilePath -> TxICFTM a -> TxICFTM a
withFileLocks paths m = do
	lcks <- inL $ liftIO $ State.mapM fileLock $ Set.toAscList paths
	liftA2 Catch.bracket_ (inL . liftIO . Foldable.mapM_ acquireFLock) (inL . liftIO . Foldable.mapM_ releaseFLock) lcks m
