{-# LANGUAGE DeriveDataTypeable, RankNTypes, TypeOperators #-}

-- | In-memory implementation of idealized filesystem locks:
-- * file locks are mandatory and exclusive
-- * directory locks acquire the entire directory, including any recursive subpath (but not following symbolic links)
-- For example, locking /a blocks /a/b, but not /c even if there is a symlink /a/b --> /c. The intuition for this is that changes to the directory and the target of the symbolic link can happen concurrently.

module Language.Forest.FS.FileLock where

import System.FilePath
import Control.Monad.IO.Class
import Control.Monad.Catch as Catch
import Control.Concurrent.Lock.Exts as Lock
import Control.Concurrent.MVar.Exts
import Data.Map.Strict (Map(..))
import qualified Data.Map.Strict as Map
import Data.Set (Set(..))
import qualified Data.Set as Set
import Control.Monad
import Data.Strict.Tuple as Strict
import System.IO.Unsafe
import Data.Typeable
import System.Mem.Concurrent.WeakMap as CWeakMap
import System.Mem.Weak.Exts as Weak

-- | A filesystem lock tree 
-- we store locks in a weak map so that they only live for the course of a transaction
-- it is important to prune the lock tree since we wait on all recursive children of a locked path
newtype LockTree = LockTree { unLockTree :: CWeakMap.WeakMap String (Lock :!: LockTree) } deriving Typeable

{-# NOINLINE fileLocks #-}
fileLocks :: (Lock :!: LockTree)
fileLocks = unsafePerformIO $ do
	noparent <- Lock.new
	tree <- liftM LockTree $ CWeakMap.new
	return (noparent :!: tree)

maxLock :: (Lock :!: Bool) -> (Lock :!: Bool) -> (Lock :!: Bool)
maxLock (x :!: b1) (y :!: b2) = (x :!: max b1 b2)

-- wait on children locks
waitChildren :: FilePath -> LockTree -> IO (Map FilePath (Lock :!: Bool)) -> IO (Map FilePath (Lock :!: Bool))
waitChildren root children mlcks = CWeakMap.toMap (unLockTree children) >>= Map.foldrWithKey waitChild mlcks
	where
	waitChild dir w mlcks = do
		mb <- Weak.deRefWeak w
		case mb of
			Nothing -> mlcks
			Just (lck :!: children) -> do
				let mlcks' = liftM (Map.insertWith maxLock (root </> dir) (lck :!: False)) mlcks -- wait on child
				waitChildren (root </> dir) children mlcks'

fileLock :: Bool -> FilePath -> IO (Map FilePath (Lock :!: Bool)) -> IO (Map FilePath (Lock :!: Bool))
fileLock isWrite path mlcks = fileLock' (Strict.fst fileLocks) "" (splitDirectories path) (Strict.snd fileLocks) mlcks
	where
	findLock :: Lock -> String -> LockTree -> IO (Lock :!: LockTree)
	findLock parentlock dir (LockTree locktree@(CWeakMap.WeakMap (tbl :!: _))) = modifyMVarMasked' tbl $ \xs -> case Map.lookup dir xs of
		Nothing -> do
			l <- Lock.new
			c <- liftM LockTree $ CWeakMap.new
			let n = l :!: c
			w <- mkWeakKey l n $ Just $ CWeakMap.deleteFinalized locktree dir
			mkWeakKey l parentlock Nothing -- the parent lock is alive at least as long as the child lock
			return (Map.insert dir w xs,n)
		Just w -> do
			mb <- Weak.deRefWeak w
			case mb of
				Nothing -> do
					l <- Lock.new
					c <- liftM LockTree $ CWeakMap.new
					let n = l :!: c
					w <- mkWeakKey l n $ Just $ CWeakMap.deleteFinalized locktree dir
					mkWeakKey l parentlock Nothing -- the parent lock is alive at least as long as the child lock
					return (Map.insert dir w xs,n)
				Just n -> return (xs,n)
		
	fileLock' :: Lock -> FilePath -> [String] -> LockTree -> IO (Map FilePath (Lock :!: Bool)) -> IO (Map FilePath (Lock :!: Bool))
	fileLock' parentlock root [dir] locktree mlcks = do
		(lock :!: childrentree) <- findLock parentlock dir locktree
		let mlcks' = liftM (Map.insertWith maxLock (root </> dir) (lock :!: isWrite)) mlcks -- wait/lock the path
		waitChildren (root </> dir) childrentree mlcks' -- wait on children
	fileLock' parentlock root (dir:dirs) locktree mlcks = do
		(lock :!: childrentree) <- findLock parentlock dir locktree
		let mlcks' = liftM (Map.insertWith maxLock (root </> dir) (lock :!: False)) mlcks -- wait on parent
		fileLock' lock (root </> dir) dirs childrentree mlcks'

acquireFileLocks :: Map FilePath (Lock :!: Bool) -> IO ([Lock],[Lock])
acquireFileLocks lcks = Map.foldr acquireFileLock (return ([],[])) lcks
	where
	acquireFileLock (lck :!: isWrite) m = if isWrite
		then liftM (\(waiting,acquired) -> (waiting,lck:acquired)) m
		else liftM (\(waiting,acquired) -> (lck:waiting,acquired)) m

-- acquisition is done in sorted order of filepaths
withFileLocks :: (MonadIO m,MonadMask m) => Set FilePath -> Set FilePath -> m a -> m a
withFileLocks reads writes m = Catch.uninterruptibleMask_ $ do
	acquired <- liftIO $ do
		rcks <- Set.foldr (fileLock False) (return Map.empty) reads
		lcks <- Set.foldr (fileLock True) (return rcks) writes

		(waiting,acquired) <- acquireFileLocks lcks -- acquire all locks
		Lock.releases waiting -- release reads
		return acquired
	x <- m -- commit data
	liftIO $ Lock.releases acquired -- release writes
	return x
		