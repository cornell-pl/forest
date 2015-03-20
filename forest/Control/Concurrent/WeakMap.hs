{-# LANGUAGE DeriveDataTypeable, TupleSections, TypeOperators, BangPatterns, PatternGuards, MagicHash, UnboxedTuples #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
-----------------------------------------------------------------------
-- | A non-blocking concurrent map from hashable keys to values.
--
-- The implementation is based on /lock-free concurrent hash tries/
-- (aka /Ctries/) as described by:
--
--    * Aleksander Prokopec, Phil Bagwell, Martin Odersky,
--      \"/Cache-Aware Lock-Free Concurent Hash Tries/\"
--    * Aleksander Prokopec, Nathan G. Bronson, Phil Bagwell, Martin
--      Odersky \"/Concurrent Tries with Efficient Non-Blocking Snapshots/\"
--
-- Operations have a worst-case complexity of /O(log n)/, with a base
-- equal to the size of the native 'Word'.
--
-----------------------------------------------------------------------

module Control.Concurrent.WeakMap
    ( WeakMap

      -- * Construction
    , empty

      -- * Modification
    , insert, insertWith, insertWithMkWeak
    , delete

      -- * Query
    , lookup
	, lookupOrInsert, lookupOrInsertMkWeak

    ) where

import Data.Typeable
import Control.Applicative ((<$>))
import Control.Monad
import Data.Atomics
import Data.Bits
import Data.Hashable (Hashable)
import qualified Data.Hashable as H
import Data.IORef
import qualified Data.List as List
import Data.Maybe
import Data.Word
import Prelude hiding (lookup)
import System.Mem.Weak as Weak
import System.Mem.WeakKey

import Data.Strict.Tuple as Strict

import GHC.IORef
import GHC.MVar
import GHC.STRef
import GHC.Weak
import GHC.Base

import qualified Control.Concurrent.Map as CMap

-----------------------------------------------------------------------

newtype WeakMap k v = WeakMap (IORef (WeakMap' k v) :!: Weak (WeakMap' k v)) deriving Typeable
type WeakMap' k v = CMap.Map k (Weak v)

-----------------------------------------------------------------------

lookupOrInsert :: (Eq k, Hashable k) => WeakMap k v -> k -> IO v -> (v -> Maybe (IO ()) -> IO (Weak v)) -> IO v
lookupOrInsert w_tbl@(WeakMap (tbl_ref :!: weak_tbl)) k mv mkWeak = do
	tbl <- readIORef tbl_ref
	CMap.lookupOrInsert k Weak.deRefWeak (mv >>= \l -> liftM (,l) $ mkWeak l $ Just $ deleteFinalized' weak_tbl k) tbl

lookupOrInsertMkWeak :: (Eq k, Hashable k) => WeakMap k v -> k -> IO v -> (v -> MkWeak) -> IO v
lookupOrInsertMkWeak w_tbl@(WeakMap (tbl_ref :!: weak_tbl)) k mv mk = do
	tbl <- readIORef tbl_ref
	CMap.lookupOrInsert k Weak.deRefWeak (mv >>= \l -> liftM (,l) $ let (MkWeak mkWeak) = mk l in mkWeak l $ Just $ deleteFinalized' weak_tbl k) tbl

{-# INLINE insert #-}
insert :: (Eq k, Hashable k) => WeakMap k v -> k -> v -> IO ()
insert tbl k v = Control.Concurrent.WeakMap.insertWith tbl k k v

-- | the key @k@ stores the entry for the value @a@ in the table
insertWith :: (Eq k, Hashable k) => WeakMap k v -> a -> k -> v -> IO ()
insertWith w_tbl@(WeakMap (tbl_ref :!: weak_tbl)) a k v = do
	tbl <- readIORef tbl_ref
	weak <- Weak.mkWeak a v $ Just $ Control.Concurrent.WeakMap.deleteFinalized' weak_tbl k
	CMap.insert k weak tbl

insertWithMkWeak :: (Eq k,Hashable k) => WeakMap k v -> MkWeak -> k -> v -> IO ()
insertWithMkWeak w_tbl@(WeakMap (tbl_ref :!: weak_tbl)) (MkWeak mkWeak) k v = do
	tbl <- readIORef tbl_ref
	finalizeEntry w_tbl k -- we need to make sure that any old weak pointer is finalized, since it may be be kept alive by the same key
	weak <- mkWeak v $ Just $ deleteFinalized' weak_tbl k
	CMap.insert k weak tbl

finalizeEntry :: (Eq k,Hashable k) => WeakMap k b -> k -> IO ()
finalizeEntry (WeakMap (_ :!: weak_tbl)) k = do
	r <- Weak.deRefWeak weak_tbl
	case r of
		Nothing -> return ()
		Just tbl -> do
			mb <- CMap.lookup k tbl
			case mb of
				Nothing -> return ()
				Just b -> Weak.finalize b

deleteFinalized' :: (Eq k, Hashable k) => Weak (WeakMap' k b) -> k -> IO ()
deleteFinalized' weak_tbl k = do
	r <- Weak.deRefWeak weak_tbl
	case r of
		Nothing -> return ()
		Just tbl -> CMap.deleteIf k (liftM isNothing . Weak.deRefWeak) tbl

lookup :: (Eq k, Hashable k) => WeakMap k v -> k -> IO (Maybe v)
lookup (WeakMap (tbl_ref :!: weak_tbl)) k = do
	tbl <- readIORef tbl_ref
	w <- CMap.lookup k tbl
	case w of
		Nothing -> return Nothing
		Just r -> Weak.deRefWeak r

delete :: (Eq k, Hashable k) => WeakMap k v -> k -> IO ()
delete (WeakMap (tbl_ref :!: _)) k = readIORef tbl_ref >>= \tbl -> CMap.delete k tbl

empty :: (Eq k, Hashable k) => IO (WeakMap k v)
empty = do
	tbl <- CMap.empty
	tbl_ref <- newIORef tbl
	weak_tbl <- mkWeakKey tbl_ref tbl $ Just $ table_finalizer tbl
	return $ WeakMap (tbl_ref :!: weak_tbl)

table_finalizer :: (Eq k, Hashable k) => WeakMap' k v -> IO ()
table_finalizer tbl = do
	pairs <- CMap.unsafeToList tbl
	sequence_ [ Weak.finalize w | (_,w) <- pairs ]
