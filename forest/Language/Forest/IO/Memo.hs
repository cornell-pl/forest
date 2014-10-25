{-# LANGUAGE FlexibleContexts, TupleSections #-}

-- Memoization to support incremental loading of file moves

module Language.Forest.IO.Memo where

import Control.Monad
import Language.Forest.FS.FSRep
import Data.Dynamic
--import Control.Monad.IO.Class
import System.IO.Unsafe
import System.Mem.Weak
import System.Mem.WeakRef
import System.Mem.WeakTable (WeakTable(..))
import System.Mem.WeakTable as WeakTable
import Language.Forest.Generic
import Control.Monad.Incremental
import Data.WithClass.MData

-- Memo table for moved data (we store a version per filename and per specification type)
-- Nothing forbids specifications from reading the same filename twice; if the same file is read into the same type then memoization will kick in, otherwise the file will be reloaded and two different for the same file entries will coexist in the memotable.
{-# NOINLINE memoTableForest #-}
memoTableForest :: WeakTable (FilePath,TypeRep) (Dynamic,FSTree fs)
memoTableForest = unsafePerformIO WeakTable.new

-- stores data data loaded at a particular tree, including an (optional) argument in the memo table
memoForest :: (WeakRef (ForestFSThunkI fs),FSRep fs,ForestInput fs FSThunk Inside,Typeable rep,Typeable md,Typeable arg) => FilePath -> (ForestFSThunk fs Inside rep,ForestFSThunk fs Inside md,arg) -> FSTree fs -> ForestI fs ()
memoForest path (rep,md,arg) tree = do
	forestIO $ WeakTable.insertWithRefKey memoTableForest rep (path,typeOf rep) (toDyn (rep,md,arg),tree) -- use the representation thunk as a key
	addUnmemoFSThunk rep $ unmemoForest path (proxyOf rep)
	addUnmemoFSThunk md $ unmemoForest path (proxyOf rep)

unmemoForest :: Typeable rep => FilePath -> Proxy rep -> IO ()
unmemoForest path rep = WeakTable.finalize memoTableForest (path,typeRep rep)

-- looks up and removes cached loaded data from the memo table
lookupmemoForest :: (FSRep fs,Typeable rep,Typeable md,Typeable arg) => FilePath -> Proxy rep -> ForestI fs (Maybe ((ForestFSThunk fs Inside rep,ForestFSThunk fs Inside md,arg),FSTree fs))
lookupmemoForest path rep = do
	mb <- forestIO $ WeakTable.lookup memoTableForest (path,typeRep rep)
	case mb of
		Just (dyn,tree) -> return $ liftM (,tree) $ fromDynamic dyn
		Nothing -> return Nothing