{-# LANGUAGE ConstraintKinds, UndecidableInstances, TupleSections, FlexibleInstances, MultiParamTypeClasses, StandaloneDeriving, GeneralizedNewtypeDeriving, FlexibleContexts, DataKinds, TypeFamilies, Rank2Types, GADTs, ViewPatterns, DeriveDataTypeable #-}

-- Original Forest with purely functional data structures

module Language.Forest.Pure.FS.PureFS where

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

-- filesystems are always different
instance Eq (FSTree PureFS) where
	t1 == t2 = False

instance FSRep PureFS where
	
	newtype ForestM PureFS a = PureFSForestM { runPureFSForestM :: IO a } deriving (Monad,MonadIO,Functor,Applicative,MonadLazy)
	
	data ForestCfg PureFS = PureFSForestCfg
	
	runForest _ (PureFSForestM m) = do
		mountAVFS
		x <- m
		unmountAVFS
		return x
	
	forestIO = PureFSForestM
	
	-- | returns the forest directory used to store auxiliary data
	getForestDirectory = liftM (</> "Forest") $ forestIO getTemporaryDirectory
	
	-- there is no notion of filesystem versions, but we still need to distinguish AVFS mode from normal mode
	data FSTree PureFS = PureFSTree | VirtualPureFSTree deriving Show
	
	-- log the modifications
	deletePath path = forestIO $ removePath path >> return ()
	writeFile path ondisk = forestIO $ movePath ondisk path >> return ()
	writeDir path = forestIO $ createDirectoryIfMissing True path
	writeLink path ondisk = forestIO $ movePath ondisk path >> return ()
	writePathMD path ondisk = return () --XXX: we are ignoring file attributes!
	
	-- registers a new temporary path
	tempPath = forestIO getTempPath >>= \path -> return path
	-- change into AVFS mode
	virtualTree _ = return VirtualPureFSTree
	latestTree = return PureFSTree
	
	-- reads from the FS or from the modification log
	pathInTree path PureFSTree = return path
	pathInTree path VirtualPureFSTree = do
		ondisk <- pathInTree path PureFSTree
		home <- forestIO $ getHomeDirectory
		let result = home </> ".avfs" </> makeRelative "/" ondisk
		return result
		
	stepPathInTree _ path rel = return $ path </> rel
	canonalizePathWithTree path _ = forestIO $ canonalizePath path









