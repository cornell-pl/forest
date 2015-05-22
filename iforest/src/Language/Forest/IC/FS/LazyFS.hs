{-# LANGUAGE ConstraintKinds, UndecidableInstances, TupleSections, FlexibleInstances, MultiParamTypeClasses, StandaloneDeriving, GeneralizedNewtypeDeriving, FlexibleContexts, DataKinds, TypeFamilies, Rank2Types, GADTs, ViewPatterns, DeriveDataTypeable #-}

-- Regular filesystem with optimistic concurrency support for transactions, without any incrementality

module Language.Forest.IC.FS.LazyFS where

import Language.Forest.FS.FSRep
import Language.Forest.IC.ICRep
import System.IO.Unsafe
import Control.Monad
import Control.Applicative

import Data.Maybe
import Data.List
import Data.WithClass.MData
import Data.Typeable
import System.Directory
import Data.List as List
import Data.IORef
import System.FilePath.Posix
import Language.Forest.IO.Utils
import Control.Concurrent.Lock (Lock(..))
import qualified Control.Concurrent.Lock as Lock
import Control.Concurrent.MVar
import Control.Monad.Incremental as Inc
import Language.Forest.IO.Shell
import Language.Forest.IC.Generic
import Control.Monad.Incremental.LazyNonInc
import Control.Monad.Incremental.Internal.LazyNonInc.Algorithm
import Control.Monad.Incremental.Internal.LazyNonInc.Layers
import Control.Monad.Incremental.Internal.LazyNonInc.Types
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

type instance IncK (IncForest LazyFS) a = (Eq a,Typeable a)


instance Incremental (IncForest LazyFS) where
	
	newtype Outside (IncForest LazyFS) a = LazyFSForestO { runLazyFSForestO :: Outside LazyNonInc a } deriving (Monad,Applicative,Functor)
	newtype Inside (IncForest LazyFS) a = LazyFSForestI { runLazyFSForestI :: Inside LazyNonInc a } deriving (Monad,Applicative,Functor)

	world = LazyFSForestO . inside . runLazyFSForestI
	unsafeWorld = LazyFSForestI . LazyNonIncInner . runLazyNonIncOuter . runLazyFSForestO

	runIncremental m = do
		mountAVFS
		x <- runLazyNonIncOuter $ (runLazyFSForestO m) 
		unmountAVFS
		return x
	
	unsafeIOToInc = inside . LazyFSForestI  . LazyNonIncInner

instance Eq (FSTree LazyFS) where
	t1 == t2 = False

instance Show (FSTree LazyFS) where
	show LazyFSTree = "LazyFSTree"
	show VirtualLazyFSTree = "VirtualLazyFSTree"

instance FSRep LazyFS where
	
	newtype ForestM LazyFS a = LazyFSForestM { runLazyFSForestM :: Inside LazyNonInc a } deriving (Monad,Applicative,Functor)
	
	forestIO =  LazyFSForestM . unsafeIOToInc where
	
	-- | returns the forest directory used to store auxiliary data
	getForestDirectory = liftM (</> "Forest") $ forestIO getTemporaryDirectory
	
	-- there is no notion of filesystem versions, but we still need to distinguish AVFS mode from normal mode
	data FSTree LazyFS = LazyFSTree | VirtualLazyFSTree
	
	-- log the modifications
	deletePath path = forestIO $ removePath path >> return ()
	writeFile path ondisk = forestIO $ movePath ondisk path >> return ()
	writeDir path = forestIO $ createDirectoryIfMissing True path
	writeLink path ondisk = forestIO $ movePath ondisk path >> return ()
	writePathMD path ondisk = return () --XXX: we are ignoring file attributes!
	
	-- registers a new temporary path
	tempPath = forestIO $ getTempPath
	-- change into AVFS mode
	virtualTree _ = return VirtualLazyFSTree
	latestTree = return LazyFSTree
	
	-- reads from the FS or from the modification log
	pathInTree path LazyFSTree = return path
	pathInTree path VirtualLazyFSTree = do
		ondisk <- pathInTree path LazyFSTree
		home <- forestIO $ getHomeDirectory
		let result = home </> ".avfs" </> makeRelative "/" ondisk
		return result
		
	stepPathInTree _ path rel = return $ path </> rel
	canonalizePathInTree path _ = forestIO $ canonalizePath path
	
instance ICRep LazyFS where

	newtype FSThunk LazyFS l inc a = LazyFSFSThunk { unLazyFSFSThunk :: LazyNonIncL l (IncForest LazyFS) a }

	forestM = undefined 
	forestO = undefined

	eqFSThunk = error "no equality for NOFSThunk"
	isUnevaluatedFSThunk = error "isUnevaluatedFSThunk"
	
	data ICThunk LazyFS l inc a = LazyFSICThunk { unLazyFSICThunk :: LazyNonIncU l (IncForest LazyFS) a }
	
	eqICThunk = error "eqICThunk"


	data HSThunk LazyFS l inc a = LazyFSHSThunk { unLazyFSHSThunk :: T l inc a }


instance ForestLayer LazyFS l => Thunk (HSThunk LazyFS) l (IncForest LazyFS) where
	new = liftM LazyFSHSThunk . Inc.new
	read (LazyFSHSThunk t) = Inc.read t

instance ForestLayer LazyFS l => Thunk (ICThunk LazyFS) l (IncForest LazyFS) where
	new = liftM LazyFSICThunk . thunkLazyNonIncU
	read (LazyFSICThunk t) = forceLazyNonIncU t

instance ForestLayer LazyFS l => Output (ICThunk LazyFS) l (IncForest LazyFS) where
	thunk = liftM LazyFSICThunk . thunkLazyNonIncU
	force (LazyFSICThunk t) = forceLazyNonIncU t
	
instance (Input LazyNonIncL l (IncForest LazyFS),ForestLayer LazyFS l) => ForestInput LazyFS FSThunk l where
	fsRef tree = liftM LazyFSFSThunk . ref
	fsForce (LazyFSFSThunk t) = Inc.get t
	fsSet tree (LazyFSFSThunk t) v = set t v

instance LiftInc Inside LazyNonInc (IncForest LazyFS) where
	liftInc = LazyFSForestI 
instance LiftInc Outside LazyNonInc (IncForest LazyFS) where
	liftInc = LazyFSForestO 










