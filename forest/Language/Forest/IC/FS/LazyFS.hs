{-# LANGUAGE ConstraintKinds, UndecidableInstances, TupleSections, FlexibleInstances, MultiParamTypeClasses, StandaloneDeriving, GeneralizedNewtypeDeriving, FlexibleContexts, DataKinds, TypeFamilies, Rank2Types, GADTs, ViewPatterns, DeriveDataTypeable #-}

-- Regular filesystem with optimistic concurrency support for transactions, without any incrementality

module Language.Forest.IC.FS.LazyFS where

import Language.Forest.FS.FSRep
import Language.Forest.IC.ICRep
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
import qualified Language.Forest.Pure.MetaData as Pure
import Control.Concurrent.Lock (Lock(..))
import qualified Control.Concurrent.Lock as Lock
import Control.Concurrent.MVar
import Control.Monad.Incremental as Inc
import Language.Forest.IO.Shell
import Language.Forest.IC.Generic
import Control.Monad.Incremental.LazyNonInc
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



instance Incremental (IncForest LazyFS) IORef IO where
	
	newtype Outside (IncForest LazyFS) IORef IO a = LazyFSForestO { runLazyFSForestO :: Outside LazyNonInc IORef IO a } deriving (Monad,MonadLazy)
	newtype Inside (IncForest LazyFS) IORef IO a = LazyFSForestI { runLazyFSForestI :: Inside LazyNonInc IORef IO a } deriving (Monad,MonadLazy)

	world = LazyFSForestO . inside . runLazyFSForestI
	unsafeWorld = LazyFSForestI . LazyNonIncInner . runLazyNonIncOuter . runLazyFSForestO

	runIncremental m = do
		mountAVFS
		x <- runLazyNonIncOuter $ (runLazyFSForestO m) 
		unmountAVFS
		return x

instance InLayer Outside (IncForest LazyFS) IORef IO where
	inL = LazyFSForestO  . LazyNonIncOuter
	{-# INLINE inL #-}
instance InLayer Inside (IncForest LazyFS) IORef IO where
	inL = LazyFSForestI  . LazyNonIncInner
	{-# INLINE inL #-}

instance Eq (FSTree LazyFS) where
	t1 == t2 = False

instance Show (FSTree LazyFS) where
	show LazyFSTree = "LazyFSTree"
	show VirtualLazyFSTree = "VirtualLazyFSTree"

instance FSRep LazyFS where
	
	newtype ForestM LazyFS a = LazyFSForestM { runLazyFSForestM :: Inside LazyNonInc IORef IO a } deriving (Monad,MonadLazy)
	
	forestIO =  LazyFSForestM . inL . liftIO where
	
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
	canonalizePathWithTree path _ = forestIO $ canonalizePath path
	
instance ICRep LazyFS where

	newtype FSThunk LazyFS l inc r m a = LazyFSFSThunk { unLazyFSFSThunk :: LazyNonIncL l (IncForest LazyFS) r m a }

	forestM = undefined 
	forestO = undefined

	eqFSThunk = error "no equality for NOFSThunk"
	isUnevaluatedFSThunk = error "isUnevaluatedFSThunk"
	isUnforcedFSThunk = error "isUnforcedFSThunk"
	
	data ICThunk LazyFS l inc r m a = LazyFSICThunk { unLazyFSICThunk :: LazyNonIncU l (IncForest LazyFS) r m a }
	
	eqICThunk = error "eqICThunk"


	data HSThunk LazyFS l inc r m a = LazyFSHSThunk { unLazyFSHSThunk :: T l inc r m a }

instance ICMemo LazyFS where

	-- no memoization
	addMemo _ _ _ _ _ = return ()
	remMemo _ _ _ = return ()
	findMemo _ _ _ = return Nothing

instance ForestLayer LazyFS l => Thunk (HSThunk LazyFS) l (IncForest LazyFS) IORef IO where
	new = liftM LazyFSHSThunk . new
	read (LazyFSHSThunk t) = Inc.read t

instance ForestLayer LazyFS l => Thunk (ICThunk LazyFS) l (IncForest LazyFS) IORef IO where
	new = liftM LazyFSICThunk . thunkLazyNonIncU
	read (LazyFSICThunk t) = forceLazyNonIncU t

instance ForestLayer LazyFS l => Output (ICThunk LazyFS) l (IncForest LazyFS) IORef IO where
	thunk = liftM LazyFSICThunk . thunkLazyNonIncU
	force (LazyFSICThunk t) = forceLazyNonIncU t

instance (Thunk LazyNonIncL l (IncForest LazyFS) IORef IO,ForestLayer LazyFS l) => Thunk (FSThunk LazyFS) l (IncForest LazyFS) IORef IO where
	new = liftM LazyFSFSThunk . new
	read (LazyFSFSThunk t) = Inc.read t
	
instance (Input LazyNonIncL l (IncForest LazyFS) IORef IO,ForestLayer LazyFS l) => Input (FSThunk LazyFS) l (IncForest LazyFS) IORef IO where
	ref = liftM LazyFSFSThunk . ref
	get (LazyFSFSThunk t) = Inc.get t
	set (LazyFSFSThunk t) v = set t v

instance LiftInc Inside LazyNonInc (IncForest LazyFS) IORef IO where
	liftInc = LazyFSForestI 
instance LiftInc Outside LazyNonInc (IncForest LazyFS) IORef IO where
	liftInc = LazyFSForestO 










