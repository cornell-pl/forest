{-# LANGUAGE EmptyDataDecls, DoAndIfThenElse, OverlappingInstances, TypeOperators, FunctionalDependencies, UndecidableInstances, ConstraintKinds,ScopedTypeVariables, TemplateHaskell, FlexibleInstances, MultiParamTypeClasses, StandaloneDeriving, GeneralizedNewtypeDeriving, FlexibleContexts, DataKinds, TypeFamilies, Rank2Types, GADTs, ViewPatterns, DeriveDataTypeable #-}

-- A module for incremental computation support

module Language.Forest.IC.ICRep where

import Prelude hiding (const,read,mod)
import System.Mem.WeakKey
import Control.Monad
import Data.Maybe
import Data.WithClass.MData
import Data.List.Split
import Data.Typeable
import System.Directory
import System.IO.Unsafe
import qualified Data.List as List
import Data.IORef
import System.FilePath.Posix
import System.Posix.IO
import System.IO
import Language.Forest.IO.Shell
import Language.Forest.IO.Utils
import Language.Forest.Pure.MetaData hiding (fileInfo,errors,Forest_md)

import Language.Pads.MetaData hiding (numErrors)
import Language.Pads.CoreBaseTypes
import Language.Forest.Errors
import Data.Data hiding (gmapT)
import System.Posix.Types
import Data.WithClass.Derive.MData
import Data.DeriveTH
import Data.Derive.Data
import Foreign.C.Types
import System.FilePath.Canonical
import System.Posix.Files
import System.Posix.User
import System.Process
import System.Posix.Time
import Control.Exception as Exception
import Control.Monad.Incremental
import Data.Unique
import Data.Int
import Control.Monad.Incremental.Adapton
import Control.Monad.Incremental.List
import Control.Monad.Incremental.Generics
import Language.Forest.IC.FS.FSDelta
import Data.Word
import Data.Time.Clock

import Data.DeepTypeable
import Data.WithClass.Derive.DeepTypeable
import Language.Haskell.TH.Syntax hiding (Loc(..))
import Language.Forest.FS.FSRep

-- * Incremental computation

data IncForest (fs :: FS) deriving Typeable

type ForestLayer fs l = (FSRep fs,Layer l (IncForest fs) IORef IO,Layer l Adapton IORef IO)
type ForestL fs (l :: * -> (* -> *) -> (* -> *) -> * -> *) = l (IncForest fs) IORef IO

type ForestFSThunk fs l = FSThunk fs l (IncForest fs) IORef IO
type ForestFSThunkI fs = ForestFSThunk fs Inside
type ForestFSThunkO fs = ForestFSThunk fs Outside

type ForestICThunk fs l = ICThunk fs l (IncForest fs) IORef IO
type ForestICThunkI fs = ForestICThunk fs Inside
type ForestICThunkO fs = ForestICThunk fs Outside

type ForestHSThunk fs l = HSThunk fs l (IncForest fs) IORef IO
type ForestHSThunkI fs = ForestHSThunk fs Inside
type ForestHSThunkO fs = ForestHSThunk fs Outside
	
type ForestO fs = Outside (IncForest fs) IORef IO
type ForestI fs = Inside (IncForest fs) IORef IO

instance ICRep fs => Eq (ForestFSThunk fs l a) where
	t1 == t2 = eqFSThunk t1 t2
instance ICRep fs => Eq (ForestICThunk fs l a) where
	t1 == t2 = eqICThunk t1 t2

-- | Class that implements IC-specific operations
class (ForestThunk fs HSThunk Inside,ForestThunk fs HSThunk Outside,ForestOutput fs ICThunk Outside,ForestOutput fs ICThunk Inside,ForestInput fs FSThunk Outside,ForestInput fs FSThunk Inside,FSRep fs,DeepTypeable fs,Incremental (IncForest fs) IORef IO) => ICRep (fs :: FS) where
	
	runIncrementalForest :: ForestCfg fs -> ForestO fs a -> IO a
	
	-- converts a forest computation to an incremental computation. note that implementors need to be careful about certain deterministic guarantees for certain operations.
	forestM :: ForestLayer fs l => ForestM fs a -> ForestL fs l a
	-- it should be possible to transpose an outer layer computation into a forest computation
	forestO :: ForestO fs a -> ForestM fs a
	
	-- * IC thunks
	
	-- | unevaluated thunks that depend on the filesystem (but not on other thunks) and can be modified, but that do not depend on other thunks
	data FSThunk fs (l :: * -> (* -> *) -> (* -> *) -> * -> *) (inc :: *) (r :: * -> *) (m :: * -> *) a :: *
	
	eqFSThunk :: FSThunk fs l inc r m a -> FSThunk fs l inc r m a -> Bool

	isUnevaluatedFSThunk :: (ForestLayer fs l,ForestLayer fs l1) => ForestFSThunk fs l1 a -> ForestL fs l Bool

	-- | tests of a @FSThunk@ has been ever forced. This test is important for efficient delta loading, as it allows us to safely stop incremental repair if the thunk has never been inspected.
	isUnforcedFSThunk :: (ForestLayer fs l,ForestLayer fs l1) => ForestFSThunk fs l1 a -> ForestL fs l Bool

	-- | incremental computation thunks that depend on @FSThunk@s and cannot be directly modified, but are updated for changes on dependencies
	data ICThunk fs (l :: * -> (* -> *) -> (* -> *) -> * -> *) (inc :: *) (r :: * -> *) (m :: * -> *) a :: *

	eqICThunk :: ICThunk fs l inc r m a -> ICThunk fs l inc r m a -> Bool

	-- | A plain thunk that simply stores a computation; cannot be mutated, and is not incrementally repaired
	data HSThunk fs (l :: * -> (* -> *) -> (* -> *) -> * -> *) (inc :: *) (r :: * -> *) (m :: * -> *) a :: *

	-- * filesystem modification events, used for incremental loading
	
	changesBetween :: FSTree fs -> FSTree fs -> ForestO fs FSTreeDelta

	-- * Forest memoization operations to support incremental loading in the presence of FS moves (dummies if no moves are supported)
		
	memo :: (ForestInput fs FSThunk Inside,Typeable rep,Typeable md,Typeable arg) => FilePath -> (ForestFSThunk fs Inside rep,ForestFSThunk fs Inside md,arg) -> FSTree fs -> ForestI fs ()
	unmemo :: Typeable rep => Proxy fs -> FilePath -> Proxy rep -> ForestI fs ()
	lookupmemo :: (Typeable rep,Typeable md,Typeable arg) => FilePath -> Proxy rep -> ForestI fs (Maybe ((ForestFSThunk fs Inside rep,ForestFSThunk fs Inside md,arg),FSTree fs))

	-- | adds a finalizer to a Forest Input thunk to be run whenever it is modified
	addUnmemoFSThunk :: ForestLayer fs l => FSThunk fs l (IncForest fs) IORef IO a -> IO () -> ForestL fs l ()

type ForestThunk fs mod l = Thunk (mod fs) l (IncForest fs) IORef IO
type ForestOutput fs mod l = Output (mod fs) l (IncForest fs) IORef IO
type ForestInput fs mod l = Input (mod fs) l (IncForest fs) IORef IO

fsRef :: (Typeable a,Eq a,FSRep fs,ForestInput fs FSThunk l) => a -> ForestL fs l (ForestFSThunk fs l a)
fsRef = ref
fsThunk :: (Typeable a,Eq a,FSRep fs,ForestInput fs FSThunk l) => ForestL fs l a -> ForestL fs l (ForestFSThunk fs l a)
fsThunk = mod
icThunk :: (Typeable a,Eq a,FSRep fs,ForestOutput fs ICThunk l) => ForestL fs l a -> ForestL fs l (ForestICThunk fs l a)
icThunk = thunk
newHSThunk :: (Typeable a,Eq a,ForestThunk fs HSThunk l,ForestLayer fs l) => ForestL fs l a -> ForestL fs l (ForestHSThunk fs l a)
newHSThunk = new

deriving instance Typeable FSThunk
deriving instance Typeable ICThunk
deriving instance Typeable HSThunk

instance (Eq a,ForestLayer fs l,ForestThunk fs FSThunk l,MData ctx (ForestL fs l) a
		, Sat (ctx (ForestFSThunk fs l a)),DeepTypeable (ForestFSThunk fs l a)
		) => MData ctx (ForestL fs l) (ForestFSThunk fs l a) where
	gfoldl ctx k z t = z new >>= flip k (read t)
	gunfold ctx k z c = z new >>= k
	toConstr ctx m = Data.WithClass.MData.dataTypeOf ctx m >>= (return . (flip indexConstr) 1)
	dataTypeOf ctx x = return ty
		where ty = mkDataType "Language.Forest.FS.FSRep.FSThunk" [mkConstr ty "FSThunk" [] Prefix]
		
instance (Eq a,ForestLayer fs l,ForestThunk fs ICThunk l,MData ctx (ForestL fs l) a
		, Sat (ctx (ForestICThunk fs l a)),DeepTypeable (ForestICThunk fs l a)
		) => MData ctx (ForestL fs l) (ForestICThunk fs l a) where
	gfoldl ctx k z t = z new >>= flip k (read t)
	gunfold ctx k z c = z new >>= k
	toConstr ctx m = Data.WithClass.MData.dataTypeOf ctx m >>= (return . (flip indexConstr) 1)
	dataTypeOf ctx x = return ty
		where ty = mkDataType "Language.Forest.FS.FSRep.ICThunk" [mkConstr ty "ICThunk" [] Prefix]

instance (Eq a,ForestLayer fs l,ForestThunk fs HSThunk l,MData ctx (ForestL fs l) a
		, Sat (ctx (ForestHSThunk fs l a)),DeepTypeable (ForestHSThunk fs l a)
		) => MData ctx (ForestL fs l) (ForestHSThunk fs l a) where
	gfoldl ctx k z t = z new >>= flip k (read t)
	gunfold ctx k z c = z new >>= k
	toConstr ctx m = Data.WithClass.MData.dataTypeOf ctx m >>= (return . (flip indexConstr) 1)
	dataTypeOf ctx x = return ty
		where ty = mkDataType "Language.Forest.FS.FSRep.HSThunk" [mkConstr ty "HSThunk" [] Prefix]

----------


proxyIncForest :: Proxy fs -> Proxy (IncForest fs)
proxyIncForest fs = Proxy

type ForestListICThunk fs l a = ListMod (ICThunk fs) l (IncForest fs) IORef IO a
type ForestListICThunk' fs l a = ListMod' (ICThunk fs) l (IncForest fs) IORef IO a
type ForestListICThunkI fs a = ForestListICThunk fs Inside a
type ForestListICThunkO fs a = ForestListICThunk fs Outside a
type ForestListICThunkI' fs a = ForestListICThunk' fs Inside a
type ForestListICThunkO' fs a = ForestListICThunk' fs Outside a

type ForestJoinListICThunk fs l a = JoinListMod (ICThunk fs) l (IncForest fs) IORef IO a
type ForestJoinListICThunk' fs l a = JoinListMod' (ICThunk fs) l (IncForest fs) IORef IO a
type ForestJoinListICThunkI fs a = ForestJoinListICThunk fs Inside a
type ForestJoinListICThunkO fs a = ForestJoinListICThunk fs Outside a
type ForestJoinListICThunkI' fs a = ForestJoinListICThunk' fs Inside a
type ForestJoinListICThunkO' fs a = ForestJoinListICThunk' fs Outside a

type ForestGenericQMemoBut ctx fs l b = GenericQMemoBut ctx (ICThunk fs) l (IncForest fs) IORef IO b
type ForestGenericQMemo ctx fs l b = GenericQMemo ctx (ICThunk fs) l (IncForest fs) IORef IO b

instance DeepTypeable fs => DeepTypeable (FSThunk fs) where
	typeTree (_::Proxy (FSThunk fs)) = MkTypeTree (mkName "Language.Forest.FS.FSRep.FSThunk") [typeTree (Proxy::Proxy fs)] []

instance DeepTypeable fs => DeepTypeable (ICThunk fs) where
	typeTree (_::Proxy (ICThunk fs)) = MkTypeTree (mkName "Language.Forest.FS.FSRep.ICThunk") [typeTree (Proxy::Proxy fs)] []
	
instance DeepTypeable fs => DeepTypeable (HSThunk fs) where
	typeTree (_::Proxy (HSThunk fs)) = MkTypeTree (mkName "Language.Forest.FS.FSRep.HSThunk") [typeTree (Proxy::Proxy fs)] []

instance (DeepTypeable fs,DeepTypeable l,DeepTypeable inc,DeepTypeable r,DeepTypeable m,DeepTypeable a) => DeepTypeable (FSThunk fs l inc r m a) where
	typeTree (_::Proxy (FSThunk fs l inc r m a)) = MkTypeTree (mkName "Language.Forest.FS.FSRep.FSThunk") args [MkConTree (mkName "Language.Forest.FS.FSRep.fsthunk") [typeTree (Proxy::Proxy a)]]
		where args = [typeTree (Proxy::Proxy fs),typeTree (Proxy::Proxy l),typeTree (Proxy::Proxy inc),typeTree (Proxy::Proxy r),typeTree (Proxy::Proxy r),typeTree (Proxy::Proxy m),typeTree (Proxy::Proxy a)]
	
instance (DeepTypeable fs,DeepTypeable l,DeepTypeable inc,DeepTypeable r,DeepTypeable m,DeepTypeable a) => DeepTypeable (ICThunk fs l inc r m a) where
	typeTree (_::Proxy (ICThunk fs l inc r m a)) = MkTypeTree (mkName "Language.Forest.FS.FSRep.ICThunk") args [MkConTree (mkName "Language.Forest.FS.FSRep.thunk") [typeTree (Proxy::Proxy a)]]
		where args = [typeTree (Proxy::Proxy fs),typeTree (Proxy::Proxy l),typeTree (Proxy::Proxy inc),typeTree (Proxy::Proxy r),typeTree (Proxy::Proxy r),typeTree (Proxy::Proxy m),typeTree (Proxy::Proxy a)]

instance (DeepTypeable fs,DeepTypeable l,DeepTypeable inc,DeepTypeable r,DeepTypeable m,DeepTypeable a) => DeepTypeable (HSThunk fs l inc r m a) where
	typeTree (_::Proxy (HSThunk fs l inc r m a)) = MkTypeTree (mkName "Language.Forest.FS.FSRep.HSThunk") args [MkConTree (mkName "Language.Forest.FS.FSRep.thunk") [typeTree (Proxy::Proxy a)]]
		where args = [typeTree (Proxy::Proxy fs),typeTree (Proxy::Proxy l),typeTree (Proxy::Proxy inc),typeTree (Proxy::Proxy r),typeTree (Proxy::Proxy r),typeTree (Proxy::Proxy m),typeTree (Proxy::Proxy a)]

-- | A type for twin-traversals
infixr 5 :.:
data a :.: b = a :.: b deriving (Typeable,Eq,Show,Ord) -- the forest datatype for pairs of arguments

fstF (x :.: y) = x
sndF (x :.: y) = y

instance (Memo a,Memo b) => Memo (a :.: b) where
	type Key (a :.: b) = (Key a,Key b)
	{-# INLINE memoKey #-}
	memoKey (x :.: y) = (andMkWeak wx wy,(kx,ky))
		where (wx,kx) = memoKey x
		      (wy,ky) = memoKey y

-- * @DeepTypeable@

$( derive makeDeepTypeable ''FS )

instance DeepTypeable LazyFS where
	typeTree (_::Proxy LazyFS) = MkTypeTree (mkName "Language.Forest.FS.FSRep.LazyFS") [] []
instance DeepTypeable NILFS where
	typeTree (_::Proxy NILFS) = MkTypeTree (mkName "Language.Forest.FS.FSRep.NILFS") [] []

instance DeepTypeable IncForest where
	typeTree (_::Proxy (IncForest)) = MkTypeTree (mkName "Language.Forest.FS.FSRep.IncForest") [] []

instance DeepTypeable fs => DeepTypeable (IncForest fs) where
	typeTree (_::Proxy (IncForest fs)) = MkTypeTree (mkName "Language.Forest.FS.FSRep.IncForest") [typeTree (Proxy::Proxy fs)] []

