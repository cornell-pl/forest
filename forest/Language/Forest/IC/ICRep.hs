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
import Language.Forest.FS.FSDelta
import Data.Word
import Data.Time.Clock

import Data.DeepTypeable
import Data.WithClass.Derive.DeepTypeable
import Language.Haskell.TH.Syntax hiding (Loc(..))
import Language.Forest.FS.FSRep

-- class to traverse to the top-level thunk of a Forest representation, by dropping newtype tags
class ForestRep rep thunk | rep -> thunk where
	iso_rep_thunk :: Iso rep thunk

data Iso a b = Iso { to :: a -> b, from :: b -> a }

instance ForestRep (ForestFSThunk fs l rep) (ForestFSThunk fs l rep) where
	iso_rep_thunk = Iso id id

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
instance ICRep fs => Ord (ForestFSThunk fs l a) where
	compare t1 t2 = compareFSThunk t1 t2
instance ICRep fs => Ord (ForestICThunk fs l a) where
	compare t1 t2 = compareICThunk t1 t2

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
	compareFSThunk :: FSThunk fs l inc r m a -> FSThunk fs l inc r m a -> Ordering

	isUnevaluatedFSThunk :: (IncK (IncForest fs) a,ForestLayer fs l) => ForestFSThunk fs l a -> ForestL fs l Bool

	-- | incremental computation thunks that depend on @FSThunk@s and cannot be directly modified, but are updated for changes on dependencies
	data ICThunk fs (l :: * -> (* -> *) -> (* -> *) -> * -> *) (inc :: *) (r :: * -> *) (m :: * -> *) a :: *

	eqICThunk :: ICThunk fs l inc r m a -> ICThunk fs l inc r m a -> Bool
	compareICThunk :: ICThunk fs l inc r m a -> ICThunk fs l inc r m a -> Ordering

	-- | A plain thunk that simply stores a computation; cannot be mutated, and is not incrementally repaired
	data HSThunk fs (l :: * -> (* -> *) -> (* -> *) -> * -> *) (inc :: *) (r :: * -> *) (m :: * -> *) a :: *

-- * Value deltas

	-- the type for value modifications
	data ValueDelta fs a :: *
	
	-- has the content of a thunk changed? value changes since a given FSTree. recursive nodifications
	diffValueThunk :: (IncK (IncForest fs) content,Typeable content,ForestRep rep (ForestFSThunkI fs content)) => FSTree fs -> rep -> ForestO fs (ValueDelta fs rep)
	diffValueAny :: FSTree fs -> rep -> ForestO fs (ValueDelta fs rep)
	
	isIdValueDelta :: ValueDelta fs a -> Bool
	idValueDelta :: ValueDelta fs a
	chgValueDelta :: ValueDelta fs a
	mapValueDelta :: Proxy fs -> ValueDelta fs a -> ValueDelta fs b
	
	-- has the content of a thunk been explicitly modified? ignores recursive modifications
	diffTopValueThunk :: (IncK (IncForest fs) content,Typeable content,ForestRep rep (ForestFSThunkI fs content)) => FSTree fs -> rep -> ForestO fs (ValueDelta fs rep)

	-- sometimes a better approximation of a value diff
	diffValueBelow :: ValueDelta fs a -> (FSTree fs -> rep -> ForestO fs (ValueDelta fs rep)) -> FSTree fs -> rep -> ForestO fs (ValueDelta fs rep)
	diffValueBelow dv diffV tree rep = if isIdValueDelta dv
		then return idValueDelta
		else diffV tree rep

class ICRep fs => ICMemo (fs :: FS) where
	
	-- * Forest memoization operations to support incremental loading in the presence of FS moves (dummies if no moves are supported)
--	addMemoFile :: (ForestInput fs FSThunk Inside,Typeable rep,Typeable md,Typeable arg) => FilePath -> (ForestFSThunk fs Inside rep,ForestFSThunk fs Inside md,arg) -> FSTree fs -> ForestI fs ()
--	remMemoFile :: Typeable rep => Proxy fs -> FilePath -> Proxy rep -> ForestI fs ()
--	findMemoFile :: (Typeable rep,Typeable md,Typeable arg) => FilePath -> Proxy rep -> ForestI fs (Maybe ((ForestFSThunk fs Inside rep,ForestFSThunk fs Inside md,arg),FSTree fs))

	-- | adds a finalizer to a Forest Input thunk to be run whenever it is modified
--	addUnmemoFSThunk :: ForestLayer fs l => FSThunk fs l (IncForest fs) IORef IO a -> IO () -> ForestL fs l ()
	
	addMemo :: (Typeable (ForestIs fs args),Typeable rep,Typeable md) => FilePath -> Proxy args -> ForestIs fs args -> (rep,md) -> FSTree fs -> ForestI fs ()
	remMemo :: Proxy fs -> FilePath -> Proxy rep -> ForestI fs ()
	findMemo :: Proxy args -> FilePath -> Proxy rep -> ForestI fs (Maybe (FSTree fs,ForestIs fs args,rep,md))
	

type ForestThunk fs mod l = Thunk (mod fs) l (IncForest fs) IORef IO
type ForestOutput fs mod l = Output (mod fs) l (IncForest fs) IORef IO
type ForestInput fs mod l = Input (mod fs) l (IncForest fs) IORef IO

fsRef :: (IncK (IncForest fs) a,FSRep fs,ForestInput fs FSThunk l) => a -> ForestL fs l (ForestFSThunk fs l a)
fsRef = ref
fsThunk :: (IncK (IncForest fs) a,FSRep fs,ForestInput fs FSThunk l) => ForestL fs l a -> ForestL fs l (ForestFSThunk fs l a)
fsThunk = mod
icThunk :: (IncK (IncForest fs) a,FSRep fs,ForestOutput fs ICThunk l) => ForestL fs l a -> ForestL fs l (ForestICThunk fs l a)
icThunk = thunk
newHSThunk :: (IncK (IncForest fs) a,ForestThunk fs HSThunk l,ForestLayer fs l) => ForestL fs l a -> ForestL fs l (ForestHSThunk fs l a)
newHSThunk = new

deriving instance Typeable FSThunk
deriving instance Typeable ICThunk
deriving instance Typeable HSThunk

instance (IncK (IncForest fs) a,ForestLayer fs l,ForestThunk fs FSThunk l,MData ctx (ForestL fs l) a
		, Sat (ctx (ForestFSThunk fs l a)),DeepTypeable (ForestFSThunk fs l a)
		) => MData ctx (ForestL fs l) (ForestFSThunk fs l a) where
	gfoldl ctx k z t = z new >>= flip k (read t)
	gunfold ctx k z c = z new >>= k
	toConstr ctx m = Data.WithClass.MData.dataTypeOf ctx m >>= (return . (flip indexConstr) 1)
	dataTypeOf ctx x = return ty
		where ty = mkDataType "Language.Forest.FS.FSRep.FSThunk" [mkConstr ty "FSThunk" [] Prefix]
		
instance (IncK (IncForest fs) a,ForestLayer fs l,ForestThunk fs ICThunk l,MData ctx (ForestL fs l) a
		, Sat (ctx (ForestICThunk fs l a)),DeepTypeable (ForestICThunk fs l a)
		) => MData ctx (ForestL fs l) (ForestICThunk fs l a) where
	gfoldl ctx k z t = z new >>= flip k (read t)
	gunfold ctx k z c = z new >>= k
	toConstr ctx m = Data.WithClass.MData.dataTypeOf ctx m >>= (return . (flip indexConstr) 1)
	dataTypeOf ctx x = return ty
		where ty = mkDataType "Language.Forest.FS.FSRep.ICThunk" [mkConstr ty "ICThunk" [] Prefix]

instance (IncK (IncForest fs) a,ForestLayer fs l,ForestThunk fs HSThunk l,MData ctx (ForestL fs l) a
		, Sat (ctx (ForestHSThunk fs l a)),DeepTypeable (ForestHSThunk fs l a)
		) => MData ctx (ForestL fs l) (ForestHSThunk fs l a) where
	gfoldl ctx k z t = z new >>= flip k (read t)
	gunfold ctx k z c = z new >>= k
	toConstr ctx m = Data.WithClass.MData.dataTypeOf ctx m >>= (return . (flip indexConstr) 1)
	dataTypeOf ctx x = return ty
		where ty = mkDataType "Language.Forest.FS.FSRep.HSThunk" [mkConstr ty "HSThunk" [] Prefix]

type family ForestICThunks (fs :: FS) l args :: * where
	ForestICThunks fs l (a :*: b) = (ForestICThunks fs l a :*: ForestICThunks fs l b)
	ForestICThunks fs l (Arg a) = ForestICThunk fs l a
	ForestICThunks fs l () = ()
type ForestICThunksI fs args = ForestICThunks fs Inside args
type ForestICThunksO fs args = ForestICThunks fs Outside args

type family ForestLs (fs :: FS) l args :: * where
	ForestLs fs l (a :*: b) = (ForestLs fs l a :*: ForestLs fs l b)
	ForestLs fs l (Arg a) = ForestL fs l a
	ForestLs fs l () = ()
type ForestOs fs args = ForestLs fs Outside args
type ForestIs fs args = ForestLs fs Inside args
type family ForestFSThunks (fs :: FS) l args :: * where
	ForestFSThunks fs l (a :*: b) = (ForestFSThunks fs l a :*: ForestFSThunks fs l b)
	ForestFSThunks fs l (Arg a) = ForestFSThunk fs l a
	ForestFSThunks fs l () = ()
type ForestFSThunksI fs args = ForestFSThunks fs Inside args
type ForestFSThunksO fs args = ForestFSThunks fs Outside args

type family ForestVs args :: * where
	ForestVs (a :*: b) = (ForestVs a :*: ForestVs b)
	ForestVs (Arg a) = a
	ForestVs () = () 

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
instance DeepTypeable TxVarFS where
	typeTree (_::Proxy TxVarFS) = MkTypeTree (mkName "Language.Forest.FS.FSRep.TxVarFS") [] []
instance DeepTypeable TxICFS where
	typeTree (_::Proxy TxICFS) = MkTypeTree (mkName "Language.Forest.FS.FSRep.TxICFS") [] []
instance DeepTypeable TxNILFS where
	typeTree (_::Proxy TxNILFS) = MkTypeTree (mkName "Language.Forest.FS.FSRep.TxNILFS") [] []

instance DeepTypeable IncForest where
	typeTree (_::Proxy (IncForest)) = MkTypeTree (mkName "Language.Forest.FS.FSRep.IncForest") [] []

instance DeepTypeable fs => DeepTypeable (IncForest fs) where
	typeTree (_::Proxy (IncForest fs)) = MkTypeTree (mkName "Language.Forest.FS.FSRep.IncForest") [typeTree (Proxy::Proxy fs)] []

type FilePathFilter fs = FilePath -> ForestI fs FilePath

fsTreeDPathFilter :: ICRep fs => Proxy fs -> FSTreeD fs -> FilePath -> FilePathFilter fs
fsTreeDPathFilter fs df root path = if isParentPathOf root path
	then do
		let rel = makeRelative root path
		let td = focusFSTreeD fs df root rel path
		case td of
			(isMoveFSTreeD fs -> Just from) -> return from
			otherwise -> return path
	else return path
	