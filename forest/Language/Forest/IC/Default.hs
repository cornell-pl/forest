{-# LANGUAGE RankNTypes, TupleSections, OverlappingInstances, KindSignatures, DataKinds, OverlappingInstances, UndecidableInstances, TypeOperators, ConstraintKinds, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts #-}

module Language.Forest.IC.Default where

import Control.Monad
import Language.Forest.FS.FSRep
import Language.Forest.IC.ICRep
import Language.Forest.Manifest
import Data.Typeable
import Control.Monad.Incremental.Display
import Control.Monad.Incremental
import Control.Monad.Incremental.Internal.Adapton.Types
import Control.Monad.Incremental.Internal.Adapton.Layers
import Language.Forest.IC.MetaData
--import Language.Forest.Pure.MetaData (FileInfo(..),FileType(..),(:*:)(..),Arg(..))
import Language.Forest.Errors

--import Language.Pads.Errors hiding (ErrMsg)
--import Language.Pads.MetaData
import Language.Pads.Source
--import Language.Pads.CoreBaseTypes

import System.Posix.Types
import Control.Monad.Trans
import Data.DeepTypeable
import Data.ByteString.Char8
import qualified Data.Map as Map
import Data.Map (Map(..))
import Data.WithClass.MData
import Data.ByteString as B
import Language.Haskell.TH.Syntax
import Data.Set as Set
import qualified Data.Data as Data
import qualified Data.Generics as Data

import Data.IORef

forestDefaultInvalid :: ForestDefault fs l a => ErrMsg -> ForestL fs l a
forestDefaultInvalid err = forestDefault Proxy Proxy $ Forest_err 1 (Just err)
forestDefaultValid :: ForestDefault fs l a => ForestL fs l a
forestDefaultValid = forestDefault Proxy Proxy cleanForestMDErr

class (ForestLayer fs l) => ForestDefault fs l a where
	forestDefault :: Proxy fs -> Proxy l -> Forest_err -> ForestL fs l a

data ForestDefaultDict fs l a = ForestDefaultDict { forestDefaultDict :: Proxy fs -> Proxy l -> Forest_err -> ForestL fs l a }

proxyForestDefaultDict :: Proxy fs -> Proxy l -> Proxy (ForestDefaultDict fs l)
proxyForestDefaultDict fs l = Proxy

instance ForestLayer fs l => ForestDefault fs l Char where
	forestDefault fs l err = return '\NUL'
instance ForestLayer fs l => ForestDefault fs l Int where
	forestDefault fs l err = return 0
instance ForestLayer fs l => ForestDefault fs l Integer where
	forestDefault fs l err = return 0
instance ForestLayer fs l => ForestDefault fs l Float where
	forestDefault fs l err = return 0
instance ForestLayer fs l => ForestDefault fs l Double where
	forestDefault fs l err = return 0
instance ForestLayer fs l => ForestDefault fs l COff where
	forestDefault fs l err = return 0
instance ForestLayer fs l => ForestDefault fs l EpochTime where
	forestDefault fs l err = return 0
instance ForestLayer fs l => ForestDefault fs l FileMode where
	forestDefault fs l err = return 0
instance ForestLayer fs l => ForestDefault fs l ByteString where
	forestDefault fs l err = return B.empty
instance ForestLayer fs l => ForestDefault fs l [a] where
	forestDefault fs l err = return []
instance ForestLayer fs l => ForestDefault fs l (Map k v) where
	forestDefault fs l err = return Map.empty
-- always uses the first constructor (this may not terminate for recursive types!)
instance (ForestLayer fs l,MData (ForestDefaultDict fs l) (ForestL fs l) a) => ForestDefault fs l a where
	forestDefault fs l err = forestDefaultGeneric fs l err (error "forestDefaultGeneric")
		where
		forestDefaultGeneric :: (ForestLayer fs l,MData (ForestDefaultDict fs l) (ForestL fs l) a) => Proxy fs -> Proxy l -> Forest_err -> a -> ForestL fs l a
		forestDefaultGeneric fs l err a = do
			d <- dataTypeOf (proxyForestDefaultDict fs l) a
			fromConstrB (proxyForestDefaultDict fs l) (forestDefaultDict dict fs l err) (indexConstr d 1)
			
instance (ForestLayer fs l) => ForestDefault fs l Forest_err where
	forestDefault fs l err = return err

mergeJustDefault :: (ICRep fs,ForestLayer fs l,ForestDefault fs l a,ForestDefault fs l b) => (Maybe a,Maybe b) -> ForestL fs l (a,b)
mergeJustDefault (Just x,Just y) = return (x,y)
mergeJustDefault (Just x,Nothing) = liftM (x,) forestDefaultValid
mergeJustDefault (Nothing,Just y) = liftM (,y) forestDefaultValid
mergeJustDefault (Nothing,Nothing) = do
	x <- forestDefaultValid
	y <- forestDefaultValid
	return (x,y)
	
-- * strictly copying @FSThunk@s

class (FSRep fs,ForestLayer fs l) => CopyFSThunks fs l a where
	copyFSThunks :: Proxy fs -> Proxy l -> (FilePath -> FilePath) -> a -> ForestL fs l a

copyFSThunksProxy :: Proxy fs -> Proxy l -> Proxy (CopyFSThunksDict fs l)
copyFSThunksProxy fs l = Proxy

data CopyFSThunksDict fs l a = CopyFSThunksDict { copyFSThunksDict :: Proxy fs -> Proxy l -> (FilePath -> FilePath) -> a -> ForestL fs l a }

instance (CopyFSThunks fs l a) => Sat (CopyFSThunksDict fs l a) where
	dict = CopyFSThunksDict { copyFSThunksDict = copyFSThunks }

-- we make a strict copy by forcing the original thunk
instance (IncK (IncForest fs) a,ForestLayer fs l,ForestInput fs FSThunk l) => CopyFSThunks fs l (ForestFSThunk fs l a) where
	copyFSThunks _ _ f t = get t >>= ref 

-- change fullpaths
instance (IncK (IncForest fs) FileInfo,IncK (IncForest fs) Forest_err,ForestInput fs FSThunk Inside,ForestLayer fs l) => CopyFSThunks fs l (Forest_md fs) where
	copyFSThunks _ _ f fmd = do
		errors' <- inside $ get (errors fmd) >>= ref
		fileInfo' <- inside $ get (fileInfo fmd) >>= ref . (\i -> i { fullpath = f (fullpath i) })
		return $ Forest_md errors' fileInfo'

-- just traverse recursively, until there are no more @FSThunks@ inside the type
instance (ForestLayer fs l,MData (CopyFSThunksDict fs l) (ForestL fs l) a) => CopyFSThunks fs l a where
	 copyFSThunks fs l f x = do
		let hasFSThunk (MkTypeTree name _ _) = showName name == "Language.Forest.FS.FSRep.FSThunk"
		if hasDeepTypeable hasFSThunk (proxyOf x)
			then gmapT (copyFSThunksProxy fs l) (copyFSThunksDict dict fs l f) x
			else return x



forestdefault ::  Data.GenericB
forestdefault = genericB forestdefault'

genericB :: (forall a. Data.Data a => a -> a) -> Data.GenericB
genericB gen = gen (error "genericB")

forestdefault' :: (forall a. (Data.Data a) => a -> a)
forestdefault' a = Data.ext1B (Data.ext2B (general a
		`Data.extB` char 
		`Data.extB` int
		`Data.extB` integer
		`Data.extB` float 
		`Data.extB` double 
		`Data.extB` coff
		`Data.extB` epochTime
		`Data.extB` fileMode
		`Data.extB` byteString)
		map)
		(list) where
	-- Generic case (does not guarantee termination for recursive types)
	general :: (Data.Data a) => a -> a
	general proxy = let d = Data.dataTypeOf proxy in Data.fromConstrB (forestdefault' (error "general")) (Data.indexConstr d 1)
	
	-- Base cases
	char    = '\NUL'
	int     = 0      :: Int
	integer = 0      :: Integer
	float   = 0.0    :: Float
	double  = 0.0    :: Double
	coff    = 0      :: COff
	epochTime = 0    :: EpochTime
	fileMode = 0     :: FileMode
	byteString = B.empty     :: B.ByteString
	list :: Data.Data b => [b]
	list   = []
	map :: (Map.Map k v)
	map = Map.empty
	set :: (Set.Set v)
	set = Set.empty