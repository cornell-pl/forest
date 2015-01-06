{-# LANGUAGE UndecidableInstances, TypeOperators, ConstraintKinds, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts #-}

module Language.Forest.IC.Display where

import Language.Forest.FS.FSRep
import Language.Forest.IC.ICRep
import Language.Forest.Manifest
import Data.Typeable
import Control.Monad.Incremental.Display
import Control.Monad.Incremental
import Control.Monad.Incremental.Adapton.Types
import Control.Monad.Incremental.Adapton.Layers
import Language.Forest.IC.MetaData
import Language.Forest.Pure.MetaData (FileInfo(..),FileType(..),(:*:)(..),Arg(..))
import Language.Forest.Errors

import Language.Pads.Errors
import Language.Pads.MetaData
import Language.Pads.Source
import Language.Pads.CoreBaseTypes

import Control.Monad.Lazy
import Control.Monad.Trans
import Data.DeepTypeable

import Data.IORef

type ForestDisplay fs l a = Display l (IncForest fs) IORef IO a

instance (ICRep fs,Display Outside (IncForest fs) IORef IO a) => Display Outside (IncForest fs) IORef IO (ForestM fs a) where
	displaysPrec m rest = forestM m >>= flip displaysPrec rest

-- displaying a manifest will evaluate the tests
instance (ICRep fs,ForestLayer fs Outside,Typeable fs,Show (FSTree fs)) => Display Outside (IncForest fs) IORef IO (Manifest fs) where
	displaysPrec man rest = do
		stests <- displaysPrec (tests man) (')':rest)
		let sentries = showsPrec 0 (entries man) (' ':stests)
		return $ "(MakeManifest " ++ show (manifestTree man) ++ " " ++ sentries

instance (Layer l inc r m) => Display l inc r m Content where
	displaysPrec x rest = return $ showsPrec 0 x rest

instance (Layer l inc r m) => Display l inc r m Status where
	displaysPrec x rest = return $ showsPrec 0 x rest

instance (Layer l inc r m) => Display l inc r m ManifestEntry where
	displaysPrec x rest = return $ showsPrec 0 x rest

-- * Display
	
instance (Typeable a,MonadLazy (Outside (IncForest fs) r m),Eq a,Display Outside (IncForest fs) r m a,Output (ICThunk fs) l (IncForest fs) r m) => Display Outside (IncForest fs) r m (ICThunk fs l (IncForest fs) r m a) where
	displaysPrec m rest = forceOutside m >>= \x -> lazily $ displaysPrec x rest
	{-# INLINE displaysPrec #-}

instance (Typeable a,MonadLazy (Inside (IncForest fs) r m),Eq a,Display Inside (IncForest fs) r m a,Output (ICThunk fs) Inside (IncForest fs) r m) => Display Inside (IncForest fs) r m (ICThunk fs Inside (IncForest fs) r m a) where
	displaysPrec m rest = force m >>= \x -> lazily $ displaysPrec x rest
	{-# INLINE displaysPrec #-}

instance (Typeable a,MonadLazy (Outside (IncForest fs) r m),Eq a,Display Outside (IncForest fs) r m a,Input (FSThunk fs) l (IncForest fs) r m) => Display Outside (IncForest fs) r m (FSThunk fs l (IncForest fs) r m a) where
	displaysPrec m rest = getOutside m >>= \x -> lazily $ displaysPrec x rest
	{-# INLINE displaysPrec #-}

instance (Typeable a,MonadLazy (Inside (IncForest fs) r m),Eq a,Display Inside (IncForest fs) r m a,Input (FSThunk fs) Inside (IncForest fs) r m) => Display Inside (IncForest fs) r m (FSThunk fs Inside (IncForest fs) r m a) where
	displaysPrec m rest = get m >>= \x -> lazily $ displaysPrec x rest
	{-# INLINE displaysPrec #-}

instance (Display l inc r m a,Display l inc r m b) => Display l inc r m (a :.: b) where
	displaysPrec (x :.: y) rest = do
		sy <- displaysPrec y (')':rest)
		sx <- displaysPrec x (":.:"++sy)
		return $ '(':sx

instance (Display l inc r m a,Display l inc r m b) => Display l inc r m (a :*: b) where
	displaysPrec (x :*: y) rest = do
		sy <- displaysPrec y (')':rest)
		sx <- displaysPrec x (":*:"++sy)
		return $ '(':sx

instance (Display l inc r m (ForestFSThunk fs Inside Forest_err)) => Display l inc r m (Forest_md fs) where
	displaysPrec fmd rest = do
		sinfo <- displaysPrec (fileInfo fmd) (')':rest)
		serrs <- displaysPrec (errors fmd) (' ':sinfo)
		return $ "(Forest_md " ++ serrs

instance Layer l inc r m => Display l inc r m FileInfo where
	displaysPrec i rest = return $ show i ++ rest
instance Layer l inc r m => Display l inc r m Forest_err where
	displaysPrec i rest = return $ show i ++ rest

instance Layer l inc r m => Display l inc r m Binary where
	displaysPrec i rest = return $ show i ++ rest
instance Layer l inc r m => Display l inc r m Base_md where
	displaysPrec i rest = return $ show i ++ rest


