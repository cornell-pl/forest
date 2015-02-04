{-# LANGUAGE OverlappingInstances, UndecidableInstances, TypeOperators, ConstraintKinds, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts #-}

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
import Data.ByteString.Char8

import Data.IORef

type ForestDisplay fs l a = Display l (IncForest fs) IORef IO a

instance (ICRep fs,Display Outside (IncForest fs) IORef IO a) => Display Outside (IncForest fs) IORef IO (ForestM fs a) where
	displaysPrec l inc r m t rest = forestM t >>= flip (displaysPrec l inc r m) rest

-- displaying a manifest will evaluate the tests
instance (ICRep fs,ForestLayer fs Outside,Typeable fs,Show (FSTree fs)) => Display Outside (IncForest fs) IORef IO (Manifest fs) where
	displaysPrec l inc r m man rest = do
		stests <- displaysPrec l inc r m (tests man) (')':rest)
		let sentries = showsPrec 0 (entries man) (' ':stests)
		return $ "(MakeManifest " ++ show (manifestTree man) ++ " " ++ sentries

instance (Layer l inc r m) => Display l inc r m Content where
	displaysPrec l inc r m x rest = return $ showsPrec 0 x rest

instance (Layer l inc r m) => Display l inc r m Status where
	displaysPrec l inc r m x rest = return $ showsPrec 0 x rest

instance (Layer l inc r m) => Display l inc r m ManifestEntry where
	displaysPrec l inc r m x rest = return $ showsPrec 0 x rest

-- * Display
	
instance (IncK (IncForest fs) a,MonadLazy (Outside (IncForest fs) r m),Display Outside (IncForest fs) r m a,Output (ICThunk fs) l (IncForest fs) r m) => Display Outside (IncForest fs) r m (ICThunk fs l (IncForest fs) r m a) where
	displaysPrec l inc r m t rest = forceOutside t >>= \x -> lazily $ displaysPrec l inc r m x rest
	{-# INLINE displaysPrec #-}

instance (IncK (IncForest fs) a,MonadLazy (Inside (IncForest fs) r m),Display Inside (IncForest fs) r m a,Output (ICThunk fs) Inside (IncForest fs) r m) => Display Inside (IncForest fs) r m (ICThunk fs Inside (IncForest fs) r m a) where
	displaysPrec l inc r m t rest = force t >>= \x -> lazily $ displaysPrec l inc r m x rest
	{-# INLINE displaysPrec #-}

instance (IncK (IncForest fs) a,MonadLazy (Outside (IncForest fs) r m),Display Outside (IncForest fs) r m a,Input (FSThunk fs) l (IncForest fs) r m) => Display Outside (IncForest fs) r m (FSThunk fs l (IncForest fs) r m a) where
	displaysPrec l inc r m t rest = getOutside t >>= \x -> lazily $ displaysPrec l inc r m x rest
	{-# INLINE displaysPrec #-}

instance (IncK (IncForest fs) a,MonadLazy (Inside (IncForest fs) r m),Display Inside (IncForest fs) r m a,Input (FSThunk fs) Inside (IncForest fs) r m) => Display Inside (IncForest fs) r m (FSThunk fs Inside (IncForest fs) r m a) where
	displaysPrec l inc r m t rest = get t >>= \x -> lazily $ displaysPrec l inc r m x rest
	{-# INLINE displaysPrec #-}

instance (Display l inc r m a,Display l inc r m b) => Display l inc r m (a :.: b) where
	displaysPrec l inc r m (x :.: y) rest = do
		sy <- displaysPrec l inc r m y (')':rest)
		sx <- displaysPrec l inc r m x (":.:"++sy)
		return $ '(':sx

instance (Display l inc r m a,Display l inc r m b) => Display l inc r m (a :*: b) where
	displaysPrec l inc r m (x :*: y) rest = do
		sy <- displaysPrec l inc r m y (')':rest)
		sx <- displaysPrec l inc r m x (":*:"++sy)
		return $ '(':sx

instance (Display l inc r m (ForestFSThunk fs Inside Forest_err)) => Display l inc r m (Forest_md fs) where
	displaysPrec l inc r m fmd rest = do
		sinfo <- displaysPrec l inc r m (fileInfo fmd) (')':rest)
		serrs <- displaysPrec l inc r m (errors fmd) (' ':sinfo)
		return $ "(Forest_md " ++ serrs
		
instance Layer l inc r m => Display l inc r m ByteString where
	displaysPrec l inc r m bstr rest = return $ show bstr ++ rest

instance Layer l inc r m => Display l inc r m FileInfo where
	displaysPrec l inc r m i rest = return $ show i ++ rest
instance Layer l inc r m => Display l inc r m Forest_err where
	displaysPrec l inc r m i rest = return $ show i ++ rest

instance Layer l inc r m => Display l inc r m Binary where
	displaysPrec l inc r m i rest = return $ show i ++ rest
instance Layer l inc r m => Display l inc r m Base_md where
	displaysPrec l inc r m i rest = return $ show i ++ rest


