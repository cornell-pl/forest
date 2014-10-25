{-# LANGUAGE UndecidableInstances, TypeOperators, ConstraintKinds, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts #-}

module Language.Forest.Display where

import Language.Forest.FS.FSRep
import Control.Monad.Incremental.Display
import Control.Monad.Incremental
import Control.Monad.Incremental.Adapton.Types
import Control.Monad.Incremental.Adapton.Layers
import Language.Forest.MetaData
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

-- * Display
	
instance (MonadLazy (Outside (IncForest fs) r m),Eq a,Display Outside (IncForest fs) r m a,Output (ICThunk fs) l (IncForest fs) r m) => Display Outside (IncForest fs) r m (ICThunk fs l (IncForest fs) r m a) where
	displaysPrec m rest = forceOutside m >>= \x -> lazily $ displaysPrec x rest
	{-# INLINE displaysPrec #-}

instance (MonadLazy (Inside (IncForest fs) r m),Eq a,Display Inside (IncForest fs) r m a,Output (ICThunk fs) Inside (IncForest fs) r m) => Display Inside (IncForest fs) r m (ICThunk fs Inside (IncForest fs) r m a) where
	displaysPrec m rest = force m >>= \x -> lazily $ displaysPrec x rest
	{-# INLINE displaysPrec #-}

instance (MonadLazy (Outside (IncForest fs) r m),Eq a,Display Outside (IncForest fs) r m a,Input (FSThunk fs) l (IncForest fs) r m) => Display Outside (IncForest fs) r m (FSThunk fs l (IncForest fs) r m a) where
	displaysPrec m rest = getOutside m >>= \x -> lazily $ displaysPrec x rest
	{-# INLINE displaysPrec #-}

instance (MonadLazy (Inside (IncForest fs) r m),Eq a,Display Inside (IncForest fs) r m a,Input (FSThunk fs) Inside (IncForest fs) r m) => Display Inside (IncForest fs) r m (FSThunk fs Inside (IncForest fs) r m a) where
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
