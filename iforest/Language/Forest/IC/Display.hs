{-# LANGUAGE OverlappingInstances, UndecidableInstances, TypeOperators, ConstraintKinds, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts #-}

module Language.Forest.IC.Display where

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

import Language.Pads.Padsc as Pads
import Language.Pads.Source


import Control.Monad.Trans
import Data.DeepTypeable
import Data.ByteString.Char8

import Data.IORef

type ForestDisplay fs l a = Display l (IncForest fs) a

instance (ICRep fs,Display Outside (IncForest fs) a) => Display Outside (IncForest fs) (ForestM fs a) where
	displaysPrec l inc t rest = forestM t >>= flip (displaysPrec l inc) rest

-- displaying a manifest will evaluate the tests
instance (ICRep fs,ForestLayer fs Outside,Typeable fs,Show (FSTree fs)) => Display Outside (IncForest fs) (Manifest fs) where
	displaysPrec l inc man rest = do
		stests <- displaysPrec l inc (tests man) (')':rest)
		let sentries = showsPrec 0 (entries man) (' ':stests)
		return $ "(MakeManifest " ++ show (manifestTree man) ++ " " ++ sentries

instance (Layer l inc) => Display l inc Content where
	displaysPrec l inc x rest = return $ showsPrec 0 x rest

instance (Layer l inc) => Display l inc Status where
	displaysPrec l inc x rest = return $ showsPrec 0 x rest

instance (Layer l inc) => Display l inc ManifestEntry where
	displaysPrec l inc x rest = return $ showsPrec 0 x rest

-- * Display
	
instance (IncK (IncForest fs) a,Display Outside (IncForest fs) a,Output (ICThunk fs) l (IncForest fs)) => Display Outside (IncForest fs) (ICThunk fs l (IncForest fs) a) where
	displaysPrec l inc t rest = forceOutside t >>= \x ->  displaysPrec l inc x rest
	{-# INLINE displaysPrec #-}

instance (IncK (IncForest fs) a,Display Inside (IncForest fs) a,Output (ICThunk fs) Inside (IncForest fs)) => Display Inside (IncForest fs) (ICThunk fs Inside (IncForest fs) a) where
	displaysPrec l inc t rest = force t >>= \x ->  displaysPrec l inc x rest
	{-# INLINE displaysPrec #-}

instance (IncK (IncForest fs) a,Display Outside (IncForest fs) a,Input (FSThunk fs) l (IncForest fs)) => Display Outside (IncForest fs) (FSThunk fs l (IncForest fs) a) where
	displaysPrec l inc t rest = getOutside t >>= \x ->  displaysPrec l inc x rest
	{-# INLINE displaysPrec #-}

instance (IncK (IncForest fs) a,Display Inside (IncForest fs) a,Input (FSThunk fs) Inside (IncForest fs)) => Display Inside (IncForest fs) (FSThunk fs Inside (IncForest fs) a) where
	displaysPrec l inc t rest = get t >>= \x -> displaysPrec l inc x rest
	{-# INLINE displaysPrec #-}

instance (Display l inc a,Display l inc b) => Display l inc (a :.: b) where
	displaysPrec l inc (x :.: y) rest = do
		sy <- displaysPrec l inc y (')':rest)
		sx <- displaysPrec l inc x (":.:"++sy)
		return $ '(':sx

instance (Display l inc a,Display l inc b) => Display l inc (a :*: b) where
	displaysPrec l inc (x :*: y) rest = do
		sy <- displaysPrec l inc y (')':rest)
		sx <- displaysPrec l inc x (":*:"++sy)
		return $ '(':sx

instance (Display l inc (ForestFSThunk fs Inside FileInfo),Display l inc (ForestFSThunk fs Inside Forest_err)) => Display l inc (Forest_md fs) where
	displaysPrec l inc fmd rest = do
		sinfo <- displaysPrec l inc (fileInfo fmd) (')':rest)
		serrs <- displaysPrec l inc (errors fmd) (' ':sinfo)
		return $ "(Forest_md " ++ serrs
		
instance Layer l inc => Display l inc ByteString where
	displaysPrec l inc bstr rest = return $ show bstr ++ rest

instance Layer l inc => Display l inc FileInfo where
	displaysPrec l inc i rest = return $ show i ++ rest
instance Layer l inc => Display l inc Forest_err where
	displaysPrec l inc i rest = return $ show i ++ rest

instance Layer l inc => Display l inc Binary where
	displaysPrec l inc i rest = return $ show i ++ rest
instance Layer l inc => Display l inc Base_md where
	displaysPrec l inc i rest = return $ show i ++ rest


