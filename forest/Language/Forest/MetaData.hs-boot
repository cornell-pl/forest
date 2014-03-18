module Language.Forest.MetaData where

import Data.Typeable

data FileInfo
instance Eq FileInfo where
instance Ord FileInfo where
instance Show FileInfo where
instance Typeable FileInfo where