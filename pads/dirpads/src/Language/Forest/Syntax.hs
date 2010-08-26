{-# LANGUAGE DeriveDataTypeable #-}

module Language.Forest.Syntax where

import Data.Generics
import Language.Haskell.TH  as TH hiding (Lit,CharL,StringL) 

data ForestTy = PadsTy String
   deriving (Eq, Data, Typeable, Show)

newtype ForestDecl = ForestDecl (Id, Maybe TH.Pat, ForestTy)
   deriving (Eq, Data, Typeable, Show)

data Id = Id String
        | AntiId String
    deriving (Eq, Ord, Data, Typeable, Show)

