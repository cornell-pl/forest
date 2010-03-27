{-# LANGUAGE DeriveDataTypeable #-}

module Language.Pads.Syntax where

import Data.Generics

data PadsTy = Pint | Ptuple [PadsTy] | Plit Char | Pname String
   deriving (Eq, Ord, Data, Typeable)

newtype PadsDecl = PadsDecl (Id, PadsTy)
   deriving (Eq, Ord, Data, Typeable)

data Id = Id String
        | AntiId String
    deriving (Eq, Ord, Data, Typeable)

