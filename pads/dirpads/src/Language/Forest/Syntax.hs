{-# LANGUAGE DeriveDataTypeable #-}

module Language.Forest.Syntax where

import Data.Generics
import Language.Haskell.TH  as TH 

newtype ForestDecl = ForestDecl (String, Maybe TH.Pat, ForestTy)
   deriving (Eq, Data, Typeable, Show)

data ForestTy = Directory DirectoryTy 
              | File FileTy 
              | Named String 
              | FMaybe ForestTy
              | Fapp ForestTy TH.Exp
   deriving (Eq, Data, Typeable, Show)

data DirectoryTy = Record String [Field]
   deriving (Eq, Data, Typeable, Show)

type FileTy = (String, Maybe TH.Exp)

type BasicField = (String, TH.Exp, ForestTy, Maybe TH.Exp)  -- internal name, external name, description type, optional predicate

data Generator = Explicit TH.Exp | Matches TH.Exp
    deriving (Eq, Data, Typeable, Show)

type CompField = (String, TH.Exp, ForestTy, TH.Pat, Generator, Maybe TH.Exp) 
     -- internal name, external expression, description type, comp pattern, generator expression, optional generator predicate

data Field = Simple BasicField
           | Comp  CompField
   deriving (Eq, Data, Typeable, Show)

