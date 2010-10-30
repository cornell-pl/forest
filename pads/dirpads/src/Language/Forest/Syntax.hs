{-# LANGUAGE DeriveDataTypeable #-}

module Language.Forest.Syntax where

import Data.Generics
import Language.Haskell.TH  as TH 

newtype ForestDecl = ForestDecl (String, Maybe TH.Pat, ForestTy)
   deriving (Eq, Data, Typeable, Show)

data ForestTy = Directory DirectoryTy 
              | File FileTy 
              | Gzip ForestTy
              | Tar ForestTy
              | Named String 
              | FMaybe ForestTy
              | SymLink
              | FConstraint TH.Pat ForestTy TH.Exp    {- pattern bound to underlying type, underlying type, predicate -}
              | Fapp ForestTy TH.Exp
              | FComp CompField
   deriving (Eq, Data, Typeable, Show)

data DirectoryTy = Record String [Field]
   deriving (Eq, Data, Typeable, Show)

type FileTy = (String, Maybe TH.Exp)

type BasicField = (String, Bool, TH.Exp, ForestTy, Maybe TH.Exp)  -- internal name, isForm, external name, description type, optional predicate

data Generator = Explicit TH.Exp | Matches TH.Exp
    deriving (Eq, Data, Typeable, Show)


data CompField = CompField 
        { internalName :: String
        , tyConNameOpt :: Maybe String
        , explicitName :: Maybe String
        , externalE    :: TH.Exp
        , descTy       :: ForestTy
        , generatorP   :: TH.Pat
        , generatorG   :: Generator
        , predEOpt     :: Maybe TH.Exp
        }
   deriving (Eq, Data, Typeable, Show)

data Field = Simple BasicField
           | Comp  CompField
   deriving (Eq, Data, Typeable, Show)

