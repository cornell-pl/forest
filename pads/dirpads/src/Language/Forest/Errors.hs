{-# LANGUAGE DeriveDataTypeable #-}

module Language.Forest.Errors where
import Text.PrettyPrint.Mainland as PP
import Data.Data

data ErrMsg = ForestError String
            | ForestIOException String
            | PredicateFailure
     deriving (Typeable, Data, Show, Eq)

{- XXX-KSF: fix pretty printing to use pretty printing combinators rather than string ++ -}
instance Pretty ErrMsg where
  ppr (ForestError str) = text ("Forest error: " ++ str ++ ".")
  ppr (ForestIOException str)   = text ("Forest IO Exception: " ++ str ++ ".")

