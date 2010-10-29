{-# LANGUAGE DeriveDataTypeable #-}

module Language.Forest.Errors where
import Text.PrettyPrint.Mainland as PP
import Data.Data

data ErrMsg = ForestError String
            | ForestIOException String
            | PredicateFailure
            | MissingFile String
            | MatchFailure String
            | NotADirectory String
            | ConstraintViolation
            | SystemError Int
     deriving (Typeable, Data, Show, Eq, Ord)

