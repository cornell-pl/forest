module Language.Forest.Errors where
import Text.PrettyPrint.Mainland as PP
import Data.Data

data ErrMsg = ForestError String

{- XXX-KSF: fix pretty printing to use pretty printing combinators rather than string ++ -}
instance Pretty ErrMsg where
  ppr (ForestError str) = text ("Forest error: " ++ str ++ ".")

