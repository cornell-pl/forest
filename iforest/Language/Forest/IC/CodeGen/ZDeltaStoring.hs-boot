module Language.Forest.IC.CodeGen.ZDeltaStoring where

import Language.Forest.Syntax
import Language.Haskell.TH as TH
import Control.Monad.Reader
import Data.Map
import Language.Forest.FS.FSRep
import Language.Forest.IC.Generic
import Language.Forest.IC.CodeGen.ZDeltaLoading


zmanifestDeltaE :: Bool -> ForestTy -> Exp -> Exp -> Exp -> Exp -> Exp -> Exp -> Exp -> Exp -> ZDeltaQ Exp

