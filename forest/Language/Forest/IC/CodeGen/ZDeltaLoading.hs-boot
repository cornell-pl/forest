module Language.Forest.IC.CodeGen.ZDeltaLoading where

import Language.Forest.Syntax
import Language.Haskell.TH as TH
import Control.Monad.Reader
import Data.Map
import Language.Forest.FS.FSRep
import Language.Forest.IC.Generic

-- for each variable name, we store (a boolean that indicates whether its value has NOT changed (changes are ALWAYS stable), the name of a thunk that holds its value, a pattern to match against the thunk's value)
type ZDeltaEnv = Map Name (TH.Exp,Maybe (Name,Pat))

type ZDeltaQ a = ReaderT (Name,ZDeltaEnv) Q a

--environments store maps from variables to (the name of the thunk that holds its value,a pattern to match against the thunk's value)
type ZEnv = Map Name (Maybe (Name,Pat))
type ZEnvQ = ReaderT (Name,ZEnv) Q

runZDeltaQ :: ZDeltaQ a -> ZEnvQ a

runZEnvQ :: ZEnvQ a -> ZDeltaQ a

zloadDeltaE :: ForestTy -> TH.Exp -> TH.Exp -> TH.Exp -> TH.Exp -> TH.Exp -> TH.Exp -> TH.Exp -> ZDeltaQ TH.Exp

