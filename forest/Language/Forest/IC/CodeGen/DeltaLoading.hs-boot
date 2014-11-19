module Language.Forest.IC.CodeGen.DeltaLoading where

import Language.Forest.Syntax
import Language.Haskell.TH as TH
import Control.Monad.Reader
import Data.Map
import Language.Forest.FS.FSRep
import Language.Forest.IC.Generic

-- for each variable name, we store (a boolean that indicates whether its value has NOT changed (changes are ALWAYS stable), the name of a thunk that holds its value, a pattern to match against the thunk's value)
type DeltaEnv = Map Name (TH.Exp,Maybe (Name,Pat))

type DeltaQ a = ReaderT (ICMode,DeltaEnv) Q a

--environments store maps from variables to (the name of the thunk that holds its value,a pattern to match against the thunk's value)
type Env = Map Name (Maybe (Name,Pat))
type EnvQ = ReaderT (ICMode,Env) Q

runDeltaQ :: DeltaQ a -> EnvQ a

runEnvQ :: EnvQ a -> DeltaQ a

loadDeltaE :: ForestTy -> TH.Exp -> TH.Exp -> TH.Exp -> TH.Exp -> TH.Exp -> TH.Exp -> DeltaQ TH.Exp

