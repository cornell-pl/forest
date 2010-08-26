module Language.Forest.CodeGen where

import Language.Forest.Syntax as PS
--import Language.Forest.MetaData
--import Language.Forest.Generic
import qualified Language.Forest.Errors as E

import Language.Haskell.TH as TH
import Language.Haskell.TH.Syntax

import Data.Data
import Data.Char
import qualified Data.Map as M


{- Code generation routines -}
make_forest_declarations :: [ForestDecl] -> Q [Dec]
make_forest_declarations ds = fmap concat (mapM make_forest_declaration ds)

make_forest_declaration :: ForestDecl -> Q [Dec]
make_forest_declaration (ForestDecl (id, pat, forestTy)) = undefined

