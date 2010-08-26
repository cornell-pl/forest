module Language.Forest.Quote
    (forest)
    where

import Prelude hiding (exp, init)
import Foreign (unsafePerformIO)

import Language.Haskell.TH
import Language.Haskell.TH.Quote (QuasiQuoter(..))

import Language.Forest.CodeGen
import qualified Language.Forest.Parser as P


parse :: Monad m
      => Loc
      -> P.Parser a
      -> String
      -> m a
parse loc p input = let
  fileName = loc_filename loc
  (line,column) = loc_start loc
  in case P.parse p fileName line column input of
       Left err -> unsafePerformIO $ fail $ show err
       Right x  -> return x


fparse1 p pToQ s
    = do  loc <- location
          x <- Language.Forest.Quote.parse loc p s
          pToQ x

fquasiquote1 p
    = QuasiQuoter  (error "parse expression")
                   (error "parse pattern")
                   (error "parse type")
                   (fparse1 p make_forest_declarations)

forest :: QuasiQuoter
forest  = fquasiquote1 P.forestDecls
    