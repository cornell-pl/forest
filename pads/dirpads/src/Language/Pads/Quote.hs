module Language.Pads.Quote
    (pads,
     pqausiquote1)
    where

import Prelude hiding (exp, init)

import qualified Data.ByteString.Char8 as B hiding (init, inits)
import Data.Generics
import Foreign (unsafePerformIO)
import Language.Haskell.TH
import Language.Haskell.TH.Quote (QuasiQuoter(..))

import qualified Language.Pads.Parser as P
import qualified Language.Pads.Syntax as C
import Language.Pads.Padsc


parse :: Monad m
      => String
      -> P.Parser a
      -> String
      -> m a
parse fileName p input =
    case P.parse p fileName input of
      Left err -> unsafePerformIO $ fail $ show err
      Right x  -> return x


pparse1 p pToQ s
    = do  x <- Language.Pads.Quote.parse "filename" p s
          pToQ x

pqausiquote1 p
    = QuasiQuoter  (error "parse expression")
                   (error "parse pattern")
                   (error "parse type")
                   (pparse1 p make_pads_declarations)

pads :: QuasiQuoter
pads  = pqausiquote1 P.padsDecl
