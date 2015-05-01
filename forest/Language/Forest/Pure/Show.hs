{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}

module Language.Forest.Pure.Show where

-- Generic @Show@ instance for forest in-memory representations

------------------------------------------------------------------------------

import System.Posix.Time
import System.Posix.Types
import Foreign.C.Types
import Language.Pads.Padsc
import Control.Monad
import Data.Data
import Text.ParserCombinators.ReadP
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Control.Monad.IO.Class
import Data.Generics.Aliases
import Language.Forest.FS.FSRep

------------------------------------------------------------------------------

-- | Generic show: an alternative to \"deriving Show\"
forestShow :: Data a => a -> String
forestShow x = gshows x ""

-- | Generic shows
gshows :: Data a => a -> ShowS
gshows = defshow `extQ` showCOff `extQ` showCMode `extQ` showCTime `extQ` showByteString `extQ` showString
  where aux a = showChar ' ' . gshows a
        defshow t = showChar '('
					. (showString . showConstr . toConstr $ t)
					. (foldr (.) id . gmapQ ((showChar ' ' .) . gshows) $ t)
					. showChar ')'

showCOff (x::COff) = shows x
showCMode (x::CMode) = shows x
showCTime (x::CTime) = shows x
showByteString (x::ByteString) = shows x




