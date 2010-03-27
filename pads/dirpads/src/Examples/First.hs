{-# LANGUAGE TemplateHaskell, QuasiQuotes, DeriveDataTypeable #-}

module Examples.First where

import qualified Data.ByteString.Char8 as C
import Data.Map as Map
import Language.Pads.Syntax
import Language.Pads.Quote
import Language.Pads.Padsc
import Language.Pads.Source as S

import Text.Parsec
import Text.Parsec.String as PS
import Text.Parsec.Error
import qualified Text.Parsec.Token as PT
import Text.Parsec.Language
import Text.ParserCombinators.Parsec.Language 



[pads|Foo = (Pint, '|', Pint)|]

((rep,pd),rest) = foo_parseS "12|23"

[pads|Bar = (Pint, ',', Pint, ';', Pint) |]

[pads|Baz = (Pint, ',', (Pint,'|',Pint), ',', Pint) |]


{- 
[Pads| Method  = GET | PUT | LINK | UNLINK | POST 

       Version = {"HTTP", 
                  major :: Pint, "/",
                  minor :: Pint }

       Request = { '"', 
                   method  :: Method,       ' ',
                   url     :: Pstring('"'), ' ',
                   version :: Version, '"'
                 } where checkVersion method version |]
-}


checkVersion method version = 
  case method of
    LINK   -> major version == 1 && minor version == 0
    UNLINK -> major version == 1 && minor version == 0
    _ -> True


{-  Will generate: -}
data Method = GET | PUT | LINK | UNLINK | POST
  deriving (Eq, Ord)

data Version = Version { major :: Int
                       , minor :: Int
                       } deriving (Eq, Ord)

data Request = Request { method  :: Method
                       , url     :: String         
                       , version :: Version 
                       } deriving (Eq, Ord)

