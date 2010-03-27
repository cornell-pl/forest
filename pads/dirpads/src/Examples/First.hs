{-# LANGUAGE TemplateHaskell, QuasiQuotes, DeriveDataTypeable #-}

module Examples.First where

import Language.Pads.Padsc
import Language.Pads.Quote


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

