{-# LANGUAGE DeriveDataTypeable #-}
module Language.Pads.RegExp where
import Data.Data


{- Regular expression support -}
data RE = RE String | ReName String
  deriving (Eq, Data, Typeable, Show)


-- generate re like [ab]
-- input: [a,b]
-- output RE "[ab]"
-- XXX: should escape ], ^, \, but underlying re library seems not to be able to handle these forms
charClass :: [Char] -> RE
charClass list = RE ("["++list++"]")

-- generate re like [ab]
-- input: [a,b]
-- output RE "[^a^b]"
-- XXX: should escape ], ^, \, but underlying re library seems not to be able to handle these forms
opCharClass :: [Char] -> RE
opCharClass list = RE ("[^" ++ list ++ "]")

chr :: Char -> RE
chr c = RE (c:[])

str :: String -> RE
str s = RE s

-- a | b
(.|.) :: RE -> RE -> RE
(RE l) .|. (RE r) = RE (l++['|']++r)

-- ab
(.&.) :: RE -> RE -> RE
(RE l) .&. (RE r) = RE (l++r)

-- kleene star
(.*.) :: RE -> RE
(.*.) (RE l) = RE (l++"*")

-- +
(.+.) :: RE -> RE
(.+.) (RE l) = RE (l++"+")

-- ?
(.?.) :: RE -> RE
(.?.) (RE l) = RE (l++"?")

dot :: RE
dot = RE "."

re_parens :: RE -> RE
re_parens (RE r) = RE ("("++r++")")

