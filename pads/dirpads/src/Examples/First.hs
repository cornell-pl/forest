{-# LANGUAGE TemplateHaskell, QuasiQuotes, DeriveDataTypeable #-}

module Examples.First where

import Language.Pads.Padsc

import Language.Pads.Quote
import Language.Pads.Source
import Language.Haskell.TH as TH
import Language.Haskell.Meta
import Language.Pads.Pretty
import Text.PrettyPrint.Mainland
import qualified Text.Regex as RE


[pads| IntPair = (Pint, '|', Pint) |]

intPair_result = intPair_parseS "12|23"

[pads|Bar = (Pint, ',', IntPair, ';', Pint) |]            -- reference to another named type
bar_result = bar_parseS "256,12|23;456:"

[pads|Bar2 = (Pint, ',', (Pint,':',Pint), ';', Pint) |]   -- nested tuple type.

[pads|BazR = Precord (Pint, ',',Pint) |]                  -- type that consumes a record boundary.
bazr_result = bazR_parseS "33,33:"

bazr_input = "33,44\n55,66\n"
bazr_results = parseAllS bazR_parseM bazr_input

[pads| MyInt = Pint |]
myInt_result = myInt_parseS "23"

testStrLen = 2
computeLen x = x - 1
[pads| StrTy = PstringFW(:testStrLen + (computeLen 4):) |]

inputStrTy = "catdog"
strty_results = strTy_parseS inputStrTy

[pads| StrTy1 = Pstring(:'o':) |]
strty1_results = strTy1_parseS inputStrTy

[pads|Baz = (PstringFW(:3:),',',Pint) |]
input_baz  = "cat,123"
baz_results = baz_parseS input_baz

[pads| StrME = PstringME(:RE "a+":) |]
input_strME = "aaaab"
strME_results = strME_parseS input_strME

[pads| StrSE = PstringSE(:RE "b|c":) |]
input_strSE_1 = "aaaab"
input_strSE_2 = "aaaac"
strSE_results_1 = strSE_parseS input_strSE_1
strSE_results_2 = strSE_parseS input_strSE_2

[pads| StrP1 (x::Pint) = PstringFW(:3:) |]
input_strP1 = "abcd"
strP1_result = strP1_parseS input_strP1

---- Play space

first (x::Pint) = "hello"

re = RE.mkRegexWithOpts "^a+" True True
re_results1 = RE.matchRegexAll re "aaaab"
re_results2 = RE.matchRegexAll re "caaaab"


optIntP = parseOpt pint_parseM (0,cleanBasePD)
optIntPRec = parseRecord optIntP

input2 = "33\n\n43"
optIntP_results = parseAllS optIntP input2
optIntPRec_results = parseAllS optIntPRec input2


str = "let b = True in if b then 3+4 else x"
parse_result = parseExp str
Right te = parse_result
teQ :: IO Exp = return te



{- 
[pads| Request = { i1 :: Pint, ',',
                   i2 :: Pint where i1 == i2 } |]

which leads to parsing code like:
  do { (rep_i1, md_i1) <- parse_PintM
       md_c <- parse_charLit ','
       (rep_i2, md_i3) <- parse_PintM
       let md = rep_i1 == rep_i2
       return ((rep_i1,rep_i2), md)
-}




{- 
[pads| Method  = GET | PUT | LINK | UNLINK | POST 

       Version = {"HTTP", 
                  major :: Pint, "/",
                  minor :: Pint}

       Request = { '"', 
                   method  :: Method,       ' ',
                   url     :: Pstring('"'), '"', ' ',
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

