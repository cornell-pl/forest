{-# LANGUAGE TemplateHaskell, QuasiQuotes, DeriveDataTypeable #-}

module Examples.First where

import Language.Pads.Padsc
import Language.Pads.BaseTypes

import Language.Pads.Quote
import Language.Pads.Source

import Language.Haskell.TH as TH
import Language.Haskell.Meta
import Language.Pads.Pretty
import Text.PrettyPrint.Mainland
import qualified Text.Regex.ByteString as BRE


[pads| IntPair = (Pint, '|', Pint) |]

intPair_result = intPair_parseS "12|23"
-- ((IntPair (Pint 12,Pint 23),(Errors: 0,(Errors: 0,Errors: 0,Errors: 0))),"")

[pads|Bar = (Pint, ',', IntPair, ';', Pint) |]            -- reference to another named type
bar_result = bar_parseS "256,12|23;456:"
-- ((Bar (Pint 256,IntPair (Pint 12,Pint 23),Pint 456),(Errors: 0,(Errors: 0,Errors: 0,(Errors: 0,(Errors: 0,Errors: 0,Errors: 0)),Errors: 0,Errors: 0))),":")

[pads|Bar2 = (Pint, ',', (Pint,':',Pint), ';', Pint) |]   -- nested tuple type.
bar2_result = bar2_parseS "56,23:46;29"
-- ((Bar2 (Pint 56,(Pint 23,Pint 46),Pint 29),(Errors: 0,(Errors: 0,Errors: 0,(Errors: 0,(Errors: 0,Errors: 0,Errors: 0)),Errors: 0,Errors: 0))),"")

[pads|BazR = Pline (Pint, ',',Pint) |]                  -- type that consumes a line boundary.
bazr_result = bazR_parseS "33,33:"
-- ((BazR (Pint 33,Pint 33),(Errors: 0,(Errors: 0,Errors: 0,Errors: 0))),"")

bazr_input = "33,44\n55,66\n"
bazr_results = parseAllS bazR_parseM bazr_input
-- ([BazR (Pint 33,Pint 44),BazR (Pint 55,Pint 66)],[(Errors: 0,(Errors: 0,Errors: 0,Errors: 0)),(Errors: 0,(Errors: 0,Errors: 0,Errors: 0))])

[pads| MyInt = Pint |]
myInt_result = myInt_parseS "23"
-- ((MyInt (Pint 23),Errors: 0),"")

testStrLen = 2
computeLen x = x - 1
[pads| StrTy = PstringFW(:testStrLen + (computeLen 4):) |]

inputStrTy = "catdog"
strty_results = strTy_parseS inputStrTy
-- ((StrTy (PstringFW "catdo"),Errors: 0),"g")

[pads| StrTy1 = Pstring(:'o':) |]
strty1_results = strTy1_parseS inputStrTy
-- ((StrTy1 (Pstring "catd"),Errors: 0),"og")

[pads|Baz = (PstringFW(:3:),',',Pint) |]
input_baz  = "cat,123"
baz_results = baz_parseS input_baz
-- ((Baz (PstringFW "cat",Pint 123),(Errors: 0,(Errors: 0,Errors: 0,Errors: 0))),"")

[pads| StrME = PstringME(:RE "a+":) |]
input_strME = "aaaab"
strME_results = strME_parseS input_strME

[pads| StrSE = PstringSE(:RE "b|c":) |]
input_strSE_1 = "aaaab"
input_strSE_2 = "aaaac"
strSE_results_1 = strSE_parseS input_strSE_1
strSE_results_2 = strSE_parseS input_strSE_2

[pads| StrP1 (x::Int) = PstringFW(:x - 1 :) |]
input_strP1 = "abcd"
strP1_result = strP1_parseS 3 input_strP1

[pads| StrHex = PstringME(:RE "[0-9A-Fa-f]+":) |]
input_strHex = "12abcds"
strHex_result = strHex_parseS input_strHex

{- Testing for Phex32FW, which is in Pads.Language.BaseTypes -}
input_hex32FW = "12bc34"  
strhex32FW_result = phex32FW_parseS 4 input_hex32FW   -- ((Phex32FW (Pint 4796),Errors: 0),"34")

input2_hex32FW = "00bc34"  
strhex32FW_result2 = phex32FW_parseS 4 input2_hex32FW    -- ((Phex32FW (Pint 188),Errors: 0),"34")

input3_hex32FW = "gbc34"  
strhex32FW_result3 = phex32FW_parseS 4 input3_hex32FW    -- Prints error message

[pads| HexPair = (Phex32FW(:2:), ',', Phex32FW(:3:)) |]
input_hexpair = "aa,bbb"
hexpair_result = hexPair_parseS input_hexpair

[pads| IntRange = x :: Pint Pwhere 0 <= x && x <= 256 |]
input_intRange24 = "24"
input_intRange0  = "0"
input_intRange256 = "256"
input_intRangeLow = "-23"
input_intRangeHigh = "512"
input_intRangeBad  = "aaa"

result_intRange24 = intRange_parseS input_intRange24
result_intRange0  = intRange_parseS input_intRange0
result_intRange256 = intRange_parseS input_intRange256
result_intRangeLow = intRange_parseS input_intRangeLow
result_intRangeHigh = intRange_parseS input_intRangeHigh
result_intRangeBad  = intRange_parseS input_intRangeBad

{- Note that the special variables "rep" and "md" are in scope in the body of the predicate. -}
{- Here rep is bound to the same value as x; md is the meta-data descriptor for the underyling type. -}

[pads| IntRangeP (low::Pint, high::Pint) = x :: Pint Pwhere low <= x && rep <= high && (numErrors md == 0)|]

result_intRangeP24 = intRangeP_parseS (0, 256) input_intRange24 
result_intRangeP0  = intRangeP_parseS (0, 256) input_intRange0  
result_intRangeP256 = intRangeP_parseS (0, 256) input_intRange256 
result_intRangePLow = intRangeP_parseS (0, 256) input_intRangeLow 
result_intRangePHigh = intRangeP_parseS (0, 256) input_intRangeHigh 
result_intRangePBad  = intRangeP_parseS (0, 256) input_intRangeBad 


[pads| Request (bound::Pint) = 
           { i1 :: Pint, ',',
             i2 :: Pint Pwhere <| i1 + i2 <= bound |> } |]

input_Request = "24,45"
result_Request = request_parseS 100 input_Request


{-
data TestRecord = TestRecord{i::Pint, j::Pint}
data TestRecord_inner_md = TestRecord_inner_md{i_md::Base_md, j_md::Base_md}

x = do
   (i, i_md) <- pint_parseM
   let bmd_0 = Language.Pads.Padsc.get_md_header i_md
   md_1 <- pcharLit_parseM ','
   let bmd_2 = Language.Pads.Padsc.get_md_header md_1
   (j, j_md) <- pint_parseM
   let bmd_3 = Language.Pads.Padsc.get_md_header j_md
   let top_md = Language.Pads.Padsc.mergeBaseMDs [bmd_0, bmd_2, bmd_3]
   return (TestRecord{i = i, j = j},
          (top_md, TestRecord_inner_md{i_md = i_md, j_md = j_md}))
-}

---- Play space

first (x::Pint) = "hello"

re = BRE.mkRegexWithOptsS "^a+" True True
re_results1 = BRE.matchRegexAllS re "aaaab"
re_results2 = BRE.matchRegexAllS re "caaaab"


optIntP = parseOpt pint_parseM (0,cleanBasePD)
optIntPRec = parseLine optIntP

input2 = "33\n\n43"
optIntP_results = parseAllS optIntP input2
optIntPRec_results = parseAllS optIntPRec input2


str = "let b = True in if b then 3+4 else x"
parse_result = parseExp str
Right te = parse_result
teQ :: IO Exp = return te



{- 

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



checkVersion method version = 
  case method of
    LINK   -> major version == 1 && minor version == 0
    UNLINK -> major version == 1 && minor version == 0
    _ -> True

-}
{-  Will generate: 
data Method = GET | PUT | LINK | UNLINK | POST
  deriving (Eq, Ord)

data Version = Version { major :: Int
                       , minor :: Int
                       } deriving (Eq, Ord)

data Request = Request { method  :: Method
                       , url     :: String         
                       , version :: Version 
                       } deriving (Eq, Ord)

-}