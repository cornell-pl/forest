{-# LANGUAGE TemplateHaskell, QuasiQuotes, DeriveDataTypeable #-}

{- Still to do:
    arrays
    switched unions
    defaults in unions
    regular expression literals (wait until new release of ghc)
    refactor code in Padsc.hs : parsing monad, etc.
    add routines to access files
    fix import declarations so don't have to include more than one file in First.hs
-}

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


[pads| type IntPair = (Pint, '|', Pint) |]

intPair_result = intPair_parseS "12|23"
-- ((IntPair (Pint 12,Pint 23),(Errors: 0,(Errors: 0,Errors: 0,Errors: 0))),"")

[pads| type Bar = (Pint, ',', IntPair, ';', Pint) |]            -- reference to another named type
bar_result = bar_parseS "256,12|23;456:"
-- ((Bar (Pint 256,IntPair (Pint 12,Pint 23),Pint 456),(Errors: 0,(Errors: 0,Errors: 0,(Errors: 0,(Errors: 0,Errors: 0,Errors: 0)),Errors: 0,Errors: 0))),":")

[pads| type Bar2 = (Pint, ',', (Pint,':',Pint), ';', Pint) |]   -- nested tuple type.
bar2_result = bar2_parseS "56,23:46;29"
-- ((Bar2 (Pint 56,(Pint 23,Pint 46),Pint 29),(Errors: 0,(Errors: 0,Errors: 0,(Errors: 0,(Errors: 0,Errors: 0,Errors: 0)),Errors: 0,Errors: 0))),"")

[pads| type BazR = Line (Pint, ',',Pint) |]                  -- type that consumes a line boundary.
bazr_result = bazR_parseS "33,33:"
-- ((BazR (Pint 33,Pint 33),(Errors: 0,(Errors: 0,Errors: 0,Errors: 0))),"")

bazr_input = "33,44\n55,66\n"
bazr_results = parseAllS bazR_parseM bazr_input
-- ([BazR (Pint 33,Pint 44),BazR (Pint 55,Pint 66)],[(Errors: 0,(Errors: 0,Errors: 0,Errors: 0)),(Errors: 0,(Errors: 0,Errors: 0,Errors: 0))])

[pads| type MyInt = Pint |]
myInt_result = myInt_parseS "23"
-- ((MyInt (Pint 23),Errors: 0),"")

testStrLen = 2
computeLen x = x - 1
[pads| type StrTy = PstringFW(:testStrLen + (computeLen 4):) |]

inputStrTy = "catdog"
strty_results = strTy_parseS inputStrTy
-- ((StrTy (PstringFW "catdo"),Errors: 0),"g")

[pads| type StrTy1 = Pstring(:'o':) |]
strty1_results = strTy1_parseS inputStrTy
-- ((StrTy1 (Pstring "catd"),Errors: 0),"og")

[pads| type Baz = (PstringFW(:3:),',',Pint) |]
input_baz  = "cat,123"
baz_results = baz_parseS input_baz
-- ((Baz (PstringFW "cat",Pint 123),(Errors: 0,(Errors: 0,Errors: 0,Errors: 0))),"")

[pads| type StrME = PstringME(:RE "a+":) |]
input_strME = "aaaab"
strME_results = strME_parseS input_strME

[pads| type  StrSE = PstringSE(:RE "b|c":) |]
input_strSE_1 = "aaaab"
input_strSE_2 = "aaaac"
strSE_results_1 = strSE_parseS input_strSE_1
strSE_results_2 = strSE_parseS input_strSE_2

[pads| type  StrP1 (x::Int) = PstringFW(:x - 1 :) |]
input_strP1 = "abcd"
strP1_result = strP1_parseS 3 input_strP1

[pads| type  StrHex = PstringME(:RE "[0-9A-Fa-f]+":) |]
input_strHex = "12abcds"
strHex_result = strHex_parseS input_strHex

{- Testing for Phex32FW, which is in Pads.Language.BaseTypes -}
input_hex32FW = "12bc34"  
strhex32FW_result = phex32FW_parseS 4 input_hex32FW   -- ((Phex32FW (Pint 4796),Errors: 0),"34")

input2_hex32FW = "00bc34"  
strhex32FW_result2 = phex32FW_parseS 4 input2_hex32FW    -- ((Phex32FW (Pint 188),Errors: 0),"34")

input3_hex32FW = "gbc34"  
strhex32FW_result3 = phex32FW_parseS 4 input3_hex32FW    -- Prints error message

[pads| type  HexPair = (Phex32FW(:2:), ',', Phex32FW(:3:)) |]
input_hexpair = "aa,bbb"
hexpair_result = hexPair_parseS input_hexpair

[pads| type  IntRange = x :: Pint where 0 <= x && x <= 256 |]
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

[pads| type  IntRangeP (low::Pint, high::Pint) = x :: Pint where low <= x && rep <= high && (numErrors md == 0)|]

result_intRangeP24 = intRangeP_parseS (0, 256) input_intRange24 
result_intRangeP0  = intRangeP_parseS (0, 256) input_intRange0  
result_intRangeP256 = intRangeP_parseS (0, 256) input_intRange256 
result_intRangePLow = intRangeP_parseS (0, 256) input_intRangeLow 
result_intRangePHigh = intRangeP_parseS (0, 256) input_intRangeHigh 
result_intRangePBad  = intRangeP_parseS (0, 256) input_intRangeBad 


[pads| type  Record (bound::Pint) = 
     {      i1 :: Pint, 
       ',', i2 :: Pint where <| i1 + i2 <= bound |>  
     } |]

input_Record = "24,45"
result_Record = record_parseS 100 input_Record



[pads| data Id =  Numeric Pint 
               |  Alpha   Pstring(:',':)  |] 

input_IdInt = "23"
result_IdInt = id_parseS input_IdInt

input_IdStr = "hello"
result_IdStr = id_parseS input_IdStr


[pads| data Id2 (bound::Pint ) = 
            Numeric2 Pint where <| numeric2 <= bound |> 
          | Alpha2   Pstring(:',':) |] 
input_IdInt2 = "23"
result_IdInt2 = id2_parseS 10 input_IdInt2

input_IdStr2 = "hello"
result_IdStr2 = id2_parseS 10 input_IdStr2

{- Fix the notation of arguments ? -}
[pads| data Id3  = Numeric3  IntRangeP(:(1,10):)
                 | Numeric3a Pint
                 | Lit3     ','                 |] 
input_IdInt3 = "24"
result_IdInt3 = id3_parseS input_IdInt3

input_IdStr3 = ","
result_IdStr3 = id3_parseS input_IdStr3


[pads| data Ab_or_a = AB "ab" | A "a" |]
input_AB = "ab"
result_Ab_or_a = ab_or_a_parseS input_AB

[pads| type  AB_test = { field_AB  :: Ab_or_a , 'b'} |]
input_AB_test1 = "abb"
input_AB_test2 = "ab"
result_AB_test1 = aB_test_parseS input_AB_test1
result_AB_test2 = aB_test_parseS input_AB_test2

[pads| data Method  = GET | PUT | LINK | UNLINK | POST  
       type Version = {"HTTP/", 
                        major :: Pint, '.',  -- major mode
                        minor :: Pint} 
|]

checkVersion :: Method -> Version -> Bool
checkVersion method version = 
  case method of
    LINK   -> major version == 1 && minor version == 0
    UNLINK -> major version == 1 && minor version == 0
    _ -> True

[pads| type Request = { '"',  method  :: Method,       
                        ' ',  url     :: Pstring(:' ':), 
                        ' ',  version :: Version where <| checkVersion method version |>, 
                        '"'
                      }  |]

input_method_get = "GET"
result_method_get = method_parseS input_method_get
input_method_put = "PUT"
result_method_put = method_parseS input_method_put
input_method_link = "LINK"
result_method_link = method_parseS input_method_link
input_method_post = "POST"
result_method_post = method_parseS input_method_post

input_Version = "HTTP/1.2"
result_Version = version_parseS input_Version

input_request_G = "\"PUT /www.google.com HTTP/1.0\""
result_request_G = request_parseS input_request_G
input_request_B = "\"LINK /www.google.com HTTP/1.3\""
result_request_B = request_parseS input_request_B

[pads| type Eor_Test = (Pint, Eor, Pint) |]
input_eor_test = "23\n56"
result_eor_test = eor_Test_parseS input_eor_test

[pads| type Eof_Test = (Pint, Eor, Pint, Eof) |]
input_eof_test_G = "23\n56"
result_eof_test_G = eof_Test_parseS input_eof_test_G
input_eof_test_B = "23\n56ab"
result_eof_test_B = eof_Test_parseS input_eof_test_B

[pads| type Opt_test = (Pint, '|', Maybe Pint, '|', Pint) |]
input_opt_test_j = "34|35|56"
result_opt_test_j = opt_test_parseS input_opt_test_j
input_opt_test_n = "34||56"
result_opt_test_n = opt_test_parseS input_opt_test_n
       
-- [pads| type Entries = [Pint] with (sep ','; term eof; length 10)  |]


---- Play space

re = BRE.mkRegexWithOptsS "^a+" True True
re_results1 = BRE.matchRegexAllS re "aaaab"
re_results2 = BRE.matchRegexAllS re "caaaab"


optIntP = parseOpt pint_parseM (0,cleanBasePD)
optIntPRec = parseLine optIntP

input2 = "33\n\n43"
optIntP_results = parseAllS optIntP input2
optIntPRec_results = parseAllS optIntPRec input2





