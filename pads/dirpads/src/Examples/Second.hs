{-# LANGUAGE TemplateHaskell, QuasiQuotes, DeriveDataTypeable #-}

module Examples.Second where

import Language.Pads.Padsc

import Language.Pads.Quote
import Language.Pads.Source
import Language.Haskell.TH as TH
import Language.Haskell.Meta
import Language.Pads.Pretty
import Text.PrettyPrint.Mainland
import qualified Text.Regex as RE
import Test.HUnit

--setupTest (a,b,c)  = TestCase(assertEqual a b (show c))


[pads| type IntPair = (Pint, '|', Pint) |]

intPair_result = intPair_parseS "12|23"
intPair_expects = "((IntPair (Pint 12,Pint 23),(Errors: 0,(Errors: 0,Errors: 0,Errors: 0))),\"\")"
test1 = TestCase(assertEqual  "intPair_result" intPair_expects (show intPair_result))

[pads| type Bar = (Pint, ',', IntPair, ';', Pint) |]            -- reference to another named type
bar_result = bar_parseS "256,12|23;456:"
bar_expects = "((Bar (Pint 256,IntPair (Pint 12,Pint 23),Pint 456),(Errors: 0,(Errors: 0,Errors: 0,(Errors: 0,(Errors: 0,Errors: 0,Errors: 0)),Errors: 0,Errors: 0))),\":\")"

test2 = TestCase(assertEqual "bar_result" bar_expects (show bar_result))



[pads| type Bar2 = (Pint, ',', (Pint,':',Pint), ';', Pint) |]   -- nested tuple type.
bar2_result = bar2_parseS "56,23:46;29"
bar2_expects = "((Bar2 (Pint 56,(Pint 23,Pint 46),Pint 29),(Errors: 0,(Errors: 0,Errors: 0,(Errors: 0,(Errors: 0,Errors: 0,Errors: 0)),Errors: 0,Errors: 0))),\"\")"

test3 = TestCase(assertEqual "bar2_result" bar2_expects (show bar2_result))



[pads| type BazR = Line (Pint, ',',Pint) |]                  -- type that consumes a line boundary.
bazr_result = bazR_parseS "33,33:"
bazr_expects = "((BazR (Pint 33,Pint 33),(Errors: 0,(Errors: 0,Errors: 0,Errors: 0))),\"\")"

test4 = TestCase(assertEqual "bazr_result" bazr_expects (show bazr_result))




bazr_input = "33,44\n55,66\n"
bazr_results = parseAllS bazR_parseM bazr_input
bazr_expects2 = "([BazR (Pint 33,Pint 44),BazR (Pint 55,Pint 66)],[(Errors: 0,(Errors: 0,Errors: 0,Errors: 0)),(Errors: 0,(Errors: 0,Errors: 0,Errors: 0))])"

test5 = TestCase(assertEqual "bazr_results2" bazr_expects2 (show bazr_results))



[pads| type MyInt = Pint |]
myInt_result = myInt_parseS "23"
myInt_expects = "((MyInt (Pint 23),Errors: 0),\"\")"

test6 = TestCase(assertEqual "myInt_expects" myInt_expects (show myInt_result))



testStrLen = 2
computeLen x = x - 1
[pads| type StrTy = PstringFW(:testStrLen + (computeLen 4):) |]

inputStrTy = "catdog"
strty_results = strTy_parseS inputStrTy
strty_expects = "((StrTy (PstringFW \"catdo\"),Errors: 0),\"g\")"

test7 = TestCase(assertEqual "strty_expects" strty_expects (show strty_results))



[pads| type StrTy1 = Pstring(:'o':) |]
strty1_results = strTy1_parseS inputStrTy
strty1_expects = "((StrTy1 (Pstring \"catd\"),Errors: 0),\"og\")"

test8 = TestCase(assertEqual "strty1_expects" strty1_expects (show strty1_results))


[pads| type Baz = (PstringFW(:3:),',',Pint) |]
input_baz  = "cat,123"
baz_results = baz_parseS input_baz
baz_expects =  "((Baz (PstringFW \"cat\",Pint 123),(Errors: 0,(Errors: 0,Errors: 0,Errors: 0))),\"\")"

test9 = TestCase(assertEqual "baz_expects" baz_expects (show baz_results))


tests = TestList[TestLabel "intpair" test1, TestLabel "bar" test2, TestLabel "three" test3, 
                           TestLabel "four" test4, TestLabel "five" test5, TestLabel "six" test6,
                           TestLabel "strty" test7,
                           TestLabel "strty1" test8,
                           TestLabel "baz" test9]
    




[pads| type StrME = PstringME(:RE "a+":) |]
input_strME = "aaaab"
strME_results = strME_parseS input_strME


[pads| type StrSE = PstringSE(:RE "b|c":) |]
input_strSE_1 = "aaaab"
input_strSE_2 = "aaaac"
strSE_results_1 = strSE_parseS input_strSE_1
strSE_results_2 = strSE_parseS input_strSE_2

[pads| type StrP1 (x::Pint) = PstringFW(:3:) |]
input_strP1 = "abcd"
strP1_result = strP1_parseS 3 input_strP1

---- Play space

re = RE.mkRegexWithOpts "^a+" True True
re_results1 = RE.matchRegexAll re "aaaab"
re_results2 = RE.matchRegexAll re "caaaab"


