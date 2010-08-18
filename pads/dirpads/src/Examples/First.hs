{-# LANGUAGE TemplateHaskell, QuasiQuotes, DeriveDataTypeable #-}

{- Still to do:
    write desecription for ai data.
    add pretty printing for reps and pds
    improve error messages
    revise test suite to cover new cases and compare values semantically rather than as strings
    if a [pads| foo |] declaration doesn't start on the first column, get a weird error message
    regular expression literals (wait until new release of ghc)
    stringln base type (can express with regular expression; wait til new release of ghc)
    add pretty printers
    polymorphic types
-}

module Examples.First where

import Language.Pads.Padsc
import System.IO.Unsafe (unsafePerformIO)

import qualified Text.Regex.ByteString as BRE

---- Play space
re = BRE.mkRegexWithOptsS "^a+" True True
re_results1 = BRE.matchRegexAllS re "aaaab"
re_results2 = BRE.matchRegexAllS re "caaaab"

data Student = Student { name :: String, number:: Int}

---- PADS EXAMPLES

[pads| type MyChar = Pchar |]
myChar_result = myChar_parseS "ab"

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



[pads| type  IntRange = x :: Pint where <| 0 <= x && x <= 256 |> |]
input_intRange24 = "24"
input_intRange0  = "0"
input_intRange256 = "256"
input_intRangeLow = "-23"
input_intRangeHigh = "512"
input_intRangeBad  = "aaa"

result_intRange24 = intRange_parseS input_intRange24
-- ((IntRange (Pint 24),(Errors: 0,Errors: 0)),"")
result_intRange0  = intRange_parseS input_intRange0
-- ((IntRange (Pint 0),(Errors: 0,Errors: 0)),"")
result_intRange256 = intRange_parseS input_intRange256
-- ((IntRange (Pint 256),(Errors: 0,Errors: 0)),"")
result_intRangeLow = intRange_parseS input_intRangeLow
-- ((IntRange (Pint (-23)),(Errors: 1 Predicate is false.,Errors: 0)),"")
result_intRangeHigh = intRange_parseS input_intRangeHigh
-- ((IntRange (Pint 512),(Errors: 1 Predicate is false.,Errors: 0)),"")
result_intRangeBad  = intRange_parseS input_intRangeBad
-- ((IntRange (Pint 0),(Errors: 1 Predicate is true, but underlying type had an error. at: Line: 0, Offset: 0,Errors: 1 Encountered 'a' when expecting Pint. at: Line: 0, Offset: 0)),"aaa")

{- Note that the special variables "rep" and "md" are in scope in the body of the predicate. -}
{- Here rep is bound to the same value as x; md is the meta-data descriptor for the underyling type. -}

[pads| type  IntRangeP (low::Pint, high::Pint) = x :: Pint where <| low <= x && rep <= high && (numErrors md == 0) |> |]

result_intRangeP24 = intRangeP_parseS (0, 256) input_intRange24 
-- ((IntRangeP (Pint 24),(Errors: 0,Errors: 0)),"")
result_intRangeP0  = intRangeP_parseS (0, 256) input_intRange0  
-- ((IntRangeP (Pint 0),(Errors: 0,Errors: 0)),"")
result_intRangeP256 = intRangeP_parseS (0, 256) input_intRange256 
-- ((IntRangeP (Pint 256),(Errors: 0,Errors: 0)),"")
result_intRangePLow = intRangeP_parseS (0, 256) input_intRangeLow 
-- ((IntRangeP (Pint (-23)),(Errors: 1 Predicate is false.,Errors: 0)),"")
result_intRangePHigh = intRangeP_parseS (0, 256) input_intRangeHigh 
-- ((IntRangeP (Pint 512),(Errors: 1 Predicate is false.,Errors: 0)),"")
result_intRangePBad  = intRangeP_parseS (0, 256) input_intRangeBad 
-- ((IntRangeP (Pint 512),(Errors: 1 Predicate is false.,Errors: 0)),"")




[pads| type  Record (bound::Pint) = 
     {      i1 :: Pint, 
       ',', i2 :: Pint where <| i1 + i2 <= bound |>  
     } |]

input_Record = "24,45"
result_Record = record_parseS 100 input_Record
-- ((Record {i1 = Pint 24, i2 = Pint 45},(Errors: 0,Record_inner_md {i1_md = Errors: 0, i2_md = Errors: 0})),"")


[pads| data Id =  Numeric Pint 
               |  Alpha   Pstring(:',':)  |] 

input_IdInt = "23"
result_IdInt = id_parseS input_IdInt
-- ((Numeric (Pint 23),(Errors: 0,Numeric_md Errors: 0)),"")

input_IdStr = "hello"
result_IdStr = id_parseS input_IdStr
-- ((Alpha (Pstring "hello"),(Errors: 0,Alpha_md Errors: 0)),"")

[pads| data Id2 (bound::Pint ) = 
            Numeric2 Pint where <| numeric2 <= bound |> 
          | Alpha2   Pstring(:',':) |] 
input_IdInt2 = "23"
result_IdInt2 = id2_parseS 10 input_IdInt2
-- ((Alpha2 (Pstring "23"),(Errors: 0,Alpha2_md Errors: 0)),"")

input_IdStr2 = "hello"
result_IdStr2 = id2_parseS 10 input_IdStr2
-- ((Alpha2 (Pstring "hello"),(Errors: 0,Alpha2_md Errors: 0)),"")


[pads| data Id3  = Numeric3  IntRangeP(:(1,10):)
                 | Numeric3a Pint
                 | Lit3     ','                 |] 
input_IdInt3 = "24"
result_IdInt3 = id3_parseS input_IdInt3
-- ((Numeric3a (Pint 24),(Errors: 0,Numeric3a_md Errors: 0)),"")

input_IdStr3 = ","
result_IdStr3 = id3_parseS input_IdStr3
-- ((Lit3,(Errors: 0,Lit3_md Errors: 0)),"")

[pads| data Ab_or_a = AB "ab" | A "a" |]
input_AB = "ab"
result_Ab_or_a = ab_or_a_parseS input_AB
-- ((AB,(Errors: 0,AB_md Errors: 0)),"")

[pads| type  AB_test = { field_AB  :: Ab_or_a , 'b'} |]
input_AB_test1 = "abb"
input_AB_test2 = "ab"
result_AB_test1 = aB_test_parseS input_AB_test1
-- ((AB_test {field_AB = AB},(Errors: 0,AB_test_inner_md {field_AB_md = (Errors: 0,AB_md Errors: 0)})),"")
result_AB_test2 = aB_test_parseS input_AB_test2
-- ((AB_test {field_AB = A},(Errors: 0,AB_test_inner_md {field_AB_md = (Errors: 0,A_md Errors: 0)})),"")

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
-- ((GET,(Errors: 0,GET_md Errors: 0)),"")
input_method_put = "PUT"
result_method_put = method_parseS input_method_put
-- ((PUT,(Errors: 0,PUT_md Errors: 0)),"")
input_method_link = "LINK"
result_method_link = method_parseS input_method_link
input_method_post = "POST"
result_method_post = method_parseS input_method_post
-- ((POST,(Errors: 0,POST_md Errors: 0)),"")

input_Version = "HTTP/1.2"
result_Version = version_parseS input_Version
-- ((Version {major = Pint 1, minor = Pint 2},(Errors: 0,Version_inner_md {major_md = Errors: 0, minor_md = Errors: 0})),"")

input_request_G = "\"PUT /www.google.com HTTP/1.0\""
result_request_G = request_parseS input_request_G
{- ((Request {method = PUT, url = Pstring "/www.google.com", version = Version {major = Pint 1, minor = Pint 0}},
    (Errors: 0,Request_inner_md {method_md = (Errors: 0,PUT_md Errors: 0), 
                                    url_md = Errors: 0, 
                                    version_md = (Errors: 0,Version_inner_md {major_md = Errors: 0,  
                                                                              minor_md = Errors: 0})})),"")
-}

input_request_B = "\"LINK /www.google.com HTTP/1.3\""
result_request_B = request_parseS input_request_B
{-
   ((Request {method = LINK, url = Pstring "/www.google.com", version = Version {major = Pint 1, minor = Pint 3}},
    (Errors: 1,Request_inner_md {method_md = (Errors: 0,LINK_md Errors: 0), 
                                    url_md = Errors: 0,         
                                version_md = (Errors: 1 Predicate is false., Version_inner_md {major_md = Errors: 0, 
                                                                                               minor_md = Errors: 0})})),"")
-}

[pads| type Eor_Test = (Pint, Eor, Pint) |]
input_eor_test = "23\n56"
result_eor_test = eor_Test_parseS input_eor_test
-- ((Eor_Test (Pint 23,Pint 56),(Errors: 0,(Errors: 0,Errors: 0,Errors: 0))),"")

[pads| type Eof_Test = (Pint, Eor, Pint, Eof) |]
input_eof_test_G = "23\n56"
result_eof_test_G = eof_Test_parseS input_eof_test_G
-- ((Eof_Test (Pint 23,Pint 56),(Errors: 0,(Errors: 0,Errors: 0,Errors: 0,Errors: 0))),"")
input_eof_test_B = "23\n56ab"
result_eof_test_B = eof_Test_parseS input_eof_test_B
-- ((Eof_Test (Pint 23,Pint 56),(Errors: 1,(Errors: 0,Errors: 0,Errors: 0,Errors: 1 Extra bytes before literal: Eof. at: Line: 1, Offset: 2))),"ab")

[pads| type Opt_test = (Pint, '|', Maybe Pint, '|', Pint) |]
input_opt_test_j = "34|35|56"
result_opt_test_j = opt_test_parseS input_opt_test_j
-- ((Opt_test (Pint 34,Just (Pint 35),Pint 56),(Errors: 0,(Errors: 0,Errors: 0,(Errors: 0,Just Errors: 0),Errors: 0,Errors: 0))),"")

input_opt_test_n = "34||56"
result_opt_test_n = opt_test_parseS input_opt_test_n
-- ((Opt_test (Pint 34,Nothing,Pint 56),(Errors: 0,(Errors: 0,Errors: 0,(Errors: 0,Nothing),Errors: 0,Errors: 0))),"")
       

{- LIST EXAMPLES -}

[pads| type Entries_nosep_noterm = [PstringFW(:3:)] |]
input_entries_nosep_noterm = "123456789"
result_entries_nosep_noterm = entries_nosep_noterm_parseS input_entries_nosep_noterm
-- ((Entries_nosep_noterm [PstringFW "123",PstringFW "456",PstringFW "789"],(Errors: 0,[Errors: 0,Errors: 0,Errors: 0])),"")

input_entries_nosep_noterm' = "1234567890"
result_entries_nosep_noterm' = entries_nosep_noterm_parseS input_entries_nosep_noterm'
-- ((Entries_nosep_noterm [PstringFW "123",PstringFW "456",PstringFW "789"],(Errors: 0,[Errors: 0,Errors: 0,Errors: 0])),"0")

[pads| type Entries_nosep_noterm2 = [Pchar] |]
input_entries_nosep_noterm2 = ""
result_entries_nosep_noterm2 = entries_nosep_noterm2_parseS input_entries_nosep_noterm2
-- ((Entries_nosep_noterm2 [],(Errors: 0,[])),"")



[pads| type  EvenInt = x :: Pdigit where <| x `mod` 2 == 0 |> 
       type  EvenInts = [EvenInt] |]
input_evenInts = "2465"
result_evenInt = evenInt_parseS input_evenInts
-- ((EvenInt (Pdigit 2),(Errors: 0,Errors: 0)),"465")

result_evenInts = evenInts_parseS input_evenInts
-- ((EvenInts [EvenInt (Pdigit 2),EvenInt (Pdigit 4),EvenInt (Pdigit 6)],(Errors: 0,[(Errors: 0,Errors: 0),(Errors: 0,Errors: 0),(Errors: 0,Errors: 0)])),"5")


[pads| type DigitList = [Pdigit] with sep (:',':) |]
input_digitListG = "1,2,3"
input_digitList2G = "1,2,3|fed"
input_digitListB = "1,b,3"
result_digitListG = digitList_parseS input_digitListG
-- ((DigitList [Pdigit 1,Pdigit 2,Pdigit 3],(Errors: 0,[Errors: 0,Errors: 0,Errors: 0])),"")

result_digitList2G = digitList_parseS input_digitList2G
-- ((DigitList [Pdigit 1,Pdigit 2,Pdigit 3],(Errors: 0,[Errors: 0,Errors: 0,Errors: 0])),"|fed")

result_digitListB = digitList_parseS input_digitListB
-- ((DigitList [Pdigit 1],(Errors: 0,[Errors: 0])),",b,3")


[pads| type DigitListLen (x::Int) = [Pdigit] with term (:length of x + 1 :)  |]
input_digitListLenG = "123456"
input_digitListLenB = "12a456"

result_digitListLenG = digitListLen_parseS 4 input_digitListLenG
-- ((DigitListLen [Pdigit 1,Pdigit 2,Pdigit 3,Pdigit 4,Pdigit 5],(Errors: 0,[Errors: 0,Errors: 0,Errors: 0,Errors: 0,Errors: 0])),"6")
result_digitListLenB = digitListLen_parseS 4 input_digitListLenB
{- ((DigitListLen [Pdigit 1,Pdigit 2,Pdigit 0,Pdigit 4,Pdigit 5],
    (Errors: 1,[Errors: 0,Errors: 0,Errors: 1 Encountered a when expecting Pdigit. at: Line: 0, Offset: 2,Errors: 0,Errors: 0])),"6")
-}

[pads| type DigitListLenSep (x::Int) = [Pdigit] with term (:length of x + 1 :) and sep(:"ab":) |]
input_digitListLenSepG = "1ab2ab3ab4ab5ab6ab7ab"
input_digitListLenSepB = "1ab2ab3abDab5ab6ab7ab"
result_digitListLenSepG = digitListLenSep_parseS 4 input_digitListLenSepG
-- ((DigitListLenSep [Pdigit 1,Pdigit 2,Pdigit 3,Pdigit 4,Pdigit 5],(Errors: 0,[Errors: 0,Errors: 0,Errors: 0,Errors: 0,Errors: 0])),"ab6ab7ab")

result_digitListLenSepB = digitListLenSep_parseS 4 input_digitListLenSepB
{-
  ((DigitListLenSep [Pdigit 1,Pdigit 2,Pdigit 3,Pdigit 0,Pdigit 5],
   (Errors: 1,[Errors: 0,Errors: 0,Errors: 0,Errors: 1 Encountered D when expecting Pdigit. at: Line: 0, Offset: 9,Errors: 0])),"ab6ab7ab")
-}

[pads| type DigitListTerm = [Pdigit] with term (:Eor:)|]
input_digitListTermG = "12345\nhello"
result_digitListTermG = digitListTerm_parseS input_digitListTermG
-- ((DigitListTerm [Pdigit 1,Pdigit 2,Pdigit 3,Pdigit 4,Pdigit 5],(Errors: 0,[Errors: 0,Errors: 0,Errors: 0,Errors: 0,Errors: 0])),"hello")

input_digitListTermB = "12345,h"
result_digitListTermB = digitListTerm_parseS input_digitListTermB
{-
   ((DigitListTerm [Pdigit 1,Pdigit 2,Pdigit 3,Pdigit 4,Pdigit 5,Pdigit 0,Pdigit 0],
    (Errors: 2,[Errors: 0,Errors: 0,Errors: 0,Errors: 0,Errors: 0,
                Errors: 1 Encountered , when expecting Pdigit. at: Line: 0, Offset: 5,
                Errors: 1 Encountered h when expecting Pdigit. at: Line: 0, Offset: 6])),"")
-}

[pads| type DigitListTermSep = [Pdigit] with sep(:'|':) and term (:';':) |]
input_digitListTermSepG = "1|2|3|4|5|6;hello"
result_digitListTermSepG = digitListTermSep_parseS input_digitListTermSepG 
{-  ((DigitListTermSep [Pdigit 1,Pdigit 2,Pdigit 3,Pdigit 4,Pdigit 5,Pdigit 6],
    (Errors: 0,[Errors: 0,Errors: 0,Errors: 0,Errors: 0,Errors: 0,Errors: 0])),"hello")
-}

input_digitListTermSepB = "1|2|3|4|56;hello"
result_digitListTermSepB = digitListTermSep_parseS input_digitListTermSepB
{-
    ((DigitListTermSep [Pdigit 1,Pdigit 2,Pdigit 3,Pdigit 4,Pdigit 5],
    (Errors: 1 Extra bytes: 6 before seperator. at: Line: 0, Offset: 9,
      [Errors: 0,Errors: 0,Errors: 0,Errors: 0,Errors: 1 Extra bytes: 6 before seperator. at: Line: 0, Offset: 9])),"hello")
-}


[pads| type TryTest = (Try Pchar, PstringFW(:3:)) |]
input_tryTest = "abc123"
result_tryTest = tryTest_parseS input_tryTest
-- ((TryTest (Pchar 'a',PstringFW "abc"),(Errors: 0,(Errors: 0,Errors: 0))),"123")

[pads| type TryTestD = (Try Pdigit, PstringFW(:3:)) |]
input_tryTestDG = "123abc"
result_tryTestDG = tryTestD_parseS input_tryTestDG
-- ((TryTestD (Pdigit 1,PstringFW "123"),(Errors: 0,(Errors: 0,Errors: 0))),"abc")

input_tryTestDB = "abc123"
result_tryTestDB = tryTestD_parseS input_tryTestDB
{- ((TryTestD (Pdigit 0,PstringFW "abc"),
    (Errors: 1 Encountered a when expecting Pdigit. at: Line: 0, Offset: 0,(Errors: 1 Encountered a when expecting Pdigit. at: Line: 0, Offset: 0,Errors: 0))),"123")

  XXX: we are getting a repeat error message because of change ot how errors are propogated.  Need to work on cleaning up error reporting.
-}


[pads| type ListWithTry = ([Pchar] with term (:Try Pdigit:), Pdigit) |]
input_ListWithTry = "cat123"
result_ListWithTry = listWithTry_parseS input_ListWithTry
-- ((ListWithTry ([Pchar 'c',Pchar 'a',Pchar 't'],Pdigit 1),(Errors: 0,((Errors: 0,[Errors: 0,Errors: 0,Errors: 0]),Errors: 0))),"23")

[pads| type WithVoid = (Pchar, ',', Void, '|') |]
input_WithVoid = "a,|rest"
result_WithVoid = withVoid_parseS input_WithVoid
-- ((WithVoid (Pchar 'a'),(Errors: 0,(Errors: 0,Errors: 0,Errors: 0,Errors: 0))),"rest")

[pads| data VoidOpt   = PDigit Pdigit | Pcolor "red" | Pnothing Void 
       type VoidEntry = (VoidOpt, PstringFW(:3:))                    |]
input_voidEntry1 = "9abcdef"
result_voidEntry1 = voidEntry_parseS input_voidEntry1
-- ((VoidEntry (PDigit (Pdigit 9),PstringFW "abc"),(Errors: 0,((Errors: 0,PDigit_md Errors: 0),Errors: 0))),"def")

input_voidEntry2 = "redabcdef"
result_voidEntry2 = voidEntry_parseS input_voidEntry2
-- ((VoidEntry (Pcolor,PstringFW "abc"),(Errors: 0,((Errors: 0,Pcolor_md Errors: 0),Errors: 0))),"def")

input_voidEntry3 = "abcdef"
result_voidEntry3 = voidEntry_parseS input_voidEntry3
-- ((VoidEntry (Pnothing,PstringFW "abc"),(Errors: 0,((Errors: 0,Pnothing_md Errors: 0),Errors: 0))),"def")

[pads| data Switch (which :: Int) =  
         case <| which |> of
             0 ->         Even Pint  where <| even `mod` 2 == 0 |>
           | 1 ->         Comma   ','
           | otherwise -> Missing Void |] 
input_switch0 = "2hello"
input_switch1 = ",hello"
input_switchOther = "hello"

result_switch0 = switch_parseS 0 input_switch0
-- ((Even (Pint 2),(Errors: 0,Even_md Errors: 0)),"hello")
result_switch1 = switch_parseS 1 input_switch1
-- ((Comma,(Errors: 0,Comma_md Errors: 0)),"hello")
result_switchOther = switch_parseS 2 input_switchOther
-- ((Missing,(Errors: 0,Missing_md Errors: 0)),"hello")

[pads| data MyBody (which::Pint) = 
         case <| which |> of
            0         -> First Pint
          | 1         -> Second Pstring(:',':)
          | otherwise -> Other Void

       type MyEntry = { header  :: Pint, ','
                      , body    :: MyBody(:header:), ','
                      , trailer :: Pchar}  

       type MyData = [Line MyEntry]       |]

input_myData = "0,23,a\n1,hello,b\n2,,c"
result_myData = myData_parseS input_myData
{-
((MyData [MyEntry {header = Pint 0, body = First (Pint 23), trailer = Pchar 'a'},
          MyEntry {header = Pint 1, body = Second (Pstring "hello"), trailer = Pchar 'b'},
          MyEntry {header = Pint 2, body = Other, trailer = Pchar 'c'}],
 (Errors: 0,[(Errors: 0,MyEntry_inner_md {header_md = Errors: 0, body_md = (Errors: 0,First_md Errors: 0), trailer_md = Errors: 0}),
             (Errors: 0,MyEntry_inner_md {header_md = Errors: 0, body_md = (Errors: 0,Second_md Errors: 0), trailer_md = Errors: 0}),
             (Errors: 0,MyEntry_inner_md {header_md = Errors: 0, body_md = (Errors: 0,Other_md Errors: 0), trailer_md = Errors: 0})])),"")

-}

pintToInt (Pint i) = i
[pads| type HP = { student_num  :: Pint , ',', 
                   student_name :: PstringFW(:pintToInt student_num:) }
       type HP_data = [Line HP] |]   

input_hp_data = "8,Hermione\n3,Ron\n5,Harry"
test_hp_data = hP_data_parseS input_hp_data
{-
   ((HP_data [HP {student_num = Pint 8, student_name = PstringFW "Hermione"},
              HP {student_num = Pint 3, student_name = PstringFW "Ron"},
              HP {student_num = Pint 5, student_name = PstringFW "Harry"}],
    (Errors: 0,[(Errors: 0,HP_inner_md {student_num_md = Errors: 0, student_name_md = Errors: 0}),
                (Errors: 0,HP_inner_md {student_num_md = Errors: 0, student_name_md = Errors: 0}),
                (Errors: 0,HP_inner_md {student_num_md = Errors: 0, student_name_md = Errors: 0})])),"")
-}

test_file = "/Users/kfisher/pads/dirpads/src/Examples/test_file"
result_hp_data_file_parse :: (HP_data, HP_data_md) = unsafePerformIO $
                                                     parseFile test_file
{-
  (HP_data [HP {student_num = Pint 8, student_name = PstringFW "Hermione"},
            HP {student_num = Pint 3, student_name = PstringFW "Ron"},
            HP {student_num = Pint 5, student_name = PstringFW "Harry"}],
  (Errors: 0,[(Errors: 0,HP_inner_md {student_num_md = Errors: 0, student_name_md = Errors: 0}),
              (Errors: 0,HP_inner_md {student_num_md = Errors: 0, student_name_md = Errors: 0}),
              (Errors: 0,HP_inner_md {student_num_md = Errors: 0, student_name_md = Errors: 0})]))
-}