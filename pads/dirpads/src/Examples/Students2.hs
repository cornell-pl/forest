{-# LANGUAGE TypeSynonymInstances, TemplateHaskell, QuasiQuotes, MultiParamTypeClasses, FlexibleInstances, DeriveDataTypeable, ScopedTypeVariables #-}

module Examples.Students where

import Language.Pads.Padsc
import Language.Forest.Forestc
import Language.Haskell.TH
import Language.Forest.Syntax
import Language.Forest.CodeGen
import System.Time.Utils
import System.IO.Unsafe (unsafePerformIO)

import Language.Haskell.Meta as LHM

ws = RE "[ \t]+"
ows =  RE "[ \t]*"
junk = RE ".*"
grade_RE = RE "[ABCD][+-]?|F|AUD|N|INC|P"

[pads| type White = PstringME(:ws:) |]
white_result = white_parseS "     h"

[pads| type Junk = PstringME(:junk:) |]
junk_result = junk_parseS "233(34h\nfred"

[pads| type Grade = PstringME(:grade_RE:) 
       type Grades = [Grade] with sep (:'|':) |]
grades_result = grades_parseS "A+|B-|C|F|AUD|N|INC|P"

[pads| 

type Grade_t = Maybe PstringME(:grade_RE:)

data Course = 
  { sort         :: /"[dto]"/,           ws
  , departmental :: /"[.D]"/,            ws
  , passfail     :: /"[.p]"/,            ws
  , level        :: /"[1234]"/,          ws
  , department   :: /"[A-Z][A-Z][A-Z]"/, ws
  , number       :: Pint where <| 100 <= number && number < 600 |>, ws
  , grade        :: Grade,               junk                               -- why doesn't this work if there is a maybe?
  } 

data Middle_name = {' ', middle :: /"[a-zA-Z]+[.]?"/ }           -- probably the middle name shouldn't be nullable

data Student_Name(myname::String) = 
  { lastname  :: /"[a-zA-Z]*"/  where <| toString lastname ==  myname |>,  ',', ows     -- yuck; we have too many different types.
  , firstname :: /"[a-zA-Z]*"/ 
  , middlename :: Maybe Middle_name
  }

data School = AB | BSE

data Person (myname::String) =
  { fullname   :: Student_Name(:myname:), ws
  , school     :: School,                 ws, '\''
  , year       :: /"[0-9][0-9]"/
  }

type Header  = [Line /".*"/] with term (: length of  7 :)
type Trailer = [Line /".*"/] with term (: Eof :)

data Student (name::String) = 
  { person :: Line (Person(:name:))
  , Header  
  , courses :: [Line Course]
  , Junk  
  }
|]

student_input_file = "/Users/kfisher/pads/dirpads/src/Examples/data/facadm/classof10/AB10/APPS.txt"
student_result :: (Student, Student_md) = unsafePerformIO $ parseFile1 "APPS" student_input_file

course_input = "d D . 3 JPN 238 INC"
course_result = course_parseS course_input

template s = or [ s == "SSSS.txt"
                , s == "SSS.txt"
                , s == "sxx.txt"
                , s == "sss.txt"
                , s == "ssss.txt" ]

splitExt s = let (dne, dotgeb) = Prelude.break (== '.') (reverse s) 
             in case dotgeb of 
                 [] -> (reverse dne, [])
                 '.':tser -> (reverse tser, reverse dne)
                 tser -> (reverse tser, reverse dne)

getName = fst . splitExt


[forest|
type Major_d = Directory 
     { students is [: s :: File (Student (: getName s :)) | s <- matches (RE "[A-Z]*.txt") where not (template s) :] }
|]

major_dir = "/Users/kfisher/pads/dirpads/src/Examples/data/facadm/classof11/AB11"
(major_rep, major_md) = unsafePerformIO $ major_d_load  major_dir


{-
-- another alternate using pads notation:
-- What should the representation type be for this list?

[pads|
type Student_file = {name :: /.*/, ".txt"}
|]

[forest|
type Major = [: s :: File (Student(name s)) 
              | s <= Student_file 
              where not (template s) :]
|]
-}


transfer = RE "TRANSFER|Transfer"
leave = RE "LEAVE|Leave"
withdrawn = RE "WITHDRAWN|WITHDRAWAL|Withdrawn|Withdrawal"

-- directory containing all students in a particular year
-- Opt is supposed to be an option type

[forest|
type Clss (year :: String) = Directory
  { bse is "BSE" ++ year :: Major
  , ab  is "AB" ++ year :: Major
  , transfer matches transfer :: Maybe Major 
  , withdrawn matches withdrawn :: Maybe Major 
  , leave matches leave :: Maybe Major 
  }
|]

{-
-- directory for all graduated students
-- using regexp matching again
type Grads = [: dir :: Clss year 
              | dir <= /classof(?<year>:[0-9][0-9])/ :]

{- alternate without regexp matching:

-- function that returns last two digits of string s
-- don't know haskell indentation conventions for let
-- s must have at least 2 digits.  should use some kind of assert.
lasttwodigits s = 
  let n = length s in
  [s!!(n-2) , s!!(n-1)]

type Grads = [: dir :: Clss (lasttwodigits dir) 
              | dir <- matches (RE "/classof[0-9][0-9]/") :]


another alternate using pads notation:

[pads|
type Classof = {'classof', year :: /[0-9][0-9]/}
]

[forest|
type Grads = [: dir :: Clss (year dir) | dir <= Classof :]
]

-}


-- top of the hierarchy
type Top = Directory
  { classof10 :: Clss "10"
  , classof11 :: Clss "11"
  , graduates :: Grads
  }

|]

-- the following two lines just copied without thought from other examples

host_dir = "/Users/kfisher/pads/dirpads/src/Examples/data/facadm"

(fac_rep, fac_md) = unsafePerformIO $ Top_load "remote" host_dir
-}