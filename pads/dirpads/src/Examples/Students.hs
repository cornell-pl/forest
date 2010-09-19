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

[pads|

-- I'm using perl syntax for regexps.

-- at least one whitespace character
regexp ws = /\s+/

-- anything up to newline
regexp junk = /\.*/

regexp grade = /[ABCD][+-]?|F|AUD|N|INC|P/

type Course = 
  { sort         :: /[dto]/,           ws
  , departmental :: /[\.D]/,           ws
  , passfail     :: /[\.p]/,           ws
  , year         :: /[1234]/,          ws
  , department   :: /[A-Z][A-Z][A-Z]/, ws
  , number       :: Pint where <| 100 <= number < 600 |>, ws
  , grade        :: grade?, ws
  , junk
  }

type Middle_name = { ws, middle :: /a-zA-Z*\.?/ } 

type Name(myname::String) = 
  { lastname :: /[a-zA-Z]*/, ',', ws? where <| lastname == myname |>
  , firsname :: /[a-zA-Z]*/ 
  , middlename :: Opt Middle_name
  }

type School = AB | BSE

type Person (myname::String) =
  { name   :: Name(myname), ws
  , school :: School, ws, '\''
  , year   :: /[0-9][0-9]/
  }

-- 7 lines of junk to be thrown away.  not sure what syntax to use.
type Header = ArrayFixed (Line /.*/) 7

-- throw away everything else
type Junk = ArrayLongest (Line /.*/)

type Student (name::String) = 
  { person :: Line (Person(name))
  , Header  -- omit this
  , courses :: ArrayLongest (Line Course)
  , Junk  -- omit this too
  }
|]

-- true if s is a student template file
-- I didn't know what Haskell disjunction was but found the function "or"
-- over lists ...
template s = or [ s == "SSSS.txt"
                , s == "SSS.txt"
                , s == "sxx.txt"
                , s == "sss.txt"
                , s == "ssss.txt" ]

[forest|

regexp transfer = /TRANSFER|Transfer/
regexp leave = /LEAVE|Leave/
regexp withdrawn = /WITHDRAWN|WITHDRAWAL|Withdrawn|Withdrawal/

-- students associated with a particular major
-- I'm using a new comprehension form "id <= /regexp/ where boolean-exp"
-- to mean match regexp against file names in current directory
type Major = [: s :: File (Student name) 
              | s <= /(?<name>:.*).txt/ where not (template s) :]

{- alternate form, if we didn't have regexp binding:

-- deletes the ".txt" from the end of the file name
delete_terminator s = ...

type Major = [: s :: File (Student(delete_terminator s)) 
              | s <- matches (RE "/(?<name>:.*)\.txt/") 
              where not (template s) :]

-- another alternate using pads notation:

[pads|
type Student_file = {name :: /.*/, ".txt"}
|]

[forest|
type Major = [: s :: File (Student(name s)) 
              | s <= Student_file 
              where not (template s) :]
|]
-}


-- directory containing all students in a particular year
-- Opt is supposed to be an option type
type Clss (year :: String) = Directory
  { bse is "BSE" ++ year :: Major
  , ab  is "AB" ++ year :: Major
  , transfer matches transfer :: Opt Major 
  , withdrawn matches withdrawn :: Opt Major 
  , leave matches leave :: Opt Major 
  }

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