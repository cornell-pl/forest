{-# LANGUAGE TypeSynonymInstances #-}

module Examples.Students where
import Language.Pads.Padsc
import System.IO.Unsafe (unsafePerformIO)

import qualified Data.ByteString.Lazy.Char8 as B

[pads|

type Studata = 

|]

-- function that returns last two digits of string s
-- don't know haskell indentation conventions for let
-- s must have at least 2 digits.  should use some kind of assert.
lasttwodigits s = 
  let n = length s in
  [s!!(n-2) , s!!(n-1)]

-- true if s is a student template file
-- I didn't know what Haskell disjunction was but found the function "or"
-- over lists ...
template s = or [ s == "SSSS.txt"
                , s == "SSS.txt"
                , s == "sxx.txt"
                , s == "sss.txt"
                , s == "ssss.txt" ]

[forest|

regexp stuname = .*[.txt]
regexp classname = [classof][0-9][0-9]
regexp transfer = [TRANSFER]|[Transfer]
regexp leave = [LEAVE]|[Leave]
regexp withdrawn = [WITHDRAWN]|[WITHDRAWAL]|[Withdrawn]|[Withdrawal]

type Major = [: s :: File Studata | s <- matches stuname where not (template s) :]

type Cla (year :: String) = Directory
  { bse is "BSE" ++ year :: Major
  , ab  is "AB" ++ year :: Major
  , transfer matches transfer :: Major option
  , withdrawn matches withdrawn :: Major option
  , leave matches leave :: Major option
  }

type Grads = [: dir :: Cla (lasttwodigits dir) | dir <- matches classname :]

type Top = Directory
  { classof10 :: Cla "10"
  , classof11 :: Cla "11"
  , graduates :: Grads
  }

|]

host_dir = "/Users/kfisher/pads/dirpads/src/Examples/data/facadm"

(fac_rep, fac_md) = unsafePerformIO $ Top_load "remote" host_dir