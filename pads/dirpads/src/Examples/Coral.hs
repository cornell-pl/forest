{-# LANGUAGE TypeSynonymInstances, TemplateHaskell, QuasiQuotes, MultiParamTypeClasses, FlexibleInstances, DeriveDataTypeable, ScopedTypeVariables #-}

module Examples.Coral where

import Language.Pads.Generic
import Language.Pads.Padsc
import Language.Forest.Forestc
import Language.Haskell.TH
import Language.Forest.Syntax
import Language.Forest.CodeGen
import System.Time.Utils
import System.IO.Unsafe (unsafePerformIO)

import Language.Haskell.Meta as LHM
import Data.Map

comma_ws = RE ",[ \t]*"

status_re = RE "[0-9]+"

[pads|
  type Time = (Pint, ".", Pint)

  type Byte = constrain x :: Pint where <| 0 <= x && x <= 256 |>

  type IP = (Byte,'.',Byte,'.',Byte,'.', Byte)  
  
  type IP_Port = (IP,':',Pint)

  type Status = PstringME(status_re)

  type Statistics = 
    { ssize       :: Pint,      comma_ws
    , sproxy      :: /"[01]"/,   comma_ws
    , slevel      :: Pint,      comma_ws
    , slookup     :: Pint,      comma_ws
    , sxfer       :: Pint,      comma_ws
    , stotal      :: Pint }

  type NoQuote = PstringME (RE "[^\"]")

  type Generic = ('"',NoQuote,'"')

  type Url = Generic

  data Header = 
    { version       :: /"[12]"/,        comma_ws
    , time          :: Time    ,        comma_ws }

  data Request = 
   { src       :: IP_Port, comma_ws
   , dst       :: IP_Port, comma_ws
   , url       :: Url } 

  data In = 
     { "IN",                 comma_ws 
     , in_req    :: Request, comma_ws
     , status1   :: Status,  comma_ws
     , status2   :: Status,  comma_ws
     , in_stats  :: Statistics }    

  data Out = 
     { "OUT",                            comma_ws 
     , remote      :: /"\"(REM|LOC)\""/, comma_ws
     , out_req     :: Request,           comma_ws
     , url2        :: Url,               comma_ws
     , status      :: Status,            comma_ws
     , out_stats   :: Statistics,        comma_ws
     , x_forwarded :: Generic,           comma_ws
     , via         :: Generic }

  type InOut = Out

  data Entry = 
     { header :: Header, 
       payload :: InOut }

  type Entries = [Entry] with term Eor
|]

coral_input_file = "/home/nate/coral-sample.log"
(res,md) = unsafePerformIO $ parseFile1 "coral-sample" coral_input_file