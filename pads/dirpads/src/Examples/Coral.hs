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

  type IP_Port = 
    { '"', 
      ip :: (Byte,'.',Byte,'.',Byte,'.', Byte), ":",
      port :: Pint, '"' }

  type Status = PstringME(status_re)

  type Statistics = 
    { stats_size       :: Pint,      comma_ws
    , stats_proxy      :: /"[01]"/,  comma_ws
    , stats_level      :: Pint,      comma_ws
    , stats_lookup     :: Pint,      comma_ws
    , stats_xfer       :: Pint,      comma_ws
    , stats_total      :: Pint }

  type NoQuote = PstringME (RE "[^\"]*")

  type Generic = ('"',NoQuote,'"')

  type Url = Generic

  data Header = 
    { version       :: /"[12]"/,        comma_ws
    , time          :: Time     }

  data Request = 
   { src       :: IP_Port, comma_ws
   , dst       :: IP_Port, comma_ws
   , url       :: Url } 

  data InData =
    { "\"IN\"",               comma_ws
    , in_req     :: Request,  comma_ws
    , in_status1 :: Status,   comma_ws
    , in_status2 :: Status,   comma_ws
    , in_stats   :: Statistics }

  data OutData = 
    { "\"OUT\"",                          comma_ws 
    , out_remote    :: /"\"(REM|LOC)\""/, comma_ws
    , out_req       :: Request,           comma_ws
    , out_referrer  :: Url,               comma_ws
    , out_status    :: Status,            comma_ws
    , out_stats     :: Statistics,        comma_ws
    , out_forwarded :: Generic,           comma_ws
    , out_via       :: Generic  }

  data InOut = In InData | Out OutData

  data Entry = 
    { header :: Header,   comma_ws
    , payload :: InOut
    , Eor }

  type Entries = [Entry] with term Eor
  
  type CoralFile = (Entries, Eof)
|]

[forest|
  type LogDir = Directory 
    { coral_log is "coralwebsrv.log" :: File CoralFile }

  type SiteDir = Directory
    { dates is [ d :: LogDir | d <- matches (RE "[0-9]{4}_[0-9]{2}_[0-9]{2}-[0-9]{2}_[0-9]{2}") ] }

  type TopDir = Directory
    { sites is [ s :: SiteDir | s <- matches (RE "[^.].*") ] }

|]

(c_rep,c_md) = unsafePerformIO $ topDir_load "/Users/nate/coral"