{-# LANGUAGE TypeSynonymInstances, TemplateHaskell, QuasiQuotes, MultiParamTypeClasses, FlexibleInstances, DeriveDataTypeable, ScopedTypeVariables #-}

module Examples.Coral where

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

[pads|
  type Time = (Pint, ".", Pint)

  type Byte = constrain x :: Pint where <| 0 <= x && x <= 256 |>

  type IP = (Byte,'.',Byte,'.',Byte,'.', Byte)  
  
  type IP_Port = (IP,':',Pint)

  type Status = 
       let s = ( "0|100|101|102" 
                ++ "|200|201|202|203|204|205|206|207"
                ++ "|300|301|302|303|304|305|306|307"
                ++ "|400|401|402|403|404|405|406|407|408|409|410"
                ++ "|411|412|413|414|415|416|417|418|422|423|424"
                ++ "|425|426|449|450"
                ++ "|500|501|502|503|504|505|506|507|508|509|510") in 
       PString_ME(RE s)

  type Statistics = 
    { size       :: Pint,      comma_ws
    , proxy      :: /"[01]"/,   comma_ws
    , level      :: Pint,      comma_ws
    , lookup     :: Pint,      comma_ws
    , xfer       :: Pint,      comma_ws
    , total      :: Pint }

  type Field = ('"', PString_ME(RE "[^\"]"), '"')

  data Pre = 
    { version       :: /"[12]"/,        comma_ws
    , time          :: Time    ,        comma_ws }

  data In = 
     { "IN",                 comma_ws 
     , src       :: IP_Port, comma_ws
     , dst       :: IP_Port, comma_ws
     , url       :: Url,     comma_ws
     , status1   :: Status,  comma_ws
     , status2   :: Status,  comma_ws
     , stats     :: Stats }    

  data Out = 
     { "OUT",                            comma_ws 
     , remote      :: /"\"(REM|LOC)\""/, comma_ws
     , src         :: IP_Port,           comma_ws
     , dst         :: IP_Port,           comma_ws
     , url1        :: Url,               comma_ws
     , url2        :: Url,               comma_ws
     , status      :: Status,            comma_ws
     , x_forwarded :: Field,             comma_ws
     , via         :: Field }

  data Entry = In | Out

  data Entries = [Entry] with term Eor
|]

coral_input_file = "/home/nate/coral-sample.log"
coral_result = (Entries, Entries_md) = usafePerformIO $ parseFile1 "coral-sample" coral_input_file