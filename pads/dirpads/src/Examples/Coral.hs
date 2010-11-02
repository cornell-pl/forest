{-# LANGUAGE TypeSynonymInstances, TemplateHaskell, QuasiQuotes, MultiParamTypeClasses, FlexibleInstances, DeriveDataTypeable, ScopedTypeVariables #-}

module Examples.Coral where

import Language.Pads.Padsc
import Language.Forest.Forestc
import Language.Pads.GenPretty

import Data.Map (fromListWith, fold, toList)
import List (sortBy)
import System.IO.Unsafe (unsafePerformIO)

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
    , stats_proxy      :: Pre "[01]",  comma_ws
    , stats_level      :: Pint,      comma_ws
    , stats_lookup     :: Pint,      comma_ws
    , stats_xfer       :: Pint,      comma_ws
    , stats_total      :: Pint }

  type NoQuote = PstringME (RE "[^\"]*")

  type Generic = ('"',NoQuote,'"')

  type Url = Generic

  data Header = 
    { version       :: Pre "[12]",        comma_ws
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
    { "\"OUT\"",                            comma_ws 
    , out_remote    :: Pre "\"(REM|LOC)\"", comma_ws
    , out_req       :: Request,             comma_ws
    , out_referrer  :: Url,                 comma_ws
    , out_status    :: Status,              comma_ws
    , out_stats     :: Statistics,          comma_ws
    , out_forwarded :: Generic,             comma_ws
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
    { coral_log is "coralwebsrv.log.gz" :: Gzip (File CoralFile) }

  type SiteDir = [ d :: LogDir | d <- matches (RE "[0-9]{4}_[0-9]{2}_[0-9]{2}-[0-9]{2}_[0-9]{2}") ] 

  type TopDir = [ s :: SiteDir | s <- matches (RE "[^.].*") ] 
|]

load_logs () = 
  let (rep,md) = unsafePerformIO $ topDir_load "/home/nate/coraldata" in 
  rep

get_stats e = 
  case payload e of 
    In i -> in_stats i
    Out o -> out_stats o

get_total::Entry -> Int
get_total e = 
  case stats_total $ get_stats e of 
    Pint n -> n

string_of_url :: Url -> String
string_of_url (Url (Generic (NoQuote (PstringME s)))) = s

get_url::Entry -> String
get_url e =                     
  case payload e of 
    In i -> string_of_url (url $ in_req i)
    Out o -> string_of_url (url $ out_req o)

get_entries :: LogDir -> [Entry]
get_entries (LogDir (CoralFile (Entries es))) = es

get_hosts :: TopDir -> [(String,SiteDir)]
get_hosts (TopDir p) = p

get_date w = Prelude.take 10 w

get_dates :: SiteDir -> [(String,LogDir)]
get_dates (SiteDir p) = p

is_in :: Entry -> Bool
is_in e =
  case payload e of 
    In _ -> True
    Out _ -> False

is_out :: Entry -> Bool
is_out e = not (is_in e)

-- MAPS --
lmap f p tdir = 
   [ f host datetime e | (host,hdir) <- get_hosts tdir,
                         (datetime,ldir) <- get_dates hdir,
                          e <- get_entries ldir,
                          p e ]

by_date = lmap (\h d e -> (get_date d, get_total e))
by_host = lmap (\h d e -> (h, get_total e))
by_url_bytes = lmap (\h d e -> (get_url e, get_total e))
by_url_counts = lmap (\h d e -> (get_url e, 1))

-- FOLDS --
tdir = load_logs ()

go_bins m p = fromListWith (+) (m p tdir)

count_bins m = 
  fromListWith (+) (fold (\ c l -> (c,1):l) [] m)

go_flat p = 
  sum [ (get_total e) | (host,hdir) <- get_hosts tdir,
                        (datetime,ldir) <- get_dates hdir,
                        e <- get_entries ldir,
                        p e ]

sortDown (x1,t1) (x2,t2)
  | t1 < t2 = GT
  | t2 < t1 = LT
  | t1 == t2 = compare x2 x1

in_total = go_flat is_in
out_total = go_flat is_out
in_by_host = go_bins by_host is_in
out_by_host = go_bins by_host is_out 
in_by_date = go_bins by_host is_in 
out_by_date = go_bins by_host is_out
in_url_bytes = go_bins by_url_bytes is_in
out_url_bytes = go_bins by_url_bytes is_out
in_url_counts = go_bins by_url_counts is_in
out_url_counts = go_bins by_url_counts is_out
in_counts_urls = count_bins $ go_bins by_url_counts is_in
out_counts_urls = count_bins $ go_bins by_url_counts is_out
topk k m = Prelude.take k (sortBy sortDown (toList m))
