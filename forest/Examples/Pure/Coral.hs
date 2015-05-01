{-# LANGUAGE TypeFamilies, FlexibleContexts, TypeSynonymInstances, TemplateHaskell, QuasiQuotes, MultiParamTypeClasses, FlexibleInstances, DeriveDataTypeable, ScopedTypeVariables #-}

module Examples.Pure.Coral where

import Language.Pads.Padsc hiding (take)
import Language.Forest.Pure hiding (Entry,Status)
import Language.Forest.Pure.Graph (mdToPDF)
import Language.Pads.GenPretty
import Control.Arrow

import Data.Map as Map (fromListWith, fold, toList, toDescList)
import Data.List as List (sortBy,length,map)
import System.IO.Unsafe (unsafePerformIO)

comma_ws = REd ",[ \t]*" " "
status_re = REd "[0-9]+" "0"

[pads|
  type Time = (Int, ".", Int)

  type Byte = constrain x :: Int where <| 0 <= x && x <= 256 |>

  data IP_Port = IP_Port                                
    { '"', 
      ip   :: (Byte,'.',Byte,'.',Byte,'.', Byte), ":",
      port :: Int, '"' }

  type Status = StringME status_re
  
  data Statistics = Statistics
    { stats_size       :: Int,              comma_ws
    , stats_proxy      :: StringME '[01]',  comma_ws
    , stats_level      :: Int,              comma_ws
    , stats_lookup     :: Int,              comma_ws
    , stats_xfer       :: Int,              comma_ws
    , stats_total      :: Int }
  
  type NoQuote = StringME '[^\"]*'
  
  type Generic = ('"',NoQuote,'"')
  
  type Url = Generic
  
  data Header = Header
    { version       :: Maybe (StringME '[12],[ \t]*')
    , time          :: Time     }
  
  data Request = Request
   { src       :: IP_Port, comma_ws
   , dst       :: IP_Port, comma_ws
   , url       :: Url } 
  
  data InData = InData
    { "\"IN\"",               comma_ws
    , in_req     :: Request,  comma_ws
    , in_status1 :: Status,   comma_ws
    , in_status2 :: Status,   comma_ws
    , in_stats   :: Statistics }
  
  data OutData = OutData
    { "\"OUT\"",                                 comma_ws 
    , out_remote    :: StringME <| RE "\"(REM|LOC)\"" |>,  comma_ws
    , out_req       :: Request,                  comma_ws
    , out_referrer  :: Url,                      comma_ws
    , out_status    :: Status,                   comma_ws
    , out_stats     :: Statistics,               comma_ws
    , out_forwarded :: Generic,                  comma_ws
    , out_via       :: Generic  }
  
  data InOut = In InData | Out OutData
  
  data Entry = Entry
    { header  :: Header,   comma_ws
    , payload :: InOut
    , EOR }
  
  type Entries = [Entry] terminator EOR
  
  data Coral = Coral (Entries, EOF)
|]

[forest|
  type Log = Directory 
    { web is "coralwebsrv.log.gz" :: Gzip (File Coral),
      dns is "coraldnssrv.log.gz" :: Maybe (Gzip TextFile),
      prb is "probed.log.gz"      :: Maybe (Gzip TextFile),
      dmn is "corald.log.gz"      :: Maybe (Gzip TextFile) }

  type Site = [ d :: Log  | d <- matches <| RE "[0-9]{4}_[0-9]{2}_[0-9]{2}-[0-9]{2}_[0-9]{2}" |> ] 

  type Top  = [ s :: Site | s <- matches <| RE "[^.].*" |> ] 
|]

cRE         = RE "classof[0-9][0-9]" 

go = do
	(rep::Log,md::Log_md) <- load () "/home/nate/coraldata"  
	return (rep,md)
load_logs = fst(go)
load_md = snd(go)
  
graph = do
	(rep::Log,md::Log_md) <- load () "/home/nate/logs"
	return $ mdToPDF md "/home/nate/coral.dot"

get_stats e = 
  case payload e of 
    In i -> in_stats i
    Out o -> out_stats o

get_total::Entry -> Int
get_total e = stats_total $ get_stats e

get_url::Entry -> String
get_url e =                     
  case payload e of 
    In i ->  (url $ in_req i)
    Out o -> (url $ out_req o)

get_entries :: Log -> [Entry]
get_entries (Log_inner (Coral es) _ _ _) = es

get_sites :: Top -> [(String,Site)]
get_sites (Top p) = p

get_date w = Prelude.take 10 w

get_dates :: Site -> [(String,Log)]
get_dates (Site p) = p

is_incoming :: Entry -> Bool
is_incoming e =
  case payload e of 
    In _ -> True
    Out _ -> False

is_in = is_incoming

is_out :: Entry -> Bool
is_out e = not (is_incoming e)

-- MAPS --
lmap f p tdir = 
   [ f host datetime e | (host,hdir) <- get_sites tdir,
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
  sum [ (get_total e) | (host,hdir) <- get_sites tdir,
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
in_by_date = go_bins by_date is_in 
out_by_date = go_bins by_date is_out
in_url_bytes = go_bins by_url_bytes is_in
out_url_bytes = go_bins by_url_bytes is_out
in_url_counts = go_bins by_url_counts is_in
out_url_counts = go_bins by_url_counts is_out
in_counts_urls = count_bins $ go_bins by_url_counts is_in
out_counts_urls = count_bins $ go_bins by_url_counts is_out
-- topk k m = Prelude.take k (sortBy sortDown (toList m))


num_sites () = 
  case load_logs () of Top l -> List.length l

get_site = fst  
get_mod (_,(f,_)) = mod_time . fileInfo $ f 
sites_mod = do
	(Top rs, (_,ms)) <- go
	return $ map (get_site *** get_mod) (zip rs ms)



descBytes = sortDown
rep = load_logs ()
topk k = 
  take k $ sortBy descBytes $ toList $
  fromListWith (+)
    [ (get_url e, get_total e)
    | (site,sdir) <- get_sites rep,
      (datetime,ldir) <- get_dates sdir,
      e <- get_entries ldir,
      is_in e ]
