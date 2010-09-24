{-# LANGUAGE TypeSynonymInstances, TemplateHaskell, QuasiQuotes, MultiParamTypeClasses, FlexibleInstances, DeriveDataTypeable, NamedFieldPuns #-}

module Examples.AI where
import Language.Pads.Padsc
import System.IO.Unsafe (unsafePerformIO)

import qualified Data.ByteString.Lazy.Char8 as B

[pads| type IP_t = (Pint, '.', Pint, '.', Pint, '.', Pint)
       type Host_t = Pstring(:' ':)
       data Source_t = IP IP_t | Host Host_t  

       data ID_t = Missing '-' | Id Pstring(:' ':)

       data Month_t = Jan | Feb | Mar | Apr | May | Jun | Jul | Aug | Sep | Oct | Nov | Dec  

       type Hours_t   = constrain h :: Pint where <| 0 <= h && h < 24 |>   
       type Minutes_t = constrain m :: Pint where <| 0 <= m && m < 60 |> 
       type Seconds_t = constrain s :: Pint where <| 0 <= s && s < 60 |> 

       data Time_t = {hours::Hours_t, ':', minutes :: Minutes_t, ':', seconds :: Seconds_t}  
       data Date_t = {day::Pint, '/', month::Month_t, '/', year::Pint}    

       type TimeZone_t = ('-', Pint)
       type TimeStamp_t = ('[', Date_t, ':', Time_t, ' ', TimeZone_t, ']')   
 
       data Method_t  = GET | PUT | POST | HEAD | DELETE
                      | LINK | UNLINK      -- obsolete after http 1.0
   
       type URL_t = Pstring(:' ':) 
       type Version_t  = {"HTTP/", major :: Pint, '.', minor :: Pint}  |]

checkVersion :: Method_t -> Version_t -> Bool
checkVersion method version = 
  case method of
    LINK   -> major version == 1 && minor version == 0
    UNLINK -> major version == 1 && minor version == 0
    _ -> True

[pads| type Request_t = { '"',  method  :: Method_t,       
                          ' ',  url     :: Pstring(:' ':), 
                          ' ',  version :: Version_t  where <| checkVersion method version |>, 
                          '"'
                        }  
       type Response_t = constrain r :: Pint where <| 100 <= r && r < 600 |> 
       data ContentLength_t = NotAvailable '-' | ContentLength Pint 
       data Entry_t = {      host       :: Source_t, 
                       ' ',  remoteID   :: ID_t, 
                       ' ',  authID     :: ID_t, 
                       ' ',  time       :: TimeStamp_t, 
                       ' ',  request    :: Request_t,
                       ' ',  response   :: Response_t,
                       ' ',  contentLen :: ContentLength_t }
       type EntryL_t = Line Entry_t
       type AI_t = [EntryL_t]                                        |]

-- [pads| derive instance Accum AI_t |]
{-
  Need to be able to:
    Given name of class, find members, including data members.
    Create instances.
-}

ai_file = "/Users/kfisher/pads/padsc/examples/data/ai.3000"
(ai_rep, ai_md) = let (AI_t rep, md) = unsafePerformIO $ parseFile ai_file in (rep,md)
ai_file_length = Prelude.length ai_rep
ai_file_take n  = Prelude.take n ai_rep

unGood (Good xs) = xs
example = Prelude.take 2 $ fst $ Prelude.head $ unGood $ runPP (parseMany pdigit_parseM) (padsSourceFromString str)

str = "1234cnbdav duisc djnklcndjkalscnj dkxbvc daseasklfhasdjkhfaksjdhflakjsdhfkjlahsdfkljahsdlfkhasdkjfhaklsjdhflkashdfjkhjmzb"++ undefined
example2 =  (padsSourceFromString ("abc\nd" ++ undefined))
example3 = B.pack str

pint_ppr (Pint x) = ppr x

instance Pretty Pint where
 ppr = pint_ppr 

pstring_ppr (Pstring s) = ppr s
instance Pretty Pstring where
 ppr = pstring_ppr


tuple_ppr ds = (text "(" <//>
--                    align (sep comma (map ppr ds )) <//>        
                    align (sep comma ds ) <//>        
                text ")")

record_ppr docs = (text "{" <//>
                    align (sep comma docs) <//>        
                 text "}")

iP_t_ppr (IP_t (r1,r2,r3,r4)) =  group $ hang 2 (text "IP_t" <+/> (tuple_ppr [ppr r1, ppr r2, ppr r3, ppr r4]))

instance Pretty IP_t where
 ppr = iP_t_ppr

host_t_ppr (Host_t h) = hang 2 (text "Host_t" <+/> ppr h)

instance Pretty Host_t where
 ppr = host_t_ppr

source_t_ppr source = case source of
  IP ip     -> hang 2 (text "IP" <+/> iP_t_ppr ip)
  Host host -> hang 2 (text "Host" <+/> host_t_ppr host)

instance Pretty Source_t where
  ppr = source_t_ppr

iD_t_ppr id = case id of
  Missing -> text ("Missing")
  Id s    -> hang 2 (text "Id" <+/> pstring_ppr s)

instance Pretty ID_t where
  ppr = iD_t_ppr

month_t_ppr month = case month of
  Jan -> text "Jan"
  Feb -> text "Feb"
  Mar -> text "Mar"
  Apr -> text "Apr"
  May -> text "May"
  Jun -> text "Jul"
  Aug -> text "Sep" 
  Oct -> text "Nov"
  Dec -> text "Dec"

instance Pretty Month_t where
  ppr = month_t_ppr

hours_t_ppr (Hours_t hours) = pint_ppr hours
instance Pretty Hours_t where
  ppr = hours_t_ppr

minutes_t_ppr (Minutes_t minutes) = pint_ppr minutes
instance Pretty Minutes_t where
  ppr = minutes_t_ppr

seconds_t_ppr (Seconds_t seconds) = pint_ppr seconds
instance Pretty Seconds_t where
  ppr = seconds_t_ppr

time_t_ppr (Time_t{hours, minutes, seconds}) = let
  hours_p   = text "hours"   <+> equals <+> (hours_t_ppr   hours)
  minutes_p = text "minutes" <+> equals <+> (minutes_t_ppr minutes)
  seconds_p = text "seconds" <+> equals <+> (seconds_t_ppr seconds)
  in hang 2 (text "Time_t" <+/>  record_ppr [hours_p, minutes_p, seconds_p])

instance Pretty Time_t where
  ppr = time_t_ppr

date_t_ppr (Date_t{day, month, year}) = let
  day_p    = text "day"   <+> equals <+> (pint_ppr   day)
  month_p  = text "month" <+> equals <+> (month_t_ppr month)
  year_p   = text "year"  <+> equals <+> (pint_ppr year)
  in hang 2 (text "Date_t" <+/>  record_ppr [day_p, month_p, year_p])

instance Pretty Date_t where
  ppr = date_t_ppr

timeZone_t_ppr (TimeZone_t (i)) =  group $ hang 2 (text "TimeZone_t" <+/> (tuple_ppr [ppr i]))
instance Pretty TimeZone_t where
  ppr = timeZone_t_ppr

timeStamp_t_ppr (TimeStamp_t (d,t,tz)) =  group $ hang 2 (text "TimeStamp_t" <+/> (tuple_ppr [ppr d, ppr t, ppr tz]))
instance Pretty TimeStamp_t where
  ppr = timeStamp_t_ppr

doc = iP_t_ppr (IP_t(123,45,67,88))
ip_s80 = pretty 80 doc
ip_s10 = pretty 15 doc

test = text "IP_t" <+/> (align ((text "nice") <+/> (text "world") <+/> (text "hello") ))

time1 = Time_t{hours= Hours_t(Pint 23), minutes= Minutes_t(Pint 24), seconds= Seconds_t(Pint 34)}
time1_doc = time_t_ppr time1
