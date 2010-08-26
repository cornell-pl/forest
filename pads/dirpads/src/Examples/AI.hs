{-# LANGUAGE TypeSynonymInstances #-}

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

       type Time_t = {hours::Hours_t, ':', minutes :: Minutes_t, ':', seconds :: Seconds_t}  
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
example = Prelude.take 2 $ fst $ Prelude.head $ unGood $ runPP (parseMany' pdigit_parseM) (padsSourceFromString str)

str = "1234cnbdav duisc djnklcndjkalscnj dkxbvc daseasklfhasdjkhfaksjdhflakjsdhfkjlahsdfkljahsdlfkhasdkjfhaklsjdhflkashdfjkhjmzb"++ undefined
example2 =  (padsSourceFromString ("abc\nd" ++ undefined))
example3 = B.pack str