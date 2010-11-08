{-# LANGUAGE TypeSynonymInstances, TemplateHaskell, QuasiQuotes, MultiParamTypeClasses, FlexibleInstances, DeriveDataTypeable, NamedFieldPuns, ScopedTypeVariables #-}

module Examples.AI where
import Language.Pads.Padsc hiding (str)
import Language.Pads.GenPretty       
import Control.Monad

import System.IO.Unsafe (unsafePerformIO)

import qualified Data.ByteString.Lazy.Char8 as B

[pads| type IP_t = (Pint, '.', Pint, '.', Pint, '.', Pint)
       type Host_t = Pstring ' '

       type IPList_t = [IP_t]

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
   
       type URL_t = Pstring ' ' 
       type Version_t  = {"HTTP/", major :: Pint, '.', minor :: Pint}  |]

checkVersion :: Method_t -> Version_t -> Bool
checkVersion method version = 
  case method of
    LINK   -> major version == 1 && minor version == 0
    UNLINK -> major version == 1 && minor version == 0
    _ -> True

[pads| 
  data Source_t = IP IP_t | Host Host_t  
  data ID_t = Missing '-' | Id (Pstring ' ')
  type Request_t = { '"',  method  :: Method_t,       
                     ' ',  url     :: Pstring ' ', 
                     ' ',  version :: Version_t  where <| checkVersion method version |>,  '"'
                    }  
  type Response_t = constrain r :: Pint where <| 100 <= r && r < 600 |> 
  data ContentLength_t = NotAvailable '-' | ContentLength Pint 
  data Entry_t = {      host       :: Source_t, 
                  ' ',  identdID   :: ID_t, 
                  ' ',  httpID     :: ID_t, 
                  ' ',  time       :: TimeStamp_t, 
                  ' ',  request    :: Request_t,
                  ' ',  response   :: Response_t,
                  ' ',  contentLen :: ContentLength_t }
  type AI_t     = [Line Entry_t]   
--  type EntryL_t = Line Entry_t

|]

mkPrettyInstance ''AI_t
mkPrettyInstance ''AI_t_md


ai_file = "/Users/kfisher/pads/padsc/examples/data/ai.3000"
(ai_rep, ai_md) = let (AI_t rep, md) = unsafePerformIO $ parseFile ai_file in (rep,md)
ai_file_length = Prelude.length ai_rep
ai_file_take n  = Prelude.take n ai_rep
result n  = do 
     { (AI_t rep, md) <- (parseFile ai_file )
     ; return (Prelude.take n ai_rep)
     } 


unGood (Good xs) = xs
example = Prelude.take 2 $ fst $ Prelude.head $ unGood $ runPP (parseMany pdigit_parseM) (padsSourceFromString str)

str = "1234cnbdav duisc djnklcndjkalscnj dkxbvc daseasklfhasdjkhfaksjdhflakjsdhfkjlahsdfkljahsdlfkhasdkjfhaklsjdhflkashdfjkhjmzb"++ undefined
example2 =  (padsSourceFromString ("abc\nd" ++ undefined))
example3 = B.pack str

first5 = AI_t (ai_file_take 5)
first5_doc = aI_t_ppr first5
output n = pretty n first5_doc

