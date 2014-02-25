module Coral_parser where

import List
import Numeric
import Data.Time.LocalTime
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Word 
import Text.ParserCombinators.Parsec

data IP = IP (Word8,Word8,Word8,Word8)
instance Show IP where 
  show (IP (b1,b2,b3,b4)) = show b1 ++ "." ++ show b2 ++ "." ++ show b3 ++ "." ++ show b4

data Addr = Addr(IP,Int)
instance Show Addr where
  show (Addr (ip,port)) = show ip ++ ":" ++ show port

data Header = Header { version::Int, time::LocalTime } 
     deriving Show

data Statistics = Statistics { status::Int, size::Int, proxy::Bool, level::Int, lookup_time::Int, xfer_time::Int, total_time::Int }
     deriving Show

data Coral = 
       In { header::Header, source::Addr, destination::Addr, request::String, statistics::Statistics, referrer_status::Int }
     | Out { header::Header, remote::Bool, source::Addr, destination::Addr, request::String, referrer::String, statistics::Statistics, x_forwarded::String,via::String }
     deriving (Show)

pfile = 
  do result <- many pline
     eof
     return result

pline = 
  do v <- oneOf "12"
     comma_spaces
     t <- ptime
     comma_spaces       
     pio <- pinout (Header { version=string2int [v], time=t })
     eol
     return pio

pnumber = 
  do d <- optionMaybe (string "-")
     n <- many1 digit
     return (let s = case d of Nothing -> 1 ; _ -> -1 in      
            s * string2int n)

pstatistics t = 
  do z <- pnumber
     comma_spaces
     p <- pnumber  
     comma_spaces
     l <- pnumber
     comma_spaces
     lt <- pnumber
     comma_spaces
     xt <- pnumber
     comma_spaces
     tt <- pnumber              
     return (Statistics { status=t, size=z, proxy=(p==1), level=l, lookup_time=lt, xfer_time=xt, total_time=tt })

pinout h = 
  try (pout h) <|> try (pin h)

pin h = 
    do quotes (string "IN")
       comma_spaces
       s <- pip
       comma_spaces
       d <- pip 
       comma_spaces
       u <- purl
       comma_spaces
       t1 <- pstatus
       comma_spaces
       t2 <- pstatus
       comma_spaces
       c <- pstatistics (string2int t1)
       return (In {header=h, source=s, destination=d, request=u, statistics=c, referrer_status=(string2int t2) })

pout h = 
  do quotes (string "OUT")
     comma_spaces
     r <- premote
     comma_spaces
     s <- pip
     comma_spaces
     d <- pip 
     comma_spaces
     u1 <- purl
     comma_spaces
     u2 <- purl
     comma_spaces
     t <- pstatus
     comma_spaces
     c <- pstatistics (string2int t)
     comma_spaces
     xf <- pfield 
     comma_spaces
     v <- pfield
     return (Out {header=h, remote=(r=="REM"), source=s, destination=d, 
                  request=u1, referrer=u2, statistics=c,
                  x_forwarded=xf, via=v })

pfield = 
  quotes (many (noneOf "\""))

pstatus = 
  let l = ["0", "100", "101", "102", 
       "200", "201", "202", "203", "204", "205", "206", "207",
       "300", "301", "302", "303", "304", "305", "306", "307", 
       "400", "401", "402", "403", "404", "405", "406", "407", "408", "409", "410", 
       "411", "412", "413", "414", "415", "416", "417", "418", "422", "423", "424", 
       "425", "426", "449", "450", 
       "500", "501", "502", "503", "504", "505", "506", "507", "508", "509", "510" ] in 
  foldl 
    (\p x -> try (string x) <|> p)
    (fail "invalid HTTP status code")
    l 
            
premote = 
  let r = string "REM" in
  let l = string "LOC" in 
  quotes (r <|> l <|> many (noneOf "\""))

ptime = 
  do t1 <- many digit
     d <- dot
     t2 <- many digit
     return (string2time (t1 ++ [d] ++ t2))

pip = 
  do char '\"'
     [b1,b2,b3,b4] <- (sepBy byte dot)
     colon
     p <- many digit
     char '\"'
     return (Addr(IP(b1,b2,b3,b4),string2int p))

purl = 
  do quotes (many (noneOf "\""))

quotes p = 
  do char '\"'
     x <- p
     char '\"'
     return x 

byte::GenParser Char st Word8
byte = 
  do d <- many digit
     let n :: Integer
         n = read d
     if n < 256
        then return (fromIntegral n)
        else fail $ d ++ " too large for a byte."

dot = 
  char '.'

colon = 
  char ':'

comma_spaces = 
  do char ',' 
     spaces
     return ()

eol = 
  char '\n'

go input = 
  parse pfile "(unknown)" input

string2int v = read v 
string2time v = 
  let u = posixSecondsToUTCTime (realToFrac ((read v) :: Float))in 
  utcToLocalTime (hoursToTimeZone 5) u
