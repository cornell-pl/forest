
--
--list of hosts
--  list of datetimes YYYY-MM-DD-HH-MM
--    (corald.log.gz)? ,
--    (coraldwebsrv.log.gz)?
--
-- corald.log.gz -- topology information
-- coraldwebsrv.log.gz -- Apache-like log
--
-- INT "," FLOAT "," ( "IN" "," IPPORT "," IPPORT "," URL ("," INT){8} 
--                  | "OUT" "," ("REM" | "LOC") "," (IP ":" PORT) "," URL "," (URL | "(null)") "," INT{7} "," IP "," AGENT)

module PADSCoral where 

import List (sort)

-- JNF: write proper Ord instance to get months right 
data Date = Date { year::Int, month::String, day::Int, hour::Int, minute::Int } 
            deriving (Eq,Ord)

-- JNF: intentionally only showing day and month
instance Show Date where
  show d = (show (day d) ++ " " ++ month d)

data Entry = Entry { payload::String, size::Int } 
             deriving (Show)

sample = [ ("denali", 
           [ (Date { year=2010, month="Aug", day=10, hour=14, minute=15 },
             [ Entry { payload="QUUX", size=2554 } ]),
             (Date { year=2010, month="Aug", day=1, hour=09, minute=37 },
             [ Entry { payload="FOO", size=1574 }, 
               Entry { payload="BAR", size=3216 } ] ) ] ),
           ("opus", 
           [ (Date { year=2010, month="Apr", day=13, hour=20, minute=15 },
             [ Entry { payload="QUUX", size=23 } ]),
             (Date { year=2010, month="Aug", day=1, hour=15, minute=45 },
             [ Entry { payload="FOO", size=1400 }, 
               Entry { payload="BAR", size=4500 } ] ) ] ) ]

by_host logs = 
  [ (host, sum sizes) | (host,datetimes) <- logs,
                        let sizes = [ size entry | (datetime,entries) <- datetimes, 
                                                   entry <- entries ] ]

by_day logs = 
  sort [ (date,sum sizes) | (_,datestimes) <- logs, 
                            (date,entries) <- datestimes, 
                            let sizes = [ size entry | entry <- entries ] ]

by_host_day logs =
  sort [ (host,date,sum sizes) | (host,datestimes) <- logs, 
                                 (date,entries) <- datestimes, 
                                 let sizes = map size entries ]
