structure Times =
struct
    open Time
    type EndingTimes = { start        : time
                       , tokenEnd    : time
                       , measure1End : time
                       , reduce1End  : time
                       , reduce2End  : time
                       , reduce3End  : time
                       , measure2End : time
                       }

    fun updateStart ( t : time ) ( et : EndingTimes ) : EndingTimes =
        { start = t, tokenEnd = #tokenEnd et, measure1End = #measure1End et
        , reduce1End = #reduce1End et, reduce2End = #reduce2End et
        , reduce3End = #reduce3End et, measure2End = #measure2End et
        }
    fun updateTokenEnd ( t : time ) ( et : EndingTimes ) : EndingTimes =
        { start = #start et, tokenEnd = t, measure1End = #measure1End et
        , reduce1End = #reduce1End et, reduce2End = #reduce2End et
        , reduce3End = #reduce3End et, measure2End = #measure2End et
        }
    fun updateMeasure1End ( t : time ) ( et : EndingTimes ) : EndingTimes =
        { start = #start et, tokenEnd = #tokenEnd et, measure1End = t
        , reduce1End = #reduce1End et, reduce2End = #reduce2End et
        , reduce3End = #reduce3End et, measure2End = #measure2End et
        }
    fun updateReduce1End ( t : time ) ( et : EndingTimes ) : EndingTimes =
        { start = #start et, tokenEnd = #tokenEnd et, measure1End = #measure1End et
        , reduce1End = t, reduce2End = #reduce2End et
        , reduce3End = #reduce3End et, measure2End = #measure2End et
        }
    fun updateReduce2End ( t : time ) ( et : EndingTimes ) : EndingTimes =
        { start = #start et, tokenEnd = #tokenEnd et, measure1End = #measure1End et
        , reduce1End = #reduce1End et, reduce2End = t
        , reduce3End = #reduce3End et, measure2End = #measure2End et
        }
    fun updateReduce3End ( t : time ) ( et : EndingTimes ) : EndingTimes =
        { start = #start et, tokenEnd = #tokenEnd et, measure1End = #measure1End et
        , reduce1End = #reduce1End et, reduce2End = #reduce2End et
        , reduce3End = t, measure2End = #measure2End et
        }
    fun updateMeasure2End ( t : time ) ( et : EndingTimes ) : EndingTimes =
        { start = #start et, tokenEnd = #tokenEnd et, measure1End = #measure1End et
        , reduce1End = #reduce1End et, reduce2End = #reduce2End et
        , reduce3End = #reduce3End et, measure2End = t
        }
    fun zeroEndingTimes () : EndingTimes =
        { start       = zeroTime
        , tokenEnd    = zeroTime
        , measure1End = zeroTime
        , reduce1End  = zeroTime
        , reduce2End  = zeroTime
        , reduce3End  = zeroTime
        , measure2End = zeroTime
        }

    type ComputeTimes = { token    : time
                        , measure1 : time
                        , reduce1  : time
                        , reduce2  : time
                        , reduce3  : time
                        , measure2 : time
                        }

    fun getComputeTimes ( et : EndingTimes ) : ComputeTimes =
        { token    = #tokenEnd et - #start et
        , measure1 = #measure1End et - #tokenEnd et
        , reduce1  = #reduce1End et  - #measure1End et
        , reduce2  = #reduce2End et  - #reduce1End et
        , reduce3  = #reduce3End et  - #reduce2End et
        , measure2 = #measure2End et - #reduce3End et
        }

    fun computeTimesToString ( t : ComputeTimes ) : string =
        let val { token = tok
                , measure1 = m1
                , reduce1 = r1
                , reduce2 = r2
                , reduce3 = r3
                , measure2 = m2
                } = t
    	    val ln1 = "====== Timing information ======\n"
            val ln2 = "Tokenization time = " ^ toString tok ^ "\n"
            val ln3 = "Measure1 time = " ^ toString m1 ^ "\n"
            val ln4 = "Reduce1 time = " ^ toString r1 ^ "\n"
            val ln5 = "Reduce2 time = " ^ toString r2 ^ "\n"
            val ln6 = "Reduce3 time = " ^ toString r3 ^ "\n"
            val ln7 = "Measure2 time = " ^ toString m2 ^ "\n"
            val ln8 = "================================\n"
        in ln1 ^ ln2 ^ ln3 ^ ln4 ^ ln5 ^ ln6 ^ ln7 ^ ln8
        end

    (* Dump the computation times to the specified file *)
    fun dumpComputeTimes ( fileName : string ) ( t : ComputeTimes ) : unit =
        let val ()   = print ( "opening: " ^ fileName ^ ", length " ^ (Int.toString (size fileName)) ^ "\n" )
            val strm = TextIO.openOut fileName
            val ()   = TextIO.output ( strm, computeTimesToString t )
        in TextIO.closeOut strm
        end

end
