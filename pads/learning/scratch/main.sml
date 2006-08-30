structure Main : sig

    val main : (string * string list) -> OS.Process.status
    val emit : unit -> unit

  end = struct
    val anyErrors = ref false
    exception Exit of OS.Process.status
    fun silenceGC () = (SMLofNJ.Internals.GC.messages false)
    structure RegExp = RegExpFn (structure P=AwkSyntax  structure E=DfaEngine) : REGEXP 

    type offset = {offset: int, span:int}
    datatype Token = Pint of int | Pstring of string | Other of char | Error
    (*    Pint < Pstring < Other < Error *)
    fun compToken (t1, t2) = 
	case (t1,t2) 
        of (Pint i1, Pint i2) =>  EQUAL
        |  (Pstring s1, Pstring s2) => EQUAL
        |  (Other c1, Other c2) => Char.compare (c1, c2)
        |  (Error, Error) => EQUAL
        |  (Pint _, _) => LESS
        |  (Pstring _, Pint _) => GREATER
        |  (Pstring _,  _) => LESS
        |  (Other _, Pint _) => GREATER
        |  (Other _, Pstring _) => GREATER
        |  (Other _, Error) => LESS
        |  (Error, _) => GREATER

   structure TokenTable = RedBlackMapFn(
                     struct type ord_key = Token
			    val compare = compToken
		     end) 

   structure IntMap = RedBlackMapFn(
                     struct type ord_key = int
			    val compare = Int.compare
		     end) 

   type recordCount = (int ref) TokenTable.map
   type histogram = (int ref) IntMap.map
   type freqDist = (histogram ref) TokenTable.map

   fun loadFile path = 
       let val strm = TextIO.openIn path
	   val data : String.string = TextIO.inputAll strm
           fun isNewline c = c = #"\n"
           val lines = String.tokens isNewline data
	   val numRecs = List.length lines
	   val () = print (Int.toString numRecs ^" records.\n")
	   val () = TextIO.closeIn strm
       in
	   lines
       end

    fun buildRegExps () = 
	let val StringR = RegExp.compileString "[:alpha:]*"
	in
	    [StringR]
	end


    (* assume offset is not at end of sstring *)
    fun getInt (sstring, len, offset) = 
	let val fst = Substring.sub(sstring,offset)
	    fun toInt d = (Char.ord d) - (Char.ord #"0")
	    fun getRest span acc = 
		if offset+span >= len then (Pint acc, {offset=offset, span=span})
		else let val nxt = Substring.sub(sstring,offset+span)
		     in
			 if Char.isDigit nxt then getRest (span + 1) (acc * (toInt nxt))
			 else (Pint acc, {offset=offset,span=span})
		     end
	in
	    if Char.isDigit fst then SOME (getRest 1 (toInt fst)) else NONE
	end

    (* assume offset is not at end of sstring *)
    fun getString (sstring, len, offset) = 
	let val fst = Substring.sub(sstring,offset)
	    fun getRest span acc = 
		if offset+span >= len then (Pstring acc, {offset=offset, span=span})
		else let val nxt = Substring.sub(sstring,offset+span)
		     in
			 if Char.isAlpha nxt then getRest (span + 1) (acc ^ (Char.toString nxt))
			 else (Pstring acc, {offset=offset,span=span})
		     end
	in
	    if Char.isAlpha fst then SOME (getRest 1 (Char.toString fst)) else NONE
	end

    fun getOther (sstring, len, offset) = 
	SOME (Other (Substring.sub(sstring,offset)), {offset=offset, span=1})
         handle _ => (print "KSF:caught exception\n"; NONE)

    fun pick [] s = (Error, {offset=0,span=0})
      | pick [f] s = Option.valOf(f s)
      | pick (f::fs) s = 
	case f s of NONE => pick fs s
        | SOME r => r

    fun tokenizeRecord record = 
	let val record = Substring.full record
	    val length = Substring.size record
	    fun getFirst offset = pick [getInt, getString, getOther] (record,length,offset)
            fun tR offset acc = 
		if offset = length then acc
		else let val first as (r, {offset=_,span=span}) = getFirst offset
		     in
			 tR (offset+span) (first::acc)
		     end
	in
	    List.rev(tR 0 [])
	end

    fun printToken t = 
	case t 
        of Pint i => print "[int]" (*(" Pint("^(Int.toString i)^")") *)
        |  Pstring s => print "[string]" (*(" Pstring("^s^")") *)
        |  Other c => print ("("^(Char.toString c)^")") (*(" Pother("^(Char.toString c)^")") *)
        |  Error => print (" Error")

    fun printTokens [] = print "\n"
      | printTokens ((t,loc)::ts) = (printToken t; printTokens ts)

    fun countFreqs tokens = 
	let fun doToken counts t = 
		case TokenTable.find (counts, t)
	        of NONE =>  TokenTable.insert(counts, t, ref 1)
                |  SOME iref => (iref := !iref + 1; counts)
	    fun doTokens counts [] = counts
	      | doTokens counts ((t,loc)::ts) = doTokens (doToken counts t) ts
            val counts = doTokens TokenTable.empty tokens
            fun printFreq (token,value) = (printToken token; print ":\t"; print (Int.toString (!value)); print "\n" )
            fun printFreqs counts = TokenTable.appi printFreq counts
(*	    val () = printFreqs counts
	    val () = print "\n" *)
	in
           counts
	end
  
    (* hs: maps tokens to histograms (which map ints to int refs) *)
    (* counts: maps tokens to int refs *)
    fun buildHistograms countslist = 
	let val () = print "Building histograms...\n"
	    fun doOneRecord (fd : freqDist) (counts:recordCount) = 
                let fun doOneToken (token,count,fd) : freqDist = 
		    case TokenTable.find(fd,token)
		    of NONE => TokenTable.insert(fd, token, ref (IntMap.insert(IntMap.empty, !count, ref 1)))
                    |  SOME iMap => (case IntMap.find(!iMap, !count)
				     of NONE => (iMap := IntMap.insert(!iMap, !count, ref 1); fd)
                                     |  SOME cref => (cref := !cref +1; fd))
		in
		    (TokenTable.foldli doOneToken fd counts : freqDist)
		end
	    fun doAllRecords (fd:freqDist) [] = fd
              | doAllRecords fd (c::cs) = doAllRecords (doOneRecord fd c) cs
            val freqs : freqDist = doAllRecords TokenTable.empty countslist
            fun printOneFreq (int, countRef) = (print "\t"; print (Int.toString int); print ":\t"; print(Int.toString(!countRef)); print "\n")
            fun printHist (token, intMap) = (print "Token: "; printToken token; print "\n"; IntMap.appi printOneFreq (!intMap); print "\n")
            fun printDist freqs = (print "Distributions:\n"; TokenTable.appi printHist freqs; print "\n")
	    val () = printDist freqs
	in
	    freqs
	end

    fun doIt fileName = 
	let val records = loadFile fileName
	    val tokens = List.map tokenizeRecord records
(*	    val () = List.app printTokens tokens  *)
            val counts = List.map countFreqs tokens
            val fd: freqDist = buildHistograms counts
	in
	    counts
	end

    fun main (cmd, args) = 
      (let val fileName = hd args
       in
         print ("Starting on file "^fileName^"\n");
         doIt fileName; 
         if !anyErrors 
	     then  OS.Process.exit(OS.Process.failure)
	 else  OS.Process.exit(OS.Process.success)
       end)  
            handle  Exit r => OS.Process.exit(OS.Process.failure)
                  | OS.SysErr(s, sopt) => (TextIO.output(TextIO.stdErr, 
					   concat[s,"\n"]); 
					   OS.Process.exit(OS.Process.failure))
                  | ex => (TextIO.output(TextIO.stdErr, concat[
		          "uncaught exception ", exnName ex,
		          " [", exnMessage ex, "]\n"
	                  ]);
			  app (fn s => TextIO.output(TextIO.stdErr, concat[
		          "  raised at ", s, "\n"
	                  ])) (SMLofNJ.exnHistory ex);
	                   OS.Process.exit(OS.Process.failure))



    (* Generates the compiler and exports an executable. *)
    fun emit () = 
	    (silenceGC();
	     SMLofNJ.exportFn ("lib/learn", main ))

  end; 

