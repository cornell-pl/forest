structure Main : sig

    val main : (string * string list) -> OS.Process.status
    val emit : unit -> unit

  end = struct
    val anyErrors = ref false
    exception Exit of OS.Process.status
    fun silenceGC () = (SMLofNJ.Internals.GC.messages false)
    structure RegExp = RegExpFn (structure P=AwkSyntax  structure E=DfaEngine) : REGEXP 
    structure MT = MatchTree
    structure SS = Substring

    type offset = {offset: int, span:int}
    datatype Token = Pint of LargeInt.int | Pstring of string | Pwhite of string | Other of char | Error

    (*Note: regular expressions must consume at least one input character! *)
    type mi = {pos:substring, len:int}
    fun getToken mt f =
	let val (root:mi option) = MT.root mt
	in 
	    case root of NONE => (Error, 0)
	  | SOME m => (f (SS.string (SS.slice(#pos m, 0, SOME(#len m)))), #len m)
	end
    val alphaString = "[:a-zA-Z:]+"
    fun alphaStringCvt mtOpt = getToken mtOpt (fn s=> Pstring s)
    val integer = "-?[:0-9:]+"
    fun integerCvt mtOpt = getToken mtOpt (fn s=> Pint (Option.valOf(LargeInt.fromString s)))
    val ws = "[: \t:]+"
    fun wsCvt mtOpt = getToken mtOpt (fn s=> Pwhite s)
(*    val other = "[:,|(){}_;!~[]:]" *)
    val other = "[:|:]" 
    fun otherCvt mtOpt = getToken mtOpt (fn s=> Other (String.sub(s,0)))
    val matchlist = [(integer, integerCvt), (alphaString, alphaStringCvt), (ws, wsCvt), (other,otherCvt)]

    (*    Pint < Pstring < Pwhite < Other < Error *)
    fun compToken (t1, t2) = 
	case (t1,t2) 
        of (Pint i1, Pint i2) =>  EQUAL
        |  (Pstring s1, Pstring s2) => EQUAL
        |  (Pwhite s1, Pwhite s2) => EQUAL
        |  (Other c1, Other c2) => Char.compare (c1, c2)
        |  (Error, Error) => EQUAL
        |  (Pint _, _) => LESS
        |  (Pstring _, Pint _) => GREATER
        |  (Pstring _,  _) => LESS
        |  (Pwhite _, Pint _) => GREATER
        |  (Pwhite _, Pstring _) => GREATER
        |  (Pwhite _, _) => LESS
        |  (Other _, Pint _) => GREATER
        |  (Other _, Pstring _) => GREATER
        |  (Other _, Pwhite _) => GREATER
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
           fun isNewline c = c = #"\n" orelse c = #"\r"
           val lines = String.tokens isNewline data
	   val numRecs = List.length lines
	   val () = print (Int.toString numRecs ^" records.\n")
	   val () = TextIO.closeIn strm
       in
	   lines
       end


    fun printToken t = 
	case t 
        of Pint i => print (*"[int]"*)(" Pint("^(LargeInt.toString i)^")") 
        |  Pstring s => print (*"[string]"*)  (" Pstring("^s^")")
        |  Pwhite s => print "[white space]" (*(" Pstring("^s^")") *)
        |  Other c => print ("("^(Char.toString c)^")") (*(" Pother("^(Char.toString c)^")") *)
        |  Error => print (" Error")

    fun printTokens [] = print "\n"
      | printTokens ((t,loc)::ts) = (printToken t; printTokens ts)

    

    fun rtokenizeRecord record = 
	let val record = Substring.full record
	    val length = Substring.size record
	    fun matchOne rest = RegExp.match matchlist SS.getc rest
            fun getMatches offset rest acc = 
		case matchOne rest of NONE => List.rev acc
		| SOME ((token,len),rest) => 
		    getMatches (offset+len) rest ((token,{offset=offset, span=len}) :: acc)
	    val matches = getMatches 0 record []
(*	    val () = printTokens matches *)
	in
	    matches
	end


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
    fun buildHistograms numRecords countslist = 
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
            fun printOneFreq (int, countRef) = 
		let val percent = (Real.fromInt (!countRef ))/(Real.fromInt numRecords)
		in
		(print "\t"; print (Int.toString int); print ":\t"; print(Int.toString(!countRef)); 
		 print "\t"; print (Real.toString percent); ( print "\n"))
		end
            fun printHist (token, intMap) = (print "Token: "; printToken token; print "\n"; IntMap.appi printOneFreq (!intMap); print "\n")
            fun printDist freqs = (print "Distributions:\n"; TokenTable.appi printHist freqs; print "\n")
	    val () = printDist freqs
	in
	    freqs
	end

    fun doIt fileName = 
	let val records = loadFile fileName
	    val numRecords = List.length records
	    val rtokens = List.map rtokenizeRecord records
(*	    val () = List.app printTokens rtokens  *)
            val counts = List.map countFreqs rtokens
            val fd: freqDist = buildHistograms numRecords counts
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

