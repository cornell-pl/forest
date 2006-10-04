structure Main : sig

    val main : (string * string list) -> OS.Process.status
    val emit : unit -> unit

  end = struct
    val PERCENTAGE = 0.01
    fun intFraction x = Real.ceil(PERCENTAGE * Real.fromInt(x)) 

    val anyErrors = ref false
    exception Exit of OS.Process.status
    fun silenceGC () = (SMLofNJ.Internals.GC.messages false)
    structure SS = Substring
    open Tokens

    (*    Ptime < Pmonth < Pip < Pint < Pstring < Pwhite < Other < Error *)
    fun compToken (t1, t2) = 
	case (t1,t2) 
        of (Ptime i1, Ptime i2) => EQUAL
	|  (Pmonth i1, Pmonth i2) => EQUAL
	|  (Pip i1, Pip i2) => EQUAL
        |  (Pint i1, Pint i2) =>  EQUAL
        |  (Pstring s1, Pstring s2) => EQUAL
        |  (Pwhite s1, Pwhite s2) => EQUAL
        |  (Other c1, Other c2) => Char.compare (c1, c2)
        |  (Error, Error) => EQUAL
        |  (Ptime _, _) => LESS
        |  (Pmonth _, Ptime _) => GREATER
        |  (Pmonth _, _) => LESS
        |  (Pip _, Ptime _) => GREATER
        |  (Pip _, Pmonth _) => GREATER
        |  (Pip _, _) => LESS
        |  (Pint _, Ptime _) => GREATER
        |  (Pint _, Pmonth _) => GREATER
        |  (Pint _, Pip _) => GREATER
        |  (Pint _, _) => LESS
        |  (Pstring _, Ptime _) => GREATER
        |  (Pstring _, Pmonth _) => GREATER
        |  (Pstring _, Pip _) => GREATER
        |  (Pstring _, Pint _) => GREATER
        |  (Pstring _,  _) => LESS
        |  (Pwhite _, Ptime _) => GREATER
        |  (Pwhite _, Pmonth _) => GREATER
        |  (Pwhite _, Pip _) => GREATER
        |  (Pwhite _, Pint _) => GREATER
        |  (Pwhite _, Pstring _) => GREATER
        |  (Pwhite _, _) => LESS
        |  (Other _, Ptime _) => GREATER
        |  (Other _, Pmonth _) => GREATER
        |  (Other _, Pip _) => GREATER
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

   type RecordCount = (int ref) TokenTable.map
   type histogram = {hist : (int ref) IntMap.map ref, total: int ref, weight : int ref, 
		     score : int ref, width : int ref}
   type freqDist = histogram TokenTable.map
   type cluster  = freqDist list

   datatype Kind = Struct

    fun printKind Struct = print "Struct"
    fun printToken t = 
	case t 
        of Ptime i => print ("[Time]")
	|  Pip i  => print ("[IP]")
        |  Pmonth m => print ("[Month]")
	|  Pint i => print ("[int]") (*" Pint("^(LargeInt.toString i)^")"*) 
        |  Pstring s => print ("[string]")  (*" Pstring("^s^")"*)
        |  Pwhite s => print "[white space]" (*(" Pstring("^s^")") *)
        |  Other c => print ("("^(Char.toString c)^")") (*(" Pother("^(Char.toString c)^")") *)
        |  Error => print (" Error")

    fun printTokens [] = print "\n"
      | printTokens ((t,loc)::ts) = (printToken t; printTokens ts)

    fun printFreq (token,value) = (printToken token; print ":\t"; print (Int.toString (!value)); print "\n" )
    fun printFreqs counts = (TokenTable.appi printFreq counts; print "\n")

    fun printOneFreq numRecords (int, countRef) = 
	let val percent = (Real.fromInt (!countRef ))/(Real.fromInt numRecords)
	in
	    (print "\t"; print (Int.toString int); print ":\t"; print(Int.toString(!countRef)); 
	     print "\t"; print (Real.toString percent); ( print "\n"))
	end
    fun printAugHist numRecords (token, (h:histogram)) = 
	(print "Token: "; 
	 printToken token; print "\n"; 
         print ("Total number of token occurrences: "^(Int.toString (!(#total h))^".\n"));
         print ("Number of records with at least one token occurrence: "^(Int.toString (!(#weight h))^".\n"));
         print ("Score: "^(Int.toString (!(#score h))^".\n"));
	 IntMap.appi (printOneFreq numRecords) (!(#hist h)); print "\n")
    
    fun printDist numRecords freqs = (print "Distributions:\n"; TokenTable.appi (printAugHist numRecords) freqs; print "\n")


    fun printClusters numRecords clusters = 
       let fun printOne n c = 
	       (print ("Cluster "^(Int.toString n)^":\n");
		List.app (printAugHist numRecords) c;
	        print "\n")
	   fun pC n [] = ()
             | pC n (c::cs) = (printOne n c; pC (n+1) cs)
       in
	   pC 0 clusters
       end



   fun mkHistogram (column:int) : histogram =
       {hist=ref (IntMap.insert(IntMap.empty, column, ref 1)), total=ref column, weight = ref 1, score = ref 0, width = ref 0}

   fun getSortedHistogramList c = 
       let val cList = List.map (!) (IntMap.listItems c)
       in
	   ListMergeSort.sort (Int.<) cList
       end

   fun fuzzyIntEq threshold (i1, i2) = abs(i1 - i2) < threshold

   (* formula: sum over all columns of columnNumber * (numRemainingRecords - columnHeight) *)
   fun histScore numRecords (h:histogram) = 
       let val cSorted = getSortedHistogramList (!(#hist h))
	   fun foldNext (columnHeight, (score, columnNumber, numRemainingRecords)) =
	       (columnNumber * (numRemainingRecords - columnHeight) + score, columnNumber +1, numRemainingRecords - columnHeight)
       in
	   (#1 (List.foldl foldNext (0,1,numRecords) cSorted ), List.length cSorted)
       end

    fun histScoreCmp threshold ({score=score1,...}:histogram,{score=score2,...}:histogram) = 
	(!score1) < (!score2)

    fun histScoreEq threshold ({score=score1,...}:histogram,{score=score2,...}:histogram) = 
	(fuzzyIntEq threshold (!score1,!score2)) 

   (* OBSOLETE function *)
   (* two histograms are equal if they have the same number of columns,
    * and when sorted by height, corresponding columns are of equal height *)
   fun histogramEqual threshold (h1:histogram, h2:histogram) = 
       let val c1 = !(#hist h1)
	   val c2 = !(#hist h2)
       in
       (fuzzyIntEq threshold (IntMap.numItems c1, IntMap.numItems c2))
       andalso
            let val c1Sorted = getSortedHistogramList c1
		val c2Sorted = getSortedHistogramList c2
	    in
		ListPair.all (fuzzyIntEq threshold) (c1Sorted, c2Sorted)
	    end
       end

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

    fun ltokenizeRecord (record:string) = 
	let val length = String.size record
	    val cursor : int ref = ref 0
            fun feedLex n = 
		let val s = if (n > (length - !cursor)) 
			    then SS.string(SS.extract(record, !cursor, NONE))  handle Subscript => ""
			    else SS.string(SS.substring(record, !cursor, n))  handle Subscript => ""
		in
		    cursor := !cursor + n;
		    s
		end
	    val lex = TokenLex.makeLexer feedLex
            fun getMatches acc =
		case lex() 
		of SOME a => getMatches(a::acc)
                |  NONE   => List.rev acc
	    val matches = getMatches []
(*	    val () = printTokens matches *)
	in
	    matches
	end

    (* This function takes a list of tokens and returns an (int ref) TokenTable.map *)
    (* This map, keyed by the token, stores the count of occurences of the token key in the input list *)
    fun countFreqs (tokens:LToken list) = 
	let fun doToken counts t = 
		case TokenTable.find (counts, t)
	        of NONE =>  TokenTable.insert(counts, t, ref 1)
                |  SOME iref => (iref := !iref + 1; counts)
	    fun doTokens counts [] = counts
	      | doTokens counts ((t,loc)::ts) = doTokens (doToken counts t) ts
            val counts = doTokens TokenTable.empty tokens
(*	    val () = printFreqs counts *)
	in
           counts
	end
  
    (* hs: maps tokens to histograms (which map ints to int refs) *)
    (* counts: maps tokens to int refs *)
    fun buildHistograms numRecords (countslist : RecordCount list)= 
	let val () = print "Building histograms...\n"
	    fun doOneRecord (fd : freqDist) (counts:RecordCount) = 
                let fun doOneToken (token,count,fd) : freqDist = 
		    case TokenTable.find(fd,token)
		    of NONE => TokenTable.insert(fd, token, mkHistogram (!count))
                    |  SOME ({hist, total, weight, ...}: histogram)  => 
		       ((case IntMap.find(!hist, !count)
			 of NONE      => hist := IntMap.insert(!hist, !count, ref 1) 
		         |  SOME cref => cref := !cref + 1);
			 total := !total + !count;
			 weight := !weight + 1;
			 fd)
		in
		    (TokenTable.foldli doOneToken fd counts : freqDist)
		end
	    fun doAllRecords (fd:freqDist) [] = fd
              | doAllRecords fd (c::cs) = doAllRecords (doOneRecord fd c) cs
            val freqs : freqDist = doAllRecords TokenTable.empty countslist
	    fun scoreHistogram (h as {hist, total, weight, score, width} : histogram) = 
		let val (s,w) = histScore numRecords h
		in
		    score := s;
		    width := w
		end
	    val () = TokenTable.app scoreHistogram freqs
	in
	    freqs
	end

    
    fun findClusters numRecords threshold (freqDist:freqDist) = 
	let val distList = TokenTable.listItemsi(freqDist)
	    fun cmpDists ((k1,h1), (k2,h2)) = histScoreCmp threshold (h1, h2)
            fun eqDists  ((k1,h1), (k2,h2)) = histScoreEq threshold (h1, h2)
	    val sortedDistList = ListMergeSort.sort cmpDists distList

	    fun buildClusters ([], acc) = acc
	      | buildClusters ((fd::fds), ((repfd::others)::acc)) = 
		let val equivfd = (repfd :: others)
		in if eqDists(fd, repfd) 
		   then buildClusters (fds, (fd :: equivfd) :: acc) 
	           else buildClusters (fds, [fd] :: (equivfd::acc))
		end
	    val clusters = case sortedDistList of [] => []
	                   | (f::fs) => buildClusters (fs, [[f]])
	in
	    clusters
	end

    fun analyzeCluster numRecords (cluster : (Token * histogram) list) = 
	let fun doOneToken((t, {hist, total, weight, score, width}), result) =
	        let val hList = List.map (fn(x,y)=>(x, !y)) (IntMap.listItemsi (!hist))
		    val sortedHlist = ListMergeSort.sort (fn((i1,c1:int),(i2,c2))=>(c1 < c2)) hList
		    val primary = List.hd sortedHlist
		    fun isStruct sortedHlist =
			let val rest = List.tl sortedHlist
			    val massOfRest = List.foldl (fn((i,c:int),acc)=> acc + c) 0 rest
			in
			    massOfRest < (intFraction numRecords)
			end
		in
		    if isStruct sortedHlist then [(t, Struct, #1 primary, #2 primary)]@result  else result
		end
	    val tokenAnalysis = List.foldl doOneToken [] cluster
	    fun printTokenAnalysis summary = 
		let fun printOne (t,k,count, coverage) =
		     (printToken t; print "\t"; printKind k;             print "\t"; 
		      print "Occurrences:"; print (Int.toString count);  print "\t";
		      print "Coverage:"; print (Int.toString coverage);  print "\n")
		in
		    List.app printOne summary
		end
            fun mergeStructEntries ta =
		let fun doOne ((token,kind,count,coverage),(cumCoverage,tlist)) = 
 		    case kind of Struct =>
			    (Int.min(coverage, cumCoverage), (token, count)::tlist)
		in
		    (Struct, List.foldl doOne (numRecords,[]) tokenAnalysis)
		end
	    val structSummary = mergeStructEntries tokenAnalysis

	    fun printStructSummary (kind, (coverage, tinfos)) = 
		let fun printOne (t,count) =
		     (printToken t; print "\t";
		      print "Occurrences:"; print (Int.toString count);  print "\n")
		in
		    (printKind kind; print "\t";
		     print "Coverage:"; print (Int.toString coverage);  print "\n";
		     List.app printOne tinfos)
		end
	    val () = printStructSummary structSummary

	in
	    structSummary
	end


    fun doIt fileName = 
	let val records = loadFile fileName
	    val numRecords = List.length records
	    val rtokens : LToken list list = List.map ltokenizeRecord records
            val counts  : RecordCount list = List.map countFreqs rtokens
            val fd: freqDist = buildHistograms numRecords counts
            val THRESHOLD = intFraction numRecords
	    val () = print ("THRESHOLD for histogram equality: "^(Int.toString THRESHOLD)^".\n")
	    val clusters : (Token * histogram) list list = findClusters numRecords THRESHOLD fd
	    val () = printClusters numRecords clusters
	    val analysis = analyzeCluster numRecords (List.hd clusters)
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

