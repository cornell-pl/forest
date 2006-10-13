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
    type TokenOrder = Token list
    type Context = LToken list
    type DerivedContexts = Context list
    type Partition = (TokenOrder * (DerivedContexts list)) list * (Context list)

    datatype Ty = Base of Token | Pvoid 
                | TBD of Context list | Bad of Context list
                | Pstruct of Ty list |  Punion of Ty list 


    fun doTranspose m =
	let fun transposeOne (dc, acc) = 
	    let fun tO' (dc,acc) result = 
		case (dc,acc) 
		    of ([], [])          => List.rev result
		  |  (nt::tks, na::accs) => tO' (tks, accs) ((nt :: na)::result)
		  | _                    => raise Fail "UnequalLengths"
	    in
		tO' (dc,acc) []
	    end
	    fun listify ls = List.map (fn x => [x]) ls 
	    val revTrans = case m
		of [] => []
	      |  [dc] => listify dc
	      |  (dc::rest) => List.foldl transposeOne (listify dc) rest 
	in
	    List.map List.rev revTrans
	end

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

   fun TokenEq (t1, t2) = compToken (t1, t2) = EQUAL

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

   (* Printing routines *)
    fun printKind Struct = print "Struct"
    fun tokenToString t = 
	case t 
        of Ptime i => i
	|  Pip i  => i
        |  Pmonth m => m
	|  Pint i => LargeInt.toString i
        |  Pstring s => s
        |  Pwhite s => s
        |  Other c => Char.toString c
        |  Error => " Error"

    fun printToken t = 
	case t 
        of Ptime i => print ("[Time]")
	|  Pip i  => print ("[IP]")
        |  Pmonth m => print ("[Month]")
	|  Pint i => print (* ("[int]")*)         (" Pint("^(LargeInt.toString i)^")")
        |  Pstring s => print (* ("[string]")*)   (" Pstring("^s^")")
        |  Pwhite s => print (* "[white space]"*) (" Pwhite("^s^")") 
        |  Other c => print ("("^(Char.toString c)^")") (*(" Pother("^(Char.toString c)^")") *)
        |  Error => print (" Error")

    fun printLTokens [] = print "\n"
      | printLTokens ((t,loc)::ts) = (printToken t; printLTokens ts)

    fun printTokens [] = print "\n"
      | printTokens (t::ts) = (printToken t; printTokens ts)

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

    fun printStructSummary (kind, (count, coverage, tinfos)) = 
	let fun printOne (t,count) =
	    (printToken t; print "\t";
	     print "Occurrences:"; print (Int.toString count);  print "\n")
	in
	    (printKind kind; print "\t";
	     print "Coverage:";    print (Int.toString coverage);  print "\n";
	     print "Token count:"; print (Int.toString count);     print "\n";
	     List.app printOne tinfos)
	end

    fun printTokenAnalysis summary = 
	let fun printOne (t,k,count, coverage) =
	    (printToken t; print "\t"; printKind k;             print "\t"; 
	     print "Occurrences:"; print (Int.toString count);  print "\t";
	     print "Coverage:"; print (Int.toString coverage);  print "\n")
	in
	    List.app printOne summary
	end

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


    fun printTList tList = (print "tokenOrder:\n";
			    List.app (fn t => (printToken t; print " ")) tList;
			    print "\n")

    fun printDerivedContexts contexts = 
	(print "Derived Contexts:\n";
	 (case contexts 
	  of [] => print "<no records matched context>\n"
	  | _ => (print "Record:\n";
	         List.app (fn tl => 
			    (case tl 
			     of [] => print "\t<empty>\n"
			     | _ => (print "\t"; printLTokens tl; print "\n"))) contexts));
	 print "\n")

    fun printContexts contexts = 
	((case contexts 
	  of [] => print "<no records matched context>\n"
	  | _ => (List.app (fn tl => 
			    (case tl 
			     of [] => print "\t<empty>\n"
			     | _ => (print "\t"; printLTokens tl; print "\n"))) contexts)))


 
   fun printOnePartition (tokenOrder, contexts) = 
       (printTList tokenOrder;
        List.app printDerivedContexts contexts)

   fun printPartitions (matches, badRecords) = 
       (print "\nPartitioned Records:\n";
        List.app printOnePartition matches;
        print "Bad records:\n";
        List.app (fn r => (print "\t"; printLTokens r)) badRecords;
        print "\n")

   fun printTyD prefix longTBDs longBad suffix ty = 
       (print prefix;
        (case ty 
         of Pvoid  => (print "Pvoid")
         |  Base t => (printToken t)
         |  TBD cl => (print "TBD";
		       if longTBDs then
			   (print "\n"; printContexts cl; print prefix; print "End TBD")
		       else ())
         |  Bad cl => (print "Bad";
	 	       if longBad then
			  (print "\n"; printContexts cl; print prefix; print "End Bad")
		       else ())
         |  Pstruct tys => (print "Pstruct\n";
	 		    List.app (printTyD (prefix^"\t") longTBDs longBad (";\n")) tys;
			    print prefix; print "End Pstruct\n")
         |  Punion tys  => (print "Punion\n";
	 		    List.app (printTyD (prefix^"\t") longTBDs longBad (";\n")) tys;
			    print prefix; print "End Punion\n"));
	print suffix)
       
    fun printTy ty = printTyD "" false false "" ty


    fun dumpLToken strm (tk,loc) = TextIO.output(strm, tokenToString tk)
    fun dumpCL fileName contexts = 
	let val strm = TextIO.openOut fileName
            fun dumpOneContext context = (List.app (dumpLToken strm) context;
					  TextIO.output(strm, "\n"))
            val () = List.app dumpOneContext contexts
	in
	    TextIO.closeOut strm
	end

    fun dumpTBDs path ty = 
	let val TBDstamp = ref ~1
	    val BADstamp = ref ~1
	    val () = (print "Outputing parititons to directory: "; print path; print "\n")
	    fun doIt ty = 
		case ty
                of Pvoid => ()
		|  Base t => ()
                |  TBD cl => dumpCL (TBDstamp := !TBDstamp + 1; path^"TBD_"^(Int.toString (!TBDstamp))) cl
                |  Bad cl => dumpCL (BADstamp := !BADstamp + 1; path^"BAD_"^(Int.toString (!BADstamp))) cl
	        |  Pstruct tys => List.app doIt tys
	        |  Punion tys => List.app doIt tys
    	in  
	   if OS.FileSys.isDir path handle SysErr => (OS.FileSys.mkDir path; true)
	   then doIt ty
	   else print "Output path should specify a directory.\n"
	end
       

   (* Histogram compuations *)
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
(*	    val () = printLTokens matches *)
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

    (* Selects the "top-level" cluster from the input cluster list.
       Return:
          kind of cluster: struct, union, array
          count of the number of tokens in the cluster
          count of the number of records in which the cluster appears
          a list of the tokens in the cluster and the number of times each token appears in the cluster
     *)
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

            fun mergeStructEntries ta =
		let fun doOne ((token,kind,count,coverage),(cumCount, cumCoverage,tlist)) = 
 		    case kind of Struct =>
			    (cumCount + count, Int.min(coverage, cumCoverage), (token, count)::tlist)
		in
		    (Struct, List.foldl doOne (0, numRecords,[]) tokenAnalysis)
		end
	    val structSummary = mergeStructEntries tokenAnalysis

	    val () = printStructSummary structSummary

	in
	    structSummary : (Kind * (int * int * ((Token * int)  list) ))
	end

    (* 
       Result of this function:
         a list of:
           a tokenOrder and a list of the derived contexts for that order
         a list of records that do not match any token order for the supplied token set.
       The derived contexts for a tokenOrder is a list of contexts, the "holes" between the tokens.
       Each context is a list of tokens.
    *)
    exception TokenMatchFailure
    fun splitRecords summary (records : Context list ) =
	let val (kind,(count, coverage, tokenfreqs)) = summary
	    fun getTokenOrder summary record = 
	        let fun insertOne ((token,freq),tTable) = TokenTable.insert(tTable, token, ref freq)
		    val tTable = List.foldl insertOne TokenTable.empty summary
		    val numFound = ref 0
		    fun doOneToken tTable ((token,loc), acc) = 
			case TokenTable.find(tTable, token)
			of NONE => acc
			|  SOME freq => 
			    if !freq = 0 then raise TokenMatchFailure
			    else (freq := !freq - 1;
				  numFound := !numFound + 1;
				  token::acc)
		    val tList = List.rev(List.foldl (doOneToken tTable) [] record)
		    val () = if not ((!numFound) = count) 
			     then (print "Did not find all desired tokens";  raise TokenMatchFailure)
			     else ()
		in
		    SOME(tList) handle TokenMatchFailure => NONE
		end
	    fun classifyOneRecordWithMatch (thisRecord:Context) (tokenOrder:TokenOrder)  = 
		let fun doMatch (tokensToMatch:TokenOrder) (recordTokens: Context)
		                (curContextAcc : Context, contextListAcc : DerivedContexts) = 
		    case (tokensToMatch, recordTokens) 
		    of ([],[])   => List.rev ((List.rev curContextAcc) :: contextListAcc)
                    |  ([], rts) => List.rev  (rts :: contextListAcc)
                    |  (tks, []) => raise TokenMatchFailure
                    |  (tokens as tk::tks, (lrtoken as (rt,loc))::rts) => 
			let val (rt, loc) = lrtoken : LToken
			in
			  if TokenEq(tk, rt)  (* found next token; push current context *)
			  then doMatch tks rts    ([], (List.rev curContextAcc) :: contextListAcc)
			  else doMatch tokens rts (lrtoken :: curContextAcc, contextListAcc)
			end
		    val thisRecordContexts = doMatch tokenOrder thisRecord ([],[])
		in
		    SOME thisRecordContexts handle TokenMatchFailure => NONE
		end
            (* Given a summary, a token order * DerivedContexts list, and a record,
                try each token order in list.  
                if matches, add to corresponding DerivedContexts list.
                if doesn't match, try to infer a different tokenOrder.
                  if matches, add to list of token orders with corresonding derivedContext
                  if doesn't match, add to bad record list *)
	    fun classifyOneRecord tokenfreqs (thisRecord, (matches, badRecords)) = 
		let (* convert to accumulator form? *)
		    fun findFirstMatch [] = (* no existing match succeeded, see if another token order matches *)
			 (case getTokenOrder tokenfreqs thisRecord
                          of NONE => raise TokenMatchFailure (* tokens don't match this record *)
                          |  SOME tokenOrder => findFirstMatch [(tokenOrder,[])])
                      | findFirstMatch ((current as (match, matchedContextLists))::rest) = 
		          (case classifyOneRecordWithMatch thisRecord match
			   of NONE => current :: (findFirstMatch rest)
                           |  SOME contexts => ((match, contexts :: matchedContextLists) :: rest) (* matches are in reverse order *))
		in
		    (findFirstMatch matches, badRecords)
		    handle tokenMatchFailure => (matches, thisRecord :: badRecords) (* bad records are in reverse *)
		end
	    val revPartition : Partition = List.foldl (classifyOneRecord tokenfreqs) ([],[]) records
            fun reversePartition (matches, badRecords) = 
		let fun revMatch (tokenOrder, matchedRecords) = (tokenOrder, List.rev matchedRecords)
		    fun revMatches [] acc = List.rev acc
                      | revMatches (m::ms) acc = revMatches ms ((revMatch m)::acc)
		in  
		    (revMatches matches [], List.rev badRecords)
		end
	    
	    val partition : Partition = reversePartition revPartition
	in
	   partition
	end

    (* This function takes a Partition and returns a Ty describing that partition *)
    fun partitionToTy partitions = 
	let val (matches, badRecords) = partitions
	    fun cnvOneMatch (tokenOrder, dclist) = 
		let val columns = doTranspose dclist
		    fun isEmpty column = not (List.exists (not o null) column)
                    fun shuffle(columns, tokens) result =
			case (columns, tokens) 
			of ([],[])      => raise Fail "token and column numbers didn't match (2)"
                        |  ([last], []) => List.rev (if isEmpty last then result else ((TBD last) :: result))
                        |  ([], tks)    => raise Fail "token and column numbers didn't match (3)"
                        |  (col::cols, tk::tks) => 
			    let val result = if isEmpty col then  (Base tk):: result 
					     else (Base tk) :: (TBD col) :: result
			    in
				shuffle(cols, tks) result
			    end
		in
		    case shuffle (columns, tokenOrder) []
		    of [] => raise Fail "Expected at least one field."
                    |  [ty] => ty
                    |  tys  => Pstruct tys
		end
	    val matchTys = List.map cnvOneMatch matches
	    val resultTy =
		case (matchTys, badRecords)
                of ([], [])   => Pvoid  (* I don't think this case can arise *)
                |  ([], brs)  => Bad brs
                |  ([ty], []) => ty
                |  (tys, brs) => Punion (tys @ [(TBD brs)])
	    val () = print "Inferred type:\n"
	    val () = printTy resultTy
	in
	    resultTy
	end

    fun doIt fileName outputDir = 
	let val records = loadFile fileName
	    val numRecords = List.length records
	    val rtokens : LToken list list = List.map ltokenizeRecord records
            val counts  : RecordCount list = List.map countFreqs rtokens
            val fd: freqDist = buildHistograms numRecords counts
            val THRESHOLD = intFraction numRecords
	    val () = print ("THRESHOLD for histogram equality: "^(Int.toString THRESHOLD)^".\n")
	    val clusters : (Token * histogram) list list = findClusters numRecords THRESHOLD fd
	    val () = printClusters numRecords clusters
	    val analysis : (Kind * (int * int * ((Token * int)  list) )) = analyzeCluster numRecords (List.hd clusters)
            val newPartitions = splitRecords analysis rtokens
            val ty = partitionToTy newPartitions
	    val () = dumpTBDs outputDir ty
	in
	    counts
	end

    fun main (cmd, args) = 
      (let val fileName = hd args
           val outputDir = case tl args of [] => "gen/"	| (d::args) => (d^"/")
       in
         print ("Starting on file "^fileName^"\n");
         doIt fileName outputDir; 
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

