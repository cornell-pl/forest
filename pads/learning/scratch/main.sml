structure Main : sig

    val main : (string * string list) -> OS.Process.status
    val emit : unit -> unit

  end = struct
    (********************************************************************************)
    (*********************  Configuration *******************************************)
    (********************************************************************************)
    val HIST_PERCENTAGE   = ref 0.01
    val STRUCT_PERCENTAGE = ref 0.01
    val JUNK_PERCENTAGE   = ref 0.1

    fun histEqTolerance   x = Real.ceil((!HIST_PERCENTAGE)   * Real.fromInt(x)) 
    fun isStructTolerance x = Real.ceil((!STRUCT_PERCENTAGE) * Real.fromInt(x)) 
    fun isJunkTolerance   x = Real.ceil((!JUNK_PERCENTAGE)   * Real.fromInt(x)) 

    val depthLimit = ref 2
    val outputDir = ref "gen/"
    val srcFile = ref "toBeSupplied"


    (********************************************************************************)
    (*********************  Configuration *******************************************)
    (********************************************************************************)
	    
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
                | TBD of int * Context list | Bottom of int * Context list 
                | Pstruct of Ty list |  Punion of Ty list 

    val TBDstamp = ref 0
    val Bottomstamp = ref 0



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

    fun lconcat ls = 
	let fun doit l a = 
	    case l of [] => a
            | (s::ss) => doit ss (s^a)
	in
	    doit (List.rev ls) ""
	end

    (*    Ptime < Pmonth < Pip < Pint < Pstring < Pwhite < Other < Pempty < Error *)
    fun compToken (t1, t2) = 
	case (t1,t2) 
        of (Ptime i1, Ptime i2) => EQUAL
	|  (Pmonth i1, Pmonth i2) => EQUAL
	|  (Pip i1, Pip i2) => EQUAL
        |  (Pint i1, Pint i2) =>  EQUAL
        |  (Pstring s1, Pstring s2) => EQUAL
        |  (Pwhite s1, Pwhite s2) => EQUAL
        |  (Other c1, Other c2) => Char.compare (c1, c2)
        |  (Pempty, Pempty) => EQUAL
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
        |  (Other _, _) => LESS

        |  (Pempty, Ptime _) => GREATER
        |  (Pempty, Pmonth _) => GREATER
        |  (Pempty, Pip _) => GREATER
        |  (Pempty, Pint _) => GREATER
        |  (Pempty, Pstring _) => GREATER
        |  (Pempty, Pwhite _) => GREATER
        |  (Pempty, Other _) => GREATER
        |  (Pempty, _) => LESS

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

   datatype Kind = Struct of {numTokensInCluster:int, numRecordsWithCluster:int,
			      tokens: (Token * int) list}
                 | Blob | Empty

   (* Printing routines *)
    fun tokenToString t = 
	case t 
        of Ptime i => i
	|  Pip i  => i
        |  Pmonth m => m
	|  Pint i => LargeInt.toString i
        |  Pstring s => s
        |  Pwhite s => s
        |  Other c => Char.toString c
        |  Pempty => ""
        |  Error => " Error"

    fun tokenTyToString t = 
	case t 
        of Ptime i   => "[Time]"
	|  Pip i     => "[IP]"
        |  Pmonth m  => "[Month]"
	|  Pint i    => "[int]"                   (*" Pint("^(LargeInt.toString i)^")"*)
        |  Pstring s => "[string]"                (*" Pstring("^s^")"*)
        |  Pwhite s  => "[white space]"           (*" Pwhite("^s^")"*) 
        |  Other c   => "("^(Char.toString c)^")" (*(" Pother("^(Char.toString c)^")") *)
        |  Pempty    => "[empty]"
        |  Error     => " Error"


    fun printTokenTy t = print (tokenTyToString t)

    fun LTokensToString [] = "\n"
      | LTokensToString ((t,loc)::ts) = ((tokenToString t) ^ (LTokensToString ts))

    fun printLTokens [] = print "\n"
      | printLTokens ((t,loc)::ts) = (printTokenTy t; printLTokens ts)

    fun printTokenTys [] = print "\n"
      | printTokenTys (t::ts) = (printTokenTy t; printTokenTys ts)

    fun printFreq (token,value) = (printTokenTy token; print ":\t"; print (Int.toString (!value)); print "\n" )
    fun printFreqs counts = (TokenTable.appi printFreq counts; print "\n")

    fun printOneFreq numRecords (int, countRef) = 
	let val percent = (Real.fromInt (!countRef ))/(Real.fromInt numRecords)
	in
	    (print "\t"; print (Int.toString int); print ":\t"; print(Int.toString(!countRef)); 
	     print "\t"; print (Real.toString percent); ( print "\n"))
	end
    fun printAugHist numRecords (token, (h:histogram)) = 
	(print "Token: "; 
	 printTokenTy token; print "\n"; 
         print ("Total number of token occurrences: "^(Int.toString (!(#total h))^".\n"));
         print ("Number of records with at least one token occurrence: "^(Int.toString (!(#weight h))^".\n"));
         print ("Score: "^(Int.toString (!(#score h))^".\n"));
	 IntMap.appi (printOneFreq numRecords) (!(#hist h)); print "\n")
    
    fun printDist numRecords freqs = (print "Distributions:\n"; TokenTable.appi (printAugHist numRecords) freqs; print "\n")

    fun printKindSummary kind = 
	case kind 
        of Struct {numTokensInCluster=count, numRecordsWithCluster=coverage, tokens=tinfos} =>
	    let fun printOne (t,count) =
		(printTokenTy t; print "\t";
		 print "Occurrences:"; print (Int.toString count);  print "\n")
	    in
		(print "Struct"; print "\t";
		 print "Coverage:";    print (Int.toString coverage);  print "\n";
		 print "Token count:"; print (Int.toString count);     print "\n";
		 List.app printOne tinfos)
	    end
        | Blob => (print  "Blob"; print "\n")
        | Empty => (print  "Empty"; print "\n")

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
			    List.app (fn t => (printTokenTy t; print " ")) tList;
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

    fun contextsToString contexts = 
	((case contexts 
	  of [] => "<no records matched context>\n"
	  | _ => (lconcat(
		  List.map (fn tl => 
			    (case tl 
			     of [] => "\t<empty>\n"
			     | _ => ("\t"^( LTokensToString tl) ^"\n"))) contexts))))


    (* Replace when debugged with print (contextsToString contexts) *)
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

   fun TyToStringD prefix longTBDs longBottom suffix ty = 
       (prefix^
        (case ty 
         of Pvoid  => "Pvoid"
         |  Base t => tokenTyToString t
         |  TBD (i,cl) => "TBD_"^(Int.toString i)^
		       (if longTBDs then
			   ("\n"^(contextsToString cl)^prefix^"End TBD")
		        else "")
         |  Bottom (i, cl) => "BTM_"^(Int.toString i)^
	 	         (if longBottom then
			     ("\n"^(contextsToString cl)^prefix^"End Bottom")
		          else "")
         |  Pstruct tys =>  "Pstruct\n"^
	 		    (lconcat (List.map (TyToStringD (prefix^"\t") longTBDs longBottom (";\n")) tys))^
			    prefix ^ "End Pstruct"
         |  Punion tys  => "Punion\n"^
	 		    (lconcat (List.map (TyToStringD (prefix^"\t") longTBDs longBottom (";\n")) tys))^
			    prefix ^ "End Punion")^
	suffix)
       
    fun TyToString ty = TyToStringD "" false false "" ty

    fun printTyD prefix longTBDs longBottom suffix ty =  print (TyToStringD prefix longTBDs longBottom suffix ty )
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

    fun dumpTy fileName ty = 
	let val strm = TextIO.openOut fileName
            val () = TextIO.output(strm, TyToString ty)
	in
	    TextIO.closeOut strm
	end

    fun dumpTyInfo path ty = 
	let fun dumpTBDs ty = 
		case ty
                of Pvoid => ()
		|  Base t => ()
                |  TBD(i, cl)    => dumpCL (path^"TBD_"^(Int.toString i)) cl
                |  Bottom(i, cl) => dumpCL (path^"BTM_"^(Int.toString i)) cl
	        |  Pstruct tys => List.app dumpTBDs tys
	        |  Punion tys => List.app dumpTBDs tys
    	in  
          (print "\nOutputing parititons to directory: "; print path; print "\n";
	   if OS.FileSys.isDir path handle SysErr => (OS.FileSys.mkDir path; true)
	   then (dumpTBDs ty; dumpTy (path^"Ty") ty)
	   else print "Output path should specify a directory.\n"
          )
	end

   (* Type simplification *)
   fun simplifyTy ty = 
       let fun collapseStruct [] a = a
	     | collapseStruct ((Pstruct tys)::tysRest) a = collapseStruct tysRest (a @ (collapseStruct tys []))
	     | collapseStruct (ty::tysRest) a = collapseStruct tysRest (a @ [simplifyTy ty])
	   fun collapseUnion [] a = a
	     | collapseUnion ((Punion tys)::tysRest) a = collapseUnion tysRest (a @ (collapseUnion tys []))
	     | collapseUnion (ty::tysRest) a = collapseUnion tysRest (a @ [simplifyTy ty])
       in
	   case ty 
	   of Pstruct tys => Pstruct (collapseStruct tys [])
           |  Punion  tys => Punion  (collapseUnion  tys [])
	   |  ty          => ty
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

   (* The call to String.tokens does not produce the correct results when
    * applied to a file with blank lines.  Instead of returning an empty
    * string in the resulting list, it omits the line from the output list *)
   fun loadFile path = 
       let val strm = TextIO.openIn path
	   val data : String.string = TextIO.inputAll strm
           fun isNewline c = c = #"\n" orelse c = #"\r"
           val lines = String.fields isNewline data
	   val numRecs = List.length lines
	   val lines = List.take(lines, numRecs-1)
	   val () = print (Int.toString (numRecs - 1)^" records.\n")
	   val () = TextIO.closeIn strm
       in
	   lines
       end

    fun ltokenizeRecord (record:string) = 
	let val length = String.size record
	    fun doNonEmpty record = 
		let val cursor : int ref = ref 0
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
	in
	    if length = 0 then [(Pempty,{offset=0, span=0})] else doNonEmpty record
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

    
    fun findClusters numRecords (freqDist:freqDist) = 
	let val distList = TokenTable.listItemsi(freqDist)
	    val threshold = histEqTolerance numRecords
	    val () = print ("THRESHOLD for histogram equality: "^(Int.toString threshold)^".\n")
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
	    val () = print "Computed clusters\n"
	in
	    clusters
	end

    (* Selects the "top-level" cluster from the input cluster list.
       Return:
        Struct of
          kind of cluster: struct, union, array
          count of the number of tokens in the cluster
          count of the number of records in which the cluster appears
          a list of the tokens in the cluster and the number of times each token appears in the cluster
        | Blob, indicating no match
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
			    massOfRest < (isStructTolerance numRecords)
			end
		in
		    if isStruct sortedHlist then [(t, #1 primary, #2 primary)]@result  else result
		end
	    val tokenAnalysis = List.foldl doOneToken [] cluster

            fun mergeStructEntries ta =
		let fun doOne ((token,count,coverage),(cumCount, cumCoverage,tlist)) = 
			    (cumCount + count, Int.min(coverage, cumCoverage), (token, count)::tlist)
		    val (numTokens, coverage, tokens) = List.foldl doOne (0, numRecords,[]) tokenAnalysis
		in
		    (print "Junk Tolerance Threshold: "; print (Int.toString(isJunkTolerance numRecords)); print "\n";
		     print "Coverage: ";                 print (Int.toString(coverage)); print "\n";
		     print "Num Tokens: ";               print (Int.toString(numTokens)); print "\n";
		    if (coverage >= (isJunkTolerance numRecords) andalso (numTokens > 0))
		    then Struct {numTokensInCluster = numTokens, numRecordsWithCluster = coverage, tokens = tokens}
		    else (print "Identifed a blob\n"; Blob)
)
		end
	    val kindSummary = mergeStructEntries tokenAnalysis

	    val () = printKindSummary kindSummary

	in
	    kindSummary : Kind
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
	let val {numTokensInCluster=count, numRecordsWithCluster=coverage, tokens=tokenfreqs} = summary
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
			     then raise TokenMatchFailure
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

    fun mkBottom which cl = 
	Bottom (!Bottomstamp, cl) before Bottomstamp := !Bottomstamp + 1

    (* Invariant: cl is not an empty column: checked before mkTBD is called with isEmpty function *)
    fun mkTBD (currentDepth, cl) = 
        (* Columns that have some empty rows must have the empty list representation
           of the empty row converted to the [Pempty] token.  Otherwise, a column
           that is either empty or some value gets silently converted to the value only. *)
	let fun cnvEmptyRowsToPempty [] = [(Pempty,{offset=0, span=0})]
              | cnvEmptyRowsToPempty l  = l
	    val cl = List.map cnvEmptyRowsToPempty cl
	in
	    if (currentDepth < !depthLimit)
	    then
	      ContextListToTy (currentDepth + 1) cl
	    else 
		TBD (!TBDstamp, cl) before TBDstamp := !TBDstamp    + 1
	end


    and clustersToTy curDepth rtokens numRecords clusters = 
	let val analysis = case clusters of [] => Empty 
                           | (c::cs) => analyzeCluster numRecords c
           (* This function takes a Partition and returns a Ty describing that partition *)
	    fun partitionToTy partitions = 
		let val (matches, badRecords) = partitions
		    fun isEmpty column = not (List.exists (not o null) column)
		    fun cnvOneMatch (tokenOrder, dclist) = 
			let val columns = doTranspose dclist
			    (* Invariant: List.length columns = List.length tokens + 1 *)
			    fun shuffle(columns, tokens) result =
				case (columns, tokens) 
				  of ([],[])      => raise Fail "token and column numbers didn't match (2)"
				  |  ([last], []) => List.rev (if isEmpty last then result else ((mkTBD  (curDepth,last)) :: result))
				  |  ([], tks)    => raise Fail "token and column numbers didn't match (3)"
				  |  (col::cols, tk::tks) => 
					let val result = if isEmpty col then  (Base tk):: result 
							 else (Base tk) :: (mkTBD (curDepth, col)) :: result
					in
					    shuffle(cols, tks) result
					end
			in
			    case shuffle (columns, tokenOrder) []
			    of   []   => raise Fail "Expected at least one field."
			      |  [ty] => ty
			      |  tys  => Pstruct tys
			end
		    val matchTys = List.map cnvOneMatch matches
		    val resultTy =
			case (matchTys, badRecords)
  		        of   ([], [])   => Pvoid                   (* I don't think this case can arise *)
			  |  ([], brs)  => mkBottom "resultTy" brs (* I'm not sure this case arises either *)
			  |  ([ty], []) => ty
			  |  (tys, brs) => if isEmpty brs then Punion tys else Punion (tys @ [(mkTBD (curDepth, brs))])
(*		    val () = print "\nInferred type:\n"
		    val () = printTy (simplifyTy resultTy) 
*)
		in
		    resultTy
		end
            val ty = case analysis 
		     of Blob => mkBottom "blob" rtokens
		     |  Empty => Base Pempty
		     |  Struct s => partitionToTy (splitRecords s rtokens)
	in
	    ty
	end

    and ContextListToTy curDepth contexts = 
	let val numContexts = List.length contexts
            val counts : RecordCount list = List.map countFreqs contexts
	    val fd: freqDist = buildHistograms numContexts counts
	    val clusters : (Token * histogram) list list = findClusters numContexts fd
	    val () = printClusters numContexts clusters
            val ty = clustersToTy curDepth contexts numContexts clusters
	in
	    ty
	end

    fun doIt () = 
	let val fileName = !srcFile
	    val () = print ("Starting on file "^fileName^"\n");
	    val records = loadFile fileName
	    val rtokens : Context list = List.map ltokenizeRecord records
	    val ty = ContextListToTy 0 rtokens
	    val sty = simplifyTy ty
	in
	    dumpTyInfo (!outputDir) sty
	end

    (********************************************************************************)
    structure PCL = ParseCmdLine

    fun setOutputDir  s = outputDir  := (s^"/")
    fun setDepth      d = depthLimit := d
    fun setHistPer    h = HIST_PERCENTAGE := h
    fun setStructPer  s = STRUCT_PERCENTAGE := s
    fun setJunkPer    j = JUNK_PERCENTAGE := j
    fun addSourceFile f = srcFile    := f

    val flags = [
         ("d",        "output directory (default gen/)",              PCL.String (setOutputDir, false)),
         ("maxdepth", "maximum depth for exploration (default 5)",    PCL.Int    (setDepth,     false)),
         ("h",        "histogram comparison tolerance (percentage, default 0.01)",  PCL.Float  (setHistPer,   false)),
         ("s",        "struct determination tolerance (percentage, default 0.01)",  PCL.Float  (setStructPer, false)),
         ("j",        "junk threshold (percentage, default 0.1)",     PCL.Float  (setJunkPer,   false))
        ]

    fun processSwitches args = 
	let val banner = PCL.genBanner("learn", "Prototype Learning System", flags)
	in
	   (PCL.parseArgs(args, flags, addSourceFile, banner);
	    print ("Source file to process: "^(!srcFile)   ^"\n");
	    print ("Output directory: "      ^(!outputDir) ^"\n");
	    print ("Max depth to explore: "  ^(Int.toString (!depthLimit))^"\n");
	    print ("Histogram comparison tolerance (percentage): "  ^(Real.toString (!HIST_PERCENTAGE))^"\n");
	    print ("Struct determination tolerance (percentage): "  ^(Real.toString (!STRUCT_PERCENTAGE))^"\n");
	    print ("Junk threshold (percentage): "                  ^(Real.toString (!STRUCT_PERCENTAGE))^"\n"))
	end
    (********************************************************************************)

    fun main (cmd, args) = 
	 (processSwitches args;
          doIt (); 
          if !anyErrors then  OS.Process.exit(OS.Process.failure)
	  else OS.Process.exit(OS.Process.success))
            handle  Exit r      => OS.Process.exit(OS.Process.failure)
                  | PCL.Invalid => OS.Process.exit(OS.Process.failure)
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

