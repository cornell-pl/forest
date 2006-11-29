structure Main : sig

    val main : (string * string list) -> OS.Process.status
    val emit : unit -> unit

  end = struct
    (********************************************************************************)
    (*********************  Configuration *******************************************)
    (********************************************************************************)
    val DEF_HIST_PERCENTAGE   = 0.01
    val DEF_STRUCT_PERCENTAGE = 0.1
    val DEF_JUNK_PERCENTAGE   =  0.1
    val DEF_NOISE_PERCENTAGE  =  0.0
    val DEF_ARRAY_WIDTH_THRESHOLD =  2

    val def_depthLimit =  5
    val def_outputDir  =  "gen/"
    val def_srcFile    = "toBeSupplied"

    val depthLimit = ref def_depthLimit
    val outputDir = ref def_outputDir
    val srcFile = ref def_srcFile

    val HIST_PERCENTAGE   = ref DEF_HIST_PERCENTAGE
    val STRUCT_PERCENTAGE = ref DEF_STRUCT_PERCENTAGE
    val JUNK_PERCENTAGE   = ref DEF_JUNK_PERCENTAGE
    val NOISE_PERCENTAGE  = ref DEF_NOISE_PERCENTAGE
    val ARRAY_WIDTH_THRESHOLD = ref DEF_ARRAY_WIDTH_THRESHOLD

    fun histEqTolerance   x = Real.ceil((!HIST_PERCENTAGE)   * Real.fromInt(x)) 
    fun isStructTolerance x = Real.ceil((!STRUCT_PERCENTAGE) * Real.fromInt(x)) 
    fun isJunkTolerance   x = Real.ceil((!JUNK_PERCENTAGE)   * Real.fromInt(x)) 
    fun isNoiseTolerance  x = Real.ceil((!NOISE_PERCENTAGE)   * Real.fromInt(x)) 

    val groupOps : (char * char) list = [(#"\"", #"\""),  (#"[", #"]"),  (#"(", #")"),  (#"{", #"}")]  

    fun parametersToString () = 
	(   ("Source file to process: "^(!srcFile)   ^"\n")^
	    ("Output directory: "      ^(!outputDir) ^"\n")^
	    ("Max depth to explore: "  ^(Int.toString (!depthLimit))^"\n")^
	    ("Histogram comparison tolerance (percentage): "  ^(Real.toString (!HIST_PERCENTAGE))^"\n")^
	    ("Struct determination tolerance (percentage): "  ^(Real.toString (!STRUCT_PERCENTAGE))^"\n")^
	    ("Noise level threshold (percentage): "           ^(Real.toString (!NOISE_PERCENTAGE))^"\n")^
	    ("Minimum width threshold for array: "            ^( Int.toString (!ARRAY_WIDTH_THRESHOLD))^"\n")^
	    ("Junk threshold (percentage): "                  ^(Real.toString (!JUNK_PERCENTAGE))^"\n"))

    fun printParameters () = print (parametersToString ())

    (********************************************************************************)
    (*********************  END Configuration ***************************************)
    (********************************************************************************)
  
    val initialRecordCount = ref ~1  (* will be set at beginning of program based on input file *)    
    val anyErrors = ref false
    exception Exit of OS.Process.status
    fun silenceGC () = (SMLofNJ.Internals.GC.messages false)
    structure SS = Substring
    open Tokens
    type TokenOrder = Token list
    type Context = LToken list
    type DerivedContexts = Context list
    type Partition = (TokenOrder * (DerivedContexts list)) list * (Context list)

    val missingspan = {offset=0, span=0}
	
    type AuxInfo = {coverage:int} (* Coverage of 
				      -- a struct is minimum coverage of its constituents;
				      -- a union is sum of coverage of its consituents; *)
				     
    datatype Ty = Base of AuxInfo * Token | Pvoid of AuxInfo
                | TBD of AuxInfo * int * Context list | Bottom of AuxInfo * int * Context list 
                | Pstruct of AuxInfo * Ty list |  Punion of AuxInfo * Ty list 
                | Parray of AuxInfo * (Token * int) list * Ty * Ty

    fun getAuxInfo ty : AuxInfo = 
	case ty 
        of Base (a,t) => a
        |  Pvoid a    => a
        |  TBD (a,i,cl) => a
        |  Bottom (a,i,cl) => a
        |  Pstruct (a,tys) => a
        |  Punion (a,tys) => a
        |  Parray (a,tokens,ty1,ty2) => a

    fun getCoverage ty = #coverage(getAuxInfo ty)
    fun sumCoverage tys = 
	case tys of [] => 0
        | (ty::tys) => (getCoverage ty) + (sumCoverage tys)
    fun minCoverage tys = 
	case tys of [] => Option.valOf Int.maxInt
        | (ty::tys) => Int.min(getCoverage ty, minCoverage tys)

    (* Function to compute the "complexity" of a type.
       -- defined to be the depth of the tree, ie, the number of alternations of type constructors
       -- plus the number of TBD contexts
       -- plus the number of Bottom contexts
     *)

    type complexity = {numAlt:int, numTBD:int, numBottom:int}
    fun mkComplexity (alt, tbd, bottom) = {numAlt=alt, numTBD=tbd,numBottom=bottom}

    fun complexity ty : complexity =
	let fun mergeComplexity (ty, (cumAlt,cumTBD,cumBottom)) = 
	        let val {numAlt,numTBD,numBottom} = complexity ty
		in
		    (Int.max(cumAlt,numAlt), cumTBD+numTBD, cumBottom+numBottom)
		end
	    fun incAlt (alt,tbd,btm) = (alt + 1,tbd,btm) 
	in
	case ty
        of Base (a,t)      => mkComplexity(0,0,0)
        |  Pvoid a         => mkComplexity(0,0,0)
        |  TBD (a,i,cl)    => mkComplexity(0,1,0)
        |  Bottom (a,i,cl) => mkComplexity(0,0,1)
        |  Pstruct (a,tys) => mkComplexity(incAlt(List.foldr mergeComplexity (1,0,0) tys))
        |  Punion (a,tys)  => mkComplexity(incAlt(List.foldr mergeComplexity (1,0,0) tys))
        |  Parray (a,tks,ty1,ty2) => mkComplexity(incAlt(List.foldr mergeComplexity (1,0,0) [ty1,ty2]))

	end


    fun complexityToString {numAlt, numTBD, numBottom} = 
	( ("numAlt = "^(Int.toString numAlt)^"  ")^
	  ("numTBD = "^(Int.toString numTBD)^"  ")^
	  ("numBtm = "^(Int.toString numBottom)))

    fun printComplexity complexity = print (complexityToString complexity)

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

    (*    Ptime < Pmonth < Pip < Pint < Pstring < Pgroup < Pwhite < Other < Pempty < Error *)
    fun compToken (t1, t2) = 
	case (t1,t2) 
        of (Ptime i1, Ptime i2) => EQUAL
	|  (Pmonth i1, Pmonth i2) => EQUAL
	|  (Pip i1, Pip i2) => EQUAL
        |  (Pint i1, Pint i2) =>  EQUAL
        |  (Pstring s1, Pstring s2) => EQUAL
        |  (Pwhite s1, Pwhite s2) => EQUAL
        |  (Pgroup g1, Pgroup g2) => compToken(#left g1, #left g2)
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
        |  (Pgroup _, Ptime _) => GREATER
        |  (Pgroup _, Pmonth _) => GREATER
        |  (Pgroup _, Pip _) => GREATER
        |  (Pgroup _, Pint _) => GREATER
        |  (Pgroup _, Pstring _) => GREATER
        |  (Pgroup _,  _) => LESS
        |  (Pwhite _, Ptime _) => GREATER
        |  (Pwhite _, Pmonth _) => GREATER
        |  (Pwhite _, Pip _) => GREATER
        |  (Pwhite _, Pint _) => GREATER
        |  (Pwhite _, Pstring _) => GREATER
        |  (Pwhite _, Pgroup _) => GREATER
        |  (Pwhite _, _) => LESS
        |  (Other _, Ptime _) => GREATER
        |  (Other _, Pmonth _) => GREATER
        |  (Other _, Pip _) => GREATER
        |  (Other _, Pint _) => GREATER
        |  (Other _, Pstring _) => GREATER
        |  (Other _, Pgroup _) => GREATER
        |  (Other _, Pwhite _) => GREATER
        |  (Other _, _) => LESS
        |  (Pempty, Ptime _) => GREATER
        |  (Pempty, Pmonth _) => GREATER
        |  (Pempty, Pip _) => GREATER
        |  (Pempty, Pint _) => GREATER
        |  (Pempty, Pstring _) => GREATER
        |  (Pempty, Pgroup _) => GREATER
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
   type histogram = {hist : (int ref) IntMap.map ref, 
		     total: int ref,        (* number of occurrences of token in file *)
		     coverage : int ref,    (* number of records with at least one occurrence of token *)
		     structScore : int ref, (* structScore metric: weights heavy mass in single column *)
		     width : int ref}       (* number of columns in histogram *)
   type freqDist = histogram TokenTable.map
   type cluster  = freqDist list

   datatype Kind = Struct of {numTokensInCluster:int, numRecordsWithCluster:int,
			      tokens: (Token * int) list}
                 | Array of {tokens: (Token * int) list}
                 | Blob | Empty

   (* Printing routines *)
    fun ltokenToString (t,loc) = tokenToString t
    and tokenToString t = 
	case t 
        of Ptime i => i
	|  Pip i  => i
        |  Pmonth m => m
	|  Pint i => if i < 0 then "-"^(LargeInt.toString (~i)) else LargeInt.toString i
        |  Pstring s => s
        |  Pgroup {left, body, right} => (tokenToString left)^(String.concat (List.map ltokenToString body))^(tokenToString right)
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
        |  Pgroup {left, body, right} => (tokenTyToString left) ^"[Group Body]"^(tokenTyToString right)
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
         print ("Number of records with at least one token occurrence: "^(Int.toString (!(#coverage h))^".\n"));
         print ("StructScore: "^(Int.toString (!(#structScore h))^".\n"));
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
      | Array {tokens} =>
	    let fun printOne (t,count) =
		(printTokenTy t; print "\t";
		 print "Occurrences:"; print (Int.toString count);  print "\n")
	    in
		(print "Array"; print "\t";
		 List.app printOne tokens)
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

   fun covToString {coverage} = Int.toString coverage

   fun TyToStringD prefix longTBDs longBottom suffix ty = 
       (prefix^
        (case ty 
         of Pvoid aux      => ("Pvoid(" ^(covToString aux)^")")
         |  Base (aux, t)  => (tokenTyToString t)^("(" ^(covToString aux)^")")
         |  TBD (aux,i,cl) => "TBD_"^(Int.toString i)^
	                      "("^(covToString aux)^")"^
		              (if longTBDs then
			          ("\n"^(contextsToString cl)^prefix^"End TBD")
		               else "")
         |  Bottom (aux,i, cl) => "BTM_"^(Int.toString i)^
	                      "("^(covToString aux)^")"^
			      (if longBottom then
				   ("\n"^(contextsToString cl)^prefix^"End Bottom")
			       else "")
         |  Pstruct (aux, tys) =>  "Pstruct("^(covToString aux)^")\n"^
	 		    (lconcat (List.map (TyToStringD (prefix^"\t") longTBDs longBottom (";\n")) tys))^
			    prefix ^ "End Pstruct"
         |  Punion (aux, tys)  => "Punion("^(covToString aux)^")\n"^
	 		    (lconcat (List.map (TyToStringD (prefix^"\t") longTBDs longBottom (";\n")) tys))^
			    prefix ^ "End Punion"
         |  Parray (aux, tkns, ty1,ty2)  => "Parray("^(covToString aux)^")"^
			    "("^(lconcat(List.map (fn (t,loc) => (tokenTyToString t) ^" ")tkns)) ^")\n"^
			    prefix ^ "Body:\n"^
                            (TyToStringD (prefix^"\t") longTBDs longBottom (";\n") ty1)^
			    prefix^"Tail:\n"^
                            (TyToStringD (prefix^"\t") longTBDs longBottom (";\n") ty2)^
			    prefix ^ "End Parray"
        )^
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

    fun dumpParameters fileName ty = 
	let val strm = TextIO.openOut fileName
            val () = TextIO.output(strm, parametersToString())
	    val () = TextIO.output(strm, "Complexity of derived type:\n\t")
	    val () = TextIO.output(strm, complexityToString(complexity ty))
	in
	    TextIO.closeOut strm
	end

    fun dumpTyInfo path ty = 
	let fun dumpTBDs ty = 
		case ty
                of Pvoid aux => ()
		|  Base (aux,t) => ()
                |  TBD(aux,i, cl)    => dumpCL (path^"TBD_"^(Int.toString i)) cl
                |  Bottom(aux,i, cl) => dumpCL (path^"BTM_"^(Int.toString i)) cl
	        |  Pstruct (aux,tys) => List.app dumpTBDs tys
	        |  Punion (aux,tys) => List.app dumpTBDs tys
	        |  Parray (aux,tkns,ty1,ty2) => List.app dumpTBDs [ty1,ty2]
    	in  
          (print "Complexity of inferred type:\n\t";
	   printComplexity (complexity ty);
	   print "\nOutputing partitions to directory: "; print path; print "\n";
	   if OS.FileSys.isDir path handle SysErr => (OS.FileSys.mkDir path; true)
	   then (dumpParameters (path^"Params") ty; dumpTBDs ty; dumpTy (path^"Ty") ty)
	   else print "Output path should specify a directory.\n"
          )
	end

   (* Type simplification *)
   fun simplifyTy ty = 
       let fun collapseStruct [] a = a
	     | collapseStruct ((Pstruct (aux,tys))::tysRest) a = collapseStruct tysRest (a @ (collapseStruct tys []))
	     | collapseStruct (ty::tysRest) a = collapseStruct tysRest (a @ [simplifyTy ty])
	   fun collapseUnion [] a = a
	     | collapseUnion ((Punion (aux,tys))::tysRest) a = collapseUnion tysRest (a @ (collapseUnion tys []))
	     | collapseUnion (ty::tysRest) a = collapseUnion tysRest (a @ [simplifyTy ty])
       in
	   case ty 
	   of Pstruct (aux,tys) => Pstruct (aux, collapseStruct tys [])
           |  Punion  (aux,tys) => Punion  (aux, collapseUnion  tys [])
           |  Parray  (aux,tkns,ty1,ty2) => Parray  (aux,tkns,simplifyTy ty1, simplifyTy ty2)
	   |  ty                => ty
       end

   (* Histogram compuations *)
   fun mkHistogram (column:int) : histogram =
       {hist=ref (IntMap.insert(IntMap.empty, column, ref 1)), total=ref column, coverage = ref 1, 
	structScore = ref 0, width = ref 0}

   fun getSortedHistogramList c = 
       let val cList = List.map (fn(x,y)=>(x,!y)) (IntMap.listItemsi c)
	   fun cmp((x1,y1),(x2,y2)) = Int.<(y1,y2)
       in
	   ListMergeSort.sort cmp cList
       end

   fun fuzzyIntEq threshold (i1, i2) = abs(i1 - i2) < threshold

   (* formula: sum over all columns of columnNumber * (numRemainingRecords - colHeight) *)
   fun histScore numRecords (h:histogram) = 
       let val cSorted = getSortedHistogramList (!(#hist h))
	   fun foldNext ((colIndex, colHeight), (structScore, columnNumber, numRemainingRecords)) =
	       let val mass = colIndex * colHeight
	       in
		   (columnNumber * (numRemainingRecords - colHeight) + structScore, columnNumber +1, numRemainingRecords - colHeight)
	       end
	   val (structScore, colNumber, numRemaining) = List.foldl foldNext (0,1,numRecords) cSorted 
       in
	   (structScore, List.length cSorted)
       end

    fun histScoreCmp threshold ({structScore=score1,...}:histogram,{structScore=score2,...}:histogram) = 
	(!score1) < (!score2)

    fun histScoreEq threshold ({structScore=score1,...}:histogram,{structScore=score2,...}:histogram) = 
	(fuzzyIntEq threshold (!score1,!score2)) 


    (********************************************************************)
    (******************** Processing functions **************************)
    (********************************************************************)

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

    fun groupToTokens {left,body,right} = (left,missingspan)::body @ [(right, missingspan)]
    fun groupToRevTokens g = List.rev (groupToTokens g)
    fun isGroup (Pgroup g) = true
      | isGroup _ = false

    fun isString (Pstring s) = true
      | isString _ = false

    fun findGroups (tokens : LToken list) : LToken list = 
	let fun findDelim (Other c,loc) = List.find (fn(f,s) => c = f) groupOps
              | findDelim _ = NONE
            fun delimMatches r (Other c,loc) = r = c
              | delimMatches r _ = false
	    fun flatten [] = []
              | flatten ((l,body,r)::rest) = ((Other l,missingspan) :: (List.rev body)) @ (flatten rest) @ [(Other r,missingspan)] 
            fun topSearch [] acc = List.rev acc
              | topSearch (t::ts) acc = case findDelim t 
		                        of NONE => topSearch ts (t::acc)
					|  SOME (l:char,r:char) => findMatch [(l,[],r)] ts acc

            and findMatch  (delims:(char * LToken list * char) list) [] acc =  (flatten delims)@ (List.rev acc) (* missing right delim: forget grouping *)
              | findMatch ((l:char,body:LToken list,r:char)::next) (t::ts) acc = 
		  if delimMatches r t 
		  then let val match =  (Pgroup{left=Other l, body = List.rev body, right= Other r}, {offset=0,span=0})
		       in
			   case next of [] => topSearch ts (match::acc)
			   | (l1,body1,r1)::rest => findMatch ((l1, match::body1,r1)::rest) ts acc
		       end
		  else case findDelim t
		       of NONE => findMatch ((l, t::body, r)::next) ts acc
                        | SOME(l1,r1) => findMatch ((l1,[],r1)::(l,body,r)::next) ts acc 
	in
	    topSearch tokens []
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
		    val groupedMatches = findGroups matches
(*		    val () = print "printing grouped tokens:\n"
		    val () = printLTokens groupedMatches
*)
		in
		    groupedMatches
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
                    |  SOME ({hist, total, coverage, ...}: histogram)  => 
		       ((case IntMap.find(!hist, !count)
			 of NONE      => hist := IntMap.insert(!hist, !count, ref 1) 
		         |  SOME cref => cref := !cref + 1);
			 total := !total + !count;
			 coverage := !coverage + 1;
			 fd)
		in
		    (TokenTable.foldli doOneToken fd counts : freqDist)
		end
	    fun doAllRecords (fd:freqDist) [] = fd
              | doAllRecords fd (c::cs) = doAllRecords (doOneRecord fd c) cs
            val freqs : freqDist = doAllRecords TokenTable.empty countslist
	    fun scoreHistogram (h as {hist, total, coverage, structScore, width} : histogram) = 
		let val (s,w) = histScore numRecords h
		in
		    structScore := s;
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

    (* Given the highest ranked cluster from the input cluster list.
       Return:
        Struct of
          kind of cluster: struct, array
          count of the number of tokens in the cluster
          count of the number of records in which the cluster appears
          a list of the tokens in the cluster and the number of times each token appears in the cluster
        | Array of tokens and number of occurrences of those tokens in possible array
        | Blob, indicating no match
     *)
    fun analyzeClusters numRecords (clusters : (Token * histogram) list list) = 
	let fun isStruct cluster = 
		let fun getStructInfo((t, {hist, total, coverage, structScore, width}), result) =
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

		    fun mergeStructEntries tokenAnalysis =
			let fun doOne ((token,count,coverage),(cumCount, cumCoverage,tlist)) = 
			    (cumCount + count, Int.min(coverage, cumCoverage), (token, count)::tlist)
			    val (numTokens, coverage, tokens) = List.foldl doOne (0, numRecords,[]) tokenAnalysis
			in
			    (print "Junk Tolerance Threshold: "; print (Int.toString(isJunkTolerance numRecords)); print "\n";
			     print "Coverage: ";                 print (Int.toString(coverage)); print "\n";
			     print "Num Tokens: ";               print (Int.toString(numTokens)); print "\n";
			     if (coverage >= (isJunkTolerance numRecords) andalso (numTokens > 0))
				 then (SOME (Struct {numTokensInCluster = numTokens, numRecordsWithCluster = coverage, tokens = tokens}))
			     else NONE
				 )
			end
		in 
		    mergeStructEntries (List.foldl getStructInfo [] cluster)
		end

                
	    fun getArrayScore (h:histogram) = !(#coverage h)
	    (* For tokens with fuzzy-equal array scores, gives preference to punctuation tokens according
             * to token order, ie, "other" characters, and then white space, etc. *)
            fun lessArrayHist((t1,h1),(t2,h2)) = 
		let val as1 = getArrayScore h1
		    val as2 = getArrayScore h2
		    fun tknCompToBool cmp = case cmp of LESS => true | _ => false
		in
		    if Int.abs(as1 - as2) < (histEqTolerance numRecords) then tknCompToBool(compToken(t1,t2))
		    else as1 < as2
		end

            fun lessArrayCluster (c1,c2) = 
		case (c1,c2) 
                of ([],_) => true
                |  (_,[]) => false
                |  (th1::_, th2::_) => lessArrayHist(th1,th2)

            fun isArray clusters = 
		let val sortedClusters = ListMergeSort.sort lessArrayCluster clusters
		    val () = print "Clusters sorted by array criteria:\n"
		    val () = printClusters numRecords sortedClusters
		    val cluster = List.hd sortedClusters (* guaranteed by earlier check not to be [] *)
		    fun getArrayInfo((t, {hist, total, coverage, structScore, width}), result) = 
		    (print "Possible array tokens:\n"; 
		     printTokenTy t; print "\n";
		     print ("Records in possible array context:"^(Int.toString numRecords)^"\n");
                     print ("Total:"^(Int.toString (!total))^"\n");
                     print ("Coverage:"^(Int.toString (!coverage))^"\n");
                     print ("Width:"^(Int.toString (!width))^"\n");
		     if (!width >= !ARRAY_WIDTH_THRESHOLD) andalso 
			(!coverage > numRecords - (isJunkTolerance numRecords))  andalso
                        (not (isString t))
			 then (t,1)::result else result)
		    (* we probably want to compute the number of times the token appears in the cluster...*)
		    val arrayTokenAnalysis = List.foldl getArrayInfo [] cluster 
		in
		    case arrayTokenAnalysis of [] => NONE | a => SOME(Array {tokens = a})
		end

	    val kindSummary = 
		case clusters 
                of [] => Empty
                |  (cluster::cs) => 
		    case isStruct cluster
		    of SOME s => s
                    |  NONE => (case isArray clusters 
			        of SOME a => a
                                |  NONE   => (print "Identified a blob\n"; Blob))

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
			  then 
			      case rt 
			      of Pgroup g => doMatch tks rts ([], (groupToTokens g) :: (List.rev curContextAcc) :: contextListAcc)
			      |  _        => doMatch tks rts ([],                      (List.rev curContextAcc) :: contextListAcc)
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

    (* convert a list of token lists into a list of token lists, 
       expanding group tokens if argument list contains entirely
       single element lists of the same group token; otherwise, 
       return argument token list list. *)
    fun crackUniformGroups cl = 
	let fun cuf [] = []
              | cuf ([(Pgroup{left,body,right},loc)]::lts) = 
	         let fun mkEntry l body r = (l,missingspan) :: body @ [(r, missingspan)]
		     fun cuf' [] acc = List.rev acc
                       | cuf' ([(Pgroup{left=l,body,right=r},loc)]::lts) acc = 
			 if l = left then cuf' lts ((mkEntry l body r) :: acc)
			 else cl
                       | cuf' _ acc = cl
		 in
		     cuf' lts [mkEntry left body right]
		 end
              | cuf lts = cl
	in
	    cuf cl
	end

    fun mkBottom (coverage,cl) = 
	Bottom ({coverage=coverage}, !Bottomstamp, cl) before Bottomstamp := !Bottomstamp + 1

    (* Invariant: cl is not an empty column: checked before mkTBD is called with isEmpty function *)
    (* coverage is number of records in this context *)
    fun mkTBD (currentDepth, coverage, cl) = 
        (* Columns that have some empty rows must have the empty list representation
           of the empty row converted to the [Pempty] token.  Otherwise, a column
           that is either empty or some value gets silently converted to the value only. *)
	let fun cnvEmptyRowsToPempty [] = [(Pempty,{offset=0, span=0})]
              | cnvEmptyRowsToPempty l  = l
	    val cl = List.map cnvEmptyRowsToPempty cl
	    val cl = crackUniformGroups cl
	in
	    if (coverage < isNoiseTolerance(!initialRecordCount))
	    then mkBottom(coverage,cl)  (* not enough data here to be worth the trouble...*)
	    else if (currentDepth >= !depthLimit)  (* we've gone far enough...*)
	    then TBD ({coverage=coverage}, !TBDstamp, cl) before TBDstamp := !TBDstamp    + 1
	    else ContextListToTy (currentDepth + 1) cl
	end

    and clustersToTy curDepth rtokens numRecords clusters = 
	let val analysis = analyzeClusters numRecords clusters
           (* This function takes a Struct Partition and returns a Ty describing that partition *)
	    fun buildStructTy partitions = 
		let val (matches, badRecords) = partitions
		    fun isEmpty column = not (List.exists (not o null) column)
		    fun cnvOneMatch (tokenOrder, dclist) = 
			let val columns = doTranspose dclist
			    fun shuffle(columns, tokens) result =
				case (columns, tokens) 
				  of ([],[])      => raise Fail "token and column numbers didn't match (2)"
				  |  ([last], []) => List.rev (if isEmpty last then result else ((mkTBD  (curDepth,(List.length last), last)) :: result))
				  |  ([], tks)    => raise Fail "token and column numbers didn't match (3)"
				  |  (col1::colg::cols, (Pgroup g)::tks) => 
					let val coverage1 = List.length col1
					    val coverageg = List.length colg
					    val col1Ty = if isEmpty col1 then  [] else [(mkTBD (curDepth, coverage1, col1))]
					    val colgTy = [(mkTBD (curDepth, coverageg, colg))]
					in
					    shuffle(cols, tks) (colgTy @ col1Ty @ result)
					end
				  |  (col::cols, tk::tks) => 
					let val coverage = List.length col
					    val thisMatch = 
						if isEmpty col then  [(Base ({coverage=coverage}, tk))]
						else [Base ({coverage=coverage},tk), (mkTBD (curDepth, coverage, col))]
					in
					    shuffle(cols, tks) (thisMatch @ result)
					end
			         |  (cols,[]) => raise Fail "Unexpected case in shuffle function."
			in
			    case shuffle (columns, tokenOrder) []
			    of   []   => raise Fail "Expected at least one field."
			      |  [ty] => ty
			      |  tys  => Pstruct ({coverage=minCoverage tys}, tys)
			end
		    val matchTys = List.map cnvOneMatch matches
		    val resultTy =
			case (matchTys, badRecords)
  		        of   ([], [])   => Pvoid {coverage=0}                   (* I don't think this case can arise *)
			  |  ([], brs)  => mkBottom (List.length brs,brs)       (* I'm not sure this case arises either *)
			  |  ([ty], []) => ty
			  |  (tys, brs) => if isEmpty brs 
					   then Punion ({coverage=sumCoverage tys}, tys) 
					   else let val badCoverage = List.length brs
						in 
						    Punion ({coverage=badCoverage + sumCoverage tys}, 
							    (tys @ [(mkTBD (curDepth, badCoverage, brs))]))
						end
		in
		    resultTy
		end
	    fun buildArrayTy ({tokens=atokens}, rtokens) = 
		let val numTokens = List.foldl (fn((token,freq),sum) => sum + freq) 0 atokens
		    fun partitionOneRecord tlist = 
		        let fun insertOne ((token,freq),tTable) = TokenTable.insert(tTable, token, ref freq)
			    val tTable = List.foldl insertOne TokenTable.empty atokens
			    val numFound = ref 0
			    fun resetTable () = 
				let fun setOne(token,freq) = 
				    let val freqRef = Option.valOf (TokenTable.find(tTable,token))
				    in
					freqRef := freq
				    end
				in
				    List.app setOne atokens;
				    numFound := 0
				end
			    (* Return two contexts: one for all tokens in array slots except for the last one,
			       and one for the tokens in the last slot; this partition is to avoid confusion
			       with the separator not being in the last slot *)
			    fun doNextToken [] (current, all) = (all, List.rev current)
                              | doNextToken ((rt as (lrt,loc))::rts) (current, all) = 
				  case TokenTable.find(tTable, lrt)
				  of NONE => doNextToken rts (rt::current, all)
                                  |  SOME freq => 
				      if !freq <= 0 then (freq := !freq - 1; doNextToken rts (rt::current, all))
				      else (freq := !freq - 1;
					    numFound := !numFound + 1;
					    if !numFound = numTokens 
					    then (resetTable(); doNextToken rts ([], (List.rev (rt::current) :: all)))
					    else doNextToken rts (rt::current, all))
			in
			    doNextToken tlist ([],[])
			end
		    fun partitionRecords rtokens = 
			let fun pR [] (mainA,lastA) = (List.rev mainA, List.rev lastA)
                              | pR (t::ts) (mainA, lastA)= 
			    let val (main,last) = partitionOneRecord t
			    in 
				pR ts (main@mainA, last::lastA)
			    end
			in
			    pR rtokens ([],[])
			end

		    val (mainContext,lastContext) = partitionRecords rtokens
		in
		    (print "Array context\n"; 
		     Parray ({coverage=numRecords}, atokens, 
			     mkTBD(curDepth, List.length mainContext, mainContext),
			     mkTBD(curDepth, List.length lastContext, lastContext)))
		end

            val ty = case analysis 
		     of Blob => mkBottom (List.length rtokens, rtokens)
		     |  Empty => Base ({coverage=0}, Pempty)
		     |  Struct s => buildStructTy (splitRecords s rtokens) (* Can produce union of structs *)
		     |  Array a =>  buildArrayTy (a, rtokens)
	in
	    ty
	end

    and ContextListToTy curDepth context = 
	let val numRecordsinContext = List.length context
            val counts : RecordCount list = List.map countFreqs context
	    val fd: freqDist = buildHistograms numRecordsinContext counts
	    val clusters : (Token * histogram) list list = findClusters numRecordsinContext fd
	    val () = printClusters numRecordsinContext clusters
            val ty = clustersToTy curDepth context numRecordsinContext clusters
	in
	    ty
	end

    fun doIt () = 
	let val fileName = !srcFile
	    val () = print ("Starting on file "^fileName^"\n");
	    val records = loadFile fileName
	    val () = initialRecordCount := (List.length records) 
	    val rtokens : Context list = List.map ltokenizeRecord records
            val rtokens = crackUniformGroups rtokens (* check if all records have same top level group token *)
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
    fun setNoisePer   n = NOISE_PERCENTAGE := n
    fun setArrayWidth a = ARRAY_WIDTH_THRESHOLD := a
    fun addSourceFile f = srcFile    := f

    val flags = [
         ("d",        "output directory (default "^def_outputDir^")",                                      PCL.String (setOutputDir, false)),
         ("maxdepth", "maximum depth for exploration (default "^(Int.toString def_depthLimit)^")",         PCL.Int    (setDepth,     false)),
         ("h",        "histogram comparison tolerance (percentage, default "^(Real.toString DEF_HIST_PERCENTAGE)^")",    PCL.Float  (setHistPer,   false)),
         ("s",        "struct determination tolerance (percentage, default "^(Real.toString DEF_STRUCT_PERCENTAGE)^")",  PCL.Float  (setStructPer, false)),
         ("n",        "noise level (percentage, default "^(Real.toString DEF_NOISE_PERCENTAGE)^")",        PCL.Float  (setNoisePer,   false)),
         ("a",        "minimum array width (default "^(Int.toString DEF_ARRAY_WIDTH_THRESHOLD)^")",        PCL.Int    (setArrayWidth, false)),
         ("j",        "junk threshold (percentage, default "^(Real.toString DEF_JUNK_PERCENTAGE)^")",      PCL.Float  (setJunkPer,    false))
        ]

    fun processSwitches args = 
	let val banner = PCL.genBanner("learn", "Prototype Learning System", flags)
	in
	   (PCL.parseArgs(args, flags, addSourceFile, banner);
	    printParameters())
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

