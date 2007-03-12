structure Structure = 
struct
    open Config
    open Utils
    open Types
    structure SS = Substring

    val TBDstamp = ref 0
    val Bottomstamp = ref 0
    val initialRecordCount = ref ~1  (* will be set at beginning of program based on input file *)    

    (* delimiter tokens *)
    val groupOps : (Token * (Token -> Token)) list = 
	 [(Other #"\"", fn t => t),  
	  (Other #"[",  fn t => Other #"]"),  
	  (Other #"(",  fn t => Other #")"),  
	  (Other #"{",  fn t => Other #"}"),
	  (PbXML("",""), fn t => 
	                     (case t 
			      of PbXML(tag,args)=> PeXML(tag,"")
			      |  _ => raise Fail "Unexpected beginning tag.\n"))]




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
        |  TBD (a,i,cl)    => mkComplexity(0,1,0)
        |  Bottom (a,i,cl) => mkComplexity(0,0,1)
        |  Pstruct (a,tys) => mkComplexity(incAlt(List.foldr mergeComplexity (1,0,0) tys))
        |  Punion (a,tys)  => mkComplexity(incAlt(List.foldr mergeComplexity (1,0,0) tys))
        |  Parray (a,{first=ty1,body=ty2,last=ty3,...}) => mkComplexity(incAlt(List.foldr mergeComplexity (1,0,0) [ty1,ty2,ty3]))
        |  RefinedBase (a,r,tl) => mkComplexity(0,0,0)
        |  Switch(a,id,branches) => mkComplexity(incAlt(List.foldr mergeComplexity (1,0,0) ((#2 o ListPair.unzip) branches)))
        |  RArray (a,sep,term,body,len,_) => complexity body (* fix this!*)
	end


    fun complexityToString {numAlt, numTBD, numBottom} = 
	( ("numAlt = "^(Int.toString numAlt)^"  ")^
	  ("numTBD = "^(Int.toString numTBD)^"  ")^
	  ("numBtm = "^(Int.toString numBottom)))

    fun printComplexity complexity = print (complexityToString complexity)


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

   datatype unionDiv = FirstToken
   datatype Kind = Struct of {numTokensInCluster:int, numRecordsWithCluster:int,
			      tokens: (Token * int) list}
                 | Array of {tokens: (Token * int) list}
                 | Union of unionDiv
                 | Blob | Empty

   (* printing *)
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
		(print "Struct"; print "\n";
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
	| Union _ => (print  "Union"; print "\n")
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
 
   fun printOnePartition (tokenOrder, contexts) = 
       (printTList tokenOrder;
        List.app printDerivedContexts contexts)

   fun printPartitions (matches, badRecords) = 
       (print "\nPartitioned Records:\n";
        List.app printOnePartition matches;
        print "Bad records:\n";
        List.app (fn r => (print "\t"; printLTokens r)) badRecords;
        print "\n")


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
           |  Parray  (aux,{tokens=tkns,lengths,first=ty1,body=ty2,last=ty3}) => 
		     Parray  (aux, {tokens=tkns,lengths=lengths, first=simplifyTy ty1, body=simplifyTy ty2, last=simplifyTy ty3})
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

   fun loadFile path = 
       let val strm = TextIO.openIn path
	   val data : String.string = TextIO.inputAll strm
           fun isNewline c = c = #"\n" orelse c = #"\r"
           fun getLines(ss,l) = 
               if (Substring.isEmpty ss) then List.rev l
	       else let val (ln, rest) = Substring.splitl (not o isNewline) ss
                        val rest = (case Substring.getc rest
				    of NONE => rest
				    |  SOME(#"\n", rest) => rest (* UNIX EOR discipline *)
				    |  SOME(#"\r", rest) => 
					(case Substring.getc rest 
					 of SOME(#"\n", rest) => rest (* DOS EOR discipline *)
                                         |  _ => rest (* Mac OS EOR discipline *))
			            | _ => rest (* This case is impossible because of the def if isNewline *))
		    in
			getLines(rest, (Substring.string ln)::l)
		    end
           val lines = getLines ( Substring.full data, [])
	   val numRecs = List.length lines
	   val () = print ((Int.toString numRecs)^" records.\n")
	   val () = TextIO.closeIn strm
       in
	   lines
       end

    fun groupToTokens {left,body,right} = left::body @ [right]
    fun groupToRevTokens g = List.rev (groupToTokens g)
    fun isGroup (Pgroup g) = true
      | isGroup _ = false

    fun isString (Pstring s) = true
      | isString _ = false

    fun findGroups (tokens : LToken list) : LToken list = 
	let fun TokenMatch (t1,t2) = case (t1,t2) 
	                             of (PbXML _, PbXML _) => (print "looking for a pbxml token\n"; true) (* begin xml tag in group list has placeholder tag, so any bXML matches *)
				     | _ => TokenEq(t1,t2)
	    fun findDelim (t,loc) = List.find (fn(f,s) => TokenMatch (t,f)) groupOps
            fun delimMatches r (t,loc) = TokenEq(t, r)
	    fun flatten [] = (print "in flatten function\n"; [])
              | flatten ((ltoken,body,r)::rest) = (ltoken :: (List.rev body)) @ (flatten rest) 
            fun getGroupLoc ((lt, {lineNo=llineNo, beginloc=lbegin, endloc=lend}), (rt, {lineNo, beginloc=rbegin, endloc=rend})) =
		{lineNo=llineNo, beginloc=lbegin, endloc=rend}
            fun topSearch [] acc = List.rev acc
              | topSearch (t::ts) acc = case findDelim t 
		                        of NONE => topSearch ts (t::acc)
					|  SOME (lt:Token,rtf:Token->Token) => findMatch [(t,[],rtf (#1 t))] ts acc

            and findMatch  (delims:(LToken * LToken list * Token) list) [] acc = (flatten delims)@ (List.rev acc) (* missing right delim: forget grouping *)
              | findMatch ((lt:LToken,body:LToken list,r:Token)::next) (t::ts) acc = 
		  if delimMatches r t 
		  then let val match =  (Pgroup{left=lt, body = List.rev body, right= t}, getGroupLoc(lt,t))
		       in
			   case next of [] => topSearch ts (match::acc)
			   | (lt1,body1,r1)::rest => findMatch ((lt1, match::body1,r1)::rest) ts acc
		       end
		  else case findDelim t
		       of NONE => findMatch ((lt, t::body, r)::next) ts acc
                        | SOME(lt1,rtf1) => findMatch ((t,[],rtf1 (#1 t))::(lt,body,r)::next) ts acc 
	in
	    topSearch tokens []
	end

    fun lengthsToHist records =
	let val lens = List.map List.length records
	    fun insOneLen(len,fd) = 
		case IntMap.find(fd,len)
		of NONE => IntMap.insert(fd, len, ref 1)
                |  SOME count => (count := !count + 1; fd)
	    val fd = List.foldl insOneLen IntMap.empty lens
	    fun printLenHist fd = 
		let fun printOne (index, count) = 
		    (print "\t"; print (Int.toString index); print ":\t"; print(Int.toString(!count)); print "\n")
		in
		    (print "Histogram of number of tokens per record:\n";
		     IntMap.appi printOne fd;
		     print "\n")
		end
	in
	    printLenHist fd
	end

    fun ltokenizeRecord recordNumberRef (record:string) = 
	let val length = String.size record
	    fun doNonEmpty record = 
		let val cursor : int ref = ref 0
		    fun addLineNo (t,{beginloc,endloc}) = (t,{lineNo=(!recordNumberRef),beginloc=beginloc,endloc=endloc})
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
                        ( case lex() of 
                               NONE   => List.rev acc
                             | SOME a => getMatches((addLineNo a)::acc)
                        )
		    val matches = getMatches []
		    val groupedMatches = findGroups matches
(*		    val () = print "printing grouped tokens:\n"
		    val () = printLTokens groupedMatches *)
		    val () = recordNumberRef := !recordNumberRef + 1
		in
		    groupedMatches 
		end
	in
	    if length = 0 then [(Pempty,{lineNo = (!recordNumberRef), beginloc=0, endloc=0})] else doNonEmpty record
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
	let fun isStruct (cluster::_) = 
		let fun getStructInfo((t, {hist, total, coverage, structScore, width}), result) =
		    let val hList = List.map (fn(x,y)=>(x, !y)) (IntMap.listItemsi (!hist))
			val sortedHlist = ListMergeSort.sort (fn((i1,c1:int),(i2,c2))=>(c1 < c2)) hList
			val primary = List.hd sortedHlist
			fun hIsStruct sortedHlist =
			    let val rest = List.tl sortedHlist
				val massOfRest = List.foldl (fn((i,c:int),acc)=> acc + c) 0 rest
			    in
				massOfRest < (isStructTolerance numRecords)
			    end
		    in
			if hIsStruct sortedHlist then [(t, #1 primary, #2 primary)]@result  else result
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
		    case arrayTokenAnalysis of [] => (print "ARRAY NOT CHOSEN\n"; NONE) | a => SOME(Array {tokens = a})
		end
	    val unionFirst = false
	    fun isUnion clusters = SOME (Union FirstToken)
            val analyses = isStruct ::
		(if unionFirst then [isUnion, isArray] else [isArray, isUnion])

	    fun findFirst arg [] = (print "Identified a blob\n"; Blob)
	      | findFirst arg (f::fs) = case f arg of NONE => findFirst arg fs
		                        | SOME s => s 

	    val kindSummary = 
		case clusters 
                of [] => (print "No clusters found\n"; Empty)
                |  _ => findFirst clusters analyses

(*	    val kindSummary = 
		case clusters 
                of [] => (print "No clusters found\n"; Empty)
                |  (cluster::cs) => 
		    case isStruct cluster
		    of SOME s => s
                    |  NONE => (case isArray clusters 
			        of SOME a => a
                                |  NONE   => (print "Identified a blob\n"; Blob))*)


	    val () = printKindSummary kindSummary

	in
	    kindSummary : Kind
	end

    (* 
       Result of this function:
         a list of:
           a tokenOrder and a list of the derived contexts for that order
         a list of records that do not match any token order for the supplied token set.
       The derived contexts for a tokenOrder is a list of contexts, the "holes" between the tokens
       and the tokens corresponding to the matched tokens.
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
			      let val matchTokens = 
				  case rt 
				  of Pgroup g => (groupToTokens g)
				  |  _        => [lrtoken]
			      in
				  doMatch tks rts ([], matchTokens :: (List.rev curContextAcc) :: contextListAcc)
			      end
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
              | cuf ([(Pgroup(g as {left,body,right}),loc)]::lts) = 
	         let fun cuf' [] acc = List.rev acc
                       | cuf' ([(Pgroup (g' as {left=l,body,right=r}), loc)]::lts) acc = 
			 if l = left then cuf' lts ((groupToTokens g') :: acc)
			 else cl
                       | cuf' _ acc = cl
		 in
		     cuf' lts [groupToTokens g]
		 end
              | cuf lts = cl
	in
	    cuf cl
	end

    fun mkBottom (coverage,cl) = 
	Bottom ( { coverage=coverage
                 , label=SOME(mkBOTLabel (!Bottomstamp))
                 , typeComp = unitComplexity
                 , dataComp = zeroComplexity
                 }
               , !Bottomstamp
               , cl
               ) before Bottomstamp := !Bottomstamp + 1

    (* Invariant: cl is not an empty column: checked before mkTBD is called with isEmpty function *)
    (* coverage is number of records in this context *)
    fun mkTBD (callsite, currentDepth, coverage, cl) = 
        (* Columns that have some empty rows must have the empty list representation
           of the empty row converted to the [Pempty] token.  Otherwise, a column
           that is either empty or some value gets silently converted to the value only. *)
	let fun cnvEmptyRowsToPempty [] = [(Pempty,{lineNo= callsite, beginloc=0, endloc=0})] (* XXX fix line number *)
              | cnvEmptyRowsToPempty l  = l
	    val cl = List.map cnvEmptyRowsToPempty cl
	    val cl = crackUniformGroups cl
	in
	    if (coverage < isNoiseTolerance(!initialRecordCount))
	    then mkBottom(coverage,cl)  (* not enough data here to be worth the trouble...*)
	    else if (currentDepth >= !depthLimit)  (* we've gone far enough...*)
                 then TBD ( { coverage=coverage
                            , label=SOME(mkTBDLabel (!TBDstamp))
                            , typeComp = unitComplexity
                            , dataComp = zeroComplexity
                            }
                          , !TBDstamp
                          , cl
                          ) before TBDstamp := !TBDstamp    + 1
                 else ContextListToTy (currentDepth + 1) cl
	end

    and clustersToTy curDepth rtokens numRecords clusters = 
	let val analysis = analyzeClusters numRecords clusters
            (* This function partitions a context into a union. *)
            (* It currently uses the first token in each context to do the partition *)
            fun buildUnionTy (FirstToken, rtokens) = 
		let val () = print "BUILDING UNION TY\n"
		    val numChunks = List.length rtokens
		    fun updateTable(token,chunk,tTable) = 
		        case TokenTable.find(tTable, token)
			of NONE => TokenTable.insert(tTable, token, ref [chunk])
                        |  SOME chunkList => (chunkList := chunk::(!chunkList);   (* note that chunks end up in reverse order *)
					      tTable)
		    fun doOneChunk([], tTable) = updateTable(Pempty,[], tTable)
                      | doOneChunk(chunk as ((t,loc)::ts), tTable) = updateTable(t,chunk,tTable)
		    val pTable = List.foldl doOneChunk TokenTable.empty rtokens 
		    (* allSame handles the case where all chunks start with the same, non-Empty token *)
                    fun allSame rtokens = 
			let fun doOne ((lt::lts), (fst,snd)) = ([lt]::fst, lts::snd)
			    val (fsts,snds) = List.foldl doOne ([],[]) rtokens
			in
			    Pstruct(mkTyAux numRecords, 
				    [mkTBD(~9, curDepth, numChunks, List.rev fsts),
				     mkTBD(~10,curDepth, numChunks, List.rev snds)])
			end
                    (* allEmpty handles the case where all chunks are the empty chunk *)
		    fun allEmpty () = Base(mkTyAux numRecords, [(Pempty,{lineNo= ~1, beginloc=0, endloc=0})])
		    (* doPartition handles the case where the chunks did not all have the same initial token *)
		    fun doPartition pTable = 
			let val items = TokenTable.listItems pTable (* list of chunks, one per intital token, in reverse order *)
			    val tys = List.map (fn item => mkTBD(~11, curDepth, List.length (!item), List.rev (!item)) ) items
			in
			    Punion(mkTyAux numRecords, tys)
			end
			    
		in
		    if TokenTable.numItems(pTable) = 1 
			then if TokenTable.inDomain(pTable, Pempty) then allEmpty () 
			else allSame rtokens
                    else doPartition pTable
		end

           (* This function takes a Struct Partition and returns a Ty describing that partition *)
	    fun buildStructTy partitions = 
		let val (matches, badRecords) = partitions
		    fun isEmpty column = not (List.exists (not o null) column)
		    fun cnvOneMatch (tokenOrder, dclist) = 
			let val columns = doTranspose dclist
			    fun recurse(columns, tokens) result =
				case (columns, tokens) 
				  of ([],[])      => raise Fail "token and column numbers didn't match (2)"
				  |  ([last], []) => List.rev (if isEmpty last then result else ((mkTBD  (~1, curDepth,(List.length last), last)) :: result))
				  |  ([], tks)    => raise Fail "token and column numbers didn't match (3)"
				  |  (col1::coltk::cols, tk::tks) => 
				       (* col1 is context before tk;
					  colt is context corresponding to tk
					  cols is list of contexts following *)
					let fun borrowLoc col1 colref = 
					        let fun doit ([],[] : LToken list list) (a : LToken list list) = List.rev a
						    |   doit ([]::r, [(t,{lineNo,beginloc,endloc})]::s) a = 
						                 doit (r,s) ([(Pempty,{lineNo=lineNo, beginloc=0,endloc=beginloc})]::a)
						    |   doit (r::rs,t::ts) a = doit (rs,ts) (r ::a)
						in
						    doit (col1, colref) []
						end
					    val coverage1 = List.length col1
					    val coveraget = List.length coltk
					    val col1Ty = if isEmpty col1 then  [] else [(mkTBD (~8, curDepth, coverage1, (borrowLoc col1 coltk)))]
					    val coltkTy = case tk 
						          of Pgroup g => [(mkTBD (~3, curDepth, coveraget, coltk))]
							   | _ =>        [(Base  (mkTyAux coveraget, List.concat coltk))]
					in
					    recurse(cols, tks) (coltkTy @ col1Ty @ result)
					end
			         |  (cols,[]) => raise Fail "Unexpected case in recurse function."
			in
			    case recurse (columns, tokenOrder) []
			    of   []   => raise Fail "Expected at least one field."
			      |  [ty] => ty
			      |  tys  => Pstruct (mkTyAux (minCoverage tys), tys)
			end
		    val matchTys = List.map cnvOneMatch matches
		    val resultTy =
			case (matchTys, badRecords)
  		        of   ([], [])   => raise Fail "Expected records in struct context."
			  |  ([], brs)  => (* This case arises if a false cluster is identified: a cluster with one
					      or more tokens coming from one subset of records, and another group of tokens
					      coming from a disjoint subset. So we should introduce a union.*)
(*			                   (print "in mkbottom case\n"; mkBottom (List.length brs,brs))       *)
					   (print "converting false struct into union\n"; buildUnionTy(FirstToken, brs))
			  |  ([ty], []) => ty
			  |  (tys, brs) => if isEmpty brs 
					   then Punion (mkTyAux(sumCoverage tys), tys) 
					   else let val badCoverage = List.length brs
						in 
						    Punion (mkTyAux(badCoverage + sumCoverage tys), 
							    (tys @ [(mkTBD (~4, curDepth, badCoverage, brs))]))
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
			    (* Return three contexts: 
			       one for all tokens in first slot,
			       one for all tokens in array slots except for the first or last one,
			       and one for the tokens in the last slot; this partition is to avoid confusion
			       with the separator not being in the last slot *)
			    fun doNextToken isFirst [] (current, first, main) = 
				 let fun getLen [] = 0
				       | getLen _ = 1
				     val length = (getLen current) + (* list of tokens in current context: if present, length is 1 *)
						  (getLen first) +   (* list of tokens in first context: if present, length is 1 *)
						  (List.length main) (* a list of matched tokens, so no need to compute div*)
				     fun getLoc [] = (print "WARNING: ARRAY first context empty!"; ~1)
				       | getLoc ((tok,loc:location)::ltocs) = #lineNo loc
				 in
				     ((length, getLoc first), first, main, List.rev current)
				 end
                              | doNextToken isFirst ((rt as (lrt,loc))::rts) (current, first, main) = 
				  case TokenTable.find(tTable, lrt)
				  of NONE => doNextToken isFirst rts (rt::current, first, main)
                                  |  SOME freq => 
				      if !freq <= 0 
				      then (freq := !freq - 1; 
					    doNextToken isFirst rts (rt::current, first, main))
				      else (freq := !freq - 1;
					    numFound := !numFound + 1;
					    if !numFound = numTokens 
					    then (resetTable(); 
						  if isFirst 
						    then doNextToken false   rts ([], List.rev (rt::current),  main)
						    else doNextToken isFirst rts ([], first, (List.rev (rt::current) :: main)))
					    else doNextToken isFirst rts (rt::current, first, main))
			in
			    doNextToken true tlist ([],[],[])
			end
		    fun partitionRecords rtokens = 
			let fun pR [] (numTokenA, firstA, mainA,lastA) = (List.rev numTokenA, List.rev firstA, List.rev mainA, List.rev lastA)
                              | pR (t::ts) (numTokenA, firstA, mainA, lastA) = 
			            let val (numTokens, first, main,last) = partitionOneRecord t
				    in 
					pR ts (numTokens::numTokenA, first::firstA, main@mainA, last::lastA)
				    end
			in
			    pR rtokens ([],[],[],[])
			end

		    val (arrayLengths, firstContext,mainContext,lastContext) = partitionRecords rtokens
		in
		    (print "Array context\n"; 
		     Parray (mkTyAux numRecords, 
			     {tokens  = atokens, 
			      lengths = arrayLengths,
			      first   = mkTBD(~5, curDepth, List.length firstContext, firstContext),
			      body    = mkTBD(~6, curDepth, List.length mainContext, mainContext),
			      last    = mkTBD(~7, curDepth, List.length lastContext, lastContext)}))
		end


            val ty = case analysis 
		     of Blob =>     mkBottom (List.length rtokens, rtokens)
		     |  Empty =>    Base (mkTyAux 0, [(Pempty,{lineNo= ~1, beginloc=0, endloc=0})])
		     |  Struct s => buildStructTy (splitRecords s rtokens) (* Can produce union of structs *)
		     |  Array a =>  buildArrayTy (a, rtokens)
                     |  Union u =>  buildUnionTy(u, rtokens)
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
    fun computeStructure fileName = 
	let val recordNumber = ref 0
	    val () = print ("Starting on file "^fileName^"\n");
	    val records = loadFile fileName
	    val () = initialRecordCount := (List.length records) 
	    val rtokens : Context list = List.map (ltokenizeRecord recordNumber) records
            val rtokens = crackUniformGroups rtokens (* check if all records have same top level group token *)
	    val () = lengthsToHist rtokens
	    val ty = ContextListToTy 0 rtokens
	    val sty = simplifyTy ty
	in
	    sty
	end


end
