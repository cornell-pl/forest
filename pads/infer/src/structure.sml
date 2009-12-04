structure Structure = 
struct
    open Config
    open Utils
    open Types
    open Options
    structure CU = ClusterUnions
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

    val delimiterCandidates : Token list = 
	   let val candidateChars = [#"|", #",", #";", #"!", #"&", #":", #"^", #"%", #"$", #"#", #"~"] 
	   in
	       List.map (fn x=> Other x) candidateChars
	   end

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
        |  Poption (a, body) => complexity body (* fix this!*)
	end


    fun complexityToString {numAlt, numTBD, numBottom} = 
	( ("numAlt = "^(Int.toString numAlt)^"  ")^
	  ("numTBD = "^(Int.toString numTBD)^"  ")^
	  ("numBtm = "^(Int.toString numBottom)))

    fun printComplexity complexity = print (complexityToString complexity)



   structure TokenTable = RedBlackMapFn(
                     struct type ord_key = Token
			    val compare = compToken
		     end) 


   structure IntMap = RedBlackMapFn(
                     struct type ord_key = int
			    val compare = Int.compare
		     end) 

   type RecordCount = (int ref) TokenTable.map
   val tokenCount : RecordCount ref = ref TokenTable.empty
   fun countToken counts t = 
       case TokenTable.find (counts, t)
       of NONE =>  TokenTable.insert(counts, t, ref 1)
       |  SOME iref => (iref := !iref + 1; counts)
   fun countTokensPerRecord counts [] = counts
     | countTokensPerRecord counts ((t,loc)::ts) = countTokensPerRecord (countToken counts t) ts
   fun countTokens counts [] = counts
     | countTokens counts (ts::tts) = countTokens (countTokensPerRecord counts ts) tts
   fun updateTokenCounts tokens = tokenCount := countTokens (!tokenCount) tokens
   fun getSeparator rtokens = 
       let val () = updateTokenCounts rtokens
	   fun tokenInFile t = not(Option.isSome (TokenTable.find (!tokenCount,t)))
       in
	   List.find tokenInFile delimiterCandidates
       end

   type histogram = {hist : (int ref) IntMap.map ref, 
		     total: int ref,        (* number of occurrences of token in file *)
		     coverage : int ref,    (* number of records with at least one occurrence of token *)
		     structScore : int ref, (* structScore metric: weights heavy mass in single column *)
		     width : int ref,              (* number of columns in histogram *)
		     minOccurrences : int ref (* minimum time token *appears* in any record *)
                    }

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
         print ("Number of records with at least one token occurrence: "^(Int.toString (!(#coverage h)))^".\n");
         print ("StructScore: "^(Int.toString (!(#structScore h))^".\n"));
         print ("Minimum number of *occurrences* of token: "^(Int.toString(!(#minOccurrences h)))^".\n");
	 IntMap.appi (printOneFreq numRecords) (!(#hist h)); print "\n")
    
    fun printDist numRecords freqs = (print "Distributions:\n"; TokenTable.appi (printAugHist numRecords) freqs; print "\n")

    fun printKindSummary kind = 
	case kind 
        of Struct {numTokensInCluster=count, numRecordsWithCluster=coverage, tokens=tinfos} =>
	    let fun printOne (t,count) =
		(printTokenTy t; print "\t";
		 print "Occurrences:"; print (Int.toString count);  print "\n")
	    in
		(print "Struct Kind"; print "\n";
		 print "Coverage:";    print (Int.toString coverage);  print "\n";
		 print "Token count:"; print (Int.toString count);     print "\n";
		 List.app printOne tinfos)
	    end
      | Array {tokens} =>
	    let fun printOne (t,count) =
		(printTokenTy t; print "\t";
		 print "Occurrences:"; print (Int.toString count);  print "\n")
	    in
		(print "Array Kind"; print "\t";
		 List.app printOne tokens)
	    end
	| Union _ => (print  "Union Kind"; print "\n")
        | Blob => (print  "Blob Kind"; print "\n")
        | Empty => (print  "Empty Kind"; print "\n")

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

    fun dumpClusters curDeps numRecords clusters fileName = 
       let fun oneFreqToStr numRecords (int, countRef) = 
	     let val percent = (Real.fromInt (!countRef ))/(Real.fromInt numRecords)
	     in
	     ("\t" ^ (Int.toString int) ^ ":\t" ^ (Int.toString(!countRef)) ^
	     "\t" ^ (Real.toString percent) ^ "\n")
	     end
           fun histToString numRecords (token, (h:histogram)) = 
	       	("Token: " ^ (tokenTyToString token) ^ "\n"  ^
	 	(lconcat (map (oneFreqToStr numRecords) (IntMap.listItemsi (!(#hist h))))) ^ "\n")
           fun oneClusterToString n c = 
	       ("Cluster "^(Int.toString n)^":\n" ^
		(lconcat (map (histToString numRecords) c)) ^ "\n")
	   fun pC n [] = ""
             | pC n (c::cs) = (oneClusterToString n c) ^ (pC (n+1) cs)
	   val clusters = pC 0 clusters
	   val strm = if curDeps = 0 then TextIO.openOut fileName
		      else TextIO.openAppend fileName
	   val () = TextIO.output (strm, ("====================\nITERATION " ^ (Int.toString curDeps) ^ "\n"))

	   val () = TextIO.output (strm, clusters)
	in TextIO.closeOut strm
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
	structScore = ref 0, width = ref 0, minOccurrences = ref 0}

   fun getSortedHistogramList c = 
       let val cList = List.map (fn(x,y)=>(x,!y)) (IntMap.listItemsi c)
	   fun cmp((x1,y1),(x2,y2)) = Int.<(y1,y2)
       in
	   ListMergeSort.sort cmp cList
       end

   fun getHistNormalForm (numRecords, h:histogram) = 
       let val coverage = !(#coverage h)
	   val intList = (0,numRecords - coverage) :: (getSortedHistogramList (!(#hist h)))
       in
           List.map (fn(x,y) => (x,Real.fromInt y)) intList
       end
    

    fun printOneRealFreq numRecords (int, freq) = 
	let val percent = freq/(Real.fromInt numRecords)
	in
	    (print "\t"; print (Int.toString int); print ":\t"; print(Real.toString(freq)); 
	     print "\t"; print (Real.toString percent); ( print "\n"))
	end

    fun printHistNormalForm s numRecords hiNorm = 
        (print ("Normalized histogram: "^s^"\n");
         List.app (printOneRealFreq numRecords) hiNorm)

    fun avgNormHist(numRecords, h1Norm, h2Norm) = 
	let fun halve [] a = List.rev a
              | halve ((f,c)::r) a = halve r ((f,c/(Real.fromInt 2))::a)
            fun avg [] rs a = (List.rev a) @ (halve rs [])
              | avg rs [] a = (List.rev a) @ (halve rs [])
              | avg ((x,xc)::h1s) ((y,yc)::h2s) a = 
		  avg h1s h2s ((x,(xc + yc)/(Real.fromInt 2)) :: a)
	    val result = avg h1Norm h2Norm []
	    val () = if print_verbose then (printHistNormalForm "h1" numRecords h1Norm;
					    printHistNormalForm "h2" numRecords h2Norm;
					    printHistNormalForm "avg" numRecords result)
		     else ()
	in
	    result
	end

    fun avgHist(numRecords, h1:histogram, h2:histogram) = 
	let val h1Norm = getHistNormalForm(numRecords, h1)
            val h2Norm = getHistNormalForm(numRecords, h2)
	in
	    avgNormHist(numRecords, h1Norm, h2Norm)
	end

    fun relativeEntropy (numRecords, h1:histogram, h2: histogram) = 
	let fun isZero r = (Real.abs(r - 0.0)) <= 0.0000001
	    fun blend (h1i, h2i) = 
		if isZero h1i then 0.0
		else let val ph1i = h1i / (Real.fromInt numRecords) (* convert count to probability *)
		         val ph2i = h2i / (Real.fromInt numRecords) (* convert count to probability *)
		     in
			 ph1i * (Math.ln(ph1i/ph2i))
		     end
            fun rawre [] h2 a = a
              | rawre h1 [] a = a
              | rawre ((x,xc)::h1s) ((y,yc)::h2s) a = rawre h1s h2s (a + (blend(xc,yc)))
	    fun re (h1, h2) = 
		let val h1Norm = getHistNormalForm(numRecords, h1)
		    val h2Norm = getHistNormalForm(numRecords, h2)
		    val avgNorm = avgNormHist (numRecords, h1Norm, h2Norm)
		    val h1RE = (rawre h1Norm avgNorm 0.0)
		    val h2RE = (rawre h2Norm avgNorm 0.0)
		    val result = 0.5 * h1RE + 0.5 * h2RE
		    val () = if print_verbose then (print "Relative Entropy: "; (print (Real.toString result)); print "\n") else ()
		in
		    result
		end
	    
	in
	    re (h1,h2)
	end

    fun getFreqs (t,h) = h
    fun testAvg (numRecords, cluster) = 
	let val first = hd cluster
        in
	    if List.length first > 1 
            then  let val firstH = hd first
		      val firstF = getFreqs firstH
		      val secondH = hd (tl first)
		      val secondF = getFreqs secondH
		  in
		     (print "Relative entropy of two histograms from same cluster:\n";
                      print(Real.toString(relativeEntropy( numRecords, firstF, secondF)));
		      print ".\n")
		  end
            else ();
            if List.length (tl cluster) > 1 
            then let val firstH = hd first
		     val firstF = getFreqs firstH
		     val secondH = hd(hd (tl cluster))
		     val secondF = getFreqs secondH
		  in
		      (print "Relative entropy of two histograms from different clusters:\n";
		       print(Real.toString(relativeEntropy(numRecords, firstF, secondF)));
                       print ".\n")
		 end
	    else ()
	end

    fun histDistance (numRecords, h1:histogram, h2:histogram) = ()


    fun fuzzyIntEq threshold (i1, i2) = abs(i1 - i2) < threshold

   (* sort histogram by column heights using getSortedHistogramList *)
   (* only non-zero heights represented in histogram *)
   (* columns numbered from 1 to n, the number of non-zero columns *)
   (* column 1 is highest column, column n is lowest non-zero column *)
   (* column height is number of records with that column's frequency *)
   (* formula: sum over all columns of columnNumber * (numRemainingRecords - colHeight) *)
   (* higher score for more columns, higher score for records not covered by current column set *)
   fun histScore numRecords (h:histogram) = 
       let val cSorted = getSortedHistogramList (!(#hist h))
	   fun findMinimumOccurrences [] a = a
             | findMinimumOccurrences ((col,height)::rest) a = 
	        if col < a then findMinimumOccurrences rest col else findMinimumOccurrences rest a 
	   fun foldNext ((colIndex, colHeight), (structScore, columnNumber, numRemainingRecords)) =
		   (columnNumber * (numRemainingRecords - colHeight) + structScore, columnNumber +1, numRemainingRecords - colHeight)
	   val (structScore, colNumber, numRemaining) = List.foldl foldNext (0,1,numRecords) cSorted 
       in
	   (structScore, List.length cSorted, findMinimumOccurrences (tl cSorted) (#1(hd cSorted)) )
       end

    fun histScoreCmp threshold ({structScore=score1,...}:histogram,{structScore=score2,...}:histogram) = 
	(!score1) < (!score2)

    fun histScoreEq threshold ({structScore=score1,...}:histogram,{structScore=score2,...}:histogram) = 
	(fuzzyIntEq threshold (!score1,!score2)) 


    (********************************************************************)
    (******************** Entropy functions    **************************)
    (********************************************************************)
    structure AtomMap = RedBlackMapFn(
			   struct type ord_key = Atom.atom
				  val  compare = Atom.compare
			   end)
    type OneGram = (int ref) AtomMap.map
    type TwoGram = (OneGram ref) AtomMap.map

    fun insOne(oneGram,atom) = 
	case AtomMap.find(oneGram,atom)
	of NONE => AtomMap.insert(oneGram,atom, ref 1)
        |  SOME count => (count := !count + 1; oneGram)

    fun insTwo(twoGram,first,second) = 
        case AtomMap.find(twoGram,first)
        of NONE => (* first time we've seen a two gram starting with first *)
	           AtomMap.insert(twoGram,first, ref (insOne(AtomMap.empty,second)))
        | SOME oneGram => (oneGram := insOne(!oneGram,second); twoGram)

    fun prob1G(totalChars, numDistinct, oneGram, next) = 
	let val denominator = Real.fromInt (totalChars + numDistinct)
	in case AtomMap.find(oneGram,next)
           of NONE => 1.0/ denominator  (* This case really should never arise...*)
           |  SOME c => ((Real.fromInt (!c)) + 1.0) /denominator
	end

    fun prob2G(oneGram, twoGram, context, next) = 
	let val numDistinct = AtomMap.numItems oneGram
	in
	case AtomMap.find(twoGram,context)
        of NONE => 0.0 (* this case should not arise *)
        |  SOME condOneGram => (
	      let val numContext = !(valOf (AtomMap.find(oneGram,context)))
		  val denominator = numContext + numDistinct (* to account for +1 to solve zero-occurrence problem *)
		  val numerator = 
		  case AtomMap.find(!condOneGram, next) 
		  of NONE => 1 
                  |  SOME count => ((!count) + 1) 
	      in
		  (Real.fromInt numerator) / (Real.fromInt denominator)
	      end
	   (* end case *))
	end
   
    fun predict(m,s,c) = 
	let val (totalChars, numDistinct,oneGram,twoGram) = m
	in
	    0.8*prob2G(oneGram,twoGram,s,c) + 
            0.2*prob1G(totalChars,numDistinct,oneGram,c)
	end
    fun ln(x:real) : real = Math.ln x
    fun I (m,s,c) = ~(ln (predict (m,s,c)))

    val Hmemo : (real ref) AtomMap.map ref = ref AtomMap.empty 
    fun H (m,s) = 
	let val s = Atom.atom (Char.toString s) (* convert char to atom *)
	in
	    case AtomMap.find(!Hmemo,s)
	    of NONE =>
		let val (totalChars, numDistinct,oneGram,twoGram) = m
		    val alphabet = AtomMap.listKeys oneGram
		    fun doSum(c,sum:real) = sum + (predict (m,s,c)*I(m,s,c)) 
		    val result = List.foldl doSum 0.0 alphabet 
		in
		    Hmemo := AtomMap.insert(!Hmemo,s, ref result);
		    result
		end
	  | SOME result => !result
	end

    fun getThreshold m = 
	let val (totalChars, numDistinct,oneGram,twoGram) = m
	in
	    (1.0/Math.e) * (ln(Real.fromInt numDistinct))
	end


    fun eTokenize (m, threshold, sdata, len) = 
	let val () = print (("Len is:")^(Int.toString len)^".\n")
	    fun finish entropy c (little:char list) (big:(string * real) list) = (String.implode(List.rev(c@little)),entropy) :: big
	    fun loop index (littleAcc:char list) (bigAcc:(string * real) list) = 
		if index = len then 
		    List.rev(finish 0.0 [] littleAcc bigAcc)
		else let val current = Substring.sub(sdata,index)
			 val entropy = H(m,current) 
(*			 val () = if index mod 1000 = 0 then
			             print ("Current index: "^(Int.toString index)^" Entropy: "^(Real.toString entropy)^"\n")
				  else () *)
		     in
			 if entropy > threshold then 
			     loop (index+1) [] (finish entropy [current] littleAcc bigAcc)
			 else
			     loop (index+1) (current::littleAcc) bigAcc
		     end
	in
	    loop 0 [] []
	end



     fun OneGramToString prefix oneGram f = 
	let val sList = List.map (fn(x,y) => 
				  "\t"^prefix^(Atom.toString x)^
				  "\t"^(Int.toString (!y))^
				  "\t"^(f (x,(!y)))^
				  "\n") 
	                (AtomMap.listItemsi oneGram)
	in
	    String.concat sList
	end

    fun TwoGramToString prefix oneGram twoGram = 
	let  
	    val sList = List.map (fn(x,y) => (OneGramToString (prefix ^(Atom.toString x)) 
					                       (!y)
							       (fn(second,_)=>Real.toString(prob2G(oneGram,twoGram,x,second)))
							       )^"\n") (AtomMap.listItemsi twoGram)			 
	in
	    String.concat sList
	end

    fun eTokens2String tks = 
	case tks 
        of [] => "\n"
        | ((s,entropy)::ts) => s^"\t"^(Real.toString entropy)^"\n"^(eTokens2String ts)

    fun printETokens tks = 
	case tks 
        of [] => print "\n"
        | ((s,entropy)::ts) => ((print (s^"\t"^(Real.toString entropy)^"\n")); (printETokens ts))

    fun buildCounts sdata = 
	let val data = Substring.full sdata
	    val len = Substring.size data
	    fun loop i (r as (oneGram, twoGram)) = 
		if i = len then r
		else let val currentAtom = Atom.atom(Char.toString(Substring.sub(data,i)))
                         val oneGram' = insOne(oneGram,currentAtom)
			 val twoGram' = 
   			      let val prevAtom = Atom.atom(Char.toString(Substring.sub(data,i-1)))
			      in
				  insTwo(twoGram,prevAtom,currentAtom) 
			      end handle Subscript => twoGram
		     in
			 loop (i+1) (oneGram', twoGram')
		     end
	    val (oneGram,twoGram) = loop 0 (AtomMap.empty, AtomMap.empty)
	    val numDistinctChars = AtomMap.numItems oneGram
	    val model = (len,numDistinctChars, oneGram,twoGram)
	    val threshold = getThreshold model
	    val eTokens = eTokenize (model,threshold, data,len)
	in
	 (   print ("Number of characters: "^(Int.toString len)^"\n");
             print ("Number of distinct characters: "^(Int.toString numDistinctChars)^"\n");
	     print "One Grams:\n";
	     print (OneGramToString "" oneGram (fn _=>""));
	     print "Two Grams:\n";
	     print (TwoGramToString "" oneGram twoGram);
	     print "END Character counts:\n\n";
	     print ("Tokens detected by entropy with threshold:"^(Real.toString threshold)^":\n");
	 (*    print (eTokens2String eTokens); *)
	     printETokens eTokens;
	     print "END Entropy tokens:\n\n"
	 )
	end

    (********************************************************************)
    (******************** Processing functions **************************)
    (********************************************************************)

    fun loadFile path = 
       let val strm = TextIO.openIn path
	   val data : String.string = TextIO.inputAll strm
	   val () = if !printEntropy then buildCounts data else ()
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
    fun loadFiles paths =
      if length paths = 1 then loadFile (hd paths)
      else
	let 
	    fun loadSingleFile path = 
		let val strm = TextIO.openIn path
		    val record = TextIO.inputAll strm
(*
		    val _ = print ("record:\n"^record^"\n")
*)
		in record
		end
	    val records = map loadSingleFile paths 
	in records
	end
    fun groupToTokens {left,body,right} = left::body @ [right]
    fun groupToRevTokens g = List.rev (groupToTokens g)
    fun isGroup (Pgroup g) = true
      | isGroup _ = false

    fun isString (Pstring s) = true
      | isString _ = false

    fun isSeparator (Other c) = true
      | isSeparator (Pwhite s) = true
      | isSeparator (Pgroup x) = isSeparator (#1 (#left x))
      | isSeparator _ = false

    (*given a list of LTokens, find groupings in them and return the list of groups, or if no groupings are found,
	return the original list *)
    fun findGroups (tokens : LToken list) : LToken list = 
	let fun TokenMatch (t1,t2) = case (t1,t2) 
	                             of (PbXML _, PbXML _) => (print "looking for a pbxml token\n"; true) (* begin xml tag in group list has placeholder tag, so any bXML matches *)
				     | _ => TokenEq(t1,t2)
	    fun findDelim (t,loc) = List.find (fn(f,s) => TokenMatch (t,f)) groupOps
            fun delimMatches r (t,loc) = TokenEq(t, r)
	    fun flatten [] = ((*print "in flatten function\n";*) [])
              | flatten ((ltoken,body,r)::rest) = (ltoken :: (List.rev body)) @ (flatten rest) 
            fun getGroupLoc ((lt, {lineNo=llineNo, beginloc=lbegin, endloc=lend, recNo=lrecNo}), 
			     (rt, {lineNo,         beginloc=rbegin, endloc=rend, recNo=rrecNo})) =
		{lineNo=llineNo, beginloc=lbegin, endloc=rend, recNo=lrecNo}
            fun topSearch [] acc = List.rev acc
              | topSearch (t::ts) acc = case findDelim t 
		                        of NONE => topSearch ts (t::acc)
					|  SOME (lt:Token,rtf:Token->Token) => findMatch [(t,[],rtf (#1 t))] ts acc

            and findMatch  (delims:(LToken * LToken list * Token) list) [] acc = 
			(List.rev acc)@(flatten (List.rev delims)) (* missing right delim: forget grouping *)
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
		    fun addLineNo (t,{beginloc,endloc}) = (t,
			{lineNo=(!recordNumberRef),beginloc=beginloc,endloc=endloc, recNo=(!recordNumberRef)})
		    fun feedLex n = 
			let val s = if (n > (length - !cursor)) 
			    then SS.string(SS.extract(record, !cursor, NONE))  handle Subscript => ""
			    else SS.string(SS.substring(record, !cursor, n))  handle Subscript => ""
			in
			    cursor := !cursor + n;
			    s
			end
		    (* can add more variations here *)
		    val lex =   if (!lexName) = "vanilla" then
				  VanillaLex.makeLexer feedLex
				else
				  TokensLex.makeLexer feedLex
		    fun getMatches acc =
                        ( case lex() of 
                               NONE   => List.rev acc
                             | SOME a => getMatches((addLineNo a)::acc)
                        )
		    val matches = getMatches []
		    val groupedMatches = findGroups matches
		    (* 
		    val () = print "printing grouped tokens:\n"
		    val () = printLTokens groupedMatches 
		    *)
		    val () = recordNumberRef := !recordNumberRef + 1
		in
		    groupedMatches 
		end
	in
	    if length = 0 then 
		[(Pempty,{lineNo = (!recordNumberRef), beginloc=0, endloc=0, recNo=(!recordNumberRef)})] 
		before (recordNumberRef := !recordNumberRef +1)
	    else doNonEmpty record
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
	    val () = if print_verbose then printFreqs counts else ()
	in
           counts
	end
  
    (* hs: maps tokens to histograms (which map ints to int refs) *)
    (* counts: maps tokens to int refs *)
    fun buildHistograms numRecords (countslist : RecordCount list)= 
	let val () = if print_verbose then print "Building histograms...\n" else ()
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
	    fun scoreHistogram (h as 
		{hist, total, coverage, structScore, width, minOccurrences} : histogram) = 
		let val (s,w, minOcc) = histScore numRecords h
		in
		    structScore := s;
		    width := w;
		    minOccurrences := minOcc
		end
	    val () = TokenTable.app scoreHistogram freqs
	in
	    freqs
	end

    fun TokenPairComp ((t1a,t1b), (t2a,t2b)) = 
	let val r1 = compToken (t1a, t2a)
	in
	    if  r1 = EQUAL then compToken (t1b, t2b) else r1
	end

    structure TokenPairTable = RedBlackMapFn(
                     struct type ord_key = Token * Token
			    val compare = TokenPairComp
		     end) 

    (* new clustering algorithm based on relative entropy *)
    (* identify each histogram by the token associated with it. *)
    (* each cluster is intuitively a set of tokens.
         We choose the smallest token in the set to represent the cluster.
         We represent the set as a map from the representative token to the list of tokens in the set, including the representative. *)
    (* d(h1,h2) = relativeEntropy(h1,h2)
       d(h, C) = minimum(hi) d(h,h1) where hi in cluster C
       d(C1,C2) = minimum (hi) d(h,C2) where hi in cluster C1 *)
    (* Initially, each token forms its own cluster *)
    (* Build a map from each pair of tokens to the relative entropy (distance) between their associated histograms 
         because the relative entropy is symmetric, we only need the map to store the distances from the smaller to the larger token.
         the relative entropy for a pair of equal tokens is zero, so we don't need to store this value either. *)
    (* This maps forms the intial set of distances between clusters *)
    (* In each round, we select the pair of clusters with the smallest difference.
          If that difference is above a given threshold, we terminate the clustering.
          Otherwise, we merge the two clusters and build a new cluster distance map from the old one. *)
       
    fun newFindClusters numRecords (freqDist:freqDist) = 
	let val distList = TokenTable.listKeys(freqDist)
            fun buildInitialClusters distList = 
		let fun doOne (token, clusters) = TokenTable.insert(clusters,token, [token])
		in List.foldl doOne TokenTable.empty distList end
	    val Cs_init : (Token list) TokenTable.map = buildInitialClusters distList
	    fun printClusters s clusters = 
		let fun doOne (t,tlist) = (print "Rep token: "; printTokenTy t; print "\n"; printTList tlist)
		in
		    (print s; print "\n";
		    TokenTable.appi doOne clusters;
		     print "\n")
		end
	    val () = if print_verbose then 
			(printClusters "Initial clusters: " Cs_init) 
			else ()
            fun mergeClusters Cs_old t1 t2 = 
		let val () = if print_verbose then (print "Merging clusters: "; printTokenTy t1; print " "; printTokenTy t2; print "\n") else ()
		    val (Cs_1, c_t1) = TokenTable.remove(Cs_old, t1)
                    val (Cs_2, c_t2) = TokenTable.remove(Cs_1, t2)
		    val result = 
			case compToken (t1,t2)
 		        of LESS    => TokenTable.insert(Cs_2, t1, c_t1 @ c_t2)
                        |  GREATER => TokenTable.insert(Cs_2, t2, c_t1 @ c_t2)
                        |  EQUAL   => (print "Bug: Unexpected equal tokens in different clusters.\n"; 
				       TokenTable.insert(Cs_2, t2, c_t1 @ c_t2))
		    val () = if print_verbose then (printClusters "old" Cs_old; printClusters "new" result) else ()
		in
		     result
		end

	    fun computeInitialDistances freqDist = 
		let fun insertDistance t1 (t2, CD)  = 
		    let fun re(t1,t2) = relativeEntropy (numRecords, valOf(TokenTable.find(freqDist, t1)), 
							             valOf(TokenTable.find(freqDist,t2)))
		    in  case compToken(t1,t2)
			of LESS    => TokenPairTable.insert(CD, (t1,t2), re(t1,t2))
                        |  GREATER => TokenPairTable.insert(CD, (t2,t1), re(t2,t1))
                        |  EQUAL   => CD
       		    end
		    fun insertDistances CD [] = CD
                      | insertDistances CD (t::ts) = 
			let val tCD = List.foldl (insertDistance t) CD ts
			in
			    insertDistances tCD ts
			end
		in
		    insertDistances TokenPairTable.empty distList
		end
	    val CD_init = computeInitialDistances freqDist

	    fun printClusterDistance ((t1,t2),d) = (print "("; printTokenTy t1; 
						print ", "; printTokenTy t2; print "):\t ";
						print (Real.toString d); 
						print "\n")
	    fun printClusterDistances CD = 
		   ( print "Cluster Distances\n";
		     TokenPairTable.appi printClusterDistance CD;
		     print "End Cluster Distances\n")


            fun clusterDistance CD (c1,c2) = 
		case compToken(c1,c2) 
                   of EQUAL    => 0.0
                   |  LESS     => (valOf(TokenPairTable.find(CD, (c1,c2))) 
				   handle NotFound => (print "LESS "; printTokenTy c1; print ", "; printTokenTy c2; 
						       print " not found.\n"; printClusterDistances CD; raise NotFound))
                   |  GREATER  => (valOf(TokenPairTable.find(CD, (c2,c1))) 
				   handle NotFound => (print "GREATER "; printTokenTy c2; 
						       print ", "; printTokenTy c1; print " not found.\n"; raise NotFound))
	    fun minMaxToken(t1,t2) = 
		case compToken(t1,t2) 
                   of EQUAL    => (t1,t2)
                   |  LESS     => (t1,t2)
                   |  GREATER  => (t2,t1)

	    (* Merging t_min and t_max
	       Require ta < tb
	       CD_new (ta,tb) = CD_old(ta,tb) if t_min not in {ta, tb}
               CD_new (ta,tb) = min {clusterDistance CD_old ta t_min, clusterDistance CD_old ta t_max} if tb = t_min
               CD_new (ta,tb) = min {clusterDistance CD_old tb t_min, clusterDistance CD_old tb t_max} if ta = t_min
             *)
            fun updateClusterDistances clusters CD_old t1 t2 = 
		let val () = if print_verbose then print "Starting to update cluster distances\n" else ()
		    val (t_min,t_max) = minMaxToken(t1, t2)  (* know t_min < t_max, so t_min is representative for new cluster *)
		    val tokens = TokenTable.listKeys clusters
		    fun insertDistance ta (tb, CD) =  (* guarantee that ta < tb *)
			if not (eqToken(ta, t_min)) andalso not (eqToken(tb, t_min))
			then TokenPairTable.insert(CD, (ta,tb), clusterDistance CD_old (ta, tb))  (* neither cluster is involved in merge *)
			else if eqToken(ta, t_min) (* a is rep for merge; newD is min of old d from tb to either cluster in merge *)
                             then let val newDistance = Real.min(clusterDistance CD_old (t_min, tb), clusterDistance CD_old(tb, t_max))
				  in
				      TokenPairTable.insert(CD, (ta,tb), newDistance)  
				  end
			      else  (* b is rep for merge; newD is min of old d from a to either cluster in merge *)
				  let val newDistance = Real.min(clusterDistance CD_old (t_min, ta), clusterDistance CD_old(ta, t_max))
				  in
				      TokenPairTable.insert(CD, (ta,tb), newDistance)  
				  end
		    fun insertDistances CD [] = CD
		      | insertDistances CD (t::ts) = 
			let val tCD = List.foldl (insertDistance t) CD ts
			in 
			    insertDistances tCD ts
			end
		in
		    insertDistances TokenPairTable.empty tokens
		end


            fun findMinDistance CD = 
		let val distList = TokenPairTable.listItemsi CD
                    fun getSmallest ( ((t1,t2),d), c as (smallest,(ta,tb))) = if d < smallest then (d,(t1,t2)) else c
                    val c as (smallest,_) = List.foldl getSmallest (1.0,(Pempty, Error)) distList
                    val () = if print_verbose then (print "Smallest cluster distance is: "; print (Real.toString smallest); print "\n") else ()
		in
		    c
		end

	    (* val initSmallestDistance = findMinDistance CD_init *)
	    val threshold = 0.01
            fun loop clusters CD = 
		let val (minDistance,(ta,tb)) = findMinDistance CD
		    val () = if print_verbose then (print "Selected tokens:"; printClusterDistance ((ta,tb),minDistance); print "\n") else ()
		in
		    if minDistance > threshold then (clusters, CD)
		    else let val () = if print_verbose then printClusterDistances CD else ()
			     val newClusters = mergeClusters clusters ta tb
			     val CD_new      = updateClusterDistances newClusters CD ta tb
			     val () = if print_verbose then printClusterDistances CD_new else ()
			 in
			     loop newClusters CD_new 
			 end
		end
	    val (Clusters, CD) = loop Cs_init CD_init
            (* we have a map from tokens to token lists to represent clusters.
               We need to convert this rep to a (Token * histogram) list list *)
            fun convertRep (cluster:Token list TokenTable.map) freqDist = 
		let fun convertOneCluster tlist = List.map (fn t=> (t,valOf(TokenTable.find(freqDist, t)))) tlist
		    fun doOneToken (tlist,clusters) = (convertOneCluster tlist) :: clusters
		in
		    TokenTable.foldl doOneToken [] cluster 
		end
	in  
	    convertRep Clusters freqDist
	end


    (* threshold = user-specified percentage (HIST_PERCENTAGE, -h, 1%) of number of 'records' in context *)
    (* histScoreCmp: compares (<) scores of two histograms, ignores threshold *)
    (* histScoreEq: tests scores of two histograms with fuzzy equality, 
                    is absolute value of difference of scores within threshold? *)
    (* sort histograms by score using histScoreCmp.  
       Compare adjacent histograms for fuzzyEq using histScoreEq, if match, then group *)
    fun findClusters numRecords (freqDist:freqDist) = 
	let val distList = TokenTable.listItemsi(freqDist)
	    val threshold = histEqTolerance numRecords
	    val () = if print_verbose then 
			print ("THRESHOLD for histogram equality: "^(Int.toString threshold)^".\n")
		     else ()
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
	    val () = if print_verbose then print "Computed clusters\n" else ()
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
            (* given the cluster with the lowest score, 
                select each struct-like token from cluster.
                a token is struct-like if the residual mass of its first column is
                  less than specified percentage of context chunks. (struct_percentage, -s, 10%)
                compute the coverage of selected tokens.
                coverage is min {mass(c(t)(1)} for selected tokens t
                we identify a struct if we have at least one selected token and the coverage is higher than specified percentage
                (junk_percentage, -j, default 10%)
            *)
	let fun isStruct (rawClusters) = 
		let fun sortClustersByStructScore rawClusters = 
			let val threshold = histEqTolerance numRecords
			    fun structCmpHist((k1,h1), (k2,h2)) = histScoreCmp threshold (h1, h2)
			    fun sortCluster cluster = ListMergeSort.sort structCmpHist cluster
			    val partClusters = List.map sortCluster rawClusters
			    fun clusterComp ((k1,h1)::_, (k2,h2)::_) = histScoreCmp threshold (h1,h2)
			in
			    List.rev(ListMergeSort.sort clusterComp partClusters)
			end
		    val clusters = sortClustersByStructScore rawClusters
		    val () = if print_verbose then (print "In isStruct, printing clusters sorted by struct criteria.\n";
						    printClusters numRecords clusters) else ()
		    val cluster = hd clusters
		    fun getStructInfo((t, {hist, total, coverage, structScore, width,minOccurrences}), result) =
		    let val () = if print_verbose then (print "Token:\t";  printTokenTy t; print "\n") else ()
			val hList = List.map (fn(x,y)=>(x, !y)) (IntMap.listItemsi (!hist))
			val sortedHlist = ListMergeSort.sort (fn((i1,c1:int),(i2,c2))=>(c1 < c2)) hList
			val primary = List.hd sortedHlist
			fun hIsStruct sortedHlist =
			    let val rest = List.tl sortedHlist
				val massOfRest = List.foldl (fn((i,c:int),acc)=> acc + c) 0 rest
				val () = if print_verbose then (print "massOfRest: "; print (Int.toString massOfRest); print "\n";
								print "structTolerance: "; print (Int.toString (isStructTolerance numRecords)); print "\n";
								print "coverage: "; print (Int.toString (!coverage)); print "\n";
								print "junk Tolerange: "; print (Int.toString (isJunkTolerance numRecords)); print "\n")
				    else ()
			    in
				(massOfRest < (isStructTolerance numRecords))
				andalso
				((!coverage) >= (isJunkTolerance numRecords))
			    end
		    in
			if hIsStruct sortedHlist then  [(t, #1 primary, #2 primary)]@result  else result
		    end

		    fun mergeStructEntries tokenAnalysis =
			let fun doOne ((token,count,coverage),(cumCount, cumCoverage,tlist)) = 
			    (cumCount + count, Int.min(coverage, cumCoverage), (token, count)::tlist)
			    val (numTokens, coverage, tokens) = List.foldl doOne (0, numRecords,[]) tokenAnalysis
			in
			     (if print_verbose then 
			     (print "Junk Tolerance Threshold: "; print (Int.toString(isJunkTolerance numRecords)); print "\n";
			      print "Cluster Coverage: ";                 print (Int.toString(coverage)); print "\n";
			      print "Num Tokens: ";               print (Int.toString(numTokens)); print "\n")
			     else ();
			     if (numTokens > 0)
				 then (SOME (Struct {numTokensInCluster = numTokens, numRecordsWithCluster = coverage, tokens = tokens}))
			     else NONE
				 )
			end
		in 
		    mergeStructEntries (List.foldl getStructInfo [] cluster)
		end

                
	    fun getArrayScore (h:histogram) = 
		if !(#coverage h) < numRecords then (!(#width h) + 1)
		else !(#width h)
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

	    fun with_sep_tok cluster =
		case cluster of
		  nil => false
		| (t, h)::c => (isSeparator t) orelse (with_sep_tok c)

            fun isArray clusters = 
		let 
		    val clusters_with_sep = List.filter with_sep_tok clusters 
		    val sortedClusters = ListMergeSort.sort lessArrayCluster clusters_with_sep
		    val () = if print_verbose then print "Clusters sorted by array criteria:\n" else ()
		    val () = if print_verbose then printClusters numRecords sortedClusters else ()
		    fun getArrayInfo((t, 
			(h as {hist, total, coverage, structScore, width, minOccurrences})), result) = 
		    (if print_verbose then (
		     print "Possible array tokens:\n"; 
		     printTokenTy t; print "\n";
		     print ("Records in possible array context:"^(Int.toString numRecords)^"\n");
                     print ("Array Score: "^(Int.toString (getArrayScore h))^"\n");
                     print ("Total:"^(Int.toString (!total))^"\n");
                     print ("Coverage:"^(Int.toString (!coverage))^"\n");
                     print ("Width:"^(Int.toString (!width))^"\n");
                     print ("minOccurrences:"^(Int.toString (!minOccurrences))^"\n"); 
		     print ("Required coverage is: " ^ 
			Int.toString (numRecords - isJunkTolerance numRecords) ^ "\n"))  
		     else ();
		     if (!width >= !ARRAY_WIDTH_THRESHOLD) andalso 
                        (!minOccurrences >= !ARRAY_MIN_WIDTH_THRESHOLD) andalso
			(!coverage >= numRecords - (isJunkTolerance numRecords))  
			andalso (isSeparator t)
			(*
			andalso 
			(not (isString t))
			*)
			 then (t,1)::result else result)
		    (* we probably want to compute the number of times the token appears in the cluster...*)
		    val arrayTokenAnalysis = 
			case sortedClusters of
			  cluster::_ => List.foldl getArrayInfo [] cluster 
			| _ => []
		in
		    case arrayTokenAnalysis of [] => 
			(if print_verbose then print "ARRAY NOT CHOSEN\n" else (); NONE) | 
			a => SOME(Array {tokens = a})
		end
	    val unionFirst = false
	    fun isUnion clusters = SOME (Union FirstToken)
            val analyses = isStruct ::
		(if unionFirst then [isUnion, isArray] else [isArray, isUnion])

	    fun findFirst arg [] = (if print_verbose then 
					print "Identified a blob\n"
				    else (); Blob)
	      | findFirst arg (f::fs) = case f arg of NONE => findFirst arg fs
		                        | SOME s => s 

	    val kindSummary = 
		case clusters 
                of [] => ((* print "No clusters found\n";*) Empty)
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


	    val () = if print_verbose then printKindSummary kindSummary else ()

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
		let fun introduceLEmpty (contextList:DerivedContexts) =  (*In progress*)
		        (* Idea: input is list of token lists, one per field in discovered struct. 
                                 Some may be empty because nothing was between tokens.
				 We can replace these empty lists with a list of the single token empty.
				 At this point, we know the location of the empty token because it is in the 
				 same input record as all the other tokens in the line.
				 This function is not called with an empty tokenOrder, so the record is
				 guaranteed to have at least one located token we can use to fix the location.*)
		        let fun findRecordNumber [] = (print "XXX: contextList had no located tokens, breaking invariant";  
						       {lineNo= ~200,beginloc=0,endloc=0,recNo= ~201}) 
			                              (* This case should never occur.*)
                              | findRecordNumber ([]::rs) = findRecordNumber rs
			      | findRecordNumber (((t,loc as {lineNo,...})::ts)::rs) = loc
(*			          (print "record number: "; print (Int.toString lineNo); (loc:location) ) *)
										       
			    val location = findRecordNumber contextList
			    fun convert [] result = List.rev result
                              | convert ([]::rest) result = convert rest ([(Pempty,location)]::result)
			      | convert (cur::rest) result = convert rest (cur::result)
			in
			    ((convert contextList []) : DerivedContexts)
			end
		    fun doMatch (tokensToMatch:TokenOrder) (recordTokens: Context)
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
(*		    val () = print "before adding empty\n"
		    val () = printContexts thisRecordContexts *)
		    val recordContextsWithEmpty = introduceLEmpty thisRecordContexts
(*		    val () = print "after adding empty\n"
		    val () = printContexts recordContextsWithEmpty *)
		in
		    SOME recordContextsWithEmpty handle TokenMatchFailure => NONE
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
                 , tycomp = zeroComps
		 , len = 0.0
                 }
               , !Bottomstamp
               , cl
               ) before Bottomstamp := !Bottomstamp + 1

    fun mkEmpty lineNo = [(Pempty,{lineNo= lineNo, beginloc=0, endloc=0, recNo=lineNo})]

    (* Invariant: cl is not an empty column: checked before mkTBD is called with isEmpty function *)
    (* coverage is number of records in this context *)
    fun mkTBD (callsite, currentDepth, coverage, cl) = 
        (* Columns that have some empty rows must have the empty list representation
           of the empty row converted to the [Pempty] token.  Otherwise, a column
           that is either empty or some value gets silently converted to the value only. *)
	let fun cnvEmptyRowsToPempty [] = [(Pempty,{lineNo= callsite, beginloc=0, endloc=0, recNo=callsite})] (* XXX fix line number *)
              | cnvEmptyRowsToPempty l  = l
	    val cl = List.map cnvEmptyRowsToPempty cl
            fun allEmpty cl =
		let fun isNonEmpty [(Pempty,_)] = false
		      | isNonEmpty _ = true
		in
		    ((*print "Checking for an allempty context\n";*)
		    not(Option.isSome(List.find isNonEmpty cl))
		     )
		end
	    val cl = crackUniformGroups cl
	in
	    if allEmpty cl then Base(mkTyAux coverage, List.concat cl)
	    else if (coverage < isNoiseTolerance(!initialRecordCount))
		 then mkBottom(coverage,cl)  (* not enough data here to be worth the trouble...*)
	    else if (currentDepth >= !depthLimit)  (* we've gone far enough...*)
                 then TBD ( { coverage=coverage
                            , label=SOME(mkTBDLabel (!TBDstamp))
                            , tycomp = zeroComps
			    , len = 0.0
                            }
                          , !TBDstamp
                          , cl
                          ) before TBDstamp := !TBDstamp    + 1
                 else ContextListToTy (currentDepth + 1) cl
	end

    and clustersToTy curDepth rtokens numRecords clusters = 
	let val analysis = analyzeClusters numRecords clusters
	    val _ = if output_histograms then 
			dumpClusters curDepth numRecords clusters ((!outputDir) ^ "histogram.dat")
		    else ()
            (* This function partitions a context into a union. *)
            fun orig_buildUnionTy (FirstToken, rtokens) = 
		let (*val () = print "BUILDING UNION TY\n"*)
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
		    fun allEmpty () = Base(mkTyAux numRecords, [(Pempty,{lineNo= ~1, beginloc=0, endloc=0, recNo= ~1})])
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

            fun buildUnionTy(FirstToken,rtokens) = case (!useUnionClustering ) 
              of SOME threshold => 
                   let val () = print "Starting to look for union clusters.\n"
                       val branches = CU.findClusters threshold rtokens
                       val () = print "Finished looking for union clusters.\n"
		   in
		      if (length branches) = 1 then orig_buildUnionTy(FirstToken,rtokens)
                      else let val tys = List.map (fn branch => mkTBD(~12, curDepth, List.length branch, branch)) branches
			   in
			       Punion(mkTyAux numRecords, tys)
			   end
		   end
  	     | NONE =>  orig_buildUnionTy(FirstToken,rtokens)

           (* This function takes a Struct Partition and returns a Ty describing that partition *)
	    fun buildStructTy partitions = 
		let val (matches, badRecords) = partitions
		    fun isEmpty column = not (List.exists (not o null) column)
		    fun cnvOneMatch (tokenOrder, dclist) = 
			let val columns = doTranspose dclist
			    fun recurse(columns, tokens) result =
				case (columns, tokens) 
				  of ([],[])      => raise Fail "token and column numbers didn't match (2)"
				  |  ([last], []) => List.rev (if isEmpty last then result else ((mkTBD  (~100, curDepth,(List.length last), last)) :: result))
				  |  ([], tks)    => raise Fail "token and column numbers didn't match (3)"
				  |  (col1::coltk::cols, tk::tks) => 
				       (* col1 is context before tk;
					  colt is context corresponding to tk
					  cols is list of contexts following *)
					let (* This function should be able to be removed 
					    fun borrowLoc col1 colref = 
					        let fun doit ([],[] : LToken list list) (a : LToken list list) = List.rev a
						    |   doit ([]::r, [(t,{lineNo,beginloc,endloc, recNo})]::s) a = 
						                 doit (r,s) ([(Pempty,{lineNo=lineNo, beginloc=0,
									       endloc=beginloc,recNo=recNo})]::a)
						    |   doit (r::rs,t::ts) a = doit (rs,ts) (r ::a)
						in
						    doit (col1, colref) []
						end*)
					    val coverage1 = List.length col1
					    val coveraget = List.length coltk
					    val col1Ty = if isEmpty col1 then  [] else [(mkTBD (~8, curDepth, coverage1, col1 (*(borrowLoc col1 coltk)*)))]
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
					   ((*print "converting false struct into union\n";*) buildUnionTy(FirstToken, brs))
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
		    val recIndex = ref 0
		    fun partitionOneRecord tlist = 
		        let fun insertOne ((token,freq),tTable) = TokenTable.insert(tTable, token, ref freq)
			    val tTable = List.foldl insertOne TokenTable.empty atokens
			    val numFound = ref 0
			    fun resetTable () = 
				let fun setOne(token,freq) = 
				    let val freqRef = valOf (TokenTable.find(tTable,token))
				    in
					freqRef := freq
				    end
				in
				    List.app setOne atokens;
				    numFound := 0
				end
			    fun getLineNo [] = (print "WARNING: ARRAY completely empty!"; ~1)
			      | getLineNo ((tok,loc:location)::ltocs) = #lineNo loc
			    val lineNo = getLineNo tlist

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
				     fun cvtEmpty [] =  mkEmpty lineNo
				       | cvtEmpty l = l
				 in
				     ((length, !recIndex), cvtEmpty first, List.map cvtEmpty main, cvtEmpty(List.rev current)) 
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
			let fun pR [] (numTokenA, firstA, mainA,lastA) = 
					(List.rev numTokenA, List.rev firstA, List.rev mainA, List.rev lastA)
                              | pR (t::ts) (numTokenA, firstA, mainA, lastA) = 
			            let 
					val (numTokens, first, main,last) = partitionOneRecord t 
				        val _ = recIndex := (!recIndex)+1
				    in 
					pR ts (numTokens::numTokenA, first::firstA, main@mainA, last::lastA)
				    end
			in
			    pR rtokens ([],[],[],[])
			end

		    val (arrayLengths, firstContext,mainContext,lastContext) = partitionRecords rtokens
		    fun pushRecNo contexts index=
		    	let 
				fun f ltoken index =
					case ltoken of 
					 (t, {lineNo, beginloc, endloc, recNo}) => 
						(t, {lineNo=lineNo, beginloc=beginloc, endloc=endloc, 
							recNo=index})
				fun pushRecNo' tl index =
(*
				  let
		    			val _ = print ("Printing main context: ("^(Int.toString index)^")\n" ^ LTokensToString tl) 
				  in
*)
					case tl of
					  t :: rest => 
					  (
					    case t of (Pgroup {left=lt, body=bts, right=rt}, loc) =>
					        (f (Pgroup {left=(f lt index), body=pushRecNo' bts index,
						    right= (f rt index)}, loc) index)::
						    (pushRecNo' rest index)
					    | _ => (f t index)::(pushRecNo' rest index)
					  )
					| nil => nil
(*
				  end
*)
			in 
				case contexts of 
				  c::rest => (pushRecNo' c index) :: pushRecNo rest (index+1)
				  | nil => nil
			end
(*
		    val _ = print ("Before pushing contexts:\n"^(contextsToString mainContext))
*)
		    val mainContext = pushRecNo mainContext 0
(*
		    val _ = print ("After pushing contexts:\n"^(contextsToString mainContext))
*)
		in
		    ((if print_verbose then 
		     (print "Array context\n"; 
                     print "First Context:\n";
                     print (contextsToString firstContext);
                     print "body Context:\n";
                     print (contextsToString mainContext);
                     print "last Context:\n";
                     print (contextsToString lastContext))
		     else ()); 
		     Parray (mkTyAux numRecords, 
			     {tokens  = atokens, 
			      lengths = arrayLengths,
			      first   = mkTBD(~500, curDepth, List.length firstContext, firstContext),
			      body    = mkTBD(~600, curDepth, List.length mainContext, mainContext),
			      last    = mkTBD(~700, curDepth, List.length lastContext, lastContext)}))
		end


            val ty = case analysis 
		     of Blob =>     mkBottom (List.length rtokens, rtokens)
		     |  Empty =>    Base (mkTyAux 0, [(Pempty,{lineNo= ~1, beginloc=0, endloc=0, recNo= ~1})])
		     |  Struct s => buildStructTy (splitRecords s rtokens) (* Can produce union of structs *)
		     |  Array a =>  buildArrayTy (a, rtokens)
                     |  Union u =>  buildUnionTy(u, rtokens)
	in
	    ty
	end

    and ContextListToTy curDepth context = 
	let val numRecordsinContext = List.length context
(*	    val _ = if print_verbose then 
		print ("Number records being considered: "^Int.toString(numRecordsinContext)^
			"\nThe records are:\n" ^(contextsToString context)) 
		else () *)
(*            val () = print ("KSF: Number records being considered: "^Int.toString(numRecordsinContext)^
			"\nThe records are:\n" ^(contextsToString context))  *)
            val counts : RecordCount list = List.map countFreqs context
	    val fd: freqDist = buildHistograms numRecordsinContext counts
	    (* val clusters : (Token * histogram) list list = findClusters numRecordsinContext fd *)
	    (* val () = if print_verbose then printClusters numRecordsinContext clusters else () *)
	    val clusters : (Token * histogram) list list = newFindClusters numRecordsinContext fd
            val ty = clustersToTy curDepth context numRecordsinContext clusters
	    (*
	    val () = (print "Inferred type:\n"; 
		      print (TyToString ty);
		      print "\n")
	    *)
	in
	    ty
	end

(* if filenames contains just one file, the whole file is the data source which
contains multiple records; if filenames contains more than one file, every
file is a record and all of them collectively represent a sample data *)
    fun computeStructure fileNames = 
	let val recordNumber = ref 0
	    val () = print ("Starting on files "^(lconcat fileNames)^"\n");
	    val records = loadFiles fileNames
	    val () = initialRecordCount := (List.length records) 
	    val rtokens : Context list = List.map (ltokenizeRecord recordNumber) records
	    val separator = getSeparator rtokens  (* for format program: returns some character not in input source *)
	    (* val _ = print (contextsToString rtokens) *) 
            val rtokens = crackUniformGroups rtokens (* check if all records have same top level group token *)
	    val () = if print_verbose = true then lengthsToHist rtokens else ()
	    val ty = ContextListToTy 0 rtokens
	    val sty = simplifyTy ty
	in
	    (sty, separator)
	end

(* this is another version of the above computeStructure function with records as input *)
  fun computeStructurefromRecords records =
	let
	    val recordNumber = ref 0
	    val () = initialRecordCount := (List.length records) 
	    val rtokens : Context list = List.map (ltokenizeRecord recordNumber) records
	    val separator = getSeparator rtokens  (* for format program: returns some character not in input source *)
	    (* val _ = print (contextsToString rtokens)  *)
            val rtokens = crackUniformGroups rtokens (* check if all records have same top level group token *)
	    val () = if print_verbose = true then lengthsToHist rtokens else ()
	    val ty = ContextListToTy 0 rtokens
	    val sty = simplifyTy ty
	in
	    (sty, separator)
	end

end
