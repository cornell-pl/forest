structure Probstruct = 
struct
    open Config
    open Utils
    open Types
    open Options
    open Regexparser
    open Basetokens
    open Probability
    open Evaluate_hmm
    open Fvector
    open Common
    structure SS = Substring

    val TBDstamp = ref 0
    val Bottomstamp = ref 0
    val initialRecordCount = ref ~1  (* will be set at beginning of program based on input file *)    

    (* delimiter tokens *)
    val groupOps : (BSToken * (BSToken -> BSToken)) list = 
	 [((PPpunc "\"", "\""), fn t => t),   
	  ((PPpunc "[", "["), fn t => (PPpunc "]", "]")),  
	  ((PPpunc "(", "("),  fn t => (PPpunc ")", ")")),  
	  ((PPpunc "{", "{"),  fn t => (PPpunc "}", "}")),
	  ((PPbXML, ""), fn t => 
	                     (case t 
			      of (*PbXML(tag,args)=> PeXML(tag,"")*) (PPbXML, s1) => (PPeXML, s1) (*should be some substring *)
			      |  _ => raise Fail "Unexpected beginning tag.\n"))]

    val delimiterCandidates : BSToken list = 
	   let val candidateChars = ["|", ",", ";", "!", "&", ":", "^", "%", "$", "#", "~"] 
	   in
	       List.map (fn x=> (PPpunc x, x)) candidateChars
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


   fun BTokenEq (t1, t2) = compBToken (t1, t2) = EQUAL

   fun BSTokenEq (t1, t2) = compBSToken (t1, t2) = EQUAL


(*
   structure IntMap = RedBlackMapFn(
                     struct type ord_key = int
			    val compare = Int.compare
		     end) 
*)
   type BRecordCount = (int ref) BSTokenTable.map

   val tokenCount : BRecordCount ref = ref BSTokenTable.empty

   (* return the token->count table *)
   fun countBSToken counts t = 
       case BSTokenTable.find (counts, t)
       of NONE =>  BSTokenTable.insert(counts, t, ref 1)
       |  SOME iref => (iref := !iref + 1; counts)

   fun countBSTokensPerRecord counts [] = counts
     | countBSTokensPerRecord counts ((bt,s,loc)::ts) = countBSTokensPerRecord (countBSToken counts bt) ts

   fun countBSTokens counts [] = counts
     | countBSTokens counts ((ts,f)::tts) = countBSTokens (countBSTokensPerRecord counts ts) tts

   fun updateBSTokenCounts tokens = tokenCount := countBSTokens (!tokenCount) tokens

   fun getSeparator rtokens = 
       let val () = updateBSTokenCounts rtokens
	   fun tokenInFile t = not(Option.isSome (BSTokenTable.find (!tokenCount,t)))
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
   type freqDist = histogram BSTokenTable.map
   type cluster  = freqDist list

   datatype unionDiv = FirstBSToken
   datatype Kind = Struct of {numTokensInCluster:int, numRecordsWithCluster:int,
			      tokens: (BSToken * int) list}
                 | Array of {tokens: (BSToken * int) list}
                 | Union of unionDiv
                 | Blob | Empty

   (* printing *)
    fun printFreq (token,value) = (printBSTokenTy token; print ":\t"; print (Int.toString (!value)); print "\n" )
    fun printFreqs counts = (BSTokenTable.appi printFreq counts; print "\n")

    fun printOneFreq numRecords (int, countRef) = 
	let val percent = (Real.fromInt (!countRef ))/(Real.fromInt numRecords)
	in
	    (print "\t"; print (Int.toString int); print ":\t"; print(Int.toString(!countRef)); 
	     print "\t"; print (Real.toString percent); ( print "\n"))
	end

    fun printAugHist numRecords (token, (h:histogram)) = 
	(print "Token: "; 
	 printBSTokenTy token; print "\n"; 
         print ("Total number of token occurrences: "^(Int.toString (!(#total h))^".\n"));
         print ("Number of records with at least one token occurrence: "^(Int.toString (!(#coverage h)))^".\n");
         print ("StructScore: "^(Int.toString (!(#structScore h))^".\n"));
         print ("Minimum number of *occurrences* of token: "^(Int.toString(!(#minOccurrences h)))^".\n");
	 IntMap.appi (printOneFreq numRecords) (!(#hist h)); print "\n")
    
    fun printDist numRecords freqs = (print "Distributions:\n"; BSTokenTable.appi (printAugHist numRecords) freqs; print "\n")

    fun printKindSummary kind = 
	case kind 
        of Struct {numTokensInCluster=count, numRecordsWithCluster=coverage, tokens=tinfos} =>
	    let fun printOne (t,count) =
		(printBSTokenTy t; print "\t";
		 print "Occurrences:"; print (Int.toString count);  print "\n")
	    in
		(print "Struct"; print "\n";
		 print "Coverage:";    print (Int.toString coverage);  print "\n";
		 print "Token count:"; print (Int.toString count);     print "\n";
		 List.app printOne tinfos)
	    end
      | Array {tokens} =>
	    let fun printOne (t,count) =
		(printBSTokenTy t; print "\t";
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

    fun dumpClusters curDeps numRecords clusters fileName = 
       let fun oneFreqToStr numRecords (int, countRef) = 
	     let val percent = (Real.fromInt (!countRef ))/(Real.fromInt numRecords)
	     in
	     ("\t" ^ (Int.toString int) ^ ":\t" ^ (Int.toString(!countRef)) ^
	     "\t" ^ (Real.toString percent) ^ "\n")
	     end
           fun histToString numRecords (token, (h:histogram)) = 
	       	("Token: " ^ (bstokenTyToString token) ^ "\n"  ^
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
			    List.app (fn t => (printBSTokenTy t; print " ")) tList;
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

   fun simplifyNewTy ty = 
       let fun collapseStruct [] a = a
	     | collapseStruct ((PPstruct (aux,tys))::tysRest) a = collapseStruct tysRest (a @ (collapseStruct tys []))
	     | collapseStruct (ty::tysRest) a = collapseStruct tysRest (a @ [simplifyNewTy ty])
	   fun collapseUnion [] a = a
	     | collapseUnion ((PPunion (aux,tys))::tysRest) a = collapseUnion tysRest (a @ (collapseUnion tys []))
	     | collapseUnion (ty::tysRest) a = collapseUnion tysRest (a @ [simplifyNewTy ty])
       in
	   case ty 
	   of PPstruct (aux,tys) => PPstruct (aux, collapseStruct tys [])
           |  PPunion  (aux,tys) => PPunion  (aux, collapseUnion  tys [])
           |  PParray  (aux,{tokens=tkns,lengths,first=ty1,body=ty2,last=ty3}) => 
		     PParray  (aux, {tokens=tkns,lengths=lengths, first=simplifyNewTy ty1, body=simplifyNewTy ty2, last=simplifyNewTy ty3})
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

(*
    fun isString (PPstring, s) = true
      | isString _ = false
*)
    (*given a list of LTokens, find groupings in them and return the list of groups, or if no groupings are found,
	return the original list *)
    fun findGroups (tokens : BSLToken list) : BSLToken list = tokens(*
	let fun TokenMatch (t1,t2) = case (t1,t2) 
	                             of ((PPbXML, _), (PPbXML, _)) => (print "looking for a ppbxml token\n"; true) (* begin xml tag in group list has placeholder tag, so any bXML matches *)
				     | _ => BSTokenEq(t1,t2)
	    fun findDelim (t, st, loc) = List.find (fn(f,s) => TokenMatch ((t,st),f)) groupOps
            fun delimMatches r (t,loc) = BSTokenEq(t, r)
	    fun flatten [] = ((*print "in flatten function\n";*) [])
              | flatten ((ltoken,body,r)::rest) = (ltoken :: (List.rev body)) @ (flatten rest) 
            fun getGroupLoc ((lt, {lineNo=llineNo, beginloc=lbegin, endloc=lend, recNo=lrecNo}), 
			     (rt, {lineNo,         beginloc=rbegin, endloc=rend, recNo=rrecNo})) =
		{lineNo=llineNo, beginloc=lbegin, endloc=rend, recNo=lrecNo}
            fun topSearch [] acc = List.rev acc
              | topSearch (t::ts) acc = case findDelim t 
		                        of NONE => topSearch ts (t::acc)
					|  SOME (lt:BSToken,rtf:BSToken->BSToken) => findMatch [(t,[],rtf (#1 t))] ts acc

            and findMatch  (delims:((BToken*string*location) * (BToken*string*location) list * BSToken) list) [] acc = (List.rev acc)@(flatten delims) (* missing right delim: forget grouping *)
              | findMatch ((lt:(BToken*string*location),body:(BToken*string*location) list,r:BSToken)::next) (t::ts) acc = 
		  if delimMatches r t 
		  then let val match =  (PPgroup{left=lt, body = List.rev body, right= t}, getGroupLoc(lt,t))
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
*)
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

    (* This function takes a basetoken sequence and returns an (int ref) BTokenTable.map *)
    (* This map, keyed by the basetoken, stores the count of occurences of the token key in the input list *)
    fun countBFreqs (tokens:NewContext) : BRecordCount = 
	let fun doToken counts t = 
		case BSTokenTable.find (counts, t)
	        of NONE =>  BSTokenTable.insert(counts, t, ref 1)
              |SOME iref => (iref := !iref + 1; counts)
	    fun doTokens counts [] = counts
	      | doTokens counts (((bt,s),loc)::ts) = doTokens (doToken counts (bt,s)) ts
        val counts = doTokens BSTokenTable.empty tokens
(*	    val () = printFreqs counts *)
	in
        counts
	end
  
    (* hs: maps basetokens to histograms (which map ints to int refs) *)
    (* counts: maps tokens to int refs *)
    fun buildHistograms numRecords (countslist : BRecordCount list)= 
	let val () = if print_verbose then print "Building histograms...\n" else ()
	    fun doOneRecord (fd : freqDist) (counts:BRecordCount) = 
        let fun doOneToken (token,count,fd) : freqDist = 
		    case BSTokenTable.find(fd,token)
		    of NONE => BSTokenTable.insert(fd, token, mkHistogram (!count))
                    |  SOME ({hist, total, coverage, ...}: histogram)  => 
		                 ((case IntMap.find(!hist, !count) of 
                                NONE      => hist := IntMap.insert(!hist, !count, ref 1) 
		                     |  SOME cref => cref := !cref + 1);
			               total := !total + !count;
			               coverage := !coverage + 1;
			               fd)
		in
		    (BSTokenTable.foldli doOneToken fd counts : freqDist)
		end
	    fun doAllRecords (fd:freqDist) [] = fd
          | doAllRecords fd (c::cs) = doAllRecords (doOneRecord fd c) cs
        val freqs : freqDist = doAllRecords BSTokenTable.empty countslist
	    fun scoreHistogram (h as {hist, total, coverage, structScore, width, minOccurrences} : histogram) = 
		  let val (s,w, minOcc) = histScore numRecords h
		  in
		    structScore := s;
		    width := w;
		    minOccurrences := minOcc
		  end
	    val () = BSTokenTable.app scoreHistogram freqs
	in
	    freqs
	end


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
       
    fun BSTokenPairComp ((t1a,t1b), (t2a,t2b)) = 
	let val r1 = compBSToken (t1a, t2a)
	in
	    if  r1 = EQUAL then compBSToken (t1b, t2b) else r1
	end

    structure BSTokenPairTable = RedBlackMapFn(
                     struct type ord_key = BSToken * BSToken
			    val compare = BSTokenPairComp
		     end) 

    fun newFindClusters numRecords (freqDist:freqDist) = 
	let val distList = BSTokenTable.listKeys(freqDist)
            fun buildInitialClusters distList = 
		let fun doOne (token, clusters) = BSTokenTable.insert(clusters,token, [token])
		in List.foldl doOne BSTokenTable.empty distList end

	    val Cs_init : (BSToken list) BSTokenTable.map = buildInitialClusters distList
	    fun printClusters s clusters = 
		let fun doOne (t,tlist) = (print "Rep token: "; printBSTokenTy t; print "\n"; printTList tlist)
		in
		    (print s; print "\n";
		    BSTokenTable.appi doOne clusters;
		     print "\n")
		end
	    val () = if print_verbose then 
			(printClusters "Initial clusters: " Cs_init) 
			else ()
            fun mergeClusters Cs_old t1 t2 = 
		let val () = if print_verbose then (print "Merging clusters: "; printBSTokenTy t1; print " "; printBSTokenTy t2; print "\n") else ()
		    val (Cs_1, c_t1) = BSTokenTable.remove(Cs_old, t1)
                    val (Cs_2, c_t2) = BSTokenTable.remove(Cs_1, t2)
		    val result = 
			case compBSToken (t1,t2)
 		        of LESS    => BSTokenTable.insert(Cs_2, t1, c_t1 @ c_t2)
                        |  GREATER => BSTokenTable.insert(Cs_2, t2, c_t1 @ c_t2)
                        |  EQUAL   => (print "Bug: Unexpected equal tokens in different clusters.\n"; 
				       BSTokenTable.insert(Cs_2, t2, c_t1 @ c_t2))
		    val () = if print_verbose then (printClusters "old" Cs_old; printClusters "new" result) else ()
		in
		     result
		end

	    fun computeInitialDistances freqDist = 
		let fun insertDistance t1 (t2, CD)  = 
		    let fun re(t1,t2) = relativeEntropy (numRecords, valOf(BSTokenTable.find(freqDist, t1)), 
							             valOf(BSTokenTable.find(freqDist,t2)))
		    in  case compBSToken(t1,t2)
			of LESS    => BSTokenPairTable.insert(CD, (t1,t2), re(t1,t2))
                        |  GREATER => BSTokenPairTable.insert(CD, (t2,t1), re(t2,t1))
                        |  EQUAL   => CD
       		    end
		    fun insertDistances CD [] = CD
                      | insertDistances CD (t::ts) = 
			let val tCD = List.foldl (insertDistance t) CD ts
			in
			    insertDistances tCD ts
			end
		in
		    insertDistances BSTokenPairTable.empty distList
		end
	    val CD_init = computeInitialDistances freqDist

	    fun printClusterDistance ((t1,t2),d) = (print "("; printBSTokenTy t1; 
						print ", "; printBSTokenTy t2; print "):\t ";
						print (Real.toString d); 
						print "\n")
	    fun printClusterDistances CD = 
		   ( print "Cluster Distances\n";
		     BSTokenPairTable.appi printClusterDistance CD;
		     print "End Cluster Distances\n")


            fun clusterDistance CD (c1,c2) = 
		case compBSToken(c1,c2) 
                   of EQUAL    => 0.0
                   |  LESS     => (valOf(BSTokenPairTable.find(CD, (c1,c2))) 
				   handle NotFound => (print "LESS "; printBSTokenTy c1; print ", "; printBSTokenTy c2; 
						       print " not found.\n"; printClusterDistances CD; raise NotFound))
                   |  GREATER  => (valOf(BSTokenPairTable.find(CD, (c2,c1))) 
				   handle NotFound => (print "GREATER "; printBSTokenTy c2; 
						       print ", "; printBSTokenTy c1; print " not found.\n"; raise NotFound))
	    fun minMaxToken(t1,t2) = 
		case compBSToken(t1,t2) 
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
		    val tokens = BSTokenTable.listKeys clusters
		    fun insertDistance ta (tb, CD) =  (* guarantee that ta < tb *)
			if not (eqBSToken(ta, t_min)) andalso not (eqBSToken(tb, t_min))
			then BSTokenPairTable.insert(CD, (ta,tb), clusterDistance CD_old (ta, tb))  (* neither cluster is involved in merge *)
			else if eqBSToken(ta, t_min) (* a is rep for merge; newD is min of old d from tb to either cluster in merge *)
                             then let val newDistance = Real.min(clusterDistance CD_old (t_min, tb), clusterDistance CD_old(tb, t_max))
				  in
				      BSTokenPairTable.insert(CD, (ta,tb), newDistance)  
				  end
			      else  (* b is rep for merge; newD is min of old d from a to either cluster in merge *)
				  let val newDistance = Real.min(clusterDistance CD_old (t_min, ta), clusterDistance CD_old(ta, t_max))
				  in
				      BSTokenPairTable.insert(CD, (ta,tb), newDistance)  
				  end
		    fun insertDistances CD [] = CD
		      | insertDistances CD (t::ts) = 
			let val tCD = List.foldl (insertDistance t) CD ts
			in 
			    insertDistances tCD ts
			end
		in
		    insertDistances BSTokenPairTable.empty tokens
		end


        fun findMinDistance CD = 
		  let val distList = BSTokenPairTable.listItemsi CD
              fun getSmallest ( ((t1,t2),d), c as (smallest,(ta,tb))) = if d < smallest then (d,(t1,t2)) else c
              val c as (smallest,_) = List.foldl getSmallest (1.0,((PPempty, ""), (PPError, ""))) distList
              val () = if print_verbose then (print "Smallest cluster distance is: "; print (Real.toString smallest); print "\n") else ()
		  in
		    c
		  end

	    val initSmallestDistance = findMinDistance CD_init
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
            fun convertRep (cluster:BSToken list BSTokenTable.map) freqDist = 
		let fun convertOneCluster tlist = List.map (fn t=> (t,valOf(BSTokenTable.find(freqDist, t)))) tlist
		    fun doOneToken (tlist,clusters) = (convertOneCluster tlist) :: clusters
		in
		    BSTokenTable.foldl doOneToken [] cluster 
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
	let val distList = BSTokenTable.listItemsi(freqDist)
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
    fun analyzeClusters numRecords (clusters : (BSToken * histogram) list list) = 
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
		    let val hList = List.map (fn(x,y)=>(x, !y)) (IntMap.listItemsi (!hist))
			val sortedHlist = ListMergeSort.sort (fn((i1,c1:int),(i2,c2))=>(c1 < c2)) hList
			val primary = List.hd sortedHlist
			fun hIsStruct sortedHlist =
			    let val rest = List.tl sortedHlist
				val massOfRest = List.foldl (fn((i,c:int),acc)=> acc + c) 0 rest
			    in
				(massOfRest < (isStructTolerance numRecords))
				andalso
				((!coverage) >= (isJunkTolerance numRecords))
			    end
		    in
			if hIsStruct sortedHlist then [(t, #1 primary, #2 primary)]@result  else result
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

                
	    fun getArrayScore (h:histogram) = !(#coverage h)

	    (* For tokens with fuzzy-equal array scores, gives preference to punctuation tokens according
             * to token order, ie, "other" characters, and then white space, etc. *)
            fun lessArrayHist((t1,h1),(t2,h2)) = 
		let val as1 = getArrayScore h1
		    val as2 = getArrayScore h2
		    fun tknCompToBool cmp = case cmp of LESS => true | _ => false
		in
		    if Int.abs(as1 - as2) < (histEqTolerance numRecords) then tknCompToBool(compBSToken(t1,t2))
		    else as1 < as2
		end

            fun lessArrayCluster (c1,c2) = 
		case (c1,c2) 
                of ([],_) => true
                |  (_,[]) => false
                |  (th1::_, th2::_) => lessArrayHist(th1,th2)

            fun isArray clusters = 
		let val sortedClusters = ListMergeSort.sort lessArrayCluster clusters
		    val () = if print_verbose then print "Clusters sorted by array criteria:\n" else ()
		    val () = if print_verbose then printClusters numRecords sortedClusters else ()
		    val cluster = List.hd sortedClusters (* guaranteed by earlier check not to be [] *)
		    fun getArrayInfo((t, {hist, total, coverage, structScore, width, minOccurrences}), result) = 
		    (if print_verbose then (
		     print "Possible array tokens:\n"; 
		     printBSTokenTy t; print "\n";
		     print ("Records in possible array context:"^(Int.toString numRecords)^"\n");
                     print ("Total:"^(Int.toString (!total))^"\n");
                     print ("Coverage:"^(Int.toString (!coverage))^"\n");
                     print ("Width:"^(Int.toString (!width))^"\n");
                     print ("minOccurrences:"^(Int.toString (!minOccurrences))^"\n")) 
		     else ();
		     if (!width >= !ARRAY_WIDTH_THRESHOLD) andalso 
                        (!minOccurrences >= !ARRAY_MIN_WIDTH_THRESHOLD) andalso
			(!coverage > numRecords - (isJunkTolerance numRecords))  (*andalso
                        (not (isString t))*)
			 then (t,1)::result else result)
		    (* we probably want to compute the number of times the token appears in the cluster...*)
		    val arrayTokenAnalysis = List.foldl getArrayInfo [] cluster 
		in
		    case arrayTokenAnalysis of [] => ((*print "ARRAY NOT CHOSEN\n";*) NONE) | a => SOME(Array {tokens = a})
		end
	    val unionFirst = false
	    fun isUnion clusters = SOME (Union FirstBSToken)
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

    exception ViterbiExit

    exception StructError

    fun splitRecords summary (records : NewContext list ) ( seqsetl : Seqset list ) tables : NPartition=

	  let 
        val {numTokensInCluster=count, numRecordsWithCluster=coverage, tokens=tokenfreqs} = summary

	    fun getTokenOrder summary record = 
	        let 
              fun insertOne ((token,freq),tTable) = BSTokenTable.insert(tTable, token, ref freq)
		      val tTable = List.foldl insertOne BSTokenTable.empty summary
		      val numFound = ref 0
		      fun doOneToken tTable ((token,loc), acc) = 
			    case BSTokenTable.find(tTable, token)of 
                     NONE => acc
			      |  SOME freq => 
			           if !freq = 0 then (print "raise here\n"; raise TokenMatchFailure)
			           else (freq := !freq - 1;
				            numFound := !numFound + 1;
				            token::acc)
		      val tList = List.rev(List.foldl (doOneToken tTable) [] record)
		      val () = if not ((!numFound) = count) 
			     then (print "raise here\n"; raise TokenMatchFailure)
			     else ()
		in
		    (print "find a token order\n"; SOME(tList))  handle TokenMatchFailure => (print "handle here\n"; NONE)
		end

        fun findTokenOrder1 summary record = 
          case (getTokenOrder summary record) of
              NONE => (NONE, NONE)
            | SOME t => (SOME(t), NONE)


        fun findTokenOrder2 summary record =
        let
          val tag = getTokenOrder summary record handle TokenMatchFailure => NONE
        in 
          case tag of
              NONE => let
val _ = print "find one without the token order\n"
                        val ((b, s), loc) = List.nth(record, 0)
                        val {lineNo=l, beginloc=bgl, endloc=enl, recNo=recn} = loc
                        val thisSeqset = getSeqset l seqsetl
(*
                        fun testOrder (ts: Tokenseq) = let val (c, f) = ts in
                                                         case getTokenOrder summary c of
                                                             NONE => false
                                                           | SOME(t) => true
                                                       end
                        val validSeqset = List.filter testOrder thisSeqset
                        val ret = case validSeqset of
                                        [] => (NONE, NONE)  (* this is a truly bad record *)
                                       | _ => let val (c, f) = getMax validSeqset in
                                                (getTokenOrder summary c, SOME c)
                                              end
*)
                        val ret = case (ViterbiWithSummary thisSeqset tables summary handle ViterbiError => (print "ViterbiExit\n"; raise ViterbiExit)) of
                                      NONE => (print "still no\n"; (NONE, NONE))
                                    | SOME c => (print "get one new\n"; print (BSLTokensToString c); (getTokenOrder summary c, SOME c))
                      in
                        ret
                      end
            | SOME t => (SOME(t), NONE)
        end

	    fun classifyOneRecordWithMatch (thisRecord:NewContext) (tokenOrder:NTokenOrder)  = 
		let fun introduceLEmpty (contextList:NDerivedContexts) =  (*In progress*)
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
                              | convert ([]::rest) result = convert rest ([((PPempty,""),location)]::result)
			      | convert (cur::rest) result = convert rest (cur::result)
			in
			    ((convert contextList []) : NDerivedContexts)
			end
		    fun doMatch (tokensToMatch:NTokenOrder) (recordTokens: NewContext)
		                (curContextAcc : NewContext, contextListAcc : NDerivedContexts) = 
		    case (tokensToMatch, recordTokens) 
		    of ([],[])   => List.rev ((List.rev curContextAcc) :: contextListAcc)
                    |  ([], rts) => List.rev  (rts :: contextListAcc)
                    |  (tks, []) => (print "raise here\n"; raise TokenMatchFailure)
                    |  (tokens as tk::tks, (lrtoken as (rt,loc))::rts) => 
			let val (rt, loc) = lrtoken : BSLToken
			in
			  if BSTokenEq(tk, rt)  (* found next token; push current context *)
			  then 
			      let val matchTokens = [lrtoken](*
				  case rt 
				  of Pgroup g => (groupToTokens g)
				  |  _        => [lrtoken]*)
			      in
				  doMatch tks rts ([], matchTokens :: (List.rev curContextAcc) :: contextListAcc)
			      end
			  else doMatch tokens rts (lrtoken :: curContextAcc, contextListAcc)
			end
            val tag = ref 0
		    val thisRecordContexts = doMatch tokenOrder thisRecord ([],[]) handle TokenMatchFailure => (print "handle here\n"; tag := 1; [])
(*		    val () = print "before adding empty\n"
		    val () = printContexts thisRecordContexts *)
(*		    val recordContextsWithEmpty = introduceLEmpty thisRecordContexts*)
(*		    val () = print "after adding empty\n"
		    val () = printContexts recordContextsWithEmpty *)
		in
          if !tag = 0 then
		    SOME (*recordContextsWithEmpty*)(introduceLEmpty thisRecordContexts) (* handle TokenMatchFailure => (print "handle here\n"; NONE)*)
          else NONE
		end

            (* Given a summary, a token order * DerivedContexts list, and a record,
                try each token order in list.  
                if matches, add to corresponding DerivedContexts list.
                if doesn't match, try to infer a different tokenOrder.
                  if matches, add to list of token orders with corresonding derivedContext
                  if doesn't match, add to bad record list *)
	    fun classifyOneRecord tokenfreqs (thisRecord, (matches, badRecords)) = (* thisRecord is NewContext *) 
		let (* convert to accumulator form? *)
            val tag = ref 0
            val nr = ref []
		    fun findFirstMatch ([], record) = (* no existing match succeeded, see if another token order matches *)
			 (print "1\n"; case (if ( !OPTIMAL_STRUCT ) then findTokenOrder2 tokenfreqs record else findTokenOrder1 tokenfreqs record)
                          of (NONE, _) => (print "really a bad record\n"; raise TokenMatchFailure (* tokens don't match this record *))
                          |  (SOME tokenOrder, NONE) => (print "1.1\n"; findFirstMatch ([(tokenOrder,[])], record)) 
                          |  (SOME tokenOrder, SOME newRecord) => (print "1.2\n"; tag := 1; nr := newRecord; findFirstMatch ([(tokenOrder,[])], newRecord)))
              | findFirstMatch (((current as (match, matchedContextLists))::rest), record) =  (* match: tokenorder *)
		          (print "2\n"; case classifyOneRecordWithMatch record match
			   of NONE => (print "find next\n"; current :: (findFirstMatch (rest, record)))
                           |  SOME contexts => (print "2.2\n"; ((match, contexts :: matchedContextLists) :: rest)) (* matches are in reverse order *))
            val firstrun = (findFirstMatch (matches, thisRecord), badRecords) handle TokenMatchFailure => (print "token match failure\n"; (matches, thisRecord :: badRecords)) (* bad records are in reverse *)
		in
		    if !tag = 0 then firstrun
		    else 
              (findFirstMatch (matches, !nr), badRecords) handle TokenMatchFailure => raise StructError
		end

	    val revPartition : NPartition = List.foldl (classifyOneRecord tokenfreqs) ([],[]) records

        fun reversePartition (matches, badRecords) = 
		  let fun revMatch (tokenOrder, matchedRecords) = (tokenOrder, List.rev matchedRecords)
		      fun revMatches [] acc = List.rev acc
                | revMatches (m::ms) acc = revMatches ms ((revMatch m)::acc)
(*val _ = print ("bad records: "^(Int.toString(List.length badRecords))^"\n")*)
val _ = print ("matches num: "^(Int.toString(List.length matches))^"\n")
		  in  
		    (revMatches matches [], List.rev badRecords)
		  end
	    
	    val partition : NPartition = reversePartition revPartition
	in
	   partition
	end

    fun splitRecords_HMM summary (records : NewContext list ) ( seqsetl : Seqset list ) : NPartition=

	  let 
        val {numTokensInCluster=count, numRecordsWithCluster=coverage, tokens=tokenfreqs} = summary

	    fun getTokenOrder summary record = 
	        let fun insertOne ((token,freq),tTable) = BSTokenTable.insert(tTable, token, ref freq)
		    val tTable = List.foldl insertOne BSTokenTable.empty summary
		    val numFound = ref 0
		    fun doOneToken tTable ((token,loc), acc) = 
			case BSTokenTable.find(tTable, token)
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
		    SOME(tList)  handle TokenMatchFailure => NONE
		end

        fun findTokenOrder summary record = 
          case (getTokenOrder summary record) of
              NONE => (NONE, NONE)
            | SOME t => (SOME(t), NONE)

	    fun classifyOneRecordWithMatch (thisRecord:NewContext) (tokenOrder:NTokenOrder)  = 
		let fun introduceLEmpty (contextList:NDerivedContexts) =  (*In progress*)
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
                              | convert ([]::rest) result = convert rest ([((PPempty,""),location)]::result)
			      | convert (cur::rest) result = convert rest (cur::result)
			in
			    ((convert contextList []) : NDerivedContexts)
			end
		    fun doMatch (tokensToMatch:NTokenOrder) (recordTokens: NewContext)
		                (curContextAcc : NewContext, contextListAcc : NDerivedContexts) = 
		    case (tokensToMatch, recordTokens) 
		    of ([],[])   => List.rev ((List.rev curContextAcc) :: contextListAcc)
                    |  ([], rts) => List.rev  (rts :: contextListAcc)
                    |  (tks, []) => raise TokenMatchFailure
                    |  (tokens as tk::tks, (lrtoken as (rt,loc))::rts) => 
			let val (rt, loc) = lrtoken : BSLToken
			in
			  if BSTokenEq(tk, rt)  (* found next token; push current context *)
			  then 
			      let val matchTokens = [lrtoken](*
				  case rt 
				  of Pgroup g => (groupToTokens g)
				  |  _        => [lrtoken]*)
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
	    fun classifyOneRecord tokenfreqs (thisRecord, (matches, badRecords)) = (* thisRecord is NewContext *) 
		let (* convert to accumulator form? *)
		    fun findFirstMatch ([], record) = (* no existing match succeeded, see if another token order matches *)
			 (case findTokenOrder tokenfreqs record
                          of (NONE, _) => raise TokenMatchFailure (* tokens don't match this record *)
                          |  (SOME tokenOrder, NONE) => findFirstMatch ([(tokenOrder,[])], record) 
                          |  (SOME tokenOrder, SOME newRecord) => findFirstMatch ([(tokenOrder,[])], newRecord))
              | findFirstMatch (((current as (match, matchedContextLists))::rest), record) =  (* match: tokenorder *)
		          (case classifyOneRecordWithMatch record match
			   of NONE => current :: (findFirstMatch (rest, record))
                           |  SOME contexts => ((match, contexts :: matchedContextLists) :: rest) (* matches are in reverse order *))
		in
		    (findFirstMatch (matches, thisRecord), badRecords)
		    handle tokenMatchFailure => (matches, thisRecord :: badRecords) (* bad records are in reverse *)
		end

	    val revPartition : NPartition = List.foldl (classifyOneRecord tokenfreqs) ([],[]) records

        fun reversePartition (matches, badRecords) = 
		  let fun revMatch (tokenOrder, matchedRecords) = (tokenOrder, List.rev matchedRecords)
		      fun revMatches [] acc = List.rev acc
                | revMatches (m::ms) acc = revMatches ms ((revMatch m)::acc)
		  in  
		    (revMatches matches [], List.rev badRecords)
		  end
	    
	    val partition : NPartition = reversePartition revPartition
	in
	   partition
	end

    (* convert a list of token lists into a list of token lists, 
       expanding group tokens if argument list contains entirely
       single element lists of the same group token; otherwise, 
       return argument token list list. *)
(*
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
*)
    fun mkBottom (coverage,cl) = 
	PPBottom ( { coverage=coverage
                 , label=SOME(mkBOTLabel (!Bottomstamp))
                 , tycomp = zeroComps
                 }
               , !Bottomstamp
               , cl
               ) before Bottomstamp := !Bottomstamp + 1

    (* debug function *)

    fun printColumn (cl: NewContext list) : unit =
      let
        fun myprint (bs, l) = 
          let
            val {lineNo = ln1, beginloc = bl1, endloc = el1, recNo = rn1} = l
          in
            printBSLTokenTy (bs, l); print "\t"; print ("("^(Int.toString bl1)^" "^(Int.toString el1)^")")
          end
        fun printOne (nc: NewContext) = (List.app (*printBSLTokenTy*)myprint nc; print "---")
      in
        (print "Column to chop: "; List.app printOne cl; print "\n")
      end

    fun printColumnString (cl: NewContext list) : unit =
      let
        fun printStr ((b,s),l) = print s
        fun printOne (nc: NewContext) = (List.app printStr nc; print "---")
      in
        (print "String to chop: "; List.app printOne cl; print "\n")
      end    

    exception EmptyColomn

    fun columnToLocations (cl: NewContext list) : location list =
      let
val _ = print "columnToLocations\n"
        (* pay attention to the PPempty *)
        fun cvtOne (nc: NewContext) = 
          let
            val ((b1, s1), l1) = List.nth(nc, 0) 
            val {lineNo = ln1, beginloc = bl1, endloc = el1, recNo = rn1} = l1
          in 
            if compBToken(b1, PPempty)=EQUAL then {lineNo = ln1, beginloc = ~1, endloc = ~1, recNo = rn1}
            else
                   let
                     val ((b2, s2), l2) = List.nth(nc, (List.length nc)-1)
                     val {lineNo = ln2, beginloc = bl2, endloc = el2, recNo = rn2} = l2
val _ = print ("recNo = "^(Int.toString rn1)^" lineNo = "^(Int.toString ln1)^" newbegin: "^(Int.toString bl1)^" newend: "^(Int.toString el2)^"\n")
                   in
                     {lineNo = ln1, beginloc = bl1, endloc = el2, recNo = rn1}
                   end
          end
      in
        List.map cvtOne cl
      end   

    fun locList2locTable cl =
      let
        fun doOne (l as {lineNo = ln1, beginloc = bl1, endloc = el1, recNo = rn1}, table) = IntMap.insert(table, rn1, l)  
      in
        List.foldl doOne IntMap.empty cl
      end
(*
    fun columnToLocations (cl: NewContext list) : location list =
      let
        (* pay attention to the PPempty *)
        fun cvtOne (nc: NewContext) = 
          let
            fun findfirstl (((b,s),l), ret) = 
              case ret of
                  NONE => if compBToken(b, PPempty)=EQUAL then NONE else SOME l
                | SOME r => SOME r
            val l1 = List.foldl findfirstl NONE nc 
          in 
            case l1 of
                NONE => 
                  let
                    val (junk, j1) = List.nth(nc, 0)
                    val {lineNo = ln1, beginloc = bl1, endloc = el1, recNo = rn1} = j1
                  in 
                    (print ("create PPempty location."^(Int.toString rn1)^"\n"); {lineNo = ln1, beginloc = ~1, endloc = ~1, recNo = rn1})
                  end
               | SOME l11 =>
                   let
                     val {lineNo = ln1, beginloc = bl1, endloc = el2, recNo = rn1} = l11
                     val l2 = Option.valOf (List.foldr findfirstl NONE nc)
                     val {lineNo = ln2, beginloc = bl2, endloc = el2, recNo = rn2} = l2
                   in
                     {lineNo = ln1, beginloc = bl1, endloc = el2, recNo = rn1}
                   end
          end
      in
        List.map cvtOne cl
      end   
*)

    exception ViterbiError2

    exception UnionSetCoverError

    (* Invariant: cl is not an empty column: checked before mkTBD is called with isEmpty function *)
    (* coverage is number of records in this context *)
    fun mkTBD (callsite, currentDepth, coverage, cl, ssl, tables) = 
        (* Columns that have some empty rows must have the empty list representation
           of the empty row converted to the [Pempty] token.  Otherwise, a column
           that is either empty or some value gets silently converted to the value only. *)
	let
 
       fun cnvEmptyRowsToPempty [] = [((PPempty, ""),{lineNo= callsite, beginloc=0, endloc=0, recNo=callsite})] (* XXX fix line number *)
              | cnvEmptyRowsToPempty l  = l
	    val cl = List.map cnvEmptyRowsToPempty cl
(*
        (* assume colomn is in order *)
        fun cnvEmptyRowsToPempty i =
          if i=0 then []
          else
            let
              val old = cnvEmptyRowsToPempty (i-1)
              val l = List.nth(cl, i-1)
            in
              case l of
                 [] => old@[[((PPempty, ""),{lineNo= i-1, beginloc=0, endloc=0, recNo=i-1})]]
                |_ => old@[l]
            end
        val cl = cnvEmptyRowsToPempty (List.length cl)
*)
            fun allEmpty cl =
		let fun isNonEmpty [((PPempty, _),_)] = false
		      | isNonEmpty _ = true
		in
		    ((*print "Checking for an allempty context\n";*)
		    not(Option.isSome(List.find isNonEmpty cl))
		     )
		end
	    (*val cl = crackUniformGroups cl*)  (* ignore groups now *)
	in
	    if allEmpty cl then PPBase(mkTyAux coverage, List.concat cl)
	    else if (coverage < isNoiseTolerance(!initialRecordCount))
		 then mkBottom(coverage,cl)  (* not enough data here to be worth the trouble...*)
	    else if (currentDepth >= !depthLimit)  (* we've gone far enough...*)
                 then PPTBD ( { coverage=coverage
                            , label=SOME(mkTBDLabel (!TBDstamp))
                            , tycomp = zeroComps
                            }
                          , !TBDstamp
                          , cl
                          ) before TBDstamp := !TBDstamp    + 1
                 else (
                   if (List.length ssl) = 0 then 
                     if ( !hmmtokenize = true ) then SeqsetListToTy_HMM (currentDepth + 1) cl 
                     else SeqsetListToTy (currentDepth + 1) [] tables
                   else
                   let
                     val locList = columnToLocations cl
val _ = printColumn cl
(*val _ = printColumnString cl*)
(*                   val locTable = locList2locTable locList*)
(*val _ = (print "Location to chop: "; List.app printLocation locList; print "\n")*)
(*val _ = List.app printSSLoc ssl*)
(*val _ = (print "Before chopping seqset list: "; List.app printlist ssl; print "\n") *)

                     val newSSL = chopSeqsets_notarray ssl locList
(*val _ = (print "Chopped seqset list: "; List.app printlist newSSL; print "\n") *)
                   in 
                     if ( !hmmtokenize = true ) then SeqsetListToTy_HMM (currentDepth + 1) cl
                     else SeqsetListToTy (currentDepth + 1) newSSL tables
                   end
                 )
	end

    and mkTBD_array (callsite, currentDepth, coverage, cl, ssl, tables) = 
        (* Columns that have some empty rows must have the empty list representation
           of the empty row converted to the [Pempty] token.  Otherwise, a column
           that is either empty or some value gets silently converted to the value only. *)
	let
 
       fun cnvEmptyRowsToPempty [] = [((PPempty, ""),{lineNo= callsite, beginloc=0, endloc=0, recNo=callsite})] (* XXX fix line number *)
              | cnvEmptyRowsToPempty l  = l
	    val cl = List.map cnvEmptyRowsToPempty cl
(*
        (* assume colomn is in order *)
        fun cnvEmptyRowsToPempty i =
          if i=0 then []
          else
            let
              val old = cnvEmptyRowsToPempty (i-1)
              val l = List.nth(cl, i-1)
            in
              case l of
                 [] => old@[[((PPempty, ""),{lineNo= i-1, beginloc=0, endloc=0, recNo=i-1})]]
                |_ => old@[l]
            end
        val cl = cnvEmptyRowsToPempty (List.length cl)
*)
            fun allEmpty cl =
		let fun isNonEmpty [((PPempty, _),_)] = false
		      | isNonEmpty _ = true
		in
		    ((*print "Checking for an allempty context\n";*)
		    not(Option.isSome(List.find isNonEmpty cl))
		     )
		end
	    (*val cl = crackUniformGroups cl*)  (* ignore groups now *)
	in
	    if allEmpty cl then PPBase(mkTyAux coverage, List.concat cl)
	    else if (coverage < isNoiseTolerance(!initialRecordCount))
		 then mkBottom(coverage,cl)  (* not enough data here to be worth the trouble...*)
	    else if (currentDepth >= !depthLimit)  (* we've gone far enough...*)
                 then PPTBD ( { coverage=coverage
                            , label=SOME(mkTBDLabel (!TBDstamp))
                            , tycomp = zeroComps
                            }
                          , !TBDstamp
                          , cl
                          ) before TBDstamp := !TBDstamp    + 1
                 else (
                   if (List.length ssl) = 0 then 
                     if ( !hmmtokenize = true ) then SeqsetListToTy_HMM (currentDepth + 1) cl 
                     else SeqsetListToTy (currentDepth + 1) [] tables
                   else
                   let
                     val locList = columnToLocations cl
val _ = printColumn cl
(*val _ = printColumnString cl*)
(*                   val locTable = locList2locTable locList*)
(*val _ = (print "Location to chop: "; List.app printLocation locList; print "\n")*)
(*val _ = List.app printSSLoc ssl*)
(*val _ = (print "Before chopping seqset list: "; List.app printlist ssl; print "\n") *)

                     val newSSL = chopSeqsets ssl locList
(*val _ = (print "Chopped seqset list: "; List.app printlist newSSL; print "\n") *)
                   in 
                     if ( !hmmtokenize = true ) then SeqsetListToTy_HMM (currentDepth + 1) cl
                     else SeqsetListToTy (currentDepth + 1) newSSL tables
                   end
                 )
	end

    and mkTBDwithContexts (callsite, currentDepth, coverage, cl, ssl, tables) = 
        (* Columns that have some empty rows must have the empty list representation
           of the empty row converted to the [Pempty] token.  Otherwise, a column
           that is either empty or some value gets silently converted to the value only. *)
	let
 
       fun cnvEmptyRowsToPempty [] = [((PPempty, ""),{lineNo= callsite, beginloc=0, endloc=0, recNo=callsite})] (* XXX fix line number *)
              | cnvEmptyRowsToPempty l  = l
	    val cl = List.map cnvEmptyRowsToPempty cl
(*
        (* assume colomn is in order *)
        fun cnvEmptyRowsToPempty i =
          if i=0 then []
          else
            let
              val old = cnvEmptyRowsToPempty (i-1)
              val l = List.nth(cl, i-1)
            in
              case l of
                 [] => old@[[((PPempty, ""),{lineNo= i-1, beginloc=0, endloc=0, recNo=i-1})]]
                |_ => old@[l]
            end
        val cl = cnvEmptyRowsToPempty (List.length cl)
*)
            fun allEmpty cl =
		let fun isNonEmpty [((PPempty, _),_)] = false
		      | isNonEmpty _ = true
		in
		    ((*print "Checking for an allempty context\n";*)
		    not(Option.isSome(List.find isNonEmpty cl))
		     )
		end
	    (*val cl = crackUniformGroups cl*)  (* ignore groups now *)
	in
	    if allEmpty cl then PPBase(mkTyAux coverage, List.concat cl)
	    else if (coverage < isNoiseTolerance(!initialRecordCount))
		 then mkBottom(coverage,cl)  (* not enough data here to be worth the trouble...*)
	    else if (currentDepth >= !depthLimit)  (* we've gone far enough...*)
                 then PPTBD ( { coverage=coverage
                            , label=SOME(mkTBDLabel (!TBDstamp))
                            , tycomp = zeroComps
                            }
                          , !TBDstamp
                          , cl
                          ) before TBDstamp := !TBDstamp    + 1
                 else (
                   if (List.length ssl) = 0 then 
                     if ( !hmmtokenize = true ) then SeqsetListToTy_HMM (currentDepth + 1) cl 
                     else SeqsetListToTy (currentDepth + 1) [] tables
                   else
                   let
                     val locList = columnToLocations cl
val _ = printColumn cl
(*val _ = printColumnString cl*)
(*                   val locTable = locList2locTable locList*)
(*val _ = (print "Location to chop: "; List.app printLocation locList; print "\n")*)
(*val _ = List.app printSSLoc ssl*)
(*val _ = (print "Before chopping seqset list: "; List.app printlist ssl; print "\n") *)

                     val newSSL = chopSeqsets ssl locList
(*val _ = (print "Chopped seqset list: "; List.app printlist newSSL; print "\n") *)
                   in 
                     if ( !hmmtokenize = true ) then SeqsetListToTy_HMM (currentDepth + 1) cl
                     else SeqsetListToTywithContexts (currentDepth + 1) cl newSSL tables
                   end
                 )
	end


    (* old: clustersToTy curDepth rtokens numRecords clusters *)
    and clustersToTy (curDepth:int) (rtokens:NewContext list) (seqsetl:Seqset list) tables (numRecords:int) clusters : NewTy = 
	let val analysis = analyzeClusters numRecords clusters
	    val _ = if output_histograms then 
			dumpClusters curDepth numRecords clusters ((!outputDir) ^ "histogram.dat")
		    else ()
            (* This function partitions a context into a union. *)
            (* It currently uses the first token in each context to do the partition *)

        fun buildUnionTy (FirstBSToken, rtokens, ssl, tables) = 
		      let (*val () = print "BUILDING UNION TY\n"*)
		        val numChunks = List.length rtokens
		        fun updateBSTable(token,chunk,tTable) = 
		          case BSTokenTable.find(tTable, token) of 
                           NONE => BSTokenTable.insert(tTable, token, ref [chunk])
                        |  SOME chunkList => (chunkList := chunk::(!chunkList);   (* note that chunks end up in reverse order *)
					      tTable)
		        fun doOneChunk([], tTable) = updateBSTable((PPempty, ""),[], tTable)
                      | doOneChunk(chunk as ((t,loc)::ts), tTable) = updateBSTable(t,chunk,tTable)
		        val pTable = List.foldl doOneChunk BSTokenTable.empty rtokens 
		    (* allSame handles the case where all chunks start with the same, non-Empty token *)
                fun allSame myrtokens = 
			      let 
                    fun doOne ((lt::lts), (fst,snd)) = ([lt]::fst, lts::snd)
			        val (fsts,snds) = List.foldl doOne ([],[]) myrtokens
			      in
			        PPstruct(mkTyAux numRecords, 
				    [mkTBD(~9, curDepth, numChunks, List.rev fsts, ssl, tables),
				     mkTBD(~10,curDepth, numChunks, List.rev snds, ssl, tables)])
			      end

                fun myallSame myrtokens = 
			      let 
                    fun doOne ((lt::lts), (fst,snd)) = ([lt]::fst, lts::snd)
			        val (fsts,snds) = List.foldl doOne ([],[]) myrtokens
			      in
			        PPstruct(mkTyAux numRecords, 
				    [mkTBDwithContexts(~9, curDepth, numChunks, List.rev fsts, ssl, tables),
				     mkTBDwithContexts(~10,curDepth, numChunks, List.rev snds, ssl, tables)])
			      end
                    (* allEmpty handles the case where all chunks are the empty chunk *)
		        fun allEmpty () = PPBase(mkTyAux numRecords, [((PPempty,""),{lineNo= ~1, beginloc=0, endloc=0, recNo= ~1})])
		    (* doPartition handles the case where the chunks did not all have the same initial token *)

		        fun doPartition pTable = 
			      let val items = BSTokenTable.listItems pTable (* list of chunks, one per intital token, in reverse order *)
			        val tys = List.map (fn item => mkTBD(~11, curDepth, List.length (!item), List.rev (!item), ssl, tables) ) items
			      in
			        PPunion(mkTyAux numRecords, tys)
			      end

		        fun doPartition2 pTable = 
			      let
                    val initTList : BSToken list list = List.map initToken ssl
                    val index = ref ~1
                    fun countOne (bslist, (iTable, indexTable)) = 
                      let
                        fun co (bs, mytable) = case BSTokenTable.find(mytable, bs) of
                                                   NONE => BSTokenTable.insert(mytable, bs, (1, IntMap.insert(IntMap.empty, !index, true)))
                                                 | SOME (i, otable) =>
                                                     let
                                                       val (rmv, junk) = BSTokenTable.remove(mytable, bs) 
                                                     in
                                                       BSTokenTable.insert(rmv, bs, (i+1, IntMap.insert(otable, !index, true)))
                                                     end
                      in
                        ( index := !index + 1; (List.foldl co iTable bslist, IntMap.insert(indexTable, !index, true)))
                      end
                    val (iTable, indexTable) = List.foldl countOne (BSTokenTable.empty, IntMap.empty) initTList
                    val ilist = BSTokenTable.listItemsi iTable
                    fun findBests ((bstoken, (count, table)), blist) = if count = numRecords then bstoken::blist else blist
                    val bestlist = List.foldl findBests [] ilist
                    fun getBest ((b, s), (min, best)) = if (BTokenCompleteEnum(b) < min) then (BTokenCompleteEnum(b), (b,s)) else (min, best) 
                  in
                    if List.length bestlist > 0 then 
                      let
                        val (junk, initt) = List.foldl getBest (Option.valOf(Int.maxInt), (PPblob, "")) bestlist
                        val newrtokens = List.map (ViterbiWithInitT tables initt) ssl
		                val newpTable = List.foldl doOneChunk BSTokenTable.empty newrtokens 
val _ = print ("initt: "^(BTokenToName(#1 initt))^"\n")
val _ = print (newcontextsToString newrtokens)
                      in
                        if BSTokenTable.numItems(newpTable) = 1 then (* PPblob? *)
                          let
                            val itokenlist = BSTokenTable.listItemsi(newpTable)
                            fun isBlob [((btoken, s), count)] = (compBToken(btoken, PPblob)=EQUAL)
                          in
                            if isBlob itokenlist then doPartition pTable
                            else myallSame newrtokens
                          end
                        else  doPartition pTable (* this is the case when the initT is not an appropriate one *)
                      end
                    else
                      if ( !RDC_UNION_BRANCHES ) then 
                        let
                          fun findBiggest ((bstoken, (count, table)), (max, maxtoken)) = 
                            if (count>max) then (count, bstoken)
                            else if (count=max) then if (BSTokenCompleteEnum(bstoken)<BSTokenCompleteEnum(maxtoken)) then (count, bstoken) else (max, maxtoken)
                            else (max, maxtoken)
                          fun greedy (myiTable, myindexTable) =
                            if IntMap.numItems(myindexTable) = 0 then []
                            else 
                              let
                                val myilist = BSTokenTable.listItemsi myiTable
                                val (mymax, thistoken) = List.foldl findBiggest (0, (PPempty, "")) myilist 
                              in
                                if mymax = 0 then raise UnionSetCoverError
                                else 
                                  let
                                    val (rmvTable, (junk, indexlistt)) = BSTokenTable.remove(myiTable, thistoken) handle NotFound => raise UnionSetCoverError
                                    val rmvList = BSTokenTable.listItemsi rmvTable
                                    fun ttolist (i, junk) = i
                                    val indexlist = List.map ttolist (IntMap.listItemsi indexlistt)
                                    fun updateTables ((bstoken, (count, table)), (newiTable, newindexTable)) = 
                                      let
                                        val preilist = IntMap.listItemsi table
                                        fun checkOne ((i, junk), postitable) = 
                                          case IntMap.find(indexlistt, i) of
                                              SOME _ => #1(IntMap.remove(postitable, i))
                                            | NONE => postitable
                                        val newitable = List.foldl checkOne table preilist
                                        val newcount = IntMap.numItems(newitable)
                                        val retnewiTable = if newcount = 0 then newiTable
                                                           else BSTokenTable.insert(newiTable, bstoken, (newcount, newitable))
                                        fun updateIndex (i, mynewindext) = 
                                          case IntMap.find(mynewindext, i) of
                                              SOME _ => #1(IntMap.remove(mynewindext, i))
                                            | NONE => mynewindext
                                        val retnewindexTable = List.foldl updateIndex newindexTable indexlist
                                      in
                                        (retnewiTable, retnewindexTable)
                                      end
                                    val (thisniTable, thisnindexTable) = List.foldl updateTables (BSTokenTable.empty, myindexTable) rmvList
                                  in
                                    thistoken :: greedy(thisniTable, thisnindexTable)
                                  end
                              end
                          val newbestlist = greedy (iTable, indexTable)
                        in
                          if (List.length newbestlist)*rdc_ratio < BSTokenTable.numItems(pTable) then  
                            let
                              fun listtotable (t, table) = BSTokenTable.insert(table, t, true)
                              val newbesttable = List.foldl listtotable BSTokenTable.empty newbestlist                  
                              fun getOne ss = 
                                let
                                  val myinitlist = initToken ss
                                  fun getmylist (t, mylist) = 
                                    case BSTokenTable.find(newbesttable, t) of
                                        SOME _ => t::mylist
                                      | NONE => mylist
                                  val mylist = List.foldl getmylist [] myinitlist
                                  val (junk, initt) = List.foldl getBest (Option.valOf(Int.maxInt), (PPblob, "")) mylist
                                  val initt = if compBSToken(initt, (PPblob, ""))=EQUAL then List.nth(myinitlist, 0) else initt
                                  val newrtoken = ViterbiWithInitT tables initt ss
                                in
                                  newrtoken
                                end
                              val newrtokens = List.map getOne ssl
		                      val newpTable = List.foldl doOneChunk BSTokenTable.empty newrtokens 
(*val _ = print (newcontextsToString newrtokens)*)
                            in
                              doPartition newpTable
                            end
                          else doPartition pTable
                        end
                      else doPartition pTable
                  end
		in
		        if BSTokenTable.numItems(pTable) = 1 
			      then if BSTokenTable.inDomain(pTable, (PPempty, "")) then allEmpty () 
			           else allSame rtokens
                else if ( !OPTIMAL_1ST_TOKEN ) then doPartition2 pTable
                     else doPartition pTable
		end


           (* This function takes a Struct Partition and returns a Ty describing that partition *)
	    fun buildStructTy partitions ssl tables = 
		let val (matches, badRecords) = partitions
		    fun isEmpty column = not (List.exists (not o null) column)  (* ??? *)

		    fun cnvOneMatch (tokenOrder, dclist) = 
			let val columns = doTranspose dclist
			    fun recurse(columns, tokens) result =
				case (columns, tokens) 
				  of ([],[])      => raise Fail "token and column numbers didn't match (2)"
				  |  ([last], []) => List.rev (if isEmpty last then result else ((mkTBD  (~100, curDepth,(List.length last), last, ssl, tables)) :: result))
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
					    val col1Ty = if isEmpty col1 then  [] else [(mkTBD (~8, curDepth, coverage1, col1, ssl, tables (*(borrowLoc col1 coltk)*)))]
					    val coltkTy = [(PPBase  (mkTyAux coveraget, List.concat coltk))](*case tk 
						          of Pgroup g => [(mkTBD (~3, curDepth, coveraget, coltk))]
							   | _ =>        [(PPBase  (mkTyAux coveraget, List.concat coltk))] *)
					in
					    recurse(cols, tks) (coltkTy @ col1Ty @ result)
					end
			         |  (cols,[]) => raise Fail "Unexpected case in recurse function."
			in
			    case recurse (columns, tokenOrder) []
			    of   []   => raise Fail "Expected at least one field."
			      |  [ty] => ty
			      |  tys  => PPstruct (mkTyAux (minNCoverage tys), tys)
			end

		    val matchTys = List.map cnvOneMatch matches
		    val resultTy =
			case (matchTys, badRecords) 
  		        of   ([], [])   => raise Fail "Expected records in struct context."
			  |  ([], brs)  => (* This case arises if a false cluster is identified: a cluster with one
					      or more tokens coming from one subset of records, and another group of tokens
					      coming from a disjoint subset. So we should introduce a union.*)
(*			                   (print "in mkbottom case\n"; mkBottom (List.length brs,brs))       *)
					   ((*print "converting false struct into union\n";*) buildUnionTy(FirstBSToken, brs, ssl, tables))
			  |  ([ty], []) => ty
			  |  (tys, brs) => if isEmpty brs 
					   then PPunion (mkTyAux(sumNCoverage tys), tys) 
					   else let val badCoverage = List.length brs
						in 
						    PPunion (mkTyAux(badCoverage + sumNCoverage tys), 
							    (tys @ [(mkTBD (~4, curDepth, badCoverage, brs, ssl, tables))]))
						end
		in
		    resultTy
		end


	    fun buildArrayTy ({tokens=atokens}, rtokens, ssl, tables) = 
		let val numTokens = List.foldl (fn((token,freq),sum) => sum + freq) 0 atokens
		    val recIndex = ref 0
		    fun partitionOneRecord tlist = 
		        let fun insertOne ((token,freq),tTable) = BSTokenTable.insert(tTable, token, ref freq)
			    val tTable = List.foldl insertOne BSTokenTable.empty atokens
			    val numFound = ref 0
			    fun resetTable () = 
				let fun setOne(token,freq) = 
				    let val freqRef = valOf (BSTokenTable.find(tTable,token))
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
				       | getLoc ((tok,loc:location)::ltocs) = #recNo loc
				 in
				     ((length, !recIndex), first, main, List.rev current)
				 end
                              | doNextToken isFirst ((rt as (lrt,loc))::rts) (current, first, main) = 
				 let 
				     val rt = (lrt,loc) 
				 in
				  case BSTokenTable.find(tTable, lrt)
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
				 end
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
                        (f t index)::(pushRecNo' rest index)
					    (*case t of (Pgroup {left=lt, body=bts, right=rt}, loc) =>
					        (f (Pgroup {left=(f lt index), body=pushRecNo' bts index,
						    right= (f rt index)}, loc) index)::
						    (pushRecNo' rest index)
					    | _ => (f t index)::(pushRecNo' rest index)*)
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
		    ((*(if print_verbose then 
		     (print "Array context\n"; 
                     print "First Context:\n";
                     print (contextsToString firstContext);
                     print "body Context:\n";
                     print (contextsToString mainContext);
                     print "last Context:\n";
                     print (contextsToString lastContext))
		     else ()); *)
		     PParray (mkTyAux numRecords, 
			     {tokens  = atokens, 
			      lengths = arrayLengths,
			      first   = mkTBD(~5, curDepth, List.length firstContext, firstContext, ssl, tables),
			      body    = mkTBD_array(~6, curDepth, List.length mainContext, mainContext, ssl, tables),
			      last    = mkTBD(~7, curDepth, List.length lastContext, lastContext, ssl, tables)}))
		end


      val ty = case analysis of 
		        Blob =>     mkBottom (List.length rtokens, rtokens)  (*can we still do sth using lower-prob seq?*)
		     |  Empty =>    PPBase (mkTyAux 0, [((PPempty,""),{lineNo= ~1, beginloc= ~1, endloc= ~1, recNo= ~1})])
		     |  Struct s => (print "struct here\n"; if (!hmmtokenize = true) then buildStructTy (splitRecords_HMM s rtokens seqsetl) seqsetl tables
                            else buildStructTy (splitRecords s rtokens seqsetl tables) seqsetl tables (* Can produce union of structs *))
		     |  Array a =>  buildArrayTy (a, rtokens, seqsetl, tables) 
             |  Union u =>  (print "union here\n"; buildUnionTy(u, rtokens, seqsetl, tables))

	in
	    ty
	end


    and SeqsetListToTy (curDepth:int) (seqsetl:Seqset list) tables : NewTy = 
	let val numRecordsinSeqsetList = List.length seqsetl
(*
	    val _ = print ("Number records being considered: "^Int.toString(numRecordsinContext)^"\nThe records are:\n"
		^(contextsToString context))
*)
        val bestProbPaths : NewContext list = (*List.map selectPath seqsetl*) List.map (basicViterbi tables) seqsetl
val _ = print "basicViterbi done\n" 
        (* val context : Context list = List.map TokenseqToContext bestProbPaths *)
        val counts : BRecordCount list = List.map countBFreqs bestProbPaths    
	    val fd: freqDist = buildHistograms numRecordsinSeqsetList counts
	    (* val clusters : (Token * histogram) list list = findClusters numRecordsinContext fd *)
	    (* val () = if print_verbose then printClusters numRecordsinContext clusters else () *)
	    val clusters : (BSToken * histogram) list list = newFindClusters numRecordsinSeqsetList fd
        val ty : NewTy = clustersToTy curDepth bestProbPaths seqsetl tables numRecordsinSeqsetList clusters (* break it *)
	    (*
	    val () = (print "Inferred type:\n"; 
		      print (TyToString ty);
		      print "\n")
	    *)
        (* val ty = PPBase (mkTyAux 0, [((PPempty,""),{lineNo= ~1, beginloc=0, endloc=0, recNo= ~1})]) *)
	in
	    ty
	end 

    and SeqsetListToTywithContexts (curDepth:int) (rtokens: NewContext list) (seqsetl:Seqset list) tables : NewTy = 
	let val numRecordsinSeqsetList = List.length seqsetl
(*
	    val _ = print ("Number records being considered: "^Int.toString(numRecordsinContext)^"\nThe records are:\n"
		^(contextsToString context))
*)
        val counts : BRecordCount list = List.map countBFreqs rtokens    
	    val fd: freqDist = buildHistograms numRecordsinSeqsetList counts
	    (* val clusters : (Token * histogram) list list = findClusters numRecordsinContext fd *)
	    (* val () = if print_verbose then printClusters numRecordsinContext clusters else () *)
	    val clusters : (BSToken * histogram) list list = newFindClusters numRecordsinSeqsetList fd
        val ty : NewTy = clustersToTy curDepth rtokens seqsetl tables numRecordsinSeqsetList clusters (* break it *)
	    (*
	    val () = (print "Inferred type:\n"; 
		      print (TyToString ty);
		      print "\n")
	    *)
        (* val ty = PPBase (mkTyAux 0, [((PPempty,""),{lineNo= ~1, beginloc=0, endloc=0, recNo= ~1})]) *)
	in
	    ty
	end 

    and SeqsetListToTy_HMM (curDepth:int) (bestProbPaths:NewContext list) : NewTy = 
	let val numRecordsinSeqsetList = List.length bestProbPaths
        val counts : BRecordCount list = List.map countBFreqs bestProbPaths    
	    val fd: freqDist = buildHistograms numRecordsinSeqsetList counts
	    val clusters : (BSToken * histogram) list list = newFindClusters numRecordsinSeqsetList fd
        val ty : NewTy = clustersToTy curDepth bestProbPaths [] (IntMap.empty, BTokenTable.empty, BTokenTable.empty, BTokenPairTable.empty, ListBTokenPairTable.empty) numRecordsinSeqsetList clusters (* break it *)
	in
	    ty
	end 

(*
    fun computeProbStructure fileName : NewTy = 
	let val recordNumber = ref 0
	    val () = print ("Starting on file "^(lconcat fileName)^"\n");
	    val records = loadFiles fileName  (* records: string list *)
	    val () = initialRecordCount := (List.length records) 
        val tokensNoBlob = List.take(tokenDefList, (List.length tokenDefList)-1)
        val dfatable = (* constrDFATable tokensNoBlob *) BTokenDFATable.empty
        val rtokens : Seqset list = List.map (pathGraph recordNumber dfatable) records
val _ = print "path graph done.\n"
        val rptokens : Seqset list = if ( !character = true ) then  computeProbChar rtokens else computeProb rtokens
val _ = print "add prob done.\n"
        val newty = SeqsetListToTy 0 rptokens
val _ = print "seqset to list done.\n"
        (*    val rtokens = crackUniformGroups rtokens *)(* check if all records have same top level group token *)
	    (* val () = if print_verbose = true then lengthsToHist rtokens else () *)
	    (* val ty = SeqsetListToTy 0 rtokens *)
	    val snewty = simplifyNewTy newty
	in
	    snewty
	end
*)

    fun computeProbStructure fileName endingtimes : NewTy * Times.EndingTimes = 
	let val recordNumber = ref 0
	    val () = print ("Starting on file "^(lconcat fileName)^"\n");
	    val records = loadFiles fileName  (* records: string list *)
	    val () = initialRecordCount := (List.length records) 
        val tokensNoBlob = List.take(tokenDefList, (List.length tokenDefList)-1)
        val dfatable = (* constrDFATable tokensNoBlob *) BTokenDFATable.empty
        val rtokens : Seqset list = List.map (pathGraph recordNumber dfatable) records
        val end2Times = Times.updateTokenEnd (Time.now()) endingtimes
        val hmmtables = readinHMM "training/"
val _ = print "path graph done.\n"
        val newty = SeqsetListToTy 0 rtokens hmmtables
val _ = print "seqset to list done.\n"
        (*    val rtokens = crackUniformGroups rtokens *)(* check if all records have same top level group token *)
	    (* val () = if print_verbose = true then lengthsToHist rtokens else () *)
	    (* val ty = SeqsetListToTy 0 rtokens *)
	    val snewty = simplifyNewTy newty
        val end3Times = Times.updateStructEnd (Time.now()) end2Times
	in
	    (snewty, end3Times)
	end

    fun examHmmResultPre fileName  = 
	let
	    val () = print ("Starting on file "^(lconcat fileName)^"\n");
	    val records = loadFiles fileName  (* records: string list *)
        fun doOne record : int list list = List.map charToList (String.explode record)
        fun doOne2 record : int list =  List.map Char.ord (String.explode record)
        val strm = TextIO.openOut ("testing/input")
        fun printList list =
          let
(*            fun listToInt (c, (bit, ret)) = if bit = ~1 then (0, ret)
                                            else (bit-1, c*(Real.toInt IEEEReal.TO_NEAREST (Math.pow(Real.fromInt 2, Real.fromInt bit)))+ret)*)
            fun printOneVec v = 
              let
(*                val (bit, ret) = List.foldl listToInt ((fvectorbits-1),0) v  *)
                  val ret = listToInt v 
              in
                TextIO.output(strm, ((Int.toString ret)^" "))
              end
            fun printOneRecord record = (List.app printOneVec record; TextIO.output(strm, "\n"))
          in
            List.app printOneRecord list
          end
        fun printList2 list =
          let
            fun printOneChar i = TextIO.output(strm, ((Int.toString i)^" "))
            fun printOneRecord record = (List.app printOneChar record; TextIO.output(strm, "\n"))
          in
            List.app printOneRecord list
          end
 	in
	    (
         if ( !character = true ) then printList2 (List.map doOne2 records)
         else printList (List.map doOne records); 
         TextIO.closeOut strm
        )
	end

    fun constrOrdBTokenTable path =
      let
        val _ = dumpTokenName path BTokenTable.empty
        val tokens = loadFile (path^"TokenName")
        fun recFn i = 
          if i<0 then OrdBTokenTable.empty
          else  
            OrdBTokenTable.insert((recFn (i-1)), i, nameToBToken(List.nth(tokens, i))) 
      in
        recFn ((List.length tokens)-1)
      end

    fun examHmmResultPost fileName  = 
	let
        val records = loadFiles fileName
	    val tokenss = loadFile "testing/output" 
        val _ = print "Tokenization by HMM:\n"
        val otable = constrOrdBTokenTable "training/"
        fun extractIntList record : BToken list = 
          let
            fun isSpace c = c = #" "
            fun doOne s : BToken list = 
              let
                val (pre, rest) = Substring.splitl (not o isSpace) s
                val ret = intToBToken(Option.valOf(Int.fromString(Substring.string pre)), otable) handle Option => (print ("s = "^(Substring.string pre)^"\n"); raise Option)
                val newrest = Substring.triml 1 rest
              in
                if (Substring.size newrest)=0 orelse (isSpace (Substring.sub(newrest, 0))) then [ret]
                else if (Substring.size pre) = (Substring.size s) then [ret]
                else ret::(doOne (Substring.triml 1 rest)) 
              end
          in
            if String.compare(record, "nil") = EQUAL then []
            else doOne (Substring.full record)
          end
        val tokensi = List.map extractIntList tokenss
(* 
        fun collapse record : BToken list = 
          let
            fun doOne (t, (pre, ret)) = 
              case t of
                  PPpunc p => (t, ret@[t])
                | _ => if compBToken(t, pre)=EQUAL then (t, ret)
                       else (t, ret@[t]) 
          in
            case record of
                [] => []
              | hd::tl => let val (junk, list) = List.foldl doOne (hd, [hd]) tl in list end
          end
*)
        fun collapse (record: string, tks: BToken list) : char list list * BToken list = 
          let
            val chars = String.explode record
            fun doOne (c, t, (pret, (retc, rett))) = 
              case t of
                  PPpunc p => (t, (retc@[[c]], rett@[t]))
                | _ => if compBToken(t, pret)=EQUAL then (t, ((List.take(retc, (List.length retc)-1)@[(List.last retc)@[c]]), rett))
                       else (t, (retc@[[c]], rett@[t])) 
          in
            case tks of
                [] => ([[]], [])
              | hd::tl => let val (junk, list) = ListPair.foldlEq doOne (hd, ([[List.hd chars]], [hd])) (List.tl chars, tl) in list end
          end
(*        val tokens = List.map collapse tokensi *)
        val ret = ListPair.mapEq collapse (records, tokensi)
        fun adjust (charll, blist) : BSToken list =
          let
            fun combineOne (charl, bt) = (bt, String.implode charl)
          in
            case blist of
                [] => []
              | _ => ListPair.mapEq combineOne (charll, blist)
          end
        val bsll : BSToken list list = List.map adjust ret
(*
        fun printOne tk = print ((BTokenToName tk)^" ")
        fun printListPair (re: string, tks: BToken list) = 
          case tks of
              [] => (print (re^"\n"); print "no tokenization result\n")
            | _ => (print (re^"\n"); List.app printOne tks; print "\n")
*)
        fun printBSToken (t, s) = print ((BTokenToName t)^"["^s^"]"^" ")
        fun printListPair (re:string, bslist: BSToken list) =
          case bslist of
              [] => (print (re^"\n"); print "no tokenization result\n")
            | _ => (print (re^"\n"); List.app printBSToken bslist; print "\n")
	in
(*	    ListPair.appEq printListPair (records, tokens) *)
        ListPair.appEq printListPair (records, bsll)
	end

    fun evaluateHmmResultPost fileName  = 
	let
        val records = loadFiles fileName
	    val tokenss = loadFile "testing/output" 
        val _ = print "Tokenization by HMM:\n"
        val otable = constrOrdBTokenTable "training/"
        fun extractIntList record : BToken list = 
          let
            fun isSpace c = c = #" "
            fun doOne s : BToken list = 
              let
                val (pre, rest) = Substring.splitl (not o isSpace) s
                val ret = intToBToken(Option.valOf(Int.fromString(Substring.string pre)), otable) handle Option => (print ("s = "^(Substring.string pre)^"\n"); raise Option)
                val newrest = Substring.triml 1 rest
              in
                if (Substring.size newrest)=0 orelse (isSpace (Substring.sub(newrest, 0))) then [ret]
                else if (Substring.size pre) = (Substring.size s) then [ret]
                else ret::(doOne (Substring.triml 1 rest)) 
              end
          in
            if String.compare(record, "nil") = EQUAL then []
            else doOne (Substring.full record)
          end
        val tokensi = List.map extractIntList tokenss
        fun collapse (record: string, tks: BToken list) : char list list * BToken list = 
          let
            val chars = String.explode record
            fun doOne (c, t, (pret, (retc, rett))) = 
              case t of
                  PPpunc p => (t, (retc@[[c]], rett@[t]))
                | _ => if compBToken(t, pret)=EQUAL then (t, ((List.take(retc, (List.length retc)-1)@[(List.last retc)@[c]]), rett))
                       else (t, (retc@[[c]], rett@[t])) 
          in
            case tks of
                [] => ([[]], [])
              | hd::tl => let val (junk, list) = ListPair.foldlEq doOne (hd, ([[List.hd chars]], [hd])) (List.tl chars, tl) in list end
          end
        val ret = ListPair.mapEq collapse (records, tokensi)
        fun adjust (charll, blist) : BSToken list =
          let
            fun combineOne (charl, bt) = (bt, String.implode charl)
          in
            case blist of
                [] => []
              | _ => ListPair.mapEq combineOne (charll, blist)
          end
        val bsll2 : BSToken list list = List.map adjust ret
        val bsll1 = extractLog "training/log/" "training/log/testname"
        fun printBSToken (t, s) = print ((BTokenToName t)^"["^s^"]"^" ")
        fun printListPair (re:string, bslist: BSToken list) =
          case bslist of
              [] => (print (re^"\n"); print "no tokenization result\n")
            | _ => (print (re^"\n"); List.app printBSToken bslist; print "\n")
        fun printRecordToken i =
          if i<0 then ()
          else
            let
              val _ = printRecordToken (i-1)
              val re = List.nth(records, i)
              val bslist = List.nth(bsll2, i)
              val _ = print ("record "^(Int.toString i)^":\n")
            in
              case bslist of
                  [] => (print (re^"\n"); print "no tokenization result\n")
                | _ => (print (re^"\n"); List.app printBSToken bslist; print "\n")
            end
(*        val _ = ListPair.appEq printListPair (records, bsll2) *)
(*        val _ = printRecordToken (List.length records-1) *)
	in
      evaluate bsll1 bsll2
	end

    fun computeProbStructure_HMMonly fileName endingtimes: NewTy * Times.EndingTimes = 
	let
        val records = loadFiles fileName
	    val tokenss = loadFile "testing/output" 
        val _ = print "Tokenization by HMM.\n"
        val otable = constrOrdBTokenTable "training/"
        fun extractIntList record : BToken list = 
          let
            fun isSpace c = c = #" "
            fun doOne s : BToken list = 
              let
                val (pre, rest) = Substring.splitl (not o isSpace) s
                val ret = intToBToken(Option.valOf(Int.fromString(Substring.string pre)), otable) handle Option => (print ("s = "^(Substring.string pre)^"\n"); raise Option)
                val newrest = Substring.triml 1 rest
              in
                if (Substring.size newrest)=0 orelse (isSpace (Substring.sub(newrest, 0))) then [ret]
                else if (Substring.size pre) = (Substring.size s) then [ret]
                else ret::(doOne (Substring.triml 1 rest)) 
              end
          in
            if String.compare(record, "nil") = EQUAL then []
            else doOne (Substring.full record)
          end
        val tokensi = List.map extractIntList tokenss
        fun collapse (record: string, tks: BToken list) : char list list * BToken list = 
          let
            val chars = String.explode record
            fun doOne (c, t, (pret, (retc, rett))) = 
              case t of
                  PPpunc p => (t, (retc@[[c]], rett@[t]))
                | _ => if compBToken(t, pret)=EQUAL then (t, ((List.take(retc, (List.length retc)-1)@[(List.last retc)@[c]]), rett))
                       else (t, (retc@[[c]], rett@[t])) 
          in
            case tks of
                [] => ([[]], [])
              | hd::tl => let val (junk, list) = ListPair.foldlEq doOne (hd, ([[List.hd chars]], [hd])) (List.tl chars, tl) in list end
          end
        val ret = ListPair.mapEq collapse (records, tokensi)
        fun adjust (charll, blist) : BSToken list =
          let
            fun combineOne (charl, bt) = (bt, String.implode charl)
          in
            case blist of
                [] => []
              | _ => ListPair.mapEq combineOne (charll, blist)
          end
        val bsll2 : BSToken list list = List.map adjust ret
        fun addFakeProb bsll = 
          let
            fun bs2bsl (bsl : BSToken list, (bsllist, i)) : NewContext list * int =
              let
                fun doOne ((b,s) : BSToken, (oldlist, p)) = 
                  let
                    val thisbsl = ((b, s), {lineNo = i, beginloc = p, endloc = p+(String.size s)-1, recNo = i})
                  in
                    (oldlist@[thisbsl], p+(String.size s))
                  end
                val (retbsl, junk) = List.foldl doOne ([], 0) bsl
              in
                (bsllist@[retbsl], i+1)
              end 
            val (retcl, junk) = List.foldl bs2bsl ([], 0) bsll
          in
            retcl
          end
        val rtokens : NewContext list = addFakeProb bsll2
        val end2Times = Times.updateTokenEnd (Time.now()) endingtimes
        val newty = SeqsetListToTy_HMM 0 rtokens 
        val snewty = simplifyNewTy newty
        val end3Times = Times.updateStructEnd (Time.now()) end2Times
	in
      (snewty, end3Times)
	end

    fun evaluateVanillaResult fileName  = 
	let
        val records = loadFiles fileName
	    val recordnum = ref (List.length records)
	    val rtokens : Context list = List.map (Structure.ltokenizeRecord recordnum) records 
        val _ = print "Tokenization by vanilla:\n"
        val bsll1 = extractLog "training/log/" "training/log/testname"
	in
      evaluate_vanilla bsll1 rtokens
	end

    fun moveIncToList path file1 file2 =
      let
        val infilesraw = loadFile (path^file1)
        fun loadOne (str, ret) = 
          if Char.compare(#"#", String.sub(str, 0))=EQUAL then ret
          else ret@[str]
        val infiles = List.foldl loadOne [] infilesraw
        val outstrm = TextIO.openAppend (path^file2)
        fun outputOne file = TextIO.output(outstrm, (file^"\n"))
        val _ = List.app outputOne infiles
      in
        TextIO.closeOut outstrm
      end

end
