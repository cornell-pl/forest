structure Viterbi =
struct
    open Types
    open Basetokens
    open Fvector
    open Common
    open Ghmm_features

    fun compPosBToken ((p1, t1), (p2, t2)) = 
      case Int.compare(p1, p2) of
          EQUAL => compBToken(t1, t2)
        | LESS => LESS
        | GREATER => GREATER

    structure BasicViterbiTable = RedBlackMapFn(
      struct type ord_key = int*BToken
			 val compare = compPosBToken
		     end
    ) 

    fun compIntList (l1, l2) =
      case Int.compare(List.length l1, List.length l2) of
          LESS => LESS
        | GREATER => GREATER
        | EQUAL => 
            let
              fun mycompare (i1, i2, result) = 
                case result of
                    EQUAL => Int.compare(i1, i2)
                  | LESS => LESS
                  | GREATER => GREATER
            in
              ListPair.foldl mycompare EQUAL (l1, l2)
            end


    structure IntListTable = RedBlackMapFn(
      struct type ord_key = int list
			 val compare = compIntList
		     end
    ) 

    exception ViterbiError

    fun defaultVal v =
      case v of
          NONE => 0.0 (* a parameter to tune *)
        | SOME n => Real.fromInt n

    fun defaultRVal v =
      case v of
          NONE => 0.0 (* a parameter to tune *)
        | SOME n => n

    fun defaultVal1 v =
      case v of
          NONE => 0.0001 (* a parameter to tune *)
        | SOME n => Real.fromInt n

    fun defaultRVal1 v =
      case v of
          NONE => 0.0001 (* a parameter to tune *)
        | SOME n => n

    fun BTokenPairComp ((t1a,t1b), (t2a,t2b)) = 
	let val r1 = compBToken (t1a, t2a)
	in
	    if  r1 = EQUAL then compBToken (t1b, t2b) else r1
	end

    structure BTokenPairTable = RedBlackMapFn(
                     struct type ord_key = BToken * BToken
			    val compare = BTokenPairComp
		     end) 

    structure BTokenTable = RedBlackMapFn(
                     struct type ord_key = BToken
			    val compare = compBToken
		     end)  

    fun compList (l1, l2) = 
      case Int.compare(List.length l1, List.length l2) of
          GREATER => GREATER
        | LESS => LESS
        | EQUAL => let
                     val len = List.length l1
                     val concat = l1@l2
                     fun comp (i, (rest, ord)) = 
                       case ord of
                           EQUAL => (List.drop(rest, 1), Int.compare(i, List.nth(rest, len)))
                         | GREATER => (rest, GREATER)
                         | LESS => (rest, LESS)
                     val (junk, order) = List.foldl comp (concat, EQUAL) l1 
                   in
                     order
                   end

    fun ListBTokenPairComp ((t1a,t1b), (t2a,t2b)) = 
	let val r1 = compBToken (t1b, t2b)
	in
	    if  r1 = EQUAL then compList (t1a, t2a) else r1
	end

    structure ListBTokenPairTable = RedBlackMapFn(
                     struct type ord_key = (int list)*BToken
			    val compare = ListBTokenPairComp
		     end)  

    fun CharBTokenPairComp ((t1a,t1b), (t2a,t2b)) = 
	let val r1 = compBToken (t1b, t2b)
	in
	    if  r1 = EQUAL then Char.compare (t1a, t2a) else r1
	end

    structure CharBTokenPairTable = RedBlackMapFn(
                     struct type ord_key = char*BToken
			    val compare = CharBTokenPairComp
		     end)  

    fun basicViterbi (tables: BToken IntMap.map * real BTokenTable.map *
                   real BTokenTable.map * real BTokenPairTable.map *
                   real ListBTokenPairTable.map) (ss: Seqset) : NewContext = 
(* use Viterbi to find the best token sequence from the seqset graph *)
      let
        val (endptable, s, lineNo, recNo, sbegin, send) = ss
      in
      if sbegin = ~1 orelse send = ~1 then [((PPempty, ""), mkLoc ~1 ~1 recNo lineNo )]
      else
      let
(*val _ = print ("basicViterbi: "^"sbegin = "^(Int.toString sbegin)^" send = "^(Int.toString send)^"\n")*)
        val (tokentable, begintokentable, endtokentable, tokenpairtable, listtokentable) = tables
        val reachable = PosBTokenTable.listItemsi endptable
        fun forward ((workingpos, bplist), oldtable) =
          let
            fun doOne (beginp, btoken) =
              let
                val lastlist = PosBTokenTable.find(oldtable, (beginp-1))
(*val _ = print ("beginp = "^(Int.toString beginp)^" endp = "^(Int.toString workingpos)^" btoken = "^(BTokenToName btoken)^"\n")*)
                fun search (((mybp, mybptoken), (lastbp, prob)), ((maxbp, maxtoken), maxprob)) = 
                  let
                    val transprob = Math.ln(defaultRVal(BTokenPairTable.find(tokenpairtable, (mybptoken, btoken))))
                    val thisprob = prob + transprob
                  in
                    if thisprob > maxprob then ((*print ("forward probability 1 = "^(Real.toString prob)^" "^(Real.toString transprob)^"\n");*) ((mybp, mybptoken), thisprob))
                    else if Real.compare(thisprob, maxprob)=EQUAL then 
                      if (BTokenCompleteEnum(mybptoken) < BTokenCompleteEnum(maxtoken)) then ((mybp, mybptoken), thisprob) else ((maxbp, maxtoken), maxprob)
                    else ((*print ("forward probability 2 = "^(Real.toString prob)^" "^(Real.toString transprob)^"\n");*) if Real.compare(maxprob, (~Real.maxFinite))=EQUAL then ((mybp, mybptoken), maxprob) else ((maxbp, maxtoken), maxprob))
                  end
                val (mymax, mymaxprob) = 
                  case lastlist of
                      SOME table1 => 
                        let
                          val prelist = BasicViterbiTable.listItemsi table1 
                          val ((b, t), mprob) = List.foldl search ((sbegin, PPblob), (~Real.maxFinite)) prelist
                        in (SOME (b,t), mprob) end
                    | NONE => if beginp=sbegin then (NONE, 0.0) else (print "1\n"; raise ViterbiError)
                fun probBToken (mybeginp, myendp) = 
                  let
                    fun addOneChar (c, v) =
                      let
                        val l = charToList c
                        val value = defaultRVal(ListBTokenPairTable.find(listtokentable, (l, btoken))) 
                      in
                        (Math.ln value) + v
                      end
                    val emitprob = List.foldl addOneChar 0.0 (String.explode (String.substring (s, mybeginp, myendp-mybeginp+1)))
                    val transprob = if (myendp-mybeginp)=0 then 0.0 
                                    else (Real.fromInt (myendp-mybeginp)) * (Math.ln(defaultRVal(BTokenPairTable.find(tokenpairtable, (btoken, btoken)))))
(*val _ = print ("emitprob = "^(Real.toString emitprob)^" transprob = "^(Real.toString transprob)^"\n") *)
                  in
                    emitprob+transprob
                  end
                val newprob = mymaxprob + probBToken(beginp, workingpos)
(*val _ = print ("beginp = "^(Int.toString beginp)^" btoken = "^(BTokenToName btoken)^" prob = "^(Real.toString newprob)^" preprob = "^(Real.toString mymaxprob)^"\n")*)
              in
                ((beginp, btoken), mymax, newprob)
              end
            val thislist = List.map doOne bplist
            fun updateTable ((thisbp, lastbp, prob), myoldtable) = BasicViterbiTable.insert(myoldtable, thisbp, (lastbp, prob)) 
            val newtable = PosBTokenTable.insert(oldtable, workingpos, (List.foldl updateTable BasicViterbiTable.empty thislist))
          in
            newtable
          end
        val forwardmsg = List.foldl forward PosBTokenTable.empty reachable
        val fmsglist = PosBTokenTable.listItemsi forwardmsg
(*val _ = print ("fmsglist length = "^(Int.toString (List.length fmsglist))^"\n")*)
        val (lastpos, lastrcd) = List.nth(fmsglist, (List.length fmsglist)-1)
        val lastrcdlist = BasicViterbiTable.listItemsi lastrcd
(*val _ = print ("last position list length = "^(Int.toString (List.length lastrcdlist))^"\n")*)
        fun findlastmax ((thisp, (lastp, prob)), (maxp, maxlastp, maxprob)) =
          if prob>maxprob then ((*print ("probability = "^(Real.toString prob)^"\n");*) (thisp, lastp, prob))
          else ((*print ("probability = "^(Real.toString prob)^"\n");*) (maxp, maxlastp, maxprob))
        val ((lastmaxp, lastmaxt), lastpremaxp, lastmaxprob) = List.foldl findlastmax ((sbegin, PPblob), NONE, (~Real.maxFinite)) lastrcdlist (* lastpremaxp is an option *)
        fun backward ((prep, pret), thisp) = (* return a bsl list *) 
          case PosBTokenTable.find(forwardmsg, thisp) of
              NONE => (print "2\n"; print ((Int.toString thisp)^"\n"); raise ViterbiError)
            | SOME table => case BasicViterbiTable.find(table, (prep, pret)) of
                                NONE => (print ("beginp = "^(Int.toString prep)^" endp = "^(Int.toString thisp)^"\n"); raise ViterbiError) 
                              | SOME (newpre, newprob) => 
                                  case newpre of
                                      NONE => if prep=sbegin then ((*print ("( "^(Int.toString prep)^" "^(Int.toString thisp)^" "^(BTokenToName pret)^")\n");*) [((pret, String.substring(s, prep, thisp-prep+1)), mkLoc prep thisp recNo lineNo)])
                                              else (print "3\n"; raise ViterbiError)
                                    | SOME (newprep, newpret) => if prep = sbegin then (print "6\n"; raise ViterbiError)
                                  else
                                  let
                                    val newbsl = backward((newprep, newpret), (prep-1))
(*val _ = print ("( "^(Int.toString prep)^" "^(Int.toString thisp)^" "^(BTokenToName pret)^")\n")*)
                                  in
                                    newbsl@[((pret, String.substring(s, prep, thisp-prep+1)), mkLoc prep thisp recNo lineNo)]
                                  end             
(*val _ = print "returned newcontext:\n"*)
        val retbsllist = 
          case lastpremaxp of
              NONE => if lastmaxp = sbegin then 
                        if compBToken(PPblob, lastmaxt)=EQUAL then ((*print "all token sequences are of zero probability, return PPblob\n";*) [((lastmaxt, String.substring(s, lastmaxp, send-lastmaxp+1)), mkLoc lastmaxp send recNo lineNo)])
                        else ((*print ("( "^(Int.toString lastmaxp)^" "^(Int.toString send)^" "^(BTokenToName lastmaxt)^")\n");*) [((lastmaxt, String.substring(s, lastmaxp, send-lastmaxp+1)), mkLoc lastmaxp send recNo lineNo)])
                      else (print "4\n"; raise ViterbiError)
            | SOME pre =>
                if lastmaxp = sbegin then (*(print "5\n"; raise ViterbiError)*)
                let
                  val (prep, pret) = pre
(*val _ = print ("prep = "^(Int.toString prep)^" pret = "^(BTokenToName pret)^"\n") *)
                in
                   (print "5\n"; raise ViterbiError)
                end
                else
                ((*print ("( "^(Int.toString lastmaxp)^" "^(Int.toString send)^" "^(BTokenToName lastmaxt)^")\n");*) backward(pre, (lastmaxp-1))@[((lastmaxt, String.substring(s, lastmaxp, send-lastmaxp+1)), mkLoc lastmaxp send recNo lineNo)])
      in
        retbsllist
      end
      end



    fun ViterbiWithInitT (tables: BToken IntMap.map * real BTokenTable.map *
                   real BTokenTable.map * real BTokenPairTable.map *
                   real ListBTokenPairTable.map) (initT: BSToken) (ss: Seqset) : NewContext = 
(* use Viterbi to find the best token sequence from the seqset graph *)
      let
        val (endptable, s, lineNo, recNo, sbegin, send) = ss
      in
      if sbegin = ~1 orelse send = ~1 then if compBSToken(initT, (PPempty, ""))= EQUAL then [((PPempty, ""), mkLoc ~1 ~1 recNo lineNo )] else raise ViterbiError
      else
      let
(*val _ = print ("ViterbiWithInitT: "^"sbegin = "^(Int.toString sbegin)^" send = "^(Int.toString send)^" initT = "^(BTokenToName(#1(initT)))^"\n")*)
        val (tokentable, begintokentable, endtokentable, tokenpairtable, listtokentable) = tables
        val reachable = PosBTokenTable.listItemsi endptable
        fun forward ((workingpos, bplist), oldtable) =
          let
            fun doOne (beginp, btoken) =
              let
                val lastlist = PosBTokenTable.find(oldtable, (beginp-1))
(*val _ = print ("beginp = "^(Int.toString beginp)^" endp = "^(Int.toString workingpos)^" btoken = "^(BTokenToName btoken)^"\n")*)
                fun search (((mybp, mybptoken), (lastbp, prob)), ((maxbp, maxtoken), maxprob)) = 
                  let
                    val transprob = Math.ln(defaultRVal(BTokenPairTable.find(tokenpairtable, (mybptoken, btoken))))
                    val thisprob = prob + transprob
                  in
                    if thisprob > maxprob then ((*print ("forward probability 1 = "^(Real.toString prob)^" "^(Real.toString transprob)^"\n");*) ((mybp, mybptoken), thisprob))
                    else if Real.compare(thisprob, maxprob)=EQUAL then 
                      if (BTokenCompleteEnum(mybptoken) < BTokenCompleteEnum(maxtoken)) then ((mybp, mybptoken), thisprob) else ((maxbp, maxtoken), maxprob)
                    else ((*print ("forward probability 2 = "^(Real.toString prob)^" "^(Real.toString transprob)^"\n");*) if Real.compare(maxprob, (~Real.maxFinite))=EQUAL then ((mybp, mybptoken), maxprob) else ((maxbp, maxtoken), maxprob))
                  end
                val (mymax, mymaxprob) = 
                  case lastlist of
                      SOME table1 => 
                        let
                          val prelist = BasicViterbiTable.listItemsi table1 
                          val ((b, t), mprob) = List.foldl search ((0, PPblob), (~Real.maxFinite)) prelist
                        in (SOME (b,t), mprob) end
                    | NONE => if beginp=sbegin then 
                                if compBSToken(initT, (btoken, ""))=EQUAL then (NONE, 0.0)
                                else (NONE, (~Real.maxFinite)) 
                              else (print "1\n"; raise ViterbiError)
                fun probBToken (mybeginp, myendp) = 
                  let
                    fun addOneChar (c, v) =
                      let
                        val l = charToList c
                        val value = defaultRVal(ListBTokenPairTable.find(listtokentable, (l, btoken))) 
                      in
                        (Math.ln value) + v
                      end
                    val emitprob = List.foldl addOneChar 0.0 (String.explode (String.substring (s, mybeginp, myendp-mybeginp+1)))
                    val transprob = if (myendp-mybeginp)=0 then 0.0 
                                    else (Real.fromInt (myendp-mybeginp)) * (Math.ln(defaultRVal(BTokenPairTable.find(tokenpairtable, (btoken, btoken)))))
(*val _ = print ("emitprob = "^(Real.toString emitprob)^" transprob = "^(Real.toString transprob)^"\n") *)
                  in
                    emitprob+transprob
                  end
                val newprob = mymaxprob + probBToken(beginp, workingpos)
(*val _ = print ("beginp = "^(Int.toString beginp)^" btoken = "^(BTokenToName btoken)^" prob = "^(Real.toString newprob)^" preprob = "^(Real.toString mymaxprob)^"\n")*)
              in
                ((beginp, btoken), mymax, newprob)
              end
            val thislist = List.map doOne bplist
            fun updateTable ((thisbp, lastbp, prob), myoldtable) = BasicViterbiTable.insert(myoldtable, thisbp, (lastbp, prob)) 
            val newtable = PosBTokenTable.insert(oldtable, workingpos, (List.foldl updateTable BasicViterbiTable.empty thislist))
          in
            newtable
          end
        val forwardmsg = List.foldl forward PosBTokenTable.empty reachable
        val fmsglist = PosBTokenTable.listItemsi forwardmsg
(*val _ = print ("fmsglist length = "^(Int.toString (List.length fmsglist))^"\n")*)
        val (lastpos, lastrcd) = List.nth(fmsglist, (List.length fmsglist)-1)
        val lastrcdlist = BasicViterbiTable.listItemsi lastrcd
(*val _ = print ("last position list length = "^(Int.toString (List.length lastrcdlist))^"\n")*)
        fun findlastmax ((thisp, (lastp, prob)), (maxp, maxlastp, maxprob)) =
          if prob>maxprob then ((*print ("choose probability = "^(Real.toString prob)^"\n");*) (thisp, lastp, prob))
          else ((*print ("ignore probability = "^(Real.toString prob)^"\n");*) (maxp, maxlastp, maxprob))
        val ((lastmaxp, lastmaxt), lastpremaxp, lastmaxprob) = List.foldl findlastmax ((sbegin, PPblob), NONE, (~Real.maxFinite)) lastrcdlist (* lastpremaxp is an option *)
        fun backward ((prep, pret), thisp) = (* return a bsl list *) 
          case PosBTokenTable.find(forwardmsg, thisp) of
              NONE => (print "2\n"; print ((Int.toString thisp)^"\n"); raise ViterbiError)
            | SOME table => case BasicViterbiTable.find(table, (prep, pret)) of
                                NONE => (print ("beginp = "^(Int.toString prep)^" endp = "^(Int.toString thisp)^"\n"); raise ViterbiError) 
                              | SOME (newpre, newprob) => 
                                  case newpre of
                                      NONE => if prep=sbegin then ((*print ("( "^(Int.toString prep)^" "^(Int.toString thisp)^" "^(BTokenToName pret)^")\n");*) [((pret, String.substring(s, prep, thisp-prep+1)), mkLoc prep thisp recNo lineNo)])
                                              else (print "3\n"; raise ViterbiError)
                                    | SOME (newprep, newpret) => if prep = sbegin then (print "6\n"; raise ViterbiError)
                                  else
                                  let
                                    val newbsl = backward((newprep, newpret), (prep-1))
(*val _ = print ("( "^(Int.toString prep)^" "^(Int.toString thisp)^" "^(BTokenToName pret)^")\n")*)
                                  in
                                    newbsl@[((pret, String.substring(s, prep, thisp-prep+1)), mkLoc prep thisp recNo lineNo)]
                                  end             
(*val _ = print "returned newcontext:\n"*)
        val retbsllist = 
          case lastpremaxp of
              NONE => if lastmaxp = sbegin then ((*print ("( "^(Int.toString lastmaxp)^" "^(Int.toString send)^" "^(BTokenToName lastmaxt)^")\n");*) [((lastmaxt, String.substring(s, lastmaxp, send-lastmaxp+1)), mkLoc lastmaxp send recNo lineNo)])
                      else (print "4\n"; raise ViterbiError)
            | SOME pre =>
                if lastmaxp = sbegin then (*(print "5\n"; raise ViterbiError)*)
                let
                  val (prep, pret) = pre
(*val _ = print ("prep = "^(Int.toString prep)^" pret = "^(BTokenToName pret)^"\n") *)
                in
                   (print "5\n"; raise ViterbiError)
                end
                else
                ((*print ("( "^(Int.toString lastmaxp)^" "^(Int.toString send)^" "^(BTokenToName lastmaxt)^")\n");*) backward(pre, (lastmaxp-1))@[((lastmaxt, String.substring(s, lastmaxp, send-lastmaxp+1)), mkLoc lastmaxp send recNo lineNo)])
      in
        retbsllist
      end
      end


   structure BSTokenTable = RedBlackMapFn(
                     struct type ord_key = BSToken
			    val compare = compBSToken
		     end) 



    fun ViterbiWithSummary (ss: Seqset) tables summary : NewContext option = (* summary: (token, freq) list *)
      let
(*val _ = print "ViterbiWithSummary\n"
fun printsum ((token, s), freq) = print (BTokenToName(token)^" : "^(Int.toString freq)^", ")
val _ = List.app printsum summary
val _ = print "\n"
*)
        val (endptable, s, lineNo, recNo, sbegin, send) = ss
(*val _ = print ("beginp = "^(Int.toString sbegin)^" endp = "^(Int.toString send)^" "(*^(String.substring (s, sbegin, send-sbegin+1))*)^"\n")*)
        fun insertOne ((token,freq), (tTable, index)) = (BSTokenTable.insert(tTable, token, (freq, index)), index+1)
		val (summaryTable, junk) = List.foldl insertOne (BSTokenTable.empty, 0) summary
        val sumlist = BSTokenTable.listItems summaryTable
        val sumlength = List.length sumlist
        fun allzero (freq, index) = (freq=0)
      in
        if sbegin = ~1 orelse send = ~1 then if (List.all allzero sumlist) then SOME [((PPempty, ""), mkLoc ~1 ~1 recNo lineNo )] else NONE 
      else
      let
        val (tokentable, begintokentable, endtokentable, tokenpairtable, listtokentable) = tables
        val reachable = PosBTokenTable.listItemsi endptable
        fun forward ((workingpos, bplist), thisoldtable) =
          let
            fun doOne ((beginp, btoken), oldtable) =
              let
                fun probBToken (mybeginp, myendp) = 
                  let
                    fun addOneChar (c, v) =
                      let
                        val l = charToList c
                        val value = defaultRVal(ListBTokenPairTable.find(listtokentable, (l, btoken))) 
                      in
                        (Math.ln value) + v
                      end
                    val emitprob = List.foldl addOneChar 0.0 (String.explode (String.substring (s, mybeginp, myendp-mybeginp+1)))
                    val transprob = if (myendp-mybeginp)=0 then 0.0
                                    else (Real.fromInt (myendp-mybeginp)) * (Math.ln(defaultRVal(BTokenPairTable.find(tokenpairtable, (btoken, btoken))))) 
                  in
                    emitprob+transprob
                  end
              in
              case PosBTokenTable.find(oldtable, (beginp-1)) of
                  NONE => ((*print "can't find a pre table\n";*)
                          if beginp = sbegin then ((*print "=begin\n";*)
                            case BSTokenTable.find(summaryTable, (btoken, "")) of
                                NONE => (* this token is not in the summary *)
                                  let
(*val _ = print "this token is not in the summary\n"*)
                                    fun constrIntList i = if i = 1 then [0] else 0::constrIntList (i-1)
                                    val mykey = constrIntList sumlength
                                    val (myoldtable, oldtable1) = case PosBTokenTable.find(oldtable, workingpos) of
                                                        NONE => (oldtable, IntListTable.empty)
                                                      | SOME ot => PosBTokenTable.remove(oldtable, workingpos)
                                    val mytable1 = 
                                          case IntListTable.find(oldtable1, mykey) of
                                              NONE => IntListTable.insert(oldtable1, mykey, BasicViterbiTable.insert(BasicViterbiTable.empty, (beginp, btoken), (NONE, probBToken(beginp, workingpos))))
                                            | SOME oldtable2 => 
                                                let
                                                  val (removet, junk) = IntListTable.remove(oldtable1, mykey)
                                                in
                                                  IntListTable.insert(removet, mykey, BasicViterbiTable.insert(oldtable2, (beginp, btoken), (NONE, probBToken(beginp, workingpos))))
                                                end          
                                  in
                                    PosBTokenTable.insert(myoldtable, workingpos, mytable1)
                                  end
                              | SOME (myfreq, myindex) =>
                                  if myfreq < 1 then (print "1\n"; raise ViterbiError) (* assume freq > 0 *)
                                  else 
                                  let
(*val _ = print "this token is in the summary\n"*)
                                    fun constrIntList i = if i = 0 then if i=myindex then [1] else [0] 
                                                          else if i=myindex then 1:: constrIntList (i-1) else 0::constrIntList (i-1)
                                    val mykey = constrIntList (sumlength-1)
                                    val (myoldtable, oldtable1) = case PosBTokenTable.find(oldtable, workingpos) of
                                                        NONE => (oldtable, IntListTable.empty)
                                                      | SOME ot => PosBTokenTable.remove(oldtable, workingpos)
                                    val mytable1 = 
                                          case IntListTable.find(oldtable1, mykey) of
                                              NONE => IntListTable.insert(oldtable1, mykey, BasicViterbiTable.insert(BasicViterbiTable.empty, (beginp, btoken), (NONE, probBToken(beginp, workingpos))))
                                            | SOME oldtable2 => 
                                                let
                                                  val (removet, junk) = IntListTable.remove(oldtable1, mykey)
                                                in
                                                  IntListTable.insert(removet, mykey, BasicViterbiTable.insert(oldtable2, (beginp, btoken), (NONE, probBToken(beginp, workingpos))))
                                                end          
(*val _ = print ("insert a table at pos: "^(Int.toString workingpos)^"\n")*)
                                  in
                                    PosBTokenTable.insert(myoldtable, workingpos, mytable1)
                                  end)
                          else (print ("2: not = begin, beginp = "^(Int.toString beginp)^"\n"); raise ViterbiError))

                 | SOME pretable1 =>
                    let
(*val _ = print "get a pre table\n"*)
                      val preslist = IntListTable.listItemsi pretable1
                    in
                      case BSTokenTable.find(summaryTable, (btoken, String.substring(s, beginp, workingpos-beginp+1))) of
                          NONE => (* repeat prelist *)
                            let
                              fun doOneIntList ((ilist, junk), oldtable1) =
                                let
                                  val lasttable = IntListTable.find(pretable1, ilist)
                                  fun search (((mybp, mybptoken), (lastbp, prob)), ((maxbp, maxtoken), maxprob)) = 
                                    let
                                       val transprob = Math.ln(defaultRVal(BTokenPairTable.find(tokenpairtable, (mybptoken, btoken))))
                                       val thisprob = prob + transprob
                                    in
                                      if thisprob > maxprob then ((mybp, mybptoken), thisprob)
                                      else if Real.compare(thisprob, maxprob)=EQUAL then 
                                        if (BTokenCompleteEnum(mybptoken) < BTokenCompleteEnum(maxtoken)) then ((mybp, mybptoken), thisprob) else ((maxbp, maxtoken), maxprob)
                                      else if Real.compare(maxprob, (~Real.maxFinite))=EQUAL then ((mybp, mybptoken), maxprob) else ((maxbp, maxtoken), maxprob)
                                    end
                                  val (mymax, mymaxprob) = 
                                    case lasttable of
                                    SOME table1 => 
                                      let
                                        val prelist = BasicViterbiTable.listItemsi table1 
                                        val ((b, t), mprob) = List.foldl search ((sbegin, PPblob), (~Real.maxFinite)) prelist
                                      in (SOME (b,t), mprob) end
                                  | NONE => (print "3\n"; raise ViterbiError)
                                in
                                  case IntListTable.find(oldtable1, ilist) of
                                      NONE => IntListTable.insert(oldtable1, ilist, BasicViterbiTable.insert(BasicViterbiTable.empty, (beginp, btoken), (mymax, mymaxprob)))
                                    | SOME oldtable2 => IntListTable.insert(oldtable1, ilist, BasicViterbiTable.insert(oldtable2, (beginp, btoken), (mymax, mymaxprob)))
                                end
                              val (myoldtable, myoldtable1) = case PosBTokenTable.find(oldtable, workingpos) of
                                                                NONE => (oldtable, IntListTable.empty)
                                                              | SOME ot => PosBTokenTable.remove(oldtable, workingpos)
                              val mytable1 = List.foldl doOneIntList myoldtable1 preslist 
                            in
                              PosBTokenTable.insert(myoldtable, workingpos, mytable1)
                            end
                        | SOME (myfreq, myindex) => (* prelist + 1 *)
                            let
                              fun doOneIntList ((preilist, junk), oldtable1) =
                                if (List.nth(preilist, myindex)+1) > myfreq then oldtable1
                                else
                                let
                                  (*fun constrIntList i = if i = myindex then i+1 else i*)
                                  fun constrIntList (i, (thisindex, retlist)) = if thisindex=myindex then (thisindex+1, retlist@[(i+1)]) else (thisindex+1, retlist@[i])
                                  (*val ilist = List.map constrIntList preilist*)
                                  val (junk, ilist) = List.foldl constrIntList (0, []) preilist
                                  val lasttable = IntListTable.find(pretable1, preilist)
                                  fun search (((mybp, mybptoken), (lastbp, prob)), ((maxbp, maxtoken), maxprob)) = 
                                    let
                                       val transprob = Math.ln(defaultRVal(BTokenPairTable.find(tokenpairtable, (mybptoken, btoken))))
                                       val thisprob = prob + transprob
                                    in
                                      if thisprob > maxprob then ((mybp, mybptoken), thisprob)
                                      else if Real.compare(thisprob, maxprob)=EQUAL then 
                                        if (BTokenCompleteEnum(mybptoken) < BTokenCompleteEnum(maxtoken)) then ((mybp, mybptoken), thisprob) else ((maxbp, maxtoken), maxprob)
                                      else if Real.compare(maxprob, (~Real.maxFinite))=EQUAL then ((mybp, mybptoken), maxprob) else ((maxbp, maxtoken), maxprob)
                                    end
                                  val (mymax, mymaxprob) = 
                                    case lasttable of
                                    SOME table1 => 
                                      let
                                        val prelist = BasicViterbiTable.listItemsi table1 
                                        val ((b, t), mprob) = List.foldl search ((sbegin, PPblob), (~Real.maxFinite)) prelist
                                      in (SOME (b,t), mprob) end
                                  | NONE => (print "4\n"; raise ViterbiError)
                                in
                                  case IntListTable.find(oldtable1, ilist) of
                                      NONE => IntListTable.insert(oldtable1, ilist, BasicViterbiTable.insert(BasicViterbiTable.empty, (beginp, btoken), (mymax, mymaxprob)))
                                    | SOME oldtable2 => IntListTable.insert(oldtable1, ilist, BasicViterbiTable.insert(oldtable2, (beginp, btoken), (mymax, mymaxprob)))
                                end
                              val (myoldtable, myoldtable1) = case PosBTokenTable.find(oldtable, workingpos) of
                                                                NONE => (oldtable, IntListTable.empty)
                                                              | SOME ot => PosBTokenTable.remove(oldtable, workingpos)
                              val mytable1 = List.foldl doOneIntList myoldtable1 preslist 
                            in
                              PosBTokenTable.insert(myoldtable, workingpos, mytable1)
                            end
                    end
              end
(*val _ = print ("workingpos = "^(Int.toString workingpos)^"\n")*)
            val thistable = List.foldl doOne thisoldtable bplist
(*val _ = print "after doOne\n"*)
          in 
            thistable
          end
        val forwardmsg = List.foldl forward PosBTokenTable.empty reachable 
(*val _ = print "still in\n"*)
        val fmsglist = PosBTokenTable.listItemsi forwardmsg
(*val _ = print "forward done\n"*)
      in 
        if List.length fmsglist = 0 then NONE
        else
        let 
        val (lastpos, lasttable) = List.nth(fmsglist, (List.length fmsglist)-1)
        fun constrTarget (freq, index) = freq
        val targetsummary = List.map constrTarget sumlist
        in
          case IntListTable.find(lasttable, targetsummary) of
              NONE => NONE
            | SOME table2 =>
        let 
        val lastrcdlist = BasicViterbiTable.listItemsi table2
        fun findlastmax ((thisp, (lastp, prob)), (maxp, maxlastp, maxprob)) =
          if prob>maxprob then (thisp, lastp, prob)
          else (maxp, maxlastp, maxprob)
        val ((lastmaxp, lastmaxt), lastpremaxp, lastmaxprob) = List.foldl findlastmax ((sbegin, PPblob), NONE, (~Real.maxFinite)) lastrcdlist (* lastpremaxp is an option *)
        fun backward ((beginp, btoken), endp, ilist) (*((prep, pret), thisp)*) = (* return a bsl list *) 
          case PosBTokenTable.find(forwardmsg, endp) of
              NONE => (print ("5: endp = "^(Int.toString endp)^" beginp = "^(Int.toString beginp)^"\n"); raise ViterbiError)
            | SOME table1 => 
                let
                  val myilist = case BSTokenTable.find(summaryTable, (btoken, String.substring(s, beginp, endp-beginp+1))) of
                                    NONE => ilist
                                  | SOME (myfreq, myindex) =>
                                      let
                                        fun minusOne i = if i = 0 then if i = myindex then [List.nth(ilist, 0)-1] else [List.nth(ilist, 0)] 
                                                         else if i = myindex then minusOne(i-1)@[(List.nth(ilist, i)-1)](*(List.nth(ilist, i)-1)::minusOne (i-1)*)
                                                                             else minusOne(i-1)@[List.nth(ilist, i)]
                                      in 
                                        minusOne (List.length ilist-1)
                                      end
                  fun printilist i = print ((Int.toString i)^" ")
                in
                  case IntListTable.find(table1, ilist) of
                      NONE => (print ("6: endp = "^(Int.toString endp)^" "); List.app printilist ilist; print "\n"; raise ViterbiError)
                    | SOME table2 =>
                           case BasicViterbiTable.find(table2, (beginp, btoken)) of
                               NONE => (print "7\n"; raise ViterbiError) 
                             | SOME (newpre, newprob) => 
                                  case newpre of
                                      NONE => if beginp=sbegin then [((btoken, String.substring(s, beginp, endp-beginp+1)), mkLoc beginp endp recNo lineNo)]
                                              else (print "8\n"; raise ViterbiError)
                                    | SOME (newprep, newpret) =>
                                        let
                                          val newbsl = backward((newprep, newpret), (beginp-1), myilist)
                                        in
                                          newbsl@[((btoken, String.substring(s, beginp, endp-beginp+1)), mkLoc beginp endp recNo lineNo)]
                                        end             
                end
        val rest = case lastpremaxp of
                       NONE => if lastmaxp = sbegin then []
                             else (print "9\n"; raise ViterbiError)
                     | SOME lastpremaxpp =>
( (*print ("lastp: "^(BTokenToName(lastmaxt))^" "^"prep: "^(Int.toString (#1(lastpremaxpp)))^" "^(BTokenToName (#2(lastpremaxpp)))^"\n");*)
                   case BSTokenTable.find(summaryTable, (lastmaxt, "")) of
                       NONE => backward(lastpremaxpp, (lastmaxp-1), targetsummary)
                     | SOME (freq, index) =>
                         let
                           fun minusOne i = if i = 0 then if i = index then [List.nth(targetsummary, 0)-1] else [List.nth(targetsummary, 0)] 
                                            else if i = index then minusOne(i-1)@[(List.nth(targetsummary, i)-1)]
                                                              else minusOne(i-1)@[List.nth(targetsummary, i)]
                         in
                           backward(lastpremaxpp, (lastmaxp-1), minusOne(List.length targetsummary-1))
                         end
)
        val retbsllist = rest@[((lastmaxt, String.substring(s, lastmaxp, send-lastmaxp+1)), mkLoc lastmaxp send recNo lineNo)]
(*val _ = print "backward done\n"*)
        in
          case lastpremaxp of
              NONE => if compBToken(PPblob, lastmaxt)=EQUAL then NONE else SOME retbsllist
            | SOME _ => SOME retbsllist
        end
        end
      end
      end

    fun initToken (ss: Seqset) (* return a list of init bstokens, but we don't care the string *) = 
      let
        val (endptable, s, lineNo, recNo, sbegin, send) = ss
        val endplist = PosBTokenTable.listItemsi endptable
        fun searchOne ((endp, bplist), list) = 
          let
            fun so (beginp, btoken) = if beginp = sbegin then true else false 
          in
            list@(List.filter so bplist)
          end
        val valid = List.foldl searchOne [] endplist
        fun convert (beginp, btoken) = (btoken, "")
      in
        List.map convert valid
      end


    fun basicViterbi_GHMM ghmmmodel (ss: Seqset) : NewContext = 
      let
        val (endptable, s, lineNo, recNo, sbegin, send) = ss
      in
      if sbegin = ~1 orelse send = ~1 then [((PPempty, ""), mkLoc ~1 ~1 recNo lineNo )]
      else
      let
(*val _ = print ("basicViterbi_GHMM: "^"sbegin = "^(Int.toString sbegin)^" send = "^(Int.toString send)^"\n")*)
        val reachable = PosBTokenTable.listItemsi endptable
        fun forward ((workingpos, bplist), oldtable) =
          let
            fun doOne (beginp, btoken) =
              let
                val lastlist = PosBTokenTable.find(oldtable, (beginp-1))
(*val _ = print ("beginp = "^(Int.toString beginp)^" endp = "^(Int.toString workingpos)^" btoken = "^(BTokenToName btoken)^"\n")*)
                val ret = case lastlist of
                             NONE => if beginp = sbegin then (beginp, btoken, Math.ln(tokenProb(btoken, s, beginp, workingpos, ghmmmodel)))
                                    else raise ViterbiError
                           | SOME (prep, pret, preprob) => (beginp, btoken, preprob + Math.ln(tokenProb(btoken, s, beginp, workingpos, ghmmmodel)))
              in
                ret
              end
            val problist = List.map doOne bplist
            fun search ((mybp, mybptoken, myprob), (maxbp, maxtoken, maxprob)) = 
              if myprob > maxprob then (mybp, mybptoken, myprob)
              else if Real.compare(myprob, maxprob)=EQUAL then 
                      if (BTokenCompleteEnum(mybptoken) < BTokenCompleteEnum(maxtoken)) then (mybp, mybptoken, myprob) else (maxbp, maxtoken, maxprob)
              else if Real.compare(maxprob, (~Real.maxFinite))=EQUAL then (mybp, mybptoken, maxprob) else (maxbp, maxtoken, maxprob)
            val myret = List.foldl search (sbegin, PPblob, ~Real.maxFinite) problist
            val newtable = PosBTokenTable.insert(oldtable, workingpos, myret)
          in
            newtable
          end
        val forwardmsg = List.foldl forward PosBTokenTable.empty reachable
        fun backward endp =
          if endp + 1 = sbegin then []
          else
            case PosBTokenTable.find(forwardmsg, endp) of
                NONE => raise ViterbiError
              | SOME (beginp, btoken, prob) => backward(beginp-1)@[((btoken, String.substring(s, beginp, endp-beginp+1)), mkLoc beginp endp recNo lineNo)]
        val retbsllist = backward send 
      in
        retbsllist
      end
      end


    fun basicViterbi_GHMM_trans ghmmmodel tokenpairtable (ss: Seqset) : NewContext = 
      let
        val (endptable, s, lineNo, recNo, sbegin, send) = ss
      in
      if sbegin = ~1 orelse send = ~1 then [((PPempty, ""), mkLoc ~1 ~1 recNo lineNo )]
      else
      let
(*val _ = print ("basicViterbi_GHMM_trans: "^"sbegin = "^(Int.toString sbegin)^" send = "^(Int.toString send)^"\n")*)
        val reachable = PosBTokenTable.listItemsi endptable
        fun forward ((workingpos, bplist), oldtable) =
          let
            fun doOne (beginp, btoken) =
              let
                val lastlist = PosBTokenTable.find(oldtable, (beginp-1))
(*val _ = print ("beginp = "^(Int.toString beginp)^" endp = "^(Int.toString workingpos)^" btoken = "^(BTokenToName btoken)^"\n")*)
                fun search (((mybp, mybptoken), (lastbp, prob)), ((maxbp, maxtoken), maxprob)) = 
                  let
                    val mybptokenc = BToken2BTokenClass mybptoken
                    val btokenc = BToken2BTokenClass btoken
                    val transprob = if ( !ghmm5 = true ) then Math.ln(defaultRVal(BTokenPairTable.find(tokenpairtable, (mybptokenc, btokenc))))
                                    else Math.ln(defaultRVal(BTokenPairTable.find(tokenpairtable, (mybptoken, btoken))))
                    val thisprob = prob + transprob
                  in
                    if thisprob > maxprob then ((*print ("forward probability 1 = "^(Real.toString prob)^" "^(Real.toString transprob)^"\n");*) ((mybp, mybptoken), thisprob))
                    else if Real.compare(thisprob, maxprob)=EQUAL then 
                      if (BTokenCompleteEnum(mybptoken) < BTokenCompleteEnum(maxtoken)) then ((mybp, mybptoken), thisprob) else ((maxbp, maxtoken), maxprob)
                    else ((*print ("forward probability 2 = "^(Real.toString prob)^" "^(Real.toString transprob)^"\n");*) if Real.compare(maxprob, (~Real.maxFinite))=EQUAL then ((mybp, mybptoken), maxprob) else ((maxbp, maxtoken), maxprob))
                  end
                val (mymax, mymaxprob) = 
                  case lastlist of
                      SOME table1 => 
                        let
                          val prelist = BasicViterbiTable.listItemsi table1 
                          val ((b, t), mprob) = List.foldl search ((sbegin, PPblob), (~Real.maxFinite)) prelist
                        in (SOME (b,t), mprob) end
                    | NONE => if beginp=sbegin then (NONE, 0.0) else (print "1\n"; raise ViterbiError)
                val newprob = mymaxprob + Math.ln(tokenProb(btoken, s, beginp, workingpos, ghmmmodel))
(*val _ = print ("beginp = "^(Int.toString beginp)^" btoken = "^(BTokenToName btoken)^" prob = "^(Real.toString newprob)^" preprob = "^(Real.toString mymaxprob)^"\n")*)
              in
                ((beginp, btoken), mymax, newprob)
              end
            val thislist = List.map doOne bplist
            fun updateTable ((thisbp, lastbp, prob), myoldtable) = BasicViterbiTable.insert(myoldtable, thisbp, (lastbp, prob)) 
            val newtable = PosBTokenTable.insert(oldtable, workingpos, (List.foldl updateTable BasicViterbiTable.empty thislist))
          in
            newtable
          end
        val forwardmsg = List.foldl forward PosBTokenTable.empty reachable
        val fmsglist = PosBTokenTable.listItemsi forwardmsg
(*val _ = print ("fmsglist length = "^(Int.toString (List.length fmsglist))^"\n")*)
        val (lastpos, lastrcd) = List.nth(fmsglist, (List.length fmsglist)-1)
        val lastrcdlist = BasicViterbiTable.listItemsi lastrcd
(*val _ = print ("last position list length = "^(Int.toString (List.length lastrcdlist))^"\n")*)
        fun findlastmax ((thisp as (thispp, thispt), (lastp, prob)), (maxp, maxlastp, maxprob)) =
          if prob>maxprob then ((*print ("beginp = "^(Int.toString thispp)^" token = "^(BTokenToName thispt)^" probability = "^(Real.toString prob)^"\n");*) (thisp, lastp, prob))
          else ((*print ("beginp = "^(Int.toString thispp)^" token = "^(BTokenToName thispt)^" probability = "^(Real.toString prob)^"\n");*) (maxp, maxlastp, maxprob))
        val ((lastmaxp, lastmaxt), lastpremaxp, lastmaxprob) = List.foldl findlastmax ((sbegin, PPblob), NONE, (~Real.maxFinite)) lastrcdlist (* lastpremaxp is an option *)
        fun backward ((prep, pret), thisp) = (* return a bsl list *) 
          case PosBTokenTable.find(forwardmsg, thisp) of
              NONE => (print "2\n"; print ((Int.toString thisp)^"\n"); raise ViterbiError)
            | SOME table => case BasicViterbiTable.find(table, (prep, pret)) of
                                NONE => (print ("beginp = "^(Int.toString prep)^" endp = "^(Int.toString thisp)^"\n"); raise ViterbiError) 
                              | SOME (newpre, newprob) => 
                                  case newpre of
                                      NONE => if prep=sbegin then ((*print ("( "^(Int.toString prep)^" "^(Int.toString thisp)^" "^(BTokenToName pret)^")\n");*) [((pret, String.substring(s, prep, thisp-prep+1)), mkLoc prep thisp recNo lineNo)])
                                              else (print "3\n"; raise ViterbiError)
                                    | SOME (newprep, newpret) => if prep = sbegin then (print "6\n"; raise ViterbiError)
                                  else
                                  let
                                    val newbsl = backward((newprep, newpret), (prep-1))
(*val _ = print ("( "^(Int.toString prep)^" "^(Int.toString thisp)^" "^(BTokenToName pret)^")\n")*)
                                  in
                                    newbsl@[((pret, String.substring(s, prep, thisp-prep+1)), mkLoc prep thisp recNo lineNo)]
                                  end             
(*val _ = print "returned newcontext:\n"*)
        val retbsllist = 
          case lastpremaxp of
              NONE => if lastmaxp = sbegin then 
                        if compBToken(PPblob, lastmaxt)=EQUAL then ((*print "all token sequences are of zero probability, return PPblob\n";*) [((lastmaxt, String.substring(s, lastmaxp, send-lastmaxp+1)), mkLoc lastmaxp send recNo lineNo)])
                        else ((*print ("( "^(Int.toString lastmaxp)^" "^(Int.toString send)^" "^(BTokenToName lastmaxt)^")\n");*) [((lastmaxt, String.substring(s, lastmaxp, send-lastmaxp+1)), mkLoc lastmaxp send recNo lineNo)])
                      else (print "4\n"; raise ViterbiError)
            | SOME pre =>
                if lastmaxp = sbegin then (*(print "5\n"; raise ViterbiError)*)
                let
                  val (prep, pret) = pre
(*val _ = print ("prep = "^(Int.toString prep)^" pret = "^(BTokenToName pret)^"\n") *)
                in
                   (print "5\n"; raise ViterbiError)
                end
                else
                ((*print ("( "^(Int.toString lastmaxp)^" "^(Int.toString send)^" "^(BTokenToName lastmaxt)^")\n");*) backward(pre, (lastmaxp-1))@[((lastmaxt, String.substring(s, lastmaxp, send-lastmaxp+1)), mkLoc lastmaxp send recNo lineNo)])
      in
        retbsllist
      end
      end


    fun basicViterbi_GHMM_length ghmmmodel (ss: Seqset) : NewContext = 
      let
        val (endptable, s, lineNo, recNo, sbegin, send) = ss
      in
      if sbegin = ~1 orelse send = ~1 then [((PPempty, ""), mkLoc ~1 ~1 recNo lineNo )]
      else
      let
(*val _ = print ("basicViterbi_GHMM_length: "^"sbegin = "^(Int.toString sbegin)^" send = "^(Int.toString send)^"\n")*)
        val reachable = PosBTokenTable.listItemsi endptable
        fun forward ((workingpos, bplist), oldtable) =
          let
            fun doOne ((beginp, btoken), myoldtable) =
              let
                val lastlist = PosBTokenTable.find(oldtable, (beginp-1))
(*val _ = print ("beginp = "^(Int.toString beginp)^" endp = "^(Int.toString workingpos)^" btoken = "^(BTokenToName btoken)^"\n")*)
                val thisprob = Math.ln(tokenProb(btoken, s, beginp, workingpos, ghmmmodel))
              in
                  case lastlist of
                      SOME table1 => 
                        let
                          val prelist = IntMap.listItemsi table1 
                          fun doOnePre ((tlength, (prep, pret, preprob)), updatemyoldtable) = 
                            let
                              val mynewlength = tlength + 1
                              val mynewprob = (preprob * Real.fromInt(tlength) + thisprob) / Real.fromInt(mynewlength)
                            in
                              case IntMap.find(updatemyoldtable, mynewlength) of
                                    NONE => IntMap.insert(updatemyoldtable, mynewlength, (beginp, btoken, mynewprob))
                                  | SOME (maxbeginp, maxbtoken, maxprob) => 
                                      if mynewprob > maxprob then IntMap.insert(#1(IntMap.remove(updatemyoldtable, mynewlength)), mynewlength, (beginp, btoken, mynewprob))
                                      else if Real.==(mynewprob, maxprob) then 
                                        if BTokenCompleteEnum(btoken) < BTokenCompleteEnum(maxbtoken) then 
                                          IntMap.insert(#1(IntMap.remove(updatemyoldtable, mynewlength)), mynewlength, (beginp, btoken, mynewprob))
                                        else updatemyoldtable
                                      else updatemyoldtable                                
                            end
                          val mynewtable = List.foldl doOnePre myoldtable prelist
                        in
                          mynewtable
                        end
                    | NONE => if beginp=sbegin then 
                                case IntMap.find(myoldtable, 1) of
                                    NONE => IntMap.insert(myoldtable, 1, (beginp, btoken, thisprob))
                                  | SOME (maxbeginp, maxbtoken, maxprob) => 
                                      if thisprob > maxprob then IntMap.insert(#1(IntMap.remove(myoldtable, 1)), 1, (beginp, btoken, thisprob))
                                      else if Real.==(thisprob, maxprob) then 
                                        if BTokenCompleteEnum(btoken) < BTokenCompleteEnum(maxbtoken) then 
                                          IntMap.insert(#1(IntMap.remove(myoldtable, 1)), 1, (beginp, btoken, thisprob))
                                        else myoldtable
                                      else myoldtable
                              else (print "1\n"; raise ViterbiError)
              end
            val newtable = List.foldl doOne IntMap.empty bplist
          in
            PosBTokenTable.insert(oldtable, workingpos, newtable)
          end
        val forwardmsg = List.foldl forward PosBTokenTable.empty reachable
        val fmsglist = PosBTokenTable.listItemsi forwardmsg
(*val _ = print ("fmsglist length = "^(Int.toString (List.length fmsglist))^"\n")*)
        val (lastpos, lastrcd) = List.nth(fmsglist, (List.length fmsglist)-1)
        val lastrcdlist = IntMap.listItemsi lastrcd
(*val _ = print ("last position list length = "^(Int.toString (List.length lastrcdlist))^"\n")*)
        fun findlastmax ((thislength, (thisp, thist, thisprob)), (maxlength, maxp, maxt, maxprob)) =
          if thisprob>maxprob then ((*print ("probability = "^(Real.toString thisprob)^"\n");*) (thislength, thisp, thist, thisprob))
          else if Real.==(thisprob, maxprob) then
            if BTokenCompleteEnum(thist)<BTokenCompleteEnum(maxt) then (thislength, thisp, thist, thisprob)
            else (maxlength, maxp, maxt, maxprob)
          else ((*print ("probability = "^(Real.toString maxprob)^"\n");*) (maxlength, maxp, maxt, maxprob))
        val (lastlength, lastmaxp, lastmaxt, lastmaxprob) = List.foldl findlastmax (1, sbegin, PPblob, (~Real.maxFinite)) lastrcdlist 
        fun backward (thislength, thisp) = (* return a bsl list *) 
          case PosBTokenTable.find(forwardmsg, thisp) of
              NONE => if thislength < 1 andalso thisp + 1 = sbegin then []
                      else raise ViterbiError
            | SOME table => case IntMap.find(table, thislength) of
                                NONE => (print (" endp = "^(Int.toString thisp)^"\n"); raise ViterbiError) 
                              | SOME (thisbeginp, thist, thisprob) => 
                                  let
                                    val newbsl = backward((thislength-1), (thisbeginp-1))
(*val _ = print ("( "^(Int.toString thisbeginp)^" "^(Int.toString thisp)^" "^(BTokenToName thist)^")\n")*)
                                  in
                                    newbsl@[((thist, String.substring(s, thisbeginp, thisp-thisbeginp+1)), mkLoc thisbeginp thisp recNo lineNo)]
                                  end             
(*val _ = print "returned newcontext:\n"*)
        val retbsllist = backward(lastlength-1, lastmaxp-1)@[((lastmaxt, String.substring(s, lastmaxp, send-lastmaxp+1)), mkLoc lastmaxp send recNo lineNo)]
      in
        retbsllist
      end
      end


    fun basicViterbi_SVM_trans_length svmmodel tokenpairtable (ss: Seqset) : NewContext = 
      let
(*val _ = print "basicViterbi_GHMM_trans_length\n"*)
        val (endptable, s, lineNo, recNo, sbegin, send) = ss
(*val _ = print ("beginp = "^(Int.toString sbegin)^" endp = "^(Int.toString send)^" "(*^(String.substring (s, sbegin, send-sbegin+1))*)^"\n")*)
      in
        if sbegin = ~1 orelse send = ~1 then  [((PPempty, ""), mkLoc ~1 ~1 recNo lineNo )] 
        else
      let
        val reachable = PosBTokenTable.listItemsi endptable
        fun forward ((workingpos, bplist), oldtable1) =
          let
            fun doOne ((beginp, btoken), oldtable2) =
              let
                val thisprob = Math.ln(tokenProbSVM(btoken, s, beginp, workingpos, svmmodel))
              in
              case PosBTokenTable.find(oldtable1, (beginp-1)) of
                  NONE => ((*print "can't find a pre table\n";*)
                          if beginp = sbegin then 
                            (
(*print "=begin\n";*)
                            case IntMap.find(oldtable2, 1) of
                                NONE => 
                                  let
                                    val newtable3 = BasicViterbiTable.insert(BasicViterbiTable.empty, (beginp, btoken), (NONE, thisprob))
                                  in
                                    IntMap.insert(oldtable2, 1, newtable3)
                                  end
                              | SOME oldtable3 =>
                                  let
                                    val newtable3 = BasicViterbiTable.insert(oldtable3, (beginp, btoken), (NONE, thisprob))
                                  in
                                    IntMap.insert(#1(IntMap.remove(oldtable2, 1)), 1, newtable3)
                                  end
                             )
                          else (print ("2: not = begin, beginp = "^(Int.toString beginp)^"\n"); raise ViterbiError))
                 | SOME pretable1 =>
                    let
(*val _ = print "get a pre table\n"*)
                      val preslist = IntMap.listItemsi pretable1
                      fun doOneLength ((tlength, preoldtable3), myoldtable2) = 
                        let
                          val newlength = tlength + 1
                          val prebplist = BasicViterbiTable.listItemsi preoldtable3
                          fun search (((mybp, mybptoken), (lastbp, prob)), ((maxbp, maxtoken), maxprob)) = 
                            let
                              val mybptokenc = BToken2BTokenClass mybptoken
                              val btokenc = BToken2BTokenClass btoken
                              val transprob = if ( !ghmm5 = true ) then Math.ln(defaultRVal(BTokenPairTable.find(tokenpairtable, (mybptokenc, btokenc))))
                                              else Math.ln(defaultRVal(BTokenPairTable.find(tokenpairtable, (mybptoken, btoken))))
(*val _ = print ("transprob = "^(Real.toString(transprob))^"\n")*)
                              val newthisprob = (prob * Real.fromInt(tlength) + transprob + thisprob) / Real.fromInt(newlength)
                            in
                              if newthisprob > maxprob then ((mybp, mybptoken), newthisprob)
                              else if Real.==(thisprob, maxprob) then 
                                if (BTokenCompleteEnum(mybptoken) < BTokenCompleteEnum(maxtoken)) then ((mybp, mybptoken), newthisprob) else ((maxbp, maxtoken), maxprob)
                              else ((maxbp, maxtoken), maxprob)
                            end
                          val ((b, t), mprob) = List.foldl search ((sbegin, PPblob), ~Real.maxFinite) prebplist
                        in
                          case IntMap.find(myoldtable2, newlength) of
                              NONE =>
                                let
                                  val newtable3 = BasicViterbiTable.insert(BasicViterbiTable.empty, (beginp, btoken), (SOME (b, t), mprob))
                                in
                                  IntMap.insert(myoldtable2, newlength, newtable3)
                                end
                            | SOME myoldtable3 =>
                                let
                                  val newtable3 = BasicViterbiTable.insert(myoldtable3, (beginp, btoken), (SOME (b, t), mprob))
                                in
                                  IntMap.insert(#1(IntMap.remove(myoldtable2, newlength)), newlength, newtable3)
                                end
                        end
                      val newtable2 = List.foldl doOneLength oldtable2 preslist
                    in
                      newtable2
                    end
              end
(*val _ = print ("workingpos = "^(Int.toString workingpos)^"\n")*)
            val thistable = List.foldl doOne IntMap.empty bplist
(*val _ = print "after doOne\n"*)
          in 
            PosBTokenTable.insert(oldtable1, workingpos, thistable)
          end
        val forwardmsg = List.foldl forward PosBTokenTable.empty reachable 
        val fmsglist = PosBTokenTable.listItemsi forwardmsg
(*val _ = print "forward done\n"*)
        val (lastpos, lasttable2) = List.nth(fmsglist, (List.length fmsglist)-1)
        val lastlist2 = IntMap.listItemsi lasttable2
        fun findlastmax ((tlength, lasttable3), (maxt, maxpret, maxlength, maxprob)) = 
          let
            val lastlist3 = BasicViterbiTable.listItemsi(lasttable3)
            fun findinside ((thisp as (thispp, thispt), (lastp, lastprob)), (mymaxt, mymaxpret, mymaxlength, mymaxprob)) = 
              (
(*print ("beginp = "^(Int.toString(thispp))^" token = "^(BTokenToName(thispt))^" prob = "^(Real.toString lastprob)^"\n");*)
              if lastprob > mymaxprob then (thisp, lastp, tlength, lastprob)
              else (mymaxt, mymaxpret, mymaxlength, mymaxprob)
              )
          in
            List.foldl findinside (maxt, maxpret, maxlength, maxprob) lastlist3
          end
        val ((lastmaxp, lastmaxt), lastpremaxp, lastlength, lastmaxprob) = List.foldl findlastmax ((sbegin, PPblob), NONE, 1, (~Real.maxFinite)) lastlist2 
        fun backward ((beginp, btoken), endp, tlength) = 
          case PosBTokenTable.find(forwardmsg, endp) of
              NONE => (print ("5: endp = "^(Int.toString endp)^" beginp = "^(Int.toString beginp)^"\n"); raise ViterbiError)
            | SOME table2 => 
                case IntMap.find(table2, tlength) of
                    NONE => (print ("6: endp = "^(Int.toString endp)^" beginp = "^(Int.toString beginp)^"\n"); raise ViterbiError)
                  | SOME table3 =>
                      case BasicViterbiTable.find(table3, (beginp, btoken)) of
                          NONE => (print ("7: endp = "^(Int.toString endp)^" beginp = "^(Int.toString beginp)^"\n"); raise ViterbiError)
                        | SOME (pret, prob) =>
                            case pret of
                                NONE => if beginp = sbegin then [((btoken, String.substring(s, beginp, endp-beginp+1)), mkLoc beginp endp recNo lineNo)]
                                        else (print "8\n"; raise ViterbiError)
                              | SOME (prep, pretoken) =>
                                  if tlength < 2 then (print "9\n"; raise ViterbiError) 
                                  else
                                  let
                                    val prebsl = backward((prep, pretoken), beginp-1, tlength-1)
                                  in
                                    prebsl@[((btoken, String.substring(s, beginp, endp-beginp+1)), mkLoc beginp endp recNo lineNo)]
                                  end
        val prelist = 
          case lastpremaxp of
              NONE => if lastlength>1 then (print "10\n"; raise ViterbiError)
                      else []
            | SOME prep => if lastlength<2 then (print "11\n"; raise ViterbiError)
                           else backward(prep, lastmaxp-1, lastlength-1)
        val retlist = prelist@[((lastmaxt, String.substring(s, lastmaxp, send-lastmaxp+1)), mkLoc lastmaxp send recNo lineNo)]
      in 
        retlist
      end
      end

    fun basicViterbi_GHMM_trans_length ghmmmodel tokenpairtable (ss: Seqset) : NewContext = 
      let
(*val _ = print "basicViterbi_GHMM_trans_length\n"*)
        val (endptable, s, lineNo, recNo, sbegin, send) = ss
(*val _ = print ("beginp = "^(Int.toString sbegin)^" endp = "^(Int.toString send)^" "(*^(String.substring (s, sbegin, send-sbegin+1))*)^"\n")*)
      in
        if sbegin = ~1 orelse send = ~1 then  [((PPempty, ""), mkLoc ~1 ~1 recNo lineNo )] 
        else
      let
        val reachable = PosBTokenTable.listItemsi endptable
        fun forward ((workingpos, bplist), oldtable1) =
          let
            fun doOne ((beginp, btoken), oldtable2) =
              let
                val thisprob = Math.ln(tokenProb(btoken, s, beginp, workingpos, ghmmmodel))
              in
              case PosBTokenTable.find(oldtable1, (beginp-1)) of
                  NONE => ((*print "can't find a pre table\n";*)
                          if beginp = sbegin then 
                            (
(*print "=begin\n";*)
                            case IntMap.find(oldtable2, 1) of
                                NONE => 
                                  let
                                    val newtable3 = BasicViterbiTable.insert(BasicViterbiTable.empty, (beginp, btoken), (NONE, thisprob))
                                  in
                                    IntMap.insert(oldtable2, 1, newtable3)
                                  end
                              | SOME oldtable3 =>
                                  let
                                    val newtable3 = BasicViterbiTable.insert(oldtable3, (beginp, btoken), (NONE, thisprob))
                                  in
                                    IntMap.insert(#1(IntMap.remove(oldtable2, 1)), 1, newtable3)
                                  end
                             )
                          else (print ("2: not = begin, beginp = "^(Int.toString beginp)^"\n"); raise ViterbiError))
                 | SOME pretable1 =>
                    let
(*val _ = print "get a pre table\n"*)
                      val preslist = IntMap.listItemsi pretable1
                      fun doOneLength ((tlength, preoldtable3), myoldtable2) = 
                        let
                          val newlength = tlength + 1
                          val prebplist = BasicViterbiTable.listItemsi preoldtable3
                          fun search (((mybp, mybptoken), (lastbp, prob)), ((maxbp, maxtoken), maxprob)) = 
                            let
                              val mybptokenc = BToken2BTokenClass mybptoken
                              val btokenc = BToken2BTokenClass btoken
                              val transprob = if ( !ghmm5 = true ) then Math.ln(defaultRVal(BTokenPairTable.find(tokenpairtable, (mybptokenc, btokenc))))
                                              else Math.ln(defaultRVal(BTokenPairTable.find(tokenpairtable, (mybptoken, btoken))))
(*val _ = print ("transprob = "^(Real.toString(transprob))^"\n")*)
                              val newthisprob = (prob * Real.fromInt(tlength) + transprob + thisprob) / Real.fromInt(newlength)
                            in
                              if newthisprob > maxprob then ((mybp, mybptoken), newthisprob)
                              else if Real.==(thisprob, maxprob) then 
                                if (BTokenCompleteEnum(mybptoken) < BTokenCompleteEnum(maxtoken)) then ((mybp, mybptoken), newthisprob) else ((maxbp, maxtoken), maxprob)
                              else ((maxbp, maxtoken), maxprob)
                            end
                          val ((b, t), mprob) = List.foldl search ((sbegin, PPblob), ~Real.maxFinite) prebplist
                        in
                          case IntMap.find(myoldtable2, newlength) of
                              NONE =>
                                let
                                  val newtable3 = BasicViterbiTable.insert(BasicViterbiTable.empty, (beginp, btoken), (SOME (b, t), mprob))
                                in
                                  IntMap.insert(myoldtable2, newlength, newtable3)
                                end
                            | SOME myoldtable3 =>
                                let
                                  val newtable3 = BasicViterbiTable.insert(myoldtable3, (beginp, btoken), (SOME (b, t), mprob))
                                in
                                  IntMap.insert(#1(IntMap.remove(myoldtable2, newlength)), newlength, newtable3)
                                end
                        end
                      val newtable2 = List.foldl doOneLength oldtable2 preslist
                    in
                      newtable2
                    end
              end
(*val _ = print ("workingpos = "^(Int.toString workingpos)^"\n")*)
            val thistable = List.foldl doOne IntMap.empty bplist
(*val _ = print "after doOne\n"*)
          in 
            PosBTokenTable.insert(oldtable1, workingpos, thistable)
          end
        val forwardmsg = List.foldl forward PosBTokenTable.empty reachable 
        val fmsglist = PosBTokenTable.listItemsi forwardmsg
(*val _ = print "forward done\n"*)
        val (lastpos, lasttable2) = List.nth(fmsglist, (List.length fmsglist)-1)
        val lastlist2 = IntMap.listItemsi lasttable2
        fun findlastmax ((tlength, lasttable3), (maxt, maxpret, maxlength, maxprob)) = 
          let
            val lastlist3 = BasicViterbiTable.listItemsi(lasttable3)
            fun findinside ((thisp as (thispp, thispt), (lastp, lastprob)), (mymaxt, mymaxpret, mymaxlength, mymaxprob)) = 
              (
(*print ("beginp = "^(Int.toString(thispp))^" token = "^(BTokenToName(thispt))^" prob = "^(Real.toString lastprob)^"\n");*)
              if lastprob > mymaxprob then (thisp, lastp, tlength, lastprob)
              else (mymaxt, mymaxpret, mymaxlength, mymaxprob)
              )
          in
            List.foldl findinside (maxt, maxpret, maxlength, maxprob) lastlist3
          end
        val ((lastmaxp, lastmaxt), lastpremaxp, lastlength, lastmaxprob) = List.foldl findlastmax ((sbegin, PPblob), NONE, 1, (~Real.maxFinite)) lastlist2 
        fun backward ((beginp, btoken), endp, tlength) = 
          case PosBTokenTable.find(forwardmsg, endp) of
              NONE => (print ("5: endp = "^(Int.toString endp)^" beginp = "^(Int.toString beginp)^"\n"); raise ViterbiError)
            | SOME table2 => 
                case IntMap.find(table2, tlength) of
                    NONE => (print ("6: endp = "^(Int.toString endp)^" beginp = "^(Int.toString beginp)^"\n"); raise ViterbiError)
                  | SOME table3 =>
                      case BasicViterbiTable.find(table3, (beginp, btoken)) of
                          NONE => (print ("7: endp = "^(Int.toString endp)^" beginp = "^(Int.toString beginp)^"\n"); raise ViterbiError)
                        | SOME (pret, prob) =>
                            case pret of
                                NONE => if beginp = sbegin then [((btoken, String.substring(s, beginp, endp-beginp+1)), mkLoc beginp endp recNo lineNo)]
                                        else (print "8\n"; raise ViterbiError)
                              | SOME (prep, pretoken) =>
                                  if tlength < 2 then (print "9\n"; raise ViterbiError) 
                                  else
                                  let
                                    val prebsl = backward((prep, pretoken), beginp-1, tlength-1)
                                  in
                                    prebsl@[((btoken, String.substring(s, beginp, endp-beginp+1)), mkLoc beginp endp recNo lineNo)]
                                  end
        val prelist = 
          case lastpremaxp of
              NONE => if lastlength>1 then (print "10\n"; raise ViterbiError)
                      else []
            | SOME prep => if lastlength<2 then (print "11\n"; raise ViterbiError)
                           else backward(prep, lastmaxp-1, lastlength-1)
        val retlist = prelist@[((lastmaxt, String.substring(s, lastmaxp, send-lastmaxp+1)), mkLoc lastmaxp send recNo lineNo)]
      in 
        retlist
      end
      end

end
