structure Svm = 
struct

  open Basetokens

  type sv_node = int list * real list

  type svm_model = real * int * int * int list * real list * real list * real list * int list * real list list * sv_node list(* gamma, nr_class, total sv, 2 list for label mapping, probA, probB *)

    fun loadFile path = 
     let 
       val strm = TextIO.openIn path
	   val data : String.string = TextIO.inputAll strm
       fun isNewline c = c = #"\n" orelse c = #"\r"
       fun getLines(ss,l) = 
         if (Substring.isEmpty ss) then List.rev l
	     else let 
                val (ln, rest) = Substring.splitl (not o isNewline) ss
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
	   val () = TextIO.closeIn strm
     in
	   lines
     end

   structure IntMap = RedBlackMapFn(
                     struct type ord_key = int
			    val compare = Int.compare
		     end) 

  exception readinSVMexp

  fun readinSVM_predict path = (* return label and prob list *)
    let
	  val lines = loadFile "training/svmoutput"
      fun isSpace c = c = #" "
      fun extractInt str = (* str : Substring *)
        let
          val (thisint, junk) = Substring.splitl (not o isSpace) str
          val after = if Substring.size thisint = 0 orelse Substring.size junk = 0 orelse Substring.size junk = 1 then []
                      else extractInt(Substring.triml 1 junk)
        in
           [Option.valOf(Int.fromString(Substring.string(thisint)))]@after handle Option => (print ((Substring.string thisint)); raise readinSVMexp)
        end
      fun extractFloat str = (* str : Substring *)
        let
          val (thisfloat, junk) = Substring.splitl (not o isSpace) str
          val after = if Substring.size thisfloat = 0 orelse Substring.size junk = 0 orelse Substring.size junk = 1 then []
                      else extractFloat(Substring.triml 1 junk)
        in
          if Substring.size thisfloat = 0 then after
          else [Option.valOf(Real.fromString(Substring.string(thisfloat)))]@after handle Option => raise readinSVMexp
        end
      val (junk, label) = Substring.splitl (not o isSpace) (Substring.full (List.nth(lines, 0)))
      val label = extractInt(Substring.triml 1 label) handle readinSVMexp => raise readinSVMexp
      val (junk, prob) = Substring.splitl (not o isSpace) (Substring.full (List.nth(lines, 1)))
      val prob = extractFloat(Substring.triml 1 prob)
    in
      (label, prob)
    end

  fun readinSVM_predict2 path = (* return label table and prob list list *)
    let
	  val lines = loadFile "training/svmoutput_predict"
      fun isSpace c = c = #" "
      fun extractInt (str, index) = (* str : Substring *)
        let
          val (thisint, junk) = Substring.splitl (not o isSpace) str
          val after = if Substring.size thisint = 0 orelse Substring.size junk = 0 orelse Substring.size junk = 1 then IntMap.empty
                      else extractInt((Substring.triml 1 junk), index+1)
        in
           IntMap.insert(after, Option.valOf(Int.fromString(Substring.string(thisint))), index) handle Option => (print ((Substring.string thisint)); raise readinSVMexp)
        end
      fun extractFloat str = (* str : Substring *)
        let
          val (thisfloat, junk) = Substring.splitl (not o isSpace) str
          val after = if Substring.size thisfloat = 0 orelse Substring.size junk = 0 orelse Substring.size junk = 1 then []
                      else extractFloat(Substring.triml 1 junk)
        in
          if Substring.size thisfloat = 0 then after
          else [Option.valOf(Real.fromString(Substring.string(thisfloat)))]@after handle Option => raise readinSVMexp
        end
      val (junk, label) = Substring.splitl (not o isSpace) (Substring.full (List.nth(lines, 0)))
      val label = extractInt((Substring.triml 1 label), 0) handle readinSVMexp => raise readinSVMexp
      val lines = List.drop(lines, 1)
      fun doOne line =
        let
          val (junk, prob) = Substring.splitl (not o isSpace) (Substring.full line)
          val prob = extractFloat(Substring.triml 1 prob)
        in
          prob
        end
      val probs = List.map doOne lines
    in
      (label, probs)
    end


  fun readinSVM path = 
    let
	  val lines = loadFile "training/mymodel_svm"
      fun isSpace c = c = #" "
      fun isColon c = c = #":"
      val (junk, gamma) = Substring.splitr (not o isSpace) (Substring.full (List.nth(lines, 2)))
      val gamma = Option.valOf(Real.fromString(Substring.string(gamma))) handle Option => raise readinSVMexp
      val (junk, nr_class) = Substring.splitr (not o isSpace) (Substring.full (List.nth(lines, 3)))
      val nr_class = Option.valOf(Int.fromString(Substring.string(nr_class))) handle Option => raise readinSVMexp
      val (junk, total_sv) = Substring.splitr (not o isSpace) (Substring.full (List.nth(lines, 4)))
      val total_sv = Option.valOf(Int.fromString(Substring.string(total_sv))) handle Option => raise readinSVMexp
      fun extractInt str = (* str : Substring *)
        let
          val (thisint, junk) = Substring.splitl (not o isSpace) str
          val after = if Substring.size thisint = 0 orelse Substring.size junk = 0 orelse Substring.size junk = 1 then []
                      else extractInt(Substring.triml 1 junk)
        in
           [Option.valOf(Int.fromString(Substring.string(thisint)))]@after handle Option => (print ((Substring.string thisint)); raise readinSVMexp)
        end
      fun extractFloat str = (* str : Substring *)
        let
          val (thisfloat, junk) = Substring.splitl (not o isSpace) str
          val after = if Substring.size thisfloat = 0 orelse Substring.size junk = 0 orelse Substring.size junk = 1 then []
                      else extractFloat(Substring.triml 1 junk)
        in
          if Substring.size thisfloat = 0 then after
          else [Option.valOf(Real.fromString(Substring.string(thisfloat)))]@after handle Option => raise readinSVMexp
        end
      val (junk, rho) = Substring.splitl (not o isSpace) (Substring.full (List.nth(lines, 5)))
      val rho = extractFloat(Substring.triml 1 rho)
(*val _ = print ((Real.toString (List.nth(rho, 0)))^"\n")*)
      val (junk, label) = Substring.splitl (not o isSpace) (Substring.full (List.nth(lines, 6)))
      val label = extractInt(Substring.triml 1 label) handle readinSVMexp => raise readinSVMexp
      val (junk, probA) = Substring.splitl (not o isSpace) (Substring.full (List.nth(lines, 7)))
      val probA = extractFloat(Substring.triml 1 probA)
      val (junk, probB) = Substring.splitl (not o isSpace) (Substring.full (List.nth(lines, 8)))
      val probB = extractFloat(Substring.triml 1 probB)
      val (junk, nr_sv) = Substring.splitl (not o isSpace) (Substring.full (List.nth(lines, 9)))
      val nr_sv = extractInt(Substring.triml 1 nr_sv) handle readinSVMexp => raise readinSVMexp
      val sv_coef = List.drop(lines, 11)
      fun extractCoef str =
        let
          val str = Substring.full str
          fun extract1 (str1, cnt) = 
            if cnt = 0 then ([], str1)
            else
              let 
                val (thisfloat, junk) = Substring.splitl (not o isSpace) str1
                val (after, rest) = extract1((Substring.triml 1 junk), cnt-1)
              in
                ([Option.valOf(Real.fromString(Substring.string(thisfloat)))]@after, rest) handle Option => raise readinSVMexp
              end
          val (wlist, rest) = extract1(str, nr_class-1)
          fun extract2 str2 = 
            let
(*val _ = print ((Substring.string str2)^"\n")*)
              val (thisrec, junk) = Substring.splitl (not o isSpace) str2
              val (after1, after2) = if Substring.size thisrec = 0 orelse Substring.size junk=0 orelse Substring.size junk = 1 then ([], [])
                                     else extract2(Substring.triml 1 junk)
              val (thisind, thisfloat) = Substring.splitl (not o isColon) thisrec
              val thisfloat = Substring.triml 1 thisfloat
              val thisind = Option.valOf(Int.fromString(Substring.string(thisind))) handle Option => (print ((Substring.string thisind)^"\n"); raise readinSVMexp)
              val thisfloat = Option.valOf(Real.fromString(Substring.string(thisfloat))) handle Option => raise readinSVMexp
            in
              (thisind::after1, thisfloat::after2) 
            end
          val (indlist, svlist) = extract2 rest
        in
          (wlist, (indlist, svlist))
        end
      val messlist = List.map extractCoef sv_coef
      fun splitlist ((wlist, node), (w2list, nodelist)) = (w2list@[wlist], nodelist@[node])
      val (coeflist, svnodelist) = List.foldl splitlist ([], []) messlist
    in
      (gamma, nr_class, total_sv, rho, label, probA, probB, nr_sv, coeflist, svnodelist)
    end

  fun k_function (node1, gamma) node2 = 
    let
      val (indlist1, svlist1) = node1
      val (indlist2, svlist2) = node2
      fun compute (i, j) = 
        if i+1 = List.length indlist1 andalso j+1 = List.length indlist2 then 0.0
        else if i+1 = List.length indlist1 then (
          let
            val sv2 = List.nth(svlist2, j) 
          in
            sv2 * sv2 + compute(i, j+1)
          end
        )
        else if j+1 = List.length indlist2 then (
          let
            val sv1 = List.nth(svlist1, i) 
          in
            sv1 * sv1 + compute(i+1, j)
          end
        )
        else (
          let
            val ind1 = List.nth(indlist1, i)
            val ind2 = List.nth(indlist2, j)
            val sum = if ind1 = ind2 then 
                         let
                           val sv1 = List.nth(svlist1, i)
                           val sv2 = List.nth(svlist2, j)
                           val diff = sv1 - sv2
(*val _ = print("i = "^(Int.toString i)^" j = "^(Int.toString j)^"\n")*)
                         in
                           diff * diff + compute(i+1, j+1)
                         end
                       else if ind1 > ind2 then
                         let
                           val sv2 = List.nth(svlist2, j)
                         in
                           sv2 * sv2 + compute(i, j+1)
                         end
                       else
                         let
                           val sv1 = List.nth(svlist1, i)
                         in
                           sv1 * sv1 + compute(i+1, j)
                         end
          in
            sum
          end
        )
      val sum = compute(0, 0)
      val ret = Math.exp(~gamma*sum)
val _ = print ((Real.toString sum)^" "^(Real.toString ret)^"\n")
    in
      ret
    end

  fun svm_predict_values (model, x) = 
    let
      val (gamma, nr_class, total_sv, rho, label, probA, probB, nr_sv, coeflist, nodelist) = model
(*val _ = print((Int.toString (List.length nodelist))^"\n")*)
val _ = print "1\n"
      val kvalues = List.map (k_function (x, gamma)) nodelist
val _ = print "2\n"
      fun getStart (last, i) = 
        if i = nr_class then []
        else 
          let
            val this = last + List.nth(nr_sv, i-1)
            val rest = getStart(this, i+1)
          in
            this::rest
          end
      val start = 0::getStart(0, 1)
      fun decValues (i, j, p) = 
        if i+1 = nr_class andalso j = nr_class then []
        else if j = nr_class then decValues (i+1, i+1+1, p)
        else 
          let
            val si = List.nth(start, i)
            val sj = List.nth(start, j)
            val ci = List.nth(nr_sv, i)
            val cj = List.nth(nr_sv, j)
            fun getsum1 k = 
              if k = ci then 0.0
              else 
                let
                  val coef = List.nth(List.nth(coeflist, si+k), j-1)
                  val kvalue = List.nth(kvalues, si+k)
                in
                  coef * kvalue + getsum1(k+1) 
                end
            fun getsum2 k = 
              if k = cj then 0.0
              else 
                let
                  val coef = List.nth(List.nth(coeflist, sj+k), i)
                  val kvalue = List.nth(kvalues, sj+k)
                in
                  coef * kvalue + getsum2(k+1) 
                end
            val sum = (getsum1 0) + (getsum2 0) - List.nth(rho, p)
          in
            sum::decValues(i, j+1, p+1)
          end
      val dec_values = decValues(0, 1, 0)
    in
      dec_values
    end

  fun multiclass_probability (k, r) = 
    let
      val p = Array.array(k, 1.0/Real.fromInt(k)) (* note p is an array *)
      val max_iter = Int.max(100, k)
      fun constrQ (t, preQ) = 
        if t = k then []
        else 
          let
            fun computett j = 
              if j = k then 0.0
              else if j = t then 0.0 + computett(j+1)
              else
              let
                val rjt = List.nth(List.nth(r, j), t)
              in
                rjt * rjt + computett(j+1)
              end
            fun beforet j =
              if j = t then []
              else List.nth(List.nth(preQ, j), t)::beforet(j+1)
            fun aftert j =
              if j = k then []
              else (~(List.nth(List.nth(r, j), t)*List.nth(List.nth(r, t), j)))::aftert(j+1)
            val thisq = beforet(0)@[computett 0]@aftert(t+1)
          in
            thisq::constrQ(t+1, preQ@[thisq])
          end
      val Q = constrQ(0, [])
      val eps = 0.005/Real.fromInt(k)
      fun iter it = 
        if it = max_iter then ()
        else
          let
            fun getpQp t = 
              if t = k then (0.0, [])
              else
                let
                  fun updateQpt j =
                    if j = k then 0.0
                    else List.nth(List.nth(Q, t), j)*Array.sub(p, j) + updateQpt(j+1)
                  val Qpt = updateQpt 0
                  val (pQpafter, Qpafter) = getpQp(t+1)
                in
                  (Array.sub(p, t)*Qpt+pQpafter, Qpt::Qpafter)
                end
            val (pQp, Qp) = getpQp 0
            fun getMaxError (Qpt, maxe) = 
              let
                val error = Real.abs(Qpt-pQp)
              in
                if error > maxe then error
                else maxe
              end
            val max_error = List.foldl getMaxError 0.0 Qp
          in
            if max_error < eps then ()
            else
              let
                fun updateP (t, newQp, newpQp) =
                  if t = k then ()
                  else
                    let
                      val diff = (~(List.nth(newQp, t)) + newpQp)/List.nth(List.nth(Q, t), t)
                      val _ = Array.update(p, t, Array.sub(p, t)+diff)
                      val mypQp = (newpQp + diff * ( diff * List.nth(List.nth(Q, t), t) + 2.0 * List.nth(newQp, t)))/(1.0+diff)/(1.0+diff)
                      fun innerUpdateQp j =
                        if j = k then []
                        else (Array.update(p, j, Array.sub(p, j)/(1.0+diff)); 
                             (List.nth(newQp, j)+diff*List.nth(List.nth(Q, t), j))/(1.0+diff)::innerUpdateQp(j+1))
                      val myQp = innerUpdateQp 0
                    in
                      updateP(t+1, myQp, mypQp)
                    end
              in
                (updateP(0, Qp, pQp); iter(it+1))
              end
          end
      fun array2list i = 
        if i = Array.length p then []
        else Array.sub(p, i)::array2list(i+1)
      val newp = array2list 0
    in
      newp
    end

  fun sigmoid_predict (decision_value, A, B) = 
    let
      val fApB = decision_value*A+B
    in
      if fApB >= 0.0 then Math.exp(~fApB)/(1.0+Math.exp(~fApB))
      else 1.0/(1.0+Math.exp(fApB))
    end

  fun svm_predict_probability (model, x) = (* return probability list *)
    let
      val (gamma, nr_class, total_sv, rho, label, probA, probB, nr_sv, coeflist, nodelist) = model
      val dec_values = svm_predict_values(model, x)
      val min_prob = 0.0000001
      fun pairprob (i, k, prepair) =
        if i = nr_class then []
        else
          let
            fun innerloop (j, k1) : real list * int = 
              if j = nr_class then ([], k1)
              else if j < i then 
                let
                  val (afterlist, newk1) = innerloop(j+1, k1)
                in
                  ((1.0-List.nth(List.nth(prepair, j), i))::afterlist, newk1) handle Subscript => raise Subscript
                end
              else if j = i then 
                let
                  val (afterlist, newk1) = innerloop(j+1, k1)
                in
                  (0.0::afterlist, newk1)   (* this value won't take any effect *)
                end
              else 
                let 
                  val (afterlist, newk1) = innerloop(j+1, k1+1)
                in
                  (Real.min(Real.max(sigmoid_predict(List.nth(dec_values, k1), List.nth(probA, k1), List.nth(probB, k1)), min_prob), 1.0-min_prob)::afterlist, newk1) handle Subscript => raise Subscript
                end
            val (innerlist, newk) = innerloop(0, k)
          in
            innerlist::pairprob(i+1, newk, prepair@[innerlist])
          end
val _ = print "1\n"
      val pairwise_prob = pairprob(0, 0, [])
val _ = print "2\n"
      val prob_estimates = multiclass_probability(nr_class, pairwise_prob)
val _ = print "3\n"
    in
      prob_estimates
    end
 
end
