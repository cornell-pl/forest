structure Evaluate_hmm = 
struct
    open Basetokens
    open Config
    open Probability

    exception InvalidTestFile

    fun evaluate bsll1 bsll2 = (* assume bsll1 is the correct tokenization *) 
      let
        val _ = if (List.length bsll1) <> (List.length bsll2) then raise InvalidTestFile else () 
        val _ = print "\n"
        fun compareOneRecord i : (int*int)*(int*int)*int = 
          if i<0 then ((0,0),(0,0),0)
          else 
            let
              val ((oldwrongc, oldtotalc),(oldwrongt, oldtotalt), oldwrongr) = compareOneRecord (i-1)
              val bsl1 = List.nth (bsll1, i)
              val bsl2 = List.nth (bsll2, i)
              fun bslToCbl thisbsl : (char*BToken) list = 
                let
                  fun doOne ((b, s), resultcblist) =
                    let
                      val clist = String.explode s
                      fun addBToken c = (c, b)
                      val mycblist = List.map addBToken clist
                    in
                      resultcblist@mycblist
                    end
                in
                  List.foldl doOne [] thisbsl
                end
            in
              case bsl2 of
                  [] => let (* no tokenization result *)
                          val cbl1 = bslToCbl bsl1
                          val _ = print ("record "^(Int.toString i)^":\n")
                          val _ = print "no tokenization result\n\n"
                        in
                          ((oldwrongc+(List.length cbl1), oldtotalc+(List.length cbl1)), (oldwrongt+(List.length bsl1), oldtotalt+(List.length bsl1)), oldwrongr+1)
                        end
                | _ =>
              let
              val cbl2 = bslToCbl bsl2
              val thistotalc = List.length cbl2
              val thistotalt = List.length bsl1
              fun compareOneToken ((b, s), (p, rewrongc, rewrongt)) = 
                let
                  val len = String.size s
                  val checkcbl = List.take(List.drop(cbl2, p), len)
                  fun checkOne ((c, b2), result) = if compBToken(b, b2)=EQUAL then result else (result+1)
                  val retwrongc = List.foldl checkOne 0 checkcbl
                  val retwrongt = if retwrongc=0 then 0 else 
                    (print ("this token is not recognized: "^(BTokenToName b)^" ["^s^"]\n"); 1) 
                in
                  (p+len, rewrongc+retwrongc, rewrongt+retwrongt)
                end
              val _ = print ("record "^(Int.toString i)^":\n")
              val (junk, thiswrongc, thiswrongt) = List.foldl compareOneToken (0, 0, 0) bsl1
              val thiswrongr = if thiswrongt=0 then 0 else 1
              fun printBSToken (t, s) = print ((BTokenToName t)^"["^s^"]"^" ")
              val _ = if thiswrongr = 0 then ()
                      else (print "wrong token sequence :"; List.app printBSToken bsl2; print "\n") 
              val _ = print ("totalc="^(Int.toString thistotalc)^
                             " wrongc="^(Int.toString thiswrongc)^
                             " totalt="^(Int.toString thistotalt)^
                             " wrongt="^(Int.toString thiswrongt)^"\n\n")
              in
              ((oldwrongc+thiswrongc, oldtotalc+thistotalc), (oldwrongt+thiswrongt, oldtotalt+thistotalt), oldwrongr+thiswrongr)
              end
            end
        val ((wrongc, totalc),(wrongt, totalt), wrongr) = compareOneRecord ((List.length bsll1)-1)
        val totalr = List.length bsll1
      in
        print ("\ntotal records = "^(Int.toString totalr)^"\twrong records = "^(Int.toString wrongr)^"\terror rate = "^(Real.toString ((Real.fromInt wrongr)/(Real.fromInt totalr)))^
               "\ntotal tokens = "^(Int.toString totalt)^"\twrong tokens = "^(Int.toString wrongt)^"\terror rate = "^(Real.toString ((Real.fromInt wrongt)/(Real.fromInt totalt)))^
               "\ntotal characters = "^(Int.toString totalc)^"\twrong characters = "^(Int.toString wrongc)^"\terror rate = "^(Real.toString ((Real.fromInt wrongc)/(Real.fromInt totalc)))^"\n")
      end

end
