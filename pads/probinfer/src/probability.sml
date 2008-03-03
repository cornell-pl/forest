structure Probability = 
struct
    open Basetokens
    open Config
    open Fvector

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

    fun tstringToBToken (s, str) : BToken =
      if String.isSubstring "int" s then PPint
      else if String.isSubstring "float" s then PPfloat
      else if String.isSubstring "time" s then PPtime
      else if String.isSubstring "date" s then PPdate
      else if String.isSubstring "ip" s then PPip
      else if String.isSubstring "hostname" s then PPhostname
      else if String.isSubstring "email" s then PPemail
      else if String.isSubstring "mac" s then PPmac
      else if String.isSubstring "path" s then PPpath
      else if String.isSubstring "urlbody" s then PPurlbody
      else if String.isSubstring "url" s then PPurl  (* although url is a substring of urlbody, if the above case is true, we won't branch here *)
      else if String.isSubstring "word" s then PPword
      else if String.isSubstring "id" s then PPid
      else if String.isSubstring "bXML" s then PPbXML
      else if String.isSubstring "eXML" s then PPeXML
      else if String.isSubstring "white" s then ((*if String.compare(" ", str)=EQUAL then print "white\n" else ();*)PPwhite)
      else if String.isSubstring "message" s then PPmessage
      else if String.isSubstring "permission" s then PPpermission      
      else if String.isSubstring "blob" s then PPblob
      else if String.isSubstring "punc" s then (if (String.size str)=1 then (PPpunc str) else PPblob)
      else if String.isSubstring "text" s then PPtext
      else if String.isSubstring "lit" s then (
        case str of 
            "," => PPpunc ","
          | "." => PPpunc "."
          | "/" => PPpunc "/"
          | ":" => PPpunc ":"
          | " " => PPwhite
          | "|" => PPpunc "|"
          | _ => PPblob
      )
      else PPblob

    exception CPtagError

    fun extractLog path listfile : BSToken list list = 
      let
        val files : string list = loadFile listfile
        fun loadOne (str, ret) = 
          if Char.compare(#"#", String.sub(str, 0))=EQUAL then ret
          else ret@(loadFile (path^str))
        val data : string list = List.foldl loadOne [] files
(*  val _ = List.app print data *)
        fun splitRec (d, l): string list list =
          case d of
            [] => List.take(l, (List.length l)-1)
           |hd::tl => if String.compare(hd, "EOR")=EQUAL then splitRec (tl, l@[[]])
                      else  
                        let
                          val wl = List.nth(l, (List.length l)-1) 
                        in
                          splitRec (tl, List.take(l, (List.length l)-1)@[(wl@[hd])])
                        end

        val splitd = splitRec(data, [[]])
(* val _ = List.app print (List.nth(splitd, 0)) *)
        fun constrOneRecord d : (BToken*string) list =
          let
            fun constrOneToken (t, (ret, cptagl)) =
            if (String.size t)=0 then (ret, cptagl)
            else (
              if String.compare(t, "CheckPoint")=EQUAL then (
                let
                  val thiscptag = List.hd cptagl
                in
                  if thiscptag<>0 then (ret, 1::cptagl)   (* nested checkpoint, need to create a new entry *) 
                  else (ret, [1])
                end 
                )
              else if String.compare(t, "Rollback")=EQUAL then (
                let
                  val thiscptag = List.hd cptagl
                  val nested = List.length cptagl
                in
                  if thiscptag<1 then raise CPtagError
                  else if thiscptag=1 then (
                    if nested=1 then (ret, [0])
                    else (ret, List.drop(cptagl, 1))
                  )
                  else ( 
                    if nested=1 then (List.drop(ret, thiscptag-1), [0])
                        else (
                          let
                            val subv = thiscptag-1
                            fun subOne i = i-thiscptag+1
                          in
                            (List.drop(ret, thiscptag-1), List.map subOne (List.drop(cptagl, 1)))
                          end)
                  )
                end
                )
              else if String.compare(t, "Commit")=EQUAL then (
                let
                  val thiscptag = List.hd cptagl
                  val nested = List.length cptagl
                in
                  if thiscptag<1 then raise CPtagError
                  else (
                    if nested=1 then (ret, [0])
                    else (ret, List.drop(cptagl, 1))
                  )
                end
                )
              else         
                let 
                  fun isColon c = c = #":"
                  val (junk1, dataString) = Substring.splitr (not o isColon) (Substring.full t)
                  val (tokenName, junk2) = Substring.splitl (not o isColon) (Substring.full t)
                in
                  if Substring.compare(dataString, Substring.full "Rollback")=EQUAL then (
                    let
                      val thiscptag = List.hd cptagl
                      val nested = List.length cptagl
                    in
                      if thiscptag<1 then raise CPtagError
                      else if thiscptag=1 then (
                        if nested=1 then (ret, [0])
                        else (ret, List.drop(cptagl, 1))
                      )
                      else (
                        if nested=1 then (List.drop(ret, thiscptag-1), [0])
                        else (
                          let
                            val subv = thiscptag-1
                            fun subOne i = i-thiscptag+1
                          in
                            (List.drop(ret, thiscptag-1), List.map subOne (List.drop(cptagl, 1)))
                          end)
                      )
                    end
                  )
                  else if Substring.compare(dataString, Substring.full "CheckPoint")=EQUAL then (
                    let
                      val thiscptag = List.hd cptagl
                    in
                      if thiscptag<>0 then (ret, 1::cptagl)
                      else (ret, [1])
                    end 
                    )
                  else ( (*print ((Substring.string tokenName)^"["^t^"]""\n");*)
                    let
                      val thiscptag = List.hd cptagl
                      fun addOne i = i+1
                      fun findLastRead s = if (not (String.isSubstring "read:" s)) then s
                                           else 
                                             let
                                               val (pres, sufs) = Substring.position "read:" (Substring.full s)
                                             in
                                               findLastRead (Substring.string(Substring.triml 5 sufs))
                                             end
                      val (junk1, dataString) = Substring.splitr (not o isColon) (Substring.full t)
                    (*  val _ = if ((Substring.size dataString)=0) then (print (Int.toString (String.size t));print t; raise CPtagError) else () *) 
                      val newdataString = 
                        if (String.isSubstring "read:" t) then (Substring.full (findLastRead t))
                        else if (Substring.size dataString)=0 then (
                          let val (s1, s2) = Substring.splitr (not o isColon) junk1 in (Substring.full ((Substring.string s2)^":")) end
                        )
                        else dataString
                      val rett = case (tstringToBToken(Substring.string tokenName, Substring.string newdataString)) of
                                      PPblob => (tstringToBToken((Substring.string junk1), Substring.string newdataString))
                                    | othert => othert
                    in
                      if (Substring.size newdataString)=0 then (ret, cptagl)
                      else (
                      if thiscptag>0 then  
                      ((rett, Substring.string newdataString)::ret, List.map addOne cptagl)
                      else 
                      ((rett, Substring.string newdataString)::ret, cptagl)
                      )
                    end
                    )
                end
              )  
(*
val (s1, s2) = List.nth ((List.foldl constrOneToken [] d), 0)
val _ = print s1 
*)
            val (revtable, retag) = List.foldl constrOneToken ([], [0]) d     
          in
            List.rev(revtable)
          end
      in
        List.map constrOneRecord splitd
      end

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

(* char-by-char HMM: there're 3 tables to construct.
   (t1, t2), (c vector, t), t
*)

    exception stringSizeError

    fun constrTokenTable l inittable = 
      let
        fun countOne (tslist, btokentable) = 
          let
(*            val _ = if List.length tslist = 0 then print "Error\n" else () *)
            fun countOneToken ((btoken, str), btt) =
              let
                val num = String.size str
                val oldnum = BTokenTable.find(btt, btoken)
              in
                if num=0 then (print (BTokenToName btoken); raise stringSizeError)
                else (
                  case oldnum of
                      NONE => BTokenTable.insert(btt, btoken, num)
                    | SOME n => (
                        let
                          val (newtable, junk) = BTokenTable.remove(btt, btoken)
                        in
                          BTokenTable.insert(newtable, btoken, num+n)
                        end
                      )
                )
              end
          in
            List.foldl countOneToken btokentable tslist
          end
      in
        List.foldl countOne inittable l
      end

    fun constrTokenPairTable l inittable = 
      let
        fun countOne (tslist, btokenpairtable) = 
          let
            fun countOneToken ((btoken, str), (pre, btt)) =
              let
                val length = String.size str
                val thistable = ref btt
              in
                ((
                case pre of
                    SOME pretoken => ( 
                      let
                        val firstoldn = BTokenPairTable.find(btt, (pretoken, btoken))
                        val _ = if length=0 then raise stringSizeError else ()
                      in 
                        thistable := (case firstoldn of
                                             NONE => BTokenPairTable.insert(btt, (pretoken, btoken), 1)
                                           | SOME n => (
                                               let
                                                 val (newtable, junk) = BTokenPairTable.remove(btt, (pretoken, btoken))
                                               in
                                                 BTokenPairTable.insert(newtable, (pretoken, btoken), n+1)
                                               end
                                             ))
                      end
                    )
                  | NONE => ());
                if length=1 then (SOME btoken, !thistable)
                else (
                  case BTokenPairTable.find(!thistable, (btoken, btoken)) of
                      NONE => (SOME btoken, BTokenPairTable.insert(!thistable, (btoken, btoken), length-1))
                    | SOME n => (
                        let
                          val (newtable, junk) = BTokenPairTable.remove(!thistable, (btoken, btoken))
                        in
                          (SOME btoken, BTokenPairTable.insert(newtable, (btoken, btoken), n+length-1))
                        end
                      )
                ))
              end
            val (junk, tableret) = List.foldl countOneToken (NONE, btokenpairtable) tslist
          in
            tableret
          end
      in
        List.foldl countOne inittable l
      end

    fun constrBeginTokenTable l inittable= 
      let
        fun countOne (tslist, btokentable) = 
          let
(*            val _ = if List.length tslist = 0 then print "Error\n" else () *)
            val (first, str) = List.nth(tslist, 0)
            val pre = BTokenTable.find(btokentable, first)
          in
            case pre of
                NONE => BTokenTable.insert(btokentable, first, 1)
              | SOME n => (
                  let 
                    val (newtable, junk) = BTokenTable.remove(btokentable, first)
                  in
                    BTokenTable.insert(newtable, first, n+1)
                  end
                  )
          end
      in
        List.foldl countOne inittable l
      end

    fun constrEndTokenTable l inittable = 
      let
        fun countOne (tslist, btokentable) = 
          let
            val (last, str) = List.nth(tslist, (List.length tslist)-1)
            val pre = BTokenTable.find(btokentable, last)
          in
            case pre of
                NONE => BTokenTable.insert(btokentable, last, 1)
              | SOME n => (
                  let 
                    val (newtable, junk) = BTokenTable.remove(btokentable, last)
                  in
                    BTokenTable.insert(newtable, last, n+1)
                  end
                  )
          end
      in
        List.foldl countOne inittable l
      end

    fun constrListTokenTable l inittable = 
      let
        fun countOne (tslist, btokentable) = 
          let
            fun countOneToken ((btoken, str), btt) =
              let
                fun countOneChar (c, btt) =
                  let
                    val v = charToList c
                  in
                    case ListBTokenPairTable.find(btt, (v, btoken)) of
                        NONE => ListBTokenPairTable.insert(btt, (v, btoken), 1)
                      | SOME n => (
                          let
                            val (newtable, junk) = ListBTokenPairTable.remove(btt, (v, btoken))
                          in
                            ListBTokenPairTable.insert(btt, (v, btoken), n+1)
                          end
                        )
                  end
              in
                List.foldl countOneChar btt (String.explode str) 
              end
          in
            List.foldl countOneToken btokentable tslist
          end
      in
        List.foldl countOne inittable l
      end

    fun constrCharTokenTable l inittable = 
      let
        fun countOne (tslist, btokentable) = 
          let
            fun countOneToken ((btoken, str), btt) =
              let
                fun countOneChar (c, btt) =
                    case CharBTokenPairTable.find(btt, (c, btoken)) of
                        NONE => CharBTokenPairTable.insert(btt, (c, btoken), 1)
                      | SOME n => (
                          let
                            val (newtable, junk) = CharBTokenPairTable.remove(btt, (c, btoken))
                          in
                            CharBTokenPairTable.insert(btt, (c, btoken), n+1)
                          end
                        )
              in
                List.foldl countOneChar btt (String.explode str) 
              end
          in
            List.foldl countOneToken btokentable tslist
          end
      in
        List.foldl countOne inittable l
      end

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

          fun dumpToken path t = 
            let
	          val strm = TextIO.openOut (path^"TokenCount")
              val outlist = BTokenTable.listItemsi t (*table1*)
              fun output (bt, i) = TextIO.output(strm, (BTokenToName bt)^"="^(Int.toString i)^"\n")  
              val _ = List.map output outlist
            in
              TextIO.closeOut strm 
            end
          fun dumpTokenPair path t = 
            let
	          val strm = TextIO.openOut (path^"TokenPairCount")
              val outlist = BTokenPairTable.listItemsi t (*table2*)
              fun output ((bt1,bt2), i) = TextIO.output(strm, ((BTokenToName bt1)^" "^(BTokenToName bt2)^"="^(Int.toString i)^"\n"))  
              val _ = List.map output outlist
            in
              TextIO.closeOut strm 
            end
          fun dumpListToken path t = 
            let
	          val strm = TextIO.openOut (path^"VecTokenCount")
              val outlist = ListBTokenPairTable.listItemsi t (*table3*)
              fun listToString il = 
                let
                  fun foo (i, ret) = ret^(Int.toString i)
                in
                  List.foldl foo "" il
                end
              fun output ((l, bt), i) = TextIO.output(strm, (listToString l)^" "^(BTokenToName bt)^"="^(Int.toString i)^"\n")  
              val _ = List.map output outlist
            in
              TextIO.closeOut strm 
            end
          fun dumpCharToken path t = 
            let
	          val strm = TextIO.openOut (path^"CharTokenCount")
              val outlist = CharBTokenPairTable.listItemsi t 
              fun listToString il = 
                let
                  fun foo (i, ret) = ret^(Int.toString i)
                in
                  List.foldl foo "" il
                end
              fun output ((c, bt), i) = TextIO.output(strm, (Char.toString c)^" "^(BTokenToName bt)^"="^(Int.toString i)^"\n")  
              val _ = List.map output outlist
            in
              TextIO.closeOut strm 
            end
          fun dumpBeginToken path t = 
            let
	          val strm = TextIO.openOut (path^"BeginTokenCount")
              val outlist = BTokenTable.listItemsi t (*table4*)
              fun output (bt, i) = TextIO.output(strm, (BTokenToName bt)^"="^(Int.toString i)^"\n")  
              val _ = List.map output outlist
            in
              TextIO.closeOut strm 
            end
          fun dumpEndToken path t = 
            let
	          val strm = TextIO.openOut (path^"EndTokenCount")
              val outlist = BTokenTable.listItemsi t (*table5*)
              fun output (bt, i) = TextIO.output(strm, (BTokenToName bt)^"="^(Int.toString i)^"\n")  
              val _ = List.map output outlist
            in
              TextIO.closeOut strm 
            end
          fun dumpInitProbSmooth path t = 
            let
	          val strm = TextIO.openOut (path^"InitProb")
              val outlist = BTokenTable.listItemsi t (*table4*)
              fun sumAll ((bt, i), ret) = ret+i
              val sum = List.foldl sumAll 0 outlist
              val wholelist = BTokenMapF.listItemsi btokentable
              val tnum = List.length wholelist
              fun output (bt, s) = 
                let val lookupr = defaultVal(BTokenTable.find(t, bt))
                in
                  TextIO.output(strm, (Real.toString((lookupr+(!lambda))/((Real.fromInt sum)+(Real.fromInt tnum)*(!lambda))))^"\n") 
                end
              val _ = List.app output wholelist
            in
              TextIO.closeOut strm 
            end
          fun dumpTransProbSmooth path t1 t2 = 
            let
	          val strm = TextIO.openOut (path^"TransProb")
              val wholelist = BTokenMapF.listItemsi btokentable
              val tnum = List.length wholelist
              fun output (bt1, s1) = 
                let
                  fun outputin ((bt2, s2), ret) =
                    let val lookupr = defaultVal(BTokenPairTable.find(t2, (bt1, bt2)))
                    in
                      ret^Real.toString((lookupr+(!lambda))/((defaultVal(BTokenTable.find(t1, bt1)))+(Real.fromInt tnum)*(!lambda)))^" "  
                    end
                  val outputstrm = List.foldl outputin "" wholelist
                in
                  TextIO.output(strm, outputstrm^"\n")
                end 
              val _ = List.app output wholelist
            in
              TextIO.closeOut strm 
            end
          fun dumpEmitProbSmooth path t1 t3 = 
            let
	          val strm = TextIO.openOut (path^"EmitProb")
(*
              fun constrList i =
                let
                  val bit9 = if i-512>=0 then 1 else 0
                  val num9 = i-512*bit9 
                  val bit8 = if num9-256>=0 then 1 else 0
                  val num8 = num9-256*bit8
                  val bit7 = if num8-128>=0 then 1 else 0
                  val num7 = num8-128*bit7
                  val bit6 = if num7-64>=0 then 1 else 0
                  val num6 = num7-64*bit6
                  val bit5 = if num6-32>=0 then 1 else 0
                  val num5 = num6-32*bit5
                  val bit4 = if num5-16>=0 then 1 else 0
                  val num4 = num5-16*bit4
                  val bit3 = if num4-8>=0 then 1 else 0
                  val num3 = num4-8*bit3
                  val bit2 = if num3-4>=0 then 1 else 0
                  val num2 = num3-4*bit2
                  val bit1 = if num2-2>=0 then 1 else 0
                  val num1 = num2-2*bit1
                  val bit0 = if num1-1>=0 then 1 else 0
                in
                  if i=1023 then [[1,1,1,1,1,1,1,1,1,1]]
                  else [bit9, bit8, bit7, bit6, bit5, bit4, bit3, bit2, bit1, bit0]::(constrList (i+1))
                end
*)
              fun constrList i =
                let
                  val thislist = intToList i
                in
(*                  if i+1 = Real.toInt IEEEReal.TO_NEAREST (Math.pow((Real.fromInt 2), (Real.fromInt fvectorbits))) then [thislist]*)
                  if i = maxFOrd then [thislist] 
                  else thislist::(constrList (i+1))
                end
              val wholelist1 = constrList 0
              val charnum = List.length wholelist1
              val wholelist2 = BTokenMapF.listItemsi btokentable
              fun output l = 
                let
                  fun outputin ((bt, s), ret) =
                    let val lookupr = defaultVal(ListBTokenPairTable.find(t3, (l, bt)))
                    in
                      ret^Real.toString((lookupr+(!lambda))/((defaultVal(BTokenTable.find(t1, bt)))+(Real.fromInt charnum)*(!lambda)))^" "
                    end
                  val outputstrm = List.foldl outputin "" wholelist2
                in
                  TextIO.output(strm, outputstrm^"\n")
                end 
              val _ = List.app output wholelist1
            in
              TextIO.closeOut strm 
            end
          fun dumpEmitProbSmoothChar path t1 t3 = 
            let
	          val strm = TextIO.openOut (path^"EmitProb")
              fun constrList i =
                  if i = Char.maxOrd then [i]
                  else i::(constrList (i+1))
              val wholelist1 = constrList 0
              val charnum = List.length wholelist1
              val wholelist2 = BTokenMapF.listItemsi btokentable
              fun output l = 
                let
                  fun outputin ((bt, s), ret) =
                    let val lookupr = defaultVal(CharBTokenPairTable.find(t3, ((Char.chr l), bt)))
                    in
                      ret^Real.toString((lookupr+(!lambda))/((defaultVal(BTokenTable.find(t1, bt)))+(Real.fromInt charnum)*(!lambda)))^" "
                    end
                  val outputstrm = List.foldl outputin "" wholelist2
                in
                  TextIO.output(strm, outputstrm^"\n")
                end 
              val _ = List.app output wholelist1
            in
              TextIO.closeOut strm 
            end
          fun dumpEndProbSmooth path t = 
            let
	          val strm = TextIO.openOut (path^"EndProb")
              val outlist = BTokenTable.listItemsi t (*table5*)
              fun sumAll ((bt, i), ret) = ret+i
              val sum = List.foldl sumAll 0 outlist
              val wholelist = BTokenMapF.listItemsi btokentable
              val tnum = List.length wholelist
              fun output (bt, s) = 
                let val lookupr = defaultVal(BTokenTable.find(t, bt))
                in
                  TextIO.output(strm, (Real.toString ((lookupr+(!lambda))/((Real.fromInt sum)+(Real.fromInt tnum)*(!lambda))))^"\n")
                end
              val _ = List.app output wholelist
            in
              TextIO.closeOut strm 
            end
          fun dumpInitProb path t = 
            let
	          val strm = TextIO.openOut (path^"InitProb")
              val outlist = BTokenTable.listItemsi t
              fun sumAll ((bt, i), ret) = ret+i
              val sum = List.foldl sumAll 0 outlist
              val wholelist = BTokenMapF.listItemsi btokentable
              fun output (bt, s) = 
                case BTokenTable.find(t, bt) of
                    NONE => TextIO.output(strm, "0.0\n")
                  | SOME n => TextIO.output(strm, (Real.toString ((Real.fromInt n)/(Real.fromInt sum)))^"\n") 
              val _ = List.app output wholelist
            in
              TextIO.closeOut strm 
            end
          fun dumpTransProb path t1 t2 = 
            let
	          val strm = TextIO.openOut (path^"TransProb")
              val wholelist = BTokenMapF.listItemsi btokentable
              fun output (bt1, s1) = 
                let
                  fun outputin ((bt2, s2), ret) =
                    case BTokenPairTable.find(t2, (bt1, bt2)) of
                        NONE => ret^"0 "
                      | SOME n => ret^Real.toString((Real.fromInt n)/(Real.fromInt (Option.valOf(BTokenTable.find(t1, bt1)))))^" "
                  val outputstrm = List.foldl outputin "" wholelist
                in
                  TextIO.output(strm, outputstrm^"\n")
                end 
              val _ = List.app output wholelist
            in
              TextIO.closeOut strm 
            end
          fun dumpEmitProb path t1 t3 = 
            let
	          val strm = TextIO.openOut (path^"EmitProb")
(*
              fun constrList i =
                let
                  val bit9 = if i-512>=0 then 1 else 0
                  val num9 = i-512*bit9 
                  val bit8 = if num9-256>=0 then 1 else 0
                  val num8 = num9-256*bit8
                  val bit7 = if num8-128>=0 then 1 else 0
                  val num7 = num8-128*bit7
                  val bit6 = if num7-64>=0 then 1 else 0
                  val num6 = num7-64*bit6
                  val bit5 = if num6-32>=0 then 1 else 0
                  val num5 = num6-32*bit5
                  val bit4 = if num5-16>=0 then 1 else 0
                  val num4 = num5-16*bit4
                  val bit3 = if num4-8>=0 then 1 else 0
                  val num3 = num4-8*bit3
                  val bit2 = if num3-4>=0 then 1 else 0
                  val num2 = num3-4*bit2
                  val bit1 = if num2-2>=0 then 1 else 0
                  val num1 = num2-2*bit1
                  val bit0 = if num1-1>=0 then 1 else 0
                in
                  if i=1023 then [[1,1,1,1,1,1,1,1,1,1]]
                  else [bit9, bit8, bit7, bit6, bit5, bit4, bit3, bit2, bit1, bit0]::(constrList (i+1))
                end
*)
              fun constrList i =
                let
                  val thislist = intToList i
                in
(*                  if i+1 = Real.toInt IEEEReal.TO_NEAREST (Math.pow((Real.fromInt 2), (Real.fromInt fvectorbits))) then [thislist]*)
                  if i = maxFOrd then [thislist] 
                  else thislist::(constrList (i+1))
                end
              val wholelist1 = constrList 0
              val wholelist2 = BTokenMapF.listItemsi btokentable
              fun output l = 
                let
                  fun outputin ((bt, s), ret) =
                    case ListBTokenPairTable.find(t3, (l, bt)) of
                        NONE => ret^"0 "
                      | SOME n => ret^Real.toString((Real.fromInt n)/(Real.fromInt (Option.valOf(BTokenTable.find(t1, bt)))))^" "
                  val outputstrm = List.foldl outputin "" wholelist2
                in
                  TextIO.output(strm, outputstrm^"\n")
                end 
              val _ = List.app output wholelist1
            in
              TextIO.closeOut strm 
            end
          fun dumpEmitProbChar path t1 t3 = 
            let
	          val strm = TextIO.openOut (path^"EmitProb")
              fun constrList i =
                  if i = Char.maxOrd then [i]
                  else i::(constrList (i+1))
              val wholelist1 = constrList 0
              val wholelist2 = BTokenMapF.listItemsi btokentable
              fun output l = 
                let
                  fun outputin ((bt, s), ret) =
                    case CharBTokenPairTable.find(t3, ((Char.chr l), bt)) of
                        NONE => ret^"0 "
                      | SOME n => ret^Real.toString((Real.fromInt n)/(Real.fromInt (Option.valOf(BTokenTable.find(t1, bt)))))^" "
                  val outputstrm = List.foldl outputin "" wholelist2
                in
                  TextIO.output(strm, outputstrm^"\n")
                end 
              val _ = List.app output wholelist1
            in
              TextIO.closeOut strm 
            end
          fun dumpEndProb path t = 
            let
	          val strm = TextIO.openOut (path^"EndProb")
              val outlist = BTokenTable.listItemsi t
              fun sumAll ((bt, i), ret) = ret+i
              val sum = List.foldl sumAll 0 outlist
              val wholelist = BTokenMapF.listItemsi btokentable
              fun output (bt, s) = 
                case BTokenTable.find(t, bt) of
                    NONE => TextIO.output(strm, "0.0\n")
                  | SOME n => TextIO.output(strm, (Real.toString ((Real.fromInt n)/(Real.fromInt sum)))^"\n") 
              val _ = List.app output wholelist
            in
              TextIO.closeOut strm 
            end
          fun dumpTokenName path t = 
            let
	          val strm = TextIO.openOut (path^"TokenName")
              val wholelist = BTokenMapF.listItemsi btokentable
              fun output (bt, s) = TextIO.output(strm, (BTokenToName bt)^"\n")
              val _ = List.app output wholelist
            in
              TextIO.closeOut strm 
            end

    fun dumpCCHMM ( path : string ) : unit = 
        let 
          val _ = print ("Printing char-by-char HMM to files under "^path^"\n")
          val list = extractLog "training/log/" "training/log/log.list"
          val table1 = constrTokenTable list BTokenTable.empty
(*          val _ = print "1\n" *)
          val table2 = constrTokenPairTable list BTokenPairTable.empty
(*          val _ = print "2\n" *)
          val table3 = constrListTokenTable list ListBTokenPairTable.empty
(*          val _ = print "3\n" *)
          val table4 = constrBeginTokenTable list BTokenTable.empty
(*          val _ = print "4\n" *)
          val table5 = constrEndTokenTable list BTokenTable.empty
(*          val _ = print "5\n" *)
          in
            if Real.compare(!lambda, 0.0)=EQUAL then 
            (
            dumpToken path table1; (*print "TokenCount generated.\n";*) 
            dumpTokenPair path table2; (*print "TokenPairCount generated.\n";*)
            dumpListToken path table3; (*print "CharTokenCount generated.\n";*)
            dumpBeginToken path table4; (*print "\n";*)
            dumpEndToken path table5; (*print "5\n";*)
            dumpInitProb path table4; (*print "6\n";*)
            dumpTransProb path table1 table2; (*print "7\n";*)
            dumpEmitProb path table1 table3; (*print "8\n";*)
            dumpEndProb path table5; (*print "9\n";*)
            dumpTokenName path table1 (*; print "10\n"*)
            )
            else
            (
            dumpToken path table1; (*print "TokenCount generated.\n";*) 
            dumpTokenPair path table2; (*print "TokenPairCount generated.\n";*)
            dumpListToken path table3; (*print "CharTokenCount generated.\n";*)
            dumpBeginToken path table4; (*print "\n";*)
            dumpEndToken path table5; (*print "5\n";*)
            dumpInitProbSmooth path table4; (*print "6\n";*)
            dumpTransProbSmooth path table1 table2; (*print "7\n";*)
            dumpEmitProbSmooth path table1 table3; (*print "8\n";*)
            dumpEndProbSmooth path table5; (*print "9\n";*)
            dumpTokenName path table1 (*; print "10\n"*)
            )
          end 

    fun dumpCCHMMChar ( path : string ) : unit = 
        let 
          val _ = print ("Printing char-by-char HMM to files under "^path^"\n")
          val list = extractLog "training/log/" "training/log/log.list"
          val table1 = constrTokenTable list BTokenTable.empty
(*          val _ = print "1\n" *)
          val table2 = constrTokenPairTable list BTokenPairTable.empty
(*          val _ = print "2\n" *)
          val table3 = constrCharTokenTable list CharBTokenPairTable.empty
(*          val _ = print "3\n" *)
          val table4 = constrBeginTokenTable list BTokenTable.empty
(*          val _ = print "4\n" *)
          val table5 = constrEndTokenTable list BTokenTable.empty
(*          val _ = print "5\n" *)
          in
            if Real.compare(!lambda, 0.0)=EQUAL then 
            (
            dumpToken path table1; (*print "TokenCount generated.\n";*) 
            dumpTokenPair path table2; (*print "TokenPairCount generated.\n";*)
            dumpCharToken path table3; (*print "CharTokenCount generated.\n";*)
            dumpBeginToken path table4; (*print "\n";*)
            dumpEndToken path table5; (*print "5\n";*)
            dumpInitProb path table4; (*print "6\n";*)
            dumpTransProb path table1 table2; (*print "7\n";*)
            dumpEmitProbChar path table1 table3; (*print "8\n";*)
            dumpEndProb path table5; (*print "9\n";*)
            dumpTokenName path table1 (*; print "10\n"*)
            )
            else
            (
            dumpToken path table1; (*print "TokenCount generated.\n";*) 
            dumpTokenPair path table2; (*print "TokenPairCount generated.\n";*)
            dumpCharToken path table3; (*print "CharTokenCount generated.\n";*)
            dumpBeginToken path table4; (*print "\n";*)
            dumpEndToken path table5; (*print "5\n";*)
            dumpInitProbSmooth path table4; (*print "6\n";*)
            dumpTransProbSmooth path table1 table2; (*print "7\n";*)
            dumpEmitProbSmoothChar path table1 table3; (*print "8\n";*)
            dumpEndProbSmooth path table5; (*print "9\n";*)
            dumpTokenName path table1 (*; print "10\n"*)
            )
          end 

    exception BadTable

    fun readTokenTable path =
      let
        val list = loadFile (path^"TokenCount")
        fun extractOne (t, table) =
          let
            fun isEqual c = c = #"="
            val (tokenj, counts) = Substring.splitr (not o isEqual) (Substring.full t)
            val token = Substring.trimr 1 tokenj 
          in 
            BTokenTable.insert(table, nameToBToken (Substring.string token), Option.valOf(Int.fromString (Substring.string counts)))
          end
      in
        List.foldl extractOne BTokenTable.empty list
      end

    fun readBoundaryTokenTable path tag =
      let
        val list = 
          case tag of
              0 => loadFile (path^"BeginTokenCount")
            | 1 => loadFile (path^"EndTokenCount")
            | _ => loadFile (path^"EndTokenCount")
        fun extractOne (t, table) =
          let
            fun isEqual c = c = #"="
            val (tokenj, counts) = Substring.splitr (not o isEqual) (Substring.full t)
            val token = Substring.trimr 1 tokenj 
          in 
            BTokenTable.insert(table, nameToBToken (Substring.string token), Option.valOf(Int.fromString (Substring.string counts)))
          end
        val inittable = List.foldl extractOne BTokenTable.empty list
        val sumlist = BTokenTable.listItems inittable
        fun addAll (i, ret) = i+ret
        val sum = List.foldl addAll 0 sumlist
        val newlist = BTokenTable.listItemsi inittable
        fun createNew ((key, i), ret) = BTokenTable.insert(ret, key, (Real.fromInt i)/(Real.fromInt sum))
      in
        List.foldl createNew BTokenTable.empty newlist
      end

    fun readRawBoundaryTokenTable path tag =
      let
        val list = 
          case tag of
              0 => loadFile (path^"BeginTokenCount")
            | 1 => loadFile (path^"EndTokenCount")
            | _ => loadFile (path^"EndTokenCount")
        fun extractOne (t, table) =
          let
            fun isEqual c = c = #"="
            val (tokenj, counts) = Substring.splitr (not o isEqual) (Substring.full t)
            val token = Substring.trimr 1 tokenj 
          in 
            BTokenTable.insert(table, nameToBToken (Substring.string token), Option.valOf(Int.fromString (Substring.string counts)))
          end
        val inittable = List.foldl extractOne BTokenTable.empty list
      in
        inittable
      end

    fun readTokenPairTable path =
      let
        val list = loadFile (path^"TokenPairCount")
        fun extractOne (t, table) =
          let
            fun isComma c = c = #" "
            fun isEqual c = c = #"="
            val (token, counts) = Substring.splitr (not o isEqual) (Substring.full t) 
            val (token1j, token2) = Substring.splitr (not o isComma) (Substring.trimr 1 token)
            val token1 = Substring.trimr 1 token1j
            val key = (nameToBToken (Substring.string token1), nameToBToken (Substring.string token2))
          in 
            BTokenPairTable.insert(table, key, Option.valOf(Int.fromString (Substring.string counts)))
          end
      in
        List.foldl extractOne BTokenPairTable.empty list
      end

    fun readListTokenTable path =
      let
        val list = loadFile (path^"VecTokenCount")
        fun extractOne (t, table) =
          let
            fun isComma c = c = #" "
            fun isEqual c = c = #"="
            val (token, counts) = Substring.splitr (not o isEqual) (Substring.full t) 
            val (charvecj, token2) = Substring.splitr (not o isComma) (Substring.trimr 1 token)
            val charvec = Substring.trimr 1 charvecj
(*            fun charToInt c = Option.valOf(Int.fromString(Char.toString c)) *)
            fun charToInt c =
              case c of
                  #"0" => 0
                | #"1" => 1
                | _ => (print (Char.toString c); raise BadTable)
            fun stringToList l = List.map charToInt (Substring.explode l)
            val key = (stringToList charvec, nameToBToken (Substring.string token2))
          in 
            ListBTokenPairTable.insert(table, key, Option.valOf(Int.fromString (Substring.string counts)))
          end
      in
        List.foldl extractOne ListBTokenPairTable.empty list
      end

    fun readCharTokenTable path =
      let
        val list = loadFile (path^"CharTokenCount")
        fun extractOne (t, table) =
          let
            val ch = String.sub(t, 0)
            val newt = Substring.triml 2 (Substring.full t) (* delete char and whitespace *)
            fun isEqual c = c = #"="
            val (token, counts) = Substring.splitr (not o isEqual) (Substring.full t) 
            val newtoken = Substring.trimr 1 token
            val key = (ch, nameToBToken (Substring.string newtoken))
          in 
            CharBTokenPairTable.insert(table, key, Option.valOf(Int.fromString (Substring.string counts)))
          end
      in
        List.foldl extractOne CharBTokenPairTable.empty list
      end

    fun computeProb pathgraph : Seqset list = 
      let
(* val _ = print "1\n"*)
        val tokentable = readTokenTable "training/"
        val begintokentable = readBoundaryTokenTable "training/" 0
        val endtokentable = readBoundaryTokenTable "training/" 1
        val tokenpairtable = readTokenPairTable "training/"
        val listtokentable = readListTokenTable "training/"
        fun transProb tslist =
          let
            fun addOne (((t, s), l), (pre, ret)) =
              if compBToken(t, PPempty)=EQUAL then (pre, ret)
              else  
              let
                val first = 
                  case pre of
                      NONE => (* Math.ln(defaultRVal1(BTokenTable.find(begintokentable, t))) *) 0.0
                    | SOME pret => Math.ln(defaultVal1(BTokenPairTable.find(tokenpairtable, (pret, t))))
                val rest = (Real.fromInt ((String.size s)-1)) * (Math.ln(defaultVal1(BTokenPairTable.find(tokenpairtable, (t, t)))))
              in
                (SOME t, first+rest+ret)
              end 
            val (lastt, most) = List.foldl addOne (NONE, 0.0) tslist
            val all = most (*+ (Math.ln(defaultRVal1(BTokenTable.find(endtokentable, Option.valOf(lastt)))))*)
          in
            all
          end 
        fun emitProb tslist =
          let
            fun addOne (((t,s),l), ret) =
              if compBToken(t, PPempty)=EQUAL then ret
              else 
              let
                fun addOneChar (c, v) =
                  let
                    val value = defaultVal1(ListBTokenPairTable.find(listtokentable, (charToList c, t)))
                  in
                    (Math.ln value) + v
                  end
              in
                List.foldl addOneChar ret (String.explode s)
              end
          in
            List.foldl addOne 0.0 tslist
          end
        fun doOneTList (tsllist, f) = 
          let
            val transp = transProb tsllist
            val emitp = emitProb tsllist
(*val _ = print (Real.toString (transp+emitp)^"\n") *)
          in
            (tsllist, transp+emitp)
          end
        fun doOneSeqset ss = List.map doOneTList ss
        val ret = List.map doOneSeqset pathgraph
(* val _ = print "2\n" *)
      in
        ret
      end

    fun computeProbChar pathgraph : Seqset list = 
      let
        val tokentable = readTokenTable "training/"
        val begintokentable = readBoundaryTokenTable "training/" 0
        val endtokentable = readBoundaryTokenTable "training/" 1
        val tokenpairtable = readTokenPairTable "training/"
        val listtokentable = readCharTokenTable "training/"
        fun transProb tslist =
          let
            fun addOne (((t, s), l), (pre, ret)) = 
              let
                val first = 
                  case pre of
                      NONE => Math.ln(defaultRVal1(BTokenTable.find(begintokentable, t)))
                    | SOME pret => Math.ln(defaultVal1(BTokenPairTable.find(tokenpairtable, (pret, t))))
                val rest = (Real.fromInt ((String.size s)-1)) * (Math.ln(defaultVal1(BTokenPairTable.find(tokenpairtable, (t, t)))))
              in
                (SOME t, first+rest+ret)
              end 
            val (lastt, most) = List.foldl addOne (NONE, 0.0) tslist
            val all = most + (Math.ln(defaultRVal1(BTokenTable.find(endtokentable, Option.valOf(lastt)))))
          in
            all
          end 
        fun emitProb tslist =
          let
            fun addOne (((t,s),l), ret) = 
              let
                fun addOneChar (c, v) =
                  let
                    val value = defaultVal1(CharBTokenPairTable.find(listtokentable, (c, t))) 
                  in
                    (Math.ln value) + v
                  end
              in
                List.foldl addOneChar ret (String.explode s)
              end
          in
            List.foldl addOne 0.0 tslist
          end
        fun doOneTList (tsllist, f) = (tsllist, (transProb tsllist) + (emitProb tsllist))
        fun doOneSeqset ss = List.map doOneTList ss
      in
        List.map doOneSeqset pathgraph
      end

    fun incdumpCCHMM ( path : string ) : unit = 
        let 
          val tokentable = readTokenTable "training/"
          val begintokentable = readRawBoundaryTokenTable "training/" 0
          val endtokentable = readRawBoundaryTokenTable "training/" 1
          val tokenpairtable = readTokenPairTable "training/"
          val listtokentable = readListTokenTable "training/"
          val _ = print ("Printing char-by-char HMM to files under "^path^"\n")
          val list = extractLog "training/log/" "training/log/inc.list"
          val table1 = constrTokenTable list tokentable
(*          val _ = print "1\n" *)
          val table2 = constrTokenPairTable list tokenpairtable
(*          val _ = print "2\n" *)
          val table3 = constrListTokenTable list listtokentable
(*          val _ = print "3\n" *)
          val table4 = constrBeginTokenTable list begintokentable
(*          val _ = print "4\n" *)
          val table5 = constrEndTokenTable list endtokentable
(*          val _ = print "5\n" *)
          in
            (
            dumpToken path table1; 
            dumpTokenPair path table2; 
            dumpListToken path table3; 
            dumpBeginToken path table4; 
            dumpEndToken path table5; 
            dumpInitProb path table4; 
            dumpTransProb path table1 table2; 
            dumpEmitProb path table1 table3; 
            dumpEndProb path table5; 
            dumpTokenName path table1
            )
          end 

    fun incdumpCCHMMChar ( path : string ) : unit = 
        let 
          val tokentable = readTokenTable "training/"
          val begintokentable = readRawBoundaryTokenTable "training/" 0
          val endtokentable = readRawBoundaryTokenTable "training/" 1
          val tokenpairtable = readTokenPairTable "training/"
          val listtokentable = readCharTokenTable "training/"
          val _ = print ("Printing char-by-char HMM to files under "^path^"\n")
          val list = extractLog "training/log/" "training/log/inc.list"
          val table1 = constrTokenTable list tokentable
(*          val _ = print "1\n" *)
          val table2 = constrTokenPairTable list tokenpairtable
(*          val _ = print "2\n" *)
          val table3 = constrCharTokenTable list listtokentable
(*          val _ = print "3\n" *)
          val table4 = constrBeginTokenTable list begintokentable
(*          val _ = print "4\n" *)
          val table5 = constrEndTokenTable list endtokentable
(*          val _ = print "5\n" *)
          in
            (
            dumpToken path table1; 
            dumpTokenPair path table2; 
            dumpCharToken path table3; 
            dumpBeginToken path table4; 
            dumpEndToken path table5; 
            dumpInitProb path table4; 
            dumpTransProb path table1 table2; 
            dumpEmitProbChar path table1 table3; 
            dumpEndProb path table5; 
            dumpTokenName path table1
            )
          end 

   fun compNewContext (nc1, nc2) = 
     if (List.length nc1) < (List.length nc2) then LESS
     else if (List.length nc1) > (List.length nc2) then GREATER 
     else
       let
         fun compOne (((b1, s1), l1), ((b2, s2), l2)) = (* no need to compare strings because of the longest match rule *)
           if compBToken(b1, b2) = EQUAL then true
           else false
       in
         if ListPair.all compOne (nc1, nc2) then EQUAL
         else LESS
       end

   structure NewContextTable = RedBlackMapFn(
     struct 
       type ord_key = NewContext
	   val compare = compNewContext
	 end
   ) 

    fun chopSeqsets (ssl: Seqset list) (ll: location list) : Seqset list = (* i may not call it chop, but cut inside *) 
      let
(*val _ = print "Chopping...\n"*)
val _ = print ("chop "^(Int.toString (List.length ssl))^" seqsets.\n")
        fun findEmpty {beginloc=thisb, endloc=thise, recNo=thisr, lineNo=thisl} = if thisb = ~1 then true else false
        val init = 
          case List.find findEmpty ll of
              SOME {beginloc=thisb, endloc=thise, recNo=thisr, lineNo=thisl} => [[([((PPempty, ""),{lineNo=thisr, beginloc=0, endloc=0, recNo=thisl})], 1.0)]] 
            | NONE => []
        fun chopOneSS (ss: Seqset, result: Seqset list) = (* chop one seqset *)
          let
            val (nc1, f1) = List.nth(ss, 0)
            val ((b1, s1), l1) = List.nth(nc1, 0)
            val {beginloc=b1, endloc=e1, recNo=r1, lineNo=l1} = l1
            fun findPos l =
              let
                val {beginloc=b2, endloc=e2, recNo=r2, lineNo=l2}=l
              in
                if r1=r2 then true else false
              end  
          in
            case (List.find findPos ll) of (* find the location in location list *)
                SOME thisl =>
                  let
(*val _ = print "Found this line\n"*)
val _ = print ("line "^(Int.toString r1)^" found.\n")
                    val {beginloc=thisb, endloc=thise, recNo=thisr, lineNo=thisll} = thisl
                    fun chopOneTS (ts: Tokenseq, (ret: Seqset, rettable)) = (* chop one token sequence in the seqset *)
                      let
                        val (nc, f) = ts
                        val headt = ref 0
                        fun findBegin (((b,s),l), head) = 
                          let
                            val {beginloc=thistb, endloc=thiste, recNo=thistr, lineNo=thistl} = l
                          in
                            if head <> ~1 then head
                            else if thisb=thistb then !headt
                            else (headt := !headt + 1; head)   
                          end
                        val begint = List.foldl findBegin ~1 nc 
                        val tailt = ref 0
                        fun findEnd (((b,s),l), tail) = 
                          let
                            val {beginloc=thistb, endloc=thiste, recNo=thistr, lineNo=thistl} = l
                          in
                            if tail <> ~1 then tail
                            else if thise=thiste then !tailt
                            else (tailt := !tailt + 1; tail)   
                          end
                        val endt = List.foldl findEnd ~1 nc
(*val _ = print ("begint: "^(Int.toString begint)^" endt: "^(Int.toString endt)^"\n") *)
                      in
                        if ((begint <> ~1) andalso (endt <> ~1) andalso (endt >= begint)) then 
                          let
                            val thislist = List.drop(List.take(nc, (endt+1)), begint)
                            val retv =
                              case NewContextTable.find(rettable, thislist) of
                                  SOME i => (ret, rettable)          (* such token sequence already exists *)
                                | NONE => ([(thislist, 1.0)]@ret, NewContextTable.insert(rettable, thislist, 0))  
                          in
                            retv
                          end
                        else (ret, rettable)
                      end
                    val noprob = if thisb = ~1 then ([[([((PPempty, ""),{lineNo=thisr, beginloc=0, endloc=0, recNo=thisll})], 1.0)]]@result )
                                 else 
                                   let
                                     val (rl, junk) = List.foldl chopOneTS ([], NewContextTable.empty) ss
                                   in
                                     result@[rl]
                                   end 
                  in
                    noprob
                  end
              | NONE =>  result
          end
        val rnoprob = List.foldl chopOneSS init ssl
      in
        if ( !character = true ) then  computeProbChar rnoprob else computeProb rnoprob
      end

end
