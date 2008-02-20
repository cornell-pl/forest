structure Regexparser =
struct
    open Types
    open Basetokens
    exception TyMismatch
    structure REParser = RegExpFn (structure P=AwkSyntax structure E=DfaEngine) : REGEXP
    structure MT = MatchTree
    structure SS = Substring

(*    val recNum = ref 0  *)

    (* This function checks if a string s has a substring that matches a regex re *)
(*
    fun matchRegEx (s:string) (re:string) (*:string*string*int*int*) (* matched, remain, pos, len *)=
      let
 val _ = print ("Match string (" ^ (String.toString s) ^") with regex ("^(String.toString re) ^ ")\n") 
        val cregex = REParser.compileString re             
      in
        case (StringCvt.scanString (REParser.find cregex) s) of
          NONE => ((*print "Regex failed!\n";*) ("", "", 0, 0); NONE)
        | SOME tree => 
        case MatchTree.nth(tree,0) of
          NONE => ((*print "Regex failed!\n";*) ("", "", 0, 0); NONE)
        | SOME {len,pos} => 
        case pos of
          0 => ((*print ("Regex succeeded! Substring matched: " ^ (String.substring (s, pos, len)) ^ 
                                         " Substring remained: " ^ (String.extract (s, pos+len, NONE)) ^ "\n");*) 
                                  ((String.substring (s, pos, len)), (String.extract (s, pos+len, NONE)), pos, len); SOME tree)
        | _ => ((*print "Regex succeeded, but not at pos 0\n";*) ((String.substring (s, pos, len)), (String.extract (s, pos+len, NONE)), pos, len); SOME tree) 
      end
*)

    fun matchRegEx (s:string) (re:string) (*:string*string*int*int*) (* matched, remain, pos, len *)=
      let
(* val _ = print ("Match string (" ^ (String.toString s) ^") with regex ("^(String.toString re) ^ ")\n") *)
        val cregex = REParser.compileString re             
      in
        case (StringCvt.scanString (REParser.find cregex) s) of
          NONE => ((*print "Regex failed!\n";*) NONE)
        | SOME tree => 
        case MatchTree.nth(tree,0) of
          NONE => ((*print "Regex failed!\n";*) NONE)
        | SOME {len,pos} => SOME (pos, len)
      end

   fun findMatchList (s:string) (re:string) =
     let
       val thisend = String.size s
       fun recFn sub thisp = 
         case matchRegEx sub re of
             NONE => []
           | SOME (pos, len) => 
               if (thisp+pos+len) = thisend then ((*print ("find a match at pos: "^Int.toString(thisp+pos)^"\n");*) [(thisp+pos, len)])
               else ((*print ("find a match at pos: "^Int.toString(thisp+pos)^"\n");*) (thisp+pos, len)::(recFn (String.extract(sub, pos+len, NONE)) (thisp+pos+len)))
     in
       recFn s 0
     end

   fun printlist (ss: Seqset) = 
     let
      fun myprint (((b, s), l): BSLToken) = print ((BTokenToName b)^"  ")
       val i = ref 0
     in
       while !i < List.length ss do (
          print ((Int.toString (!i)) ^ ": ");
          let val (tl, f) = List.nth(ss, !i) in List.app myprint tl end;
          print "\n";
          i := !i+1
       )
     end


(*    fun matchtest () = (matchRegEx "105.0.1" "[0-9]+[.][0-9]+[.][0-9]+[.][0-9]+") *)  

(* findPath algorithm:
1. Find all possible tokens except Pblob starting from position 0  of this string, and
concatenate these tokens with the token  sequences returned from calling findPath on
the rest of the string.
2. If step 1 fails, find all possible tokens except Pblob from  other position t of the
string and view the substring from position  0 to t-1 as a Pblob token. Concatenate
Pblob and these tokens with  the token sequences returned from calling findPath on the
rest of  the string.
3. If step 2 fails, return Pblob.
*)
    exception MatchError

    structure PosBTokenTable = RedBlackMapFn(
                     struct type ord_key = int
			    val compare = Int.compare
		     end) 

    structure BTokenTable = RedBlackMapFn(
                     struct type ord_key = BToken
			    val compare = compBToken
		     end) 

    fun constrPosBTokenTable s = 
      let
        val tokensNoBlob = List.take(tokenDefList, (List.length tokenDefList)-1)
        fun matchOneToken ((btoken, str), (r1, r2)) =
          case (findMatchList s str) of
              [] => ((*print "2\n";*) (r1, r2))
            | mlist => ((*print "2\n";*) 
                let
(*
val num = List.length mlist
val _ = print ((BTokenToName btoken)^" : "^"match num is "^(Int.toString num)^"\n")
fun printplist (pos, len) = print (Int.toString pos^" "^Int.toString len^"\n")
val _ = (print (BTokenToName btoken^":\n"); List.app printplist mlist)
*)
                  fun extractOne ((pos,len), basetable) = 
                            case PosBTokenTable.find(basetable, pos) of
                                NONE => PosBTokenTable.insert(basetable, pos, [(btoken, len)])
                              | SOME _ => 
                                  let
                                    val (newtable, list) = PosBTokenTable.remove(basetable, pos)
                                  in
                                    PosBTokenTable.insert(newtable, pos, ((btoken, len)::list))
                                  end
(*
                    case MatchTree.nth(tree,(i-1)) of 
                        NONE => raise MatchError
                      | SOME {pos,len} => 
                          let
                            val basetable = if (i>1) then extractOne (i-1) else r1
                          in
                            case PosBTokenTable.find(basetable, pos) of
                                NONE => PosBTokenTable.insert(basetable, pos, [(btoken, len)])
                              | SOME _ => 
                                  let
                                    val (newtable, list) = PosBTokenTable.remove(basetable, pos)
                                  in
                                    PosBTokenTable.insert(newtable, pos, ((btoken, len)::list))
                                  end
                          end
                  fun consBToken i =
                    case MatchTree.nth(tree, i) of 
                        NONE => raise MatchError
                      | SOME {pos, len} =>
                          let
                            val baselist = if (i=(num-1)) then [] else consBToken (i-1) 
                          in
                            (pos, len)::baselist
                          end
*)
                in
                  ((List.foldl extractOne r1 mlist), BTokenTable.insert(r2, btoken, mlist))
                end)
      in
        List.foldl matchOneToken (PosBTokenTable.empty, BTokenTable.empty) tokensNoBlob
      end
                          
    fun findPaths (s: string, recNo: int) : NewContext list =
      let
        val (pbtable, btable) = constrPosBTokenTable s
val _ = print "regexp all found\n"
        val thisendp = (String.size s)-1
        fun recFn p lookuptable  = (
(*print ("inside redFn "^Int.toString p^"\n");*)
          case PosBTokenTable.find(lookuptable, p) of
              SOME nlist => (nlist, lookuptable)
            | NONE => (
                case PosBTokenTable.find(pbtable, p) of
                    NONE => ( 
                      let
(*val _ = print "no tokens at this pos\n"*)
                        fun constrNearestList table : (BToken*(int*int)) list= 
                          let
                            val tokensNoBlob = List.take(tokenDefList, (List.length tokenDefList)-1)
                            fun findMe ((btoken, bstr), nearesttokenlist) =
                              case BTokenTable.find(btable, btoken) of
                                  NONE => nearesttokenlist
                                | SOME plist => 
                                    let
                                      fun findNearest ((pbegin, plen), nearesttoken) = 
                                        case nearesttoken of
                                            NONE => if pbegin>p then SOME (btoken, (pbegin, plen)) else NONE
                                          | SOME nt => SOME nt
                                    in
                                      case (List.foldl findNearest NONE plist) of
                                          NONE => nearesttokenlist
                                        | SOME tpp => tpp::nearesttokenlist
                                    end
                          in
                            List.foldl findMe [] tokensNoBlob
                          end
                        val nblist = constrNearestList btable
                        fun addOneTokenBlob ((btoken, (beginp, lenp)), (nclist, newlookuptable)) =
                          let
                            val blobitem = ((PPblob, String.substring(s, p, beginp-p)), mkLoc p (beginp-1) recNo recNo)
                            val iitem = ((btoken, String.substring(s, beginp, lenp)), mkLoc beginp (beginp+lenp-1) recNo recNo)
                            val (return, returntable) =
                            if (beginp+lenp-1)=thisendp then ([[blobitem, iitem]], newlookuptable)
                            else (
                              let
                                val (tl, lookuptable1) = recFn (beginp+lenp) newlookuptable
(*val _ = print ("return from recFn "^Int.toString (beginp+lenp)^" has "^Int.toString(List.length tl)^" lists\n")*)
                                fun insert1st (tli: NewContext) = [blobitem, iitem]@tli 
                              in
                                case tl of
                                    [] => ([], lookuptable1) (* problematic? *)
                                  | _ => ((List.map insert1st tl), lookuptable1)
                              end
                            )
                          in
                            (return@nclist, returntable)
                          end
                        val (retv, savetable) = List.foldl addOneTokenBlob ([], lookuptable) nblist
                        val newretv = case retv of
                                          [] => [[((PPblob, String.extract(s, p, NONE)), mkLoc p thisendp recNo recNo)]]
                                        | retlist => retlist
                      in
                        (newretv, PosBTokenTable.insert(savetable, p, newretv))
                      end
                    )
                  | SOME bllist => (
                      let
(*val _ = print ("have "^(Int.toString (List.length bllist))^" tokens at this pos\n")*)
                        fun addOneToken ((btoken, lenp), (nclist, newlookuptable)) =
                          let
                            val iitem = ((btoken, String.substring(s, p, lenp)), mkLoc p (p+lenp-1) recNo recNo)
                            val (return, returntable) =
                            if (p+lenp-1)=thisendp then ([[iitem]], newlookuptable)
                            else (
                              let
                                val (tl, lookuptable1) = recFn (p+lenp) newlookuptable
(*val _ = print ("return from recFn "^Int.toString (p+lenp)^" has" ^(Int.toString (List.length tl))^" lists\n")*)
                                fun insert1st (tli: NewContext) = iitem::tli 
                              in
                                case tl of
                                    [] => ([], lookuptable1)
                                  | _ => ((List.map insert1st tl), lookuptable1)
                              end
                            )
                          in
                            (return@nclist, returntable)
                          end
                        val (retv, savetable) = List.foldl addOneToken ([], lookuptable) bllist
                        val newretv = case retv of
                                          [] => [[((PPblob, String.extract(s, p, NONE)), mkLoc p thisendp recNo recNo)]]
                                        | retlist => retlist
                      in
                        (newretv, PosBTokenTable.insert(savetable, p, newretv))
                      end
                    )
              )
              )
        val (retlist, rettable) = recFn 0 PosBTokenTable.empty
      in
        retlist
      end

(*
    fun gothrough1 (s: string, recNo: int, pos: int) : NewContext list =
      let
(*        val _ = print "before gothrough1\n" *)
        val tokensNoBlob = List.take(tokenDefList, (List.length tokenDefList)-1)
        fun matchOne ((btoken, str), result) =
        let val ret =
          case matchRegEx s str of
              ("", "", _, _) => [] 
            | (matched, rest, 0, len) => (
                 case rest of
                     "" => [[((btoken, matched), mkLoc pos (pos+len) recNo recNo)]]
                   | _ =>
                       let
                         val tl = findPaths(rest, recNo, pos+len+1)
                         fun insert1st (tli: NewContext) = [((btoken, matched), mkLoc pos (pos+len) recNo recNo)]@tli
                       in
                         (
                         case tl of
                            [] => []
                          | _ => List.map insert1st tl
                         )
                       end
                 )
           | _ => []
        in
          ret@result
        end  
      in
        List.foldl matchOne [] tokensNoBlob
      end

    and gothrough2 (s: string, recNo: int, pos: int) : NewContext list =
      let
        val _ = print ("Find no match for "^s^"\n")
        val tokensNoBlob = List.take(tokenDefList, (List.length tokenDefList)-1)
        fun matchOne ((btoken, str), result) =
        let val ret =
          case matchRegEx s str of
              ("", "", 0, 0) => []
            | (_, _, 0, _) => raise MatchError
            | (matched, rest, p, len) => (
                 case rest of
                     "" => [[((PPblob, String.substring(s, 0, p)), mkLoc pos (pos+p-1) recNo recNo), ((btoken, matched), mkLoc (pos+p) (pos+p+len) recNo recNo)]]
                   | _ =>
                       let
                         val tl = findPaths(rest, recNo, pos+p+len+1)
                         fun insert1st (tli: NewContext) = [((PPblob, String.substring(s, 0, p)), mkLoc pos (pos+p-1) recNo recNo), ((btoken, matched), mkLoc (pos+p) (pos+p+len) recNo recNo)]@tli
                       in
                         (case tl of
                            [] => []
                          | _ => List.map insert1st tl)
                       end
               )
        in
          ret@result
        end  
      in
        List.foldl matchOne [] tokensNoBlob
      end


    and findPaths (s: string, recNo: int, pos: int) : NewContext list = gothrough1(s, recNo, pos)
*)
(*
      case gothrough1(s, recNo, pos) of
          [] => ((*print "before gothrough2\n";*) case gothrough2(s, recNo, pos) of
                    [] => [[((PPblob, s), mkLoc pos (pos+(String.size s)) recNo recNo)]]
                  | l1 => l1)
        | l2 => l2
*)
(*
    fun findPaths (s: string, recNo: int, pos: int) : NewContext list =
      let
        val intlist = 
          case matchRegEx s (getRegex PPint) of
            ("", "", 0, 0) => []
          | (matched, rest, pos, len) =>  
              case rest of
                "" => [[((PPint, matched), 
                         mkLoc pos (pos+len) recNo recNo)]]
              | _ => 
                  let 
                    val tl = findPaths(rest, recNo, pos+len+1)
                    fun insert1st (tli: NewContext) = [((PPint, matched), mkLoc pos (pos+len) recNo recNo)]@tli 
                  in
                    case tl of
                      [] => []
                     | _ => List.map insert1st tl
                  end
        val floatlist = 
          case matchRegEx s "[0-9]+[.][0-9]+|[0-9]+" of
            ("", "", 0, 0) => []
          | (matched, rest, pos, len) =>
              let
                val ss = Substring.full matched
                val (pre, suff) = Substring.position "." ss
                val integer = Substring.string pre
                val frac = Substring.string (Substring.triml 1 suff)
                val retv = [[((PPfloat, matched), mkLoc pos (pos+len) recNo recNo)]]
              in
              case rest of
                "" => retv
              | _ => 
                  let 
                    val tl = findPaths(rest, recNo, pos+len+1)
                    fun insert1st (tli: NewContext)  = [((PPfloat, matched), mkLoc pos (pos+len) recNo recNo)]@tli
                  in
                    case tl of
                      [] => []
                     | _ => List.map insert1st tl
                  end
              end
        val iplist = 
          case matchRegEx s "[0-9]+[.][0-9]+[.][0-9]+[.][0-9]+" of
            ("", "", 0, 0) => []
          | (matched, rest, pos, len) =>  
              case rest of
                "" => [[((PPip, matched), mkLoc pos (pos+len) recNo recNo)]]
              | _ => 
                  let 
                    val tl = findPaths(rest, recNo, pos+len+1)
                    fun insert1st (tli: NewContext) = [((PPip, matched), mkLoc pos (pos+len) recNo recNo)]@tli 
                  in
                    case tl of
                      [] => []
                     | _ => List.map insert1st tl
                  end
        val whitelist = 
          case matchRegEx s (getRegex PPwhite) of
            ("", "", 0, 0) => []
          | (matched, rest, pos, len) =>  
              case rest of
                "" => [[((PPwhite, matched), mkLoc pos (pos+len) recNo recNo)]]
              | _ => 
                  let 
                    val tl = findPaths(rest, recNo, pos+len+1)
                    fun insert1st (tli: NewContext) = [((PPwhite, matched), mkLoc pos (pos+len) recNo recNo)]@tli 
                  in
                    case tl of
                      [] => []
                     | _ => List.map insert1st tl
                  end
        val dotlist = 
          case matchRegEx s (getRegex (PPpunc ".")) of
            ("", "", 0, 0) => []
          | (matched, rest, pos, len) =>  
              case rest of
                "" => [[((PPpunc ".", matched), mkLoc pos (pos+len) recNo recNo)]]
              | _ => 
                  let 
                    val tl = findPaths(rest, recNo, pos+len+1)
                    fun insert1st (tli: NewContext) = [((PPwhite, matched), mkLoc pos (pos+len) recNo recNo)]@tli 
                  in
                    case tl of
                      [] => []
                     | _ => List.map insert1st tl
                  end

     in
        intlist@floatlist@iplist@whitelist@dotlist 
     end
*)

    fun pathGraph recNum record : Seqset = 
      let
(*val _ = print "before findPaths\n" *)
        val raw : NewContext list = findPaths (record, !recNum)
(*val _ = print "after findPaths\n"*)
        fun addProb tokens = (tokens, 0.0)
        val ret : Seqset = List.map addProb raw
      in
        (
        recNum := !recNum + 1;
(*
print "before printlist\n";
print ("seqset size: "^(Int.toString (List.length ret))^"\n");*)
        printlist ret;
(*print "after printlist\n";*)
        ret
        )
      end

    
    fun selectPath (ss: Seqset) : NewContext =
      let 
        fun search ((bsltl, prob), ret) = if prob>ret then prob else ret
        val max = List.foldl search 0.0 ss
        fun findMax (bsltl:NewContext, prob:real) = if Real.compare(prob, max)=EQUAL then true else false
        val (r, rprob) = Option.valOf(List.find findMax ss)  
      in
        r
      end  
(*
      let
        val r = ref 0
        val minlen =  ref 10000
        val i = ref 0
        fun countOne (bsltl, prob) =
          if !minlen > List.length bsltl
            then (minlen := List.length bsltl; r := !i; i := !i+1)
            else i := !i+1
        val _ = List.app countOne ss
        val (ret, rprob) = List.nth (ss, !r)
      in
        ret
      end
*)
(*
   fun matchtest () = 
     let
       val tl = findPaths "162.105.0.1"
     in
       printlist tl
     end
*)
(*
    fun probSelect (paths: Context list): Context = 
      let
        (* compute the probability of each path*)
        val r = ref 0
        val minlen =  ref 10000
        val i = ref 0 
        val _ = 
          while !i < List.length paths do (
            if !minlen > List.length (List.nth (paths, !i))
              then (minlen := List.length (List.nth (paths, !i)); r := !i)
              else ();
            i:= !i+1
          )
      in
        List.nth (paths, !r)
      end
*)
(*
    fun selectPath (record: string, recNo: int) : Seqset =
      let
        val pathGraph :Context list = findPaths (record, recNo, 0)
val _ = (printlist pathGraph; print "\n\n") 
      in
        probSelect pathGraph
      end
*) 

end
