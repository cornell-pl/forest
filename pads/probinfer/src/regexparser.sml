structure Regexparser =
struct
    open Types
    open Basetokens
    open Fvector
    open Common
    exception TyMismatch
    structure REParser = RegExpFn (structure P=AwkSyntax structure E=DfaEngine) : REGEXP
    structure MT = MatchTree
    structure SS = Substring
(*    open Matcher Matcher.RegExps *)

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

    structure BTokenDFATable = RedBlackMapFn(
      struct type ord_key = BToken
			 val compare = compBToken
	  end) 


    fun matchRegEx s btoken cregex(*(s:string) (cregex:REGEXP.regexp)*) (*:string*string*int*int*) (* matched, remain, pos, len *)=
      let
(*
        val regexp = parse "\/"
        val matches = match regexp
        val _ = if matches "/" then print "yes\n" else print "no\n"
        val _ = raise out
*)
(* val _ = print ("Match string (" ^ (String.toString s) ^") with token ("^(BTokenToName btoken) ^ ")\n") *)
(*        val cregex = REParser.compileString re
val _ = print "0\n"
*)             
      in
        case (StringCvt.scanString (REParser.find cregex) s) of
          NONE => ((*print "1\n";*) NONE)
        | SOME tree => 
((*print "2\n";*)
        case MatchTree.nth(tree,0) of
          NONE => ((*print "3\n";*) NONE)
        | SOME {len,pos} => ((*print "4\n";*) SOME (pos, len))
)
      end

   fun findMatchList s btoken re (*(s:string) (re:REGEXP.regexp)*) =
     let
       val thisend = String.size s
       fun recFn sub thisp = 
         case matchRegEx sub btoken re of 
(*         case Regex_c.match (sub, btoken, re) of  (* using c re lib! *)*)
             NONE => []
           | SOME (pos, len) => 
               if (thisp+pos+len) = thisend then ((*print ("find a match at pos: "^Int.toString(thisp+pos)^"\n");*) [(thisp+pos, len)])
               else ((*print ("find a match at pos: "^Int.toString(thisp+pos)^"\n");*) (thisp+pos, len)::(recFn (String.extract(sub, pos+len, NONE)) (thisp+pos+len)))
     in
       recFn s 0
     end

   fun findMatchList_c s btoken re (*(s:string) (re:REGEXP.regexp)*) =
     let
       val thisend = String.size s
       fun recFn sub thisp = 
(*         case matchRegEx sub btoken re of *)
         case Regex_c.match (sub, btoken, re) of  (* using c re lib! *)
             NONE => []
           | SOME (pos, len) => 
               if (thisp+pos+len) = thisend then ((*print ("find a match at pos: "^Int.toString(thisp+pos)^"\n");*) [(thisp+pos, len)])
               else ((*print ("find a match at pos: "^Int.toString(thisp+pos)^"\n");*) (thisp+pos, len)::(recFn (String.extract(sub, pos+len, NONE)) (thisp+pos+len)))
     in
       recFn s 0
     end
(*
   fun printlist (ss: Seqset) = 
     let
      fun myprint (((b, s), l): BSLToken) = print ((BTokenToName b)^"  ")
       val i = ref 0
     in (
       while !i < List.length ss do (
          print ((Int.toString (!i)) ^ ": ");
          let val (tl, f) = List.nth(ss, !i) in List.app myprint tl end;
          print "\n";
          i := !i+1
       );
       print "\n")
     end
*)

   fun printSeqset (ss: Seqset) =
     let
       val (endptable, s, lineNo, recNo, sbegin, send) = ss
       val endplist = PosBTokenTable.listItemsi endptable
       fun myprint (endp, bplist) = 
         let
           fun doOne (beginp, btoken) = print ("beginp = "^(Int.toString beginp)^"\t"^(BTokenToName btoken)^"\t"^(String.substring(s, beginp, endp-beginp+1))^"\n")
         in
           (print ("endp = "^(Int.toString endp)^" has "^(Int.toString (List.length bplist))^" tokens:\n");
            List.app doOne bplist)
         end
     in
       List.app myprint endplist
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

    structure BTokenTable = RedBlackMapFn(
                     struct type ord_key = BToken
			    val compare = compBToken
		     end) 

    fun constrPosBTokenTable s dfatable = 
      let
        val tokensNoBlob = List.take(tokenDefList, (List.length tokenDefList)-1)
        fun matchOneToken ((btoken, str), (r1, r2)) =
        let
          val re = Option.valOf(BTokenDFATable.find(dfatable, btoken))
        in
          case (findMatchList s btoken re) of
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
           end
      in
        List.foldl matchOneToken (PosBTokenTable.empty, BTokenTable.empty) tokensNoBlob
      end

    fun constrPosBTokenTableNoDFA s dfatable = 
      let
        val tokensNoBlob = List.take(tokenDefList, (List.length tokenDefList)-1)
        fun matchOneToken ((btoken, str), (r1, r2)) =
        let
(*          val re = Option.valOf(BTokenDFATable.find(dfatable, btoken))*)
            val re = str
        in
          case (findMatchList_c s btoken re) of
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
           end
      in
        List.foldl matchOneToken (PosBTokenTable.empty, BTokenTable.empty) tokensNoBlob
      end

    fun constrDFATable tlist =
      let
val _ = print "constructing DFAs...\n"
        fun addOneToken ((t, re), retable) =  (print ((BTokenToName t)^"\n"); BTokenDFATable.insert(retable, t, REParser.compileString re))
      in
        List.foldl addOneToken BTokenDFATable.empty tlist
      end
                          
    fun findPaths (s, recNo, dfatable) : NewContext list =
      let
        val (pbtable, btable) = (* constrPosBTokenTable s dfatable *) constrPosBTokenTableNoDFA s dfatable
val _ = print "regexp all found\n"
        val thisendp = (String.size s)-1
        fun recFn p lookuptable  = (
print ("inside redFn "^Int.toString p^"\n");
          case PosBTokenTable.find(lookuptable, p) of
              SOME nlist => (nlist, lookuptable) (* already have a saved list in the table *)
            | NONE => (
                case PosBTokenTable.find(pbtable, p) of
                    NONE => ( 
                      let
val _ = print "no tokens at this pos\n"
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
val _ = print ("return from recFn "^Int.toString (beginp+lenp)^" has "^Int.toString(List.length tl)^" lists\n")
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
val _ = print ("have "^(Int.toString (List.length bllist))^" tokens at this pos\n")
                        fun addOneToken ((btoken, lenp), (nclist, newlookuptable)) =
                          let
                            val iitem = ((btoken, String.substring(s, p, lenp)), mkLoc p (p+lenp-1) recNo recNo)
                            val (return, returntable) =
                            if (p+lenp-1)=thisendp then ([[iitem]], newlookuptable)
                            else (
                              let
                                val (tl, lookuptable1) = recFn (p+lenp) newlookuptable
val _ = print ("return from recFn "^Int.toString (p+lenp)^" has " ^(Int.toString (List.length tl))^" lists\n")
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

    fun constrEndPosTable s = 
      let
        fun doOnePos (workingtable : bool PosBTokenTable.map) = (* returns notreachabletable, beginptable, endptable *)
          let
            val workinglist = PosBTokenTable.listItemsi workingtable
            val (workingpos, junk) = List.nth(workinglist, 0)
(*val _ = print ("working pos is: "^(Int.toString workingpos)^"\n")*)
          in
            if workingpos = (String.size s) then (PosBTokenTable.empty, PosBTokenTable.empty, PosBTokenTable.empty)
            else
              let
                val tokensNoBlob = List.take(tokenDefList, (List.length tokenDefList)-1)
                val workings = String.extract(s, workingpos, NONE)
                fun findAllTokens ((btoken, re), btlist) = 
                  case Regex_c.match (workings, btoken, re) of
                      NONE => btlist
                    | SOME (pos, len) => if pos=0 then (workingpos+len-1, btoken)::btlist
                                         else btlist
                val mybtlist = List.foldl findAllTokens [] tokensNoBlob
                fun updateWorkingTable ((endp, btoken), wtable) = 
                  case PosBTokenTable.find(wtable, (endp+1)) of
                      NONE => PosBTokenTable.insert(wtable, (endp+1), true)
                    | SOME i => wtable
                val (rmvworkingtable, junk) = PosBTokenTable.remove(workingtable, workingpos)
                val newworkingtable = List.foldl updateWorkingTable rmvworkingtable mybtlist
                val (notreachabletable, beginptable, endptable) = doOnePos newworkingtable     (* recursive call *)
(*
fun printNotRTable nrtable = 
  let
    val nrlist = PosBTokenTable.listItemsi nrtable
    fun printOne (nr, tag) = print ((Int.toString nr)^" ")
    val _ = print "notreachable: "
    val _ = List.app printOne nrlist
  in
    print "\n"
  end
val _ = printNotRTable notreachabletable 
*)
                fun updateTables ((endp, btoken), (tag, bplist, ept)) = (* search if endp is reacheable *)
                  let
                    val thistag = case PosBTokenTable.find(notreachabletable, endp) of
                                      NONE => false
                                    | SOME p => true
                  in
                    if thistag = true then (tag, bplist, ept)
                    else case PosBTokenTable.find(ept, endp) of
                             NONE => (false, (endp, btoken)::bplist, PosBTokenTable.insert(ept, endp, [(workingpos, btoken)]))
                           | SOME eplist => 
                               let
(*val _ = print ("i'm here\t endp = "^(Int.toString endp)^"\n")*)
                                 val (removet, junk) = PosBTokenTable.remove(ept, endp)
                               in
                                 (false, (endp, btoken)::bplist, PosBTokenTable.insert(removet, endp, (workingpos, btoken)::eplist))
                               end
                  end
(*val _ = print ("bplist length: "^(Int.toString (List.length mybtlist))^"\n")*)
                val (mytag, mybplist, newendptable) = List.foldl updateTables (true, [], endptable) mybtlist
(*val _ = print ("bplist length: "^(Int.toString (List.length mybplist))^"\n")*)
                val newnotreachabletable = if mytag = true then PosBTokenTable.insert(notreachabletable, workingpos, true)
                                           else notreachabletable
                val newbeginptable =  if mytag = true then beginptable
                                      else PosBTokenTable.insert(beginptable, workingpos, mybplist) 
              in
                (newnotreachabletable, newbeginptable, newendptable)
              end
          end
        val initworkingtable = PosBTokenTable.insert(PosBTokenTable.empty, 0, true)
        val (junk, fbeginptable, fendptable) = doOnePos initworkingtable handle Subscript => (PosBTokenTable.empty, PosBTokenTable.insert(PosBTokenTable.empty, 0, [(((String.size s)-1), PPblob)]), PosBTokenTable.insert(PosBTokenTable.empty, ((String.size s)-1), [(0, PPblob)]))
      in
        (fbeginptable, fendptable)
      end

    fun findPaths2 (s, recNo, dfatable) : Seqset =
      let
        val (beginptable, endptable) = constrEndPosTable s
      in
        (endptable, s, recNo, recNo, 0, (String.size s -1))
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
(*
    fun pathGraph recNum dfatable record : Seqset = 
      let
(*val _ = print "before findPaths\n" *)
        val raw : NewContext list = findPaths (record, !recNum, dfatable)
(*val _ = print "after findPaths\n"*)
        fun addProb tokens = (tokens, 0.0)
        val ret : Seqset = List.map addProb raw
      in
        (
        recNum := !recNum + 1;
(*
print "before printlist\n";
print ("seqset size: "^(Int.toString (List.length ret))^"\n");*)
        print "seqset:\n";
        printlist ret;
        print "\n";
(*print "after printlist\n";*)
        ret
        )
      end
*)

    fun pathGraph recNum dfatable record : Seqset = 
      let
(*val _ = print "before findPaths\n" *)
        val raw : Seqset = if String.size(record) = 0 then (PosBTokenTable.empty, "", !recNum, !recNum, ~1, ~1) (*PPempty*) 
                           else findPaths2 (record, !recNum, dfatable)
(*val _ = print "after findPaths\n"*)
      in
        (
        recNum := !recNum + 1;
        print "seqset:\n";
        printSeqset raw;
        print "\n";
        raw
        )
      end

(*    
    fun selectPath (ss: Seqset) : NewContext =
      let 
        fun search ((bsltl, prob), ret) = if prob>ret then prob else ret
        val max = List.foldl search (~Real.maxFinite) ss
(*val _ = print (Real.toString max) *)
        fun findMax (bsltl:NewContext, prob:real) = if Real.compare(prob, max)=EQUAL then true else false
        val (r, rprob) = Option.valOf(List.find findMax ss)  
      in
        r
      end  
*)
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
val _ = print ("basicViterbi: "^"sbegin = "^(Int.toString sbegin)^" send = "^(Int.toString send)^"\n")
        val (tokentable, begintokentable, endtokentable, tokenpairtable, listtokentable) = tables
        val reachable = PosBTokenTable.listItemsi endptable
        fun forward ((workingpos, bplist), oldtable) =
          let
            fun doOne (beginp, btoken) =
              let
                val lastlist = PosBTokenTable.find(oldtable, (beginp-1))
val _ = print ("beginp = "^(Int.toString beginp)^" endp = "^(Int.toString workingpos)^" btoken = "^(BTokenToName btoken)^"\n")
                fun search (((mybp, mybptoken), (lastbp, prob)), ((maxbp, maxtoken), maxprob)) = 
                  let
                    val transprob = Math.ln(defaultRVal(BTokenPairTable.find(tokenpairtable, (mybptoken, btoken))))
                    val thisprob = prob + transprob
                  in
                    if thisprob > maxprob then (print ("forward probability 1 = "^(Real.toString prob)^" "^(Real.toString transprob)^"\n"); ((mybp, mybptoken), thisprob))
                    else if Real.compare(thisprob, maxprob)=EQUAL then 
                      if (BTokenCompleteEnum(mybptoken) < BTokenCompleteEnum(maxtoken)) then ((mybp, mybptoken), thisprob) else ((maxbp, maxtoken), maxprob)
                    else (print ("forward probability 2 = "^(Real.toString prob)^" "^(Real.toString transprob)^"\n"); if Real.compare(maxprob, (~Real.maxFinite))=EQUAL then ((mybp, mybptoken), maxprob) else ((maxbp, maxtoken), maxprob))
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
val _ = print ("emitprob = "^(Real.toString emitprob)^" transprob = "^(Real.toString transprob)^"\n") 
                  in
                    emitprob+transprob
                  end
                val newprob = mymaxprob + probBToken(beginp, workingpos)
val _ = print ("beginp = "^(Int.toString beginp)^" btoken = "^(BTokenToName btoken)^" prob = "^(Real.toString newprob)^" preprob = "^(Real.toString mymaxprob)^"\n")
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
val _ = print ("fmsglist length = "^(Int.toString (List.length fmsglist))^"\n")
        val (lastpos, lastrcd) = List.nth(fmsglist, (List.length fmsglist)-1)
        val lastrcdlist = BasicViterbiTable.listItemsi lastrcd
val _ = print ("last position list length = "^(Int.toString (List.length lastrcdlist))^"\n")
        fun findlastmax ((thisp, (lastp, prob)), (maxp, maxlastp, maxprob)) =
          if prob>maxprob then (print ("probability = "^(Real.toString prob)^"\n"); (thisp, lastp, prob))
          else (print ("probability = "^(Real.toString prob)^"\n"); (maxp, maxlastp, maxprob))
        val ((lastmaxp, lastmaxt), lastpremaxp, lastmaxprob) = List.foldl findlastmax ((sbegin, PPblob), NONE, (~Real.maxFinite)) lastrcdlist (* lastpremaxp is an option *)
        fun backward ((prep, pret), thisp) = (* return a bsl list *) 
          case PosBTokenTable.find(forwardmsg, thisp) of
              NONE => (print "2\n"; print ((Int.toString thisp)^"\n"); raise ViterbiError)
            | SOME table => case BasicViterbiTable.find(table, (prep, pret)) of
                                NONE => (print ("beginp = "^(Int.toString prep)^" endp = "^(Int.toString thisp)^"\n"); raise ViterbiError) 
                              | SOME (newpre, newprob) => 
                                  case newpre of
                                      NONE => if prep=sbegin then (print ("( "^(Int.toString prep)^" "^(Int.toString thisp)^" "^(BTokenToName pret)^")\n"); [((pret, String.substring(s, prep, thisp-prep+1)), mkLoc prep thisp recNo lineNo)])
                                              else (print "3\n"; raise ViterbiError)
                                    | SOME (newprep, newpret) => if prep = sbegin then (print "6\n"; raise ViterbiError)
                                  else
                                  let
                                    val newbsl = backward((newprep, newpret), (prep-1))
val _ = print ("( "^(Int.toString prep)^" "^(Int.toString thisp)^" "^(BTokenToName pret)^")\n")
                                  in
                                    newbsl@[((pret, String.substring(s, prep, thisp-prep+1)), mkLoc prep thisp recNo lineNo)]
                                  end             
val _ = print "returned newcontext:\n"
        val retbsllist = 
          case lastpremaxp of
              NONE => if lastmaxp = sbegin then 
                        if compBToken(PPblob, lastmaxt)=EQUAL then (print "all token sequences are of zero probability, return PPblob\n"; [((lastmaxt, String.substring(s, lastmaxp, send-lastmaxp+1)), mkLoc lastmaxp send recNo lineNo)])
                        else (print ("( "^(Int.toString lastmaxp)^" "^(Int.toString send)^" "^(BTokenToName lastmaxt)^")\n"); [((lastmaxt, String.substring(s, lastmaxp, send-lastmaxp+1)), mkLoc lastmaxp send recNo lineNo)])
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
                (print ("( "^(Int.toString lastmaxp)^" "^(Int.toString send)^" "^(BTokenToName lastmaxt)^")\n"); backward(pre, (lastmaxp-1))@[((lastmaxt, String.substring(s, lastmaxp, send-lastmaxp+1)), mkLoc lastmaxp send recNo lineNo)])
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
val _ = print ("ViterbiWithInitT: "^"sbegin = "^(Int.toString sbegin)^" send = "^(Int.toString send)^" initT = "^(BTokenToName(#1(initT)))^"\n")
        val (tokentable, begintokentable, endtokentable, tokenpairtable, listtokentable) = tables
        val reachable = PosBTokenTable.listItemsi endptable
        fun forward ((workingpos, bplist), oldtable) =
          let
            fun doOne (beginp, btoken) =
              let
                val lastlist = PosBTokenTable.find(oldtable, (beginp-1))
val _ = print ("beginp = "^(Int.toString beginp)^" endp = "^(Int.toString workingpos)^" btoken = "^(BTokenToName btoken)^"\n")
                fun search (((mybp, mybptoken), (lastbp, prob)), ((maxbp, maxtoken), maxprob)) = 
                  let
                    val transprob = Math.ln(defaultRVal(BTokenPairTable.find(tokenpairtable, (mybptoken, btoken))))
                    val thisprob = prob + transprob
                  in
                    if thisprob > maxprob then (print ("forward probability 1 = "^(Real.toString prob)^" "^(Real.toString transprob)^"\n"); ((mybp, mybptoken), thisprob))
                    else if Real.compare(thisprob, maxprob)=EQUAL then 
                      if (BTokenCompleteEnum(mybptoken) < BTokenCompleteEnum(maxtoken)) then ((mybp, mybptoken), thisprob) else ((maxbp, maxtoken), maxprob)
                    else (print ("forward probability 2 = "^(Real.toString prob)^" "^(Real.toString transprob)^"\n"); if Real.compare(maxprob, (~Real.maxFinite))=EQUAL then ((mybp, mybptoken), maxprob) else ((maxbp, maxtoken), maxprob))
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
val _ = print ("emitprob = "^(Real.toString emitprob)^" transprob = "^(Real.toString transprob)^"\n") 
                  in
                    emitprob+transprob
                  end
                val newprob = mymaxprob + probBToken(beginp, workingpos)
val _ = print ("beginp = "^(Int.toString beginp)^" btoken = "^(BTokenToName btoken)^" prob = "^(Real.toString newprob)^" preprob = "^(Real.toString mymaxprob)^"\n")
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
val _ = print ("fmsglist length = "^(Int.toString (List.length fmsglist))^"\n")
        val (lastpos, lastrcd) = List.nth(fmsglist, (List.length fmsglist)-1)
        val lastrcdlist = BasicViterbiTable.listItemsi lastrcd
val _ = print ("last position list length = "^(Int.toString (List.length lastrcdlist))^"\n")
        fun findlastmax ((thisp, (lastp, prob)), (maxp, maxlastp, maxprob)) =
          if prob>maxprob then (print ("choose probability = "^(Real.toString prob)^"\n"); (thisp, lastp, prob))
          else (print ("ignore probability = "^(Real.toString prob)^"\n"); (maxp, maxlastp, maxprob))
        val ((lastmaxp, lastmaxt), lastpremaxp, lastmaxprob) = List.foldl findlastmax ((sbegin, PPblob), NONE, (~Real.maxFinite)) lastrcdlist (* lastpremaxp is an option *)
        fun backward ((prep, pret), thisp) = (* return a bsl list *) 
          case PosBTokenTable.find(forwardmsg, thisp) of
              NONE => (print "2\n"; print ((Int.toString thisp)^"\n"); raise ViterbiError)
            | SOME table => case BasicViterbiTable.find(table, (prep, pret)) of
                                NONE => (print ("beginp = "^(Int.toString prep)^" endp = "^(Int.toString thisp)^"\n"); raise ViterbiError) 
                              | SOME (newpre, newprob) => 
                                  case newpre of
                                      NONE => if prep=sbegin then (print ("( "^(Int.toString prep)^" "^(Int.toString thisp)^" "^(BTokenToName pret)^")\n"); [((pret, String.substring(s, prep, thisp-prep+1)), mkLoc prep thisp recNo lineNo)])
                                              else (print "3\n"; raise ViterbiError)
                                    | SOME (newprep, newpret) => if prep = sbegin then (print "6\n"; raise ViterbiError)
                                  else
                                  let
                                    val newbsl = backward((newprep, newpret), (prep-1))
val _ = print ("( "^(Int.toString prep)^" "^(Int.toString thisp)^" "^(BTokenToName pret)^")\n")
                                  in
                                    newbsl@[((pret, String.substring(s, prep, thisp-prep+1)), mkLoc prep thisp recNo lineNo)]
                                  end             
val _ = print "returned newcontext:\n"
        val retbsllist = 
          case lastpremaxp of
              NONE => if lastmaxp = sbegin then (print ("( "^(Int.toString lastmaxp)^" "^(Int.toString send)^" "^(BTokenToName lastmaxt)^")\n"); [((lastmaxt, String.substring(s, lastmaxp, send-lastmaxp+1)), mkLoc lastmaxp send recNo lineNo)])
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
                (print ("( "^(Int.toString lastmaxp)^" "^(Int.toString send)^" "^(BTokenToName lastmaxt)^")\n"); backward(pre, (lastmaxp-1))@[((lastmaxt, String.substring(s, lastmaxp, send-lastmaxp+1)), mkLoc lastmaxp send recNo lineNo)])
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
val _ = print "ViterbiWithSummary\n"
        val (endptable, s, lineNo, recNo, sbegin, send) = ss
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

                  NONE => (print "can't find a pre table\n";
                          if beginp = sbegin then (print "=begin\n";
                            case BSTokenTable.find(summaryTable, (btoken, "")) of
                                NONE => (* this token is not in the summary *)
                                  let
val _ = print "this token is not in the summary\n"
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
val _ = print "this token is in the summary\n"
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
val _ = print ("insert a table at pos: "^(Int.toString workingpos)^"\n")
                                  in
                                    PosBTokenTable.insert(myoldtable, workingpos, mytable1)
                                  end)
                          else (print ("2: not = begin, beginp = "^(Int.toString beginp)^"\n"); raise ViterbiError))

                 | SOME pretable1 =>
                    let
val _ = print "get a pre table\n"
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
                                  fun constrIntList i = if i = myindex then i+1 else i
                                  val ilist = List.map constrIntList preilist
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
val _ = print ("workingpos = "^(Int.toString workingpos)^"\n")
            val thistable = List.foldl doOne thisoldtable bplist
val _ = print "after doOne\n"
          in 
            thistable
          end
        val forwardmsg = List.foldl forward PosBTokenTable.empty reachable 
val _ = print "still in\n"
        val fmsglist = PosBTokenTable.listItemsi forwardmsg
val _ = print "forward done\n"
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
                                                         else if i = myindex then (List.nth(ilist, i)-1)::minusOne (i-1)
                                                                             else List.nth(ilist, i)::minusOne (i-1)
                                      in 
                                        minusOne (List.length ilist-1)
                                      end
                  fun printilist i = print ((Int.toString i)^" ")
                in
                  case IntListTable.find(table1, ilist) of
                      NONE => (print "6: "; List.app printilist ilist; print "\n"; raise ViterbiError)
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
( print ("prep: "^(Int.toString (#1(lastpremaxpp)))^" "^(BTokenToName (#2(lastpremaxpp)))^"\n");
                   case BSTokenTable.find(summaryTable, (lastmaxt, "")) of
                       NONE => backward(lastpremaxpp, (lastmaxp-1), targetsummary)
                     | SOME (freq, index) =>
                         let
                           fun minusOne i = if i = 0 then if i = index then [List.nth(targetsummary, 0)-1] else [List.nth(targetsummary, 0)] 
                                            else if i = index then (List.nth(targetsummary, i)-1)::minusOne (i-1)
                                                              else List.nth(targetsummary, i)::minusOne (i-1)
                         in
                           backward(lastpremaxpp, (lastmaxp-1), minusOne(List.length targetsummary-1))
                         end
)
        val retbsllist = rest@[((lastmaxt, String.substring(s, lastmaxp, send-lastmaxp+1)), mkLoc lastmaxp send recNo lineNo)]
val _ = print "backward done\n"
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
