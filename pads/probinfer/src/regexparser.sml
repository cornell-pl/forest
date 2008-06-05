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

    exception InvalidSSFile

    fun readinPathGraph records filename : Seqset list =
      let
        fun isSlash c = c = #"/"
        val filename = List.nth(filename, 0)   (* only allow a single test file at once *)
        val (junk, testname) = Substring.splitr (not o isSlash) (Substring.full filename)
        val testname = Substring.string testname
        val data : string list = loadFile ("seqsets/"^testname^".ss") handle Io => raise InvalidSSFile
        fun splitRec (str, d, l): string list list =
          case d of
            [] => List.take(l, (List.length l)-1)
           |hd::tl => if String.compare(hd, str)=EQUAL then splitRec (str, tl, l@[[]])
                      else  
                        let
                          val wl = List.nth(l, (List.length l)-1) 
                        in
                          splitRec (str, tl, List.take(l, (List.length l)-1)@[(wl@[hd])])
                        end

        val splitd = splitRec("EOR", data, [[]])
        val _ = if (List.length records) <> (List.length splitd) then 
                  (print ("input data: "^(Int.toString (List.length records))^" records, seqset file data: "^(Int.toString (List.length splitd))^" records\n"); raise InvalidSSFile) 
                else () 
        val recNum = ref ~1
        fun constrOneRecord (record, ssdata) =
          let
            val endpdata = splitRec("EOP", ssdata, [[]])
            fun doOneEndp (pdata, oldtable) =
              let
                val myendp = Option.valOf(Int.fromString(List.nth(pdata, 0)))
                fun constrList beginstring : int*BToken =
                  let
                    fun isSpace c = c = #" "
                    val (beginp, btoken) = Substring.splitl (not o isSpace) (Substring.full beginstring)
                    val beginp = Option.valOf(Int.fromString(Substring.string beginp))
                    val btoken = nameToBToken (Substring.string(Substring.triml 1 btoken))
                  in
                    (beginp, btoken)
                  end
                val beginlist = List.map constrList (List.drop(pdata, 1))
              in
                PosBTokenTable.insert(oldtable, myendp, beginlist)
              end
            val endptable = List.foldl doOneEndp PosBTokenTable.empty endpdata 
            val _ = recNum := !recNum + 1
          in
            (endptable, record, !recNum, !recNum, 0, (String.size record -1))
          end
        val ssl = ListPair.mapEq constrOneRecord (records, splitd)
      in
        ssl
      end

    fun pathGraph recNum dfatable record : Seqset =
      let
(*val _ = print "before findPaths\n" *)
       val raw : Seqset = if String.size(record) = 0 then (PosBTokenTable.empty, "", !recNum, !recNum, ~1, ~1) (*PPempty*) 
                           else findPaths2 (record, !recNum, dfatable)
(*val _ = print "after findPaths\n"*)
      in
        (
        recNum := !recNum + 1;
(*
        print "seqset:\n";
        printSeqset raw;
        print "\n";
*)
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

  fun dumpSeqsets fileName = 
	let val recordNumber = ref 0
	    val records = loadFiles fileName  (* records: string list *)
        val tokensNoBlob = List.take(tokenDefList, (List.length tokenDefList)-1)
        val dfatable = (* constrDFATable tokensNoBlob *) BTokenDFATable.empty
        val rtokens : Seqset list = List.map (pathGraph recordNumber dfatable) records
        fun isSlash c = c = #"/"
        val filename = List.nth(fileName, 0)   (* only allow a single test file at once *)
        val (junk, testname) = Substring.splitr (not o isSlash) (Substring.full filename)
        val testname = Substring.string testname
        val strm = TextIO.openOut ("seqsets/"^testname^".ss")
        fun dumpOne ss =
          let
            val (endptable, s, lineNo, recNo, sbegin, send) = ss
            val endplist = PosBTokenTable.listItemsi endptable
            fun doOnePos (endp, beginplist) =
              let
                val _ = TextIO.output(strm, (Int.toString(endp)^"\n"))
                fun doOne (beginp, btoken) = TextIO.output(strm, ((Int.toString beginp)^" "^(BTokenToName btoken)^"\n"))
                val _ = List.app doOne beginplist
              in
                TextIO.output(strm, "EOP\n")
              end
            val _ = List.app doOnePos endplist
          in
            TextIO.output(strm, "EOR\n")
          end
        val _ = List.app dumpOne rtokens
    in
      TextIO.closeOut strm
    end


end
