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
    fun matchRegEx (s:string) (re:string) :string*string*int*int (* matched, remain, pos, len *)=
      let
 val _ = print ("Match string (" ^ (String.toString s) ^") with regex ("^(String.toString re) ^ ")\n") 
        val cregex = REParser.compileString re             
      in
        case (StringCvt.scanString (REParser.find cregex) s) of
          NONE => ((*print "Regex failed!\n";*) ("", "", 0, 0))
        | SOME tree => 
        case MatchTree.nth(tree,0) of
          NONE => ((*print "Regex failed!\n";*) ("", "", 0, 0))
        | SOME {len,pos} => 
        case pos of
          0 => ((*print ("Regex succeeded! Substring matched: " ^ (String.substring (s, pos, len)) ^ 
                                         " Substring remained: " ^ (String.extract (s, pos+len, NONE)) ^ "\n");*) 
                                  ((String.substring (s, pos, len)), (String.extract (s, pos+len, NONE)), pos, len))
        | _ => ((*print "Regex succeeded, but not at pos 0\n";*) ((String.substring (s, pos, len)), (String.extract (s, pos+len, NONE)), pos, len)) 
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


    and findPaths (s: string, recNo: int, pos: int) : NewContext list =
      case gothrough1(s, recNo, pos) of
          [] => ((*print "before gothrough2\n";*) case gothrough2(s, recNo, pos) of
                    [] => [[((PPblob, s), mkLoc pos (pos+(String.size s)) recNo recNo)]]
                  | l1 => l1)
        | l2 => l2
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
(*        val _ = print "before findPaths\n" *)
        val raw : NewContext list = findPaths (record, !recNum, 0)
        fun addProb tokens = (tokens, 0.0)
        val ret : Seqset = List.map addProb raw
      in
        (
        recNum := !recNum + 1;
        printlist ret;
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
