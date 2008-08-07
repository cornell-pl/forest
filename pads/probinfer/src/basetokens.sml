structure Basetokens = struct
    open Complexity
    open Hosts
    open Tokens

    fun compLoc (l1: location, l2: location): order = 
        let val {lineNo = ln1, beginloc = b1, endloc = e1, recNo = rn1 } = l1
            val {lineNo = ln2, beginloc = b2, endloc = e2, recNo = rn2 } = l2
        in ( case Int.compare (b1, b2) of
                  LESS    => LESS
                | GREATER => GREATER
                | EQUAL   => Int.compare (e1, e2)
           )
        end

    datatype BToken = PPint | PPfloat | PPtime | PPdate | PPip | PPhostname | PPemail | PPmac | PPpath | PPurl | PPurlbody | PPword | PPhstring | PPid | PPbXML | PPeXML | PPwhite | PPmessage | PPtext | PPpermission | PPpunc of string | PPblob | PPempty | PPError(*| PPgroup of { left : BToken*string*location, body : (BToken*string*location) list, right : BToken*string*location } *) 
(*Pdot | Pslash | Pbackslash | Psemicolon | Pbar | Pless | Ptilde | Pbquote | Pbang | Pat | Phash | Pdollar | Ppercent | Pcaret | Pand | Pstar | Plpar | Prpar | Pdash | Punderscore | Pplus | Pequa | Plbrac | Prbrac | Plsqubrac | Prsqubrac | Pcolon | Pdquote | Pquote | Pgreater | Pcomma | Pquestion*)

    type BSToken = BToken * string
    
    type BSLToken = (BToken * string) * location

(*    type BSToken = BToken * string *)
    
(*    type BSLToken = BToken * string * location *)

    type NewContext = BSLToken list

    structure PosBTokenTable = RedBlackMapFn
    (
      struct type ord_key = int
			 val compare = Int.compare
	  end
    ) 

    type Seqset = (((int*BToken) list) PosBTokenTable.map) * string * int * int * int * int (* associated with lineNo, recNo, beginp, endp *)

    fun getSeqset (l:int) (seqsetl: Seqset list) : Seqset = 
      let
        fun findLoc (ss:Seqset) = let 
                                    val (graph, s, lineNo, recNo, sb, se) = ss
                                  in (lineNo=l) end
      in
        Option.valOf(List.find findLoc seqsetl)
      end

(*
    type Tokenseq = NewContext * real

    type Seqset = Tokenseq list

    fun getSeqset (l:int) (seqsetl: Seqset list) : Seqset = 
      let
        fun findLoc (ss:Seqset) = let 
                                    val (c, f) = List.nth(ss, 0)
                                    val ((b, s), l1) = List.nth(c, 0)
                                    val {lineNo=ln, beginloc=bgnl, endloc=endl, recNo=rn} = l1
                                  in (ln=l) end
      in
        Option.valOf(List.find findLoc seqsetl)
      end

    fun getMax ( ss: Seqset ) : Tokenseq = 
      let
        val (_,m) = List.nth (ss, 0)
        val max = ref m
        val n = ref 0
        val i = ref 0
      in
        while (!i < List.length ss) do (
          (
          let 
            val (_,j) = List.nth (ss, !i)
          in
           if !max < j then ( max := j; n := !i ) else ()
          end);
          i := !i+1
        );
        List.nth (ss, !n)
      end
*)

(*
    fun chopSeqset (ss: Seqset, ll: location list) : Seqset list = 
      let
        fun initSeqset (l: location) = []
        val ssl = [[]]@(List.map initSeqset ll)

        fun chopOne (s: Tokenseq) : Tokenseq list = 
          let
            val i = ref 0 (* location list index *)
            val j = ref 0 (* token list index *)
            val k = ref 0 (* previous position *)
            val (tlist,_) = s
            fun chopPoint (bsl: BSLToken) = 
              let
                val ((b,st),l) = bsl
                val thisloc = List.nth (ll, !i)
                val tmp = !k
              in
                case compLoc(l, thisloc) of
                    EQUAL => (case Int.compare(!j, !k) of
                                 EQUAL => (i := !i+1; ([], EQUAL))
                               | _ => (i := !i+1; k := !j+1; (List.drop(List.take(tlist, !j), tmp), EQUAL)))
                  | LESS => ([], LESS)
                  | GREATER => ([], GREATER)
              end 
            val ret = ref []
            val tag = ref LESS
          in
            while (!j < List.length tlist) andalso (!i <  List.length ll) andalso (!tag<>GREATER) do (
              let 
                val (ts, re) = chopPoint (List.nth(tlist,!j))
                val _ = j := !j+1
              in
                case re of
                    EQUAL => ret := !ret @ [(ts, 0.0)]
                  | LESS => () 
                  | GREATER => tag := GREATER
              end
            );
            case !tag of 
                GREATER => []
              | _ => (!ret @ [(List.drop(tlist,!k), 0.0)])
          end

        fun chopAppend ((s: Tokenseq), (result: Seqset list)) : Seqset list = 
          let
            val re = chopOne s (* re: Tokenseq list *)
            val i = ref 0
            fun f (ss:Seqset) = ( 
              i := !i+1;
              ss@[List.nth(re, !i-1)])
          in
            case re of
                [] => List.map f result
              | _ => result
          end
      in
        List.foldl chopAppend ssl ss
      end
*)
 
    exception bust

    fun compStr ( s1 : string, s2 : string ) : order =
        if s1 < s2
        then LESS
        else if s1 > s2
             then GREATER
             else EQUAL

(*
    fun compBSToken ((t1, s1):BSToken, (t2, s2):BSToken):order = 
	case (t1,t2) of
           (PPint, PPint) => EQUAL
        |  (PPfloat, PPfloat) => EQUAL
        |  (PPtime, PPtime)           => EQUAL
        |  (PPdate, PPdate)           => EQUAL
        |  (PPurl, PPurl)             => EQUAL
        |  (PPurlbody, PPurlbody) => EQUAL
        |  (PPpath, PPpath)           => EQUAL
        |  (PPip, PPip)               => EQUAL
        |  (PPhostname, PPhostname)   => EQUAL
        |  (PPemail, PPemail)     	  => EQUAL
        |  (PPmac, PPmac)             => EQUAL
        |  (PPbXML, PPbXML) => String.compare(s1, s2) (* should be some substring *)
        |  (PPeXML, PPeXML) => String.compare(s1, s2)
        |  (PPword, PPword) => EQUAL
        |  (PPid, PPid) => EQUAL
        |  (PPwhite, PPwhite) => EQUAL
        |  (PPmessage, PPmessage) => EQUAL
        |  (PPpermission, PPpermission) => EQUAL
        |  (PPpunc s1, PPpunc s2) => compStr(s1, s2)
        |  (PPblob, PPblob) => EQUAL
        |  (_, _) => GREATER
*)


    (*    Establish an order on Token using the following constraints:
          Ptime < Pdate < Pip < Pemail < Ppermission < Pmac < Pfloat < Pint < Phostname < Purl < Purlbody < Ppath < PbXML < PeXML  <  Pword < Phstring < Pid < Ppunc < Pwhite < Ptext < Pmessage < Pblob < Pempty
     *)


    fun BTokenEnum (t: BToken) : int =
      case t of
       PPint => 8
    |  PPfloat => 7
    |  PPtime     => 1
	|  PPdate     => 2
	|  PPip       => 3
	|  PPhostname  => 9
	|  PPpath     => 12
	|  PPurl      => 10
    |  PPurlbody => 11
	|  PPemail      => 4
	|  PPmac      => 6
    |  PPword => 15
    |  PPhstring => 16
    |  PPid => 17
    |  PPbXML  => 13
    |  PPeXML  => 14
    |  PPwhite => 19
    |  PPmessage => 21
    |  PPtext => 20
    |  PPpermission => 5
    |  PPpunc s => 18
    |  PPblob => 23
    |  PPempty => 22

    fun BTokenCompleteEnum (t: BToken) : int =
      case t of
       PPint => 8
    |  PPfloat => 7
    |  PPtime     => 1
	|  PPdate     => 2
	|  PPip       => 3
	|  PPhostname  => 9
	|  PPpath     => 12
	|  PPurl      => 10
    |  PPurlbody => 11
	|  PPemail      => 4
	|  PPmac      => 6
    |  PPword => 15
    |  PPhstring => 16
    |  PPid => 17
    |  PPbXML  => 13
    |  PPeXML  => 14
    |  PPwhite => 50
    |  PPmessage => 52
    |  PPtext => 51
    |  PPpermission => 5
    |  PPpunc s => 12 + (
         case s of
             "." => 6
           | "/" => 7
           | "\\" => 8
           | ";" => 9
           | "|" => 10
           | "<" => 11
           | "~" => 12
           | "`" => 13
           | "!" => 14
           | "@" => 15
           | "#" => 16
           | "$" => 17
           | "%" => 18
           | "^" => 19
           | "&" => 20
           | "*" => 21
           | "(" => 22
           | ")" => 23
           | "-" => 24
           | "_" => 25
           | "+" => 26
           | "=" => 27
           | "{" => 28
           | "}" => 29
           | "[" => 30
           | "]" => 31
           | ":" => 32
           | "\"" => 33
           | "'" => 34
           | ">" => 35
           | "," => 36
           | "?" => 37
       )
    |  PPblob => 54
    |  PPempty => 53

    fun BSTokenCompleteEnum ((t, s): BSToken) : int = BTokenCompleteEnum(t)

    structure OrdBTokenTable = RedBlackMapFn(
                     struct type ord_key = int
			    val compare = Int.compare
		     end) 

    fun intToBToken (i, table) = Option.valOf(OrdBTokenTable.find (table, i)) 

    fun indexToBToken i =
      case i+1 of 
       8 => PPint 
    |  7 => PPfloat
    |  1 => PPtime
	|  2 => PPdate
	|  3 => PPip
	|  9 => PPhostname
	|  12 => PPpath
	|  10 => PPurl
    |  11 => PPurlbody
	|  4 => PPemail
	|  6 => PPmac
    |  15 => PPword
    |  16 => PPhstring
    |  17 => PPid
    |  13 => PPbXML
    |  14 => PPeXML
    |  50 => PPwhite
    |  52 => PPmessage 
    |  51 => PPtext 
    |  5 => PPpermission 
    |  18 => PPpunc "." 
           | 19 => PPpunc "/" 
           | 20 => PPpunc "\\" 
           | 21 => PPpunc ";"
           | 22 => PPpunc "|" 
           | 23 => PPpunc "<" 
           | 24 => PPpunc "~" 
           | 25 => PPpunc "`"
           | 26 => PPpunc "!" 
           | 27 => PPpunc "@"
           | 28 => PPpunc "#" 
           | 29 => PPpunc "$" 
           | 30 => PPpunc "%" 
           | 31 => PPpunc "^"
           | 32 => PPpunc "&" 
           | 33 => PPpunc "*" 
           | 34 => PPpunc "("
           | 35 => PPpunc ")" 
           | 36 => PPpunc "-" 
           | 37 => PPpunc "_" 
           | 38 => PPpunc "+" 
           | 39 => PPpunc "=" 
           | 40 => PPpunc "{" 
           | 41 => PPpunc "}"
           | 42 => PPpunc "["
           | 43 => PPpunc "]" 
           | 44 => PPpunc ":" 
           | 45 => PPpunc "\"" 
           | 46 => PPpunc "'" 
           | 47 => PPpunc ">" 
           | 48 => PPpunc "," 
           | 49 => PPpunc "?" 
    |  54 => PPblob
    |  53 => PPempty

    fun compBToken (t1:BToken, t2:BToken):order = 
	case (t1,t2) of
           (PPpunc s1, PPpunc s2) => compStr(s1, s2)
        |  _ => Int.compare(BTokenEnum t1, BTokenEnum t2)


    fun compBSToken ((t1, s1):BSToken, (t2, s2):BSToken):order = (* not really compare the string *) 
	case (t1,t2) of
          (PPbXML, PPbXML) => String.compare(s1, s2) (* should be some substring *)
        | (PPeXML, PPeXML) => String.compare(s1, s2)
        |  _ => compBToken(t1, t2)

    fun compBToken_rough (t1:BToken, t2:BToken):order = 
	case (t1,t2) of
           (PPint, PPfloat) => EQUAL
        |  (PPfloat, PPint) => EQUAL
        |  (PPid, PPfloat) => EQUAL
        |  (PPfloat, PPid) => EQUAL
        |  (PPint, PPid) => EQUAL
        |  (PPid, PPint) => EQUAL
        |  (PPhstring, id) => EQUAL
        |  (PPid, PPhstring) => EQUAL
        |  (PPword, PPid) => EQUAL
        |  (PPid, PPword) => EQUAL
        |  (PPhostname, PPid) => EQUAL
        |  (PPid, PPhostname) => EQUAL
        |  (_, _) => compBToken(t1, t2)


    fun eqBToken(t1,t2) = case compBToken(t1,t2) of EQUAL => true | _ => false

    fun eqBSToken(t1,t2) = case compBSToken(t1,t2) of EQUAL => true | _ => false
    
    structure BTokenMapF = RedBlackMapFn ( struct type ord_key = BToken
                                                       val compare = compBToken
                                           end
                                         )

    structure BSTokenMapF = RedBlackMapFn ( struct type ord_key = BSToken
                                                       val compare = compBSToken
                                           end
                                         )

    type BTokenMap = string BTokenMapF.map
    type BSTokenMap = string BSTokenMapF.map

    fun buildBToken ( kvs : ( BToken * string ) list ) : BTokenMap =
    let fun f ( kv : ( BToken * string ), m : BTokenMap ) : BTokenMap =
        let val ( k, v ) = kv
        in BTokenMapF.insert ( m, k, v )
        end
    in foldl f BTokenMapF.empty kvs
    end 

    val tokenDefList =
        [ ( PPint, "[-~]?([0-9]+)" )
        , ( PPfloat, "([-~]?([0-9]+))|[-~]?([0-9]+)\\.([0-9]+)|[-~]?[1-9]\\.([0-9]+)(E|e)[-~]?([0-9]+)")
        , ( PPtime, "([0-9]{2}):([0-9]{2}):([0-9]{2})([ ]*(am|AM|pm|PM))?([ \\t]+([+-][0-1][0-9]00))?|([0-9]{2}):([0-9]{2})")
        , ( PPdate, "((Jan|jan|Feb|feb|Mar|mar|Apr|apr|May|may|Jun|jun|Jul|jul|Aug|aug|Sep|sep|Oct|oct|Nov|nov|Dec|dec|January|February|March|April|May|June|July|August|September|October|November|December)|(0?[1-9]|1[0-2]))\\/([1-9]|[1-2][0-9]|0[1-9]|3[0-1])\\/([0-2][0-9]{3})|([1-9]|[1-2][0-9]|0[1-9]|3[0-1])\\/((Jan|jan|Feb|feb|Mar|mar|Apr|apr|May|may|Jun|jun|Jul|jul|Aug|aug|Sep|sep|Oct|oct|Nov|nov|Dec|dec|January|February|March|April|May|June|July|August|September|October|November|December)|(0?[1-9]|1[0-2]))\\/([0-2][0-9]{3})|([0-2][0-9]{3})\\/((Jan|jan|Feb|feb|Mar|mar|Apr|apr|May|may|Jun|jun|Jul|jul|Aug|aug|Sep|sep|Oct|oct|Nov|nov|Dec|dec|January|February|March|April|May|June|July|August|September|October|November|December)|(0?[1-9]|1[0-2]))\\/([1-9]|[1-2][0-9]|0[1-9]|3[0-1])|((Jan|jan|Feb|feb|Mar|mar|Apr|apr|May|may|Jun|jun|Jul|jul|Aug|aug|Sep|sep|Oct|oct|Nov|nov|Dec|dec|January|February|March|April|May|June|July|August|September|October|November|December)|(0?[1-9]|1[0-2]))\\-([1-9]|[1-2][0-9]|0[1-9]|3[0-1])\\-([0-2][0-9]{3})|([1-9]|[1-2][0-9]|0[1-9]|3[0-1])\\-((Jan|jan|Feb|feb|Mar|mar|Apr|apr|May|may|Jun|jun|Jul|jul|Aug|aug|Sep|sep|Oct|oct|Nov|nov|Dec|dec|January|February|March|April|May|June|July|August|September|October|November|December)|(0?[1-9]|1[0-2]))\\-([0-2][0-9]{3})|([0-2][0-9]{3})\\-((Jan|jan|Feb|feb|Mar|mar|Apr|apr|May|may|Jun|jun|Jul|jul|Aug|aug|Sep|sep|Oct|oct|Nov|nov|Dec|dec|January|February|March|April|May|June|July|August|September|October|November|December)|(0?[1-9]|1[0-2]))\\-([1-9]|[1-2][0-9]|0[1-9]|3[0-1])|((Jan|jan|Feb|feb|Mar|mar|Apr|apr|May|may|Jun|jun|Jul|jul|Aug|aug|Sep|sep|Oct|oct|Nov|nov|Dec|dec|January|February|March|April|May|June|July|August|September|October|November|December)|(0?[1-9]|1[0-2]))\\.([1-9]|[1-2][0-9]|0[1-9]|3[0-1])\\.([0-2][0-9]{3})|([1-9]|[1-2][0-9]|0[1-9]|3[0-1])\\.((Jan|jan|Feb|feb|Mar|mar|Apr|apr|May|may|Jun|jun|Jul|jul|Aug|aug|Sep|sep|Oct|oct|Nov|nov|Dec|dec|January|February|March|April|May|June|July|August|September|October|November|December)|(0?[1-9]|1[0-2]))\\.([0-2][0-9]{3})|([0-2][0-9]{3})\\.((Jan|jan|Feb|feb|Mar|mar|Apr|apr|May|may|Jun|jun|Jul|jul|Aug|aug|Sep|sep|Oct|oct|Nov|nov|Dec|dec|January|February|March|April|May|June|July|August|September|October|November|December)|(0?[1-9]|1[0-2]))\\.([1-9]|[1-2][0-9]|0[1-9]|3[0-1])|((Mon|Monday|Tue|Tuesday|Wed|Wednesday|Thu|Thursday|Fri|Friday|Sat|Saturday|Sun|Sunday|mon|tue|wed|thu|fri|sat|sun),?[ \\t]+)?(Jan|jan|Feb|feb|Mar|mar|Apr|apr|May|may|Jun|jun|Jul|jul|Aug|aug|Sep|sep|Oct|oct|Nov|nov|Dec|dec|January|February|March|April|May|June|July|August|September|October|November|December)[ \\t]+([1-9]|[1-2][0-9]|0[1-9]|3[0-1])(,[ \\t]+([0-2][0-9]{3}))?|((Mon|Monday|Tue|Tuesday|Wed|Wednesday|Thu|Thursday|Fri|Friday|Sat|Saturday|Sun|Sunday|mon|tue|wed|thu|fri|sat|sun),?[ \\t]+)?([1-9]|[1-2][0-9]|0[1-9]|3[0-1])[ \\t]+(Jan|jan|Feb|feb|Mar|mar|Apr|apr|May|may|Jun|jun|Jul|jul|Aug|aug|Sep|sep|Oct|oct|Nov|nov|Dec|dec|January|February|March|April|May|June|July|August|September|October|November|December)(,[ \\t]+([0-2][0-9]{3}))?")  (* slow *) 
       , ( PPip, "([0-9]{1,3})\\.([0-9]{1,3})\\.([0-9]{1,3})\\.([0-9]{1,3})")
       , ( PPhostname, "((([0-9A-Za-z]|[0-9A-Za-z][0-9A-Za-z]|[0-9A-Za-z][A-Za-z0-9_-]{1,61}[0-9A-Za-z])\\.)+(com|net|edu|org|gov)(\\.[a-z][a-z])?)")
       , ( PPemail, "(([0-9A-Za-z!#$%&'*+/=?\\^_`{|}~]|(\\-))([.]?([a-zA-Z0-9!#$%&'*+/=?\\^_`{|}~]|(\\-))){1,61}([a-zA-Z0-9!#$%&'*+/=?\\^_`{|}~]|(\\-))|([a-zA-Z0-9!#$%&'*+/=?\\^_`{|}~]|(\\-))|([a-zA-Z0-9!#$%&'*+/=?\\^_`{|}~]|(\\-))([a-zA-Z0-9!#$%&'*+/=?\\^_`{|}~]|(\\-)))@((([0-9A-Za-z_\\-]{1,63})\\.)+([0-9A-Za-z_\\-]{1,63}))")
       , ( PPmac, "(([0-9a-fA-F]{2})(:|\\-)){5}([0-9a-fA-F]{2})")
(*       , ( PPpath, "(\\/([^\\/\\\\?*:<>\"\\[\\] ]+))(\\/([^\\/\\\\?*:<>\"\\[\\] ]+))*\\/?|(([^\\/\\\\?*:<>\"\\[\\] ]+)\\/)(([^\\/\\\\?*:<>\"\\[\\] ]+)\\/)*([^\\/\\\\?*:<>\"\\[\\] ]+)?|\\\\?(\\\\([^\\/\\\\?*:<>\"\\[\\] ]+))(\\\\([^\\/\\\\?*:<>\"\\[\\] ]+))*\\\\?|(([^\\/\\\\?*:<>\"\\[\\] ]+)\\\\)(([^\\/\\\\?*:<>\"\\[\\] ]+)\\\\)*([^\\/\\\\?*:<>\"\\[\\] ]+)?|([^\\/\\\\?*:<>\"\\[\\] ]+)|\\/([^\\/\\\\?*:<>\"\\[\\] ]+)|\\\\([^\\/\\\\?*:<>\"\\[\\] ]+)|\\/|\\\\") (* slow *) *)
       , ( PPpath, "(\\/([^][\\/\\\\?*:<>+\" ]+))(\\/([^][\\/\\\\?*:<>+\" ]+))*\\/?|(([^][\\/\\\\?*:<>+\" ]+)\\/)(([^][\\/\\\\?*:<>+\" ]+)\\/)*([^][\\/\\\\?*:<>+\" ]+)?|\\\\?(\\\\([^][\\/\\\\?*:<>+\" ]+))(\\\\([^][\\/\\\\?*:<>+\" ]+))*\\\\?|(([^][\\/\\\\?*:<>+\" ]+)\\\\)(([^][\\/\\\\?*:<>+\" ]+)\\\\)*([^][\\/\\\\?*:<>+\" ]+)?|\\\\([^][\\/\\\\?*:<>+\" ]+)|\\/|\\\\|([^][\\/\\\\?*:<>+\" ]+)")
(*(*(*(*(*(*(* *)*)*)*)*)*)*)
       , ( PPurl, "(http|ftp|https):\\/\\/((([0-9A-Za-z_-])\\.)+([0-9A-Za-z_-]))(:([1-9][0-9]*))?\\/?(\\/([^\\/\\\\?*:<>\"\\[\\] ]+))*\\/?(\\?)?\\&?([^&=]+=[^&]*(\\&[^&=]+=[^&]*)*\\&?)?(#([0-9A-Za-z][A-Za-z0-9_-]*))?|(http|ftp|https):\\/\\/(([0-9]{1,3})\\.([0-9]{1,3})\\.([0-9]{1,3})\\.([0-9]{1,3}))(:([1-9][0-9]*))?\\/?(\\/([^\\/\\\\?*:<>\"\\[\\] ]+))*\\/?(\\?)?\\&?([^&=]+=[^&]*(\\&[^&=]+=[^&]*)*\\&?)?(#([0-9A-Za-z][A-Za-z0-9_-]*))?")  (* extremely slow *) 
(*(*(*(*(*(*(* *)*)*)*)*)*)*)
       , ( PPurlbody, "((([0-9A-Za-z_-])\\.)+([0-9A-Za-z_-]))(:([1-9][0-9]*))?\\/?(\\/([^\\/\\\\?*:<>\"\\[\\] ]+))*\\/?(\\?)?\\&?([^&=]+=[^&]*(\\&[^&=]+=[^&]*)*\\&?)?(#([0-9A-Za-z][A-Za-z0-9_-]*))?|\\/\\/(([0-9]{1,3})\\.([0-9]{1,3})\\.([0-9]{1,3})\\.([0-9]{1,3}))(:([1-9][0-9]*))?\\/?(\\/([^\\/\\\\?*:<>\"\\[\\] ]+))*\\/?(\\?)?\\&?([^&=]+=[^&]*(\\&[^&=]+=[^&]*)*\\&?)?(#([0-9A-Za-z][A-Za-z0-9_-]*))?") (* extremely slow *)
       , ( PPword, "[A-Za-z'-]+")
       , ( PPid, "[-0-9A-Za-z_.]+")
(*       , ( PPbXML, "\\<([a-zA-Z])+\\>")
       , ( PPeXML, "\\<\\/[^>]+\\>") *) 
       , ( PPwhite, "[ \t\r\n]+") 
       , ( PPmessage, "\\\".+\\\"|[[].+[]]|[(].+[)]|[{].+[}]|.+\\?|:[^:]+")  (* slow *)
       , ( PPtext, "([\"'])[-A-Za-z0-9_,:;. ]+([\"'])|[-A-Za-z0-9_,:;. ]+")
       , ( PPpermission, "[-dl]([-r][-w][-xsStT]){3}")  
       , ( PPhstring, "[0-9a-f]+")
       , ( PPpunc ".", "[.]")
       , ( PPpunc "/", "[/]")
       , ( PPpunc "\\", "[\\\\]") 
       , ( PPpunc ";", "[;]")
       , ( PPpunc "|", "[|]")
       , ( PPpunc "<", "[<]")
       , ( PPpunc "~", "[~]")
       , ( PPpunc "`", "[`]")       
       , ( PPpunc "!", "[!]")
       , ( PPpunc "@", "[@]")
       , ( PPpunc "#", "[#]")
       , ( PPpunc "$", "[$]")
       , ( PPpunc "%", "[%]")
       , ( PPpunc "^", "[\\^]") 
       , ( PPpunc "&", "[&]")
       , ( PPpunc "*", "[*]")
       , ( PPpunc "(", "[(]")
       , ( PPpunc ")", "[)]")
       , ( PPpunc "-", "[-]")
       , ( PPpunc "_", "[_]")
       , ( PPpunc "+", "[+]")
       , ( PPpunc "=", "[=]")
       , ( PPpunc "{", "[{]")
       , ( PPpunc "}", "[}]")
       , ( PPpunc "[", "[[]")
       , ( PPpunc "]", "[]]")
       , ( PPpunc ":", "[:]")
       , ( PPpunc "\"", "[\"]")
       , ( PPpunc "'", "[']")
       , ( PPpunc ">", "[>]")
       , ( PPpunc ",", "[,]")
       , ( PPpunc "?", "[?]")
       , ( PPblob, "^([0-9]+)([0-9]+\\.[0-9]+)(([0-9]{2}):([0-9]{2}):([0-9]{2})([ ]*(am|AM|pm|PM))?([#\\t]+([+-][0-1][0-9]00))?)(((Jan|jan|Feb|feb|Mar|mar|Apr|apr|May|may|Jun|jun|Jul|jul|Aug|aug|Sep|sep|Oct|oct|Nov|nov|Dec|dec|January|February|March|April|May|June|July|August|September|October|November|December)|(0?[1-9]|1[0-2]))\\/([1-9]|[1-2][0-9]|0[1-9]|3[0-1])\\/([0-2][0-9]{3})|([1-9]|[1-2][0-9]|0[1-9]|3[0-1])\\/((Jan|jan|Feb|feb|Mar|mar|Apr|apr|May|may|Jun|jun|Jul|jul|Aug|aug|Sep|sep|Oct|oct|Nov|nov|Dec|dec|January|February|March|April|May|June|July|August|September|October|November|December)|(0?[1-9]|1[0-2]))\\/([0-2][0-9]{3})|([0-2][0-9]{3})\\/((Jan|jan|Feb|feb|Mar|mar|Apr|apr|May|may|Jun|jun|Jul|jul|Aug|aug|Sep|sep|Oct|oct|Nov|nov|Dec|dec|January|February|March|April|May|June|July|August|September|October|November|December)|(0?[1-9]|1[0-2]))\\/([1-9]|[1-2][0-9]|0[1-9]|3[0-1])|((Jan|jan|Feb|feb|Mar|mar|Apr|apr|May|may|Jun|jun|Jul|jul|Aug|aug|Sep|sep|Oct|oct|Nov|nov|Dec|dec|January|February|March|April|May|June|July|August|September|October|November|December)|(0?[1-9]|1[0-2]))\\-([1-9]|[1-2][0-9]|0[1-9]|3[0-1])\\-([0-2][0-9]{3})|([1-9]|[1-2][0-9]|0[1-9]|3[0-1])\\-((Jan|jan|Feb|feb|Mar|mar|Apr|apr|May|may|Jun|jun|Jul|jul|Aug|aug|Sep|sep|Oct|oct|Nov|nov|Dec|dec|January|February|March|April|May|June|July|August|September|October|November|December)|(0?[1-9]|1[0-2]))\\-([0-2][0-9]{3})|([0-2][0-9]{3})\\-((Jan|jan|Feb|feb|Mar|mar|Apr|apr|May|may|Jun|jun|Jul|jul|Aug|aug|Sep|sep|Oct|oct|Nov|nov|Dec|dec|January|February|March|April|May|June|July|August|September|October|November|December)|(0?[1-9]|1[0-2]))\\-([1-9]|[1-2][0-9]|0[1-9]|3[0-1])|((Jan|jan|Feb|feb|Mar|mar|Apr|apr|May|may|Jun|jun|Jul|jul|Aug|aug|Sep|sep|Oct|oct|Nov|nov|Dec|dec|January|February|March|April|May|June|July|August|September|October|November|December)|(0?[1-9]|1[0-2]))\\.([1-9]|[1-2][0-9]|0[1-9]|3[0-1])\\.([0-2][0-9]{3})|([1-9]|[1-2][0-9]|0[1-9]|3[0-1])\\.((Jan|jan|Feb|feb|Mar|mar|Apr|apr|May|may|Jun|jun|Jul|jul|Aug|aug|Sep|sep|Oct|oct|Nov|nov|Dec|dec|January|February|March|April|May|June|July|August|September|October|November|December)|(0?[1-9]|1[0-2]))\\.([0-2][0-9]{3})|([0-2][0-9]{3})\\.((Jan|jan|Feb|feb|Mar|mar|Apr|apr|May|may|Jun|jun|Jul|jul|Aug|aug|Sep|sep|Oct|oct|Nov|nov|Dec|dec|January|February|March|April|May|June|July|August|September|October|November|December)|(0?[1-9]|1[0-2]))\\.([1-9]|[1-2][0-9]|0[1-9]|3[0-1])|((Mon|Monday|Tue|Tuesday|Wed|Wednesday|Thu|Thursday|Fri|Friday|Sat|Saturday|Sun|Sunday|mon|tue|wed|thu|fri|sat|sun),?[ \\t]+)?(Jan|jan|Feb|feb|Mar|mar|Apr|apr|May|may|Jun|jun|Jul|jul|Aug|aug|Sep|sep|Oct|oct|Nov|nov|Dec|dec|January|February|March|April|May|June|July|August|September|October|November|December)[ \\t]+([1-9]|[1-2][0-9]|0[1-9]|3[0-1])(,[ \\t]+([0-2][0-9]{3}))?|((Mon|Monday|Tue|Tuesday|Wed|Wednesday|Thu|Thursday|Fri|Friday|Sat|Saturday|Sun|Sunday|mon|tue|wed|thu|fri|sat|sun),?[ \\t]+)?([1-9]|[1-2][0-9]|0[1-9]|3[0-1])[ \\t]+(Jan|jan|Feb|feb|Mar|mar|Apr|apr|May|may|Jun|jun|Jul|jul|Aug|aug|Sep|sep|Oct|oct|Nov|nov|Dec|dec|January|February|March|April|May|June|July|August|September|October|November|December)(,[ \\t]+([0-2][0-9]{3}))?)(([0-9]{1,3})\\.([0-9]{1,3})\\.([0-9]{1,3})\\.([0-9]{1,3}))(((([0-9A-Za-z]|[0-9A-Za-z][0-9A-Za-z]|[0-9A-Za-z][A-Za-z0-9_\\-]{1,61}[0-9A-Za-z])\\.)+(com|net|edu|org|gov)(\\.[a-z][a-z])?))(([0-9A-Za-z!#$%&'*+\\-/=?\\^_`{|}~]([.]?[a-zA-Z0-9!#$%&'*+\\-/=?\\^_`{|}~]){1,61}[a-zA-Z0-9!#$%&'*+\\-/=?\\^_`{|}~]|[a-zA-Z0-9!#$%&'*+\\-/=?\\^_`{|}~]|[a-zA-Z0-9!#$%&'*+\\-/=?\\^_`{|}~][a-zA-Z0-9!#$%&'*+\\-/=?\\^_`{|}~])@((([0-9A-Za-z]|[0-9A-Za-z][0-9A-Za-z]|[0-9A-Za-z][A-Za-z0-9_\\-]{1,61}[0-9A-Za-z])\\.)+([0-9A-Za-z]|[0-9A-Za-z][0-9A-Za-z]|[0-9A-Za-z][A-Za-z0-9_\\-]{1,61}[0-9A-Za-z])))((([0-9a-fA-F]{2})(:|\\-)){5}([0-9a-fA-F]{2}))((\\/([^\\/\\\\?*:<>\"\\[\\] ]+)){2}(\\/([^\\/\\\\?*:<>\"\\[\\] ]+))*\\/?|(([^\\/\\\\?*:<>\"\\[\\] ]+)\\/){2}(([^\\/\\\\?*:<>\"\\[\\] ]+)\\/)*([^\\/\\\\?*:<>\"\\[\\] ]+)?|\\\\?(\\\\([^\\/\\\\?*:<>\"\\[\\] ]+)){2}(\\\\([^\\/\\\\?*:<>\"\\[\\] ]+))*\\\\?|(([^\\/\\\\?*:<>\"\\[\\] ]+)\\\\){2}(([^\\/\\\\?*:<>\"\\[\\] ]+)\\\\)*([^\\/\\\\?*:<>\"\\[\\] ]+)?)((http|ftp|https):\\/\\/((([0-9A-Za-z]|[0-9A-Za-z][0-9A-Za-z]|[0-9A-Za-z][A-Za-z0-9_\\-]{1,61}[0-9A-Za-z])\\.)+([0-9A-Za-z]|[0-9A-Za-z][0-9A-Za-z]|[0-9A-Za-z][A-Za-z0-9_\\-]{1,61}[0-9A-Za-z]))(:([1-9][0-9]*))?\\/?(\\/([^\\/\\\\?*:<>\"\\[\\] ]+))*\\/?(\\?)?\\&?([^&=]+=[^&]*(\\&[^&=]+=[^&]*)*\\&?)?(#([0-9A-Za-z][A-Za-z0-9_\\-]*))?|(http|ftp|https):\\/\\/(([0-9]{1,3})\\.([0-9]{1,3})\\.([0-9]{1,3})\\.([0-9]{1,3}))(:([1-9][0-9]*))?\\/?(\\/([^\\/\\\\?*:<>\"\\[\\] ]+))*\\/?(\\?)?\\&?([^&=]+=[^&]*(\\&[^&=]+=[^&]*)*\\&?)?(#([0-9A-Za-z][A-Za-z0-9_\\-]*))?)(((([0-9A-Za-z]|[0-9A-Za-z][0-9A-Za-z]|[0-9A-Za-z][A-Za-z0-9_\\-]{1,61}[0-9A-Za-z])\\.)+([0-9A-Za-z]|[0-9A-Za-z][0-9A-Za-z]|[0-9A-Za-z][A-Za-z0-9_\\-]{1,61}[0-9A-Za-z]))(:([1-9][0-9]*))?\\/?(\\/([^\\/\\\\?*:<>\"\\[\\] ]+))*\\/?(\\?)?\\&?([^&=]+=[^&]*(\\&[^&=]+=[^&]*)*\\&?)?(#([0-9A-Za-z][A-Za-z0-9_\\-]*))?|(http|ftp|https):\\/\\/(([0-9]{1,3})\\.([0-9]{1,3})\\.([0-9]{1,3})\\.([0-9]{1,3}))(:([1-9][0-9]*))?\\/?(\\/([^\\/\\\\?*:<>\"\\[\\] ]+))*\\/?(\\?)?\\&?([^&=]+=[^&]*(\\&[^&=]+=[^&]*)*\\&?)?(#([0-9A-Za-z][A-Za-z0-9_\\-]*))?)([A-Za-z'\\-]+)([0-9A-Za-z\\-_\\.]+)(\\<\\/[^>]+\\>)(\\<([a-zA-Z])+\\>)([ \\t\\r\\n]+)(\".+\"|[[].+[]]|[(].+[)]|[{].+[}]|.+\\?$)((d|\\-)((r|\\-)(w|\\-)(x|-))[3])([.])([/])([\\])([;])([|])([<])([~])([`])([!])([@])([#])([$])([%])([^])([&])([*])([(])([)])([-])([_])([+])([=])([{])([}])([[])([]])([:])([\"])(['])([>])([,])([?])")
        ]
    
    val btokentable : BTokenMap = buildBToken tokenDefList

    fun hasRegex ( t: BToken ) : bool = BTokenMapF.inDomain ( btokentable, t )

    fun getReg ( t : BToken ) : string option  = BTokenMapF.find ( btokentable, t )

    exception Bust

    fun BTokenToName ( t : BToken ) : string = 
	case t of
       PPint => "int"
    |  PPfloat => "float"
    |  PPtime     => "time"
	|  PPdate     => "date"
	|  PPip       => "ip"
	|  PPhostname  => "host"
	|  PPpath     => "path"
	|  PPurl      => "url"
    |  PPurlbody => "urlbody"
	|  PPemail      => "email"
	|  PPmac      => "mac"
    |  PPword => "word"
    |  PPhstring => "hstring"
    |  PPid => "id"
        |  PPbXML  => "bXML"
        |  PPeXML  => "eXML"
    |  PPwhite => "white"
    |  PPmessage => "message"
    |  PPpermission => "permission"
    |  PPpunc s => "punctuation:"^s
    |  PPblob => "blob"
    |  PPtext => "text"
    |  PPempty => "empty"

    fun BTokenToName_print ( t : BToken ) : string = 
	case t of
       PPint => "int"
    |  PPfloat => "float"
    |  PPtime     => "time"
	|  PPdate     => "date"
	|  PPip       => "ip"
	|  PPhostname  => "host"
	|  PPpath     => "path"
	|  PPurl      => "url"
    |  PPurlbody => "urlbody"
	|  PPemail      => "email"
	|  PPmac      => "mac"
    |  PPword => "word"
    |  PPhstring => "hstring"
    |  PPid => "id"
        |  PPbXML  => "bXML"
        |  PPeXML  => "eXML"
    |  PPwhite => "white"
    |  PPmessage => "message"
    |  PPpermission => "permission"
    |  PPpunc s => "punc"
    |  PPblob => "blob"
    |  PPtext => "text"
    |  PPempty => "empty"

    fun BSTokenToName_print (b, s) = BTokenToName_print b

    fun nameToBToken ( t : string ) : BToken = 
	case t of
       "int" => PPint
    |  "float" => PPfloat 
    |  "time" => PPtime    
	|  "date" => PPdate
	|  "ip" => PPip      
	|  "host" => PPhostname 
	|  "path" => PPpath     
	|  "url" => PPurl      
    |  "urlbody" => PPurlbody 
	|  "email" => PPemail     
	|  "mac" => PPmac      
    |  "word" => PPword 
    |  "hstring" => PPhstring
    |  "id" => PPid
    |  "bXML" => PPbXML
    |  "eXML" => PPeXML
    |  "white" => PPwhite 
    |  "message" => PPmessage
    |  "permission" => PPpermission
    |  "blob" => PPblob
    |  "text" => PPtext
    |  "empty" => PPempty
    |  _ => 
         let
           fun isColon c = c = #":"
           val (junk, puncs) = Substring.splitl (not o isColon) (Substring.full t)
           val punc = Substring.triml 1 puncs 
         in
           PPpunc (Substring.string punc)
         end

    fun charToBToken c =
            case c of
                #"." => PPpunc "."
              | #"/" => PPpunc "/"
              | #"\\" => PPpunc "\\" 
              | #";" => PPpunc ";"
              | #"|" => PPpunc "|"
              | #"<" => PPpunc "<"
              | #"~" => PPpunc "~"
              | #"`" => PPpunc "`"   
              | #"!" => PPpunc "!"
              | #"@" => PPpunc "@"
              | #"#" => PPpunc "#"
              | #"$" => PPpunc "$"
              | #"%" => PPpunc "%"
              | #"^" => PPpunc "^" 
              | #"&" => PPpunc "&"
              | #"*" => PPpunc "*"
              | #"(" => PPpunc "("
              | #")" => PPpunc ")"
              | #"-" => PPpunc "-"
              | #"_" => PPpunc "_"
              | #"+" => PPpunc "+"
              | #"=" => PPpunc "="
              | #"{" => PPpunc "{"
              | #"}" => PPpunc "}"
              | #"[" => PPpunc "["
              | #"]" => PPpunc "]"
              | #":" => PPpunc ":"
              | #"\"" => PPpunc "\""
              | #"'" => PPpunc "'"
              | #">" => PPpunc ">"
              | #"," => PPpunc ","
              | #"?" => PPpunc "?"
              | _ => PPblob

    fun tokenToBToken t : BToken list = (* a mapping from old tokens to corresponding new tokens *) 
      case t of    
          Ptime i => [PPtime]
	    | Pdate i => [PPdate]
	    | Pip i => [PPip]
        | Phostname i => [PPhostname]
	    | Ppath i => [PPpath]
	    | Purl i => [PPurl]
	    | Pemail i => [PPemail]
	    | Pmac i => [PPmac]
        | PbXML (f,s) => [PPbXML]
        | PeXML (f,s) => [PPeXML]
    	| Pint _  => [PPint]
	    | Pfloat _ => [PPfloat]           
        | Pstring s => [PPblob]
        | Pwhite s  => [PPwhite] 
        | Pgroup {left=(lt, ll), body=mybody, right=(rt, tl)} =>
            let
              val l = tokenToBToken lt
              val r = tokenToBToken rt
              fun extractOne ((bt, bl), ret) = (tokenToBToken bt)@ret
              val b = List.foldl extractOne [] mybody
            in
              (l@b)@b
            end 
        | Other c  => [charToBToken c]
        | Pempty    => [PPempty]
        | Error => [PPblob] 

    fun tokenToBSToken t : BSToken list = (* a mapping from old tokens to corresponding new tokens *) 
      case t of    
          Ptime i => [(PPtime, i)]
	    | Pdate i => [(PPdate, i)]
	    | Pip i => [(PPip, i)]
        | Phostname i => [(PPhostname, i)]
	    | Ppath i => [(PPpath, i)]
	    | Purl i => [(PPurl, i)]
	    | Pemail i => [(PPemail, i)]
	    | Pmac i => [(PPmac, i)]
        | PbXML (f,s) => [(PPbXML, f^s)]
        | PeXML (f,s) => [(PPeXML, f^s)]
    	| Pint (i,s)  => [(PPint, s)]
	    | Pfloat (i,f) => [(PPfloat, i^"."^f)]           
        | Pstring s => [(PPblob, s)]
        | Pwhite s  => [(PPwhite, s)] 
        | Pgroup {left=(lt, ll), body=mybody, right=(rt, tl)} =>
            let
              val l = tokenToBSToken lt
              val r = tokenToBSToken rt
              fun extractOne ((bt, bl), ret) = (tokenToBSToken bt)@ret
              val b = List.foldl extractOne [] mybody
            in
              (l@b)@b
            end 
        | Other c  => [(charToBToken c, Char.toString c)]
        | Pempty    => [(PPempty, "")]
        | Error => [(PPblob, "")] 

    fun getRegex ( t : BToken ) : string =
(*
      let
        fun printall (item,s) = (print (BTokenToName item); print "\n")
      in 
        (List.app printall (BTokenMapF.listItemsi btokentable); raise Bust)
      end
*)
(*
      if (BTokenMapF.member (btokentable, PPint) ) then (print "yes\n"; "0")
      else (print "no\n"; raise Bust)
*)


      case ( getReg t ) of
               NONE   => raise Bust
             | SOME t => t
     
    fun sumBSLTokenLength ( ts : BSLToken list ) : LargeInt.int =
        foldl ( fn ( ((b,s),l) : BSLToken, x : LargeInt.int ) =>
                x + Int.toLarge ( String.size s ) ) 0 ts


    fun avgBSLTokenLength ( ts : BSLToken list ) : real =
        ( Real.fromLargeInt ( sumBSLTokenLength ts ) ) /
        ( Real.fromInt ( length ts ) ) 


    fun btokenOf (t:BSLToken):BToken = #1 (#1 t)

    fun bstokenOf (t:BSLToken):BSToken = #1 t

    fun btokenLength (t:BSToken):int =
        ( case t of
               (PPbXML, s) => size s
             | (PPeXML, s) => size s
             | (PPtime, s) => size s
             | (PPdate, s) => size s
             | (PPpath, s) => 
                 let val nsep       : int    = countCh #"/" s
                     fun isSep ( x : char ) : bool = x = #"/"
                     val components : string list =
                       map Substring.string
                           ( Substring.fields isSep ( Substring.full s ) )
                 in if nsep = 0 then size s else size s - nsep
                 end
               (* TODO: URLs and emails are too hard to parse right now, 
		also the issue of case sensitivity *)
             | (PPurl, s)         => size s
             | (PPurlbody, s) => size s
             | (PPemail, s)         => size s
             | (PPmac, s)         => 12  (*6 hex numbers, take away the delimiters *)
             | (PPip, s)          => countCh #"." s + 1
             | (PPhostname, s)    =>
                 let val ndot          : int            = countCh #"." s
                     fun isDot ( x : char ) : bool = x = #"."
                     val components    : string list =
                           map Substring.string ( Substring.fields isDot ( Substring.full s ) )
                     val lastComponent : string = List.last components
                 in if ndot = 0
                    then size s
                    else if isDomainName lastComponent
                         then size s - ndot - ( size lastComponent )
                         else size s - ndot
                 end
             | (PPint, s)    => size s (*ignore the length of the s as it's aux info*)
             | (PPfloat, s)   => size s - 1 
             | (PPwhite, s)       => size s
             | (PPempty, s)         => 0
             | (PPpunc c, s) => 1
             | (PPword, s) => size s
             | (PPid, s) => size s
             | (PPhstring, s) => size s
             | (PPmessage, s) => size s
             | (PPtext, s) => size s
             | (PPpermission, s) => size s (* can be compressed *)
             | (PPblob, s) => size s
             | (PPError, s) => 0
        )

    fun bslTokenLength (t:BSLToken):int = btokenLength (bstokenOf t)
   
    fun stringToPuncName s =
      case s of
          "." => "dot"
        | "/" => "slash"
        | "\\" => "bslash"
        | ";" => "scolon"
        | "|" => "bar"
        | "<" => "less"
        | "~" => "tilde"
        | "`" => "bquote"
        | "!" => "bang"
        | "@" => "at"
        | "#" => "hash"
        | "$" => "dollar"
        | "%" => "percent"
        | "^" => "caret"
        | "&" => "and"
        | "*" => "star"
        | "(" => "lpar"
        | ")" => "rpar"
        | "-" => "hyphen"
        | "_" => "underscore"
        | "+" => "plus"
        | "=" => "equa"
        | "{" => "lbrac"
        | "}" => "rbrac"
        | "[" => "lsqubrac"
        | "]" => "rsqubrac"
        | ":" => "colon"
        | "\"" => "dquote"
        | "'" => "quote"
        | ">" => "greater"
        | "," => "comma"
        | "?" => "question" 

  (* classify tokens into 3 classes:
     punctuation: represented by PPpunc "."
     white: represented by PPwhite
     other: represented by PPblob
  *)
  fun BToken2BTokenClass t =
    case t of
        PPwhite => PPwhite
      | PPpunc "." => PPpunc "."
      | PPpunc ":" => PPpunc ":"
      | PPpunc "_" => PPpunc "_"
      | PPpunc "/" => PPpunc "/"
      | PPpunc _ => PPpunc "|"   (* "|" represents all other punctuations *)
(*      | PPword => PPword *)
      | _ => PPblob

  val BTokenClass = [PPwhite, PPpunc ".", PPpunc ":", PPpunc "_", PPpunc "/", PPpunc "|", (*PPword,*) PPblob]

 (* test of two tokens are adjacent to each other assuming none of them are Pempty *)
 fun adjacent ((t1, l1):BSLToken) ((t2, l2):BSLToken) : bool =
      if (#lineNo l1) = (#lineNo l2) 
         andalso ((#endloc l1) + 1) = (#beginloc l2) 
      then true else false

end
