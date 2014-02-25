structure Tokens = struct
    open Complexity
    open Hosts

    type location = { lineNo: int, beginloc: int, endloc:int, recNo:int}
    fun mkLoc( b : int ) ( e : int ) ( r : int ) ( ln : int ) : location =
        { lineNo = ln, beginloc = b, endloc = e, recNo = r }

    fun combLoc ({lineNo=l1, beginloc=b1, endloc=e1, recNo = r1}, 
		    {lineNo=l2, beginloc=b2, endloc=e2, recNo = r2}) =
		{lineNo=l1, beginloc=b1, endloc=e2, recNo=r1}
    (* Establish an order on locations *)
    fun compLocation (l1:location, l2:location):order =
        let val {lineNo = ln1, beginloc = b1, endloc = e1, ... } = l1
            val {lineNo = ln2, beginloc = b2, endloc = e2, ... } = l2
        in ( case Int.compare (ln1, ln2) of
                  LESS    => LESS
                | GREATER => GREATER
                | EQUAL   => ( case Int.compare (b1, b2) of
                                    LESS    => LESS
                                  | GREATER => GREATER
                                  | EQUAL   => Int.compare (e1, e2)
                             )
           )
        end

    (* Number of possible ASCII characters for string values, according
       to the definition in tokens.lex
     *)
    val numAlphaChars  : LargeInt.int  = 52
    val numDigits      : LargeInt.int  = 10
    val numStringChars : LargeInt.int  = numAlphaChars + numDigits + 1 + 1
    val numWhiteChars  : LargeInt.int  = 4 (* Space, tab, \n and \r *)
    val numXMLChars    : LargeInt.int  = numAlphaChars
    val numHexChars    : LargeInt.int  = 10+6
    val numOtherChars  : LargeInt.int  = 256 - numStringChars - numWhiteChars
    val compXML        : Complexity    = int2Comp numXMLChars
    val numWordChars   : LargeInt.int  = numAlphaChars + 2
    val numIdChars     : LargeInt.int  = numAlphaChars + numDigits + 3
    val numTextChars   : LargeInt.int  = numAlphaChars + numDigits + 6
    val numPermissionChars : LargeInt.int = 4
    val numPuncChars   : LargeInt.int  = 32


    (* Some analysis of the structure of tokens from tokens.lex: *)
    fun powerL ( x : LargeInt.int ) ( y : int ) : LargeInt.int =
        if ( y = 0 ) then 1 else x * powerL x ( y - 1 )

    val numTriplet       : LargeInt.int = 1000
    val numIPTriplet     : LargeInt.int = 256
    val numIP            : LargeInt.int = powerL numIPTriplet 4
    val compIP           : Complexity   = int2Comp numIP
    val numDoublet       : LargeInt.int = 100
    val numTimeZone      : LargeInt.int = 2 * 2 * 10
    val numAMPM          : LargeInt.int = 4
    val numTime          : LargeInt.int =
        let val numDoublet3 = numDoublet * numDoublet * numDoublet
        in numDoublet3 * numAMPM * numTimeZone + numDoublet3 * numAMPM +
           numDoublet3 * numTimeZone + numDoublet3
        end
    val numPort          : LargeInt.int = powerL 2 16 (* 65535 *)
    val numFileNameChars : LargeInt.int = 256 (* Revisit this one *)
    val numDay           : LargeInt.int = 4 + 30
    val numWeekDay       : LargeInt.int = 21
    val numMonth         : LargeInt.int = 48
    val numYear          : LargeInt.int = 3000
    val numDate          : LargeInt.int = 6 + ( numMonth * numDay * numYear ) +
                                          2 + 2 + 2 + ( ( 2 + numWeekDay ) *
                                                        ( 2 + numMonth ) * numDay *
                                                        ( 2 + numYear ) )
    val numDomain        : LargeInt.int = 256
    val numToken         : LargeInt.int = 16 (* Number of cases in datatype Token *)

    val compTime         : Complexity = int2Comp numTime

    (* Raw token format, pass one over the data *)
    datatype Token = PbXML       of string * string |
                     PeXML       of string * string |
	             Ptime       of string |
	             Pdate       of string |
	             Ppath       of string |
	             Purl        of string |
		     Pip         of string |
		     Phostname   of string |
	             Pemail      of string |
	             Pmac        of string |
                     Pint        of LargeInt.int * string |  (* a pair of int and string representations *)
                     Pfloat      of string * string | (*string representations of the integer and fraction parts*) 
		     Pstring     of string |
                     Pgroup      of { left : LToken, body : LToken list, right : LToken } |
	             Pwhite      of string |
		     Other       of char |
		     Pempty |
		     Error
    withtype LToken = Token * location

    (*    Establish an order on Token using the following constraints:
          Ptime < Pdate < Pip< Phostname < Purl < Ppath < PbXML < PeXML < Pemail < Pmac < Pfloat < Pint <  Pstring < Pgroup <
          Pwhite < Other < Pempty < Error
     *)
    fun compToken (t1:Token, t2:Token):order = 
	case (t1,t2) of
           (Ptime i1, Ptime i2)           => EQUAL
        |  (Pdate i1, Pdate i2)           => EQUAL
        |  (Purl i1, Purl i2)             => EQUAL
        |  (Ppath i1, Ppath i2)           => EQUAL
        |  (Pip i1, Pip i2)               => EQUAL
        |  (Phostname i1, Phostname i2)   => EQUAL
        |  (Pemail i1, Pemail i2)     	  => EQUAL
        |  (Pmac i1, Pmac i2)             => EQUAL
        |  (PbXML (f1,s1), PbXML (f2,s2)) => String.compare(f1,f2)
        |  (PeXML (f1,s1), PeXML (f2,s2)) => String.compare(f1,f2)
        |  (Pint _, Pint _)             => EQUAL
        |  (Pfloat _, Pfloat _)             => EQUAL
        |  (Pstring s1, Pstring s2)       => EQUAL
        |  (Pwhite s1, Pwhite s2)         => EQUAL
        |  (Pgroup g1, Pgroup g2)         => compToken(#1(#left g1), (#1(#left g2)))
        |  (Other c1, Other c2)           => Char.compare (c1, c2)
        |  (Pempty, Pempty)               => EQUAL
        |  (Error, Error)                 => EQUAL
        |  (Ptime _, _)                   => LESS
        |  (Pdate _, Ptime _)             => GREATER
        |  (Pdate _, _)                   => LESS
        |  (Pip _, Ptime _)               => GREATER
        |  (Pip _, Pdate _)               => GREATER
        |  (Pip _, _)                     => LESS
        |  (Phostname _, Ptime _)         => GREATER
        |  (Phostname _, Pdate _)         => GREATER
        |  (Phostname _, Pip _)           => GREATER
        |  (Phostname _, _)               => LESS 
        |  (Purl _, Ptime _)              => GREATER
        |  (Purl _, Pdate _)              => GREATER
        |  (Purl _, Pip _)                => GREATER
        |  (Purl _, Phostname _)          => GREATER
        |  (Purl _, _)            	  => LESS
        |  (Ppath _, Ptime _)             => GREATER
        |  (Ppath _, Pdate _)             => GREATER
        |  (Ppath _, Pip _)               => GREATER
        |  (Ppath _, Phostname _)         => GREATER
        |  (Ppath _, Purl _)              => GREATER
        |  (Ppath _, _)               	  => LESS
        |  (PbXML _, Ptime _ )            => GREATER
        |  (PbXML _, Pdate _ )            => GREATER
        |  (PbXML _, Pip _)               => GREATER
        |  (PbXML _, Phostname _)         => GREATER
        |  (PbXML _, Ppath _)             => GREATER
        |  (PbXML _, Purl _)              => GREATER
        |  (PbXML _,  _)                  => LESS
        |  (PeXML _, Ptime _ )            => GREATER
        |  (PeXML _, Pdate _ )            => GREATER
        |  (PeXML _, Pip _)               => GREATER
        |  (PeXML _, Phostname _)         => GREATER
        |  (PeXML _, Ppath _)             => GREATER
        |  (PeXML _, Purl _)              => GREATER
        |  (PeXML _, PbXML _)             => GREATER
        |  (PeXML _,  _)                  => LESS
        |  (Pemail _, Ptime _ )            => GREATER
        |  (Pemail _, Pdate _ )            => GREATER
        |  (Pemail _, Pip _)               => GREATER
        |  (Pemail _, Phostname _)         => GREATER
        |  (Pemail _, Ppath _)             => GREATER
        |  (Pemail _, Purl _)              => GREATER
        |  (Pemail _, PbXML _)             => GREATER
        |  (Pemail _, PeXML _)             => GREATER
        |  (Pemail _,  _)                  => LESS
        |  (Pmac _, Ptime _ )            => GREATER
        |  (Pmac _, Pdate _ )            => GREATER
        |  (Pmac _, Pip _)               => GREATER
        |  (Pmac _, Phostname _)         => GREATER
        |  (Pmac _, Ppath _)             => GREATER
        |  (Pmac _, Purl _)              => GREATER
        |  (Pmac _, PbXML _)             => GREATER
        |  (Pmac _, PeXML _)             => GREATER
        |  (Pmac _, Pemail _)            => GREATER
        |  (Pmac _,  _)                  => LESS
        |  (Pfloat _, Ptime _)            => GREATER
        |  (Pfloat _, Pdate _)            => GREATER
        |  (Pfloat _, Pip _)              => GREATER
        |  (Pfloat _, Phostname _)        => GREATER
        |  (Pfloat _, Ppath _)            => GREATER
        |  (Pfloat _, Purl _)             => GREATER
        |  (Pfloat _, PbXML _)            => GREATER
        |  (Pfloat _, PeXML _)            => GREATER
        |  (Pfloat _, Pemail _)            => GREATER
        |  (Pfloat _, Pmac _)            => GREATER
        |  (Pfloat _, _)                  => LESS
        |  (Pint _, Ptime _)              => GREATER
        |  (Pint _, Pdate _)              => GREATER
        |  (Pint _, Pip _)                => GREATER
        |  (Pint _, Phostname _)          => GREATER
        |  (Pint _, Ppath _)              => GREATER
        |  (Pint _, Purl _)               => GREATER
        |  (Pint _, PbXML _)              => GREATER
        |  (Pint _, PeXML _)              => GREATER
        |  (Pint _, Pemail _)             => GREATER
        |  (Pint _, Pmac _)               => GREATER
        |  (Pint _, Pfloat _)             => GREATER
        |  (Pint _, _)                    => LESS
        |  (Pstring _, Ptime _)           => GREATER
        |  (Pstring _, Pdate _)           => GREATER
        |  (Pstring _, Pip _)             => GREATER
        |  (Pstring _, Phostname _)       => GREATER
        |  (Pstring _, Ppath _)           => GREATER
        |  (Pstring _, Purl _)            => GREATER
        |  (Pstring _, PbXML _)           => GREATER
        |  (Pstring _, PeXML _)           => GREATER
        |  (Pstring _, Pemail _)           => GREATER
        |  (Pstring _, Pmac _)           => GREATER
        |  (Pstring _, Pint _)            => GREATER
        |  (Pstring _, Pfloat _)          => GREATER
        |  (Pstring _,  _)                => LESS
        |  (Pgroup _, Ptime _)            => GREATER
        |  (Pgroup _, Pdate _)            => GREATER
        |  (Pgroup _, Pip _)              => GREATER
        |  (Pgroup _, Phostname _)        => GREATER
        |  (Pgroup _, Ppath _)            => GREATER
        |  (Pgroup _, Purl _)             => GREATER
        |  (Pgroup _, PbXML _)            => GREATER
        |  (Pgroup _, PeXML _)            => GREATER
        |  (Pgroup _, Pemail _)            => GREATER
        |  (Pgroup _, Pmac _)            => GREATER
        |  (Pgroup _, Pint _)             => GREATER
        |  (Pgroup _, Pfloat _)           => GREATER
        |  (Pgroup _, Pstring _)          => GREATER
        |  (Pgroup _,  _)                 => LESS
        |  (Pwhite _, Ptime _)            => GREATER
        |  (Pwhite _, Pdate _)            => GREATER
        |  (Pwhite _, Pip _)              => GREATER
        |  (Pwhite _, Phostname _)        => GREATER
        |  (Pwhite _, Ppath _)            => GREATER
        |  (Pwhite _, Purl _)             => GREATER
        |  (Pwhite _, PbXML _)            => GREATER
        |  (Pwhite _, PeXML _)            => GREATER
        |  (Pwhite _, Pemail _)            => GREATER
        |  (Pwhite _, Pmac _)            => GREATER
        |  (Pwhite _, Pint _)             => GREATER
        |  (Pwhite _, Pfloat _)           => GREATER
        |  (Pwhite _, Pstring _)          => GREATER
        |  (Pwhite _, Pgroup _)           => GREATER
        |  (Pwhite _, _)                  => LESS
        |  (Other _, Ptime _)             => GREATER
        |  (Other _, Pdate _)             => GREATER
        |  (Other _, Pip _)               => GREATER
        |  (Other _, Phostname _)         => GREATER
        |  (Other _, Ppath _)             => GREATER
        |  (Other _, Purl _)              => GREATER
        |  (Other _, PbXML _)             => GREATER
        |  (Other _, PeXML _)             => GREATER
        |  (Other _, Pemail _)             => GREATER
        |  (Other _, Pmac _)             => GREATER
        |  (Other _, Pint _)              => GREATER
        |  (Other _, Pfloat _)            => GREATER
        |  (Other _, Pstring _)           => GREATER
        |  (Other _, Pgroup _)            => GREATER
        |  (Other _, Pwhite _)            => GREATER
        |  (Other _, _)                   => LESS
        |  (Pempty, Ptime _)              => GREATER
        |  (Pempty, Pdate _)              => GREATER
        |  (Pempty, Pip _)                => GREATER
        |  (Pempty, Phostname _)          => GREATER
        |  (Pempty, Ppath _)              => GREATER
        |  (Pempty, Purl _)               => GREATER
        |  (Pempty, PbXML _)              => GREATER
        |  (Pempty, PeXML _)              => GREATER
        |  (Pempty, Pemail _)              => GREATER
        |  (Pempty, Pmac _)              => GREATER
        |  (Pempty, Pint _)               => GREATER
        |  (Pempty, Pfloat _)             => GREATER
        |  (Pempty, Pstring _)            => GREATER
        |  (Pempty, Pgroup _)             => GREATER
        |  (Pempty, Pwhite _)             => GREATER
        |  (Pempty, Other _)              => GREATER
        |  (Pempty, _)                    => LESS
        |  (Error, _)                     => GREATER

    fun eqToken(t1,t2) = case compToken(t1,t2) of EQUAL => true | _ => false

    (* Establish an order on LTokens based on the order on Tokens *)
    fun compLToken (ltok1:LToken, ltok2:LToken):order =
        let val (t1,l1) = ltok1
            val (t2,l2) = ltok2
        in ( case compToken (t1, t2) of
                    LESS    => LESS
                  | GREATER => GREATER
                  | EQUAL   => compLocation (l1, l2)
           )
        end

    fun repeatLToken ( cnt : int ) ( tok : Token ) ( b : int ) ( e : int ) : LToken list =
    let fun repeatLToken' ( n : int ) ( cnt : int ) ( tok : Token ) ( b : int ) ( e : int ) : LToken list =
           if n = cnt
           then []
           else ( tok, mkLoc b e 1 n ) :: repeatLToken' (n+1) cnt tok b e
    in repeatLToken' 0 cnt tok b e
    end

    (* Mapping having LTokens as a domain *)
    structure LTokenMap = RedBlackMapFn ( struct type ord_key = LToken
                                                 val  compare = compLToken
                                          end
                                        )
    (* Mapping from LTokens to frequency *)
    type LTokenFreq = int LTokenMap.map
    val emptyLTokenFreq : LTokenFreq = LTokenMap.empty
    fun addLTokenFreq (t:LToken, f:LTokenFreq):LTokenFreq =
        ( case LTokenMap.find (f,t) of
               NONE   => f
             | SOME n => LTokenMap.insert (f,t,n+1)
        )

    (* Count occurences of a character in a string *)
    fun countCh ( c : char ) ( s : string ) : int =
        let fun countCh' ( c : char ) ( cl : char list ) : int =
                ( case cl of
                       []      => 0
                     | (x::xs) => if ( x = c )
                                  then 1 + countCh' c xs
                                  else countCh' c xs
                )
        in countCh' c ( explode s )
        end

    fun tokenOf (t:LToken):Token = #1 t
    fun tokenLength (t:Token):int =
        ( case t of
               PbXML (s1, s2) => size s1 + size s2
             | PeXML (s1, s2) => size s1 + size s2
             | Ptime s        => size s
             | Pdate s        => size s
             | Ppath s        =>
                 let val nsep       : int    = countCh #"/" s
                     fun isSep ( x : char ) : bool = x = #"/"
                     val components : string list =
                       map Substring.string
                           ( Substring.fields isSep ( Substring.full s ) )
                 in if nsep = 0 then size s else size s - nsep
                 end
               (* TODO: URLs and emails are too hard to parse right now, 
		also the issue of case sensitivity *)
             | Purl s         => size s
             | Pemail s         => size s
             | Pmac s         => 12  (*6 hex numbers, take away the delimiters *)
             | Pip s          => countCh #"." s + 1
             | Phostname s    =>
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
             | Pint (n, s)    => size s (*ignore the length of the s as it's aux info*)
             | Pfloat (i,f)   => size (i) + size (f) 
             | Pstring s      => size s
             | Pgroup grp     => 0
             | Pwhite s       => size s
             | Other c        => 1
             | Pempty         => 0
             | Error          => 0
        )

    fun lTokenLength (t:LToken):int = tokenLength (tokenOf t)

    (* Calculate the maximum token length
     * This number is probably garbage unless all the tokens in the
       list are of the same type.
     *)
    fun maxTokenLength ( ts : LToken list ) : int =
        foldl (fn (t:LToken,x:int) => Int.max (x, lTokenLength t)) 0 ts

    fun sumTokenLength ( ts : LToken list ) : LargeInt.int =
        foldl ( fn ( t : LToken, x : LargeInt.int ) =>
                x + Int.toLarge ( lTokenLength t ) ) 0 ts

    fun avgTokenLength ( ts : LToken list ) : real =
        ( Real.fromLargeInt ( sumTokenLength ts ) ) /
        ( Real.fromInt ( length ts ) ) 

    (* A record is a special kind of list of tokens *)
    type Record = Token list

    fun tokenTyToName ( t : Token ) : string = 
	case t 
        of Ptime i     => "time"
	|  Pdate i     => "date"
	|  Pip i       => "ip"
	|  Phostname i => "host"
	|  Ppath i     => "path"
	|  Purl i      => "url"
	|  Pemail i      => "email"
	|  Pmac i      => "mac"
        |  PbXML (f,s) => "bXML"
        |  PeXML (f,s) => "eXML"
(*
	|  Pint (i, s)    => " Pint("^(LargeInt.toString i)^")"
        |  Pstring s => " Pstring("^s^")"
        |  Pwhite s  => " Pwhite("^s^")" 
*)
	|  Pint _    => "int"              (*" Pint("^(LargeInt.toString i)^")"*)
	|  Pfloat _    => "float"           
        |  Pstring s => "string"            (*" Pstring("^s^")"*)
        |  Pwhite s  => "white"             (*" Pwhite("^s^")"*) 
        |  Pgroup {left, body, right} => "group"
        |  Other c   => "char"
        |  Pempty    => "empty"
        |  Error     => "error"

end
