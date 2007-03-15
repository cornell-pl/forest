structure Tokens = struct
    open Complexity

    type location = { lineNo: int, beginloc: int, endloc:int }

    (* Establish an order on locations *)
    fun compLocation (l1:location, l2:location):order =
        let val {lineNo = ln1, beginloc = b1, endloc = e1} = l1
            val {lineNo = ln2, beginloc = b2, endloc = e2} = l2
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
    val numStringChars : LargeInt.int  = 26 + 26 + 10 + 1 + 1
    val numWhiteChars  : LargeInt.int  = 2 (* Space and tab *)
    val numXMLChars    : LargeInt.int  = 26 + 26
    val numDigits      : LargeInt.int  = 10

    (* Some analysis of the structure of tokens from tokens.lex: *)
    fun powerL ( x : LargeInt.int ) ( y : int ) : LargeInt.int =
        if ( y = 0 ) then 1 else x * powerL x ( y - 1 )

    val numTriplet       : LargeInt.int = 1000
    val numIPTriplet     : LargeInt.int = 256
    val numIP            : LargeInt.int = powerL numIPTriplet 4
    val numDoublet       : LargeInt.int = 100
    val numTimeZone      : LargeInt.int = 2 * 2 * 10
    val numAMPM          : LargeInt.int = 4
    val numTime          : LargeInt.int = 2 + 2 + 2 + ( numDoublet * numDoublet *
                                          numDoublet * numAMPM * numTimeZone )
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
    val numToken         : LargeInt.int = 15 (* Number of cases in datatype Token *)

    (* Raw token format, pass one over the data *)
    datatype Token = PbXML       of string * string |
                     PeXML       of string * string |
	             Ptime       of string |
	             Pdate       of string |
	             Ppath       of string |
	             Purl        of string |
		     Pip         of string |
		     Phostname   of string |
                     Pint        of LargeInt.int |
		     Pstring     of string |
                     Pgroup      of {left : LToken, body : LToken list, right : LToken} |
	             Pwhite      of string |
		     Other       of char |
		     Pempty |
		     Error
    withtype LToken = Token * location

    (*    Establish an order on Token using the following constraints:
          Ptime < Pdate < Pip< Phostname < Ppath < Purl < PbXML < PeXML < Pint < Pstring < Pgroup <
          Pwhite < Other < Pempty < Error
     *)
    fun compToken (t1:Token, t2:Token):order = 
	case (t1,t2) of
           (Ptime i1, Ptime i2)           => EQUAL
        |  (Pdate i1, Pdate i2)           => EQUAL
        |  (Ppath i1, Ppath i2)           => EQUAL
        |  (Purl i1, Purl i2)             => EQUAL
        |  (Pip i1, Pip i2)               => EQUAL
        |  (Phostname i1, Phostname i2)               => EQUAL
        |  (PbXML (f1,s1), PbXML (f2,s2)) => String.compare(f1,f2)
        |  (PeXML (f1,s1), PeXML (f2,s2)) => String.compare(f1,f2)
        |  (Pint i1, Pint i2)             => EQUAL
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
        |  (Phostname _, Ptime _)               => GREATER
        |  (Phostname _, Pdate _)               => GREATER
        |  (Phostname _, Pip _)               => GREATER
        |  (Phostname _, _)               => LESS 
        |  (Ppath _, Ptime _)               => GREATER
        |  (Ppath _, Pdate _)               => GREATER
        |  (Ppath _, Pip _)               => GREATER
        |  (Ppath _, Phostname _)               => GREATER
        |  (Ppath _, _)               	  => LESS
        |  (Purl _, Ptime _)               => GREATER
        |  (Purl _, Pdate _)               => GREATER
        |  (Purl _, Pip _)               => GREATER
        |  (Purl _, Phostname _)               => GREATER
        |  (Purl _, Ppath _)            	  => GREATER
        |  (Purl _, _)            	  => LESS
        |  (PbXML _, Ptime _ )            => GREATER
        |  (PbXML _, Pdate _ )            => GREATER
        |  (PbXML _, Pip _)               => GREATER
        |  (PbXML _, Phostname _)               => GREATER
        |  (PbXML _, Ppath _)               => GREATER
        |  (PbXML _, Purl _)               => GREATER
        |  (PbXML _,  _)                  => LESS
        |  (PeXML _, Ptime _ )            => GREATER
        |  (PeXML _, Pdate _ )            => GREATER
        |  (PeXML _, Pip _)               => GREATER
        |  (PeXML _, Phostname _)               => GREATER
        |  (PeXML _, Ppath _)               => GREATER
        |  (PeXML _, Purl _)               => GREATER
        |  (PeXML _, PbXML _)             => GREATER
        |  (PeXML _,  _)                  => LESS
        |  (Pint _, Ptime _)              => GREATER
        |  (Pint _, Pdate _)              => GREATER
        |  (Pint _, Pip _)                => GREATER
        |  (Pint _, Phostname _)                => GREATER
        |  (Pint _, Ppath _)                => GREATER
        |  (Pint _, Purl _)                => GREATER
        |  (Pint _, PbXML _)              => GREATER
        |  (Pint _, PeXML _)              => GREATER
        |  (Pint _, _)                    => LESS
        |  (Pstring _, Ptime _)           => GREATER
        |  (Pstring _, Pdate _)           => GREATER
        |  (Pstring _, Pip _)             => GREATER
        |  (Pstring _, Phostname _)             => GREATER
        |  (Pstring _, Ppath _)             => GREATER
        |  (Pstring _, Purl _)             => GREATER
        |  (Pstring _, Pint _)            => GREATER
        |  (Pstring _, PbXML _)           => GREATER
        |  (Pstring _, PeXML _)           => GREATER
        |  (Pstring _,  _)                => LESS
        |  (Pgroup _, Ptime _)            => GREATER
        |  (Pgroup _, Pdate _)            => GREATER
        |  (Pgroup _, Pip _)              => GREATER
        |  (Pgroup _, Phostname _)              => GREATER
        |  (Pgroup _, Ppath _)              => GREATER
        |  (Pgroup _, Purl _)              => GREATER
        |  (Pgroup _, Pint _)             => GREATER
        |  (Pgroup _, Pstring _)          => GREATER
        |  (Pgroup _, PbXML _)            => GREATER
        |  (Pgroup _, PeXML _)            => GREATER
        |  (Pgroup _,  _)                 => LESS
        |  (Pwhite _, Ptime _)            => GREATER
        |  (Pwhite _, Pdate _)            => GREATER
        |  (Pwhite _, Pip _)              => GREATER
        |  (Pwhite _, Phostname _)              => GREATER
        |  (Pwhite _, Ppath _)              => GREATER
        |  (Pwhite _, Purl _)              => GREATER
        |  (Pwhite _, Pint _)             => GREATER
        |  (Pwhite _, Pstring _)          => GREATER
        |  (Pwhite _, Pgroup _)           => GREATER
        |  (Pwhite _, PbXML _)            => GREATER
        |  (Pwhite _, PeXML _)            => GREATER
        |  (Pwhite _, _)                  => LESS
        |  (Other _, Ptime _)             => GREATER
        |  (Other _, Pdate _)             => GREATER
        |  (Other _, Pip _)               => GREATER
        |  (Other _, Phostname _)               => GREATER
        |  (Other _, Ppath _)               => GREATER
        |  (Other _, Purl _)               => GREATER
        |  (Other _, Pint _)              => GREATER
        |  (Other _, Pstring _)           => GREATER
        |  (Other _, Pgroup _)            => GREATER
        |  (Other _, Pwhite _)            => GREATER
        |  (Other _, PbXML _)             => GREATER
        |  (Other _, PeXML _)             => GREATER
        |  (Other _, _)                   => LESS
        |  (Pempty, Ptime _)              => GREATER
        |  (Pempty, Pdate _)              => GREATER
        |  (Pempty, Pip _)                => GREATER
        |  (Pempty, Phostname _)                => GREATER
        |  (Pempty, Ppath _)                => GREATER
        |  (Pempty, Purl _)                => GREATER
        |  (Pempty, Pint _)               => GREATER
        |  (Pempty, Pstring _)            => GREATER
        |  (Pempty, Pgroup _)             => GREATER
        |  (Pempty, Pwhite _)             => GREATER
        |  (Pempty, Other _)              => GREATER
        |  (Pempty, PbXML _)              => GREATER
        |  (Pempty, PeXML _)              => GREATER
        |  (Pempty, _)                    => LESS
        |  (Error, _)                     => GREATER

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
             | Ppath s        => size s
             | Purl s         => size s
             | Pip s          => size s
               (* For now, we assume the last component is a domain name.
                  we should check this later *)
             | Phostname s    =>
                 let val ndot       : int            = countCh #"." s
                     fun isDot ( x : char ) : bool = x = #"."
                     val components : substring list =
                           Substring.fields isDot ( Substring.full s )
                 in if ndot = 0
                    then size s
                    else size s - ndot - ( size ( Substring.string ( List.last components ) ) )
                 end
             | Pint n         => size (LargeInt.toString n)
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

    (* A record is a special kind of list of tokens *)
    type Record = Token list

end
