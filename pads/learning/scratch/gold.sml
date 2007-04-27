structure Gold = struct
    open AI
    open CRASHREPORTER
    open LS
    open QUARTERLY
    open Yumtxt

    exception Bust

    fun compStr ( s1 : string, s2 : string ) : order =
        if s1 < s2
        then LESS
        else if s1 > s2
             then GREATER
             else EQUAL
    
    structure GoldenMapF = RedBlackMapFn ( struct type ord_key = string
                                                       val compare = compStr
                                           end
                                         )

    type GoldenMap = Ty GoldenMapF.map

    fun buildGold ( kvs : ( string * Ty ) list ) : GoldenMap =
    let fun f ( kv : ( string * Ty ), m : GoldenMap ) : GoldenMap =
        let val ( k, v ) = kv
        in GoldenMapF.insert ( m, k, v )
        end
    in foldl f GoldenMapF.empty kvs
    end 

    val goldens : GoldenMap = buildGold
        [ ( "ai.3000", ai )
        , ( "crashreporter.log", crashreport )
        , ( "lsof", entry_t )
        , ( "quarterlypersonalincome", quarterly_t )
        , ( "yum.txt", yum )
        ]

    (* Determine if there is a useable golden data file *)
    fun hasGold ( s : string ) : bool = GoldenMapF.inDomain ( goldens, s )
    (* Find the golden data file *)
    fun getGold ( s : string ) : Ty option  = GoldenMapF.find ( goldens, s )
    (* Find the golden data file, or die *)
    fun getGolden ( s : string ) : Ty =
      ( case ( getGold s ) of
               NONE   => raise Bust
             | SOME t => t
     )

end
