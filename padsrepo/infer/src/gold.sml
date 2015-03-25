structure Gold = struct
    open AI
    open BOOT
    open CRASHREPORTER
    open CRASHREPORTER_MOD
    open LS
    open DIBBLER
    open QUARTERLY
    open RAILROAD
    open RPMPKGS
    open Trans
    open Yumtxt 
    open ASL
    open PAGE_LOG
    open WINDOWSERVER_LOG 
    open MER
    open NETSTAT
    open IRV
    open TEST_INC 
    open Populate
    open ScrollKeeper
    open Model
    open Types

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
        [ ( "1967Transactions.short", trans )
        , ( "ai.3000", ai )
        , ( "boot.log", boot_entry )
        , ( "crashreporter.log", crashreport )
        , ( "crashreporter.log.modified", crashreport_mod )
        , ( "dibbler.1000", dibbler_t )
        , ( "ls-l.txt", ls_l )
        , ( "quarterlypersonalincome", quarterly_t )
        , ( "railroad.txt", railroad )
        , ( "rpmpkgs", rpmpkgs )
        , ( "yum.txt", yum )
        , ( "asl.log", asl)
        , ( "page_log", page_log)
        , ( "windowserver_last.log", windowserver_log)
        , ( "MER_T01_01.csv", mer)
        , ( "netstat-an", netstat)
        , ( "scrollkeeper.log", scrollkeeper)
        , ( "irvpiv1.tail.sel", irv)
        , ( "test-inc.txt", test_inc)
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
    fun goldenReport ( descname : string ) : string =
        if hasGold descname
        then let val goldenTy : Ty  = getGolden descname
                 val measured : Ty = populateDataFile ( "data/" ^ descname ) goldenTy
                 val ()             = print "\n"
		(*
		 val (_, pads) = TyToPADSFile measured "vanilla.p"
		 val () = print (pads ^ "\n")
		*)
                 val nbits : int    = OS.FileSys.fileSize ( "data/" ^ descname ) * 8
                 val goldtystr      = TyToString ( measured )
             in "Golden complexity =\n" ^
                showTyCompNormalized nbits ( getComps measured ) ^
                goldtystr ^ "\n" 
             end
        else "NO GOLDEN FILE FOR: " ^ descname ^ "\n"


end