structure NETSTAT = struct
    open Model
    val aux: AuxInfo = {coverage = 3000, label = NONE, tycomp = zeroComps }
    val loc : location = {lineNo = 0, beginloc = 0, endloc = 0, recNo = 0}

    val sp: Ty = Base(aux, [(Pwhite " ", loc)])
    val dot: Ty = RefinedBase(aux, StringConst ".", [(Pstring ".", loc)])
    val star: Ty = RefinedBase(aux, StringConst "*", [(Pstring "*", loc)])
    val stardot: Ty = RefinedBase(aux, StringConst "*.", [(Pstring "*", loc)])
    val num: Ty = Base(aux, [(Pint (37, "37"), loc)])
    val title1: Ty = RefinedBase(aux, StringConst "Active Internet connections (including servers)", 
			[(Pstring "Active Internet connections (including servers)", loc)])
    val title2: Ty = RefinedBase(aux, StringConst "Active LOCAL (UNIX) domain sockets", 
			[(Pstring "Active LOCAL (UNIX) domain sockets", loc)])
    
    val header1: Ty = RefinedBase(aux, StringConst "Proto Recv-Q Send-Q  Local Address          Foreign Address        (state)", [(Pstring "Proto Recv-Q Send-Q  Local Address          Foreign Address        (state)", loc)])
    val header2: Ty = RefinedBase(aux, StringConst "Address  Type   Recv-Q Send-Q    Inode     Conn     Refs  Nextref Addr", [(Pstring "Address  Type   Recv-Q Send-Q    Inode     Conn     Refs  Nextref Addr", loc)])
    val proto : Ty = RefinedBase(aux, Enum [StringConst "tcp4", StringConst "tcp6", 
			StringConst "udp4", StringConst "udp6", StringConst "icm6"], [])
    val recv_q: Ty = Base(aux, [(Pint (37, "37"), loc)])
    val send_q: Ty = Base(aux, [(Pint (0, "0"), loc)])
    val ip : Ty = Pstruct(aux, [Base(aux, [(Pip "135.207.30.101", loc)]), dot, 
					Base(aux, [(Pint(143, "143"), loc)])])
    val state: Ty = RefinedBase(aux, Enum[StringConst "CLOSE_WAIT", StringConst "ESTABLISHED", 
		StringConst "LISTEN", StringConst "CLOSED"], [])
    val stateop: Ty = Poption(aux, state)
    val otherip : Ty = Pstruct(aux, [stardot, Punion(aux, [num, star])])
    val ip_field: Ty = Punion(aux, [ip, otherip])
    val entry1: Ty = Pstruct(aux, [proto, sp, recv_q, sp, send_q, sp, ip_field, sp, ip_field, sp, stateop])

    val hex: Ty = RefinedBase(aux, StringME "/[0-9a-f]+/", [])
    val conntype: Ty = RefinedBase(aux, Enum [StringConst "stream", StringConst "dgram"], [])
    val inode: Ty = RefinedBase(aux, StringME "/[0-9a-f]+/", [])
    val conn: Ty = RefinedBase(aux, StringME "/[0-9a-f]+/", [])
    val reff: Ty = RefinedBase(aux, StringME "/[0-9a-f]+/", [])
    val nextref: Ty = RefinedBase(aux, StringME "/[0-9a-f]+/", [])
    val addr : Ty = Poption(aux, Base(aux, [(Ppath "/tmp/.X11-unix/X0", loc)]))
    val entry2: Ty = Pstruct(aux, [sp, hex, sp, conntype, sp, recv_q, sp, send_q, sp, 
			inode, sp, conn, sp, reff, sp, nextref, Poption(aux, sp), addr])

    val netstat : Ty = Punion(aux, [title1, header1, entry1, title2, header2, entry2])
end	
