structure ASL = struct
    open Model
    val aux: AuxInfo = {coverage = 1, label = NONE, tycomp = zeroComps }
    val loc : location = {lineNo = 0, beginloc = 0, endloc = 0, recNo = 0}
    val space : Ty = RefinedBase(aux, StringConst " ", [(Pstring " ", loc)])
    val lb: Ty = RefinedBase(aux, StringConst "[", [(Pstring "[", loc)])
    val rb: Ty = RefinedBase(aux, StringConst "]", [(Pstring "]", loc)])

    val date : Ty = Base(aux, [(Pdate "2006.11.21", loc)])
    val time : Ty = Base(aux, [(Ptime "18:46:51", loc)])
    val begintime: Ty = RefinedBase(aux, StringConst "Time", [(Pstring "Time", loc)])
    val utc: Ty = RefinedBase(aux, StringConst "UTC", [(Pstring "UTC", loc)])
    val mydate: Ty = Pstruct(aux, [lb, begintime, space, date, space, time, space, utc, rb])

    val beginfac: Ty = RefinedBase(aux, StringConst "Facility", [(Pstring "Facility ", loc)])
    val user: Ty = Base(aux, [(Pstring "user", loc)])
    val fac: Ty = Pstruct(aux, [lb, beginfac, space, user, rb])

    val senderstr: Ty = RefinedBase(aux, StringConst "Sender", [(Pstring "Sender", loc)])
    val sender: Ty = RefinedBase(aux, StringME "/[^\\]]+/", [(Pstring "xxx", loc)])
    val mysender: Ty = Pstruct(aux, [lb, senderstr, space, sender, rb]) 

    val pidstr: Ty = RefinedBase(aux, StringConst "PID", [(Pstring "PID", loc)])
    val pid: Ty = Base(aux, [(Pint (~1, "-1"), loc)])
    val mypid: Ty = Pstruct(aux, [lb, pidstr, space, pid, rb]) 

    val msgstr: Ty = RefinedBase(aux, StringConst "Message", [(Pstring "Message", loc)])
    
    val msg: Ty = RefinedBase(aux, StringME "/[^\\]]+/", [(Pstring "xxx", loc)])
    val msg1: Ty = RefinedBase(aux, StringME "/[^\\\\]+/", [(Pstring "xxx", loc)])
    val slashbr: Ty = RefinedBase(aux, StringConst "\\]", [(Pstring "xxx", loc)])
    val strangemsg:Ty = Pstruct(aux, [msg1, slashbr, msg])
    val genmsg: Ty = Punion(aux, [strangemsg, msg])
    val mymsg: Ty = Pstruct(aux, [lb, msgstr, space, genmsg, rb]) 

    val levelstr: Ty = RefinedBase(aux, StringConst "Level", [(Pstring "Level", loc)])
    val level: Ty = Base(aux, [(Pint (~1, "-1"), loc)])
    val mylevel: Ty = Pstruct(aux, [lb, levelstr, space, level, rb]) 

    val uidstr: Ty = RefinedBase(aux, StringConst "UID", [(Pstring "UID", loc)])
    val uid: Ty = Base(aux, [(Pint (~1, "-1"), loc)])
    val myuid: Ty = Pstruct(aux, [lb, uidstr, space, uid, rb]) 

    val gidstr: Ty = RefinedBase(aux, StringConst "GID", [(Pstring "GID", loc)])
    val gid: Ty = Base(aux, [(Pint (~1, "-1"), loc)])
    val mygid: Ty = Pstruct(aux, [lb, gidstr, space, gid, rb]) 

    val hoststr: Ty = RefinedBase(aux, StringConst "Host", [(Pstring "Host", loc)])
    val host: Ty = Base(aux, [(Pstring "Babylon", loc)])
    val myhost: Ty = Pstruct(aux, [lb, hoststr, space, host, rb]) 

    val asl: Ty = Pstruct(aux, [mydate, space, fac, space, mysender, space, mypid, space,
				mymsg, space, mylevel, space, myuid, space, mygid, space, myhost])
end	
