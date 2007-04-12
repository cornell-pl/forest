structure BOOT = struct
    open Model
    val aux: AuxInfo = {coverage = 35, label = NONE, tycomp = zeroComps }
    val loc : location = {lineNo = 0, beginloc = 0, endloc = 0, recNo = 0}
    val date : Ty = Base(aux, [(Pdate "15/Oct/1997", loc)])
    val time : Ty = Base(aux, [(Ptime "18:46:51 -0700", loc)])
    val space : Ty = RefinedBase(aux, StringConst " ", [(Pstring " ", loc)])
    val server: Ty = RefinedBase(aux, StringConst "srv7", [(Pstring "srv7", loc)])
    val daemon : Ty = Base(aux, [(Pstring ("acpid"), loc)])
    val msg: Ty = Base(aux, [(Pstring ("some message"), loc)])
    val colonsp: Ty = RefinedBase(aux, StringConst ": ", [(Pstring ": ", loc)]) 
    val lastmsg: Ty = RefinedBase(aux, StringConst "last message repeated ", [(Pstring "last message repeated ", loc)]) 
    val inttimes: Ty = Base(aux, [(Pint(32, "32"), loc)])
    val times: Ty = RefinedBase(aux, StringConst " times", [(Pstring " times", loc)]) 
    val daemon_msg : Ty = Pstruct(aux, [daemon, colonsp, Poption(aux, msg)])
    val sys_msg : Ty = Pstruct(aux, [lastmsg, inttimes, times])
    val message: Ty = Punion (aux, [daemon_msg, sys_msg])
    val boot_entry: Ty = Pstruct(aux, [date, space, time, space, server, space, message])
end	
