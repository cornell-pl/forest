structure CRASHREPORTER_MOD = struct
    open Model
    val aux: AuxInfo = {coverage = 35, label = NONE, tycomp = zeroComps }
    val loc : location = {lineNo = 0, beginloc = 0, endloc = 0, recNo = 0}
    val date : Ty = Base(aux, [(Pdate "15/Oct/1997", loc)])
    val time : Ty = Base(aux, [(Ptime "18:46:51 -0700", loc)])
    val year: Ty = Base(aux, [(Pint (2004, "2004"), loc)])
    val space : Ty = RefinedBase(aux, StringConst " ", [(Pstring " ", loc)])
    val dash: Ty = RefinedBase(aux, StringConst "-", [(Pstring "-", loc)])
    val dateop: Ty = Punion(aux, [Pstruct(aux, [date, space, time, space, year]),
				dash])
    val swLabel: Id = Atom.atom("sw")
    val swAux: AuxInfo = {coverage = 35, label = SOME swLabel, tycomp = zeroComps }
    val crashdump: Ty = RefinedBase(swAux, Enum[StringConst "crashdump", StringConst "crashreporterd"], 
				[(Pstring "crashdump", loc)])
    val leftbracket: Ty = RefinedBase(aux, StringConst "[", [(Pstring "[", loc)])
    val rightbracket: Ty = RefinedBase(aux, StringConst "]", [(Pstring "]", loc)])
    val dumpid: Ty = Base(aux, [(Pint (2004, "2004"), loc)])
    val colonsp: Ty = RefinedBase(aux, StringConst ": ", [(Pstring ": ", loc)]) 
    val started: Ty = RefinedBase(aux, StringConst "crashdump started", [(Pstring ": ", loc)]) 
    val start: Ty = RefinedBase(aux, StringConst "Started writing crash report to: ", 
			[(Pstring "Started writing crash report to: ", loc)]) 
    val finish: Ty = RefinedBase(aux, StringConst "Finished writing crash report to: ", 
			[(Pstring "Finished writing crash report to: ", loc)]) 
    val path: Ty = Base(aux, [(Ppath "/Users/kfisher/Library/Logs/CrashReporter/Preview.crash.log", loc)])
    val filename: Ty = RefinedBase(aux, StringME "/[a-zA-Z0-9. ]*/", [(Pstring " ayx", loc)])
    val filepath: Ty = Pstruct(aux, [path, Poption(aux, filename)])
    val unable: Ty = RefinedBase(aux, StringConst "Unable to determine task_t for pid: ", 
			[(Pstring "Unable to determine task_t for pid: ", loc)]) 
    val pid: Ty = Base(aux, [(Pint(95, "95"), loc)])
    val name: Ty = RefinedBase(aux, StringConst " name: Exited process", [(Pstring "name: Exited process", loc)])
    val function: Ty = RefinedBase(aux, StringME "/[^ ]*/", [(Pstring ("mach_msg()"), loc)])
    val reply: Ty = RefinedBase(aux, StringConst " reply failed: ", [(Pstring "srv7", loc)])
    val failedmsg: Ty = RefinedBase(aux, StringME "/.*/", [(Pstring ("(ipc/send) invalid destination port"), loc)])
    val failtorelaunch: Ty = RefinedBase(aux, StringConst "Failed to re-launch ", 
			[(Pstring "Failed to re-launch ", loc)])
    val spdashsp: Ty = RefinedBase(aux, StringConst " - ", [(Pstring " - ", loc)])
    val errorkind: Ty = Base (aux, [(Pstring "CGSError", loc)])
    val errorcode: Ty = Base(aux, [(Pint (1025, "1025"), loc)])
    val shriek: Ty = RefinedBase(aux, StringConst "!", [(Other #"!", loc)])
    val errormsg: Ty = Pstruct(aux, [errorkind, colonsp, errorcode, shriek])
    val dumpreport: Ty = Punion(aux, [started,
				Pstruct(aux, [start, filepath]),
				Pstruct(aux, [finish, filepath]),
				Pstruct(aux, [unable, pid, name]),
				Pstruct(aux, [failtorelaunch, path, spdashsp, errormsg])
				])
    val reporterreport: Ty = Pstruct(aux, [function, reply, failedmsg])
    val report: Ty = Switch(aux, swLabel, [(StringConst "crashdump", dumpreport), 
					   (StringConst "crashreporterd", reporterreport)])
    val crashreport_mod: Ty = Pstruct(aux, [dateop, space, crashdump, leftbracket,
				dumpid, rightbracket, colonsp, report])
end	
