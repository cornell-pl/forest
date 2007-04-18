structure CRASHREPORTER = struct
    open Model
    val aux: AuxInfo = {coverage = 35, label = NONE, tycomp = zeroComps }
    val loc : location = {lineNo = 0, beginloc = 0, endloc = 0, recNo = 0}
    val date : Ty = Base(aux, [(Pdate "15/Oct/1997", loc)])
    val time : Ty = Base(aux, [(Ptime "18:46:51 -0700", loc)])
    val year: Ty = Base(aux, [(Pint (2004, "2004"), loc)])
    val space : Ty = RefinedBase(aux, StringConst " ", [(Pstring " ", loc)])
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
    val filepath: Ty = Base(aux, [(Ppath "/Users/kfisher/Library/Logs/CrashReporter/Preview.crash.log", loc)])
    val unable: Ty = RefinedBase(aux, StringConst "Unable to determine task_t for pid: ", 
			[(Pstring "Unable to determine task_t for pid: ", loc)]) 
    val pid: Ty = Base(aux, [(Pint(95, "95"), loc)])
    val name: Ty = RefinedBase(aux, StringConst " name: Exited process", [(Pstring "name: Exited process", loc)])
    val function: Ty = Base(aux, [(Pstring ("mach_msg()"), loc)])
    val reply: Ty = RefinedBase(aux, StringConst " reply failed: ", [(Pstring "srv7", loc)])
    val failedmsg: Ty = Base(aux, [(Pstring ("(ipc/send) invalid destination port"), loc)])
    val failtorelaunch: Ty = RefinedBase(aux, StringConst "Failed to re-launch ", 
			[(Pstring "Failed to re-launch ", loc)])
    val spdashsp: Ty = RefinedBase(aux, StringConst " - ", [(Pstring " - ", loc)])
    val errormsg: Ty = Base(aux, [(Pstring ("CGSError: 1025!"), loc)])
    val dumpreport: Ty = Punion(aux, [started,
				Pstruct(aux, [start, filepath]),
				Pstruct(aux, [finish, filepath]),
				Pstruct(aux, [unable, pid, name]),
				Pstruct(aux, [failtorelaunch, filepath, spdashsp, errormsg])
				])
    val reporterreport: Ty = Pstruct(aux, [function, reply, failedmsg])
    val report: Ty = Switch(aux, swLabel, [(StringConst "crashdump", dumpreport), 
					   (StringConst "crashreporterd", reporterreport)])
    val crashreport: Ty = Pstruct(aux, [date, space, time, space, year, space, crashdump, leftbracket,
				dumpid, rightbracket, colonsp, report])
end	
