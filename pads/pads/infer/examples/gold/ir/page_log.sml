structure PAGE_LOG = struct
    open Model
    val aux: AuxInfo = {coverage = 1, label = NONE, tycomp = zeroComps, len=0.0 }
    val loc : location = {lineNo = 0, beginloc = 0, endloc = 0, recNo = 0}
    val space : Ty = RefinedBase(aux, StringConst " ", [(Pstring " ", loc)])
    val lb: Ty = RefinedBase(aux, StringConst "[", [(Pstring "[", loc)])
    val rb: Ty = RefinedBase(aux, StringConst "]", [(Pstring "]", loc)])

    val date : Ty = Base(aux, [(Pdate "2006.11.21", loc)])
    val time : Ty = Base(aux, [(Ptime "18:46:51", loc)])
    val printername : Ty = Base(aux, [(Pstring "officejet", loc)])
    val op_name: Ty = Poption(aux, RefinedBase(aux, StringConst "(Printer)", [(Pstring "(Printer)", loc)]))
    val printer : Ty = Pstruct(aux, [printername, op_name])
    val user: Ty = Base(aux, [(Pstring "kfisher", loc)])
    val id: Ty = Base(aux, [(Pint (4, "4"), loc)])
    val colon: Ty = RefinedBase(aux, StringConst ":", [(Pstring ":", loc)])
    val pageno: Ty = Base(aux, [(Pint (~1, "-1"), loc)])
    val status: Ty = Base(aux, [(Pint (~1, "-1"), loc)])
    val dash : Ty = RefinedBase(aux, StringConst "-", [(Pstring "-", loc)])

    val localhost: Ty = RefinedBase(aux, StringConst "localhost", [(Pstring "localhost", loc)])

    val page_log: Ty = Pstruct(aux, [printer, space, user, space, id, space, lb, date, colon, time, rb, space,
				pageno, space, status, space, dash, space, localhost])
end	
