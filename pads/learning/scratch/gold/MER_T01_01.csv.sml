structure MER = struct
    open Model
    val aux : AuxInfo = { coverage = 999, label = NONE, tycomp = zeroComps }
    val loc: location = { lineNo = 0, beginloc = 0, endloc = 0, recNo = 0 }
    val sp1 : Ty = RefinedBase (aux, StringME "/[ ]+/", [] )
    val sp2 : Ty = RefinedBase (aux, StringME "/[ ]+/", [] )
    val sp3 : Ty = RefinedBase (aux, StringME "/[ ]+/", [] )
    val sp4 : Ty = RefinedBase (aux, StringME "/[ ]+/", [] )
    val sp5 : Ty = RefinedBase (aux, StringME "/[ ]+/", [] )
    val comma : Ty = RefinedBase (aux, StringConst ",", [] )
    val type_t: Ty = RefinedBase(aux, Enum [IntConst 0101, IntConst 0102, IntConst 0103, IntConst 0104 ], [])
    val header: Ty = RefinedBase(aux, StringConst "\"MSN\",\"YYYYMM\",\"Publication Value\",\"Publication Unit\",\"Column Order\"", [])
    val busname: Ty = RefinedBase(aux, Enum [StringConst "\"TEAJBUS\"", 
		StringConst "\"TEEXBUS\"",
		StringConst "\"TEIMBUS\"", StringConst "\"TEPRBUS\"", 
		StringConst "\"TETCBUS\""], [])
    val unitname:Ty = RefinedBase(aux, StringConst "Quadrillion Btu", [])
    val date:Ty = Base (aux, [(Pint (199913, "199913"), loc)])
    val value:Ty = Base (aux, [(Pfloat ("99", "07"), loc)])
    val order:Ty = Base (aux, [(Pint (4, "4"), loc)])
    val record: Ty = Pstruct(aux, [busname, comma, date, comma, value, comma, 
				unitname, comma, order])
    val mer: Ty = Punion(aux, [header, record])
end
