structure RAILROAD = struct
    open Model
    val aux: AuxInfo = {coverage = 35, label = NONE, tycomp = zeroComps }
    val loc : location = {lineNo = 0, beginloc = 0, endloc = 0, recNo = 0}
    val notes: Ty = Base(aux, [(Pstring "SOURCE", loc)])
    val commas : Ty =RefinedBase(aux, StringConst ",,,,,,,,,,,,,,,", [(Pstring ",,,,,,,,,,,,,,,", loc)])
    val comments: Ty = Pstruct(aux, [notes, commas])
    val years = RefinedBase(aux, StringConst ",,1996,1997,1998,1999,2000,2001,2002,1996,1997,1998,1999,2000,2001,2002", 
			[(Pstring ",,1996,1997,1998,1999,2000,2001,2002,1996,1997,1998,1999,2000,2001,2002", loc)])
    val railtype = Base(aux, [(Pstring "Heavy rail", loc)])
    val comma= RefinedBase(aux, StringConst ",", [(Pstring ",", loc)])
    val quote = RefinedBase(aux, StringConst "\"", [(Pstring "\"", loc)])
    val space= RefinedBase(aux, StringConst " ", [(Pstring " ", loc)])
    val city = Pstruct(aux, [quote, Base(aux, [(Pstring "San Diego", loc)]), 
				comma, space, 
				RefinedBase(aux, StringME "/[A-Z][A-Z]/", [(Pstring "CA", loc)]),
				quote])
    val nostations = Punion(aux, [Base(aux, [(Pint(36, "36"), loc)]), 
    				RefinedBase(aux, Enum [StringConst "U", StringConst "NA"], [(Pstring "U", loc)])])
    val stations = RArray(aux, SOME(StringConst ","), NONE, nostations, SOME (IntConst 14), [])
    val record = Pstruct(aux, [railtype, comma, city, comma, stations])
    val railroad = Punion(aux, [comments, record])
end	
