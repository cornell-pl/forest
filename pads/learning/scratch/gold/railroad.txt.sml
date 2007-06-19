structure RAILROAD = struct
    open Model
    val aux: AuxInfo = {coverage = 35, label = NONE, tycomp = zeroComps }
    val loc : location = {lineNo = 0, beginloc = 0, endloc = 0, recNo = 0}
    val space= RefinedBase(aux, StringConst " ", [(Pstring " ", loc)])
    val spaceop = Poption(aux, space);
    val comma= RefinedBase(aux, StringConst ",", [(Pstring ",", loc)])
    val quote = RefinedBase(aux, StringConst "\"", [(Pstring "\"", loc)])
    val noteinQuotes = RefinedBase(aux, StringME "/\\\"[^\"]*\\\"/", [(Pstring "Railtype", loc)])
    val noteOutSideQuotes = RefinedBase(aux, StringME "/[^,]*/", [(Pstring "Railtype", loc)])
    val note: Ty = Punion (aux, [noteinQuotes, noteOutSideQuotes])
    val commas : Ty =RefinedBase(aux, StringConst ",,,,,,,,,,,,,,,", 
				[(Pstring ",,,,,,,,,,,,,,,", loc)])
    val comments: Ty = Pstruct(aux, [note, commas])
    val tableheader: Ty = RefinedBase(aux, StringConst "Type of rail transit / agency,Primary city served,Number of stations,,,,,,,Number of ADA-accessible stations,,,,,,", [(Pstring "Table 1-9", loc)])
    val railkind : Ty = Base(aux, [(Pstring "Heavy", loc)])
    val rail = RefinedBase(aux, StringConst "rail", [(Pstring " ", loc)])
    val thecolumns: Ty = RefinedBase(aux, StringConst "Table 1-9:  ADA-Accessible Rail Transit Stations by Agency,,,,,,,,,,,,,,,", [(Pstring "Table 1-9", loc)])

    val years = RefinedBase(aux, StringConst ",,1996,1997,1998,1999,2000,2001,2002,1996,1997,1998,1999,2000,2001,2002", 
			[(Pstring ",,1996,1997,1998,1999,2000,2001,2002,1996,1997,1998,1999,2000,2001,2002", loc)])
    val secHeader: Ty = Pstruct (aux, [railkind, space, rail, commas])
    val railtype1 = Pstruct(aux, [quote, RefinedBase(aux, StringME "/[^\"]*/", [(Pstring "Railtype", loc)]), quote])
    val railtype2 = RefinedBase(aux, StringME "/[^,]*/", [(Pstring "Railtype", loc)])
    val railtype = Punion(aux, [railtype1, railtype2])
    val cityname = RefinedBase(aux, StringME "/[^,]*/", [(Pstring "San Jose", loc)])
    val city = Pstruct(aux, [quote, cityname, comma, spaceop, 
				RefinedBase(aux, StringME "/[A-Z][A-Z]/", [(Pstring "CA", loc)]),
				quote])
    val nostations = Punion(aux, [Base(aux, [(Pint(36, "36"), loc)]), 
    				RefinedBase(aux, Enum [StringConst "U", StringConst "NA"], 
				[(Pstring "U", loc)])])
    val stations = RArray(aux, SOME(StringConst ","), NONE, nostations, SOME (IntConst 14), [])
    val record = Pstruct(aux, [railtype, comma, city, comma, stations])
    val railroad = Punion(aux, [tableheader, thecolumns, years, secHeader, comments, record])
end	
