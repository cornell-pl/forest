structure QUARTERLY = struct
    open Types
    val aux: AuxInfo = {coverage = 35, label = NONE, tycomp = zeroComps }
    val loc : location = {lineNo = 0, beginloc = 0, endloc = 0, recNo = 0}
    val quote: Ty = RefinedBase(aux, StringConst "\"", [(Other #":", loc)])
    val comma: Ty = RefinedBase(aux, StringConst ",", [(Other #",", loc)])
    val myyear: Ty = Base(aux, [(Pint(1998, "1998"), loc)])
    val dot: Ty = RefinedBase(aux, StringConst ".", [(Other #".", loc)])
    val myquarter: Ty = Base(aux, [(Pint(1, "1"), loc)])
    val quarter_t: Ty = Pstruct(aux, [quote, myyear, dot, myquarter, quote])
    val quarters_t: Ty = RArray(aux, SOME(StringConst ","), NONE, quarter_t, 
			NONE, [])
    val table_header_t: Ty = Pstruct(aux,[
	RefinedBase(aux, StringConst "\"Personal income\",\"FIPS\",\"AreaName\",", [(Pstring "...", loc)]), quarters_t])
    val incomeseq = RArray(aux, SOME(StringConst ","), NONE, 
			Base(aux, [(Pint(1998, "1998"), loc)]), NONE, [])
    val code: Ty = Base(aux, [(Pint(10, "010"), loc)])
    val areacode: Ty = Base(aux, [(Pint(50, "50"), loc)])
    val areaname = RefinedBase(aux, StringME "/[A-Za-z ]*/", [(Pstring "Colorado", loc)])
    val quotecomma= RefinedBase(aux, StringConst "\",", [(Pstring "\",", loc)])
    val entry_t = Pstruct(aux, [quote, code, quote, comma, quote, areacode, quote, comma, quote, 
		areaname, quotecomma, incomeseq])
    val quarterly_t = Punion(aux, [table_header_t, entry_t, 
    		RefinedBase(aux, StringConst "\"Source: Regional Economic Information System, Bureau of Economic Analysis, U.S. Department of Commerce\"", [(Pstring " ", loc)])])
end	
