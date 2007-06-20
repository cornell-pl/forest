structure QUARTERLY = struct
    open Model
    val aux: AuxInfo = {coverage = 35, label = NONE, tycomp = zeroComps }
    val loc : location = {lineNo = 0, beginloc = 0, endloc = 0, recNo = 0}
    val quote: Ty = RefinedBase(aux, StringConst "\"", [(Other #"\"", loc)])
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
    val code =RefinedBase(aux, StringConst "\"010\",\"", [(Pstring "\"010\",\"", loc)])
    val areacode = Base(aux, [(Pint (02, "02"), loc)])
    val quotecommaquote = RefinedBase(aux, StringConst "\",\"", [(Pstring "\",\"", loc)])
    val areaname = RefinedBase(aux, StringME "/[A-Za-z ]*/", [(Pstring "Colorado", loc)])
    val quotecomma= RefinedBase(aux, StringConst "\",", [(Pstring "\",", loc)])
    val entry_t = Pstruct(aux, [code, areacode, quotecommaquote, areaname, quotecomma, incomeseq])
    val quarterly_t = Punion(aux, [table_header_t, entry_t, 
    		RefinedBase(aux, StringConst "\"Source: Regional Economic Information System, Bureau of Economic Analysis, U.S. Department of Commerce\"", [(Pstring " ", loc)])])
end	
