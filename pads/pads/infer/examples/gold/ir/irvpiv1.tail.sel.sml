structure IRV = struct
    open Model
    val aux: AuxInfo = {coverage = 35, label = NONE, tycomp = zeroComps, len=0.0 }
    val loc : location = {lineNo = 0, beginloc = 0, endloc = 0, recNo = 0}
    val dirLabel = Atom.atom("dirLabel")
    val dirAux: AuxInfo = {coverage = 34, label = SOME dirLabel, tycomp = zeroComps, len=0.0 }
    val sp = RefinedBase (aux, StringConst " ", [])
    val dashR:Refined = StringConst "-"
    val dR:Refined = StringConst "d"
    val word = Base(aux, [(Pstring("abc"), loc)])
    val date = Base(aux, [(Pdate("14/Dec/2006"), loc)])
    val time = Base(aux, [(Ptime("14/Dec/2006"), loc)])
    val ip = Base(aux, [(Pip("68.63.10.255"), loc)])
    val path = Base(aux, [(Ppath("68.63.10.255"), loc)])
    val url = Base(aux, [(Purl("68.63.10.255"), loc)])
    val intnum = Base (aux, [(Pint(0, "0"), loc)])
    val stuff = RefinedBase (aux, Blob(NONE, SOME "/; |\\)\"|\\) /"), []) 
    val array1 = RArray (aux, SOME (StringConst "; "), SOME (StringConst ")"), stuff, NONE, [])
    val sep : Refined = StringME "/[;#][ ]?/"
    val browsers = Pstruct(aux, [RefinedBase(aux, StringConst " Gecko/", []),
				 Base(aux, [(Pint (0, "0"), loc)]),
				RefinedBase(aux, StringConst " Firefox/", []),
				RefinedBase (aux, StringME("/[0-9.]+/"), [])])
    val quad_dash = RefinedBase(aux, StringConst "____", [])
    val equal = RefinedBase(aux, StringConst "=", [])
    val pair = Pstruct(aux, [
		Poption(aux, quad_dash),
		word,
		equal,
		Poption (aux, RefinedBase (aux, StringME("/[^;#]+/"), []))
		])
    val arraybody : Ty = Punion (aux, 
				[
				 pair,
				 quad_dash,
				 Base (aux, [(Pempty, loc)])
				])
    val irv: Ty = Pstruct (aux, 
	[ip, 
	RefinedBase (aux, StringConst " - - [", []),
	date,
	RefinedBase (aux, StringConst ":", []),
	time,
	RefinedBase (aux, StringConst "] \"GET ", []),
	Punion (aux, [url, path]),
	RefinedBase (aux, StringConst " HTTP/", []),
    	Base(aux, [(Pfloat("1", "0"), loc)]),
	RefinedBase (aux, StringConst "\" ", []),
	intnum,
	RefinedBase (aux, StringConst " ", []),
	intnum,
	RefinedBase (aux, StringConst " \"", []),
	Punion(aux, [url, RefinedBase (aux, dashR, [])]),
	RefinedBase (aux, StringConst "\" \"", []),
	word,
	RefinedBase (aux, StringConst "/", []),
    	Base(aux, [(Pfloat("1", "0"), loc)]),
	Poption (aux, Pstruct(aux, [
			RefinedBase (aux, StringConst " (", []),
			array1,
			RefinedBase (aux, StringConst ")", []),
			Poption(aux, browsers)])),
	RefinedBase (aux, StringConst "\" ", []),
	word,
	sp,
	intnum,
	sp,
	RArray(aux, SOME (sep), NONE, arraybody, NONE, [])
	])
	
end	
