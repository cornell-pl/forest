structure RPMPKGS = struct
    open Model
    val aux: AuxInfo = {coverage = 35, label = NONE, tycomp = zeroComps, len=0.0 }
    val loc : location = {lineNo = 0, beginloc = 0, endloc = 0, recNo = 0}
    val arch_t : Ty = RefinedBase(aux, Enum [StringConst "noarch", StringConst "i386",
			StringConst "i586", StringConst "i686", StringConst "alpha",
			StringConst "sparc", StringConst "mips", StringConst "ppc",
			StringConst "m68k", StringConst "SGI", StringConst "(none)"],
			[(Pstring "i386", loc)])
    val dash : Ty = RefinedBase(aux, StringConst "-", [(Pstring "-", loc)])
    val dot: Ty = RefinedBase(aux, StringConst ".", [(Pstring ".", loc)])
    val rpm: Ty = RefinedBase(aux, StringConst ".rpm", [(Pstring ".rpm", loc)])
    val name: Ty = RefinedBase(aux, StringME "/[0-9a-zA-Z]+/", [(Pstring "4Suite", loc)])
    val version: Ty = RefinedBase(aux, StringME "/[0-9a-zA-Z.]+/", [(Pstring "4Suite", loc)])
    val release: Ty = RefinedBase(aux, StringME "/[0-9a-zA-Z.]+/", [(Pstring "4Suite", loc)])
    val rpmpkgs: Ty = Pstruct(aux, [name, dash, version, dash, release, dot, arch_t, rpm])
end	
