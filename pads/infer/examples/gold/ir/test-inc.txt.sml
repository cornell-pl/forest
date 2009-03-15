structure TEST_INC = struct
    open Model
    val aux: AuxInfo = {coverage = 35, label = NONE, tycomp = zeroComps }
    val loc : location = {lineNo = 0, beginloc = 0, endloc = 0, recNo = 0}
    val dirLabel = Atom.atom("dirLabel")
    val dirAux: AuxInfo = {coverage = 34, label = SOME dirLabel, tycomp = zeroComps }
    val dashR:Refined = StringConst "-"
    val dR:Refined = StringConst "d"
    val test_inc : Ty = RefinedBase (aux, StringConst "abc", [])
end	
