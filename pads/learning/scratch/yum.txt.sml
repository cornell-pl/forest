structure Yumtxt = struct
    open Model
    val yumAux : AuxInfo = { coverage = 327, label = NONE, tycomp = zeroComps }
    val swLabel : Id = Atom.atom("sw1")
    val swAux : AuxInfo = { coverage = 327, label = SOME swLabel, tycomp = zeroComps }
    val yumLoc : location = { lineNo = 0, beginloc = 0, endloc = 0, recNo = 0 }
    val date1 : Ty = Base ( yumAux, [ ( Pdate "Dec 10", yumLoc ) ] )
    val sp1 : Ty = RefinedBase ( yumAux, StringConst " ", [] )
    val time1 : Ty = Base ( yumAux, [ ( Ptime "04:07:50", yumLoc ) ] )
    val sp2 : Ty = RefinedBase ( yumAux, StringConst " ", [] )
    val installUpdateR : Refined = Enum [StringConst "Installed", StringConst "Updated" ]
    val erasedR : Refined = StringConst "Erased"
    val method:Ty = RefinedBase ( swAux, Enum [ StringConst "Installed", StringConst "Updated", StringConst "Erased" ], [] )
    val colsp : Ty = RefinedBase ( yumAux, StringConst ": ", [] )
    val str1 : Ty = Base ( yumAux, [ ( Pstring "anything", yumLoc ) ] )
    val dot : Ty = RefinedBase ( yumAux, StringConst ".", [] )
    val dash : Ty = RefinedBase ( yumAux, StringConst "-", [] )
    val ins_update_package : Ty = Pstruct ( yumAux, [ str1, dot, str1, sp2, str1, dash, str1 ] )
    val eor : Ty = Base ( yumAux, [ ( Pstring "", yumLoc ) ] )
    val erase_package : Ty = Pstruct ( yumAux, [ eor ] )
    val switch : Ty = Switch ( yumAux, swLabel, [ ( installUpdateR, ins_update_package), ( erasedR, erase_package ) ] )
    val yum : Ty = Pstruct ( yumAux, [ date1, sp1, time1, sp2, method, colsp, switch ] )
end
