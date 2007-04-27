structure Yumtxt = struct
    open Types

    val yumAux : AuxInfo = { coverage = 327, label = NONE, tycomp = zeroComps }
    val swLabel : Id = Atom.atom("sw1")
    val swAux : AuxInfo = { coverage = 327, label = SOME swLabel, tycomp = zeroComps }

    val yumLoc : location = { lineNo = 0, beginloc = 0, endloc = 0, recNo = 0 }

    val date1 : Ty = Base ( yumAux, [] )
    val sp1 : Ty = RefinedBase ( yumAux, StringConst " ", [] )
    val time1 : Ty = Base ( yumAux, [] );
    val sp2 : Ty = RefinedBase ( yumAux, StringConst " ", [] )
    val insupdsp : Ty = RefinedBase ( yumAux, StringConst " ", [] )
    val installUpdateR : Refined = Enum [StringConst "Installed", StringConst "Updated" ]
    val erasedR : Refined = StringConst "Erased"

    val methodTok : LToken list = []
    val method : Ty =
           RefinedBase ( swAux, Enum [ StringConst "Installed", StringConst "Updated", StringConst "Erased" ], methodTok )

    val colsp : Ty = RefinedBase ( yumAux, StringConst ": ", [] )
    val packagename : Ty = Base ( yumAux, [] )
    val arch : Ty = Base ( yumAux, [] )
    val major : Ty = Base ( yumAux, [] )
    val minor : Ty = Base ( yumAux, [] )
    val dot : Ty = RefinedBase ( yumAux, StringConst ".", [] )
    val dash : Ty = RefinedBase ( yumAux, StringConst "-", [] )
    val ins_update_package : Ty =
        Pstruct ( yumAux, [ packagename, dot, arch, insupdsp, major, dash, minor ] )
    val eor : Ty = Base ( yumAux, [] )
    val erase_package : Ty = Pstruct ( yumAux, [ eor ] )
    val switch : Ty = Switch ( yumAux, swLabel, [ ( installUpdateR, ins_update_package), ( erasedR, erase_package ) ] )
    val yum : Ty = Pstruct ( yumAux, [ date1, sp1, time1, sp2, method, colsp ] )

end
