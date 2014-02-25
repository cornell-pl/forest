structure Yumtxt = struct
    open Types

    val yumAux : AuxInfo = { coverage = 327, label = NONE, tycomp = zeroComps, len=0.0 }
    val swLabel : Id = Atom.atom("sw1")
    val swAux : AuxInfo = { coverage = 327, label = SOME swLabel, tycomp = zeroComps, len=0.0 }

    val yumLoc : location = { lineNo = 0, beginloc = 0, endloc = 0, recNo = 0 }

    val date1 : Ty = Base ( yumAux, [(Pdate "Dec 10", yumLoc)] )
    val sp1 : Ty = RefinedBase ( yumAux, StringConst " ", [(Pwhite " ", yumLoc)] )
    val time1 : Ty = Base ( yumAux, [(Ptime "04:07:59", yumLoc)] );
    val sp2 : Ty = RefinedBase ( yumAux, StringConst " ", [(Pwhite " ", yumLoc)] )
    val insupdsp : Ty = RefinedBase ( yumAux, StringConst " ", [(Pwhite " ", yumLoc)] )
    val installUpdateR : Refined = Enum [StringConst "Installed", StringConst "Updated" ]
    val erasedR : Refined = StringConst "Erased"

    val methodTok : LToken list = [(Pstring "Installed", yumLoc)]
    val method : Ty =
           RefinedBase ( swAux, Enum [ StringConst "Installed", StringConst "Updated", StringConst "Erased" ], methodTok )

    val colsp : Ty = RefinedBase ( yumAux, StringConst ": ", [] )
    val packagename : Ty = RefinedBase ( yumAux, StringME "/[^.]+/", [(Pstring "util-linux", yumLoc)] )
    val arch : Ty = Base ( yumAux, [(Pstring "x86_64", yumLoc)])
    val version: Ty = RefinedBase (yumAux, StringME "/[0-9a-zA-Z.\\-_]+/", [])
    val dot : Ty = RefinedBase ( yumAux, StringConst ".", [] )
    val ins_update_package : Ty =
        Pstruct ( yumAux, [ packagename, dot, arch, insupdsp, version ] )
    val eor : Ty = Base ( yumAux, [(Pstring "ruby", yumLoc)] )
    val erase_package : Ty = Pstruct ( yumAux, [ eor ] )
    val switch : Ty = Switch ( yumAux, swLabel, [ ( installUpdateR, ins_update_package), ( erasedR, erase_package ) ] )
    val yum : Ty = Pstruct ( yumAux, [ date1, sp1, time1, sp2, method, colsp, switch ] )

end
