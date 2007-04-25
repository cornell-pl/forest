structure Yumtxt = struct
    open Model
    open YumArch
    open YumMajor
    open YumMinor
    open YumPackageName
    open YumTokens
    open YumDates
    open YumTimes
    open YumInsUpd
    open YumErased

    val yumAux : AuxInfo = { coverage = 327, label = NONE, tycomp = zeroComps }
    val swLabel : Id = Atom.atom("sw1")
    val swAux : AuxInfo = { coverage = 327, label = SOME swLabel, tycomp = zeroComps }

    val yumLoc : location = { lineNo = 0, beginloc = 0, endloc = 0, recNo = 0 }

    val date1 : Ty = Base ( yumAux, dates )
    val sp1 : Ty = RefinedBase ( yumAux, StringConst " ", sp1tok )
    val time1 : Ty = Base ( yumAux, times );
    val sp2 : Ty = RefinedBase ( yumAux, StringConst " ", sp2tok )
    val insupdsp : Ty = RefinedBase ( yumAux, StringConst " ", insupdsptok )
    val installUpdateR : Refined = Enum [StringConst "Installed", StringConst "Updated" ]
    val erasedR : Refined = StringConst "Erased"

    val methodTok : LToken list = insupdTokens @ erasedTokens
    val method : Ty =
           RefinedBase ( swAux, Enum [ StringConst "Installed", StringConst "Updated", StringConst "Erased" ], methodTok )

    val colsp : Ty = RefinedBase ( yumAux, StringConst ": ", colsptok )
    val packagename : Ty = Base ( yumAux, packagenameTokens )
    val arch : Ty = Base ( yumAux, archTokens )
    val major : Ty = Base ( yumAux, majorTokens )
    val minor : Ty = Base ( yumAux, minorTokens )
    val dot : Ty = RefinedBase ( yumAux, StringConst ".", dottok )
    val dash : Ty = RefinedBase ( yumAux, StringConst "-", dashtok )
    val ins_update_package : Ty =
        Pstruct ( yumAux, [ packagename, dot, arch, insupdsp, major, dash, minor ] )
    val eor : Ty = Base ( yumAux, erasedpkgTokens )
    val erase_package : Ty = Pstruct ( yumAux, [ eor ] )
    val switch : Ty = Switch ( yumAux, swLabel, [ ( installUpdateR, ins_update_package), ( erasedR, erase_package ) ] )
    val yum : Ty = Pstruct ( yumAux, [ date1, sp1, time1, sp2, method, colsp ] )

    val myum : Ty = measure yum;

end
