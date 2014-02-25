structure ScrollKeeper= struct
    open Types

    val aux : AuxInfo = { coverage = 327, label = NONE, tycomp = zeroComps }
    val sw1: Id = Atom.atom("sw1")
    val swAux1 : AuxInfo = { coverage = 327, label = SOME sw1, tycomp = zeroComps }

    val loc : location = { lineNo = 0, beginloc = 0, endloc = 0, recNo = 0 }
    val date : Ty = Base ( aux, [(Pdate "Dec 10", loc)] )
    val sp : Ty = RefinedBase ( aux, StringConst " ", [(Pwhite " ", loc)] )
    val time : Ty = Base ( aux, [(Ptime "04:07:59", loc)] );
    val installing: Ty = RefinedBase (aux, StringConst "Installing ScrollKeeper ", [])
    val num: Ty = Base ( aux, [(Pint (0, "0"), loc)])
    val dot : Ty = RefinedBase ( aux, StringConst ".", [] )
    val dots : Ty = RefinedBase ( aux, StringConst "...", [] )
    val version: Ty = Pstruct(aux, [num, dot, num, dot, num])
    val installlog: Ty = Pstruct(aux, [installing, version, dots])

    val action:Ty = RefinedBase(swAux1, Enum[StringConst "scrollkeeper-rebuilddb: ",
					StringConst "scrollkeeper-update: "], [])
    val sender:Ty = RefinedBase(aux, Enum[StringConst "/usr/bin/scrollkeeper-update: ",
					StringConst "scrollkeeper-update: "], [])
    val colonsp: Ty = RefinedBase ( aux, StringConst ": ", [] )
    val registering: Ty = RefinedBase ( aux, StringConst "Registering ", [] )
    val buildmsg: Ty = RefinedBase(aux, Enum[StringConst "Rebuilding ScrollKeeper database...",
					StringConst "Done rebuilding ScrollKeeper database."], [])
    val path: Ty = Base(aux, [(Ppath "/opt/fbf", loc)])
    val nosuchfile: Ty = RefinedBase ( aux, StringConst ": No such file or directory", [] )
    val regmsg: Ty = Pstruct(aux, [registering, path])
    val update: Ty = Pstruct(aux, [sender, path, nosuchfile])
    val updatemsg: Ty = Punion( aux, [regmsg, update] )

    val switch : Ty = Switch (aux, sw1, [ (StringConst "scrollkeeper-rebuilddb: ", buildmsg), 
			(StringConst "scrollkeeper-update: ", updatemsg) ] )
    val log: Ty = Punion(aux, [installlog, Pstruct(aux, [action, switch])])
    val scrollkeeper : Ty = Pstruct ( aux, [date, sp, time, sp, log])
end
