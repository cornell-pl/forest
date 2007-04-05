structure AI = struct
    open Model
    val aiAux: AuxInfo = {coverage = 3000, label = NONE, tycomp = zeroComps }
    val aiLoc : location = {lineNo = 0, beginloc = 0, endloc = 0, recNo = 0}
    val ip : Ty = Base(aiAux, [(Pip "135.207.23.32", aiLoc)])
    val host : Ty = Base(aiAux, [(Phostname "www.cnn.com", aiLoc)])
    val client_t : Ty = Punion (aiAux, [ip, host])
    val unauthedId : Ty = RefinedBase(aiAux, StringConst "-", [(Pstring "-", aiLoc)])
    val authedId : Ty = Base(aiAux, [(Pstring "Amnesty", aiLoc)])
    val auth_Id : Ty = Punion(aiAux, [authedId, unauthedId])
    val space : Ty = RefinedBase(aiAux, StringConst " ", [(Pstring " ", aiLoc)])
    val date : Ty = Base(aiAux, [(Pdate "15/Oct/1997", aiLoc)])
    val time : Ty = Base(aiAux, [(Ptime "18:46:51 -0700", aiLoc)])
    val datetime : Ty = Pstruct(aiAux, [RefinedBase(aiAux, StringConst "[", [(Pstring "[", aiLoc)]),
		date, RefinedBase(aiAux, StringConst ":", [(Pstring ":", aiLoc)]), time,
		RefinedBase(aiAux, StringConst "]", [(Pstring "]", aiLoc)])])
    val method: Ty = RefinedBase (aiAux, Enum ([StringConst "GET", StringConst "PUT", StringConst "POST", 
			StringConst "HEAD", StringConst "DELETE", StringConst "LINK", StringConst "UNLINK"]), 
			[(Pstring "GET", aiLoc)]) 
    val quote: Ty = RefinedBase(aiAux, StringConst ":", [(Pstring ":", aiLoc)])
    val dot: Ty = RefinedBase(aiAux, StringConst ".", [(Pstring ".", aiLoc)])
    val slash: Ty = RefinedBase(aiAux, StringConst "/", [(Pstring "/", aiLoc)])
    val path:Ty = Base(aiAux, [(Ppath "/turkey/amnty1.gif", aiLoc)])
    val intbase:Ty = Base(aiAux, [(Pint (1, "1"), aiLoc)])
    val http:Ty = RefinedBase(aiAux, StringConst "HTTP", [(Pstring "HTTP", aiLoc)])
    val request:Ty = Pstruct (aiAux, [quote, method, space, path, space, http, slash, intbase, dot, intbase, quote])
    val response:Ty = RefinedBase(aiAux, Int (100, 600), [(Pint (100, "100"), aiLoc)])
    val ai : Ty = Pstruct(aiAux, [client_t, space, auth_Id, space, auth_Id, space, datetime, space, 
			request, space, response, space, intbase])
end	
