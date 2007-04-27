structure DIBBLER = struct
    open Model
    val aux: AuxInfo = {coverage = 35, label = NONE, tycomp = zeroComps }
    val loc : location = {lineNo = 0, beginloc = 0, endloc = 0, recNo = 0}
    val pn_t: Ty = Base(aux, [(Pint(1243, "1243"), loc)])
    val zipSep_t: Ty = RefinedBase(aux, Enum [StringConst "-", StringConst "/", StringConst " "],
		[(Pstring " ", loc)])
    val zip: Ty = Base(aux, [(Pint(34232, "34232"), loc)])
    val suffix: Ty = Base(aux, [(Pint(4232, "4232"), loc)])
    val extended_zip_t: Ty = Pstruct (aux, [zip, zipSep_t, suffix])
    val smallZip: Ty = Base(aux, [(Pint(34232, "34232"), loc)])
    val largeZip: Ty = Base(aux, [(Pint(34232, "34232"), loc)])
    val pzip : Ty = Punion(aux, [extended_zip_t, smallZip, largeZip])
    val summary_header_t: Ty = Pstruct(aux, [RefinedBase(aux, StringConst "0|", [(Pstring "0|", loc)]),
			Base(aux, [(Pint (234255, "234255"), loc)])])
    val no_ii: Ty = RefinedBase(aux, StringConst "no_ii", [(Pstring "no_ii", loc)])
    val id: Ty = Base(aux, [(Pint (234255, "234255"), loc)])
    val no_ramp_t : Ty = Pstruct(aux, [no_ii, id])
    val ramp: Ty = Base(aux, [(Pint(234255, "234255"), loc)])
    val dib_ramp_t : Ty = Punion(aux, [ramp, no_ramp_t])
    val space : Ty = RefinedBase(aux, StringConst " ", [(Pstring " ", loc)])
    val bar : Ty = RefinedBase(aux, StringConst "|", [(Pstring "|", loc)])
    val order_num: Ty = Base(aux, [(Pint (234255, "234255"), loc)])
    val att_order_num: Ty = Base(aux, [(Pint (234255, "234255"), loc)])
    val ord_version: Ty = Base(aux, [(Pint (234255, "234255"), loc)])
    val service_tn = Poption (aux, pn_t)
    val billing_tn = Poption (aux, pn_t)
    val nlp_service_tn = Poption (aux, pn_t)
    val nlp_billing_tn = Poption (aux, pn_t)
    val zip_code = Poption (aux, pn_t)
    val order_type: Ty = Base(aux, [(Pstring "34232", loc)])
    val order_details: Ty = Base(aux, [(Pint(34232, "34232"), loc)])
    val unused: Ty = Base(aux, [(Pstring("34232"), loc)])
    val stream: Ty = Base(aux, [(Pstring("34232"), loc)])
    val order_header_t: Ty = Pstruct(aux, [
		     order_num, 
		bar, att_order_num, 
		bar, ord_version,
		bar, service_tn, 
		bar, billing_tn, 
		bar, nlp_service_tn, 
		bar, nlp_billing_tn,
		bar, zip_code,
		bar, ramp,
		bar, order_type,
		bar, order_details,
		bar, unused,
		bar, stream,
		bar])
    val state: Ty = Base(aux, [(Pstring("34232"), loc)])
    val tstamp: Ty = Base(aux, [(Pint(34232, "34232"), loc)])
    val event_t : Ty = Pstruct(aux, [state, bar, tstamp])
    val eventSeq: Ty = RArray(aux, SOME(StringConst "|"), 
			SOME(StringConst "\n"), event_t, NONE, [])
    val entry_t = Pstruct(aux, [order_header_t, eventSeq])
    val dibbler_t = Punion(aux, [summary_header_t, entry_t])
end	
