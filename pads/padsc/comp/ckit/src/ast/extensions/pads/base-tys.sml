structure PBaseTys = 

struct
   type baseInfoTy = {padsname : Atom.atom, 
		      repname  : Atom.atom, 
                      emname   : Atom.atom,
                      edname   : Atom.atom,
  		      readname : Atom.atom,
		      scanname : Atom.atom option}

   val baseInfo : baseInfoTy list = [
	  {padsname = Atom.atom "auint8", 
	   repname  = Atom.atom "uint8", 
	   emname   = Atom.atom "base_em", 
	   edname   = Atom.atom "base_ed", 
	   readname = Atom.atom "auint8_read",
           scanname = NONE},

	  {padsname = Atom.atom "aint8", 
	   repname  = Atom.atom "PDC_int8", 
	   emname   = Atom.atom "PDC_base_em", 
	   edname   = Atom.atom "PDC_base_ed", 
	   readname = Atom.atom "PDC_aint8_read",
           scanname = NONE},

	  {padsname = Atom.atom "auint32", 
	   repname  = Atom.atom "PDC_uint32", 
	   emname   = Atom.atom "PDC_base_em", 
	   edname   = Atom.atom "PDC_base_ed", 
	   readname = Atom.atom "PDC_auint32_read",
           scanname = NONE},

	  {padsname = Atom.atom "aint32", 
	   repname  = Atom.atom "PDC_int32", 
	   emname   = Atom.atom "PDC_base_em", 
	   edname   = Atom.atom "PDC_base_ed", 
	   readname = Atom.atom "PDC_aint32_read",
           scanname = NONE},

	  {padsname = Atom.atom "PDC_char_lit", 
	   repname  = Atom.atom "PDC_uint8", 
	   emname   = Atom.atom "PDC_base_em", 
	   edname   = Atom.atom "PDC_base_ed", 
	   readname = Atom.atom "PDC_char_lit_read",
           scanname = SOME(Atom.atom "PDC_char_lit_scan")}
       ]


  structure PBST = RedBlackMapFn(
                     struct type ord_key = Atom.atom
			    val compare = Atom.compare
		     end) 
 
  type baseTyMap = baseInfoTy PBST.map

  val baseInfo : baseTyMap = 
      let fun ins m []  = m
            | ins m ((b:baseInfoTy)::bs) = ins (PBST.insert (m, #padsname b, b)) bs 
      in
	  ins PBST.empty baseInfo
      end

  val find : (baseTyMap * Atom.atom) -> baseInfoTy option = PBST.find
end