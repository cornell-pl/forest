structure PNames =
struct
  val unionVal    = "val"
  val unionTag    = "tag"
  val arrayLen    = "length"
  val arrayElts   = "elts"
  val pdElts      = "pds"
  val arrayCur    = "current"
  val consume     = "consume"
  val arrayBegin  = "arrayBegin"  (* tloc.b     : Ppos_t *)
  val arrayEnd    = "arrayEnd"    (* tloc.e     : Ppos_t *)
  val elemBegin   = "eltBegin"    (* pd->loc.b  : Ppos_t *)
  val elemEnd     = "eltEnd"      (* pd->loc.e  : Ppos_t *)
  val structBegin = "structBegin"  (* tloc.b     : Ppos_t *)
  val structEnd   = "structEnd"    (* tloc.e     : Ppos_t *)
  val unionBegin  = "unionBegin"  (* tloc.b     : Ppos_t *)
  val unionEnd    = "unionEnd"    (* tloc.e     : Ppos_t *)
  val position    = "position"

  val structLevel = "structLevel"
  val unionLevel  = "unionLevel"
  val arrayLevel  = "arrayLevel"

  val m         = "m" 
  val pd        = "pd"
  val acc       = "acc"
  fun repSuf  s = s (* Make rep type same as pads name; s^"_rep" *)
  fun mSuf   s = s^"_"^m
  fun mConSuf s = s^"_con"
  fun pdSuf   s = s^"_"^pd
  fun accSuf  s = s^"_"^acc
  fun initSuf s = s^"_init"
  fun resetSuf s = s^"_reset"
  fun cleanupSuf s = s^"_cleanup"
  fun copySuf s = s^"_copy"
  fun srcSuf s = s^"_src"
  fun dstSuf s = s^"_dst"
  fun addSuf  s = s^"_add"
  fun readSuf s = s^"_read"
  fun scan1Suf s = s^"_scan1"
  fun scan2Suf s = s^"_scan2"
  fun maskInitSuf s = s^"_m_init"
  fun writeSuf s = s^"_write"
  fun writeXMLSuf s = s^"_write_xml_"
  fun ioSuf s = s^"2io"
  fun bufSuf s = s^"2buf"
  fun reportSuf s = s^"_report"
  fun mapSuf s = s^"_map"
  fun toStringSuf s = s^"2str"
  fun errSuf s = s^"_err"
  fun findEORSuf s = s^"_findpostlude"
  fun findEndSuf s = s^"_end"
  fun gTemp base = "tmp"^base
  fun childrenSuf name = name^"_children" 
  fun vTableSuf name = name^"_vtable"
  fun isPref name = "is_"^name

end