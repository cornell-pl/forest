structure PNames =
struct
  val unionVal    = "val"
  val unionTag    = "tag"
  val arrayLen    = "length"
  val numRead     = "numRead"
  val arrayElts   = "elts"
  val pdElts      = "pds"
  val arrayCur    = "current"
  val curElt      = "elt"
  val curPd       = "pd"
  val consume     = "consume"
  val offset      = "offset"
  val nerr        = "nerr"
  val arrayBegin  = "begin"  (* tloc.b     : Ppos_t *)
  val arrayEnd    = "end"    (* tloc.e     : Ppos_t *)
  val elemBegin   = "eltBegin"    (* pd->loc.b  : Ppos_t *)
  val elemEnd     = "eltEnd"      (* pd->loc.e  : Ppos_t *)
  val structBegin = "begin"  (* tloc.b     : Ppos_t *)
  val structEnd   = "end"    (* tloc.e     : Ppos_t *)
  val unionBegin  = "begin"  (* tloc.b     : Ppos_t *)
  val unionEnd    = "end"    (* tloc.e     : Ppos_t *)
  val position    = "position"
  val identifier  = "_id_"

  val structLevel = "compoundLevel"
  val unionLevel  = "compoundLevel"
  val arrayLevel  = "compoundLevel"
  val user        = "compoundLevel"  (* typedef case *)


  val elt_rep   = "elt_rep"
  val elt_pd    = "elt_pd"
  val m         = "m" 
  val pd        = "pd"
  val rep       = "rep"
  val acc       = "acc"
  val sfstderr  = "sfstderr"
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
  fun roDriverSuf s = s^"_ro_driver"
  fun roArgsSuf s = s^"_roArgs"
  fun roInitSuf s = s^"_read_one_init"
  fun readOneSuf s = s^"_read_one"
  fun finalChecksSuf s = s^"_final_checks"
  fun scan1Suf s = s^"_scan1"
  fun scan2Suf s = s^"_scan2"
  fun maskInitSuf s = s^"_m_init"
  fun writeSuf s = s^"_write"
  fun writeXMLSuf s = s^"_write_xml_"
  fun fmtSuf s = s^"_fmt"
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
  fun kthChildSuf name = name^"_kthChild" 
  fun kthChildNamedSuf name = name^"_kthChildNamed" 
  fun pathWalkSuf name = name^"_pathWalk" 
  fun vTableSuf name = name^"_vtable"
  fun cnInitSuf name = name^"_cachedNode_init"
  fun sndInitSuf name = name^"_sndNode_init"

  fun nodeSuf name = name^"_node"
  fun cnSuf name = name^"_cachedNode"  
  fun sndSuf name = name^"_sndNode"  

  fun nodeNewSuf name = (nodeSuf name)^"_new" 
  fun nodeKCSuf name = kthChildSuf (nodeSuf name)
  fun nodeKCNSuf name = kthChildNamedSuf (nodeSuf name)
  fun nodePWSuf name = pathWalkSuf (nodeSuf name)  
  fun nodeVTableSuf name = vTableSuf (nodeSuf name)

  fun cnKCSuf name = kthChildSuf (cnSuf name)
  fun cnVTableSuf name = vTableSuf (cnSuf name)

  fun sndKCSuf name = kthChildSuf (sndSuf name)
  fun sndKCNSuf name = kthChildNamedSuf (sndSuf name)
  fun sndVTableSuf name = vTableSuf (sndSuf name)

  fun isPref name = "is_"^name

end
