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
  val elemBegin   = "eltBegin"   (* pd->loc.b  : Ppos_t *)
  val elemEnd     = "eltEnd"      (* pd->loc.e  : Ppos_t *)
end