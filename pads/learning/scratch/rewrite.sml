structure Rewrite = struct
open Common

(* runs analysis using a Ty and return a refined Ty *)
fun run (ty : Ty) =
let
  val _ = print "\nBefore reduction:\n"
  val _ = printTy ty
  val _ = print "\n"
(*  val _ = printConstMap cmap *)
  val reduced_ty = Reduce.reduce NONE ty 
(*
  val _ = print "\nAfter initial reduction:\n"
  val _ = printTy reduced_ty 
*)
  val cmap = Constraint.constrain'(reduced_ty)
  val reduced_ty' = Reduce.reduce (SOME(cmap)) reduced_ty 
  val _ = print "\nAfter final reduction:\n"
  val _ = printTy reduced_ty'
  val _ = print "\n"
in
  reduced_ty'
end

end

