structure Rewrite = struct
open Common

(* runs analysis using a Ty and return a refined Ty *)
fun run (ty : Ty) =
let
  val _ = printTy ty
  val _ = print "\n"
  val cmap = Constraint.constrain'(ty);
  val _ = printConstMap cmap
  val reduced_ty = Reduce.reduce (SOME(cmap)) ty 
  val _ = printTy reduced_ty
in
  reduced_ty
end

end

