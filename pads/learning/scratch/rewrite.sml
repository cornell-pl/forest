structure Rewrite = struct
open Common

(* runs analysis using a Ty and return a refined Ty *)
fun run (ty : Ty) =
let
  val cmap = Constraint.constrain'(ty);
  val reduced_ty = Reduce.reduce (SOME(cmap)) ty 
in
  reduced_ty
end

end

