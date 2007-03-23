structure Rewrite = struct
open Common
open Model 

(* runs analysis using a Ty and return a refined Ty *)
fun run (ty : Ty) =
let
  val measured_ty = measure ty
  val (tycomp, datacomp) = getComps measured_ty
  val rawcomp = combine tycomp datacomp
  val _ = print "\nBefore reduction:\n"
  val _ = printTy measured_ty
  val _ = print "\n"
(*  val _ = printConstMap cmap 
*)
  val reduced_ty = Reduce.reduce NONE ty 
(*
  val _ = print "\nAfter initial reduction:\n"
  val _ = printTy reduced_ty 
*)
  val cmap = Constraint.constrain'(reduced_ty)
  val reduced_ty' = Reduce.reduce (SOME(cmap)) reduced_ty 
  val measured_reduced_ty' = measure reduced_ty'
  val (tycomp', datacomp') = getComps measured_reduced_ty'
  val rawcomp' = combine tycomp' datacomp'
  val _ = print "\nAfter final reduction:\n"
  val _ = printTy measured_reduced_ty'
  val _ = print "\n"
  val _ =  print ("type comp = "^ (showComp tycomp) ^"\n");
  val _ =  print ("data comp = "^ (showComp datacomp) ^"\n");
  val _ =  print ("total comp = "^ (showComp rawcomp) ^"\n");
  val _ =  print ("new type comp = "^ (showComp tycomp') ^"\n");
  val _ =  print ("new data comp = "^ (showComp datacomp') ^"\n");
  val _ =  print ("new total comp = "^ (showComp rawcomp') ^"\n");
in
  reduced_ty' 
end

end

