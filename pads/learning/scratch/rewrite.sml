structure Rewrite = struct
open Common
open Model 

(* runs analysis using a Ty and return a refined Ty *)
fun run (ty : Ty) =
let
  val measuredTy = measure ty
  val comps = getComps measuredTy
  val tycomp = #tc comps
  val acomp  = #adc comps
  val datacomp = #dc comps
  val rawcomp = combine tycomp datacomp
  val _ = print "\nBefore reduction:\n"
  val _ = printTy measuredTy
(*phase one *)
(*
  val _ = print "Phase one ...\n";
*)
  val ty1 = Reduce.reduce 1 ty 
(*
  val _ = printTy ty1
*)
(*phase two*) 
(*
  val _ = print "Phase two ...\n";
*)
  val ty2 = Reduce.reduce 2 ty1
(*
  val _ = printTy ty2
*)
  (*phase three, redo constraint-free reduction *)
(*
  val _ = print "Phase three ...\n";
*)
  val ty3 = Reduce.reduce 3 ty2

  val measured_reduced_ty = measure ty3
  val _ = print "\nRefined Ty:\n"
  val _ = printTy measured_reduced_ty
  val _ = print "\n"
  val _ = print "----- The PADS description -----\n\n"
  val _ = print (TyToPADSFile measured_reduced_ty) 
  val _ = print "\n----- End of PADS description -----\n"
  val comps' = getComps measured_reduced_ty
  val tycomp' = #tc comps'
  val acomp' = #adc comps'
  val datacomp' = #dc comps'
  val rawcomp' = combine tycomp' datacomp'
(*
  val _ =  print ("type comp = "^ (showComp tycomp) ^"\n");
  val _ =  print ("atomic comp = "^ (showComp acomp) ^"\n");
  val _ =  print ("data comp = "^ (showComp datacomp) ^"\n");
  val _ =  print ("total comp = "^ (showComp rawcomp) ^"\n");
  val _ =  print ("new type comp = "^ (showComp tycomp') ^"\n");
  val _ =  print ("new atomic comp = "^ (showComp acomp') ^"\n");
  val _ =  print ("new data comp = "^ (showComp datacomp') ^"\n");
  val _ =  print ("new total comp = "^ (showComp rawcomp') ^"\n");
*)
in
  measured_reduced_ty 
end

end

