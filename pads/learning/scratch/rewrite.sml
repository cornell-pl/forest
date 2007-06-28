structure Rewrite = struct
open Common
open Model 
open Times

(* runs analysis using a Ty and return a refined Ty *)
fun run ( et : EndingTimes ) (ty : Ty) : Ty * Ty * EndingTimes =
let
  val measuredTy = measure ty
  val measured1_time = Time.now ()
  val comps    = getComps measuredTy
  val tycomp   = #tc comps
  val acomp    = #adc comps
  val datacomp = #dc comps
  val rawcomp  = combine tycomp datacomp
(*
  val _ = print "\nBefore reduction:\n"
  val _ = printTy measuredTy
*)
(*phase one *)
(*
  val _ = print "Phase one ...\n";
*)
  val ty1 = Reduce.reduce 1 ty 
  val reduce1_time : Time.time = Time.now ()
(*
  val _ = printTy (measure ty1)
*)
(*phase two*) 
(*
  val _ = print "Phase two ...\n";
*)
  val ty2 = Reduce.reduce 2 ty1
  val reduce2_time : Time.time = Time.now ()
(*
  val _ = printTy (measure ty2)
*)
  (*phase three, redo constraint-free reduction *)
(*
  val _ = print "Phase three ...\n";
*)
  val ty3 = Reduce.reduce 3 ty2
  val reduce3_time : Time.time = Time.now ()

  val measured_reduced_ty = measure ty3
  val measured2_time : Time.time = Time.now ()
(*
  val _ = print "\nRefined Ty:\n"
  val _ = printTy measured_reduced_ty
  val _ = print "\n"
  val _ = print "----- The PADS description -----\n\n"
  val _ = print (#2 (TyToPADSFile measured_reduced_ty (!lexName^".p"))) 
  val _ = print "\n----- End of PADS description -----\n"
*)
  val comps'    = getComps measured_reduced_ty
  val tycomp'   = #tc comps'
  val acomp'    = #adc comps'
  val datacomp' = #dc comps'
  val rawcomp'  = combine tycomp' datacomp'
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
  val endingTimes : EndingTimes = { start       = #start et
                                  , tokenEnd    = #tokenEnd et
                                  , measure1End = measured1_time
                                  , reduce1End  = reduce1_time
                                  , reduce2End  = reduce2_time
                                  , reduce3End  = reduce3_time
                                  , measure2End = measured2_time
				  , padsEnd = #padsEnd et
                                  }

in ( measuredTy, measured_reduced_ty, endingTimes )
end

end

