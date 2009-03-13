structure Rewrite = struct
open Common
open Model 
open Times

(* runs analysis using a Ty and return a refined Ty with optional header and footer *)
fun run ( et : EndingTimes ) (ty : Ty) : Ty * Ty * int * int * EndingTimes =
let
  val measuredTy = measure (removePempty ty)
  val measured1_time = Time.now ()
  val comps    = getComps measuredTy
  val tycomp   = #tc comps
  val acomp    = #adc comps
  val datacomp = #dc comps
  val rawcomp  = combine tycomp datacomp
(*
  val _ = (print "Before Reduction:\n"; printTy measuredTy) 
*)
  (*before doing reduction, try to extract 
	the possible header and footer first*)
  val (headers, footers, auxOp, body) = 
	if DEF_EXTRACT_HEADER_FOOTER = true then extractHeaderFooter measuredTy
 	else (nil, nil, NONE, measuredTy)
(*phase one *)
(*
  val _ = print "Phase one ...\n";
  val _ = case (headerTyOp, footerTyOp) of (NONE, NONE) => ()
	  | _ => print "Found a header or footer!\n"
*)
  val ty1 = Reduce.reduce 1 body
  val headers= map (Reduce.reduce 1) headers
  val footers= map (Reduce.reduce 1) footers
  val reduce1_time : Time.time = Time.now ()
(*
  val _ = printTy (measure ty1)
*)
(*phase two*) 
(*
  val _ = print "Phase two ...\n";
*)
  val ty2 = measure (Reduce.reduce 2 ty1)
  val headers= map (Reduce.reduce 2) headers
  val footers= map (Reduce.reduce 2) footers
  val reduce2_time : Time.time = Time.now ()
  (*phase three, redo constraint-free reduction *)
(*
  val _ = print "Phase three ...\n";
*)
  val ty3 = Reduce.reduce 3 ty2
  val headers= map (Reduce.reduce 3) headers
  val footers= map (Reduce.reduce 3) footers
  val reduce3_time : Time.time = Time.now ()

(*
  val _ = print "Before mkBlob ...\n"
  val _ = printTy ty3
*)

  val ty3 = if Options.do_blob_finding then
		sortUnionBranches (Reduce.updateWithBlobs NONE ty3)
	    else sortUnionBranches ty3
(*
  val _ = print "After mkBlob ...\n"
  val _ = printTy ty3
*)

  val finalTy = case auxOp of
	SOME aux => Punion(aux, headers @ [ty3] @ footers)
	| NONE => ty3
  val measured_reduced_ty = measure finalTy
  val measured2_time : Time.time = Time.now ()
  val _ = print "\nRefined Ty:\n"
  val _ = printTy measured_reduced_ty
(*
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

in (measuredTy, measured_reduced_ty, length headers, length footers, endingTimes)
end

end

