structure Rewrite = struct
open Common
open Model 
open Times

(* runs analysis using a Ty and return a refined Ty with optional header and footer *)
(* mode 0 is for normal learn program, mode 1 is for incremental learning *)
(* we assume the ty passed to the run function has beem "measured" *)

fun run ( et : EndingTimes ) (mode : int) (ty : Ty) : Ty * Ty * int * int * EndingTimes =
let
  val initTy = Reduce.removePempty ty
  val measured1_time = Time.now ()
  val comps    = getComps initTy
  val tycomp   = #tc comps
  val acomp    = #adc comps
  val datacomp = #dc comps
  val rawcomp  = combine tycomp datacomp

  val _ = (print "Before Reduction:\n"; printTy initTy) 

  (*before doing reduction, try to extract 
	the possible header and footer first*)
  val (headers, footers, auxOp, body) = 
	if DEF_EXTRACT_HEADER_FOOTER = true then extractHeaderFooter initTy
 	else (nil, nil, NONE, initTy)
(*phase one *)
(*
  val _ = case (headerTyOp, footerTyOp) of (NONE, NONE) => ()
	  | _ => print "Found a header or footer!\n"
*)
  (* val _ = print "Phase one ...\n"; *)
  val ty1 = Reduce.reduce 1 NONE body
  val headers= map (Reduce.reduce 1 NONE) headers
  val footers= map (Reduce.reduce 1 NONE) footers
  val reduce1_time : Time.time = Time.now ()
(*
  val _ = printTy ty1  
*)
(*phase two*) 
  (* val _ = print "Phase two ...\n"; *)
  val ty2 = Reduce.reduce 2 NONE ty1 
  val headers = map (Reduce.reduce 2 NONE) headers 
  val footers = map (Reduce.reduce 2 NONE) footers 
  val reduce2_time : Time.time = Time.now ()
  (*phase three, redo constraint-free reduction *)
(*
  val _ = print "After phase 2...\n"
  val _ = printTy ty2
*)
  (* val _ = print "Phase three ...\n"; *)
  val ty3 = (if mode = 0 then Reduce.reduce 3 NONE ty2 else Reduce.reduce 6 NONE ty2)
  val headers = if mode = 0 then map (Reduce.reduce 3 NONE) headers 
		else map (Reduce.reduce 6 NONE) headers
  val footers = if mode = 0 then map (Reduce.reduce 3 NONE) footers 
		else map (Reduce.reduce 6 NONE) footers 
  val ty3 = sortUnionBranches ty3
  val headers = map sortUnionBranches headers
  val footers = map sortUnionBranches footers
  val reduce3_time : Time.time = Time.now ()

  val finalTy = case auxOp of
	SOME aux => Punion(aux, headers @ [ty3] @ footers)
	| NONE => ty3
  val measured_reduced_ty = measure 1 finalTy
  val measured2_time : Time.time = Time.now ()

  val _ = print "\nRefined Ty:\n"
  val _ = printTy measured_reduced_ty
(*
  val t1 = Editdistance.numNodes initTy
  val t2 = Editdistance.numNodes measured_reduced_ty
  val _ = print ("size of init tree: " ^  Int.toString t1 ^ "\n")
  val _ = print ("size of final tree: " ^ Int.toString t2 ^ "\n")
  val _ = print ("order of time/space: " ^ Int.toString (t1 * t1 * t2 * t2) ^ "\n")
*)
  (*
  val d = Editdistance.treeEditDistance (initTy, measured_reduced_ty)
  val _ = print ("\n*** Edit distance between initTy and finalTy = " ^ Int.toString d ^ "\n")
  *)
(*
  val dist = Editdistance.editDist([initTy], [measured_reduced_ty])
  val _ = print ("Edit distance = " ^ Int.toString dist ^ "\n")
*)

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

in (initTy, measured_reduced_ty, length headers, length footers, endingTimes)
end

end

