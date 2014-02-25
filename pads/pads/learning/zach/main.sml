(*
Zach DeVito
This structure combines the various parts into a single unit
*)
structure Main = struct
open Common

(* reports the results in the constraint map. Prunes unused labels from the 
report to make it simpler *)

fun report ir cmap = 
let
  fun inUseLabels ir =
  let
  	val labSet = ref(Label.Set.empty)
  	fun update ir =
  	  case ir of
  	    Tuple irlist => (map update irlist;())
  	  | Sum irlist => (map update irlist;())
  	  | Array ir'' => update ir''
  	  | Base b => ()
  	  | Label(id,ir') => (labSet := Label.Set.add(!labSet,id); update ir')
  	val _ = update ir
  in
  	!labSet
  end
  val labSet = inUseLabels ir
  val used_cmap = Label.Map.filteri (fn(id,consts) => Label.Set.member(labSet,id)) cmap
in (
    print ("Report\n\nIR:\n" ^ (Common.irtos ir) ^ "\nConstraints:\n") ;
    printConstMap used_cmap
   )
end

(* takes an IR and the data parsed using it; constrains it; and then uses the
information to reduce it *)
fun run ((desc,name), data) = 
	(
		print ("Running: " ^ name ^ "\n");
		let
		  (*val (sIR,_) = Reduce.reduce NONE desc*)
		  val (labIR,cmap, usedLabels) = Constraint.constrain'(desc,data)
		  val sIR2 = Reduce.reduce (SOME(cmap,usedLabels)) labIR
		  val _ = if Options.print_report then report sIR2 cmap else ()
		  val _ = if Options.reparse_data then (Interp.interpFile(sIR2, name);()) else ()
		in
		  ()
		end
	)
(* runs analysis using an IR to parse fileName *)
fun main'(ir : IR, fileName : string) =
let
  val ir2 = Reduce.reduce NONE ir
  val data = Interp.interpFile(ir2, fileName)
  val (labIR, cmap, usedLabels) = Constraint.constrain'(ir2,data);
  val ir3 = Reduce.reduce (SOME(cmap,usedLabels)) labIR
  val _ = if Options.print_report then report ir3 cmap else ()
  val _ = if Options.reparse_data then (Interp.interpFile(ir3, fileName);()) else ()
in
  (ir3,cmap,usedLabels)
end
(* runs analysis on a few test data formats *)
fun main() = 
let
	val examples = [(Desc.test1_desc, "test1"),
					(Desc.test2_desc, "test2"),
					(Desc.test3_desc, "test3"),
					(Desc.ls_desc, "lsd"),
					(Desc.plist_desc, "com.apple.Terminal.xml"),
					(Desc.apache_desc, "apache_small"),
					(Desc.eg, "example.txt")
					]
	(* simplify the original format then parse the file *)
	val examples = map (fn (x,y) => ((Reduce.reduce NONE x),y)) examples
	val data = map Interp.interpFile examples
	val eg_with_data = ListPair.zip(examples, data)
in
 (run (List.nth (eg_with_data,5) ); ()) (* replace the number to run a differen eg *)
end
(*val _ = TimeIt.time main *)
(* val _ = OS.Process.exit(OS.Process.success); *)
end
