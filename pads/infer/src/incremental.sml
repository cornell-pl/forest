structure Incremental: sig
  
    val main : (string * string list) -> OS.Process.status
    val emit : unit -> unit

  end = struct

    val anyErrors = ref false
    val max_int = 10000000
    val max_parses_per_line = 2
    val max_aggregates = 10
    exception Exit of OS.Process.status
    fun silenceGC () = (SMLofNJ.Internals.GC.messages false)
    open Config
    open Types
    open Structure
    open Model
    open Options
    open Common

    fun main (cmd, args) = 
     (
     if length args <> 2 then
	(print "Usage: increment DESC FILENAME\n";
	anyErrors := true)
     else
       let 
	 val [descname, filename] = args
	 val lines = loadFile filename
	 val _ = print "loadFile complete \n"
	 val goldenTy : Ty  = Gold.getGolden descname
	 val _ = printTy goldenTy
	 val init_aggr = Aggregate.initialize goldenTy
	 val _ = print "Aggregate initialization complete \n"

	 (* invariant: number of aggregates <= max_aggregates *)
	 fun add (line, aggregates) =
	    let 
		val _ = print (line ^ "\n")
         	val set = Parse.parse_all(goldenTy, LabelMap.empty, 0, line)
		val _ = print "Parse complete\n"
		val list_parses = Parse.ParseSet.listItems set
		val num_parses = 
			let val len = length list_parses 
			in if len > max_parses_per_line then max_parses_per_line
			   else  len
			end
		val top_parses = List.take ((ListMergeSort.sort 
			(fn ((_, m1, _), (_, m2, _)) => m1 > m2) list_parses), num_parses)
		val _ = print ("The top " ^ Int.toString max_parses_per_line ^ " parses: \n")
	 	val _ = List.app (fn (rep, m, j) => 
			print (Rep.repToString "" rep ^ "Metric = " ^ Int.toString m ^ "\n\n")) 
			top_parses
		val all_aggregates = List.concat (map (fn a => map 
					(fn (r, m, j) => (Aggregate.merge a r))
					top_parses) aggregates)
		val _ = print ("After all aggr: " ^ Int.toString (length all_aggregates) ^ "\n")
		val sorted_aggregates = ListMergeSort.sort
			(fn (a1, a2) => Aggregate.cost a1 > Aggregate.cost a2) all_aggregates
		val num_to_take = 
			let val len = length sorted_aggregates 
			in if len > max_aggregates then max_aggregates
			   else len
			end
		val top_aggregates = List.take (sorted_aggregates, num_to_take)
(*
		val (min_a, min_c) = Parse.ParseSet.foldl 
				(fn ((r, m, j), (mina, minc: int)) => 
				  let 
					val newaggr = Aggregate.merge a r 
					val newcost = Aggregate.cost newaggr
				  in 
				    if newcost < minc then (newaggr, newcost)
				    else (mina, minc)
				  end) (Aggregate.BaseA nil, max_int) set 
*)
					(* use a dummy aggregate to start *)
	     in
		top_aggregates
	     end 	  
	     val final_aggr = hd (foldl add [init_aggr] lines)
       in
	 print "The Best Aggregate:\n";
	 print (Aggregate.aggrToString "" final_aggr);
	 print ("Cost of Best Aggregation = " ^ Int.toString (Aggregate.cost final_aggr) ^ "\n")
       end handle e =>(TextIO.output(TextIO.stdErr, concat[
		          "uncaught exception ", exnName e,
		          " [", exnMessage e, "]\n"
	                  ]); app (fn s => TextIO.output(TextIO.stdErr, concat[
		          "  raised at ", s, "\n"
	                  ])) (SMLofNJ.exnHistory e)); 
	 if !anyErrors then  OS.Process.exit(OS.Process.failure)          
	 else OS.Process.exit(OS.Process.success)
            handle  Exit r      => OS.Process.exit(OS.Process.failure)
                  | OS.SysErr(s, sopt) => (TextIO.output(TextIO.stdErr, 
					   concat[s,"\n"]); 
					   OS.Process.exit(OS.Process.failure))
                  | ex => (TextIO.output(TextIO.stdErr, concat[
		          "uncaught exception ", exnName ex,
		          " [", exnMessage ex, "]\n"
	                  ]);
			  app (fn s => TextIO.output(TextIO.stdErr, concat[
		          "  raised at ", s, "\n"
	                  ])) (SMLofNJ.exnHistory ex);
	                   OS.Process.exit(OS.Process.failure))
     )

    (* Generates the compiler and exports an executable. *)
    fun emit () = 
	    (silenceGC();
	     SMLofNJ.exportFn ("../lib/increment", main ))
  end; 

