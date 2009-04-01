structure Incremental: sig
  
    val main : (string * string list) -> OS.Process.status
    val emit : unit -> unit

  end = struct

    val anyErrors = ref false
    val max_parses_per_line = Parse.max_parses_per_line
    val max_aggregates = 10
    exception Exit of OS.Process.status
    fun silenceGC () = (SMLofNJ.Internals.GC.messages false)
    open Config
    open Types
    open Structure
    open Model
    open Options
    open Common

    structure AG = Aggregate

    fun main (cmd, args) = 
     (
     if length args <> 4 then
	(print "Usage: increment [-l FILE_TO_LEARN | -g GOLDEN_DESC] -p FILE_TO_PARSE\n";
	anyErrors := true)
     else
       let 
	 val [flag1, value1, flag2, value2] = args
         val (goldenTy, filename) = 
		if (flag1 = "-l" andalso flag2 = "-p") then 
			let val (ty, _) = computeStructure [value1]
			    val (_, ty, _, _, _) = 
                                 Rewrite.run (Times.zeroEndingTimes()) ty
			in (ty, value2)
			end
		 else if (flag2 = "-l" andalso flag1 = "-p") then 
			let val (ty, _) = computeStructure [value2]
			    val (_, ty, _, _, _) = 
                                 Rewrite.run (Times.zeroEndingTimes()) ty
			in (ty, value1)
			end
		 else if (flag1 = "-g" andalso flag2 = "-p") then
			case Gold.getGold value1 of
			  NONE => raise (Fail "Golden description not found!")
			| SOME ty => (ty, value2)
		 else if (flag2 = "-g" andalso flag1 = "-p") then
			case Gold.getGold value2 of
			  NONE => raise (Fail "Golden description not found!")
			| SOME ty => (ty, value1)
		 else (print "Usage: increment [-l FILE_TO_LEARN | -g GOLDEN_DESC] -p FILE_TO_PARSE\n"; 
			raise (Fail "Parameter error!"))
			    
	 (* val goldenTy : Ty  = Gold.getGolden descname 
	 val _ = printTy goldenTy *)
	 val lines = loadFile filename
	 val _ = print "loadFile complete \n"
	 val start_time = Time.now()
	 val init_aggr = AG.TupleA [AG.initialize goldenTy, AG.Ln nil]
	 val _ = print "Aggregate initialization complete \n"

	 (* invariant: number of aggregates <= max_aggregates *)
	 fun add (line, aggregates) =
	    let 
		val _ = print (line ^ "\n") 
         	val set = Parse.parse_all(goldenTy, LabelMap.empty, 0, line)
		(* val _ = print "Parse complete\n" *)
		val list_parses = Parse.ParseSet.listItems set
		val list_parses = map (fn (r, m, j) => 
			let val len = String.size line 
			in if j < len then (r, Rep.add_metric m (2, len-j, 0), j)
			   else (r, m, j)
			end) list_parses
		(* if there are some perfect parses, only take those *)
		val perfect_parses = List.filter (fn (r, m, j) => (#1 m) = 0) list_parses
		val top_parses =
			if List.length perfect_parses > 0 then
				if List.length perfect_parses <= max_parses_per_line then
				  perfect_parses
				else List.take (perfect_parses, max_parses_per_line)
			else
			  let 
			    val num_parses =  
			        let val len = length list_parses 
			        in if len > max_parses_per_line then max_parses_per_line
			           else len
				end
			  in
			     List.take ((ListMergeSort.sort 
			     (fn ((_, m1, _), (_, m2, _)) => Rep.better_metric m2 m1) list_parses), num_parses)
			  end
		(*
		val _ = print ("The top " ^ Int.toString (length top_parses) ^ " parses: \n")
	 	val _ = List.app (fn (rep, m, j) => 
			print (Rep.repToString "" rep ^ "Metric = " ^ Rep.metricToString m ^ "\n\n")) 
			top_parses
		*)
		val all_aggregates = List.concat (map (fn (AG.TupleA [a, AG.Ln ss]) => map 
				(fn (r, m, j) => 
				  let 
					val remainder = String.extract (line, j, NONE)
					val newa = if remainder = "" then 
							AG.TupleA [(AG.merge a r), AG.Ln ss]
						   else 
							AG.TupleA [(AG.merge a r), AG.Ln (ss@[remainder])]
		(*	
				      val _ = print (AG.aggrToString "" newa)
				      val _ = print ("Cost = " ^ Real.toString (AG.cost newa) ^ "\n")
		*)
				  in newa
				  end)
					top_parses) aggregates)
		(*
		val _ = print ("After all aggr: " ^ Int.toString (length all_aggregates) ^ "\n")
		*)
		val sorted_aggregates = ListMergeSort.sort
			(fn (a1, a2) => AG.cost a1 > AG.cost a2) all_aggregates
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
					val newaggr = AG.merge a r 
					val newcost = AG.cost newaggr
				  in 
				    if newcost < minc then (newaggr, newcost)
				    else (mina, minc)
				  end) (AG.BaseA nil, some(Int.maxInt)) set 
*)
					(* use a dummy aggregate to start *)
	     in
		top_aggregates
	     end 	  
	     val final_aggrs = (foldl add [init_aggr] lines)
	     val final_aggr = if length final_aggrs = 0 then
				(print "Warning! Number of aggregates is 0!\n"; init_aggr)
			      else hd final_aggrs
	     val elapse = Time.- (Time.now(), start_time)
       in
	 print "The Best Aggregate:\n";
	 print (AG.aggrToString "" final_aggr);
	 print ("Cost of Best Aggregation = " ^ Real.toString (AG.cost final_aggr) ^ "\n");
	 print ("Time elapsed: " ^ Time.toString elapse ^ " secs\n")
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

