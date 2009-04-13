structure Incremental: sig
  
    val main : (string * string list) -> OS.Process.status
    val emit : unit -> unit

  end = struct

(*
    val _ = Compiler.Profile.setProfMode true
    val _ = Compiler.Profile.setTimingMode true
    val _ = SMLofNJ.Internals.ProfControl.spaceProfiling:= true
*)

    val anyErrors = ref false
    val max_parses_per_line = Parse.max_parses_per_line
    val max_aggregates = 10
    val dir_name = "inc_output"
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
     if length args <> 1 then
	(print "Usage: increment ORIG_DATA_FILE\n";
	anyErrors := true)
     else
       let
	 val file_prefix = hd args
	 (* create a directory to store the .p files *)
	 val _ = if (OS.FileSys.isDir dir_name handle SysErr => (OS.FileSys.mkDir dir_name; true))
		 then () else () 
	 val subdir = (dir_name ^ "/" ^ file_prefix)
	 val _ = if (OS.FileSys.isDir subdir handle SysErr => (OS.FileSys.mkDir subdir; true))
		 then () else ()
	 val init_file = file_prefix ^ ".learn"
	 val otherfiles = List.tabulate (10, (fn n => file_prefix ^ ".chunk" ^ Int.toString n))
	 val (_, initTy, numHeaders, numFooters, _) = Rewrite.run (Times.zeroEndingTimes()) 
		(#1 (computeStructure [init_file]))

(* 
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
			| SOME ty => (#2 (Populate.initializeTy LabelMap.empty ty), value2)
		 else if (flag2 = "-g" andalso flag1 = "-p") then
			case Gold.getGold value2 of
			  NONE => raise (Fail "Golden description not found!")
			| SOME ty => (#2 (Populate.initializeTy LabelMap.empty ty), value1)
		 else (print "Usage: increment [-l FILE_TO_LEARN | -g GOLDEN_DESC] -p FILE_TO_PARSE\n"; 
			raise (Fail "Parameter error!"))
*)
			    
	 (* val goldenTy : Ty  = Gold.getGolden descname 
	 val _ = printTy goldenTy *)

	 
	 val start_time = Time.now()

	 fun inc_learn (filename, goldenTy) =
	   let
	     (* invariant: number of aggregates <= max_aggregates *)
	     val _ = print ("\n**** Incrementally learning datafile: " ^ filename ^ "...\n")
	     fun add (line, aggregates) =
	      let 
		(*
		val _ = print (line ^ "\n")  
		val tm = Time.now() 
		*)
		(*
		val _ = print ("size of tokenMap = " ^ Int.toString (Parse.TokenMap.numItems (!Parse.tokenRegexMap)) ^ "\n")
		val _ = print ("size of strMap = " ^ Int.toString (Parse.StringMap.numItems (!Parse.strRegexMap)) ^ "\n")
		*)
		val _ = Parse.memo:=Parse.MemoMap.empty
         	val set = Parse.parse_all(goldenTy, LabelMap.empty, 0, line)
		(* val _ = print ("Time to parse: " ^ Time.toString (Time.- (Time.now(), tm)) ^ "\n") *)
		(* val _ = print "Parse complete\n" *)
		(* val _ = print ("Number of parses: " ^ Int.toString (Parse.ParseSet.numItems set) ^ "\n") *)
		val list_parses = Parse.ParseSet.listItems set
		(* val _ = print ("Num of parses: " ^ Int.toString (length list_parses) ^ "\n") *)
		val list_parses = map (fn (r, m, j) => 
			let val len = String.size line 
			in if j < len then (r, Rep.add_metric m (2, len-j, 0), j)
			   else (r, m, j)
			end) list_parses
		(* if there are some perfect parses, only take those *)
		val perfect_parses = List.filter (fn (r, m, j) => (#1 m) = 0) list_parses
		(* val _ = print ("Num of perfect parses: " ^ Int.toString (length perfect_parses) ^ "\n") *)
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
		(* val _ = print ("Num of top parses: " ^ Int.toString (length top_parses) ^ "\n")  *)
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
		(* val _ = print ("After all aggr: " ^ Int.toString (length all_aggregates) ^ "\n") *)

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
		(* val _ = print ("Time to aggregate : " ^ Time.toString (Time.- (Time.now(), tm)) ^ "\n") *)
	       in
		  top_aggregates
	       end 

(*
	     fun wrapper n line aggrs =
		if n = 250001 then aggrs
		else 
		  let val new_aggrs = add (line, aggrs)
		      val _ = if n mod 5000 = 0 then 
	     			let val elapse = Time.- (Time.now(), start_time)
				in 
				  print (Int.toString n ^ " Records - Time elapsed: " ^ 
					Time.toString  elapse ^ " secs\n")
				end
			      else ()
		  in wrapper (n+1) line new_aggrs
		  end
	     val final_aggrs = wrapper 1 (hd lines) [init_aggr]
*)

	     val fstream = TextIO.openIn filename
	     (* val _ = print "loadFile complete \n" *)
	     val init_aggr = AG.TupleA [AG.initialize goldenTy, AG.Ln nil]
	     val aggrs = ref [init_aggr]
	     (* val _ = print "Aggregate initialization complete \n" *)
	     (* val final_aggrs = (foldl add [init_aggr] lines) *)
	     fun remove_newline line =
		let val s_line = Substring.full line
		    val s_line' = Substring.dropr (fn c => c = #"\n" orelse c = #"\r") s_line
		in Substring.string s_line'
		end
	     val eof = ref false
	     val _ = 
	       while (not (!eof)) do
	         case TextIO.inputLine fstream of
	           SOME line => aggrs:= add (remove_newline line, !aggrs)
	         | NONE => (eof:=true)
	     val final_aggr = if length (!aggrs) = 0 then
				(print "Warning! Number of aggregates is 0!\n"; init_aggr)
			      else hd (!aggrs)
	     val _ = (print "The Best Aggregate:\n"; print (AG.aggrToString "" final_aggr);
	 	      print ("Cost of Best Aggregation = " ^ Real.toString (AG.cost final_aggr) ^ "\n"))
	     val newTy = AG.updateTy goldenTy final_aggr
	     val _ = (print "**** Newly updated Ty: \n"; printTy newTy)
	     val padscFile = subdir ^ "/" ^ filename ^ ".p"
	     val _ = print ("Output PADS description to " ^ padscFile ^ "\n")
	     val padsstrm = TextIO.openOut padscFile
	     val desc = #5 (Padsc_printer.tyToPADSC newTy numHeaders numFooters ((!lexName)^ ".p"))
	     val _ = TextIO.output (padsstrm, desc)
	     val _ = TextIO.closeOut padsstrm
	   in
	     newTy
	   end
	   val finalTy = foldl inc_learn initTy otherfiles
	   val elapse = Time.- (Time.now(), start_time)
       in

	 print ("Time elapsed: " ^ Time.toString elapse ^ " secs\n")
	 (* Compiler.Profile.reportAll TextIO.stdOut *)
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

