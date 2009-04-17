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
    fun remove_newline line =
	let val s_line = Substring.full line
	    val s_line' = Substring.dropr (fn c => c = #"\n" orelse c = #"\r") s_line
	in Substring.string s_line'
	end

    fun get_num_lines file =
	let val strm = TextIO.openIn file 
	    val eof = ref false
	    val count = ref 0
	    val _ = while not (!eof) do
			case TextIO.inputLine strm of
			  SOME x => count:=(!count) + 1
			| _ => eof := true
	    val _ = TextIO.closeIn strm
	in (!count)
	end

    fun get_cont_lines file start num =
	let val strm = TextIO.openIn file 
	    val count = ref 0
	    val lines = ref nil
	    val _ = while !count < start + num do
			case TextIO.inputLine strm of
			  SOME x => (if !count >= start then
					lines:= (!lines @ [remove_newline x])
				    else ();
				    count:=(!count) + 1)
			| NONE => ()
	    val _ = TextIO.closeIn strm
	in  !lines
	end
		
    fun get_learn_chunk (file, size) =
	let val hd_sz = size div 2
	    val tail_sz = size - hd_sz
	    val total_sz = get_num_lines file
	    val hdr_lines = get_cont_lines file 0 hd_sz
	    val tail_lines = get_cont_lines file (total_sz - tail_sz) tail_sz
	in hdr_lines @ tail_lines
	end

    fun add (ty, line, aggregates) =
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
 	val set = Parse.parse_all(ty, LabelMap.empty, 0, line)
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

       fun inc_learn (chunk, index, goldenTy) =
	   let
	     val start_time = Time.now()
	     (* invariant: number of aggregates <= max_aggregates *)
	     val _ = print ("\n**** Incrementally learning from chunk No. " ^ 
		Int.toString index ^ "...\n")
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

	     (* val _ = print "loadFile complete \n" *)
	     val init_aggr = AG.TupleA [AG.initialize goldenTy, AG.Ln nil]
	     (* val _ = print "Aggregate initialization complete \n" *)
	     val final_aggrs = (foldl (fn (line, aggrs) => add (goldenTy, line, aggrs)) 
			[init_aggr] chunk)
	     val final_aggr = if length final_aggrs = 0 then
				(print "Warning! Number of aggregates is 0!\n"; init_aggr)
			      else hd (final_aggrs)
	     val final_cost = AG.cost final_aggr
	     val (newTy, changed) = 
		if final_cost = 0 andalso AG.equal_aggr(init_aggr, final_aggr) then 
		(* no change to the description *)
		  (print "**** No Change to Description!\n";
		   (goldenTy, false))
		else 
		  let  
	     	    val _ = (print "The Best Aggregate:\n"; print (AG.aggrToString "" final_aggr)) 
	     	    val _ = print ("Cost of Best Aggregation = " ^ Int.toString final_cost ^ "\n")
	     	    val newTy = Reduce.reduce 3 (AG.updateTy goldenTy final_aggr)
	     	    val _ = (print "**** Newly updated Ty: \n"; printTy newTy)
		  in (newTy, true)
		  end
	   in
	     (newTy, final_cost, changed)
	   end

     fun parse_single_file (initTy, numHeaders, numFooters, filename, chunksize, dir) =
      let

	   val logFile = dir ^ "/" ^ filename ^ ".log"

	   val strm = TextIO.openIn filename
	   val index = ref 0 
	   val count = ref 0
	   val lines = ref nil
	   val eof = ref false
	   val myTy = ref initTy
	   val begin_time = Time.now()


	   val _ = while not (!eof) do
		(
		if (!count) = chunksize orelse !eof = true then
		  let 
	   	    val start_time = Time.now()
		    val (newTy, final_cost, changed) = inc_learn (!lines, !index, !myTy) 
		    val elapse = Time.- (Time.now(), start_time)
		    val _ = 
		      if changed then
		      let
			val tyFile = dir ^ "/" ^ filename ^ ".chunk" ^ Int.toString (!index) ^ ".ty"
	   		val _ = print ("Output IR to " ^ tyFile ^ "\n")
			val tystrm = TextIO.openOut tyFile
			val _ = TextIO.output (tystrm, TyToStringD "" false false  "\n" newTy)
			val _ = TextIO.closeOut tystrm

			val padscFile = dir ^ "/" ^ filename ^ ".chunk" ^ Int.toString (!index) ^ ".p"
	   		val _ = print ("Output PADS description to " ^ padscFile ^ "\n")
	   		val padsstrm = TextIO.openOut padscFile
	   		val desc = #5 (Padsc_printer.tyToPADSC newTy numHeaders numFooters 
				((!lexName)^ ".p"))
	   		val _ = TextIO.output (padsstrm, desc)
	   		val _ = TextIO.closeOut padsstrm
		      in () end
		      else ()
	   	    val _ = print ("Time elapsed: " ^ Time.toString elapse ^ " secs\n")
	   	    val msg = "Chunk " ^ Int.toString (!index) ^ 
			" (" ^ Int.toString(!count) ^ " lines): Aggregate Cost = " ^ 
			(Int.toString final_cost) ^ 
			"\tTime elapsed = " ^ Time.toString elapse ^ " secs  " ^
			(if changed then 
				 "(desc changed!)\n" else 
				 "\n") 
	   	    val logstrm = TextIO.openAppend logFile
	   	    val _ = TextIO.output (logstrm, msg)
	   	    val _ = TextIO.closeOut logstrm
		  in
		    (myTy := newTy; count := 0; lines := nil; index := (!index) + 1) 
		  end
		else ();
		case TextIO.inputLine strm of
		  SOME x => (lines:= (!lines @ [remove_newline x]);
			     count:=(!count) + 1)
		| NONE => eof:=true 
		)
	   val total_elapse = Time.- (Time.now(), begin_time)
	   val _ = TextIO.closeIn strm
	   val logstrm = TextIO.openAppend logFile
	   val msg = "\nTotal time elapsed = " ^ Time.toString total_elapse ^ " secs\n"
	   val _ = TextIO.output (logstrm, msg)
	   val _ = TextIO.closeOut logstrm
		   
	   val finalTy = Reduce.reduce 4 (!myTy) 

	   val _ = (print "**** Final Ty: \n"; printTy finalTy)
	   val _ = print msg
	   val _ = print ("Log written to " ^ logFile ^ ".\n")
	   (* val finalTy = foldl inc_learn initTy otherfiles *)
(*
	     val fstream = TextIO.openIn filename
	     val init_aggr = AG.TupleA [AG.initialize ty, AG.Ln nil]
	     val aggrs = ref [init_aggr]
	     val eof = ref false
	     val _ = 
	       while (not (!eof)) do
	         case TextIO.inputLine fstream of
	           SOME line => aggrs:= add (ty, remove_newline line, (!aggrs))
	         | NONE => (eof:=true)
*)
	  in ()
	  end



    fun main (cmd, args) = 
     (
     if length args < 3 then
	(print "Usage: increment ORIG_DATA_FILE LEARN_SIZE PARSE_SIZE [FILE_TO_PARSE]\nSizes are in # of lines\n";
	anyErrors := true)
     else
       let
	 val [learn_file, ls, cs] = List.take (args, 3)
	 val parse_file = if length args = 4 then (List.last args)
			  else learn_file
	 val learnsize = valOf (Int.fromString ls)
	 val chunksize = valOf (Int.fromString cs)
	 (* create a directory to store the .p files *)
	 val _ = if (OS.FileSys.isDir dir_name handle SysErr => (OS.FileSys.mkDir dir_name; true))
		 then () else ()
	 val subdir = (dir_name ^ "/" ^ learn_file)
	 val _ = if (OS.FileSys.isDir subdir handle SysErr => (OS.FileSys.mkDir subdir; true))
		 then () else ()
	 val timestamp= Date.fmt "%Y-%m-%d_%H-%M-%S" (Date.fromTimeLocal (Time.now()))
	 val timedir = subdir ^ "/" ^ timestamp
	 val _ = if (OS.FileSys.isDir timedir handle SysErr => (OS.FileSys.mkDir timedir; true))
		 then () else ()
	 val learn_lines = get_learn_chunk (learn_file, learnsize)
	 (*
	 val otherfiles = List.tabulate (10, (fn n => learn_file ^ ".chunk" ^ Int.toString n))
	 *)
	 
	 val (_, initTy, numHeaders, numFooters, _) = Rewrite.run (Times.zeroEndingTimes()) 
		(#1 (computeStructurefromRecords learn_lines))

         val padscFile = timedir ^ "/" ^ learn_file ^ ".init.p"
         val _ = print ("\nOutput initial PADS description to " ^ padscFile ^ "\n")
         val padsstrm = TextIO.openOut padscFile
         val desc = #5 (Padsc_printer.tyToPADSC initTy numHeaders numFooters ((!lexName)^ ".p"))
         val _ = TextIO.output (padsstrm, desc)
         val _ = TextIO.closeOut padsstrm

         val logFile = timedir ^ "/" ^ parse_file ^ ".log"
         val logstrm = TextIO.openOut logFile
         val _ = TextIO.output (logstrm, "Learn Chunk = " ^ Int.toString learnsize ^ 
                 	" lines\nParse Chunk = " ^ Int.toString chunksize ^ " lines\n\n")
         val _ = TextIO.closeOut logstrm

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

       in
         parse_single_file (initTy, numHeaders, numFooters, parse_file, chunksize, timedir) 
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

