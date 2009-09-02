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
    exception InvalidFlags
    fun silenceGC () = (SMLofNJ.Internals.GC.messages false)
    open Config
    open Types
    open Structure
    open Model
    open Options
    open Common

    structure AG = Aggregate
    structure StringMap = Parse.StringMap

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
	
    (* return the first n lines of data from the file as the initial chunk *)
    (* previous implementation returns n/2 lines from the top and n/2 lines from
       the bottom of the file - this is not so appropriate when the file is really
       large as it has to scan the file multiple times *)
    fun get_learn_chunk (file, size) =
	(*
	let val hd_sz = size div 2
	    val tail_sz = size - hd_sz
	    val total_sz = get_num_lines file
	    val hdr_lines = get_cont_lines file 0 hd_sz
	    val tail_lines = get_cont_lines file (total_sz - tail_sz) tail_sz
	in hdr_lines @ tail_lines
	end
	*)
   	get_cont_lines file 0 size

    (* each aggregate is actually a pair: (aggr, OptsTable) *)
    fun add (ty, line, aggregates) =
      let 
	(* val _ = print (line ^ "\n")  *) 
	val tm = Time.now() 
	val _ = Parse.memo:=Parse.MemoMap.empty
 	val set = Parse.parse_all(ty, LabelMap.empty, 0, line, !Parse.do_parse_cutoff)
        val set = if Parse.ParseSet.numItems set = 0 then
		  (Parse.memo:=Parse.MemoMap.empty;
		  Parse.parse_all(ty, LabelMap.empty, 0, line, false))
		  else set
	(*
	val _ = print ("Time to parse: " ^ Time.toString (Time.- (Time.now(), tm)) ^ "\n") 
        val _ = print ("Size of tMap = " ^ Int.toString (Parse.TokenMap.numItems Parse.tmap) ^ "\n")
        val _ = print ("Size of tokenRegexMap = " ^ Int.toString (Parse.TokenMap.numItems (!Parse.tokenRegexMap)) ^ "\n")
        val _ = print ("Size of strRegexMap = " ^ Int.toString (Parse.StringMap.numItems (!Parse.strRegexMap)) ^ "\n")
        val _ = print ("Size of memo = " ^ Int.toString (Parse.MemoMap.numItems (!Parse.memo)) ^ "\n")
	*)
	(* val _ = print "Parse complete\n" *)
	(* val _ = print ("Number of parses: " ^ Int.toString (Parse.ParseSet.numItems set) ^ "\n") *)
	val list_parses = Parse.ParseSet.listItems set
	val list_parses = map (fn (r, m, j) => 
		let val len = String.size line 
		in if j < len then (r, Rep.add_metric m (2, len-j, 0), j)
		   else (r, m, j)
		end) list_parses
	(* if there are some perfect parses, only take those *)
	val perfect_parses = List.filter (fn (r, m, j) => (#1 m) = 0) list_parses
	(* val _ = print ("Num of perfect parses: " ^ Int.toString (length perfect_parses) ^ "\n") *)
	val (top_parses, has_good_parse) =
		if List.length perfect_parses > 0 then
			if List.length perfect_parses <= max_parses_per_line then
			  (perfect_parses, true)
			else (List.take (perfect_parses, max_parses_per_line), true)
		else
		  let 
		    val num_parses =  
		        let val len = length list_parses 
		        in if len > max_parses_per_line then max_parses_per_line
		           else len
			end
		  in
		     (List.take ((ListMergeSort.sort 
		     (fn ((_, m1, _), (_, m2, _)) => Rep.better_metric m2 m1) list_parses), num_parses), 
		      false)
		  end
(*
	val _ = print ("The top " ^ Int.toString (length top_parses) ^ " parses: \n")
 	val _ = List.app (fn (rep, m, j) => 
		print (Rep.repToString "" rep ^ "Metric = " ^ Rep.metricToString m ^ "\n\n")) 
		top_parses
	val _ = print ("Num of top parses: " ^ Int.toString (length top_parses) ^ "\n")  
*)
	val all_aggregates = List.concat (map (fn (AG.TupleA (cov, [a, AG.Ln(id, ss)]), table) => map 
			(fn (r, m, j) => 
			  let 
				(*
				val _ = print (AG.aggrToString "" a)
				val _ = print (Rep.repToString "" r)
				*)
				val (a', pairs) = (AG.merge a r)
				val remainder = String.extract (line, j, NONE)
				val (newa, newpairs) = 
				  if remainder = "" then 
					(AG.TupleA (cov+1, [a', AG.Ln(id, ss)]), (id, 0)::pairs)
				  else 
					(AG.TupleA (cov+1, [a', AG.Ln (id, ss@[remainder])]), 
						(id, 1)::pairs)
				val sorted_pairs = ListMergeSort.sort (
				  fn ((id1, _), (id2, _)) => (Atom.compare (id1, id2) = GREATER))
				  newpairs
				val table' = AG.addToTable table sorted_pairs
	(*	
			        val _ = AG.printTableSize table'
			      val _ = print (AG.aggrToString "" newa)
			      val _ = print ("Cost = " ^ Real.toString (AG.cost newa) ^ "\n")
	*)
			  in (newa, table')
			  end)
				top_parses) aggregates)
	(* val _ = print ("# of all_aggregates = " ^ Int.toString (length all_aggregates) ^ "\n") *)

	val sorted_aggregates = ListMergeSort.sort
		(fn ((a1, _), (a2, _)) => AG.cost a1 > AG.cost a2) all_aggregates
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
	  (top_aggregates, has_good_parse)
       end 

(*
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
***)


     fun parse_single_file (initTy, numHeaders, numFooters, filename, chunksize, dir, start_pos) =
      let

	   val logFile = dir ^ "/" ^ filename ^ ".log"
	   val strm = TextIO.openIn filename
	   val index = ref 0 
	   val count = ref 0
	   val badcount = ref 0
	   val eof = ref false
	   val myTy = ref initTy
	   val begin_time = Time.now()
	   val init_aggr = AG.TupleA (0, [AG.initialize initTy, AG.Ln(mkNextTyLabel(), nil)])
	   val init_table = AG.initTable()
	   val aggrs = ref [(init_aggr, init_table)]
	   val start_time = ref begin_time
	   (* skip the first start_pos lines from the input file *)
	   val skip = ref 0
	   val _ = while (not (!eof) andalso (!skip) < start_pos) do
		case TextIO.inputLine strm of
	  	  SOME x => skip:=(!skip) + 1
		  | _ => eof := true

	   fun output aggrs ty start_time index count =
		  let 
	     	    val (chunk_aggr, table) = if length aggrs = 0 then
				(print "Warning! Number of aggregates is 0!\n"; (init_aggr, init_table))
			      else hd aggrs
	     	    val chunk_cost = AG.cost chunk_aggr
		(*
	     	    val _ = (print "The Best Aggregate:\n"; print (AG.aggrToString "" chunk_aggr)) 
	     	    val _ = print ("Cost of Best Aggregation = " ^ Int.toString chunk_cost ^ "\n")
		    val _ = AG.printTable table 
		*)
		    val trans_map = AG.transpose table
(*
		    val _ = LabelMap.appi (fn (id, l) => (print ((Atom.toString id) ^ ": " ^
				(String.concat (map (Int.toString) l))); print "\n")) trans_map
*)
		    val newTy = (AG.updateTy ty chunk_aggr)
	     	    val newTy = Reduce.reduce 5 newTy 
		    val newTy = AG.merge_adj_options trans_map newTy
		    val newTy = AG.alt_options_to_unions trans_map newTy
		    val newTy = Reduce.reduce 5 newTy
		    (* val _ = (print "Updated newty:\n"; printTy newTy) *)
		    val elapse = Time.- (Time.now(), start_time)
		    (* val refinedTy = Reduce.reduce 4 newTy *)
(*

		    val tyFile = (dir ^ "/" ^ filename ^ ".chunk" ^ Int.toString index ^ ".ty") 
		    val tystrm = TextIO.openOut tyFile
	   	    val _ = print ("Output IR to " ^ tyFile ^ "\n")
		    val _ = TextIO.output (tystrm, TyToStringD "" false false  "\n" refinedTy)
		    val _ = TextIO.closeOut tystrm
		    val padscFile = dir ^ "/" ^ filename ^ ".chunk" ^ Int.toString index ^ ".p"
	   	    val _ = print ("Output PADS description to " ^ padscFile ^ "\n")
	   	    val padsstrm = TextIO.openOut padscFile
	   	    val desc = #5 (Padsc_printer.tyToPADSC refinedTy numHeaders numFooters 
				((!lexName)^ ".p"))
	   	    val _ = TextIO.output (padsstrm, desc)
	   	    val _ = TextIO.closeOut padsstrm
*)
	   	    val _ = print ("Time elapsed: " ^ Time.toString elapse ^ " secs\n")
	   	    val msg = "Chunk " ^ Int.toString index ^ 
			" (" ^ Int.toString(count) ^ " lines): Aggregate Cost = " ^ 
			(Int.toString chunk_cost) ^ 
			"\tTime elapsed = " ^ Time.toString elapse ^ " secs\n"
	   	    val logstrm = TextIO.openAppend logFile
	   	    val _ = TextIO.output (logstrm, msg)
	   	    val _ = TextIO.closeOut logstrm
		  in newTy
		  end

	   val _ = while not (!eof) do
		(
		if (!badcount) = chunksize then
		    let val newTy = output (!aggrs) (!myTy) (!start_time) (!index) (!count)
		    in
		     myTy := newTy; count := 0; badcount := 0; index := (!index) + 1;
		     start_time:=Time.now();
	     	     aggrs := [(AG.TupleA (0, [AG.initialize newTy, AG.Ln(mkNextTyLabel(), nil)]), 
				AG.initTable())]
		    end 
		else ();
		(*
		if (Int.mod(!count, 1000) = 0) then print ((Int.toString (!count)) ^ "\n")
		else ();
		*)
		case TextIO.inputLine strm of
		  SOME x => 
			let 
			    val (aggrs', good_data) = add (!myTy, remove_newline x, !aggrs)
			    val _ = aggrs := aggrs'
			in 
			  if (not good_data) then
			    badcount:=(!badcount) + 1
			  else (); 
			  count:=(!count) + 1
			end
		| NONE => 
		        (myTy := output (!aggrs) (!myTy) (!start_time) (!index) (!count);
			eof:=true) 
		)
	   val _ = TextIO.closeIn strm
	   val logstrm = TextIO.openAppend logFile
	   val finalTy = sortUnionBranches (Reduce.reduce 4 (!myTy)) 
	   val total_elapse = Time.- (Time.now(), begin_time)
	   val _ = (print "**** Final Ty: \n"; printTy (measure 0 finalTy))
	   val tycomp = getComps finalTy
	   val _ = print ("Final comps = (" ^ showBits (#tc tycomp) ^ ", " ^ 
			showBits (#dc tycomp) ^ ", " ^
			(Real.toString (Reduce.score finalTy)) ^ ")\n")
	   val msg = "Total time = " ^ Time.toString total_elapse ^ " secs\n"
	   val _ = TextIO.output (logstrm, msg)
	   val _ = TextIO.closeOut logstrm
	   val _ = print msg
		   
	   val tyFile = (dir ^ "/" ^ filename ^ ".ty") 
	   val tystrm = TextIO.openOut tyFile
	   val _ = print ("Output IR to " ^ tyFile ^ "\n")
	   val _ = TextIO.output (tystrm, TyToStringD "" false false  "\n" finalTy)
	   val _ = TextIO.closeOut tystrm
	   (* val finalTy = (!myTy)*) 

	   val padscFile = dir ^  "/" ^ filename ^ ".p"
	   val pmlFile = dir ^  "/" ^ filename ^ ".pml"
	   val _ = print ("\nOutput final PADS description to " ^ padscFile ^ "\n")
	   val (topName, hdrName, tyName, trlName) = Printing.dumpPADSdesc padscFile 
				pmlFile finalTy numHeaders numFooters
	   val _ = Printing.dumpAccumProgram (dir ^ "/") filename hdrName tyName trlName
	   val _ = Printing.cpFile (dir ^ "/") "GNUmakefile" "GNUmakefile.output"
	   val _ = Printing.cpFile (dir ^ "/") "vanilla.p" "vanilla.p"
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

 
    fun loadArgs smap args =
	case args of
	  flag::v::args => 
	   if String.sub (flag, 0) = #"-" then 
		let val smap' = StringMap.insert (smap, flag, v)
		in loadArgs smap' args
		end
	   else raise InvalidFlags
	| nil => smap

    fun main (cmd, args) = 
     (
     if length args < 1 then
	(print ("Usage: increment -f ORIG_DATA_FILE [-i INIT_SIZE (500)] [-l INC_SIZE (100)] \n" ^ 
	"[-d INIT_DESC_XML] [-opt OPT_LEVEL (3)] [-tmout SECS (1800)] [-w ADC_WEIGHT (5)]\n" ^
	"[-p FILE_TO_PARSE] [-output OUTPUT_DIR]\nSizes are in # of lines\n");
	anyErrors := true)
     else
       let
	 val argMap = loadArgs StringMap.empty args
	 val learn_file = valOf (StringMap.find (argMap, "-f"))
	 val learnsize = case StringMap.find (argMap, "-i") of
		  NONE => default_init_size
		| SOME x => valOf (Int.fromString x)
	 val chunksize = case StringMap.find (argMap, "-l") of
		  NONE => default_chunk_size
		| SOME x => valOf (Int.fromString x)
	 val opt_level  = case StringMap.find (argMap, "-opt") of
			  NONE => 3 (* highest optimization level *)
			| SOME x => valOf(Int.fromString x)
	 val _ = case StringMap.find (argMap, "-tmout") of
		 NONE => ()
		 | SOME x => learn_timeout := valOf(Int.fromString x)

	 val _ = if opt_level = 0 then
			(
			Parse.do_clean:=false;
			Parse.do_parse_cutoff:=false;
			Parse.do_memo:=false
			)
		 else if opt_level = 1 then
			(
			Parse.do_clean:=true;
			Parse.do_parse_cutoff:=false;
			Parse.do_memo:=false
			)
		 else if opt_level = 2 then
			(
			Parse.do_clean:=true;
			Parse.do_parse_cutoff:=true;
			Parse.do_memo:=false
			)
		 else () 
	 val _ = case StringMap.find(argMap, "-w") of
		NONE => ()
		| SOME x =>  adcCoeff:= valOf (Real.fromString (x))
	 val (parse_file, start_pos) = 
		case StringMap.find (argMap, "-p") of
		  NONE => (learn_file, learnsize)
		| SOME x => (x, 0)
	 val learn_file_name = OS.Path.file learn_file

	 val pxml = StringMap.find (argMap, "-d") 
	 val start_pos = case pxml of
			SOME _ => 0
			| _ =>  start_pos

(*
         val varcard = StringMap.find (argMap, "-varcard")
	 val _ = case varcard of
		   SOME x => var_card_bits := valOf(Bool.fromString x)
		| _ => ()
*)

         val _ = executableDir :=
		(case (OS.Process.getEnv "LEARN_HOME") of
		  SOME x => x
		| NONE => "")
	 val timedir = 
	   case StringMap.find(argMap, "-output") of 
	     NONE => 
	       let 
		 (* create a directory to store the .p files *)
	 	 val _ = if (OS.FileSys.isDir dir_name handle SysErr => (OS.FileSys.mkDir dir_name; true))
		    then () else ()
	 	 val subdir = (dir_name ^ "/" ^ learn_file_name)
	 	 val _ = if (OS.FileSys.isDir subdir handle SysErr => (OS.FileSys.mkDir subdir; true))
		    then () else ()
	 	 val timestamp= Date.fmt "%Y-%m-%d_%H-%M-%S" (Date.fromTimeLocal (Time.now()))
	       in
		 subdir ^ "/" ^ timestamp
	       end
	   | SOME x => x
	 val _ = if (OS.FileSys.isDir timedir handle SysErr => (OS.FileSys.mkDir timedir; true))
		 then () else ()
 	 (* val _ = List.app (fn s => print (s ^ "\n")) learn_lines *)
	 (*
	 val otherfiles = List.tabulate (10, (fn n => learn_file_name ^ ".chunk" ^ Int.toString n))
	 *)
         val _ = Posix.Process.alarm(Time.fromSeconds(LargeInt.fromInt(!learn_timeout)))
	 val (_, initTy, numHeaders, numFooters, _) = 
		case pxml of 
		  NONE =>
		  let val learn_lines = get_learn_chunk (learn_file, learnsize)
		  in
			Rewrite.run (Times.zeroEndingTimes()) 1 
			(measure 0 (#1 (computeStructurefromRecords learn_lines)))
		  end
		| SOME xmlfile =>
		  let val xml = PxmlParse.loadXML xmlfile
		      val ty = Pxml.xmlToIR Pxml.StringMap.empty xml
		      (* the ty decribes the entire data, we need a ty that describes just one record *)
		      val ty = case ty of
				  RArray (_, _, _, body, _, _) => body
				| _ => ty
		      val _ = printTy (measure 0 ty)
		  in
			(ty, (measure 0 ty), 0, 0, Times.zeroEndingTimes ())
			(* ;raise TyMismatch *)
		  end
	 (*  
	 val (initTy, numHeaders, numFooters) = (valOf (Gold.getGold "irvpiv1.tail.sel"), 0, 0)
	 val (initTy, numHeaders, numFooters) = (valOf (Gold.getGold "ai.3000"), 0, 0) 
	 val (_, initTy) = Populate.initializeTy LabelMap.empty initTy 
	 *)

         val padscFile = timedir ^ "/" ^ learn_file_name ^ ".init.p"
         val _ = print ("\nOutput initial PADS description to " ^ padscFile ^ "\n")
         val padsstrm = TextIO.openOut padscFile
         val desc = #5 (Padsc_printer.tyToPADSC initTy numHeaders numFooters ((!lexName)^ ".p"))
         val _ = TextIO.output (padsstrm, desc)
         val _ = TextIO.closeOut padsstrm

         val logFile = timedir ^ "/" ^ parse_file ^ ".log"
         val logstrm = TextIO.openOut logFile
         val _ = TextIO.output (logstrm, "Learn Chunk = " ^ Int.toString learnsize ^ 
                 	" lines\nParse Chunk = " ^ Int.toString chunksize ^ " bad lines\n\n")
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
         parse_single_file (initTy, numHeaders, numFooters, parse_file, chunksize, timedir, start_pos) 
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

