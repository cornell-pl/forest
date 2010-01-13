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
    exception InvalidAggrs
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
   	    val eof = ref false
	    val _ = while (!count < start + num andalso not(!eof)) do
			case TextIO.inputLine strm of
			  SOME x => (if !count >= start then
					lines:= (!lines @ [remove_newline x])
				    else ();
				    count:=(!count) + 1)
			| NONE => eof:=true
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
	(* val _ = print (line ^ "\n") *)
	val tm = Time.now() 
	val _ = Parse.memo:=Parse.MemoMap.empty
 	val set = Parse.parse_all(ty, LabelMap.empty, 0, line, !Parse.do_parse_cutoff)
	(* val _ = print ("Number of parses: " ^ Int.toString (Parse.ParseSet.numItems set) ^ "\n") *)
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
	val list_parses = Parse.ParseSet.listItems set
	val list_parses = map (fn (r, m, j) => 
		let val len = String.size line 
		in if j < len then 
		(* if the line is not completely consumed, we need to penalize the metric *)
			(r, Rep.add_metric m (1, 0, len-j, 0), j)
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
				(*
				val _ = print "after merge:\n"
				val _ = print (AG.aggrToString "" a')
				val _ = List.app (fn (id, branch) => print ("(" ^ Atom.toString id ^ ", " ^
					Int.toString branch ^ ") ")) pairs
				val _ = print "\n"
				*)
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

     (* this function reparses the data files using the learned description - no learning is done *)
     fun reparse_files(ty, filepaths) =
      let 
	val init_aggr = AG.TupleA (0, [AG.initialize ty, AG.Ln(mkNextTyLabel(), nil)])
	val init_table = AG.initTable()
	val init_aggrs = [(init_aggr, init_table)]
	fun reparse_one_file (filepath, (a, c, bc)) =
	 let
   	   val eof = ref false
 	   val badcount = ref bc
	   val count = ref c 
	   val aggrs = ref a
	   val strm = TextIO.openIn filepath
	   val _ = while not (!eof) do
	       (
		case TextIO.inputLine strm of
		  SOME x => 
			let 
			    val (aggrs', good_data) = add (ty, remove_newline x, !aggrs)
			    val _ = aggrs := aggrs'
			    val _ = count := (!count)+1
			    val _ = if  (not good_data) then badcount:=(!badcount)+1
				    else ()
			in ()
			end
		| NONE => 
			eof:=true 
		)
   	    val _ = TextIO.closeIn strm
(*
	   val (aggr, _ ) = hd (!aggrs)
     	   val cost = AG.cost aggr
	   val _ = print ("The Best Aggregate:\n" ^ (AG.aggrToString "" aggr)) 
	   val _ = print ("Cost of Best Aggregation = " ^ Int.toString cost ^ "\n")
*)
	in (!aggrs, !count, !badcount)
	end
      val (_, count, badcount)=  List.foldl reparse_one_file (init_aggrs, 0, 0) filepaths
     in (count, badcount)
     end

     fun output aggrs ty start_time index count logFile =
	  let 
     	    val (chunk_aggr, table) = if length aggrs = 0 then
			(print "Warning! Number of aggregates is 0!\n"; raise InvalidAggrs)
		      else hd aggrs
     	    val chunk_cost = AG.cost chunk_aggr
	    (*
     	    val _ = (print "The Best Aggregate:\n"; print (AG.aggrToString "" chunk_aggr)) 
     	    val _ = print ("Cost of Best Aggregation = " ^ Int.toString chunk_cost ^ "\n")
	    *)
	    (* val _ = AG.printTable table *)
	    (* we update the ty even if there's no bad data in the
		aggregate because we want the updated aux in ty *)
	    val newTy = AG.updateTy ty chunk_aggr
	    val newTy =
	    if chunk_cost > 0 then 
	     let 
	       val trans_map = AG.transpose table
     	       val newTy = Reduce.reduce 5 NONE newTy 
	       (* val _ = (print "Before merge_adj_options: \n"; printTy newTy) *)
	       val newTy = AG.merge_adj_options trans_map newTy
	       (* val _ = (print "After merge_adj_options: \n"; printTy newTy) *)
	       val newTy = AG.alt_options_to_unions trans_map newTy
	       val newTy = Reduce.reduce 5 NONE newTy
	     in newTy
	     end
	    else newTy

	    (* val _ = (print "Updated newty:\n"; printTy newTy) *)
	    (* val refinedTy = Reduce.reduce 4 newTy *)
	    val elapse = Time.- (Time.now(), start_time)
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

     fun parse_one_file (filepath, ty, prev_aggrs, logFile, chunksize, 
		stime, prev_index, prev_count, prev_badcount, start_pos) =
      let
	   val eof = ref false
	   val myTy = ref ty
	   val aggrs = ref prev_aggrs
	   val start_time = ref stime
           val index = ref prev_index
	   val count = ref prev_count
	   val badcount = ref prev_badcount
	   val skip = ref 0
	   val strm = TextIO.openIn filepath
	   (* skiping a number lines to start_pos *)
	   val _ = while (not (!eof) andalso (!skip) < start_pos) do
		case TextIO.inputLine strm of
	  	  SOME x => skip:=(!skip) + 1
		  | _ => eof := true

	   val _ = while not (!eof) do
		(
		if (!badcount) = chunksize then
		    let val newTy = output (!aggrs) (!myTy) (!start_time) (!index) (!count) logFile
		    in
		     myTy := newTy; count := 0; badcount := 0; index := (!index) + 1;
		     start_time:=Time.now();
	     	     aggrs := [(AG.TupleA (0, [AG.initialize newTy, AG.Ln(mkNextTyLabel(), nil)]), 
				AG.initTable())]
		    end 
		else ();
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
		        ((* myTy := output (!aggrs) (!myTy) (!start_time) (!index) (!count) logFile; *)
			eof:=true) 
		)
	   val _ = TextIO.closeIn strm
       in
	  (!myTy, !aggrs, !start_time, !index, !count, !badcount, 0)
       end

     fun parse_files (initTy, numHeaders, numFooters, filepaths, chunksize, 
		begin_time, dir, start_pos, reparse) =
      let
	   val filepath = hd filepaths
	   val filename = OS.Path.file filepath
	   val logFile = dir ^ "/" ^ filename ^ ".log"

	   val init_aggr = AG.TupleA (0, [AG.initialize initTy, AG.Ln(mkNextTyLabel(), nil)])
	   val init_table = AG.initTable()
	   val aggrs = [(init_aggr, init_table)]
	   val start_time = ref (Time.now())
           fun batch(file, (ty, aggrs, stime, index, count, badcount, start_pos)) =
               parse_one_file (file, ty, aggrs, logFile, chunksize, 
			stime, index, count, badcount, start_pos) 
	   val (finalTy, aggrs, start_time, index, count, _, _) = 
		List.foldl batch (initTy, aggrs, Time.now(), 0, 0, 0, start_pos) filepaths
	   val finalTy = output aggrs finalTy start_time index count logFile
	   val finalTy = sortUnionBranches (Reduce.reduce 4 NONE finalTy)

	   val total_elapse = Time.- (Time.now(), begin_time)
	   val _ = (print "**** Final Ty: \n"; printTy (measure 0 finalTy))
	   val tycomp = getComps finalTy
	   val _ = print ("Final comps = (" ^ showBits (#tc tycomp) ^ ", " ^ 
			showBits (#adc tycomp) ^ ", " ^
			(Real.toString (Reduce.score finalTy)) ^ ")\n")
	   val msg = "Total time = " ^ Time.toString total_elapse ^ " secs\n"

	   val logstrm = TextIO.openAppend logFile
	   val _ = TextIO.output (logstrm, msg)
	   val _ = TextIO.closeOut logstrm
	   val _ = print msg
		   
	   val tyFile = (dir ^ "/" ^ filename ^ ".ty") 
	   val tystrm = TextIO.openOut tyFile
	   val _ = print ("Output IR to " ^ tyFile ^ "\n")
	   val _ = TextIO.output (tystrm, TyToStringD "" false false  "\n" finalTy)
	   val _ = TextIO.closeOut tystrm

	   val padscFile = dir ^  "/" ^ filename ^ ".p"
	   val pmlFile = dir ^  "/" ^ filename ^ ".pml"
	   val _ = print ("\nOutput final PADS description to " ^ padscFile ^ "\n")
	   val (topName, hdrName, tyName, trlName) = Printing.dumpPADSdesc padscFile 
				pmlFile finalTy numHeaders numFooters
	   (* val _ = Printing.dumpAccumProgram (dir ^ "/") filename hdrName tyName trlName *)
	   val _ = Printing.dumpParseProgram (dir ^ "/") filename hdrName tyName trlName
	   val _ = Printing.dumpXMLProgram (dir ^ "/") filename topName hdrName tyName trlName
	   val _ = Printing.cpFile (dir ^ "/") "GNUmakefile" "GNUmakefile.output"
	   val _ = Printing.cpFile (dir ^ "/") "vanilla.p" "vanilla.p"
	   val _ = print ("Log written to " ^ logFile ^ ".\n")
	   val _ = if reparse then
		    let
			val btime = Time.now()
			val (nTotal, nBads) = reparse_files(finalTy, filepaths)
			val elapsed = Time.- (Time.now(), btime)
			val _ = print ("Reparse time = " ^ Time.toString elapsed ^ " secs\n") 
			val _ = print ("Total recs = " ^ Int.toString nTotal ^ 
			"  Bad recs = " ^ Int.toString nBads ^ "\n") 
		    in ()
		    end
		   else () 
	
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

    fun get_files args =
	case args of
	  nil => (nil, nil)
	| a::args => if String.sub(a, 0) = #"-" then (nil, a::args)
		     else 
			let val (fs, args)= get_files args
			in (a::fs, args)
			end
 
    fun loadArgs smap args =
	case args of
	  flag::v::args => 
	   if flag = "-f" then
		let val (files, remaining_args) = get_files (v::args)
		    val smap' = StringMap.insert (smap, flag, String.concatWith "|" files)
		in loadArgs smap' remaining_args
		end
	   else if String.sub(flag, 0) = #"-" then 
		let val smap' = StringMap.insert (smap, flag, v)
		in loadArgs smap' args
		end
	   else raise InvalidFlags
	| nil => smap

    fun main (cmd, args) = 
     (
     if length args < 1 then
	(print ("Usage: increment -f DATA_FILE(S) [-i INIT_SIZE (500)] [-l INC_SIZE (100)] \n" ^ 
	"[-d INIT_DESC_XML] [-opt OPT_LEVEL (3)] [-tmout SECS (900)] [-w ADC_WEIGHT (10)]\n" ^
	"[-output OUTPUT_DIR] [-reparse BOOL] [-u FLOAT]\nSizes are in # of lines\n");
	anyErrors := true)
     else
       let
	 val argMap = loadArgs StringMap.empty args
	 val learn_files = 
		let val s = valOf (StringMap.find (argMap, "-f"))
		in String.tokens (fn c => c = #"|") s
		end
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

         val reparse = case StringMap.find (argMap, "-reparse") of
		NONE => false
		| SOME x => valOf(Bool.fromString x)

         val _ = case StringMap.find (argMap, "-u") of
		NONE => useUnionClustering := NONE
		| SOME x => 
			let val threshold = valOf(Real.fromString x)
			in
			  if threshold > 1.0 orelse threshold < 0.0 then
			    useUnionClustering := NONE
			  else
			    useUnionClustering := SOME threshold
			end

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
	 val learn_file_name = OS.Path.file (hd learn_files)

	 val pxml = StringMap.find (argMap, "-d") 
	 val start_pos = case pxml of
			SOME _ => 0
			| _ => learnsize 

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
	 (*
         val _ = Posix.Process.alarm(Time.fromSeconds(LargeInt.fromInt(!learn_timeout)))
	 *)

	 val stime = Time.now()
	 fun my_learn (a, b, c) = Rewrite.run a b c
	 val timed_learn = TimeLimit.timeLimit (Time.fromSeconds
		(LargeInt.fromInt(!learn_timeout))) my_learn 
	 val (_, initTy, numHeaders, numFooters, _) = 
		case pxml of 
		  NONE =>
		  let val learn_lines = 
			get_learn_chunk ((hd learn_files), learnsize)
		  in
		    timed_learn ((Times.zeroEndingTimes()), 1, 
		    (measure 0 (#1 (computeStructurefromRecords learn_lines))))
		  end
		| SOME xmlfile =>
		  let val xml = PxmlParse.loadXML xmlfile
		      val ty = Pxml.xmlToIR Pxml.StringMap.empty xml
		      (* the ty decribes the entire data, we need a ty that describes just one record *)
		      val ty = case ty of
				  RArray (_, _, _, body, _, _) => body
				| _ => ty
		      val measured_ty = (measure 0 ty)
		      (* val _ = printTy measured_ty *)
		  in
			(ty, measured_ty, 0, 0, Times.zeroEndingTimes ())
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

         val logFile = timedir ^ "/" ^ learn_file_name ^ ".log"
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
         parse_files (initTy, numHeaders, numFooters, learn_files, chunksize, 
			stime, timedir, start_pos, reparse) 
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

