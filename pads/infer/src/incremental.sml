structure Incremental: sig
  
    val main : (string * string list) -> OS.Process.status
    val emit : unit -> unit

  end = struct

    val anyErrors = ref false
    val max_int = 10000000
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
     print ((Int.toString (length args)) ^ "\n");
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
	 fun add (line, a) =
	    let 
		val _ = print (line ^ "\n")
         	val set = Parse.parse_all(goldenTy, LabelMap.empty, 0, line)
		val _ = print "Parse complete\n"
	 	val _ = Parse.ParseSet.app (fn (rep, m, j) => print (Rep.repToString "" rep)) set
		val (min_a, min_c) = Parse.ParseSet.foldl 
				(fn ((r, m, j), (mina, minc: int)) => 
				  let 
					val newaggr = Aggregate.merge a r 
					val newcost = Aggregate.cost newaggr
				  in 
				    if newcost < minc then (newaggr, newcost)
				    else (mina, minc)
				  end) (Aggregate.BaseA nil, max_int) set 
					(* use a dummy aggregate to start *)
	     in
		min_a
	     end
	  val final_aggr = foldl add init_aggr lines
       in
	 ()
       end; 
	 if !anyErrors then  OS.Process.exit(OS.Process.failure)          
	 else OS.Process.exit(OS.Process.success)
     )

    (* Generates the compiler and exports an executable. *)
    fun emit () = 
	    (silenceGC();
	     SMLofNJ.exportFn ("../lib/increment", main ))
  end; 

