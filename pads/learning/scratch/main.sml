structure Main : sig

    val main : (string * string list) -> OS.Process.status
    val emit : unit -> unit

  end = struct

    val anyErrors = ref false
    exception Exit of OS.Process.status
    fun silenceGC () = (SMLofNJ.Internals.GC.messages false)
    open Config
    open Types
    open Structure
    open Model
    open Options

    fun doIt () = 
	let val startT           = Time.now ()
            val fileNames        = !srcFiles
	    val ty               = computeStructure fileNames
            val tokenT'          = Time.now ()
            val tokenT           = Time.- (tokenT', startT)
            val ( rewrittenTy
                , start_time
                , measure1_time
                , reduce1_time
                , reduce2_time
                , reduce3_time
                , measured_reduced_time
                ) = Rewrite.run(ty)
            val measure1T        = Time.- (measure1_time, start_time)
            val reduce1T         = Time.- (reduce1_time, measure1_time)
            val reduce2T         = Time.- (reduce2_time, reduce1_time)
            val reduce3T         = Time.- (reduce3_time, reduce2_time)
            val measure2T        = Time.- (measured_reduced_time, reduce3_time)
            val ()               = print "Finished rewriting.\n"          
            val comps            = getComps rewrittenTy
            val tcomp            = #tc comps
            val acomp            = #adc comps
            val dcomp            = #dc comps
	in
	    ( print ( "\n====== Timing information ======\n" )
            , print ( "Tokenization time = " ^ ( Time.toString tokenT ) ^ "\n" )
            , print ( "Measure1 time = " ^ ( Time.toString measure1T ) ^ "\n" )
            , print ( "Reduce1 time = " ^ ( Time.toString reduce1T ) ^ "\n" )
            , print ( "Reduce2 time = " ^ ( Time.toString reduce2T ) ^ "\n" )
            , print ( "Reduce3 time = " ^ ( Time.toString reduce3T ) ^ "\n" )
            , print ( "Measure2 time = " ^ ( Time.toString measure2T ) ^ "\n" )
            , print ( "================================\n" )
            , Printing.dumpTyInfo (!outputDir) (!descName) rewrittenTy
            , print ( "\nCompleted " ^ (lconcat (!srcFiles)) ^ "\n" )
            )
	end

    (********************************************************************************)
    structure PCL = ParseCmdLine

    fun setOutputDir    s = outputDir  := (s^"/")
    fun setDescName     n = descName   := n
    fun setDepth        d = depthLimit := d
    fun setHistPer      h = HIST_PERCENTAGE := h
    fun setStructPer    s = STRUCT_PERCENTAGE := s
    fun setJunkPer      j = JUNK_PERCENTAGE := j
    fun setNoisePer     n = NOISE_PERCENTAGE := n
    fun setArrayWidth   a = ARRAY_WIDTH_THRESHOLD := a
    fun setPrintLineNos b = (if b then printLineNos := b else ())
    fun setPrintIDs     b = (if b then  printIDs := b else ())
    fun setEntropy      b = (if b then  printEntropy := b else ())
    fun addSourceFile   f  =  srcFiles := !srcFiles @ [f]

    val flags = [
         ("d",        "output directory (default "^def_outputDir^")",                                      PCL.String (setOutputDir, false)),
         ("n",        "name of output file (default "^def_descName^")",                                     PCL.String (setDescName,  false)),
         ("maxdepth", "maximum depth for exploration (default "^(Int.toString def_depthLimit)^")",         PCL.Int    (setDepth,     false)),
         ("lineNos",  "print line numbers in output contexts (default "^(Bool.toString def_printLineNos)^")",            PCL.Bool  setPrintLineNos),
         ("ids",      "print ids in type and tokens matching base types (default "^(Bool.toString def_printLineNos)^")", PCL.Bool  setPrintIDs),
         ("h",        "histogram comparison tolerance (percentage, default "^(Real.toString DEF_HIST_PERCENTAGE)^")",    PCL.Float  (setHistPer,   false)),
         ("s",        "struct determination tolerance (percentage, default "^(Real.toString DEF_STRUCT_PERCENTAGE)^")",  PCL.Float  (setStructPer, false)),
         ("noise",    "noise level (percentage, default "^(Real.toString DEF_NOISE_PERCENTAGE)^")",        PCL.Float  (setNoisePer,   false)),
         ("a",        "minimum array width (default "^(Int.toString DEF_ARRAY_WIDTH_THRESHOLD)^")",        PCL.Int    (setArrayWidth, false)),
         ("j",        "junk threshold (percentage, default "^(Real.toString DEF_JUNK_PERCENTAGE)^")",      PCL.Float  (setJunkPer,    false)),
         ("e",        "Print entropy tokens (default "^(Bool.toString def_entropy)^")",                                  PCL.Bool    setEntropy)
        ]

    fun processSwitches (execDir::args) = 
	let val banner = PCL.genBanner("learn", "Prototype Learning System", flags)
	in
	   (PCL.parseArgs(args, flags, addSourceFile, banner);
	    executableDir := execDir;
	    if print_verbose=true then printParameters() else () )
	end
    (********************************************************************************)

    fun main (cmd, args) = 
	 (processSwitches args;
          doIt (); 
          if !anyErrors then  OS.Process.exit(OS.Process.failure)
	  else OS.Process.exit(OS.Process.success))
            handle  Exit r      => OS.Process.exit(OS.Process.failure)
                  | PCL.Invalid => OS.Process.exit(OS.Process.failure)
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



    (* Generates the compiler and exports an executable. *)
    fun emit () = 
	    (silenceGC();
	     SMLofNJ.exportFn ("lib/learn", main ))
  end; 

