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

    fun doIt () = 
	let val fileName         = !srcFile
	    val ty               = computeStructure fileName
	    val rewrittenTy      = Rewrite.run(ty)
            val measuredTy       = measure rewrittenTy
            val comps            = getComps measuredTy
            val tcomp            = #tc comps
            val acomp            = #adc comps
            val dcomp            = #dc comps
	in
	    ( Printing.dumpTyInfo (!outputDir) measuredTy
            , print ( "\nCompleted " ^ !srcFile )
            , print ( "\nOverall type complexity = " ^ showBits tcomp )
            , print ( "\nOverall atomic complexity = " ^ showBits acomp )
            , print ( "\nOverall data complexity = " ^ showBits dcomp ^ "\n\n" )
            )
	end

  (*** Another doIt function that invokes the inferencing ***)
  (*
  fun doIt1 () =
	let val fileName = !srcFile
	    val recordNumber = ref 0
	    val () = print ("Starting on file "^fileName^"\n");
	    val records = loadFile fileName
	    val () = initialRecordCount := (List.length records) 
	    val rtokens : Context list = List.map (ltokenizeRecord recordNumber) records
            val rtokens = crackUniformGroups rtokens (* check if all records have same top level group token *)
	    val () = lengthsToHist rtokens
	    val ty = ContextListToTy 0 rtokens
	    val sty = simplifyTy ty
	    val rewrited_ty = Rewrite.run sty
	in
	    Printing.dumpTyInfo (!outputDir) rewrited_ty 
	end
*)	

    (********************************************************************************)
    structure PCL = ParseCmdLine

    fun setOutputDir    s = outputDir  := (s^"/")
    fun setDepth        d = depthLimit := d
    fun setHistPer      h = HIST_PERCENTAGE := h
    fun setStructPer    s = STRUCT_PERCENTAGE := s
    fun setJunkPer      j = JUNK_PERCENTAGE := j
    fun setNoisePer     n = NOISE_PERCENTAGE := n
    fun setArrayWidth   a = ARRAY_WIDTH_THRESHOLD := a
    fun setPrintLineNos b = (if b then printLineNos := b else ())
    fun setPrintIDs     b = (if b then  printIDs := b else ())
    fun setEntropy      b = (if b then  printEntropy := b else ())
    fun addSourceFile   f = srcFile    := f

    val flags = [
         ("d",        "output directory (default "^def_outputDir^")",                                      PCL.String (setOutputDir, false)),
         ("maxdepth", "maximum depth for exploration (default "^(Int.toString def_depthLimit)^")",         PCL.Int    (setDepth,     false)),
         ("lineNos",  "print line numbers in output contexts (default "^(Bool.toString def_printLineNos)^")",            PCL.Bool  setPrintLineNos),
         ("ids",      "print ids in type and tokens matching base types (default "^(Bool.toString def_printLineNos)^")", PCL.Bool  setPrintIDs),
         ("h",        "histogram comparison tolerance (percentage, default "^(Real.toString DEF_HIST_PERCENTAGE)^")",    PCL.Float  (setHistPer,   false)),
         ("s",        "struct determination tolerance (percentage, default "^(Real.toString DEF_STRUCT_PERCENTAGE)^")",  PCL.Float  (setStructPer, false)),
         ("n",        "noise level (percentage, default "^(Real.toString DEF_NOISE_PERCENTAGE)^")",        PCL.Float  (setNoisePer,   false)),
         ("a",        "minimum array width (default "^(Int.toString DEF_ARRAY_WIDTH_THRESHOLD)^")",        PCL.Int    (setArrayWidth, false)),
         ("j",        "junk threshold (percentage, default "^(Real.toString DEF_JUNK_PERCENTAGE)^")",      PCL.Float  (setJunkPer,    false)),
         ("e",        "Print entropy tokens (default "^(Bool.toString def_entropy)^")",                                  PCL.Bool    setEntropy)
        ]

    fun processSwitches args = 
	let val banner = PCL.genBanner("learn", "Prototype Learning System", flags)
	in
	   (PCL.parseArgs(args, flags, addSourceFile, banner);
	    printParameters())
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

