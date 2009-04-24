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
    open Times
    open Gold

    fun doIt () =
    let val srcFile = hd ( !srcFiles )
        val { dir = dataDir, file = dataFile } = OS.Path.splitDirFile srcFile
    in if ( !goldenRun = true )
       then let val strm      = TextIO.openOut "gen/GoldComplexity"
                val rep       = goldenReport ( dataFile )
            in ( print rep
               ; TextIO.output ( strm, rep )
               ; TextIO.closeOut strm
               )
            end
       else let val end1Times    = zeroEndingTimes ()
                val end2Times    = updateStart ( Time.now () ) end1Times
		(* records stores the original records from the file(s) *)
                val (ty,sep)     = computeStructure ( !srcFiles )
(*		val _ 		 = printTy (measure ty) *)
                val end3Times    = updateTokenEnd ( Time.now () ) end2Times
                val ( measuredTy, rewrittenTy, numHeaders, numFooters, end4Times) = 
				   Rewrite.run end3Times 0 ty
                val computeTimes = getComputeTimes end4Times
                val ()           = Printing.dumpTyInfo (!outputDir)
						       dataDir
                                                       dataFile
                                                       measuredTy
                                                       rewrittenTy
						       numHeaders
						       numFooters
                                                       end4Times
						       sep
            in print ( "\nCompleted " ^ (lconcat (!srcFiles)) ^ "\n" )
            end
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
    fun setMinArrayWidth a = ARRAY_MIN_WIDTH_THRESHOLD := a
    fun setPrintLineNos b = (if b then printLineNos := b else ())
    fun setPrintIDs     b = (if b then  printIDs := b else ())
    fun setEntropy      b = (if b then  printEntropy := b else ())
    fun addSourceFile   f  =  srcFiles := !srcFiles @ [f]
    fun setLexName	n = lexName    := n
    fun setGoldenRun    s = goldenRun  := (s = "true")
    fun setBlobRatio    r = blobRatio := r
    val flags = [
         ("d",        "output directory (default "^def_outputDir^")",                                      PCL.String (setOutputDir, false)),
         ("n",        "name of output file (default "^def_descName^")",                                     PCL.String (setDescName,  false)),
         ("maxdepth", "maximum depth for exploration (default "^(Int.toString def_depthLimit)^")",         PCL.Int    (setDepth,     false)),
         ("lineNos",  "print line numbers in output contexts (default "^(Bool.toString def_printLineNos)^")",            PCL.Bool  setPrintLineNos),
         ("ids",      "print ids in type and tokens matching base types (default "^(Bool.toString def_printLineNos)^")", PCL.Bool  setPrintIDs),
         ("h",        "histogram comparison tolerance (percentage, default "^(Real.toString DEF_HIST_PERCENTAGE)^")",    PCL.Float  (setHistPer,   false)),
         ("s",        "struct determination tolerance (percentage, default "^(Real.toString DEF_STRUCT_PERCENTAGE)^")",  PCL.Float  (setStructPer, false)),
         ("noise",    "noise level (percentage, default "^(Real.toString DEF_NOISE_PERCENTAGE)^")",        PCL.Float  (setNoisePer,   false)),
         ("a",        "array width requirement (default "^(Int.toString DEF_ARRAY_WIDTH_THRESHOLD)^")",    PCL.Int    (setArrayWidth, false)),
         ("ma",       "minimum array width (default "^(Int.toString DEF_ARRAY_MIN_WIDTH_THRESHOLD)^")",    PCL.Int    (setMinArrayWidth, false)),
         ("j",        "junk threshold (percentage, default "^(Real.toString DEF_JUNK_PERCENTAGE)^")",      PCL.Float  (setJunkPer,    false)),
         ("e",        "Print entropy tokens (default "^(Bool.toString def_entropy)^")",                    PCL.Bool    setEntropy),
         ("lex",      "prefix of the lex config to be used (default \"vanilla\")",	                   PCL.String (setLexName, false)),
         ("au",	      "run only the golden file",	                                                   PCL.String (setGoldenRun, true)),
         ("blob",     "threshold ratio used for blob finding (default 1.0), higher means fewer blobs",	   
       PCL.Float (setBlobRatio, false))
        ]

    fun checkOutputDir() =(
	print ("Output directory:"^(!outputDir)^".\n");
	if not (OS.FileSys.isDir (!outputDir)) then 
	    (print ("Specified output directory "^(!outputDir)^" must be a directory.\n") ;
	     OS.Process.exit(OS.Process.failure))
	else () )
	    handle SysErr => ((
			       print ("Specified output directory "^(!outputDir)^" does not exist. Trying to create it.\n");
			       OS.FileSys.mkDir (!outputDir);
			       print ("Succeeded in creating output directory "^(!outputDir)^".\n")
			       ) handle SysErr => (print "Failed to creat output directory.\n"))
    fun checkDescName() = 
	if (!descName) = def_descName then
	    let val dataFileName = List.hd (!srcFiles)
		val {dir,file} = OS.Path.splitDirFile dataFileName
	    in
		descName := file
	    end
	else ()
    fun processSwitches (execDir::args) = 
	let val banner = PCL.genBanner("learn", "PADS Learning System 1.0", flags)
	in
	   (PCL.parseArgs(args, flags, addSourceFile, banner);
	    executableDir := execDir;
	    checkOutputDir();
	    checkDescName();
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
	     SMLofNJ.exportFn ("../lib/learn", main ))
  end; 

