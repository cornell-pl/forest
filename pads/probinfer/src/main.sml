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
    open Probstruct
    open Probability
    open Common

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

       else if ( !inctrainingRun = true ) then
         let 
           val _ = print "Incremental training run.\n";
           val _ = if ( !character = true ) then incdumpCCHMMChar "training/"
                   else incdumpCCHMM "training/";
(*val (token, str) = List.nth (List.nth (table, 0), 0)
print token *)
           val _ = moveIncToList "training/log/"
         in
           ()
         end

       else if ( !trainingRun = true ) then
         let 
           val _ = print "trainingRun\n"; 
           val _ = if ( !character = true ) then dumpCCHMMChar "training/"
                   else dumpCCHMM "training/";
(*val (token, str) = List.nth (List.nth (table, 0), 0)
print token *)
         in
           ()
         end

       else if ( !trainingWeightsRun = true ) then
         let 
           val _ = print "trainingRun with weights\n"; 
           val _ = dumpCCHMMWeights "training/";
(*val (token, str) = List.nth (List.nth (table, 0), 0)
print token *)
         in
           ()
         end

       else if ( !testingRun = true)
         then 
            let 
              val _ = print "testingRun\n"
              val end1Times = zeroEndingTimes ()
              val end2Times = updateStart (Time.now()) end1Times
              val ty = removePPempty(computeProbStructure ( !srcFiles ))
              val sep = []
              val end3Times = updateTokenEnd (Time.now()) end2Times
              val _ = Printing.dumpNewTy (!outputDir^"NewTy") ty              
            in print ( "\nCompleted " ^ (lconcat (!srcFiles)) ^ "\n" )
            end

       else if ( !examHMMPre = true )
         then
            let
              val _ = print "examing HMM library, constructing input...\n"
              val end1Times = zeroEndingTimes ()
              val end2Times = updateStart (Time.now()) end1Times
              val _ = examHmmResultPre (!srcFiles)             
            in print ( "Input character feature vector lists to HMM library.\n" )
            end

       else if ( !examHMMPost = true )
         then
            let
              val _ = print "examing HMM returned tokens...\n"
              val end1Times = zeroEndingTimes ()
              val end2Times = updateStart (Time.now()) end1Times
              val _ = examHmmResultPost (!srcFiles)             
            in print ( "\nCompleted " ^ (lconcat (!srcFiles)) ^ "\n" )
            end

       else if ( !evaluateHMMPost = true )
         then
            let
              val _ = print "examing HMM returned tokens...\n"
              val end1Times = zeroEndingTimes ()
              val end2Times = updateStart (Time.now()) end1Times
              val _ = evaluateHmmResultPost (!srcFiles)             
            in print ( "\nCompleted " ^ (lconcat (!srcFiles)) ^ "\n" )
            end

       else let val end1Times    = zeroEndingTimes ()
                val end2Times    = updateStart ( Time.now () ) end1Times
                val (ty,sep)     = computeStructure ( !srcFiles )
(*		val _ 		 = printTy (measure ty) *)
val _ = Printing.dumpTy (!outputDir^"OldTy") ty
                val end3Times    = updateTokenEnd ( Time.now () ) end2Times
                val ( measuredTy, rewrittenTy, numHeaders, numFooters, end4Times) = 
				   Rewrite.run end3Times ty
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
    fun setTrainingRun  s = trainingRun  := (s = "true")
    fun setTrainingWeightsRun  s = trainingWeightsRun  := (s = "true")
    fun setIncTrainingRun  s = inctrainingRun  := (s = "true")
    fun setTestingRun   s = testingRun  := (s = "true") 
    fun setExamHMMPre   s = examHMMPre  := (s = "true")
    fun setExamHMMPost  s = examHMMPost  := (s = "true")
    fun setEvaluateHMMPost  s = evaluateHMMPost  := (s = "true")
    fun setCharacter  s = character  := (s = "true")
    fun setLambda       l = (if Real.compare(l, 0.0)=EQUAL then lambda := defaultLambda else lambda := l)
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
         ("training", "training run",	                                                   PCL.String (setTrainingRun, true)),
         ("trainingw", "training run with weights",	                                                   PCL.String (setTrainingWeightsRun, true)),
         ("inctraining", "incremental training run",	                                                   PCL.String (setIncTrainingRun, true)),
         ("testing",  "testing run",	                                                   PCL.String (setTestingRun, true)),
         ("hmm1",  "testing HMM library: 1st step",	                                                   PCL.String (setExamHMMPre, true)),
         ("hmm2",  "testing HMM library: 2nd step",	                                                   PCL.String (setExamHMMPost, true)),
         ("hmm3",  "evaluating HMM library: 2nd step",	                                                   PCL.String (setEvaluateHMMPost, true)),
         ("char", "use character other than character feature vector for training",                        PCL.String (setCharacter, true)),
         ("smooth", "use smoothing in training (default lambda"^(Real.toString defaultLambda)^")",         PCL.Float    (setLambda,     false))
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

