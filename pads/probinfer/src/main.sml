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
    let 
        val srcFile = hd ( !srcFiles )
        val { dir = dataDir, file = dataFile } = OS.Path.splitDirFile srcFile
    in if ( !goldenRun = true )
       then let val strm      = TextIO.openOut "gen/GoldComplexity"
                val rep       = goldenReport ( dataFile )
            in ( print rep
               ; TextIO.output ( strm, rep )
               ; TextIO.closeOut strm
               )
            end

       else if ( !dumpseqsets = true ) then
         let 
           val _ = print "Construct seqsets and print to a file.\n";
           val _ = dumpSeqsets ( !srcFiles )
         in
           ()
         end

       else if ( !inctrainingRun = true ) then
         let 
           val _ = print "Incremental training run.\n";
           val _ = if ( !character = true ) then incdumpCCHMMChar "training/"
                   else incdumpCCHMM "training/";
(*val (token, str) = List.nth (List.nth (table, 0), 0)
print token *)
           val _ = moveIncToList "training/log/" "inc.list" "log.list"
         in
           ()
         end

       else if ( !inctrainingWeightsRun = true ) then
         let 
           val _ = print "Incremental training run with weights.\n";
           val _ = incdumpCCHMMWeights "training/"
(*val (token, str) = List.nth (List.nth (table, 0), 0)
print token *)
           val _ = moveIncToList "training/log/" "IncTrainingWeightList" "TrainingWeightList"
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

       else if ( !ghmmtrainingRun = true )
         then
            let
              val _ = print "generalized HMM training run...\n"
              val _ = GHMMTraining "training/"             
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

       else if ( !svmtrainingRun = true )
         then
            let
              val _ = print "Support Vector Machine training run...\n"
              val _ = SVMTraining "training/"             
            in 
              ()
            end

       else if ( !examHMMPre = true )
         then
            let
              val _ = print "examining HMM library, constructing input...\n"
              val _ = examHmmResultPre (!srcFiles)             
            in print ( "Input character feature vector lists to HMM library.\n" )
            end

       else if ( !examHMMPost = true )
         then
            let
              val _ = print "examining HMM returned tokens...\n"
              val _ = examHmmResultPost (!srcFiles)             
            in print ( "\nCompleted " ^ (lconcat (!srcFiles)) ^ "\n" )
            end

       else if ( !evaluateHMMPost = true )
         then
            let
              val _ = print "examining HMM tokenization result without seqsets...\n"
              val _ = evaluateHmmResultPost (!srcFiles)             
            in print ( "\nCompleted " ^ (lconcat (!srcFiles)) ^ "\n" )
            end

       else if ( !evaluateVanilla = true )
         then
            let
              val _ = print "examining old vanilla tokenization result...\n"
              val _ = evaluateVanillaResult (!srcFiles)             
            in print ( "\nCompleted " ^ (lconcat (!srcFiles)) ^ "\n" )
            end

       else if ( !evaluatehmmseqset = true )
         then
            let
              val _ = print "examining HMM tokenization result with seqsets...\n"
              val _ = evaluate_HMM_seqset (!srcFiles)             
            in print ( "\nCompleted " ^ (lconcat (!srcFiles)) ^ "\n" )
            end

       else if ( !evaluateghmmseqset = true )
         then
            let
              val _ = print "examining generalized HMM tokenization result with seqsets...\n"
              val _ = evaluate_GHMM_seqset (!srcFiles)             
            in print ( "\nCompleted " ^ (lconcat (!srcFiles)) ^ "\n" )
            end

       else if ( !evaluatesvmseqset = true )
         then
            let
              val _ = print "examining SVM tokenization result with seqsets...\n"
              val _ = evaluate_SVM_seqset (!srcFiles)             
            in print ( "\nCompleted " ^ (lconcat (!srcFiles)) ^ "\n" )
            end

       else if ( !showtokenvanilla = true )
         then
            let
              val _ = print "printing token sequences by lex...\n"
              val _ =  showTokenSeqsVanilla(!srcFiles)             
            in print ( "\nCompleted " ^ (lconcat (!srcFiles)) ^ "\n" )
            end

       else if ( !showtokenhmm = true )
         then
            let
              val _ = print "printing token sequences by hmm...\n"
              val _ =  showTokenSeqsHmm(!srcFiles)             
            in print ( "\nCompleted " ^ (lconcat (!srcFiles)) ^ "\n" )
            end

       else if ( !showtokenghmm = true )
         then
            let
              val _ = print "printing token sequences by ghmm...\n"
              val _ =  showTokenSeqsGhmm(!srcFiles)             
            in print ( "\nCompleted " ^ (lconcat (!srcFiles)) ^ "\n" )
            end

       else if ( !showtokensvm = true )
         then
            let
              val _ = print "printing token sequences by svm...\n"
              val _ =  showTokenSeqsSvm(!srcFiles)             
            in print ( "\nCompleted " ^ (lconcat (!srcFiles)) ^ "\n" )
            end

       else if ( !testingRun = true)
         then 
            let 
              val _ = print "testingRun\n"
              val end1Times = zeroEndingTimes ()
              val end2Times = updateStart (Time.now()) end1Times
              val (ty, end3Times) = computeProbStructure ( !srcFiles ) end2Times
              val _ = Printing.dumpNewTy (!outputDir^"BeforeRefine.NewTy") ty
              val ( measuredTy, rewrittenTy, numHeaders, numFooters, end4Times) = 
				   Rewrite.newrun end3Times ty
              val computeTimes = getComputeTimes end4Times
              val _ = Printing.dumpNewTy (!outputDir^"AfterRefine.NewTy") rewrittenTy              
              val ()           = Printing.dumpNewTyInfo (!outputDir)
						       dataDir
                                                       dataFile
                                                       measuredTy
                                                       rewrittenTy
						       numHeaders
						       numFooters
                                                       end4Times
                               NONE
            in print ( "\nCompleted " ^ (lconcat (!srcFiles)) ^ "\n" )
            end

       else if ( !hmmtokenize = true)
         then 
            let 
              val _ = print "testingRun\n"
              val end1Times = zeroEndingTimes ()
              val end2Times = updateStart (Time.now()) end1Times
              val (ty, end3Times) = computeProbStructure_HMMonly ( !srcFiles ) end2Times
(*              val ty = removePPempty(ty) *)
              val _ = Printing.dumpNewTy (!outputDir^"BeforeRefine.HMMTy") ty
              val ( measuredTy, rewrittenTy, numHeaders, numFooters, end4Times) = 
				   Rewrite.newrun end3Times ty
              val computeTimes = getComputeTimes end4Times
              val _ = Printing.dumpNewTy (!outputDir^"AfterRefine.HMMTy") rewrittenTy              
              val ()           = Printing.dumpNewTyInfo (!outputDir)
						       dataDir
                                                       dataFile
                                                       measuredTy
                                                       rewrittenTy
						       numHeaders
						       numFooters
                                                       end4Times
                               NONE
            in print ( "\nCompleted " ^ (lconcat (!srcFiles)) ^ "\n" )
            end

       else if ( !ghmmtestingRun = true )
         then
           let  val end1Times    = zeroEndingTimes ()
                val end2Times    = updateStart ( Time.now () ) end1Times
                val (ty, end3Times)     = computeProbStructure_GHMM ( !srcFiles ) end2Times
(*		val _ 		 = printTy (measure ty) *)
                val _ = Printing.dumpNewTy (!outputDir^"BeforeRefine.GHMMTy") ty
                val ( measuredTy, rewrittenTy, numHeaders, numFooters, end4Times) = 
				   Rewrite.newrun end3Times ty
                val computeTimes = getComputeTimes end4Times
                val _ = Printing.dumpNewTy (!outputDir^"AfterRefine.GHMMTy") rewrittenTy
                val ()           = Printing.dumpNewTyInfo (!outputDir)
						       dataDir
                                                       dataFile
                                                       measuredTy
                                                       rewrittenTy
						       numHeaders
						       numFooters
                                                       end4Times
                               NONE
           in print ( "\nCompleted " ^ (lconcat (!srcFiles)) ^ "\n" )
           end

       else if ( !svmtestingRun = true )
         then
           let  val end1Times    = zeroEndingTimes ()
                val end2Times    = updateStart ( Time.now () ) end1Times
                val (ty, end3Times)     = computeProbStructure_SVM ( !srcFiles ) end2Times
(*		val _ 		 = printTy (measure ty) *)
                val _ = Printing.dumpNewTy (!outputDir^"BeforeRefine.GHMMTy") ty
                val ( measuredTy, rewrittenTy, numHeaders, numFooters, end4Times) = 
				   Rewrite.newrun end3Times ty
                val computeTimes = getComputeTimes end4Times
                val _ = Printing.dumpNewTy (!outputDir^"AfterRefine.GHMMTy") rewrittenTy
                val ()           = Printing.dumpNewTyInfo (!outputDir)
						       dataDir
                                                       dataFile
                                                       measuredTy
                                                       rewrittenTy
						       numHeaders
						       numFooters
                                                       end4Times
                               NONE
           in print ( "\nCompleted " ^ (lconcat (!srcFiles)) ^ "\n" )
           end

       else let val end1Times    = zeroEndingTimes ()
                val end2Times    = updateStart ( Time.now () ) end1Times
                val (ty,sep, end3Times)     = computeStructure ( !srcFiles ) end2Times
(*		val _ 		 = printTy (measure ty) *)
                val _ = Printing.dumpTy (!outputDir^"BeforeRefine.OldTy") ty
                val ( measuredTy, rewrittenTy, numHeaders, numFooters, end4Times) = 
				   Rewrite.run end3Times ty
                val computeTimes = getComputeTimes end4Times
                val _ = Printing.dumpTy (!outputDir^"AfterRefine.OldTy") rewrittenTy
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
    fun setGHMMTrainingRun  s = ghmmtrainingRun  := (s = "true")
    fun setTrainingWeightsRun  s = trainingWeightsRun  := (s = "true")
    fun setSVMTrainingRun  s = svmtrainingRun  := (s = "true")
    fun setIncTrainingRun  s = inctrainingRun  := (s = "true")
    fun setIncTrainingWeightsRun  s = inctrainingWeightsRun  := (s = "true")
    fun setTestingRun   s = testingRun  := (s = "true") 
    fun setGHMMTestingRun  s = ghmmtestingRun  := (s = "true")
    fun setSVMTestingRun  s = svmtestingRun  := (s = "true")
    fun setExamHMMPre   s = examHMMPre  := (s = "true")
    fun setExamHMMPost  s = examHMMPost  := (s = "true")
    fun setEvaluateHMMPost  s = evaluateHMMPost  := (s = "true")
    fun setEvaluateVanilla  s = evaluateVanilla  := (s = "true")
    fun setEvaluate_hmm_seqset  s = evaluatehmmseqset  := (s = "true")
    fun setEvaluate_ghmm_seqset  s = evaluateghmmseqset  := (s = "true")
    fun setEvaluate_svm_seqset  s = evaluatesvmseqset  := (s = "true")
    fun setHMMtokenize  s = hmmtokenize  := (s = "true")
    fun setGHMM1 s = ghmm1 := (s = "true")
    fun setGHMM2 s = ghmm2 := (s = "true")
    fun setGHMM3 s = ghmm3 := (s = "true")
    fun setGHMM4 s = ghmm4 := (s = "true")
    fun setGHMM5 s = ghmm5 := (s = "true")
    fun setShowTokenVanilla s = showtokenvanilla := (s = "true")
    fun setShowTokenHmm s = showtokenhmm := (s = "true")
    fun setShowTokenGhmm s = showtokenghmm := (s = "true")
    fun setShowTokenSvm s = showtokensvm := (s = "true")
    fun setDumpSeqsets  s = dumpseqsets  := (s = "true")
    fun setCharacter  s = character  := (s = "true")
    fun setModelExist  s = modelexist  := (s = "true")
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
         ("dumpseqsets", "dump seqsets and print to file",	                                                   PCL.String (setDumpSeqsets, true)),
         ("ghmmtraining", "training generalized HMM",	                                                   PCL.String (setGHMMTrainingRun, true)),
         ("inctraining", "incremental training run",	                                                   PCL.String (setIncTrainingRun, true)),
         ("svmtraining", "training Support Vector Machine",	                                                   PCL.String (setSVMTrainingRun, true)),
         ("inctrainingw", "incremental training run with weights",	                                       PCL.String (setIncTrainingWeightsRun, true)),
         ("testing",  "testing run",	                                                   PCL.String (setTestingRun, true)),
         ("ghmmtesting",  "testing generalized HMM",	                                                   PCL.String (setGHMMTestingRun, true)),
         ("svmtesting",  "testing Support Vector Machine",	                                                   PCL.String (setGHMMTestingRun, true)),
         ("hmm1",  "testing HMM library: 1st step",	                                                   PCL.String (setExamHMMPre, true)),
         ("hmm2",  "testing HMM library: 2nd step",	                                                   PCL.String (setExamHMMPost, true)),
         ("hmm3",  "evaluating HMM library: 2nd step",	                                                   PCL.String (setEvaluateHMMPost, true)),
         ("ghmm1",  "use basicViterbi_GHMM",	                                                   PCL.String (setGHMM1, true)), 
         ("ghmm2",  "use basicViterbi_GHMM_trans",	                                                   PCL.String (setGHMM2, true)),         
         ("ghmm3",  "use basicViterbi_GHMM_length",	                                                   PCL.String (setGHMM3, true)),         
         ("ghmm4",  "use basicViterbi_GHMM_trans_length",	                                                   PCL.String (setGHMM4, true)),         
         ("ghmm5",  "use token class to compute transition probability",	                                                   PCL.String (setGHMM5, true)),         
         ("showtokenvanilla",  "show token sequences by lex",	                                                   PCL.String (setShowTokenVanilla, true)),         
         ("showtokenhmm",  "show token sequences by hmm",	                                                   PCL.String (setShowTokenHmm, true)),         
         ("showtokenghmm",  "show token sequences by ghmm",	                                                   PCL.String (setShowTokenGhmm, true)),         
         ("showtokensvm",  "show token sequences by svm",	                                                   PCL.String (setShowTokenSvm, true)),         
         ("evaluate_hmm_ss",  "evaluating HMM tokenization result with seqsets",	                                                   PCL.String (setEvaluate_hmm_seqset, true)),
         ("evaluate_ghmm_ss",  "evaluating GHMM tokenization result with seqsets",	                                                   PCL.String (setEvaluate_ghmm_seqset, true)),
         ("evaluate_svm_ss",  "evaluating GHMM tokenization result with seqsets",	                                                   PCL.String (setEvaluate_ghmm_seqset, true)),
         ("hmmtokenize",  "evaluate descriptions with tokenization by hmm library",	                                                   PCL.String (setHMMtokenize, true)),
         ("vanilla",  "evaluating vanilla tokenization",	                                                   PCL.String (setEvaluateVanilla, true)),
         ("char", "use character other than character feature vector for training",                        PCL.String (setCharacter, true)),
         ("modelexist", "svm model already exists in the training dir",                        PCL.String (setModelExist, true)),
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

