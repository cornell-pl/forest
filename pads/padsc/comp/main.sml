(* main.sml
 *
 * COPYRIGHT (c) 2002 AT&T Research
 *)

structure Main : sig

    datatype ReturnType = Parse of ParseTree.externalDecl list
      | Ast of Ast.ast
      | Nothing

    val main : bool -> (string * string list) -> OS.Process.status
    val emit : bool -> ReturnType

  end = struct
    structure PCL  = ParseCmdLine
    structure CKIT = ParseToAst

    datatype ReturnType = Parse of ParseTree.externalDecl list
      | Ast of Ast.ast
      | Nothing

    exception Exit of OS.Process.status
    exception DebugExn of ReturnType
 
    (* Translation state *)
    val anyErrors = ref false
    val curFile = ref ""
    val stage = ref ""

    (* Values/Flags supplied by user at command line *)
    datatype ArgType = Pads | Unknown
    val srcFiles = ref [] : (ArgType * string) list ref 

    val includes = ref ""

    val traceFlag = ref true
    val parseTreeOnlyFlag = ref false
    val astOnlyFlag = ref false
    val stdoutFlag = ref false
    val outputHeaderFileName = ref ""
    val outputHeaderFileFlag = ref false
    val outputCFileName = ref ""
    val outputCFileFlag = ref false

    fun addInclude i = (includes := (" -I "^i^(!includes)))
    fun addPadsFile s =    srcFiles := ((Pads,s) :: !srcFiles)
    fun addUnknownFile s = srcFiles := ((Unknown,s) :: !srcFiles)
  
    fun setHeaderOutputFile s = (
        outputHeaderFileName := s; 
	stdoutFlag := false; 
        outputHeaderFileFlag := true)

    fun setCOutputFile s = (
        outputCFileName := s; 
	stdoutFlag := false; 
        outputCFileFlag := true)

    val extensions = [("p", "PADS files", PCL.Extension(addPadsFile,true))]

    val flags_release = [
         ("h", "output header file",      PCL.String (setHeaderOutputFile, false)),
         ("c", "output code file",        PCL.String (setCOutputFile, false)),
         ("s", "send output to standard out", PCL.BoolSet(stdoutFlag)),
	 ("I", "augment include path",        PCL.String (addInclude, true)),
         ("t", "trace system commands",       PCL.BoolSet traceFlag)
        ]

    val flags_debug = [
	 ("parse", "generate parsetree only", PCL.BoolSet parseTreeOnlyFlag),
	 ("ast",   "generate ast only", PCL.BoolSet astOnlyFlag)
        ]

    (* Error handling *)
    fun error msg = (
	  anyErrors := true;
	  TextIO.output(TextIO.stdErr, !curFile ^ ": ");
	  TextIO.output(TextIO.stdErr, msg);
	  TextIO.output1(TextIO.stdErr, #"\n"))

    fun err s = (print  (!stage ^ " failed: " ^ s ^ "\n");
		 raise Exit OS.Process.failure)

    (* SML directives *)
    fun silenceGC () = (SMLofNJ.Internals.GC.messages false)
    fun setPrintDepth () =
	(Compiler.Control.Print.printDepth := valOf(Int.maxInt);
	 Compiler.Control.Print.printLength := valOf(Int.maxInt))

    (* Utilities ***************************************************)
    val tempFiles = ref [] : string list ref
    fun tmp ext = 
	let val t = (OS.FileSys.tmpName ()) ^ ext
	in
	    tempFiles := t :: !tempFiles;
	    t
	end

    fun mungeFileName (fileName, from, to) = 
    (case OS.Path.splitBaseExt fileName
     of {base, ext=SOME from} =>
	 SOME (OS.Path.joinBaseExt{base=base, ext=SOME to})
     | _ => NONE (*end case *))

    fun rmFile f = 
	 let val relFopt = mungeFileName(f,"c", "o")
	 in
	  if !traceFlag then print ("rm " ^ f ^ "\n") else ();
	  OS.FileSys.remove f handle e => ();
	  (case relFopt of NONE => () 
              | SOME f' => (	
		 if !traceFlag then print ("rm " ^ f' ^ "\n") else ();
	         OS.FileSys.remove f' handle e => () )(*end case*))
	 end
	     
    fun rm f = 
	(tempFiles := List.filter (fn x => x<>f) (!tempFiles);
        rmFile f)

    fun rmTmp () = (
	    List.app rmFile (!tempFiles);
	    tempFiles := [])

    fun exec s =
	((if !traceFlag then print (s ^ "\n") else ());
	 OS.Process.system s)

    (* Run preprocessor ********************************************)
    fun preprocess(srcFile, destFile) = 
	let val srcFile = OS.FileSys.fullPath srcFile
            val compositeFile = tmp ".c"
            val includePrefix = ("#include <padslib-internal.h>\n"^
                                 "\n")
            val compositeProg = (includePrefix ^
				   ("#include \"" ^srcFile^"\"\n"))
	    val outStrm = TextIO.openOut compositeFile
            val () = (TextIO.output(outStrm, compositeProg);
		      TextIO.closeOut outStrm)
            val command = (" cc "  (* invoke c compiler *)
                          ^ "-E "  (* preprocessor only *)
                          ^ (!includes)  (* augment include path *)
                          ^ " "
                          ^ compositeFile (* on composite file *)
                          ^ " >"
                          ^ destFile)
	    val status = exec command
	in
	    (rm compositeFile; status)
	end

    fun doParseOnly(srcFile:string) = 
       let val () = stage := "Parsing"
           val errorState = Error.mkErrState TextIO.stdErr
           val tree = Parser.parseFile errorState srcFile
       in (setPrintDepth(); 
	   raise DebugExn(Parse tree))
       end

    fun generateOutput (astInfo : BuildAst.astBundle, destFile) =
      let val {ast,tidtab,errorCount,warningCount,...} = astInfo
      in
          if !stdoutFlag then 
             PPLib.ppToStrm ((PPAst.ppAst PPAst.ALL) () tidtab) TextIO.stdOut ast
          else let 
	        val houtstream = 
		    if !outputHeaderFileFlag then TextIO.openOut (!outputHeaderFileName)
		    else TextIO.openOut (valOf (mungeFileName(destFile, "p", "h")))
	        val coutstream = 
		    if !outputCFileFlag then TextIO.openOut (!outputCFileName)
		    else TextIO.openOut (valOf (mungeFileName(destFile, "p", "c")))
	       in
		   PPLib.ppToStrm ((PPAst.ppAst PPAst.HEADER) () tidtab) houtstream ast;		   
		   TextIO.flushOut houtstream;
		   TextIO.closeOut houtstream;
		   PPLib.ppToStrm ((PPAst.ppAst PPAst.IMPL) () tidtab) coutstream ast;		   
		   TextIO.flushOut coutstream;
		   TextIO.closeOut coutstream
	       end
      end
	    
    fun doFile (typ, fname) = 
      (curFile := fname;
       case typ of Pads =>
	 let val () = stage := "Preprocessing"
	     val ppoutFile = tmp ".c"
	     val status = preprocess(fname, ppoutFile)
	     val () = if status <> OS.Process.success 
	              then err "Pre-processor failed."
		      else ()
             val () = if (!parseTreeOnlyFlag) then doParseOnly ppoutFile else ()
	     val astInfo as {ast, tidtab, ...} = CKIT.fileToAst ppoutFile
             val () = if (!astOnlyFlag) then (setPrintDepth(); raise DebugExn(Ast ast)) else ()
	 in
	     generateOutput(astInfo, fname)
	 end
      | _ => error "Unrecognized file type")

    fun checkFlags _ = (* Check that the user didn't supply bogus flag combinations. *)
        let val () = if (!stdoutFlag) andalso ((!outputHeaderFileFlag) orelse (!outputCFileFlag))
			 then error "Cannot specify both standard out and output file."
		     else ()
        in
           ()
	end

    fun main release (cmd, args) = 
      (stage := "Command-line processing";
       let val homeDir = hd args
           val arguments = tl args
           val flags = if release then flags_release @ extensions
                       else flags_release @ flags_debug @ extensions
           val banner = PCL.genBanner("padsc", 
				      "PADS Compiler version 0.1", flags)
           val () = PCL.parseArgs(arguments, flags, addUnknownFile, banner)
           val () = checkFlags()
           (* At this point, flag booleans have been set from command-line *)
       in
         app doFile (!srcFiles); 
         if !anyErrors 
	     then  OS.Process.exit(OS.Process.failure)
	 else  OS.Process.exit(OS.Process.success)
       end)  
            handle PCL.Invalid => (rmTmp(); OS.Process.exit(OS.Process.failure))
                  | DebugExn r => raise DebugExn r
                  | ex => (TextIO.output(TextIO.stdErr, concat[
		          "uncaught exception ", exnName ex,
		          " [", exnMessage ex, "]\n"
	                  ]);
			  app (fn s => TextIO.output(TextIO.stdErr, concat[
		          "  raised at ", s, "\n"
	                  ])) (SMLofNJ.exnHistory ex);
	                   OS.Process.exit(OS.Process.failure))



    (* Generates the compiler and exports an executable. *)
    fun emit release =
	if release then 
	    (silenceGC();
	     SMLofNJ.exportFn ("lib/padsc", main release);
	    Nothing)
	else 
	    (if not (SMLofNJ.exportML "lib/padsc") then 
		 (print "\nPadsc translator image built.\n";
		  Nothing)
	     else ((silenceGC ();
		    main release ("", CommandLine.arguments()); Nothing)
		   handle DebugExn r => r))

  end; 

