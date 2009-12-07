structure Descdist: sig
  
    val main : (string * string list) -> OS.Process.status
    val emit : unit -> unit

  end = struct

(*
    val _ = Compiler.Profile.setProfMode true
    val _ = Compiler.Profile.setTimingMode true
    val _ = SMLofNJ.Internals.ProfControl.spaceProfiling:= true
*)

    exception Exit of OS.Process.status
    exception InvalidInput
    val anyErrors = ref false

    fun silenceGC () = (SMLofNJ.Internals.GC.messages false)

    fun main (cmd, args) = 
     (
     (if length args < 2 then
	(print ("Usage: descdist xml1 xml2\n");
	anyErrors := true)
     else
       case args of
	xmlpath1::xmlpath2::_ =>
	  let val xml1 = PxmlParse.loadXML xmlpath1
	      val xml2 = PxmlParse.loadXML xmlpath2
	      val ty1 = Pxml.xmlToIR Pxml.StringMap.empty xml1
	      val ty2 = Pxml.xmlToIR Pxml.StringMap.empty xml2
	      val d = Editdistance.treeEditDistance(ty1, ty2)
          in
            print ("Edit distance = " ^ Int.toString d ^ "\n")
	  end
	| _ => raise InvalidInput
      )
	 (* Compiler.Profile.reportAll TextIO.stdOut *)
        handle e =>(TextIO.output(TextIO.stdErr, concat[
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
	     SMLofNJ.exportFn ("../lib/descdist", main ))
  end; 

