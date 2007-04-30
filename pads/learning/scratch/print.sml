structure Printing = 
struct
    open Types
    open Model
    open Times
(*    open Structure*)
   
    fun dumpLToken (strm:TextIO.outstream) (tk:Token,loc:location) : unit =
        TextIO.output(strm, tokenToString tk)

    fun dumpCL (fileName:string) (contexts:Context list) : unit = 
	let val strm = TextIO.openOut fileName
	    fun getLineNo (context:Context):string = 
		let fun extractLineNo (t,{lineNo,...}:location) = lineNo
		in
		    if !printLineNos then 
			case context of [] => "No line information."
                        | (lt::rest) => ((Int.toString (extractLineNo lt)) ^ ": ")
		    else ""
		end
            fun dumpOneContext (context:Context) : unit =
                ( TextIO.output(strm, getLineNo context)
                ; List.app (dumpLToken strm) context
                ; TextIO.output(strm, "\n")
                )
            val () = List.app dumpOneContext contexts
	in
	    TextIO.closeOut strm
	end

    (* Dump a type to the specified file *)
    fun dumpTy (fileName:string) (ty:Ty) : unit = 
        let val strm = TextIO.openOut fileName
            val ()   = TextIO.output(strm, TyToString ty)
        in
            TextIO.closeOut strm
        end

    (* Dump type complexity to the specified file *)
    fun dumpTyComp ( fileName : string ) ( t : TyComp ) : unit =
        let val strm = TextIO.openOut fileName
            val ()   = TextIO.output ( strm, showTyComp t )
        in TextIO.closeOut strm
        end

    (* Dump learning program parameters to the specified file *)
    fun dumpParameters (fileName:string) (ty:Ty) : unit = 
	let val strm = TextIO.openOut fileName
            val () = TextIO.output(strm, parametersToString())
	    val () = TextIO.output(strm, "Complexity of derived type:\n\t")
	    (* remove dependency of this file to the structure.sml *)
	    val () = TextIO.output(strm, showTyComp(getComps (measure ty)))
	in
	    TextIO.closeOut strm
	end

    fun dumpPADSdesc (fileName:string) (ty:Ty) : string = 
	let val strm = TextIO.openOut fileName
            val (tyName, desc) = TyToPADSFile ty ((!lexName)^".p")
            val () = TextIO.output(strm,desc )
	    val () = TextIO.closeOut strm
	in
	    tyName
	end

    fun dumpAccumProgram (path:string) (descName:string) (tyName:string) : unit = 
	let val accumProgram = "#define PADS_TY(suf) "^tyName^" ## suf\n"^
                               "#include \""^descName^".h\"\n"^
                               "#include \"template/accum_report.h\"\n"

	    val strm = TextIO.openOut (path^descName^"-accum.c")
            val () = TextIO.output(strm, accumProgram)
	in
	    TextIO.closeOut strm
	end

    fun dumpAccumXMLProgram (path:string) (descName:string) (tyName:string) : unit = 
	let val accumProgram = "#define PADS_TY(suf) "^tyName^" ## suf\n"^
                               "#define PADS_TY_STR \""^descName^"\"\n"^
                               "#include \""^descName^".h\"\n"^
                               "#include \"template/accum_report_xml.h\"\n"

	    val strm = TextIO.openOut (path^descName^"-accum_xml.c")
            val () = TextIO.output(strm, accumProgram)
	in
	    TextIO.closeOut strm
	end

    fun dumpXMLProgram (path:string) (descName:string) (tyName:string) : unit = 
	let val accumProgram = "#define PADS_TY(suf) "^tyName^" ## suf\n"^
                               "#include \""^descName^".h\"\n"^
                               "#include \"template/read_orig_write_xml.h\"\n"

	    val strm = TextIO.openOut (path^descName^"-xml.c")
            val () = TextIO.output(strm, accumProgram)
	in
	    TextIO.closeOut strm
	end

    fun dumpTyInfo ( path : string ) ( descName : string ) ( baseTy : Ty ) ( rewrittenTy : Ty ) ( ct : ComputeTimes ) : unit = 
	let fun dumpTBDs (ty:Ty):unit = 
		case ty
                of Base (aux,tls) =>
                     if !printIDs
                     then dumpCL (path^(getLabelString aux)) (List.map (fn ty=>[ty]) tls)
                     else ()
                 | TBD(aux,i, cl)    => dumpCL (path ^ "TBD_"^(Int.toString i)) cl
                 | Bottom(aux,i, cl) => dumpCL (path ^ "BTM_"^(Int.toString i)) cl
                 | Pstruct (aux,tys) => List.app dumpTBDs tys
                 | Punion (aux,tys) => List.app dumpTBDs tys
                 | Parray (aux,{first=ty1,body=ty2,last=ty3,...}) => List.app dumpTBDs [ty1,ty2,ty3]
                 | RefinedBase (aux, r,tl ) => ()  (* to be filled in *)
                 | Switch (aux, id, labeledTys) => ()(* to be filled in *)
                 | RArray _ => () (* to be filled in *)
		 | Poption _ => () (* to be filled in *)
	    fun cpMkFile () = 
		let val fileName = path^"GNUmakefile"
		    in
			ignore (TextIO.openIn fileName)
			    handle Io => 
			     (let val cpcmd = "cp "^(!executableDir)^"/GNUMakefile.output "^fileName
			      in
				  print "copy command: "; print cpcmd; print "\n";
				  OS.Process.system cpcmd;
				  ()
			      end)
		end
    	in  
          ( print "\nOutputing partitions to directory: "; print path; print "\n";
            if OS.FileSys.isDir path handle SysErr => 
		(OS.FileSys.mkDir path; true)
            then ( dumpParameters (path ^ "Params") rewrittenTy
                 ; dumpTBDs rewrittenTy
                 ; dumpTy (path ^ "Ty") rewrittenTy
                 ; dumpTyComp ( path ^ "BaseComplexity" ) ( getComps baseTy )
                 ; dumpTyComp ( path ^ "Complexity" ) ( getComps rewrittenTy )
                 ; dumpComputeTimes ( path ^ "Timing" ) ct
                 ; let val tyName = dumpPADSdesc(path^descName^".p") rewrittenTy
                   in 
		       dumpAccumProgram path descName tyName;
		       dumpAccumXMLProgram path descName tyName;
		       dumpXMLProgram path descName tyName
		   end;
		   print "Excutable directory:"; print (!executableDir); print "\n";
		   cpMkFile()
                 )
            else print "Output path should specify a directory.\n"
          )
	end
end
