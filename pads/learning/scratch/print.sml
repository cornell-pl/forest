structure Printing = struct
    open Types
    open Model
    open Times
    open Gold

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
    fun dumpTy ( fileName : string ) ( ty : Ty ) : unit = 
        let val () = print ("Printing ty to file: "^fileName^"\n")
	    val strm = TextIO.openOut fileName
            val ()   = TextIO.output(strm, TyToString ty)
        in
            TextIO.closeOut strm 
        end 

    (* Dump type complexity to the specified file *)
    fun dumpTyComp ( path : string ) ( fileName : string ) ( descName : string )
                   ( t : TyComp ) : unit =
        let val strm  = TextIO.openOut ( path ^ fileName )
            (* WARNING: hard wired path to data file *)
            val nbits = OS.FileSys.fileSize ( "data/" ^ descName ) * 8
            val ()    = TextIO.output ( strm, showTyCompNormalized nbits t )
        in TextIO.closeOut strm
        end

    (* Dump a string to the specified file *)
    fun dumpString ( fileName : string ) ( s : string ) : unit =
        let val strm = TextIO.openOut fileName
            val ()   = TextIO.output ( strm, s )
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
	let val xmlProgram = "#define PADS_TY(suf) "^tyName^" ## suf\n"^
                               "#include \""^descName^".h\"\n"^
                               "#include \"template/read_orig_write_xml.h\"\n"

	    val strm = TextIO.openOut (path^descName^"-xml.c")
            val () = TextIO.output(strm, xmlProgram)
	in
	    TextIO.closeOut strm
	end

    fun dumpFmtProgram (path:string) (descName:string) (tyName:string) (sep): unit = 
	let val separator = case sep of NONE => "| /*WARNING: separator occurs in data file*/"
	                    | SOME t => tokenToString t
	    val FmtProgram = "#define PADS_TY(suf) "^tyName^" ## suf\n"^
                               "#include \""^descName^".h\"\n"^
                               "#define DELIMS \""^separator^" \"\n"^
                               "#include \"template/read_format.h\"\n"

	    val strm = TextIO.openOut (path^descName^"-fmt.c")
            val () = TextIO.output(strm, FmtProgram)
	in
	    TextIO.closeOut strm
	end

    fun dumpTyInfo ( path : string ) ( descName : string ) ( baseTy : Ty ) ( rewrittenTy : Ty ) ( et : EndingTimes) (sep:Token option) : unit = 
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
	    fun cpFile src dest = 
		let val fileName = path^src
		    in
			ignore (TextIO.openIn fileName)
			    handle Iox => 
			     (let val cpcmd = "cp "^(!executableDir)^"/"^dest^" "^fileName
			      in
				  print "copy command: "; print cpcmd; print "\n";
				  OS.Process.system cpcmd;
				  ()
			      end)
		end
	    fun cpMkFile () = cpFile "GNUmakefile" "GNUmakefile.output"
            fun cpTokenFile tokenFileName = cpFile tokenFileName tokenFileName
    	in  
          ( print "\nOutputing partitions to directory: "; print path; print "\n"
          ; print ( "descName.1 = " ^ descName ^ "\n")
          ; if OS.FileSys.isDir path handle SysErr => 
		(OS.FileSys.mkDir path; true)
            then ( dumpParameters (path ^ "Params") rewrittenTy
                 ; dumpTBDs rewrittenTy
                 ; dumpTy (path ^ "Ty") rewrittenTy 
                 ; dumpTyComp path "BaseComplexity" descName ( getComps baseTy ) 
                 ; dumpTyComp path "Complexity" descName ( getComps rewrittenTy )
                 ; let val tyName = dumpPADSdesc(path^descName^".p") rewrittenTy
		      val ct = getComputeTimes (updatePadsEnd (Time.now()) et)
                   in 
		       print ("Ty name ="^tyName^"\n");
		       dumpAccumProgram path descName tyName;
		       dumpAccumXMLProgram path descName tyName;
		       dumpXMLProgram path descName tyName;
		       dumpFmtProgram path descName tyName sep;
                       dumpComputeTimes ( path ^ "Timing" ) ct
		   end
		 ; print "Excutable directory:"; print (!executableDir); print "\n"
                 ; print ( "descName.2 = " ^ descName ^ "\n")
                 ; cpMkFile()
                 ; cpFile "vanilla.p" "vanilla.p"
                 ; cpFile "tokens.p" "tokens.p"
                 )
            else print "Output path should specify a directory.\n"
          )
	end
end
