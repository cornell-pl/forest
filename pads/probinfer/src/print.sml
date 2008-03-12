structure Printing = struct
    open Types
    open Ast
    open Padsc_printer
    open Padsml_printer
    open Model
    open Times
    open Gold
    open Probmodel

    fun dumpLToken (strm:TextIO.outstream) (tk:Token,loc:location) : unit =
        TextIO.output(strm, tokenToString tk)

    fun dumpBSLToken (strm:TextIO.outstream) (tk:BSToken,loc:location) : unit =
        TextIO.output(strm, bstokenTyToString tk)

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

    fun dumpNewCL (fileName:string) (contexts:NewContext list) : unit = 
	let val strm = TextIO.openOut fileName
	    fun getLineNo (context:NewContext):string = 
		let fun extractLineNo (t,{lineNo,...}:location) = lineNo
		in
		    if !printLineNos then 
			case context of [] => "No line information."
                        | (lt::rest) => ((Int.toString (extractLineNo lt)) ^ ": ")
		    else ""
		end
            fun dumpOneContext (context:NewContext) : unit =
                ( TextIO.output(strm, getLineNo context)
                ; List.app (dumpBSLToken strm) context
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

    fun dumpNewTy ( fileName : string ) ( ty : NewTy ) : unit = 
        let val () = print ("Printing NewTy to file: "^fileName^"\n")
	    val strm = TextIO.openOut fileName
            val ()   = TextIO.output(strm, NewTyToString ty)
        in
            TextIO.closeOut strm 
        end 


    (* Dump type complexity to the specified file *)
    fun dumpTyComp ( path : string ) ( fileName : string ) ( dataFile : string )
                   ( t : TyComp ) : unit =
        let val strm  = TextIO.openOut ( path ^ fileName )
            (* WARNING: hard wired path to data file *)
            val nbits = OS.FileSys.fileSize ( dataFile ) * 8
            val ()    = TextIO.output ( strm, showTyCompNormalized nbits t )
        in TextIO.closeOut strm
        end

    (* Dump variance to the specified file *)
    fun dumpVariance (fileName:string) (cov:int) (variance:int) =
        let val strm  = TextIO.openOut ( fileName )
	    val normVar = (Real.fromInt variance) / (Real.fromInt cov)
            val ()    = TextIO.output ( strm, "Variance = "^ (Int.toString variance) ^ 
			" normalized by " ^ (Int.toString cov) ^ " is " ^ 
			(Real.toString normVar) ^ "\n")
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

    fun dumpNewParameters (fileName:string) (ty:NewTy) : unit = 
	let val strm = TextIO.openOut fileName
            val () = TextIO.output(strm, parametersToString())
	    val () = TextIO.output(strm, "Complexity of derived type:\n\t")
	    (* remove dependency of this file to the structure.sml *)
	    val () = TextIO.output(strm, showTyComp(getNComps (newmeasure ty)))
	in
	    TextIO.closeOut strm
	end

    (* This function dumps both pads/c and pads/ml descriptions from a ty *)
    fun dumpPADSdesc (padscFile:string) (padsmlFile:string) 
		(ty:Ty) (numHeaders:int) (numFooters:int) : (string*string*string*string) = 
	let val strmc = TextIO.openOut padscFile
	    val strmml = TextIO.openOut padsmlFile
(*
	    val irs = tyToIR true nil ty
	    val pads = lconcat (map irToPML irs)
	    val () = print pads
*)
            val (topName, headerName, bodyName, footerName, desc) = 
		tyToPADSC ty numHeaders numFooters ((!lexName)^".p")
            val descml = tyToPADSML ty numHeaders numFooters ("Build_ins")
            val () = TextIO.output(strmc,desc )
            val () = TextIO.output(strmml, descml )
	    val () = TextIO.closeOut strmc
	    val () = TextIO.closeOut strmml
	in
	    (topName, headerName, bodyName, footerName)
	end
(*
    fun dumpNewPADSdesc (padscFile:string) (padsmlFile:string) 
		(ty:NewTy) (numHeaders:int) (numFooters:int) : (string*string*string*string) = 
	let val strmc = TextIO.openOut padscFile
	    val strmml = TextIO.openOut padsmlFile
(*
	    val irs = tyToIR true nil ty
	    val pads = lconcat (map irToPML irs)
	    val () = print pads
*)
            val (topName, headerName, bodyName, footerName, desc) = 
		tyToPADSC ty numHeaders numFooters ((!lexName)^".p")
            val descml = tyToPADSML ty numHeaders numFooters ("Build_ins")
            val () = TextIO.output(strmc,desc )
            val () = TextIO.output(strmml, descml )
	    val () = TextIO.closeOut strmc
	    val () = TextIO.closeOut strmml
	in
	    (topName, headerName, bodyName, footerName)
	end
*)
    fun dumpAccumProgram (path:string) (descName:string) (hdrName:string) (tyName:string) (trlName:string) : unit = 
	let val hdrDecl = if hdrName = "" then "" else "#define PADS_HDR_TY(suf) "^hdrName^" ## suf\n"
	    val trlDecl = if trlName = "" then "" else "#define PADS_TRL_TY(suf) "^trlName^" ## suf\n"
	    val accumProgram = hdrDecl ^
		               "#define PADS_TY(suf) "^tyName^" ## suf\n"^
			       trlDecl ^
                               "#include \""^descName^".h\"\n"^
                               "#include \"template/accum_report.h\"\n"

	    val strm = TextIO.openOut (path^descName^"-accum.c")
            val () = TextIO.output(strm, accumProgram)
	in
	    TextIO.closeOut strm
	end

    fun dumpAccumXMLProgram (path:string) (descName:string) (hdrName:string) (tyName:string) (trlName) : unit = 
	let 
	    val hdrDecl = if hdrName = "" then "" else "#define PADS_HDR_TY(suf) "^hdrName^" ## suf\n"
	    val trlDecl = if trlName = "" then "" else "#define PADS_TRL_TY(suf) "^trlName^" ## suf\n"
	    val accumProgram = hdrDecl ^
		               "#define PADS_TY(suf) "^tyName^" ## suf\n"^
			       trlDecl ^
                               "#define PADS_TY_STR \""^descName^"\"\n"^
                               "#include \""^descName^".h\"\n"^
                               "#include \"template/accum_report_xml.h\"\n"

	    val strm = TextIO.openOut (path^descName^"-accum_xml.c")
            val () = TextIO.output(strm, accumProgram)
	in
	    TextIO.closeOut strm
	end

    fun dumpPADX (path:string) (descName:string) (tyName:string) : unit = 
	let val padxPrelude =  "#define PADS_TY_ " ^ tyName ^"\n" ^
                               "#define PADS_TY(suf) "^tyName^" ## suf\n"^
	                       "#define PPADS_TY(pref) pref ## "^tyName^"\n"^
			       "#define PADS_TY_STR \""^descName^"\"\n"^
                               "#include \""^descName^".h\"\n"
	    val padxLoad =     padxPrelude^
                               "#include \"template/pglx_load.h\"\n"
	    val padxBulk =     padxPrelude^
                               "#include \"template/pglx_bulk_query.h\"\n"
	    val padxSmart =    padxPrelude^
                               "#include \"template/pglx_smart_query.h\"\n"

	    val strm1 = TextIO.openOut (path^"load_"^descName^".c")
            val () = TextIO.output(strm1, padxLoad)
	    val strm2 = TextIO.openOut (path^"bulk_"^descName^".c")
            val () = TextIO.output(strm2, padxBulk)
	    val strm3 = TextIO.openOut (path^"smart_"^descName^".c")
            val () = TextIO.output(strm3, padxSmart)
	in
	    TextIO.closeOut strm1;
	    TextIO.closeOut strm2;
	    TextIO.closeOut strm3
	end

    fun dumpXMLProgram (path:string) (descName:string) (topName:string) (hdrName:string) (tyName:string) (trlName:string): unit = 
	let val (copyStringDecl,readTy) = if hdrName = "" andalso trlName = "" then ("",tyName) else ("#define COPY_STRINGS 1\n",topName)
	    val xmlProgram = "#define PADS_TY(suf) "^readTy^" ## suf\n"^
		             copyStringDecl ^
                             "#include \""^descName^".h\"\n"^
                             "#include \"template/read_orig_write_xml.h\"\n"

	    val strm = TextIO.openOut (path^descName^"-xml.c")
            val () = TextIO.output(strm, xmlProgram)
	in
	    TextIO.closeOut strm
	end

    fun dumpFmtProgram (path:string) (descName:string) (hdrName:string) (tyName:string) (trlName:string) (sep): unit = 
	let val hdrDecl = if hdrName = "" then "" else "#define PADS_HDR_TY(suf) "^hdrName^" ## suf\n"
	    val trlDecl = if trlName = "" then "" else "#define PADS_TRL_TY(suf) "^trlName^" ## suf\n"
	    val isUnion = String.isPrefix "Union" tyName 
	    val (separator,warning) = case (sep,isUnion) of 
		              (NONE,false) => ("|",  "  /*WARNING: separator occurs in data file*/")
		            | (NONE,true) =>  ("||", "  /*WARNING: separator occurs in data file*/")  
	                    | (SOME t,false) => (tokenToString t,"")
	                    | (SOME t,true) => let val ts = tokenToString t in (ts^ts,"") end
	    
	    val FmtProgram =   hdrDecl ^
		               "#define PADS_TY(suf) "^tyName^" ## suf\n"^
			       trlDecl ^
                               "#include \""^descName^".h\"\n"^
                               "#define DELIMS \""^separator^" \""^warning^"\n"^
                               "#include \"template/read_format.h\"\n"

	    val strm = TextIO.openOut (path^descName^"-fmt.c")
            val () = TextIO.output(strm, FmtProgram)
	in
	    TextIO.closeOut strm
	end

    fun dumpGrapher (path:string) (dataDir: string) (descName:string) (ty:Ty) (sep:Token option) : unit =
	case ty of 
	Pstruct (_, tys) =>
		let
		  fun getBaseTypes (tys : Ty list) (index:int) : string =
			case tys of
			  nil =>  ""
			  | ty::tail => 
				(
				  case ty of 
				    Base (a, (tok, _)::_) =>
				      let val label = getLabelString a
				      in
					(
					 case tok of
						Pint _ => label ^ " Col #"^(Int.toString index)^"\t [Int]\n"
					  |	Pfloat _ => label ^ " Col #"^(Int.toString index)^"\t [Float]\n"
					  |	Pdate _ => label ^ " Col #"^(Int.toString index)^"\t [Date]\n"
					  |	Ptime _ => label ^ " Col #"^(Int.toString index)^"\t [Time]\n"
					  (*|	Pstring _ => label ^ "Col #"^(Int.toString index)^"\t [String]\n"*)
					  (*|	Other _ => label ^ "Col #"^(Int.toString index)^"\t [Char]\n"*)
					  |	_ => ""
					) ^ (getBaseTypes tail (Int.+ (index, 1)))
				      end
				  | RefinedBase (a, re, _) =>
				      let val label = getLabelString a
				      in
					(
					 case re of
						Int _ => label ^ " Col #"^(Int.toString index)^"\t [Int]\n" ^ 
							 (getBaseTypes tail (Int.+ (index, 1))) 
					  |	Enum (r::_) =>
						(
						  case r of 
							IntConst _ => label ^ " Col #"^(Int.toString index)^"\t [Int]\n"
							(*| StringConst _ => "Col #"^(Int.toString index)^"\t [String]\n"*)
							| _ => ""
						) ^ (getBaseTypes tail (Int.+ (index, 1))) 	
					  | 	StringConst _ => (getBaseTypes tail index)
					  | _ => (getBaseTypes tail (Int.+ (index, 1))) 
					)	
				      end
				  | _ => (getBaseTypes tail (Int.+ (index, 1)))
				) 
		  val graphOutStr:string = getBaseTypes tys 1 
		  val sepStr = case sep of 
				SOME tok => tokenToString tok
				| NONE => "none"
	    	  val strm = TextIO.openOut (path^descName^".graph")
            	  val () = TextIO.output(strm, "Sep: "^sepStr^"\n"^graphOutStr)
	    	  val () = TextIO.closeOut strm
	    	  val strm = TextIO.openOut (path^descName^"-graph")
		  val datapath = if OS.Path.isAbsolute dataDir then dataDir
				 else OS.Path.mkAbsolute {path=dataDir, relativeTo=OS.FileSys.getDir ()}
		  val () = TextIO.output(strm, "#!/usr/bin/perl\n$desc = \"" ^ descName ^ "\";\n" ^
			"$datapath = \"" ^ datapath ^"\";\n")
	    	  val () = TextIO.closeOut strm
		  val templatePath = (!executableDir)^"/scripts/grapher.template"
		  val status = OS.Process.system ("cat "^templatePath^" >> " ^ path ^ descName ^ "-graph")
		  val status = OS.Process.system ("chmod u+x "^ path ^ descName ^ "-graph")
		in
			()
		end
		  
	| _ => (* not meaningful for grapher, hence not output anything *)
		print "Data not suitable for graphing!\n"


    fun dumpNewGrapher (path:string) (dataDir: string) (descName:string) (ty:NewTy) (sep:Token option) : unit =
	case ty of 
	PPstruct (_, tys) =>
		let
		  fun getBaseTypes (tys : NewTy list) (index:int) : string =
			case tys of
			  nil =>  ""
			  | ty::tail => 
				(
				  case ty of 
				    PPBase (a, (tok, _)::_) =>
				      let val label = getLabelString a
				      in
					(
					 case tok of
						(PPint, _) => label ^ " Col #"^(Int.toString index)^"\t [Int]\n"
					  |	(PPfloat, _) => label ^ " Col #"^(Int.toString index)^"\t [Float]\n"
					  |	(PPdate, _) => label ^ " Col #"^(Int.toString index)^"\t [Date]\n"
					  |	(PPtime, _) => label ^ " Col #"^(Int.toString index)^"\t [Time]\n"
					  (*|	Pstring _ => label ^ "Col #"^(Int.toString index)^"\t [String]\n"*)
					  (*|	Other _ => label ^ "Col #"^(Int.toString index)^"\t [Char]\n"*)
					  |	_ => ""
					) ^ (getBaseTypes tail (Int.+ (index, 1)))
				      end
				  | PPRefinedBase (a, re, _) =>
				      let val label = getLabelString a
				      in
					(
					 case re of
						Int _ => label ^ " Col #"^(Int.toString index)^"\t [Int]\n" ^ 
							 (getBaseTypes tail (Int.+ (index, 1))) 
					  |	Enum (r::_) =>
						(
						  case r of 
							IntConst _ => label ^ " Col #"^(Int.toString index)^"\t [Int]\n"
							(*| StringConst _ => "Col #"^(Int.toString index)^"\t [String]\n"*)
							| _ => ""
						) ^ (getBaseTypes tail (Int.+ (index, 1))) 	
					  | 	StringConst _ => (getBaseTypes tail index)
					  | _ => (getBaseTypes tail (Int.+ (index, 1))) 
					)	
				      end
				  | _ => (getBaseTypes tail (Int.+ (index, 1)))
				) 
		  val graphOutStr:string = getBaseTypes tys 1 
		  val sepStr = case sep of 
				SOME tok => tokenToString tok
				| NONE => "none"
	    	  val strm = TextIO.openOut (path^descName^".graph")
            	  val () = TextIO.output(strm, "Sep: "^sepStr^"\n"^graphOutStr)
	    	  val () = TextIO.closeOut strm
	    	  val strm = TextIO.openOut (path^descName^"-graph")
		  val datapath = if OS.Path.isAbsolute dataDir then dataDir
				 else OS.Path.mkAbsolute {path=dataDir, relativeTo=OS.FileSys.getDir ()}
		  val () = TextIO.output(strm, "#!/usr/bin/perl\n$desc = \"" ^ descName ^ "\";\n" ^
			"$datapath = \"" ^ datapath ^"\";\n")
	    	  val () = TextIO.closeOut strm
		  val templatePath = (!executableDir)^"/scripts/grapher.template"
		  val status = OS.Process.system ("cat "^templatePath^" >> " ^ path ^ descName ^ "-graph")
		  val status = OS.Process.system ("chmod u+x "^ path ^ descName ^ "-graph")
		in
			()
		end
		  
	| _ => (* not meaningful for grapher, hence not output anything *)
		print "Data not suitable for graphing!\n"

    fun dumpTyInfo ( path : string ) (dataDir: string) ( inputFileName : string ) ( baseTy : Ty ) 
			( rewrittenTy : Ty ) (numHeaders: int) (numFooters: int)
			( et : EndingTimes) (sep:Token option) : unit = 
	let val descName = !descName
	    fun dumpTBDs (ty:Ty):unit = 
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
		    val destName = (!executableDir)^"/include/"^dest
		    in
			ignore (TextIO.openIn fileName)
			    handle Iox => 
			     (let val cpcmd = "cp "^destName^" "^fileName
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
          ; if OS.FileSys.isDir path handle SysErr => 
		(OS.FileSys.mkDir path; true)
            then ( dumpParameters (path ^ "Params") rewrittenTy
                 ; dumpTBDs rewrittenTy
                 ; dumpTy (path ^ "Ty") rewrittenTy 
                 ; dumpTyComp path "BaseComplexity" (dataDir^"/"^inputFileName) ( getComps baseTy ) 
                 ; dumpTyComp path "Complexity" (dataDir^"/"^inputFileName) ( getComps rewrittenTy )
                 ; print "Finished printing Complexity\n"
                 ; let val (topName, hdrName, tyName, trlName) = dumpPADSdesc (path^descName^".p") (path^descName^".pml") 
						rewrittenTy numHeaders numFooters
		      val ct = getComputeTimes (updatePadsEnd (Time.now()) et)
                   in 
		       print ("Ty name = "^tyName^"\n");
		       dumpAccumProgram path descName hdrName tyName trlName;
		       dumpAccumXMLProgram path descName hdrName tyName trlName;
		       dumpXMLProgram path descName topName hdrName tyName trlName;
		       dumpPADX path descName tyName;
		       dumpFmtProgram path descName hdrName tyName trlName sep;
		       dumpGrapher path dataDir descName rewrittenTy sep;
                       dumpComputeTimes ( path ^ "Timing" ) ct; 
		       dumpVariance ( path ^ "Variance" ) (getCoverage rewrittenTy) (variance rewrittenTy)
		   end
                 ; cpMkFile()
                 ; cpFile "vanilla.p" "vanilla.p"
                 ; cpFile "tokens.p" "tokens.p"
                 )
            else print "Output path should specify a directory.\n"
          )
	end


    fun dumpNewTyInfo ( path : string ) (dataDir: string) ( inputFileName : string ) ( baseTy : NewTy ) 
			( rewrittenTy : NewTy ) (numHeaders: int) (numFooters: int)
			( et : EndingTimes) (sep:Token option) : unit = 
	let val descName = !descName
	    fun dumpTBDs (ty:NewTy):unit = 
		case ty
                of PPBase (aux,tls) =>
                     if !printIDs
                     then dumpNewCL (path^(getLabelString aux)) (List.map (fn ty=>[ty]) tls)
                     else ()
                 | PPTBD(aux,i, cl)    => dumpNewCL (path ^ "TBD_"^(Int.toString i)) cl
                 | PPBottom(aux,i, cl) => dumpNewCL (path ^ "BTM_"^(Int.toString i)) cl
                 | PPstruct (aux,tys) => List.app dumpTBDs tys
                 | PPunion (aux,tys) => List.app dumpTBDs tys
                 | PParray (aux,{first=ty1,body=ty2,last=ty3,...}) => List.app dumpTBDs [ty1,ty2,ty3]
                 | PPRefinedBase (aux, r,tl ) => ()  (* to be filled in *)
                 | PPSwitch (aux, id, labeledTys) => ()(* to be filled in *)
                 | PPRArray _ => () (* to be filled in *)
		 | PPoption _ => () (* to be filled in *)
	    fun cpFile src dest = 
		let val fileName = path^src
		    val destName = (!executableDir)^"/include/"^dest
		    in
			ignore (TextIO.openIn fileName)
			    handle Iox => 
			     (let val cpcmd = "cp "^destName^" "^fileName
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
          ; if OS.FileSys.isDir path handle SysErr => 
		(OS.FileSys.mkDir path; true)
            then ( dumpNewParameters (path ^ "Params") rewrittenTy
                 ; dumpTBDs rewrittenTy
(*                 ; dumpNewTy (path ^ "Ty") rewrittenTy *) (* dump ty in main.sml *)
                 ; dumpTyComp path "NewBaseComplexity" (dataDir^"/"^inputFileName) ( getNComps baseTy ) 
                 ; dumpTyComp path "NewComplexity" (dataDir^"/"^inputFileName) ( getNComps rewrittenTy )
                 ; print "Finished printing Complexity\n"
                 ; let (* val (topName, hdrName, tyName, trlName) = dumpPADSdesc (path^descName^".p") (path^descName^".pml") 
						rewrittenTy numHeaders numFooters *)
		      val ct = getComputeTimes (updatePadsEnd (Time.now()) et)
                   in 
(*
		       print ("Ty name = "^tyName^"\n");
		       dumpAccumProgram path descName hdrName tyName trlName;
		       dumpAccumXMLProgram path descName hdrName tyName trlName;
		       dumpXMLProgram path descName topName hdrName tyName trlName;
		       dumpPADX path descName tyName;
		       dumpFmtProgram path descName hdrName tyName trlName sep;
		       dumpNewGrapher path dataDir descName rewrittenTy sep;
*)
                       dumpComputeTimes ( path ^ "NewTiming" ) ct; 
		       dumpVariance ( path ^ "NewVariance" ) (getNCoverage rewrittenTy) (newvariance rewrittenTy)
		   end
                 ; cpMkFile()
                 ; cpFile "vanilla.p" "vanilla.p"
                 ; cpFile "tokens.p" "tokens.p"
                 )
            else print "Output path should specify a directory.\n"
          )
	end

end
