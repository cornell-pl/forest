structure Printing = 
struct
    open Types
    open Structure


   
    fun dumpLToken strm (tk,loc) = TextIO.output(strm, tokenToString tk)
    fun dumpCL fileName contexts = 
	let val strm = TextIO.openOut fileName
	    fun getLineNo context = 
		let fun extractLineNo (t,{lineNo,...}:location) = lineNo
		in
		    if !printLineNos then 
			case context of [] => "No line information."
                        | (lt::rest) => ((Int.toString (extractLineNo lt)) ^ ": ")
		    else ""
		end
            fun dumpOneContext context = (TextIO.output(strm, getLineNo context);
					  List.app (dumpLToken strm) context;
					  TextIO.output(strm, "\n"))
            val () = List.app dumpOneContext contexts
	in
	    TextIO.closeOut strm
	end

    fun dumpTy fileName ty = 
	let val strm = TextIO.openOut fileName
            val () = TextIO.output(strm, TyToString ty)
	in
	    TextIO.closeOut strm
	end

    fun dumpParameters fileName ty = 
	let val strm = TextIO.openOut fileName
            val () = TextIO.output(strm, parametersToString())
	    val () = TextIO.output(strm, "Complexity of derived type:\n\t")
	    val () = TextIO.output(strm, complexityToString(complexity ty))
	in
	    TextIO.closeOut strm
	end

    fun dumpTyInfo path ty = 
	let fun dumpTBDs ty = 
		case ty
                of Base (aux,tls) => if !printIDs then dumpCL (path^(getLabelString aux)) (List.map (fn ty=>[ty]) tls) else ()
                |  TBD(aux,i, cl)    => dumpCL (path^"TBD_"^(Int.toString i)) cl
                |  Bottom(aux,i, cl) => dumpCL (path^"BTM_"^(Int.toString i)) cl
	        |  Pstruct (aux,tys) => List.app dumpTBDs tys
	        |  Punion (aux,tys) => List.app dumpTBDs tys
	        |  Parray (aux,{first=ty1,body=ty2,last=ty3,...}) => List.app dumpTBDs [ty1,ty2,ty3]
                |  RefinedBase (aux, r,tl ) => ()  (* to be filled in *)
                |  Switch (aux, id, labeledTys) => ()(* to be filled in *)
                |  RArray _ => () (* to be filled in *)
    	in  
          (print "Complexity of inferred type:\n\t";
	   printComplexity (complexity ty);
	   print "\nOutputing partitions to directory: "; print path; print "\n";
	   if OS.FileSys.isDir path handle SysErr => (OS.FileSys.mkDir path; true)
	   then (dumpParameters (path^"Params") ty; dumpTBDs ty; dumpTy (path^"Ty") ty)
	   else print "Output path should specify a directory.\n"
          )
	end
end