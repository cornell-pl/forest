structure PBaseTys = struct
   structure PT = ParseTree

   type baseInfoTy = {padsname : Atom.atom, 
		      repname  : Atom.atom, 
                      mname    : Atom.atom,
                      pdname   : Atom.atom,
  		      readname : Atom.atom,
		      scanname : Atom.atom option,
		      accname  : Atom.atom option,
		      diskSize : TyProps.diskSize,
                      memChar  : TyProps.memChar,
		      numArgs  : int,
		      endian   : bool}


   fun printEntry    {padsname : Atom.atom, 
		      repname  : Atom.atom, 
		      numArgs  : int,
                      mname    : Atom.atom,
                      pdname   : Atom.atom,
  		      readname : Atom.atom,
		      scanname : Atom.atom option,
		      accname  : Atom.atom option,
		      diskSize : TyProps.diskSize,
		      memChar  : TyProps.memChar,
		      endian   : bool} = (
    (print (String.concat["padsname = ", (Atom.toString padsname), "\n"]));
    (print (String.concat["numArgs= ", (Int.toString numArgs), "\n"]));
    (print (String.concat["repname = ", Atom.toString repname, "\n"]));
    (print (String.concat["mname = ", Atom.toString mname, "\n"]));
    (print (String.concat["pdname = ", Atom.toString pdname, "\n"]));
    (print (String.concat["readname = ", Atom.toString readname, "\n"]));
    (print (String.concat["scanname = ", case scanname of NONE => "-" | SOME n =>  Atom.toString n, "\n"]));
    (print (String.concat["accname = ", case accname of NONE => "-" | SOME n =>  Atom.toString n, "\n"]));
    (print (String.concat["diskSize = ", 
			  case diskSize of TyProps.Size (n,r) => IntInf.toString n 
                                         | TyProps.Param (params, SOME s, exp, _) =>  ("P"^s)
                                         | TyProps.Param (params, NONE, exp,_) =>  ("P")
                                         | TyProps.Variable => "V", "\n"]));
    (print (String.concat["memory characteristic = ", 
			  case memChar of TyProps.Static => "S"
                                        | TyProps.Dynamic =>  "D", "\n" ]));
    (print (String.concat["supports endian recovery = ", 
			  if endian then "Y" else "N", 
			  "\n"]));
    print "\n")


   fun extract e = 
       let fun extractDecl (PT.MARKdeclaration(m,decl)) = extractDecl decl
             | extractDecl (PT.Declaration(d, del)) = #2(hd(del))
	   fun extractStmt (PT.MARKstatement(m,st)) = extractStmt st
	     | extractStmt (PT.Compound stms) = extractStmt (hd stms)
	     | extractStmt (PT.Decl decl) = extractDecl decl
	   fun extractED (PT.MARKexternalDecl(m,ed)) = extractED ed
	     | extractED (PT.FunctionDef {body,...}) = extractStmt body
       in
	   extractED e
       end

   fun cnvStrToCExp (name, s) = 
       let val errorState = Error.mkErrState TextIO.stdErr
           val result = Parser.parseString errorState ("main(){int x="^s^";}\n") name
       in
           extract (hd result)
       end

   fun processLine s = 
       if String.isPrefix "#" s then [] 
       else 
	   let val fields = String.tokens (fn c => c = #" " orelse c = #"\n") s
	       val numColumns = List.length fields
	       val padsname = if numColumns > 0 then List.nth(fields,0) else ""
	       val errStr = "Compiler bug: error in "^padsname^" entry in base-ty-info table.\n"
               val numArgs = if numColumns > 1 then Option.valOf(Int.fromString (List.nth(fields,1))) else 0
		             handle Option => (print errStr; 0)
	       val r = if (numColumns = 0) then [] else 
		       if (numColumns >=11 ) then 
	               [{padsname = Atom.atom padsname,
			 numArgs  = numArgs,
			 repname  = Atom.atom(List.nth(fields,2)),
			 mname    = Atom.atom(List.nth(fields,3)),
			 pdname   = Atom.atom(List.nth(fields,4)),
			 readname = Atom.atom(List.nth(fields,5)),
			 scanname = if List.nth(fields,6) = "-" then NONE
				    else SOME (Atom.atom(List.nth(fields,6))),
			 accname  = if List.nth(fields,7) = "-" then NONE
				    else SOME (Atom.atom(List.nth(fields,7))),
			 diskSize = let val str = List.nth(fields,8) 
				    in
			               if str = "P" then TyProps.Variable
				       else if String.isPrefix "P" str then 
					   let val expStr = String.extract(str, 1, NONE) 
					       val exp = cnvStrToCExp(padsname, expStr)
					       val fNames =  List.take(["p1", "p2", "p3", "p4", "p5", 
									"p6", "p7", "p8", "p9", "p0"], numArgs)
					       val () = if not (ParseTreeSubst.expIsClosed(fNames, exp)) then print errStr else ()
					   in
					       TyProps.Param (fNames, SOME expStr, exp, ParseTreeUtil.zero) 
					   end handle Subscript => (print errStr; TyProps.Variable)
				       else if str = "V" then TyProps.Variable
				       else case Int.fromString str
					    of NONE => TyProps.Variable
					    | SOME n => TyProps.mkSize (n,0)
				    end,
			 memChar  = if "S" =  List.nth(fields,9) then TyProps.Static else TyProps.Dynamic,
		         endian   = if "Y" =  List.nth(fields,10) then true else false}]
		       else (print errStr; [])
	   in
	       r
	   end

   fun buildBaseInfo path = 
       let val strm = TextIO.openIn path
           fun loop(s) = if s = ""
	                 then []
                         else processLine(s) @ (loop(TextIO.inputLine strm))
       in
           loop(TextIO.inputLine strm) before (TextIO.closeIn strm)
       end

  fun buildBaseInfoList (paths) = 
      let val tble = List.concat(List.map buildBaseInfo paths)
      in
	  tble
      end

  val baseInfoList : baseInfoTy list ref = ref []

  structure PBST = RedBlackMapFn(
                     struct type ord_key = Atom.atom
			    val compare = Atom.compare
		     end) 

  type baseTyMap = baseInfoTy PBST.map

  val baseInfo : baseTyMap ref = ref PBST.empty

  fun initBaseInfoMap (baseInfoList) = 
      let fun ins m []  = m
            | ins m ((b:baseInfoTy)::bs) = ins (PBST.insert (m, #padsname b, b)) bs 
      in
	  ins (!baseInfo) baseInfoList
      end

  val find : (baseTyMap ref * Atom.atom) -> baseInfoTy option = 
      fn (bRef, a) => PBST.find(!bRef, a)

  fun listItemsi(bRef : baseTyMap ref) : (Atom.atom * baseInfoTy) list = 
      PBST.listItemsi(!bRef)

  fun genTypedef strm (r:baseInfoTy) = 
      (TextIO.output(strm, "typedef ");
       TextIO.output(strm, Atom.toString(#repname(r)));
       TextIO.output(strm, "\t");
       TextIO.output(strm, Atom.toString(#padsname(r)));
       TextIO.output(strm, ";\n"))

  fun genPadsInternal(basePaths, filename) = 
      let val outStrm = TextIO.openOut(filename)
      in
	  baseInfoList := buildBaseInfoList(basePaths);
          baseInfo := initBaseInfoMap(!baseInfoList);
	  TextIO.output(outStrm, "#ifndef __PADS_INTERNAL__H__\n");
	  TextIO.output(outStrm, "#define __PADS_INTERNAL__H__\n");
	  TextIO.output(outStrm, "#include \"padsc.h\"\n");
	  TextIO.output(outStrm, "#include \"padsc-internal.h\"\n");
          TextIO.output(outStrm, "#ifdef sfstropen\n#undef sfstropen\n#endif\n");
          TextIO.output(outStrm, "Sfio_t *sfstropen();\n");
          TextIO.output(outStrm, "#ifdef sfstrclose\n#undef sfstrclose\n#endif\n");
          TextIO.output(outStrm, "void sfstrclose(Sfio_t *);\n");
          TextIO.output(outStrm, "#ifdef sfstruse\n#undef sfstruse\n#endif\n");
          TextIO.output(outStrm, "const char* sfstruse(Sfio_t *);\n");
          List.app (genTypedef outStrm) (!baseInfoList);
	  TextIO.output(outStrm, "#endif /*  __PADS_INTERNAL__H__  */\n");
	  TextIO.flushOut outStrm;
	  TextIO.closeOut outStrm
      end


end