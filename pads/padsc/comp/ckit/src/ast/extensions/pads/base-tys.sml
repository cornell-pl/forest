structure PBaseTys = 

struct
   type baseInfoTy = {padsname : Atom.atom, 
		      repname  : Atom.atom, 
                      mname    : Atom.atom,
                      pdname   : Atom.atom,
  		      readname : Atom.atom,
		      scanname : Atom.atom option,
		      accname  : Atom.atom option,
		      diskSize : TyProps.diskSize,
                      memChar  : TyProps.memChar,
		      endian   : bool}


   fun printEntry    {padsname : Atom.atom, 
		      repname  : Atom.atom, 
                      mname    : Atom.atom,
                      pdname   : Atom.atom,
  		      readname : Atom.atom,
		      scanname : Atom.atom option,
		      accname  : Atom.atom option,
		      diskSize : TyProps.diskSize,
		      memChar  : TyProps.memChar,
		      endian   : bool} = (
    (print (String.concat["padsname = ", (Atom.toString padsname), "\n"]));
    (print (String.concat["repname = ", Atom.toString repname, "\n"]));
    (print (String.concat["mname = ", Atom.toString mname, "\n"]));
    (print (String.concat["pdname = ", Atom.toString pdname, "\n"]));
    (print (String.concat["readname = ", Atom.toString readname, "\n"]));
    (print (String.concat["scanname = ", case scanname of NONE => "-" | SOME n =>  Atom.toString n, "\n"]));
    (print (String.concat["accname = ", case accname of NONE => "-" | SOME n =>  Atom.toString n, "\n"]));
    (print (String.concat["diskSize = ", 
			  case diskSize of TyProps.Size (n,r) => Int.toString n 
                                         | TyProps.Param (s, _) =>  ("P"^s)
                                         | TyProps.Variable => "V", "\n"]));
    (print (String.concat["memory characteristic = ", 
			  case memChar of TyProps.Static => "S"
                                        | TyProps.Dynamic =>  "D", "\n" ]));
    (print (String.concat["supports endian recovery = ", 
			  if endian then "Y" else "N", 
			  "\n"]));
    print "\n")

   fun processLine s = 
       if String.isPrefix "#" s then [] 
       else 
	   let val fields = String.tokens (fn c => c = #" " orelse c = #"\n") s
	       val r = if (List.length fields >=10 ) then 
	               [{padsname = Atom.atom(List.nth(fields,0)),
			 repname  = Atom.atom(List.nth(fields,1)),
			 mname    = Atom.atom(List.nth(fields,2)),
			 pdname   = Atom.atom(List.nth(fields,3)),
			 readname = Atom.atom(List.nth(fields,4)),
			 scanname = if List.nth(fields,5) = "-" then NONE
				    else SOME (Atom.atom(List.nth(fields,5))),
			 accname  = if List.nth(fields,6) = "-" then NONE
				    else SOME (Atom.atom(List.nth(fields,6))),
			 diskSize = let val str = List.nth(fields,7) 
				    in
			               if str = "P" then TyProps.Variable
				       else if String.isPrefix "P" str then TyProps.Param (String.extract(str, 1, NONE), ref NONE) 
					   before print (String.extract(str,1,NONE))
				       else if str = "V" then TyProps.Variable
				       else case Int.fromString str
					    of NONE => TyProps.Variable
					    | SOME n => TyProps.Size (n,0)
				    end,
			 memChar  = if "S" =  List.nth(fields,8) then TyProps.Static else TyProps.Dynamic,
		         endian   = if "Y" =  List.nth(fields,9) then true else false}]
		       else []
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

  fun buildBaseInfoList (paths) = List.concat(List.map buildBaseInfo paths)

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
	  TextIO.output(outStrm, "#include \"libpadsc.h\"\n");
	  TextIO.output(outStrm, "#include \"libpadsc-internal.h\"\n");
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