structure PBaseTys = 

struct
   type baseInfoTy = {padsname : Atom.atom, 
		      repname  : Atom.atom, 
                      emname   : Atom.atom,
                      edname   : Atom.atom,
  		      readname : Atom.atom,
		      scanname : Atom.atom option,
		      accname  : Atom.atom option,
		      diskSize : TyProps.diskSize,
                      memChar  : TyProps.memChar,
		      endian   : bool}


   fun printEntry {padsname : Atom.atom, 
		      repname  : Atom.atom, 
                      emname   : Atom.atom,
                      edname   : Atom.atom,
  		      readname : Atom.atom,
		      scanname : Atom.atom option,
		      accname  : Atom.atom option,
		      diskSize : TyProps.diskSize,
		      memChar  : TyProps.memChar,
		      endian   : bool} = (
    (print (String.concat["padsname = ", (Atom.toString padsname), "\n"]));
    (print (String.concat["repname = ", Atom.toString repname, "\n"]));
    (print (String.concat["emname = ", Atom.toString emname, "\n"]));
    (print (String.concat["edname = ", Atom.toString edname, "\n"]));
    (print (String.concat["readname = ", Atom.toString readname, "\n"]));
    (print (String.concat["scanname = ", case scanname of NONE => "-" | SOME n =>  Atom.toString n, "\n"]));
    (print (String.concat["accname = ", case accname of NONE => "-" | SOME n =>  Atom.toString n, "\n"]));
    (print (String.concat["diskSize = ", 
			  case diskSize of TyProps.Size (n,r) => Int.toString n 
                                         | TyProps.Param =>  "P"
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
			 emname   = Atom.atom(List.nth(fields,2)),
			 edname   = Atom.atom(List.nth(fields,3)),
			 readname = Atom.atom(List.nth(fields,4)),
			 scanname = if List.nth(fields,5) = "-" then NONE
				    else SOME (Atom.atom(List.nth(fields,5))),
			 accname  = if List.nth(fields,6) = "-" then NONE
				    else SOME (Atom.atom(List.nth(fields,6))),
			 diskSize = let val str = List.nth(fields,7) 
				    in
			               if str = "P" then TyProps.Param 
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

   fun buildBaseInfo () = 
       let val strm = TextIO.openIn("ckit/src/ast/extensions/pads/base-ty-info.txt")
           fun loop(s) = if s = ""
	                 then []
                         else processLine(s) @ (loop(TextIO.inputLine strm))
       in
           loop(TextIO.inputLine strm)
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


  fun genTypedef strm (r:baseInfoTy) = 
      (TextIO.output(strm, "typedef ");
       TextIO.output(strm, Atom.toString(#repname(r)));
       TextIO.output(strm, "\t");
       TextIO.output(strm, Atom.toString(#padsname(r)));
       TextIO.output(strm, ";\n"))

  fun genPadsInternal(filename) = 
      let val outStrm = TextIO.openOut(filename)
      in
	  baseInfoList := buildBaseInfo();
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