structure PBaseTys = 

struct
   type baseInfoTy = {padsname : Atom.atom, 
		      repname  : Atom.atom, 
                      emname   : Atom.atom,
                      edname   : Atom.atom,
  		      readname : Atom.atom,
		      scanname : Atom.atom option,
		      accname  : Atom.atom option}

   fun printEntry {padsname : Atom.atom, 
		      repname  : Atom.atom, 
                      emname   : Atom.atom,
                      edname   : Atom.atom,
  		      readname : Atom.atom,
		      scanname : Atom.atom option,
		      accname  : Atom.atom option} = (
    (print (String.concat["padsname = ", (Atom.toString padsname), "\n"]));
    (print (String.concat["repname = ", Atom.toString repname, "\n"]));
    (print (String.concat["emname = ", Atom.toString emname, "\n"]));
    (print (String.concat["edname = ", Atom.toString edname, "\n"]));
    (print (String.concat["readname = ", Atom.toString readname, "\n"]));
    (print (String.concat["scanname = ", case scanname of NONE => "-" | SOME n =>  Atom.toString n, "\n"]));
    (print (String.concat["accname = ", case accname of NONE => "-" | SOME n =>  Atom.toString n, "\n\n"])))

   fun processLine s = 
       if String.isPrefix "#" s then [] 
       else 
	   let val fields = String.tokens (fn c => c = #" " orelse c = #"\n") s
	       val r = if (List.length fields >=7 ) then 
	               [{padsname = Atom.atom(List.nth(fields,0)),
			 repname  = Atom.atom(List.nth(fields,1)),
			 emname   = Atom.atom(List.nth(fields,2)),
			 edname   = Atom.atom(List.nth(fields,3)),
			 readname = Atom.atom(List.nth(fields,4)),
			 scanname = if List.nth(fields,5) = "-" then NONE
				    else SOME (Atom.atom(List.nth(fields,5))),
			 accname  = if List.nth(fields,6) = "-" then NONE
				    else SOME (Atom.atom(List.nth(fields,6)))}]
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

  val baseInfoList = buildBaseInfo()

  fun genTypedef strm (r:baseInfoTy) = 
      (TextIO.output(strm, "typedef ");
       TextIO.output(strm, Atom.toString(#repname(r)));
       TextIO.output(strm, "\t");
       TextIO.output(strm, Atom.toString(#padsname(r)));
       TextIO.output(strm, ";\n"))
      
  fun genPadsInternal(filename) = 
      let val outStrm = TextIO.openOut(filename)
      in
	  TextIO.output(outStrm, "#ifndef __PADS_INTERNAL__H__\n");
	  TextIO.output(outStrm, "#define __PADS_INTERNAL__H__\n");
	  TextIO.output(outStrm, "#include \"libpadsc.h\"\n");
	  TextIO.output(outStrm, "#include \"libpadsc-internal.h\"\n");
          List.app (genTypedef outStrm) baseInfoList;
	  TextIO.output(outStrm, "#endif /*  __PADS_INTERNAL__H__  */\n");
	  TextIO.flushOut outStrm;
	  TextIO.closeOut outStrm
      end

  structure PBST = RedBlackMapFn(
                     struct type ord_key = Atom.atom
			    val compare = Atom.compare
		     end) 
 
  type baseTyMap = baseInfoTy PBST.map

  val baseInfo : baseTyMap = 
      let fun ins m []  = m
            | ins m ((b:baseInfoTy)::bs) = ins (PBST.insert (m, #padsname b, b)) bs 
      in
	  ins PBST.empty baseInfoList
      end

  val find : (baseTyMap * Atom.atom) -> baseInfoTy option = PBST.find

end