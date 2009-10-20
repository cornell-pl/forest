structure Pxml = struct
  exception InvalidXML
  exception InvalidPath
  open Types
  structure StringMap = SplayMapFn(struct
                 type ord_key = string
                 val compare = String.compare
        end)

  val env : string StringMap.map ref = ref StringMap.empty
  datatype xmltype = 
	Element of (string * xmltype list)
      | PCData of string 
      | Comment of string 

  fun toString prefix r =
	case r of
	  PCData s => prefix ^ s ^ "\n"
	| Comment s => prefix ^ s ^ "\n"
	| Element (s, elements) =>
		prefix ^ "<" ^ s ^ ">\n"
		^ String.concat (map (toString (prefix ^ "    ")) elements) 
		^ prefix ^ "</" ^ s ^ ">\n"

  fun unescape s = 
  (* this is the dual function of escape - it converts \\ to \ or single \ to nothing *) 
    let val chars = String.explode s
	fun process remainder result =
	  case remainder of
	    c::remainder => 
		if c = #"\\" then
		(
		  case remainder of
		    nil => List.rev result
		| d::remainder1 =>
			if d = #"\\" then process remainder1 (#"\\"::result)
		        else process remainder result
		)
		else process remainder (c::result)
	  | nil => List.rev result
	val escaped_chars = process chars nil
    in
	String.implode escaped_chars
    end

  (* function to strip the single quotes or double quotes around a literal string *) 
  fun strip s =
   if (String.sub(s, 0) = #"\"" andalso String.sub(s, (size s)-1) = #"\"")
      orelse (String.sub(s, 0) = #"'" andalso String.sub(s, (size s)-1) = #"'")
   then String.substring (s, 1, (size s) - 2)
   else s

(*****************
   fun getName s xml = 
     case xml of
       Element(_, elems) => 
	let fun helper elems =
		case elems of
		  nil => (print (toString "" xml); raise InvalidXML)
		| (Element (s, [(Element ("name", [PCData name]))])) :: _ => name
		| _ => helper (tl elems)
	in helper elems
	end
     | _ => (print (toString "" xml); raise InvalidXML)
********************)
   (* function to return an element pointed to by a path, or NONE if not any *) 
   fun search xmls path =
	case path of
	  nil => raise InvalidPath
	| [s] => 
	  (
	   case xmls of
	     nil => NONE
	   | (x as Element (name, _ ))::xmls => 
		if (name = s orelse s = "_") then SOME x
		else search xmls path
	   | _::xmls => search xmls path
	  )
	| s::path => 
	  (
	   case xmls of
	     nil => NONE
	   | (x as Element (name, xmls))::tail => 
		if (name = s orelse s = "_") then search xmls path
		else search tail (s::path)
	   | _::xmls => search xmls (s::path)
	  )

   fun selectPCData xml path =
	case search [xml] path of
	  SOME (Element (_, [PCData s])) => SOME s
	| SOME (Element (_, [Comment s])) => SOME s
	| _ => NONE
 
   fun loadToMap xmls map = 
     case xmls of
	  nil => map
	| xml::tail => 
	    let val name = valOf (selectPCData xml ["_", "decl", "name"]) 
	        val map' = StringMap.insert (map, name, xml)
	    in  	
		loadToMap tail map'
	    end

   fun getNumFromCons s =
	let val subs = Substring.full s
	    val (_, suff) = Substring.position "==" subs
	    val num = Substring.string (Substring.dropl (fn ch => ch = #" " orelse ch = #"=") suff)
	in num
	end

   fun ptypeToIR id ptype constraint = 
	let val tyName = valOf (selectPCData ptype ["ptype", "name"])
	    (* val _ = print ("ptype name = " ^ tyName ^ "\n") *)
	    val aux = mkTyAux1 (1, id)
	    val loc = mkLoc 0 0 0 0
	in
  	    if tyName = "PPtime" then
		Base (aux, [(Ptime (""), loc)])
	    else if tyName = "PPdate" then
		Base (aux, [(Pdate (""), loc)])
	    else if tyName = "PPpath" then
		Base (aux, [(Ppath (""), loc)])
	    else if tyName = "Purl" then
		Base (aux, [(Purl (""), loc)])
	    else if tyName = "PPhostname" then
		Base (aux, [(Phostname (""), loc)])
	    else if tyName = "PPip" then
		Base (aux, [(Pip(""), loc)])
	    else if tyName = "PPemail" then
		Base (aux, [(Pemail(""), loc)])
	    else if tyName = "PPmac" then
		Base (aux, [(Pmac(""), loc)])
	    else if tyName = "Pfloat64" then
		  Base (aux, [(Pfloat("", ""), loc)])
	    else if tyName = "Pint64" orelse tyName = "Pint8" orelse tyName = "Pint16"
		 orelse tyName = "Pint32" orelse tyName = "Puint8" 
		 orelse tyName = "Puint16" orelse tyName = "Puint32"  then
		(
		case constraint of
		SOME cons_str =>
		  let 
			val num = valOf (LargeInt.fromString (getNumFromCons cons_str))
		  in
		    RefinedBase (aux, IntConst num, [(Pint(num, LargeInt.toString num), loc)])
		  end
		| _ => Base (aux, [(Pint(0, ""), loc)])
		)
	    else if tyName = "PPstring" then
		Base (aux, [(Pstring(""), loc)])
	    else if tyName = "Pstring" then
		(
		 (* this is a blob *)
		  case selectPCData ptype ["ptype", "argument"] of
		    SOME arg =>
			RefinedBase (aux, Blob (SOME (unescape (strip arg)), 
					NONE), [(Pstring(""), loc)])
		  | _ =>
			RefinedBase (aux, Blob (NONE, NONE), [(Pstring(""), loc)])
		)
	    else if tyName = "PPwhite" then
		Base (aux, [(Pwhite(""), loc)])
	    else if tyName = "PPchar" then
		Base (aux, [(Other(#" "), loc)])
	    else if tyName = "PPtext" then
		Base (aux, [(Ptext(""), loc)])
	    else if tyName = "PPempty" then
		Base (aux, [(Pempty, loc)])
	    else if tyName = "Pstring_ME" then (* this is const string *)
		let 
		    val arg = valOf(selectPCData ptype ["ptype", "argument"])
		    val str = unescape (String.substring (arg, 2, (size arg - 4)))
		in RefinedBase (aux, StringConst str, [(Pstring str, loc)])
		end
	    else if tyName = "Pstring_SE" then (* this is a blob *)
		let val arg = valOf(selectPCData ptype ["ptype", "argument"])
		    val patt = strip arg
		in
		    if patt = "/$/" then
			RefinedBase (aux, Blob (NONE, NONE), [])
		    else 
			RefinedBase (aux, Blob (NONE, SOME patt), [])
		end 
	    else raise InvalidXML
	end

   and mkArray xml label map = 
	let
	    val bodyname = valOf(selectPCData xml ["_", "ptype", "name"])
	    val sep = 
		case selectPCData xml ["_", "delimiterConstraints", "sep", "_"] of
	        NONE => NONE
	      | SOME x => SOME (StringConst (strip x))
	    val term = 
		case selectPCData xml ["_", "delimiterConstraints", "term", "_"] of
	        NONE => NONE
	      | SOME x => SOME (StringConst (strip x))
	    val len = 
		case search [xml] ["_", "sizeConstraints", "max"] of
	        SOME (Element ("max", [PCData x])) => SOME(IntConst (valOf(LargeInt.fromString x)))
	      | SOME (Element ("max", [expr])) => (* TODO: hack here for dependent length *)
		 SOME(LabelRef (Atom.atom 
			(valOf(selectPCData expr ["expr", "native"]))))
	      | _ => NONE
	    val cons = selectPCData xml ["_", "postConstraints", "expr", "native"]
	    val aux = mkTyAux1 (1, Atom.atom label)
	in
	  if (String.substring(bodyname, 0, 2) = "PP") then
	    (* this is a base type *)
	    let val bodyTy = ptypeToIR (getNextLabel ()) 
		(valOf(search [xml] ["_", "ptype"])) cons
	    in
	      RArray (aux,sep, term, bodyTy, len, nil)
	    end
	  else 
	  case StringMap.find (map, bodyname) of
	    NONE => 
		let val bodyTy = ptypeToIR (Atom.atom bodyname) 
		                 (valOf (search [xml] ["_", "ptype"])) cons
		in
	           RArray (aux,sep, term, bodyTy, len, nil)
		end
	  | SOME xml => 
		let val bodyTy = xmlToIR map xml
		in
		  RArray(aux, sep, term, bodyTy, len, nil)
		end
	end
   
   and mkOption xml label map =	
	let 
	    val bodyName = valOf(selectPCData xml ["_", "ptype", "name"])
	    val aux = mkTyAux1 (1, Atom.atom label)
  	    (* val _ = print ("bodyName = " ^ bodyName ^ "\n") *)
	    val cons = selectPCData xml ["_", "postConstraints", "expr", "native"]
	in
	  if (String.substring(bodyName, 0, 2) = "PP") then
	    (* this is a base type *)
	    let val bodyTy = ptypeToIR (getNextLabel ()) 
			(valOf(search [xml] ["_", "ptype"])) cons
	    in
	      Poption (aux, bodyTy)
	    end
	  else 
	    case StringMap.find (map, bodyName) of 
	    SOME xml => 
		let val bodyTy = xmlToIR map xml
		in
		  Poption (aux, bodyTy)
		end
	  | NONE => Poption (aux, ptypeToIR (getNextLabel ()) 
			(valOf (search [xml] ["_", "ptype"])) cons)
	end


   and fieldToIRs smap xml = 
     case xml of
       Element ("field", xmls) =>
	let 
	    (* val _ = print "In field \n"  *)
	    val varName = valOf (selectPCData xml ["field", "name"])
	in
	  case search xmls ["ptype"] of
	    NONE => raise InvalidXML
	  | SOME (Element("ptype", [e as (Element("nestedArray", _))])) =>
		(* nested array *)
		[mkArray e varName smap]
	  | SOME (Element("ptype", [e as (Element("nestedOption", _))])) =>
		(* nested option *)
		[mkOption e varName smap]
	  | SOME _ =>
	    let
	      val tyName = valOf (selectPCData xml ["field", "ptype", "name"])
	      val cons = selectPCData xml ["field", "postConstraints", "expr", "native"]
	    in
	      if (String.substring(tyName, 0, 2) = "PP") then
	      (* this is a base type *)
	        [ptypeToIR (Atom.atom varName) (valOf(search [xml] ["field", "ptype"])) cons]
	      else
	      (* this is a compound type *)
	        case StringMap.find (smap, tyName) of
        	    NONE => [ptypeToIR (Atom.atom varName) 
			(valOf(search [xml] ["field", "ptype"])) cons]
        	  | SOME x => 
        		(* we first need to check if this type has any argument *)
        		let val _ = 
        		  (case (search [xml] ["field", "ptype", "argument", "expr", "native"]) of
        		    SOME (Element ("native", [Comment param])) => 
        			(
        		 	(* print ("inserting " ^ param ^ "\n"); *)
        		       (env:= StringMap.insert (!env, tyName, param)))
        		   | _ => ()
        		  )
        		in
        		  [xmlToIR smap x]
        		end
	    end
	end
     | Element ("literal", _) =>
	let 
	    (* val _ = print "In literal\n"  *)
	    val str = unescape (strip(valOf 
			(selectPCData xml ["literal", "_"])))
	    (*val _ = print ("the string = " ^ str ^ "\n") *)
	    val loc = mkLoc 0 0 0 0
	in [RefinedBase (mkTyAux (1), StringConst str, [(Pstring str, loc)])]
	end
     | Element ("computed", _) =>
       (* this is for the case of Pempty in union,
          kenny: i believe the way Pempty is encoded in xml is not right now,
          so this is just a hack to handle the xml *)
       	(
       	case selectPCData xml  ["computed", "comment"] of
         SOME "EmptyField" => [Base(mkTyAux(1), [(Pempty, mkLoc 0 0 0 0)])]
       	| _ => nil
       	)
     | _ => (nil)

   and branchToIR map switchty xml =
	let val cs = (search [xml] ["branch", "case"])
	    val re = 
		case cs of
		  SOME (Element ("case", [Element ("default", _)])) => StringConst ("*")
		| SOME (Element ("case", [PCData csval])) => (* integer *)
		  if Substring.isSubstring "int" (Substring.full switchty) then
		     IntConst (valOf (LargeInt.fromString csval))
		  else (print ("Bad xml:\n" ^ (toString "" xml)); raise InvalidXML)
		| SOME (Element ("case", [expr_xml])) => 
			let val label = valOf(selectPCData expr_xml ["expr", "native"])
			in
			(
			  case StringMap.find(!env, label) of
			    SOME s => StringConst(strip s)
			  | _ => raise InvalidXML
			)
			end
		| _ => (print ("Bad xml:\n" ^ (toString "" xml)); raise InvalidXML)
	    val field = valOf(search [xml] ["branch", "body", "_"])
	    val branchTy = hd (fieldToIRs map field)
	in
	   (re, branchTy)
	end

   and xmlToIR map xml =
    let 
	val _ = ()
	(* val _ = print ("Processing XML element:\n" ^ toString "" xml)  *)
    in
    case xml of
      Element ("typedef", xmls) =>
	let val tyName = valOf(selectPCData xml ["typedef", "decl", "name"])
	    val cons = selectPCData xml ["typedef", "predicate", "constraints", "expr", "native"]
	in 
	    ptypeToIR (Atom.atom tyName) (valOf(search xmls ["ptype"])) cons
	end
	(* base types *)
    | Element ("PadsC", ty_xmls) =>
	let val rev_xmls = List.rev ty_xmls
	    val map = loadToMap rev_xmls map
	    (*val _ = print ("Load to Map complete. Elements in Map = " ^ 
			Int.toString (StringMap.numItems map) ^ "\n") *)
	in  if length rev_xmls <1 then raise InvalidXML
	    else xmlToIR map (hd rev_xmls)
	end
    | Element ("enum", decl::fields) =>
	let 
	  val label = valOf (selectPCData decl ["decl", "name"])
	  val aux = mkTyAux1(1, Atom.atom label)
	  val res = List.map (fn field => 
			 let val label = valOf(selectPCData field ["enumField", "label"])
			     val str =  valOf(selectPCData field ["enumField", "physicalName"])
			     val _ = env:=StringMap.insert(!env, label, str)
			 in
			    StringConst (strip str) (* at the moment we only have const strings *)
			 end) fields
	  val loc = mkLoc 0 0 0 0
	  val firsttok = case res of
			  (StringConst s::_) => Pstring s
			 | _ => Pstring ""
	in
	  RefinedBase (aux, Enum res, [(firsttok, loc)])
	end
    | Element ("struct", xmls) =>
	let 
	    (* val _ = print "struct\n" *)
	    val label = valOf (selectPCData xml ["struct", "decl", "name"]) 
	    (* val _ = print ("label = " ^ label ^ "\n") *)
	    val tys = List.concat (List.map (fieldToIRs map) xmls)
	    val aux = mkTyAux1 (1, Atom.atom label)
	    (* val _ = print ("done with struct " ^ label ^ "\n") *)
	in
	    Pstruct(aux, tys)
	end 
    | Element ("union", xmls) =>
	let val label = valOf (selectPCData xml ["union", "decl", "name"]) 
	    val aux = mkTyAux1 (1, Atom.atom label)
	in
	  case search xmls ["inplace"] of
	    SOME (Element("inplace", xmls)) =>
		let val tys = List.concat (List.map (fieldToIRs map) xmls)
		    (* val _ = print ("done with union " ^ label ^ "\n") *)
		in Punion(aux, tys)
		end
	  | NONE =>  (* it's a switch, not a normal union *)
	    (
	     case search xmls ["switched", "branches"] of
		SOME (Element("branches", xmls)) =>
		  let val switched_id = 
			case StringMap.find (!env, label) of
			  SOME id => Atom.atom(id)
			| _ => raise InvalidXML
		      val switch_type = valOf(selectPCData xml ["union", "decl", "params", "param", "type"])
		      val retys = List.map (branchToIR map switch_type) xmls
		  in
		    Switch (aux, switched_id, retys)
		  end
	     | _ => raise InvalidXML
	    )
	end
    | Element ("array", xmls) =>
	let val label = valOf(selectPCData xml ["array", "decl", "name"])
	in mkArray xml label map
	end
    | Element ("opt", xmls) => 
	let val label = valOf(selectPCData xml ["opt", "decl", "name"])
        in mkOption xml label map
	end
    | _ => raise InvalidXML
    end 
end 
