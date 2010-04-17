structure Padsml_printer =
struct
  open Ast

  fun getRefStr sep_term refined = case refined of
    StringME s => "Regexp_" ^ sep_term ^ " \"" ^ escapeRegex s ^"\""
  | StringConst s => if (size s) = 1 then ("Char_" ^ sep_term ^ " '" ^ s ^ "'")
			else ("String_" ^ sep_term ^ " \"" ^ s ^ "\"")
  | IntConst i => "Regexp_" ^ sep_term ^ " \"/0*" ^ LargeInt.toString i ^ "/\""
  | _ => raise TyMismatch

  fun tyNameToPML tyName =
     case tyName of 
       IRref s => s
     | IRbXML => "ppbXML"  
     | IReXML => "ppeXML"   
     | IRtime => "pptime" 
     | IRdate => "ppdate" 
     | IRpath => "pppath"
     | IRurl => "ppurl"
     | IRip => "ppip"
     | IRhostname => "pphostname"
     | IRemail => "ppemail"
     | IRmac => "ppmac"  
     | IRint => "pint64"
     | IRintrange (min, max) =>
        let val minLen = int2Bits min
            val maxLen = int2Bits max
            val maxBits = Real.max(minLen, maxLen)
            val (typeName, maxBits)  = if (min>=0) then ("puint", maxBits) 
				       else ("pint", maxBits+1.0)
        in 
            if (maxBits<= 8.0) then typeName ^ "8"
            else if maxBits <=16.0 then typeName ^ "16"
                 else if maxBits <=32.0 then typeName ^ "32"
                      else typeName ^ "64" 
        end
     | IRfloat => "pfloat64"
     | IRstring => "ppstring"
     | IRstringME s => "pstring_ME(\"" ^ escapeRegex s ^ "\")"
     | IRwhite => "ppwhite"
     | IRchar => "ppchar"
     | IRtext => "pptext"
     | IRblob (s, p) => 
	(
	case (s, p) of
	  (SOME s, NONE) => 
        	if size s = 0 then raise TyMismatch
		else if size s = 1 then
          	  "pstring('" ^ s ^ "')"
		else 
          	  "pstring_SE(\"/" ^ escapeRegex (escape s) ^ "/\")"
	| (NONE, SOME s) =>
          	  "pstring_SE(\"" ^ escapeRegex s ^ "\")"
        | _ => "pstring_SE(Peor)"
	)
     | IRempty => "ppempty"

  fun fieldToPML isStruct f =
    case f of
      StringField (SOME v, s) => (upFirstChar v) ^ " of \"" ^ (String.toCString s) ^ "\""
    | StringField (NONE, s) => "\"" ^ (String.toCString s) ^ "\""
    | CharField (SOME v, s) => (upFirstChar v) ^ " of '" ^ (String.toCString s) ^ "'"
    | CharField (NONE, s) => "'" ^ (String.toCString s) ^ "'" 
    | CompField (t, (v, NONE, NONE, SOME (IntConst x))) => 
                "with pdefault " ^ (upFirstChar v) ^ " of " ^ (tyNameToPML t) ^ " = " ^ 
		(LargeInt.toString x)
    | ArrayField (v, tyName, sep, term, len) =>
		let val tyNameStr = tyNameToPML tyName
		val sep_str = 
		  case sep of
		    SOME refsep => getRefStr "sep" refsep
		  | NONE => "No_sep"
		val term_str = 
		  case term of
		    SOME refterm => getRefStr "term" refterm
		  | NONE => "No_term"
		in tyNameStr ^ " plist (" ^ sep_str ^ ", " ^ term_str ^ ")"
		end
    | OptionField (v, tyName) => v ^ " : " ^ (tyNameToPML tyName) ^ " popt"
		
    | FullField (v, t, sw, c) => 
        let val tyname = tyNameToPML t in
	(if isStruct then
          v ^ " : " ^ tyname ^ 
          (case sw of 
           SOME swv => "(" ^ swv ^ ")"
           | NONE => ""
          ) 
	else upFirstChar v ^ " of " ^ tyname)
	^ 
        (case c of 
           SOME (consv, min, max, SOME eq) => 
                let val consVal = 
                (case eq of
                   IntConst i => LargeInt.toString i
                 | FloatConst (i, f) => i ^ f
                 | _ => raise TyMismatch
                )
                in (" = " ^ consVal) 
                end
         | NONE => ""
         | _ => raise TyMismatch
        ) 
        end
     | _ => raise TyMismatch

  fun irToPML ir = 
    let val (levels2Rec, tyVar, tyDef) = ir
	val tyVarStr = tyNameToPML tyVar
  	val precord = if levels2Rec = 0 then "precord " else ""
    in
      case tyDef of
        TyBase (tyName, cons_op) =>
	  let val tyNameStr = tyNameToPML tyName
	  in
	   case cons_op of
	     NONE => "ptype " ^ tyVarStr ^ " = " ^ tyNameStr ^  " " ^ precord ^ "\n\n"
	   | SOME (var, NONE, NONE, SOME (IntConst i)) =>
		     "ptypedef " ^ tyVarStr ^ " = [" ^ var ^ ": " ^  tyNameStr ^ 
			" | " ^ var ^ " = " ^ (LargeInt.toString i) ^ "]\n\n"
	   | SOME (var, NONE, NONE, SOME (FloatConst (i, f))) =>
		     "ptypedef " ^ tyVarStr ^ " = [" ^ var ^ ": " ^  tyNameStr ^ 
			" | " ^ var ^ " = " ^ i ^ "." ^ f ^ "]\n\n"
	   | SOME (var, NONE, NONE, SOME (StringConst s)) =>
		     "ptype " ^ tyVarStr ^ " = pstring_ME(\"/" ^ escapeRegex s ^ "/\")\n\n" 
	   | _ => raise TyMismatch
	  end
    	| TyStruct fields => "ptype " ^ tyVarStr ^ " = {\n\t" ^ 
				(String.concatWith ";\n\t" (map (fieldToPML true) fields)) ^ "\n} " ^ 
				"\n\n"
	| TyUnion fields => "ptype " ^ tyVarStr ^ " = \n\t" ^ 
				(String.concatWith "\n\t| " (map (fieldToPML false) fields)) ^ "\n\n"
	| TyEnum fields => "ptype " ^ tyVarStr ^ " = \n\t" ^ 
				(String.concatWith "\n\t| " (map (fieldToPML false) fields)) ^ "\n\n"
	| TySwitch (swVar, swTyName, branches) => 
		let
		  val swTyNameStr = tyNameToPML swTyName 
		  fun branchtoStr (e, f) = 
		  case e of
		    EnumInt i => (LargeInt.toString i) ^ " -> \t" ^ (fieldToPML false f) ^ "\n"
		  | EnumVar v => (upFirstChar v) ^ " _ -> \t" ^ (fieldToPML false f) ^ "\n"
		  | EnumDefault => "_ -> \t" ^ (fieldToPML false f) ^ "\n"
		in
		  "ptype " ^ tyVarStr ^ " (" ^ swVar ^ " : " ^ swTyNameStr ^ ") =\n" ^
		  "  pmatch " ^ swVar ^ " with\n\t" ^ 
		  (String.concatWith "\t| " (map branchtoStr branches)) ^
		  "\n" ^
		  "\n"
		end
		
	| TyArray (tyName, sep, term, len) =>
		let val tyNameStr = tyNameToPML tyName
		val sep_str = 
		  case sep of
		    SOME refsep => getRefStr "sep" refsep
		  | NONE => "No_sep"
		val term_str = 
		  case term of
		    SOME refterm => getRefStr "term" refterm
		  | NONE => "No_term"
		in "ptype " ^ tyVarStr ^ " = " ^ tyNameStr ^ 
		   " plist (" ^ sep_str ^ ", " ^ term_str ^ ") " ^ precord ^"\n\n"
		end
	| TyOption tyName => 
		let val tyNameStr = tyNameToPML tyName
		in ("ptype " ^ tyVarStr ^ " = " ^ tyNameStr ^ " popt " ^ precord ^"\n\n")
		end
    end

     fun tyToPADSML ty numHeaders numFooters includeFile =
        (* assume that if a ty has header and footer, the body is just one single Ty*)
        let
	  val _ = tyMapRef := TyMap.empty
          val recordLabel = 
	    if numHeaders>0 orelse numFooters>0 then
		let val l = getLabelString (getAuxInfo ty)
		in
	    	  if String.substring(l, 0, 3) = "BTy" then
			   "struct_" ^ (String.extract (l, 4, NONE))
		  else l
		end
	    else
		tyNameToPML (
                case ty of
                  Base _ => getBaseTyName ty
                | RefinedBase (_, Enum _, _) => getTypeName ty
                | RefinedBase _ => getBaseTyName ty
                | _ => getTypeName ty)
          val pads = "open "^ includeFile ^"\n" ^
                (if numHeaders=0 andalso numFooters=0 then
                    let val irTys = tyToIR 0 nil ty
                        val body = (lconcat (map irToPML irTys))
                    in
                        body ^
                        "ptype entries_t = " ^ recordLabel ^ " precord plist (No_sep, No_term)\n"
                    end
                else 
                  case ty of 
                    Punion (_, tys) =>
                      if (numHeaders + numFooters + 1) <> length tys then
                        raise Fail "Header Footer incorrect!"
                      else 
                              let
                                val headers = List.take (tys, numHeaders)
                                val body = List.nth (tys, numHeaders)
                                val footers = List.drop (tys, (numHeaders+1))
                                val (headerVars, headerIRss) = ListPair.unzip (map 
					(fn t => 
					  let val irs = tyToIR 0 nil t
					      val varTy = case TyMap.find (!tyMapRef, t) of
							    NONE => raise TyMismatch
							    | SOME n => tyNameToPML n
					  in (varTy, irs) end)  headers)
				val headerIRs = List.concat headerIRss
                                val bodyIRs = tyToIR 0 nil body
				val bodyVar = case TyMap.find (!tyMapRef, body) of
						NONE => (print "no body\n"; raise TyMismatch)
						| SOME n => tyNameToPML n
                                val (footerVars, footerIRss) = ListPair.unzip (map 
					(fn t => 
					  let val irs = tyToIR 0 nil t
					      val varTy = case TyMap.find (!tyMapRef, t) of
							    NONE => (print "no footer\n"; raise TyMismatch)
							    | SOME n => tyNameToPML n
					  in (varTy, irs) end)  footers)
				val footerIRs = List.concat footerIRss
                              in
                                (lconcat (map irToPML (headerIRs @ bodyIRs @ footerIRs))) ^
                                "ptype " ^ recordLabel ^ " = " ^ 
				(lconcat (map (fn v => "(" ^ v ^ " precord) * ") headerVars)) ^ 
				"(" ^ bodyVar ^ " precord plist (No_sep, No_term))" ^
				(lconcat (map (fn v => " * (" ^ v ^ " precord)") footerVars)) ^ 
                                "\n" 
                              end
                  | _ => tyToPADSML ty 0 0 includeFile 
                )
        in pads
        end 

end
