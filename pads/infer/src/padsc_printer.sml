structure Padsc_printer =
struct
open Ast
  fun getRefStr refined = case refined of
    StringME s => "Pre \"" ^ escapeRegex s ^"\""
  | StringConst s => if (size s) = 1 then ("'" ^ s ^ "'")
			else ("\"" ^ s ^ "\"")
  | _ => (print "Refined type not supported!\n"; raise TyMismatch) 

  fun largeIntToStr i =
      if i>=0 then LargeInt.toString i
	  else "-"^(LargeInt.toString (~i))

  fun tyNameToPADSCString tyName =
     case tyName of 
       IRref s => upFirstChar s
     | IRbXML => "PPbXML"  
     | IReXML => "PPeXML"   
     | IRtime => "PPtime" 
     | IRdate => "PPdate" 
     | IRpath => "PPpath"
     | IRurl => "PPurl"
     | IRip => "PPip"
     | IRhostname => "PPhostname"
     | IRemail => "PPemail"
     | IRmac => "PPmac"  
     | IRint => "Pint64"
     | IRintrange (min, max) =>
    	let val minLen = int2Bits min
    	    val maxLen = int2Bits max
    	    val maxBits = Real.max(minLen, maxLen)
	    val typeName = if (min>=0) then "Puint" else "Pint"
    	in 
    	    if (maxBits<= 8.0) then typeName ^ "8"
    	    else if maxBits <=16.0 then typeName ^ "16"
    	         else if maxBits <=32.0 then typeName ^ "32"
    	              else typeName ^ "64" 
    	end
     | IRfloat => "Pfloat64"
     | IRstring => "PPstring"
     | IRstringME s => "Pstring_ME(:\"" ^ s ^ "\":)"
     | IRwhite => "PPwhite"
     | IRchar => "PPchar"
     | IRtext => "PPtext"
     | IRblob s => 
	if size s = 0 then
	  "Pstring_SE(:Peor:)"
	else if size s = 1 then
	  "Pstring(:'" ^ s ^ "':)"
	else
	  "Pstring_SE(:\"/" ^ escape s ^ "/\":)"
     | IRempty => "PPempty"

  fun arrayFieldToPADSC (var_opt, tyName, sep, term, len) =
	let val tyNameStr = tyNameToPADSCString tyName
	in "\t" ^ tyNameStr ^ 
	         "[" ^ 
		    ( case len of 
			SOME (IntConst x) => largeIntToStr x
			| SOME (LabelRef id) =>
			   (
			   case LabelMap.find (!varMapRef, id) of
			     SOME v => v
			   | _ => ""
			   )
			| _ => ""
		    ) ^ "]" ^
		    (case var_opt of
		      SOME v => " " ^ v 
		     | _ => "") ^
		    ( case sep of 
		     SOME refsep => 
			let val sepstr = getRefStr refsep
			in
			  (case term of
			  SOME refterm => 
			    let val termstr = getRefStr refterm
			    in " : Psep(" ^ sepstr ^ ") && Pterm(" ^ termstr ^ ")"
			    end
			  | NONE => " : Psep(" ^ sepstr ^ ") && Plongest"
			  )
			 end
		     | NONE => 
			(case term of
			  SOME refterm => 
			    (
			    let 
				val termstr = getRefStr refterm
			    in 
				" : Pterm(" ^ termstr ^ ")"
			    end
			    )
			  | NONE => " : Plongest"
			)
		    ) 
	end


  fun fieldToPADSC isEnum f =
    case f of
      StringField (SOME v, s) => 
		if not isEnum andalso isCIdentifier s then ("\t\"" ^ s ^ "\"")
		else "\t" ^ v ^ " Pfrom(\"" ^ (String.toCString s) ^ "\")"
    | StringField (NONE, s) => "\t\"" ^ (String.toCString s) ^ "\""
    | CharField (SOME v, s) => "\t" ^ v ^ " Pfrom(\"" ^ (String.toCString s) ^ "\")"
    | CharField (NONE, s) => "\t'" ^ (String.toCString s) ^ "'" 
    (*We currently piggieback Pempty in CompField because there is no
      Pempty constructor in padsml. When we do have it, we will add an
      EmptyField in the AST for Pempty*)
    | CompField (t, (v, NONE, NONE, SOME (IntConst x))) => 
		if x = 0 then "\tPempty" else
		"\tPcompute " ^ tyNameToPADSCString t ^ " " ^ v ^ " = " ^ (largeIntToStr x)
    | ArrayField (v, tyName, sep, term, len) => arrayFieldToPADSC (SOME v, tyName, sep, term, len)
    | FullField (v, t, sw, c) => 
	let val tyname = tyNameToPADSCString t in
	"\t" ^ tyname ^ " " ^  
	(case sw of 
	   SOME swv => "(:" ^ swv ^ ":)"
	 | NONE => ""
	) ^ " " ^ v ^
	(case c of 
	   SOME (consv, min, max, SOME eq) => 
		let val consVal = 
		(case eq of
		   IntConst i => largeIntToStr i
		 | FloatConst (i, f) => i ^ f
		 | _ => raise TyMismatch
		)
		in (" : " ^ consv ^ " == " ^ consVal) 
		end
	 | NONE => ""
	 | _ => raise TyMismatch
	) 
	end
     | _ => raise TyMismatch

  fun irToPADSC irTy = 
    let val (levels2Rec, tyVar, tyDef) = irTy
	val tyVarStr = tyNameToPADSCString tyVar
  	val precord = if levels2Rec = 0 then "Precord " else ""
    in
      precord ^
      (
      case tyDef of
        TyBase (tyName, cons_op) =>
	  let val tyNameStr = tyNameToPADSCString tyName
	  in
	   case cons_op of
	     NONE => "Ptypedef " ^ tyNameStr ^ " " ^ tyVarStr ^";\n"
	   | SOME (var, NONE, NONE, SOME (IntConst i)) =>
		     "Ptypedef " ^ tyNameStr ^ " " ^ tyVarStr ^" : " ^ tyVarStr ^ " " ^ var ^ 
		     " => {" ^ var ^ " == " ^ (largeIntToStr i) ^ "};\n"
	   | SOME (var, NONE, NONE, SOME (FloatConst (i, f))) =>
		     "Ptypedef " ^ tyNameStr ^ " " ^ tyVarStr ^" : " ^ tyVarStr ^ " " ^ var ^ 
		     " => {" ^ var ^ " == " ^ i ^"." ^ f ^ "};\n"
	   | _ => raise TyMismatch
	  end
    	| TyStruct fields => "Pstruct " ^ tyVarStr ^ " {\n" ^ 
				(String.concatWith ";\n" (map (fieldToPADSC false) fields)) ^ ";\n};\n"
	| TyUnion fields => "Punion " ^ tyVarStr ^ " {\n" ^ 
				(String.concatWith ";\n" (map (fieldToPADSC false) fields)) ^ ";\n};\n"
	| TyEnum fields => "Penum " ^ tyVarStr ^ " {\n" ^ 
				(String.concatWith ",\n" (map (fieldToPADSC true) fields)) ^ "\n};\n"
	| TySwitch (swVar, swTyName, branches) => 
		let
		  val swTyNameStr = tyNameToPADSCString swTyName 
		  fun branchtoStr (e, f) = 
		  case e of
		    EnumInt i => "\tPcase " ^ (largeIntToStr i) ^ ": " ^ (fieldToPADSC true f) ^ ";\n"
		  | EnumVar v => "\tPcase " ^ v ^ ": " ^ (fieldToPADSC true f) ^ ";\n"
		  | EnumDefault => "\tPdefault: " ^(fieldToPADSC true f) ^ ";\n"
		in
		  "Punion " ^ tyVarStr ^ " (:" ^ swTyNameStr ^ " " ^ swVar ^ ":) {\n" ^
		  "  Pswitch (" ^ swVar ^ ") {\n" ^
		  (lconcat (map branchtoStr branches)) ^
		  "  }\n" ^
		  "};\n"
		end
		
	| TyArray (tyName, sep, term, len) =>
		let val tyNameStr = tyNameToPADSCString tyName
		    val arrayStr = arrayFieldToPADSC (NONE, tyName, sep, term, len)
		in "Parray " ^ tyVarStr ^ " {\n" ^ arrayStr ^ ";\n};\n"
		end
	| TyOption tyName => 
		let val tyNameStr = tyNameToPADSCString tyName
		in "Popt " ^ tyNameStr ^ " " ^ tyVarStr ^ ";\n"
		end
      )
    end

     fun tyToPADSC ty numHeaders numFooters includeFile =
	(* assume that if a ty has header and footer, the body is just one single Ty*)
	(* there can be multiple headers or footers but each header/footer describes just one
	   record, the headers and footers are all different from each other *)
	let
	  val _ = tyMapRef := TyMap.empty
	  val _ = varMapRef := LabelMap.empty
	  val incString = "#include \""^ includeFile ^"\"\n" 
	  val (pads, topLabel, headerLabel, bodyLabel, footerLabel) =
		if numHeaders=0 andalso numFooters=0 then
		    let val irTys = tyToIR 0 nil ty
			val body = (lconcat (map irToPADSC irTys))
			val bodyLabel = tyNameToPADSCString (
				case ty of
		  		Base _ => getBaseTyName ty
				| RefinedBase (_, Enum _, _) => getTypeName ty
				| RefinedBase _ => getBaseTyName ty
				| _ => getTypeName ty)
		    in
		       (incString ^
			body ^
			 "Psource Parray entries_t {\n" ^
		    	 "\t" ^ bodyLabel ^ "[];\n" ^
			 "};\n",
			 "entries_t", "", bodyLabel, "")
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
                        val l = getLabelString (getAuxInfo ty)
                        val topLabel = "Struct_" ^ (String.extract (l, 4, NONE))
			val headerty = case headers of
					nil => NONE
					| [h] => SOME h
					| hdrs => (*create struct with dummy auxinfo*)
					  let val hdraux = mkTyAux 0 in
					  SOME (Pstruct (hdraux, hdrs)) end
			val footerty = case footers of
					nil => NONE
					| [f] => SOME f
					| ftrs => (*create struct with dummy auxinfo*)
					  let val ftraux = mkTyAux 0 in
					  SOME (Pstruct (ftraux, ftrs)) end
			(* header can't be found in the tyMap so use getTypeName for label *)
			val headerLabel = case headerty of
					      SOME h => 
						tyNameToPADSCString (
						case h of
		  				Base _ => getBaseTyName h
						| RefinedBase (_, Enum _, _) => getTypeName h
						| RefinedBase _ => getBaseTyName h
						| _ => getTypeName h)
					      | NONE => ""
			val hdIRs = case length headers of
				0 => nil
				| 1 => tyToIR 0 nil (some headerty) 
				| _ => tyToIR 1 nil (some headerty)
			(* body may be found after tyToIR for the header so need to check *)
			val bodyLabel = tyNameToPADSCString (
					case TyMap.find (!tyMapRef, body) of
					SOME n => n
					| _ => 
					  (case body of
		  			  Base _ => getBaseTyName body
					  | RefinedBase (_, Enum _, _) => getTypeName body
					  | RefinedBase _ => getBaseTyName body
					  | _ => getTypeName body)
					)

			val bodyIRs = tyToIR 0 nil body
			(* footer may be found after tyToIR for the header and body so need to check *)
			val footerLabel = case footerty of
					    SOME f =>
					      tyNameToPADSCString (
					      case TyMap.find (!tyMapRef, f) of
					      SOME n => n
					      | _ => 
					      (
					        case f of
		  				Base _ => getBaseTyName f
						| RefinedBase (_, Enum _, _) => getTypeName f
						| RefinedBase _ => getBaseTyName f
						| _ => getTypeName f)
					      )
					    | NONE => ""
			val ftIRs = case length footers of
				0 => nil
				| 1 => tyToIR 0 nil (some footerty)
				| _ => tyToIR 1 nil (some footerty)
			in
			        (incString ^
				 lconcat (map irToPADSC (hdIRs @ bodyIRs @ ftIRs)) ^
				"Psource Pstruct " ^ topLabel ^ " {\n" ^
                                (case headerty of
                                        NONE => ""
                                        | SOME h => "\t" ^ headerLabel ^ " v_" ^ headerLabel ^ ";\n"
                                ) ^
                                ("\t" ^ bodyLabel ^ "[] v_" ^ bodyLabel ^" : Plongest;\n") ^
                                (case footerty of 
                                        NONE => ""
                                        | SOME f => "\t" ^ footerLabel ^ " v_" ^ footerLabel ^ ";\n"
                                ) ^
                                "};\n",
				topLabel, headerLabel, bodyLabel, footerLabel)
			      end
		  | _ => raise TyMismatch
	in
	  (topLabel, headerLabel, bodyLabel, footerLabel, pads)
	end 


end
