structure Padsc_printer =
struct
open Ast
  fun getRefStr refined = case refined of
    StringME s => "\"" ^ s ^"\""
  | StringConst s => if (size s) = 1 then ("'" ^ s ^ "'")
			else ("\"" ^ s ^ "\"")
  | _ => ""

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
     | IRempty => "PPempty"

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
    let val (isRecord, tyVar, tyDef) = irTy
	val tyVarStr = tyNameToPADSCString tyVar
  	val precord = if isRecord then "Precord " else ""
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
		in "Parray " ^ tyVarStr ^ " {\n\t" ^ tyNameStr ^
		         ("[" ^ 
			    ( case len of 
				SOME (IntConst x) => largeIntToStr x
				| _ => ""
			    ) ^ "]" ^
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
			    ) ^ ";\n"
			  ) ^ "};\n"
		end
	| TyOption tyName => 
		let val tyNameStr = tyNameToPADSCString tyName
		in "Popt " ^ tyNameStr ^ " " ^ tyVarStr ^ ";\n"
		end
      )
    end

     fun tyToPADSC ty numHeaders numFooters includeFile =
	(* assume that if a ty has header and footer, the body is just one single Ty*)
	let
	  val bodyLabel = 
	    if numHeaders>0 orelse numFooters>0 then
		case ty of
		  Punion (_, tys) => 
		    let
		      val body = List.nth (tys, numHeaders)
		    in (tyNameToPADSCString (getTypeName body)) 
		    end
		  | _ => raise TyMismatch
	    else
		tyNameToPADSCString (
		case ty of
		  Base _ => getBaseTyName ty
		| RefinedBase _ => getBaseTyName ty
		| _ => getTypeName ty)
	  val headerLabel = 
	    if numHeaders = 0 then ""
	    else if numHeaders = 1 then 
		case ty of
		  Punion (_, tys) => (tyNameToPADSCString (getTypeName (List.nth (tys, 0)))) 
		  | _ => raise TyMismatch
	    else "Header"
	  val footerLabel = 
	    if numFooters = 0 then ""
	    else if numHeaders = 1 then 
		case ty of
		  Punion (_, tys) => (tyNameToPADSCString (getTypeName (List.nth (tys, (numHeaders+1))))) 
		  | _ => raise TyMismatch
	    else "Footer"

	  val incString = "#include \""^ includeFile ^"\"\n" 
	  val (pads, topLabel) =
		(if numHeaders=0 andalso numFooters=0 then
		    let val irTys = tyToIR true nil ty
			val body = (lconcat (map irToPADSC irTys))
		    in
		       (incString ^
			body ^
			 "Psource Parray entries_t {\n" ^
		    	 "\t" ^ bodyLabel ^ "[];\n" ^
			 "};\n",
			 "entries_t")
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
				val headerIRs = List.concat (map (tyToIR true nil) headers)
				val bodyIRs = tyToIR true nil body
				val footerIRs = List.concat (map (tyToIR true nil) footers) 
				val l = getLabelString (getAuxInfo ty)
	    			val topLabel = "Struct_" ^ (String.extract (l, 4, NONE))
			      in
			        (incString ^
				 (case headerIRs of
					nil => ""
					| _ => (lconcat (map irToPADSC headerIRs)) 
				 ) ^
				(case headerIRs of
					nil => ""
					| [_] => ""
					| _ => "Pstruct Header {\n" ^
					  (String.concat (map 
					  (fn t => ("\t" ^ tyNameToPADSCString (getTypeName t) ^ " " ^
						getVar t ^ ";\n")) 
					  headers)) ^ "};\n"
				) ^
				(lconcat (map irToPADSC bodyIRs)) ^
				(case footerIRs of
					nil => ""
					| _ => (lconcat (map irToPADSC footerIRs))
				) ^
				(case footerIRs of
					nil => ""
					| [_] => ""
					| _ => "Pstruct Footer {\n" ^
					  (String.concat (map 
					  (fn t => ("\t" ^ tyNameToPADSCString (getTypeName t) ^ " " ^
						getVar t ^ ";\n")) 
					  footers)) ^ "};\n"
				) ^
				"Psource Pstruct " ^ topLabel ^ " {\n" ^
				(case headerIRs of
					nil => ""
					| [_] => "\t" ^ headerLabel ^ " " ^ getVar (hd headers) ^ ";\n"
					| _ => "\tHeader v_header;\n"
				) ^
				("\t" ^ (tyNameToPADSCString (getTypeName body)) ^ 
					"[] " ^ (getVar body) ^" : Plongest;\n") ^
				(case footerIRs of 
					nil => ""
					| [_] => "\t" ^ footerLabel ^ " " ^ getVar (hd footers) ^ ";\n"
					| _ => "\tFooter v_footer;\n"
				) ^
				"};\n",
				topLabel)
			      end
		  | _ => raise TyMismatch
		)
	in
	  (topLabel, headerLabel, bodyLabel, footerLabel, pads)
	end 


end
