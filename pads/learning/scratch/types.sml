structure Types =

struct
    open Config
    open Utils
    open Complexity
    open Tokens
    open Options
    exception InvalidId
    exception TyMismatch
    exception InvalidTokenTy

    type TokenOrder = Token list
    type Context    = LToken list
    fun myand(a,b) = a andalso b
    fun myor(a,b) = a orelse b
    fun maxContextLength ( cl : Context list ) : int =
        foldl (fn (ltl : LToken list,x : int) => Int.max (maxTokenLength ltl,x)) 0 cl

    type DerivedContexts = Context list
    type Partition = (TokenOrder * (DerivedContexts list)) list * (Context list)

    val Tystamp = ref 0  (* used to give unique ids to nodes in type trees *)
    type Id = Atom.atom			     
    type TyComp = { tc       : Complexity (* Type complexity *)
                  , adc      : Complexity (* Atomic data complexity *)
                  , dc       : Complexity (* Data complexity *)
                  }

    val zeroComps : TyComp = { tc = zeroComp, adc = zeroComp, dc = zeroComp }

    fun showTyComp ( tyc : TyComp ) : string =
        "{" ^ showComp (#tc tyc) ^ ", " ^ showComp (#adc tyc) ^ ", " ^ showComp (#dc tyc) ^ "}"

    fun combTyComp ( t1 : TyComp ) ( t2 : TyComp ) : TyComp =
        { tc  = combine (#tc t1) (#tc t2)
        , adc = combine (#adc t1) (#adc t2)
        , dc  = combine (#dc t1) (#dc t2)
        }

    fun combTyComp2 ( t1 : TyComp, t2 : TyComp ) : TyComp =
        { tc  = combine (#tc t1) (#tc t2)
        , adc = combine (#adc t1) (#adc t2)
        , dc  = combine (#dc t1) (#dc t2)
        }

    type AuxInfo = { coverage : int        (* Coverage of
                                                 -- a struct is minimum coverage
                                                 --      of its constituents;
                                                 -- a union is sum of coverage
                                                         of its consituents; *)
                   , label    : Id option  (* introduced during refinement as a tag *)
                   , tycomp   : TyComp
                   }

    (* Update the type and data complexity of an AuxInfo *)
    fun updateComps ( aux : AuxInfo ) ( comps : TyComp ) : AuxInfo =
        { coverage = #coverage aux
        , label    = #label aux
        , tycomp   = comps
        }

    val numRefined      : LargeInt.int = 7 (* Number of cases in datatype Refined *)
    val numConstRefined : LargeInt.int = 3 (* Number of constant cases in datatype Refined *)

    datatype Refined = StringME of string
	             | Int of LargeInt.int * LargeInt.int  (* min, max *)
	             | IntConst of LargeInt.int    (* value *)
		     | FloatConst of string * string (*value*)
                     | StringConst of string (* string literal *)
                     | Enum of Refined list  
                     | LabelRef of Id     (* for synthetic nodes: lengths, branch tags*)

    val numTy : LargeInt.int = 10 (* Number of constructors in datatype Ty *)
    datatype Ty = Base    of AuxInfo * LToken list (* list will never be empty *)
	        (* TBD is introduced when maxdepth limit kicks in *)
                | TBD     of AuxInfo *
                             int     * (* Sequence number on tbds *) 
                             Context list
                (* Bottom is introduced when structure inference gives up *)
                | Bottom  of AuxInfo *
                             int     * (* Sequence number on bottoms *)
                             Context list 
                | Pstruct of AuxInfo * Ty list 
                | Punion  of AuxInfo * Ty list
                | Parray  of AuxInfo * { tokens:(Token * int) list, (* list of tokens and freq count in 
								       cluster identified as an array cell *)
					 lengths: (int * int) list, (* list of array (lengths,linenumbers) *)
					 first : Ty, (* inferred type for first cell of list of arrays *)
					 body  : Ty, (* inferred type for middle cells of list of arrays *)
					 last  : Ty} (* inferred type for last cell of list of arrays *)

                | RefinedBase of AuxInfo * Refined * LToken list
                | Switch  of AuxInfo * Id * (Refined (* switch value *) * Ty) list
                | RArray  of AuxInfo * Refined option (* separator *)
                                     * Refined option (* terminator *)
	                             * Ty             (* body type *)
                                     * Refined option (* fixed length *) 
                                     * (int*int) list (* (length, linenumber) list*)
                | Poption of AuxInfo * Ty (* a Ty which is optional *)

    (* Number of kinds of tree nodes in Ty *)
    val numConstruct    : LargeInt.int = numTy + numRefined + numToken
    val constructorComp : Complexity   = int2Comp numConstruct
    fun lengthsToString lens =
	case lens of
	(len, recno)::rest => "("^ (Int.toString len)^", "^(Int.toString recno)^")" ^ (lengthsToString  rest)
	| nil => "\n"
    fun getAuxInfo ( ty : Ty ) : AuxInfo = 
	case ty 
        of Base (a,t)                   => a
        |  TBD (a,i,cl)                 => a
        |  Bottom (a,i,cl)              => a
        |  Pstruct (a,tys)              => a
        |  Punion (a,tys)               => a
        |  Parray (a, _)                => a
        |  RefinedBase (a,r,tl)         => a
        |  Switch(a,id,branches)        => a
        |  RArray (a,sep,term,body,len,lengths) => a
        |  Poption (a, _)               => a

    fun setAuxInfo ty aux = 
	case ty 
        of Base (a,t)                   => Base(aux, t)
        |  TBD (a,i,cl)                 => TBD(aux, i, cl)
        |  Bottom (a,i,cl)              => Bottom(aux, i, cl)
        |  Pstruct (a,tys)              => Pstruct(aux, tys)
        |  Punion (a,tys)               => Punion(aux, tys)
        |  Parray (a, x)                => Parray(aux, x)
        |  RefinedBase (a,r,tl)         => RefinedBase(aux, r, tl)
        |  Switch(a,id,branches)        => Switch(aux, id, branches)
        |  RArray (a,sep,term,body,len,lengths) => RArray(aux, sep, term, body, len, lengths)
        |  Poption (a,ty)               => Poption(aux, ty)

    (* Compute the length of an RArray. This function should always be
       passed a Ty value under the RArray constructor, if not, it throws
       an exception
     *)
    exception NotRArray
    exception BadRArrayLength
    fun getLengthRArray   ( ty : Ty ) : int =
        ( case ty of
               RArray ( a, osep, oterm, body, olen, _) =>
                  ( case olen of
                         NONE => 1 (***** TODO: For now !!!!!!*)
                       | SOME r =>
                           ( case r of
                                  Int (min, max) => Int.fromLarge max
                                | IntConst n     => Int.fromLarge n
                                | _              => raise BadRArrayLength
                           )
                  )
             | _ => raise NotRArray
        )

    (* Retrieve computed type complexity from a type *)
    fun getTypeComp ( ty : Ty ) : Complexity = #tc (#tycomp (getAuxInfo ty))
    (* Retrieve computed data complexity from a type *)
    fun getDataComp ( ty : Ty ) : Complexity = #dc (#tycomp (getAuxInfo ty))
    (* Retrieve atomic data complexity from a type *)
    fun getAtomicComp ( ty : Ty ) : Complexity = #adc (#tycomp (getAuxInfo ty))
    (* Retrieve all complexities from a measured type *)
    fun getComps ( ty : Ty ) : TyComp = #tycomp (getAuxInfo ty)

    (* Sum the type complexities of a measured type *)
    fun sumTypeComps ( tys : Ty list ) : Complexity =
        foldl ( fn (t,c) => combine (getTypeComp t) c ) zeroComp tys

    (* Sum the data complexities of a measured type *)
    fun sumDataComps ( tys : Ty list ) : Complexity =
        foldl ( fn (t,c) => combine (getDataComp t) c ) zeroComp tys

    (* Sum the atomic complexities of a measured type *)
    fun sumAtomicComps ( tys : Ty list ) : Complexity =
        foldl ( fn (t,c) => combine (getAtomicComp t) c ) zeroComp tys
                 
    fun mkLabel (prefix:string) (i:int) : Id = Atom.atom("BTy_"^(Int.toString i))
    fun mkTyLabel  (i:int) : Id = mkLabel "BTy_" i
    fun mkTBDLabel (i:int) : Id = mkLabel "TBD_" i
    fun mkBOTLabel (i:int) : Id = mkLabel "BOT_" i

    fun getLabel ( a : AuxInfo ) : Id =
    let val { coverage = c, label = l, ... } = a
    in case l of
            NONE => (mkTyLabel (!Tystamp)) before Tystamp := !Tystamp + 1 
          | SOME id => id
    end

    fun getLabelString ( a : AuxInfo ) : string = Atom.toString (getLabel a)

    fun getLabelInt (a: AuxInfo): int =
	let
	  val label = getLabelString a
	  val sublabel = Substring.full label
	  val id = Substring.string (Substring.triml 4 sublabel)
	  val idop = Int.fromString id
        in
	  case idop of
		NONE => (print "The id in invalid!"; raise InvalidId)
	|	SOME i => i
	end

    fun mkTyAux ( coverage : int ) : AuxInfo = 
	let val next = !Tystamp
            val () = Tystamp := !Tystamp + 1
            val label = mkTyLabel next
	in { coverage = coverage
           , label    = SOME label
           , tycomp   = { tc  = zeroComp
                        , adc = zeroComp
                        , dc  = zeroComp
                        }
           }
	end

    fun mkTyAux1 ( coverage : int, id : Id ) : AuxInfo = 
	let val next = !Tystamp
            val () = Tystamp := !Tystamp + 1
            val label = id
	in { coverage = coverage
           , label    = SOME label
           , tycomp   = { tc  = zeroComp
                        , adc = zeroComp
                        , dc  = zeroComp
                        }
           }
	end

    (*function to get a Ty from a list of Tys by Id *)
    fun getTyById tylist id = 
	let val filtered = List.filter (fn ty => Atom.same ((getLabel (getAuxInfo ty)), id)) tylist
	in case filtered of
		nil => NONE
		| _ => SOME(hd filtered)
	end

    fun getCoverage ( ty : Ty ) : int = #coverage ( getAuxInfo ty )
    fun sumCoverage ( tys : Ty list ) : int =
        foldl ( fn (t:Ty,n:int) => getCoverage t + n ) 0 tys
    fun minCoverage ( tys : Ty list ) : int = 
	case tys of [] => Option.valOf Int.maxInt
        | (ty::tys) => Int.min(getCoverage ty, minCoverage tys)


    (* the two refine types are exactly the same *)
    fun refine_equal (a:Refined, b:Refined):bool =
    	case (a, b) of 
    		(StringME(x), StringME(y)) => (x = y)
    	       |(Int(x, y), Int(x1, y1)) => (x = x1 andalso y = y1)
    	       |(IntConst(x), IntConst(y)) => (x = y)
    	       |(StringConst(x), StringConst(y)) => (x = y)
    	       |(Enum(l1), Enum(l2)) => foldr myand true 
    			(ListPair.map refine_equal(l1, l2))
    	       |(LabelRef(x), LabelRef(y)) => Atom.same(x, y)
    	       | _ => false
    fun refine_equal_op (a:Refined option, b:Refined option):bool =
    	case (a,b) of 
    		(SOME a', SOME b') => refine_equal(a', b')
    	|_ => false
    
    fun refine_equal_op1 (a:Refined option, b:Refined option):bool =
    	case (a,b) of 
    		(SOME a', SOME b') => refine_equal(a', b')
    	| (NONE, NONE) => true
    	|_ => false

    fun locationToString ({lineNo, beginloc, endloc, recNo}:location) = 
(*
	let fun slotsToString [] = ""
              | slotsToString [x] = Int.toString x
              | slotsToString (x::xs) = (Int.toString x)^", "^(slotsToString xs)
	    val indexList = 
  	        case arrayIndexList 
	        of [] => ""
	        |  indexes => "Array Slots: "^(slotsToString indexes)
	in
*)
	    "Line #"^(Int.toString lineNo) ^" Rec #"^(Int.toString recNo)
(*
	end
*)

    fun stringToPrintable s =
		case s of 
			"\t" => "\\t"
			| "\n" => "\\n"
			| "\r" => "\\r"
			| _ => s

    fun ltokenToString ( t : Token, loc : location ) : string = "("^tokenToString t^") loc: "^locationToString loc^" "
    and tokenToString ( t : Token ) : string = 
	case t 
        of Ptime i     => i
	|  Pip i       => i
	|  Phostname i => i
	|  Pdate i     => i
	|  Ppath i     => i
	|  Purl i      => i
        |  PbXML (f,s) => "<"^f^" "^s^">"
        |  PeXML (f,s) => "</"^f^" "^s^">"
	|  Pint (i, s)      =>
             if i < 0 then "-"^(LargeInt.toString (~i)) else LargeInt.toString i
	|  Pfloat (a, b) => a ^ "." ^ b
        |  Pstring s   => s
        |  Pgroup {left, body, right} => (ltokenToString left)^(String.concat (List.map ltokenToString body))^(ltokenToString right)
        |  Pwhite s => stringToPrintable s
        |  Other c  => Char.toString c
        |  Pempty   => "\\0"
        |  Error    => " Error"
    and refinedToString ( re : Refined ) : string =
	case re
	of StringME s   => "[StringME] \""^ s ^ "\""
        | Int(min, max) => "[Int] [" ^ LargeInt.toString(min) ^ "..." ^
                           LargeInt.toString(max)^"]"
        | IntConst a    => "[IntConst] ["^ LargeInt.toString(a) ^"]" 
        | FloatConst (a,b)    => "[FloatConst] ["^ a ^"."^ b ^"]" 
        | StringConst s =>  "[StringConst] \""^(stringToPrintable s) ^ "\"" 
        | Enum rel      => "[Enum] {"^ String.concat(map (fn x => (refinedToString x) ^
                           ", ") rel) ^ "}"
        | LabelRef id   => "[Label] id="^ Atom.toString(id)    

    fun ltokenTyToString ( t : Token, loc : location ) : string = tokenTyToString t 
    and tokenTyToString ( t : Token ) : string = 
	case t 
        of Ptime i     => "[Time]"
	|  Pdate i     => "[Date]"
	|  Pip i       => "[IP]"
	|  Phostname i => "[Host]"
	|  Ppath i     => "[Path]"
	|  Purl i      => "[URL]"
        |  PbXML (f,s) => "bXML["^f^"]"
        |  PeXML (f,s) => "eXML["^f^"]"
(*
	|  Pint (i, s)    => " Pint("^(LargeInt.toString i)^")"
        |  Pstring s => " Pstring("^s^")"
        |  Pwhite s  => " Pwhite("^s^")" 
*)
	|  Pint _    => "[Pint]"              (*" Pint("^(LargeInt.toString i)^")"*)
	|  Pfloat _    => "[Pfloat]"           
        |  Pstring s => "[String]"            (*" Pstring("^s^")"*)
        |  Pwhite s  => "[White]"             (*" Pwhite("^s^")"*) 
        |  Pgroup {left, body, right} =>
             (ltokenTyToString left) ^ "[Group Body]" ^ (ltokenTyToString right)
           (*(" Other("^(Char.toString c)^")") *)
        |  Other c   => "[Other]("^(Char.toString c)^")"
        |  Pempty    => "[Empty]"
        |  Error     => " Error"

    fun printTokenTy ( t : Token ) : unit = print (tokenTyToString t)

    fun LTokensToString [] = "\n"
      | LTokensToString (t::ts) = (ltokenToString t) ^ (LTokensToString ts)

    fun printLocation ( loc : location ) : unit = print (locationToString loc)

    fun printLTokens [] = print "\n"
      | printLTokens ((t,loc)::ts) = (printLocation loc; print ":\t"; printTokenTy t; printLTokens ts)

    fun printTokenTys [] = print "\n"
      | printTokenTys (t::ts) = (printTokenTy t; printTokenTys ts)


    fun covToString ( a : AuxInfo ) : string =
    let val { coverage = cov, label=l, ... } = a
        val lbl = if !printIDs then 
                     case l of
                          NONE => ""
                        | SOME id => ("Id = "^(Atom.toString id)^" ")
                  else ""
    in lbl ^ Int.toString cov
    end

    fun contextsToString ( contexts : Context list ) : string = 
	((case contexts 
	  of [] => "<no records matched context>\n"
	  | _ => (lconcat(
		  List.map (fn tl => 
			    (case tl 
			     of [] => "\t<empty>\n"
			     | _ => ("\t"^( LTokensToString tl) ^"\n"))) contexts))))


    (* Replace when debugged with print (contextsToString contexts) *)
    fun printContexts ( contexts : Context list ) : unit = 
	((case contexts 
	  of [] => print "<no records matched context>\n"
	  | _ => (List.app (fn tl => 
			    (case tl 
			     of [] => print "\t<empty>\n"
			     | _ => (print "\t"; printLTokens tl; print "\n"))) contexts)))

    fun TyToStringD (prefix:string) (longTBDs:bool) (longBottom:bool)
                    (suffix:string) (ty:Ty) : string = 
    let val aux = getAuxInfo ty
        val { tc = tcomp, adc = acomp, dc = dcomp } = #tycomp aux
        val stats = ( "(" ^  (covToString aux)  ^
		      (if print_complexity then (
                      ", tc: " ^ (showBits tcomp)  ^
                      ", ac: " ^ (showBits acomp)  ^
                      ", dc: " ^ (showBits dcomp)  ^ 
		      ", raw: "^ (showBits (combine tcomp dcomp))
		      )
		      else "") ^
			")"
                    )
        val partialD = TyToStringD (prefix^"\t") longTBDs longBottom (";\n")
    in ( prefix ^
         ( case ty of
               Base (aux, ts)  =>
                 let val avg = avgTokenLength ts
                     val tot = sumTokenLength ts
                 in ( case ts of nil =>
                         "[NULL]"
                       | _ => (ltokenTyToString (hd ts))
                    ) ^ " " ^ stats ^
		    (if print_complexity then 
			(" (avg: " ^ Real.fmt (StringCvt.FIX (SOME 2)) avg ^
                                      ", tot: " ^ LargeInt.toString tot ^ ")")
		    else "") 
                 end
             | TBD (aux,i,cl) =>
                "TBD_" ^ (Int.toString i) ^ stats ^
                ( if longTBDs then ( "\n"^ (contextsToString cl) ^ prefix ^ "End TBD" ) else "" )
             | Bottom (aux,i, cl) =>
                "BTM_" ^ (Int.toString i) ^ stats ^
                (if longBottom then ("\n"^(contextsToString cl)^prefix^"End Bottom") else "")
             | Pstruct (aux, tys) =>
                "Pstruct" ^ stats ^ "\n" ^
                (lconcat (List.map partialD tys)) ^ prefix ^ "End Pstruct"
             | Punion (aux, tys)  =>
                "Punion" ^ stats ^ "\n" ^ (lconcat (List.map partialD tys)) ^ prefix ^ "End Punion"
             | Parray (aux, {tokens=tkns, lengths, first=ty1,body=ty2,last=ty3}) =>
                "Parray" ^ stats ^ 
                "("^(lconcat(List.map (fn (t,loc) => (tokenTyToString t) ^" ")tkns)) ^")\n"^
                prefix ^ "First:\n" ^ (partialD ty1) ^ prefix ^ "Body:\n" ^ (partialD ty2) ^
                prefix ^ "Tail:\n" ^ (partialD ty3) ^ prefix ^ "End Parray"
             | RefinedBase (aux, refined, tl) =>
                 let val avg = avgTokenLength tl
                     val tot = sumTokenLength tl
                 in ( refinedToString refined ) ^ " " ^ stats ^ 
		    (if print_complexity then 
			(" (avg: " ^ Real.fmt (StringCvt.FIX (SOME 2)) avg ^
                    	", tot: " ^ LargeInt.toString tot ^ ")") 
		     else "")
                 end
             | Switch(aux ,id, retys) =>
                "Switch(" ^ Atom.toString(id)^")" ^ stats ^ ":\n" ^
                (lconcat (List.map (fn (re, ty) => (prefix^"case "^(refinedToString re)^":\n"^ 
                (partialD ty))) retys)) ^ prefix ^ "End Switch"
             | RArray (aux, sep, term, body, len, lengths) => 
                "RArray" ^ stats ^ "\n" ^
                ( case sep of
                       SOME septok =>
                         prefix ^ "\tSeparator: "^ refinedToString(septok) ^ "\n"
                     | _ => "" ) ^
                ( case term of
                       SOME termtok =>
                         prefix ^ "\tTerminator: "^ refinedToString(termtok) ^ "\n"
                     | _ => "" ) ^
                ( partialD body ) ^ prefix ^ "End RArray" 
             | Poption (aux, ty) =>
                "Poption" ^ stats ^ "\n" ^
                (partialD ty) ^ prefix ^ "End Poption"
         ) ^
         suffix )
    end 

    fun TyToString (ty:Ty):string = TyToStringD "" false false "" ty

    fun printTyD (prefix:string) (longTBDs:bool) (longBottom:bool)
                  (suffix:string) (ty:Ty) : unit =
         print (TyToStringD prefix longTBDs longBottom suffix ty ) 

    fun printTy ( ty : Ty ) : unit = printTyD "" false false "\n" ty

   (**** Function to convert a Ty to a PADS string ***)
   (*suffix is the suffix for variable name*)
    fun TyToPADS (prefix:string) (suffix:string) (isRecord:bool) (mode:int) 
		(siblings: Ty list) (ty:Ty) : string = 
	(*mode = 0: not inline
	  mode = 1: in line (not array)
	  mode = 2: in array body
	  mode = 3: in struct body (can only be core literals)
	  these are used for base and refined base types mostly
	*)
    let val label = getLabelString (getAuxInfo ty)
   	fun getLabelParam ty =
		case ty of
		Switch (aux, id, tys) => (getLabelString aux, SOME ("v" ^ (Atom.toString id)))
		| _ => (getLabelString (getAuxInfo ty), NONE)
    	fun labelToTyString (label:string, paramop) = 
		case paramop of
		NONE => label 
		| SOME param => label ^ "(:" ^ param ^ ":)"
   
    	fun labelToPADS prefix mode (label:string, paramop) = 
		prefix ^ (labelToTyString (label, paramop)) ^ 
		(if (mode = 2) then "" else (" v" ^ label ^ suffix))
	fun notInlineTy ty = case ty of 
		Base _ => false 
		| RefinedBase (_, Enum _, _) => true 
		| RefinedBase _ => false
		| TBD _ => false
		| Bottom _  => false
		| Poption (_, t) => notInlineTy t
		| _ => true
    	fun tyToInlinePADS prefix mode ty = labelToPADS prefix mode (getLabelParam ty)
        fun tokenToPADS label suffix token mode =
	let 
	  val typedef = if mode =0 then "Ptypedef " else ""
	  val label' = if mode =2 then "" 
			else if mode = 0 then (label ^ ";\n")
			else ("v" ^ label ^ suffix ^ ";\n")
	in
	  typedef ^
          (case token of 
		Pstring _ => "Pstring_ME(:\"/[A-Za-z][0-9a-zA-Z_\\-]*/\":) " ^ label'
              | Pint _ => "Pint32 " ^ label'
	      | Pfloat _ => "Pfloat32 " ^ label'
	      | Ptime _ => "Ptime " ^ label'
	      | Pdate _ => "Pdate " ^ label'
	      | Pip _ => "Pip " ^ label' 
	      | Phostname _ => "Phostname " ^ label'
	      | Purl _ => "Pstring_ME(:\"/[:print:]/\":) " ^ label'
	      | Ppath _ => "Pstring_ME(:\"/[:print:]/\":) " ^ label'
	      | Pwhite _ => "Pstring_ME(:\"/[:space:]{1}/\":) " ^ label'
	      | Other c => "Pchar " ^ label'
	      | PbXML _ => "Pstring_ME(:\"/[0-9a-zA-Z_\\-<>]+/\":) " ^ label'
	      | PeXML _ => "Pstring_ME(:\"/[0-9a-zA-Z_\\-<>]+/\":) "  ^ label'
	      | Pgroup _ => (print "Pgroup exists!\n"; raise InvalidTokenTy)
	      | _ => raise InvalidTokenTy (*there should not be any Pempty*)
          )
	end 

	fun allStringConsts relist =
		foldr myand true (map (fn re => (case re of 
						StringConst _ => true 
						| _ => false)
				     ) relist)
        fun isNumConst ty =
		case ty of 
		RefinedBase (aux, (IntConst x), tl) => true
		| RefinedBase (aux, (FloatConst (i, f)), tl) => true
		| _ => false

(*
	fun declareConst prefix ty =
	  let val label = getLabelString(getAuxInfo ty)
	  in
	    case ty of 
		RefinedBase (aux, IntConst x, tl) => prefix ^ "Ptypedef Pint32 "^ label ^
			" : " ^ label ^ " x => {x == " ^ (LargeInt.toString x) ^ "};\n"
		| RefinedBase (aux, FloatConst (i, f), tl) => prefix ^ "Ptypedef Pfloat32 "^ label ^
			" : " ^ label ^ " x => {x == " ^ (i ^ "." ^ f) ^ "};\n"
		| _ => raise TyMismatch
	  end
*)
	fun refinedToPADS label prefix suffix mode refined =
	let
	  val typedef = if mode =0 then "Ptypedef " else ""
	  val label' = if mode =2 then "" 
		       else if mode = 0 then (label ^ ";\n")
		       else ("v" ^ label ^ suffix ^ ";\n")
	in
	  typedef ^ 
  	    (
	      case refined of 
	      StringME re => "Pstring_ME(:\""^ re ^"\":) " ^ label'
	      | Int (min, max) => 
	    	let val minLen = int2Bits min
	    	    val maxLen = int2Bits max
	    	    val maxBits = Real.max(minLen, maxLen)
	    	in 
	    	    if (maxBits<= 8.0) then "Pint8 " ^ label'
	    	    else if maxBits <=16.0 then "Pint16 " ^ label'
	    	         else if maxBits <=32.0 then "Pint32 " ^ label'
	    	              else "Pint64 " ^ label'
	    	end
	      | IntConst i => 
	    	let val bits = int2Bits i
	    	in
		  (if (mode <> 2) then
	    	  (
	    	  if (bits<=8.0) then "Pint8 "
	    	  else if bits<=16.0 then "Pint16 "
	    	  else if bits<=32.0 then "Pint32 "
	    	  else "Pint64 " 
	    	  ) else "") ^ 
		  (if mode=0 then (label ^ " : " ^ label ^ " x => {x == " ^ (LargeInt.toString i) ^ "};\n")
		   else if mode=1 orelse mode = 3 then 
			("v" ^ label ^ suffix ^ " : v" ^ label ^ suffix ^ 
			" == " ^ (LargeInt.toString i) ^ ";\n")
		   else (label ^ " ")
		  )
	    	end
	      | FloatConst (i, f) => 
		  (if (mode <> 2) then "Pfloat32 " else "") ^ 
		  ( if mode = 0 then (label ^ " : " ^ label ^ " x => {x == " ^ (i ^ "." ^ f) ^ "};\n")
		  else if mode = 1 orelse mode = 3 then 
			("v" ^ label ^ suffix ^ " : v" ^ label ^ suffix ^ " == " ^ (i ^ "." ^ f) ^ "};\n")
		  else (label ^ " ")
		  )
	      | StringConst s => if (size s) = 1 then 
				(
				 if mode = 0 then 
					("Pchar " ^ label ^ " : " ^ label ^ " x =>  {x == '" ^ s ^ "'};\n")
			    	 else if mode=1 then ("v" ^ label ^ suffix ^ " Pfrom('" ^ s ^ "');\n")
			    	 else if mode=2 then (label ^ " ")
				 else ("'" ^ s ^ "';\n")
				)
				else if mode = 1 then ("v" ^ label ^ suffix ^ " Pfrom(\"" ^ s ^ "\");\n")
				     else if mode = 3 then ("\"" ^ s ^ "\";\n")
				     else ("Pstring_ME(:\"/"^ s ^"/\":) " ^ label')
	      | Enum res => ""
	      | LabelRef _ => ""
	    ) 
	end
(*
	fun refinedToInlineArray prefix refined =
	    (*only handles a few types of refined here *)
	    prefix ^
	    (
	      case refined of 
	           StringME re => "Pstring_ME(:\""^ re ^"\":) " 
	         | Int (min, max) => 
		    	let val minLen = int2Bits min
		    	    val maxLen = int2Bits max
		    	    val maxBits = Real.max(minLen, maxLen)
		    	in 
		    	    if (maxBits<= 8.0) then "Pint8 " 
		    	    else if maxBits <=16.0 then "Pint16 " 
		    	         else if maxBits <=32.0 then "Pint32 "
		    	              else "Pint64 "
		    	end
	         | StringConst s => "PstringME(:\"/" ^ s ^ "/\":) "
		 | _ => raise TyMismatch
	    )
*)

	fun getRefStr refined = case refined of
	    StringME s => "\"" ^ s ^"\""
	  | StringConst s => if (size s) = 1 then ("'" ^ s ^ "'")
				else ("\"" ^ s ^ "\"")
	  | _ => ""

        fun arrayBodyToInlinePADS ty sep term len =
           ("[" ^ 
	    ( case len of 
		SOME (IntConst x) => LargeInt.toString x
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
		  | NONE => " : Psep(" ^ sepstr ^ ")"
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
		  | NONE => ""
		)
	    ) ^ ";\n"
	)

	fun getIndexes (res, re) =
	  let
		fun getIndex refinedlist re index =
		  case refinedlist of 
		    nil => (~1) (*not found*)
		  | head::tail =>
			if refine_equal (head, re) then index
			else getIndex tail re (index+1)
	  in
		case re of
		StringConst _ => 
			let
				val i = getIndex res re 0
			in 
			  if (i<>(~1)) then [i] 
			  else (case re of
				StringConst "*" => [(~1)]
				| _ => nil
			       )
			 end 
		| IntConst _ => 
			let
				val i = getIndex res re 0
			in if (i<>(~1)) then [i] else nil
			end
		| Enum l => 
		  let 
			val candidates = map (fn x => getIndex res x 0) l
			val finallist = List.filter (fn x => x <> (~1)) candidates
		  in finallist
		  end
		| _ => []
	  end
	fun reToSwitch switchedTy branchno (re, targetTy) =
	  let
	  fun indexes n = List.tabulate (n, (fn x => x))
	  fun getPairs res = ListPair.zip (res, (indexes (length res)))
  	  val suffix = "_" ^ (Int.toString branchno)
  	  in
	    (case switchedTy of
		Base (aux, (Pint _, l)::ts) => 
		  (case re of IntConst x => "\tPcase " ^ (LargeInt.toString x) ^ " : " ^ 
						(TyToPADS "\t" suffix false 1 nil targetTy)
		  	   | StringConst "*" => "\tPdefault : " ^ (TyToPADS "\t" "" false 1 nil targetTy)
			   | Enum res => lconcat (map (fn (re, i) => reToSwitch switchedTy i (re, targetTy))
					 (getPairs res)) 
			   | _ => raise TyMismatch
		  )
		| RefinedBase (aux, Int _, _) =>
		  (case re of IntConst x => "\tPcase " ^ (LargeInt.toString x) ^ " : " ^ 
						(TyToPADS "\t" suffix false 1 nil targetTy) 
		  	   | StringConst "*" => "\tPdefault : " ^ (TyToPADS "\t" "" false 1 nil targetTy)
			   | Enum res => lconcat (map (fn (re, i) => reToSwitch switchedTy i (re, targetTy)) 
					 (getPairs res)) 
			   | _ => raise TyMismatch
		  )
		(* we assume the Enum will only contain IntConst or StringConst and not another Enum *)
		| RefinedBase (aux, Enum res, _) =>
			let
			  val indexes = getIndexes (res, re)
			in
			  case indexes of
				[~1] => "\tPdefault " ^ (TyToPADS "\t" "" false 1 nil targetTy)
			 	(*TODO: multi ids pointing to the same targetTy may not be correct *)
				| _ => lconcat (map (fn i => "\tPcase " ^ (Int.toString i) ^ " : " ^
					(TyToPADS "\t" ("_"^(Int.toString i)) false 1 nil targetTy)) indexes)
			end
		| _ => raise TyMismatch
	    ) 
	  end

	val pRecord = if isRecord then "Precord " else ""  
    in ( prefix ^
       (
         ( case ty of
               Base (aux, (t, loc)::ts)  => pRecord ^ (tokenToPADS label suffix t mode) 
	     | RefinedBase (aux, Enum res, tl) => 
		if mode = 1 orelse mode = 3 then ((tyToInlinePADS "" mode ty)^";\n")
		else if mode = 2 then (tyToInlinePADS "" mode ty)
		else
		(
		if allStringConsts res then
		  let 
		    fun strConstToEnumItem re =
			case re of StringConst s => 
			  let
			    val newlabel = (Atom.toString (getLabel 
					    ({coverage=0, label=NONE, tycomp=zeroComps})))
			  in ("\tv" ^ newlabel ^ " Pfrom(\"" ^ s ^ "\")")
			  end
		    	| _ => raise TyMismatch
		  in (pRecord ^
		    "Penum " ^ label ^ " {\n" ^
	    	     (join (map strConstToEnumItem res) ",\n") ^
	    	     "\n" ^ prefix ^ "};\n")
		  end
	    	else
		  (pRecord ^
		     "Punion "^ label ^ " {\n" ^
		      (lconcat (map (fn x => refinedToPADS 
			            (Atom.toString (getLabel ({coverage=0, label=NONE, tycomp=zeroComps}))) 
				    (prefix^"\t") suffix 1 x) res)) ^
		      prefix ^ "};\n")
		)
             | RefinedBase (aux, refined, tl) => pRecord ^ (refinedToPADS label prefix suffix mode refined) 
             | TBD _ =>
                pRecord ^ "Pstring_ME(:\"/[:print:]/\":) " ^ (if mode=2 then "" else "TBD_" ^ label ^";\n")
             | Bottom _ =>
                pRecord ^ "Pstring_ME(:\"/[:print:]/\":) " ^ (if mode=2 then "" else "BTM_" ^ label ^";\n")
             | Pstruct (aux, tys) =>
		if mode = 1 orelse mode = 3 then ((tyToInlinePADS "" mode ty)^";\n")
		else if mode = 2 then (tyToInlinePADS "" mode ty)
		else
		  let
		   val nonInlineTys = List.filter notInlineTy tys
		   val pre = lconcat (map (TyToPADS prefix "" false 0 tys) nonInlineTys)
		  in pre ^ pRecord ^
		   "Pstruct "^ label ^ " {\n" ^
			(lconcat (map (TyToPADS (prefix ^ "\t") "" false 3 tys) tys)) ^
		   prefix ^ "};\n"
		  end	
             | Punion (aux, tys)  =>
		if mode = 1 orelse mode = 3 then ((tyToInlinePADS "" mode ty)^";\n")
		else if mode = 2 then (tyToInlinePADS "" mode ty)
		else
		  let
		   val nonInlineTys = List.filter notInlineTy tys
		   val pre = lconcat (map (TyToPADS prefix "" false 0 nil) nonInlineTys)
		  in (pre ^ pRecord ^
		   "Punion "^ label ^ " {\n" ^
			(lconcat (map (TyToPADS (prefix ^ "\t") "" false 1 nil) tys)) ^
		   prefix ^ "};\n")
		  end	
             | Switch(aux ,id, retys) =>
		if mode = 1 orelse mode = 3 then ((tyToInlinePADS "" mode ty)^";\n")
		else if mode = 2 then (tyToInlinePADS "" mode ty)
		else
		  let
		   val tys = map #2 retys
		   val nonInlineTys = List.filter notInlineTy tys
		   val pre = lconcat (map (TyToPADS prefix "" false 0 nil) nonInlineTys)
		   val switch = Atom.toString id
		   val switchvar = "v" ^ switch
		   val switchedTyOp = getTyById siblings id
	  	   fun indexes n = List.tabulate (n, (fn x => x))
	           fun getPairs res = ListPair.zip (res, (indexes (length res)))
		  in 
		  case switchedTyOp of
		  NONE =>  (*switched id not found, go back to printing union *)
		   (pre ^ pRecord ^
		   "Punion "^ label ^ " {\n" ^
			(lconcat (map (TyToPADS (prefix ^ "\t") "" false 1 nil) tys)) ^
		   prefix ^ "};\n")
		  | SOME switchedTy =>
		   (pre ^ pRecord ^
		   "Punion "^ label ^ "(:"^ switch ^ " " ^ switchvar ^ ":) {\n" ^ 
		   prefix ^ "  Pswitch (" ^ switchvar ^ ") {\n" ^
			(lconcat (map (fn (i, rety) => reToSwitch switchedTy i rety) 
					(ListPair.zip((indexes (length retys)), retys)))) ^
		   prefix ^ "  }\n" ^
		   prefix ^ "};\n")
		  end	
             | RArray (aux, sep, term, body, len, lengths) => 
		if mode = 1 orelse mode = 3 then ((tyToInlinePADS "" mode ty)^";\n")
		else if mode = 2 then (tyToInlinePADS "" mode ty)
		else
		  let 
		   val pre = if (notInlineTy body) orelse (isNumConst body) 
			     then (TyToPADS prefix "" false 0 nil body)
			     else ""
		  in (pre ^ pRecord ^
                      "Parray " ^ label ^ " {\n" ^ 
		      (TyToPADS (prefix^"\t") "" false 2 nil body) ^ 
		      (arrayBodyToInlinePADS body sep term len) ^
		      prefix ^ "};\n")
		  end
             | Poption (aux, ty) =>
		if (mode = 0)
		then 
		  if notInlineTy ty then (TyToPADS prefix "" false 0 nil ty)
		  else ""
		else "Popt " ^ (TyToPADS "" "" false 2 nil ty) ^ " v" ^ label ^ ";\n"
	     | _ => ""
         )
	)
        )
     end 
     fun TyToPADSFile ty =
	let
	  val recordLabel = getLabelString (getAuxInfo ty)
	  val pads = (TyToPADS "" "" true 0 nil ty) ^
			"Psource Parray entries_t {\n" ^
		    	"\t" ^ recordLabel ^ "[];\n" ^
			"};\n"
	in
	  (recordLabel, pads)
	end  
end
