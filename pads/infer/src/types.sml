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
    exception LocMismatch
    exception NoOption

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
        "{ TC = " ^ showBits (#tc tyc) ^
        ", ADC = " ^ showBits (#adc tyc) ^
        ", DC = " ^ showBits (#dc tyc) ^ " }" ^ "\n\n"

    fun normalizeTyComp ( n : int ) ( tyc : TyComp ) : real =
        ( ( toReal ( #tc tyc ) ) + ( toReal ( #dc tyc ) ) ) / ( Real.fromInt n )

    fun showTyCompNormalized ( n : int ) ( tyc : TyComp ) : string =
        "{ TC = " ^ showBits (#tc tyc) ^
        ", ADC = " ^ showBits (#adc tyc) ^
        ", DC = " ^ showBits (#dc tyc) ^ " }" ^
        " normalized by " ^ Int.toString n ^ " is " ^
        Real.toString ( normalizeTyComp n tyc ) ^ "\n\n"

    (* A fancier print statement for type complexity *)
    fun printTyComp ( pre : string ) ( t : TyComp ) : unit =
        let val () = print ( "\n" ^ pre ^ " type complexity = "   ^ showBits ( #tc t ) )
            val () = print ( "\n" ^ pre ^ " atomic complexity = " ^ showBits ( #adc t ) )
            val () = print ( "\n" ^ pre ^ " data complexity = "   ^ showBits ( #dc t ) ^ "\n\n" )
        in ()
        end

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
		     | Blob of string option * string option (* stopping string or stopping regex *)

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
		(* TODO: Poption should not throw away Pempty tokens *)
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

    exception NotParray
    fun avgParrayBodyLength ( ty : Ty ) : real =
        case ty of
               Parray (a, x) =>
                 let val lens = map #2 (#lengths x)
                 in avgInts lens
                 end
             | _ => raise NotParray

    exception NotRArray
    fun avgRArrayBodyLength ( ty : Ty ) : real =
        case ty of
               RArray (a, sep, term, body, len, lengths ) =>
                 let val lens = map #2 lengths
                 in avgInts lens
                 end
             | _ => raise NotRArray

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
    fun getIdString ( a : AuxInfo ) : string = String.extract ((getLabelString a), 4, NONE)
    fun getLabelForPADS ty =
	let
	  val label = getLabelString (getAuxInfo ty)
	  val id = String.extract (label, 4, NONE)
	in
	  case ty of
	   Base _                => "Base_"^id
        |  TBD _                 => "TBD_"^id
        |  Bottom _              => "BTM_"^id 
        |  Pstruct _             => "Struct_" ^ id
        |  Punion _              => "Union_" ^ id
        |  Parray _              => "Parray_"^id
        |  RefinedBase (aux, re, l) => 
		(case re of Enum _ => "Enum_" ^ id
			| _ => "RBase_"^id
		)
        |  Switch _        	 => "Switch_"^id
        |  RArray _ 		 => "Array_"^id
        |  Poption _           	 => "Opt_"^id
	end
    (*given a pads ty label, return the var name to go with it*)
    fun getVarName (padslabel:string) = 
	let val l = Substring.full padslabel
	    val id = Substring.taker (fn c => c <> #"_") l 
	in "var_" ^ (Substring.string id)
	end
    fun getVarNameFromID id = "var_" ^ (String.extract(id, 4, NONE))
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
	let val label = id
	in { coverage = coverage
           , label    = SOME label
           , tycomp   = { tc  = zeroComp
                        , adc = zeroComp
                        , dc  = zeroComp
                        }
           }
	end

    fun mkTyAux3 ( coverage : int, comp: TyComp ) : AuxInfo = 
	let val next = !Tystamp
            val () = Tystamp := !Tystamp + 1
            val label = mkTyLabel next
	in { coverage = coverage
           , label    = SOME label
           , tycomp   = comp
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
    fun incCoverage (a : AuxInfo) : AuxInfo = 
	case a of 
	{coverage = c, label = l, tycomp = tc} => {coverage = (c+1), label = l, tycomp = tc}
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
	    "Line #"^(Int.toString lineNo) ^" Rec #"^(Int.toString recNo)
*)
	    "Line #"^(Int.toString lineNo) ^ " RecNo " ^ (Int.toString recNo) ^ 
		" ("^(Int.toString beginloc) ^ ":" ^ (Int.toString endloc) ^ ")"
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
   (* this function returns a "printable" string format of the token *)
    and tokenToString ( t : Token ) : string = 
	case t 
        of Ptime i     => i
	|  Pip i       => i
	|  Phostname i => i
	|  Pdate i     => i
	|  Ppath i     => i
	|  Purl i      => i
	|  Pemail i      => i
	|  Pmac i      => String.map (Char.toLower) i
	|  Ptext i      => i
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
	| Blob (str, patt) => 
	   (
	     case (str, patt) of
	     (SOME s, _) => "[Blob] (" ^ s ^ ")"
	     | (_, SOME s) => "[Blob] (" ^ s ^ ")"
	     | _ => "[Blob] (Peor)"
	   )

    fun ltokenTyToString ( t : Token, loc : location ) : string = tokenTyToString t 
    and tokenTyToString ( t : Token ) : string = 
	case t 
        of Ptime i     => "[Time]"
	|  Pdate i     => "[Date]"
	|  Pip i       => "[IP]"
	|  Phostname i => "[Host]"
	|  Ppath i     => "[Path]"
	|  Purl i      => "[URL]"
	|  Pemail i      => "[Email]"
	|  Pmac i      => "[MAC]"
	|  Ptext i      => "[Text]"
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
                      ", dc: " ^ (showBits dcomp) 
                      )
		      else "") ^
		      ", raw: "^ (showBits (combine tcomp dcomp))  ^
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
                       | _ => (ltokenTyToString (hd ts)) (* ^ (LTokensToString ts) *)
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
		(* prefix ^ (lengthsToString lengths) ^ *)
                prefix ^ "First:\n" ^ (partialD ty1) ^ prefix ^ "Body:\n" ^ (partialD ty2) ^
                prefix ^ "Tail:\n" ^ (partialD ty3) ^ prefix ^ "End Parray"
             | RefinedBase (aux, refined, tl) =>
                 let val avg = avgTokenLength tl
                     val tot = sumTokenLength tl
                 in ( refinedToString refined ) (*^ (LTokensToString tl)*)
		      ^ " " ^ stats ^ 
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
		(* prefix ^ (lengthsToString lengths) ^ *)
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

    fun allStringConsts relist =
		foldr myand true (map (fn re => (case re of 
						StringConst _ => true 
						| _ => false)
				     ) relist)

(* Function to measure the variances of the structure by computing the total number of
  union/option/enum branches in the tree *) 
    fun variance ty =
	  case ty of
	   Base (a, _)           => 1.0
        |  TBD (a, _, _)            => 1.0
        |  Bottom (a, _, _)         => 1.0
        |  Pstruct (a, tys)      => foldl Real.max 1.0 (map variance tys)
        |  Punion (a, tys)       => foldl op+ 0.0 (map variance tys)
        |  Parray (a, {tokens=t, lengths=len, first=f, body=b, last=l}) => 
		let val fv = variance f
		    val bv = variance b
		    val lv = variance l
		    val avglen = avgInts (#1 (ListPair.unzip len))
		    val bv' = bv * avglen 
		in
		    foldl Real.max 1.0 [bv, bv', lv]
		end
        |  RefinedBase (aux, re, l) => 
		(case re of Enum res => Real.fromInt (length res)
			| _ => 1.0
		)
        |  Switch (a, id, retys)  	 => foldl op+ 0.0 (map (fn (re, ty) => variance ty) retys)
        |  RArray (a,sep,term,body,len,lengths) => variance body
        |  Poption (a, body)     	 => 1.0 + (variance body)

(* Function to test of a ty is empty *)
  fun isEmpty(ty) = case ty of 
	 Base(_, tkl) =>
		( 
		  case (hd tkl) of 
			(Pempty, _) => true
			| _ => false
		)
	| _=> false

(* Function to compute the total number of tokens associated with this Ty,
   excluding Pemptys *)
    fun getNumTokens ty =
	  case ty of
	   Base (a, l)           => if isEmpty ty then 0 else length l
        |  TBD (a, _, l)            => 0
        |  Bottom (a, _, l)         => 0
        |  Pstruct (a, tys)      => sumSmallInts (map getNumTokens tys)
        |  Punion (a, tys)       => sumSmallInts (map getNumTokens tys)
        |  Parray (a, {tokens=t, lengths=len, first=f, body=b, last=l}) => 
		getNumTokens f + getNumTokens b + getNumTokens l
        |  RefinedBase (aux, re, l) => length l
        |  Switch (a, id, retys)  	 => sumSmallInts (map (fn (r, t) => getNumTokens t) retys)
        |  RArray (a,sep,term,body,len,lengths) => getNumTokens body
        |  Poption (a, body)     	 => getNumTokens body

(*Function to extract the header and footer from a given ty if available
  this is currently only used at the top level of the data*)
    fun extractHeaderFooter ty =
	case ty of
	  Punion(aux, tys) =>
	    let
		fun less (a:int) (b:int) : bool = a < b
		fun greater (a:int) (b:int) : bool = a > b
		(*get the first line # of a given ty*)
		fun firstLine ty =
		  case ty of
		    Pstruct (_, tys) => firstLine (hd tys)
		  | Punion (_, tys) => min less (map firstLine tys)
		  | Parray (_, {tokens=_, lengths=_, first=f, body=_, last=_}) =>
			firstLine f
		  | Base (_, ltokens:LToken list) => min less (map (fn (t, l)=> (#lineNo l)) ltokens)
		  | _ => raise TyMismatch
		(*get the last line # of a given ty*)
		fun lastLine ty =
		  case ty of
		    Pstruct (_, tys) => lastLine (hd tys)
		  | Punion (_, tys) => max greater (map lastLine tys)
		  | Parray (_, {tokens=_, lengths=_, first=f, body=_, last=_}) =>
			lastLine f
		  | Base (_, ltokens:LToken list) => max greater (map (fn (t, l)=> (#lineNo l)) ltokens)
		  | _ => raise TyMismatch

		(*order of two tys by the first line*)	
		fun lineGreater (ty1, ty2) = (firstLine ty1) > (firstLine ty2)
		(*order of two tys by the last line*)	
		fun lineGreater1 (ty1, ty2) = (lastLine ty1) > (lastLine ty2)
		(*this function returns the headers as well as the remaining tys *)
		fun getHeaders tys numLines = 
			case tys of
			  nil => (nil, nil)
			| ty::tail => if numLines>0 andalso getCoverage ty = 1 then
				      let val (newheaders, newtail) = getHeaders tail (numLines-1)
				      in (ty::newheaders, newtail)
				      end
				      else (nil, tys) (* only allow single line headers *)
		fun getFooters tys numLines = 
			let val (rev_footers, rev_tail) = getHeaders (rev tys) numLines
			in (rev rev_footers, rev rev_tail)
			end
		val sortedTys = ListMergeSort.sort lineGreater tys
		val (headers, tail) = getHeaders sortedTys def_maxHeaderChunks
		val sortedTail = ListMergeSort.sort lineGreater1 tail
		val (footers, bodyTys) = getFooters sortedTail def_maxHeaderChunks
	  	(* function to test of all data attached to ty are 
		  consecutive chunks from record b to record e *)
(*
		fun consecChunks b e ty =
		  case ty of
		    Pstruct (_, tys) => consecChunks b e (hd tys)
		  | Punion (_, tys) => foldr myand 
					true (map (consecChunks b e) tys)
		  | Parray (_, {tokens=_, lengths=_, first=f, body=_, last=_}) =>
			consecChunks b e f
		  | Base (_, ltokens:LToken list) => 
			let 
			    val sorted_ltokens = ListMergeSort.sort 
				(fn ((t1, l1), (t2, l2)) => ((#recNo l1) > (#recNo l2))) ltokens
			in
 			    (getCoverage ty = e - b + 1) andalso (#recNo (#2 (hd sorted_ltokens))) = b 
				andalso (#recNo (#2 (List.last sorted_ltokens))) = e
			end
		  | _ => raise TyMismatch
		fun isHeader ty = 
		  if getCoverage ty > def_maxHeaderChunks then false
		  else consecChunks 0 ((getCoverage ty)-1) ty
		fun isFooter totalChunks ty = 
		  if getCoverage ty > def_maxHeaderChunks then false
		  else ((* print "check consec for footer...\n";*) 
		     consecChunks (totalChunks-(getCoverage ty)) (totalChunks-1) ty)
		val totalChunks = (#coverage aux) 
		val possibleHeaders = List.filter isHeader tys
		val possibleFooters = List.filter (isFooter totalChunks) tys
		val headerTyOp = if length possibleHeaders = 0
			then (NONE)
			else if length possibleHeaders = 1
			then SOME (hd possibleHeaders)
			else NONE
		val footerTyOp = if length possibleFooters = 0
			then (NONE)
			else if length possibleFooters = 1
			then SOME (hd possibleFooters)
			else NONE
		fun notLabelEqual headerOp footerOp ty =
		  let
			val tyLabel = getLabel (getAuxInfo ty)
		  in
			case (headerOp, footerOp) of
			  (SOME h, SOME f) => not (Atom.same(tyLabel, getLabel (getAuxInfo h)))
				andalso (not (Atom.same(tyLabel, getLabel (getAuxInfo f))))
			| (SOME h, NONE) => not (Atom.same(tyLabel, getLabel (getAuxInfo h)))
		 	| (NONE, SOME f) => not (Atom.same(tyLabel, getLabel (getAuxInfo f)))
			| _ => true
		  end
*)
		fun combAux (ty, (coverage, comp)) =
		  ((coverage + getCoverage ty), (combTyComp (#tycomp (getAuxInfo ty)) comp))
            in
		case (headers, footers) of
		  (nil, nil) => (nil, nil, NONE, ty)
		| _ => 
		  let
			val len = length bodyTys
		  in
			if len = 1 then
			  (headers, footers, SOME aux, (hd bodyTys))
			else if len = 0 then raise TyMismatch
			else 
			  let 
			    val (newcov, newcomp) = List.foldl combAux (0, zeroComps) bodyTys
			    val newUnionAux = mkTyAux3 (newcov, newcomp)
			  in
			    (headers, footers, SOME aux, Punion(newUnionAux, bodyTys))
			  end
		  end
	    end
	| _ => (nil, nil, NONE, ty)
		

(**************
    val dateStoppingChar= ref #""
    val dateStoppingRe = ref ""
    val timeStoppingChar = ref #""
    val timeStoppingRe = ref ""
	(* this function locate date and time in the tylist of a Pstruct 
	  and their stopping character or patters *)
	fun updateDateTimeStopping tylist =
	  (*first clear the existing stopping chars and res *)
	  let
            val _ = dateStoppingChar := #""
    	    val _ = dateStoppingRe := ""
    	    val _ = timeStoppingChar := #""
    	    val _ = timeStoppingRe := ""
	    fun findRefined ty =
	    (*funtion to find the first base or refine type and convert it to refined type *)
	      case ty of
		  Pstruct(_, tylist) => findRefined (hd tylist)
		| RefinedBase(_, refined, _) => SOME(refined)
		| Base(_, ltokens) => ltokenlToRefinedOp ltokens
		| _ => NONE

	    fun getDate tylist =
		case tylist of
		Base (a, (Pdate t, loc)::tl)::tlist => 
			let
			  val re = findRefined (hd tlist)
			in
			  case re of 
			    StringConst s => s
			    | _ => ""
			end
		_
		 if size s = 1 then dateStoppingChar := (Char.fromString s)
						else dateStoppingRe := ("/" ^ s ^ "/")
*************)

(***************

   (**** Function to convert a Ty to a PADS string ***)
   (*suffix is the suffix for variable name*)
    fun TyToPADS (prefix:string) (suffix:string) (isRecord:bool) (mode:int) 
		(siblings: Ty list) (ty:Ty) : string = 
	(*mode = 0: not inline
	  mode = 1: in line (not array)
	  mode = 2: in array body
	  mode = 3: in struct body (can only be core literals)
	  mode = 4: the target Ty in a switch branch where there's dup (to use Pfrom to avoid dup)
	  these are used for base and refined base types mostly
	*)
    let 
	val label = getLabelForPADS ty
	(* returns the label for type, parameter and variable name to be used with it *)
   	fun getLabelParamVar ty =
	   let 
	     val rawLabel = getLabelString (getAuxInfo ty)
	   in
		case ty of
		Switch (aux, id, tys) => (getLabelForPADS ty, SOME(getVarNameFromID (Atom.toString id)), 
			getVarNameFromID rawLabel)
		| _ => (getLabelForPADS ty, NONE, getVarNameFromID rawLabel)
	   end
    	fun labelToTyString (label:string, paramop) = 
		case paramop of
		NONE => label 
		| SOME param => label ^ "(:" ^ param ^ ":)"
    	fun labelToPADS prefix mode (label, paramop, varName) = 
		prefix ^ (labelToTyString (label, paramop)) ^ 
		(if (mode = 2) then "" else (" "^ varName ^ suffix))
	fun notInlineTy ty = case ty of 
		Base _ => false 
		| RefinedBase (_, Enum _, _) => true 
		| RefinedBase _ => false
		| TBD _ => false
		| Bottom _  => false
		| _ => true
    	fun tyToInlinePADS prefix mode ty = labelToPADS prefix mode (getLabelParamVar ty)
        fun tokenToPADS label suffix token mode =
	let 
	  val typedef = if mode =0 then "Ptypedef " else ""
	  val label' = if mode =2 then "" 
			else if mode = 0 then (label ^ ";\n")
			else ((getVarName label) ^ suffix ^ ";\n")
	in
	  typedef ^
          (case token of 
		Pstring _ => "PPstring " ^ label'
              | Pint _ => "Pint64 " ^ label'
	      | Pfloat _ => "Pfloat64 " ^ label'
	      | Ptime _ =>  "PPtime " ^ label'
(********
			   if !timeStoppingChar<> #"" then 
			      ("Ptime(:'" ^ (Char.toString !timeStoppingChar) ^ "':) " ^ label')
			   else if !timeStoppingRe <> "" then
			      ("Ptime_SE(:\"" ^ !timeStoppingRe ^ "\":) " ^ label')
			   else ("PPTime " ^ label')
********)
	      | Pdate _ => "PPdate " ^ label'
(********
			   if !dateStoppingChar<> #"" then 
			      ("Pdate(:'" ^ (Char.toString !dateStoppingChar) ^ "':) " ^ label')
			   else if !dateStoppingRe <> "" then
			      ("Pdate_SE(:\"" ^ !dateStoppingRe ^ "\":) " ^ label')
			   else ("PPDate " ^ label')
********)
	      | Pip _ => "PPip " ^ label'
	      | Phostname _ => "PPhostname " ^ label'
	      | Purl _ => "PPurl " ^ label'
	      | Ppath _ => "PPpath " ^ label'
	      | Pemail _ => "PPemail " ^ label'
	      | Pmac _ => "PPmac " ^ label'
	      | Pwhite _ => "PPwhite " ^ label'
	      | Ptext _ => "PPtext " ^ label'
	      | Other c => "PPchar " ^ label'
	      | PbXML _ => "PPbXML " ^ label'
	      | PeXML _ => "PPeXML "  ^ label'
	      | Pgroup _ => (print "Pgroup exists!\n"; raise InvalidTokenTy)
	      | Pempty => "Pcompute Pint8 " ^ (getVarName label) ^ suffix ^ " = 0;\n" 
	      | _ => raise InvalidTokenTy
          )
	end 
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
		       else ((getVarName label) ^ suffix ^ ";\n")
	in
	  typedef ^ 
  	    (
	      case refined of 
	      StringME re => "Pstring_ME(:\""^ re ^"\":) " ^ label'
	      | Int (min, max) => 
	    	let val minLen = int2Bits min
	    	    val maxLen = int2Bits max
	    	    val maxBits = Real.max(minLen, maxLen)
		    val typeName = if (min>=0) then "Puint" else "Pint"
	    	in 
	    	    if (maxBits<= 8.0) then typeName ^ "8 " ^ label'
	    	    else if maxBits <=16.0 then typeName ^ "16 " ^ label'
	    	         else if maxBits <=32.0 then typeName ^ "32 " ^ label'
	    	              else typeName ^ "64 " ^ label'
	    	end
	      | IntConst i => 
	    	let val bits = int2Bits i
		    val typeName = if (i>=0) then "Puint" else "Pint"
	    	in
		  (if (mode <> 2) then
	    	  (
	    	  if (bits<=8.0) then typeName ^ "8 "
	    	  else if bits<=16.0 then typeName ^ "16 "
	    	  else if bits<=32.0 then typeName ^ "32 "
	    	  else typeName ^ "64 " 
	    	  ) else "") ^ 
		  (if mode=0 then (label ^ " : " ^ label ^ " x => {x == " ^ (intToCString i) ^ "};\n")
		   else if mode=1 orelse mode = 3 orelse mode = 4 then 
			((getVarName label) ^ suffix ^ " : " ^ (getVarName label) ^ suffix ^ 
			" == " ^ (intToCString i) ^ ";\n")
		   else (label ^ " ")
		  )
	    	end
	      | FloatConst (i, f) => 
		  (if (mode <> 2) then "Pfloat32 " else "") ^ 
		  ( if mode = 0 then (label ^ " : " ^ label ^ " x => {x == " ^ (i ^ "." ^ f) ^ "};\n")
		  else if mode = 1 orelse mode = 3 orelse mode = 4 then 
			((getVarName label) ^ suffix ^ " : " ^ (getVarName label) ^ suffix ^ 
				" == " ^ (i ^ "." ^ f) ^ ";\n")
		  else (label ^ " ")
		  )
	      | StringConst s => if (size s) = 1 then 
				(
				 if mode = 0 then 
					("Pchar " ^ label ^ " : " ^ label ^ " x =>  {x == '" ^ s ^ "'};\n")
			    	 else if mode=1 then 
				   if isCIdentifier s then "'" ^ s ^ "';\n"
				   else ((getVarName label) ^ suffix ^ 
					" Pfrom('" ^ (String.toCString s) ^ "');\n")
			    	 else if mode=2 then ("Pstring_ME(:\"/" ^ escape(s) ^ "/\":) ")
				 else if mode=4 then ((getVarName label) ^ suffix ^ 
					" Pfrom('" ^ (String.toCString s) ^ "');\n")
				 else ("'" ^ (String.toCString s) ^ "';\n")
				)
				else if mode = 1 then 
				  if isCIdentifier s then ("\"" ^ s ^ "\";\n")
				  else ((getVarName label) ^ suffix ^ 
					" Pfrom(\"" ^ (String.toCString s) ^ "\");\n")
				 else if mode = 3 then ("\"" ^ (String.toCString s) ^ "\";\n")
				 else if mode=4 then ((getVarName label) ^ suffix ^ 
					" Pfrom(\"" ^ (String.toCString s) ^ "\");\n")
				 else ("Pstring_ME(:\"/"^ escape(s) ^"/\":) " ^ label')
	      | Enum res => ""
	      | LabelRef _ => ""
	    ) 
	end

	(*this function returns the type string of a Ty for use in Switches*)
	fun getSwitchTypeString ty =
	  if notInlineTy ty then getLabelForPADS ty
	  else case ty of
		Base (_, (tok, loc)::tl) => tokenToPADS "" "" tok 2
		| RefinedBase (_, refined, _) => refinedToPADS "" "" "" 2 refined
		| _ => raise TyMismatch

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
	)

	fun getIndexes (res, re, enumIdStr) =
	  let
		fun getIndex refinedlist re index =
		  case refinedlist of 
		    nil => (~1) (*not found*)
		  | head::tail =>
			if refine_equal (head, re) then index
			else getIndex tail re (index+1)
	  in
		case re of
		StringConst s => 
			let
				val i = getIndex res re 0
			in 
			  if (i<>(~1)) then 
				if isCIdentifier s then [s^enumIdStr]
				else [Int.toString i] 
			  else (case re of
				StringConst "*" => ["P_DEFAULT"]
				| _ => nil
			       )
			 end 
		| IntConst _ => 
			let
				val i = getIndex res re 0
			in if (i<>(~1)) then [Int.toString i] else nil
			end
		| Enum l => List.concat (map (fn x => getIndexes (res, x, enumIdStr)) l)
(*
		  let 
			val candidates = map (fn x => getIndex res x 0) l
			val finallist= List.filter (fn x => x <> (~1)) candidates
		  in finallist
		  end
*)
		| _ => []
	  end
	fun reToSwitch switchedTy branchno dup (re, targetTy) =
	  let
	  fun indexes n = List.tabulate (n, (fn x => x))
	  fun getPairs res = ListPair.zip (res, (indexes (length res)))
  	  val suffix = "_" ^ (Int.toString branchno)
	  val mode = case re of Enum _ => 4
			| _ => if dup then 4 else 1
  	  in
	    (case switchedTy of
		Base (aux, (Pint _, l)::ts) => 
		  (case re of IntConst x => "\tPcase " ^ (LargeInt.toString x) ^ " : " ^ 
						(TyToPADS "\t" suffix false mode nil targetTy)
		  	   | StringConst "*" => "\tPdefault : " ^ (TyToPADS "\t" "" false 1 nil targetTy)
			   | Enum res => lconcat (map (fn (re, i) => 
						reToSwitch switchedTy i true (re, targetTy)) (getPairs res)) 
			   | _ => raise TyMismatch
		  )
		| RefinedBase (aux, Int _, _) =>
		  (case re of IntConst x => "\tPcase " ^ (LargeInt.toString x) ^ " : " ^ 
						(TyToPADS "\t" suffix false mode nil targetTy) 
		  	   | StringConst "*" => "\tPdefault : " ^ (TyToPADS "\t" "" false 1 nil targetTy)
			   | Enum res => lconcat (map (fn (re, i) => 
						reToSwitch switchedTy i true (re, targetTy)) (getPairs res)) 
			   | _ => raise TyMismatch
		  )
		(* we assume the Enum will only contain IntConst or StringConst and not another Enum *)
		| RefinedBase (aux, Enum res, _) =>
			let
			  val indexes = getIndexes (res, re, (getIdString aux))
			in
			  case indexes of
				["P_DEFAULT"] => "\tPdefault : " ^ (TyToPADS "\t" "" false 1 nil targetTy)
				| _ => lconcat (map (fn i => "\tPcase " ^ i ^ " : " ^
					(TyToPADS "\t" ("_"^ i) false mode nil targetTy)) indexes)
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
		if mode = 2 then (tyToInlinePADS "" mode ty)
		else if mode = 0 then
		(
		if allStringConsts res then
		  let 
		    val idStr = getIdString aux
		    fun strConstToEnumItem re =
			case re of StringConst s => 
			  if isCIdentifier s then 
			    ("\t"^ s ^ idStr ^ " Pfrom(\"" ^ s ^ "\")")
			  else
			    let
			      val newlabel = (Atom.toString (getLabel 
					    ({coverage=0, label=NONE, tycomp=zeroComps})))
			    in ("\t" ^ (getVarName newlabel) ^ " Pfrom(\"" ^ (String.toCString s) ^ "\")")
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
		else ((tyToInlinePADS "" mode ty)^";\n")
             | RefinedBase (aux, refined, tl) => pRecord ^ (refinedToPADS label prefix suffix mode refined) 
             | TBD _ =>
                pRecord ^ "Pstring_ME(:\"/[:print:]/\":) " ^ (if mode=2 then "" else "TBD_" ^ label ^";\n")
             | Bottom _ =>
                pRecord ^ "Pstring_ME(:\"/[:print:]/\":) " ^ (if mode=2 then "" else "BTM_" ^ label ^";\n")
             | Pstruct (aux, tys) =>
		if mode = 2 then (tyToInlinePADS "" mode ty)
		else if mode = 0 then
		  let
(*
		   val _ = updateDateTimeStopping (tys)
*)
		   val nonInlineTys = List.filter notInlineTy tys
		   val pre = lconcat (map (TyToPADS prefix "" false 0 tys) nonInlineTys)
		  in pre ^ pRecord ^
		   "Pstruct "^ label ^ " {\n" ^
			(lconcat (map (TyToPADS (prefix ^ "\t") "" false 3 tys) tys)) ^
		   prefix ^ "};\n"
		  end	
		else ((tyToInlinePADS "" mode ty)^";\n")
             | Punion (aux, tys)  =>
		if mode = 2 then (tyToInlinePADS "" mode ty)
		else if mode = 0 then
		  let
		   val nonInlineTys = List.filter notInlineTy tys
		   val pre = lconcat (map (TyToPADS prefix "" false 0 nil) nonInlineTys)
		  in (pre ^ pRecord ^
		   "Punion "^ label ^ " {\n" ^
			(lconcat (map (TyToPADS (prefix ^ "\t") "" false 1 nil) tys)) ^
		   prefix ^ "};\n")
		  end	
		else ((tyToInlinePADS "" mode ty)^";\n")
             | Switch(aux ,id, retys) =>
		if mode = 2 then (tyToInlinePADS "" mode ty)
		else if mode = 0 then
		  let
		   val tys = map #2 retys
		   val nonInlineTys = List.filter notInlineTy tys
		   val pre = lconcat (map (TyToPADS prefix "" false 0 nil) nonInlineTys)
		   val switchvar = (getVarNameFromID (Atom.toString id))
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
		    let 
			val switch = getSwitchTypeString switchedTy
		    in
		   	(pre ^ pRecord ^
		   	"Punion "^ label ^ "(:"^ switch ^ " " ^ switchvar ^ ":) {\n" ^ 
		   	prefix ^ "  Pswitch (" ^ switchvar ^ ") {\n" ^
			(lconcat (map (fn (i, rety) => reToSwitch switchedTy i false rety) 
					(ListPair.zip((indexes (length retys)), retys)))) ^
		   	prefix ^ "  }\n" ^
		   	prefix ^ "};\n")
		    end
		  end	
		else ((tyToInlinePADS "" mode ty)^";\n")
             | RArray (aux, sep, term, body, len, lengths) => 
		if mode = 2 then (tyToInlinePADS "" mode ty)
		else if mode = 0 then
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
		else ((tyToInlinePADS "" mode ty)^";\n")
             | Poption (aux, body) =>
		if mode = 2 then (tyToInlinePADS "" mode ty)
		else if mode = 0 then
		  let
		    val pre = if (notInlineTy body) orelse (isNumConst body) then 
				TyToPADS prefix "" false 0 nil body
			      else ""
		  in (pre ^ pRecord ^
		    "Popt " ^ (TyToPADS "" "" false 2 nil body) ^ " " ^ label ^ ";\n")
		  end
		else ((tyToInlinePADS "" mode ty)^";\n")
	     | _ => ""
         )
	)
        )
     end 
     fun TyToPADSFile ty numHeaders numFooters includeFile =
	(* assume that if a ty has header and footer, the body is just one single Ty*)
	let
	  val recordLabel = getLabelForPADS (ty)
	  val pads = "#include \""^ includeFile ^"\"\n" ^
		(if numHeaders=0 andalso numFooters=0 then
			((TyToPADS "" "" true 0 nil ty) ^
			"Psource Parray entries_t {\n" ^
		    	"\t" ^ recordLabel ^ "[];\n" ^
			"};\n")
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
			      in
				(case headers of
					nil => ""
					| _ => String.concat (map (TyToPADS "" "" true 0 nil) headers)
				) ^
				(TyToPADS "" "" true 0 nil body) ^
				(case footers of
					nil => ""
					| _ => String.concat (map (TyToPADS "" "" true 0 nil) footers)
				) ^
				"Psource Pstruct " ^ recordLabel ^ " {\n" ^
				(case headers of
					nil => ""
					| _ => String.concat (map 
					  (fn t => ("\t" ^ (getLabelForPADS t) ^ 
						" header" ^ getIdString (getAuxInfo t) ^ ";\n")) 
					  headers)
				) ^
				("\t" ^ (getLabelForPADS body) ^ "[] body : Plongest;\n") ^
				(case footers of 
					nil => ""
					| _ => String.concat (map 
					  (fn t => ("\t" ^ (getLabelForPADS t) ^ 
					  " footer" ^ getIdString (getAuxInfo t) ^ ";\n")) 
					  footers)
				) ^
				"};\n"
			      end
		  | _ => raise TyMismatch
		)
	in
	  (recordLabel, pads)
	end 
******************)

end
