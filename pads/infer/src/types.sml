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
    exception AllOptions

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

(*
    fun combTyComp2 ( t1 : TyComp, t2 : TyComp ) : TyComp =
        { tc  = combine (#tc t1) (#tc t2)
        , adc = combine (#adc t1) (#adc t2)
        , dc  = combine (#dc t1) (#dc t2)
        }
*)

    type AuxInfo = { coverage : int        (* Coverage of
                                                 -- a struct is minimum coverage
                                                 --      of its constituents;
                                                 -- a union is sum of coverage
                                                         of its consituents; *)
                   , label    : Id option  (* introduced during refinement as a tag *)
                   , tycomp   : TyComp
		   , len : real    	   (* this is the average length of characters 
					      described by this ty *)
                   }

(*
    fun updateComps ( aux : AuxInfo ) ( comps : TyComp ) : AuxInfo =
        { coverage = #coverage aux
        , label    = #label aux
        , tycomp   = comps
	, len = #len aux
        }
*)

    (* Update the len and type and data complexity of an AuxInfo *)
     fun updateLenComps ( aux : AuxInfo ) (l: real ) (comps : TyComp) : AuxInfo =
        { coverage = #coverage aux
        , label    = #label aux
        , tycomp   = comps
	, len = l
        }

     fun updateLen (aux : AuxInfo) (len : real) : AuxInfo = 
	{ coverage = #coverage aux
        , label    = #label aux
        , tycomp   = #tycomp aux
	, len = len
        }

    fun updateCoverage (aux : AuxInfo) (cov : int) : AuxInfo =
	{ coverage = cov
        , label    = #label aux
        , tycomp   = #tycomp aux
	, len = #len aux
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

    fun getCoverage ( ty : Ty ) : int = #coverage ( getAuxInfo ty )
    fun incCoverage (a : AuxInfo) : AuxInfo = 
	case a of 
	{coverage = c, label = l, tycomp = tc, len = leng} => 
		{coverage = (c+1), label = l, tycomp = tc, len = leng}
    fun sumCoverage ( tys : Ty list ) : int =
        foldl ( fn (t:Ty,n:int) => getCoverage t + n ) 0 tys
    fun minCoverage ( tys : Ty list ) : int = 
	case tys of [] => Option.valOf Int.maxInt
        | (ty::tys) => Int.min(getCoverage ty, minCoverage tys)

    fun sort_ltokens (ltokens : LToken list ) : LToken list =
	let fun f ((t1, l1) : LToken, (t2, l2) : LToken) : bool =
		(#lineNo l1 > #lineNo l2) orelse
		((#lineNo l1 = #lineNo l2) andalso 
		((#beginloc l1) >= (#endloc l2)))
	in
           ListMergeSort.sort f ltokens
	end

    exception NotParray
    fun avgParrayBodyLength ( ty : Ty ) : real =
        case ty of
               Parray (a, x) =>
                 let val lens = map #1 (#lengths x)
                 in avgInts lens
                 end
             | _ => raise NotParray

    exception NotRArray
    fun avgRArrayBodyLength ( ty : Ty ) : real =
        case ty of
               RArray (a, sep, term, body, len, lengths ) =>
                 let val lens = map #1 lengths
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

    (* weighted sum of type complexities of a list of tys *)
    fun weightedTypeComps (tys : Ty list) : Complexity =
      let val sum = foldl (fn (t, c) => 
	combine (multCompS (getCoverage t) (getTypeComp t)) c) zeroComp tys
      in divComp (LargeInt.fromInt (sumCoverage tys)) sum
      end

    (* Sum the data complexities of a measured type *)
    fun sumDataComps ( tys : Ty list ) : Complexity =
        foldl ( fn (t,c) => combine (getDataComp t) c ) zeroComp tys

    (* Sum the atomic complexities of a measured type *)
    fun sumAtomicComps ( tys : Ty list ) : Complexity =
        foldl ( fn (t,c) => combine (getAtomicComp t) c ) zeroComp tys

    (* Compute the weighted sum of the data complexities of a list of types *)
    fun weightedData ( tot : int ) ( tys : Ty list ) : Complexity =
    let fun f ( t : Ty, c : Complexity ) : Complexity =
              combine c ( multCompR ( frac ( getCoverage t ) tot )
                                    ( getDataComp t )
                        )
    in foldl f zeroComp tys
    end

    fun weightedAtomic ( tot : int ) ( tys : Ty list ) : Complexity =
    let fun f ( t : Ty, c : Complexity ) : Complexity =
              combine c ( multCompR ( frac ( getCoverage t ) tot )
                                    ( getAtomicComp t )
                        )
    in foldl f zeroComp tys
    end

    fun score ty =
	let
		val comps = getComps ty
		(* val rawcomp = combine (#tc comps) (#dc comps) *)
		val rawcomp = combine (#tc comps) 
				(multCompR (!adcCoeff) (#adc comps))
	in (toReal rawcomp)
    end
                 
    fun mkLabel (prefix:string) (i:int) : Id = Atom.atom("BTy_"^(Int.toString i))
    fun mkTyLabel  (i:int) : Id = mkLabel "BTy_" i
    fun mkTBDLabel (i:int) : Id = mkLabel "TBD_" i
    fun mkBOTLabel (i:int) : Id = mkLabel "BOT_" i
    fun mkNextTyLabel () = (mkTyLabel (!Tystamp)) before Tystamp := !Tystamp + 1 
    fun getLabel ( a : AuxInfo ) : Id =
    let val { coverage = c, label = l, ... } = a
    in case l of
            NONE => (mkTyLabel (!Tystamp)) before Tystamp := !Tystamp + 1 
          | SOME id => id
    end

    fun getNextLabel () : Id = 
     (mkTyLabel (!Tystamp)) before Tystamp := !Tystamp + 1 
         
    fun getLabelString ( a : AuxInfo ) : string = Atom.toString (getLabel a)
    fun getIdString ( a : AuxInfo ) : string = 
	let val label = getLabelString a
	in
	  String.map Char.toLower label
	end

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
	   , len = 0.0
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
	   , len = 0.0 
           }
	end

    fun mkTyAux3 ( coverage : int, comp: TyComp ) : AuxInfo = 
	let val next = !Tystamp
            val () = Tystamp := !Tystamp + 1
            val label = mkTyLabel next
	in { coverage = coverage
           , label    = SOME label
           , tycomp   = comp
	   , len = 0.0
           }
	end

    (*function to get a Ty from a list of Tys by Id *)
    fun getTyById tylist id = 
	let val filtered = List.filter (fn ty => Atom.same ((getLabel (getAuxInfo ty)), id)) tylist
	in case filtered of
		nil => NONE
		| _ => SOME(hd filtered)
	end

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
    	       |(Blob x, Blob y) => (x = y)
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
        | LabelRef id   => "[Label] id = "^ Atom.toString(id) 
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
        val { tc = tcomp, adc = acomp, dc = dcomp} = #tycomp aux
	val l = #len aux
        val stats = ( "(" ^  (covToString aux)  ^
			", len: " ^ Real.fmt (StringCvt.FIX (SOME 1)) l ^	
		      (if print_complexity then (
                      ", tc: " ^ (showBits tcomp)  ^
                      ", ac: " ^ (showBits acomp)  ^
                      ", dc: " ^ (showBits dcomp) 
                      )
		      else "") ^
		      ", raw: "^ (showBits (combine tcomp (multCompR (!adcCoeff) acomp)))  ^
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
                    ) ^ " " ^ stats (* ^
		    (if print_complexity then 
			(" (avg: " ^ Real.fmt (StringCvt.FIX (SOME 2)) avg ^
                                      ", tot: " ^ LargeInt.toString tot ^ ")")
		    else "") 
		    *)
                 end
             | RefinedBase (aux, refined, tl) =>
                 let val avg = avgTokenLength tl
                     val tot = sumTokenLength tl
                 in ( refinedToString refined ) (* ^ (LTokensToString tl) *)
		      ^ " " ^ stats (* ^ 
		    (if print_complexity then 
			(" (avg: " ^ Real.fmt (StringCvt.FIX (SOME 2)) avg ^
                    	", tot: " ^ LargeInt.toString tot ^ ")") 
		     else "")
		    *)
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
		( case len of
                       SOME r  =>
                         prefix ^ "\tLens: "^ refinedToString(r) ^ "\n"
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
	let val containsNonStr = 
	  List.exists (fn re => 
		case re of 
		  StringConst _ => false
		| _ => true) relist 
	in
	  not containsNonStr
	end

    fun allWordConsts relist =
	let fun isWord s =
	   let val head = String.sub (s, 0)
	       val tail = String.explode (String.substring (s, 1, (size(s) - 1)))
	   in
	      (Char.isAlpha head) andalso not (List.exists 
 		(fn c => not (Char.isAlphaNum c orelse (c = #"_") orelse (c = #"-"))) tail)
	   end
	fun f res =
	    case res of
		nil => true
	    | (StringConst s)::res => isWord s andalso f res
	    | _ => false
	in
	  f relist
	end

(* Function to measure the variances of the structure by computing the total number of
  union/option/enum branches in the tree *) 
    fun variance ty =
	  case ty of
	   Base (a, _)           => 1.0
        |  TBD (a, _, _)            => 1.0
        |  Bottom (a, _, _)         => 1.0
        |  Pstruct (a, tys)      => foldl op+ 0.0 (map variance tys)
        |  Punion (a, tys)       => foldl op+ 0.0 (map variance tys)
        |  Parray (a, {tokens=t, lengths=len, first=f, body=b, last=l}) => 
		let val fv = variance f
		    val bv = variance b
		    val lv = variance l
		    val avglen = avgInts (#1 (ListPair.unzip len))
		    val bv' = bv * avglen 
		in
		    foldl Real.max 1.0 [fv, bv', lv]
		end
        |  RefinedBase (aux, re, l) => 
		(case re of Enum res => Real.fromInt (length res)
			| _ => 1.0
		)
        |  Switch (a, id, retys)  	 => foldl op+ 0.0 (map (fn (re, ty) => variance ty) retys)
        |  RArray (a,sep,term,body,len,lengths) => 
		let val bv = variance body
		    val avglen = avgInts (#1 (ListPair.unzip lengths))
		in
		    bv + 1.0 (* * avglen *)
		end
        |  Poption (a, body)     	 => 1.0 + (variance body)

(* Function to get the height of the type tree *)
  fun getHeight ty =
    let fun greater x y = x > y
    in
	  case ty of
	   Base (a, _)           => 1
        |  TBD (a, _, _)            => 1
        |  Bottom (a, _, _)         => 1
        |  Pstruct (a, tys)      => 1 + (max greater (map getHeight tys))
        |  Punion (a, tys)       => 1 + (max greater (map getHeight tys))
        |  Parray (a, {tokens=t, lengths=len, first=f, body=b, last=l}) => 
		let val fh = getHeight f
		    val bh = getHeight b
		    val lh = getHeight l
		in
		    1 + (max greater [fh, bh, lh])
		end
        |  RefinedBase (aux, re, l) => 1
        |  Switch (a, id, retys)  => 
		1 + (max greater (map (fn (re, ty) => getHeight ty) retys))
        |  RArray (a,sep,term,body,len,lengths) => 1 + getHeight body
        |  Poption (a, body)     	 => 1 + (getHeight body)
    end

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
	   Base (a, l)           => if isEmpty ty then 0 else #coverage a
        |  TBD (a, _, l)            => 0
        |  Bottom (a, _, l)         => 0
        |  Pstruct (a, tys)      => sumSmallInts (map getNumTokens tys)
        |  Punion (a, tys)       => sumSmallInts (map getNumTokens tys)
        |  Parray (a, {tokens=t, lengths=len, first=f, body=b, last=l}) => 
		getNumTokens f + getNumTokens b + getNumTokens l
        |  RefinedBase (aux, re, l) => #coverage aux
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
			else if len = 0 then (nil, nil, NONE, ty)
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

(* one tricky thing is that we have to skip options because it
   doesn't all the ltokens, therefore the minRecNo is unknown *)
fun getSmallestRecNo ty = 
  let 
	fun less x y = x < y
	fun notOption ty = case ty of
		  Poption _ => false
		| Parray _ => false
		| RArray _ => false
		| Base _ => not (isEmpty ty) (* we also have to excluse Pempty as the LineNo can be wrong *)
		| _ => true
  in
        case ty 
        of Base (a,t) => 
		let val recNos = map (fn (_, loc) => #recNo loc) t
		in min less recNos
		end
        |  Pstruct (a, tys) => 
	    let fun skip tys =
		case tys of
		  nil => raise AllOptions
		| Poption _::tys => skip tys
		| Parray _ :: tys => skip tys
		| RArray _ :: tys => skip tys
		| ty::tys => getSmallestRecNo ty
	    in skip tys
	    end	
        |  Punion (a,tys) => 
		let val nonOptions = List.filter notOption tys in
		  min less (map getSmallestRecNo nonOptions)
		end
        |  Parray (a, t)                => raise TyMismatch
        |  RefinedBase (a,r,tl)=> 
		let val recNos = map (fn (_, loc) => #recNo loc) tl
		in min less recNos
		end
        |  Switch(a,id,branches)        => 
		let val tys = (map #2 branches)
		    val nonOptions = List.filter notOption tys 
		in min less (map getSmallestRecNo nonOptions)
		end
        |  RArray (a,sep,term,body,len,lengths) => raise TyMismatch
        |  Poption (a, ty) => 0 (* this is a top level poption so recNo starts from 0 *) 
        |  _      => raise TyMismatch
  end

fun getLTokens ty =
  case ty of
    Base (a, t) => t
  | RefinedBase (a, r, tl) => tl
  | _ => raise TyMismatch

(* function to check if a given ty is an int *)
fun isInt ty =
  case ty of
    Base (a, (Pint _, _)::_) => true
  | RefinedBase (a, Int _ , _) => true
  | RefinedBase (a, IntConst _, _) => true
  | RefinedBase (a, Enum el, _) => not (List.exists (fn r => 
					case r of
					  Int _ => false
					| IntConst _ => false
					| _ => true
					) el)
  | _ => false

fun isFloat ty =
  case ty of
    Base (a, (Pfloat _, _)::_) => true
  | RefinedBase (a, FloatConst _, _) => true
  | RefinedBase (a, Enum el, _) => not (List.exists (fn r => 
					case r of
					  FloatConst _ => false
					| _ => true
				       ) el)
  | _ => false

(* function to check if a given ty is a dot *)
fun isDot ty =
  case ty of 
    Base(a1, (Other (#"."), _)::_) => true
  | RefinedBase (a1, StringConst ".", _) => true
  | _ => false  

(* function to test of a list of tys are mergeable *)
fun mergeable tys = 
  case tys of
    ty::tys =>
     ((isInt ty) andalso not(List.exists (fn t => not (isInt t)) tys))
     orelse
     ((isFloat ty) andalso not(List.exists (fn t => not (isFloat t)) tys))
  | nil => false   
end
