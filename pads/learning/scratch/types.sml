structure Types =

struct
    open Config
    open Utils
    open Complexity
    open Tokens

    type TokenOrder = Token list
    type Context    = LToken list

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

    val numRefined      : LargeInt.int = 6 (* Number of cases in datatype Refined *)
    val numConstRefined : LargeInt.int = 2 (* Number of constant cases in datatype Refined *)

    datatype Refined = StringME of string
	             | Int of LargeInt.int * LargeInt.int  (* min, max *)
	             | IntConst of LargeInt.int    (* value *)
                     | StringConst of string (* string literal *)
                     | Enum of Refined list  
                     | LabelRef of Id     (* for synthetic nodes: lengths, branch tags*)

    val numTy : LargeInt.int = 9 (* Number of constructors in datatype Ty *)
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
                         NONE => 1 (***** For now !!!!!!*)
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
            NONE => (mkTyLabel (!Tystamp)) before Tystamp := !Tystamp 
          | SOME id => id
    end

    fun getLabelString ( a : AuxInfo ) : string = Atom.toString (getLabel a)

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

    fun getCoverage ( ty : Ty ) : int = #coverage ( getAuxInfo ty )
    fun sumCoverage ( tys : Ty list ) : int =
        foldl ( fn (t:Ty,n:int) => getCoverage t + n ) 0 tys
    fun minCoverage ( tys : Ty list ) : int = 
	case tys of [] => Option.valOf Int.maxInt
        | (ty::tys) => Int.min(getCoverage ty, minCoverage tys)

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
        |  PbXML (f,s) => "<"^f^s^">"
        |  PeXML (f,s) => "</"^f^s^">"
	|  Pint (i, s)      =>
             if i < 0 then "-"^(LargeInt.toString (~i)) else LargeInt.toString i
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
        val stats = ( "(" ^  (covToString aux) ^
                      ", tc: " ^ (showBits tcomp)  ^
                      ", ac: " ^ (showBits acomp)  ^
                      ", dc: " ^ (showBits dcomp)  ^ ")"
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
                    ) ^ " " ^ stats ^ " (avg: " ^ Real.fmt (StringCvt.FIX (SOME 2)) avg ^
                                      ", tot: " ^ LargeInt.toString tot ^ ")"
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
                    " (avg: " ^ Real.fmt (StringCvt.FIX (SOME 2)) avg ^
                    ", tot: " ^ LargeInt.toString tot ^ ")"
                 end
             | Switch(aux ,id, retys) =>
                "Switch(" ^ Atom.toString(id)^")" ^ stats ^ ":\n" ^
                (lconcat (List.map (fn (re, ty) => (prefix^"case "^(refinedToString re)^":\n"^ 
                (partialD ty))) retys)) ^ prefix ^ "End Switch"
             | RArray (aux, sep, term, body, len, _) => 
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

end
