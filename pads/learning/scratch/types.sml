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
    type AuxInfo = { coverage : int        (* Coverage of
                                                 -- a struct is minimum coverage
                                                 --      of its constituents;
                                                 -- a union is sum of coverage
                                                         of its consituents; *)
                   , label    : Id option  (* introduced during refinement as a tag *)
                   , typeComp : Complexity (* Inherent complexity of the type *)
                   , dataComp : Complexity (* Average complexity of data given type *)
                   }

    (* Update the type and data complexity of an AuxInfo *)
    fun updateComps (a : AuxInfo) (t : Complexity) (d : Complexity) : AuxInfo =
        { coverage = #coverage a
        , label    = #label a
        , typeComp = t
        , dataComp = d
        }

    datatype Refined = StringME of string
	             | Int of LargeInt.int * LargeInt.int  (* min, max *)
	             | IntConst of LargeInt.int    (* value *)
                     | StringConst of string (* string literal *)
                     | Enum of Refined list  
                     | LabelRef of Id     (* for synthetic nodes: lengths, branch tags*)

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
    fun getTypeComp ( ty : Ty ) : Complexity = #typeComp (getAuxInfo ty)
    (* Retrieve computed data complexity from a type *)
    fun getDataComp ( ty : Ty ) : Complexity = #dataComp (getAuxInfo ty)
    (* Retrieve both complexities from a measured type *)
    fun getComps ( ty : Ty ) : Complexity * Complexity =
        (getTypeComp ty, getDataComp ty)

    (* Sum the type complexities of a measured type *)
    fun sumTypeComps ( tys : Ty list ) : Complexity =
        foldl ( fn (t,c) => combine (getTypeComp t) c ) zeroComplexity tys

    (* Sum the data complexities of a measured type *)
    fun sumDataComps ( tys : Ty list ) : Complexity =
        foldl ( fn (t,c) => combine (getDataComp t) c ) zeroComplexity tys
                 
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
           , label = SOME label
           , typeComp = zeroComplexity
           , dataComp = zeroComplexity
           }
	end

    fun mkTyAux1 ( coverage : int, id : Id ) : AuxInfo = 
	let val next = !Tystamp
            val () = Tystamp := !Tystamp + 1
            val label = id
	in { coverage = coverage
           , label = SOME label
           , typeComp = zeroComplexity
           , dataComp = zeroComplexity
           }
	end

    fun getCoverage ( ty : Ty ) : int = #coverage(getAuxInfo ty)
    fun sumCoverage ( tys : Ty list ) : int = 
	case tys of [] => 0
        | (ty::tys) => (getCoverage ty) + (sumCoverage tys)
    fun minCoverage ( tys : Ty list ) : int = 
	case tys of [] => Option.valOf Int.maxInt
        | (ty::tys) => Int.min(getCoverage ty, minCoverage tys)

    fun ltokenToString ( t : Token, loc : location ) : string = tokenToString t
    and tokenToString ( t : Token ) : string = 
	case t 
        of Ptime i => i
	|  Pip i  => i
        |  Pmonth m => m
        |  PbXML (f,s) => "<"^f^s^">"
        |  PeXML (f,s) => "</"^f^s^">"
	|  Pint i => if i < 0 then "-"^(LargeInt.toString (~i)) else LargeInt.toString i
        |  Pstring s => s
        |  Pgroup {left, body, right} => (ltokenToString left)^(String.concat (List.map ltokenToString body))^(ltokenToString right)
        |  Pwhite s => s
        |  Other c => String.implode [c]
        |  Pempty => ""
        |  Error => " Error"
    and refinedToString ( re : Refined ) : string =
	case re
	of StringME s => "\""^ s ^ "\""
        | Int(min, max) => "["^LargeInt.toString(min)^"..."^LargeInt.toString(max)^"]"
        | IntConst a => "["^ LargeInt.toString(a) ^"]" 
        | StringConst s => "\""^s^"\"" 
        | Enum rel => "{" ^ String.concat(map (fn x => (refinedToString x) ^", ") rel) ^ "}"
        | LabelRef id => "id="^ Atom.toString(id)    

    fun ltokenTyToString ( t : Token, loc : location ) : string = tokenTyToString t
    and tokenTyToString ( t : Token ) : string = 
	case t 
        of Ptime i   => "[Time]"
	|  Pip i     => "[IP]"
        |  Pmonth m  => "[Month]"
        |  PbXML (f,s) => "bXML["^f^"]"
        |  PeXML (f,s) => "eXML["^f^"]"
(*
	|  Pint i    => " Pint("^(LargeInt.toString i)^")"
        |  Pstring s => " Pstring("^s^")"
        |  Pwhite s  => " Pwhite("^s^")" 
*)
	|  Pint i    => "[int]"                   (*" Pint("^(LargeInt.toString i)^")"*)
        |  Pstring s => "[string]"                (*" Pstring("^s^")"*)
        |  Pwhite s  => "[white space]"           (*" Pwhite("^s^")"*) 
        |  Pgroup {left, body, right} => (ltokenTyToString left) ^"[Group Body]"^(ltokenTyToString right)
        |  Other c   => "("^(Char.toString c)^")" (*(" Pother("^(Char.toString c)^")") *)
        |  Pempty    => "[empty]"
        |  Error     => " Error"


    fun printTokenTy ( t : Token ) : unit = print (tokenTyToString t)

    fun LTokensToString [] = "\n"
      | LTokensToString ((t,loc)::ts) = ((tokenToString t) ^ (LTokensToString ts))

    fun locationToString {lineNo, beginloc, endloc} = "Line #:"^(Int.toString lineNo)
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
        val tcomp = #typeComp aux
        val dcomp = #dataComp aux
        val stats = ( "(" ^  (covToString aux) ^
                      ", " ^ (showBits tcomp)  ^
                      ", " ^ (showBits dcomp)  ^ ")"
                    )
        val partialD = TyToStringD (prefix^"\t") longTBDs longBottom (";\n")
    in ( prefix ^
         ( case ty of
               Base (aux, t)  => (case t of nil => "[NULL]"
					| _ => ( ltokenTyToString (hd t) )) ^ stats 
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
             | RefinedBase (aux, refined, tl) => (refinedToString refined) ^ stats 
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
         ) ^
         suffix )
     end 

     fun TyToString (ty:Ty):string = TyToStringD "" false false "" ty

     fun printTyD (prefix:string) (longTBDs:bool) (longBottom:bool)
                  (suffix:string) (ty:Ty) : unit =
         print (TyToStringD prefix longTBDs longBottom suffix ty ) 

     fun printTy ( ty : Ty ) : unit = printTyD "" false false "\n" ty

end
