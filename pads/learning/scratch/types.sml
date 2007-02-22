structure Types =

struct
    open Config
    open Utils
    type location = {lineNo: int, beginloc: int, endloc:int}

    datatype Token = PbXML of string * string |
                     PeXML of string * string |
	             Ptime of string | 
		     Pmonth of string | 
		     Pip of string | 
                     Pint of LargeInt.int | 
		     Pstring of string | 
                     Pgroup of {left : LToken, body : LToken list, right : LToken} | 
	             Pwhite of string | 
		     Other of char | 
		     Pempty | 
		     Error
    withtype LToken = Token * location

    type TokenOrder = Token list
    type Context = LToken list
    type DerivedContexts = Context list
    type Partition = (TokenOrder * (DerivedContexts list)) list * (Context list)

    val Tystamp = ref 0  (* used to give unique ids to nodes in type trees *)
    type Id = Atom.atom			     
    type AuxInfo = {coverage:int, (* Coverage of 
				      -- a struct is minimum coverage of its constituents;
				      -- a union is sum of coverage of its consituents; *)
                    label : Id option (* introduced during refinement as a tag *)}


    datatype Refined = StringME of string (* describe regular expression in pads syntax *) 
	             | Int of LargeInt.int * LargeInt.int  (* min, max *)
	             | IntConst of LargeInt.int    (* value *)
                     | StringConst of string (* string literal *)
                     | Enum of Refined list  
                     | LabelRef of Id     (* for synthetic nodes: lengths, branch tags*)

    datatype Ty = Base    of AuxInfo * LToken list (* list will never be empty *)
                | TBD     of AuxInfo * int * Context list 
                | Bottom  of AuxInfo * int * Context list 
                | Pstruct of AuxInfo * Ty list 
                | Punion  of AuxInfo * Ty list 
                | Parray  of AuxInfo * {tokens:(Token * int) list,
					lengths: (int * int) list, (* list of array (lengths,linenumbers) *)
					first : Ty,
					body  : Ty,
					last  : Ty}

                | RefinedBase of AuxInfo * Refined * LToken list
                | Switch  of AuxInfo * Id * (Refined (* switch value *)* Ty) list
                | RArray of AuxInfo * Refined option (*sepatator*) * Refined option (* terminator *)
	                            * Ty (*Body type *) * Refined option (* length *) 


    fun getAuxInfo ty : AuxInfo = 
	case ty 
        of Base (a,t) => a
        |  TBD (a,i,cl) => a
        |  Bottom (a,i,cl) => a
        |  Pstruct (a,tys) => a
        |  Punion (a,tys) => a
        |  Parray (a, _) => a
        |  RefinedBase (a,r,tl) => a
        |  Switch(a,id,branches) =>a
        |  RArray (a,sep,term,body,len) => a

    fun mkLabel prefix i = Atom.atom("BTy_"^(Int.toString i))
    fun mkTyLabel i = mkLabel "BTy_" i
    fun mkTBDLabel i = mkLabel "TBD_" i
    fun mkBOTLabel i = mkLabel "BOT_" i

    fun getLabel {coverage, label} = 
	case label of NONE => (mkTyLabel (!Tystamp)) before Tystamp := !Tystamp 
        | SOME id => id
    fun getLabelString aux = Atom.toString (getLabel aux)

    fun mkTyAux coverage = 
	let val next = !Tystamp
            val () = Tystamp := !Tystamp + 1
            val label = mkTyLabel next
	in
	  {coverage = coverage, label = SOME label}
	end

    fun getCoverage ty = #coverage(getAuxInfo ty)
    fun sumCoverage tys = 
	case tys of [] => 0
        | (ty::tys) => (getCoverage ty) + (sumCoverage tys)
    fun minCoverage tys = 
	case tys of [] => Option.valOf Int.maxInt
        | (ty::tys) => Int.min(getCoverage ty, minCoverage tys)

    fun ltokenToString (t,loc) = tokenToString t
    and tokenToString t = 
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
    and refinedToString re =
	case re
	of StringME s => "\""^ s ^ "\""
        | Int(min, max) => "["^LargeInt.toString(min)^", "^LargeInt.toString(max)^"]"
        | IntConst a => "["^ LargeInt.toString(a) ^"]" 
        | StringConst s => "\""^s^"\"" 
        | Enum rel => "{" ^ String.concat(map (fn x => (refinedToString x) ^", ") rel) ^ "}"
        | LabelRef id => "id="^ Atom.toString(id)    

    fun ltokenTyToString (t,loc) = tokenTyToString t
    and tokenTyToString t = 
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


    fun printTokenTy t = print (tokenTyToString t)

    fun LTokensToString [] = "\n"
      | LTokensToString ((t,loc)::ts) = ((tokenToString t) ^ (LTokensToString ts))

    fun locationToString {lineNo, beginloc, endloc} = "Line #:"^(Int.toString lineNo)
    fun printLocation loc = print (locationToString loc)

    fun printLTokens [] = print "\n"
      | printLTokens ((t,loc)::ts) = (printLocation loc; print ":\t"; printTokenTy t; printLTokens ts)

    fun printTokenTys [] = print "\n"
      | printTokenTys (t::ts) = (printTokenTy t; printTokenTys ts)


   fun covToString {coverage, label} = 
       let val label = 
	   if !printIDs then 
	       case label 
               of NONE => ""
               | SOME id => ("Id = "^(Atom.toString id)^" ")
	   else ""
       in
	   label ^ Int.toString coverage
       end


    fun contextsToString contexts = 
	((case contexts 
	  of [] => "<no records matched context>\n"
	  | _ => (lconcat(
		  List.map (fn tl => 
			    (case tl 
			     of [] => "\t<empty>\n"
			     | _ => ("\t"^( LTokensToString tl) ^"\n"))) contexts))))


    (* Replace when debugged with print (contextsToString contexts) *)
    fun printContexts contexts = 
	((case contexts 
	  of [] => print "<no records matched context>\n"
	  | _ => (List.app (fn tl => 
			    (case tl 
			     of [] => print "\t<empty>\n"
			     | _ => (print "\t"; printLTokens tl; print "\n"))) contexts)))




   fun TyToStringD prefix longTBDs longBottom suffix ty = 
       (prefix^
        (case ty 
         of Base (aux, t)  => (ltokenTyToString (hd t))^("(" ^(covToString aux)^")") 
         |  TBD (aux,i,cl) => "TBD_"^(Int.toString i)^
	                      "("^(covToString aux)^")"^
		              (if longTBDs then
			          ("\n"^(contextsToString cl)^prefix^"End TBD")
		               else "")
         |  Bottom (aux,i, cl) => "BTM_"^(Int.toString i)^
	                      "("^(covToString aux)^")"^
			      (if longBottom then
				   ("\n"^(contextsToString cl)^prefix^"End Bottom")
			       else "")
         |  Pstruct (aux, tys) =>  "Pstruct("^(covToString aux)^")\n"^
	 		    (lconcat (List.map (TyToStringD (prefix^"\t") longTBDs longBottom (";\n")) tys))^
			    prefix ^ "End Pstruct"
         |  Punion (aux, tys)  => "Punion("^(covToString aux)^")\n"^
	 		    (lconcat (List.map (TyToStringD (prefix^"\t") longTBDs longBottom (";\n")) tys))^
			    prefix ^ "End Punion"
         |  Parray (aux, {tokens=tkns, lengths, first=ty1,body=ty2,last=ty3})  => "Parray("^(covToString aux)^")"^
			    "("^(lconcat(List.map (fn (t,loc) => (tokenTyToString t) ^" ")tkns)) ^")\n"^
			    prefix ^ "First:\n"^
                            (TyToStringD (prefix^"\t") longTBDs longBottom (";\n") ty1)^
			    prefix ^ "Body:\n"^
                            (TyToStringD (prefix^"\t") longTBDs longBottom (";\n") ty2)^
			    prefix^"Tail:\n"^
                            (TyToStringD (prefix^"\t") longTBDs longBottom (";\n") ty3)^
			    prefix ^ "End Parray"
        |  RefinedBase (aux, refined, tl) => (refinedToString refined)^("(" ^(covToString aux)^")") 
        |  Switch(aux ,id, retys) => "Switch("^Atom.toString(id)^"):\n"^
	 		    (lconcat (List.map (fn (re, ty) => (prefix^"\t case "^(refinedToString re)^": "^ 
			    (TyToStringD prefix longTBDs longBottom (";\n") ty))) retys))^
			    prefix ^ "End Switch"
        |  RArray (aux, sep, term, body, len) => "RArray("^(covToString aux)^")\n"^
			    (case sep of SOME septok =>
			    prefix ^ "Separator: "^
                            refinedToString(septok)^"\n"
			    | _ => ""
			    )^
			    (case term of SOME termtok =>
			    prefix ^ "Terminator: "^
                            refinedToString(termtok)^"\n"
			    | _ => ""
			    )^
			    prefix ^ "Body:\n"^
                            (TyToStringD (prefix^"\t") longTBDs longBottom (";\n") body) ^ 
			    prefix ^ "End RArray"
        )^
	suffix)
       
    fun TyToString ty = TyToStringD "" false false "" ty

    fun printTyD prefix longTBDs longBottom suffix ty =  print (TyToStringD prefix longTBDs longBottom suffix ty )
    fun printTy ty = printTyD "" false false "\n" ty

end
