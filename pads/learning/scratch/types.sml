    type AuxInfo = {label : Id option, (* introduced during structure refinement *)
		    coverage:int} (* Coverage of 
				      -- a struct is minimum coverage of its constituents;
				      -- a union is sum of coverage of its consituents; *)

    type offset = {offset: int, span:int}

    datatype Token = Ptime of string | Pmonth of string | Pip of string | 
                     Pint of LargeInt.int | Pstring of string | 
                     Pgroup of {left : Token, body : LToken list, right : Token} | 
	             Pwhite of string | Other of char | Pempty | Error
    withtype LToken = Token * offset

    type Context = LToken list
    type DerivedContexts = Context list
				     
    datatype Ty = Base    of AuxInfo * Token list 
                | Pvoid   of AuxInfo
                | TBD     of AuxInfo * (* which tbd context*) int * Context list 
                | Bottom  of AuxInfo * (* which bottom context *) int * Context list 
                | Pstruct of AuxInfo * Ty list 
                | Punion  of AuxInfo * Ty list 
                | Parray  of AuxInfo * (Token list * int) list * Ty * Ty
                | RefinedBase of AuxInfo * Refined
                | Switch  of AuxInfo * Id * (Refined (* switch value *)* Ty) list
                | RArray of AuxInfo * Ty option (*sepatator*) * Ty option (* terminator *)
	                            * Ty (*Body type *) * Refined option (* length *) 

     datatype Refined = 
            StringME of String (* describe regular expression in pads syntax *) |
            Int of (int, int) |
            IntConst of int |
            StringConst of String |
            Enum of Refined list  |
            LabelRef of Id 

     type Id = Atom.atom
          

(* how to convert TBD/Bottom to a PADS description:
 - determine first of follow of type
 - analyze TBd/Bttom to make sure first is not in TBD/Bottom
 - make TBD/Bottom into string ended by first of follow.
 invariant (claimed) (and hoped)! first of follow can't appear in bottom/tbd context
  because of how context is formed in top down approach.

 string -> string(terminator)  
 - or - string_me(tokenizer regular expression)
Note: we need to make sure that the reg exp used to tokenize gets passed to the refinement piece 
      converting strings into pads types. 

 make token locations contain record numbers, convert tokens to located tokens
 make sure we can determine record associated with empty
 for every ty, we need to be able to determine the set of record numbers that it describes.

Kathleen: make sure arrays partition off any empty records first by introducing appropriate union
Kathleen: augment type data structure to keep all raw information
Kathleen: merge two array types
*)