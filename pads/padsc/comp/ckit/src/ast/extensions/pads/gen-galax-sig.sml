(** generation of common PADS-Galax stuff **)

signature GENGALAX = sig
  val nameStr : string
  val parent : string
  val result : string
  val self : string
  val whatfn : string
  val childName : string
  val idx : string
  val kind : string

  val NEFName : string

  val makeNodeVtable : string -> ParseTree.externalDecl
  val makeCachedNodeVtable : string -> ParseTree.externalDecl
  val makeSNDNodeVtable : string -> ParseTree.externalDecl

  val makeCNInitFun : string * ParseTree.expression -> ParseTree.externalDecl
  val makeNodeNewFun : string -> ParseTree.externalDecl

 val makeCNKCFun : string * ParseTree.expression -> ParseTree.externalDecl
 val makeSNDInitFun : string -> ParseTree.externalDecl
 val makeStructSNDKthChildFun : string * (string * string * bool) list -> ParseTree.externalDecl
 val makeStructPathWalkFun : string * (string * string * bool) list -> ParseTree.externalDecl

  val makeArrSNDKthChildFun : string * string -> ParseTree.externalDecl
  val makeArrPathWalkFun : string * string -> ParseTree.externalDecl

  (* Internal *)

  val headerGalaxChildrenFun : string -> ParseTree.statement list
  val headerGalaxKthChildFun : string -> ParseTree.statement list
  val ifGalaxChildren : ParseTree.expression * ParseTree.expression * string
                        -> ParseTree.statement list
  val makeInvisibleDecls : string list * string list
                           -> ParseTree.statement list
  val makeKCCase : string
                   -> int * (string * string * bool) -> ParseTree.statement

  val apply : ('a -> 'b) list -> 'a -> 'b list
  val inc : int -> int
  val listOf : int -> int list
  val enumerate : 'a list -> (int * 'a) list
  val getUniqueTys : ('a * ''b * 'c) list -> ''b list

  val macroTNode : ParseTree.expression * string * string
                   * ParseTree.expression * string
                   -> ParseTree.statement list
  val macroTNodeCall : ParseTree.expression * ParseTree.expression * string
                       * string * ParseTree.expression * string
                       -> ParseTree.statement list
  val macroTNodeCallGeneral : ParseTree.expression * string * string
                              * ParseTree.expression * string
                              -> ParseTree.statement list
  val macroTNodeGeneral : ParseTree.expression * string * string
                          * ParseTree.expression * string
                          -> ParseTree.statement list

  val macroArrKC : string * string -> ParseTree.statement
  val macroArrKCN : string -> ParseTree.statement
  val macroArrKCNRet : unit -> ParseTree.expression
  val macroArrKCRet : unit -> ParseTree.expression
  val macroArrLength : string -> ParseTree.expression
  val macroCNInit : string * ParseTree.expression -> ParseTree.statement
  val macroCNInitRet : unit -> ParseTree.expression
  val macroKCCase : string * int * string * string -> ParseTree.statement
  val macroKCCaseComp : string * int * string * string -> ParseTree.statement
  val macroNodeCall : ParseTree.expression * ParseTree.expression * string
                      * ParseTree.expression * ParseTree.expression
                      * ParseTree.expression * ParseTree.expression * string
                      -> ParseTree.statement
  val macroNodeCallGeneral : ParseTree.expression * string
                             * ParseTree.expression * ParseTree.expression
                             * ParseTree.expression * ParseTree.expression
                             * string
                             -> ParseTree.statement
  val macroNodeNew : string -> ParseTree.statement
  val macroNodeNewRet : unit -> ParseTree.expression

  val macroStructKCBegin : string -> ParseTree.statement
  val macroStructKCEnd : unit -> ParseTree.statement
  val macroStructKCRet : unit -> ParseTree.expression

  val macroStructKCN : string * string list -> ParseTree.statement
  val macroStructKCNRet : unit -> ParseTree.expression
   
end