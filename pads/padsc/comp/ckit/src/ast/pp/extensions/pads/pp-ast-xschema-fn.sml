
functor PPAstXschemaFn (structure PPAstPaidAdornment : PPASTPAIDADORNMENT) : PP_XML_AST = struct 

  structure Tid = Tid
  structure Pid = Pid
  structure PP = OldPrettyPrint
  structure B = Bindings
  structure PPAA = PPAstPaidAdornment
  structure PPAE = PPAstExtFn (type aidinfo = PPAstPaidAdornment.aidinfo);
  structure PPL = PPLib

  open PPLib
  open Ast

  type aidinfo = PPAE.aidinfo
  type paidinfo = PPAstPaidAdornment.paidinfo
  type ptyinfo = PTys.pTyInfo

  val printLocation = false (* internal flag - pretty print locations as comments *)

  fun ppLoc pps (SourceMap.LOC {srcFile, beginLine, beginCol, endLine, endCol}) =
      if printLocation then ( PPL.addStr pps " /*["
			    ; PPL.addStr pps (srcFile)
			    ; PPL.addStr pps ":"
			    ; PPL.addStr pps (Int.toString beginLine)
			    ; PPL.addStr pps "]*/ "
			    )
      else ()
    | ppLoc pps _ = ()

  val warning = PPL.warning

  val ppLParen = PPL.ppGuarded "("

  val ppRParen = PPL.ppGuarded ")"

  fun getCtype ({stClass,ctype,...}: Ast.id) = (stClass,ctype)

  fun isPostFix PostInc = true
    | isPostFix PostDec = true
    | isPostFix _ = false

  fun ppBinop aidinfo tidtab pps binop = 
      case binop
	of Plus   => PPL.addStr pps "+"
         | Minus  => PPL.addStr pps "-"
	 | Times  => PPL.addStr pps "*"
	 | Divide => PPL.addStr pps "/"
	 | Mod    => PPL.addStr pps "%"
	 | Gt     => PPL.addStr pps ">"
	 | Lt     => PPL.addStr pps "<"
	 | Gte    => PPL.addStr pps ">="
	 | Lte    => PPL.addStr pps "<="
	 | Eq     => PPL.addStr pps "=="
	 | Neq    => PPL.addStr pps "!="
	 | And    => PPL.addStr pps "&&"
	 | Or     => PPL.addStr pps "||"
	 | BitOr  => PPL.addStr pps "|"
	 | BitAnd => PPL.addStr pps "&"
	 | BitXor => PPL.addStr pps "^"
	 | Lshift => PPL.addStr pps "<<"
	 | Rshift => PPL.addStr pps ">>"
	 | PlusAssign   => PPL.addStr pps "+="
	 | MinusAssign  => PPL.addStr pps "-="
	 | TimesAssign  => PPL.addStr pps "*="
	 | DivAssign    => PPL.addStr pps "/="
	 | ModAssign    => PPL.addStr pps "%="
	 | XorAssign    => PPL.addStr pps "^="
	 | OrAssign     => PPL.addStr pps "|="
	 | AndAssign    => PPL.addStr pps "&="
	 | LshiftAssign => PPL.addStr pps "<<="
	 | RshiftAssign => PPL.addStr pps ">>="
	 | BinopExt be => PPAE.ppBinopExt aidinfo tidtab pps be

  fun ppUnop aidinfo tidtab pps unop = 
      case unop
	of Uplus   => PPL.addStr pps "+"
         | Not     => PPL.addStr pps "!"
	 | Negate  => PPL.addStr pps "-"
	 | BitNot  => PPL.addStr pps "~"
	 | PreInc  => PPL.addStr pps "++"
	 | PostInc => PPL.addStr pps "++"
	 | PreDec  => PPL.addStr pps "--"
	 | PostDec => PPL.addStr pps "--"
	 | UnopExt ue => PPAE.ppUnopExt aidinfo tidtab pps ue

  datatype Identifier
    = ID of Ast.id
    | MEMBER of Ast.member
    | TID of Tid.uid

  datatype params = 
      EMPTY
    | ANSI of Ast.id list
    | KNR of Ast.id list

  datatype ctStkItem
    = Arr of (LargeInt.int * Ast.expression) option
    | Qua of Ast.qualifier
    | Fun of Ast.ctype list * params
    | Ptr 


  val printConst = ref true (* PADS *)

  fun ppIdentifier tidtab pps =
      fn (ID id) => ppId pps id
       | (MEMBER member) => ppMember pps member
       | (TID tid) => ppTid tidtab pps tid

  fun ppQualifier pps qf = 
    let val s = case qf
	          of CONST => if !printConst then "const " else ""
		   | VOLATILE => "volatile "
    in addStr pps s end

  fun ppStorageClass pps sc = 
    let val s = case sc
	          of STATIC => "static "
		   | EXTERN => "extern "
		   | REGISTER => "register "
		   | AUTO => ""
		   | DEFAULT => ""
    in addStr pps s end

      
  fun ppSignedness pps sign = 
    let val s = case sign
	          of SIGNED   => ""
		   | UNSIGNED => "unsigned "
    in addStr pps s end

  fun ppFractionality pps frac = 
    let val s = case frac 
	          of FRACTIONAL  => "fractional "
		   | WHOLENUM => ""
    in addStr pps s end

  fun ppSaturatedness pps sat = 
    let val s = case sat
	          of SATURATE   => "saturate "
		   | NONSATURATE => ""
    in addStr pps s end

  fun ppIntKind pps ik = 
    let val s = case ik
	          of CHAR   => "char"
		   | SHORT  => "short"
		   | INT    => "int"
		   | LONG   => "long"
		   | LONGLONG   => "long long"
		   | FLOAT  => "float"
		   | DOUBLE => "double"
		   | LONGDOUBLE => "long double"
    in addStr pps s end

  fun ppStk aidinfo tidtab pps (idOpt,stk) =
        let fun loop (prev,[]) = ppOpt (ppIdentifier tidtab) pps idOpt
	      | loop (prev,(Qua qf)::l) =
		  (ppQualifier pps qf
		  ;loop (prev,l)
		  )
	      | loop (prev,(a as Arr opt)::l) =
		  (loop (a,l)
		  ;addStr pps "["
		  ;(case opt of
		      SOME(i, expr) => ppExpr {nested=false} aidinfo tidtab pps expr
		    | NONE => ())
		  ;addStr pps "]"
		  )
	      | loop (prev, ((f as Fun (cts,idsOpt))::l)) =
		 (loop (f,l)
		 ;space pps
		 ;case idsOpt 
		    of EMPTY =>
		         ppList {pp=ppCtype aidinfo tidtab
				,sep=","
				,lDelim="("
				,rDelim=")"
				} pps cts
		     | ANSI ids => 
			 ppList {pp=ppIdDecl aidinfo tidtab
				,sep=","
				,lDelim="("
				,rDelim=")"
				} pps ids
		     | KNR ids => 
			 ppList {pp=ppId
				,sep=","
				,lDelim="("
				,rDelim=")"
				} pps ids
		 )
	      | loop (Ptr,p::l) =
		  (addStr pps "*"
		  ;loop (Ptr,l)
		  )
	      | loop (_,Ptr::l) =
		  (addStr pps "("
		  ;addStr pps "*"
		  ;loop (Ptr,l)
		  ;addStr pps ")"
		  )
	in loop (Ptr,stk) end

  and ppSpStk aidinfo tidtab pps (pair as (NONE,[])) = ppStk aidinfo tidtab pps pair
    | ppSpStk aidinfo tidtab pps (pair as (_,stk)) = (space pps; ppStk aidinfo tidtab pps pair)

  and ppDecl0 aidinfo tidtab pps (idOpt,idsOpt,ctype) =
    let fun loop (idsOpt,ctype,stk) = 
	  case ctype
	    of Void =>
		 (addStr pps "void"
		 ;ppSpStk aidinfo tidtab pps (idOpt,stk)
		 )
	     | Ellipses => 
		 (case stk
		    of [] => addStr pps "..."
		     | _  => (warning
			        "ppDecl"
				"ill-formed ellipses type"
			     ;addStr pps "..."
			     ))
	     | Qual (qf,ct) =>
		   loop (idsOpt,ct,(Qua qf)::stk)
	     | Numeric (NONSATURATE,WHOLENUM, _, CHAR, SIGNASSUMED)  => 
		 (addStr pps "char"
		 ;ppSpStk aidinfo tidtab pps (idOpt,stk)
		 ) 
	     | Numeric (NONSATURATE,WHOLENUM, SIGNED, CHAR, SIGNDECLARED)  => 
		 (addStr pps "signed char"
		 ;ppSpStk aidinfo tidtab pps (idOpt,stk)
		 ) 
	     | Numeric (NONSATURATE,WHOLENUM, UNSIGNED, CHAR, SIGNDECLARED)  => 
		 (addStr pps "unsigned char"
		 ;ppSpStk aidinfo tidtab pps (idOpt,stk)
		 ) 
	     | Numeric (sat, frac, sign, ik, _) =>
		 (ppSaturatedness pps sat
		 ;ppFractionality pps frac
		 ;ppSignedness pps sign
		 ;ppIntKind pps ik
		 ;ppSpStk aidinfo tidtab pps (idOpt,stk)
		 )
	     | Array (opt,ct) => loop (idsOpt,ct,Arr opt::stk)
	     | Pointer ct =>  loop (idsOpt,ct,Ptr::stk)
	     | Function (ct,cts) => loop (EMPTY,ct,Fun (cts,idsOpt)::stk)
	     | EnumRef tid => 
		 (case Tidtab.find (tidtab,tid)
		    of SOME {ntype=SOME (B.Enum _),...} => 
			(addStr pps "enum "
			;ppTid tidtab pps tid
			)
		     | _ => (* print out partially defined enums *)
			(addStr pps "enum "
			;ppTid tidtab pps tid
			)
			(* addStr pps ("EnumRef(" ^ (Tid.toString tid) ^ ")") *)
		 ;ppSpStk aidinfo tidtab pps (idOpt,stk)
		 )	 
	     | StructRef tid =>
		 (addStr pps "struct "
		 ;ppTid tidtab pps tid
		 ;ppSpStk aidinfo tidtab pps (idOpt,stk))
	     | UnionRef tid =>
		 (addStr pps "union "
		 ;ppTid tidtab pps tid
		 ;ppSpStk aidinfo tidtab pps (idOpt,stk))
	     | TypeRef tid => 
		 (case Tidtab.find (tidtab,tid)
		    of SOME {ntype=SOME (B.Typedef _),...} => ppTid tidtab pps tid
		     | _ => addStr pps ("TypeRef(" ^ (Tid.toString tid) ^ ")")
		 ;ppSpStk aidinfo tidtab pps (idOpt,stk)
		 )
	     | Error => (addStr pps "/* ErrorType */ "
			;ppSpStk aidinfo tidtab pps (idOpt,stk))
    in loop (idsOpt,ctype,[]) end

  and ppCtype aidinfo tidtab pps ctype = ppDecl0 aidinfo tidtab pps (NONE,EMPTY,ctype)
      
  (* Note: id is only used for printing purposes.
   All information needed to interpret a type is obtained via tid *)
  and ppNamedCtype aidinfo tidtab pps nct = 
    let fun ppOptList ppElt sep [] = ()
	  | ppOptList ppElt sep l = 
	    (addStr pps "{"
	    ;blockify 2 (separate (ppElt,fn pps => (addStr pps sep; newline pps))) pps l
	    ;newline pps
	    ;addStr pps "}"
	    )
	fun pseparate (ppElt, sep, ppTrail) pps [] = ()
          | pseparate (ppElt, sep, ppTrail) pps [x] = (ppElt pps x; ppTrail pps x)
	  | pseparate (ppElt, sep, ppTrail) pps (x::xs) = 
              (ppElt pps x; sep pps; ppTrail pps x; pseparate(ppElt,sep,ppTrail) pps xs)
	fun ppOptList2 ppElt sep ppTrail [] = ()
	  | ppOptList2 ppElt sep ppTrail l = 
	    (addStr pps "{"
	    ;blockify 2 (pseparate (ppElt,fn pps => (addStr pps sep), 
				    fn pps => (fn x => (ppTrail pps x; newline pps)))) pps l
	    ;addStr pps "}"
	    )
    in case nct
	 of B.Struct (tid,members) =>
	      let fun ppLI' pps li = (addStr pps ":"; ppLI pps li)

		  fun ppMember pps (ct, memberOpt, LIOpt, strOpt) = (* strOpt is PADS *)
		    (ppDecl0 aidinfo tidtab pps (Option.map MEMBER memberOpt,EMPTY,ct)
		    ;ppOpt ppLI' pps LIOpt
		    ;addStr pps ";"
		    ;(case strOpt of SOME s => addStr pps ("\t\t/* "^ s ^" */")
			          | NONE => () (* PADS *))
		    )

	      in (addStr pps "struct "
		 ;ppTid tidtab pps tid
		 ;space pps
		 ;ppOptList ppMember "" members)
	      end
	  | B.Union (tid,members) =>
	      let 
		  fun ppMember pps (ct, member,strOpt) =
		    (ppDecl0 aidinfo tidtab pps (SOME(MEMBER member),EMPTY,ct)
		    ;addStr pps ";"
		    ;(case strOpt of SOME s => addStr pps ("\t\t/* "^ s ^" */")
			          | NONE => () (* PADS *))
		    )
	      in addStr pps "union "
		;ppTid tidtab pps tid
		;space pps
	        ;ppOptList ppMember "" members
	      end
          | B.Enum (tid,members) =>
	        let fun ppMemberInt pps (member,li,commentOpt) =  (* commentOpt is PADS *)
		      (ppMember pps member
		      ;addStr pps "="
		      ;ppLI pps li
		      )
		    fun ppTrail pps (member,li,commentOpt) = 
			(case commentOpt of SOME s => addStr pps ("\t\t/* "^ s ^" */")
			          | NONE => () (* PADS *))
		in (addStr pps "enum "
		   ;ppTid tidtab pps tid
		   ;space pps
		   ;ppOptList2 ppMemberInt "," ppTrail members
		   )
		end
	  | B.Typedef (tid,ctype) =>
	      (addStr pps "typedef "
	      ;ppDecl0 aidinfo tidtab pps (SOME (TID tid),EMPTY,ctype)
	      )
    end 


  and ppDecl aidinfo tidtab pps (id,ct) = ppDecl0 aidinfo tidtab pps (SOME (ID id),EMPTY,ct)

  and ppDeclaration aidinfo tidtab pps (TypeDecl{shadow=NONE, tid}) = 
       (case Tidtab.find (tidtab,tid)
	  of SOME {ntype=SOME nct,location,...} => 
	       (ppLoc pps location; ppNamedCtype aidinfo tidtab pps nct)
	   | _ => (warning
		     "ppCoreStmt" 
		     ("No type associated with tid:"^(Tid.toString tid));
		   PPL.addStr pps "...");
        PPL.addStr pps ";")
    | ppDeclaration aidinfo tidtab pps (TypeDecl{shadow=SOME{strct=true}, tid}) = 
	  (PPLib.addStr pps "struct "
	  ;PPLib.ppTid tidtab pps tid
	  ;PPL.addStr pps ";")
    | ppDeclaration aidinfo tidtab pps (TypeDecl{shadow=SOME{strct=false}, tid}) = 
	  (PPLib.addStr pps "union "
	  ;PPLib.ppTid tidtab pps tid
	  ;PPL.addStr pps ";")
    | ppDeclaration aidinfo tidtab pps (VarDecl (id as {location,...}, initOpt)) = 
       (ppLoc pps location
       ;ppIdDecl aidinfo tidtab pps id
       ;case initOpt
	  of SOME initExpr => 
	      (PPL.addStr pps "=";
	       ppInitExpression aidinfo tidtab pps initExpr)
	   | NONE => ()
       ;PPL.addStr pps ";")

  and ppIdDecl aidinfo tidtab pps (id: Ast.id) =
    let val (stClass,ctype) = getCtype id
    in (ppStorageClass pps stClass
       ;ppDecl aidinfo tidtab pps (id,ctype)
       )
    end

  and blockStmt aidinfo tidtab pps stmt = PPL.blockify 2 (ppStmt aidinfo tidtab) pps stmt

  and ppStmt aidinfo tidtab pps (stmt as (STMT (_,_,loc))) = 
      ( ppLoc pps loc
      ; PPAA.ppStatementAdornment ppCoreStmt aidinfo tidtab pps stmt
      )

  and ppCoreStmt aidinfo tidtab pps coreStmt = 
    case coreStmt
      of Expr expOpt => 
	   ( PPL.ppOpt (ppExpr {nested=false} aidinfo tidtab) pps expOpt
	   ; PPL.addStr pps ";"
	   )
       | Compound (decls,stmts) => 
	   ( PPL.addStr pps "{"
	   ; (case decls of 
		nil => ()
	      | _ => PPL.blockify 2 (PPL.separate (ppDeclaration aidinfo tidtab, PPL.newline)) pps decls)
	   ; (case stmts of
		nil => ()
	      | _ => PPL.blockify 2 (PPL.separate (ppStmt aidinfo tidtab, PPL.newline)) pps stmts)
	   ; PPL.newline pps
	   ; PPL.addStr pps "}"
	   )
       | While (exp,stmt) => 
	   ( PPL.addStr pps "while ("
	   ; ppExpr {nested=false} aidinfo tidtab pps exp
	   ; PPL.addStr pps ")"
	   ; blockStmt aidinfo tidtab pps stmt
	   )
       | Do (exp,stmt) => 
	   ( PPL.addStr pps "do"
	   ; blockStmt aidinfo tidtab pps stmt
	   ; PPL.newline pps
	   ; PPL.addStr pps "while ("
	   ; ppExpr {nested=false} aidinfo tidtab pps exp
	   ; PPL.addStr pps ");"
	   )
       | For (expOpt0,expOpt1,expOpt2,stmt) =>
	   ( PPL.addStr pps "for ("
	   ; PPL.ppOpt (ppExpr {nested=false} aidinfo tidtab) pps expOpt0
	   ; PPL.addStr pps "; "
	   ; PPL.ppOpt (ppExpr {nested=false} aidinfo tidtab) pps expOpt1
	   ; PPL.addStr pps "; "
	   ; PPL.ppOpt (ppExpr {nested=false} aidinfo tidtab) pps expOpt2
	   ; PPL.addStr pps ")"
	   ; blockStmt aidinfo tidtab pps stmt
	   )
       | Labeled (label,stmt) => 
	   ( PPL.bBlock pps PP.INCONSISTENT ~2
	   ; PPL.newline pps
	   ; PPL.ppLabel pps label
	   ; PPL.addStr pps ": "
	   ; PPL.eBlock pps
	   ; PPL.newline pps
	   ; ppStmt aidinfo tidtab pps stmt
	   )
       | CaseLabel (li,expOpt (*PADS *), stmt) => 
	   ( PPL.bBlock pps PP.INCONSISTENT ~2
	   ; PPL.newline pps
	   ; PPL.addStr pps "case "
	   ; (case expOpt of NONE => PPL.ppLI pps li
	                  | SOME exp => (ppExpr {nested=false} aidinfo tidtab) pps exp)
	   ; PPL.addStr pps ": "
	   ; PPL.eBlock pps
	   ; PPL.newline pps
	   ; ppStmt aidinfo tidtab pps stmt
	   )
       | DefaultLabel stmt => 
	   ( PPL.bBlock pps PP.INCONSISTENT ~2
	   ; PPL.newline pps 
	   ; PPL.addStr pps "default: "
	   ; PPL.eBlock pps
	   ; PPL.newline pps
	   ; ppStmt aidinfo tidtab pps stmt
	   )
       | Goto label => 
	   ( PPL.addStr pps "goto "
	   ; PPL.ppLabel pps label
	   ; PPL.addStr pps ";"
	   )
       | Break => PPL.addStr pps "break;"
       | Continue => PPL.addStr pps "continue;"
       | Return expOpt => 
	   ( PPL.addStr pps "return "
	   ; PPL.ppOpt (ppExpr {nested=false} aidinfo tidtab) pps expOpt
	   ; PPL.addStr pps ";"
	   )
       | IfThen (exp,stmt) => 
	   ( PPL.addStr pps "if ("
	   ; ppExpr {nested=false} aidinfo tidtab pps exp
	   ; PPL.addStr pps ") "
	   ; blockStmt aidinfo tidtab pps stmt
	   )
       | IfThenElse (exp,stmt0,stmt1) => 
	   ( PPL.addStr pps "if ("
	   ; ppExpr {nested=false} aidinfo tidtab pps exp
	   ; PPL.addStr pps ") "
	   ; blockStmt aidinfo tidtab pps stmt0
	   ; PPL.newline pps
	   ; PPL.addStr pps "else"
	   ; blockStmt aidinfo tidtab pps stmt1
	   )
       | Switch (exp,stmt) =>
	   ( PPL.addStr pps "switch ("
	   ; ppExpr {nested=false} aidinfo tidtab pps exp
	   ; PPL.addStr pps ")"
	   ; blockStmt aidinfo tidtab pps stmt
	   )
       | ErrorStmt =>
	   ( PPL.addStr pps "/* ErrorStmt */"
	   )
    | StatExt se => PPAE.ppStatementExt (ppExpr {nested=false},ppStmt,ppBinop,ppUnop) aidinfo tidtab pps se

  and ppExpr nested aidinfo tidtab pps expr =
       PPAA.ppExpressionAdornment (ppCoreExpr nested) aidinfo tidtab pps expr

  and ppCoreExpr {nested} aidinfo tidtab pps coreExpr = 
    case coreExpr
      of IntConst li => PPL.ppLLI pps li
       | RealConst r => PPL.ppReal pps r
       | StringConst s => PPL.ppString pps s
       | Call (exp,exps) => 
	   ( ppExpr {nested=true} aidinfo tidtab pps exp
	   ; PPL.space pps
	   ; PPL.ppList { pp=ppExpr {nested=false} aidinfo tidtab
		        , sep=","
		        , lDelim="("
		        , rDelim=")"
		        } pps exps
	   )
       | QuestionColon (e0,e1,e2) =>
	   ( ppLParen nested pps 
	   ; ppExpr {nested=true} aidinfo tidtab pps e0
	   ; PPL.addStr pps " ? "
	   ; ppExpr {nested=false} aidinfo tidtab pps e1
	   ; PPL.addStr pps " : "
	   ; ppExpr {nested=false} aidinfo tidtab pps e2
	   ; ppRParen nested pps 
	   )
       | Assign (e0,e1) =>
	   ( ppLParen nested pps 
	   ; ppExpr {nested=false} aidinfo tidtab pps e0
	   ; PPL.addStr pps " = "
	   ; ppExpr {nested=true} aidinfo tidtab pps e1
	   ; ppRParen nested pps 
	   )
       | Comma (e0,e1) =>
	   ( PPL.addStr pps "("
	   ; ppExpr {nested=false} aidinfo tidtab pps e0
	   ; PPL.addStr pps ","
	   ; ppExpr {nested=false} aidinfo tidtab pps e1
	   ; PPL.addStr pps ")"
	   )
       | Sub (e0,e1) =>
	   ( ppExpr {nested=true} aidinfo tidtab pps e0
	   ; PPL.addStr pps "["
	   ; ppExpr {nested=false} aidinfo tidtab pps e1
	   ; PPL.addStr pps "]"
	   )
       | Member (exp,member) =>
	   ( ppLParen nested pps 
	   ; ppExpr {nested=true} aidinfo tidtab pps exp
	   ; PPL.addStr pps "."
	   ; PPL.ppMember pps member
	   ; ppRParen nested pps 
	   )
       | Arrow (exp,member) =>
	   ( ppLParen nested pps 
	   ; ppExpr {nested=true} aidinfo tidtab pps exp
	   ; PPL.addStr pps "->"
	   ; PPL.ppMember pps member
	   ; ppRParen nested pps 
	   )
       | Deref exp => 
	   ( ppLParen nested pps 
	   ; PPL.addStr pps "*"
	   ; ppExpr {nested=true} aidinfo tidtab pps exp
	   ; ppRParen nested pps 
	   )
       | AddrOf exp => 
	   ( ppLParen nested pps 
	   ; PPL.addStr pps "&"
	   ; ppExpr {nested=true} aidinfo tidtab pps exp
	   ; ppRParen nested pps 
	   )
       | Binop (binop,exp0,exp1) => 
	   ( ppLParen nested pps 
	   ; ppExpr {nested=true} aidinfo tidtab pps exp0
	   ; ppBinop aidinfo tidtab pps binop
	   ; ppExpr {nested=true} aidinfo tidtab pps exp1
	   ; ppRParen nested pps 
	   )
       | Unop (unop,exp) => 
 	   ( ppLParen nested pps 
 	   ; if (isPostFix unop)
 		 then (ppExpr {nested=true} aidinfo tidtab pps exp; ppUnop aidinfo tidtab pps unop)
 	     else (ppUnop aidinfo tidtab pps unop; ppExpr {nested=true} aidinfo tidtab pps exp)
 	   ; ppRParen nested pps 
 	   )
       | Cast (ctype,exp) => 
	   ( ppLParen nested pps 
	   ; PPL.addStr pps "("
	   ; ppCtype aidinfo tidtab pps ctype
	   ; PPL.addStr pps ") "
	   ; ppExpr {nested=true} aidinfo tidtab pps exp
	   ; ppRParen nested pps 
	   )
       | Id id => PPL.ppId pps id
       | EnumId (id,li) => PPL.ppMember pps id
       | SizeOf ctype =>
	   ( ppLParen nested pps
	   ; PPL.addStr pps "sizeof("
	   ; ppCtype aidinfo tidtab pps ctype
	   ; PPL.addStr pps ")"
	   ; ppRParen nested pps 
	   )

       | ExprExt ee => PPAE.ppExpressionExt (ppExpr {nested=false},ppStmt,ppBinop,ppUnop) aidinfo tidtab pps ee 
       | ErrorExpr => ( warning "ppCoreExpression" "found an error expression"
		      ; PPL.addStr pps "/* error expression */ 0"
		      )

  and ppInitExpression aidinfo tidtab pps initExpr =
      case initExpr 
	of Simple expr => ppExpr {nested=false} aidinfo tidtab pps expr
	 | Aggregate initExprs => 
	     PPL.ppList { pp=ppInitExpression aidinfo tidtab
		        , sep=","
		        , lDelim="{"
		        , rDelim="}"
		        } pps initExprs

  fun getCTyName tidtab (cty:Ast.ctype) = 
      let fun getName tid = 
	  let val SOME (tyBinding : Bindings.tidBinding) = Tidtab.find(tidtab,tid)
	  in
	      #name tyBinding
	  end
         fun getIntName(intKind, signedness, signednessTag) =
	     case (intKind, signedness) 
	     of (Ast.INT, Ast.SIGNED) => SOME("xsd:int")
             | _ => SOME ("ToBeImplemented")
      in
       (case cty 
        of Qual(qual, ctype) => getCTyName tidtab ctype
        |  Numeric(_,_,signedness,intKind,signednessTag) => getIntName (intKind, signedness, signednessTag)
        |  StructRef tid => getName tid
        |  UnionRef tid => getName tid
        |  EnumRef tid => getName tid
        |  TypeRef tid => getName tid
        |  _ => NONE ) 
       handle Option => NONE
      end

  fun structInfo tidtab tid = 
      let val bindingOpt = Tidtab.find(tidtab,tid)
	  val binding : Bindings.tidBinding = valOf bindingOpt 
	  val name = valOf (#name binding)
	  val Bindings.Typedef(tid',cty) = valOf(#ntype binding)
	  val (Ast.StructRef stid) = cty
	  val sbindingOpt = Tidtab.find(tidtab,stid)
       	  val sbinding : Bindings.tidBinding = valOf sbindingOpt 
	  val Bindings.Struct(tid'',fields) = valOf(#ntype sbinding)
	  fun cnvField (cty,memOpt : Ast.member option,_,_) = 
	      let val fsym : Symbol.symbol = #name(valOf memOpt)
	      in
		  (getCTyName tidtab cty, Symbol.name fsym)
	      end
      in
	 (name,List.map cnvField fields)
	 handle Match => (PError.bug "expected typedef to struct binding"; ("bogus", []))
  	  handle Option => (PError.bug "expected SOME"; ("bogus", []))
      end

  fun ppStrPairs pps (tyNameOpt, fieldName) = 
      let val tyName = case tyNameOpt of NONE => "NoTypeName" | SOME name => name
      in
	  ( PPL.addStr pps tyName
	  ; space pps
          ; PPL.addStr pps fieldName
          )
      end
  fun ppPStruct (ptyInfo:PTys.pTyInfo) tidtab pps (Ast.TypeDecl{tid,...})  = 
      let val edTid = #edTid ptyInfo
	  val (edName, edFields) = structInfo tidtab edTid
          val (repName, repFields) = structInfo tidtab tid
      in
	((PPL.addStr pps "Struct" 
        ; space pps
        ; PPL.addStr pps repName
	; newline pps
        ; PPL.ppList { pp=ppStrPairs
		        , sep=","
		        , lDelim="{"
		        , rDelim="}"
		        } pps (repFields @ edFields)
        ; newline pps)
	 handle _ => PPL.addStr pps "ERROR: unbound tid" (* fix this *))
      end  
    | ppPStruct ptyInfo tidtab pps _ = PPL.addStr pps "ERROR: Unexepected variable" (* fix this *)

  fun ppPKind (ptyInfo : PTys.pTyInfo (* cmp-tys.sml*) ) tidtab pps decl = 
      case #kind ptyInfo
      of PTys.Typedef => PPL.addStr pps "foo Typedef"
      |  PTys.Struct => ppPStruct ptyInfo tidtab pps decl
      |  PTys.Union => PPL.addStr pps "Union" 
      |  PTys.Array => PPL.addStr pps "Array" 
      |  PTys.Enum => PPL.addStr pps "Enum" 

  fun ppCoreExternalDecl' ptyInfo aidinfo tidtab pps edecl =
    case edecl
      of ExternalDecl decl => 
	  (  ppPKind ptyInfo tidtab pps decl
	   ; PPL.newline pps
	   ; ppDeclaration aidinfo tidtab pps decl
	   ; PPL.newline pps)		    
       | FunctionDef (id,ids,stmt) =>  (* This branch will not be called as functions aren't being tagged *)
	   let val {location,...} = id
	       val (stClass,ctype) = getCtype id
	       val (ctype,kNr,params) =
		   case ctype
		     of Ast.Function (retTy,paramTys) =>
			 if null paramTys andalso not (null ids)
			     then (ctype,true,KNR ids)
			 else (ctype,false,ANSI ids)
		      | _ =>
			 (warning
			  "ppCoreExternalDecl" 
			  ("No function type associated with id:"
			   ^(PPL.ppToString PPL.ppId id))
			 ;(Ast.Function (Ast.Void,[]),false,ANSI [])
			 )
	       fun kr pps [] = []
		 | kr pps (id::ids) = 
		   (ppIdDecl aidinfo tidtab pps id
		   ;PPL.addStr pps ";"
		   ;if null ids then () else newline pps
		   ;kr pps ids
		   )
	   in ppLoc pps location
	     ;ppStorageClass pps stClass
	     ;ppDecl0 aidinfo tidtab pps (SOME (ID id),params,ctype)
	     ;PPL.newline pps
	     ;if kNr then (blockify 2 kr pps ids; newline pps) else ()
	     ;ppStmt aidinfo tidtab pps stmt
             ;PPL.newline pps
	   end 
       | ExternalDeclExt ed => 
	   PPAE.ppExternalDeclExt (* PADS *) NONE (ppExpr {nested=false},ppStmt,ppBinop,ppUnop) aidinfo tidtab pps ed

  fun ppCoreExternalDecl NONE aidinfo tidtab pps edecl = ()
    | ppCoreExternalDecl (SOME p) aidinfo tidtab pps edecl = ppCoreExternalDecl' p aidinfo tidtab pps edecl

  fun ppExternalDecl paidinfo aidinfo tidtab pps edecl = 
       PPAA.ppExternalDeclAdornment NONE paidinfo ppCoreExternalDecl aidinfo tidtab pps edecl
 
  (* PADS: takes source name and description of output kind *)
  fun ppExternalDeclRefined srcFile paidinfo aidinfo tidtab pps edecl = 
      let val ppCoreED : ptyinfo option -> aidinfo -> Tables.tidtab -> Ast.coreExternalDecl pp = ppCoreExternalDecl
      in
	  PPAA.ppExternalDeclAdornment srcFile paidinfo ppCoreED aidinfo tidtab pps edecl
      end

  fun ppAst srcFile paidinfo aidinfo tidtab pps edecls = 
      List.app (ppExternalDeclRefined srcFile paidinfo aidinfo tidtab pps) edecls

  (* The pretty-printer expects a block at top level, so all of the
   * external interfaces are wrapped to give it one.
   *)
  fun wrap pp aidinfo tidtab pps v = 
    ( PPL.bBlock pps PP.INCONSISTENT 0
    ; pp aidinfo tidtab pps v
    ; PPL.newline pps
    ; PPL.eBlock pps
    )

  val ppBinop = wrap ppBinop
  val ppUnop = wrap ppUnop
  val ppDeclaration  = wrap ppDeclaration
  val ppStatement = wrap ppStmt
  val ppCoreStatement = wrap ppCoreStmt
  val ppExpression = wrap (ppExpr {nested=false})
  val ppCoreExpression = wrap (ppCoreExpr {nested=false})
  val ppExternalDecl = fn paidInfo => wrap (ppExternalDecl paidInfo) (* PADS*)
  val ppCoreExternalDecl = fn ptyInfoOpt => wrap (ppCoreExternalDecl ptyInfoOpt) (* PADS *)
  val ppAst  = fn srcFile => fn paidInfo => wrap (ppAst srcFile paidInfo)  (* PADS *)
end
