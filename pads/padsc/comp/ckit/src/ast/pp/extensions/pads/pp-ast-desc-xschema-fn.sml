(*
  This module implements the rules to generate an XML representation
  of the PADS description itself.  The schema for this representation
  is available in pads/padsc/schemata/pads-xml.xsd.
*)
functor PPAstDescXschemaFn (structure PPAstPaidAdornment : PPASTPAIDADORNMENT) : PP_DESC_XML_AST = struct 

  structure Tid = Tid
  structure Pid = Pid
  structure PP = OldPrettyPrint
  structure B = Bindings
  structure PPAA = PPAstPaidAdornment
  structure PPAE = PPAstExtFn (type aidinfo = PPAstPaidAdornment.aidinfo);
  structure PPL = PPLib
  structure PX = ParseTreeExt

  open PPLib
  open Ast
  open PTyUtils

  type aidinfo = PPAE.aidinfo
  type paidinfo = PPAstPaidAdornment.paidinfo
  type ptyinfo = PTys.pTyInfo

  val printLocation = false (* internal flag - pretty print locations as comments *)

  fun ppLoc pps (SourceMap.LOC {srcFile, beginLine, beginCol, endLine, endCol}) =
      if printLocation then ( PPL.addStr pps "/*["
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

  fun strip suf s = String.extract(s, 0, SOME (String.size s - String.size suf))
      
  fun isSuffix suf s = 
      if String.size suf >= String.size s then false
      else let val schars = String.explode s
	       val endStr = String.implode (
					    List.drop (schars, String.size s - String.size suf))
	   in
	       endStr = suf
	   end

  fun stripAll s = if isSuffix "_pd" s 
		       then strip "_pd" s
		   else if isSuffix "_tag" s
			    then strip "_tag" s
			else if isSuffix "_pd_u" s
				 then strip "_pd_u" s
			     else if isSuffix "\" minOccurs=\"0\" maxOccurs=\"unbounded" s
				      then strip "\" minOccurs=\"0\" maxOccurs=\"unbounded" s
				  else s 

  (* An XML Schema type attribute *)
  fun isBaseTypeName s = 
      not(Option.isSome (PTys.find (Atom.atom (stripAll s))))
(* PBaseTys.isBaseTy(PBaseTys.baseInfo, ParseTreeExt.Name (stripAll s))  *)

  fun typeAttribute s = (" type=\"" ^ s ^ "\"")

  fun mapBaseTypeName name = 
      if isBaseTypeName (name) then ("p:"^name) else name
  
  fun mapFieldTypeName name = 
      if isBaseTypeName (name) then ("p:val_"^name) else name

  fun makeFieldNames flds = 
      List.map (fn (t,n) => (mapFieldTypeName, t, n)) flds 

  fun makeBaseNames flds = 
      List.map (fn (t,n) => (mapBaseTypeName, t, n)) flds 

  fun mapPdName s = 
      if isBaseTypeName s then ("p:Pbase_pd") else s^"_pd"

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
    = Arr of (IntInf.int * Ast.expression) option
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
	      let fun ppLI' pps li = (addStr pps ":"; ppLI pps (IntInf.toLarge li))

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
		      ;ppLI pps (IntInf.toLarge li)
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
	   ; (case expOpt of NONE => PPL.ppLI pps (IntInf.toLarge li)
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

  (** PADS to XML XSchema translation **)

  (* ppXMLHeader prints the header for a Schema declaration: 
     opentag [ name="Name" ]  [ type="typeOpt" ] closetag 
  
     If the name of the element is "pd", it is always optional. 
     This is a hack!!!!
  *)
  fun ppXMLHeader opentag closetag pps (mapTypeName, tyNameOpt, name) =	
      ( PPL.addStr pps opentag
      ; 
       let val (NameAttr,isPdField) = (("name=\"" ^ name ^ "\""), name = "pd")
           val tyNameAttr = 
	       (case tyNameOpt of 
		   NONE => "" | 
		   SOME name => typeAttribute (mapTypeName name))
	   val optStr = 
	       if isPdField then " minOccurs=\"0\" maxOccurs=\"1\"" else ""
       in
	   PPL.addStr pps (NameAttr ^ tyNameAttr ^ optStr)
       end
       ; PPL.addStr pps closetag)


  fun complexTypeOpen pps NameOpt =	
      ( PPL.addStr pps "<xs:complexType "
      ; 
       let val NameAttr = 
	   case NameOpt of 
	       NONE => ""
	     | SOME name => (("name=\"" ^ name ^ "\""))
       in
	   PPL.addStr pps NameAttr 
       end
       ; PPL.addStr pps ">\n")

  fun complexTypeClose pps = PPL.addStr pps "</xs:complexType>\n"

  fun simpleTypeOpen pps NameOpt =	
      ( PPL.addStr pps "<xs:simpleType "
      ; 
       let val NameAttr = 
	   case NameOpt of 
	       NONE => ""
	     | SOME name => (("name=\"" ^ name ^ "\""))
       in
	   PPL.addStr pps NameAttr 
       end
       ; PPL.addStr pps ">\n")

  fun simpleTypeClose pps = PPL.addStr pps "</xs:simpleType>\n"

  fun ppXMLFields pps eFields =		
      ( PPL.addStr pps "obsolete")

  fun ppXMLSequence pps eFields =		(* <seq> eFields </seq> *)
      ( PPL.addStr pps "<xs:sequence>\n"
      ; ppXMLFields pps eFields
      ; newline pps
      ; PPL.addStr pps "</xs:sequence>\n")

  fun ppXMLRestriction pps base =
      PPL.addStr pps ("<xs:restriction base=\"" ^ base ^ "\"/> \n")
      
  fun isIdentity  arg = case arg of
    (_,_,name) => name = PNames.identifier 

  fun id s = s

  (*
      <complexType name=[[identifier]]> 
        <sequence>
          [[ fields ]]_local 
        <sequence>
      </complexType>
  *)
  fun ppXMLComplex pps (eNameOpt,eFields) =	
        ( complexTypeOpen pps eNameOpt
        ; ppXMLSequence pps (List.filter (not o isIdentity) eFields) 
        ; complexTypeClose pps)

  (*
      <complexType name=[[identifier]]> 
        <choice>
          [[ fields ]]_local 
        <choice>
      </complexType>
  *)
  fun ppXMLChoice pps Fields =		
      ( PPL.addStr pps "<xs:choice>\n"
      ; ppXMLFields pps Fields
      ; newline pps
      ; PPL.addStr pps "</xs:choice>\n")

  (*
      The Psource modifier may be used as an annotation on any Pads type,
      indicating that the type in question describes the entirety of the
      external representation of the data.  In the XML representation of a
      PADS data source, the corresponding top-level or root XML element,
      which contains entirety of the PADS data, is named 'Psource'.
    
      [[ Psource pads_decl ]] 
      identifier is name of type in pads_decl
              ==
      [[ pads_decl ]]_schema @ 
      [ <element name="PSource" type=[[identifier]]> ]

  *)

  fun ppTagIndent tag ppArg pps arg  = 
      ( PPL.addStr pps ("<"^tag^">")
      ; blockify 2 ppArg pps arg
      ; PPL.newline pps
      ; PPL.addStr pps ("</"^tag^">"))

  fun ppTag tag ppArg pps arg  = 
      (PPL.addStr pps ("<"^tag^">")
      ;ppArg pps arg
      ;PPL.addStr pps ("</"^tag^">"))

  fun ppTopElemIfPsource pps (ptyInfo:PTys.pTyInfo,repNameOpt) =
	if (#isSource ptyInfo) 
	then ( ppXMLHeader "<xs:element " "/>" pps (mapFieldTypeName, repNameOpt, "PSource")
	     ; newline pps)
	else ()

  fun ppExp pps exp = PPL.addStr pps (ParseTreeUtil.expToString exp)
  fun ppCExpr pps exp = ppTag "expr"  (ppTag "native" ppExp) pps exp
  fun ppPExpr pps exp = 
      case exp 
      of ParseTree.MARKexpression(l,e) => ppPExpr pps e 
      |  ParseTree.String s => ppExp pps exp
      |  ParseTree.IntConst i => ppExp pps exp
      |  ParseTree.RealConst r => ppExp pps exp
      |  _ => ppCExpr pps exp

  fun ppConstraint pps con = 
      case con 
      of PX.General    exp => ppPExpr pps exp
      |  PX.ParseCheck exp => ppTag "parsecheck" ppPExpr pps exp

  fun ppConstraints pps cons = 
      case cons of [] => ()
      | _ =>  PPL.separate (ppConstraint, PPL.newline) pps cons

  fun ppPostConds pps cons = 
      case cons of [] => ()
      | _=>  (PPL.newline pps; ppTagIndent "postConstraints" ppConstraints pps cons)

  fun ppTyApp pps (name,args) = 
       let fun ppArg pps a = ppTag "argument" ppPExpr pps a
       in
	   (ppTag "name" PPL.addStr pps name
	  ;(case args of nil => ()
	    | _ => PPL.blockify 2 (PPL.separate (ppArg, PPL.newline)) pps args))
       end

  fun ppTy pps ty = 
        ( PPL.addStr pps "<ptype>" 
	; blockify 2 ppTyApp pps ty
	; PPL.newline pps
        ; PPL.addStr pps "</ptype>")

  fun ppPTy aidinfo tidtab pps pty = 
      case pty 
      of TyProps.Pads {tyCon,args} => ppTy  pps (tyCon, args)
      |  TyProps.C cty => ppTag "ctype" (ppCtype aidinfo tidtab) pps cty

  fun ppPChar pps exp = 
      case ParseTreeUtil.stripExp exp 
      of ParseTree.IntConst i => (
           PPL.addStr pps (Char.toString(Char.chr (IntInf.toInt i)))
           handle _ => ppCExpr pps exp
          )
      | _ => ppCExpr pps exp

  fun ppPString pps exp = 
      case ParseTreeUtil.stripExp exp 
      of ParseTree.String s => PPL.addStr pps s
      | _ => ppCExpr pps exp

  fun ppLit pps lit = 
      case lit
      of TyProps.DChar exp =>   ppTag "char" ppPChar pps exp
      |  TyProps.DString exp => ppTag "string" ppPString pps exp
      |  TyProps.DRegExp exp => ppTag "regexp" ppPString pps exp
      |  TyProps.DNoSep =>      PPL.addStr pps "<nosep/>"

  fun ppComment pps comment =
      case comment of NONE => ()
      | SOME s => (newline pps; ppTag "comment" PPL.addStr pps s)

  fun ppNestedOpt pps (name,args,optPred) = 
      let fun ppOptDecon pps {some,none} = ()  (* fix this *)
	  fun ppOptPred pps optPred = 
	      case optPred 
	      of NONE => ()
              | SOME(PX.Simple l) => ppTagIndent "pred" ppConstraints pps l
	      | SOME(PX.Decon r) => ppTagIndent "optionPred" ppOptDecon pps r
      in
        ( PPL.addStr pps "<nestedOption>" 
	; blockify 2 ppTyApp pps (name,args)
	; PPL.newline pps
        ; ppOptPred pps optPred
        ; PPL.addStr pps "</nestedOption>")
      end

  fun ppSize which pps size = ppTag which ppPExpr pps size
  fun ppSizeConstraints pps size = 
      case size 
      of {min=NONE,minConst=NONE,max=NONE,maxConst=NONE} => ()
      |  {min,minConst,max,maxConst} 
	  => ( newline pps
	     ; PPL.addStr pps "<sizeConstraints>"
	     ; Option.map (fn s => blockify 2 (ppSize "min") pps s) min
	     ; Option.map (fn s => blockify 2 (ppSize "max") pps s) max
	     ; PPL.newline pps
	     ; PPL.addStr pps "</sizeConstraints>"
	     )
  fun ppTerm' pps term = 
      case term of PX.Expr e => ppCExpr pps e
    | PX.noSep => PPL.addStr pps "<nosep/>"

  fun ppPred pps (PX.Last  e) = ppTagIndent "last"   ppConstraints pps e
    | ppPred pps (PX.Ended e) = ppTagIndent "ended"  ppConstraints pps e
    | ppPred pps (PX.Skip  e) = ppTagIndent "omit"   ppConstraints pps e
    | ppPred pps (PX.Longest) = PPL.addStr pps "</longest>"
    | ppPred pps (PX.Sep e)   = ppTagIndent "sep"   ppCExpr pps e  (* fix these *)
    | ppPred pps (PX.Term e)  = ppTagIndent "term"  ppTerm' pps e

  fun ppPredList pps preds = 
      case preds of [] => () | _ => PPL.blockify 2 (PPL.separate (ppPred, PPL.newline)) pps preds

  fun ppNestedArray pps (name,args,size,arrayPred) = 
      let fun ppSizeOpt pps size = 
	      case size of NONE => ()
              | SOME (PX.SizeInfo{min,max,maxTight}) => ppSizeConstraints pps {min=min,max=max,maxConst=NONE, minConst=NONE} (* fix this*)
	  fun ppDelims pps arrayPred = 
	      case arrayPred of [] => ()
              | _ => ( PPL.addStr pps "<delimiterConstraints>"
                     ; ppPredList pps arrayPred
                     ; PPL.addStr pps "</delimiterConstraints>")
      in
        ( PPL.addStr pps "<nestedArray>" 
	; blockify 2 ppTyApp pps (name,args)
	; PPL.newline pps
        ; blockify 2 ppSizeOpt pps size
        ; blockify 2 ppDelims pps arrayPred 
        ; PPL.addStr pps "</nestedArray>")
      end

  fun ppNestedTy (name,args,isOpt,optPred,isArray,size,arrayPred) pps = 
      let fun ppBody pps () = 
	  if isOpt then ppNestedOpt pps (name, args, optPred) 
	  else if isArray then ppNestedArray pps (name,args,size,arrayPred) 
	  else ppTyApp pps (name, args) 
      in
        ( PPL.newline pps
	; PPL.addStr pps "<ptype>" 
	; blockify 2 ppBody pps ()
	; PPL.newline pps
        ; PPL.addStr pps "</ptype>")
      end

  fun ppFullField pps {ty,name,pred,comment,isOpt,optPred,isArray,size,arrayPred} =	
      let val {tyCon, args} = ty
      in
	  ( ppTag "name" PPL.addStr pps name
	  ; ppNestedTy (tyCon,args,isOpt,optPred,isArray,size,arrayPred) pps
          ; ppComment pps comment
	  ; ppPostConds pps pred
          )
      end

  fun ppComputeField aidinfo tidtab pps {ty, name, def, pred, comment} =
	  ( ppTag "name" PPL.addStr pps name
	  ; PPL.newline pps
	  ; ppPTy aidinfo tidtab pps ty
	  ; PPL.newline pps
          ; ppTag "definition" ppPExpr pps def
          ; ppComment pps comment
	  ; ppPostConds pps pred
          )

  fun ppPField aidinfo tidtab pps (field:TyProps.fieldInfoTy) = 
      case field
      of TyProps.Full r => ppTagIndent "full" ppFullField pps r
      |  TyProps.Literal l => ppTag "literal" ppLit pps l
      |  TyProps.Compute c => ppTagIndent "computed" (ppComputeField aidinfo tidtab) pps c

  fun ppPFields aidinfo tidtab pps eFields =		
      ( PPL.addStr pps "<fields>"
      ; blockify 2 (separate (ppPField aidinfo tidtab, newline)) pps eFields
      ; PPL.newline pps
      ; PPL.addStr pps "</fields>"
      )

  fun ppPTyParam aidinfo tidtab pps (name,acty) = 
      (  ppTag "name" PPL.addStr pps name
       ; ppTag "type" (ppCtype aidinfo tidtab) pps acty)

  fun ppPTyParams aidinfo tidtab pps typarams = 
      ( PPL.addStr pps "<params>"
       ; blockify 2 (separate (ppPTyParam aidinfo tidtab, newline)) pps typarams
       ; newline pps
       ; PPL.addStr pps "</params>"
       )
        
  fun ppPDecl aidinfo tidtab pps (name, typarams) = 
      ( PPL.addStr pps "<decl>"
       ; blockify 2 (ppTag "name" PPL.addStr) pps name
       ; case typarams of [] => () | _ => blockify 2 (ppPTyParams aidinfo tidtab) pps typarams
       ; newline pps
       ; PPL.addStr pps "</decl>")

  fun ppPDeclaration pps kind ptyInfo ppKind = 
	(PPL.addStr pps ("<"^kind^">" )
        ; blockify 2 ppKind pps ptyInfo
    	; newline pps
	; PPL.addStr pps ("</"^kind^">" )
    	; newline pps
        )

  fun ppPStruct (ptyInfo:PTys.pTyInfo) aidinfo tidtab pps (Ast.TypeDecl{tid,...})  = 
      let fun ppPStruct' pps (ptyInfo:PTys.pTyInfo) = 
	      let val declName = #repName ptyInfo
		  val typarams = #typarams ptyInfo
		  val {fields,pred} = PTys.getStructInfo ptyInfo
	      in
	      ( ppPDecl aidinfo tidtab pps (declName, typarams)
              ; newline pps
	      ; ppPFields aidinfo tidtab pps fields
              ; ppPostConds pps pred
              )  
	      end
      in
	(ppPDeclaration pps "struct" ptyInfo ppPStruct' 
	handle _ => PPL.addStr pps "ERROR: unbound tid")
      end  
    | ppPStruct ptyInfo aidinfo tidtab pps _ = PPL.addStr pps "ERROR: Unexepected variable" (* fix this *)


  fun ppPOpt (ptyInfo:PTys.pTyInfo) aidinfo tidtab pps (Ast.TypeDecl{tid,...}) =
      let fun ppPOpt' pps (ptyInfo:PTys.pTyInfo) = 
	      let val declName = #repName ptyInfo
		  val typarams = #typarams ptyInfo
		  val {fields,pred,...} = PTys.getUnionInfo ptyInfo
		  val (TyProps.Full{ty={tyCon, args},pred=fpred,...})::rs = fields
	      in
	      ( ppPDecl aidinfo tidtab pps (declName, typarams)
	      ; PPL.newline pps
	      ; ppTagIndent "base" ppTyApp pps (tyCon,args) 
              ; ppPostConds pps fpred
              )
	      end
       in 
	(ppPDeclaration pps "option" ptyInfo ppPOpt' 
	 handle _ => PPL.addStr pps "ERROR: unbound tid" (* fix this *))
    end
    | ppPOpt ptyInfo aidinfo tidtab pps _ = PPL.addStr pps "ERROR: Unexepected variable" (* fix this *)


  fun ppPUnion (ptyInfo:PTys.pTyInfo) aidinfo tidtab pps (Ast.TypeDecl{tid,...}) =
      let fun ppCaseOpt pps caseOpt = 
	      case caseOpt of NONE => PPL.addStr pps "<default/>"
              | SOME e => ppPExpr pps e
	  fun ppBranchBody aidinfo tidtab pps (caseOpt, field) = 
	      ( ppTag "case" ppCaseOpt pps caseOpt
              ; PPL.newline pps
              ; ppTagIndent "body" (ppPField aidinfo tidtab) pps field
              )
	  fun ppPBranch aidinfo tidtab pps cf = ppTagIndent "branch" (ppBranchBody aidinfo tidtab) pps cf
	  fun ppBranches aidinfo tidtab pps cfs =  separate (ppPBranch aidinfo tidtab, newline) pps cfs
          fun ppPSwitched aidinfo tidtab pps (desc, cases, fields) = 
	      ( ppTag "descriminator" ppPExpr pps desc
              ; PPL.newline pps
	      ; ppTagIndent "branches" (ppBranches aidinfo tidtab) pps (ListPair.zip(cases, fields))
	      ) 
	  fun ppPUnion' pps (ptyInfo:PTys.pTyInfo) = 
	      let val declName = #repName ptyInfo
		  val typarams = #typarams ptyInfo
		  val {descriminator, cases, fields,pred,...} = PTys.getUnionInfo ptyInfo
	      in
	      ( ppPDecl aidinfo tidtab pps (declName, typarams)
              ; newline pps
	      ; if PTys.isSwitchedUnion ptyInfo 
		    then ppTagIndent "switched" (ppPSwitched aidinfo tidtab) pps (Option.valOf descriminator,cases,fields) 
		    else ppTagIndent "inplace" (ppPFields aidinfo tidtab) pps fields
	      ; ppPostConds pps pred
	      )  
	      end
       in 
	(ppPDeclaration pps "union" ptyInfo ppPUnion' 
	 handle _ => PPL.addStr pps "ERROR: unbound tid" (* fix this *))
    end
    | ppPUnion ptyInfo aidinfo tidtab pps _ = PPL.addStr pps "ERROR: Unexepected variable" (* fix this *)




  fun ppPArray (ptyInfo:PTys.pTyInfo) aidinfo tidtab pps (Ast.TypeDecl{tid,...})  = 
      let fun ppSep pps sep   = ppTag "sep"   ppLit pps sep
	  fun ppTerm pps term = ppTag "term"  ppLit pps term
	  fun ppArrayDelims pps delims = 
	      case delims
	      of {sep=NONE,term=NONE,preds=[]} => ()
              |  {sep, term, preds}
                  => ( newline pps
		     ; PPL.addStr pps "<delimiterConstraints>"
		     ; Option.map (fn s => blockify 2 ppSep  pps s) sep
		     ; Option.map (fn t => blockify 2 ppTerm pps t) term
		     ; ppPredList pps preds
		     ; PPL.newline pps
		     ; PPL.addStr pps "</delimiterConstraints>")
	  fun ppRange pps r = 
	      case r 
              of PX.ArrayName s => blockify 2 (ppTag "in" PPL.addStr) pps s
              |  PX.Bounds (min,max) 
                 => ( blockify 2 (ppSize "min") pps min
                    ; blockify 2 (ppSize "max") pps max)
	  fun ppAConstraint pps con = 
	      case con 
	      of PX.AGeneral    exp => ppCExpr pps exp
              |  PX.AParseCheck exp => ppTag "parsecheck" ppExp pps exp
              |  PX.Forall {index, range, body} =>
                 ( PPL.addStr pps "<forAll>"
                 ; blockify 2 (ppTag "var" PPL.addStr) pps index
                 ; ppRange pps range
                 ; blockify 2 (ppTag "constraints" ppCExpr) pps body
		 ; PPL.newline pps
		 ; PPL.addStr pps "</forAll>"
                 ) 

	  fun ppAConstraints pps cons = 
               PPL.blockify 2 (PPL.separate (ppAConstraint, PPL.newline)) pps cons

          fun ppPostConds pps postConds = 
              case postConds of [] => ()
              | _=> ( newline pps
		    ; PPL.addStr pps "<arrayConstraints>"
		    ; ppAConstraints pps postConds
                    ; PPL.newline pps
		    ; PPL.addStr pps "</arrayConstraints>"
                    )
	  fun ppPArray' pps ptyInfo = 
	      let val declName = #repName ptyInfo
		  val typarams = #typarams ptyInfo
		  val {baseTy={tyCon,args}, delims, size, post} = PTys.getArrayInfo ptyInfo 
	      in
	      ( ppPDecl aidinfo tidtab pps (declName, typarams)
              ; PPL.newline pps
              ; ppTy pps (tyCon, args)
              ; ppArrayDelims pps delims
              ; ppSizeConstraints pps size
              ; ppPostConds pps post
              )
	      end
      in
	(ppPDeclaration pps "array" ptyInfo ppPArray' 
	handle _ => PPL.addStr pps "ERROR: unbound tid" (* fix this *))
      end  
    | ppPArray ptyInfo aidinfo tidtab pps _ = PPL.addStr pps "ERROR: Unexepected variable" (* fix this *)

	  (* 
              Galax does not support derivation by extension yet, so we have 
              to use full-blown restriction, which requires specifying explicitly
              fields from the base type :

	   [[ Penum identifier [p_formals] [p_enum_prefix] { p_enum_fields } ]]_schema
                       == 
	   [
	     <complexType name=[[identifier]]>
               <xs:choice>
                 <xs:element name="val" type=[[p_ty]]_typename/>
                 <xs:element name="pd" type=[[[[p_ty]]_typename]]_pdname/>
              </xs:choice>
	     </complexType>
	   ]
	 *)

  fun ppPEnum (ptyInfo:PTys.pTyInfo) aidinfo tidtab pps (Ast.TypeDecl{tid,...})  = 
      let val (repName, repFields) = enumInfo tidtab tid
      in
	((newline pps
	  ; complexTypeOpen pps (repName)
	  ; ppXMLSequence pps [(mapBaseTypeName, SOME "Puint8", "val"), (mapPdName o mapFieldTypeName, SOME "Puint8", "pd")]
	  ; complexTypeClose pps
	  ; ppTopElemIfPsource pps (ptyInfo,repName)	
        )
	handle _ => PPL.addStr pps "ERROR: unbound tid" (* fix this *))
      end  
    | ppPEnum ptyInfo aidinfo tidtab pps _ = PPL.addStr pps "ERROR: Unexepected variable" (* fix this *)


  fun ppPTypedef (ptyInfo:PTys.pTyInfo) aidinfo tidtab pps (Ast.TypeDecl{tid,...})  =
      let val (Name, Ty) = typedefInfo tidtab tid
	  val Name_pd = mapPdName (valOf Name)
	  val base = (valOf Ty)
      in 
	  if isBaseTypeName base then 
	      (newline pps
	       (* ; PPL.addStr pps "<!-- Ptypedef "^Name^" -->" *)
	       ; complexTypeOpen pps (Name)
	       (* ; ppXMLRestriction pps base_name *)
	       ; ppXMLSequence pps [(mapBaseTypeName, SOME base, "val"), (mapPdName o mapFieldTypeName, SOME base, "pd")]
	       ; complexTypeClose pps
		   ; ppTopElemIfPsource pps (ptyInfo,Name)
	       )
	      handle _ => PPL.addStr pps "ERROR: unbound tid" (* fix this *)
	  else
	      (let val base_name = mapBaseTypeName base
		  val pd_base_name = mapPdName base_name
	       in
		   newline pps
		   ; complexTypeOpen pps Name
		   ; ppXMLRestriction pps base_name
		   ; complexTypeClose pps
		   ; complexTypeOpen pps (SOME Name_pd)
		   ; ppXMLRestriction pps pd_base_name
		   ; complexTypeClose pps
		   ; ppTopElemIfPsource pps (ptyInfo,Name)
	       end
	      )
	      handle _ => PPL.addStr pps "ERROR: unbound tid" (* fix this *)
      end
    | ppPTypedef ptyInfo aidinfo tidtab pps _ = PPL.addStr pps "ERROR: Unexepected variable" (* fix this *)


  fun ppPKind (ptyInfo : PTys.pTyInfo (* cmp-tys.sml*) ) aidinfo tidtab pps decl = 
    ( PPL.newline pps
    ; case #info ptyInfo
      of TyProps.TypedefInfo _ => ppPTypedef ptyInfo aidinfo tidtab pps decl
      |  TyProps.StructInfo _ => ppPStruct ptyInfo aidinfo tidtab pps decl
      |  TyProps.UnionInfo {fromOpt,...} => if fromOpt then ppPOpt ptyInfo aidinfo tidtab pps decl
					    else ppPUnion ptyInfo aidinfo tidtab pps decl 
      |  TyProps.ArrayInfo _ => ppPArray ptyInfo aidinfo tidtab pps decl 
      |  TyProps.EnumInfo _ => ppPEnum ptyInfo aidinfo tidtab pps decl
      |  TyProps.BaseInfo _ => PPL.addStr pps "Unexpected Base Type")

  fun ppCoreExternalDecl' ptyInfo aidinfo tidtab pps edecl =
    case edecl
      of ExternalDecl decl => ppPKind ptyInfo aidinfo tidtab pps decl
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

  (*
    A complete PADS source file is mapped to a complete XML Schema.  The target
    namespace is the name of the PADS source file.  The default PADS
    schema is imported and associated with the 'p' namespace prefix.
    The content of the XML schema is the concatenation of all the global 
    declarations that result from mapping the top-level PADS types.
    
       [[ pads_decl pads_decl_list ]]_schema
          == 
       <schema targetNamespace=[[name-of-pads-source]]
         xmlns=[[name-of-pads-source]]
         xmlns:xs="http://www.w3.org/2001/XMLSchema"
         xmlns:p="http://www.padsproj.org/pads.xsd" >
         <import namespace="http://www.padsproj.org/pads.xsd"
                 schemaLocation=[[location-of-pads.xsd]]/>
         [[ pads_decl ]] @ [[ pads_decl_list ]]_schema
       </schema>
  *)
  fun ppAst padsDir srcFile paidinfo aidinfo tidtab pps edecls =
      let val fileName = case srcFile of NONE => "" | SOME name => OS.Path.file name
	  val xmlnsp  = "<PadsC xmlns=\"http://www.padsproj.org/pads-xml.xsd\">"
      in
        ( PPL.addStr pps xmlnsp
        ; PPL.newline pps 
        ; List.app (ppExternalDeclRefined srcFile paidinfo aidinfo tidtab pps) edecls
        ; PPL.newline pps
        ; PPL.addStr pps "</PadsC>" 
        ; PPL.newline pps
        )  
      end  

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
  val ppAst  = fn padsDir => fn srcFile => fn paidInfo => wrap (ppAst padsDir srcFile paidInfo)  (* PADS *)
end
 

