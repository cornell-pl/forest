structure ParseTreeUtil =
struct

    structure PT = ParseTree
    structure PX = ParseTreeExt

    fun makePCT s : PT.ctype = {qualifiers=[], specifiers=s}
    fun makePDT s : PT.decltype = {qualifiers=[],specifiers=s,storage=[]}
    fun pctToPDT (ct : PT.ctype) : PT.decltype = 
	let val {specifiers,qualifiers} = ct
	in
	    {specifiers=specifiers,qualifiers=qualifiers,storage=[]}
	end

    fun pctToPTyDefDT (ct : PT.ctype) : PT.decltype = 
	let val {specifiers,qualifiers} = ct
	in
	    {specifiers=specifiers,qualifiers=qualifiers,storage=[PT.TYPEDEF]}
	end

    fun mkParam(ct,s) = (pctToPDT ct, PT.VarDecr s)


    fun makeStructPCT (fields : (string * PT.ctype * string option) list, tag : string option) =
	let fun genField (id,ct,sOpt) = 
	    (ct,[ (PT.VarDecr id, PT.EmptyExpr) ],sOpt)
	in
	    makePCT [PT.Struct {isStruct = true,
				 tagOpt = tag,
				 members = List.map genField fields
				 }
		      ]
	end

    fun makeTypedefPCT s  = makePCT [PT.TypedefName s]
    fun ptrPCT ty         = makePCT [PT.Pointer ty]

    val char       = makePCT [PT.Char]
    val uchar      = makePCT [PT.Unsigned, PT.Char]
    val uint       = makePCT [PT.Unsigned, PT.Int]
    val int        = makePCT [PT.Int]
    val ushort     = makePCT [PT.Unsigned, PT.Short]
    val short      = makePCT [PT.Short]
    val long       = makePCT [PT.Long]
    val ulong      = makePCT [PT.Long, PT.Unsigned]
    val longlong   = makePCT [PT.Long, PT.Long]
    val ulonglong  = makePCT [PT.Long, PT.Long, PT.Unsigned]
    val float      = makePCT [PT.Float]
    val double     = makePCT [PT.Double]
    val charPtr    = ptrPCT  char
    val ucharPtr   = ptrPCT  uchar
    val charPtrPtr = ptrPCT  charPtr
    val void       = makePCT [PT.Void]
    val voidPtr    = ptrPCT  void
    val voidPtrPtr = ptrPCT (voidPtr)

    fun intX i = PT.IntConst (IntInf.fromInt i)
    fun int32X i = (PT.IntConst (IntInf.fromInt (Int32.toInt i)))

    val zero = intX 0

    val trueX             = intX 1
    val falseX            = zero

    fun addrX e           = PT.Unop(PT.AddrOf,e)
    fun andX (e1,e2)      = PT.Binop(PT.And,e1,e2)
    fun arrowX (e1,e2)    = PT.Binop(PT.Arrow,e1,e2)
    fun assignX (lhs,rhs) = PT.Binop(PT.Assign,lhs,rhs)
    fun assignS (lhs,rhs) = PT.Expr(assignX (lhs,rhs))
    fun callS(name, args) = PT.Expr(PT.Call(name, args))
    fun commaX (e1,e2)    = PT.Binop(PT.Comma,e1,e2)
    fun dotX (lhs,rhs)    = PT.Binop(PT.Dot,lhs,rhs)
    fun eqX (e1,e2)       = PT.Binop(PT.Eq,e1,e2)
    fun neqX (e1,e2)      = PT.Binop(PT.Neq,e1,e2)
    fun ltX (e1,e2)       = PT.Binop(PT.Lt,e1,e2)
    fun lteX(e1,e2)       = PT.Binop(PT.Lte,e1,e2)
    fun gtX (e1,e2)       = PT.Binop(PT.Gt,e1,e2)
    fun gteX (e1,e2)      = PT.Binop(PT.Gte,e1,e2)
    fun notX e            = PT.Unop(PT.Not,e)
    fun orX (e1,e2)       = PT.Binop(PT.Or,e1,e2)
    fun plusX (e1,e2)     = PT.Binop(PT.Plus,e1,e2)
    fun minusX (e1,e2)    = PT.Binop(PT.Minus,e1,e2)
    fun timesX (e1,e2)    = PT.Binop(PT.Times,e1,e2)
    fun rshiftX (e1,e2)   = PT.Binop(PT.Rshift,e1,e2)
    fun modX (e1,e2)      = PT.Binop(PT.Mod,e1,e2)
    fun plusAssignS(e1,e2)= PT.Expr(PT.Binop(PT.PlusAssign,e1,e2))
    fun postIncX e        = PT.Unop(PT.PostInc, e)
    fun postIncS e        = PT.Expr(PT.Unop(PT.PostInc, e))
    fun postDecX e        = PT.Unop(PT.PostDec, e)
    fun starX e           = PT.Unop(PT.Star,e)
    fun sizeofX ct        = PT.Unop(PT.SizeofType(ct),PT.EmptyExpr)
    fun sizeofEX e        = PT.Unop(PT.Sizeof, e)
    fun strIsNonNull name = PT.Binop(PT.Neq,PT.Id name,zero)
    fun subX(e1,e2)       = PT.Binop(PT.Sub, e1, e2)
    val emptyS            = (PT.Expr PT.EmptyExpr)
    fun returnS e         = PT.Return(e) 
    fun condX (e1,e2,e3)  = PT.QuestionColon(e1,e2,e3)

    fun declS (dt : PT.decltype,
	       decr : PT.declarator,
	       init : PT.expression) : PT.statement =
	PT.Decl(PT.Declaration (dt,[(decr,init)]))

    fun varDeclS (ct,v,init) = declS(pctToPDT ct,PT.VarDecr v,init)
    fun varDeclS' (ct,v) = varDeclS(ct,v,PT.EmptyExpr)
    fun mkCommentS s = PT.StatExt(PX.PComment s)


    fun makeStructEDecl (fields : (string*PT.ctype*string option) list, tag : string option) =
        PT.ExternalDecl(
          PT.Declaration(
              pctToPDT(makeStructPCT(fields,tag)),
              []))

    fun makeTyDefStructEDecl (fields : (string*PT.ctype * string option) list, tag : string) =
        PT.ExternalDecl(
          PT.Declaration(
              pctToPTyDefDT(makeStructPCT(fields,SOME (tag^"_s"))),
              [(PT.VarDecr tag, PT.EmptyExpr)]))

    fun mkFunctionEDecl(funName, paramList, bodyS, retTy:PT.ctype) =
           PT.FunctionDef
              {body = bodyS,
               funDecr = PT.FuncDecr(PT.VarDecr funName, paramList),
               krParams = [],
               retType = pctToPDT retTy}

    fun expToString p =
        case p
        of PT.EmptyExpr => ""
        |  PT.IntConst i => IntInf.toString i
        |  PT.RealConst r => Real.toString r
        |  PT.String s => "\"" ^ s ^ "\""
        |  PT.Id s => s
        |  PT.Unop uexp => printUnopExp uexp
        |  PT.Binop binexp => printBinopExp binexp
        |  PT.QuestionColon (e1,e2,e3) => (expToString e1)^ " ? " ^
                                          (expToString e2)^ " : " ^
                                          (expToString e3)
        |  PT.Call (e,es) => (expToString e)^"("^(printExpList "," es) ^")"
        | PT.MARKexpression (loc,e) => expToString e
        |  _ => ""

    and printUnopExp (rator, exp) =
	case rator
        of PT.Uplus => "+ "^(expToString exp)
        |  _ => "unop(" ^ (expToString exp) ^")"

    and printBinopExp (rator, exp1, exp2) =
	case rator
        of PT.Plus => (expToString exp1)^ " + " ^(expToString exp2)
        |  PT.Minus => (expToString exp1)^ " - " ^(expToString exp2)
        |  PT.Times => (expToString exp1)^ " * " ^(expToString exp2)
        |  PT.Divide => (expToString exp1)^ " / " ^(expToString exp2)
        |  PT.Mod => (expToString exp1)^ " % " ^(expToString exp2)
        |  PT.Gt => (expToString exp1)^ " > " ^(expToString exp2)
        |  PT.Lt => (expToString exp1)^ " < " ^(expToString exp2)
        |  PT.Gte => (expToString exp1)^ " >= " ^(expToString exp2)
        |  PT.Lte => (expToString exp1)^ " <= " ^(expToString exp2)
        |  PT.Eq => (expToString exp1)^ " == " ^(expToString exp2)
        |  _ => (expToString exp1) ^ "binop"  ^ (expToString exp2)

    and printExpList s [] = ""
      | printExpList s [e] = expToString e
      | printExpList s (e::es) = ((expToString e) ^ s ^ " " ^ (printExpList s es))

end

