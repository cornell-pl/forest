structure CnvExt : CNVEXT = struct

  structure PT   = ParseTree     (* the parse tree *)
  structure PX   = ParseTreeExt  (* Pads extensions *)
  structure P    = ParseTreeUtil (* Utility functions for manipulating the parse tree *)
  structure PE   = PError        (* Error reporting utilities *)
  structure PTys = PBaseTys      (* Information about the pads base types *)
  structure PL   = PLib          (* Information about values/functions available from pads library *)
  structure PTSub= ParseTreeSubst(* Function for subtituting an expression for a string in an expression *)
  structure PPL  = PPLib

  structure TU   = TypeUtil      (* Ckit module: type utility functions *)
  structure SYM  = Symbol
  structure B    = Bindings

  type coreConversionFuns = 
	{
	 stateFuns : State.stateFuns,
	 mungeTyDecr: (Ast.ctype*ParseTree.declarator ->Ast.ctype * string option),

	 cnvType : bool*ParseTree.decltype -> Ast.ctype*Ast.storageClass,
	 cnvExpression: ParseTree.expression -> Ast.ctype * Ast.expression,
	 cnvStatement : ParseTree.statement -> Ast.statement,
	 cnvExternalDecl: ParseTree.externalDecl -> Ast.externalDecl list,

	 wrapEXPR: (Ast.ctype*Ast.coreExpression -> Ast.ctype*Ast.expression),
	 wrapSTMT: Ast.coreStatement -> Ast.statement,
	 wrapDECL: Ast.coreExternalDecl -> Ast.externalDecl,
	 evalExpr: ParseTree.expression -> 
	              (IntInf.int option * Ast.ctype * Ast.expression * bool) (* PADS *)
	 }

  type expressionExt = (ParseTree.specifier, ParseTree.declarator, ParseTree.ctype, ParseTree.decltype,
			ParseTree.operator, ParseTree.expression, ParseTree.statement)
                       ParseTreeExt.expressionExt

  type statementExt = (ParseTree.specifier, ParseTree.declarator, ParseTree.ctype, ParseTree.decltype,
		       ParseTree.operator, ParseTree.expression, ParseTree.statement)
	              ParseTreeExt.statementExt

  type externalDeclExt = (ParseTree.specifier, ParseTree.declarator, ParseTree.ctype, ParseTree.decltype,
		          ParseTree.operator, ParseTree.expression, ParseTree.statement)
	                 ParseTreeExt.externalDeclExt 

  type specifierExt = (ParseTree.specifier, ParseTree.declarator, ParseTree.ctype, ParseTree.decltype,
		       ParseTree.operator, ParseTree.expression, ParseTree.statement)
	              ParseTreeExt.specifierExt

  type declaratorExt = (ParseTree.specifier, ParseTree.declarator, ParseTree.ctype, ParseTree.decltype,
		        ParseTree.operator, ParseTree.expression, ParseTree.statement)
	               ParseTreeExt.declaratorExt

  type declarationExt = (ParseTree.specifier, ParseTree.declarator, ParseTree.ctype, ParseTree.decltype,
		        ParseTree.operator, ParseTree.expression, ParseTree.statement)
	               ParseTreeExt.declarationExt

  type extensionFuns = 
      {CNVExp: expressionExt -> Ast.ctype * Ast.expression,
       CNVStat: statementExt -> Ast.statement,
       CNVBinop: {binop: ParseTreeExt.operatorExt, arg1Expr: ParseTree.expression, arg2Expr: ParseTree.expression}
                 -> Ast.ctype * Ast.expression,
       CNVUnop: {unop: ParseTreeExt.operatorExt, argExpr: ParseTree.expression}
                 -> Ast.ctype * Ast.expression,
       CNVExternalDecl: externalDeclExt -> Ast.externalDecl list,
       CNVSpecifier: {isShadow: bool, rest : ParseTree.specifier list} 
                 -> specifierExt
                 -> Ast.ctype,
       CNVDeclarator: Ast.ctype * declaratorExt 
                 -> Ast.ctype * string option,
       CNVDeclaration: declarationExt -> Ast.declaration list}

  (****************** Abbreviations ******************************)
  type pty = PX.Pty

  type pcty = ParseTree.ctype
  type pcexp = ParseTree.expression
  type pcstmt = ParseTree.statement
  type pcdecr = ParseTree.declarator

  type acty = Ast.ctype
  type aexp = Ast.expression


  (****************** Conversion Functions ***********************)

  exception CnvExt of string

  fun CNVExp _ = raise (CnvExt "No proper extensions to expressions")

  fun CNVStat _ = raise (CnvExt "No proper extensions to statements")

  fun CNVBinop _ = raise (CnvExt "No proper extensions to binops")

  fun CNVUnop _ =  raise (CnvExt "No proper extensions to unnops")

  fun CNVSpecifier _ _ = raise (CnvExt "No proper extensions to specifiers")

  fun CNVDeclarator _ = raise (CnvExt "No proper extensions to declarators")

  fun CNVDeclaration _ = raise (CnvExt "No proper extensions to declarations")

  fun makeExtensionFuns ( {stateFuns,
			   mungeTyDecr,
			   cnvType,
			   cnvExpression,
			   cnvStatement,
			   cnvExternalDecl,
			   wrapEXPR,
			   wrapSTMT,
			   wrapDECL,
			   evalExpr}:coreConversionFuns) = 
      let 

(* Imported Values ***********************************************************)
    val {locFuns =
	 {pushLoc, popLoc, getLoc, error, warn},
	 tidsFuns =
	 {pushTids, resetTids},
	 envFuns =
	 {topLevel, pushLocalEnv, popLocalEnv, lookSym, bindSym,
	  lookSymGlobal, bindSymGlobal, lookLocalScope, getGlobalEnv},
	 uidTabFuns =
	 {bindAid, lookAid=lookAid0, bindTid, lookTid},
	 funFuns =
	 {newFunction, getReturnTy, checkLabels, addLabel, addGoto}, 
	 switchFuns =
	 {pushSwitchLabels, popSwitchLabels, addSwitchLabel, addDefaultLabel},
	 ...}
	= stateFuns
    val ttab = (#ttab (#uidTables (#globalState stateFuns)))

(* Parse tree utility********************************************************)
    fun PTisConstExp e = 
	case e of 
	    PT.EmptyExpr => (PE.bug "EmptyExpression passed to PTisConstExp.")
          |  PT.RealConst _ => true   (*XXX- ksf: generalize this to constant real expressions. *)
          | _ => (case evalExpr e of
		     (SOME _,_,_,false) => true
		   | _ => false)

(* Utility functions ********************************************************)
    fun formatComment s = 
	let val s = " "^s^" "
	    val len = String.size s
	    val line = 50
	    val space = (line-len-4)
	    val prefix = Int.div(space,2)
	    val filler = #"*" 	
	    fun padLeft s = StringCvt.padLeft filler (prefix+len) s
	    fun padRight s = StringCvt.padRight filler (space+len) s
	in
	    if space<0 then ("\n"^s^"\n")
	    else padRight (padLeft s)
	end

    fun stringOptMerge (s1Opt: string option, s2Opt:string option) =
	case s1Opt
	of NONE => s2Opt
        | SOME s1 => Option.map (fn s2=> (s1 ^". " ^ s2)) s2Opt

    fun CTtoString (ct:Ast.ctype) =  
	let val underscore = !PPL.suppressTidUnderscores
	    val          _ =  PPL.suppressTidUnderscores := true
	    val        str =  PPL.ppToString (PPAst.ppCtype () ttab) ct
	    val          _ =  PPL.suppressTidUnderscores := underscore
	in 
	    str 
	end

    fun CExptoString (acexp: Ast.expression) =
	let val underscore = !PPL.suppressTidUnderscores
	    val          _ =  PPL.suppressTidUnderscores := true
	    val        str =  PPL.ppToString (PPAst.ppExpression () ttab) acexp
	    val          _ =  PPL.suppressTidUnderscores := underscore
            val        len = String.size str
	in 
	     String.extract (str, 0, SOME (len -1))
	end

(* Error Reporting  **********************************************************)

	(* Setup the error reporting. *)
    val _ = (PE.setup (#errorState (#globalState stateFuns)) 
	     (#error (#locFuns (stateFuns))) 
	     (#warn  (#locFuns (stateFuns))))

    fun unbound sym = (case lookSym sym of
			   SOME _ => PE.fail ("Redeclaration of " ^ 
					   (Symbol.name sym))
			 | NONE   => ())



(* AST help functions ********************************************************)

    val isFunction          = TU.isFunction          ttab
    val isStructOrUnion     = TU.isStructOrUnion     ttab
    val getCoreType         = TU.getCoreType         ttab
    val equalType           = TU.equalType           ttab

    fun isAssignable (t1,t2,rhsOpt) = 
	let val isRhs0 = 
	    case rhsOpt of
		SOME (Ast.EXPR (Ast.IntConst i, _, _)) => i = IntInf.fromInt 0
	      | _ => false
	in
	    (TU.isAssignable ttab {lhs = t1, rhs = t2, rhsExpr0 = isRhs0 })
	end


    fun ASTid(sym:SYM.symbol,
	      ct:Ast.ctype,
	      isGlobal:bool,
	      status : Ast.declStatus) : Ast.id =
	{name = sym,
	 uid = Pid.new (),
	 location = getLoc (),
	 ctype = ct,
	 stClass = if isGlobal then Ast.STATIC else Ast.DEFAULT,
	 status = status, 
	 global = isGlobal,
	 kind = (if (isFunction ct) then 
		     (Ast.FUNCTION {hasFunctionDef = false})
		 else Ast.NONFUN)
	 }

    fun ASTlocalId (s,ct) = ASTid(s,ct,false,Ast.DEFINED)


    fun localInitVar (id:string,ct:Ast.ctype) : SYM.symbol * Ast.id =
	let val sym = Symbol.object id
	    val id = ASTlocalId(sym,ct)
	in
	    bindSym (sym,B.ID id);
	    (sym,id)
	end

    fun declTid tid = 
	wrapDECL (Ast.ExternalDecl (Ast.TypeDecl {shadow=NONE,
						  tid=tid})) 

    fun insTempVar (id, pcty:pcty) = 
        let val (acty, sc) = cnvType (false, P.pctToPDT pcty)
	in
	    localInitVar(id, acty)
	end
    (* Typedefs name to be ct.  Returns the related tid. Guarantees
     that this name is not previously typedef'd *)
    fun ASTtypedefGen bSym (name:string,ct:Ast.ctype): Tid.uid =
	let val sym = Symbol.typedef name
	    val _ = unbound sym
	    val tid = Tid.new ()
	    val symBinding = {name     = sym,
			      uid      = Pid.new (),
			      location = getLoc (),
			      ctype    = Ast.TypeRef tid }
	    val tidBinding = {name     = SOME name,
			      ntype    = SOME (B.Typedef(tid,ct)),
			      location = getLoc (),
			      global   = true } (*  XXX - should always be global? *)
	in
	    bSym (sym,B.TYPEDEF symBinding);
	    bindTid (tid,tidBinding);
	    tid
	end

    val ASTtypedef =ASTtypedefGen bindSym

    fun ASTmkEDeclComment s = 
	wrapDECL(Ast.ExternalDeclExt(AstExt.EComment s))

(* Ctype *********************************************************************)

    val CTint = Ast.Numeric (Ast.NONSATURATE,Ast.WHOLENUM,Ast.SIGNED,Ast.INT,
			     Ast.SIGNDECLARED)
    val CTuint = Ast.Numeric (Ast.NONSATURATE,Ast.WHOLENUM,Ast.UNSIGNED,Ast.INT,
			     Ast.SIGNDECLARED)
    val CTchar = Ast.Numeric (Ast.NONSATURATE,Ast.WHOLENUM,Ast.SIGNED,Ast.CHAR,
			     Ast.SIGNASSUMED)
    val CTuchar = Ast.Numeric (Ast.NONSATURATE,Ast.WHOLENUM,Ast.UNSIGNED,Ast.CHAR,
			     Ast.SIGNASSUMED)
    val CTintTys = [CTint, CTuint, CTchar, CTuchar]

    fun CTcnvType (ct : PT.ctype) : (acty * Ast.storageClass) 
	= cnvType(false,P.pctToPDT ct)

    datatype CTsign = Signed | Unsigned | Any
    type CTnum =  Ast.intKind * CTsign

    fun CTgetNum ct =
	(case getCoreType ct of
	     Ast.Numeric(_,_,s', ik', _) => SOME (ik', s')
	   | _ => NONE)

    fun CTisNum (ik,s) ty =
	(case getCoreType ty of
	     Ast.Numeric(_, _, s', ik', _) => 
		 (if ik' = ik then
		     (case (s,s') of 
			  (Any     , _           ) => true
			| (Signed  , Ast.SIGNED  ) => true
			| (Unsigned, Ast.UNSIGNED) => true
			| _ => false)
		  else false)
	   | _ => false)
	
    val CTisChar  = CTisNum (Ast.CHAR,Any)
    val CTisSChar = CTisNum (Ast.CHAR,Signed)
    val CTisUChar = CTisNum (Ast.CHAR,Unsigned)

    val CTisInt  = CTisNum (Ast.INT,Any)
    val CTisSInt = CTisNum (Ast.INT,Signed)
    val CTisUInt = CTisNum (Ast.INT,Unsigned)

    val CTisPointer = TU.isPointer ttab

    fun CTisString ty = 
        let val coreTy = getCoreType ty
            val isPointer = CTisPointer coreTy
            fun getBase coreTy =
                let val derefTyOpt = TU.deref ttab coreTy
                in
		    case derefTyOpt
		    of SOME(baseTy) => baseTy
                    | _ => PE.bug "Impossible: must be able to dereference a pointer.\n"
                end
        in
            isPointer andalso (CTisChar (getBase coreTy))
        end

    fun CTisStruct ty = 
	case isStructOrUnion ty
	    of SOME tid => 
		(case lookTid tid of
		     SOME {ntype = SOME(B.Struct(_)),...} => true
		   | _ => false)
	  | NONE => false

    (* Type-utils implements but does not export an essentially identical
     function!!! *)
    fun CTreduce ct = 
	(case ct of 
	     Ast.TypeRef tid =>
		 (case lookTid tid of
		      SOME {ntype = SOME (B.Typedef (_,ct)),...} => 
			  (CTreduce ct)
		    | NONE => PE.bug "Ill-formed type table."
		    | _ => ct)
	   | _ => ct)

    fun CTgetTyName ct = 
	(case ct of 
	     Ast.TypeRef tid =>
		 (case lookTid tid of
		      SOME {name,ntype = SOME (B.Typedef (_,ct)),...} => name
		    | NONE => (PE.bug "Ill-formed type table."; SOME "bogus")
		    | _ => NONE)
	   | _ => NONE)

    fun CTisEnum ty = 
        case CTreduce ty
          of Ast.Qual (_,ty) => CTisEnum ty
           | (Ast.EnumRef tid) => SOME tid
           | _ => NONE

    fun expEqualTy(expPT, CTtys, genErrMsg) = 
	let val (expTy, expAst) = cnvExpression expPT
	in
            if List.exists (fn cty => equalType(cty, expTy)) CTtys
	    then ()
	    else PE.error (genErrMsg (CTtoString expTy)) 
	end

    fun CTcnvType (ct : PT.ctype) : (acty * Ast.storageClass) 
	= cnvType(false,P.pctToPDT ct)

    fun CTcnvDecr(ct,d) : Ast.ctype * string option = 
	let val (ct', sc) = CTcnvType ct  	(* check storage class okay*)
	in                                 	(* XXX - missing piece *)
	    mungeTyDecr(ct',d)
	end


    (* The following function "decompiles" a ctype.  *)
    fun CTtoPTct (ct:acty) : PT.ctype =
	(case ct of
	     Ast.Void => P.void
	   | Ast.Ellipses => P.makePCT [PT.Ellipses]
	   | Ast.Qual (q,ct') => 
		 let val q' = (case q of 
				   Ast.CONST => PT.CONST 
				 | _ => PT.VOLATILE)
		     val {qualifiers=q'',specifiers = s''} = CTtoPTct ct'
		 in
		     { qualifiers = q' :: q'',
		       specifiers = s''
		       }
		 end		     
	   | Ast.Numeric(s,f,sgn,intk,sgntag) => 
		 let val sat = (case s of 
				    Ast.SATURATE => PT.Saturate 
				  | _ => PT.Nonsaturate)
		     val frac = (case f of 
				     Ast.FRACTIONAL => PT.Fractional
				   | Ast.WHOLENUM => PT.Wholenum)
		     fun cnvSgn Ast.SIGNED = [PT.Signed]
		       | cnvSgn Ast.UNSIGNED = [PT.Unsigned]
		     val sgn = (case sgntag of
				            Ast.SIGNASSUMED => []
     				          | Ast.SIGNDECLARED => cnvSgn sgn)
		     val ik = (case intk of
				   Ast.CHAR => [PT.Char]
				 | Ast.SHORT => [PT.Short]
				 | Ast.INT => [PT.Int]
				 | Ast.LONG => [PT.Long]
				 | Ast.LONGLONG => [PT.Long, PT.Long]
				 | Ast.FLOAT => [PT.Float]
				 | Ast.DOUBLE => [PT.Double]
				 | Ast.LONGDOUBLE => [PT.Long, PT.Double])
		     val specs = sat :: frac :: sgn @ ik
		 in
		     P.makePCT specs
		 end
	   | Ast.Array (iopt,ct') =>
		 let val e = (case iopt of 
				  NONE => PT.EmptyExpr
				| SOME (i,_) => P.int32X i) (* XXX: should get expression but it is an AST expression. *)
		     val ct'' = CTtoPTct ct'
		 in
		     P.makePCT [PT.Array(e,ct'')]
		 end
	   | Ast.Pointer ct' => P.makePCT [PT.Pointer (CTtoPTct ct')]
	   | Ast.Function (ct',cts) =>
		 let val ct'' = CTtoPTct ct'
		     fun f ct = (P.pctToPDT (CTtoPTct ct),PT.EmptyDecr)
		 in
		     P.makePCT [ PT.Function { retType = ct'',
					      params = (List.map f cts)
					      } ]
		 end
	   | Ast.StructRef t => 
		 let fun procMem (ct,mopt : Ast.member option,iopt, commentOpt) =
		     let val ct' = CTtoPTct ct 
			 val dr = 
			     case mopt of
				 NONE => PT.EmptyDecr
			       | SOME {name,...} => PT.VarDecr (SYM.name name)
			 val e = 
			     case iopt of
				 NONE => PT.EmptyExpr
			       | SOME i => P.int32X i
		     in
			 (ct',[(dr,e)], commentOpt)
		     end
		 in case lookTid t of
(*		     SOME {name=SOME n,ntype=NONE,...} =>
			 P.makePCT [PT.StructTag {isStruct=true, name=n }] *)
		     SOME {name=SOME n,...} =>
			 P.makePCT [PT.StructTag {isStruct=true, name=n }] 
		   | SOME {name=nopt,ntype=SOME (B.Struct (_,ms)), ...} =>
			 P.makePCT [PT.Struct {isStruct=true,
					      tagOpt=nopt,
					      members=List.map procMem ms}]
		   
		   | _ => PE.bug "Ill-formed type table (struct)."
		 end
	   | Ast.UnionRef t => 
		 let fun procMem (ct,m:Ast.member) =
		     let val ct' = CTtoPTct ct
			 val dr = PT.VarDecr (SYM.name (#name m))
		     in
			 (ct',[(dr,PT.EmptyExpr)], NONE)
		     end
		 in case lookTid t of
		     SOME {name=SOME n,ntype=NONE,...} => 
			 P.makePCT [PT.StructTag {isStruct=false, name=n}]
		   | SOME {name=nopt,ntype=SOME (B.Union (_,ms)),...} =>
			 P.makePCT [PT.Struct {isStruct = false,
					      tagOpt = nopt,
					      members = List.map procMem ms}
				    ]			 
		   | _ => PE.bug "Ill-formed type table (union)."
		 end
	   | Ast.EnumRef t =>
		 let fun procMem ({name,...}:Ast.member,i,commentOpt) = 
		     (SYM.name name, P.int32X i,commentOpt)
		 in case lookTid t of
		     SOME {name=SOME n,ntype=NONE,...} => 
			 P.makePCT [PT.EnumTag n]
		   | SOME {name=nopt,ntype=SOME (B.Enum (_,ms)),...} =>
			 P.makePCT [PT.Enum {tagOpt = nopt,
					    enumerators = List.map procMem ms,
					    trailingComma = false}]
		   | _ => PE.bug "Ill-formed type table (enum)."
		 end
	   | Ast.TypeRef t =>
		 let in case lookTid t of
		     SOME {name= SOME n,...} => P.makePCT [PT.TypedefName n]
		   | _ => PE.bug "Ill-formed type table (typedef)."
		 end
	   | Ast.Error => PE.fail "Error type found."
	     )


(* Conversions ***************************************************************)
      fun pcnvExternalDecl decl = 
	  let (* Some useful names *)
              val nerr      = "nerr"
              val errCode   = "errCode"
              val loc       = "loc"
              val panic     = "panic"
	      val ts = "ts"
	      val em = "em" 
	      val ed = "ed"
	      val disc = "disc"
	      val rep = "rep"
	      val ted = "ted"
	      val tem = "tem"
	      val all = "structLevel"


		      
	      (* Some useful functions *)
	      fun repSuf  s = s (* Make rep type same as pads name; s^"_rep" *)
              fun emSuf   s = s^"_em"
              fun edSuf   s = s^"_ed"
              fun readSuf s = s^"_read"
              fun iSuf s = s^"_internal"
	      fun gTemp base = "tmp"^base
	      fun gMod  base = "mod"^base
	      fun lookupTy (ty:pty, sufFun:string->string, fldSelect:PTys.baseInfoTy ->Atom.atom) = 
                  case ty 
                  of PX.Name s => ( case PTys.find(PTys.baseInfo, Atom.atom s) 
                                    of NONE => (sufFun s)
			            | SOME (b:PTys.baseInfoTy) => Atom.toString(fldSelect b))
              fun lookupScan(ty:pty) = 
		  case ty
                  of PX.Name s => ( case PTys.find(PTys.baseInfo, Atom.atom s)
				    of NONE => NONE
                                    |  SOME(b:PTys.baseInfoTy) => #scanname b)
              fun mungeParam(pcty:pcty, decr:pcdecr) : string * pcty = 
		  let val (act, nOpt) = CTcnvDecr(pcty, decr)
                      (* convert padsc name to c name, if a pads typedef *)
                      val pct = case CTgetTyName act
			        of NONE => CTtoPTct act
                                | SOME tyName => 
				    P.makeTypedefPCT(lookupTy(PX.Name tyName, repSuf, #repname))

                      val name = case nOpt
			         of NONE => (PE.error "Parameters to PADSC data types must have names.\n"; 
					     "bogus")
				 | SOME n => n
		  in
                      (name, pct)
		  end
	      fun fieldX (bsName, fName) = P.arrowX(PT.Id(gMod bsName), PT.Id fName)
	      fun genLocTemp (pcty, paramName, firstTyopt) = 
                  let val initX = case firstTyopt of NONE => P.zero
                                     | SOME ty => PT.Cast(ty,P.zero) 
		  in
		      [P.varDeclS(pcty, gTemp paramName, PT.InitList [initX]),
		       P.varDeclS(P.ptrPCT pcty, gMod paramName, PT.Id paramName)]
		  end
	      fun genLocInit paramName =
		  PT.IfThen(P.notX(PT.Id(gMod paramName)),
			    PT.Compound[
 			      P.assignS(PT.Id (gMod paramName), P.addrX(PT.Id(gTemp paramName)))])

	      fun genReadFun (readName, cParams:(string * pcty)list, 
			      emPCT,edPCT,canonicalPCT, emFirstPCT, hasNErr, bodySs) = 
		  let val iReadName = iSuf readName			  
		      val (cNames, cTys) = ListPair.unzip cParams
                      val paramTys = [P.ptrPCT PL.toolStatePCT, P.ptrPCT emPCT]
			             @ cTys
			             @ [P.ptrPCT edPCT, P.ptrPCT canonicalPCT, P.ptrPCT PL.toolDiscPCT]
                      val paramNames = [ts, em] @ cNames @ [ed,rep,disc]
		      val iParamNames = [ts, gMod em] @ cNames @ [gMod ed, gMod rep, disc]
                      val formalParams = List.map P.mkParam (ListPair.zip (paramTys, paramNames))
		      val iFormalParams = List.map P.mkParam (ListPair.zip (paramTys, iParamNames))
                      val paramArgs = List.map PT.Id iParamNames
		      val incNerrSs = if hasNErr then
			              [P.assignS(P.arrowX(PT.Id(gMod(ed)), PT.Id nerr), P.zero)]
				      else []
		      val returnTy =  PL.toolErrPCT

                      (* -- internal entry point function *)
		      val readFunInternalED = 
			  P.mkFunctionEDecl(iReadName, iFormalParams, PT.Compound bodySs, returnTy)
                      (* -- external entry point function *)
		      val decls =   genLocTemp(canonicalPCT, rep, NONE) 
			          @ genLocTemp(emPCT, em, emFirstPCT) 
			          @ genLocTemp(edPCT, ed, NONE)
		      val initDecls =  [genLocInit rep, genLocInit em, genLocInit ed]
				     @ incNerrSs  
				     @ [P.assignS(P.arrowX(PT.Id(gMod(ed)), PT.Id panic), P.falseX)]
                      val callIntSs = [PT.Return (PT.Call(PT.Id iReadName, paramArgs))]
		      val bodySs' = decls @ initDecls @ callIntSs
		      val bodyS = PT.Compound bodySs'
		      val readFunED = P.mkFunctionEDecl(readName, formalParams, bodyS, returnTy)
		  in
		      [readFunInternalED, readFunED]
		  end

	      fun checkParamTys (fieldName, functionName, extraargs, numBefore, numAfter) = 
		  let val (eaty, _) = cnvExpression (PT.Id functionName)
		      val fargtysOpt = case eaty
			  of Ast.Pointer(Ast.Function (retTy, argTys)) => (
			      (SOME (List.take(List.drop(argTys, numBefore), 
			                  (List.length argTys) - numAfter - numBefore)))
				  handle Subscript => NONE)
			| _ =>   NONE (* error, to be reported later *)
		      val aargtys = #1( ListPair.unzip (List.map cnvExpression extraargs))
		      fun match([], []) = true
			| match(fty::ftys, aty::atys) = 
			  isAssignable(fty,aty,NONE) andalso
			  match(ftys,atys)
			| match _ = false
		      val errMsg = "Actual argument(s) for field "^
			  fieldName ^" did not have expected type(s)."
		  in
		      case fargtysOpt
			  of NONE => (PE.error errMsg)
			| SOME fargtys => (
					   if not (match(fargtys, aargtys))
					       then (PE.error errMsg)
					   else ()
		  (* end case *))
		  end
                                      
              fun reportErrorSs(shouldIncNerr, errCodeC, shouldPrint, msg, args) = 
		  let val locX = P.addrX(fieldX(ed,loc))
                      val errCodeX = fieldX(ed,errCode)
		      val msgX = if msg = "" then P.zero else PT.String msg
		      val nErrSs = if shouldIncNerr 
			           then [P.postIncS (fieldX(ed,nerr))]
				   else []
                      val printSs = if shouldPrint 
				    then [PL.userErrorS(PT.Id ts, PT.Id disc, locX, 
							errCodeX, msgX, args)]
				    else []
		  in
                     nErrSs
                    @[P.assignS(fieldX(ed,errCode), errCodeC),
                       PL.getLocS(PT.Id ts,locX,PT.Id disc)]
                    @ printSs
		  end

              fun getEMExp(exp:pcexp) = 
		  let val (expTy,_) = cnvExpression exp
		  in
		      if CTisStruct expTy then P.dotX(exp, PT.Id all)
		      else exp
		  end

              (* handles problem if first element of an initializer is an enumerated type *)
              fun getFirstEMPCT emFields = 
		  case emFields
		  of [] => NONE
		  | ((_, ty, _)::fs) => 
		      let val aty = #1 (CTcnvType ty)
		      in
			  if Option.isSome(CTisEnum aty) 
			      then SOME ty else NONE
		      end

	      fun cnvPTypedef ({name : string, params: (pcty * pcdecr) list, 
			        baseTy: PX.Pty, args: pcexp list, 
			        predTy: PX.Pty, thisVar: string, pred: pcexp}) = 
		  let val base = "base"
		      val user = "user"
		      (* Generate canonical representation: typedef to base representation *)
		      val baseTyPCT = P.makeTypedefPCT(lookupTy(baseTy, repSuf, #repname))
		      val canonicalStructED = P.makeTyDefEDecl (baseTyPCT, repSuf name)
		      val canonicalDecls = cnvExternalDecl canonicalStructED 
                      val canonicalPCT = P.makeTypedefPCT (repSuf name)			 

                      (* Generate error mask *)
		      val baseEMPCT = P.makeTypedefPCT(lookupTy(baseTy,emSuf, #emname))
                      val emFields  = [(base, baseEMPCT, SOME "Base error mask"),
				       (user, PL.base_emPCT, SOME "User constraint")]
		      val emED      = P.makeTyDefStructEDecl (emFields, emSuf name)
		      val emDecls   = cnvExternalDecl emED
                      val emPCT     = P.makeTypedefPCT (emSuf name)		

                      (* Generate error description *)
		      val baseEDPCT = P.makeTypedefPCT(lookupTy(baseTy,edSuf, #edname))
                      val edFields  = [(nerr, P.int, NONE), (errCode, P.int, NONE),
				       (loc, PL.locPCT,NONE), (panic, P.int, NONE),
				       (base, baseEDPCT, SOME "Base error description")]
		      val edED      = P.makeTyDefStructEDecl (edFields, edSuf name)
		      val edDecls   = cnvExternalDecl edED
                      val edPCT     = P.makeTypedefPCT (edSuf name)		

                      (* Generate read function *)
                      (* -- Some helper functions *)
		      val readName = readSuf name
                      val baseReadFun = lookupTy(baseTy, iSuf o readSuf, #readname)
		      val () = checkParamTys(name, baseReadFun, args, 2, 3)
		      val modPredX = PTSub.substExp (thisVar, P.starX(PT.Id (gMod rep)), pred)

                      fun genReadSs () = 
			  let 
			      val () = expEqualTy(modPredX, CTintTys, 				
					  fn s=> (" constraint for typedef "^
						  name ^ " has type: " ^ s ^
						  ". Expected an int."))
			      val readBaseSs = 
				  [PT.IfThen( 
					     PL.readFunChkX(PL.PDC_ERROR, 
							    baseReadFun, 
							    PT.Id ts, 
							    P.addrX (fieldX(em,base)),
							    args,
							    P.addrX (fieldX(ed,base)),
							    PT.Id (gMod rep), PT.Id disc),
					     PT.Return PL.PDC_ERROR)]

			      val checkConstraintSs = 
				  [PT.IfThen(
					     P.andX(P.lteX(fieldX(em,user), PL.EM_CHECK),
						    P.notX modPredX),
					     PT.Compound (reportErrorSs(true,
									PL.PDC_TYPEDEF_CONSTRAINT_ERR,
									true,"", [])
							  @ [PT.Return PL.PDC_ERROR])
					     )]
			      val okSs = [PT.Return PL.PDC_OK]
		      in
			  readBaseSs @ checkConstraintSs @ okSs
		      end

                      (* -- Assemble read function *)
		      val _ = pushLocalEnv()                                        (* create new scope *)
		      val () = ignore (insTempVar(gMod rep, P.ptrPCT canonicalPCT)) (* add modrep to scope *)
		      val cParams : (string * pcty) list = List.map mungeParam params
                      val () = ignore (List.map insTempVar cParams)  (* add params for type checking *)
		      val readFields = genReadSs ()                                   (* does type checking *)
		      val _ = popLocalEnv()                                         (* remove scope *)
		      val bodySs = readFields 
		      val readFunEDs = genReadFun(readName, cParams, emPCT,edPCT,canonicalPCT, 
						  NONE, true, bodySs)
		  in
		        canonicalDecls
                      @ emDecls
                      @ edDecls
                      @ List.concat(List.map cnvExternalDecl readFunEDs)
		  end

	      fun cnvPStruct ({name:string, params: (pcty * pcdecr) list, fields : pcexp PX.PSField list}) = 
	          let (* Functions for walking over lists of struct elements *)
		      fun mungeField f b (PX.Full fd) = f fd
                        | mungeField f b (PX.Brief e) = b e
		      fun mungeFields f b [] = []
			| mungeFields f b (x::xs) = (mungeField f b x) @ (mungeFields f b xs)

		      (* Generate local variables  *)
		      fun genLocFull {pty :PX.Pty, args : pcexp list, name:string, isVirtual:bool, 
				      pred:pcexp option, comment:string option} = 
			  if not isVirtual then []
			  else [(name, P.makeTypedefPCT(lookupTy (pty,repSuf,#repname)))]
		      fun genLocBrief e = []
		      val localVars = mungeFields genLocFull genLocBrief fields

		      (* Generate canonical representation *)
		      fun genRepFull {pty :PX.Pty, args : pcexp list, name:string, isVirtual:bool, 
				      pred:pcexp option, comment:string option} = 
			  if not isVirtual then 
			    let val predStringOpt = Option.map P.expToString pred
			        val fullCommentOpt = stringOptMerge(comment, predStringOpt)
			    in
			      [(name,P.makeTypedefPCT(lookupTy (pty,repSuf,#repname)), fullCommentOpt )]
			    end
			  else []
		      fun genRepBrief e = []
		      val canonicalFields = mungeFields genRepFull genRepBrief fields
		      val canonicalStructED = P.makeTyDefStructEDecl (canonicalFields, repSuf name)
		      val canonicalDecls = cnvExternalDecl canonicalStructED 
                      val canonicalPCT = P.makeTypedefPCT (repSuf name)			 
		       
		      (* Generate error mask *)
		      fun genEMFull {pty :PX.Pty, args : pcexp list, 
				     name:string, isVirtual:bool, pred:pcexp option, comment} = 
			  [(name,P.makeTypedefPCT(lookupTy (pty,emSuf,#emname)), NONE)]
		      fun genEMBrief e = []
		      val emFieldsNested = mungeFields genEMFull genEMBrief fields
		      val auxEMFields = [(all, PL.base_emPCT, NONE)]
		      val emFields = auxEMFields @ emFieldsNested

		      val emFirstPCT = getFirstEMPCT emFields
		      val emStructED = P.makeTyDefStructEDecl (emFields, emSuf name)
		      val emDecls = cnvExternalDecl emStructED 
                      val emPCT = P.makeTypedefPCT (emSuf name)			  

		      (* Generate error description *)
		      fun genEDFull {pty :PX.Pty, args : pcexp list,
				     name:string,  isVirtual:bool, pred:pcexp option, comment} = 
			  [(name,P.makeTypedefPCT(lookupTy (pty,edSuf,#edname)),NONE)]
		      fun genEDBrief e = []
		      val auxEDFields = [(nerr, P.int,NONE), (errCode, P.int,NONE),
					 (loc, PL.locPCT,NONE), (panic, P.int,NONE)]
		      val edFields = auxEDFields @ (mungeFields genEDFull genEDBrief fields)
		      val edStructED = P.makeTyDefStructEDecl (edFields, edSuf name)
                      val edPCT = P.makeTypedefPCT (edSuf name)			  

                      (* Generate read function *)
                      (* -- Some useful names *)
		      val readName = readSuf name

                      (* -- collection of expressions to be substituted for in constraints *)
                      (* -- efficiency could be improved with better representations *)
                      val subList : (string * pcexp) list ref = ref []
                      fun addSub (a : string * pcexp) = subList := (a:: (!subList))

                      (* -- Some helper functions *)
		      fun genReadFull {pty :PX.Pty, args:pcexp list,
				       name:string, isVirtual:bool, pred:pcexp option, comment} = 
			  let val readFieldName = lookupTy(pty, iSuf o readSuf, #readname)
                              val modEdNameX = fieldX(ed,name)
			      val repX = if isVirtual then PT.Id name else fieldX(rep,name)
                              val () = if not isVirtual 
					   then addSub(name, fieldX(rep,name))  (* record additional binding *)
				       else ()
			      val modArgs = List.map (PTSub.substExps (!subList)) args
                              val () = checkParamTys(name, readFieldName, modArgs, 2, 3)
			      val commentS = P.mkCommentS ("Reading field: "^ name )
			      val readS = 
			      PT.IfThenElse
                                 (fieldX(ed,panic), (* if moded->panic *)
				  PT.Compound 
                                   [(* moded->name.panic = true *)
				    P.assignS(P.dotX(modEdNameX, PT.Id panic),P.trueX),  
				    (* moded->name.errCode = PANIC_SKIPPED *)
				    P.assignS(P.dotX(modEdNameX, PT.Id errCode),PL.PDC_PANIC_SKIPPED),  
                                    PL.getLocS(PT.Id ts,  (* PDC_get_loc(ts, &moded->name.loc, disc) *)
					       P.addrX(P.dotX(modEdNameX,PT.Id loc)),
					       PT.Id disc),
				    (* moded->nerr += 1 *)
				    P.plusAssignS(fieldX (ed,nerr),P.intX 1)],
                                  (* if PDC_ERROR = readFieldName(ts, &em->name, &ed->name, 
				                                  &res->name, disc) *)
                                  PT.Compound [
                                   PT.IfThenElse
                                    (P.eqX(PL.PDC_ERROR,
					   PL.readFunX(readFieldName, 
						       PT.Id ts, 
						       P.addrX(fieldX(em,name)),
						       modArgs,
						       P.addrX(fieldX(ed,name)),
						       P.addrX repX, PT.Id disc)),
				     PT.Compound[ (* error reading field *)
				      (* if (moded->name.panic) *)
                                      PT.IfThen(P.dotX(fieldX(ed, name), PT.Id panic),
 				        (* moded->panic = true *)
				        PT.Compound[P.assignS(fieldX(ed,panic), P.trueX)]),
                                      (* if (moded->nerr == 0) *)
                                      PT.IfThen(P.eqX(P.zero, fieldX(ed,nerr)), 
                                       PT.Compound [
					(* moded->errCode = PDC_STRUCT_FIELD_ERR *)
                                        P.assignS(fieldX(ed, errCode), PL.PDC_STRUCT_FIELD_ERR ),
                                        (* moded->loc = moded->name.loc; *)
					P.assignS(fieldX(ed, loc), P.dotX(fieldX(ed, name), PT.Id loc))
                                       ]),
				      (* moded->nerr += 1 *)
				      P.plusAssignS(fieldX(ed,nerr), P.intX 1)],
				     PT.Compound(* else no error reading field *)
                                      (* If user supplied constraint, check that constraint *)
                                      (case pred 
                                       of NONE => []
                                       | SOME exp => 
                                           let val exp = PTSub.substExps (!subList) exp
					       val () = expEqualTy(exp, CTintTys, 
								 fn s=> ("Constraint for field "^
								  name ^ " " ^
								  "does not have integer type."))
					   in
					       [(* if ((modem->name <= Check) && (!(exp))) *)
						PT.IfThen(
                                                 P.andX(P.lteX(getEMExp(fieldX(em,name)), PL.EM_CHECK),
							P.notX exp),
						 PT.Compound[
						   (* moded->name.errCode = PDC_STRUCT_FIELD_ERROR; *)
                                                   P.assignS(P.dotX(fieldX(ed,name), PT.Id errCode), 
								    PL.PDC_STRUCT_FIELD_ERR),
						   (* PDC_get_loc(ts, &(moded->name.loc), disc);  *)
					           PL.getLocS(PT.Id ts,
							      P.addrX(P.dotX(modEdNameX,PT.Id loc)),
							      PT.Id disc),
                                                   (* PDC_report_err(ts,disc,&(moded->name.loc),
						                     moded->name.errCode, 
								     "constraint violated") *)
                                                   PL.userErrorS(PT.Id ts, PT.Id disc, 
								 P.addrX(P.dotX(fieldX(ed,name), PT.Id loc)),
								 P.dotX(fieldX(ed,name), PT.Id errCode),
								 PT.String("User constraint on field "^
									name ^ " " ^
									"violated."), []),
						   (* if (0 == moded->nerr) *)
                                                   PT.IfThen(P.eqX(P.zero, fieldX(ed,nerr)),
						    PT.Compound[
						      (* moded->errCode = USER_CONSTRAINT_VIOLATION *)
                                                      P.assignS(fieldX(ed,errCode), 
								    PL.PDC_USER_CONSTRAINT_VIOLATION),
                                                      (* moded->loc = moded->name.loc *)
                                                      P.assignS(fieldX(ed,loc), 
								P.dotX(fieldX(ed,name),PT.Id loc))
                                                      ]
                                                   ),
						   (* moded->nerr += 1 *)
						   P.plusAssignS(fieldX(ed,nerr), P.intX 1)
						   ]
						)]
					   end
                                      )
                                     )])
			  in
			      [commentS, readS]
			  end

		      fun genReadBrief e = 
			  let val e = PTSub.substExps (!subList) e
			      val (expTy, expAst) = cnvExpression e
			      fun getCharComment eX = 
				  let val cval = #1(evalExpr eX)
				  in
				      case cval of NONE => CExptoString expAst
				      | SOME e => (Char.toCString(Char.chr (IntInf.toInt e))
					  	   handle _ => CExptoString expAst)
				  end
			      fun getStrLen eX = 
				  case eX of PT.String s => P.intX (String.size s)
                                  | PT.MARKexpression(l,e) => getStrLen e
				  | _ => PL.strLen eX
			      val (pTyName, litdecls,expr,commentV) = 
				  if CTisInt expTy then (PL.charlit, [], e, getCharComment e)
				  else if CTisString expTy 
				       then (PL.strlit,
					     [P.varDeclS(PL.stringPCT, "strlit", 
							 PT.InitList[P.zero, e]),
					      P.assignS(P.dotX(PT.Id "strlit", PT.Id "len"), getStrLen e)],
					     P.addrX(PT.Id "strlit"), CExptoString expAst)
				  else (PE.error ("Currently only characters and strings "^
					          "supported as delimiters.");
					(PL.charlit, [], e, CExptoString expAst))
			      val commentS = P.mkCommentS ("Reading delimiter field: "^
						           commentV)
                              (* base_ed ted; *)
			      val tedDecl = P.varDeclS'(PL.base_edPCT, ted)
			      fun genPanicRecovery (pTyName:string) : pcstmt list -> pcstmt list = 
                                  case PTys.find(PTys.baseInfo, Atom.atom pTyName)
                                  of NONE => (fn id => id)     (* don't know how to recover *)
                                  | SOME(b:PTys.baseInfoTy) => (
                                      case (#scanname b) 
                                      of NONE => (fn id => id) (* don't know how to recover *)
                                      | SOME a => 
                                        (fn elseSs =>
                                           [PT.IfThenElse((* if (moded->panic) *)
                                              fieldX(ed,panic),
					      PT.Compound [
                                                (* base_em tmask = Ignore; *)
						P.varDeclS'(PL.sizePCT, "n"),
						PT.IfThen(
						 P.neqX(PL.PDC_ERROR,
						       PL.scanFunX(Atom.toString a, PT.Id ts, 
								   expr, expr, 
								   P.zero,
                                                                   P.addrX (PT.Id "n"), 
								   PT.Id disc)),
						 (* moded->panic = false *)
						 PT.Compound[P.assignS(fieldX(ed,panic),P.falseX)])
                                              ], 
                                              PT.Compound elseSs
                                           )]
				        (* end SOME a *))
                                      (* end SOME b *))
			      val readFieldName = lookupTy(PX.Name pTyName, iSuf o readSuf, #readname)
                              val notPanicSs = 
                                  [(* base_em tem = Check *)
				   P.varDeclS(PL.base_emPCT, tem, PL.EM_CHECK),
                                   PT.IfThen( (* PDC_ERROR == readFieldName(ts, &tem, &ted, e,disc) *)
                                             PL.readFunChkX(PL.PDC_ERROR, 
							    readFieldName, 
							    PT.Id ts, 
							    P.addrX (PT.Id tem),
							    [],
							    P.addrX (PT.Id ted),
							    expr, PT.Id disc),
                                     PT.Compound[
				       (* PDC_report_err(ts,disc, &ted.loc, 
					                   MISSING_LITERAL,"missing separator", e) *)
				       PL.userErrorS(PT.Id ts, PT.Id disc, 
						     P.addrX(P.dotX(PT.Id ted, PT.Id loc)),
						     PL.PDC_MISSING_LITERAL,
						     PT.String "Missing separator: %s.", 
						     [PT.String commentV]),
				      PT.IfThen((* if (0 == moded->nerr) *)
                                       P.eqX(P.zero, fieldX(ed,nerr)),
					     PT.Compound[
				              (* moded->errCode = MISSING_LITERAL *)
					      P.assignS(fieldX(ed,errCode), 
							PL.PDC_MISSING_LITERAL),
                                              (* moded->loc = ted.loc *)
                                              P.assignS(fieldX(ed,loc), 
							P.dotX(PT.Id ted,PT.Id loc))
                                             ]),
				      (* moded->nerr += 1 *)
				      P.plusAssignS(fieldX(ed,nerr), P.intX 1),
				      (* moded->panic = true *)
				      P.assignS(fieldX(ed,panic),P.trueX)
                                     ]
				   )]
			  in
			      [PT.Compound(
                                   [commentS, 
				    PT.Compound(
				     tedDecl 
				     :: litdecls
				     @ (genPanicRecovery pTyName notPanicSs))])]
			  end

                      (* -- Assemble read function *)
		      val _ = pushLocalEnv()                                        (* create new scope *)
		      val () = ignore (insTempVar(gMod rep, P.ptrPCT canonicalPCT)) (* add modrep to scope *)
		      val () = ignore (insTempVar(gMod em,  P.ptrPCT emPCT))        (* add modem to scope *)
		      val () = ignore(List.map insTempVar localVars)                (* insert virtuals into scope *)
		      val cParams : (string * pcty) list = List.map mungeParam params
                      val () = ignore (List.map insTempVar cParams)  (* add params for type checking *)
		      val readFields = mungeFields genReadFull genReadBrief fields  (* does type checking *)
		      val _ = popLocalEnv()                                         (* remove scope *)
		      val localDeclSs = List.map (P.varDeclS' o (fn(x,y) => (y,x))) localVars
		      val returnS = P.returnS (
				      P.condX(P.eqX(P.arrowX(PT.Id (gMod(ed)), PT.Id nerr),P.zero),
				      PL.PDC_OK, PL.PDC_ERROR))
		      val bodySs = localDeclSs @ readFields @ [returnS]

		      val readFunEDs = genReadFun(readName, cParams, emPCT,edPCT,canonicalPCT, 
						  emFirstPCT, true, bodySs)
	      in 
 		   canonicalDecls (* converted earlier because used in typechecking constraints *)
                 @ emDecls
                 @ cnvExternalDecl edStructED
                 @ (List.concat(List.map cnvExternalDecl readFunEDs))
	      end

	     fun cnvPUnion ({name:string, params: (pcty * pcdecr) list, variants : pcexp PX.PSField list}) = 
		 let (* Some useful names *)
                     val value = "val"
		     val tag = "tag"
		     fun tgSuf s = s^"_tag"
		     fun unSuf s = s^"_u"

		     (* Functions for walking over list of variants *)
		     fun mungeVariant f b (PX.Full fd) = f fd
		       | mungeVariant f b (PX.Brief e) = b e
		     fun mungeVariants f b [] = []
		       | mungeVariants f b (x::xs) = (mungeVariant f b x) @ (mungeVariants f b xs)

                     (* generate enumerated type describing tags *)
		     fun genTagFull {pty :PX.Pty, args : pcexp list, 
				     name:string, isVirtual:bool, pred:pcexp option, comment:string option} = 
			 [(name,PT.EmptyExpr,NONE)]

		     fun genTagBrief e = []
		     val tagFields = mungeVariants genTagFull genTagBrief variants
		     val tagED = P.makeTyDefEnumEDecl(tagFields, tgSuf name)
		     val tagDecls = cnvExternalDecl tagED
		     val tagPCT = P.makeTypedefPCT(tgSuf name)

                     (* generate canonical representation *)
		     fun genRepFull {pty :PX.Pty, args : pcexp list, 
				     name:string, isVirtual:bool, pred:pcexp option, comment:string option} = 
			 let val predStringOpt = Option.map P.expToString pred
			     val fullCommentOpt = stringOptMerge(comment, predStringOpt)
			 in
			     [(name,P.makeTypedefPCT(lookupTy (pty,repSuf,#repname)), fullCommentOpt )]
			 end
		     fun genRepBrief e = (PE.error "Unions do not currently support brief fields.\n"; [])
		     val canonicalVariants = mungeVariants genRepFull genRepBrief variants
		     val unionED = P.makeTyDefUnionEDecl(canonicalVariants, unSuf name)
		     val unionDecls = cnvExternalDecl unionED
                     val unionPCT = P.makeTypedefPCT(unSuf name)
                     val structFields = [(tag, tagPCT, NONE),
					 (value, unionPCT, NONE)]
		     val canonicalStructED = P.makeTyDefStructEDecl (structFields, repSuf name)
		     val canonicalDecls = cnvExternalDecl canonicalStructED 
                     val canonicalPCT = P.makeTypedefPCT (repSuf name)			 

		      (* Generate error mask *)
		     fun genEMFull {pty :PX.Pty, args : pcexp list, 
				    name:string, isVirtual:bool, pred:pcexp option, comment} = 
			 [(name,P.makeTypedefPCT(lookupTy (pty,emSuf,#emname)), NONE)]
		     fun genEMBrief e = []
		     val emFields = mungeVariants genEMFull genEMBrief variants
		     val emFirstPCT = getFirstEMPCT emFields
		     val emStructED = P.makeTyDefStructEDecl (emFields, emSuf name)
		     val emPCT = P.makeTypedefPCT (emSuf name)			  

		     (* Generate error description *)
		     fun genEDFull {pty :PX.Pty, args : pcexp list,
				    name:string, isVirtual:bool, pred:pcexp option, comment} = 
			 [(name,P.makeTypedefPCT(lookupTy (pty,edSuf,#edname)),NONE)]
		     fun genEDBrief e = []
		     val auxEDFields = [(nerr, P.int,NONE), (errCode, P.int,NONE),
					(loc, PL.locPCT,NONE), (panic, P.int,NONE)]
		     val edFields = auxEDFields @ (mungeVariants genEDFull genEDBrief variants)
		     val edStructED = P.makeTyDefStructEDecl (edFields, edSuf name)
		     val edPCT = P.makeTypedefPCT (edSuf name)			  

                     (* Generate read function *)
                     (* -- Some useful names *)
		     val readName = readSuf name

                     (* -- some helper functions *)
                     fun genReadFull{pty :PX.Pty, args:pcexp list,
				       name:string, isVirtual:bool, pred:pcexp option, comment} = 
			  let val readFieldName = lookupTy(pty, iSuf o readSuf, #readname)
                              val () = checkParamTys(name, readFieldName, args, 2, 3)
                              val predXOpt = case pred of NONE => NONE
				       | SOME constraint => ( 
 			                   (SOME (PTSub.substExp (name, P.dotX(fieldX(rep,value),PT.Id name),
								   constraint)))
					   before
				           expEqualTy(constraint, CTintTys, 
						      fn s=> (" constraint for variant "^
							      name ^ " has type: " ^ s ^
							      ". Expected an int.")))
			      val commentS = P.mkCommentS ("Reading field: "^ name )
			      val foundItSs = PT.Compound(
					       PL.commitS(PT.Id ts, PT.Id disc)
					       @[P.assignS(fieldX(rep,tag),PT.Id name),
					         PT.Return PL.PDC_OK])
			      fun doConstraint predX = case predX of NONE => foundItSs
				  | SOME constraint => PT.Compound[
                                          PT.IfThenElse(
                                           P.andX(P.lteX(fieldX(em,name), PL.EM_CHECK),
						  P.notX constraint),
					   PT.Compound(PL.restoreS(PT.Id ts, PT.Id disc)),
					   foundItSs
					  )]

			      val readS = 
				    PL.chkPtS(PT.Id ts, PT.Id disc)
				  @[PT.IfThenElse(
				      PL.readFunChkX(
					 PL.PDC_ERROR, readFieldName, PT.Id ts, 
					 P.addrX(fieldX(em,name)), args, 
					 P.addrX(fieldX(ed,name)),
					 P.addrX(P.dotX(fieldX(rep,value), PT.Id name)),
					 PT.Id disc),
				       PT.Compound (PL.restoreS(PT.Id ts, PT.Id disc)),
				       doConstraint predXOpt)
				    ]
			  in
			      [commentS] @  readS
			  end

                     fun genReadBrief _ = []

		     val cleanupSs =  [P.mkCommentS("We didn't match any branch")]
			             @ reportErrorSs(true,
					PL.PDC_UNION_MATCH_FAILURE,
					true, 
					("Did not match any branch of union "^name^"."),
					[])
			             @ [P.assignS(fieldX(ed,panic), P.trueX),
					PT.Return PL.PDC_ERROR]


                     (* -- Assemble read function *)
		     val _ = pushLocalEnv()                                        (* create new scope *)
		     val () = ignore (insTempVar(gMod rep, P.ptrPCT canonicalPCT)) (* add modrep to scope *)
		     val cParams : (string * pcty) list = List.map mungeParam params
                     val () = ignore (List.map insTempVar cParams)  (* add params for type checking *)
		     val readFields = mungeVariants genReadFull genReadBrief variants  (* does type checking *)
		     val _ = popLocalEnv()                                         (* remove scope *)
		     val bodySs = readFields @ cleanupSs
		     val readFunEDs = genReadFun(readName, cParams,emPCT,edPCT,canonicalPCT, 
						 emFirstPCT, true, bodySs)

		 in
		       tagDecls
		     @ unionDecls
		     @ canonicalDecls
	             @ cnvExternalDecl emStructED
	             @ cnvExternalDecl edStructED
	             @ (List.concat (List.map cnvExternalDecl readFunEDs))
		 end
	  
             fun cnvPArray {name:string, params : (pcty * pcdecr) list, args : pcexp list, baseTy:PX.Pty, 
			    sizeSpec:pcexp PX.PSize option, constraints: pcexp PX.PConstraint list} =
	     let val length = "length"
		 val element = "element"
                 val array = "array"
                 val neerr = "neerr"
                 val firstError = "firstError"
                 val elemRepPCT = P.makeTypedefPCT(lookupTy(baseTy, repSuf, #repname))
                 val elemEdPCT  = P.makeTypedefPCT(lookupTy(baseTy, edSuf, #edname))
                 val elemEmPCT  = P.makeTypedefPCT(lookupTy(baseTy, emSuf, #emname))
                 val elemReadName = lookupTy(baseTy, iSuf o readSuf, #readname)

                 (* Some useful functions *)
                 fun recordArrayErrorS (errCodeC, shouldPrint,msg,args, setPanic) = 
                     PT.Compound([
  		       PT.IfThenElse(P.notX(fieldX(ed,nerr)),
			  PT.Compound (reportErrorSs(true,errCodeC,shouldPrint,msg,args)),
			  PT.Compound[P.postIncS(fieldX(ed,nerr))])]
                       @ (if setPanic then [P.assignS(fieldX(ed,panic),P.trueX)] else []))
  

                 fun amCheckingE(SOME testE) = 
                     P.andX(P.lteX(fieldX(em,array), PL.EM_CHECK), testE)
                   | amCheckingE(NONE) = P.lteX(fieldX(em,array), PL.EM_CHECK)

		 (* Generate canonical representatin *)
		 val canonicalFields = [(length, P.int, NONE), 
				        (name, P.ptrPCT elemRepPCT, NONE)]
		 val canonicalStructED = P.makeTyDefStructEDecl (canonicalFields, repSuf name)
		 val canonicalDecls = cnvExternalDecl canonicalStructED 
		 val canonicalPCT = P.makeTypedefPCT (repSuf name)			 

	         (* Generate error mask *)
		 val emFields = [(element, P.makeTypedefPCT(lookupTy(baseTy, emSuf, #emname)),
				  SOME "per-element checks"),
				 (array,   PL.base_emPCT, SOME "entire array checks")]
		 val emStructED = P.makeTyDefStructEDecl (emFields, emSuf name)
		 val emPCT = P.makeTypedefPCT (emSuf name)			  

	         (* Generate error description *)
                 val edFields = [(nerr, P.int,    SOME "Number of array errors"), 
				 (errCode, PL.errCodePCT, NONE),
				 (neerr, P.int,   SOME "Number of element errors"), 
				 (loc, PL.locPCT, NONE), 
				 (panic, P.int,   NONE),
				 (firstError, P.int, 
				    SOME "if errCode == ARRAY_ELEM_ERR, index of first error"),
				 (length, P.int, NONE),
				 (name, P.ptrPCT(P.makeTypedefPCT(lookupTy(baseTy, edSuf, #edname))), NONE)]
		 val edStructED = P.makeTyDefStructEDecl (edFields, edSuf name)
		 val edPCT = P.makeTypedefPCT (edSuf name)			  

		 (* Generate read function *)
                 (* -- Some useful names *)
                 val readName     = readSuf name
                 val foundTerm    = "foundTerm"
		 val reachedLimit = "reachedLimit"
		 val resRBuffer   = "resRBuffer"
		 val resBuffer    = "resBuffer"
		 val indexX       = P.minusX(fieldX(rep,length), P.intX 1)
		 val resNext      = P.subX(PT.Id resBuffer, indexX)
		 val edRBuffer    = "edRBuffer"
                 val edBuffer     = "edBuffer"
 		 val edNext       = P.subX(PT.Id edBuffer, indexX)
          
                 (* add local variables, ie, parameters,  to scope *)
		 val _ = pushLocalEnv()                                        (* create new scope *)
		 val cParams : (string * pcty) list = List.map mungeParam params
		 val () = ignore (List.map insTempVar cParams)  (* add params for type checking *)
		 (* scope is removed at end of cnvPArray *)

                 (* -- Check size specification for array *)
                 val (minOpt, maxOpt, chkBoundsSs) = 
                     let fun allocBuffs  countX = 
                             PL.chkNewRBufS(PT.Id resRBuffer, false, PT.Id ts, PT.Id disc)
			   @ PL.chkNewRBufS(PT.Id edRBuffer, true, PT.Id ts, PT.Id disc)
			 fun checkSizeTy (boundX, which) = 
			      expEqualTy(boundX, CTintTys, fn s=> (which ^" size specification "^
							    "for array " ^ name ^ " has type: " ^ s ^
							    ". Expected an unsigned int."))
			 fun chkSize(boundX, which) = 
			     let val () = checkSizeTy(boundX, which)
				 val boundConstOpt = #1(evalExpr boundX)
			     in
				 case boundConstOpt of NONE => NONE
                                 | cOpt as SOME cVal => (
				     if IntInf.<(cVal, IntInf.fromInt 0)
				     then (PE.error("Mininum value for the size of array "^
						    name ^ " (" ^ (IntInf.toString cVal) ^")"^
						    " must be greater than zero."))
                                     else ();
				     cOpt
                                 (* end SOME cVal *))
			     end
			 fun genPosMinCheckSs (minConstOpt,minX) = 
			     if isSome minConstOpt then []
			     else [PT.IfThen( (* if (minX<0) *)
					     amCheckingE(SOME (P.ltX(minX,P.zero))),
					     recordArrayErrorS(
							       PL.PDC_ARRAY_MIN_NEGATIVE,true,
							       "Minimum value for the size of array "^
							       name ^  "(%d) " ^
							       "is negative.", [minX], false))]

			 fun genPosMaxCheckSs (maxConstOpt,maxX) = 
			     if isSome maxConstOpt then []
			     else [PT.IfThen( (* if (maxX<0) *)
					     amCheckingE(SOME(P.ltX(maxX,P.zero))),
					     recordArrayErrorS(
							       PL.PDC_ARRAY_MAX_NEGATIVE,true,
							       "Maximum value for the size of array "^
							       name ^  "(%d) " ^
							       "is negative.", [maxX],true))]

		     in
                     (case sizeSpec 
                      of NONE => (NONE, NONE, [])
                      |  SOME (PX.SizeInfo {min, max, maxTight}) => (
                           case (min,max) 
                           of (NONE,NONE) => (NONE, NONE, [])
                           |  (SOME minX, SOME maxX) => (
				let val minConstOpt = chkSize(minX, "Minimum")
				    val maxConstOpt = chkSize(maxX, "Maximum")
				    val staticBounds = (isSome minConstOpt) andalso (isSome maxConstOpt)
				    val minMaxCheckSs = 
					   if staticBounds 
                                             then if IntInf.> (valOf minConstOpt, valOf maxConstOpt) 
					          then (PE.error("Mininum value for the size of array "^
								name ^ " " ^
								" is greater than its maximum size.");
							[])
                                                  else [] (* no static error, no need for dynamic checks*)
					     else ([PT.IfThen( (* if (minX > maxX) *)
						     amCheckingE(SOME(P.gtX(minX,maxX))), 
						      recordArrayErrorS(PL.PDC_ARRAY_MIN_BIGGER_THAN_MAX_ERR,
                                                                        true, "Mininum value for "^
									      "the size of array "^
									      name ^ "(%d) " ^
									      "is greater than "^
									      "its maximum size (%d).",
									 [minX, maxX],false)
						      )])

				    val dynBoundsCheckSs =  minMaxCheckSs 
					                  @ genPosMinCheckSs(minConstOpt, minX) 
					                  @ genPosMaxCheckSs(maxConstOpt, maxX)
				    val fixedSize =  (valOf minConstOpt) = (valOf maxConstOpt)
							 handle Option => false
				    val sizeAllocSs = 
					if fixedSize 
                                        then allocBuffs (P.intX (IntInf.toInt(valOf maxConstOpt)))
					else allocBuffs (P.zero)
					             
				in
				   (SOME minX, SOME maxX,  
				      dynBoundsCheckSs 
                                    @ sizeAllocSs)
				end
                              (* end Some minX, Some maxX*))
                           | (SOME minX, NONE) => (
				let val minConstOpt = chkSize(minX, "Minumum")
				    val posMinCheckSs = genPosMinCheckSs(minConstOpt, minX)
				    val allocSizeX = P.intX (IntInf.toInt(valOf (#1(evalExpr minX))))
						     handle Option => P.zero
				in
				   (SOME minX, NONE, posMinCheckSs @ allocBuffs(allocSizeX))
				end
                              (* end SOME minX, NONE *))
                           | (NONE, SOME maxX) => (
                                let val maxConstOpt = chkSize(maxX, "Maximum")
				    val posMaxCheckSs = genPosMaxCheckSs(maxConstOpt, maxX)
				    val allocSizeX = P.intX (IntInf.toInt(valOf (#1(evalExpr maxX))))
						     handle Option => P.zero
				in
				   (NONE, SOME maxX, posMaxCheckSs @ allocBuffs(allocSizeX))
				end
                              (* end NONE, SOME maxX *))
                           (* end case (min,max) *))
                         (* END size case *))
		     end

                 (* -- process constriants *)
                 val (sepXOpt, termXOpt, arrayXOpt, genXOpt) = 
                      let fun doOne (constr:pcexp PX.PConstraint) = 
                              case constr 
                              of PX.Sep exp => (
				 expEqualTy(exp, CTintTys,fn s=>("Separator expression for array "^
								 name ^" has type "^s^". Expected "^
								 "type char."));
				 let val pTyName = PL.charlit
				     val readFun = lookupTy(PX.Name pTyName, iSuf o readSuf, #readname)
				     val scanFun = lookupScan(PX.Name pTyName)
				 in
				    (SOME (exp, readFun, scanFun), NONE,NONE,NONE)
                                 end

                              (* end Sep case *))
                              |  PX.Term exp => (
				 expEqualTy(exp, CTintTys,fn s=>("Terminator expression for array "^
								 name ^" has type "^s^". Expected "^
								 "type char."));
                                 let val pTyName = PL.charlit 
				     val readFun = lookupTy(PX.Name pTyName, iSuf o readSuf, #readname)
				     val scanFun = lookupScan(PX.Name pTyName)
				 in
				   (NONE, SOME (exp,readFun,scanFun), NONE, NONE)
                                 end
                              (* end Term case *))
                              |  PX.Forall (r as {index,range,body}) => (
                                 let val subList = [(length, fieldX(rep,length)), 
						    (name, fieldX(rep,name))]
				     val (lower, upper) = 
					(case range 
					 of PX.ArrayName n => (
					    (if n = name then ()
					     else PE.error ("Array name in bound expression ("^
							    n^") does not match the name "^
							    "of the array ("^ name ^ ").")
                                            ); (P.zero, PT.Id length))
					 | PX.Bounds(lower, upper) => (lower,upper))
				     val modBodyX = PTSub.substExps subList body
				     val modLowerX = PTSub.substExps subList lower
				     val modUpperX = PTSub.substExps subList upper
				     fun errMsg which = (fn s => 
							  (which^" bound for forall expression for array "^
						           name ^" has type"^s^". Expected type int."))
				 in
				   pushLocalEnv();
				   ignore(insTempVar(index, P.int));
				   ignore(insTempVar(length, P.int));
				   ignore(insTempVar(name, P.ptrPCT elemRepPCT)); 
				   expEqualTy(lower, CTintTys, errMsg "Lower");
				   expEqualTy(lower, CTintTys, errMsg "Upper");
				   expEqualTy(body, CTintTys, fn s=>("Forall expression for array "^
								   name ^" has type "^s^". Expected "^
								   "type int."));

				   popLocalEnv();
                                   (NONE,NONE,SOME {index=index, lower=modLowerX, 
						    upper=modUpperX, body=modBodyX}, NONE)
				 end
                              (* end Array case *))
			      |  PX.General exp => (
				 expEqualTy(exp, CTintTys,fn s=>("General constraint for array "^
								 name ^" has type "^s^". Expected "^
								 "type int."));
				 (NONE, NONE, NONE, SOME exp)
                              (* end General case *))
			  val constrs = List.map doOne constraints
			  fun mergeOpt which (o1,o2) = 
			      case (o1,o2) 
			      of (NONE,NONE) => NONE
			      |  (NONE, SOME q) => SOME q
			      |  (SOME p, NONE) => SOME p
                              |  (SOME p, SOME q) => (PE.error("Multiple "^which^" clauses."); SOME p)
                          fun mergeAll ((a,b,c,d),(ra,rb,rc,rd)) =
			      (mergeOpt "separator"  (a,ra),
                               mergeOpt "terminator" (b,rb),
                               mergeOpt "array"      (c,rc),
                               mergeOpt "general"    (d,rd))
		      in
			  List.foldr mergeAll (NONE,NONE,NONE,NONE) constrs
                      end

                 (* -- Check parameters to base type read function *)
		 val () = checkParamTys(name, elemReadName, args, 2, 3)

                 (* -- Declare top-level variables and initialize them *)
                 val initSs = [P.varDeclS(PL.base_emPCT, tem, PL.EM_CHECK), (* base_em tem = CHECK; *)
                               P.varDeclS'(PL.base_edPCT, ted)]             (* base_ed ted; *)
                              @ (if Option.isSome termXOpt then             (* int foundTerm = false *)
                                   [P.varDeclS(P.int, foundTerm, P.falseX)] 
                                 else [])
                              @ (if Option.isSome maxOpt then               (* int reachedLimit = false *)
				   [P.varDeclS(P.int, reachedLimit, P.falseX)]
			        else [])
                              @ [
                               P.varDeclS(P.ptrPCT PL.rbufferPCT, resRBuffer, P.zero),
			                                                    (* RBuf_t *resRBuffer = 0; *)
                               P.varDeclS(P.ptrPCT elemRepPCT, resBuffer, P.zero),
			                                                    (* canonical *resBuffer = 0; *)
                               P.varDeclS(P.ptrPCT PL.rbufferPCT, edRBuffer, P.zero),
			                                                    (* RBuf_t *edRBuffer = 0; *)
                               P.varDeclS(P.ptrPCT elemEdPCT, edBuffer, P.zero),
			                                                    (* canonical *edBuffer = 0; *)
                               P.assignS(fieldX(rep,length), P.zero)]       (* modres->length = 0; *)

                 (* -- fragments for while loop for reading input *)

                 (* -- code for checking if terminator is next in input *)
                 fun genTermCheck NONE = []
                   | genTermCheck (SOME (exp, readFun, scanFun)) = 
                      [P.mkCommentS("Looking for terminator"),
		       PT.IfThen(
                          PL.readFunChkX(PL.PDC_OK, readFun, PT.Id ts, P.addrX(PT.Id tem),  [], 
						             P.addrX(PT.Id ted), exp, PT.Id disc),
			  PT.Compound
                           [P.assignS(PT.Id foundTerm, P.trueX)])]

                 (* -- Code for checking termination conditions *)
                 fun genBreakCheckX (termOpt, sizeOpt) = 
		     let val isEofX = PL.isEofX(PT.Id ts, PT.Id disc)
			 val termFoundX = PT.Id foundTerm
			 val limitReachedX = PT.Id reachedLimit
		     in
                        case (termOpt,sizeOpt)
			of (NONE,NONE) => isEofX
                        |  (NONE, SOME _)  => P.orX(isEofX, limitReachedX)
                        |  (SOME _, NONE)  => P.orX(isEofX, termFoundX)
                        |  (SOME _, SOME _)  => P.orX(isEofX, 
						      P.orX(termFoundX,limitReachedX))
			    
		     end

                 fun genBreakCheckSs (term,size) = 
		     [P.mkCommentS("Have we finished reading array?"),
		      PT.IfThen(genBreakCheckX(term,size), PT.Compound[PT.Break])]

                 (* -- Check that we found separator on last loop. *)
                 fun genSepCheck NONE = []
                   | genSepCheck (SOME (sepX, readSep, scanSepOpt)) = 
                      let val scanSep = Option.valOf scanSepOpt (* must exist *)
			                handle Option => (PE.error "Expected scan function."; 
							  Atom.atom "bogus")
			  val (scanStopX, chkTermSs) = 
			      case termXOpt of NONE => (P.intX 0, [])
			    | SOME(termX,_,_) => 
				  let val chkTermSs = 
				      [PT.IfThen(P.eqX(PT.Id "c", termX),
					 PT.Compound[
				          recordArrayErrorS(PL.PDC_ARRAY_EXTRA_BEFORE_TERM,true,"",[],false),
					  P.assignS(PT.Id foundTerm, P.trueX),
					  PT.Break])]
				  in
				    (termX, chkTermSs)
				  end

		      in
                       [P.mkCommentS("Array not finished; read separator."),
                         PT.Compound[
	 		 P.varDeclS'(P.uchar, "c"),
		 	 P.varDeclS'(PL.sizePCT, "n"),
		         PT.IfThenElse(
			    P.eqX(PL.PDC_OK,
				  PL.scanFunX(Atom.toString scanSep, PT.Id ts, 
					      sepX, scanStopX, P.addrX (PT.Id "c"),
					      P.addrX (PT.Id "n"),PT.Id disc)),
			    PT.Compound[
                              PT.IfThen(amCheckingE NONE, 
	  		       PT.Compound[ (* if am checking *)
			         PT.IfThenElse(P.andX(P.eqX(PT.Id "c", sepX),P.gtX(PT.Id "n", P.zero)),
				    recordArrayErrorS(PL.PDC_ARRAY_EXTRA_BEFORE_SEP, true,
						   "", [],false),
                                    PT.Compound (chkTermSs))])],
                            PT.Compound[ (* else error in reading separator *)
			      P.mkCommentS("Error reading separator"),
			      recordArrayErrorS(PL.PDC_ARRAY_SEP_ERR, 
						true, "Missing separator.",[],true),
			      PT.Break]
                            )]]

                      end
                 (* -- read next element *)
                 val readElementSs = 
                       [P.postIncS(fieldX(rep,length))]
                     @ (if Option.isSome maxOpt 
		        then [P.assignS(PT.Id reachedLimit, P.gteX(fieldX(rep,length), Option.valOf maxOpt))]
		        else [])
		     @ (PL.chkReserveSs(PT.Id ts, PT.Id disc, PT.Id resRBuffer, 
				     P.addrX(PT.Id resBuffer), P.sizeofX elemRepPCT,
				     fieldX(rep,length),P.zero))
		     @ (PL.chkReserveSs(PT.Id ts, PT.Id disc, PT.Id edRBuffer, 
				     P.addrX(PT.Id edBuffer), P.sizeofX edPCT,
				     fieldX(rep,length),P.zero))
                     @ [PT.IfThen(
                          PL.readFunChkX(PL.PDC_ERROR, elemReadName, 
					               PT.Id ts, 
					               P.addrX(fieldX(em,element)),
						       args,
						       P.addrX(edNext),
						       P.addrX(resNext),
						       PT.Id disc),
                          PT.Compound[
			   PT.IfThen(P.lteX(fieldX(em,array), PL.EM_CHECK),
			     PT.Compound[
                              PT.IfThen(P.notX(fieldX(ed,nerr)),
                                 PT.Compound (
	 			   (reportErrorSs(true,PL.PDC_ARRAY_ELEM_ERR, false, "", []))
                                  @ [P.mkCommentS("Index of first element with an error."),
				     P.assignS(fieldX(ed,firstError), P.minusX(fieldX(rep,length),P.intX 1))])),
                              P.postIncS(fieldX(ed,neerr))
                            ])])]

                 (* -- panic recovery code *)
		 fun genPanicRecoveryS (sepXOpt, termXOpt, maxOpt) = 
                     let val panicSs = [P.assignS(fieldX(ed,panic), P.trueX), PT.Break]
                         val recoveryFailedSs = P.mkCommentS("Recovery failed.") :: panicSs
			 val noRecoverySs = P.mkCommentS("No recovery possible.") :: panicSs
			 fun recoverToCharSs (which, scan, forX, stopX) = [
				  P.mkCommentS("Try to recover to " ^ which ^"."),
				  PT.IfThenElse(P.eqX(PL.PDC_OK,
						   PL.scanFunX(Atom.toString scan, PT.Id ts, 
							       forX, stopX, P.zero, P.zero, PT.Id disc)),
                                    PT.Compound[
				     P.mkCommentS("We recovered; restore invariant."),
				     PL.backOne(PT.Id ts, PT.Id disc)],
				    PT.Compound(recoveryFailedSs)
                                 )]
			 val recoverSs = 
			 case (sepXOpt, termXOpt, maxOpt) 
                         of (NONE,NONE,_) => noRecoverySs
                         |  (SOME (sepX, _, NONE), NONE, _) => noRecoverySs
                         |  (SOME (sepX, _, SOME sepScan), NONE, NONE) => 
                               recoverToCharSs("separator", sepScan, sepX, P.intX 0)
                         |  (SOME (sepX, _, SOME sepScan), NONE, SOME _) => 
			       [PT.IfThenElse(PT.Id reachedLimit,
				 PT.Compound(noRecoverySs), 
				 PT.Compound(recoverToCharSs ("separator", sepScan, sepX, P.intX 0)))]
                         |  (NONE, SOME(termX, _, NONE), _ ) => noRecoverySs
                         |  (NONE, SOME(termX, _, SOME termScan), _ ) => 
			       recoverToCharSs("terminator", termScan, termX, P.intX 0)
                         |  (SOME (sepX, _, SOME sepScan), SOME(termX, _, SOME termScan), _ ) =>
			      (if Atom.sameAtom (sepScan, termScan)
			       then recoverToCharSs("separator and/or terminator", termScan, sepX,termX)
                               else (PE.error ("Different scanning functions for separators and terminators "^
					      "not yet implemented."); noRecoverySs))
			 |  (SOME (sepX, _, SOME sepScan), SOME(termX, _, NONE), SOME _ ) =>
			       [PT.IfThenElse(PT.Id reachedLimit,
				 PT.Compound(noRecoverySs), 
				 PT.Compound(recoverToCharSs ("separator", sepScan, sepX, P.intX 0)))]
			 |  (SOME (sepX, _, SOME sepScan), SOME(termX, _, NONE), NONE ) =>
                               recoverToCharSs("separator", sepScan, sepX, P.intX 0)
			 |  (SOME (sepX, _, NONE), SOME(termX, _, SOME termScan),  _ ) =>
			       recoverToCharSs("terminator", termScan, termX, P.intX 0)
			 |  (SOME (sepX, _, NONE), SOME(termX, _, NONE),  _ ) => noRecoverySs
		     in
			 PT.Compound recoverSs
		     end
                 val panicRecoverySs = [PT.IfThen(P.dotX(edNext, PT.Id panic), 
					   PT.Compound[genPanicRecoveryS(sepXOpt, termXOpt, maxOpt)])]

                 (* -- while loop for reading input *)
                 val whileSs = 
		     let fun insTermChk bdyS = 
			     case termXOpt of NONE => bdyS
			     | SOME (termX, termRead, _) => (
                                PT.IfThenElse(
                                 PL.readFunChkX(PL.PDC_OK, termRead, PT.Id ts, 
						P.addrX (PT.Id tem), [], P.addrX (PT.Id ted),
						termX, PT.Id disc),
				 PT.Compound[
				  P.assignS(PT.Id foundTerm, P.trueX)
                                 ],
                                 PT.Compound[bdyS])
                             (* end case SOME *))
			 val bdyS = 
			     PT.While(P.trueX,  
                                 PT.Compound(
				     [P.mkCommentS("Ready to read next element.")]
				   @ readElementSs 
				   @ panicRecoverySs
                                   @ (genTermCheck termXOpt)
				   @ genBreakCheckSs (termXOpt,maxOpt)
                                   @ (genSepCheck sepXOpt)
                                 ))
		     in 
			 [P.mkCommentS("Reading input until we reach a termination condition"),
                                PT.IfThen(P.andX(P.notX(fieldX(ed,panic)), 
						 P.notX(PL.isEofX(PT.Id ts, PT.Id disc))) ,
					  PT.Compound[insTermChk bdyS])]
		     end

                 (* -- Check if there was junk before trailing terminator *)
	         val trailingJunkChkSs = 
		     case termXOpt of NONE => []
                     | SOME (termX, _, NONE) => (PE.error "Expected a scan function"; [])
		     | SOME (termX, _, SOME termScan) => 
			 [P.mkCommentS("End of loop. Read trailing terminator if there was trailing junk."),
			  PT.IfThen(P.andX(P.notX(fieldX(ed,panic)),P.notX(PT.Id foundTerm)),
			   PT.Compound[
		           PT.IfThenElse(
			     P.eqX(PL.PDC_OK,
				  PL.scanFunX(Atom.toString termScan, PT.Id ts, 
					      termX, P.zero, P.zero,
					      P.zero,PT.Id disc)),
                             PT.Compound[
			      PT.IfThen(amCheckingE NONE, 
			        PT.Compound[
				 recordArrayErrorS(PL.PDC_ARRAY_EXTRA_BEFORE_TERM,true,"",[],false),
				 P.assignS(PT.Id foundTerm, P.trueX)])],
			     recordArrayErrorS(PL.PDC_ARRAY_TERM_ERR, true, "Missing terminator.",[],true))
			 ])]

                 (* -- Set data fields in canonical rep and ed from growable buffers *)
                 val setDataFieldsSs = 
                     [P.mkCommentS "Set fixed buffers from growable ones.",
		      PL.chkFreeRBufferS(PT.Id ts, PT.Id disc, PT.Id resRBuffer,  P.addrX(fieldX(rep,name))),
		      P.assignS(fieldX(ed,length), fieldX(rep,length)),
		      PL.chkFreeRBufferS(PT.Id ts, PT.Id disc, PT.Id edRBuffer,  P.addrX(fieldX(ed,name)))
                     ]

                 (* -- Check array-level constriaints *)
                 (* -- -- Check that we read at least min elements, if min specified *)
                 fun genMinReachedConstraintSs minX =  
                     let val lengthTestX = P.ltX(fieldX(rep,length), minX)
			 val testX = if Option.isSome maxOpt 
			             then P.andX(P.notX(PT.Id reachedLimit), 
						 lengthTestX)
				     else lengthTestX
		     in
		      [P.mkCommentS("Checking that we read enough elements"),
		       PT.IfThen(testX,
			  recordArrayErrorS(PL.PDC_ARRAY_SIZE_ERR, true,
			    ("Read %d element(s) for array "^name^"; required %d."),
			    [fieldX(rep,length), minX], false))]
		     end
                 (* -- -- Check that the user's whole array constraint is satisfied. *)
                 fun genArrayConstraintSs {index:string, lower, upper, body:PT.expression}  = 
		     [P.mkCommentS "Checking user's array constraint.",
                      PT.Compound[
                       P.varDeclS'(P.int, index),
                       P.varDeclS(P.int, "violated", P.falseX),
		       PT.IfThen(P.notX(P.andX(P.lteX(P.zero, lower),
				 	P.ltX(upper, fieldX(rep,length)))),
				 P.assignS(PT.Id "violated", P.trueX)),
		       PT.For(P.assignX(PT.Id index, lower),
			      P.andX(P.notX(PT.Id "violated"), P.lteX(PT.Id index, upper)), 
			      P.postIncX(PT.Id index),
                              PT.Compound[
                               PT.IfThen(P.notX(body),
				 PT.Compound[
                                  P.assignS(PT.Id "violated", P.trueX)
                                 ] (* end if *))
                              ] (* end for *)),
		       PT.IfThen(PT.Id "violated",
			recordArrayErrorS(PL.PDC_ARRAY_USER_CONSTRAINT_ERR, true,
					  ("User constraint for array "^name^" violated."), [], false))
                     ]]
                 val arrayConstraintsSs = 
		     let fun condWrap bdySs = 
			 [P.mkCommentS "Checking constraints",
			  PT.IfThen(amCheckingE(SOME(P.notX (fieldX(ed,panic)))),
				    PT.Compound bdySs)]
		     in
		       case (minOpt, arrayXOpt) of (NONE,NONE) => []
                       | (SOME minX, NONE) => condWrap(genMinReachedConstraintSs minX)
                       | (NONE, SOME r) => condWrap(genArrayConstraintSs r)
                       | (SOME minX, SOME r) =>
			   condWrap((genMinReachedConstraintSs minX) @ (genArrayConstraintSs r))
		     end

                 (* -- return value *)
                 val returnS = P.returnS (
				      P.condX(P.eqX(P.arrowX(PT.Id (gMod(ed)), PT.Id nerr),P.zero),
				      PL.PDC_OK, PL.PDC_ERROR))
	         (* -- Assemble read function *)
		 val bodySs =   [PT.Compound (
				  initSs 
                                @ chkBoundsSs
                                @ whileSs
				@ trailingJunkChkSs
				@ setDataFieldsSs
				@ arrayConstraintsSs
                                @ [returnS])]
                 val readFunEDs = genReadFun(readName, cParams, emPCT,edPCT,canonicalPCT, 
					     NONE, true, bodySs)
                 val _ = popLocalEnv()
	     in
		   canonicalDecls
		 @ cnvExternalDecl emStructED
                 @ cnvExternalDecl edStructED
                 @ (List.concat(List.map cnvExternalDecl readFunEDs))
	     end

	  fun cnvPEnum  {name:string, params : (pcty * pcdecr) list, 
			 members : (string * pcexp option * string option) list } =
	      let val baseTy = PX.Name PL.strlit
                  (* generate canonical representation *)
                  fun mungeMembers (name, expOpt, commentOpt) = 
		      case expOpt of NONE => (name, PT.EmptyExpr, commentOpt)
 		                   | SOME e => (name, e, commentOpt)
		  val enumFields = List.map mungeMembers members
		  val canonicalED = P.makeTyDefEnumEDecl(enumFields, repSuf name)
		  val canonicalDecls = cnvExternalDecl canonicalED
		  val canonicalPCT = P.makeTypedefPCT(repSuf name)

                  (* generate error mask *)
		  val baseEMPCT = P.makeTypedefPCT(lookupTy(baseTy,emSuf, #emname))
		  val emED      = P.makeTyDefEDecl (baseEMPCT, emSuf name)
		  val emDecls   = cnvExternalDecl emED
		  val emPCT     = P.makeTypedefPCT (emSuf name)		

                  (* generate error description *)
		  val baseEDPCT = P.makeTypedefPCT(lookupTy(baseTy,edSuf, #edname))
		  val edED      = P.makeTyDefEDecl (baseEDPCT, edSuf name)
		  val edDecls   = cnvExternalDecl edED
		  val edPCT     = P.makeTypedefPCT (edSuf name)		

                  (* Generate read function *)
                  (* -- Some useful names *)
                  val readName = readSuf name
		  val baseReadFun = lookupTy(baseTy, iSuf o readSuf, #readname)
		  fun readOneBranch (bname, bvalOpt, commentOpt) =
		      let val labelLenX = P.intX(String.size bname)
		      in
                         [P.assignS(P.dotX(PT.Id "strlit", PT.Id (PL.str)), PT.String bname),
			  P.assignS(P.dotX(PT.Id "strlit", PT.Id (PL.len)), labelLenX)]
                       @ PL.chkPtS(PT.Id ts, PT.Id disc)
                       @ [PT.IfThenElse(
			    PL.readFunChkX(PL.PDC_ERROR, baseReadFun, PT.Id ts, 
						         PT.Id (gMod em), [], PT.Id(gMod ed),
						         P.addrX(PT.Id "strlit"), PT.Id disc),
			    PT.Compound (PL.restoreS(PT.Id ts, PT.Id disc)),
			    PT.Compound (  PL.commitS(PT.Id ts, PT.Id disc)
				         @ [P.assignS(P.starX (PT.Id (gMod rep)), PT.Id bname),
					    PT.Return PL.PDC_OK]))]
		      end
                  fun genReadBranches () = 
                      [P.varDeclS'(PL.stringPCT, "strlit")]
		      @ List.concat(List.map readOneBranch members)
		  val cleanupSs =  [P.mkCommentS("We didn't match any branch")]
			         @ reportErrorSs(false,
					PL.PDC_ENUM_MATCH_FAILURE,
					true, 
					("Did not match any branch of enum "^name^"."),
					[])
			         @ [P.assignS(fieldX(ed,panic), P.trueX),
				    PT.Return PL.PDC_ERROR]


		  (* -- Assemble read function *)
		  val _ = pushLocalEnv()                                        (* create new scope *)
		  val () = ignore (insTempVar(gMod rep, P.ptrPCT canonicalPCT)) (* add modrep to scope *)
		  val cParams : (string * pcty) list = List.map mungeParam params
		  val () = ignore (List.map insTempVar cParams)  (* add params for type checking *)
		  val readFields = genReadBranches()                            (* does type checking *)
		  val _ = popLocalEnv()                                         (* remove scope *)
		  val bodySs = readFields @ cleanupSs
		  val readFunEDs = genReadFun(readName, cParams, 
					      emPCT,edPCT,canonicalPCT, NONE, false, bodySs)


	      in
		  canonicalDecls
                @ emDecls
                @ edDecls
                @ (List.concat (List.map cnvExternalDecl readFunEDs))
	      end


        

	  in
	      case decl 
	      of PX.PTypedef t => cnvPTypedef t
              |  PX.PStruct  s => cnvPStruct  s
              |  PX.PUnion   u => cnvPUnion   u
              |  PX.PArray   a => cnvPArray   a
              |  PX.PEnum    e => cnvPEnum    e
	  end

      fun pcnvStat (PX.PComment s) =  wrapSTMT(Ast.StatExt(AstExt.SComment(formatComment s)))

      in
	  {CNVExp = CNVExp,
	   CNVStat = pcnvStat,
	   CNVBinop = CNVBinop,
	   CNVUnop = CNVUnop,
	   CNVExternalDecl = pcnvExternalDecl,
	   CNVSpecifier = CNVSpecifier,
	   CNVDeclarator = CNVDeclarator,
	   CNVDeclaration = CNVDeclaration}
      end
end

