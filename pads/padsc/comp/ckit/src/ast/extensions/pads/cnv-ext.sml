structure CnvExt : CNVEXT = struct

  structure PT   = ParseTree     (* the parse tree *)
  structure PX   = ParseTreeExt  (* Pads extensions *)
  structure P    = ParseTreeUtil (* Utility functions for manipulating the parse tree *)
  structure PE   = PError        (* Error reporting utilities *)
  structure PBTys = PBaseTys      (* Information about the pads base types *)
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
  type pdty = ParseTree.decltype
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
        | SOME s1 => (case s2Opt of NONE => SOME s1
			         | SOME s2 => SOME (s1 ^". " ^ s2))

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

    fun CisStatic(acty: acty) = 
     case acty 
      of Ast.Qual (_,ty) => CisStatic ty
       | Ast.Numeric _ => true
       | Ast.Pointer _ => false
       | Ast.Array(SOME _, ty)  => CisStatic ty
       | Ast.Array(NONE, _) => false
       | Ast.EnumRef _ => true 
       | Ast.TypeRef _ => CisStatic (TU.getCoreType ttab acty)
       | Ast.Function _ => false (* although a function can be viewed as a pointer *)
       | Ast.StructRef tid => 		
	  (case lookTid tid of
	       SOME {ntype = SOME(B.Struct(_, bl)),...} => 
		   let fun f(cty,_,_,_) = CisStatic cty
		   in
		       List.all f bl
		   end
	     | _ => false)
       | Ast.UnionRef tid => 
	  (case lookTid tid of
	       SOME {ntype = SOME(B.Union(_, bl)),...} => 
		   let fun f(cty,_,_) = CisStatic cty
		   in
		       List.all f bl
		   end
	     | _ => false)
       | Ast.Ellipses => false  (* can't occur *)
       | Ast.Void => false
       | Ast.Error => false


(* Error Reporting  **********************************************************)

	(* Setup the error reporting. *)
    val errorState = #errorState (#globalState stateFuns)
    val _ = (PE.setup errorState
	     (#error (#locFuns (stateFuns))) 
	     (#warn  (#locFuns (stateFuns))))

    fun unbound sym = (case lookSym sym of
			   SOME _ => PE.fail ("Redeclaration of " ^ 
					   (Symbol.name sym))
			 | NONE   => ())


    val bug = Error.bug errorState

(* AST help functions ********************************************************)

    val isFunction          = TU.isFunction          ttab
    val isStructOrUnion     = TU.isStructOrUnion     ttab
    val getCoreType         = TU.getCoreType         ttab
    val equalType           = TU.equalType           ttab

    fun sizeof ty = 
      LargeInt.fromInt (#bytes (Sizeof.byteSizeOf {sizes=Sizes.defaultSizes, err=error, warn=warn, bug=bug} ttab ty))


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

    fun expAssignTy(expPT, CTtys, genErrMsg) = 
	let val (expTy, expAst) = cnvExpression expPT
	in
            if List.exists (fn cty => isAssignable(cty,expTy,NONE)) CTtys
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

    fun cnvDeclaration(dt, del: (ParseTree.declarator * pcexp) list ) = 
	let val (ct', sc) = cnvType(false, dt)
	    val (ds,es) = ListPair.unzip del
	    val (cts, nameOpts) = ListPair.unzip(List.map (fn d => mungeTyDecr(ct', d)) ds)
            fun zip3 ([],[],[]) = []
	      | zip3 (b::bs, c::cs, d::ds) = (b,c,d) :: (zip3 (bs,cs,ds))
              | zip3 (_,_,_) = raise Fail "Zipping unequal length lists."
	in
	    zip3 (cts,nameOpts,es)
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
		 let fun procMem (ct,m:Ast.member,s) =
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
	      val ts = "pdc"
	      val em = "em" 
	      val ed = "ed"
	      val acc = "acc"
	      val rep = "rep"
	      val ted = "ted"
	      val tem = "tem"
	      val all = "structLevel"
	      val prefix = "prefix"
	      val what = "what"
	      val nst = "nst"
	      val tmpstr = "tmpstr"
	      val outstr = "outstr"
	      val result = "result"
	      val errorf = "errorf"

	      (* Some useful functions *)
	      fun repSuf  s = s (* Make rep type same as pads name; s^"_rep" *)
              fun emSuf   s = s^"_em"
              fun edSuf   s = s^"_ed"
              fun accSuf  s = s^"_acc"
              fun initSuf s = s^"_init"
              fun resetSuf s = s^"_reset"
              fun cleanupSuf s = s^"_cleanup"
              fun addSuf  s = s^"_add"
              fun readSuf s = s^"_read"
              fun iSuf s = s^"_internal"
              fun reportSuf s = s^"_report"
	      fun mapSuf s = s^"_map"
	      fun toStringSuf s = s^"2str"
	      fun errSuf s = s^"_err"
	      fun findEORSuf s = s^"_findEOR"
	      fun gTemp base = "tmp"^base
	      fun gMod  base = "mod"^base

	      fun fieldX (bsName, fName) = P.arrowX(PT.Id(gMod bsName), PT.Id fName)
	      fun getFieldX(base,field) = P.addrX(P.arrowX(PT.Id base, PT.Id field))

	      val discX = P.arrowX(PT.Id ts, PT.Id PL.disc)
	      val ioDiscX =  P.arrowX(P.arrowX(PT.Id ts, PT.Id PL.disc), PT.Id PL.io_disc)
	      val errorFX =  P.arrowX(P.arrowX(PT.Id ts, PT.Id PL.disc), PT.Id PL.errorf)
	      val d_endianX =  P.arrowX(P.arrowX(PT.Id ts, PT.Id PL.disc), PT.Id PL.d_endian)
	      val m_endianX =  P.arrowX(P.arrowX(PT.Id ts, PT.Id PL.disc), PT.Id PL.m_endian)
	      val locX      =  P.addrX(fieldX(ed,loc))
              val locS      =  PL.getLocS(PT.Id ts,P.addrX(fieldX(ed,loc)))
	      val locBS     =  PL.getLocBeginS(PT.Id ts, P.addrX(fieldX(ed,loc)))
	      val locES     =  PL.getLocEndS(PT.Id ts, P.addrX(fieldX(ed,loc)))

	      fun getDynamicFunctions (name,memChar) = 
		  case memChar of TyProps.Static => (NONE,NONE,NONE,NONE)
		| TyProps.Dynamic => (SOME (initSuf name),
				      SOME (cleanupSuf name),
				      SOME ((initSuf o edSuf) name),
				      SOME ((cleanupSuf o edSuf) name))

              fun buildTyProps (name, diskSize,memChar,endian,isRecord) = 
		  let val (repInit, repClean, edInit, edClean) = getDynamicFunctions (name,memChar)
		  in
		      {diskSize=diskSize,memChar=memChar,endian=endian, isRecord=isRecord,
		       repName  = name, 
		       repInit  = repInit,
		       repRead  = readSuf name, 
		       repClean = repClean,
		       edName   = edSuf name,
		       edInit   = edInit,
		       edClean  = edClean,
		       accName  = accSuf name,
		       accInit  = (initSuf o accSuf) name,
		       accAdd   = (addSuf o accSuf) name,
		       accReport= (reportSuf o accSuf) name,
		       accClean = (cleanupSuf o accSuf) name}
		  end

	      fun lookupTy (ty:pty, sufFun:string->string, fldSelect:PBTys.baseInfoTy ->Atom.atom) = 
                  case ty 
                  of PX.Name s => ( case PBTys.find(PBTys.baseInfo, Atom.atom s) 
                                    of NONE => (sufFun s)
			            | SOME (b:PBTys.baseInfoTy) => Atom.toString(fldSelect b))
              fun lookupScan(ty:pty) = 
		  case ty
                  of PX.Name s => ( case PBTys.find(PBTys.baseInfo, Atom.atom s)
				    of NONE => NONE
                                    |  SOME(b:PBTys.baseInfoTy) => #scanname b)
              fun lookupAcc(ty:pty) = 
		  case ty
                  of PX.Name s => ( case PBTys.find(PBTys.baseInfo, Atom.atom s)
				    of NONE => SOME(accSuf s)  (* non-base type; acc constructed from type name*)
                                    |  SOME(b:PBTys.baseInfoTy) => Option.map Atom.toString (#accname b))

              fun reverseLookup(cty:Ast.ctype) : PX.Pty option = 
		  let val entries : (Atom.atom * PBTys.baseInfoTy) list = PBTys.listItemsi(PBTys.baseInfo)
		      fun find f [] = NONE
                        | find f (x::xs) = case (f x) of NONE => find f xs | r => r
		      fun chkOne(a,b:PBTys.baseInfoTy) = 
			  let val n = #repname b
			      val accPCT = P.makeTypedefPCT (Atom.toString n)
			      val (accCT,sc) = CTcnvType accPCT
			  in
			      if equalType(cty, accCT) then SOME (PX.Name (Atom.toString a))
			      else NONE
			  end
		  in
		      find chkOne entries
		  end

              fun lookupMemFun(ty:pty) = 
		  case ty
                  of PX.Name s => ( case PBTys.find(PBTys.baseInfo, Atom.atom s)
				    of NONE => s  (* non-base type; mem constructed from rep name*)
                                    |  SOME(b:PBTys.baseInfoTy) => Atom.toString (#repname b))
              fun lookupMemChar (ty:pty) = 
                  case ty 
                  of PX.Name s => ( case PBTys.find(PBTys.baseInfo, Atom.atom s)
				    of NONE => (case PTys.find(Atom.atom s)
						of NONE => TyProps.Dynamic
						| SOME (b:PTys.pTyInfo) => (#memChar b)
						    (* end nested case *))
                                    |  SOME(b:PBTys.baseInfoTy) => (#memChar b))
              fun lookupDiskSize (ty:pty) = 
                  case ty 
                  of PX.Name s => ( case PBTys.find(PBTys.baseInfo, Atom.atom s)
				    of NONE => (case PTys.find(Atom.atom s)
						of NONE => TyProps.Variable
						| SOME (b:PTys.pTyInfo) => (#diskSize b)
						    (* end nested case *))
                                    |  SOME(b:PBTys.baseInfoTy) => (#diskSize b))

              fun lookupEndian (ty:pty) = 
                  case ty 
                  of PX.Name s => ( case PBTys.find(PBTys.baseInfo, Atom.atom s)
				    of NONE => (case PTys.find(Atom.atom s)
						of NONE => false
						| SOME (b:PTys.pTyInfo) => (#endian b)
						    (* end nested case *))
                                    |  SOME(b:PBTys.baseInfoTy) => (#endian b))

              fun lookupRecord (ty:pty) = 
                  case ty 
                  of PX.Name s => ( case PBTys.find(PBTys.baseInfo, Atom.atom s)
				    of NONE => (case PTys.find(Atom.atom s)
						of NONE => false
						| SOME (b:PTys.pTyInfo) => (#isRecord b)
						    (* end nested case *))
                                    |  SOME(b:PBTys.baseInfoTy) => false)

              fun tyName (ty:pty) = case ty of PX.Name s => s


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

              fun genReturnChk e =  P.returnS (P.condX(P.eqX(e,P.zero), PL.PDC_OK, PL.PDC_ERROR))

	      fun reportStructErrorSs (code, locX) = 
		  [PT.IfThen(
		     P.eqX(P.zero, fieldX(ed,nerr)), 
		     PT.Compound 
		      [P.assignS(fieldX(ed, errCode), code),
		       P.assignS(fieldX(ed, loc), locX)]),
		   P.plusAssignS(fieldX(ed,nerr), P.intX 1)]

	      fun reportBaseErrorSs (code, locX) = 
		  [P.assignS(fieldX(ed, errCode), code),
		   P.assignS(fieldX(ed, loc), locX)]

	      fun reportUnionErrorSs (code, locX) = 
                 [PT.IfThen(
		   P.eqX(PT.Id result, PL.PDC_OK), (* only report scanning error if correctly read field*)
		   PT.Compound
		    [PT.IfThen(
		      P.eqX(P.zero, fieldX(ed,nerr)), 
		      PT.Compound 
		       [P.assignS(fieldX(ed, errCode), code),
		        P.assignS(fieldX(ed, loc), locX)]),
		     P.plusAssignS(fieldX(ed,nerr), P.intX 1)])]



              fun genReadEOR (reportErrorSs) () = 
		  [P.mkCommentS ("Reading to EOR"),
		    PT.Compound[
			   P.varDeclS'(PL.base_edPCT, ted),
			   P.varDeclS'(PL.sizePCT, "n"),
			   PL.getLocBeginS(PT.Id ts, P.addrX(P.dotX(PT.Id ted, PT.Id loc))),
                           PT.IfThenElse(
			      P.eqX(PL.PDC_OK, 
				    PL.IONextRecX(PT.Id ts, P.addrX (PT.Id "n"))),
			      PT.Compound
			       [PT.IfThen(
				 P.gtX(PT.Id "n", P.zero),
				 PT.Compound
                                  [PT.IfThen(
				    PL.getSpecLevelX(PT.Id ts),
				    PT.Compound
				     [PT.Return PL.PDC_ERROR]),
				   PL.getLocEndS(PT.Id ts, P.addrX(P.dotX(PT.Id ted, PT.Id loc))),
				   PT.IfThenElse(
				     P.notX(fieldX(ed,panic)),
				     PT.Compound(
				       [PL.userErrorS(PT.Id ts, 
						      P.addrX(P.dotX(PT.Id ted, PT.Id loc)),
						      PL.PDC_EXTRA_BEFORE_EOR,
						      P.zero, 
						      [])]
				       @ reportErrorSs(PL.PDC_EXTRA_BEFORE_EOR, P.dotX(PT.Id ted,PT.Id loc))),
				     PT.Compound
					[PL.getLocS(PT.Id ts, P.addrX(P.dotX(PT.Id ted, PT.Id loc))),
					 PL.userWarnS(PT.Id ts, 
						       P.addrX(P.dotX(PT.Id ted, PT.Id loc)),
						       PT.String "Resynching at EOR", 
						       [])])]),
				P.assignS(fieldX(ed,panic), P.zero)],
			      PT.Compound
			       [PT.IfThen(
				 PL.getSpecLevelX(PT.Id ts),
				 PT.Compound[PT.Return PL.PDC_ERROR]),
				P.assignS(fieldX(ed,panic), P.zero),
				PL.getLocEndS(PT.Id ts, P.addrX(P.dotX(PT.Id ted, PT.Id loc))),
				PL.userErrorS(PT.Id ts, 
					      P.addrX(P.dotX(PT.Id ted, PT.Id loc)),
					      PL.PDC_AT_EOR,
					      PT.String "Found EOF when searching for EOR", 
					      [])])]]


	      fun genReadFun (readName, cParams:(string * pcty)list, 
			      emPCT,edPCT,canonicalPCT, emFirstPCT, hasNErr, bodySs) = 
		  let val iReadName = iSuf readName			  
		      val (cNames, cTys) = ListPair.unzip cParams
                      val paramTys = [P.ptrPCT PL.toolStatePCT, P.ptrPCT emPCT]
			             @ cTys
			             @ [P.ptrPCT edPCT, P.ptrPCT canonicalPCT]
                      val paramNames = [ts, em] @ cNames @ [ed,rep]
		      val iParamNames = [ts, gMod em] @ cNames @ [gMod ed, gMod rep]
                      val formalParams = List.map P.mkParam (ListPair.zip (paramTys, paramNames))
		      val iFormalParams = List.map P.mkParam (ListPair.zip (paramTys, iParamNames))
                      val paramArgs = List.map PT.Id iParamNames
		      val incNerrSs = if hasNErr then
			              [P.assignS(P.arrowX(PT.Id(gMod(ed)), PT.Id nerr), P.zero)]
				      else []
		      val innerInitDecls = incNerrSs  
				     @ [P.assignS(P.arrowX(PT.Id(gMod ed), PT.Id panic), P.falseX),
					P.assignS(P.arrowX(PT.Id(gMod ed), PT.Id errCode), PL.PDC_OK)]
		      val returnTy =  PL.toolErrPCT
		      val innerBody = innerInitDecls @ bodySs

                      (* -- internal entry point function *)
		      val readFunInternalED = 
			  P.mkFunctionEDecl(iReadName, iFormalParams, PT.Compound innerBody, returnTy)
                      (* -- external entry point function *)
		      val decls =   genLocTemp(canonicalPCT, rep, NONE) 
			          @ genLocTemp(emPCT, em, emFirstPCT) 
			          @ genLocTemp(edPCT, ed, NONE)
		      val wrapperinitDecls =  [genLocInit rep, genLocInit em, genLocInit ed]
		      val checkParamsSs = 
			  [PT.IfThen(
			    P.notX(PT.Id ts),
			    PT.Compound[PL.warnDefault(readName^": null pdc parameter."),
					PT.Return (PL.PDC_ERROR)]),
			   PT.IfThen(
			    P.notX(discX),
			    PT.Compound[PL.warnDefault(readName^": null pdc->disc."),
					PT.Return (PL.PDC_ERROR)]),
			   PL.trace(discX, readName^" called."),
			   PT.IfThen(
			    P.notX(ioDiscX),
			    PT.Compound[PL.warn(discX,readName^": IO discipline not installed."),
					PT.Return (PL.PDC_ERROR)])]

                      val callIntSs = [PT.Return (PT.Call(PT.Id iReadName, paramArgs))]
		      val bodySs' = decls @ checkParamsSs @ wrapperinitDecls @ callIntSs
		      val bodyS = PT.Compound bodySs'
		      val readFunED = P.mkFunctionEDecl(readName, formalParams, bodyS, returnTy)
		  in
		      [readFunInternalED, readFunED]
		  end

              (* PDC_error_t foo_init/foo_clear(PDC_t* pdc, foo *r) *)
              fun genInitFun(funName, argName, argPCT, bodySs, dummy) = 
		  let val paramTys = [P.ptrPCT PL.toolStatePCT, 
				      P.ptrPCT argPCT]
		      val paramNames = [ts, argName]
		      val formalParams = List.map P.mkParam (ListPair.zip (paramTys, paramNames))
		      val chkTSSs = if dummy then [] 
				    else [PT.IfThen(P.orX(P.notX(PT.Id ts),(P.notX(PT.Id argName))), 
						   PT.Return PL.PDC_ERROR)]
		      val bodySs = chkTSSs @ bodySs @ [PT.Return PL.PDC_OK]
		      val returnTy =  PL.toolErrPCT
		      val initFunED = 
			  P.mkFunctionEDecl(funName, formalParams, 
					    PT.Compound bodySs, returnTy)
		  in
		      initFunED
		  end

              fun genInitTmpStrSs str = [P.varDeclS'(PL.sfioPCT, str),
					 PT.IfThen(P.notX(P.assignX(PT.Id str, PL.sfstropen)),
						   PT.Compound[PT.Return PL.PDC_ERROR])]

              (* PDC_error_t foostruct_report(PDC_t* pdc, const char * prefix,
	                                      const char* what, int nst, foostruct_acc* acc) *)
              fun genExternalReport(reportName, intlParamNames, formalParams) = 
		  let val resDecl = P.varDeclS'(PL.toolErrPCT, result)
		      val initTmpStrSs = genInitTmpStrSs outstr
                      val chkTSandAccS = PT.IfThen(P.orX(
						     P.orX(P.notX(PT.Id ts), P.notX(PT.Id acc)), 
						     P.notX discX),
						   PT.Compound[PT.Return PL.PDC_ERROR])
		      val chkErrorFS = PT.IfThen(P.notX errorFX, PT.Compound[PT.Return PL.PDC_OK])
		      val internalCallS = P.assignS(PT.Id result,
						    PT.Call(PT.Id (iSuf reportName),
							    List.map PT.Id intlParamNames))
                      val reportS = PT.IfThen(P.eqX(PL.PDC_OK, PT.Id result),
					      PT.Compound[
							  PT.Expr(PT.Call(errorFX,
							      [P.zero, P.zero, 
							       PT.String "%s", PL.sfstruse (PT.Id outstr)]))])
		      val closeSs = [PL.sfstrclose(PT.Id outstr), PT.Return (PT.Id result)]
		      val bodySs =  (resDecl :: initTmpStrSs) 
			          @ (chkTSandAccS :: chkErrorFS :: internalCallS :: reportS :: closeSs)
		      val returnTy =  PL.toolErrPCT
		      val reportFunED = 
			  P.mkFunctionEDecl(reportName, formalParams, PT.Compound bodySs, returnTy)
		  in
		      reportFunED
		  end
              (* PDC_error_t foostruct_report(PDC_t* pdc, [sfio_t *str], const char * prefix,
	                                      const char* what, int nst, foostruct_acc* acc) *)
	      fun genReportFuns (reportName, whatStr, accPCT,intlBodySs) = 
		  let fun genParamTys extraPCTs =
		          [P.ptrPCT PL.toolStatePCT] 
			 @ extraPCTs
			 @[P.ccharPtr,
			   P.ccharPtr,
			   P.int,
			   P.ptrPCT accPCT]
                      fun genParamNames extraNames = [ts] @ extraNames @ [ prefix, what, nst, acc]
                      val intlParamNames = genParamNames [outstr]
                      val extlFormalParams = List.map P.mkParam (ListPair.zip (genParamTys [], genParamNames []))
		      val intlFormalParams = List.map P.mkParam 
			                        (ListPair.zip (genParamTys [PL.sfioPCT], intlParamNames))
 		      val initTmpStrSs = genInitTmpStrSs tmpstr
		      val setPrefixS = PT.IfThen(P.orX(P.notX(PT.Id prefix), P.eqX(P.zero, P.starX(PT.Id prefix))),
						 PT.Compound[P.assignS(PT.Id prefix, PT.String "<top>")])
		      val setWhatS = PT.IfThen(P.notX(PT.Id what),
						 PT.Compound[P.assignS(PT.Id what, PT.String whatStr)])
                      val printNstS = PL.nstPrefixWhat(PT.Id outstr, P.addrX(PT.Id nst), PT.Id prefix, PT.Id what)
		      val intlBodySs = intlBodySs (* parameter from above *)
		      val closeSs = [PL.sfstrclose(PT.Id tmpstr), PT.Return PL.PDC_OK]
		      val bodySs = initTmpStrSs
			          @[setPrefixS, setWhatS, printNstS]
			          @ intlBodySs
			          @ closeSs
		      val bodyS = PT.Compound bodySs
		      val returnTy = PL.toolErrPCT
		      val internalReportFunED = P.mkFunctionEDecl(iSuf reportName, intlFormalParams, bodyS, returnTy)
		      val externalReportFunED = genExternalReport(reportName, intlParamNames, extlFormalParams)
		  in
		      [internalReportFunED, externalReportFunED]
		  end

                  (* const char * name2str(enumPCT which) *)
                  fun genEnumToStringFun(name, enumPCT, members) = 
  		      let val cnvName = toStringSuf name
			  val which = "which"
			  val paramNames = [which]
			  val paramTys = [enumPCT]
			  val formalParams = List.map P.mkParam(ListPair.zip(paramTys, paramNames))
			  fun cnvOneBranch (bname, _, _) = 
			      [PT.CaseLabel(PT.Id bname, PT.Return (PT.String bname))]
			  val defBranch = 
			      [PT.DefaultLabel(PT.Return (PT.String "* unknown meth *"))]
			  val branches = (List.concat(List.map cnvOneBranch members)) @ defBranch
			  val bodySs = [PT.Switch ((PT.Id which), PT.Compound branches)]
			  val returnTy = P.ccharPtr
			  val cnvFunED = 
			      P.mkFunctionEDecl(cnvName, formalParams, PT.Compound bodySs, returnTy)
		      in
			  cnvFunED
		      end
			 


             (*  PDC_error_t T_acc_name(PDC_t* , T_acc* ) *)
	      fun gen3PFun (name, accPCT, bodySs) = 
		  let val paramTys = [P.ptrPCT PL.toolStatePCT, P.ptrPCT accPCT]
                      val paramNames = [ts, acc]
                      val formalParams = List.map P.mkParam (ListPair.zip (paramTys, paramNames))
		      val returnTy =  PL.toolErrPCT

		      val zeroFunED = 
			  P.mkFunctionEDecl(name, formalParams, PT.Compound bodySs, returnTy)
		  in
		      zeroFunED
		  end

              fun chk3Pfun (funName, e) = 
		  [PT.IfThen(P.eqX(PL.PDC_ERROR, 
				   PT.Call(PT.Id funName, 
					   [PT.Id ts, e])),
			     PT.Compound[PT.Expr(P.postIncX (PT.Id nerr))])]


              (*  PDC_error_t T_acc_add (PDC_t* , T_acc* , T_ed*, T* ) *)
	      fun genAddFun (addName, accPCT, edPCT, repPCT, bodySs) = 
		  let val paramTys = [P.ptrPCT PL.toolStatePCT, 
				      P.ptrPCT accPCT, 
				      P.ptrPCT edPCT,
				      P.ptrPCT repPCT]
                      val paramNames = [ts, acc, ed, rep]
                      val formalParams = List.map P.mkParam (ListPair.zip (paramTys, paramNames))
		      val returnTy =  PL.toolErrPCT
		      val addFunED = 
			  P.mkFunctionEDecl(addName, formalParams, PT.Compound bodySs, returnTy)
		  in
		      addFunED
		  end



              fun chkAddFun (funName, accX,edX,repX) = 
		  [PT.IfThen(P.eqX(PL.PDC_ERROR, 
				   PT.Call(PT.Id funName, 
					   [PT.Id ts, accX, edX, repX])),
			     PT.Compound[PT.Expr(P.postIncX (PT.Id nerr))])]

              fun chkPrint (bodyX) = 		   
		  PT.IfThen(
		      P.eqX(PL.PDC_ERROR, bodyX), 
		      PT.Compound[PL.sfstrclose (PT.Id tmpstr),
				  PT.Return PL.PDC_ERROR])

              fun printScaffolding (fieldDescriptor, extraArgsXs, bodyX) = 
		  [PL.sfprintf(PT.Id tmpstr, PT.String ("%s."^fieldDescriptor),[PT.Id prefix]@extraArgsXs),
		   chkPrint bodyX]

              fun genPrintPiece(reportName, fieldDescriptor, whatX, fieldX, extraArgsXs) = 
                  let val bodyX = PT.Call(PT.Id reportName, 
				    [PT.Id ts, PT.Id outstr, PL.sfstruse (PT.Id tmpstr), whatX, PT.Id nst, 
				     fieldX])
		  in
		      printScaffolding(fieldDescriptor, extraArgsXs, bodyX)
		  end

	      fun callIntPrint (reportName, prefixX, whatX, nstX, fieldX) = 
		  PT.Call(PT.Id reportName, 
			  [PT.Id ts, PT.Id outstr, prefixX, whatX, nstX, fieldX])

	      fun callEnumPrint (reportName, prefixX, whatX, nstX, mapFnX, fieldX) = 
		  PT.Call(PT.Id reportName, 
			  [PT.Id ts, PT.Id outstr,  prefixX, whatX, nstX,
			   PT.Cast(PL.intCvtPCT, mapFnX),
			   fieldX])

              fun genEnumPrint(reportName, fieldDescriptor, prefixX, whatX, nstX, mapFnX, fieldX) = 
		  let val bodyX = callEnumPrint(reportName, prefixX, whatX, nstX, mapFnX, fieldX)
		  in
		      printScaffolding(fieldDescriptor, [], bodyX)
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
                                      
              fun reportErrorSs(locCodeSs, shouldIncNerr, errCodeC, shouldPrint, msg, args) = 
		  let val errCodeX = fieldX(ed,errCode)
		      val msgX = if msg = "" then P.zero else PT.String msg
		      val nErrSs = if shouldIncNerr 
			           then [P.postIncS (fieldX(ed,nerr))]
				   else []
                      val printSs = if shouldPrint 
				    then [PL.userErrorS(PT.Id ts, locX, 
							errCodeX, msgX, args)]
				    else []
		  in
                     nErrSs
                    @[P.assignS(fieldX(ed,errCode), errCodeC)]
		    @ locCodeSs
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

	      (* Given a manifest field description, calculate shared properties *)
	      fun genTyPropsMan {decl, comment:string option} =
		  let val ctNoptEs = cnvDeclaration decl
		      val (cty,nameOpt,_) = hd ctNoptEs
		      val name = case nameOpt of NONE => "" | SOME n => (n^" ")
		      val isStatic = CisStatic cty
		      val () = if not isStatic then 
			  PE.error ("Representation of manifest field "^name
				    ^"contains a pointer.")
			       else ()
		  in
		      [{diskSize = TyProps.Size (0,0), 
			memChar = if isStatic 
				      then TyProps.Static 
				  else TyProps.Dynamic,
				      endian = false, isRecord = false}]
		  end

	      (* Given a manifest field description, generate canonical representation *)
	      fun genRepMan {decl, comment:string option} = 
		  let val ctNoptEs = cnvDeclaration decl
		      fun doOne (cty, nameOpt, exp) = 
			  let val name = (case nameOpt of NONE =>
					      (PE.error "Manifest fields must have a name."; "bogus")
			                  | SOME n => n )
			      val () = (case exp of PT.EmptyExpr =>
					    PE.error "Manifest fields must have an initializing expression." 
 			                  | _ => ())
			      val defStringOpt =  SOME(P.expToString exp)
			      val fullCommentOpt = stringOptMerge(comment, defStringOpt)
			      val ty = case reverseLookup cty 
				       of NONE => CTtoPTct cty
				       | SOME pty => (P.makeTypedefPCT(lookupTy(pty, repSuf, #repname)))
			  in
			      (name, ty, fullCommentOpt)
			  end
		  in
		      List.map doOne ctNoptEs
		  end
	      
	      (* Given representation of manifest field, generate accumulator representation. *)
	      fun genAccMan {decl, comment:string option} = 
		  let val ctNoptEs = cnvDeclaration decl
		      fun doOne (cty, nameOpt, exp) = 
			  let val name = case nameOpt of NONE => "bogus"
			| SOME n => n  
			      val defStringOpt = SOME(P.expToString exp)
			      val fullCommentOpt = stringOptMerge(comment, defStringOpt)
			  in
			      case reverseLookup cty 
				  of SOME pty => 
				      [(name, P.makeTypedefPCT (valOf (lookupAcc pty)), fullCommentOpt)]
				| NONE => []
			  end
		  in
		      List.concat(List.map doOne ctNoptEs)
		  end

	      fun cnvPtyMan (theName, acc, name)  = 
		  let val fieldX = P.addrX(P.arrowX(PT.Id acc, PT.Id name))
		  in
		      chk3Pfun (theName, fieldX)
		  end

              fun genAssignMan(cty, name, repX, exp) = 
		  let fun assignS exp = 
		      case exp 
			  of PT.MARKexpression(loc,exp) => assignS exp
			| PT.EmptyExpr => P.assignS(repX, P.zero)
			| PT.InitList l => 
			      PT.Compound[P.varDeclS(CTtoPTct cty, name, exp),
					  P.assignS(repX, PT.Id name)]
			| exp =>
			      (expAssignTy(exp, [cty], 
					   fn s=> ("Value for field "^
						   name ^ " " ^
						   "has type: "^s^", expected type "^
						   (CTtoString cty)^".\n"));
			       P.assignS(repX, exp))
		  in
		      assignS exp
		  end

	      (* Given manifest representation, generate accumulator functions(init,reset, cleanup *)
	      fun genAccTheMan theSuf {decl, comment:string option} = 
		  let val ctNoptEs = cnvDeclaration decl
		      fun doOne(cty,nameOpt,exp) = 
			  let val name= case nameOpt of NONE => "bogus" | SOME n => n
			      val accPtyOpt = reverseLookup cty
			  in
			      case accPtyOpt of NONE => [] | 
				   SOME pty => (case lookupAcc(pty) of NONE => []
                                                | SOME a => cnvPtyMan(theSuf a, acc, name))
			  end
		  in
		      List.concat(List.map doOne ctNoptEs)
		  end

	      (* Given manifest represetation, generate accumulator function *)

	      fun cnvPtyForAdd (pty, name, errDescX) = 
		  case lookupAcc(pty) of NONE => []
		| SOME a => (
			     let val addName = addSuf a
				 fun gfieldX base = getFieldX(base,name)
			     in
				 [PT.IfThen(
					    P.eqX(PL.PDC_ERROR, 
						  PT.Call(PT.Id addName, 
							  [PT.Id ts, gfieldX acc, errDescX, gfieldX rep])),
					    PT.Compound[PT.Expr(P.postIncX (PT.Id nerr))])]
			     end
	      (* end accOpt SOME case *))


	      (* Given manifest representatin, generate report function *)
	      fun cnvPtyForReport(reportSuf, iSuf, pty, name) = 
		  case lookupAcc(pty) of NONE => []
		| SOME a => (
			     let val reportName = reportSuf a
				 fun gfieldX base = getFieldX(base,name)
			     in
				 genPrintPiece(iSuf reportName, name, P.zero, gfieldX acc,[])
			     end
	      (* end accOpt SOME case *))

	      fun genAccReportMan (reportSuf, iSuf) {decl, comment:string option} = 
		  let val ctNoptEs = cnvDeclaration decl
		      fun doOne(cty,nameOpt,exp) = 
			  let val name= case nameOpt of NONE => "bogus" | SOME n => n
			      val accPtyOpt = reverseLookup cty
			  in
			      case accPtyOpt of NONE => [] 
			    | SOME pty => (cnvPtyForReport(reportSuf, iSuf, pty,name))
			  end
		  in
		      List.concat(List.map doOne ctNoptEs)
		  end


	      fun cnvPTypedef ({name : string, params: (pcty * pcdecr) list, isRecord,
			        baseTy: PX.Pty, args: pcexp list, 
			        predTy: PX.Pty, thisVar: string, pred: pcexp}) = 
		  let val base = "base"
		      val user = "user"
		      val baseTyName = lookupTy(baseTy, fn s => s, #padsname)

		      (* Insert type properties into type table *)
                      val ds = lookupDiskSize baseTy
                      val mc = lookupMemChar baseTy
                      val endian = lookupEndian baseTy
		      val isRecord = lookupRecord baseTy orelse isRecord
		      val typedefProps = buildTyProps(name, ds, mc,endian,isRecord)
                      val () = PTys.insert(Atom.atom name, typedefProps)

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
                      val edFields  = [(nerr, P.int, NONE), (errCode, PL.errCodePCT, NONE),
				       (loc, PL.locPCT,NONE), (panic, P.int, NONE),
				       (base, baseEDPCT, SOME "Base error description")]
		      val edED      = P.makeTyDefStructEDecl (edFields, edSuf name)
		      val edDecls   = cnvExternalDecl edED
                      val edPCT     = P.makeTypedefPCT (edSuf name)		

  		      (* Generate accumulator type *)
		      val PX.Name baseName = baseTy
		      val baseAccPCT = case PBTys.find(PBTys.baseInfo, Atom.atom baseName) 
			               of NONE => P.makeTypedefPCT (accSuf baseName)  (* must have been generated *)
                                       | SOME(b:PBTys.baseInfoTy) => 
					   (case (#accname b) 
					    of NONE => P.voidPtr   (* accumulation not defined for this base type *)
 			                    | SOME acc => (P.makeTypedefPCT (Atom.toString acc)))
		      val accED     = P.makeTyDefEDecl (baseAccPCT, accSuf name)
		      val accDecls  = cnvExternalDecl accED
		      val accPCT    = P.makeTypedefPCT (accSuf name)		

                      (* Generate read function *)
                      (* -- Some helper functions *)
		      val readName = readSuf name
                      val baseReadFun = lookupTy(baseTy, iSuf o readSuf, #readname)
		      val () = checkParamTys(name, baseReadFun, args, 2, 2)
		      val modPredX = PTSub.substExp (thisVar, P.starX(PT.Id (gMod rep)), pred)
		      fun chk () = expEqualTy(modPredX, CTintTys, 				
					  fn s=> (" constraint for typedef "^
						  name ^ " has type: " ^ s ^
						  ". Expected an int."))

                      fun genReadSs () = 
			  let val resDeclSs = [P.varDeclS'(P.int, result)]
			      val readBaseSs = 
				  [P.assignS(PT.Id result, 
					     PL.readFunX(baseReadFun, 
							 PT.Id ts, 
							 P.addrX (fieldX(em,base)),
							 args,
							 P.addrX (fieldX(ed,base)),
							 PT.Id (gMod rep))),
				   PT.IfThen(P.eqX(PT.Id result, PL.PDC_ERROR),
					     PT.Goto (findEORSuf name))]

			      val checkConstraintSs = 
				  [PT.IfThen(P.andX(P.lteX(fieldX(em,user), PL.EM_CHECK),
						    P.notX modPredX),
					     PT.Compound (reportErrorSs([locS],
									true,
									PL.PDC_TYPEDEF_CONSTRAINT_ERR,
									true,"", [])
							  @ [P.assignS(PT.Id result, PL.PDC_ERROR),
							     PT.Goto (findEORSuf name)])
					     )]
			      val slurpSs = if isRecord then genReadEOR reportStructErrorSs () else []
			      val endSs = [PT.Labeled(findEORSuf name, 
						     PT.Compound(slurpSs @ [PT.Return (PT.Id result)]))]
		      in
			  [PT.Compound (resDeclSs @ readBaseSs @ checkConstraintSs @ endSs)]
		      end

                      (* -- Assemble read function *)
		      val _ = pushLocalEnv()                                        (* create new scope *)
		      val () = ignore (insTempVar(gMod rep, P.ptrPCT canonicalPCT)) (* add modrep to scope *)
		      val cParams : (string * pcty) list = List.map mungeParam params
                      val () = ignore (List.map insTempVar cParams)  (* add params for type checking *)
		      val () = chk()
		      val readFields = genReadSs ()                                   (* does type checking *)
		      val _ = popLocalEnv()                                         (* remove scope *)
		      val bodySs = readFields 
		      val readFunEDs = genReadFun(readName, cParams, emPCT,edPCT,canonicalPCT, 
						  NONE, true, bodySs)


                      (* -- generate accumulator init, reset, and cleanup functions (typedef case) *)
		      fun genResetInitCleanup theSuf = 
			  let val theFun = (theSuf o accSuf) name
			  in case lookupAcc baseTy 
			      of NONE => (gen3PFun(theFun, accPCT, 
						   [P.mkCommentS ("Accumulation not defined for base type of ptyepdef."),
						    PT.Return PL.PDC_OK])
			                                     (* end NONE *))
				| SOME a => (
				   let val theBodyE = PT.Call(PT.Id (theSuf a), 
							      [PT.Id ts, PT.Id acc])
				       val theReturnS = PT.Return theBodyE
				       val theFunED = gen3PFun(theFun, accPCT, [theReturnS])
				   in
				      theFunED
				   end
				       (* end SOME *))
			  end
		      val initFunED = genResetInitCleanup initSuf
		      val resetFunED = genResetInitCleanup resetSuf
                      val cleanupFunED = genResetInitCleanup cleanupSuf

                      (* -- generate accumulator function *)
                      (*  PDC_error_t T_acc_add (PDC_t* , T_acc* , T_ed*, T* ) *)
		      val addFun = (addSuf o accSuf) name
		      fun genAdd NONE = genAddFun(addFun, accPCT, edPCT, canonicalPCT, 
						  [P.mkCommentS ("Accumulation not defined for base type of ptypedef."),
						   PT.Return PL.PDC_OK])
                        | genAdd (SOME a) =
                           let val addX = PT.Call(PT.Id (addSuf  a), 
						  [PT.Id ts, PT.Id acc, 
						   P.addrX(P.arrowX(PT.Id ed,PT.Id base)), PT.Id rep])
			       val addReturnS = PT.Return addX
			       val addBodySs =  [addReturnS]
			   in
			       genAddFun(addFun, accPCT, edPCT, canonicalPCT, addBodySs)
			   end

                          (* end SOME case *)
                      val addFunED = genAdd (lookupAcc baseTy)

                      (* -- generate report function ptypedef *)
                      (*  PDC_error_t T_acc_report (PDC_t* , T_acc* , const char* prefix) *)
		      val reportFun = (reportSuf o accSuf) name
		      val reportFields = 
			  case lookupAcc(baseTy) of NONE => []
                             | SOME a => (
				 genPrintPiece((iSuf o reportSuf) a, name, P.zero, PT.Id acc,[])
		             (* end accOpt SOME case *))

                      val reportFunEDs = genReportFuns(reportFun, "typedef "^name, accPCT, reportFields)

                      (* Generate Init function (typedef case) *)
		      val baseFunName = lookupMemFun (PX.Name baseTyName)
		      val initFunName = lookupMemFun (PX.Name name)
                      fun genInitEDs (suf, argName, aPCT) = case #memChar typedefProps
                          of TyProps.Static => 
				  [genInitFun(suf initFunName, argName, aPCT, [],true)]
                           | TyProps.Dynamic =>
			      let val bodySs = 
				  [PT.Expr(
				    PT.Call(PT.Id (suf baseFunName),
					    [PT.Id ts, PT.Id rep]))]
			      in
				  [genInitFun(suf initFunName, argName, aPCT, bodySs,false)]
			      end
                      val initRepEDs = genInitEDs (initSuf, rep, canonicalPCT)
                      val initEDEDs  = genInitEDs ((initSuf o edSuf), ed, edPCT)
                      val cleanupRepEDs = genInitEDs (cleanupSuf, rep, canonicalPCT)
                      val cleanupEDEDs  = genInitEDs ((cleanupSuf o edSuf), ed, edPCT)

		  in
		        canonicalDecls
                      @ emDecls
                      @ edDecls
                      @ accDecls
		      @ (List.concat(List.map cnvExternalDecl initRepEDs))
		      @ (List.concat(List.map cnvExternalDecl initEDEDs))
		      @ (List.concat(List.map cnvExternalDecl cleanupRepEDs))
		      @ (List.concat(List.map cnvExternalDecl cleanupEDEDs))
                      @ (List.concat(List.map cnvExternalDecl readFunEDs))
		      @ cnvExternalDecl initFunED
                      @ cnvExternalDecl resetFunED
                      @ cnvExternalDecl cleanupFunED
                      @ cnvExternalDecl addFunED
		      @ (List.concat(List.map cnvExternalDecl reportFunEDs))
		  end

	      fun cnvPStruct ({name:string, isRecord, params: (pcty * pcdecr) list, 
			       fields : (pdty, pcdecr, pcexp) PX.PSField list}) = 
	          let (* Functions for walking over lists of struct elements *)
		      fun mungeField f b r m (PX.Full fd) = f fd
                        | mungeField f b r m (PX.Brief e) = b e
                        | mungeField f b r m (PX.EOR) = r ()
                        | mungeField f b r m (PX.Manifest md) = m md
		      fun mungeFields f b r m [] = []
			| mungeFields f b r m (x::xs) = (mungeField f b r m x) @ (mungeFields f b r m xs)

		      (* Generate local variables  *)
		      fun genLocFull {pty :PX.Pty, args : pcexp list, name:string, isVirtual:bool, 
				      isEndian:bool,
				      pred:pcexp option, comment:string option} = 
			  if not isVirtual then []
			  else [(name, P.makeTypedefPCT(lookupTy (pty,repSuf,#repname)))]
		      fun genLocBrief e = []
		      fun genLocEOR () = []
		      fun genLocMan m = []
		      val localVars = mungeFields genLocFull genLocBrief genLocEOR genLocMan fields

		      (* Generate canonical representation *)
		      fun genRepFull {pty :PX.Pty, args : pcexp list, name:string, 
				      isVirtual:bool, isEndian:bool, 
				      pred:pcexp option, comment:string option} = 
			  if not isVirtual then 
			    let val predStringOpt = Option.map P.expToString pred
			        val fullCommentOpt = stringOptMerge(comment, predStringOpt)
			    in
			      [(name,P.makeTypedefPCT(lookupTy (pty,repSuf,#repname)), fullCommentOpt )]
			    end
			  else []
		      fun genRepBrief e = []
		      fun genRepEOR () = []
		      val canonicalFields = mungeFields genRepFull genRepBrief genRepEOR genRepMan fields
		      val canonicalStructED = P.makeTyDefStructEDecl (canonicalFields, repSuf name)
		      val canonicalDecls = cnvExternalDecl canonicalStructED 
                      val canonicalPCT = P.makeTypedefPCT (repSuf name)			 

		      (* Calculate and insert type properties into type table *)
		      fun genTyPropsFull {pty :PX.Pty, args : pcexp list, name:string, 
					  isVirtual:bool,  isEndian : bool, 
				          pred:pcexp option, comment:string option} = 
			  let val mc = lookupMemChar pty
			      val ds = lookupDiskSize pty
                              val supportsEndian = lookupEndian pty
			      val () = if isEndian andalso not supportsEndian
				       then PE.error ("Endian annotation not supported on fields of type "
						      ^(tyName pty)^".\n")
				       else ()
			      val () = if isEndian andalso not (Option.isSome pred)
				       then PE.error ("Endian annotations require constraints ("^name^").\n")
				       else ()
			  in [{diskSize = ds, memChar = mc, endian = false, isRecord=false}] end
		      fun genTyPropsBrief e = [{diskSize = TyProps.Size (1,0), memChar = TyProps.Static, 
						endian = false, isRecord = false}]
		      fun genTyPropsEOR () =  [{diskSize = TyProps.Size (0,1), memChar = TyProps.Static, 
						endian = false, isRecord = true}]
		      val tyProps = mungeFields genTyPropsFull genTyPropsBrief genTyPropsEOR genTyPropsMan fields
		      fun mStruct((x1,x2),(y1,y2)) = TyProps.Size ((x1+y1),(x2+y2))
                      val {diskSize,memChar,endian,isRecord=_} = 
			  List.foldl (PTys.mergeTyInfo mStruct) PTys.minTyInfo tyProps
		      val structProps = buildTyProps(name,diskSize,memChar,endian,isRecord)
                      val () = PTys.insert(Atom.atom name, structProps)

		       
		      (* Generate error mask *)
		      fun genEMFull {pty :PX.Pty, args : pcexp list, name:string, 
				     isVirtual:bool, isEndian : bool, 
				     pred:pcexp option, comment} = 
			  [(name,P.makeTypedefPCT(lookupTy (pty,emSuf,#emname)), NONE)]
		      fun genEMBrief e = []
		      fun genEMEOR () = []
		      fun genEMMan m = []
		      val emFieldsNested = mungeFields genEMFull genEMBrief genEMEOR genEMMan fields
		      val auxEMFields = [(all, PL.base_emPCT, NONE)]
		      val emFields = auxEMFields @ emFieldsNested

		      val emFirstPCT = getFirstEMPCT emFields
		      val emStructED = P.makeTyDefStructEDecl (emFields, emSuf name)
		      val emDecls = cnvExternalDecl emStructED 
                      val emPCT = P.makeTypedefPCT (emSuf name)			  

		      (* Generate error description *)
		      fun genEDFull {pty :PX.Pty, args : pcexp list, name:string,  
				     isVirtual:bool, isEndian:bool, 
				     pred:pcexp option, comment} = 
			  [(name,P.makeTypedefPCT(lookupTy (pty,edSuf,#edname)),NONE)]
		      fun genEDBrief e = []
		      fun genEDEOR () = []
		      fun genEDMan () = []
		      val auxEDFields = [(nerr, P.int,NONE), (errCode, PL.errCodePCT, NONE),
					 (loc, PL.locPCT,NONE), (panic, P.int,NONE)]
		      val edFields = auxEDFields @ (mungeFields genEDFull genEDBrief genEDEOR genEMMan fields)
		      val edStructED = P.makeTyDefStructEDecl (edFields, edSuf name)
		      val edDecls = cnvExternalDecl edStructED 
                      val edPCT = P.makeTypedefPCT (edSuf name)			  

		      (* Generate accumulator type *)
		      fun genAccFull {pty :PX.Pty, args : pcexp list, name:string, 
				      isVirtual:bool,  isEndian:bool, 
				      pred:pcexp option, comment:string option} = 
			  if not isVirtual then 
			    let val predStringOpt = Option.map P.expToString pred
			        val fullCommentOpt = stringOptMerge(comment, predStringOpt)
				val accOpt = lookupAcc pty
			    in
				case accOpt of NONE => []
                                | SOME acc => 
  			          [(name,P.makeTypedefPCT acc, fullCommentOpt )]
			    end
			  else []
		      fun genAccBrief e = []
		      fun genAccEOR () = []
		      val accFields = mungeFields genAccFull genAccBrief genAccEOR genAccMan fields
		      val auxAccFields = [(nerr, PL.intAccPCT, NONE)]
		      val accStructED = P.makeTyDefStructEDecl (auxAccFields @ accFields, accSuf name)
		      val accDecls = cnvExternalDecl accStructED 
                      val accPCT = P.makeTypedefPCT (accSuf name)			 

                      (* Generate read function *)
                      (* -- Some useful names *)
		      val readName = readSuf name

                      (* -- collection of expressions to be substituted for in constraints *)
                      (* -- efficiency could be improved with better representations *)
                      val subList : (string * pcexp) list ref = ref []
                      fun addSub (a : string * pcexp) = subList := (a:: (!subList))

                      (* -- Some helper functions *)
		      val first = ref true
		      fun genReadFull {pty :PX.Pty, args:pcexp list, name:string, 
				       isVirtual:bool, isEndian:bool, 
				       pred:pcexp option, comment} = 
			  let val readFieldName = lookupTy(pty, iSuf o readSuf, #readname)
                              val modEdNameX = fieldX(ed,name)
			      val repX = if isVirtual then PT.Id name else fieldX(rep,name)
                              val () = if not isVirtual 
					   then addSub(name, fieldX(rep,name))  (* record additional binding *)
				       else ()
			      val modArgs = List.map (PTSub.substExps (!subList)) args
                              val () = checkParamTys(name, readFieldName, modArgs, 2, 2)
			      val comment = ("Reading field: "^ name ^ 
					     (if isEndian then ". Doing endian check." else "."))
			      val commentS = P.mkCommentS (comment)
			      val ifPanicSs = 				  PT.Compound 
                                   [(* moded->name.panic = true *)
				    P.assignS(P.dotX(modEdNameX, PT.Id panic),P.trueX),  
				    (* moded->name.errCode = PANIC_SKIPPED *)
				    P.assignS(P.dotX(modEdNameX, PT.Id errCode),PL.PDC_PANIC_SKIPPED),  
                                    PL.getLocS(PT.Id ts,  (* PDC_get_loc(ts, &moded->name.loc) *)
					       P.addrX(P.dotX(modEdNameX,PT.Id loc))),
				    (* moded->nerr += 1 *)
				    P.plusAssignS(fieldX (ed,nerr),P.intX 1)]
			      val ifNoPanicSs =
                                  PT.Compound ([
                                   PT.IfThenElse
                                    (P.eqX(PL.PDC_ERROR,
					   PL.readFunX(readFieldName, 
						       PT.Id ts, 
						       P.addrX(fieldX(em,name)),
						       modArgs,
						       P.addrX(fieldX(ed,name)),
						       P.addrX repX)),
				     PT.Compound( (* error reading field *)
				      [PT.IfThen(PL.getSpecLevelX(PT.Id ts),
						 PT.Compound[PT.Return PL.PDC_ERROR]),
                                       PT.IfThen(P.dotX(fieldX(ed, name), PT.Id panic),
				                 PT.Compound[P.assignS(fieldX(ed,panic), P.trueX)])]
				      @reportStructErrorSs(PL.PDC_STRUCT_FIELD_ERR, P.dotX(fieldX(ed, name), PT.Id loc))),
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
					       val reportErrSs = 
						   [P.assignS(P.dotX(fieldX(ed,name), PT.Id errCode), 
							      PL.PDC_STRUCT_FIELD_ERR),
						    PL.getLocS(PT.Id ts,
							       P.addrX(P.dotX(modEdNameX,PT.Id loc))),
						    PL.userErrorS(PT.Id ts,
								  P.addrX(P.dotX(fieldX(ed,name), PT.Id loc)),
								  P.dotX(fieldX(ed,name), PT.Id errCode),
								  PT.String("User constraint on field "^
									    name ^ " " ^
									    "violated."), []),
						    PT.IfThen(P.eqX(P.zero, fieldX(ed,nerr)),
						     PT.Compound
                                                      [P.assignS(fieldX(ed,errCode), 
								 PL.PDC_USER_CONSTRAINT_VIOLATION),
						       P.assignS(fieldX(ed,loc), 
								 P.dotX(fieldX(ed,name),PT.Id loc))
						       ]),
						    P.plusAssignS(fieldX(ed,nerr), P.intX 1)]

					       fun swap reportErrSs = 
                                                   [PL.swapBytesS(fieldX(rep, name)),
						    PT.IfThenElse(
						       exp,
                                                       PT.Compound
							[P.assignS(d_endianX,
								   P.condX(P.eqX(d_endianX,PL.bigEndian),
									   PL.littleEndian,
									   PL.bigEndian)),
							 PL.userWarnS(PT.Id ts, 
							    P.addrX(P.dotX(fieldX(ed,name),PT.Id loc)), 
							    PT.String ("New endian values: "^
								       "data = %s, machine = %s "^
								       "(from "^name^" field test)."),
							    [PL.end2StringX d_endianX,
							     PL.end2StringX m_endianX])],
						       PT.Compound
							([PL.swapBytesS(fieldX(rep, name)),
							  PT.IfThen(PL.getSpecLevelX(PT.Id ts),
								    PT.Compound[PT.Return PL.PDC_ERROR])]
							 @ reportErrSs))]
					   in
					       [PT.IfThen(
                                                 P.andX(P.lteX(getEMExp(fieldX(em,name)), PL.EM_CHECK),
							P.notX exp),
						 PT.Compound
					           (if isEndian then
						     swap reportErrSs
						    else reportErrSs))]
					   end
                                      ))])
			      val readS = if !first then (first:= false; ifNoPanicSs)
					  else PT.IfThenElse
					      (fieldX(ed,panic), (* if moded->panic *)
					       ifPanicSs, ifNoPanicSs)
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
							 PT.InitList[e, P.zero]),
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
                                  case PBTys.find(PBTys.baseInfo, Atom.atom pTyName)
                                  of NONE => (fn id => id)     (* don't know how to recover *)
                                  | SOME(b:PBTys.baseInfoTy) => (
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
								   expr, expr, P.trueX,
								   P.zero,
                                                                   P.addrX (PT.Id "n"))),
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
                                   PT.IfThen( (* PDC_ERROR == readFieldName(ts, &tem, &ted, e) *)
                                             PL.readFunChkX(PL.PDC_ERROR, 
							    readFieldName, 
							    PT.Id ts, 
							    P.addrX (PT.Id tem),
							    [],
							    P.addrX (PT.Id ted),
							    expr),
                                     PT.Compound(
				       [PT.IfThen(PL.getSpecLevelX(PT.Id ts),
					 	  PT.Compound[PT.Return PL.PDC_ERROR]),
				        PL.userErrorS(PT.Id ts, 
						      P.addrX(P.dotX(PT.Id ted, PT.Id loc)),
						      PL.PDC_MISSING_LITERAL,
						      PT.String "Missing separator: %s.", 
						      [PL.fmtStr(commentV)])]
					@ reportStructErrorSs(PL.PDC_MISSING_LITERAL, P.dotX(PT.Id ted,PT.Id loc))
					@[P.assignS(fieldX(ed,panic),P.trueX)]))]
			  in
			      [PT.Compound(
                                   [commentS, 
				    PT.Compound(
				     tedDecl 
				     :: litdecls
				     @ (genPanicRecovery pTyName notPanicSs))])]
			  end

		     (* Given manifest representation, generate operations to set representation *)
		     fun genReadMan {decl, comment} = 
			 let val ctNoptEs = cnvDeclaration decl
			     fun doOne (cty, nameOpt, exp) = 
				 let val name = case nameOpt of NONE => "bogus"
			       | SOME n => n  
				     val repX = fieldX(rep, name)
				     val () = addSub(name, repX) (* should this be here, or after the subst? *)
				     val comment = ("Computing field: "^ name ^ ".")
				     val commentS = P.mkCommentS (comment)
				     val exp = PTSub.substExps (!subList) exp
				     val initS = genAssignMan(cty,name,repX, exp)
				 in
				     [commentS, initS]
				 end
			 in
			     List.concat(List.map doOne ctNoptEs)
			 end


                      (* -- Assemble read function *)
		      val _ = pushLocalEnv()                                        (* create new scope *)
		      val () = ignore (insTempVar(gMod rep, P.ptrPCT canonicalPCT)) (* add modrep to scope *)
		      val () = ignore (insTempVar(gMod em,  P.ptrPCT emPCT))        (* add modem to scope *)
		      val () = ignore(List.map insTempVar localVars)                (* insert virtuals into scope *)
		      val cParams : (string * pcty) list = List.map mungeParam params
                      val () = ignore (List.map insTempVar cParams)  (* add params for type checking *)
		      val readFields = mungeFields genReadFull genReadBrief 
			                     (genReadEOR reportStructErrorSs) 
					     genReadMan fields  
		                                                                    (* does type checking *)
                      val readRecord = if isRecord then genReadEOR reportStructErrorSs () else []
		      val _ = popLocalEnv()                                         (* remove scope *)
		      val localDeclSs = List.map (P.varDeclS' o (fn(x,y) => (y,x))) localVars
		      val bodySs = if 0 = List.length localVars then (readFields @ readRecord)
			          else [PT.Compound (localDeclSs @ readFields @ readRecord)]
		      val returnS = genReturnChk (P.arrowX(PT.Id (gMod(ed)), PT.Id nerr))
		      val bodySs = bodySs @ [returnS]

		      val readFunEDs = genReadFun(readName, cParams, emPCT,edPCT,canonicalPCT, 
						  emFirstPCT, true, bodySs)


                      (* Generate Accumulator functions *)
                      (* -- generate accumulator init, reset, cleanup, and report functions *)
		      fun genResetInitCleanup theSuf = 
			  let val theFun = (theSuf o accSuf) name
			      val auxFields = chk3Pfun(theSuf PL.intAct, getFieldX(acc,nerr))
			      fun genAccTheFull {pty :PX.Pty, args:pcexp list, name:string, 
						 isVirtual:bool, isEndian:bool, 
						 pred:pcexp option, comment} = 
				  if not isVirtual then
				      case lookupAcc(pty) of NONE => []
				    | SOME a => cnvPtyMan(theSuf a,acc,name)
				  else []
			      fun genAccTheBrief e = []
			      fun genAccTheEOR ()= []


			      val theDeclSs = [P.varDeclS(P.int, nerr, P.zero)]
			      val theFields = mungeFields genAccTheFull genAccTheBrief 
				                 genAccTheEOR (genAccTheMan theSuf) fields
			      val theReturnS = genReturnChk (PT.Id nerr)
			      val theBodySs = theDeclSs @ auxFields @ theFields @ [theReturnS]
			      val theFunED = gen3PFun(theFun, accPCT, theBodySs)
			  in
			      theFunED
			  end
		      val initFunED = genResetInitCleanup initSuf
		      val resetFunED = genResetInitCleanup resetSuf
                      val cleanupFunED = genResetInitCleanup cleanupSuf

                      (* -- generate accumulator function *)
                      (*  PDC_error_t T_acc_add (PDC_t* , T_acc* , T_ed*, T* ,) *)
		      val addFun = (addSuf o accSuf) name
		      val addDeclSs = [P.varDeclS(P.int, nerr, P.zero),  P.varDeclS'(PL.base_edPCT, ted)]
		      val initTedSs = [P.assignS(P.dotX(PT.Id ted, PT.Id errCode), 
						 P.arrowX(PT.Id ed, PT.Id errCode))]

		      fun genAccAddFull {pty :PX.Pty, args:pcexp list, name:string, 
					 isVirtual:bool, isEndian:bool, 
					 pred:pcexp option, comment} = 
			  if not isVirtual then cnvPtyForAdd(pty,name, getFieldX(ed,name)) else []
                      fun genAccAddBrief e = []
                      fun genAccAddEOR () = []
		      fun genAccAddMan {decl, comment:string option} = 
			  let val ctNoptEs = cnvDeclaration decl
			      fun doOne(cty,nameOpt,exp) = 
				  let val name= case nameOpt of NONE => "bogus" | SOME n => n
				      val accPtyOpt = reverseLookup cty
				  in
				      case accPtyOpt of NONE => [] 
				    | SOME pty => (
 				       [PT.Compound(
					 [P.varDeclS'(P.makeTypedefPCT(lookupTy(pty, edSuf, #edname)), ted),
					  P.assignS(P.dotX(PT.Id ted,PT.Id errCode),
						    P.arrowX(PT.Id ed, PT.Id errCode))]
						    @ cnvPtyForAdd(pty,name, P.addrX(PT.Id ted)))]
				  (* end case *))
				  end
			  in
			      List.concat(List.map doOne ctNoptEs)
			  end

		      val addNErrSs = chkAddFun(addSuf PL.intAct, getFieldX(acc,nerr), 
						P.addrX(PT.Id ted), 
						PT.Cast(P.ptrPCT PL.intPCT, getFieldX(ed,nerr)))

		      val addFields = mungeFields genAccAddFull genAccAddBrief 
			                 genAccAddEOR genAccAddMan fields
		      val addReturnS = genReturnChk (PT.Id nerr)
                      val addBodySs = addDeclSs @ initTedSs @ addNErrSs @ addFields @ [addReturnS]
                      val addFunED = genAddFun(addFun, accPCT, edPCT, canonicalPCT, addBodySs)

                      (* -- generate report function pstruct *)
                      (*  PDC_error_t T_acc_report (PDC_t* , T_acc* , const char* prefix , ) *)
		      val reportFun = (reportSuf o accSuf) name

		      val reportNerrSs = [chkPrint(
 				         callIntPrint((iSuf o reportSuf) PL.intAct, PT.String "Errors", 
							 PT.String "errors", P.intX ~1, getFieldX(acc,nerr))) ]
		      val headerSs = [PL.sfprintf(PT.Id outstr, 
						  PT.String "\n[Describing each field of %s]\n", 
						  [PT.Id prefix])]

		      fun genAccReportFull {pty :PX.Pty, args:pcexp list, name:string, 
					    isVirtual:bool, isEndian:bool, 
					    pred:pcexp option, comment} = 
			  if not isVirtual then cnvPtyForReport(reportSuf, iSuf, pty,name)
			  else []
                      fun genAccReportBrief e = []
                      fun genAccReportEOR () = []
		      val reportFields = mungeFields genAccReportFull genAccReportBrief 
			                  genAccReportEOR (genAccReportMan (reportSuf, iSuf)) fields
                      val reportFunEDs = genReportFuns(reportFun, "struct "^name, accPCT, 
						       (* reportNerrSs @ *)headerSs @ reportFields)

                      (* Generate Init Function struct case *)
		      val initFunName = lookupMemFun (PX.Name name)
                      fun genInitEDs(suf,base,aPCT) = case #memChar structProps
			  of TyProps.Static => [genInitFun(suf initFunName, base, aPCT, [],true)]
			   | TyProps.Dynamic => 
			       let fun genInitFull {pty as PX.Name tyName :PX.Pty, args : pcexp list, 
						    name:string, isVirtual:bool, isEndian:bool,
						    pred:pcexp option, comment:string option} = 
				   if not isVirtual then
				       if TyProps.Static = lookupMemChar pty then []
				       else let val baseFunName = lookupMemFun (PX.Name tyName)
					    in
					      [PT.Expr(
					        PT.Call(PT.Id (suf baseFunName),
							[PT.Id ts, 
							 P.addrX(P.arrowX(
								       PT.Id base,
								       PT.Id name))]))]
					    end
				   else []
				   fun genInitBrief _ = []
				   fun genInitEOR _ = []
				   fun genInitMan _ = []
				   val bodySs = mungeFields genInitFull genInitBrief 
				                     genInitEOR genInitMan fields
			       in
				   [genInitFun(suf initFunName, base, aPCT, bodySs,false)]
		               end
		      val initRepEDs = genInitEDs (initSuf, rep, canonicalPCT)
                      val initEDEDs  = genInitEDs (initSuf o edSuf, ed, edPCT)
		      val cleanupRepEDs = genInitEDs (cleanupSuf, rep, canonicalPCT)
                      val cleanupEDEDs  = genInitEDs (cleanupSuf o edSuf, ed, edPCT)
	      in 
 		   canonicalDecls (* converted earlier because used in typechecking constraints *)
                 @ emDecls
                 @ edDecls
                 @ accDecls
                 @ (List.concat(List.map cnvExternalDecl initRepEDs))
                 @ (List.concat(List.map cnvExternalDecl initEDEDs))
                 @ (List.concat(List.map cnvExternalDecl cleanupRepEDs))
                 @ (List.concat(List.map cnvExternalDecl cleanupEDEDs))
                 @ (List.concat(List.map cnvExternalDecl readFunEDs))
                 @ cnvExternalDecl initFunED
                 @ cnvExternalDecl resetFunED
                 @ cnvExternalDecl cleanupFunED
                 @ cnvExternalDecl addFunED
                 @ (List.concat(List.map cnvExternalDecl reportFunEDs))
	      end

	     fun cnvPUnion ({name:string, params: (pcty * pcdecr) list, 
			     isRecord : bool, variants : (pdty, pcdecr, pcexp) PX.PBranches}) = 
		 let (* Some useful names *)
		     val unionName = name
                     val value = "val"
		     val tag = "tag"
		     fun tgSuf s = s^"_tag"
		     fun unSuf s = s^"_u"
                     fun unionBranchX (name) = P.addrX(P.dotX(P.arrowX(PT.Id rep, PT.Id value), PT.Id name))

		     (* Functions for walking over list of variants *)
		     fun mungeVariant f b m (PX.Full fd) = f fd
		       | mungeVariant f b m (PX.Brief e) = b e
		       | mungeVariant f b m (PX.Manifest md)  = m md
		       | mungeVariant f b m _  = raise Fail "Illegal mode in union"
		     fun mungeVariants f b m [] = []
		       | mungeVariants f b m (x::xs) = (mungeVariant f b m x) @ (mungeVariants f b m xs)

		     (* Functions for walking over list of branch,variant *)
		     fun mungeBV f b m eopt (PX.Full fd) = f (eopt, fd)
		       | mungeBV f b m eopt (PX.Brief e) = b (eopt, e)
		       | mungeBV f b m eopt (PX.Manifest md)  = m (eopt, md)
		       | mungeBV f b m _ _  = raise Fail "Illegal mode in union"
		     fun mungeBVs f b m [] [] = []
		       | mungeBVs f b m (x::xs) (y::ys) = (mungeBV f b m x y) @ (mungeBVs f b m xs ys)

                     (* Function for moving default clause to end of switched union branch list *)
                     fun mungeBranches (cases, branches) = 
                       let fun mB([],[], acs, abs, acds, abds) = 
			          if (List.length acds >= 2) 
				      then (PE.error ("Switched union "^ unionName ^" can have at most "^
						      "one default clause.\n");
					    ((List.rev acs)@[hd acds], (List.rev abs)@[hd abds]))
					    
				  else ((List.rev acs)@acds, (List.rev abs)@abds)
                             | mB(NONE::cases, b::bs, acs,abs,acds,abds) = 
					    mB(cases,bs, acs, abs, NONE::acds, b::abds)
                             | mB(c::cases, b::bs, acs,abs,acds,abds) = 
					    mB(cases,bs, c::acs, b::abs, acds, abds)
		       in
			   mB(cases,branches,[],[],[],[])
		       end


                     val branches = variants
                     val (descOpt, cases, variants) = 
				    case branches 
			            of PX.Ordered v => (NONE, [], v)
			            |  PX.Switched {descriminator, cases, branches} => 
					    let val (cases,branches) = mungeBranches(cases,branches)
					    in
						(SOME descriminator, cases, branches)
					    end

		     (* Calculate and insert type properties into type table *)
		     fun genTyPropsFull {pty :PX.Pty, args : pcexp list, name:string, 
					 isVirtual:bool, isEndian:bool, 
					 pred:pcexp option, comment:string option} = 
			  let val mc = lookupMemChar pty
			      val ds = lookupDiskSize pty
			      val isRecord = lookupRecord pty 
			  in [{diskSize = ds, memChar = mc, endian = false, isRecord = isRecord}] end
		     fun genTyPropsBrief e = [] (* not used in unions *)
		     val tyProps = mungeVariants genTyPropsFull genTyPropsBrief genTyPropsMan variants
                     (* check that all variants are records if any are *)
		     val () = case tyProps of [] => ()
			      | ({isRecord=first,...}::xs) => 
			           (if List.exists (fn {isRecord,diskSize,memChar,endian}=> not (isRecord = first)) xs 
				    then PE.error "All branches of union must terminate record if any branch does."
				    else ())
					
		     fun mUnion (x,y) = if (x = y) then TyProps.Size x else TyProps.Variable
		     val {diskSize,memChar,endian,isRecord=_} = 
			 List.foldr (PTys.mergeTyInfo mUnion) PTys.minTyInfo tyProps
		     val unionProps = buildTyProps(name,diskSize,memChar,endian,isRecord)
                     val () = PTys.insert(Atom.atom name, unionProps)

                     (* generate enumerated type describing tags *)
		     val tagVal = ref 0
		     val firstTag = ref "bogus"
		     fun chkTag(name) = 			 
			 (if !tagVal = 0 then firstTag := name else ();
			  tagVal := !tagVal + 1;
			  [(name,P.intX(!tagVal),NONE)])

		     fun genTagFull {pty :PX.Pty, args : pcexp list, name:string, 
				     isVirtual:bool, isEndian:bool, 
				     pred:pcexp option, comment:string option} = 
			 chkTag(name)
		     fun genTagBrief e = []
                     fun genTagMan {decl,comment} = 
			 let val ctNoptEs = cnvDeclaration decl
			     val (cty,nameOpt,_) = hd ctNoptEs
			 in
			     case nameOpt of NONE => [] | SOME n => chkTag n
			 end

		     val tagFields = mungeVariants genTagFull genTagBrief genTagMan variants
		     val tagFieldsWithError = (errSuf name, P.zero, NONE) :: tagFields 
		     val tagED = P.makeTyDefEnumEDecl(tagFieldsWithError, tgSuf name)
		     val tagDecls = cnvExternalDecl tagED
		     val tagPCT = P.makeTypedefPCT(tgSuf name)

                     (* generate canonical representation *)
		     fun genRepFull {pty :PX.Pty, args : pcexp list, name:string, 
				     isVirtual:bool, isEndian:bool, 
				     pred:pcexp option, comment:string option} = 
			 let val predStringOpt = Option.map P.expToString pred
			     val fullCommentOpt = stringOptMerge(comment, predStringOpt)
			 in
			     [(name,P.makeTypedefPCT(lookupTy (pty,repSuf,#repname)), fullCommentOpt )]
			 end
		     fun genRepBrief e = (PE.error "Unions do not currently support brief fields.\n"; [])
		     val canonicalVariants = mungeVariants genRepFull genRepBrief genRepMan variants
		     val unionED = P.makeTyDefUnionEDecl(canonicalVariants, unSuf name)
		     val unionDecls = cnvExternalDecl unionED
                     val unionPCT = P.makeTypedefPCT(unSuf name)
                     val structFields = [(tag, tagPCT, NONE),
					 (value, unionPCT, NONE)]
		     val canonicalStructED = P.makeTyDefStructEDecl (structFields, repSuf name)
		     val canonicalDecls = cnvExternalDecl canonicalStructED 
                     val canonicalPCT = P.makeTypedefPCT (repSuf name)			 

		      (* Generate error mask *)
		     fun genEMFull {pty :PX.Pty, args : pcexp list, name:string, 
				    isVirtual:bool, isEndian:bool,
				    pred:pcexp option, comment} = 
			 [(name,P.makeTypedefPCT(lookupTy (pty,emSuf,#emname)), NONE)]
		     fun genEMBrief e = []
		     fun genEMMan m = []
		     val emFields = mungeVariants genEMFull genEMBrief genEMMan variants
		     val emFirstPCT = getFirstEMPCT emFields
		     val emStructED = P.makeTyDefStructEDecl (emFields, emSuf name)
		     val emPCT = P.makeTypedefPCT (emSuf name)			  

		     (* Generate error description *)
		     fun genEDFull {pty :PX.Pty, args : pcexp list, name:string, 
				    isVirtual:bool, isEndian:bool,
				    pred:pcexp option, comment} = 
			 [(name,P.makeTypedefPCT(lookupTy (pty,edSuf,#edname)),NONE)]
		     fun genEDBrief e = []
		     fun genEDMan m = []
		     val auxEDFields = [(nerr, P.int,NONE), (errCode, PL.errCodePCT, NONE),
					(loc, PL.locPCT,NONE), (panic, P.int,NONE),
					(tag, PL.base_edPCT,NONE)]
		     val edFields = auxEDFields @ (mungeVariants genEDFull genEDBrief genEDMan variants)
		     val edStructED = P.makeTyDefStructEDecl (edFields, edSuf name)
		     val edPCT = P.makeTypedefPCT (edSuf name)			  

		     (* Generate accumulator type *)
		     fun genAccFull {pty :PX.Pty, args : pcexp list, name:string, 
				     isVirtual:bool, isEndian:bool, 
				     pred:pcexp option, comment} = 
			 case lookupAcc pty of NONE => []
			 | SOME a => [(name,P.makeTypedefPCT a,NONE)]
		     fun genAccBrief e = []
		     val auxAccFields = [(tag, PL.intAccPCT, NONE)]
		     val accFields = auxAccFields @ (mungeVariants genAccFull genAccBrief genAccMan variants)
		     val accStructED = P.makeTyDefStructEDecl (accFields, accSuf name)
		     val accPCT = P.makeTypedefPCT (accSuf name)			  

                     (* Generate tag to string function *)
		     val toStringED = genEnumToStringFun(tgSuf name, tagPCT, tagFields)

                     (* Generate read function *)
                     (* -- Some useful names *)
		     val readName = readSuf name

                     (* -- some helper functions *)
		     val doDeallocOldSpaceSs = 
			 [PT.Expr(PT.Call(PT.Id (cleanupSuf unionName), 
					  [PT.Id ts, PT.Id (gMod rep)])),
			  PT.Expr(PT.Call(PT.Id (initSuf unionName), 
					  [PT.Id ts, PT.Id (gMod rep)])),
			  PT.Expr(PT.Call(PT.Id ((cleanupSuf o edSuf) unionName), 
					  [PT.Id ts, PT.Id (gMod ed)])),
			  PT.Expr(PT.Call(PT.Id ((initSuf o edSuf) unionName), 
					  [PT.Id ts, PT.Id (gMod ed)]))]
		     val deallocOldSpaceSs = (* optimization for reusing if space if
					      hits first tag again *)
			 case #memChar unionProps of TyProps.Static => []
		       | TyProps.Dynamic => 
			     if name = !firstTag then 
				 [PT.IfThen(
					    P.neqX(fieldX(rep,tag), PT.Id (!firstTag)),
					    PT.Compound doDeallocOldSpaceSs)]
			     else doDeallocOldSpaceSs

		     val returnSs = if isRecord then
			 [P.assignS(PT.Id result, PL.PDC_OK),
			  PT.Goto (findEORSuf unionName)]
				    else [PT.Return PL.PDC_OK]


                     fun doConstraint (predOpt,name,foundSs,notFoundSs) = 
 		          case predOpt of NONE => foundSs
                          | SOME constraint => 
			    let val predX = (PTSub.substExp (name, P.dotX(fieldX(rep,value),PT.Id name), constraint))
					    before expEqualTy(constraint, CTintTys, 
							      fn s=> (" constraint for variant "^
								      name ^ " has type: " ^ s ^
								      ". Expected an int."))
				 in
				     PT.Compound[
                                      PT.IfThenElse(
                                         P.andX(P.lteX(fieldX(em,name), PL.EM_CHECK),
						P.notX predX),
					   notFoundSs,
					   foundSs
					  )]
				 end

                     fun readVariant(pred,name,args,readFieldName,foundSs,notFoundSs) = 
			 let val constraintChkS = doConstraint(pred,name,foundSs,notFoundSs)
			 in
			     deallocOldSpaceSs 
			   @ [P.assignS(fieldX(rep,tag),PT.Id name),
			      PT.IfThenElse(
				 PL.readFunChkX(
				     PL.PDC_ERROR, readFieldName, PT.Id ts, 
				     P.addrX(fieldX(em,name)), args, 
				     P.addrX(fieldX(ed,name)),
				     P.addrX(P.dotX(fieldX(rep,value), PT.Id name))),
				 notFoundSs,
				 constraintChkS)]
			 end

                     fun genReadFull{pty :PX.Pty, args:pcexp list, name:string, 
				     isVirtual:bool, isEndian:bool, 
				     pred:pcexp option, comment} = 
			  let val readFieldName = lookupTy(pty, iSuf o readSuf, #readname)
                              val () = checkParamTys(name, readFieldName, args, 2, 2)
			      val commentS = P.mkCommentS ("Reading variant "^name^".")
			      val foundSs = PT.Compound(
					     PL.commitS(PT.Id ts)
					     @returnSs)
			      val notFoundSs = PT.Compound(PL.restoreS(PT.Id ts))
			      val readS =   PL.chkPtS(PT.Id ts) 
				          @ readVariant(pred,name,args,readFieldName,foundSs,notFoundSs)
			  in
			      [commentS] @  readS
			  end

                     fun genReadBrief _ = []
                     fun genReadUnionEOR _ = []

		     (* Given manifest representation, generate operations to set representation *)
		     fun genReadMan {decl, comment} = 
			 let val ctNoptEs = cnvDeclaration decl
			     fun doOne (cty, nameOpt, exp) = 
				 let val name = case nameOpt of NONE => "bogus"
			       | SOME n => n  
				     val repX = P.dotX(fieldX(rep, value), PT.Id name)
				     val commentS = P.mkCommentS ("Computing variant: "^ name ^ ".")
				     val initS = genAssignMan(cty,name,repX, exp)
				 in
				      deallocOldSpaceSs
				      @[commentS,
				        P.assignS(fieldX(rep, tag), PT.Id name),
				        initS]
				      @ returnSs
				 end
			 in
			     List.concat(List.map doOne ctNoptEs)
			 end


                     fun genErrorSs s = [P.mkCommentS s]
			             @ reportErrorSs([locS],true,
					PL.PDC_UNION_MATCH_FAILURE,
					true, s, [])
			             @ [P.assignS(fieldX(rep,tag),PT.Id (errSuf name))]

		     fun genCleanupSs s =  (genErrorSs s)
			             @ [P.assignS(fieldX(ed,panic), P.trueX),
					PT.Labeled(findEORSuf name, 
						   PT.Compound (genReadEOR reportUnionErrorSs ()))]
				     @ [PT.Return (PT.Id result)]
                     
		     fun chkCaseLabel eOpt = 
			 case eOpt of NONE => ()
		       | SOME e => expEqualTy(e, CTintTys, 
					      fn s=> (" case label for variant "^
						      name ^ " has type: " ^ s ^
						      ". Expected an int."))
		     fun genReadSwFull (eOpt,
			               {pty :PX.Pty, args:pcexp list, name:string, 
				        isVirtual:bool, isEndian:bool, 
				        pred:pcexp option, comment}) = 
			 let val readFieldName = lookupTy(pty, iSuf o readSuf, #readname)
			     val () = checkParamTys(name, readFieldName, args, 2, 2)
			     val () = chkCaseLabel eOpt
			     val commentS = P.mkCommentS("Reading variant " ^ name^".")
			     val foundSs = PT.Compound returnSs
			     val notFoundSs = PT.Compound (genErrorSs ("Failed to match branch "^ name^"."))
			     val readS = PT.Compound (readVariant(pred,name,args,readFieldName,foundSs,notFoundSs))
			 in
			     case eOpt of NONE => [PT.DefaultLabel(readS)]
			     | SOME e =>  [PT.CaseLabel(e,readS)]
			 end

		     fun genReadSwMan(eOpt, manRep) = 
			 let val () = chkCaseLabel eOpt
			     val readS = PT.Compound (genReadMan manRep)
			 in
			     case eOpt of NONE => [PT.DefaultLabel(readS)]
			     | SOME e =>  [PT.CaseLabel(e,readS)]
			 end

                     fun buildSwitchRead (descriminator) = 
			     let val () = expEqualTy(descriminator, CTintTys, 
						     fn s=> (" Descriminator for union "^
							     name ^ " has type: " ^ s ^
							     ". Expected an int."))
				 val readFields = mungeBVs genReadSwFull genReadBrief genReadSwMan cases variants
				 val bodyS = PT.Switch(descriminator, PT.Compound readFields)
			     in
				 [PT.Compound ([P.varDeclS(P.int, result, PL.PDC_ERROR), 
						bodyS,
						PT.Return (PT.Id result)])]
			     end

                     fun buildReadFun () = 
			 case descOpt of NONE => 
			     let val readFields = mungeVariants genReadFull genReadBrief genReadMan
 					           variants  (* does type checking *)
				 val cleanupSs = genCleanupSs ("Failed to match any branch of union"^name^".")
			     in
				 [PT.Compound ([P.varDeclS(P.int, result, PL.PDC_ERROR)] 
					       @ readFields @ cleanupSs)]
			     end
                         | SOME descriminator => buildSwitchRead(descriminator)

                     (* -- Assemble read function *)
		     val _ = pushLocalEnv()                                        (* create new scope *)
		     val () = ignore (insTempVar(gMod rep, P.ptrPCT canonicalPCT)) (* add modrep to scope *)
		     val cParams : (string * pcty) list = List.map mungeParam params
                     val () = ignore (List.map insTempVar cParams)  (* add params for type checking *)
		     val bodySs = buildReadFun() 
		     val _ = popLocalEnv()                                         (* remove scope *)
		     val readFunEDs = genReadFun(readName, cParams,emPCT,edPCT,canonicalPCT, 
						 emFirstPCT, true, bodySs)

                      (* Generate Accumulator functions (union case) *)
                      (* -- generate accumulator init, reset, and cleanup functions *)
		      fun genResetInitCleanup theSuf = 
			  let val theFun = (theSuf o accSuf) name
			      val theDeclSs = [P.varDeclS(P.int, nerr, P.zero)]
			      fun fieldX(base,name) = P.addrX(P.arrowX(PT.Id base, PT.Id name))
			      fun genAccTheFull {pty :PX.Pty, args:pcexp list, name:string, 
						 isVirtual:bool, isEndian:bool,
						 pred:pcexp option, comment} = 
				  case lookupAcc(pty) of NONE => []
				| SOME a => (let val theName = theSuf a
						 val fieldX = fieldX(acc,name)
					     in chk3Pfun(theName, fieldX)
					     end
                                             (* end accOpt SOME case *))
			      fun genAccTheBrief e = []
			      val tagFields = mungeVariants genAccTheFull genAccTheBrief 
				              (genAccTheMan theSuf) variants
			      val auxFields = chk3Pfun(theSuf PL.intAct, fieldX(acc,tag))
			      val theFields = auxFields @ tagFields
			      val theReturnS = genReturnChk (PT.Id nerr)
			      val theBodySs = theDeclSs @ theFields @ [theReturnS]
			      val theFunED = gen3PFun(theFun, accPCT, theBodySs)
			  in
			      theFunED
			  end
		      val initFunED = genResetInitCleanup initSuf
		      val resetFunED = genResetInitCleanup resetSuf
                      val cleanupFunED = genResetInitCleanup cleanupSuf

                      (* -- generate accumulator function *)
                      (*  PDC_error_t T_acc_add (PDC_t* , T_acc* , T_ed*, T* ) *)
		      val addFun = (addSuf o accSuf) name
		      val addDeclSs = [P.varDeclS(P.int, nerr, P.zero), P.varDeclS'(PL.base_edPCT, ted)]
		      val initTedSs = [P.assignS(P.dotX(PT.Id ted, PT.Id errCode), 
						 P.condX(P.eqX(P.arrowX(PT.Id ed, PT.Id errCode),
							       PL.PDC_UNION_MATCH_FAILURE),
							 PL.PDC_UNION_MATCH_FAILURE, PL.PDC_OK))]
		      val addTagSs = chkAddFun(addSuf PL.intAct, getFieldX(acc,tag), P.addrX(PT.Id ted), 
						  PT.Cast(P.ptrPCT PL.intPCT, getFieldX(rep,tag)))
		      fun fieldAddrX (base,name) = P.addrX(P.arrowX(PT.Id base, PT.Id name))

		      fun genCase (name,pty, initSs, edX) = 
			  case lookupAcc(pty) of NONE => []
			| SOME a => (let val funName = addSuf a
					 val repX = unionBranchX(name)
				     in 
					 [PT.CaseLabel(PT.Id name, 
						       PT.Compound (initSs 
								    @ chkAddFun(funName, fieldAddrX(acc, name), 
										edX, repX)
						                    @ [PT.Break]))]
				     end
		      (* end accOpt SOME case *))
		      fun genAccAddFull {pty :PX.Pty, args:pcexp list, name:string, 
					 isVirtual:bool, isEndian:bool, 
					 pred:pcexp option, comment} = 
			  genCase (name,pty, [], fieldAddrX(ed,name))

		      fun genAccAddBrief e = []
		      fun genAccAddMan {decl, comment:string option} = 
			  let val ctNoptEs = cnvDeclaration decl
			      fun doOne(cty,nameOpt,exp) = 
				  let val name= case nameOpt of NONE => "bogus" | SOME n => n
				      val accPtyOpt = reverseLookup cty
				  in
				      case accPtyOpt of NONE => [] 
				    | SOME pty => 
				       let val initSs = 
					  [P.varDeclS'(P.makeTypedefPCT(lookupTy(pty, edSuf, #edname)), ted),
					   P.assignS(P.dotX(PT.Id ted,PT.Id errCode),
						     P.arrowX(PT.Id ed, PT.Id errCode))] 
				       in
					  genCase(name,pty,initSs,P.addrX(PT.Id ted))
				       end
				  end
			  in
			      List.concat(List.map doOne  ctNoptEs)
			  end
		      val addBranches = mungeVariants genAccAddFull genAccAddBrief genAccAddMan variants
                      val addVariantsSs = [PT.Switch (P.arrowX(PT.Id rep, PT.Id tag), PT.Compound addBranches)]
		      val addReturnS = genReturnChk (PT.Id nerr)
                      val addBodySs = addDeclSs @ initTedSs @ addTagSs @ addVariantsSs @ [addReturnS]
                      val addFunED = genAddFun(addFun, accPCT, edPCT, canonicalPCT, addBodySs)

                      (* -- generate report function (internal and external)  punion *)
                      (*  PDC_error_t T_acc_report (PDC_t* , [Sfio_t * outstr], const char* prefix, 
		                                    const char * what, int nst, T_acc*  ) *)
		      val reportFun = (reportSuf o accSuf) name
                      val reportTags = [chkPrint(callEnumPrint((iSuf o mapSuf o reportSuf) PL.intAct,
						    PT.String "Union tag", PT.String "tag", P.intX ~1,
						    PT.Id ((toStringSuf o tgSuf) name), getFieldX(acc,tag))),
					PL.sfprintf(PT.Id outstr, 
						    PT.String "\n[Describing each tag arm of %s]\n", 
						    [PT.Id prefix])]
		      fun genAccReportFull {pty :PX.Pty, args:pcexp list, name:string, 
					    isVirtual:bool, isEndian: bool, 
					    pred:pcexp option, comment} = 
			  cnvPtyForReport(reportSuf, iSuf,pty,name)
                      fun genAccReportBrief e = []
		      val reportVariants = mungeVariants genAccReportFull genAccReportBrief 
			                      (genAccReportMan (reportSuf, iSuf))variants
                      val reportFunEDs = genReportFuns(reportFun, "union "^name, 
						       accPCT, reportTags @ reportVariants)

                      (* Generate init function, union case *)
		      val initFunName = lookupMemFun (PX.Name name)
                      val initRepEDs = case #memChar unionProps
			  of TyProps.Static => 
			      [genInitFun(initSuf initFunName, rep, canonicalPCT, [],true)]
			   | TyProps.Dynamic => 
			       let fun genInitFull {pty as PX.Name tyName :PX.Pty, args : pcexp list, 
						    name:string, isVirtual:bool, isEndian:bool,
						    pred:pcexp option, comment:string option} = 
				  [ [ P.assignS(P.arrowX(PT.Id rep, PT.Id tag), PT.Id name)]
				    @ (if TyProps.Static = lookupMemChar pty then []
				       else let val baseFunName = lookupMemFun (PX.Name tyName)
					    in [PT.Expr(
					           PT.Call(PT.Id (initSuf baseFunName),
							   [PT.Id ts, 
							    unionBranchX(name)]))]
					    end)]
				   fun genInitBrief _ = [[]]
				   fun genInitMan _ = []
				   val bodySs = case (mungeVariants genInitFull 
						      genInitBrief genInitMan variants)
				                of [] => [] | (x::xs) => x
			       in
				   [genInitFun(initSuf initFunName, rep, canonicalPCT, bodySs,false)]
		               end

                      fun genInitEDEDs suf = case #memChar unionProps
			  of TyProps.Static => 
				   [genInitFun(suf initFunName, ed, edPCT, [],true)]
			   | TyProps.Dynamic => 
			       let fun genInitFull {pty as PX.Name tyName :PX.Pty, args : pcexp list, 
						    name:string, isVirtual:bool, isEndian:bool,
						    pred:pcexp option, comment:string option} = 
				    if TyProps.Static = lookupMemChar pty then []
				    else let val baseFunName = lookupMemFun (PX.Name tyName)
					 in [PT.Expr(
					      PT.Call(PT.Id (suf baseFunName),
							   [PT.Id ts, 
							    P.addrX(P.arrowX(PT.Id ed, PT.Id name))]))]
					 end
				   fun genInitBrief _ = []
				   fun genInitMan _ = []
				   val bodySs = mungeVariants genInitFull genInitBrief genInitMan variants
			       in
				   [genInitFun(suf initFunName, ed, edPCT, bodySs, false)]
		               end
		      val initEDEDs = genInitEDEDs (initSuf o edSuf)

                      (* Generate cleanup function, union case *)
		      val cleanupFunName = lookupMemFun (PX.Name name)
                      val cleanupRepEDs = case #memChar unionProps
			  of TyProps.Static => 
			      [genInitFun(cleanupSuf cleanupFunName,rep,canonicalPCT,[],true)]
			   | TyProps.Dynamic => 
			       let fun genCleanupFull {pty as PX.Name tyName :PX.Pty, args : pcexp list, 
						    name:string, isVirtual:bool, isEndian:bool,
						    pred:pcexp option, comment:string option} = 
				    if TyProps.Static = lookupMemChar pty then []
				    else let val baseFunName = lookupMemFun (PX.Name tyName)
					 in [PT.CaseLabel(PT.Id name,
					      PT.Compound[
					       PT.Expr(
					           PT.Call(PT.Id (cleanupSuf baseFunName),
							   [PT.Id ts, 
							    unionBranchX(name)])), 
					       PT.Break])]
					 end
				   fun genCleanupBrief _ = []
				   fun genCleanupMan _ = []
				   val branchSs = mungeVariants genCleanupFull 
				                   genCleanupBrief genCleanupMan variants
				   val bodySs = [PT.Switch(P.arrowX(PT.Id rep, PT.Id tag), PT.Compound branchSs),
						 PL.bzeroS(PT.Id rep, P.sizeofX canonicalPCT)]
			       in
				   [genInitFun(cleanupSuf cleanupFunName, rep, canonicalPCT, bodySs, false)]
		               end
		      val cleanupEDEDs = genInitEDEDs(cleanupSuf o edSuf)
		 in
		       tagDecls
		     @ unionDecls
		     @ canonicalDecls
	             @ cnvExternalDecl emStructED
	             @ cnvExternalDecl edStructED
	             @ cnvExternalDecl accStructED
	             @ cnvExternalDecl toStringED
                     @ (List.concat(List.map cnvExternalDecl initRepEDs))     (* init used in read function *)
                     @ (List.concat(List.map cnvExternalDecl initEDEDs))      (* init ed used in read function *)
                     @ (List.concat(List.map cnvExternalDecl cleanupRepEDs))  (* cleanup used in read function *)
                     @ (List.concat(List.map cnvExternalDecl cleanupEDEDs))   (* cleanup ed used in read function *)
	             @ (List.concat (List.map cnvExternalDecl readFunEDs))
	             @ cnvExternalDecl initFunED
	             @ cnvExternalDecl resetFunED
	             @ cnvExternalDecl cleanupFunED
	             @ cnvExternalDecl addFunED
                     @ (List.concat(List.map cnvExternalDecl reportFunEDs))
		 end
	  
             fun cnvPArray {name:string, params : (pcty * pcdecr) list, isRecord, args : pcexp list, baseTy:PX.Pty, 
			    sizeSpec:pcexp PX.PSize option, constraints: pcexp PX.PConstraint list} =
	     let val length = "length"
                 val internal = "_internal"
		 val element = "element"
                 val array = "array"
                 val arrayDetail = "arrayDetail"
                 val neerr = "neerr"
                 val firstError = "firstError"
                 val elemRepPCT = P.makeTypedefPCT(lookupTy(baseTy, repSuf, #repname))
                 val elemEdPCT  = P.makeTypedefPCT(lookupTy(baseTy, edSuf, #edname))
                 val elemEmPCT  = P.makeTypedefPCT(lookupTy(baseTy, emSuf, #emname))
                 val elemReadName = lookupTy(baseTy, iSuf o readSuf, #readname)

                 (* Some useful functions *)
                 fun recordArrayErrorS (getLocSs, errCodeC, shouldPrint,msg,args, setPanic) = 
                     PT.Compound([
		       PT.IfThen(PL.getSpecLevelX(PT.Id ts),
				 PT.Return PL.PDC_ERROR),
  		       PT.IfThenElse(P.notX(fieldX(ed,nerr)),
			  PT.Compound (reportErrorSs(getLocSs,true,errCodeC,shouldPrint,msg,args)),
			  PT.Compound[P.postIncS(fieldX(ed,nerr))])]
                       @ (if setPanic then [P.assignS(fieldX(ed,panic),P.trueX)] else []))
  

                 fun amCheckingE(SOME testE) = 
                     P.andX(P.lteX(fieldX(em,array), PL.EM_CHECK), testE)
                   | amCheckingE(NONE) = P.lteX(fieldX(em,array), PL.EM_CHECK)


		 (* Generate canonical representation *)
		 val canonicalFields = [(length, PL.intPCT, NONE), 
				        (name, P.ptrPCT elemRepPCT, NONE),
					(internal, P.ptrPCT PL.rbufferPCT, NONE) ]
		 val canonicalStructED = P.makeTyDefStructEDecl (canonicalFields, repSuf name)
		 val canonicalDecls = cnvExternalDecl canonicalStructED 
		 val canonicalPCT = P.makeTypedefPCT (repSuf name)			 

	         (* Generate error mask *)
		 val emFields = [(element, P.makeTypedefPCT(lookupTy(baseTy, emSuf, #emname)),
				  SOME "per-element checks"),
				 (array,   PL.base_emPCT, SOME "entire array checks")]
		 val emStructED = P.makeTyDefStructEDecl (emFields, emSuf name)
		 val emStructDecls = cnvExternalDecl emStructED 
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
				 (name, P.ptrPCT(P.makeTypedefPCT(lookupTy(baseTy, edSuf, #edname))), NONE),
				 (internal, P.ptrPCT PL.rbufferPCT, NONE)] 
		 val edStructED = P.makeTyDefStructEDecl (edFields, edSuf name)
		 val edStructDecls = cnvExternalDecl edStructED 
		 val edPCT = P.makeTypedefPCT (edSuf name)			  


		 (* Generate read function *)
                 (* -- Some useful names *)
                 val readName     = readSuf name
                 val foundTerm    = "foundTerm"
		 val reachedLimit = "reachedLimit"

		 val resRBufferX  = fieldX(rep, internal)
		 val resBufferX   = fieldX(rep, name)
		 val indexX       = P.minusX(fieldX(rep,length), P.intX 1)
		 val resNext      = P.subX(resBufferX, indexX)

		 val edRBufferX   = fieldX(ed, internal)
		 val edBufferX    = fieldX(ed, name)
 		 val edNext       = P.subX(edBufferX, indexX)
          
                 (* add local variables, ie, parameters,  to scope *)
		 val _ = pushLocalEnv()                                        (* create new scope *)
		 val cParams : (string * pcty) list = List.map mungeParam params
		 val () = ignore (List.map insTempVar cParams)  (* add params for type checking *)
		 (* scope is removed at end of cnvPArray *)

                 (* -- Check size specification for array *)
                 val (minOpt, maxOpt, minConstOpt, maxConstOpt, chkBoundsSs) = 
                     let fun allocBuffs  countX = 
			 let val zeroCanonical = 
			     case lookupMemChar baseTy
			     of TyProps.Dynamic => true | _ => false
			 in
                               PL.chkNewRBufS(resRBufferX, zeroCanonical, PT.Id ts)
			     @ PL.chkNewRBufS(edRBufferX, true, PT.Id ts)
			 end
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
					     recordArrayErrorS([locS],
							       PL.PDC_ARRAY_MIN_NEGATIVE,true,
							       "Minimum value for the size of array "^
							       name ^  "(%d) " ^
							       "is negative.", [minX], false))]

			 fun genPosMaxCheckSs (maxConstOpt,maxX) = 
			     if isSome maxConstOpt then []
			     else [PT.IfThen( (* if (maxX<0) *)
					     amCheckingE(SOME(P.ltX(maxX,P.zero))),
					     recordArrayErrorS([locS],
							       PL.PDC_ARRAY_MAX_NEGATIVE,true,
							       "Maximum value for the size of array "^
							       name ^  "(%d) " ^
							       "is negative.", [maxX],true))]

		     in
                     (case sizeSpec 
                      of NONE => (NONE, NONE, NONE, NONE, allocBuffs P.zero)
                      |  SOME (PX.SizeInfo {min, max, maxTight}) => (
                           case (min,max) 
                           of (NONE,NONE) => (NONE, NONE, NONE, NONE, allocBuffs P.zero)
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
						      recordArrayErrorS([locS],
									PL.PDC_ARRAY_MIN_BIGGER_THAN_MAX_ERR,
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
				   (SOME minX, SOME maxX, minConstOpt, maxConstOpt,
				      dynBoundsCheckSs @ sizeAllocSs)
				end
                              (* end Some minX, Some maxX*))
                           | (SOME minX, NONE) => (
				let val minConstOpt = chkSize(minX, "Minumum")
				    val posMinCheckSs = genPosMinCheckSs(minConstOpt, minX)
				    val allocSizeX = P.intX (IntInf.toInt(valOf (#1(evalExpr minX))))
						     handle Option => P.zero
				in
				   (SOME minX, NONE, minConstOpt, NONE, posMinCheckSs @ allocBuffs(allocSizeX))
				end
                              (* end SOME minX, NONE *))
                           | (NONE, SOME maxX) => (
                                let val maxConstOpt = chkSize(maxX, "Maximum")
				    val posMaxCheckSs = genPosMaxCheckSs(maxConstOpt, maxX)
				    val allocSizeX = P.intX (IntInf.toInt(valOf (#1(evalExpr maxX))))
						     handle Option => P.zero
				    val (minXOpt, minConstOpt) = if maxTight then (SOME maxX, maxConstOpt)
						                 else (NONE, NONE)
				in
				   (minXOpt, SOME maxX, minConstOpt, maxConstOpt, 
				    posMaxCheckSs @ allocBuffs(allocSizeX))
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
		 val () = checkParamTys(name, elemReadName, args, 2, 2)

                 (* -- Declare top-level variables and initialize them *)
                 val initSs = [P.varDeclS(PL.base_emPCT, tem, PL.EM_CHECK), (* base_em tem = CHECK; *)
                               P.varDeclS'(PL.base_edPCT, ted)]             (* base_ed ted; *)
                              @ (if Option.isSome termXOpt then             (* int foundTerm = false *)
                                   [P.varDeclS(P.int, foundTerm, P.falseX)] 
                                 else [])
                              @ (if Option.isSome maxOpt then               (* int reachedLimit = false *)
				   [P.varDeclS(P.int, reachedLimit, P.falseX)]
			        else [])
                              @ [ P.assignS(fieldX(rep,length), P.zero)]       (* modres->length = 0; *)

                 (* -- fragments for while loop for reading input *)

                 (* -- code for checking if terminator is next in input *)
                 fun genTermCheck NONE = []
                   | genTermCheck (SOME (exp, readFun, scanFun)) = 
                      [P.mkCommentS("Looking for terminator"),
		       PT.IfThen(
                          PL.readFunChkX(PL.PDC_OK, readFun, PT.Id ts, P.addrX(PT.Id tem),  [], 
						             P.addrX(PT.Id ted), exp),
			  PT.Compound
                           [P.assignS(PT.Id foundTerm, P.trueX)])]

                 (* -- Code for checking termination conditions *)
                 fun genBreakCheckX (termOpt, sizeOpt) = 
		     let val isEofX = PL.isEofX(PT.Id ts)
			 val isEorX = PL.isEorX(PT.Id ts)
			 val termFoundX = PT.Id foundTerm
			 val limitReachedX = PT.Id reachedLimit
		     in
                        case (termOpt,sizeOpt,isRecord)
			of (NONE,NONE,_) => P.orX(isEofX,isEorX)
                        |  (NONE, SOME _,false)    => P.orX(isEofX, limitReachedX)
                        |  (NONE, SOME _,true)     => P.orX(isEofX, P.orX(isEorX,limitReachedX))
                        |  (SOME _, NONE,false)    => P.orX(isEofX, termFoundX)
                        |  (SOME _, NONE,true)     => P.orX(isEofX, P.orX(isEorX, termFoundX))
                        |  (SOME _, SOME _, false) => P.orX(isEofX, 
						      P.orX(termFoundX,limitReachedX))
                        |  (SOME _, SOME _,true)   => P.orX(isEofX, 
						      P.orX(isEorX,
						      P.orX(termFoundX,limitReachedX)))
			    
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
				          recordArrayErrorS([locES],
							    PL.PDC_ARRAY_EXTRA_BEFORE_TERM,true,"",[],false),
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
			 locBS,
		         PT.IfThenElse(
			    P.eqX(PL.PDC_OK,
				  PL.scanFunX(Atom.toString scanSep, PT.Id ts, 
					      sepX, scanStopX, P.trueX, P.addrX (PT.Id "c"),
					      P.addrX (PT.Id "n"))),
			    PT.Compound[
                              PT.IfThen(amCheckingE NONE, 
	  		       PT.Compound[ (* if am checking *)
			         PT.IfThenElse(P.andX(P.eqX(PT.Id "c", sepX),P.gtX(PT.Id "n", P.zero)),
				    recordArrayErrorS([locES],PL.PDC_ARRAY_EXTRA_BEFORE_SEP, true,
						   "", [],false),
                                    PT.Compound (chkTermSs))])],
                            PT.Compound[ (* else error in reading separator *)
			      P.mkCommentS("Error reading separator"),
			      recordArrayErrorS([locES],PL.PDC_ARRAY_SEP_ERR, 
						true, "Missing separator.",[],true),
			      PT.Break]
                            )]]

                      end
                 (* -- read next element *)
		 val (chkLenSs, bufSugX) = case maxOpt of NONE => ([], P.zero)
	             | SOME sizeX => 
		        ([P.assignS(PT.Id reachedLimit, P.gteX(fieldX(rep,length), Option.valOf maxOpt))],
			 sizeX)
                 val readElementSs = 
                       [P.postIncS(fieldX(rep,length))]
                     @ chkLenSs
		     @ (PL.chkReserveSs(PT.Id ts,  resRBufferX, 
				     P.addrX resBufferX, P.sizeofX elemRepPCT,
				     fieldX(rep,length),bufSugX))
		     @ (PL.chkReserveSs(PT.Id ts, edRBufferX, 
				     P.addrX edBufferX, P.sizeofX elemEdPCT,
				     fieldX(rep,length),bufSugX))
                     @ [PT.IfThen(
                          PL.readFunChkX(PL.PDC_ERROR, elemReadName, 
					               PT.Id ts, 
					               P.addrX(fieldX(em,element)),
						       args,
						       P.addrX(edNext),
						       P.addrX(resNext)),
                          PT.Compound[
			   PT.IfThen(PL.getSpecLevelX(PT.Id ts),
				     PT.Return PL.PDC_ERROR),
			   PT.IfThen(P.lteX(fieldX(em,array), PL.EM_CHECK),
			     PT.Compound[
                              PT.IfThen(P.notX(fieldX(ed,nerr)),
                                 PT.Compound (
	 			   (reportErrorSs([locS],true,PL.PDC_ARRAY_ELEM_ERR, false, "", []))
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
							       forX, stopX, P.falseX, P.zero, P.zero)),
                                    PT.Compound[
				     P.mkCommentS("We recovered; restored invariant.")],
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
		     let fun insLengthChk bdyS = 
			    case (maxOpt,maxConstOpt) 
                            of (SOME maxX, NONE) => (
				PT.IfThenElse(
                                 P.gteX(fieldX(rep,length), maxX),
				 PT.Compound[P.assignS(PT.Id reachedLimit, P.trueX)],
                                 PT.Compound[bdyS])
			      (* end case *))
			    | (_,_) => bdyS

			 fun insTermChk bdyS = 
			     case termXOpt of NONE => bdyS
			     | SOME (termX, termRead, _) => (
                                PT.IfThenElse(
                                 PL.readFunChkX(PL.PDC_OK, termRead, PT.Id ts, 
						P.addrX (PT.Id tem), [], P.addrX (PT.Id ted),
						termX),
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
			 val termCondX = if isRecord then 
			                   P.andX(P.notX(PL.isEofX(PT.Id ts)),
						  P.notX(PL.isEorX(PT.Id ts)))
					 else
			                   P.notX(PL.isEofX(PT.Id ts))
		     in 
			 [P.mkCommentS("Reading input until we reach a termination condition"),
                                PT.IfThen(P.andX(P.notX(fieldX(ed,panic)), 
						 termCondX),
					  PT.Compound[insTermChk (insLengthChk bdyS)])]
		     end

                 (* -- Check if there was junk before trailing terminator *)
	         val trailingJunkChkSs = 
		     case termXOpt of NONE => []
                     | SOME (termX, _, NONE) => (PE.error "Expected a scan function"; [])
		     | SOME (termX, _, SOME termScan) => 
			 [P.mkCommentS("End of loop. Read trailing terminator if there was trailing junk."),
			  PT.IfThen(P.andX(P.notX(fieldX(ed,panic)),P.notX(PT.Id foundTerm)),
			   PT.Compound[
			   locBS,
		           PT.IfThenElse(
			     P.eqX(PL.PDC_OK,
				  PL.scanFunX(Atom.toString termScan, PT.Id ts, 
					      termX, termX, P.trueX, P.zero,
					      P.zero)),
                             PT.Compound[
			      PT.IfThen(amCheckingE NONE, 
			        PT.Compound[
				 recordArrayErrorS([locES],PL.PDC_ARRAY_EXTRA_BEFORE_TERM,true,"",[],false),
				 P.assignS(PT.Id foundTerm, P.trueX)])],
			     recordArrayErrorS([locES],PL.PDC_ARRAY_TERM_ERR, true, "Missing terminator.",[],true))
			 ])]

		 val readEORSs = if isRecord then genReadEOR reportStructErrorSs () else []
                 (* -- Set data fields in canonical rep and ed from growable buffers *)
                 val setDataFieldsSs = 
                     [
		      P.assignS(fieldX(ed,length), fieldX(rep,length))
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
			  recordArrayErrorS([locS],PL.PDC_ARRAY_SIZE_ERR, true,
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
			recordArrayErrorS([locS],PL.PDC_ARRAY_USER_CONSTRAINT_ERR, true,
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
				@ readEORSs
				@ setDataFieldsSs
				@ arrayConstraintsSs
                                @ [returnS])]
                 val readFunEDs = genReadFun(readName, cParams, emPCT,edPCT,canonicalPCT, 
					     NONE, true, bodySs)
                 val _ = popLocalEnv()

	         (* Calculate and insert type properties into type table *)
                 val baseMemChar = lookupMemChar baseTy
		 val arrayMemChar = TyProps.Dynamic (* at the moment, all arrays are dynamically allocated. *)
                 val baseDiskSize = lookupDiskSize baseTy
                 val arrayDiskSize = case (maxConstOpt, minConstOpt, baseDiskSize)
		                     of (SOME min, SOME max, TyProps.Size (n,r)) => 
					 if min = max then TyProps.Size(n * (IntInf.toInt max), r * (IntInf.toInt max))
					 else TyProps.Variable
				     | _ => TyProps.Variable
                 val arrayProps = buildTyProps(name,arrayDiskSize, arrayMemChar, false, isRecord)
                 val () = PTys.insert(Atom.atom name, arrayProps)


		 (* Generate accumulator type (array case) *)
                 val numElemsToTrack = case maxConstOpt of NONE => 10
		                       | SOME x => Int.min(10,IntInf.toInt x)
		 val baseFields = 
		     case lookupAcc baseTy of NONE => [] 
		     | SOME acc => 
			 [(array, P.makeTypedefPCT acc, SOME "Accumulator for all array elements"),
			  (arrayDetail, P.arrayPCT (P.intX numElemsToTrack, P.makeTypedefPCT acc), 
			   SOME ("Accumulator for first "^(Int.toString numElemsToTrack)^" array elements"))]
		 val accFields = (length, PL.intAccPCT, SOME "Accumulator for array length")::baseFields
		 val accStructED = P.makeTyDefStructEDecl (accFields, accSuf name)
		 val accDecls = cnvExternalDecl accStructED 
		 val accPCT = P.makeTypedefPCT (accSuf name)			

                 (* Generate accumulator functions *) 
  	         (* -- generate accumulator reset, init, and cleanup function *)
                 fun genResetInitCleanup theSuf = 
		     let val theFun = (theSuf o accSuf) name
                         val doElems = 
			     case lookupAcc baseTy of NONE => []
			   | SOME a => (
			       let val elemFunName = theSuf a
				   fun doOne eX = chk3Pfun (elemFunName, eX)
				   val fieldX = P.addrX(P.subX(P.arrowX(PT.Id acc, PT.Id arrayDetail), PT.Id "i"))
				   val doArrayDetailSs = [
					PT.Compound
					 [P.varDeclS'(P.int, "i"),
					  PT.For(P.assignX(PT.Id "i",P.zero),
						 P.ltX(PT.Id "i", P.intX numElemsToTrack),
						 P.postIncX (PT.Id "i"),
						 PT.Compound (doOne fieldX)
						 )]]
				   val arrayX = P.addrX(P.arrowX(PT.Id acc, PT.Id array))
				   val doArraySs = doOne arrayX
			       in
				   doArraySs @ doArrayDetailSs
			       end(* end SOME acc case *))
			 val lengthX = P.addrX(P.arrowX(PT.Id acc, PT.Id length))
			 val doLength = chk3Pfun(theSuf PL.intAct, lengthX)
			 val theDeclSs = [P.varDeclS(P.int, nerr, P.zero)]
			 val theReturnS = genReturnChk (PT.Id nerr)
			 val theBodySs = theDeclSs @ doLength @ doElems @ [theReturnS]
			 val theFunED = gen3PFun(theFun, accPCT, theBodySs)
		     in
			 theFunED
		     end

  	         (* -- generate accumulator add function *)
                 fun genAdd () = 
		     let val theSuf = addSuf
			 val theFun = (theSuf o accSuf) name
			 val theDeclSs = [P.varDeclS(P.int, nerr, P.zero), P.varDeclS'(PL.base_edPCT, ted)]
			 val initTedSs = [P.assignS(P.dotX(PT.Id ted, PT.Id errCode), 
						    P.arrowX(PT.Id ed, PT.Id errCode))]
                         val doElems = 
			     case lookupAcc baseTy of NONE => []
			   | SOME a => (
			       let val elemFunName = theSuf a
				   fun getArrayFieldX (base,field) = 
					P.addrX(P.subX(P.arrowX(PT.Id base, PT.Id field), PT.Id "i"))
				   fun doOne (accX,edX,repX) = chkAddFun (elemFunName, accX,edX,repX)
				   val doArrayDetailSs = [
					PT.Compound
					 [P.varDeclS'(P.int, "i"),
					  PT.For(P.assignX(PT.Id "i",P.zero),
						 P.ltX(PT.Id "i", P.arrowX(PT.Id rep, PT.Id length)),
						 P.postIncX (PT.Id "i"),
						 PT.Compound ([PT.IfThen(P.ltX(PT.Id "i", P.intX numElemsToTrack),
							       PT.Compound (doOne (getArrayFieldX(acc,arrayDetail), 
										   getArrayFieldX(ed,name), 
										   getArrayFieldX(rep,name))))]
							      @ (doOne (getFieldX(acc,array), 
								        getArrayFieldX(ed,name), 
									getArrayFieldX(rep,name))))
						 )]]
			       in
				   doArrayDetailSs
			       end(* end SOME acc case *))
			 val doLength = chkAddFun(theSuf PL.intAct, getFieldX(acc,length), P.addrX(PT.Id ted), 
						  getFieldX(rep,length))
			 val theReturnS = genReturnChk (PT.Id nerr)
			 val theBodySs = theDeclSs @ initTedSs @ doLength @ doElems @ [theReturnS]
			 val theFunED = genAddFun(theFun, accPCT, edPCT, canonicalPCT, theBodySs)
		     in
			 theFunED
		     end

		 val initFunED = genResetInitCleanup  initSuf
		 val resetFunED = genResetInitCleanup resetSuf
		 val cleanupFunED = genResetInitCleanup cleanupSuf
                 val addFunED = genAdd()

		 (* -- generate report function array *)
		 (*  PDC_error_t T_acc_report (PDC_t* , T_acc* , const char* prefix  ) *)
                 fun genReport () = 
		     let val reportFun = (reportSuf o accSuf) name
			 val lengthX = P.addrX(P.arrowX(PT.Id acc, PT.Id length))
			 val doLengthSs = [chkPrint(
					     callIntPrint((iSuf o reportSuf) PL.intAct, PT.String "Array lengths", 
						 	 PT.String "lengths", P.intX ~1, lengthX)) ]
                         val doElems = 
			     case lookupAcc baseTy of NONE => []
			   | SOME a => (
			       let val elemFunName = reportSuf a
				   fun doOne (descriptor, prefixX, eX,extraArgXs) = 
					genPrintPiece (iSuf elemFunName, descriptor, prefixX, eX,extraArgXs)
				   val fieldX = P.addrX(P.subX(P.arrowX(PT.Id acc, PT.Id arrayDetail), PT.Id "i"))
				   val doArrayDetailSs = [
					PT.Compound
					 [P.varDeclS'(P.int, "i"),
					  PT.For(P.assignX(PT.Id "i",P.zero),
						 P.ltX(PT.Id "i", P.intX numElemsToTrack),
						 P.postIncX (PT.Id "i"),
						 PT.Compound (doOne (arrayDetail^"[%d]", PT.String "array element", 
								     fieldX, [PT.Id "i"]))
						 )]]
				   val arrayX = P.addrX(P.arrowX(PT.Id acc, PT.Id array))
				   val doArraySs = doOne ("allArrayElts", PT.String "all array elements", arrayX, [])
			       in
				   doArraySs @ doArrayDetailSs
			       end(* end SOME acc case *))
			 val theBodySs = doLengthSs @ doElems 
			 val baseTyStr = case baseTy of PX.Name n => n
			 val theFunEDs = genReportFuns(reportFun, "array "^ name ^" of "^baseTyStr, accPCT, theBodySs)
		     in
			 theFunEDs
		     end
                 val reportFunEDs = genReport()

		 (* Generate init function, array case *)
		 fun genInitEDs(suf, base, aPCT) = 
		   case #memChar arrayProps
		   of TyProps.Static => [genInitFun(suf name, base, aPCT, [],true)]
		   |  TyProps.Dynamic => 
			 let val bodySs = 
			     [P.assignS(P.arrowX(PT.Id base, PT.Id length), P.zero),
			      P.assignS(P.arrowX(PT.Id base, PT.Id name), P.zero),
			      P.assignS(P.arrowX(PT.Id base, PT.Id internal), P.zero)]
			 in
			     [genInitFun(suf name, base, aPCT, bodySs,false)]
			 end
		 val initRepEDs = genInitEDs(initSuf, rep, canonicalPCT)
		 val initEDEDs = genInitEDs(initSuf o edSuf, ed, edPCT)


		 (* Generate cleanup function, array case *)
		 fun genCleanupEDs(suf, base, aPCT) = 
		   case #memChar arrayProps
		   of TyProps.Static => 
			      [genInitFun(suf name, base,aPCT,[],true)]
		   |  TyProps.Dynamic => 
			 let val bodySs = 
			     [P.assignS(P.arrowX(PT.Id base, PT.Id length), P.zero),
			      P.assignS(P.arrowX(PT.Id base, PT.Id name), P.zero),
			      PT.IfThen(
				P.arrowX(PT.Id base, PT.Id internal),
			        PT.Compound[
			          PL.chkCFreeRBufferS(PT.Id ts,
						      P.arrowX(PT.Id base, PT.Id internal))])]
			 in
			     [genInitFun(suf name, base, aPCT, bodySs,false)]
			 end
		 val cleanupRepEDs = genCleanupEDs(cleanupSuf, rep, canonicalPCT)
		 val cleanupEDEDs = genCleanupEDs(cleanupSuf o edSuf, ed, edPCT)
	     in
		   canonicalDecls
		 @ emStructDecls
                 @ edStructDecls
                 @ accDecls
                 @ (List.concat(List.map cnvExternalDecl initRepEDs))
                 @ (List.concat(List.map cnvExternalDecl initEDEDs))
                 @ (List.concat(List.map cnvExternalDecl cleanupRepEDs))
                 @ (List.concat(List.map cnvExternalDecl cleanupEDEDs))
                 @ (List.concat(List.map cnvExternalDecl readFunEDs))
                 @ cnvExternalDecl initFunED
                 @ cnvExternalDecl resetFunED 
                 @ cnvExternalDecl cleanupFunED 
                 @ cnvExternalDecl addFunED
                 @ (List.concat(List.map cnvExternalDecl reportFunEDs))
	     end

	  fun cnvPEnum  {name:string, params : (pcty * pcdecr) list, isRecord,
			 members : (string * pcexp option * string option) list } =
	      let val baseTy = PX.Name PL.strlit
                  fun mungeMembers (name, expOpt, commentOpt) = 
		      case expOpt of NONE => (name, PT.EmptyExpr, commentOpt)
 		                   | SOME e => (name, e, commentOpt)
		  val enumFields = List.map mungeMembers members

                  (* Calculate and insert type properties into type table for enums. *)
                  val labels = List.map #1 enumFields
		  val ds = if List.length labels > 1 then
		              let val len = String.size (hd labels)
			      in
				  if List.all (fn s => len = String.size s) labels
				      then TyProps.Size (len, 0)
				  else TyProps.Variable
			      end
			   else TyProps.Size (0,0)
                  val enumProps = buildTyProps(name,ds,TyProps.Static, true, isRecord)
		  val () = PTys.insert(Atom.atom name, enumProps)

                  (* generate canonical representation *)
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

		  (* Generate accumulator type *)
		  val accED     = P.makeTyDefEDecl (PL.intAccPCT, accSuf name)
		  val accDecls  = cnvExternalDecl accED
		  val accPCT    = P.makeTypedefPCT (accSuf name)		

                  (* Generate read function *)
                  (* -- Some useful names *)
                  val readName = readSuf name
		  val baseReadFun = lookupTy(baseTy, iSuf o readSuf, #readname)
		  fun readOneBranch (bname, bvalOpt, commentOpt) =
		      let val labelLenX = P.intX(String.size bname)
		      in
                         [P.assignS(P.dotX(PT.Id "strlit", PT.Id (PL.str)), PT.String bname),
			  P.assignS(P.dotX(PT.Id "strlit", PT.Id (PL.len)), labelLenX)]
                       @ PL.chkPtS(PT.Id ts)
                       @ [PT.IfThenElse(
			    PL.readFunChkX(PL.PDC_ERROR, baseReadFun, PT.Id ts, 
						         PT.Id (gMod em), [], PT.Id(gMod ed),
						         P.addrX(PT.Id "strlit")),
			    PT.Compound (PL.restoreS(PT.Id ts)),
			    PT.Compound (  PL.commitS(PT.Id ts)
				         @ [P.assignS(P.starX (PT.Id (gMod rep)), PT.Id bname),
					    P.assignS(PT.Id result, PL.PDC_OK),
					    PT.Goto (findEORSuf name)]))]
		      end
                  fun genReadBranches () = 
                      [P.varDeclS'(PL.stringPCT, "strlit"),
		       P.varDeclS'(P.int, result)]
		      @ List.concat(List.map readOneBranch members)
		  val cleanupSs =  [P.mkCommentS("We didn't match any branch")]
			         @ reportErrorSs([locS],false,
					PL.PDC_ENUM_MATCH_FAILURE,
					true, 
					("Did not match any branch of enum "^name^"."),
					[])
			         @ [P.assignS(fieldX(ed,panic), P.trueX),
				    P.assignS(PT.Id result, PL.PDC_ERROR)]
		  val slurpToEORSs = if isRecord then genReadEOR reportBaseErrorSs () else []
                  val gotoSs = [PT.Labeled(findEORSuf name,
					PT.Compound (slurpToEORSs @ [PT.Return (PT.Id result)]))]


		  (* -- Assemble read function *)
		  val _ = pushLocalEnv()                                        (* create new scope *)
		  val () = ignore (insTempVar(gMod rep, P.ptrPCT canonicalPCT)) (* add modrep to scope *)
		  val cParams : (string * pcty) list = List.map mungeParam params
		  val () = ignore (List.map insTempVar cParams)  (* add params for type checking *)
		  val readFields = genReadBranches()                            (* does type checking *)
		  val _ = popLocalEnv()                                         (* remove scope *)
		  val bodySs = [PT.Compound(readFields @ cleanupSs @ gotoSs)]
		  val readFunEDs = genReadFun(readName, cParams, 
					      emPCT,edPCT,canonicalPCT, NONE, false, bodySs)

                  (* Generate Accumulator functions (enum case) *)
                  (* -- generate accumulator init, reset, and cleanup functions *)
		  fun genResetInitCleanup theSuf = 
		      let val theFun : string = (theSuf o accSuf) name
			  val theBodyE = PT.Call(PT.Id (theSuf PL.intAct),[PT.Id ts, PT.Id acc])
                          val theReturnS = PT.Return theBodyE
			  val theFunED = gen3PFun(theFun, accPCT, [theReturnS])
			  in
			      theFunED
			  end
		   val initFunED = genResetInitCleanup initSuf
		   val resetFunED = genResetInitCleanup resetSuf
                   val cleanupFunED = genResetInitCleanup cleanupSuf

                   (* -- generate accumulator function *)
                   (*  PDC_error_t T_acc_add (PDC_t* , T_acc* , T_ed*, T* ) *)
		   val addFun = (addSuf o accSuf) name
		   val addX = PT.Call(PT.Id (addSuf PL.intAct), 
				      [PT.Id ts, PT.Id acc, PT.Id ed, 
				       PT.Cast(P.ptrPCT PL.intPCT, PT.Id rep)])
		   val addReturnS = PT.Return addX
		   val addBodySs =  [addReturnS]
		   val addFunED = genAddFun(addFun, accPCT, edPCT, canonicalPCT, addBodySs)

		   (* -- generate report function enum *)
		   (*  PDC_error_t T_acc_report (PDC_t* , T_acc* , const char* prefix ) *)
		   val reportFun = (reportSuf o accSuf) name
		   val reportFields = genEnumPrint((iSuf o mapSuf o reportSuf) PL.intAct, "branchDistribution", 
						   PT.Id prefix, PT.Id what, PT.Id nst, 
						   PT.Id (toStringSuf name), PT.Id acc)
		   val reportFunEDs = genReportFuns(reportFun, "enum "^name, accPCT, reportFields)

		   (* Generate Init function (enum case) *)
		   val initFunName = lookupMemFun (PX.Name name)
		   fun genInitEDs (suf, argName, aPCT) =  (* always static *)
		       [genInitFun(suf initFunName, argName, aPCT, [],true)]
		   val initRepEDs = genInitEDs (initSuf, rep, canonicalPCT)
		   val initEDEDs  = genInitEDs ((initSuf o edSuf), ed, edPCT)
		   val cleanupRepEDs = genInitEDs (cleanupSuf, rep, canonicalPCT)
		   val cleanupEDEDs  = genInitEDs ((cleanupSuf o edSuf), ed, edPCT)

		  (* Generate enum to string function *)
		  val cnvFunED = genEnumToStringFun(name, canonicalPCT, members)
	      in
		  canonicalDecls
                @ emDecls
                @ edDecls
                @ accDecls
                @ cnvExternalDecl cnvFunED
		@ (List.concat(List.map cnvExternalDecl initRepEDs))
		@ (List.concat(List.map cnvExternalDecl initEDEDs))
		@ (List.concat(List.map cnvExternalDecl cleanupRepEDs))
		@ (List.concat(List.map cnvExternalDecl cleanupEDEDs))
                @ (List.concat(List.map cnvExternalDecl readFunEDs))
                @ cnvExternalDecl initFunED
                @ cnvExternalDecl resetFunED
                @ cnvExternalDecl cleanupFunED
                @ cnvExternalDecl addFunED
                @ (List.concat (List.map cnvExternalDecl reportFunEDs))

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

