structure CnvExt : CNVEXT = struct

  structure PT   = ParseTree     (* the parse tree *)
  structure PX   = ParseTreeExt  (* Pads extensions *)
  structure P    = ParseTreeUtil (* Utility functions for manipulating the parse tree *)
  structure PE   = PError        (* Error reporting utilities *)
  structure PBTys = PBaseTys     (* Information about the pads base types *)
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
	 {bindAid, lookAid=lookAid0, bindTid, lookTid, bindPaid, lookPaid},
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

    fun CTisIntorChar cty = (CTisInt cty) orelse (CTisChar cty)

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
              val pstate    = "pstate"
              val errCode   = "errCode"
              val loc       = "loc"
              val nerr      = "nerr"
	      val pdc       = "pdc"
	      val m         = "m" 
	      val pd        = "pd"
	      val acc       = "acc"
	      val rep       = "rep"
	      val io        = "io"
	      val buf       = "buf"
	      val bufLen    = "buf_len"
	      val bufFull   = "buf_full"
	      val bufCursor = "buf_cursor"
	      val length    = "length"
	      val tpd       = "tpd"
	      val tm        = "tm"
	      val tloc      = "tloc"
	      val tlen      = "tlen"
	      val all       = "structLevel"
	      val prefix    = "prefix"
	      val what      = "what"
	      val nst       = "nst"
	      val tmpstr    = "tmpstr"
	      val outstr    = "outstr"
	      val result    = "result"
	      val errorf    = "errorf"
	      val self      = "self"

	      (* Some useful functions *)
	      fun repSuf  s = s (* Make rep type same as pads name; s^"_rep" *)
              fun mSuf   s = s^"_"^m
              fun pdSuf   s = s^"_"^pd
              fun accSuf  s = s^"_"^acc
              fun initSuf s = s^"_init"
              fun resetSuf s = s^"_reset"
              fun cleanupSuf s = s^"_cleanup"
              fun copySuf s = s^"_copy"
              fun srcSuf s = s^"_src"
              fun dstSuf s = s^"_dst"
              fun addSuf  s = s^"_add"
              fun readSuf s = s^"_read"
              fun scanSuf s = s^"_scan"
              fun maskInitSuf s = s^"_m_init"
              fun writeSuf s = s^"_write"
	      fun ioSuf s = s^"2io"
	      fun bufSuf s = s^"2buf"
              fun reportSuf s = s^"_report"
	      fun mapSuf s = s^"_map"
	      fun toStringSuf s = s^"2str"
	      fun errSuf s = s^"_err"
	      fun findEORSuf s = s^"_findEOR"
	      fun findEndSuf s = s^"_end"
	      fun gTemp base = "tmp"^base
              fun childrenSuf name = name^"_children" 
              fun vTableSuf name = name^"_vtable"
		
	      fun fieldX (bsName, fName) = P.arrowX(PT.Id bsName, PT.Id fName)
	      fun getFieldX(base,field) = P.addrX(P.arrowX(PT.Id base, PT.Id field))

	      val discX = P.arrowX(PT.Id pdc, PT.Id PL.disc)
	      val ioDiscX =  P.arrowX(P.arrowX(PT.Id pdc, PT.Id PL.disc), PT.Id PL.io_disc)
	      val errorFX =  P.arrowX(P.arrowX(PT.Id pdc, PT.Id PL.disc), PT.Id PL.errorf)
	      val d_endianX =  P.arrowX(P.arrowX(PT.Id pdc, PT.Id PL.disc), PT.Id PL.d_endian)
	      val m_endianX =  P.arrowX(PT.Id pdc, PT.Id PL.m_endian)
	      val locX      =  P.addrX(fieldX(pd,loc))
              val locS      =  PL.getLocS(PT.Id pdc,P.addrX(fieldX(pd,loc)))
	      val locBS     =  PL.getLocBeginS(PT.Id pdc, P.addrX(fieldX(pd,loc)))
	      val locES     =  PL.getLocEndS(PT.Id pdc, P.addrX(fieldX(pd,loc)), ~2) 
	      val locES1    =  PL.getLocEndS(PT.Id pdc, P.addrX(fieldX(pd,loc)), ~1) 

	      fun getDynamicFunctions (name,memChar) = 
		  case memChar of TyProps.Static => (NONE,NONE,NONE,NONE)
		| TyProps.Dynamic => (SOME (initSuf name),
				      SOME (cleanupSuf name),
				      SOME ((initSuf o pdSuf) name),
				      SOME ((cleanupSuf o pdSuf) name))

              fun buildTyProps (name,kind,diskSize,compoundDiskSize,memChar,endian,isRecord,containsRecord,largeHeuristic,isFile,pdTid, numArgs) = 
     		  let val (repInit, repClean, pdInit, pdClean) = getDynamicFunctions (name,memChar)
		  in
		      {kind     = kind,
		       diskSize = diskSize,
		       compoundDiskSize = compoundDiskSize,
	 	       memChar  = memChar,
		       endian   = endian, 
		       isRecord = isRecord,
		       containsRecord = containsRecord,
                       largeHeuristic = largeHeuristic, 
		       isFile   = isFile,
		       numArgs  = numArgs,
		       repName  = name, 
		       repInit  = repInit,
		       repRead  = readSuf name, 
		       repClean = repClean,
		       pdName   = pdSuf name,
		       pdTid    = pdTid,
		       pdInit   = pdInit,
		       pdClean  = pdClean,
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

              fun lookupWrite(ty:pty) = 
		  case ty
                  of PX.Name s => ( case PBTys.find(PBTys.baseInfo, Atom.atom s)
				    of NONE => s  (* non-base type; acc constructed from type name*)
                                    |  SOME(b:PBTys.baseInfoTy) => (PL.prefix^(String.extract(s,1,NONE))))

	      fun lookupLitWrite s = (bufSuf o writeSuf) s

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
						| SOME (b:PTys.pTyInfo) => #diskSize b
						    (* end nested case *))
                                    |  SOME(b:PBTys.baseInfoTy) =>  #diskSize b)

              fun lookupEndian (ty:pty) = 
                  case ty 
                  of PX.Name s => ( case PBTys.find(PBTys.baseInfo, Atom.atom s)
				    of NONE => (case PTys.find(Atom.atom s)
						of NONE => false
						| SOME (b:PTys.pTyInfo) => (#endian b)
						    (* end nested case *))
                                    |  SOME(b:PBTys.baseInfoTy) => (#endian b))

             fun lookupContainsRecord (ty:pty) = 
                  case ty 
                  of PX.Name s => ( case PBTys.find(PBTys.baseInfo, Atom.atom s)
				    of NONE => (case PTys.find(Atom.atom s)
						of NONE => false
						| SOME (b:PTys.pTyInfo) => (#isRecord b orelse #containsRecord b)
						    (* end nested case *))
                                    |  SOME(b:PBTys.baseInfoTy) => false)


	      fun lookupHeuristic (ty:pty) =
		  case ty
		  of PX.Name s => ( case PBTys.find(PBTys.baseInfo, Atom.atom s)
                                    of NONE => (case PTys.find(Atom.atom s)
                                                of NONE => false
                                                | SOME (b:PTys.pTyInfo) => (#largeHeuristic b)
                                                    (* end nested case *)) 
                                    |  SOME(b:PBTys.baseInfoTy) => false)(* ???? *)

	      fun lookupCompoundDiskSize (ty:pty) =
		  case ty
		  of PX.Name s => ( case PBTys.find(PBTys.baseInfo, Atom.atom s)
                                    of SOME(b:PBTys.baseInfoTy) => TyProps.Base(#diskSize b)
		                    |  NONE => (case PTys.find(Atom.atom s)
                                                of NONE => raise Fail ("Type "^s^" not defined.")
                                                |  SOME (b:PTys.pTyInfo) => (#compoundDiskSize b)
                                                    (* end nested case *))) 
 
              fun tyName (ty:pty) = case ty of PX.Name s => s

	      fun reduceArgList(args, (params, bodies):TyProps.argList)=
		  let val subList = ListPair.zip(params,args)
		      val results = List.map (PTSub.substExps subList) bodies
		  in
		      results
		  end

              fun reduceSizeSpec (cFormals, args, sizeSpec) = 
		  let fun g (formals, exp, recExp) = 
			  let val subList = ListPair.zip(formals, args)
			      val rExp = PTSub.substExps subList exp
			      val rrecExp = PTSub.substExps subList recExp
			  in
      			      if  (PTSub.expIsClosed(cFormals, rExp)) andalso (PTSub.expIsClosed(cFormals, rrecExp)) then
				  let val () = (Error.warningsEnabled errorState false;
						 Error.errorsEnabled errorState false)
				      val cval = #1(evalExpr rExp)
				      val crecval = #1(evalExpr rrecExp)
				      val () = (Error.warningsEnabled errorState true;
						Error.errorsEnabled errorState true)
				  in
				      case (cval, crecval)
			              of (NONE,NONE) => TyProps.Param(cFormals, NONE, rExp,rrecExp)
			              | (NONE, SOME e) => TyProps.Param(cFormals, NONE, rExp, PT.IntConst e)
				      | (SOME e, NONE) => TyProps.Param(cFormals, NONE, PT.IntConst e, rrecExp)
				      | (SOME e1, SOME e2) => 
					   TyProps.Size(e1, e2)
				  end
			      else TyProps.Variable  (* must have a dependency on an earlier portion of data *)
			  end
		  in
		      case sizeSpec 
		      of TyProps.Param(formals,_,exp,recExp) => g (formals,exp, recExp)
                      |  x => x
		  end

	      fun reduceCDSize(args, sizeSpec) = reduceSizeSpec([],args,sizeSpec)

	      fun computeDiskSize(cName, cFormals, pty, args) =
		  let val sizeSpec = lookupDiskSize pty
		      val () = case sizeSpec of TyProps.Param(formals, _,_,_) =>
			          if not (List.length formals = List.length args) then
				     PE.error ("Number of arguments does not match "^
					       "specified number of args in type: "^cName^".\n")
				  else ()
			       | _ => ()
		  in
		      reduceSizeSpec(cFormals, args, sizeSpec)
		  end

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

              fun genReturnChk e =  P.returnS (P.condX(P.eqX(e,P.zero), PL.PDC_OK, PL.PDC_ERROR))

	      fun reportStructErrorSs (code, shouldGetLoc, locX) = 
		  let val setLocSs = if shouldGetLoc 
				     then [PL.getLocEndS(PT.Id pdc, P.addrX(locX), ~1)]
				     else []
		  in
		  [PT.IfThen(
		     P.eqX(P.zero, fieldX(pd,nerr)), 
		     PT.Compound(
		      [P.assignS(fieldX(pd, errCode), code)]
		      @ setLocSs
		      @ [P.assignS(fieldX(pd, loc), locX)])),
		   P.plusAssignS(fieldX(pd,nerr), P.intX 1)]
		  end

	      fun reportBaseErrorSs (code, shouldGetLoc, locX) = 
		  [P.assignS(fieldX(pd, errCode), code),
		   P.assignS(fieldX(pd, loc), locX)]

	      fun reportUnionErrorSs (code, shouldGetLoc, locX) = 
                 [PT.IfThen(
		   P.eqX(PT.Id result, PL.PDC_OK), (* only report scanning error if correctly read field*)
		   PT.Compound
		    [PT.IfThen(
		      P.eqX(P.zero, fieldX(pd,nerr)), 
		      PT.Compound 
		       [P.assignS(fieldX(pd, errCode), code),
		        P.assignS(fieldX(pd, loc), locX)]),
		     P.plusAssignS(fieldX(pd,nerr), P.intX 1)])]



              fun genReadEOR (readName, reportErrorSs) () = 
		  [P.mkCommentS ("Reading to EOR"),
		    PT.Compound[
			   P.varDeclS'(PL.base_pdPCT, tpd),
			   P.varDeclS'(PL.sizePCT, "n"),
			   PL.getLocBeginS(PT.Id pdc, P.addrX(P.dotX(PT.Id tpd, PT.Id loc))),
                           PT.IfThenElse(
			      P.eqX(PL.PDC_OK, 
				    PL.IOReadNextRecX(PT.Id pdc, P.addrX (PT.Id "n"))),
			      PT.Compound
			       [PT.IfThen(
				 P.gtX(PT.Id "n", P.zero),
				 PT.Compound
                                  [PT.IfThen(
				    PL.getSpecLevelX(PT.Id pdc),
				    PT.Compound
				     [PT.Return PL.PDC_ERROR]),
				   PL.getLocEndS(PT.Id pdc, P.addrX(P.dotX(PT.Id tpd, PT.Id loc)), ~1),
				   PT.IfThenElse(
				     PL.testNotPanicX(PT.Id pd),
				     PT.Compound(
				       [PL.userErrorS(PT.Id pdc, 
						      P.addrX(P.dotX(PT.Id tpd, PT.Id loc)),
						      PL.PDC_EXTRA_BEFORE_EOR,
						      readName, PT.String "Unexpected data before EOR.",
						      [])]
				       @ reportErrorSs(PL.PDC_EXTRA_BEFORE_EOR, true, P.dotX(PT.Id tpd,PT.Id loc))),
				     PT.Compound
					[PL.getLocEndS(PT.Id pdc, P.addrX(P.dotX(PT.Id tpd, PT.Id loc)), ~1),
					 PL.userInfoS(PT.Id pdc, 
						       P.addrX(P.dotX(PT.Id tpd, PT.Id loc)),
						       readName,
						       PT.String "Resynching at EOR", 
						       [])])]),
				PL.unsetPanicS(PT.Id pd)],
			      PT.Compound
			       [PT.IfThen(
				 PL.getSpecLevelX(PT.Id pdc),
				 PT.Compound[PT.Return PL.PDC_ERROR]),
				PL.unsetPanicS(PT.Id pd),
				PL.getLocEndS(PT.Id pdc, P.addrX(P.dotX(PT.Id tpd, PT.Id loc)), ~1),
				PL.userErrorS(PT.Id pdc, 
					      P.addrX(P.dotX(PT.Id tpd, PT.Id loc)),
					      PL.PDC_AT_EOR,
					      readName,
					      PT.String "Found EOF when searching for EOR", 
					      [])])]]


	      fun genReadFun (readName, cParams:(string * pcty)list, 
			      mPCT,pdPCT,canonicalPCT, mFirstPCT, hasNErr, bodySs) = 
		  let val (cNames, cTys) = ListPair.unzip cParams
                      val paramTys = [P.ptrPCT PL.toolStatePCT, P.ptrPCT mPCT]
			             @ cTys
			             @ [P.ptrPCT pdPCT, P.ptrPCT canonicalPCT]
                      val paramNames = [pdc, m] @ cNames @ [pd,rep]
                      val formalParams = List.map P.mkParam (ListPair.zip (paramTys, paramNames))
		      val incNerrSs = if hasNErr then
			              [P.assignS(P.arrowX(PT.Id pd, PT.Id nerr), P.zero)]
				      else []
		      val innerInitDecls = incNerrSs
				     @ [PL.unsetPanicS(PT.Id pd),
					P.assignS(P.arrowX(PT.Id pd, PT.Id errCode), PL.PDC_NO_ERROR)]
		      val returnTy =  PL.toolErrPCT
		      val checkParamsSs = [PL.IODiscChecks3P(PT.String readName, PT.Id m, PT.Id pd, PT.Id rep)]
		      val innerBody = checkParamsSs @ innerInitDecls @ bodySs
		      val readFunED = 
			  P.mkFunctionEDecl(readName, formalParams, PT.Compound innerBody, returnTy)
		  in
		      [readFunED]
		  end

(*
ssize_t test_write2io_internal (PDC_t *pdc, Sfio_t *io, <test_params>, test_pd *pd, test *rep);
ssize_t test_write2io          (PDC_t *pdc, Sfio_t *io, <test_params>, test_pd *pd, test *rep)

ssize_t test_write2buf_internal(PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, <test_params>, test_pd *pd, test *rep)
ssize_t test_write2buf         (PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, <test_params>, test_pd *pd, test *rep)

*)

	      fun writeAdjustLenSs shouldAdjustBuffer = 
		  ([PT.IfThen(P.ltX(PT.Id tlen, P.zero), PT.Return (P.intX ~1)),
		    P.plusAssignS(PT.Id length, PT.Id tlen)]
		   @ (if shouldAdjustBuffer then
			  [P.plusAssignS(PT.Id bufCursor, PT.Id tlen),
			   P.minusAssignS(PT.Id bufLen, PT.Id tlen)]
		      else []))

	      fun writeFieldSs (fname, argXs, adjustLengths) = 
		  [P.assignS(PT.Id tlen, 
			     PT.Call(PT.Id fname,
				     ([PT.Id pdc, PT.Id bufCursor, PT.Id bufLen, PT.Id bufFull]
				      @ argXs)))]
		  @ (writeAdjustLenSs adjustLengths)


	      fun genWriteFuns (writeName, isRecord, cParams:(string * pcty)list, 
		 	        pdPCT, canonicalPCT, iBodySs) = 
		  let val writeIOName = ioSuf writeName
		      val writeBufName = bufSuf writeName
		      val (cNames, cTys) = ListPair.unzip cParams
		      val commonTys = cTys @ [P.ptrPCT pdPCT, P.ptrPCT canonicalPCT]
                      val IOparamTys =   [P.ptrPCT PL.toolStatePCT, PL.sfioPCT] @ commonTys
                      val IOparamNames = [pdc, io] @ cNames @ [pd,rep]
		      val BufParamTys =   [P.ptrPCT PL.toolStatePCT, P.ptrPCT PL.bytePCT, PL.sizePCT, P.intPtr] @ commonTys
		      val BufParamNames = [pdc, buf, bufLen, bufFull] @ cNames @ [pd, rep]
                      val IOformalParams = List.map P.mkParam (ListPair.zip (IOparamTys, IOparamNames))
		      val BufFormalParams = List.map P.mkParam (ListPair.zip (BufParamTys, BufParamNames))
                      val BufparamArgs = List.map PT.Id BufParamNames
		      val returnTy =  PL.ssizePCT
                      
                      (* -- write2buf *)
		      val bufDeclSs = [P.varDeclS(P.ptrPCT PL.bytePCT, bufCursor, PT.Id buf),
				        P.varDeclS(PL.ssizePCT, length, P.zero),
				        P.varDeclS'(PL.ssizePCT, tlen)]
		      val bufCheckParamsSs = [PL.IODiscChecksSizeRet3P(PT.String writeBufName, 
								       PT.Id buf, PT.Id bufFull, PT.Id rep)]
		      val bufIntroSs = [P.assignS(P.starX (PT.Id bufFull), P.zero)]
		      val (bufRecordIntroSs, bufRecordCloseSs)  = 
                           if isRecord then
			       (([P.assignS(PT.Id tlen, 
					    PL.recOpenBufWrite(PT.Id pdc, PT.Id bufCursor, 
							       PT.Id bufLen, PT.Id bufFull, PT.String writeIOName))]
				  @ (writeAdjustLenSs true)),
				[P.assignS(PT.Id tlen,
					   PL.recCloseBufWrite(PT.Id pdc, PT.Id bufCursor,
							       PT.Id bufLen, PT.Id bufFull, 
							       PT.Id buf, PT.Id length, PT.String writeIOName))]
				  @  (writeAdjustLenSs false))
			   else ([],[])
		      val bufCloseSs = [PT.Return (PT.Id length)]
		      val bufBodySs  = bufDeclSs @ bufCheckParamsSs @ bufIntroSs @ bufRecordIntroSs @ iBodySs 
			                @ bufRecordCloseSs @bufCloseSs
		      val writeBufFunED = 
			  P.mkFunctionEDecl(writeBufName, BufFormalParams, PT.Compound bufBodySs, returnTy)

                      (* -- write2io_internal *)
		      val setBuf = "set_buf"
		      val introSs = [P.varDeclS'(P.ptrPCT PL.bytePCT, buf),
				     P.varDeclS'(P.int, setBuf),
				     P.varDeclS'(P.int, bufFull),
				     P.varDeclS'(PL.ssizePCT, length),
				     P.varDeclS'(PL.sizePCT, bufLen) ]
			  (* can optimize here if we know that type has static disk size:
			   /* XXX CASE 2: test is static length 107; test not a record type */
			   size_t       buf_len = 107;
			   /* XXX CASE 3: test is static length 107, test is a record type */
			   size_t       buf_len = 107 + pdc->disc->io_disc->rec_obytes + pdc->disc->io_disc->rec_cbytes;
			   *)
		      val ioCheckParamsSs = [PL.IODiscChecksSizeRet2P(PT.String writeIOName, PT.Id io, PT.Id rep)]
		      val ioBuflenInitSs = [P.assignS(PT.Id bufLen, P.arrowX(PT.Id pdc, PT.Id PL.outBufRes))]
                      (* beginning of loop to write record; loop in case original buffer isn't big enough *)
  		        val loopInitSs = [P.assignS(PT.Id setBuf, P.zero), 
					  P.assignS(PT.Id bufFull, P.zero)]
			val writeStartS = P.assignS(PT.Id buf,
					    PL.writeStartX(PT.Id pdc, PT.Id io, P.addrX(PT.Id bufLen), 
							   P.addrX (PT.Id setBuf), PT.String writeIOName))
			val chkBufS = PT.IfThen(P.notX (PT.Id buf),
			                PT.Compound[
				         P.mkCommentS "Don't have to abort because start failed.",
					 PT.Return (P.intX ~1)])
			val doWriteS = P.assignS(PT.Id length,
				         PT.Call(PT.Id writeBufName, 
					         [PT.Id pdc, PT.Id buf, PT.Id bufLen,
						  P.addrX (PT.Id bufFull)] @ (List.map PT.Id (cNames @ [pd, rep]))))
			val chkResS = PT.IfThen(PT.Id bufFull,
				        PT.Compound[
				         P.mkCommentS("Try again with a bigger buffer"),
				         PL.writeAbortS(PT.Id pdc, PT.Id io, PT.Id buf, PT.Id setBuf, PT.String writeIOName),
				         P.timesAssignS(PT.Id bufLen, P.intX 2),
				         PT.Continue])
		        val loopEndS = PT.Break
		      val whileLoopS = PT.While(P.intX 1, 
                                        PT.Compound(loopInitSs @
						    [writeStartS, chkBufS, doWriteS, chkResS, loopEndS]))
		      val chkResS = PT.IfThen(P.gteX(PT.Id length, P.zero),
				      PT.Compound[
					 PT.Return 
					   (PL.writeCommitX(PT.Id pdc, PT.Id io, PT.Id buf, 
							    PT.Id setBuf, PT.Id length, PT.String writeIOName))])
		      val abortS = PL.writeAbortS(PT.Id pdc, PT.Id io, PT.Id buf, PT.Id setBuf, PT.String writeIOName)
					 
		      val returnS = PT.Return (P.intX ~1)
		      val bodySs = introSs @ ioCheckParamsSs @ ioBuflenInitSs @ [ whileLoopS, chkResS, abortS, returnS]
		      val bodyS = PT.Compound bodySs
		      val writeIOFunED = P.mkFunctionEDecl(writeIOName, IOformalParams, bodyS, returnTy)
		  in
		      [writeBufFunED, writeIOFunED]
		  end

              (* PDC_error_t foo_init/foo_clear(PDC_t* pdc, foo *r) *)
              fun genInitFun(funName, argName, argPCT, bodySs, dummy) = 
		  let val paramTys = [P.ptrPCT PL.toolStatePCT, 
				      P.ptrPCT argPCT]
		      val paramNames = [pdc, argName]
		      val formalParams = List.map P.mkParam (ListPair.zip (paramTys, paramNames))
		      val chkTSSs = if dummy then [] 
				    else [PT.IfThen(P.orX(P.notX(PT.Id pdc),(P.notX(PT.Id argName))), 
						   PT.Return PL.PDC_ERROR)]
		      val bodySs = chkTSSs @ bodySs @ [PT.Return PL.PDC_OK]
		      val returnTy =  PL.toolErrPCT
		      val initFunED = 
			  P.mkFunctionEDecl(funName, formalParams, 
					    PT.Compound bodySs, returnTy)
		  in
		      initFunED
		  end

	      fun genMaskInitFun(funName, maskPCT) = 
		  let val mask = "mask"
		      val baseMask = "baseMask"
		      val paramTys = [P.ptrPCT PL.toolStatePCT, P.ptrPCT maskPCT, PL.base_mPCT]
		      val paramNames = [pdc, mask, baseMask]  
		      val formalParams = List.map P.mkParam (ListPair.zip(paramTys, paramNames))
		      val bodySs = [PL.fillMaskS(PT.Id mask, PT.Id baseMask, maskPCT)]
		      val returnTy =  P.void
		      val maskInitFunED = 
			  P.mkFunctionEDecl(funName, formalParams, 
					    PT.Compound bodySs, returnTy)
		  in
		      [maskInitFunED]
		  end

              (* PDC_error_t foo_copy(PDC_t* pdc, foo *dst, foo* src) *)
              fun genCopyFun(funName, dst, src, argPCT, bodySs, static) = 
		  let val paramTys = [P.ptrPCT PL.toolStatePCT, 
				      P.ptrPCT argPCT,
				      P.ptrPCT argPCT]
		      val paramNames = [pdc, dst, src]
		      val formalParams = List.map P.mkParam (ListPair.zip (paramTys, paramNames))
		      val chkTSSs = if static then [] 
				    else [PT.IfThen(P.orX(P.notX(PT.Id pdc),
							  P.orX(P.notX(PT.Id src), P.notX(PT.Id dst))),
						   PT.Return PL.PDC_ERROR)]
		      val bodySs = chkTSSs @ bodySs @ [PT.Return PL.PDC_OK]
		      val returnTy =  PL.toolErrPCT
		      val copyFunED = 
			  P.mkFunctionEDecl(funName, formalParams, 
					    PT.Compound bodySs, returnTy)
		  in
		      copyFunED
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
						     P.orX(P.notX(PT.Id pdc), P.notX(PT.Id acc)), 
						     P.notX discX),
						   PT.Compound[PT.Return PL.PDC_ERROR])
		      val chkErrorFS = PT.IfThen(P.notX errorFX, PT.Compound[PT.Return PL.PDC_OK])
		      val internalCallS = P.assignS(PT.Id result,
						    PT.Call(PT.Id (ioSuf reportName),
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
                      fun genParamNames extraNames = [pdc] @ extraNames @ [ prefix, what, nst, acc]
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
		      val toioReportFunED = P.mkFunctionEDecl(ioSuf reportName, intlFormalParams, bodyS, returnTy)
		      val externalReportFunED = genExternalReport(reportName, intlParamNames, extlFormalParams)
		  in
		      [toioReportFunED, externalReportFunED]
		  end

		  (** generation of common PADS-Galax stuff **)
		  (* auxiliary functions *)
		  fun apply [] x = []
	            | apply (f::fs) x = (f x)::(apply fs x)

		  fun inc x = x + 1

		  fun listOf n = List.tabulate (n,inc)

		  fun enumerate xs = ListPair.zip(listOf (List.length xs),xs)

                  (* header: common declaration part in foo_children function *) 
		  fun headerGalaxChildrenFun(nameTy) =
    		      let val nodeRepTy = PL.nodeT
			  fun varDecl(field,ty) = 
			      let fun typePref n = P.ptrPCT (P.makeTypedefPCT n)        	
				  val typeField = typePref ty
			      in P.varDeclS(typeField, field, PT.Cast(typeField, fieldX(self,field)))
			      end
		      in List.map varDecl (ListPair.zip([rep,pd,m],(apply [repSuf,pdSuf,mSuf] nameTy)))
			 @ [P.varDeclS'(P.ptrPCT (P.ptrPCT nodeRepTy), result)]
		      end

		  (* if: common if-then in foo_children function *)
		  fun ifGalaxChildren(returnName, number, errorString) =
		      [PT.IfThen(P.notX(P.assignX(returnName,
			       			  PT.Call(PL.PDCI_NEW_NODE_PTR_LIST, 
					                  [fieldX(self,pdc), 
						           number]))),		
                                 PT.Expr(PT.Call(PT.Id "failwith",[PT.String errorString])))]
	
		  (* PDCI_MK_TNODE: common in foo_children function *)
		  fun macroTNodeCall (returnName, index, structId, valStr, valId, cnvName) = 
		       [PT.Expr(PT.Call(PL.PDCI_MK_TNODE,	
                                       [P.subX(returnName,index), 
                                        P.addrX(PT.Id (vTableSuf structId)),
                                        PT.Id self, 
                                        PT.String valStr, 
                                        valId,
                                        PT.String cnvName]))]

		  fun macroTNode (returnName, structId, valStr, valId, cnvName) = 
		      (P.mkCommentS "parse descriptor child")::
		       macroTNodeCall(returnName,P.zero,structId,valStr,valId,cnvName)

		  (* PDCI_MK_NODE: common in foo_children function *)
		  fun macroNodeCall (returnName,n,tyField,nameField,getField1,getField2,getField3,nameStruct) = 
	  	      PT.Expr(PT.Call(PL.PDCI_MK_NODE,
                                      [P.subX(returnName,n), 
                                       P.addrX(PT.Id (vTableSuf tyField)),
                                       PT.Id self, 
                                       PT.String nameField, 
				       getField1, getField2, getField3,	
                                       PT.String nameStruct])) 
	
		  (* const PDCI_vtable_t foo_vtable = {foo_children,PDCI_error_typed_value,0}; *)
                  fun genGalaxVtable(name) =
		      PT.ExternalDecl(PT.Declaration({specifiers=[PL.PDCI_vtable_t],qualifiers=[PT.CONST],storage=[]},
                                                     [(PT.VarDecr (vTableSuf name),
			                               PT.InitList [PT.Id (childrenSuf name),
                                              		            PL.PDCI_error_typed_value,
	                                         		    P.zero])])) 

		  (** end generation of common PADS-Galax stuff **)		

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
			      [PT.DefaultLabel(PT.Return (PT.String "*unknown_tag*"))]
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
                      val paramNames = [pdc, acc]
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
					   [PT.Id pdc, e])),
			     PT.Compound[PT.Expr(P.postIncX (PT.Id nerr))])]


              (*  PDC_error_t T_acc_add (PDC_t* , T_acc* , T_pd*, T* ) *)
	      fun genAddFun (addName, accPCT, pdPCT, repPCT, bodySs) = 
		  let val paramTys = [P.ptrPCT PL.toolStatePCT, 
				      P.ptrPCT accPCT, 
				      P.ptrPCT pdPCT,
				      P.ptrPCT repPCT]
                      val paramNames = [pdc, acc, pd, rep]
                      val formalParams = List.map P.mkParam (ListPair.zip (paramTys, paramNames))
		      val returnTy =  PL.toolErrPCT
		      val addFunED = 
			  P.mkFunctionEDecl(addName, formalParams, PT.Compound bodySs, returnTy)
		  in
		      addFunED
		  end



              fun chkAddFun (funName, accX,pdX,repX) = 
		  [PT.IfThen(P.eqX(PL.PDC_ERROR, 
				   PT.Call(PT.Id funName, 
					   [PT.Id pdc, accX, pdX, repX])),
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
				    [PT.Id pdc, PT.Id outstr, PL.sfstruse (PT.Id tmpstr), whatX, PT.Id nst, 
				     fieldX])
		  in
		      printScaffolding(fieldDescriptor, extraArgsXs, bodyX)
		  end

	      fun callIntPrint (reportName, prefixX, whatX, nstX, fieldX) = 
		  PT.Call(PT.Id reportName, 
			  [PT.Id pdc, PT.Id outstr, prefixX, whatX, nstX, fieldX])

	      fun callEnumPrint (reportName, prefixX, whatX, nstX, mapFnX, fieldX) = 
		  PT.Call(PT.Id reportName, 
			  [PT.Id pdc, PT.Id outstr,  prefixX, whatX, nstX,
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
                                      
              fun reportErrorSs(locCodeSs, locX, shouldIncNerr, errCodeC, shouldPrint, funStr, msgStr, args) = 
		  let val errCodeX = fieldX(pd,errCode)
		      val msgX = if msgStr = "" then P.zero else PT.String msgStr
		      val nErrSs = if shouldIncNerr 
			           then [P.postIncS (fieldX(pd,nerr))]
				   else []
                      val printSs = if shouldPrint 
				    then [PL.userErrorS(PT.Id pdc, locX, 
							errCodeX, funStr, msgX, args)]
				    else []
		  in
                     nErrSs
                    @[P.assignS(fieldX(pd,errCode), errCodeC)]
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
		      [{diskSize = TyProps.mkSize (0,0), 
			memChar = if isStatic 
				      then TyProps.Static 
				  else TyProps.Dynamic,
			endian = false, isRecord = false, 
                        containsRecord = false, largeHeuristic = false, labels = [SOME (name, "Pcompute",([],[]))]}]
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

              (* Given manifest field, use f to generate struct field declaration from pads pty*)
	      fun genMan f {decl, comment:string option} = 
		  let val ctNoptEs = cnvDeclaration decl
		      fun doOne (cty, nameOpt, exp) = 
			  let val name = case nameOpt of NONE => "bogus"
			| SOME n => n  
			      val defStringOpt = SOME(P.expToString exp)
			      val fullCommentOpt = stringOptMerge(comment, defStringOpt)
			  in
			      case reverseLookup cty 
				  of SOME pty => 
				      [(name, P.makeTypedefPCT (f pty), fullCommentOpt)]
				| NONE => []
			  end
		  in
		      List.concat(List.map doOne ctNoptEs)
		  end
              
	      
	      (* Given representation of manifest field, generate accumulator representation. *)
	      fun genAccMan m = 
		  let fun getName (PX.Name n) = n
		      fun f pty = 
		        valOf (lookupAcc pty) handle x => (PE.error ("Failed to find accumulator:" ^(getName pty)); "foo")
		  in
		     genMan f m
		  end

	      (* Given representation of manifest field, generate parse descriptor representation. *)
	      fun genEDMan m = 
		  let fun f pty = lookupTy(pty, pdSuf, #pdname)
		  in
		      genMan f m
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

	      (* Given manifest representation, generate accumulator functions(init,reset, cleanup) *)
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
							  [PT.Id pdc, gfieldX acc, errDescX, gfieldX rep])),
					    PT.Compound[PT.Expr(P.postIncX (PT.Id nerr))])]
			     end
	      (* end accOpt SOME case *))


	      (* Given manifest representation, generate report function *)
	      fun cnvPtyForReport(reportSuf, ioSuf, pty, name) = 
		  case lookupAcc(pty) of NONE => []
		| SOME a => (
			     let val reportName = reportSuf a
				 fun gfieldX base = getFieldX(base,name)
			     in
				 genPrintPiece(ioSuf reportName, name, P.zero, gfieldX acc,[])
			     end
	      (* end accOpt SOME case *))

	      fun genAccReportMan (reportSuf, ioSuf) {decl, comment:string option} = 
		  let val ctNoptEs = cnvDeclaration decl
		      fun doOne(cty,nameOpt,exp) = 
			  let val name= case nameOpt of NONE => "bogus" | SOME n => n
			      val accPtyOpt = reverseLookup cty
			  in
			      case accPtyOpt of NONE => [] 
			    | SOME pty => (cnvPtyForReport(reportSuf, ioSuf, pty,name))
			  end
		  in
		      List.concat(List.map doOne ctNoptEs)
		  end

	      fun emit (condition, eds) = 
		  if condition then 
		      List.concat(List.map cnvExternalDecl eds)
		  else []

              fun emitAccum eds = emit (!(#outputAccum(PInput.inputs)), eds)
              fun emitRead  eds = emit (!(#outputRead(PInput.inputs)), eds)
              fun emitWrite eds = emit (!(#outputWrite(PInput.inputs)), eds)
              fun emitXML   eds = emit (!(#outputXML(PInput.inputs)), eds)

              fun cnvCTy ctyED = 
		  let val astdecls = cnvExternalDecl ctyED
		  in
		    (if not (List.length astdecls = 1) then (PE.bug "Expected no more than one external declaration") 
                                                       else ();
		     case List.hd astdecls 
                      of Ast.DECL(Ast.ExternalDecl (Ast.TypeDecl {shadow, tid}), aid, paid, loc) => (astdecls, tid)
                      | _ => (PE.error "Expected ast declaration"; (astdecls, Tid.new())))
		  end

              fun cnvRep (canonicalED, padsInfo) = 
		  case cnvCTy canonicalED 
		  of (Ast.DECL(coreDecl, aid, paid,loc)::xs, tid) =>
                       (Ast.DECL(coreDecl, aid, bindPaid padsInfo, loc) ::xs, tid)
                  | _ => (PE.bug "Expected Ast declaration"; ([], Tid.new()))

(*  Typedef case *)
	      fun cnvPTypedef ({name : string, params: (pcty * pcdecr) list, isRecord, containsRecord, 
			        largeHeuristic,	isFile : bool, baseTy: PX.Pty, args: pcexp list, 
			        predTy: PX.Pty, thisVar: string, pred: pcexp}) = 
		  let val base = "base"
		      val user = "user"
		      val baseTyName = lookupTy(baseTy,repSuf,#padsname)		
		      val baseTypeName = lookupTy(baseTy,repSuf,#repname)		

                      (* Generate CheckSet mask typedef case*)
		      val baseMPCT = P.makeTypedefPCT(lookupTy(baseTy,mSuf, #mname))
                      val mFields  = [(base, baseMPCT, SOME "Base mask"),
				       (user, PL.base_mPCT, SOME "Typedef mask")]
		      val mED      = P.makeTyDefStructEDecl (mFields, mSuf name)
		      val mDecls   = cnvExternalDecl mED
                      val mPCT     = P.makeTypedefPCT (mSuf name)		

                      (* Generate parse description *)
		      val baseEDPCT = P.makeTypedefPCT(lookupTy(baseTy,pdSuf, #pdname))
                      val pdFields  = [(pstate, PL.flags_t, NONE), (errCode, PL.errCodePCT, NONE),
				       (loc, PL.locPCT,NONE), (nerr, PL.uint32, NONE),
				       (base, baseEDPCT, SOME "Base parse description")]
		      val pdED      = P.makeTyDefStructEDecl (pdFields, pdSuf name)
		      val (pdDecls,pdTid)  = cnvCTy pdED
                      val pdPCT     = P.makeTypedefPCT (pdSuf name)		

  		      (* Generate accumulator type *)
		      val PX.Name baseName = baseTy
		      val baseAccPCT = case PBTys.find(PBTys.baseInfo, Atom.atom baseName) 
			               of NONE => P.makeTypedefPCT (accSuf baseName)  (* must have been generated *)
                                       | SOME(b:PBTys.baseInfoTy) => 
					   (case (#accname b) 
					    of NONE => P.voidPtr   (* accumulation not defined for this base type *)
 			                    | SOME acc => (P.makeTypedefPCT (Atom.toString acc)))
		      val accED     = P.makeTyDefEDecl (baseAccPCT, accSuf name)
		      val accPCT    = P.makeTypedefPCT (accSuf name)		

		      (* Insert type properties into type table *)
                      val ds = lookupDiskSize baseTy
                      val mc = lookupMemChar baseTy
                      val endian = lookupEndian baseTy
                      val contR = lookupContainsRecord baseTy
 		      val lH = lookupHeuristic baseTy
		      val numArgs = List.length params
		      val typedefProps = buildTyProps(name, PTys.Typedef, ds, TyProps.Typedef ds, mc, endian, isRecord, contR, lH, isFile, pdTid, numArgs)
                      val () = PTys.insert(Atom.atom name, typedefProps)


		      (* Generate canonical representation: typedef to base representation *)
		      val baseTyPCT = P.makeTypedefPCT(lookupTy(baseTy, repSuf, #repname))
		      val canonicalED = P.makeTyDefEDecl (baseTyPCT, repSuf name)
		      val (canonicalDecls,canonicalTid) = cnvRep(canonicalED, valOf (PTys.find (Atom.atom name)))
                      val canonicalPCT = P.makeTypedefPCT (repSuf name)			 



                      (* Generate Init function (typedef case) *)
		      val baseFunName = lookupMemFun (PX.Name baseTyName)
		      val initFunName = lookupMemFun (PX.Name name)
                      fun genInitEDs (suf, argName, aPCT) = case #memChar typedefProps
                          of TyProps.Static => 
				  [genInitFun(suf initFunName, argName, aPCT, [],true)]
                           | TyProps.Dynamic =>
			      let val bodySs = [PL.bzeroS(PT.Id argName, P.sizeofX(aPCT))]
			      in
				  [genInitFun(suf initFunName, argName, aPCT, bodySs,false)]
			      end
                      val initRepEDs = genInitEDs (initSuf, rep, canonicalPCT)
                      val initPDEDs  = genInitEDs ((initSuf o pdSuf), pd, pdPCT)
                      fun genCleanupEDs (suf, argName, aPCT) = case #memChar typedefProps
                          of TyProps.Static => 
				  [genInitFun(suf initFunName, argName, aPCT, [],true)]
                           | TyProps.Dynamic =>
			      let val bodySs = 
				  [PT.Expr(
				    PT.Call(PT.Id (suf baseFunName),
					    [PT.Id pdc, PT.Id rep]))]
			      in
				  [genInitFun(suf initFunName, argName, aPCT, bodySs,false)]
			      end
                      val cleanupRepEDs = genCleanupEDs (cleanupSuf, rep, canonicalPCT)
                      val cleanupPDEDs  = genCleanupEDs ((cleanupSuf o pdSuf), pd, pdPCT)

                      (* Generate Copy Function typedef case *)
                      fun genCopyEDs(suf, base, aPCT) = 
			  let val copyFunName = suf initFunName
			      val dst = dstSuf base
			      val src = srcSuf base
			      val nestedCopyFunName = suf baseFunName
			      val bodySs = 
				  case #memChar typedefProps
				   of TyProps.Static => [PL.memcpyS(PT.Id dst, PT.Id src, P.sizeofX aPCT)]
				   | _ => [PT.Expr(PT.Call(PT.Id nestedCopyFunName, 
							    [PT.Id pdc, PT.Id dst, PT.Id src]))]
			  in
			      [genCopyFun(copyFunName, dst, src, aPCT, bodySs,false)]
			  end
		      val copyRepEDs = genCopyEDs(copySuf o repSuf, rep, canonicalPCT)
		      val copyPDEDs  = genCopyEDs(copySuf o pdSuf,  pd,  pdPCT)

                      (* Generate m_init function typedef case *)
                      val maskInitName = maskInitSuf name 
                      val maskFunEDs = genMaskInitFun(maskInitName, mPCT)

                      (* Generate read function *)
                      (* -- Some helper functions *)
		      val readName = readSuf name
                      val baseReadFun = lookupTy(baseTy, readSuf, #readname)
		      val () = checkParamTys(name, baseReadFun, args, 2, 2)
		      val modPredX = PTSub.substExp (thisVar, P.starX(PT.Id rep), pred)
		      fun chk () = expEqualTy(modPredX, CTintTys, 				
					  fn s=> (" constraint for typedef "^
						  name ^ " has type: " ^ s ^
						  ". Expected an int."))

                      fun genReadSs () = 
			  let val resDeclSs = [P.varDeclS'(PL.toolErrPCT, result)]
			      val readBaseSs = 
				  [P.assignS(PT.Id result, 
					     PL.readFunX(baseReadFun, 
							 PT.Id pdc, 
							 P.addrX (fieldX(m,base)),
							 args,
							 P.addrX (fieldX(pd,base)),
							 PT.Id rep)),
				   PT.IfThen(P.eqX(PT.Id result, PL.PDC_ERROR),
					     PT.Goto (findEORSuf name))]

			      val checkConstraintSs = 
				  [PT.IfThen(P.andX(PL.mTestSemCheckX(fieldX(m,user)),
						    P.notX modPredX),
					     PT.Compound (reportErrorSs([locS],locX,
									true,
									PL.PDC_TYPEDEF_CONSTRAINT_ERR,
									true, readName, "", [])
							  @ [P.assignS(PT.Id result, PL.PDC_ERROR),
							     PT.Goto (findEORSuf name)])
					     )]
			      val slurpSs = if isRecord then genReadEOR (readName,reportStructErrorSs) () else []
			      val endSs = [PT.Labeled(findEORSuf name, 
						     PT.Compound(slurpSs @ [PT.Return (PT.Id result)]))]
		      in
			  [PT.Compound (resDeclSs @ readBaseSs @ checkConstraintSs @ endSs)]
		      end

                      (* -- Assemble read function typedef case *)
		      val _ = pushLocalEnv()                                        (* create new scope *)
		      val () = ignore (insTempVar(rep, P.ptrPCT canonicalPCT))      (* add rep to scope *)
		      val cParams : (string * pcty) list = List.map mungeParam params
                      val () = ignore (List.map insTempVar cParams)                 (* add params for type checking *)
		      val () = chk()
		      val readFields = genReadSs ()                                 (* does type checking *)
		      val _ = popLocalEnv()                                         (* remove scope *)
		      val bodySs = readFields 
		      val readFunEDs = genReadFun(readName, cParams, mPCT,pdPCT,canonicalPCT, 
						  NONE, true, bodySs)

                      val readEDs = initRepEDs @ initPDEDs @ cleanupRepEDs @ cleanupPDEDs
			          @ copyRepEDs @ copyPDEDs @ maskFunEDs @ readFunEDs


                      (* -- generate accumulator init, reset, and cleanup functions (typedef case) *)
		      fun genResetInitCleanup theSuf = 
			  let val theFun = (theSuf o accSuf) name
			  in case lookupAcc baseTy 
			      of NONE => (gen3PFun(theFun, accPCT, 
						   [P.mkCommentS ("Accumulation not defined for base type of ptypedef."),
						    PT.Return PL.PDC_OK])
			                                     (* end NONE *))
				| SOME a => (
				   let val theBodyE = PT.Call(PT.Id (theSuf a), 
							      [PT.Id pdc, PT.Id acc])
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
                      (*  PDC_error_t T_acc_add (PDC_t* , T_acc* , T_pd*, T* ) *)
		      val addFun = (addSuf o accSuf) name
		      fun genAdd NONE = genAddFun(addFun, accPCT, pdPCT, canonicalPCT, 
						  [P.mkCommentS ("Accumulation not defined for base type of ptypedef."),
						   PT.Return PL.PDC_OK])
                        | genAdd (SOME a) =
                           let val addX = PT.Call(PT.Id (addSuf  a), 
						  [PT.Id pdc, PT.Id acc, 
						   P.addrX(P.arrowX(PT.Id pd,PT.Id base)), PT.Id rep])
			       val addReturnS = PT.Return addX
			       val addBodySs =  [addReturnS]
			   in
			       genAddFun(addFun, accPCT, pdPCT, canonicalPCT, addBodySs)
			   end

                          (* end SOME case *)
                      val addFunED = genAdd (lookupAcc baseTy)

                      (* -- generate report function ptypedef *)
                      (*  PDC_error_t T_acc_report (PDC_t* , T_acc* , const char* prefix) *)
		      val reportFun = (reportSuf o accSuf) name
		      val reportFields = 
			  case lookupAcc(baseTy) of NONE => []
                             | SOME a => (
				 genPrintPiece((ioSuf o reportSuf) a, name, P.zero, PT.Id acc,[])
		             (* end accOpt SOME case *))

                      val reportFunEDs = genReportFuns(reportFun, "typedef "^name, accPCT, reportFields)
		      val accumEDs = accED :: initFunED :: resetFunED :: cleanupFunED :: addFunED :: reportFunEDs

                      (* Generate Write function typedef case *)
		      val writeName = writeSuf name
		      val writeBaseName = (bufSuf o writeSuf) (lookupWrite baseTy) 
		      val bodySs = writeFieldSs(writeBaseName, args @ [getFieldX(pd,base), PT.Id rep], isRecord)
                      val writeFunEDs = genWriteFuns(writeName, isRecord, cParams, pdPCT, canonicalPCT, bodySs)

	              (***** typedef PADS-Galax *****)

    	              (* PDCI_node_t** fooTy_children(PDCI_node_t *self) *)
		      fun genGalaxTyChildrenFun(name) =		
		          let val nodeRepTy = PL.nodeT
                              val returnName = PT.Id result
			      val returnTy = P.ptrPCT (P.ptrPCT (nodeRepTy))
                              val cnvName = childrenSuf name 
                              val paramNames = [self]
                              val paramTys = [P.ptrPCT nodeRepTy]
                              val formalParams =  List.map P.mkParam(ListPair.zip(paramTys, paramNames))
			      val enumType = P.ptrPCT(P.makeTypedefPCT name)
			      val baseType = P.ptrPCT(PL.base_pdPCT)
 		              val bodySs = headerGalaxChildrenFun(name) @
					   ifGalaxChildren(returnName,P.intX 2, "ALLOC_ERROR: in " ^ cnvName) @
					   macroTNode(returnName,PL.PDCI_structured_pd,pd,PT.Id pd,cnvName) @
					   [P.mkCommentS "base child",
					    macroNodeCall(returnName,P.intX 1,baseTypeName,base,
				     			  getFieldX(m,base),getFieldX(pd,base),PT.Id rep,cnvName),
					    P.returnS (returnName)]
                              in   
                               P.mkFunctionEDecl(cnvName, formalParams, PT.Compound bodySs, returnTy)
                              end

	              val galaxEDs = [genGalaxTyChildrenFun(name),
				      genGalaxVtable(name)]

		  in
		        canonicalDecls
                      @ mDecls
                      @ pdDecls
		      @ (emitRead readEDs)
                      @ (emitAccum accumEDs)
                      @ (emitWrite writeFunEDs)
  		      @ (emitXML galaxEDs)
		  end



	      fun cnvPStruct ({name: string, isRecord, containsRecord, largeHeuristic, isFile, 
                               params: (pcty * pcdecr) list, fields: (pdty, pcdecr, pcexp) PX.PSField list, 
                               postCond}) = 
	          let val structName = name
		      val dummy = "_dummy"
		      val cParams : (string * pcty) list = List.map mungeParam params
		      val paramNames = #1(ListPair.unzip cParams)

		      (* Functions for walking over lists of struct elements *)
		      fun mungeField f b m (PX.Full fd) = f fd
                        | mungeField f b m (PX.Brief e) = b e
                        | mungeField f b m (PX.Manifest md) = m md
		      fun mungeFields f b m [] = []
			| mungeFields f b m (x::xs) = (mungeField f b m x) @ (mungeFields f b m xs)

		      (* Struct: Error checking *)
		      fun checkFull {pty: PX.Pty, args: pcexp list, name: string, isVirtual: bool, 
				     isEndian: bool, isRecord, containsRecord, largeHeuristic: bool,
				     pred: pcexp option, comment: string option} = 
			  (if name = "pd" orelse name = "structLevel" 
			       then PE.error ("Pstruct "^ structName ^" contains field with reserved name '"^name^"'.\n")  
			   else (); 
			   let val ty = P.makeTypedefPCT(lookupTy(pty, repSuf, #repname))
			   in
			       CTcnvType ty  (* ensure that the type has been defined *)
			   end; [])
		      fun checkBrief e = []
		      fun checkMan m = []
		      val _ = mungeFields checkFull checkBrief checkMan fields


		      (* Generate local variables  *)
		      fun genLocFull {pty: PX.Pty, args: pcexp list, name: string, isVirtual: bool, 
				      isEndian: bool, isRecord, containsRecord, largeHeuristic: bool,
				      pred: pcexp option, comment: string option} = 
			  if not isVirtual then []
			  else [(name, P.makeTypedefPCT(lookupTy (pty,repSuf,#repname)))]
		      fun genLocBrief e = []
		      fun genLocMan m = []
		      val localVars = mungeFields genLocFull genLocBrief genLocMan fields

		       
		      (* Generate CheckSet mask *)
		      fun genMFull {pty: PX.Pty, args: pcexp list, name: string, 
				     isVirtual: bool, isEndian: bool, isRecord, containsRecord, largeHeuristic: bool,
				     pred:pcexp option, comment} = 
			  [(name,P.makeTypedefPCT(lookupTy (pty,mSuf,#mname)), NONE)]
		      fun genMBrief e = []
		      fun genMMan m = []
		      val mFieldsNested = mungeFields genMFull genMBrief genMMan fields
		      val auxMFields = [(all, PL.base_mPCT, NONE)]
		      val mFields = auxMFields @ mFieldsNested

		      val mFirstPCT = getFirstEMPCT mFields
		      val mStructED = P.makeTyDefStructEDecl (mFields, mSuf name)
		      val mDecls = cnvExternalDecl mStructED 
                      val mPCT = P.makeTypedefPCT (mSuf name)			  

		      (* Generate parse description *)
		      fun genEDFull {pty: PX.Pty, args: pcexp list, name: string,  
				     isVirtual: bool, isEndian: bool, 
                                     isRecord, containsRecord, largeHeuristic: bool, 
				     pred:pcexp option, comment} = 
			  [(name,P.makeTypedefPCT(lookupTy (pty,pdSuf,#pdname)),NONE)]
		      fun genEDBrief e = []
		      val auxEDFields = [(pstate, PL.flags_t, NONE), (errCode, PL.errCodePCT, NONE),
					 (loc, PL.locPCT, NONE), (nerr, PL.uint32, NONE)]
		      val pdFields = auxEDFields @ (mungeFields genEDFull genEDBrief genMMan fields)
		      val pdStructED = P.makeTyDefStructEDecl (pdFields, pdSuf name)
		      val (pdDecls,pdTid) = cnvCTy pdStructED 
                      val pdPCT = P.makeTypedefPCT (pdSuf name)			  

		      (* Generate accumulator type *)
		      fun genAccFull {pty: PX.Pty, args: pcexp list, name: string, 
				      isVirtual: bool, isEndian: bool, 
				      isRecord, containsRecord, largeHeuristic: bool,
				      pred: pcexp option, comment: string option} = 
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
		      val accFields = mungeFields genAccFull genAccBrief genAccMan fields
		      val auxAccFields = [(nerr, PL.intAccPCT, NONE)]
		      val accED = P.makeTyDefStructEDecl (auxAccFields @ accFields, accSuf name)
                      val accPCT = P.makeTypedefPCT (accSuf name)			 

		      (* Struct: Calculate and insert type properties into type table *)
		      fun genTyPropsFull {pty: PX.Pty, args: pcexp list, name: string, 
					  isVirtual: bool, isEndian: bool, isRecord, containsRecord, 
					  largeHeuristic: bool, pred: pcexp option, comment:string option} = 
			  let val PX.Name ftyName = pty
			      val mc = lookupMemChar pty
			      val ds = computeDiskSize (name, paramNames, pty, args)
                              val supportsEndian = lookupEndian pty
			      val isE1 = if isEndian andalso not supportsEndian
				         then (PE.error ("Endian annotation not supported on fields of type "
							 ^(tyName pty)^".\n"); false)
				         else true
			      val isE2 = if isEndian andalso not (Option.isSome pred)
				         then (PE.error ("Endian annotations require constraints ("^name^").\n"); false)
				         else true
			      val contR = lookupContainsRecord pty 
			      val lH = lookupHeuristic pty
			  in [{diskSize = ds, memChar = mc, endian = isEndian andalso isE1 andalso isE2, 
                               isRecord = isRecord, containsRecord = contR, 
			       largeHeuristic = lH, labels = [SOME (name, ftyName, (paramNames,args))]}] 
                          end
		      fun genTyPropsBrief e = 
			  (* assume field is correct; error checking done in genReadBrief below *)
                          (* conservative analysis: variable expresions with type char could also lead to size of 1,0*)
			  let fun getStaticSize eX =
			          case eX of PT.MARKexpression(l,e) => getStaticSize e
				  | PT.String s => TyProps.mkSize(String.size s, 0)
				  | PT.IntConst i => TyProps.mkSize(1,0)
				  | _ => TyProps.Variable
			      val diskSize = getStaticSize e
			  in
			      [{diskSize = diskSize, memChar = TyProps.Static, 
				endian = false, isRecord = false, 
				containsRecord = false, largeHeuristic = false, labels = [NONE]}]
		          end
		      val tyProps = mungeFields genTyPropsFull genTyPropsBrief genTyPropsMan fields
                      val {diskSize, memChar, endian, isRecord=_, containsRecord, largeHeuristic,labels} = 
 			               List.foldl (PTys.mergeTyInfo TyProps.add) PTys.minTyInfo tyProps
		      val compoundDiskSize = TyProps.Struct ((ListPair.zip(List.rev labels, 
									  (List.map (fn (r : PTys.sTyInfo) => #diskSize r) tyProps))))
		      val numArgs = List.length params
		      val structProps = buildTyProps(name, PTys.Struct, diskSize, compoundDiskSize, memChar, endian, 
                                                     isRecord, containsRecord, largeHeuristic, isFile, pdTid, numArgs)
                      val () = PTys.insert(Atom.atom name, structProps)

		      (* Struct: Generate canonical representation *)
		      fun genRepFull {pty: PX.Pty, args: pcexp list, name: string, 
				      isVirtual: bool, isEndian: bool, isRecord, containsRecord, largeHeuristic: bool,
				      pred:pcexp option, comment:string option} = 
			  if not isVirtual then 
			    let val predStringOpt = Option.map P.expToString pred
			        val fullCommentOpt = stringOptMerge(comment, predStringOpt)
			    in
			      [(name,P.makeTypedefPCT(lookupTy (pty,repSuf,#repname)), fullCommentOpt )]
			    end
			  else []
		      fun genRepBrief e = []
		      val canonicalFields = mungeFields genRepFull genRepBrief genRepMan fields
		      val canonicalFields = if List.length canonicalFields = 0 
			                    then (PE.warn ("PStruct "^structName^" does not contain any non-omitted fields.\n");
						 [(dummy, PL.uint32, SOME "Dummy field inserted to avoid empty struct")])

					    else canonicalFields
		      val canonicalStructED = P.makeTyDefStructEDecl (canonicalFields, repSuf name)
		      val (canonicalDecls, canonicalTid) = cnvRep(canonicalStructED, valOf (PTys.find (Atom.atom name)))

                      val canonicalPCT = P.makeTypedefPCT (repSuf name)			 

                      (* Generate Init Function struct case *)
		      val baseFunName = lookupMemFun (PX.Name name)
                      fun genInitEDs(suf,base,aPCT) = case #memChar structProps
			  of TyProps.Static => [genInitFun(suf baseFunName, base, aPCT, [],true)]
			   | TyProps.Dynamic => 
			      let val zeroSs = [PL.bzeroS(PT.Id base, P.sizeofX(aPCT))]
			      in
				   [genInitFun(suf baseFunName, base, aPCT, zeroSs,false)]
			      end
		      val initRepEDs = genInitEDs (initSuf o repSuf, rep, canonicalPCT)
                      val initPDEDs  = genInitEDs (initSuf o pdSuf,  pd, pdPCT)
                      fun genCleanupEDs(suf,base,aPCT) = case #memChar structProps
			  of TyProps.Static => [genInitFun(suf baseFunName, base, aPCT, [],true)]
			   | TyProps.Dynamic => 
			       let fun genInitFull {pty as PX.Name tyName: PX.Pty, args: pcexp list, 
						    name: string, isVirtual: bool, isEndian: bool,
						    isRecord, containsRecord, largeHeuristic: bool,
						    pred: pcexp option, comment: string option} = 
				   if not isVirtual then
				       if TyProps.Static = lookupMemChar pty then []
				       else let val baseFunName = lookupMemFun (PX.Name tyName)
					    in
					      [PT.Expr(
					        PT.Call(PT.Id (suf baseFunName),
							[PT.Id pdc, 
							 P.addrX(P.arrowX(
								       PT.Id base,
								       PT.Id name))]))]
					    end
				   else []
				   fun genInitBrief _ = []
				   fun genInitMan _ = [] (* should this be dependent up type (but C type...)?*)
				   val bodySs = mungeFields genInitFull genInitBrief genInitMan fields
			       in
				   [genInitFun(suf baseFunName, base, aPCT, bodySs,false)]
		               end
		      val cleanupRepEDs = genCleanupEDs (cleanupSuf o repSuf, rep, canonicalPCT)
                      val cleanupPDEDs  = genCleanupEDs (cleanupSuf o pdSuf,  pd, pdPCT)

                      (* Generate Copy Function struct case *)
                      fun genCopyEDs(suf, base, aPCT) = 
			  let val copyFunName = suf baseFunName
			      val dst = dstSuf base
			      val src = srcSuf base
			      val copySs = [PL.memcpyS(PT.Id dst, PT.Id src, P.sizeofX aPCT)]
			  in
			      case #memChar structProps
			      of TyProps.Static => [genCopyFun(copyFunName, dst, src, aPCT, copySs,true)]
			      |  TyProps.Dynamic => 
			         let fun genCopyFull {pty as PX.Name tyName: PX.Pty, args: pcexp list, 
						      name: string, isVirtual: bool, isEndian: bool, 
						      isRecord, containsRecord, largeHeuristic: bool,
						      pred: pcexp option, comment: string option} = 
				     let val nestedCopyFunName = suf (lookupMemFun pty)
				     in
				       if not isVirtual then
				         if TyProps.Static = lookupMemChar pty then []
				         else 
					      [PT.Expr(
					        PT.Call(PT.Id (nestedCopyFunName),
							[PT.Id pdc, 
							 getFieldX(dst, name),
							 getFieldX(src, name)]))]
				       else []
				     end
				     fun noop _ = []
				     val bodySs = mungeFields genCopyFull noop noop fields
				 in
				     [genCopyFun(copyFunName, dst, src, aPCT, copySs @ bodySs,false)]
				 end
			  end
		      val copyRepEDs = genCopyEDs(copySuf o repSuf, rep, canonicalPCT)
		      val copyPDEDs  = genCopyEDs(copySuf o pdSuf,  pd,  pdPCT)


                      (* Generate m_init function struct case *)
                      val maskInitName = maskInitSuf name 
                      val maskFunEDs = genMaskInitFun(maskInitName, mPCT)

                      (* Generate read function struct case *)
                      (* -- Some useful names *)
		      val readName = readSuf name

                      (* -- collection of expressions to be substituted for in constraints *)
                      (* -- efficiency could be improved with better representations *)
                      val subList : (string * pcexp) list ref = ref []
                      fun addSub (a : string * pcexp) = subList := (a:: (!subList))

                      (* -- Some helper functions *)
		      val first = ref true
		      fun genReadFull {pty: PX.Pty, args: pcexp list, name: string, 
				       isVirtual: bool, isEndian: bool, isRecord, containsRecord, largeHeuristic: bool,
				       pred:pcexp option, comment} = 
			  let val readFieldName = lookupTy(pty, readSuf, #readname)
                              val modEdNameX = fieldX(pd,name)
			      val repX = if isVirtual then PT.Id name else fieldX(rep,name)
			      val (locDecls, locX) = if isVirtual then ([P.varDeclS'(PL.locPCT,tloc)],PT.Id tloc) 
						    else ([], P.dotX(modEdNameX, PT.Id loc))
                              val () = if not isVirtual 
					   then addSub(name, fieldX(rep,name))  (* record additional binding *)
				       else ()
			      val modArgs = List.map (PTSub.substExps (!subList)) args
                              val () = checkParamTys(name, readFieldName, modArgs, 2, 2)
			      val comment = ("Reading field: "^ name ^ 
					     (if isEndian then ". Doing endian check." else "."))
			      val commentS = P.mkCommentS (comment)
			      val ifPanicSs = 
				  PT.Compound
                                   [PL.setPanicS(P.addrX(modEdNameX)),
				    P.assignS(P.dotX(modEdNameX, PT.Id errCode),PL.PDC_PANIC_SKIPPED),  
                                    PL.getLocS(PT.Id pdc, P.addrX locX),
				    P.plusAssignS(fieldX (pd,nerr),P.intX 1)]
			      val ifNoPanicSs =
                                  PT.Compound ([
				   PL.getLocBeginS(PT.Id pdc, P.addrX locX),
                                   PT.IfThenElse
                                    (P.eqX(PL.PDC_ERROR,
					   PL.readFunX(readFieldName, 
						       PT.Id pdc, 
						       P.addrX(fieldX(m,name)),
						       modArgs,
						       P.addrX(fieldX(pd,name)),
						       P.addrX repX)),
				     PT.Compound( (* error reading field *)
				      [PT.IfThen(PL.getSpecLevelX(PT.Id pdc),
						 PT.Compound[PT.Return PL.PDC_ERROR]),
                                       PT.IfThen(PL.testPanicX(P.addrX(fieldX(pd, name))),
				                 PT.Compound[PL.setPanicS(PT.Id pd)])]
				      @reportStructErrorSs(PL.PDC_STRUCT_FIELD_ERR, true, locX)),
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
						     [P.assignS(P.dotX(fieldX(pd,name), PT.Id errCode), 
								PL.PDC_USER_CONSTRAINT_VIOLATION),
						      PL.getLocEndS(PT.Id pdc, P.addrX(locX), ~1)]
						   @ reportStructErrorSs(PL.PDC_STRUCT_FIELD_ERR, false,locX)
						   @ [PL.userErrorS(PT.Id pdc,
								    P.addrX(locX),
								    P.dotX(fieldX(pd,name), PT.Id errCode),
								    readName,
								    PT.String("User constraint on field "^
									      name ^ " " ^
									      "violated."), [])]
					       fun swap reportErrSs = 
                                                   [PL.swapBytesS(fieldX(rep, name)),
						    PT.IfThenElse(
						       exp,
                                                       PT.Compound
							[P.assignS(d_endianX,
								   P.condX(P.eqX(d_endianX,PL.bigEndian),
									   PL.littleEndian,
									   PL.bigEndian)),
							 PL.userInfoS(PT.Id pdc, 
							    P.addrX(locX), 
							    readName,
							    PT.String ("New data endian value: "^
								       "%s.  Machine endian value: %s "^
								       "(from "^name^" field test)."),
							    [PL.end2StringX d_endianX,
							     PL.end2StringX m_endianX])],
						       PT.Compound
							([PL.swapBytesS(fieldX(rep, name)),
							  PT.IfThen(PL.getSpecLevelX(PT.Id pdc),
								    PT.Compound[PT.Return PL.PDC_ERROR])]
							 @ reportErrSs))]
					   in
					       [PT.IfThen(
                                                 P.andX(PL.mTestSemCheckX(getEMExp(fieldX(m,name))),
							P.notX exp),
						 PT.Compound
					           (if isEndian then
						     swap reportErrSs
						    else reportErrSs))]
					   end
                                      (* end case pred *) ) )])
			      fun addLocDecl s = PT.Compound (locDecls @ s)
			      val readS = if !first then (first:= false; addLocDecl [ifNoPanicSs])
					  else addLocDecl(
					       [PT.IfThenElse(PL.testPanicX(PT.Id pd), ifPanicSs, ifNoPanicSs)])
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
				  if CTisIntorChar expTy then (PL.charlit, [], e, getCharComment e)
				  else if CTisString expTy 
				       then (PL.strlit,
					     [P.varDeclS(PL.stringPCT, "strlit", 
							 PT.InitList[e, P.zero]),
					      P.assignS(P.dotX(PT.Id "strlit", PT.Id "len"), getStrLen e)],
					     P.addrX(PT.Id "strlit"), CExptoString expAst)
				  else (PE.error ("Currently only characters and strings "^
					          "supported as delimiters. Delimiter type: "^ (CTtoString expTy) ^".");
					(PL.charlit, [], e, CExptoString expAst))
			      val commentS = P.mkCommentS ("Reading delimiter field: "^
						           commentV)
                              (* base_pd tpd; *)
			      val tpdDecl = P.varDeclS'(PL.base_pdPCT, tpd)
			      val offsetDecl = P.varDeclS'(PL.sizePCT, "n")

			      val scanFieldName = scanSuf pTyName

			      fun genPanicRecovery (pTyName:string) : pcstmt list -> pcstmt list = 
                                        fn elseSs =>
                                           [PT.IfThenElse((* if (PDC_PS_isPanic(pd) *)
                                              PL.testPanicX(PT.Id pd),
					      PT.Compound [
                                                (* base_m tmask = Ignore; *)
						PT.IfThen(
						 P.neqX(PL.PDC_ERROR,
						       PL.scanFunX(scanFieldName, PT.Id pdc, 
								   expr, expr, P.trueX,
								   P.zero,
                                                                   P.addrX (PT.Id "n"))),
						 (* PDC_PS_unsetPanic(pd) *)
						 PT.Compound[PL.unsetPanicS(PT.Id pd)])
                                              ], 
                                              PT.Compound elseSs
                                           )]

			      val readFieldName = lookupTy(PX.Name pTyName, readSuf, #readname)
			      fun reportBriefErrorSs (code, msg, offset) = 
				  let val locX = P.dotX(PT.Id tpd, PT.Id loc)
				  in
				   [PT.IfThen(
				      PL.getSpecLevelX(PT.Id pdc),
				      PT.Compound[PT.Return PL.PDC_ERROR]),
				    PT.IfThen(
				      P.eqX(P.zero, fieldX(pd,nerr)), 
				      PT.Compound 
				       [P.assignS(fieldX(pd, errCode), code),
					PL.getLocEndS(PT.Id pdc, P.addrX(locX), offset),
					P.assignS(fieldX(pd, loc), locX),
					PL.userErrorS(PT.Id pdc, 
						      P.addrX(P.dotX(PT.Id tpd, PT.Id loc)),
						      code,
						      readName,
						      PT.String (msg^": %s."), 
						      [PL.fmtStr(commentV)])]),
				    P.plusAssignS(fieldX(pd,nerr), P.intX 1)]
				  end
			      val notPanicSs = 
				  [PL.getLocBeginS(PT.Id pdc, P.addrX(P.dotX(PT.Id tpd, PT.Id loc))),
				   PT.IfThenElse(
				      P.eqX(PL.PDC_OK,
					    PL.scanFunX(scanFieldName, 
							PT.Id pdc, expr, expr, P.trueX,
							P.zero, P.addrX (PT.Id "n"))),
				      PT.Compound(
					 [PT.IfThen(
                                           P.gtX(PT.Id "n", P.zero),
					   PT.Compound(
						  reportBriefErrorSs (PL.PDC_STRUCT_EXTRA_BEFORE_SEP, 
						                     "Extra data before separator", ~2)))]),
				      PT.Compound(reportBriefErrorSs (PL.PDC_MISSING_LITERAL,
						                       "Missing literal", ~1)
                                                  @[PL.setPanicS(PT.Id pd)]))]

			  in
			      [PT.Compound(
                                   [commentS, 
				    PT.Compound(
				     tpdDecl 
				     :: offsetDecl
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

		     fun genCheckPostConstraint postCon = 
			 case postCon of NONE => ([],[])  (* get begin loc for struct, check constraint*)
                         | SOME expr => (
                            let val strLocD = P.varDeclS'(PL.locPCT, tloc)
				val locX = PT.Id tloc
				val getBeginLocS = PL.getLocBeginS(PT.Id pdc, P.addrX locX)
				val getEndLocSs = [PL.getLocEndS(PT.Id pdc, P.addrX locX, ~1)]
				val expr = PTSub.substExps (!subList) expr
				val () = expEqualTy(expr, CTintTys, 
						    fn s=> ("Post condition for pstruct "^
							    name ^ " " ^
							    "does not have integer type."))
				val reportErrSs = 
				       getEndLocSs
				     @ reportStructErrorSs(PL.PDC_USER_CONSTRAINT_VIOLATION, false, locX)
				     @ [PL.userErrorS(PT.Id pdc,
						      P.addrX(locX),
						      fieldX(pd,errCode),
						      readName,
						      PT.String("Post condition for pstruct "^
								name ^ " " ^
								"violated."), [])]
				val condSs = 
				       [P.mkCommentS ("Checking post constraint for pstruct "^ name ^".\n"),
					PT.IfThen(
                                           P.andX( PL.mTestSemCheckX(fieldX(m,all)), P.notX expr),
					   PT.Compound reportErrSs)]
			    in
			       ([strLocD,getBeginLocS], condSs)
			    end (* end some case *))
				 
			     

                      (* -- Assemble read function *)
		      val _ = pushLocalEnv()                                        (* create new scope *)
		      val () = ignore (insTempVar(rep, P.ptrPCT canonicalPCT))      (* add rep to scope *)
		      val () = ignore (insTempVar(m,  P.ptrPCT mPCT))               (* add m to scope *)
		      val () = ignore(List.map insTempVar localVars)                (* insert virtuals into scope *)
                      val () = ignore (List.map insTempVar cParams)  (* add params for type checking *)
		      val readFields = mungeFields genReadFull genReadBrief genReadMan fields  
		                                                                    (* does type checking *)
		      val (postLocSs, postCondSs) = genCheckPostConstraint postCond
                      val readRecord = if isRecord  then genReadEOR (readName, reportStructErrorSs) () else []
		      val _ = popLocalEnv()                                         (* remove scope *)
		      val localDeclSs = List.map (P.varDeclS' o (fn(x,y) => (y,x))) localVars
		      val bodyS = localDeclSs @ postLocSs @ readFields @ postCondSs @ readRecord
		      val bodySs = if 0 = List.length localDeclSs andalso not (Option.isSome postCond)
				       then bodyS else [PT.Compound bodyS]
		      val returnS = genReturnChk (P.arrowX(PT.Id pd, PT.Id nerr))
		      val bodySs = bodySs @ [returnS]

		      val readFunEDs = genReadFun(readName, cParams, mPCT,pdPCT,canonicalPCT, 
						  mFirstPCT, true, bodySs)

                      val readEDs = initRepEDs @ initPDEDs @ cleanupRepEDs @ cleanupPDEDs
			          @ copyRepEDs @ copyPDEDs @ maskFunEDs @ readFunEDs


                      (* Generate Accumulator functions struct case *)
                      (* -- generate accumulator init, reset, cleanup, and report functions *)
		      fun genResetInitCleanup theSuf = 
			  let val theFun = (theSuf o accSuf) name
			      val auxFields = chk3Pfun(theSuf PL.intAct, getFieldX(acc,nerr))
			      fun genAccTheFull {pty: PX.Pty, args: pcexp list, name: string, 
						 isVirtual: bool, isEndian: bool, 
						 isRecord, containsRecord, largeHeuristic: bool,
						 pred: pcexp option, comment} = 
				  if not isVirtual then
				      case lookupAcc(pty) of NONE => []
				    | SOME a => cnvPtyMan(theSuf a,acc,name)
				  else []
			      fun genAccTheBrief e = []

			      val theDeclSs = [P.varDeclS(PL.uint32, nerr, P.zero)]
			      val theFields = mungeFields genAccTheFull genAccTheBrief (genAccTheMan theSuf) fields
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
                      (*  PDC_error_t T_acc_add (PDC_t* , T_acc* , T_pd*, T* ,) *)
		      val addFun = (addSuf o accSuf) name
		      val addDeclSs = [P.varDeclS(PL.uint32, nerr, P.zero),  P.varDeclS'(PL.base_pdPCT, tpd)]
		      val initTpdSs = [P.assignS(P.dotX(PT.Id tpd, PT.Id errCode), PL.PDC_NO_ERROR)]

		      fun genAccAddFull {pty: PX.Pty, args: pcexp list, name: string, 
					 isVirtual: bool, isEndian: bool, 
					 isRecord, containsRecord, largeHeuristic: bool,
					 pred: pcexp option, comment} = 
			  if not isVirtual then cnvPtyForAdd(pty,name, getFieldX(pd,name)) else []
                      fun genAccAddBrief e = []
		      fun genAccAddMan {decl, comment:string option} = 
			  let val ctNoptEs = cnvDeclaration decl
			      fun doOne(cty,nameOpt,exp) = 
				  let val name= case nameOpt of NONE => "bogus" | SOME n => n
				      val accPtyOpt = reverseLookup cty
				  in
				      case accPtyOpt of NONE => [] 
				    | SOME pty => (
 				       [PT.Compound(
					 [P.varDeclS'(P.makeTypedefPCT(lookupTy(pty, pdSuf, #pdname)), tpd),
					  P.assignS(P.dotX(PT.Id tpd,PT.Id errCode),
						    P.arrowX(PT.Id pd, PT.Id errCode))]
						    @ cnvPtyForAdd(pty,name, P.addrX(PT.Id tpd)))]
				  (* end case *))
				  end
			  in
			      List.concat(List.map doOne ctNoptEs)
			  end

		      val addNErrSs = chkAddFun(addSuf PL.intAct, getFieldX(acc,nerr), 
						P.addrX(PT.Id tpd),
						PT.Cast(P.ptrPCT PL.intPCT, getFieldX(pd,nerr)))

		      val addFields = mungeFields genAccAddFull genAccAddBrief genAccAddMan fields
		      val addReturnS = genReturnChk (PT.Id nerr)
                      val addBodySs = addDeclSs @ initTpdSs @ addNErrSs @ addFields @ [addReturnS]
                      val addFunED = genAddFun(addFun, accPCT, pdPCT, canonicalPCT, addBodySs)

                      (* -- generate report function pstruct *)
                      (*  PDC_error_t T_acc_report (PDC_t* , T_acc* , const char* prefix , ) *)
		      val reportFun = (reportSuf o accSuf) name

		      val reportNerrSs = [chkPrint(
 				         PL.errAccReport(PT.Id pdc, PT.Id outstr, PT.String "Errors", 
							 PT.String "errors", P.intX ~1, getFieldX(acc,nerr))) ]
		      val headerSs = [PL.sfprintf(PT.Id outstr, 
						  PT.String "\n[Describing each field of %s]\n", 
						  [PT.Id prefix])]

		      fun genAccReportFull {pty: PX.Pty, args: pcexp list, name: string, 
					    isVirtual: bool, isEndian: bool, 
					    isRecord, containsRecord, largeHeuristic: bool,
					    pred: pcexp option, comment} = 
			  if not isVirtual then cnvPtyForReport(reportSuf, ioSuf, pty,name)
			  else []
                      fun genAccReportBrief e = []
		      val reportFields = (mungeFields genAccReportFull genAccReportBrief 
			                  (genAccReportMan (reportSuf, ioSuf)) fields)
                      val reportFunEDs = genReportFuns(reportFun, "struct "^name, accPCT, 
						        reportNerrSs @ headerSs @ reportFields)

		      val accumEDs = accED :: initFunED :: resetFunED :: cleanupFunED :: addFunED :: reportFunEDs

                      (* Generate Write function struct case *)
		      val writeName = writeSuf name
		      fun getLastField fs = 
			  let fun f'([], s) = s
                                | f'(f::fs, s) = 
			             (case f of PX.Full{name,...} => f'(fs, SOME f)
					      | PX.Brief _ => f'(fs, SOME f)
			                      | PX.Manifest _ => f'(fs, s) (* end case *))
			  in
			      f' (fs, NONE)
			  end
		      fun matchesLast (f, NONE) = false
                        | matchesLast (PX.Full f, SOME (PX.Full f')) = #name f = #name f'
                        | matchesLast (f as PX.Brief e, f' as SOME (PX.Brief e')) = false
                        | matchesLast _ = false

		      val lastField = getLastField fields
                      val () = subList := [] (* reset substitution list *)
		      fun genWriteFull (f as {pty: PX.Pty, args: pcexp list, name: string, 
					      isVirtual: bool, isEndian: bool, 
  					      isRecord=_, containsRecord, largeHeuristic: bool,
					      pred: pcexp option, comment}) = 
			  if isVirtual then [] (* have no rep of virtual (omitted) fields, so can't print *)
                          else
			    let val writeFieldName = (bufSuf o writeSuf) (lookupWrite pty) 
				val () = addSub(name, fieldX(rep,name))
				val modArgs = List.map(PTSub.substExps (!subList)) args
				val adjustLengths = isRecord orelse  not (matchesLast(PX.Full f, lastField))
			    in
				writeFieldSs(writeFieldName, modArgs @[getFieldX(pd,name), 
								       getFieldX(rep,name)], adjustLengths)
			    end
		      fun genWriteBrief e = 
			  let val e = PTSub.substExps (!subList) e
			      val (expTy, expAst) = cnvExpression e
			      val pTyName = if CTisIntorChar expTy then PL.charlit else PL.strlitWrite
			      val writeFieldName = lookupLitWrite pTyName
			      val adjustLengths = isRecord orelse not(matchesLast(PX.Brief e, lastField))
			      val writeFieldSs = writeFieldSs(writeFieldName,[e], adjustLengths)
			  in
			      writeFieldSs
			  end
		      fun genWriteOther _ = []     (* Manifest fields do not need to be written *)
		      val _ = pushLocalEnv()       (* We convert literals to determine which write function to use*)
		      val cParams : (string * pcty) list = List.map mungeParam params (* so we have to add params to scope *)
                      val () = ignore (List.map insTempVar cParams)  (* add params for type checking *)
		      val writeFieldsSs = mungeFields genWriteFull genWriteBrief genWriteOther fields
		      val _ = popLocalEnv()                                         (* remove scope *)
		      val bodySs = writeFieldsSs 
                      val writeFunEDs = genWriteFuns(writeName, isRecord, cParams, pdPCT, canonicalPCT, bodySs)


		      (***** struct PADS-Galax *****)

		      fun genFieldFull {pty: PX.Pty, args: pcexp list, name: string, isVirtual: bool, 
				      isEndian: bool, isRecord, containsRecord, largeHeuristic: bool,
				      pred: pcexp option, comment: string option} = 
			  [(name, lookupTy (pty,repSuf,#repname))]
		      fun genFieldBrief e = []
		      fun genFieldMan m = []
		      val localFields = mungeFields genFieldFull genFieldBrief genFieldMan fields

		      (* counting Full and Computed fields 
		      fun isFullorManif (PX.Full f) = 1
			| isFullorManif (PX.Manifest f) = 1
		        | isFullorManif _ = 0
		      fun plus (a,b) = a+b
		      fun countFields fs = List.foldr plus 1 (List.map isFullorManif fs)
		      *)

		      fun countFieldFull f = [1]
		      fun countFieldMan m = []
		      fun countFieldBrief e = [1]
		      val countFields = List.length (mungeFields countFieldFull countFieldMan countFieldBrief fields) 

    	              (* PDCI_node_t** fooStruct_children(PDCI_node_t *self) *)
		      fun genGalaxStructChildrenFun(name,fields) =		
		          let val nodeRepTy = PL.nodeT
                              val returnName = PT.Id result
			      val returnTy = P.ptrPCT (P.ptrPCT (nodeRepTy))
                              val cnvName = childrenSuf name 
                              val paramNames = [self]
                              val paramTys = [P.ptrPCT nodeRepTy]
                              val formalParams =  List.map P.mkParam(ListPair.zip(paramTys, paramNames))
		              fun macroNode (n,(nameField,tyField)) 
					= macroNodeCall(returnName,P.intX n,tyField,nameField,
							getFieldX(m,nameField),getFieldX(pd,nameField),
      							getFieldX(rep,nameField),cnvName)
			      val numChildren = countFields + 1
 		              val bodySs = headerGalaxChildrenFun(name) @
					   ifGalaxChildren(returnName,P.intX numChildren, "ALLOC_ERROR: in " ^ cnvName) @
					   macroTNode(returnName,PL.PDCI_structured_pd,pd,PT.Id pd,cnvName) @
				 	   (List.map macroNode (enumerate localFields)) @
					   [P.returnS (returnName)]
                          in   
                            P.mkFunctionEDecl(cnvName, formalParams, PT.Compound bodySs, returnTy)
                          end

		      val galaxEDs = [genGalaxStructChildrenFun(name,fields),
		                      genGalaxVtable(name)] 


	      in 
 		   canonicalDecls (* converted earlier because used in typechecking constraints *)
                 @ mDecls
                 @ pdDecls
	         @ (emitRead readEDs)
                 @ (emitAccum accumEDs)
                 @ (emitWrite writeFunEDs)
  		 @ (emitXML galaxEDs)
	      end

	     fun cnvPUnion ({name: string, params: (pcty * pcdecr) list, 
			     isRecord: bool, containsRecord, largeHeuristic, isFile: bool, 
			     variants: (pdty, pcdecr, pcexp) PX.PBranches}) = 
		 let (* Some useful names *)
		     val unionName = name
		     val cParams : (string * pcty) list = List.map mungeParam params
		     val paramNames = #1(ListPair.unzip cParams)
                     val value = PNames.unionVal
		     val tag = PNames.unionTag
		     fun tgSuf s = s^"_tag"
		     fun unSuf s = s^"_u"
                     fun unionBranchX (base, name) = P.addrX(P.dotX(P.arrowX(PT.Id base, PT.Id value), PT.Id name))

		     (* Functions for walking over list of variants *)
		     fun mungeVariant f b m (PX.Full fd) = f fd
		       | mungeVariant f b m (PX.Brief e) = b e
		       | mungeVariant f b m (PX.Manifest md)  = m md
		     fun mungeVariants f b m [] = []
		       | mungeVariants f b m (x::xs) = (mungeVariant f b m x) @ (mungeVariants f b m xs)

		     (* Functions for walking over list of branch,variant *)
		     fun mungeBV f b m eopt (PX.Full fd) = f (eopt, fd)
		       | mungeBV f b m eopt (PX.Brief e) = b (eopt, e)
		       | mungeBV f b m eopt (PX.Manifest md)  = m (eopt, md)
		     fun mungeBVs f b m [] [] = []
		       | mungeBVs f b m (x::xs) (y::ys) = (mungeBV f b m x y) @ (mungeBVs f b m xs ys)
		       | mungeBVs f b m _ _ = raise Fail "This case should never happen"

                     (* Function for moving default clause to end of switched union branch list *)
                     fun mungeBranches (cases, branches) = 
                       let fun mB([],[], acs, abs, acds, abds) = 
			       let val numDefaults = List.length acds
			       in
			          if (numDefaults >= 2) 
				      then (PE.error ("Switched union "^ unionName ^" can have at most "^
						      "one default clause.\n");
					    (true (* has default clause*), 
					     (List.rev acs)@[hd acds], (List.rev abs)@[hd abds]))
				  else (numDefaults = 1, (List.rev acs)@acds, (List.rev abs)@abds)
			       end
                             | mB(NONE::cases, b::bs, acs,abs,acds,abds) = 
					    mB(cases,bs, acs, abs, NONE::acds, b::abds)
                             | mB(c::cases, b::bs, acs,abs,acds,abds) = 
					    mB(cases,bs, c::acs, b::abs, acds, abds)
                             | mB _ = raise Fail "This case can't happen."
		       in
			   mB(cases,branches,[],[],[],[])
		       end


                     val branches = variants
                     val (descOpt, hasDefault, cases, variants) = 
				    case branches 
			            of PX.Ordered v => (NONE, false (* no default clause *), [], v)
			            |  PX.Switched {descriminator, cases, branches} => 
					    let val (hasDefault, cases,branches) = mungeBranches(cases,branches)
					    in
						(SOME descriminator, hasDefault, cases, branches)
					    end

		      (* Union: Error checking *)
		      fun checkFull {pty: PX.Pty, args: pcexp list, name: string, isVirtual: bool, 
				     isEndian: bool, isRecord, containsRecord, largeHeuristic: bool,
				     pred: pcexp option, comment: string option} = 
			  (if name = "pd" then PE.error ("Punion "^ unionName ^" contains variant with reserved name 'pd'.\n") 
			   else (); 
			   let val ty = P.makeTypedefPCT(lookupTy(pty, repSuf, #repname))
			   in
			       CTcnvType ty  (* ensure that the type has been defined *)
			   end; [])
		      fun checkBrief e = []
		      fun checkMan m = []
		      val _ = mungeVariants checkFull checkBrief checkMan variants



                     (* generate enumerated type describing tags *)
		     val tagVal = ref 0
		     val firstTag = ref "bogus"
		     fun chkTag(name) = 			 
			 (if !tagVal = 0 then firstTag := name else ();
			  tagVal := !tagVal + 1;
			  [(name,P.intX(!tagVal),NONE)])

		     fun genTagFull {pty: PX.Pty, args: pcexp list, name: string, 
				     isVirtual: bool, isEndian: bool, 
				     isRecord, containsRecord, largeHeuristic: bool,
				     pred: pcexp option, comment: string option} = 
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

		      (* Generate CheckSet mask *)
		     fun genMFull {pty: PX.Pty, args: pcexp list, name: string, 
				    isVirtual: bool, isEndian: bool, 
                                    isRecord, containsRecord, largeHeuristic: bool,
				    pred: pcexp option, comment} = 
			 [(name,P.makeTypedefPCT(lookupTy (pty,mSuf,#mname)), NONE)]
		     fun genMBrief e = []
		     fun genMMan m = []
		     val mFields = mungeVariants genMFull genMBrief genMMan variants
		     val mFirstPCT = getFirstEMPCT mFields
		     val mStructED = P.makeTyDefStructEDecl (mFields, mSuf name)
		     val mPCT = P.makeTypedefPCT (mSuf name)			  

		     (* Generate parse description *)
		     fun genEDFull {pty: PX.Pty, args: pcexp list, name: string, 
				    isVirtual: bool, isEndian: bool,
				    isRecord, containsRecord, largeHeuristic: bool,
				    pred: pcexp option, comment} = 
			 [(name,P.makeTypedefPCT(lookupTy (pty,pdSuf,#pdname)),NONE)]
		     fun genEDBrief e = []
		     val pdVariants = mungeVariants genEDFull genEDBrief genEDMan variants
		     val unionPD = P.makeTyDefUnionEDecl(pdVariants, (unSuf o pdSuf) name)
		     val (unionPDDecls, updTid) = cnvCTy unionPD
		     val unionPDPCT = P.makeTypedefPCT((unSuf o pdSuf) name)
		     val structEDFields = [(pstate, PL.flags_t, NONE), (errCode, PL.errCodePCT, NONE),
				  	   (loc, PL.locPCT, NONE), (nerr, PL.uint32, NONE),
					   (tag, tagPCT, NONE), (value, unionPDPCT, NONE)]
		     val pdStructED = P.makeTyDefStructEDecl (structEDFields, pdSuf name)
		     val (pdStructPDDecls, pdTid) = cnvCTy pdStructED
		     val pdPCT = P.makeTypedefPCT (pdSuf name)			  

		     (* Generate accumulator type *)
		     fun genAccFull {pty: PX.Pty, args: pcexp list, name: string, 
				     isVirtual: bool, isEndian: bool, 
				     isRecord, containsRecord, largeHeuristic: bool, 
				     pred: pcexp option, comment} = 
			 case lookupAcc pty of NONE => []
			 | SOME a => [(name,P.makeTypedefPCT a,NONE)]
		     fun genAccBrief e = []
		     val auxAccFields = [(tag, PL.intAccPCT, NONE)]
		     val accFields = auxAccFields @ (mungeVariants genAccFull genAccBrief genAccMan variants)
		     val accED = P.makeTyDefStructEDecl (accFields, accSuf name)
		     val accPCT = P.makeTypedefPCT (accSuf name)			  

		     (* Calculate and insert type properties into type table *)
		     fun genTyPropsFull {pty: PX.Pty, args: pcexp list, name: string, 
					 isVirtual: bool, isEndian: bool, 
					 isRecord, containsRecord, largeHeuristic: bool,
					 pred: pcexp option, comment: string option} = 
			  let val PX.Name ftyName = pty
			      val mc = lookupMemChar pty
			      val ds = lookupDiskSize pty
			      val contR = lookupContainsRecord pty	 
			      val lH = lookupHeuristic pty 
			      val () = if isVirtual 
				       then PE.error ("Omitted fields not supported in punions ("^name ^"). ")
				       else ()
			  in [{diskSize=ds, memChar=mc, endian=false, isRecord=isRecord, 
			       containsRecord=contR, largeHeuristic=lH, labels = [SOME (name,ftyName,(paramNames,args))]}] 
			  end
		     fun genTyPropsBrief e = [] (* not used in unions *)
		     val tyProps = mungeVariants genTyPropsFull genTyPropsBrief genTyPropsMan variants
                     (* check that all variants are records if any are *)
		     val () = case tyProps of [] => ()
			      | ({isRecord=first,...}::xs) => 
			           (if List.exists (fn {isRecord,diskSize,memChar,endian,
							containsRecord,largeHeuristic,labels} => not (isRecord = first)) xs 
				    then PE.error "All branches of union must terminate record if any branch does."
				    else ())
					
		     fun mUnion (x,y) = if (x = y) then TyProps.Size x else TyProps.Variable
		     val {diskSize,memChar,endian,isRecord=_,containsRecord,largeHeuristic, labels} = 
			 List.foldr (PTys.mergeTyInfo (TyProps.mergeDiskSize mUnion)) PTys.minTyInfo tyProps
		     val numArgs = List.length params
		     val unionProps = buildTyProps(name, PTys.Union, diskSize, TyProps.Union [], memChar, 
						   endian, isRecord, containsRecord, largeHeuristic, isFile, pdTid, numArgs)
                     val () = PTys.insert(Atom.atom name, unionProps)

                     (* union: generate canonical representation *)
		     fun genRepFull {pty: PX.Pty, args: pcexp list, name: string, 
				     isVirtual: bool, isEndian: bool, isRecord, containsRecord, largeHeuristic: bool,
				     pred: pcexp option, comment: string option} = 
			 let val predStringOpt = Option.map P.expToString pred
			     val fullCommentOpt = stringOptMerge(comment, predStringOpt)
			 in
			     [(name,P.makeTypedefPCT(lookupTy (pty,repSuf,#repname)), fullCommentOpt )]
			 end
		     fun genRepBrief e = (PE.error "Unions do not currently support brief fields.\n"; [])
		     val canonicalVariants = mungeVariants genRepFull genRepBrief genRepMan variants
		     val unionPD = P.makeTyDefUnionEDecl(canonicalVariants, unSuf name)
		     val unionDecls = cnvExternalDecl unionPD
                     val unionPCT = P.makeTypedefPCT(unSuf name)
                     val structFields = [(tag, tagPCT, NONE),
					 (value, unionPCT, NONE)]
		     val canonicalStructED = P.makeTyDefStructEDecl (structFields, repSuf name)
		     val (canonicalDecls, canonicalTid) = cnvRep(canonicalStructED, valOf (PTys.find (Atom.atom name)))
                     val canonicalPCT = P.makeTypedefPCT (repSuf name)			 

                     (* Generate tag to string function *)
		     val toStringEDs = [genEnumToStringFun(tgSuf name, tagPCT, tagFields)]

                      (* Generate m_init function union case *)
                      val maskInitName = maskInitSuf name 
                      val maskFunEDs = genMaskInitFun(maskInitName, mPCT)

                      (* Generate init function, union case *)
                      val baseFunName = lookupMemFun (PX.Name name)
                      fun genInitEDs (suf, var, varPCT) = 
			  case #memChar unionProps
			  of TyProps.Static => 
			      [genInitFun(suf baseFunName, var, varPCT, [],true)]
			   | TyProps.Dynamic => 
			       let val zeroSs = [PL.bzeroS(PT.Id var, P.sizeofX(varPCT))]
			       in
				   [genInitFun(suf baseFunName, var, varPCT, zeroSs,false)]
		               end
                      val initRepEDs = genInitEDs (initSuf, rep, canonicalPCT)
		      val initPDEDs = genInitEDs((initSuf o pdSuf), pd, pdPCT)

                      (* Generate cleanup function, union case *)
		      fun genCleanupEDs (suf, var, varPCT) = 
			  case #memChar unionProps
			  of TyProps.Static => 
			      [genInitFun(suf baseFunName,var,varPCT,[],true)]
			   | TyProps.Dynamic => 
			       let fun genCleanupFull {pty as PX.Name tyName :PX.Pty, args : pcexp list, 
						    name:string, isVirtual:bool, isEndian:bool,isRecord,containsRecord,largeHeuristic:bool,
						    pred:pcexp option, comment:string option} = 
				    if TyProps.Static = lookupMemChar pty then []
				    else let val baseFunName = lookupMemFun (PX.Name tyName)
					 in [PT.CaseLabel(PT.Id name,
					      PT.Compound[
					       PT.Expr(
					           PT.Call(PT.Id (suf baseFunName),
							   [PT.Id pdc, 
							    unionBranchX(var,name)])), 
					       PT.Break])]
					 end
				   fun genCleanupBrief _ = []
				   fun genCleanupMan _ = []
				   val branchSs = mungeVariants genCleanupFull 
				                   genCleanupBrief genCleanupMan variants
				   val allBranchSs = branchSs @ [PT.DefaultLabel PT.Break]
				   val bodySs = [PT.Switch(P.arrowX(PT.Id var, PT.Id tag), PT.Compound allBranchSs)]
			       in
				   [genInitFun(suf baseFunName, var, varPCT, bodySs, false)]
		               end
			   
		      val cleanupRepEDs = genCleanupEDs(cleanupSuf, rep, canonicalPCT)
		      val cleanupPDEDs = genCleanupEDs(cleanupSuf o pdSuf, pd, pdPCT)

                      (* Generate Copy Function union case *)
                      fun genCopyEDs(suf, base, aPCT) = 
			  let val copyFunName = suf baseFunName
			      val dst = dstSuf base
			      val src = srcSuf base
			      val copySs = [PL.memcpyS(PT.Id dst, PT.Id src, P.sizeofX aPCT)]
			  in
			      case #memChar unionProps
			      of TyProps.Static => [genCopyFun(copyFunName, dst, src, aPCT, copySs,true)]
			      |  TyProps.Dynamic => 
			         let fun genCopyFull {pty as PX.Name tyName :PX.Pty, args : pcexp list, 
						      name:string, isVirtual:bool, isEndian:bool,isRecord,containsRecord,largeHeuristic:bool,
						      pred:pcexp option, comment:string option} = 
				     let val nestedCopyFunName = suf (lookupMemFun pty)
				     in
				       if TyProps.Static = lookupMemChar pty then []
				       else 
					 [PT.CaseLabel(PT.Id name,
					   PT.Compound[
					       PT.Expr(
						  PT.Call(PT.Id (nestedCopyFunName),
							[PT.Id pdc, 
							 unionBranchX(dst, name),
							 unionBranchX(src, name)])),
					       PT.Break])]
				     end
				     fun noop _ = []
				     val branchSs = mungeVariants genCopyFull noop noop variants
				     val branchSs = branchSs @ [PT.DefaultLabel PT.Break]
				     val bodySs = [PT.Switch (P.arrowX(PT.Id src, PT.Id tag), PT.Compound branchSs)]
				 in
				       [genCopyFun(copyFunName, dst, src, aPCT, copySs @ bodySs,false)]
				 end
			  end
		      val copyRepEDs = genCopyEDs(copySuf o repSuf, rep, canonicalPCT)
		      val copyPDEDs  = genCopyEDs(copySuf o pdSuf,  pd,  pdPCT)

                     (* Generate read function *)
                     (* -- Some useful names *)
		     val readName = readSuf name

                     (* -- some helper functions *)
		     val initSpaceSs = [PT.Expr(PT.Call(PT.Id (initSuf unionName), 
					  [PT.Id pdc, PT.Id rep])),
					PT.Expr(PT.Call(PT.Id ((initSuf o pdSuf) unionName), 
					  [PT.Id pdc, PT.Id  pd]))]
		     val cleanupSpaceSs =
			 [PT.Expr(PT.Call(PT.Id (cleanupSuf unionName), 
					  [PT.Id pdc, PT.Id rep])),
			  PT.Expr(PT.Call(PT.Id ((cleanupSuf o pdSuf) unionName), 
					  [PT.Id pdc, PT.Id pd]))]

		     val deallocOldSpaceSs = (* optimization for reusing if space if
					      hits first tag again *)
			 case #memChar unionProps of TyProps.Static => []
		       | TyProps.Dynamic => 
				 [PT.IfThen(
					    P.neqX(fieldX(rep,tag), PT.Id (!firstTag)),
					    PT.Compound (cleanupSpaceSs @ initSpaceSs))]

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
                                         P.andX(PL.mTestSemCheckX(fieldX(m,name)),
						P.notX predX),
					   notFoundSs,
					   foundSs
					  )]
				 end

                     fun readVariant(pred,name,args,readFieldName,foundSs,notFoundSs) = 
			 let val constraintChkS = doConstraint(pred,name,foundSs,notFoundSs)
			 in
			     [P.assignS(fieldX(rep,tag),PT.Id name),
			      P.assignS(fieldX(pd,tag),PT.Id name),
			      PT.IfThenElse(
				 PL.readFunChkX(
				     PL.PDC_ERROR, readFieldName, PT.Id pdc, 
				     P.addrX(fieldX(m,name)), args, 
				     P.addrX(P.dotX(fieldX(pd,value), PT.Id name)),
				     P.addrX(P.dotX(fieldX(rep,value), PT.Id name))),
				 notFoundSs,
				 constraintChkS)]
			 end

                     fun genReadFull{pty :PX.Pty, args:pcexp list, name:string, 
				     isVirtual:bool, isEndian:bool,isRecord,containsRecord,largeHeuristic:bool, 
				     pred:pcexp option, comment} = 
			  let val readFieldName = lookupTy(pty, readSuf, #readname)
                              val () = checkParamTys(name, readFieldName, args, 2, 2)
			      val commentS = P.mkCommentS ("Reading variant "^name^".")
			      val mc = lookupMemChar pty
			      val initSpaceSs = if name = !firstTag orelse mc = TyProps.Static then [] else initSpaceSs
			      val cleanupSpaceSs = if mc = TyProps.Static then [] else cleanupSpaceSs
			      val foundSs = PT.Compound(
					     PL.commitS(PT.Id pdc, readName)
					     @returnSs)
			      val notFoundSs = PT.Compound(PL.restoreS(PT.Id pdc, readName) @ cleanupSpaceSs)
			      val readS =   [commentS]
				          @ initSpaceSs
				          @ PL.chkPtS(PT.Id pdc, readName) 
				          @ readVariant(pred,name,args,readFieldName,foundSs,notFoundSs)
			  in
			      readS
			  end

                     fun genReadBrief _ = []
                     fun genReadUnionEOR _ = []

		     (* Given manifest representation, generate operations to set representation *)
		     fun genReadMan {decl, comment} = 
			 let val ctNoptEs = cnvDeclaration decl
			     fun doOne (cty, nameOpt, exp) = 
				 let val name = case nameOpt of NONE => "bogus"
			       | SOME n => n  
				     fun glhsX base = P.dotX(fieldX(base, value), PT.Id name)
				     val repX = glhsX rep
				     val pdX = glhsX pd
				     val commentS = P.mkCommentS ("Computing variant: "^ name ^ ".")
				     val initS = genAssignMan(cty,name,repX, exp)
				 in
				      cleanupSpaceSs
				      @[commentS,
				        P.assignS(fieldX(rep, tag), PT.Id name),
				        initS,
				        P.assignS(fieldX(pd, tag), PT.Id name),
					P.assignS(P.dotX(pdX, PT.Id errCode), PL.PDC_NO_ERROR),
					PL.initParseStateS(P.addrX(pdX)),
					PL.getLocS(PT.Id pdc,P.addrX(P.dotX(pdX,PT.Id loc)))]
				      @ returnSs
				 end
			 in
			     List.concat(List.map doOne ctNoptEs)
			 end


                     fun genErrorSs (s,locS) = [P.mkCommentS s]
			             @ reportErrorSs([locS], locX,true,
					PL.PDC_UNION_MATCH_FAILURE,
					true, readName,s, [])
			             @ [P.assignS(fieldX(rep,tag),PT.Id (errSuf name)),
					P.assignS(fieldX(pd, tag),PT.Id (errSuf name))]

		     fun genCleanupSs (s,locS) =  (genErrorSs (s,locS))
			             @ [PL.setPanicS(PT.Id pd)]
				     @ (if isRecord then
			                 [PT.Labeled(findEORSuf name, 
						   PT.Compound (genReadEOR (readName, reportUnionErrorSs) ()))]
				        else [])
				     @ [PT.Return (PT.Id result)]
                     
		     fun chkCaseLabel eOpt = 
			 case eOpt of NONE => ()
		       | SOME e => expEqualTy(e, CTintTys, 
					      fn s=> (" case label for variant "^
						      name ^ " has type: " ^ s ^
						      ". Expected an int."))
		     fun genReadSwFull (eOpt,
			               {pty :PX.Pty, args:pcexp list, name:string, 
				        isVirtual:bool, isEndian:bool,isRecord,containsRecord,largeHeuristic:bool, 
				        pred:pcexp option, comment}) = 
			 let val readFieldName = lookupTy(pty, readSuf, #readname)
			     val () = checkParamTys(name, readFieldName, args, 2, 2)
			     val () = chkCaseLabel eOpt
			     val commentS = P.mkCommentS("Reading variant " ^ name^".")
			     val foundSs = PT.Compound returnSs
			     val notFoundSs = PT.Compound (
						genErrorSs ("Failed to match branch "^ name^".", locES1))
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

                     fun genSwDefaultIfAbsent () = 
			 let val errorSs = 
			         genErrorSs ("Failed to match any branch of "^ name^".", locES1)
			 in
			     [PT.DefaultLabel(PT.Compound errorSs)]
			 end

                     fun buildSwitchRead (descriminator) = 
			     let val () = expEqualTy(descriminator, CTintTys, 
						     fn s=> (" Descriminator for union "^
							     name ^ " has type: " ^ s ^
							     ". Expected an int."))
				 val readFields = mungeBVs genReadSwFull genReadBrief genReadSwMan cases variants
				 val augReadFields = if hasDefault then readFields 
				                     else readFields @ (genSwDefaultIfAbsent())
				 val bodyS = PT.Switch(descriminator, PT.Compound augReadFields)
			     in
				 [PT.Compound (  [P.varDeclS(PL.toolErrPCT, result, PL.PDC_ERROR)] 
					       @ deallocOldSpaceSs 
                                               @ [locBS,
						  bodyS,
						  PT.Return (PT.Id result)])]
			     end

                     fun buildReadFun () = 
			 case descOpt of NONE => 
			     let val readFields = mungeVariants genReadFull genReadBrief genReadMan
 					           variants  (* does type checking *)
				 val cleanupSs = genCleanupSs ("Failed to match any branch of union "^name^".",
							       locES)
			     in
				 [PT.Compound ([P.varDeclS(PL.toolErrPCT, result, PL.PDC_ERROR),
						locBS] 
					       @ deallocOldSpaceSs @ readFields @ cleanupSs)]
			     end
                         | SOME descriminator => buildSwitchRead(descriminator)

                     (* -- Assemble read function union case *)
		     val _ = pushLocalEnv()                                        (* create new scope *)
		     val () = ignore (insTempVar(rep, P.ptrPCT canonicalPCT)) (* add modrep to scope *)
		     val cParams : (string * pcty) list = List.map mungeParam params
                     val () = ignore (List.map insTempVar cParams)  (* add params for type checking *)
		     val bodySs = buildReadFun() 
		     val _ = popLocalEnv()                                         (* remove scope *)
		     val readFunEDs = genReadFun(readName, cParams,mPCT,pdPCT,canonicalPCT, 
						 mFirstPCT, true, bodySs)

                     val readEDs = toStringEDs @ initRepEDs @ initPDEDs @ cleanupRepEDs @ cleanupPDEDs
			         @ copyRepEDs @ copyPDEDs @ maskFunEDs @ readFunEDs

                      (* Generate Accumulator functions (union case) *)
                      (* -- generate accumulator init, reset, and cleanup functions *)
		      fun genResetInitCleanup theSuf = 
			  let val theFun = (theSuf o accSuf) name
			      val theDeclSs = [P.varDeclS(PL.uint32, nerr, P.zero)]
			      fun genAccTheFull {pty :PX.Pty, args:pcexp list, name:string, 
						 isVirtual:bool, isEndian:bool,isRecord,containsRecord,largeHeuristic:bool,
						 pred:pcexp option, comment} = 
				  case lookupAcc(pty) of NONE => []
				| SOME a => chk3Pfun(theSuf a, getFieldX(acc,name))
			      fun genAccTheBrief e = []
			      val tagFields = mungeVariants genAccTheFull genAccTheBrief 
				              (genAccTheMan theSuf) variants
			      val auxFields = chk3Pfun(theSuf PL.intAct, getFieldX(acc,tag))
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
                      (*  PDC_error_t T_acc_add (PDC_t* , T_acc* , T_pd*, T* ) *)
		      val addFun = (addSuf o accSuf) name
		      val addDeclSs = [P.varDeclS(PL.uint32, nerr, P.zero), P.varDeclS'(PL.base_pdPCT, tpd)]
		      val initTpdSs = [P.assignS(P.dotX(PT.Id tpd, PT.Id errCode), 
						 P.condX(P.eqX(P.arrowX(PT.Id pd, PT.Id errCode),
							       PL.PDC_UNION_MATCH_FAILURE),
							 PL.PDC_UNION_MATCH_FAILURE, PL.PDC_NO_ERROR))]
		      val addTagSs = chkAddFun(addSuf PL.intAct, getFieldX(acc,tag), P.addrX(PT.Id tpd), 
						  PT.Cast(P.ptrPCT PL.intPCT, getFieldX(rep,tag)))
		      fun fieldAddrX (base,name) = P.addrX(P.arrowX(PT.Id base, PT.Id name))

		      fun genCase (name,pty, initSs, pdX) = 
			  case lookupAcc(pty) of NONE => []
			| SOME a => (let val funName = addSuf a
					 val repX = unionBranchX(rep,name)
				     in 
					 [PT.CaseLabel(PT.Id name, 
						       PT.Compound (initSs 
								    @ chkAddFun(funName, fieldAddrX(acc, name), 
										pdX, repX)
						                    @ [PT.Break]))]
				     end
		      (* end accOpt SOME case *))
		      fun genAccAddFull {pty :PX.Pty, args:pcexp list, name:string, 
					 isVirtual:bool, isEndian:bool,isRecord,containsRecord,largeHeuristic:bool, 
					 pred:pcexp option, comment} = 
			  genCase (name,pty, [], unionBranchX(pd,name))

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
					  [P.varDeclS'(P.makeTypedefPCT(lookupTy(pty, pdSuf, #pdname)), tpd),
					   P.assignS(P.dotX(PT.Id tpd,PT.Id errCode),
						     P.arrowX(PT.Id pd, PT.Id errCode))] 
				       in
					  genCase(name,pty,initSs,unionBranchX(pd,name))
				       end
				  end
			  in
			      List.concat(List.map doOne  ctNoptEs)
			  end
		      val nameBranchSs = mungeVariants genAccAddFull genAccAddBrief genAccAddMan variants
		      val errBranchSs = [PT.CaseLabel(PT.Id (errSuf name), PT.Break)]
		      val addBranchSs = nameBranchSs @ errBranchSs
                      val addVariantsSs = [PT.Switch (P.arrowX(PT.Id rep, PT.Id tag), PT.Compound addBranchSs)]
		      val addReturnS = genReturnChk (PT.Id nerr)
                      val addBodySs = addDeclSs @ initTpdSs @ addTagSs @ addVariantsSs @ [addReturnS]
                      val addFunED = genAddFun(addFun, accPCT, pdPCT, canonicalPCT, addBodySs)

                      (* -- generate report function (internal and external)  punion *)
                      (*  PDC_error_t T_acc_report (PDC_t* , [Sfio_t * outstr], const char* prefix, 
		                                    const char * what, int nst, T_acc*  ) *)
		      val reportFun = (reportSuf o accSuf) name
                      val reportTags = [chkPrint(callEnumPrint((ioSuf o reportSuf o mapSuf) PL.intAct,
						    PT.String "Union tag", PT.String "tag", P.intX ~1,
						    PT.Id ((toStringSuf o tgSuf) name), getFieldX(acc,tag))),
					PL.sfprintf(PT.Id outstr, 
						    PT.String "\n[Describing each tag arm of %s]\n", 
						    [PT.Id prefix])]
		      fun genAccReportFull {pty :PX.Pty, args:pcexp list, name:string, 
					    isVirtual:bool, isEndian: bool,isRecord,containsRecord,largeHeuristic:bool, 
					    pred:pcexp option, comment} = 
			  cnvPtyForReport(reportSuf, ioSuf, pty, name)
                      fun genAccReportBrief e = []
		      val reportVariants = mungeVariants genAccReportFull genAccReportBrief 
			                      (genAccReportMan (reportSuf, ioSuf))variants
                      val reportFunEDs = genReportFuns(reportFun, "union "^name, 
						       accPCT, reportTags @ reportVariants)

		      val accumEDs = accED :: initFunED :: resetFunED :: cleanupFunED :: addFunED :: reportFunEDs

                      (* Generate Write function union case *)
		      val writeName = writeSuf name
		      fun genWriteFull {pty :PX.Pty, args:pcexp list, name:string, 
					isVirtual:bool, isEndian:bool,isRecord,containsRecord,largeHeuristic:bool, 
					pred:pcexp option, comment} = 
			  if isVirtual then [] (* have no rep of virtual (omitted) fields, so can't print *)
                          else
			    let val writeFieldName = (bufSuf o writeSuf) (lookupWrite pty) 
			    in
				[PT.CaseLabel(PT.Id name,
				  PT.Compound(
				     writeFieldSs(writeFieldName, args@[unionBranchX(pd,name),unionBranchX(rep,name)],true)
                                   @ [PT.Break]))]
			    end
		      fun genWriteBrief e = []
		      fun genWriteMan _ = []     (* Manifest fields do not need to be written *)
		      val nameBranchSs = mungeVariants genWriteFull genWriteBrief genWriteMan variants
		      val errBranchSs = [PT.DefaultLabel  PT.Break]
		      val writeBranchSs = nameBranchSs @ errBranchSs
                      val writeVariantsSs = [PT.Switch (P.arrowX(PT.Id rep, PT.Id tag), PT.Compound writeBranchSs)]
		      val bodySs = writeVariantsSs 
                      val writeFunEDs = genWriteFuns(writeName, isRecord, cParams, pdPCT, canonicalPCT, bodySs)





		      (***** union PADS-Galax *****)

		      fun genBranchFull {pty: PX.Pty, args: pcexp list, name: string, isVirtual: bool, 
				      isEndian: bool, isRecord, containsRecord, largeHeuristic: bool,
				      pred: pcexp option, comment: string option} = 
			  [(name, lookupTy (pty,repSuf,#repname))]
		      fun genBranchBrief e = []
		      fun genBranchMan m = []
		      val localBranches = mungeVariants genBranchFull genBranchBrief genBranchMan variants		

		      fun tagbranches [] = []
		  	| tagbranches ((i,(n,f))::ps) = 
				(PT.CaseLabel(PT.Id n,
				              PT.Compound ([macroNodeCall(PT.Id result,P.intX i,f,n,getFieldX(m,n),
							       P.addrX(P.dotX(P.arrowX(PT.Id pd,PT.Id value),PT.Id n)),
(* check out 'branch' in Macro Call*)	   		       P.addrX(P.dotX(P.arrowX(PT.Id pd,PT.Id value),PT.Id n)),
							       childrenSuf name),
						            PT.Break])))::(tagbranches ps)

		      fun switchTag xs = PT.Switch (P.arrowX(PT.Id rep, PT.Id tag),
						    PT.Compound (tagbranches (enumerate xs)))

    	              (* PDCI_node_t** fooUnion_children(PDCI_node_t *self) *)
		      fun genGalaxUnionChildrenFun(name,variants) =		
		          let val nodeRepTy = PL.nodeT
                              val returnName = PT.Id result
			      val returnTy = P.ptrPCT (P.ptrPCT (nodeRepTy))
                              val cnvName = childrenSuf name 
                              val paramNames = [self]
                              val paramTys = [P.ptrPCT nodeRepTy]
                              val formalParams =  List.map P.mkParam(ListPair.zip(paramTys, paramNames))
			      val numFields = List.length variants
 		              val bodySs = headerGalaxChildrenFun(name) @
					   [P.varDeclS(P.ccharPtr,"branch",
                                                       PT.Call(PT.Id (toStringSuf (tgSuf name)),[fieldX(rep,tag)]))] @
					   ifGalaxChildren(returnName,P.intX 2, "ALLOC_ERROR: in " ^ cnvName) @
					   macroTNode(returnName,PL.PDCI_structured_pd,pd,PT.Id pd,cnvName) @
				 	   [switchTag localBranches] @
					   [P.returnS (returnName)]
                          in   
                            P.mkFunctionEDecl(cnvName, formalParams, PT.Compound bodySs, returnTy)
                          end

		      val galaxEDs = [genGalaxUnionChildrenFun(name,variants),
		                      genGalaxVtable(name)]

		 in
		       tagDecls
		     @ unionDecls
		     @ canonicalDecls
	             @ cnvExternalDecl mStructED
                     @ unionPDDecls
	             @ pdStructPDDecls
                     @ (emitRead readEDs)
		     @ (emitAccum accumEDs)
                     @ (emitWrite writeFunEDs)
                     @ (emitXML galaxEDs)
		 end
	  
             fun cnvPArray {name:string, params : (pcty * pcdecr) list, isRecord, containsRecord, 
                            largeHeuristic, isFile : bool, args : pcexp list, baseTy:PX.Pty, 
			    sizeSpec:pcexp PX.PSize option, constraints: pcexp PX.PConstraint list} =
	     let val length = PNames.arrayLen
                 val elts = PNames.arrayElts
                 val internal = "_internal"
		 val element = "element"
		 val elt = "elt"
                 val array = "array"
                 val arrayDetail = "arrayDetail"
                 val neerr = "neerr"
                 val firstError = "firstError"
                 val elemRepPCT = P.makeTypedefPCT(lookupTy(baseTy, repSuf, #repname))
                 val elemEdPCT  = P.makeTypedefPCT(lookupTy(baseTy, pdSuf, #pdname))
                 val elemMPCT  = P.makeTypedefPCT(lookupTy(baseTy, mSuf, #mname))
                 val elemReadName = lookupTy(baseTy, readSuf, #readname)
		 val tLocX      =  P.addrX(PT.Id tloc)
		 val tlocES     =  PL.getLocEndS(PT.Id pdc, tLocX, ~1) 


                 (* Some useful functions *)
                 fun recordArrayErrorS (getLocSs, locX, errCodeC, shouldPrint, whatFun, msg, args, setPanic) = 
                     PT.Compound([
		       PT.IfThen(PL.getSpecLevelX(PT.Id pdc),
				 PT.Return PL.PDC_ERROR),
  		       PT.IfThenElse(P.notX(fieldX(pd,nerr)),
			  PT.Compound (reportErrorSs(getLocSs,locX, true,errCodeC,shouldPrint,whatFun,msg,args)),
			  PT.Compound[P.postIncS(fieldX(pd,nerr))])]
                       @ (if setPanic then [PL.setPanicS(PT.Id pd)] else []))
  

                 fun amCheckingBasicE(SOME testE) = 
                     P.andX(PL.mTestSynCheckX(fieldX(m,array)), testE)
                   | amCheckingBasicE(NONE) = PL.mTestSynCheckX(fieldX(m,array))

                 fun amCheckingForallE(SOME testE) = 
                     P.andX(PL.mTestSemCheckX(fieldX(m,array)), testE)
                   | amCheckingForallE(NONE) = PL.mTestSemCheckX(fieldX(m,array))


                 (* Calculate bounds on array, generate statements for checking values *)
                 (* used in read function, defined below *)
		 val readName = readSuf name
		 val pdRBufferX   = fieldX(pd, internal)
		 val resRBufferX  = fieldX(rep, internal)

                 (* Array: error checking *)
                 val _ = CTcnvType elemRepPCT 

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
                               PL.chkNewRBufS(readName, resRBufferX, zeroCanonical, PT.Id pdc)
			     @ PL.chkNewRBufS(readName, pdRBufferX, true, PT.Id pdc)
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
					     amCheckingBasicE(SOME (P.ltX(minX,P.zero))),
					     recordArrayErrorS([tlocES], tLocX,
							       PL.PDC_ARRAY_MIN_NEGATIVE,true,
							       readName,
							       "Minimum value for the size of array "^
							       name ^  "(%d) " ^
							       "is negative.", [minX], false))]

			 fun genPosMaxCheckSs (maxConstOpt,maxX) = 
			     if isSome maxConstOpt then []
			     else [PT.IfThen( (* if (maxX<0) *)
					     amCheckingBasicE(SOME(P.ltX(maxX,P.zero))),
					     recordArrayErrorS([tlocES], tLocX,
							       PL.PDC_ARRAY_MAX_NEGATIVE,true, readName,
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
						     amCheckingBasicE(SOME(P.gtX(minX,maxX))), 
						      recordArrayErrorS([tlocES], tLocX,
									PL.PDC_ARRAY_MIN_BIGGER_THAN_MAX_ERR,
                                                                        true, readName,
									      "Mininum value for "^
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
		 val () = popLocalEnv()  (* remove scope with parameters used for type checking
					  size specifications *)

	         (* Generate CheckSet mask *)
		 val mFields = [(element, P.makeTypedefPCT(lookupTy(baseTy, mSuf, #mname)),
				  SOME "per-element"),
				 (array,   PL.base_mPCT, SOME "entire array")]
		 val mStructED = P.makeTyDefStructEDecl (mFields, mSuf name)
		 val mStructDecls = cnvExternalDecl mStructED 
		 val mPCT = P.makeTypedefPCT (mSuf name)			  

	         (* Generate parse description *)
                 val pdFields = [(pstate, PL.flags_t, NONE), 
				 (errCode, PL.errCodePCT, NONE),
				 (loc, PL.locPCT, NONE), 
				 (nerr, PL.uint32,    SOME "Number of array errors"),
				 (neerr, PL.uint32,   SOME "Number of element errors"),
				 (firstError, PL.uint32, 
				    SOME "if errCode == ARRAY_ELEM_ERR, index of first error"),
				 (length, PL.uint32, NONE),
				 (elts, P.ptrPCT(elemEdPCT), NONE),
				 (internal, P.ptrPCT PL.rbufferPCT, NONE)] 
		 val pdStructED = P.makeTyDefStructEDecl (pdFields, pdSuf name)
		 val (pdStructDecls,pdTid) = cnvCTy pdStructED 
		 val pdPCT = P.makeTypedefPCT (pdSuf name)			  

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
		 val accED = P.makeTyDefStructEDecl (accFields, accSuf name)
		 val accPCT = P.makeTypedefPCT (accSuf name)			


	         (* Calculate and insert type properties into type table *)
                 val baseMemChar = lookupMemChar baseTy
		 val arrayMemChar = TyProps.Dynamic (* at the moment, all arrays are dynamically allocated. *)
                 val baseDiskSize = lookupDiskSize baseTy
                 val arrayDiskSize = case (maxConstOpt, minConstOpt, baseDiskSize)
		                     of (SOME min, SOME max, TyProps.Size (n,r)) => 
					 if min = max then TyProps.Size(IntInf.*(n, max), IntInf.*(r, max))
					 else TyProps.Variable
				     | _ => TyProps.Variable
		 val contR = lookupContainsRecord baseTy 
		 val lH = contR orelse (lookupHeuristic baseTy)
                 val numArgs = List.length params
		 val compoundArrayDiskSize = TyProps.Array {elem=TyProps.Variable, sep = TyProps.Variable, term=TyProps.Variable, length = TyProps.Variable}
                 val arrayProps = buildTyProps(name, PTys.Array, arrayDiskSize, compoundArrayDiskSize,
					       arrayMemChar,false,isRecord,contR,lH,isFile,pdTid, numArgs)
                 val () = PTys.insert(Atom.atom name, arrayProps)


		 (* array: Generate canonical representation *)
		 val canonicalFields = [(length, PL.intPCT, NONE), 
				        (elts, P.ptrPCT elemRepPCT, NONE),
					(internal, P.ptrPCT PL.rbufferPCT, NONE) ]
		 val canonicalStructED = P.makeTyDefStructEDecl (canonicalFields, repSuf name)
		 val (canonicalDecls,canonicalTid) = cnvRep(canonicalStructED, valOf (PTys.find (Atom.atom name)))
		 val canonicalPCT = P.makeTypedefPCT (repSuf name)			 


		 (* Generate init function, array case *)
		 fun genInitEDs(suf, base, aPCT) = 
		   case #memChar arrayProps
		   of TyProps.Static => [genInitFun(suf name, base, aPCT, [],true)]
		   |  TyProps.Dynamic => 
			 let val bodySs =  [PL.bzeroS(PT.Id base, P.sizeofX(aPCT))]
			 in
			     [genInitFun(suf name, base, aPCT, bodySs,false)]
			 end
		 val initRepEDs = genInitEDs(initSuf, rep, canonicalPCT)
		 val initPDEDs = genInitEDs(initSuf o pdSuf, pd, pdPCT)


		 (* Generate cleanup function, array case *)
		 fun genCleanupEDs(suf, base, aPCT) = 
		     let val funName = suf name
		     in case #memChar arrayProps
		        of TyProps.Static => 
			      [genInitFun(suf name, base,aPCT,[],true)]
		        |  TyProps.Dynamic => 
			      let val bodySs = 
				  [P.assignS(P.arrowX(PT.Id base, PT.Id length), P.zero),
			           P.assignS(P.arrowX(PT.Id base, PT.Id elts), P.zero),
				   PT.IfThen(
				      P.arrowX(PT.Id base, PT.Id internal),
			              PT.Compound[
			              PL.chkCFreeRBufferS(PT.Id pdc, funName,
						          P.arrowX(PT.Id base, PT.Id internal))])]
			 in
			     [genInitFun(funName, base, aPCT, bodySs,false)]
			 end
		     end
		 val cleanupRepEDs = genCleanupEDs(cleanupSuf, rep, canonicalPCT)
		 val cleanupPDEDs = genCleanupEDs(cleanupSuf o pdSuf, pd, pdPCT)

		 (* Generate copy function, array case *)
		 fun genCopyEDs(suf, base, aPCT, elemPCT) = 
		     let val copyFunName = suf name
			 val dst = dstSuf base
			 val src = srcSuf base
			 val lengthX = fieldX(src,length)
			 val arraySizeX = P.timesX(lengthX, P.sizeofX elemPCT)
			 val varSizeX = P.plusX(P.sizeofX (P.ptrPCT elemPCT), P.sizeofX(P.ptrPCT PL.rbufferPCT))
			 val fixedSizeX = P.minusX(P.sizeofX aPCT, varSizeX)
			 val copyLenSs = [PL.memcpyS(PT.Id dst, PT.Id src, fixedSizeX)]
			 val copyRBufSs = [PL.rbufCopyS(fieldX(dst,internal), fieldX(src, internal),
							fieldX(dst, elts),
							arraySizeX, PL.nonZeroMM(PT.Id pdc))]
			 val (copyElemsSs, isStatic) = 
			     case baseMemChar
			     of TyProps.Static => 
				 ([PL.memcpyS(fieldX(dst,elts), fieldX(src,elts), arraySizeX)], true)
			     |  TyProps.Dynamic => 
				 let val bodySs = 
				     []
				 in
				     ([], false)
				 end
			 val bodySs = copyLenSs @ copyRBufSs @ copyElemsSs
		     in
			 [genCopyFun(copyFunName, dst, src, aPCT, bodySs,isStatic)]
		     end

		 val copyRepEDs = genCopyEDs(copySuf o repSuf, rep, canonicalPCT, elemRepPCT)
		 val copyPDEDs = genCopyEDs(copySuf o pdSuf, pd, pdPCT, elemEdPCT)

                 (* Generate m_init function array case *)
                 val maskInitName = maskInitSuf name 
                 val maskFunEDs = genMaskInitFun(maskInitName, mPCT)

		 (* Array: Generate read function *)
                 (* -- Some useful names *)
                 val readName = readName (* defined above *)
                 val foundTerm    = "foundTerm"
		 val reachedLimit = "reachedLimit"


		 val resBufferX   = fieldX(rep, elts)
		 val indexX       = P.minusX(fieldX(rep,length), P.intX 1)
		 val resNext      = P.subX(resBufferX, indexX)


		 val edBufferX    = fieldX(pd, elts)
 		 val edNext       = P.subX(edBufferX, indexX)

		 val _ = pushLocalEnv()                                        (* create new scope *)
		 val cParams : (string * pcty) list = List.map mungeParam params
		 val () = ignore (List.map insTempVar cParams)  (* add params for type checking *)
		 (* scope is removed at end of cnvPArray *)
          

                 (* -- process constriants *)
                 val (sepXOpt, termXOpt, arrayXOpt, genXOpt, sepTermDynamicCheck) = 
                      let fun doOne (constr:pcexp PX.PConstraint) = 
                              case constr 
                              of PX.Sep exp => (
				 expEqualTy(exp, CTintTys,fn s=>("Separator expression for array "^
								 name ^" has type "^s^". Expected "^
								 "type char."));
				 let val pTyName = PL.charlit
				     val readFun = readSuf pTyName
				     val scanFun = SOME(Atom.atom(scanSuf pTyName))
				     val writeFun = lookupLitWrite pTyName
				     val (valOpt,_,_,_) = evalExpr exp
				 in
				    (SOME (exp, valOpt, readFun, scanFun, writeFun), NONE,NONE,NONE)
                                 end

                              (* end Sep case *))
                              |  PX.Term exp => (
				 expEqualTy(exp, CTintTys,fn s=>("Terminator expression for array "^
								 name ^" has type "^s^". Expected "^
								 "type char."));
                                 let val pTyName = PL.charlit 
				     val readFun = readSuf pTyName
				     val scanFun = SOME(Atom.atom(scanSuf pTyName))
				     val writeFun = lookupLitWrite pTyName
				     val (valOpt,_,_,_) = evalExpr exp
				 in
				   (NONE, SOME (exp,valOpt,readFun,scanFun,writeFun), NONE, NONE)
                                 end
                              (* end Term case *))
                              |  PX.Forall (r as {index,range,body}) => (
                                 let val subList = [(length, fieldX(rep,length)), 
						    (name, fieldX(rep,elts)),
						    (elts, fieldX(rep,elts))]
				     val (lower, upper) = 
					(case range 
					 of PX.ArrayName n => (
					    (if n = name orelse n = elts then ()
					     else PE.error ("Array name in bound expression ("^
							    n^") does not match the name "^
							    "of the array (must use '"^ name ^ "' or 'elts').")
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
				   ignore(insTempVar(length, PL.uint32));
				   ignore(insTempVar(name, P.ptrPCT elemRepPCT)); 
				   ignore(insTempVar(elts, P.ptrPCT elemRepPCT)); 
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
			  val (sepXOpt, termXOpt, arrayXOpt, genXOpt) = 
			      List.foldr mergeAll (NONE,NONE,NONE,NONE) constrs
			  val sepTermErrorMsg = "Psep and Pterm expressions for Parray "^ name^
							 " have the same value"
			  val sepTermDynamicCheck = 
			      case (sepXOpt, termXOpt) 
			      of (SOME(_, SOME i, _, _, _), SOME(_, SOME j, _, _, _)) => 
				  if i = j then (PE.error (sepTermErrorMsg^"."); [])
				  else []
			      | (SOME(sepX, _, _, _, _), SOME(termX, _, _, _, _)) => 
				      [PT.IfThen(P.eqX(sepX,termX),
						 PL.userErrorS(PT.Id pdc, locX, 
							       PL.PDC_ARRAY_SEP_TERM_SAME_ERR, readName, 
							       PT.String (sepTermErrorMsg^": %c"), [sepX]))]
			      |  (_,_) => []
		      in
			  (sepXOpt, termXOpt, arrayXOpt, genXOpt, sepTermDynamicCheck)
                      end

                 (* -- Check parameters to base type read function *)
		 val () = checkParamTys(name, elemReadName, args, 2, 2)
                 val termTmpDecls = case termXOpt of NONE => [] | SOME _ =>
		                    [P.varDeclS(PL.base_mPCT, tm, PL.M_CHECK), (* base_m tm = Check; *)
          		             P.varDeclS'(PL.base_pdPCT, tpd)]
                 (* -- Declare top-level variables and initialize them *)
                 val initSs =   termTmpDecls 
			      @ [P.varDeclS'(PL.locPCT, tloc)] 
                              @ (if Option.isSome termXOpt then             (* int foundTerm = false *)
                                   [P.varDeclS(P.int, foundTerm, P.falseX)] 
                                 else [])
                              @ (if Option.isSome maxOpt then               (* int reachedLimit = false *)
				   [P.varDeclS(P.int, reachedLimit, P.falseX)]
			        else [])
                              @ [ P.assignS(fieldX(rep,length), P.zero),
				  P.assignS(fieldX(pd, neerr), P.zero),
				  P.assignS(fieldX(pd, firstError), P.zero),
				  PL.getLocBeginS(PT.Id pdc, P.addrX(PT.Id tloc))]      

                 (* -- fragments for while loop for reading input *)

                 (* -- code for checking if terminator is next in input *)
                 fun genTermCheck NONE = []
                   | genTermCheck (SOME (exp, cExp, readFun, scanFun, writeFun)) = 
                      [P.mkCommentS("Looking for terminator"),
	               PL.incNestLevS(PT.Id pdc),
		       PT.IfThen(
                          PL.readFunChkX(PL.PDC_OK, readFun, PT.Id pdc, P.addrX(PT.Id tm),  [], 
						             P.addrX(PT.Id tpd), exp),
			  PT.Compound
                           [P.assignS(PT.Id foundTerm, P.trueX)]),
		       PL.decNestLevS(PT.Id pdc)]

                 (* -- Code for checking termination conditions *)
                 fun genBreakCheckX (termOpt, sizeOpt) = 
		     let val isEofX = PL.isEofX(PT.Id pdc)
			 val isEorX = PL.isEorX(PT.Id pdc)
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
                   | genSepCheck (SOME (sepX, cSepX, readSep, scanSepOpt, writeSep)) = 
                      let val scanSep = Option.valOf scanSepOpt (* must exist *)
			                handle Option => (PE.error "Expected scan function."; 
							  Atom.atom "bogus")
			  val (scanStopX, chkTermSs) = 
			      case termXOpt of NONE => (P.intX 0, [])
			    | SOME(termX,cTermX,_,_,_) => 
				  let val chkTermSs = 
				      [PT.IfThen(P.eqX(PT.Id "c", termX),
					 PT.Compound[
				          recordArrayErrorS([locES], locX,
							    PL.PDC_ARRAY_EXTRA_BEFORE_TERM,true,
							    readName,"",[],false),
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
				  PL.scanFunX(Atom.toString scanSep, PT.Id pdc, 
					      sepX, scanStopX, P.trueX, P.addrX (PT.Id "c"),
					      P.addrX (PT.Id "n"))),
			    PT.Compound[
                              PT.IfThen(amCheckingBasicE NONE, 
	  		       PT.Compound[ (* if am checking *)
			         PT.IfThenElse(P.andX(P.eqX(PT.Id "c", sepX),P.gtX(PT.Id "n", P.zero)),
				    recordArrayErrorS([locES],locX,PL.PDC_ARRAY_EXTRA_BEFORE_SEP, true,
						      readName,"", [],false),
                                    PT.Compound (chkTermSs))])],
                            PT.Compound[ (* else error in reading separator *)
			      P.mkCommentS("Error reading separator"),
			      recordArrayErrorS([locES],locX,PL.PDC_ARRAY_SEP_ERR, 
						true, readName, "Missing separator.",[],true),
			      PT.Break]
                            )]]

                      end
                 (* -- read next element *)
		 val (chkLenSs, bufSugX) = case maxOpt of NONE => ([], P.zero)
	             | SOME sizeX => 
		        ([P.assignS(PT.Id reachedLimit, P.gteX(fieldX(rep,length), Option.valOf maxOpt))],
			 sizeX)
                 val readElementSs = 
                       [P.postIncS(fieldX(rep,length)),
			locBS]
                     @ chkLenSs
		     @ (PL.chkReserveSs(PT.Id pdc,  readName, resRBufferX, 
				     P.addrX resBufferX, P.sizeofX elemRepPCT,
				     fieldX(rep,length),bufSugX))
		     @ (PL.chkReserveSs(PT.Id pdc, readName, pdRBufferX, 
				     P.addrX edBufferX, P.sizeofX elemEdPCT,
				     fieldX(rep,length),bufSugX))
                     @ [PT.IfThen(
                          PL.readFunChkX(PL.PDC_ERROR, elemReadName, 
					               PT.Id pdc, 
					               P.addrX(fieldX(m,element)),
						       args,
						       P.addrX(edNext),
						       P.addrX(resNext)),
                          PT.Compound[
			   PT.IfThen(PL.getSpecLevelX(PT.Id pdc),
				     PT.Return PL.PDC_ERROR),
			   PT.IfThen(PL.mTestNotIgnoreX(fieldX(m,array)),
			     PT.Compound[
                              PT.IfThen(P.notX(fieldX(pd,nerr)),
                                 PT.Compound (
	 			   (reportErrorSs([locES],locX,true,PL.PDC_ARRAY_ELEM_ERR, false, readName, "", []))
                                  @ [P.mkCommentS("Index of first element with an error."),
				     P.assignS(fieldX(pd,firstError), P.minusX(fieldX(rep,length),P.intX 1))])),
                              P.postIncS(fieldX(pd,neerr))
                            ])])]

                 (* -- panic recovery code *)
		 fun genPanicRecoveryS (sepXOpt, termXOpt, maxOpt) = 
                     let val panicSs = [PL.setPanicS(PT.Id pd), PT.Break]
                         val recoveryFailedSs = P.mkCommentS("Recovery failed.") :: panicSs
			 val noRecoverySs = P.mkCommentS("No recovery possible.") :: panicSs
			 fun recoverToCharSs (which, scan, forX, stopX) = [
				  P.mkCommentS("Try to recover to " ^ which ^"."),
				  PT.IfThenElse(P.eqX(PL.PDC_OK,
						   PL.scanFunX(Atom.toString scan, PT.Id pdc, 
							       forX, stopX, P.falseX, P.zero, P.zero)),
                                    PT.Compound[
				     P.mkCommentS("We recovered; restored invariant.")],
				    PT.Compound(recoveryFailedSs)
                                 )]
			 val recoverSs = 
			 case (sepXOpt, termXOpt, maxOpt) 
                         of (NONE,NONE,_) => noRecoverySs
                         |  (SOME (sepX, _, _, NONE, _), NONE, _) => noRecoverySs
                         |  (SOME (sepX, _, _, SOME sepScan, _), NONE, NONE) => 
                               recoverToCharSs("separator", sepScan, sepX, P.intX 0)
                         |  (SOME (sepX, _,_ , SOME sepScan, _), NONE, SOME _) => 
			       [PT.IfThenElse(PT.Id reachedLimit,
				 PT.Compound(noRecoverySs), 
				 PT.Compound(recoverToCharSs ("separator", sepScan, sepX, P.intX 0)))]
                         |  (NONE, SOME(termX, _, _, NONE, _), _ ) => noRecoverySs
                         |  (NONE, SOME(termX, _, _, SOME termScan, _), _ ) => 
			       recoverToCharSs("terminator", termScan, termX, P.intX 0)
                         |  (SOME (sepX, _, _, SOME sepScan, _), SOME(termX, _, _, SOME termScan,_), _ ) =>
			      (if Atom.sameAtom (sepScan, termScan)
			       then recoverToCharSs("separator and/or terminator", termScan, sepX,termX)
                               else (PE.error ("Different scanning functions for separators and terminators "^
					      "not yet implemented."); noRecoverySs))
			 |  (SOME (sepX, _, _, SOME sepScan,_), SOME(termX, _, _, NONE,_), SOME _ ) =>
			       [PT.IfThenElse(PT.Id reachedLimit,
				 PT.Compound(noRecoverySs), 
				 PT.Compound(recoverToCharSs ("separator", sepScan, sepX, P.intX 0)))]
			 |  (SOME (sepX, _, _, SOME sepScan,_), SOME(termX, _, _, NONE,_), NONE ) =>
                               recoverToCharSs("separator", sepScan, sepX, P.intX 0)
			 |  (SOME (sepX, _, _, NONE,_), SOME(termX, _, _, SOME termScan,_),  _ ) =>
			       recoverToCharSs("terminator", termScan, termX, P.intX 0)
			 |  (SOME (sepX, _, _, NONE,_), SOME(termX, _, _, NONE,_),  _ ) => noRecoverySs
		     in
			 PT.Compound recoverSs
		     end
                 val panicRecoverySs = [PT.IfThen(PL.testPanicX(P.addrX(edNext)),
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
			     case termXOpt of NONE => PT.Compound[bdyS]
			     | SOME (termX, _, termRead, _,_) => (
				PT.Compound[PL.incNestLevS(PT.Id pdc),
                                PT.IfThenElse(
                                 PL.readFunChkX(PL.PDC_OK, termRead, PT.Id pdc, 
						P.addrX (PT.Id tm), [], P.addrX (PT.Id tpd),
						termX),
				 PT.Compound[
				  PL.decNestLevS(PT.Id pdc),
				  P.assignS(PT.Id foundTerm, P.trueX)
                                 ],
                                 PT.Compound[PL.decNestLevS(PT.Id pdc), bdyS])
                             (* end case SOME *)])
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
			                   P.andX(P.notX(PL.isEofX(PT.Id pdc)),
						  P.notX(PL.isEorX(PT.Id pdc)))
					 else
			                   P.notX(PL.isEofX(PT.Id pdc))
		     in 
			 [P.mkCommentS("Reading input until we reach a termination condition"),
                                PT.IfThen(P.andX(PL.testNotPanicX(PT.Id pd), 
						 termCondX),
					  insTermChk(insLengthChk bdyS))]
		     end

                 (* -- Check if there was junk before trailing terminator *)
	         val trailingJunkChkSs = 
		     case termXOpt of NONE => []
                     | SOME (termX, _, _, NONE, _) => (PE.error "Expected a scan function"; [])
		     | SOME (termX, _, _, SOME termScan,_) => 
			 [P.mkCommentS("End of loop. Read trailing terminator if there was trailing junk."),
			  PT.IfThen(P.andX(PL.testNotPanicX(PT.Id pd),P.notX(PT.Id foundTerm)),
			   PT.Compound[
			   locBS,
		           PT.IfThenElse(
			     P.eqX(PL.PDC_OK,
				  PL.scanFunX(Atom.toString termScan, PT.Id pdc, 
					      termX, termX, P.trueX, P.zero,
					      P.zero)),
                             PT.Compound[
			      PT.IfThen(amCheckingBasicE NONE, 
			        PT.Compound[
				 recordArrayErrorS([locES],locX, PL.PDC_ARRAY_EXTRA_BEFORE_TERM,
						   true,readName,"",[],false),
				 P.assignS(PT.Id foundTerm, P.trueX)])],
			     recordArrayErrorS([locES],locX,PL.PDC_ARRAY_TERM_ERR, true, readName,
					       "Missing terminator.",[],true))
			 ])]

		 val readEORSs = if isRecord then genReadEOR (readName, reportStructErrorSs) () else []
                 (* -- Set data fields in canonical rep and ed from growable buffers *)
                 val setDataFieldsSs = 
                     [
		      P.assignS(fieldX(pd,length), fieldX(rep,length))
                     ]

                 (* -- Check array-level constriaints *)
                 (* -- -- Check that we read at least min elements, if min specified *)
                 fun genMinReachedConstraintSs minX =  
                     let val lengthTestX = P.ltX(fieldX(rep,length), minX)
			 val testX = if Option.isSome maxOpt 
			             then P.andX(P.notX(PT.Id reachedLimit), lengthTestX)
				     else lengthTestX
		     in
		      [P.mkCommentS("Checking that we read enough elements"),
		       PT.IfThen(testX,
			  recordArrayErrorS([tlocES],tLocX,PL.PDC_ARRAY_SIZE_ERR, true, readName,
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
			recordArrayErrorS([tlocES],tLocX,PL.PDC_ARRAY_USER_CONSTRAINT_ERR, true, readName,
					  ("User constraint for array "^name^" violated."), [], false))
                     ]]
                 val arrayConstraintsSs = 
		     let fun condWrapBase bdySs = 
			 [P.mkCommentS "Checking basic array constraints",
			  PT.IfThen(amCheckingBasicE(SOME(PL.testNotPanicX(PT.Id pd))),
				    PT.Compound bdySs)]
                         fun condWrapUser bdySs = 
			 [P.mkCommentS "Checking user-defined array constraints",
			  PT.IfThen(amCheckingForallE(SOME(PL.testNotPanicX(PT.Id pd))),
				    PT.Compound bdySs)]
                         fun condWrapBoth (bdySs1, bdySs2) = 
			 [PT.IfThen(PL.testNotPanicX(PT.Id pd),
				PT.Compound[P.mkCommentS "Checking basic array constraints",
				            PT.IfThen(amCheckingBasicE NONE,
					              PT.Compound bdySs1),
				            P.mkCommentS "Checking user-defined array constraints",
				            PT.IfThen(amCheckingForallE NONE,
					              PT.Compound bdySs2)])]
		     in
		       case (minOpt, arrayXOpt) of (NONE,NONE) => []
                       | (SOME minX, NONE) => condWrapBase(genMinReachedConstraintSs minX)
                       | (NONE, SOME r) => condWrapUser(genArrayConstraintSs r)
                       | (SOME minX, SOME r) =>
			   condWrapBoth(genMinReachedConstraintSs minX, genArrayConstraintSs r)
		     end

                 (* -- return value *)
                 val returnS = P.returnS (
				      P.condX(P.eqX(P.arrowX(PT.Id pd, PT.Id nerr), P.zero),
				      PL.PDC_OK, PL.PDC_ERROR))
	         (* -- Assemble read function array case *)
		 val bodySs =   [PT.Compound (
				  initSs 
                                @ sepTermDynamicCheck
                                @ chkBoundsSs
                                @ whileSs
				@ trailingJunkChkSs
				@ readEORSs
				@ setDataFieldsSs
				@ arrayConstraintsSs
                                @ [returnS])]
                 val readFunEDs = genReadFun(readName, cParams, mPCT,pdPCT,canonicalPCT, 
					     NONE, true, bodySs)
                 val _ = popLocalEnv()


                 val readEDs = initRepEDs @ initPDEDs @ cleanupRepEDs @ cleanupPDEDs
			     @ copyRepEDs @ copyPDEDs @ maskFunEDs @ readFunEDs

                 (***** array PADS-Galax *****)

    	         (* PDCI_node_t** fooArray_children(PDCI_node_t *self) *)
		 fun genGalaxArrayChildrenFun(name) =		
		     let val nodeRepTy = PL.nodeT
                         val returnName = PT.Id result
			 val returnTy = P.ptrPCT (P.ptrPCT (nodeRepTy))
                         val cnvName = childrenSuf name 
                         val paramNames = [self]
                         val paramTys = [P.ptrPCT nodeRepTy]
                         val formalParams =  List.map P.mkParam(ListPair.zip(paramTys, paramNames))
			 val elemName = lookupTy(baseTy,repSuf,#repname)
		         val index = "i"
			 val indexId = PT.Id index
		         val bodySs = headerGalaxChildrenFun(name) @
				      [P.varDeclS'(P.int, index)] @
				      ifGalaxChildren(returnName,P.plusX(fieldX(rep,length),P.intX 2), 
						      "ALLOC_ERROR: in " ^ cnvName) @
				      macroTNode(returnName,PL.PDCI_sequenced_pd,pd,PT.Id pd,cnvName) @
				      macroTNodeCall(returnName,P.intX 1,"PDC_uint32_val",length,
						     P.addrX(fieldX(rep,length)),cnvName) @
				      [P.mkCommentS "now do elements",
                                       PT.For(P.assignX(indexId,P.zero),
                                              P.ltX(indexId,fieldX(rep,length)),
                                              P.postIncX(indexId),
					      macroNodeCall(returnName,P.plusX(indexId,P.intX 2),elemName,elt,
							     getFieldX(m,element),P.addrX(P.subX(edBufferX,indexId)),
                                     			     P.addrX(P.subX(edBufferX,indexId)),cnvName)),
				       P.returnS (returnName)]
                          in   
                            P.mkFunctionEDecl(cnvName, formalParams, PT.Compound bodySs, returnTy)
                          end

	         val galaxEDs = [genGalaxArrayChildrenFun(name),
				 genGalaxVtable(name)]
	         

		 (* Generate Write function array case *)
		 val writeName = writeSuf name
		 val writeBaseName = (bufSuf o writeSuf) (lookupWrite baseTy) 
		 fun elemX base = P.addrX(P.subX(P.arrowX(PT.Id base, PT.Id elts), PT.Id "i"))
                 val writeBaseSs = writeFieldSs(writeBaseName, args @[elemX pd, elemX rep], true)
		 val writeSepSs = case sepXOpt of NONE => [] 
		                  | SOME(e,_, _,_,writeSep) => writeFieldSs(writeSep, [e], true)
		 val writeArraySs = [PT.Compound (
				     [P.varDeclS'(P.int, "i"),
				      PT.For(P.assignX(PT.Id "i",P.zero),
					     P.ltX(PT.Id "i", P.minusX(P.intX numElemsToTrack, P.intX 1)),
					     P.postIncX (PT.Id "i"),
					     PT.Compound (writeBaseSs @ writeSepSs) )] @ writeBaseSs)]
		 val writeTermSs = case termXOpt of NONE => []
		                   | SOME(e, _, _,_,writeTerm) => writeFieldSs(writeTerm, [e], true)
		 val bodySs = writeArraySs @ writeTermSs
		 val writeFunEDs = genWriteFuns(writeName, isRecord, cParams, pdPCT, canonicalPCT, bodySs)


                 (* Generate accumulator functions array case *) 
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
			 val theDeclSs = [P.varDeclS(PL.uint32, nerr, P.zero)]
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
			 val theDeclSs = [P.varDeclS(PL.uint32, nerr, P.zero), P.varDeclS'(PL.base_pdPCT, tpd)]
			 val initTpdSs = [P.assignS(P.dotX(PT.Id tpd, PT.Id errCode), 
						    P.arrowX(PT.Id pd, PT.Id errCode))]
                         val doElems = 
			     case lookupAcc baseTy of NONE => []
			   | SOME a => (
			       let val elemFunName = theSuf a
				   fun getArrayFieldX (base,field) = 
					P.addrX(P.subX(P.arrowX(PT.Id base, PT.Id field), PT.Id "i"))
				   fun doOne (accX,pdX,repX) = chkAddFun (elemFunName, accX,pdX,repX)
				   val doArrayDetailSs = [
					PT.Compound
					 [P.varDeclS'(P.int, "i"),
					  PT.For(P.assignX(PT.Id "i",P.zero),
						 P.ltX(PT.Id "i", P.arrowX(PT.Id rep, PT.Id length)),
						 P.postIncX (PT.Id "i"),
						 PT.Compound ([PT.IfThen(P.ltX(PT.Id "i", P.intX numElemsToTrack),
							       PT.Compound (doOne (getArrayFieldX(acc,arrayDetail), 
										   getArrayFieldX(pd,elts), 
										   getArrayFieldX(rep,elts))))]
							      @ (doOne (getFieldX(acc,array), 
								        getArrayFieldX(pd,elts), 
									getArrayFieldX(rep,elts))))
						 )]]
			       in
				   doArrayDetailSs
			       end(* end SOME acc case *))
			 val doLength = chkAddFun(theSuf PL.intAct, getFieldX(acc,length), P.addrX(PT.Id tpd), 
						  getFieldX(rep,length))
			 val theReturnS = genReturnChk (PT.Id nerr)
			 val theBodySs = theDeclSs @ initTpdSs @ doLength @ doElems @ [theReturnS]
			 val theFunED = genAddFun(theFun, accPCT, pdPCT, canonicalPCT, theBodySs)
		     in
			 theFunED
		     end

		 val initFunED = genResetInitCleanup  initSuf
		 val resetFunED = genResetInitCleanup resetSuf
		 val cleanupFunED = genResetInitCleanup cleanupSuf
                 val addFunED = genAdd()

		 (* -- generate accumulator report function array *)
		 (*  PDC_error_t T_acc_report (PDC_t* , T_acc* , const char* prefix  ) *)
                 fun genReport () = 
		     let val reportFun = (reportSuf o accSuf) name
			 val lengthX = P.arrowX(PT.Id acc, PT.Id length)
			 val doLengthSs = [chkPrint(
					     callIntPrint((ioSuf o reportSuf) PL.intAct, PT.String "Array lengths", 
						 	 PT.String "lengths", P.intX ~1, P.addrX lengthX)) ]
			 val maxX = P.dotX(lengthX, PT.Id "max")
			 val limitX = PT.QuestionColon(P.ltX(maxX,P.intX 10), maxX, P.intX 10)
						 
                         val doElems = 
			     case lookupAcc baseTy of NONE => []
			   | SOME a => (
			       let val elemFunName = reportSuf a
				   fun doOne (descriptor, prefixX, eX,extraArgXs) = 
					genPrintPiece (ioSuf elemFunName, descriptor, prefixX, eX,extraArgXs)
				   val fieldX = P.addrX(P.subX(P.arrowX(PT.Id acc, PT.Id arrayDetail), PT.Id "i"))
				   val doArrayDetailSs = [
					PT.Compound
					 [P.varDeclS'(P.int, "i"),
					  PT.For(P.assignX(PT.Id "i",P.zero),
						 P.ltX(PT.Id "i", limitX),
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

      		 val accumEDs = accED :: initFunED :: resetFunED :: cleanupFunED :: addFunED :: reportFunEDs

	     in
		   canonicalDecls
		 @ mStructDecls
                 @ pdStructDecls
		 @ (emitRead readEDs)
                 @ (emitAccum accumEDs)
                 @ (emitWrite writeFunEDs)
                 @ (emitXML galaxEDs)
	     end

	  fun cnvPEnum  {name:string, params: (pcty * pcdecr) list, 
			 isRecord, containsRecord, largeHeuristic, isFile,
			 members: (string * pcexp option * string option) list } =
	      let val baseTy = PL.strlit
		  val baseEM = mSuf baseTy
		  val basePD = pdSuf baseTy
                  fun mungeMembers (name, expOpt, commentOpt) = 
		      case expOpt of NONE => (name, PT.EmptyExpr, commentOpt)
 		                   | SOME e => (name, e, commentOpt)
		  val enumFields = List.map mungeMembers members

                  (* generate CheckSet mask *)
		  val baseMPCT = PL.base_mPCT
		  val mED      = P.makeTyDefEDecl (baseMPCT, mSuf name)
		  val mDecls   = cnvExternalDecl mED
		  val mPCT     = P.makeTypedefPCT (mSuf name)		

                  (* generate parse description *)
		  val baseEDPCT = PL.base_pdPCT
		  val pdED      = P.makeTyDefEDecl (baseEDPCT, pdSuf name)
		  val (pdDecls,pdTid) = cnvCTy pdED
		  val pdPCT     = P.makeTypedefPCT (pdSuf name)		

		  (* Generate accumulator type *)
		  val accED     = P.makeTyDefEDecl (PL.intAccPCT, accSuf name)
		  val accPCT    = P.makeTypedefPCT (accSuf name)		

                  (* Calculate and insert type properties into type table for enums. *)
                  val labels = List.map #1 enumFields
		  val ds = if List.length labels > 1 then
		              let val len = String.size (hd labels)
			      in
				  if List.all (fn s => len = String.size s) labels
				      then TyProps.mkSize (len, 0)
				  else TyProps.Variable
			      end
			   else TyProps.mkSize (0,0)
		  val numArgs = List.length params
                  val enumProps = buildTyProps(name,PTys.Enum, ds, TyProps.Enum ds,
					       TyProps.Static,true,isRecord,containsRecord,
					       largeHeuristic,isFile,pdTid, numArgs)
		  val () = PTys.insert(Atom.atom name, enumProps)

                  (* enums: generate canonical representation *)
		  val canonicalED = P.makeTyDefEnumEDecl(enumFields, repSuf name)
		  val (canonicalDecls,canonicalTid) = cnvRep(canonicalED, valOf (PTys.find (Atom.atom name)))
		  val canonicalPCT = P.makeTypedefPCT(repSuf name)

		  (* Generate enum to string function *)
		  val toStringEDs = [genEnumToStringFun(name, canonicalPCT, members)]

		   (* Generate Init function (enum case) *)
		   val initFunName = lookupMemFun (PX.Name name)
		   fun genInitEDs (suf, argName, aPCT) =  (* always static *)
		       [genInitFun(suf initFunName, argName, aPCT, [],true)]
		   val initRepEDs = genInitEDs (initSuf, rep, canonicalPCT)
		   val initPDEDs  = genInitEDs ((initSuf o pdSuf), pd, pdPCT)
		   val cleanupRepEDs = genInitEDs (cleanupSuf, rep, canonicalPCT)
		   val cleanupPDEDs  = genInitEDs ((cleanupSuf o pdSuf), pd, pdPCT)

                   (* Generate Copy Function enum case *)
		   fun genCopyEDs(suf, base, aPCT) = 
		       let val copyFunName = suf initFunName
			   val dst = dstSuf base
			   val src = srcSuf base
			   val bodySs = [PL.memcpyS(PT.Id dst, PT.Id src, P.sizeofX aPCT)]
		       in
			   [genCopyFun(copyFunName, dst, src, aPCT, bodySs,false)]
		       end
		   val copyRepEDs = genCopyEDs(copySuf o repSuf, rep, canonicalPCT)
		   val copyPDEDs  = genCopyEDs(copySuf o pdSuf,  pd,  pdPCT)


                  (* Generate m_init function enum case *)
                  val maskInitName = maskInitSuf name 
                  val maskFunEDs = genMaskInitFun(maskInitName, mPCT)


                  (* Generate read function *)
                  (* -- Some useful names *)
                  val readName = readSuf name
		  val baseReadFun = readSuf baseTy
		  fun readOneBranch (bname, bvalOpt, commentOpt) =
		      let val labelLenX = P.intX(String.size bname)
		      in
                         [P.assignS(P.dotX(PT.Id "strlit", PT.Id (PL.str)), PT.String bname),
			  P.assignS(P.dotX(PT.Id "strlit", PT.Id (PL.len)), labelLenX)]
                       @ PL.chkPtS(PT.Id pdc, readName)
                       @ [PT.IfThenElse(
			    PL.readFunChkX(PL.PDC_ERROR, baseReadFun, PT.Id pdc, 
						         PT.Id m, [], PT.Id pd,
						         P.addrX(PT.Id "strlit")),
			    PT.Compound (PL.restoreS(PT.Id pdc, readName)),
			    PT.Compound (  PL.commitS(PT.Id pdc, readName)
				         @ [P.assignS(P.starX (PT.Id rep), PT.Id bname),
					    P.assignS(PT.Id result, PL.PDC_OK),
					    PT.Goto (findEORSuf name)]))]
		      end
                  fun genReadBranches () = 
                      [P.varDeclS'(PL.stringPCT, "strlit"),
		       P.varDeclS'(PL.toolErrPCT, result)]
		      @ List.concat(List.map readOneBranch members)
		  val cleanupSs =  [P.mkCommentS("We didn't match any branch")]
			         @ reportErrorSs([locS],locX,false,
					PL.PDC_ENUM_MATCH_FAILURE,
					true, 
					readName,
					("Did not match any branch of enum "^name^"."),
					[])
			         @ [PL.setPanicS(PT.Id pd),
				    P.assignS(PT.Id result, PL.PDC_ERROR)]
		  val slurpToEORSs = if isRecord then genReadEOR (readName, reportBaseErrorSs) () else []
                  val gotoSs = [PT.Labeled(findEORSuf name,
					PT.Compound (slurpToEORSs @ [PT.Return (PT.Id result)]))]


		  (* -- Assemble read function *)
		  val _ = pushLocalEnv()                                        (* create new scope *)
		  val () = ignore (insTempVar(rep, P.ptrPCT canonicalPCT)) (* add modrep to scope *)
		  val cParams : (string * pcty) list = List.map mungeParam params
		  val () = ignore (List.map insTempVar cParams)  (* add params for type checking *)
		  val readFields = genReadBranches()                            (* does type checking *)
		  val _ = popLocalEnv()                                         (* remove scope *)
		  val bodySs = [PT.Compound(readFields @ cleanupSs @ gotoSs)]
		  val readFunEDs = genReadFun(readName, cParams, 
					      mPCT,pdPCT,canonicalPCT, NONE, false, bodySs)

                  val readEDs = toStringEDs @ initRepEDs @ initPDEDs @ cleanupRepEDs @ cleanupPDEDs
			     @ copyRepEDs @ copyPDEDs @ maskFunEDs @ readFunEDs




                  (* Generate Accumulator functions (enum case) *)
                  (* -- generate accumulator init, reset, and cleanup functions *)
		  fun genResetInitCleanup theSuf = 
		      let val theFun : string = (theSuf o accSuf) name
			  val theBodyE = PT.Call(PT.Id (theSuf PL.intAct),[PT.Id pdc, PT.Id acc])
                          val theReturnS = PT.Return theBodyE
			  val theFunED = gen3PFun(theFun, accPCT, [theReturnS])
			  in
			      theFunED
			  end
		   val initFunED = genResetInitCleanup initSuf
		   val resetFunED = genResetInitCleanup resetSuf
                   val cleanupFunED = genResetInitCleanup cleanupSuf

                   (* -- generate accumulator function *)
                   (*  PDC_error_t T_acc_add (PDC_t* , T_acc* , T_pd*, T* ) *)
		   val addFun = (addSuf o accSuf) name
		   val addX = PT.Call(PT.Id (addSuf PL.intAct), 
				      [PT.Id pdc, PT.Id acc, PT.Id pd, 
				       PT.Cast(P.ptrPCT PL.intPCT, PT.Id rep)])
		   val addReturnS = PT.Return addX
		   val addBodySs =  [addReturnS]
		   val addFunED = genAddFun(addFun, accPCT, pdPCT, canonicalPCT, addBodySs)

		   (* -- generate report function enum *)
		   (*  PDC_error_t T_acc_report (PDC_t* , T_acc* , const char* prefix ) *)
		   val reportFun = (reportSuf o accSuf) name
		   val reportFields = genEnumPrint((ioSuf o reportSuf o mapSuf) PL.intAct, "branchDistribution", 
						   PT.Id prefix, PT.Id what, PT.Id nst, 
						   PT.Id (toStringSuf name), PT.Id acc)
		   val reportFunEDs = genReportFuns(reportFun, "enum "^name, accPCT, reportFields)
		   val accumEDs = accED :: initFunED :: resetFunED :: cleanupFunED :: addFunED :: reportFunEDs
 
                  (* Generate Write functions (enum case) *)
		  val writeName = writeSuf name
		  val writeBaseName = lookupLitWrite PL.strlitWrite
                  val expX = PT.Call(PT.Id (toStringSuf name), [P.starX(PT.Id rep)])
		  val bodySs = writeFieldSs(writeBaseName, [expX], isRecord)
		  val writeFunEDs = genWriteFuns(writeName, isRecord, cParams, pdPCT, canonicalPCT, bodySs)

	          (***** enum PADS-Galax *****)

    	          (* PDCI_node_t** fooEnum_children(PDCI_node_t *self) *)
		  fun genGalaxEnumChildrenFun(name) =		
		      let val nodeRepTy = PL.nodeT
                          val returnName = PT.Id result
			  val returnTy = P.ptrPCT (P.ptrPCT (nodeRepTy))
                          val cnvName = childrenSuf name 
                          val paramNames = [self]
                          val paramTys = [P.ptrPCT nodeRepTy]
                          val formalParams =  List.map P.mkParam(ListPair.zip(paramTys, paramNames))
			  val enumType = P.ptrPCT(P.makeTypedefPCT name)
			  val baseType = P.ptrPCT(PL.base_pdPCT)
			  val Cstr = "Cstr"
			  val bodySs = [P.varDeclS'(P.charPtr,Cstr),
					P.varDeclS(enumType,rep,PT.Cast(enumType,fieldX(self,rep))),
					P.varDeclS(baseType,pd,PT.Cast(baseType,fieldX(self,pd))),
                                        P.varDeclS'(P.ptrPCT (P.ptrPCT nodeRepTy), result)] @
				        ifGalaxChildren(returnName,P.intX 2, "ALLOC_ERROR: in " ^ cnvName) @
					macroTNode(returnName,PL.PDC_base_pd,pd,PT.Id pd,cnvName) @
					[P.mkCommentS "string val child",
					P.assignS(PT.Id Cstr,PT.Cast(P.charPtr,PT.Call(PT.Id (toStringSuf name),
										       [P.starX (PT.Id rep)])))] @ 
											(* problem with *rep *)
					macroTNode(returnName,PL.PDCI_Cstr_val,"val",PT.Id Cstr,cnvName) @
					[P.returnS (returnName)]
                      in   
                        P.mkFunctionEDecl(cnvName, formalParams, PT.Compound bodySs, returnTy)
                      end

	          val galaxEDs = [genGalaxEnumChildrenFun(name),
				  genGalaxVtable(name)]


	      in
		  canonicalDecls
                @ mDecls
                @ pdDecls
                @ (emitRead readEDs)
                @ (emitAccum accumEDs)
                @ (emitWrite writeFunEDs)
                @ (emitXML galaxEDs)
	      end

	  fun cnvPSelect {tyName, varName, path} = 
	      let val (Select.Id root):: path = Select.sexprToPath(P.stripExp path)
		  val () = if root = varName then ()
			       else raise Fail ("Select name ("^varName^")and root of path expression ("^root^") don't match.")
		  val errS = "Request "^varName^" ill-typed."
		  fun getPos(tyName,path,accumSize,args) = 
		      let val cds = lookupCompoundDiskSize (PX.Name tyName)
			            handle Fail s => raise Fail (s^" Required for " ^varName^" request.")
fun printArgs [exp] = print ((P.expToString exp)^".\n")
  | printArgs (e::es) = (print ((P.expToString e)^", "); printArgs es)
  | printArgs [] = print "No arguments.\n"
val () = printArgs args
		      in
			  case cds
			  of TyProps.Base ds => 
(			      print "Base type:";
 print "orig size:";
			      TyProps.printSize (ds);
 print "reduced by args size:";
			      TyProps.printSize (reduceCDSize(args,ds));
			      print "Result: ";
			      TyProps.printSize (TyProps.add(accumSize, reduceCDSize(args,ds)));
(tyName, accumSize, reduceCDSize(args, ds)))
                          |  TyProps.Struct dsl => 
			      let val (f,path) = case path of ((Select.Dot f)::path) => (f,path) | _ => raise Fail errS
				  val () = print "In struct case\n"
				  fun findField ([], accum) = raise Fail errS
				    | findField((sOpt, diskSize)::ss, accum) = 
				      let val closedDS = reduceCDSize(args,diskSize)
				      in
				         case sOpt of NONE => findField(ss, TyProps.add(closedDS,accum)) (* literal*)
                                         | SOME (l,tyName,sargs:TyProps.argList) => 
					     if not (l = f) then 
						 (print ("did field "^l^".\nSize:"); 
						  TyProps.printSize 
						  (TyProps.add(closedDS,accum));
						 findField(ss, TyProps.add(closedDS,accum)))
					     else if (tyName = "Pcompute") then
						 raise Fail ("Ill-formed request: Computed field "^l^
							     " has no external representation.")
					     else (print ("found field "^l^".\nSize:"); 
							 TyProps.printSize  accum;
						  getPos(tyName,path,accum,reduceArgList(args,sargs)))
				      end
			      in
				  findField(dsl,accumSize)
			      end (* struct case *)
                          |  _ => raise Fail "Not yet implemented"
		      end
		  val (itemType, offset, size) = getPos(tyName, path, TyProps.mkSize(0,0),[](* top level must be closed *))
	      in
		((case offset
		  of TyProps.Variable => PE.error ("Location of "^varName^" request depends on data.\n") 
                  |  TyProps.Param(_) => PE.error ("Location of "^varName^" request depends on parameters.\n") 
                  |  TyProps.Size(location,nr) => 
		      (case size 
		       of TyProps.Variable => PE.error ("Size of "^varName^" request depends on data.\n") 
		       |  TyProps.Param(_) => PE.error ("Size of "^varName^" request depends on parameters.\n") 
                       |  TyProps.Size(n,nr) => 
			   Select.insert(Select.Select{tyName = itemType, offset = location, size = n})
		      (* end case size *))
	        (* end case offset*));
		[] (* return no AST decls *))
	      end handle Fail s => (PE.error s; [])

	  in
	      case decl 
	      of PX.PTypedef t => cnvPTypedef t
              |  PX.PStruct  s => cnvPStruct  s
              |  PX.PUnion   u => cnvPUnion   u
              |  PX.PArray   a => cnvPArray   a
              |  PX.PEnum    e => cnvPEnum    e
	      |  PX.PSelect  s => cnvPSelect  s
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




