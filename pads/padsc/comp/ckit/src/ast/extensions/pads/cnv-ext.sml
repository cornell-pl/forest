structure CnvExt : CNVEXT = struct

  structure PT   = ParseTree     (* the parse tree *)
  structure PX   = ParseTreeExt  (* Pads extensions *)
  structure P    = ParseTreeUtil (* Utility functions for manipulating the parse tree *)
  structure PE   = PError        (* Error reporting utilities *)
  structure PTys = PBaseTys      (* Information about the pads base types *)
  structure PL   = PLib          (* Information about values/functions available from pads library *)
  structure PTSub= ParseTreeSubst(* Function for subtituting an expression for a string in an expression *)

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
	 wrapDECL: Ast.coreExternalDecl -> Ast.externalDecl
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
			   wrapDECL}:coreConversionFuns) = 
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
    val getCoreType         = TU.getCoreType         ttab

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

(* Ctype *********************************************************************)

    val CTint = Ast.Numeric (Ast.NONSATURATE,Ast.WHOLENUM,Ast.SIGNED,Ast.INT,
			     Ast.SIGNDECLARED)

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

(* Conversions ***************************************************************)
      fun pcnvExternalDecl decl = 
	  let fun lookupTy (ty:pty, sufFun:string->string, fldSelect:PTys.baseInfoTy ->Atom.atom) = 
                  case ty 
                  of PX.Name s => ( case PTys.find(PTys.baseInfo, Atom.atom s) 
                                    of NONE => (sufFun s)
			            | SOME (b:PTys.baseInfoTy) => Atom.toString(fldSelect b))
	      fun repSuf  s = s^"_rep"
              fun emSuf   s = s^"_em"
              fun edSuf   s = s^"_ed"
              fun readSuf s = s^"_read"
              val nerr      = "nerr"
              val errCode   = "errCode"
              val loc       = "loc"
              val panic     = "panic"
	      val locPCT       = P.makeTypedefPCT "loc_t"
	      val toolErrPCT   = P.makeTypedefPCT "toolError_t"
	      val toolStatePCT = P.makeTypedefPCT "toolState_t"

	      fun cnvPStruct ({name:string, fields : (pcty, pcexp) PX.PSField list}) = 
	          let (* Functions for walking over lists of struct elements *)
		      fun mungeField f b (PX.Full fd) = f fd
                        | mungeField f b (PX.Brief e) = b e
		      fun mungeFields f b [] = []
			| mungeFields f b (x::xs) = (mungeField f b x) @ (mungeFields f b xs)

		      (* Generate canonical representation *)
		      fun genRepFull {pty :PX.Pty, name:string, pred:pcexp option} = 
			  [(name,P.makeTypedefPCT(lookupTy (pty,repSuf,#repname)))]
		      fun genRepBrief e = []
		      val canonicalFields = mungeFields genRepFull genRepBrief fields
		      val canonicalStructED = P.makeTyDefStructEDecl (canonicalFields, repSuf name)
		      val canonicalDecls = cnvExternalDecl canonicalStructED 
                      val canonicalPCT = P.makeTypedefPCT (repSuf name)			 
		       
		      (* Generate error mask *)
		      fun genEMFull {pty :PX.Pty, name:string, pred:pcexp option} = 
			  [(name,P.makeTypedefPCT(lookupTy (pty,emSuf,#emname)))]
		      fun genEMBrief e = []
		      val emFields = mungeFields genEMFull genEMBrief fields
		      val emStructED = P.makeTyDefStructEDecl (emFields, emSuf name)
                      val emPCT = P.makeTypedefPCT (emSuf name)			  

		      (* Generate error description *)
		      fun genEDFull {pty :PX.Pty, name:string, pred:pcexp option} = 
			  [(name,P.makeTypedefPCT(lookupTy (pty,edSuf,#edname)))]
		      fun genEDBrief e = []
		      val auxEDFields = [(nerr, P.int), (errCode, P.int),
					 (loc, locPCT), (panic, P.int)]
		      val edFields = auxEDFields @ (mungeFields genEDFull genEDBrief fields)
		      val edStructED = P.makeTyDefStructEDecl (edFields, edSuf name)
                      val edPCT = P.makeTypedefPCT (edSuf name)			  

                      (* Generate read function *)
                      (* -- Some useful names *)
		      val readName = readSuf name
		      val ts = "ts"
                      val em = "em" 
                      val ed = "ed"
		      val rep = "rep"
                      fun gTemp base = "tmp"^base
                      fun gMod  base = "mod"^base

                      (* -- collection of expressions to be substituted for in constraints *)
                      (* -- efficiency could be improved with better representations *)
                      val subList : (string * pcexp) list ref = ref []
                      fun addSub (a : string * pcexp) = subList := (a:: (!subList))

                      (* -- Some helper functions *)
		      fun fieldX (bsName, fName) = P.arrowX(PT.Id(gMod bsName), PT.Id fName)
			      
		      fun genReadFull {pty :PX.Pty, name:string, pred:pcexp option} = 
			  let val readFieldName = lookupTy(pty, readSuf, #readname)
                              val modEdNameX = fieldX(ed,name)
                              val () = addSub(name, fieldX(rep,name))  (* record additional binding *)
			      val readX = 
			      PT.IfThenElse
                                 (fieldX(ed,panic), (* if moded->panic *)
				  PT.Compound 
                                   [(* moded->name.panic = true *)
				    P.assignS(P.dotX(modEdNameX, PT.Id panic),P.trueX),  
				    (* moded->name.errCode = PANIC_SKIPPED *)
				    P.assignS(P.dotX(modEdNameX, PT.Id errCode),PL.PANIC_SKIPPED),  
                                    PL.getLocS(PT.Id ts,  (* get_location_info(ts, &moded->name.loc) *)
					       P.addrX(P.dotX(modEdNameX,PT.Id loc))),
				    (* moded->nerr += 1 *)
				    P.plusAssignS(fieldX (ed,nerr),P.intX 1)],
                                  (* if TOOL_ERROR = readFieldName(ts, &em->name, &ed->name, &res->name) *)
                                  PT.Compound [
                                   PT.IfThenElse
                                    (P.eqX(PL.TOOL_ERROR,
					   PT.Call(PT.Id readFieldName, 
						   [PT.Id ts, (* aopt, *)
						    P.addrX(fieldX(em,name)),
						    P.addrX(fieldX(ed,name)),
						    P.addrX(fieldX(rep,name))])),
				     PT.Compound[ (* error reading field *)
				      (* if (moded->name.panic) *)
                                      PT.IfThen(P.dotX(fieldX(ed, name), PT.Id panic),
 				        (* moded->panic = true *)
				        P.assignS(fieldX(ed,panic), P.trueX)),
                                      (* if (moded->nerr == 0) *)
                                      PT.IfThen(P.eqX(P.zero, fieldX(ed,nerr)), 
                                       PT.Compound [
					(* moded->errCode = STRUCT_FIELD_ERROR *)
                                        P.assignS(fieldX(ed, errCode), PL.STRUCT_FIELD_ERROR ),
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
                                           let val exp = PTSub.substExps ((!subList), exp)
					       val (expTy,expAst) = cnvExpression exp
					       val () = if isAssignable(CTint, expTy, NONE)
							then ()
							else PE.error ("Constraint for field "^
								       name ^ " " ^
								       "does not have integer type.")
					   in
					       [(* if ((modem->name <= Check) && (!(exp))) *)
						PT.IfThen(
                                                 P.andX(P.lteX(fieldX(em,name), PL.EM_CHECK),
							P.notX exp),
						 PT.Compound[
						   (* moded->name.errCode = STRUCT_FIELD_ERROR; *)
                                                   P.assignS(P.dotX(fieldX(ed,name), PT.Id errCode), 
								    PL.STRUCT_FIELD_ERROR),
						   (* get_location_info(st, &(moded->name.loc));  *)
					           PL.getLocS(PT.Id ts,
							      P.addrX(P.dotX(modEdNameX,PT.Id loc))),
                                                   (* if (USER_ERROR_FUN(ts)) *)
						   PT.IfThen(PL.userErrorF(PT.Id ts),
							     (* USER_ERROR_FUN(ts)(moded->name.loc,
							                           "constraint violated",
										   moded->name.errCode) *)
						    P.callS(PL.userErrorF(PT.Id ts),
							    [P.dotX(fieldX(ed,name), PT.Id loc),
							     PT.String ("User constraint on field "^
									name ^ " " ^
									"violated."),
							     P.dotX(fieldX(ed,name), PT.Id errCode)])),
						   (* if (0 == moded->nerr) *)
                                                   PT.IfThen(P.eqX(P.zero, fieldX(ed,nerr)),
						    PT.Compound[
						      (* moded->errCode = USER_CONSTRAINT_VIOLATION *)
                                                      P.assignS(fieldX(ed,errCode), 
								    PL.USER_CONSTRAINT_VIOLATION),
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
			      [readX]
			  end

		      fun genReadBrief e = 
			  let val e = PTSub.substExps((!subList), e)
			      val (expTy, expAst) = cnvExpression e
			      val () = if CTisInt expTy then ()
				       else PE.error "Currently only characters supported as delimiters."
			      val pTyName = "char_lit"
			      val ted = "ted"
			      val tem = "tem"
                              (* base_ed ted; *)
			      val tedDecl = P.varDeclS'(P.makeTypedefPCT "base_ed", ted)
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
						P.varDeclS(P.makeTypedefPCT ("base_em"), tem, PL.EM_IGNORE),
						PT.IfThen(
                                                 (* if TOOL_ERROR == ty_scan(ts, &tem, &ted, e) *)
						 P.neqX(PL.TOOL_ERROR,
						       PT.Call(PT.Id (Atom.toString a),
							       [PT.Id ts, 
								P.addrX(PT.Id tem),
								P.addrX(PT.Id ted),
								e])),
						 (* moded->panic = false *)
						 P.assignS(fieldX(ed,panic),P.falseX))
                                              ], 
                                              PT.Compound elseSs
                                           )]
				        (* end SOME a *))
                                      (* end SOME b *))
			      val readFieldName = lookupTy(PX.Name pTyName, readSuf, #readname)
                              val notPanicSs = 
                                  [(* base_em tem = Check *)
				   P.varDeclS(P.makeTypedefPCT ("base_em"), tem, PL.EM_CHECK),
                                   PT.IfThen( (* TOOL_ERROR == readFieldName(ts, &tem, &ted, 0) *)
                                     P.eqX(PL.TOOL_ERROR,
					   PT.Call(PT.Id readFieldName, 
						   [PT.Id ts, 
						    P.addrX (PT.Id tem),
						    P.addrX (PT.Id ted),
						    e])),
                                     PT.Compound[
				      PT.IfThen((* user_error_fun(ts) *)
					PL.userErrorF(PT.Id ts),
					(* user_error_fun(ts)(ted.loc, "missing separator", MISSING_LITERAL) *)
					P.callS(PL.userErrorF(PT.Id ts),
						[P.dotX(PT.Id ted, PT.Id loc),
						 PT.String ("Missing separator."),
						 PL.MISSING_LITERAL])),
				      PT.IfThen((* if (0 == moded->nerr) *)
                                       P.eqX(P.zero, fieldX(ed,nerr)),
					     PT.Compound[
				              (* moded->errCode = MISSING_LITERAL *)
					      P.assignS(fieldX(ed,errCode), 
							PL.MISSING_LITERAL),
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
			      [PT.Compound(tedDecl :: (genPanicRecovery pTyName notPanicSs))]
			  end

                      (* -- Assemble read function *)
		      val paramList = List.map P.mkParam [(P.ptrPCT toolStatePCT, ts),
							  (P.ptrPCT emPCT, em),
							  (P.ptrPCT edPCT, ed),
							  (P.ptrPCT canonicalPCT, rep)]
		      val returnTy =  toolErrPCT
                      fun genLocTemp (pcty, paramName) = 
                          [P.varDeclS(pcty, gTemp paramName, PT.InitList [P.zero]),
                           P.varDeclS(P.ptrPCT pcty, gMod paramName, PT.Id paramName)]
		      fun genLocInit paramName =
			  PT.IfThen(P.notX(PT.Id(gMod paramName)),
					 P.assignS(PT.Id (gMod paramName), P.addrX(PT.Id(gTemp paramName))))
                      val decls = genLocTemp(canonicalPCT, rep) @ 
			          genLocTemp(emPCT, em) @
                                  genLocTemp(edPCT, ed)
                      val initDecls = [genLocInit rep, genLocInit em, genLocInit ed,
				       P.assignS(P.arrowX(PT.Id(gMod(ed)), PT.Id nerr), P.zero),
				       P.assignS(P.arrowX(PT.Id(gMod(ed)), PT.Id panic), P.falseX)]
		      val _ = pushLocalEnv()                                        (* create new scope *)
		      val () = ignore (insTempVar(gMod rep, P.ptrPCT canonicalPCT)) (* add modrep to scope *)
		      val readFields = mungeFields genReadFull genReadBrief fields  (* does type checking *)
		      val _ = popLocalEnv()                                         (* remove scope *)
		      val returnS = P.returnS (P.eqX(P.arrowX(PT.Id (gMod(ed)), PT.Id nerr),P.zero))
		      val bodySs = decls @ initDecls @ readFields @ [returnS]
                      val bodyS = PT.Compound bodySs
		      val readFunED = P.mkFunctionEDecl(readName, paramList, bodyS, returnTy)
	      in 
 		   canonicalDecls (* converted earlier because used in typechecking constraints *)
                 @ cnvExternalDecl emStructED
                 @ cnvExternalDecl edStructED
                 @ cnvExternalDecl readFunED
	      end
	  in
	      case decl of PX.PStruct s => cnvPStruct s
	  end
      in
	  {CNVExp = CNVExp,
	   CNVStat = CNVStat,
	   CNVBinop = CNVBinop,
	   CNVUnop = CNVUnop,
	   CNVExternalDecl = pcnvExternalDecl,
	   CNVSpecifier = CNVSpecifier,
	   CNVDeclarator = CNVDeclarator,
	   CNVDeclaration = CNVDeclaration}
      end
end
