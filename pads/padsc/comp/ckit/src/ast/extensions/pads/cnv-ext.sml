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

	      (* Some useful functions *)
	      fun repSuf  s = s^"_rep"
              fun emSuf   s = s^"_em"
              fun edSuf   s = s^"_ed"
              fun readSuf s = s^"_read"
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

	      fun genReadFun (readName, emPCT,edPCT,canonicalPCT, emFirstPCT, bodySs) = 
		  let val paramList = List.map P.mkParam [(P.ptrPCT PL.toolStatePCT, ts),
							  (P.ptrPCT emPCT, em),
							  (P.ptrPCT edPCT, ed),
							  (P.ptrPCT canonicalPCT, rep),
							  (P.ptrPCT PL.toolDiscPCT, disc)]
		      val returnTy =  PL.toolErrPCT
		      val decls =   genLocTemp(canonicalPCT, rep, NONE) 
			          @ genLocTemp(emPCT, em, emFirstPCT) 
			          @ genLocTemp(edPCT, ed, NONE)
		      val initDecls = [genLocInit rep, genLocInit em, genLocInit ed,
				       P.assignS(P.arrowX(PT.Id(gMod(ed)), PT.Id nerr), P.zero),
				       P.assignS(P.arrowX(PT.Id(gMod(ed)), PT.Id panic), P.falseX)]
		      val bodySs' = decls @ initDecls @ bodySs
		      val bodyS = PT.Compound bodySs'
		      val readFunED = P.mkFunctionEDecl(readName, paramList, bodyS, returnTy)
		  in
		      readFunED
		  end

              fun reportErrorSs(errCodeC, shouldPrint, msg, args) = 
		  let val locX = P.addrX(fieldX(ed,loc))
                      val errCodeX = fieldX(ed,errCode)
		      val msgX = if msg = "" then P.zero else PT.String msg
                      val printSs = if shouldPrint 
				    then [PL.userErrorS(PT.Id ts, PT.Id disc, locX, 
							errCodeX, msgX, args)]
				    else []
		  in
                    [P.postIncS (fieldX(ed,nerr)),
                     P.assignS(fieldX(ed,errCode), errCodeC),
                     PL.getLocS(PT.Id ts,locX,PT.Id disc)]
                    @ printSs
		  end

	      fun cnvPStruct ({name:string, fields : (pcty, pcexp) PX.PSField list}) = 
	          let (* Functions for walking over lists of struct elements *)
		      fun mungeField f b (PX.Full fd) = f fd
                        | mungeField f b (PX.Brief e) = b e
		      fun mungeFields f b [] = []
			| mungeFields f b (x::xs) = (mungeField f b x) @ (mungeFields f b xs)

		      (* Generate canonical representation *)
		      fun genRepFull {pty :PX.Pty, name:string, pred:pcexp option, comment:string option} = 
			  let val predStringOpt = Option.map P.expToString pred
			      val fullCommentOpt = stringOptMerge(comment, predStringOpt)
			  in
			      [(name,P.makeTypedefPCT(lookupTy (pty,repSuf,#repname)), fullCommentOpt )]
			  end
		      fun genRepBrief e = []
		      val canonicalFields = mungeFields genRepFull genRepBrief fields
		      val canonicalStructED = P.makeTyDefStructEDecl (canonicalFields, repSuf name)
		      val canonicalDecls = cnvExternalDecl canonicalStructED 
                      val canonicalPCT = P.makeTypedefPCT (repSuf name)			 
		       
		      (* Generate error mask *)
		      fun genEMFull {pty :PX.Pty, name:string, pred:pcexp option, comment} = 
			  [(name,P.makeTypedefPCT(lookupTy (pty,emSuf,#emname)), NONE)]
		      fun genEMBrief e = []
		      val emFields = mungeFields genEMFull genEMBrief fields
		      val emFirstPCT = case emFields
                                       of [] => NONE
                                       | (f::fs) => let val ty = #2 f
							val aty = #1 (CTcnvType ty)
						    in
						       if Option.isSome(CTisEnum aty) 
							   then SOME ty else NONE
						    end
		      val emStructED = P.makeTyDefStructEDecl (emFields, emSuf name)
                      val emPCT = P.makeTypedefPCT (emSuf name)			  

		      (* Generate error description *)
		      fun genEDFull {pty :PX.Pty, name:string, pred:pcexp option, comment} = 
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
		      fun genReadFull {pty :PX.Pty, name:string, pred:pcexp option, comment} = 
			  let val readFieldName = lookupTy(pty, readSuf, #readname)
                              val modEdNameX = fieldX(ed,name)
                              val () = addSub(name, fieldX(rep,name))  (* record additional binding *)
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
						       PT.Id ts, (* aopt, *)
						       P.addrX(fieldX(em,name)),
						       P.addrX(fieldX(ed,name)),
						       P.addrX(fieldX(rep,name)), PT.Id disc)),
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
                                           let val exp = PTSub.substExps ((!subList), exp)
					       val () = expEqualTy(exp, CTintTys, 
								 fn s=> ("Constraint for field "^
								  name ^ " " ^
								  "does not have integer type."))
					   in
					       [(* if ((modem->name <= Check) && (!(exp))) *)
						PT.IfThen(
                                                 P.andX(P.lteX(fieldX(em,name), PL.EM_CHECK),
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
			  let val e = PTSub.substExps((!subList), e)
			      val (expTy, expAst) = cnvExpression e
			      val () = if CTisInt expTy then ()
				       else PE.error "Currently only characters supported as delimiters."
			      val commentS = P.mkCommentS ("Reading delimiter field: "^
						           (CExptoString expAst))
			      val pTyName = PL.charlit
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
								   e, P.zero, 
								   PT.Cast(P.ptrPCT P.uchar,P.zero),
                                                                   P.addrX (PT.Id "n"), 
								   PT.Id disc)),
						 (* moded->panic = false *)
						 PT.Compound[P.assignS(fieldX(ed,panic),P.falseX)])
                                              ], 
                                              PT.Compound elseSs
                                           )]
				        (* end SOME a *))
                                      (* end SOME b *))
			      val readFieldName = lookupTy(PX.Name pTyName, readSuf, #readname)
                              val notPanicSs = 
                                  [(* base_em tem = Check *)
				   P.varDeclS(PL.base_emPCT, tem, PL.EM_CHECK),
                                   PT.IfThen( (* PDC_ERROR == readFieldName(ts, &tem, &ted, e,disc) *)
                                             PL.readFunChkX(PL.PDC_ERROR, 
							    readFieldName, 
							    PT.Id ts, 
							    P.addrX (PT.Id tem),
							    P.addrX (PT.Id ted),
							    e, PT.Id disc),
                                     PT.Compound[
				       (* PDC_report_err(ts,disc, &ted.loc, 
					                   MISSING_LITERAL,"missing separator", e) *)
				       PL.userErrorS(PT.Id ts, PT.Id disc, 
						     P.addrX(P.dotX(PT.Id ted, PT.Id loc)),
						     PL.PDC_MISSING_LITERAL,
						     PT.String "Missing separator: %c.", [e]),
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
				     :: (genPanicRecovery pTyName notPanicSs))])]
			  end

                      (* -- Assemble read function *)
		      val _ = pushLocalEnv()                                        (* create new scope *)
		      val () = ignore (insTempVar(gMod rep, P.ptrPCT canonicalPCT)) (* add modrep to scope *)
		      val readFields = mungeFields genReadFull genReadBrief fields  (* does type checking *)
		      val _ = popLocalEnv()                                         (* remove scope *)
		      val returnS = P.returnS (
				      P.condX(P.eqX(P.arrowX(PT.Id (gMod(ed)), PT.Id nerr),P.zero),
				      PL.PDC_OK, PL.PDC_ERROR))
		      val bodySs = readFields @ [returnS]
		      val readFunED = genReadFun(readName, emPCT,edPCT,canonicalPCT, emFirstPCT, bodySs)
	      in 
 		   canonicalDecls (* converted earlier because used in typechecking constraints *)
                 @ cnvExternalDecl emStructED
                 @ cnvExternalDecl edStructED
                 @ cnvExternalDecl readFunED
	      end
	  
             fun cnvPArray {name:string, baseTy:PX.Pty, 
			    sizeSpec:pcexp PX.PSize option, constraints: pcexp PX.PConstraint list} =
	     let val length = "length"
		 val element = "element"
                 val array = "array"
                 val neerr = "neerr"
                 val firstError = "firstError"
                 val elemRepPCT = P.makeTypedefPCT(lookupTy(baseTy, repSuf, #repname))
                 val elemEdPCT  = P.makeTypedefPCT(lookupTy(baseTy, edSuf, #edname))
                 val elemEmPCT  = P.makeTypedefPCT(lookupTy(baseTy, emSuf, #emname))
                 val elemReadName = lookupTy(baseTy, readSuf, #readname)

                 (* Some useful functions *)
                 fun recordArrayErrorS (errCodeC, shouldPrint,msg,args, setPanic) = 
                     PT.Compound([
  		       PT.IfThenElse(P.notX(fieldX(ed,nerr)),
			  PT.Compound (reportErrorSs(errCodeC,shouldPrint,msg,args)),
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
				     val readFun = lookupTy(PX.Name pTyName, readSuf, #readname)
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
				     val readFun = lookupTy(PX.Name pTyName, readSuf, #readname)
				     val scanFun = lookupScan(PX.Name pTyName)
				 in
				   (NONE, SOME (exp,readFun,scanFun), NONE, NONE)
                                 end
                              (* end Term case *))
                              |  PX.Forall (r as {index,arrayName,body}) => (
                                 let val subList = [(length, fieldX(rep,length)), 
						    (arrayName, fieldX(rep,arrayName))]
				     val modBodyX = PTSub.substExps(subList, body)
				 in
				   pushLocalEnv();
				   ignore(insTempVar(index, P.int));
				   ignore(insTempVar(length, P.int));
				   ignore(insTempVar(arrayName, P.ptrPCT elemRepPCT)); 
				   expEqualTy(body, CTintTys, fn s=>("Forall expression for array "^
								   name ^" has type "^s^". Expected "^
								   "type int."));
				   popLocalEnv();
                                   (NONE,NONE,SOME {index=index, arrayName=arrayName, body=modBodyX}, NONE)
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
                          PL.readFunChkX(PL.PDC_OK, readFun, PT.Id ts, P.addrX(PT.Id tem),
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
						       P.addrX(edNext),
						       P.addrX(resNext),
						       PT.Id disc),
                          PT.Compound[
			   PT.IfThen(P.lteX(fieldX(em,element), PL.EM_CHECK),
			     PT.Compound[
                              PT.IfThen(P.notX(fieldX(ed,nerr)),
                                 PT.Compound (
	 			   (reportErrorSs(PL.PDC_ARRAY_ELEM_ERR, false, "", []))
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
						P.addrX (PT.Id tem), P.addrX (PT.Id ted),
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
                 fun genArrayConstraintSs {index:string, arrayName:string, body:PT.expression}  = 
		     [P.mkCommentS "Checking user's array constraint.",
                      PT.Compound[
                       P.varDeclS'(P.int, index),
                       P.varDeclS(P.int, "violated", P.falseX),
		       PT.For(P.assignX(PT.Id index, P.zero),
			      P.ltX(PT.Id index, fieldX(rep,length)),
			      P.postIncX(PT.Id index),
                              PT.Compound[
                               PT.IfThen(P.notX(body),
				 PT.Compound[
                                  P.assignS(PT.Id "violated", P.trueX),
				  PT.Break
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
                 val readFunED = genReadFun(readName, emPCT,edPCT,canonicalPCT, NONE, bodySs)

	     in
		   canonicalDecls
		 @ cnvExternalDecl emStructED
                 @ cnvExternalDecl edStructED
                 @ cnvExternalDecl readFunED
	     end
        

	  in
	      case decl 
	      of PX.PStruct s => cnvPStruct s
              |  PX.PArray  a => cnvPArray  a
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

