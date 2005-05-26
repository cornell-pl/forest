structure BuildUtils = struct
  structure PT   = ParseTree     (* the parse tree *)
  structure PX   = ParseTreeExt  (* Pads extensions *)
  structure P    = ParseTreeUtil (* Utility functions for manipulating the parse tree *)
  structure PL   = PLib          (* Information about values/functions available from pads library *)
  structure PBTys = PBaseTys     (* Information about the pads base types *)
  open PNames

  type pcexp = ParseTree.expression
  type pcty = ParseTree.ctype
  type pty = PX.Pty

  type pfieldty = 
      {pty : PX.Pty, 
       args : pcexp list, 
       name : string, 
       isVirtual : bool,
       isEndian : bool,
       isRecord : bool,
       containsRecord : bool,
       largeHeuristic : bool,
       pred : pcexp PX.PPostCond list option, 
       comment : string option,
       optPred : (pcexp PX.OptPredicate) option,
       optDecl : bool, 
       arrayDecl : bool,
       size : (pcexp PX.PSize) option,
       arraypred : (pcexp PX.PConstraint) list}


  type pmanty = 
      {tyname : pcty, 
       name : string, 
       args : pcexp list, 
       isVirtual : bool,
       expr : pcexp,
       pred : pcexp PX.PPostCond list option, 
       comment : string option}


  fun lookupTy (ty:pty, sufFun:string->string, fldSelect:PBTys.baseInfoTy ->Atom.atom) = 
      case ty 
	  of PX.Name s => ( case PBTys.find(PBTys.baseInfo, Atom.atom s) 
			   of NONE => (sufFun s)
			 | SOME (b:PBTys.baseInfoTy) => Atom.toString(fldSelect b))

  fun constraintToString arg = 
       (case arg of [] => ""
        | ((PX.General e)::rest) => (P.expToString e) ^ "&&" ^ constraintToString rest
        | ((PX.ParseCheck e) :: rest) => "ParseCheck("^P.expToString e^") &&" ^ constraintToString rest
       (* end case *))

  fun stringOptMerge (s1Opt: string option, s2Opt:string option) =
	case s1Opt
	of NONE => s2Opt
        | SOME s1 => (case s2Opt of NONE => SOME s1
			         | SOME s2 => SOME (s1 ^". " ^ s2))

  fun manComment (name, comment, expr, pred) =
	let val defStringOpt =  SOME(name^" = "^P.expToString expr)
	    val predStringOpt = Option.map constraintToString pred
	    val partialCommentOpt = stringOptMerge(comment,  defStringOpt)
	    val fullCommentOpt = stringOptMerge(partialCommentOpt, predStringOpt)
	in
	    fullCommentOpt
	end


  fun ifNotPanicSkippedSs (stmts) =
      [PT.IfThen(P.neqX(P.fieldX(pd, errCode), PL.P_PANIC_SKIPPED), PT.Compound(stmts))]

  fun ifNotPanicSkippedElseSs (stmts, elseStmts) =
      [PT.IfThenElse(P.neqX(P.fieldX(pd, errCode), PL.P_PANIC_SKIPPED), PT.Compound(stmts), PT.Compound(elseStmts))]


  val incNerrSs      = [PT.Expr(P.postIncX(PT.Id nerr))]
  val incPDNerrSs    = [PT.Expr(P.postIncX(P.fieldX(pd,nerr)))]
  val incNerrCompS   = PT.Compound(incNerrSs)
  val incPDNerrCompS = PT.Compound(incPDNerrSs)

  fun chk3Pfun (funName, e) = 
      [PT.IfThen(P.eqX(PL.P_ERROR, 
		       PT.Call(PT.Id funName, 
			       (PT.Id pads):: e)),
		 incNerrCompS)]

  fun callFun (theName, var, name)  = 
      let val fieldX = P.addrX(P.arrowX(PT.Id var, PT.Id name))
      in
	  chk3Pfun (theName, [fieldX])
      end


  fun chkAddFun (funName, accX, pdX, repX) = 
      [PT.IfThen(P.eqX(PL.P_ERROR, 
		       PT.Call(PT.Id funName, 
			       [PT.Id pads, accX, pdX, repX])),
		 incNerrCompS)]

  fun chkPrint (bodyX) = 		   
      PT.IfThen(
		P.eqX(PL.P_ERROR, bodyX), 
		PT.Compound[PL.sfstrclose(PT.Id tmpstr),
			    PT.Return PL.P_ERROR])


  fun genMan (isPadsTy, getPadsName) (f, defaultTyOpt, virtOK, {tyname, name, args, isVirtual, expr, pred, comment})= 
      if virtOK orelse not isVirtual then
	  let val fullCommentOpt = manComment(name, comment, expr, pred)
	      fun mkEntry ty = [(name, ty, fullCommentOpt)]
	  in
	      case isPadsTy tyname
		  of  PTys.CTy => (case defaultTyOpt of NONE => [] | SOME ty => mkEntry ty)
		|   _ => mkEntry (P.makeTypedefPCT(f (getPadsName tyname)))
	  end
      else []

  fun genFunMan (isPadsTy, getPadsName) (f, theSuf, var, {tyname, name, args, isVirtual, expr, pred, comment}) = 
      if isVirtual then [] else
         case isPadsTy tyname 
         of PTys.CTy => []
         | _ => callFun(theSuf (f (getPadsName tyname)), var, name)

  fun genAddMan (isPadsTy, getPadsName) (f, theSuf, var, tyname, name, isVirtual, errDescX) = 
      if isVirtual then [] else
         case isPadsTy tyname 
         of PTys.CTy => []
         | _ => chkAddFun (theSuf (f (getPadsName tyname)), P.getFieldX(var,name), errDescX, P.getFieldX(rep,name))


  fun reportErrorSs(locCodeSs, locX, shouldIncNerr, errCodeC, shouldPrint, funStr, msgStr, args) = 
      let val errCodeX = P.fieldX(pd, errCode)
	  val msgX = if msgStr = "" then P.zero else PT.String msgStr
	  val nErrSs = if shouldIncNerr 
			   then incPDNerrSs
		       else []
	  val printSs = if shouldPrint 
			    then [PL.userErrorS(PT.Id pads, locX, 
						errCodeX, funStr, msgX, args)]
			else []
      in
	  nErrSs
	  @ [P.assignS(P.fieldX(pd, errCode), errCodeC)]
	  @ locCodeSs
	  @ printSs
      end

  fun recordArrayErrorS (getLocSs, locX, errCodeC, shouldPrint, whatFun, msg, args, setPanic, endSpec) = 
      PT.Compound([PT.IfThenElse(P.notX(P.fieldX(pd, nerr)),
				 PT.Compound (reportErrorSs(getLocSs, locX, true, errCodeC,
							    shouldPrint, whatFun, msg, args)),
				 incPDNerrCompS)]
		  @ (case endSpec of SOME(esRetX) => [PL.endSpec pads esRetX] | _ => [])
		  @ (if setPanic then [PL.setPanicS(PT.Id pd)] else []))

  fun genReturnChk e =  P.returnS (P.condX(P.eqX(e, P.zero), PL.P_OK, PL.P_ERROR))
  val stdReturnS = genReturnChk (P.arrowX(PT.Id pd, PT.Id nerr))

  (*  Perror_t T_acc_name(P_t* , T_acc* ) *)
  fun gen3PFun (name, thePCT, var, bodySs) = 
      let val paramTys = [P.ptrPCT PL.toolStatePCT, P.ptrPCT thePCT]
	  val paramNames = [pads, var]
	  val formalParams = List.map P.mkParam (ListPair.zip (paramTys, paramNames))
	  val returnTy =  PL.toolErrPCT
	      
	  val zeroFunED = 
	      P.mkFunctionEDecl(name, formalParams, PT.Compound bodySs, returnTy)
      in
	  zeroFunED
      end

  (*  Perror_t T_acc_add (P_t* , T_acc* , T_pd*, T* ) *)
  fun genAddFun (addName, var, varPCT, pdPCT, repPCT, bodySs) = 
      let val paramTys = [P.ptrPCT PL.toolStatePCT, 
			  P.ptrPCT varPCT, 
			  P.ptrPCT pdPCT,
			  P.ptrPCT repPCT]
	  val paramNames = [pads, var, pd, rep]
	  val formalParams = List.map P.mkParam (ListPair.zip (paramTys, paramNames))
	  val returnTy =  PL.toolErrPCT
	  val addFunED = 
	      P.mkFunctionEDecl(addName, formalParams, PT.Compound bodySs, returnTy)
      in
	  addFunED
      end

    fun genInitTmpStrSs str = [P.varDeclS'(PL.sfioPCT, str),
			       PT.IfThen(P.notX(P.assignX(PT.Id str, PL.sfstropen)),
					 PT.Compound[PT.Return PL.P_ERROR])]

    (* Perror_t foostruct_report(P_t* pads, const char * prefix,
     const char* what, int nst, foostruct_acc* acc) *)

  fun genExternalReportFun(reportName, intlParamNames, formalParams, var) = 
      let val resDecl = P.varDeclS'(PL.toolErrPCT, result)
	  val initTmpStrSs = genInitTmpStrSs outstr
	  val chkTSandAccS = PT.IfThen(P.orX(
					     P.orX(P.notX(PT.Id pads), P.notX(PT.Id var)), 
					     P.notX PL.discX),
				       PT.Compound[PT.Return PL.P_ERROR])
	  val chkErrorFS = PT.IfThen(P.notX PL.errorFX, PT.Compound[PT.Return PL.P_OK])
	  val internalCallS = P.assignS(PT.Id result,
					PT.Call(PT.Id(ioSuf reportName),
						List.map PT.Id intlParamNames))
	  val reportS = PT.IfThen(P.eqX(PL.P_OK, PT.Id result),
				  PT.Compound[
					      PT.Expr(PT.Call(PL.errorFX,
							      [P.zero, P.zero, 
							       PT.String "%s", PL.sfstruse (PT.Id outstr)]))])
	  val closeSs = [PL.sfstrclose(PT.Id outstr), PT.Return (PT.Id result)]
	  val bodySs =  (resDecl :: initTmpStrSs) 
	                @ (chkTSandAccS :: chkErrorFS :: internalCallS :: reportS :: closeSs)
	  val returnTy =  PL.toolErrPCT
	  val reportFunED = P.mkFunctionEDecl(reportName, formalParams, PT.Compound bodySs, returnTy)
      in
	  reportFunED
      end



end