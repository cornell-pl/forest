structure Hist = struct
  structure PT   = ParseTree     (* the parse tree *)
  structure PX   = ParseTreeExt  (* Pads extensions *)
  structure P    = ParseTreeUtil (* Utility functions for manipulating the parse tree *)
  structure PE   = PError        (* Error reporting utilities *)
  structure PBTys = PBaseTys     (* Information about the pads base types *)
  structure PL   = PLib          (* Information about values/functions available from pads library *)
  structure PN = PNames
  structure BU = BuildUtils

  open PNames

  type pty = PX.Pty
  type pcexp = ParseTree.expression
  type pcty = ParseTree.ctype

  val hist = "hist"
  fun histSuf  s = s^"_"^hist

  fun lookupHist(ty:pty) = 
      case ty
	  of PX.Name s => histSuf
	      ( case PBTys.find(PBTys.baseInfo, Atom.atom s)
		    of NONE => s  (* non-base type; hist constructed from type name*)
		  |  SOME(b:PBTys.baseInfoTy) => s)

  fun genHistRepStruct ptyfuns name fields = 
      let fun genHistFull ({pty: PX.Pty, args: pcexp list, name: string, 
			    isVirtual: bool, isEndian: bool, 
			    isRecord, containsRecord, largeHeuristic: bool,
			    pred, comment: string option,...}: BU.pfieldty) = 
	  if not isVirtual then 
	      let val predStringOpt = Option.map BU.constraintToString pred
		  val fullCommentOpt = BU.stringOptMerge(comment, predStringOpt)
	      in
		  [(name, P.makeTypedefPCT (lookupHist pty), fullCommentOpt )]
	      end
	  else []
	  fun genHistBrief e = []
	  fun genHistMan ptyfuns m = BU.genMan ptyfuns (lookupHist, NONE, false, m)
	  val histFields = P.mungeFields genHistFull genHistBrief (genHistMan ptyfuns) fields
	  val histED = P.makeTyDefStructEDecl (histFields, histSuf name)
	  val histPCT = P.makeTypedefPCT (histSuf name)			 
      in
	  (histED, histPCT)
      end

  fun genWalkFunsStruct (ptyfuns, name, fields, thePCT, whichSuf) = 
      let val whichFun = (whichSuf o histSuf) name
	  fun genWhichFull ({pty: PX.Pty, name: string,isVirtual: bool, ...}:BU.pfieldty) = 
	      if isVirtual then []
	      else BU.callFun(whichSuf (lookupHist pty), hist, name)
          
	  fun genWhichBrief e = []
	  fun genWhichMan (ptyfuns, whichSuf) m = BU.genFunMan ptyfuns (lookupHist, whichSuf, hist, m)

	  val whichDeclSs = [P.varDeclS(PL.uint32PCT, nerr, P.zero)]
	  val whichFields = P.mungeFields genWhichFull genWhichBrief (genWhichMan (ptyfuns, whichSuf)) fields
	  val whichReturnS = BU.genReturnChk (PT.Id nerr)
	  val whichBodySs = whichDeclSs @ whichFields @ [whichReturnS]
	  val whichFunED = BU.gen3PFun(whichFun, thePCT, hist, whichBodySs)
      in
	  whichFunED
      end


  fun genAddFunStruct (ptyfuns, name, fields, histPCT, repPCT, pdPCT) = 
      let val addFun = (addSuf o histSuf) name
	  val addDeclSs = [P.varDeclS(PL.uint32PCT, nerr, P.zero),  P.varDeclS'(PL.base_pdPCT, tpd)]
	  val initTpdSs = [P.assignS(P.dotX(PT.Id tpd, PT.Id errCode), PL.P_NO_ERROR)]

	  fun genAddFull ({pty: PX.Pty, name: string,isVirtual: bool, ...}:BU.pfieldty) = 
	      if isVirtual then []
	      else BU.chkAddFun(addSuf (lookupHist pty), P.getFieldX(hist,name),  P.getFieldX(pd, name), P.getFieldX(rep,name))

	  fun genAddBrief e = []
	  fun genAddMan (ptyfuns as (isPadsTy, getPadsName)) ({tyname, name,isVirtual,...}:BU.pmanty) = 
	      if isVirtual then []
	      else case isPadsTy tyname 
                   of PTys.CTy => [] 
	     	    | _  => (let val pty = getPadsName tyname
			     in
				 BU.chkAddFun(addSuf(lookupHist pty), P.getFieldX(hist,name), P.addrX(PT.Id tpd), P.getFieldX(rep, name))
			     end)

	  val addFields = P.mungeFields genAddFull genAddBrief (genAddMan ptyfuns) fields
	  val addReturnS = BU.genReturnChk (PT.Id nerr)
	  val addBodySs = addDeclSs @ initTpdSs @ BU.ifNotPanicSkippedSs(addFields) @ [addReturnS]
	  val addFunED = BU.genAddFun(addFun, hist, histPCT, pdPCT, repPCT, addBodySs)
      in
	  addFunED
      end

  (* Perror_t Pint8_hist_report   (P_t *pads, Pint8_hist *h); *)
  (* Perror_t Pint8_hist_report2io(P_t *pads, Sfio_t * io, Pint8_hist *h); *)
  fun genReportFun (reportName, histPCT, intlBodySs) = 
      let fun genParamTys extraPCTs = [P.ptrPCT PL.toolStatePCT] @ extraPCTs @ [P.ptrPCT histPCT]
	  fun genParamNames extraNames = [pads] @ extraNames @ [ hist ]
	  val intlParamNames = genParamNames [outstr]
	  val extlFormalParams = List.map P.mkParam (ListPair.zip (genParamTys [], genParamNames []))
	  val intlFormalParams = List.map P.mkParam (ListPair.zip (genParamTys [PL.sfioPCT], intlParamNames))
	  val bodyS = PT.Compound intlBodySs
	  val returnTy = PL.toolErrPCT
	  val toioReportFunED = P.mkFunctionEDecl(ioSuf reportName, intlFormalParams, bodyS, returnTy)
	  val externalReportFunED = BU.genExternalReportFun(reportName, intlParamNames, extlFormalParams, hist)
      in
	  [toioReportFunED, externalReportFunED]
      end

  fun genReportFunStruct (ptyfuns, name, fields, histPCT) = 
      let val reportFun = (reportSuf o histSuf) name
	  val reportDeclSs = [P.varDeclS(PL.uint32PCT, nerr, P.zero)]
	  fun genReportFull ({pty: PX.Pty, name: string,isVirtual: bool, ...}:BU.pfieldty) = 
	      if isVirtual then []
	      else BU.chk3Pfun(ioSuf(reportSuf (lookupHist pty)), [PT.Id outstr, P.getFieldX(hist,name)])

	  fun genReportBrief e = []
	  fun genReportMan (ptyfuns as (isPadsTy, getPadsName)) ({tyname, name,isVirtual,...}:BU.pmanty) = 
	      if isVirtual then []
	      else case isPadsTy tyname 
                   of PTys.CTy => [] 
	     	    | _  => (let val pty = getPadsName tyname
			     in
				 BU.chk3Pfun(ioSuf (reportSuf(lookupHist pty)), [PT.Id outstr, P.getFieldX(hist,name)])
			     end)

	  val reportFields  = P.mungeFields genReportFull genReportBrief (genReportMan ptyfuns) fields
	  val reportReturnS = BU.genReturnChk (PT.Id nerr)
	  val reportBodySs  = reportDeclSs @ reportFields @ [reportReturnS]
	  val reportFunEDs  = genReportFun(reportFun, histPCT, reportBodySs)
      in
	  reportFunEDs
      end

  fun genHistStruct ptyfuns (name, fields, repPCT, pdPCT) = 
      let val (histED, histPCT) = genHistRepStruct ptyfuns name fields
	  val initFunED = genWalkFunsStruct (ptyfuns, name, fields, histPCT, initSuf)
	  val resetFunED = genWalkFunsStruct (ptyfuns, name, fields, histPCT, resetSuf)
	  val addFunED = genAddFunStruct(ptyfuns, name, fields, histPCT, repPCT, pdPCT)
	  val reportFunEDs = genReportFunStruct(ptyfuns, name, fields, histPCT)
	  val cleanupFunED = genWalkFunsStruct (ptyfuns, name, fields, histPCT, cleanupSuf)
      in
	  [histED, initFunED, resetFunED, addFunED] @ reportFunEDs @ [cleanupFunED]
      end

end
