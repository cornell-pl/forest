structure PLib =
struct
  structure PT = ParseTree
  structure P  = ParseTreeUtil

  val P_ERROR = PT.Id "P_ERR"
  val P_OK    = PT.Id "P_OK"

  val P_NO_ERROR                       = PT.Id "P_NO_ERR"
  val P_SKIPPED                        = PT.Id "P_SKIPPED"
  val P_NOT_PARSED                     = PT.Id "P_NOT_PARSED"

  val P_CHKPOINT_FAILURE               = PT.Id "P_CHKPOINT_ERR"
  val P_COMMIT_FAILURE                 = PT.Id "P_COMMIT_ERR"
  val P_RESTORE_FAILURE                = PT.Id "P_RESTORE_ERR"
  val P_ALLOC_FAILURE                  = PT.Id "P_ALLOC_ERR"

  val P_PANIC_SKIPPED                  = PT.Id "P_PANIC_SKIPPED"

  val P_USER_CONSTRAINT_VIOLATION      = PT.Id "P_USER_CONSTRAINT_VIOLATION"
  val P_MISSING_LITERAL                = PT.Id "P_MISSING_LITERAL"

  val P_ARRAY_ELEM_ERR                 = PT.Id "P_ARRAY_ELEM_ERR"
  val P_ARRAY_SEP_ERR                  = PT.Id "P_ARRAY_SEP_ERR"
  val P_ARRAY_TERM_ERR                 = PT.Id "P_ARRAY_TERM_ERR"
  val P_ARRAY_SIZE_ERR                 = PT.Id "P_ARRAY_SIZE_ERR"
  val P_ARRAY_SEP_TERM_SAME_ERR        = PT.Id "P_ARRAY_SEP_TERM_SAME_ERR"

  val P_ARRAY_USER_CONSTRAINT_ERR      = PT.Id "P_ARRAY_USER_CONSTRAINT_ERR"
  val P_ARRAY_MIN_BIGGER_THAN_MAX_ERR  = PT.Id "P_ARRAY_MIN_BIGGER_THAN_MAX_ERR"
  val P_ARRAY_MIN_NEGATIVE             = PT.Id "P_ARRAY_MIN_NEGATIVE"
  val P_ARRAY_MAX_NEGATIVE             = PT.Id "P_ARRAY_MAX_NEGATIVE"

  val P_ARRAY_EXTRA_BEFORE_SEP         = PT.Id "P_ARRAY_EXTRA_BEFORE_SEP"
  val P_ARRAY_EXTRA_BEFORE_TERM        = PT.Id "P_ARRAY_EXTRA_BEFORE_TERM"   
  

  val P_STRUCT_FIELD_ERR               = PT.Id "P_STRUCT_FIELD_ERR"
  val P_STRUCT_EXTRA_BEFORE_SEP        = PT.Id "P_STRUCT_EXTRA_BEFORE_SEP"
  val P_UNION_MATCH_FAILURE            = PT.Id "P_UNION_MATCH_ERR"
  val P_ENUM_MATCH_FAILURE             = PT.Id "P_ENUM_MATCH_ERR"
  val P_TYPEDEF_CONSTRAINT_ERR         = PT.Id "P_TYPEDEF_CONSTRAINT_ERR"

  val P_AT_EOF                         = PT.Id "P_AT_EOF"
  val P_AT_EOR                         = PT.Id "P_AT_EOR"
  val P_EXTRA_BEFORE_EOR               = PT.Id "P_EXTRA_BEFORE_EOR"
  val P_RANGE                          = PT.Id "P_RANGE"
  val P_INVALID_AINT                   = PT.Id "P_INVALID_AINT"
  val P_INVALID_AUINT                  = PT.Id "P_INVALID_AUINT"

  val P_CHAR_LIT_NOT_FOUND             = PT.Id "P_CHAR_LIT_NOT_FOUND"

  val P_INVALID_REGEXP                 = PT.Id "P_INVALID_REGEXP"

  val M_CHECK_AND_SET = PT.Id "P_CheckAndSet"
  val M_CHECK         = PT.Id "P_BothCheck"
  val M_IGNORE        = PT.Id "P_Ignore"

  val PlittleEndian = PT.Id "PlittleEndian"
  val PbigEndian    = PT.Id "PbigEndian"

  val ERROR_INFO  = PT.Id "P_LEV_INFO" 
  val ERROR_WARN  = PT.Id "P_LEV_WARN" 
  val ERROR_ERROR = PT.Id "P_LEV_ERR"
  val ERROR_FATAL = PT.Id "P_LEV_FATAL"

  val PDCI_MK_TNODE = PT.Id "PDCI_MK_TNODE"	
  val PDCI_MK_NODE = PT.Id "PDCI_MK_NODE"	
  val PDCI_NEW_NODE_PTR_LIST = PT.Id "PDCI_NEW_NODE_PTR_LIST"
  val nodeT = P.makeTypedefPCT "PDCI_node_t"
  val PDCI_structured_pd = "PDCI_structured_pd"
  val PDCI_sequenced_pd = "PDCI_sequenced_pd"
  val PDCI_cstr_val = "PDCI_cstr_val"
  val Pbase_pd = "Pbase_pd"
  val PDCI_error_typed_value = PT.Id "PDCI_error_typed_value"
  val PDCI_vtable_t = PT.TypedefName "PDCI_vtable_t"


  val toolErrPCT   = P.makeTypedefPCT "Perror_t"
  val toolStatePCT = P.makeTypedefPCT "P_t"
  val toolDiscPCT  = P.makeTypedefPCT "Pdisc_t"
  val locPCT       = P.makeTypedefPCT "Ploc_t"
  val posPCT       = P.makeTypedefPCT "Ppos_t"
  val errCodePCT   = P.makeTypedefPCT "PerrCode_t"
  val sizePCT      = P.makeTypedefPCT "size_t"
  val ssizePCT     = P.makeTypedefPCT "ssize_t"
  val regexpPCT    = P.makeTypedefPCT "Pregexp_t"

  val rbufferPCT   = P.makeTypedefPCT "RBuf_t"
  val rMMPCT       = P.makeTypedefPCT "RMM_t"

  val base_mPCT    = P.makeTypedefPCT "Pbase_m"
  val base_pdPCT   = P.makeTypedefPCT "Pbase_pd"
  val bytePCT      = P.makeTypedefPCT "Pbyte"
  val bytePtr      = P.ptrPCT(bytePCT)
  val VoidPtr      = P.ptrPCT(P.makeTypedefPCT "Void_t")
  val stringPCT    = P.makeTypedefPCT "Pstring"

  val charlit         = "Pchar_lit"
  val charlitMatch    = "Pchar_lit_match"
  val charlitScan1    = "Pchar_lit_scan1"
  val charlitScan2    = "Pchar_lit_scan2"
  val charlitWriteBuf = "Pchar_lit_write2buf"

  val cstrlit         = "Pcstr_lit"
  val cstrlitMatch    = "Pcstr_lit_match"
  val cstrlitScan1    = "Pcstr_lit_scan1"
  val cstrlitScan2    = "Pcstr_lit_scan2"
  val cstrlitWrite    = "Pcstr_lit"
  val cstrlitWriteBuf = "Pcstr_lit_write2buf"

  val strlit         = "Pstr_lit"
  val strlitMatch    = "Pstr_lit_match"
  val strlitScan1    = "Pstr_lit_scan1"
  val strlitScan2    = "Pstr_lit_scan2"
  val strlitWrite    = "Pstr_lit"
  val strlitWriteBuf = "Pstr_lit_write2buf"

  val reMatch    = "Pre_match"
  val reWriteBuf = cstrlitWriteBuf (* not yet implemented.  what should go here?*)
  val reScan1    = "Pre_scan1"
  val reScan2    = "Pre_scan2"

  val str          = "str"
  val len          = "len"
  val errorf       = "errorf"
  val disc         = "disc"
  val io_disc      = "io_disc"
  val outbuf       = "outbuf"
  val outBufLen    = "outbuf_len"
  val outBufRes    = "outbuf_res"

  val d_endian     = "d_endian"
  val m_endian     = "m_endian"
  val littleEndian = PlittleEndian
  val bigEndian    = PbigEndian
  val flags_t      = P.makeTypedefPCT "Pflags_t"

  val uint32PCT    = P.makeTypedefPCT "Puint32"
  val uint32Act    = "Puint32_acc"
  val uint32AccPCT = P.makeTypedefPCT "Puint32_acc"

  val intPCT       = P.makeTypedefPCT "Pint32"
  val intAct       = "Pint32_acc"
  val intAccPCT    = P.makeTypedefPCT "Pint32_acc"
  val intCvtPCT    = P.makeTypedefPCT "Pint32_map_fn"

  val sfioPCT      = P.ptrPCT (P.makeTypedefPCT "Sfio_t")

  fun fmtChar(chr:PT.expression) =
   (*  char*       P_fmtChar(char c); *)
   PT.Call(PT.Id "P_fmtChar", [chr])

  fun fmtStr s =
   PT.Call(PT.Id "P_fmt_cstr_n", [PT.String s, P.intX (String.size s)] )

(* error functions *)
  fun mkFName s = PT.String (s)
  fun trace(disc: PT.expression, msg:string) =
    PT.Expr(PT.Call(PT.Id "P_TRACE", [disc, PT.String msg]))

  fun warn(disc: PT.expression, msg :string) =
    PT.Expr(PT.Call(PT.Id "P_WARN", [disc, PT.String msg]))

  fun warnDefault(msg :string) =
    PT.Expr(PT.Call(PT.Id "P_WARN", [P.addrX (PT.Id "Pdefault_disc"), PT.String msg]))

  fun userErrorS(pads:PT.expression, loc:PT.expression, errCode:PT.expression, 
		 whatFunction: string, format:PT.expression, args:PT.expression list) = 
    PT.Expr(PT.Call(PT.Id "PDCI_report_err", [pads, ERROR_WARN, loc, errCode, 
					      mkFName whatFunction, format]@args))

  fun userInfoS(pads:PT.expression, loc:PT.expression,
                whatFunction: string,  format:PT.expression, args:PT.expression list) = 
    PT.Expr(PT.Call(PT.Id "PDCI_report_err", [pads, ERROR_INFO, loc, P_NO_ERROR, 
					      mkFName whatFunction , format]@args))

  fun userFatalErrorS(pads:PT.expression, loc:PT.expression, whatFunction: string,
                 errCode:PT.expression, format:PT.expression, args:PT.expression list) = 
    PT.Expr(PT.Call(PT.Id "PDCI_report_err", [pads, ERROR_FATAL, loc, errCode, mkFName whatFunction, format]@args))

  fun chkError(pads:PT.expression, argX :PT.expression, whatFun: string, errCode:PT.expression) =
    [ PT.IfThen(P.eqX(P_ERROR, argX),
       PT.Compound[
         userFatalErrorS(pads, P.zero, whatFun, errCode, P.zero, [])
       ])]

  fun chkIntErrorSs(pads:PT.expression, argX :PT.expression,  whatFun : string, errCode:PT.expression) =
     [PT.IfThen(P.neqX(P.zero, argX),
       PT.Compound[
         userFatalErrorS(pads, P.zero, whatFun, errCode, P.zero, [])
       ])]

  fun errAccReport(pads, outStrmX, prefixX, whatX, nstX, fieldX) = 
      PT.Call(PT.Id "P_nerr_acc_report2io",[pads, outStrmX, prefixX, whatX, nstX, fieldX])

(* Growable buffers *)
  fun zeroMM(pads:PT.expression) = 
    PT.Call(PT.Id "P_rmm_zero", [pads])
  fun nonZeroMM(pads:PT.expression) = 
    PT.Call(PT.Id "P_rmm_nozero", [pads])

  fun newRBufE(mm:PT.expression) = 
    PT.Call(PT.Id "RMM_new_rbuf", [mm])

  fun chkNewRBufS(whatFun, rBufV: PT.expression, zero: bool,
		    pads:PT.expression) = 
    let val rBufX = newRBufE(if zero then zeroMM(pads) else nonZeroMM(pads))
    in
	[PT.IfThen(
	  P.eqX(P.zero, rBufV),
	  PT.Compound
	   [ P.assignS(rBufV, rBufX),
	     PT.IfThen(P.eqX(P.zero, rBufV),
		       PT.Compound[
			  userFatalErrorS(pads, P.zero, whatFun, P_ALLOC_FAILURE, PT.String "", [])
				   ])])]
    end

  fun reserveE(pads:PT.expression, 
		rbuf:PT.expression, buf:PT.expression, sizeX:PT.expression, 
		numElementsX:PT.expression,growHintX:PT.expression) = 
   PT.Call(PT.Id "RBuf_reserve", 
	   [rbuf, PT.Cast(P.voidPtrPtr, buf), sizeX, numElementsX, growHintX ])

  fun chkReserveSs(pads, whatFun, rbuf,buf,sizeX,numElementsX,growHintX) = 
      let val reserveX = reserveE(pads,rbuf,buf,sizeX,numElementsX,growHintX)
      in
	  chkIntErrorSs(pads,reserveX,whatFun,P_ALLOC_FAILURE)
      end

  fun freeRBufferE(pads, prbuf:PT.expression, ppbuf:PT.expression) = 
     PT.Call(PT.Id "RMM_free_rbuf_keep_buf", [prbuf, PT.Cast(P.voidPtrPtr, ppbuf), P.zero])

  fun chkFreeRBufferS(pads, whatFun, prbuf:PT.expression, ppbuf:PT.expression) = 
    PT.IfThen(
      P.neqX(P.zero,freeRBufferE(pads, prbuf,ppbuf)),
      PT.Compound[
         userFatalErrorS(pads, P.zero, whatFun, P_ALLOC_FAILURE, 
			 PT.String "Couldn't free growable buffer.", [])]
    )

  fun cfreeRBufferE(prbuf:PT.expression) = 
   (* int       RMM_free_rbuf(RBuf_t* rbuf); *)
     PT.Call(PT.Id "RMM_free_rbuf", [prbuf])

  fun chkCFreeRBufferS(pads, whatFun, prbuf:PT.expression) = 
    PT.IfThen(
      P.neqX(P.zero,cfreeRBufferE(prbuf)),
      PT.Compound[
         userFatalErrorS(pads, P.zero, whatFun, P_ALLOC_FAILURE, 
			 PT.String "Couldn't free growable buffer", [])]
    )

  fun rbufCopyS(pdstRbuf, psrcRbuf, destX, size, mm) = 
      (*void RBuf_CPY_SRC2DEST(RBuf_t*, RBuf_t*, void * dest, size_t, RMM_t* ); *)
    PT.Expr(PT.Call(PT.Id "RBuf_CPY_SRC2DEST", [psrcRbuf, pdstRbuf, destX, size, mm]))

  fun discChecks(prefix) = 
    PT.Expr(PT.Call(PT.Id "PDCI_DISC_INIT_CHECKS", [prefix]))

  fun IODiscChecks(prefix) = 
    PT.Expr(PT.Call(PT.Id "PDCI_IODISC_INIT_CHECKS", [prefix]))

  fun IODiscChecks0P(prefix) = 
    PT.Expr(PT.Call(PT.Id "PDCI_IODISC_0P_CHECKS", [prefix]))

  fun IODiscChecks1P(prefix, p1) = 
    PT.Expr(PT.Call(PT.Id "PDCI_IODISC_1P_CHECKS", [prefix, p1]))

  fun IODiscChecks2P(prefix, p1, p2) = 
    PT.Expr(PT.Call(PT.Id "PDCI_IODISC_2P_CHECKS", [prefix, p1, p2]))

  fun IODiscChecks3P(prefix, p1, p2, p3) = 
    PT.Expr(PT.Call(PT.Id "PDCI_IODISC_3P_CHECKS", [prefix, p1, p2, p3]))

  fun IODiscChecks4P(prefix, p1, p2, p3, p4) = 
    PT.Expr(PT.Call(PT.Id "PDCI_IODISC_4P_CHECKS", [prefix, p1, p2, p3, p4]))

  fun IODiscChecksSizeRet0P(prefix) = 
    PT.Expr(PT.Call(PT.Id "PDCI_IODISC_0P_CHECKS_RET_SSIZE", [prefix]))

  fun IODiscChecksSizeRet1P(prefix, p1) = 
    PT.Expr(PT.Call(PT.Id "PDCI_IODISC_1P_CHECKS_RET_SSIZE", [prefix, p1]))

  fun IODiscChecksSizeRet2P(prefix, p1, p2) = 
    PT.Expr(PT.Call(PT.Id "PDCI_IODISC_2P_CHECKS_RET_SSIZE", [prefix, p1, p2]))

  fun IODiscChecksSizeRet3P(prefix, p1, p2, p3) = 
    PT.Expr(PT.Call(PT.Id "PDCI_IODISC_3P_CHECKS_RET_SSIZE", [prefix, p1, p2, p3]))

  fun IODiscChecksSizeRet4P(prefix, p1, p2, p3, p4) = 
    PT.Expr(PT.Call(PT.Id "PDCI_IODISC_3P_CHECKS_RET_SSIZE", [prefix, p1, p2, p3, p4]))

  fun nullCheck(prefix, ptrX) =
    PT.Expr(PT.Call(PT.Id "PDCI_NULLPARAM_CHECK", [prefix, PT.Cast(P.voidPtr, ptrX)]))

  fun discChecksSizeRet(prefix) = 
    PT.Expr(PT.Call(PT.Id "PDCI_DISC_INIT_CHECKS_RET_SSIZE", [prefix]))

  fun IODiscChecksSizeRet(prefix) = 
    PT.Expr(PT.Call(PT.Id "PDCI_IODISC_INIT_CHECKS_RET_SSIZE", [prefix]))

  fun nullCheckSizeRet(prefix, ptrX) =
    PT.Expr(PT.Call(PT.Id "PDCI_NULLPARAM_CHECK_RET_SSIZE", [prefix, PT.Cast(P.voidPtr, ptrX)]))

  fun incNestLevS(pads) =
    PT.Expr(P.postIncX (P.arrowX(pads, PT.Id "inestlev")))

  fun decNestLevS(pads) =
    PT.Expr(P.postDecX (P.arrowX(pads, PT.Id "inestlev")))

(* -- Parse state check/manipulation routines *)

  fun initParseStateS(pd) =
    PT.Expr(PT.Call(PT.Id "P_PS_init", [pd]))

  fun testPanicX(pd) =
    PT.Call(PT.Id "P_PS_isPanic", [pd])

  fun testNotPanicX(pd) =
    P.notX(PT.Call(PT.Id "P_PS_isPanic", [pd]))

  fun setPanicS(pd) =
    PT.Expr(PT.Call(PT.Id "P_PS_setPanic", [pd]))

  fun unsetPanicS(pd) =
    PT.Expr(PT.Call(PT.Id "P_PS_unsetPanic", [pd]))

(* -- Mask check/manipulation routines *)

  fun fillMaskS(maskPtr, mask, sizeTy) = 
    PT.Expr(PT.Call(PT.Id "PDCI_fill_mask", [PT.Cast(P.ptrPCT base_mPCT, maskPtr), mask, P.sizeofX sizeTy]))

  fun mTestSetX(m) =
    PT.Call(PT.Id "P_Test_Set", [m])

  fun mTestNotSetX(m) =
    PT.Call(PT.Id "P_Test_NotSet", [m])

  fun mTestSynCheckX(m) =
    PT.Call(PT.Id "P_Test_SynCheck", [m])

  fun mTestNotSynCheckX(m) =
    PT.Call(PT.Id "P_Test_NotSynCheck", [m])

  fun mTestSemCheckX(m) =
    PT.Call(PT.Id "P_Test_SemCheck", [m])

  fun mTestNotSemCheckX(m) =
    PT.Call(PT.Id "P_Test_NotSemCheck", [m])

  fun mTestWriteCheckX(m) =
    PT.Call(PT.Id "P_Test_WriteCheck", [m])

  fun mTestNotWriteCheckX(m) =
    PT.Call(PT.Id "P_Test_NotWriteCheck", [m])

  fun mTestCheckAndSetX(m) =
    PT.Call(PT.Id "P_Test_CheckAndSet", [m])

  fun mTestNotCheckAndSet(m) =
    PT.Call(PT.Id "P_Test_NotCheckAndSet", [m])

  fun mTestBothCheckX(m) =
    PT.Call(PT.Id "P_Test_BothCheck", [m])

  fun mTestNotBothCheck(m) =
    PT.Call(PT.Id "P_Test_NotBothCheck", [m])

  fun mTestIgnoreX(m) =
    PT.Call(PT.Id "P_Test_Ignore", [m])

  fun mTestNotIgnoreX(m) =
    PT.Call(PT.Id "P_Test_NotIgnore", [m])

(* -- File manipulation routines *)
  fun getLocS(pads:PT.expression, locAddr:PT.expression) = 
    PT.Expr(PT.Call(PT.Id "P_io_getLoc", [pads, locAddr, P.zero]))

  fun getLocBeginS(pads:PT.expression, locAddr:PT.expression) = 
    PT.Expr(PT.Call(PT.Id "P_io_getLocB", [pads, locAddr, P.zero]))

  fun getLocEndS(pads:PT.expression, locAddr:PT.expression, offset:int) = 
    PT.Expr(PT.Call(PT.Id "P_io_getLocE", [pads, locAddr,P.intX offset]))

  fun isEofX(pads:PT.expression) = 
    PT.Call(PT.Id "P_io_at_eof", [pads])

  fun isEorX(pads:PT.expression) = 
    PT.Call(PT.Id "P_io_at_eor", [pads])

(* check point routines *)
  fun chkPtS(pads:PT.expression, whatFun) =
    chkError(pads, (PT.Call(PT.Id "P_io_checkpoint", [pads, P.trueX])), (* always speculative *)
	     whatFun,
	     P_CHKPOINT_FAILURE)

  fun getSpecLevelX(pads:PT.expression) =
     PT.Call(PT.Id "P_spec_level", [pads])

  fun commitS(pads:PT.expression, whatFun) =
    chkError(pads, (PT.Call(PT.Id "P_io_commit", [pads])),
	     whatFun,
	     P_COMMIT_FAILURE)

  fun restoreS(pads:PT.expression, whatFun) =
    chkError(pads, (PT.Call(PT.Id "P_io_restore", [pads])),
	     whatFun,
	     P_RESTORE_FAILURE)

  fun matchFunX(n:string, pads:PT.expression, res:PT.expression, eatX) = 
      PT.Call(PT.Id n, [pads, res, eatX])

  fun matchFunChkX(expectedValX, n, pads, res,eatX) = 
      P.eqX(expectedValX, matchFunX(n,pads,res,eatX))

  fun readFunX(n:string, pads:PT.expression, loc:PT.expression, 
	                 optArgs: PT.expression list,
			 pd:PT.expression, 
	                 res:PT.expression) = 
      PT.Call(PT.Id n, [pads, loc] @ optArgs @[pd,res])

  fun readFunChkX(expectedValX : PT.expression,
		  n:string, pads:PT.expression, 
		  loc:PT.expression, optArgs:PT.expression list,
		  pd:PT.expression, 
	          res:PT.expression) = 
      P.eqX(expectedValX, readFunX(n,pads,loc,optArgs,pd,res))

  fun scan1FunX(n: string, pads: PT.expression, c: PT.expression,
	        eatLit: PT.expression, panic: PT.expression,
	        offset: PT.expression) = 
      PT.Call(PT.Id n, [pads,c,eatLit,panic,offset])

  fun scan2FunX(n: string, pads: PT.expression, c: PT.expression, s: PT.expression,
	        eatLit: PT.expression, eatStop: PT.expression, panic: PT.expression,
	        res: PT.expression, offset: PT.expression) = 
      PT.Call(PT.Id n, [pads,c,s,eatLit,eatStop,panic,res,offset])

  fun nstPrefixWhat(outstr, pnst, prefix, what) = 
      PT.Expr(PT.Call(PT.Id "PDCI_nst_prefix_what", [outstr, pnst, prefix, what]))


(* -- regexp routines *)
  fun regexpDeclNullS(regexp) = P.varDeclS(regexpPCT, regexp, PT.InitList[P.zero])
  fun regexpLitFromCharX(padsX, charX) = 
        PT.Call(PT.Id "P_RE_STRING_FROM_CHAR", [padsX, charX])
  fun regexpLitFromCStrX(padsX, strX) = 
        PT.Call(PT.Id "P_RE_STRING_FROM_CSTR", [padsX, strX])
  fun regexpCompileCStrX(padsX, regCstrX, regExpX, prefixX, whatFunX) = 
        PT.Call(PT.Id "PDCI_regexp_compile_cstr", [padsX, regCstrX, regExpX, prefixX, whatFunX ])
  fun regexpCleanupS(padsX, regexpX) = 
        PT.Expr(PT.Call(PT.Id "Pregexp_cleanup", [padsX, regexpX]))

(* -- reading/writing record functions *)
  fun IOReadNextRecX(pads, namp) = PT.Call(PT.Id "P_io_next_rec", [pads, namp])

  fun writeStartX(pads,io,bufLen,setBuf, whatFn) = 
      PT.Call(PT.Id "PDCI_io_write_start", [pads,io,bufLen,setBuf, whatFn])

  fun writeCommitX(pads,io,buf,setBuf, length, whatFn) = 
      PT.Call(PT.Id "PDCI_io_write_commit", [pads,io,buf,setBuf,length, whatFn])

  fun writeAbortX(pads,io,buf,setBuf, whatFn) = 
      PT.Call(PT.Id "PDCI_io_write_abort", [pads,io,buf,setBuf, whatFn])
  fun writeAbortS(pads,io,buf,setBuf,whatFn) = PT.Expr(writeAbortX(pads,io,buf,setBuf, whatFn))

  fun recOpenBufWrite(pads, bufCursor, bufLen, bufFull, whatFn) = 
      PT.Call(PT.Id "PDCI_io_rec_open_write2buf", [pads, bufCursor,bufLen,bufFull, whatFn])

  fun recCloseBufWrite(pads, bufCursor, bufLen, bufFull, buf, length, whatFn) = 
      PT.Call(PT.Id "PDCI_io_rec_close_write2buf", [pads, bufCursor,bufLen,bufFull,buf,length, whatFn])

(* -- Sfio functions *)
  val SF_LOCKR = PT.Id "SF_LOCKR"

  fun sfstrclose(str:PT.expression) = 
    PT.Expr(PT.Call(PT.Id "sfstrclose", [str]))

  val sfstropen = PT.Call(PT.Id "sfstropen", [])

  fun sfprintf (tmpstr :PT.expression, control:PT.expression, args : PT.expression list) =
      PT.Expr(PT.Call(PT.Id "sfprintf", tmpstr::control::args))

  fun sfstruse (tmpstr : PT.expression) = 
      PT.Call(PT.Id "sfstruse", [tmpstr])

  fun sfsetbufX (io, buf, flags) = 
      PT.Call(PT.Id "sfsetbuf", [io,PT.Cast(VoidPtr, buf),flags])
  fun sfsetbufS (io, buf, flags) = PT.Expr (sfsetbufX(io,buf,flags))

  fun sfreserveX (io, buf, flags) = PT.Cast(bytePtr, PT.Call(PT.Id "sfreserve", [io,buf,flags]))
  fun sfreserveS (io, buf, flags) = PT.Expr (sfreserveX(io,buf,flags))

  fun sfwriteX(ioX, bufX, lengthX) = PT.Call(PT.Id "sfwrite", [ioX, PT.Cast(VoidPtr, bufX), lengthX])
  fun sfwriteS(ioX, bufX, lengthX) = PT.Expr(sfwriteX(ioX, bufX, lengthX))

(* -- C helper functions *)
  fun bzeroX (spX, sizeX) = PT.Call(PT.Id "memset",[PT.Cast(P.voidPtr, spX), P.zero, sizeX])
  fun bzeroS (spX, sizeX) = PT.Expr(bzeroX(spX,sizeX))

  fun memcpyX (dstX, srcX, sizeX) = PT.Call(PT.Id "memcpy", 
					   [PT.Cast(P.voidPtr, dstX), 
					    PT.Cast(P.voidPtr, srcX), sizeX])
  fun memcpyS (dstX, srcX, sizeX) = PT.Expr(memcpyX(dstX, srcX,sizeX))
  fun strLen(s:PT.expression)= PT.Call(PT.Id "strlen", [s])
  fun strCmp(s1:PT.expression, s2: PT.expression )= PT.Call(PT.Id "strcmp", [s1,s2])

(* -- Other helper functions *)
  fun swapBytesS(exp) = PT.Expr(PT.Call(PT.Id "P_swap_bytes",
					[PT.Cast(P.ptrPCT bytePCT, P.addrX exp), PT.Cast(P.uint,P.sizeofEX exp)]))
  fun end2StringX(endian) = PT.Call(PT.Id "Pendian2str", [endian])
end