structure PLib =
struct
  structure PT = ParseTree
  structure P  = ParseTreeUtil

  val PDC_ERROR = PT.Id "PDC_ERR"
  val PDC_OK    = PT.Id "PDC_OK"

  val PDC_NO_ERROR                       = PT.Id "PDC_NO_ERR"
  val PDC_SKIPPED                        = PT.Id "PDC_SKIPPED"
  val PDC_NOT_PARSED                     = PT.Id "PDC_NOT_PARSED"

  val PDC_CHKPOINT_FAILURE               = PT.Id "PDC_CHKPOINT_ERR"
  val PDC_COMMIT_FAILURE                 = PT.Id "PDC_COMMIT_ERR"
  val PDC_RESTORE_FAILURE                = PT.Id "PDC_RESTORE_ERR"
  val PDC_ALLOC_FAILURE                  = PT.Id "PDC_ALLOC_ERR"

  val PDC_PANIC_SKIPPED                  = PT.Id "PDC_PANIC_SKIPPED"

  val PDC_USER_CONSTRAINT_VIOLATION      = PT.Id "PDC_USER_CONSTRAINT_VIOLATION"
  val PDC_MISSING_LITERAL                = PT.Id "PDC_MISSING_LITERAL"

  val PDC_ARRAY_ELEM_ERR                 = PT.Id "PDC_ARRAY_ELEM_ERR"
  val PDC_ARRAY_SEP_ERR                  = PT.Id "PDC_ARRAY_SEP_ERR"
  val PDC_ARRAY_TERM_ERR                 = PT.Id "PDC_ARRAY_TERM_ERR"
  val PDC_ARRAY_SIZE_ERR                 = PT.Id "PDC_ARRAY_SIZE_ERR"

  val PDC_ARRAY_USER_CONSTRAINT_ERR      = PT.Id "PDC_ARRAY_USER_CONSTRAINT_ERR"
  val PDC_ARRAY_MIN_BIGGER_THAN_MAX_ERR  = PT.Id "PDC_ARRAY_MIN_BIGGER_THAN_MAX_ERR"
  val PDC_ARRAY_MIN_NEGATIVE             = PT.Id "PDC_ARRAY_MIN_NEGATIVE"
  val PDC_ARRAY_MAX_NEGATIVE             = PT.Id "PDC_ARRAY_MAX_NEGATIVE"

  val PDC_ARRAY_EXTRA_BEFORE_SEP         = PT.Id "PDC_ARRAY_EXTRA_BEFORE_SEP"
  val PDC_ARRAY_EXTRA_BEFORE_TERM        = PT.Id "PDC_ARRAY_EXTRA_BEFORE_TERM"   

  val PDC_STRUCT_FIELD_ERR               = PT.Id "PDC_STRUCT_FIELD_ERR"
  val PDC_STRUCT_EXTRA_BEFORE_SEP        = PT.Id "PDC_STRUCT_EXTRA_BEFORE_SEP"
  val PDC_UNION_MATCH_FAILURE            = PT.Id "PDC_UNION_MATCH_ERR"
  val PDC_ENUM_MATCH_FAILURE             = PT.Id "PDC_ENUM_MATCH_ERR"
  val PDC_TYPEDEF_CONSTRAINT_ERR         = PT.Id "PDC_TYPEDEF_CONSTRAINT_ERR"

  val PDC_AT_EOF                         = PT.Id "PDC_AT_EOF"
  val PDC_AT_EOR                         = PT.Id "PDC_AT_EOR"
  val PDC_EXTRA_BEFORE_EOR               = PT.Id "PDC_EXTRA_BEFORE_EOR"
  val PDC_RANGE                          = PT.Id "PDC_RANGE"
  val PDC_INVALID_AINT                   = PT.Id "PDC_INVALID_AINT"
  val PDC_INVALID_AUINT                  = PT.Id "PDC_INVALID_AUINT"

  val PDC_CHAR_LIT_NOT_FOUND             = PT.Id "PDC_CHAR_LIT_NOT_FOUND"

  val M_CHECK_AND_SET = PT.Id "PDC_CheckAndSet"
  val M_CHECK         = PT.Id "PDC_BothCheck"
  val M_IGNORE        = PT.Id "PDC_Ignore"

  val PDC_littleEndian = PT.Id "PDC_littleEndian"
  val PDC_bigEndian    = PT.Id "PDC_bigEndian"

  val ERROR_INFO  = PT.Id "PDC_LEV_INFO" 
  val ERROR_WARN  = PT.Id "PDC_LEV_WARN" 
  val ERROR_ERROR = PT.Id "PDC_LEV_ERR"
  val ERROR_FATAL = PT.Id "PDC_LEV_FATAL"

  val PDCI_MK_TNODE = PT.Id "PDCI_MK_TNODE"	
  val PDCI_MK_NODE = PT.Id "PDCI_MK_NODE"	
  val PDCI_NEW_NODE_PTR_LIST = PT.Id "PDCI_NEW_NODE_PTR_LIST"
  val nodeT = P.makeTypedefPCT "PDCI_node_t"
  val PDCI_structured_pd = "PDCI_structured_pd"
  val PDCI_sequenced_pd = "PDCI_sequenced_pd"
  val PDCI_Cstr_val = "PDCI_Cstr_val"
  val PDC_base_pd = "PDC_base_pd"
  val PDCI_error_typed_value = PT.Id "PDCI_error_typed_value"
  val PDCI_vtable_t = PT.TypedefName "PDCI_vtable_t"

  val toolErrPCT   = P.makeTypedefPCT "PDC_error_t"
  val toolStatePCT = P.makeTypedefPCT "PDC_t"
  val toolDiscPCT  = P.makeTypedefPCT "PDC_disc_t"
  val locPCT       = P.makeTypedefPCT "PDC_loc_t"
  val errCodePCT   = P.makeTypedefPCT "PDC_errCode_t"
  val sizePCT      = P.makeTypedefPCT "size_t"
  val ssizePCT     = P.makeTypedefPCT "ssize_t"

  val rbufferPCT   = P.makeTypedefPCT "RBuf_t"
  val rMMPCT       = P.makeTypedefPCT "RMM_t"

  val base_mPCT    = P.makeTypedefPCT "PDC_base_m"
  val base_pdPCT   = P.makeTypedefPCT "PDC_base_pd"
  val bytePCT      = P.makeTypedefPCT "PDC_byte"
  val bytePtr      = P.ptrPCT(bytePCT)
  val VoidPtr      = P.ptrPCT(P.makeTypedefPCT "Void_t")

  val charlit      = "PDC_a_char_lit"
  val strlit       = "PDC_a_str_lit"
  val strlitWrite  = "PDC_a_Cstr_lit"
  val stringPCT    = P.makeTypedefPCT "PDC_string"
  val str          = "str"
  val len          = "len"
  val errorf       = "errorf"
  val disc         = "disc"
  val io_disc      = "io_disc"
  val outbuf       = "outbuf"
  val outBufLen    = "outbuf_len"
  val outBufRes    = "outbuf_res"

  val prefix       = "PDC_"
 
  val d_endian     = "d_endian"
  val m_endian     = "m_endian"
  val littleEndian = PDC_littleEndian
  val bigEndian    = PDC_bigEndian
  val intAct       = "PDC_int32_acc"
  val intCvtPCT    = P.makeTypedefPCT "PDC_int32_map_fn"
  val intAccPCT    = P.makeTypedefPCT "PDC_int32_acc"
  val intPCT       = P.makeTypedefPCT "PDC_int32"
  val sfioPCT      = P.ptrPCT (P.makeTypedefPCT "Sfio_t")

  fun fmtChar(chr:PT.expression) =
   (*  char*       PDC_fmtChar(char c); *)
   PT.Call(PT.Id "PDC_fmtChar", [chr])

  fun fmtStr s =
   PT.Call(PT.Id "PDC_fmt_Cstr", [PT.String s, P.intX (String.size s)] )

(* error functions *)
  fun mkFName s = PT.String (s)
  fun trace(disc: PT.expression, msg:string) =
    PT.Expr(PT.Call(PT.Id "PDC_TRACE", [disc, PT.String msg]))

  fun warn(disc: PT.expression, msg :string) =
    PT.Expr(PT.Call(PT.Id "PDC_WARN", [disc, PT.String msg]))

  fun warnDefault(msg :string) =
    PT.Expr(PT.Call(PT.Id "PDC_WARN", [P.addrX (PT.Id "PDC_default_disc"), PT.String msg]))

  fun userErrorS(pdc:PT.expression, loc:PT.expression, errCode:PT.expression, 
		 whatFunction: string, format:PT.expression, args:PT.expression list) = 
    PT.Expr(PT.Call(PT.Id "PDCI_report_err", [pdc, ERROR_WARN, loc, errCode, 
					      mkFName whatFunction, format]@args))

  fun userInfoS(pdc:PT.expression, loc:PT.expression,
                whatFunction: string,  format:PT.expression, args:PT.expression list) = 
    PT.Expr(PT.Call(PT.Id "PDCI_report_err", [pdc, ERROR_INFO, loc, PDC_NO_ERROR, 
					      mkFName whatFunction , format]@args))

  fun userFatalErrorS(pdc:PT.expression, loc:PT.expression, whatFunction: string,
                 errCode:PT.expression, format:PT.expression, args:PT.expression list) = 
    PT.Expr(PT.Call(PT.Id "PDCI_report_err", [pdc, ERROR_FATAL, loc, errCode, mkFName whatFunction, format]@args))

  fun chkError(pdc:PT.expression, argX :PT.expression, whatFun: string, errCode:PT.expression) =
    [ PT.IfThen(P.eqX(PDC_ERROR, argX),
       PT.Compound[
         userFatalErrorS(pdc, P.zero, whatFun, errCode, P.zero, [])
       ])]

  fun chkIntErrorSs(pdc:PT.expression, argX :PT.expression,  whatFun : string, errCode:PT.expression) =
     [PT.IfThen(P.neqX(P.zero, argX),
       PT.Compound[
         userFatalErrorS(pdc, P.zero, whatFun, errCode, P.zero, [])
       ])]

  fun errAccReport(pdc, outStrmX, prefixX, whatX, nstX, fieldX) = 
      PT.Call(PT.Id "PDC_nerr_acc_report2io",[pdc, outStrmX, prefixX, whatX, nstX, fieldX])

(* Growable buffers *)
  fun zeroMM(pdc:PT.expression) = 
    PT.Call(PT.Id "PDC_rmm_zero", [pdc])
  fun nonZeroMM(pdc:PT.expression) = 
    PT.Call(PT.Id "PDC_rmm_nozero", [pdc])

  fun newRBufE(mm:PT.expression) = 
    PT.Call(PT.Id "RMM_new_rbuf", [mm])

  fun chkNewRBufS(whatFun, rBufV: PT.expression, zero: bool,
		    pdc:PT.expression) = 
    let val rBufX = newRBufE(if zero then zeroMM(pdc) else nonZeroMM(pdc))
    in
	[PT.IfThen(
	  P.eqX(P.zero, rBufV),
	  PT.Compound
	   [ P.assignS(rBufV, rBufX),
	     PT.IfThen(P.eqX(P.zero, rBufV),
		       PT.Compound[
			  userFatalErrorS(pdc, P.zero, whatFun, PDC_ALLOC_FAILURE, PT.String "", [])
				   ])])]
    end

  fun reserveE(pdc:PT.expression, 
		rbuf:PT.expression, buf:PT.expression, sizeX:PT.expression, 
		numElementsX:PT.expression,growHintX:PT.expression) = 
   PT.Call(PT.Id "RBuf_reserve", 
	   [rbuf, PT.Cast(P.voidPtrPtr, buf), sizeX, numElementsX, growHintX ])

  fun chkReserveSs(pdc, whatFun, rbuf,buf,sizeX,numElementsX,growHintX) = 
      let val reserveX = reserveE(pdc,rbuf,buf,sizeX,numElementsX,growHintX)
      in
	  chkIntErrorSs(pdc,reserveX,whatFun,PDC_ALLOC_FAILURE)
      end

  fun freeRBufferE(pdc, prbuf:PT.expression, ppbuf:PT.expression) = 
     PT.Call(PT.Id "RMM_free_rbuf_keep_buf", [prbuf, PT.Cast(P.voidPtrPtr, ppbuf), P.zero])

  fun chkFreeRBufferS(pdc, whatFun, prbuf:PT.expression, ppbuf:PT.expression) = 
    PT.IfThen(
      P.neqX(P.zero,freeRBufferE(pdc, prbuf,ppbuf)),
      PT.Compound[
         userFatalErrorS(pdc, P.zero, whatFun, PDC_ALLOC_FAILURE, 
			 PT.String "Couldn't free growable buffer.", [])]
    )

  fun cfreeRBufferE(prbuf:PT.expression) = 
   (* int       RMM_free_rbuf(RBuf_t* rbuf); *)
     PT.Call(PT.Id "RMM_free_rbuf", [prbuf])

  fun chkCFreeRBufferS(pdc, whatFun, prbuf:PT.expression) = 
    PT.IfThen(
      P.neqX(P.zero,cfreeRBufferE(prbuf)),
      PT.Compound[
         userFatalErrorS(pdc, P.zero, whatFun, PDC_ALLOC_FAILURE, 
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

  fun incNestLevS(pdc) =
    PT.Expr(P.postIncX (P.arrowX(pdc, PT.Id "inestlev")))

  fun decNestLevS(pdc) =
    PT.Expr(P.postDecX (P.arrowX(pdc, PT.Id "inestlev")))

(* -- Mask check/manipulation routines *)

  fun fillMaskS(maskPtr, mask, sizeTy) = 
    PT.Expr(PT.Call(PT.Id "PDCI_fill_mask", [PT.Cast(P.ptrPCT base_mPCT, maskPtr), mask, P.sizeofX sizeTy]))

  fun mTestSetX(m) =
    PT.Call(PT.Id "PDC_Test_Set", [m])

  fun mTestNotSetX(m) =
    PT.Call(PT.Id "PDC_Test_NotSet", [m])

  fun mTestSynCheckX(m) =
    PT.Call(PT.Id "PDC_Test_SynCheck", [m])

  fun mTestNotSynCheckX(m) =
    PT.Call(PT.Id "PDC_Test_NotSynCheck", [m])

  fun mTestSemCheckX(m) =
    PT.Call(PT.Id "PDC_Test_SemCheck", [m])

  fun mTestNotSemCheckX(m) =
    PT.Call(PT.Id "PDC_Test_NotSemCheck", [m])

  fun mTestWriteCheckX(m) =
    PT.Call(PT.Id "PDC_Test_WriteCheck", [m])

  fun mTestNotWriteCheckX(m) =
    PT.Call(PT.Id "PDC_Test_NotWriteCheck", [m])

  fun mTestCheckAndSetX(m) =
    PT.Call(PT.Id "PDC_Test_CheckAndSet", [m])

  fun mTestNotCheckAndSet(m) =
    PT.Call(PT.Id "PDC_Test_NotCheckAndSet", [m])

  fun mTestBothCheckX(m) =
    PT.Call(PT.Id "PDC_Test_BothCheck", [m])

  fun mTestNotBothCheck(m) =
    PT.Call(PT.Id "PDC_Test_NotBothCheck", [m])

  fun mTestIgnoreX(m) =
    PT.Call(PT.Id "PDC_Test_Ignore", [m])

  fun mTestNotIgnoreX(m) =
    PT.Call(PT.Id "PDC_Test_NotIgnore", [m])

(* -- File manipulation routines *)
  fun getLocS(pdc:PT.expression, locAddr:PT.expression) = 
    PT.Expr(PT.Call(PT.Id "PDC_IO_getLoc", [pdc, locAddr, P.zero]))

  fun getLocBeginS(pdc:PT.expression, locAddr:PT.expression) = 
    PT.Expr(PT.Call(PT.Id "PDC_IO_getLocB", [pdc, locAddr, P.zero]))

  fun getLocEndS(pdc:PT.expression, locAddr:PT.expression, offset:int) = 
    PT.Expr(PT.Call(PT.Id "PDC_IO_getLocE", [pdc, locAddr,P.intX offset]))

  fun isEofX(pdc:PT.expression) = 
    PT.Call(PT.Id "PDC_IO_at_EOF", [pdc])

  fun isEorX(pdc:PT.expression) = 
    PT.Call(PT.Id "PDC_IO_at_EOR", [pdc])

(* check point routines *)
  fun chkPtS(pdc:PT.expression, whatFun) =
    chkError(pdc, (PT.Call(PT.Id "PDC_IO_checkpoint", [pdc, P.trueX])), (* always speculative *)
	     whatFun,
	     PDC_CHKPOINT_FAILURE)

  fun getSpecLevelX(pdc:PT.expression) =
     PT.Call(PT.Id "PDC_spec_level", [pdc])

  fun commitS(pdc:PT.expression, whatFun) =
    chkError(pdc, (PT.Call(PT.Id "PDC_IO_commit", [pdc])),
	     whatFun,
	     PDC_COMMIT_FAILURE)

  fun restoreS(pdc:PT.expression, whatFun) =
    chkError(pdc, (PT.Call(PT.Id "PDC_IO_restore", [pdc])),
	     whatFun,
	     PDC_RESTORE_FAILURE)


  fun readFunX(n:string, pdc:PT.expression, loc:PT.expression, 
	                 optArgs: PT.expression list,
			 pd:PT.expression, 
	                 res:PT.expression) = 
      PT.Call(PT.Id n, [pdc, loc] @ optArgs @[pd,res])

  fun readFunChkX(expectedValX : PT.expression,
		  n:string, pdc:PT.expression, 
		  loc:PT.expression, optArgs:PT.expression list,
		  pd:PT.expression, 
	          res:PT.expression) = 
      P.eqX(expectedValX, readFunX(n,pdc,loc,optArgs,pd,res))

  fun scanFunX(n:string, pdc:PT.expression, c : PT.expression, s : PT.expression,eatLit:PT.expression,
	                res:PT.expression, offset:PT.expression) = 
      PT.Call(PT.Id n, [pdc,c,s,eatLit,res,offset])

  fun nstPrefixWhat(outstr, pnst, prefix, what) = 
      PT.Expr(PT.Call(PT.Id "PDCI_nst_prefix_what", [outstr, pnst, prefix, what]))


(* -- reading/writing record functions *)
  fun IOReadNextRecX(pdc, namp) = PT.Call(PT.Id "PDC_IO_next_rec", [pdc, namp])

  fun writeStartX(pdc,io,bufLen,setBuf, whatFn) = 
      PT.Call(PT.Id "PDCI_IO_write_start", [pdc,io,bufLen,setBuf, whatFn])

  fun writeCommitX(pdc,io,buf,setBuf, length, whatFn) = 
      PT.Call(PT.Id "PDCI_IO_write_commit", [pdc,io,buf,setBuf,length, whatFn])

  fun writeAbortX(pdc,io,buf,setBuf, whatFn) = 
      PT.Call(PT.Id "PDCI_IO_write_abort", [pdc,io,buf,setBuf, whatFn])
  fun writeAbortS(pdc,io,buf,setBuf,whatFn) = PT.Expr(writeAbortX(pdc,io,buf,setBuf, whatFn))

  fun recOpenBufWrite(pdc, bufCursor, bufLen, bufFull, whatFn) = 
      PT.Call(PT.Id "PDCI_IO_rec_open_write2buf", [pdc, bufCursor,bufLen,bufFull, whatFn])

  fun recCloseBufWrite(pdc, bufCursor, bufLen, bufFull, buf, length, whatFn) = 
      PT.Call(PT.Id "PDCI_IO_rec_close_write2buf", [pdc, bufCursor,bufLen,bufFull,buf,length, whatFn])

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

(* -- Other helper functions *)
  fun swapBytesS(exp) = PT.Expr(PT.Call(PT.Id "PDC_swap_bytes",
					[PT.Cast(P.ptrPCT bytePCT, P.addrX exp), PT.Cast(P.uint,P.sizeofEX exp)]))
  fun end2StringX(endian) = PT.Call(PT.Id "PDC_endian2str", [endian])
end