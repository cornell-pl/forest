structure PLib =
struct
  structure PT = ParseTree
  structure P  = ParseTreeUtil

  val PDC_ERROR = PT.Id "PDC_ERR"
  val PDC_OK    = PT.Id "PDC_OK"

  val PDC_NO_ERROR                       = PT.Id "PDC_NO_ERR"
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

  val CSM_CHECK_AND_SET = PT.Id "PDC_CheckAndSet"
  val CSM_CHECK         = PT.Id "PDC_Check"
  val CSM_IGNORE        = PT.Id "PDC_Ignore"

  val PDC_littleEndian = PT.Id "PDC_littleEndian"
  val PDC_bigEndian    = PT.Id "PDC_bigEndian"



  val ERROR_INFO  = PT.Id "PDC_LEV_INFO" 
  val ERROR_ERROR = PT.Id "PDC_LEV_ERR"
  val ERROR_FATAL = PT.Id "PDC_LEV_FATAL"

  val toolErrPCT   = P.makeTypedefPCT "PDC_error_t"
  val toolStatePCT = P.makeTypedefPCT "PDC_t"
  val toolDiscPCT  = P.makeTypedefPCT "PDC_disc_t"
  val locPCT       = P.makeTypedefPCT "PDC_loc_t"
  val errCodePCT   = P.makeTypedefPCT "PDC_errCode_t"
  val sizePCT      = P.makeTypedefPCT "size_t"

  val rbufferPCT   = P.makeTypedefPCT "RBuf_t"
  val rMMPCT       = P.makeTypedefPCT "RMM_t"

  val base_csmPCT  = P.makeTypedefPCT "PDC_base_csm"
  val base_edPCT   = P.makeTypedefPCT "PDC_base_ed"
  val bytePCT      = P.makeTypedefPCT "PDC_byte"

  val charlit      = "PDC_a_char_lit"
  val strlit       = "PDC_a_str_lit"
  val stringPCT    = P.makeTypedefPCT "PDC_string"
  val str          = "str"
  val len          = "len"
  val errorf       = "errorf"
  val disc         = "disc"
  val io_disc      = "io_disc"

 
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
  fun mkFName s = PT.String ("[in "^s^"]")
  fun trace(disc: PT.expression, msg:string) =
    PT.Expr(PT.Call(PT.Id "PDC_TRACE", [disc, PT.String msg]))

  fun warn(disc: PT.expression, msg :string) =
    PT.Expr(PT.Call(PT.Id "PDC_WARN", [disc, PT.String msg]))

  fun warnDefault(msg :string) =
    PT.Expr(PT.Call(PT.Id "PDC_WARN", [P.addrX (PT.Id "PDC_default_disc"), PT.String msg]))

  fun userErrorS(pdc:PT.expression, loc:PT.expression, errCode:PT.expression, 
		 whatFunction: string, format:PT.expression, args:PT.expression list) = 
    PT.Expr(PT.Call(PT.Id "PDCI_report_err", [pdc, ERROR_INFO, loc, errCode, 
					      mkFName whatFunction, format]@args))

  fun userWarnS(pdc:PT.expression, loc:PT.expression,
                whatFunction: string,  format:PT.expression, args:PT.expression list) = 
    PT.Expr(PT.Call(PT.Id "PDCI_report_err", [pdc, ERROR_ERROR, loc, PDC_NO_ERROR, 
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
      PT.Call(PT.Id "PDC_nerr_acc_report_internal",[pdc, outStrmX, prefixX, whatX, nstX, fieldX])

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
			 ed:PT.expression, 
	                 res:PT.expression) = 
      PT.Call(PT.Id n, [pdc, loc] @ optArgs @[ed,res])

  fun readFunChkX(expectedValX : PT.expression,
		  n:string, pdc:PT.expression, 
		  loc:PT.expression, optArgs:PT.expression list,
		  ed:PT.expression, 
	          res:PT.expression) = 
      P.eqX(expectedValX, readFunX(n,pdc,loc,optArgs,ed,res))

  fun scanFunX(n:string, pdc:PT.expression, c : PT.expression, s : PT.expression,eatLit:PT.expression,
	                res:PT.expression, offset:PT.expression) = 
      PT.Call(PT.Id n, [pdc,c,s,eatLit,res,offset])

  fun IONextRecX(pdc, namp) = PT.Call(PT.Id "PDC_IO_next_rec", [pdc, namp])

  fun nstPrefixWhat(outstr, pnst, prefix, what) = 
      PT.Expr(PT.Call(PT.Id "PDCI_nst_prefix_what", [outstr, pnst, prefix, what]))

(* -- Sfio functions *)
  fun sfstrclose(str:PT.expression) = 
    PT.Expr(PT.Call(PT.Id "sfstrclose", [str]))

  val sfstropen = PT.Call(PT.Id "sfstropen", [])

  fun sfprintf (tmpstr :PT.expression, control:PT.expression, args : PT.expression list) =
      PT.Expr(PT.Call(PT.Id "sfprintf", tmpstr::control::args))

  fun sfstruse (tmpstr : PT.expression) = 
      PT.Call(PT.Id "sfstruse", [tmpstr])


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
  fun end2StringX(endian) = PT.Call(PT.Id "PDC_Endian2String", [endian])
end