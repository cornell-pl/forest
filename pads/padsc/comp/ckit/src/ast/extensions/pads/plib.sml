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

  val EM_CHECK_AND_SET = PT.Id "PDC_CheckAndSet"
  val EM_CHECK         = PT.Id "PDC_Check"
  val EM_IGNORE        = PT.Id "PDC_Ignore"

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

  val base_emPCT   = P.makeTypedefPCT "PDC_base_em"
  val base_edPCT   = P.makeTypedefPCT "PDC_base_ed"

  val charlit      = "PDC_achar_lit"
  val strlit       = "PDC_astr_lit"
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

(* error functions *)
  fun trace(disc: PT.expression, msg:string) =
    PT.Expr(PT.Call(PT.Id "PDC_TRACE", [disc, PT.String msg]))

  fun warn(disc: PT.expression, msg :string) =
    PT.Expr(PT.Call(PT.Id "PDC_WARN", [disc, PT.String msg]))

  fun warnDefault(msg :string) =
    PT.Expr(PT.Call(PT.Id "PDC_WARN", [P.addrX (PT.Id "PDC_default_disc"), PT.String msg]))

  fun userErrorS(ts:PT.expression, loc:PT.expression,
                 errCode:PT.expression, format:PT.expression, args:PT.expression list) = 
    PT.Expr(PT.Call(PT.Id "PDCI_report_err", [ts, ERROR_INFO, loc,errCode,format]@args))

  fun userWarnS(ts:PT.expression, loc:PT.expression,
                 format:PT.expression, args:PT.expression list) = 
    PT.Expr(PT.Call(PT.Id "PDCI_report_err", [ts, ERROR_ERROR, loc,PDC_NO_ERROR,format]@args))

  fun userFatalErrorS(ts:PT.expression, loc:PT.expression,
                 errCode:PT.expression, format:PT.expression, args:PT.expression list) = 
    PT.Expr(PT.Call(PT.Id "PDCI_report_err", [ts, ERROR_FATAL, loc,errCode,format]@args))

  fun chkError(ts:PT.expression, argX :PT.expression, errCode:PT.expression) =
    [ PT.IfThen(P.eqX(PDC_ERROR, argX),
       PT.Compound[
         userFatalErrorS(ts, P.zero, errCode, P.zero, [])
       ])]

  fun chkIntErrorSs(ts:PT.expression, argX :PT.expression, errCode:PT.expression) =
     [PT.IfThen(P.neqX(P.zero, argX),
       PT.Compound[
         userFatalErrorS(ts, P.zero, errCode, P.zero, [])
       ])]


(* Growable buffers *)
  fun zeroMM(ts:PT.expression) = 
    PT.Call(PT.Id "PDC_rmm_zero", [ts])
  fun nonZeroMM(ts:PT.expression) = 
    PT.Call(PT.Id "PDC_rmm_nozero", [ts])

  fun newRBufE(mm:PT.expression) = 
    PT.Call(PT.Id "RMM_new_rbuf", [mm])

  fun chkNewRBufS(rBufV: PT.expression, zero: bool,
		    ts:PT.expression) = 
    let val rBufX = newRBufE(if zero then zeroMM(ts) else nonZeroMM(ts))
    in
	[PT.IfThen(
	  P.eqX(P.zero, rBufV),
	  PT.Compound
	   [ P.assignS(rBufV, rBufX),
	     PT.IfThen(P.eqX(P.zero, rBufV),
		       PT.Compound[
			  userFatalErrorS(ts, P.zero, PDC_ALLOC_FAILURE, PT.String "", [])
				   ])])]
    end

  fun reserveE(ts:PT.expression, 
		rbuf:PT.expression, buf:PT.expression, sizeX:PT.expression, 
		numElementsX:PT.expression,growHintX:PT.expression) = 
   PT.Call(PT.Id "RBuf_reserve", 
	   [rbuf, PT.Cast(P.voidPtrPtr, buf), sizeX, numElementsX, growHintX ])

  fun chkReserveSs(ts, rbuf,buf,sizeX,numElementsX,growHintX) = 
      let val reserveX = reserveE(ts,rbuf,buf,sizeX,numElementsX,growHintX)
      in
	  chkIntErrorSs(ts,reserveX,PDC_ALLOC_FAILURE)
      end

  fun freeRBufferE(ts, prbuf:PT.expression, ppbuf:PT.expression) = 
     PT.Call(PT.Id "RMM_free_rbuf_keep_buf", [prbuf, PT.Cast(P.voidPtrPtr, ppbuf), P.zero])

  fun chkFreeRBufferS(ts, prbuf:PT.expression, ppbuf:PT.expression) = 
    PT.IfThen(
      P.neqX(P.zero,freeRBufferE(ts, prbuf,ppbuf)),
      PT.Compound[
         userFatalErrorS(ts, P.zero, PDC_ALLOC_FAILURE, 
			 PT.String "Couldn't free growable buffer.", [])]
    )

  fun cfreeRBufferE(prbuf:PT.expression) = 
   (* int       RMM_free_rbuf(RBuf_t* rbuf); *)
     PT.Call(PT.Id "RMM_free_rbuf", [prbuf])

  fun chkCFreeRBufferS(ts, prbuf:PT.expression) = 
    PT.IfThen(
      P.neqX(P.zero,cfreeRBufferE(prbuf)),
      PT.Compound[
         userFatalErrorS(ts, P.zero, PDC_ALLOC_FAILURE, 
			 PT.String "Couldn't free growable buffer", [])]
    )

(* -- File manipulation routines *)
  fun getLocS(ts:PT.expression, locAddr:PT.expression) = 
    PT.Expr(PT.Call(PT.Id "PDC_IO_getLoc", [ts, locAddr, P.zero]))

  fun getLocBeginS(ts:PT.expression, locAddr:PT.expression) = 
    PT.Expr(PT.Call(PT.Id "PDC_IO_getLocB", [ts, locAddr, P.zero]))

  fun getLocEndS(ts:PT.expression, locAddr:PT.expression) = 
    PT.Expr(PT.Call(PT.Id "PDC_IO_getLocE", [ts, locAddr,P.zero]))

  fun isEofX(ts:PT.expression) = 
    PT.Call(PT.Id "PDC_IO_at_EOF", [ts])

(* check point routines *)
  fun chkPtS(ts:PT.expression) =
    chkError(ts, (PT.Call(PT.Id "PDC_IO_checkpoint", [ts, P.trueX])), (* always speculative *)
	     PDC_CHKPOINT_FAILURE)

  fun getSpecLevelX(ts:PT.expression) =
     PT.Call(PT.Id "PDC_spec_level", [ts])

  fun commitS(ts:PT.expression) =
    chkError(ts, (PT.Call(PT.Id "PDC_IO_commit", [ts])),
	     PDC_COMMIT_FAILURE)

  fun restoreS(ts:PT.expression) =
    chkError(ts, (PT.Call(PT.Id "PDC_IO_restore", [ts])),
	     PDC_RESTORE_FAILURE)


  fun readFunX(n:string, ts:PT.expression, loc:PT.expression, 
	                 optArgs: PT.expression list,
			 ed:PT.expression, 
	                 res:PT.expression) = 
      PT.Call(PT.Id n, [ts, loc] @ optArgs @[ed,res])

  fun readFunChkX(expectedValX : PT.expression,
		  n:string, ts:PT.expression, 
		  loc:PT.expression, optArgs:PT.expression list,
		  ed:PT.expression, 
	          res:PT.expression) = 
      P.eqX(expectedValX, readFunX(n,ts,loc,optArgs,ed,res))

  fun scanFunX(n:string, ts:PT.expression, c : PT.expression, s : PT.expression,eatLit:PT.expression,
	                res:PT.expression, offset:PT.expression) = 
      PT.Call(PT.Id n, [ts,c,s,eatLit,res,offset])

  fun IONextRecX(ts, namp) = PT.Call(PT.Id "PDC_IO_next_rec", [ts, namp])

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
  fun bzeroX (spX, sizeX) = PT.Call(PT.Id "bzero",[PT.Cast(P.voidPtr, spX), sizeX])
  fun bzeroS (spX, sizeX) = PT.Expr(bzeroX(spX,sizeX))
  fun strLen(s:PT.expression)= PT.Call(PT.Id "strlen", [s])

(* -- Other helper functions *)
  fun swapBytesS(exp) = PT.Expr(PT.Call(PT.Id "PDC_swap_bytes",
					[PT.Cast(P.charPtr, exp), PT.Cast(P.uint,P.sizeofEX exp)]))
  fun end2StringX(endian) = PT.Call(PT.Id "PDC_Endian2String", [endian])
end