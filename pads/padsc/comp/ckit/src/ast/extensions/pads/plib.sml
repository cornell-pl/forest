structure PLib =
struct
  structure PT = ParseTree
  structure P  = ParseTreeUtil

  val PDC_ERROR = PT.Id "PDC_ERROR"
  val PDC_OK    = PT.Id "PDC_OK"

  val PDC_CHKPOINT_FAILURE               = PT.Id "PDC_CHKPOINT_FAILURE"
  val PDC_COMMIT_FAILURE                 = PT.Id "PDC_COMMIT_FAILURE"
  val PDC_RESTORE_FAILURE                = PT.Id "PDC_RESTORE_FAILURE"
  val PDC_ALLOC_FAILURE                  = PT.Id "PDC_ALLOC_FAILURE"

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
  val PDC_UNION_MATCH_FAILURE            = PT.Id "PDC_UNION_MATCH_FAILURE"
  val PDC_ENUM_MATCH_FAILURE             = PT.Id "PDC_ENUM_MATCH_FAILURE"

  val PDC_AT_EOF                         = PT.Id "PDC_AT_EOF"
  val PDC_RANGE                          = PT.Id "PDC_RANGE"
  val PDC_INVALID_AINT                   = PT.Id "PDC_INVALID_AINT"
  val PDC_INVALID_AUINT                  = PT.Id "PDC_INVALID_AUINT"

  val PDC_CHAR_LIT_NOT_FOUND             = PT.Id "PDC_CHAR_LIT_NOT_FOUND"

  val EM_CHECK_AND_SET = PT.Id "PDC_CheckAndSet"
  val EM_CHECK         = PT.Id "PDC_Check"
  val EM_IGNORE        = PT.Id "PDC_Ignore"

  val ERROR_INFO  = P.intX 0
  val ERROR_ERROR = P.intX 2
  val ERROR_FATAL = P.intX 3

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

  val charlit      = "PDC_char_lit"

  fun userErrorS(ts:PT.expression, disc:PT.expression, loc:PT.expression,
                 errCode:PT.expression, format:PT.expression, args:PT.expression list) = 
    (* PDC_error_t PDC_report_err(PDC_t* pdc, PDC_disc_t* disc, PDC_loc_t* loc, 
                                  int errCode, /* format, args */ ...) *)
    PT.Expr(PT.Call(PT.Id "PDC_report_err", [ts,disc, ERROR_INFO, loc,errCode,format]@args))

  fun userFatalErrorS(ts:PT.expression, disc:PT.expression, loc:PT.expression,
                 errCode:PT.expression, format:PT.expression, args:PT.expression list) = 
    (* PDC_error_t PDC_report_err(PDC_t* pdc, PDC_disc_t* disc, PDC_loc_t* loc, 
                                  int errCode, /* format, args */ ...) *)
    PT.Expr(PT.Call(PT.Id "PDC_report_err", [ts,disc,ERROR_FATAL,loc,errCode,format]@args))

  fun chkError(ts:PT.expression, disc:PT.expression,argX :PT.expression, errCode:PT.expression) =
    [ PT.IfThen(P.eqX(PDC_ERROR, argX),
       PT.Compound[
         userFatalErrorS(ts, disc, P.zero, errCode, P.zero, [])
       ])]

  fun chkIntErrorSs(ts:PT.expression, disc:PT.expression,argX :PT.expression, errCode:PT.expression) =
     [PT.IfThen(P.neqX(P.zero, argX),
       PT.Compound[
         userFatalErrorS(ts, disc, P.zero, errCode, P.zero, [])
       ])]


(* Growable buffers *)
  fun zeroMM(ts:PT.expression, disc :PT.expression) = 
    PT.Call(PT.Id "PDC_rmm_zero", [ts, disc])
  fun nonZeroMM(ts:PT.expression, disc :PT.expression) = 
    PT.Call(PT.Id "PDC_rmm_nozero", [ts, disc])

  fun newRBufE(mm:PT.expression) = 
    PT.Call(PT.Id "RMM_new_rbuf", [mm])

  fun chkNewRBufS(rBufV: PT.expression, zero: bool,
		    ts:PT.expression, disc:PT.expression) = 
    let val rBufX = newRBufE(if zero then zeroMM(ts,disc) else nonZeroMM(ts,disc))
    in
      [ P.assignS(rBufV, rBufX),
        PT.IfThen(P.eqX(P.zero, rBufV),
	 PT.Compound[
           userFatalErrorS(ts, disc, P.zero, PDC_ALLOC_FAILURE, PT.String "", [])
         ])]
    end

  fun reserveE(ts:PT.expression, disc: PT.expression, 
		rbuf:PT.expression, buf:PT.expression, sizeX:PT.expression, 
		numElementsX:PT.expression,growHintX:PT.expression) = 
   PT.Call(PT.Id "RBuf_reserve", 
	   [rbuf, PT.Cast(P.voidPtrPtr, buf), sizeX, numElementsX, growHintX ])

  fun chkReserveSs(ts, disc, rbuf,buf,sizeX,numElementsX,growHintX) = 
      let val reserveX = reserveE(ts,disc,rbuf,buf,sizeX,numElementsX,growHintX)
      in
	  chkIntErrorSs(ts,disc,reserveX,PDC_ALLOC_FAILURE)
      end

  fun freeRBufferE(ts, disc, prbuf:PT.expression, ppbuf:PT.expression) = 
   (* int       RMM_free_rbuf_keep_buf(RBuf_t* rbuf, void** buf_out, RMM_t* mgr_out); *)
     PT.Call(PT.Id "RMM_free_rbuf_keep_buf", [prbuf, PT.Cast(P.voidPtrPtr, ppbuf), P.zero])

  fun chkFreeRBufferS(ts, disc, prbuf:PT.expression, ppbuf:PT.expression) = 
    PT.IfThen(
      P.neqX(P.zero,freeRBufferE(ts, disc, prbuf,ppbuf)),
      PT.Compound[
         userFatalErrorS(ts, disc, P.zero, PDC_ALLOC_FAILURE, 
			 PT.String "Couldn't free growable buffer", [])]
    )

(* -- File manipulation routines *)
  fun getLocS(ts:PT.expression, locAddr:PT.expression, disc:PT.expression) = 
    (* PDC_error_t  PDC_get_loc       (PDC_t* pdc, PDC_loc_t* l, PDC_disc_t* disc); *)
    PT.Expr(PT.Call(PT.Id "PDC_get_loc", [ts, locAddr, disc]))

  fun isEofX(ts:PT.expression, disc:PT.expression) = 
    (* int          PDC_IO_is_EOF      (PDC_t* pdc, PDC_disc_t* disc); *)
    PT.Call(PT.Id "PDC_IO_is_EOF", [ts, disc])

  fun backOne(ts: PT.expression, disc:PT.expression) = 
    (* PDC_error_t  PDC_IO_back        (PDC_t* pdc, size_t num_chars, PDC_disc_t* disc); *)
    PT.Expr(PT.Call (PT.Id "PDC_IO_back", [ts, P.intX 1, disc]))

(* check point routines *)
  fun chkPtS(ts:PT.expression, disc:PT.expression) =
    (* PDC_error_t  PDC_IO_checkpoint  (PDC_t* pdc, PDC_disc_t* disc); *)
    chkError(ts, disc, (PT.Call(PT.Id "PDC_IO_checkpoint", [ts,disc])), 
	     PDC_CHKPOINT_FAILURE)

  fun commitS(ts:PT.expression, disc:PT.expression) =
    (* PDC_error_t  PDC_IO_commit  (PDC_t* pdc, PDC_disc_t* disc); *)
    chkError(ts, disc, (PT.Call(PT.Id "PDC_IO_commit", [ts,disc])),
	     PDC_COMMIT_FAILURE)

  fun restoreS(ts:PT.expression, disc:PT.expression) =
    (* PDC_error_t  PDC_IO_restore  (PDC_t* pdc, PDC_disc_t* disc); *)
    chkError(ts, disc, (PT.Call(PT.Id "PDC_IO_restore", [ts,disc])),
	     PDC_RESTORE_FAILURE)


  fun readFunX(n:string, ts:PT.expression, loc:PT.expression, 
	                 optArgs: PT.expression list,
			 ed:PT.expression, 
	                 res:PT.expression, disc:PT.expression) = 
      PT.Call(PT.Id n, [ts, loc] @ optArgs @[ed,res,disc])

  fun readFunChkX(expectedValX : PT.expression,
		  n:string, ts:PT.expression, 
		  loc:PT.expression, optArgs:PT.expression list,
		  ed:PT.expression, 
	          res:PT.expression, disc:PT.expression) = 
      P.eqX(expectedValX, readFunX(n,ts,loc,optArgs,ed,res,disc))

  fun scanFunX(n:string, ts:PT.expression, c : PT.expression, s : PT.expression,
	                res:PT.expression, offset:PT.expression, disc:PT.expression) = 
      (* PDC_error_t PDC_char_lit_scan(PDC_t* pdc, unsigned char c, unsigned char s, 
			      unsigned char* c_out, size_t* offset_out,
			      PDC_disc_t* disc); *)
      PT.Call(PT.Id n, [ts,c,s,res,offset,disc])

end