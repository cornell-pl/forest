structure PLib =
struct
  structure PT = ParseTree
  structure P  = ParseTreeUtil

  val PDC_ERROR = P.intX ~1

  val PDC_PANIC_SKIPPED             = P.intX 10
  val PDC_USER_CONSTRAINT_VIOLATION = P.intX 100
  val PDC_MISSING_LITERAL           = P.intX 101

  val PDC_STRUCT_FIELD_ERROR = P.intX 120


  val EM_CHECK = PT.Id "PDC_Check"
  val EM_IGNORE = PT.Id "PDC_Ignore"


  val toolErrPCT   = P.makeTypedefPCT "PDC_error_t"
  val toolStatePCT = P.makeTypedefPCT "PDC_t"
  val toolDiscPCT  = P.makeTypedefPCT "PDC_disc_t"
  val locPCT       = P.makeTypedefPCT "PDC_loc_t"

  val base_emPCT   = P.makeTypedefPCT "PDC_base_em"
  val base_edPCT   = P.makeTypedefPCT "PDC_base_ed"

  val charlit      = "PDC_char_lit"

  fun getLocS(ts:PT.expression, locAddr:PT.expression, disc:PT.expression) = 
    (* PDC_error_t  PDC_get_loc       (PDC_t* pdc, PDC_loc_t* l, PDC_disc_t* disc); *)
    PT.Expr(PT.Call(PT.Id "PDC_get_loc", [ts, locAddr, disc]))

  fun userErrorS(ts:PT.expression, disc:PT.expression, loc:PT.expression,
                 errCode:PT.expression, format:PT.expression, args:PT.expression list) = 
    (* error_f USER_ERROR_FUN(toolState_t *ts) *)
    PT.Expr(PT.Call(PT.Id "PDC_report_err", [ts,disc,loc,errCode,format]@args))

  fun readFunX(n:string, ts:PT.expression, loc:PT.expression, ed:PT.expression, 
	                res:PT.expression, disc:PT.expression) = 
      PT.Call(PT.Id n, [ts,loc,ed,res,disc])
end