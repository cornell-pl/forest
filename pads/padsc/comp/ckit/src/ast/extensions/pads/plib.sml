structure PLib =
struct
  structure PT = ParseTree
  structure P  = ParseTreeUtil

  val TOOL_ERROR = P.intX ~1

  val PANIC_SKIPPED             = P.intX 10
  val MISSING_LITERAL           = P.intX 101
  val USER_CONSTRAINT_VIOLATION = P.intX 100

  val STRUCT_FIELD_ERROR = P.intX 120


  val EM_CHECK = PT.Id "Check"
  val EM_IGNORE = PT.Id "Ignore"


  fun getLocS(ts:PT.expression, locAddr:PT.expression) = 
    (* void get_location_info(toolState_t *ts, loc_t *loc) *)
    PT.Expr(PT.Call(PT.Id "get_location_info", [ts, locAddr]))

  fun userErrorF(ts:PT.expression) = 
    (* error_f USER_ERROR_FUN(toolState_t *ts) *)
    PT.Call(PT.Id "USER_ERROR_FUN", [ts])
end