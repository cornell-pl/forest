/*
 * switch value functions for cmonster
 *
 * Kathleen Fisher, Robert Gruber
 * AT&T Labs Research
 */

#include "cmonster.h"

#define CM_SVAL_FN_IMPL(ty) \
CM_SVAL_FN_DECL(ty) \
{ \
  error(0, "Function CM_" PDCI_MacroArg2String(ty) "_sval called, bytes: [%.*s]", end-begin, begin); \
  return PDC_ERR; \
}

CM_SVAL_FN_IMPL(int32_FW);
CM_SVAL_FN_IMPL(a_int32_FW);
CM_SVAL_FN_IMPL(e_int32_FW);
CM_SVAL_FN_IMPL(b_int32);
CM_SVAL_FN_IMPL(ebc_int32);
CM_SVAL_FN_IMPL(bcd_int32);
CM_SVAL_FN_IMPL(sbl_int32);
CM_SVAL_FN_IMPL(sbh_int32);
CM_SVAL_FN_IMPL(char);
CM_SVAL_FN_IMPL(a_char);
CM_SVAL_FN_IMPL(e_char);
