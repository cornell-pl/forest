/*
 * readwrite functions for cmonster
 *
 * Kathleen Fisher, Robert Gruber
 * AT&T Labs Research
 */

#include "cmonster.h"

#define CM_RW_FN_IMPL(ty) \
CM_RW_FN_DECL(ty) \
{ \
  error(0, "Function CM_" PDCI_MacroArg2String(ty) "_rw called, bytes: [%.*s]", end-begin, begin); \
  return PDC_ERR; \
}

CM_RW_FN_IMPL(int8_FW);
CM_RW_FN_IMPL(int16_FW);
CM_RW_FN_IMPL(int32_FW);
CM_RW_FN_IMPL(int64_FW);
CM_RW_FN_IMPL(uint8_FW);
CM_RW_FN_IMPL(uint16_FW);
CM_RW_FN_IMPL(uint32_FW);
CM_RW_FN_IMPL(uint64_FW);

CM_RW_FN_IMPL(a_int8_FW);
CM_RW_FN_IMPL(a_int16_FW);
CM_RW_FN_IMPL(a_int32_FW);
CM_RW_FN_IMPL(a_int64_FW);
CM_RW_FN_IMPL(a_uint8_FW);
CM_RW_FN_IMPL(a_uint16_FW);
CM_RW_FN_IMPL(a_uint32_FW);
CM_RW_FN_IMPL(a_uint64_FW);

CM_RW_FN_IMPL(e_int8_FW);
CM_RW_FN_IMPL(e_int16_FW);
CM_RW_FN_IMPL(e_int32_FW);
CM_RW_FN_IMPL(e_int64_FW);
CM_RW_FN_IMPL(e_uint8_FW);
CM_RW_FN_IMPL(e_uint16_FW);
CM_RW_FN_IMPL(e_uint32_FW);
CM_RW_FN_IMPL(e_uint64_FW);

CM_RW_FN_IMPL(b_int8);
CM_RW_FN_IMPL(b_int16);
CM_RW_FN_IMPL(b_int32);
CM_RW_FN_IMPL(b_int64);
CM_RW_FN_IMPL(b_uint8);
CM_RW_FN_IMPL(b_uint16);
CM_RW_FN_IMPL(b_uint32);
CM_RW_FN_IMPL(b_uint64);

CM_RW_FN_IMPL(ebc_int8);
CM_RW_FN_IMPL(ebc_int16);
CM_RW_FN_IMPL(ebc_int32);
CM_RW_FN_IMPL(ebc_int64);
CM_RW_FN_IMPL(ebc_uint8);
CM_RW_FN_IMPL(ebc_uint16);
CM_RW_FN_IMPL(ebc_uint32);
CM_RW_FN_IMPL(ebc_uint64);

CM_RW_FN_IMPL(bcd_int8);
CM_RW_FN_IMPL(bcd_int16);
CM_RW_FN_IMPL(bcd_int32);
CM_RW_FN_IMPL(bcd_int64);
CM_RW_FN_IMPL(bcd_uint8);
CM_RW_FN_IMPL(bcd_uint16);
CM_RW_FN_IMPL(bcd_uint32);
CM_RW_FN_IMPL(bcd_uint64);

CM_RW_FN_IMPL(sbl_int8);
CM_RW_FN_IMPL(sbl_int16);
CM_RW_FN_IMPL(sbl_int32);
CM_RW_FN_IMPL(sbl_int64);
CM_RW_FN_IMPL(sbl_uint8);
CM_RW_FN_IMPL(sbl_uint16);
CM_RW_FN_IMPL(sbl_uint32);
CM_RW_FN_IMPL(sbl_uint64);

CM_RW_FN_IMPL(sbh_int8);
CM_RW_FN_IMPL(sbh_int16);
CM_RW_FN_IMPL(sbh_int32);
CM_RW_FN_IMPL(sbh_int64);
CM_RW_FN_IMPL(sbh_uint8);
CM_RW_FN_IMPL(sbh_uint16);
CM_RW_FN_IMPL(sbh_uint32);
CM_RW_FN_IMPL(sbh_uint64);

CM_RW_FN_IMPL(ebc_fpoint8);
CM_RW_FN_IMPL(ebc_fpoint16);
CM_RW_FN_IMPL(ebc_fpoint32);
CM_RW_FN_IMPL(ebc_fpoint64);
CM_RW_FN_IMPL(ebc_ufpoint8);
CM_RW_FN_IMPL(ebc_ufpoint16);
CM_RW_FN_IMPL(ebc_ufpoint32);
CM_RW_FN_IMPL(ebc_ufpoint64);

CM_RW_FN_IMPL(bcd_fpoint8);
CM_RW_FN_IMPL(bcd_fpoint16);
CM_RW_FN_IMPL(bcd_fpoint32);
CM_RW_FN_IMPL(bcd_fpoint64);
CM_RW_FN_IMPL(bcd_ufpoint8);
CM_RW_FN_IMPL(bcd_ufpoint16);
CM_RW_FN_IMPL(bcd_ufpoint32);
CM_RW_FN_IMPL(bcd_ufpoint64);

CM_RW_FN_IMPL(sbl_fpoint8);
CM_RW_FN_IMPL(sbl_fpoint16);
CM_RW_FN_IMPL(sbl_fpoint32);
CM_RW_FN_IMPL(sbl_fpoint64);
CM_RW_FN_IMPL(sbl_ufpoint8);
CM_RW_FN_IMPL(sbl_ufpoint16);
CM_RW_FN_IMPL(sbl_ufpoint32);
CM_RW_FN_IMPL(sbl_ufpoint64);

CM_RW_FN_IMPL(sbh_fpoint8);
CM_RW_FN_IMPL(sbh_fpoint16);
CM_RW_FN_IMPL(sbh_fpoint32);
CM_RW_FN_IMPL(sbh_fpoint64);
CM_RW_FN_IMPL(sbh_ufpoint8);
CM_RW_FN_IMPL(sbh_ufpoint16);
CM_RW_FN_IMPL(sbh_ufpoint32);
CM_RW_FN_IMPL(sbh_ufpoint64);

CM_RW_FN_IMPL(char);
CM_RW_FN_IMPL(string_FW);

CM_RW_FN_IMPL(a_char);
CM_RW_FN_IMPL(a_string_FW);

CM_RW_FN_IMPL(e_char);
CM_RW_FN_IMPL(e_string_FW);

