#ifdef _USE_PROTO
#pragma prototyped
#endif
/*
 * padsc library interface: macro implementations of certain function calls
 *   
 * NOTE: C prototypes for these functions are given in libpadsc.h
 *
 * Kathleen Fisher, Robert Gruber
 * AT&T Labs Research
 */

#ifndef __LIBPADSC_IMPL_H__
#define __LIBPADSC_IMPL_H__

#ifndef FOR_CKIT

/* ================================================================================
 * STRING COMPARISON
 */

#define PDC_string_eq(str1, str2) \
  ((str1)->len == (str2)->len && strncmp((str1)->str, (str2)->str, (str1)->len) == 0)

#define PDC_string_eq_Cstr(PDCstr, Cstr) \
  ((PDCstr)->len == strlen(Cstr) && strncmp((PDCstr)->str, (Cstr), (PDCstr)->len) == 0)

/* ================================================================================
 * Helper Macro
 * 
 * PDC_SAFE_DEF_CHARSET(pdc) : safely produces pdc->disc->def_charset (or PDC_charset_INVALID)
 *
 * PDC_CHARSET_SWITCH(pdc,fn_pre, fn_post, ,args) selects from a set of
 * functions based on pdc->disc->def_charset.  It inserts an appropriate
 * charset marker ("_a_", "_e_", etc.) between fn_pre and fn_post to form a
 * function name, and calls that function with the specified args, which must be
 * given in parens.  If charset is not valid it will still choose one
 * of the possible functions -- that function should detect and report the problem.
 *
 * Example:
 *
 *    PDC_CHARSET_SWITCH(pdc, my, fun, (pdc, x, y))
 *
 * Is converted to one of these forms:
 * 
 *    my_a_fun(pdc, x, y)
 *    my_e_fun(pdc, x, y)
 *    etc. 
 */

#define PDC_SAFE_DEF_CHARSET(pdc) \
  ((pdc && pdc->disc) ? (pdc->disc->def_charset) : PDC_charset_INVALID)

#define PDC_CHARSET_SWITCH(pdc,fn_pre,fn_post,args) \
 ((pdc && pdc->disc && pdc->disc->def_charset == PDC_charset_ASCII) \
    ?  fn_pre ## _a_ ## fn_post args \
    :  fn_pre ## _e_ ## fn_post args)

/* ================================================================================
 * CHAR/STRING SCAN FUNCTIONS
 */

#define PDC_char_lit_scan(pdc, c, s, eat_lit, c_out, offset_out) \
  PDCI_char_lit_scan(pdc, c, s, eat_lit, c_out, offset_out, PDC_SAFE_DEF_CHARSET(pdc), "PDC_char_lit_scan", 1)

#define PDC_a_char_lit_scan(pdc, c, s, eat_lit, c_out, offset_out) \
  PDCI_char_lit_scan(pdc, c, s, eat_lit, c_out, offset_out, PDC_charset_ASCII, "PDC_a_char_lit_scan", 1)

#define PDC_e_char_lit_scan(pdc, c, s, eat_lit, c_out, offset_out) \
  PDCI_char_lit_scan(pdc, c, s, eat_lit, c_out, offset_out, PDC_charset_EBCDIC, "PDC_e_char_lit_scan", 1)

#define PDC_str_lit_scan(pdc, findStr, stopStr, eat_lit, str_out, offset_out) \
  PDCI_str_lit_scan(pdc, findStr, stopStr, eat_lit, str_out, offset_out, PDC_SAFE_DEF_CHARSET(pdc), "PDC_str_lit_scan", 1)

#define PDC_a_str_lit_scan(pdc, findStr, stopStr, eat_lit, str_out, offset_out) \
  PDCI_str_lit_scan(pdc, findStr, stopStr, eat_lit, str_out, offset_out, PDC_charset_ASCII, "PDC_a_str_lit_scan", 1)

#define PDC_e_str_lit_scan(pdc, findStr, stopStr, eat_lit, str_out, offset_out) \
  PDCI_str_lit_scan(pdc, findStr, stopStr, eat_lit, str_out, offset_out, PDC_charset_EBCDIC, "PDC_e_str_lit_scan", 1)

#define PDC_Cstr_lit_scan(pdc, findStr, stopStr, eat_lit, str_out, offset_out) \
  PDCI_Cstr_lit_scan(pdc, findStr, stopStr, eat_lit, str_out, offset_out, PDC_SAFE_DEF_CHARSET(pdc), "PDC_Cstr_lit_scan", 1)

#define PDC_a_Cstr_lit_scan(pdc, findStr, stopStr, eat_lit, str_out, offset_out) \
          (pdc, findStr, stopStr, eat_lit, str_out, offset_out, PDC_charset_ASCII, "PDC_a_Cstr_lit_scan", 1)

#define PDC_e_Cstr_lit_scan(pdc, findStr, stopStr, eat_lit, str_out, offset_out) \
  PDCI_Cstr_lit_scan(pdc, findStr, stopStr, eat_lit, str_out, offset_out, PDC_charset_EBCDIC, "PDC_e_Cstr_lit_scan", 1)

/* ================================================================================
 * CHAR/STRING READ FUNCTIONS
 */

#define PDC_char_lit_read(pdc, csm, ed, c) \
  PDCI_char_lit_read(pdc, csm, ed, c, PDC_SAFE_DEF_CHARSET(pdc), "PDC_char_lit_read", 1)

#define PDC_a_char_lit_read(pdc, csm, ed, c) \
  PDCI_char_lit_read(pdc, csm, ed, c, PDC_charset_ASCII, "PDC_a_char_lit_read", 1)

#define PDC_e_char_lit_read(pdc, csm, ed, c) \
  PDCI_char_lit_read(pdc, csm, ed, c, PDC_charset_EBCDIC, "PDC_e_char_lit_read", 1)

#define PDC_str_lit_read(pdc, csm, ed, s) \
  PDCI_str_lit_read(pdc, csm, ed, s, PDC_SAFE_DEF_CHARSET(pdc), "PDC_str_lit_read", 1)

#define PDC_a_str_lit_read(pdc, csm, ed, s) \
  PDCI_str_lit_read(pdc, csm, ed, s, PDC_charset_ASCII, "PDC_a_str_lit_read", 1)

#define PDC_e_str_lit_read(pdc, csm, ed, s) \
  PDCI_str_lit_read(pdc, csm, ed, s, PDC_charset_EBCDIC, "PDC_e_str_lit_read", 1)

#define PDC_Cstr_lit_read(pdc, csm, ed, s) \
  PDCI_Cstr_lit_read(pdc, csm, ed, s, PDC_SAFE_DEF_CHARSET(pdc), "PDC_Cstr_lit_read", 1)

#define PDC_a_Cstr_lit_read(pdc, csm, ed, s) \
  PDCI_Cstr_lit_read(pdc, csm, ed, s, PDC_charset_ASCII, "PDC_a_Cstr_lit_read", 1)

#define PDC_e_Cstr_lit_read(pdc, csm, ed, s) \
  PDCI_Cstr_lit_read(pdc, csm, ed, s, PDC_charset_EBCDIC, "PDC_e_Cstr_lit_read", 1)

#define PDC_countX(pdc, csm, x, eor_required, ed, res_out) \
  PDCI_countX(pdc, csm, x, eor_required, ed, res_out, PDC_SAFE_DEF_CHARSET(pdc), "PDC_countX", 1)

#define PDC_a_countX(pdc, csm, x, eor_required, ed, res_out) \
  PDCI_countX(pdc, csm, x, eor_required, ed, res_out, PDC_charset_ASCII, "PDC_a_countX", 1)

#define PDC_e_countX(pdc, csm, x, eor_required, ed, res_out) \
  PDCI_countX(pdc, csm, x, eor_required, ed, res_out, PDC_charset_EBCDIC, "PDC_e_countX", 1)

#define PDC_countXtoY(pdc, csm, x, y, ed, res_out) \
  PDCI_countXtoY(pdc, csm, x, y, ed, res_out, PDC_SAFE_DEF_CHARSET(pdc), "PDC_countXtoY", 1)

#define PDC_a_countXtoY(pdc, csm, x, y, ed, res_out) \
  PDCI_countXtoY(pdc, csm, x, y, ed, res_out, PDC_charset_ASCII, "PDC_a_countXtoY", 1)

#define PDC_e_countXtoY(pdc, csm, x, y, ed, res_out) \
  PDCI_countXtoY(pdc, csm, x, y, ed, res_out, PDC_charset_EBCDIC, "PDC_e_countXtoY", 1)

#define PDC_date_read(pdc, csm, stopChar, ed, res_out) \
  PDCI_date_read(pdc, csm, stopChar, ed, res_out, PDC_SAFE_DEF_CHARSET(pdc), "PDC_date_read", 1)

#define PDC_a_date_read(pdc, csm, stopChar, ed, res_out) \
  PDCI_date_read(pdc, csm, stopChar, ed, res_out, PDC_charset_ASCII, "PDC_a_date_read", 1)

#define PDC_e_date_read(pdc, csm, stopChar, ed, res_out) \
  PDCI_date_read(pdc, csm, stopChar, ed, res_out, PDC_charset_EBCDIC, "PDC_e_date_read", 1)

#define PDC_char_read(pdc, csm, ed, c_out) \
  PDCI_char_read(pdc, csm, ed, c_out, PDC_SAFE_DEF_CHARSET(pdc), "PDC_char_read", 1)

#define PDC_a_char_read(pdc, csm, ed, c_out) \
  PDCI_char_read(pdc, csm, ed, c_out, PDC_charset_ASCII, "PDC_a_char_read", 1)

#define PDC_e_char_read(pdc, csm, ed, c_out) \
  PDCI_char_read(pdc, csm, ed, c_out, PDC_charset_EBCDIC, "PDC_e_char_read", 1)

#define PDC_string_FW_read(pdc, csm, width, ed, s_out) \
  PDCI_string_FW_read(pdc, csm, width, ed, s_out, PDC_SAFE_DEF_CHARSET(pdc), "PDC_string_FW_read", 1)

#define PDC_a_string_FW_read(pdc, csm, width, ed, s_out) \
  PDCI_string_FW_read(pdc, csm, width, ed, s_out, PDC_charset_ASCII, "PDC_a_string_FW_read", 1)

#define PDC_e_string_FW_read(pdc, csm, width, ed, s_out) \
  PDCI_string_FW_read(pdc, csm, width, ed, s_out, PDC_charset_EBCDIC, "PDC_e_string_FW_read", 1)


#define PDC_string_read(pdc, csm, stopChar, ed, s_out) \
  PDCI_string_read(pdc, csm, stopChar, ed, s_out, PDC_SAFE_DEF_CHARSET(pdc), "PDC_string_read", 1)

#define PDC_a_string_read(pdc, csm, stopChar, ed, s_out) \
  PDCI_string_read(pdc, csm, stopChar, ed, s_out, PDC_charset_ASCII, "PDC_a_string_read", 1)

#define PDC_e_string_read(pdc, csm, stopChar, ed, s_out) \
  PDCI_string_read(pdc, csm, stopChar, ed, s_out, PDC_charset_EBCDIC, "PDC_e_string_read", 1)


#define PDC_string_ME_read(pdc, csm, matchRegexp, ed, s_out) \
  PDCI_string_ME_read(pdc, csm, matchRegexp, ed, s_out, PDC_SAFE_DEF_CHARSET(pdc), "PDC_string_ME_read", 1)

#define PDC_a_string_ME_read(pdc, csm, matchRegexp, ed, s_out) \
  PDCI_string_ME_read(pdc, csm, matchRegexp, ed, s_out, PDC_charset_ASCII, "PDC_a_string_ME_read", 1)

#define PDC_e_string_ME_read(pdc, csm, matchRegexp, ed, s_out) \
  PDCI_string_ME_read(pdc, csm, matchRegexp, ed, s_out, PDC_charset_EBCDIC, "PDC_e_string_ME_read", 1)


#define PDC_string_CME_read(pdc, csm, matchRegexp, ed, s_out) \
  PDCI_string_CME_read(pdc, csm, matchRegexp, ed, s_out, PDC_SAFE_DEF_CHARSET(pdc), "PDC_string_CME_read", 1)

#define PDC_a_string_CME_read(pdc, csm, matchRegexp, ed, s_out) \
  PDCI_string_CME_read(pdc, csm, matchRegexp, ed, s_out, PDC_charset_ASCII, "PDC_a_string_CME_read", 1)

#define PDC_e_string_CME_read(pdc, csm, matchRegexp, ed, s_out) \
  PDCI_string_CME_read(pdc, csm, matchRegexp, ed, s_out, PDC_charset_EBCDIC, "PDC_e_string_CME_read", 1)


#define PDC_string_SE_read(pdc, csm, stopRegexp, ed, s_out) \
  PDCI_string_SE_read(pdc, csm, stopRegexp, ed, s_out, PDC_SAFE_DEF_CHARSET(pdc), "PDC_string_SE_read", 1)

#define PDC_a_string_SE_read(pdc, csm, stopRegexp, ed, s_out) \
  PDCI_string_SE_read(pdc, csm, stopRegexp, ed, s_out, PDC_charset_ASCII, "PDC_a_string_SE_read", 1)

#define PDC_e_string_SE_read(pdc, csm, stopRegexp, ed, s_out) \
  PDCI_string_SE_read(pdc, csm, stopRegexp, ed, s_out, PDC_charset_EBCDIC, "PDC_e_string_SE_read", 1)


#define PDC_string_CSE_read(pdc, csm, stopRegexp, ed, s_out) \
  PDCI_string_CSE_read(pdc, csm, stopRegexp, ed, s_out, PDC_SAFE_DEF_CHARSET(pdc), "PDC_string_CSE_read", 1)

#define PDC_a_string_CSE_read(pdc, csm, stopRegexp, ed, s_out) \
  PDCI_string_CSE_read(pdc, csm, stopRegexp, ed, s_out, PDC_charset_ASCII, "PDC_a_string_CSE_read", 1)

#define PDC_e_string_CSE_read(pdc, csm, stopRegexp, ed, s_out) \
  PDCI_string_CSE_read(pdc, csm, stopRegexp, ed, s_out, PDC_charset_EBCDIC, "PDC_e_string_CSE_read", 1)


/* ================================================================================
 * DEFAULT STRING TO INTEGER READ FUNCTIONS
 */

#define PDC_int8_read(pdc, csm, ed, res_out) \
  PDC_CHARSET_SWITCH(pdc, PDC, int8_read, (pdc, csm, ed, res_out))

#define PDC_int16_read(pdc, csm, ed, res_out) \
  PDC_CHARSET_SWITCH(pdc, PDC, int16_read, (pdc, csm, ed, res_out))

#define PDC_int32_read(pdc, csm, ed, res_out) \
  PDC_CHARSET_SWITCH(pdc, PDC, int32_read, (pdc, csm, ed, res_out))

#define PDC_int64_read(pdc, csm, ed, res_out) \
  PDC_CHARSET_SWITCH(pdc, PDC, int64_read, (pdc, csm, ed, res_out))

#define PDC_uint8_read(pdc, csm, ed, res_out) \
  PDC_CHARSET_SWITCH(pdc, PDC, uint8_read, (pdc, csm, ed, res_out))

#define PDC_uint16_read(pdc, csm, ed, res_out) \
  PDC_CHARSET_SWITCH(pdc, PDC, uint16_read, (pdc, csm, ed, res_out))

#define PDC_uint32_read(pdc, csm, ed, res_out) \
  PDC_CHARSET_SWITCH(pdc, PDC, uint32_read, (pdc, csm, ed, res_out))

#define PDC_uint64_read(pdc, csm, ed, res_out) \
  PDC_CHARSET_SWITCH(pdc, PDC, uint64_read, (pdc, csm, ed, res_out))

#define PDC_int8_FW_read(pdc, csm, width, ed, res_out) \
  PDC_CHARSET_SWITCH(pdc, PDC, int8_FW_read, (pdc, csm, width, ed, res_out))

#define PDC_int16_FW_read(pdc, csm, width, ed, res_out) \
  PDC_CHARSET_SWITCH(pdc, PDC, int16_FW_read, (pdc, csm, width, ed, res_out))

#define PDC_int32_FW_read(pdc, csm, width, ed, res_out) \
  PDC_CHARSET_SWITCH(pdc, PDC, int32_FW_read, (pdc, csm, width, ed, res_out))

#define PDC_int64_FW_read(pdc, csm, width, ed, res_out) \
  PDC_CHARSET_SWITCH(pdc, PDC, int64_FW_read, (pdc, csm, width, ed, res_out))

#define PDC_uint8_FW_read(pdc, csm, width, ed, res_out) \
  PDC_CHARSET_SWITCH(pdc, PDC, uint8_FW_read, (pdc, csm, width, ed, res_out))

#define PDC_uint16_FW_read(pdc, csm, width, ed, res_out) \
  PDC_CHARSET_SWITCH(pdc, PDC, uint16_FW_read, (pdc, csm, width, ed, res_out))

#define PDC_uint32_FW_read(pdc, csm, width, ed, res_out) \
  PDC_CHARSET_SWITCH(pdc, PDC, uint32_FW_read, (pdc, csm, width, ed, res_out))

#define PDC_uint64_FW_read(pdc, csm, width, ed, res_out) \
  PDC_CHARSET_SWITCH(pdc, PDC, uint64_FW_read, (pdc, csm, width, ed, res_out))

/* ================================================================================
 * WRITE FUNCTIONS
 */

#define PDC_a_char_lit_write2io(pdc, io, c) \
  PDCI_char_lit_write2io(pdc, io, c, PDC_charset_ASCII, "PDC_a_char_lit_write2io", 1)

#define PDC_e_char_lit_write2io(pdc, io, c) \
  PDCI_char_lit_write2io(pdc, io, c, PDC_charset_EBCDIC, "PDC_e_char_lit_write2io", 1)

#define PDC_char_lit_write2io(pdc, io, c) \
  PDCI_char_lit_write2io(pdc, io, c, PDC_SAFE_DEF_CHARSET(pdc), "PDC_char_lit_write2io", 1)

#define PDC_a_str_lit_write2io(pdc, io, s) \
  PDCI_str_lit_write2io(pdc, io, s, PDC_charset_ASCII, "PDC_a_str_lit_write2io", 1)

#define PDC_e_str_lit_write2io(pdc, io, s) \
  PDCI_str_lit_write2io(pdc, io, s, PDC_charset_EBCDIC, "PDC_e_str_lit_write2io", 1)

#define PDC_a_str_lit_write2io(pdc, io, s) \
  PDCI_str_lit_write2io(pdc, io, s, PDC_SAFE_DEF_CHARSET(pdc), "PDC_str_lit_write2io", 1)

#define PDC_a_Cstr_lit_write2io(pdc, io, s) \
  PDCI_Cstr_lit_write2io(pdc, io, s, PDC_charset_ASCII, "PDC_a_Cstr_lit_write2io", 1)

#define PDC_e_Cstr_lit_write2io(pdc, io, s) \
  PDCI_Cstr_lit_write2io(pdc, io, s, PDC_charset_EBCDIC, "PDC_e_Cstr_lit_write2io", 1)

#define PDC_Cstr_lit_write2io(pdc, io, s) \
  PDCI_Cstr_lit_write2io(pdc, io, s, PDC_SAFE_DEF_CHARSET(pdc), "PDC_Cstr_lit_write2io", 1)

#define PDC_a_char_lit_write2buf(pdc, buf, buf_len, buf_full, c) \
  PDCI_char_lit_write2buf(pdc, buf, buf_len, buf_full, c, PDC_charset_ASCII, "PDC_a_char_lit_write2buf", 1)

#define PDC_e_char_lit_write2buf(pdc, buf, buf_len, buf_full, c) \
  PDCI_char_lit_write2buf(pdc, buf, buf_len, buf_full, c, PDC_charset_EBCDIC, "PDC_e_char_lit_write2buf", 1)

#define PDC_char_lit_write2buf(pdc, buf, buf_len, buf_full, c) \
  PDCI_char_lit_write2buf(pdc, buf, buf_len, buf_full, c, PDC_SAFE_DEF_CHARSET(pdc), "PDC_char_lit_write2buf", 1)

#define PDC_a_str_lit_write2buf(pdc, buf, buf_len, buf_full, s) \
  PDCI_str_lit_write2buf(pdc, buf, buf_len, buf_full, s, PDC_charset_ASCII, "PDC_a_str_lit_write2buf", 1)

#define PDC_e_str_lit_write2buf(pdc, buf, buf_len, buf_full, s) \
  PDCI_str_lit_write2buf(pdc, buf, buf_len, buf_full, s, PDC_charset_EBCDIC, "PDC_e_str_lit_write2buf", 1)

#define PDC_str_lit_write2buf(pdc, buf, buf_len, buf_full, s) \
  PDCI_str_lit_write2buf(pdc, buf, buf_len, buf_full, s, PDC_SAFE_DEF_CHARSET(pdc), "PDC_str_lit_write2buf", 1)

#define PDC_a_Cstr_lit_write2buf(pdc, buf, buf_len, buf_full, s) \
  PDCI_Cstr_lit_write2buf(pdc, buf, buf_len, buf_full, s, PDC_charset_ASCII, "PDC_a_Cstr_lit_write2buf", 1)

#define PDC_e_Cstr_lit_write2buf(pdc, buf, buf_len, buf_full, s) \
  PDCI_Cstr_lit_write2buf(pdc, buf, buf_len, buf_full, s, PDC_charset_EBCDIC, "PDC_e_Cstr_lit_write2buf", 1)

#define PDC_Cstr_lit_write2buf(pdc, buf, buf_len, buf_full, s) \
  PDCI_Cstr_lit_write2buf(pdc, buf, buf_len, buf_full, s, pdc->disc->dif_charset, "PDC_Cstr_lit_write2buf", 1)

#define PDC_int8_write2io(pdc, io, ed, val) \
  PDC_CHARSET_SWITCH(pdc, PDC, int8_write2io, (pdc, io, ed, val))

#define PDC_int16_write2io(pdc, io, ed, val) \
  PDC_CHARSET_SWITCH(pdc, PDC, int16_write2io, (pdc, io, ed, val))

#define PDC_int32_write2io(pdc, io, ed, val) \
  PDC_CHARSET_SWITCH(pdc, PDC, int32_write2io, (pdc, io, ed, val))

#define PDC_int64_write2io(pdc, io, ed, val) \
  PDC_CHARSET_SWITCH(pdc, PDC, int64_write2io, (pdc, io, ed, val))

#define PDC_uint8_write2io(pdc, io, ed, val) \
  PDC_CHARSET_SWITCH(pdc, PDC, uint8_write2io, (pdc, io, ed, val))

#define PDC_uint16_write2io(pdc, io, ed, val) \
  PDC_CHARSET_SWITCH(pdc, PDC, uint16_write2io, (pdc, io, ed, val))

#define PDC_uint32_write2io(pdc, io, ed, val) \
  PDC_CHARSET_SWITCH(pdc, PDC, uint32_write2io, (pdc, io, ed, val))

#define PDC_uint64_write2io(pdc, io, ed, val) \
  PDC_CHARSET_SWITCH(pdc, PDC, uint64_write2io, (pdc, io, ed, val))

#define PDC_int8_write2buf(pdc, buf, buf_len, buf_full, ed, val) \
  PDC_CHARSET_SWITCH(pdc, PDC, int8_write2buf, (pdc, buf, buf_len, buf_full, ed, val))

#define PDC_int16_write2buf(pdc, buf, buf_len, buf_full, ed, val) \
  PDC_CHARSET_SWITCH(pdc, PDC, int16_write2buf, (pdc, buf, buf_len, buf_full, ed, val))

#define PDC_int32_write2buf(pdc, buf, buf_len, buf_full, ed, val) \
  PDC_CHARSET_SWITCH(pdc, PDC, int32_write2buf, (pdc, buf, buf_len, buf_full, ed, val))

#define PDC_int64_write2buf(pdc, buf, buf_len, buf_full, ed, val) \
  PDC_CHARSET_SWITCH(pdc, PDC, int64_write2buf, (pdc, buf, buf_len, buf_full, ed, val))

#define PDC_uint8_write2buf(pdc, buf, buf_len, buf_full, ed, val) \
  PDC_CHARSET_SWITCH(pdc, PDC, uint8_write2buf, (pdc, buf, buf_len, buf_full, ed, val))

#define PDC_uint16_write2buf(pdc, buf, buf_len, buf_full, ed, val) \
  PDC_CHARSET_SWITCH(pdc, PDC, uint16_write2buf, (pdc, buf, buf_len, buf_full, ed, val))

#define PDC_uint32_write2buf(pdc, buf, buf_len, buf_full, ed, val) \
  PDC_CHARSET_SWITCH(pdc, PDC, uint32_write2buf, (pdc, buf, buf_len, buf_full, ed, val))

#define PDC_uint64_write2buf(pdc, buf, buf_len, buf_full, ed, val) \
  PDC_CHARSET_SWITCH(pdc, PDC, uint64_write2buf, (pdc, buf, buf_len, buf_full, ed, val))


#endif   /*   ! FOR_CKIT             */
#endif   /*   ! __LIBPADSC_IMPL_H__  */
