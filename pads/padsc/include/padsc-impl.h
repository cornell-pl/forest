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
 * Helper Macro
 * 
 * PDC_SAFE_DEF_CHARSET(pdc) : safely produces pdc->disc->def_charset (or PDC_charset_INVALID)
 *
 * PDCI_CHARSET_SWITCH(pdc,fn_pre, fn_post, ,args) selects from a set of
 * functions based on pdc->disc->def_charset.  It inserts an appropriate
 * charset marker ("_a_", "_e_", etc.) between fn_pre and fn_post to form a
 * function name, and calls that function with the specified args, which must be
 * given in parens.  If charset is not valid it will still choose one
 * of the possible functions -- that function should detect and report the problem.
 *
 * Example:
 *
 *    PDCI_CHARSET_SWITCH(pdc, my, fun, (pdc, x, y))
 *
 * Is converted to one of these forms:
 * 
 *    my_a_fun(pdc, x, y)
 *    my_e_fun(pdc, x, y)
 *    etc. 
 */

#define PDCI_SAFE_DEF_CHARSET(pdc) \
  ((pdc && pdc->disc) ? (pdc->disc->def_charset) : PDC_charset_INVALID)

#define PDCI_CHARSET_SWITCH(pdc,fn_pre,fn_post,args) \
 ((pdc && pdc->disc && pdc->disc->def_charset == PDC_charset_ASCII) \
    ?  fn_pre ## _a_ ## fn_post args \
    :  fn_pre ## _e_ ## fn_post args)

/* ================================================================================ */
/* INTERNAL SCAN ROUTINES (helpers) */

PDC_error_t PDCI_char_lit_scan(PDC_t *pdc, PDC_char c, PDC_char s, int eat_lit,
			       PDC_char *c_out, size_t *offset_out, PDC_charset char_set,
			       const char *whatfn, int safe);

PDC_error_t PDCI_str_lit_scan(PDC_t *pdc, const PDC_string *findStr, const PDC_string *stopStr, int eat_lit,
			      PDC_string **str_out, size_t *offset_out, PDC_charset char_set,
			      const char *whatfn, int safe);

PDC_error_t PDCI_Cstr_lit_scan(PDC_t *pdc, const char *findStr, const char *stopStr, int eat_lit,
			       const char **str_out, size_t *offset_out, PDC_charset char_set,
			       const char *whatfn, int safe);

/* ================================================================================ */
/* INTERNAL READ ROUTINES (helpers) */

PDC_error_t PDCI_char_lit_read(PDC_t *pdc, const PDC_base_csm *csm, PDC_base_ed *ed,
			       PDC_char c, PDC_charset char_set,
			       const char* whatfn, int safe);

PDC_error_t PDCI_str_lit_read(PDC_t *pdc, const PDC_base_csm *csm, PDC_base_ed *ed, const PDC_string *s,
			      PDC_charset char_set, const char *whatfn, int safe);

PDC_error_t PDCI_Cstr_lit_read(PDC_t *pdc, const PDC_base_csm *csm, PDC_base_ed *ed, const char *s,
			      PDC_charset char_set, const char *whatfn, int safe);

PDC_error_t PDCI_countX(PDC_t *pdc, const PDC_base_csm *csm, PDC_uint8 x, int eor_required,
			PDC_base_ed *ed, PDC_int32 *res_out,
			PDC_charset char_set, const char *whatfn, int safe);


PDC_error_t PDCI_countXtoY(PDC_t *pdc, const PDC_base_csm *csm, PDC_uint8 x, PDC_uint8 y,
			   PDC_base_ed *ed, PDC_int32 *res_out,
			   PDC_charset char_set, const char *whatfn, int safe);

PDC_error_t PDCI_date_read(PDC_t *pdc, const PDC_base_csm *csm, PDC_char stopChar,
			   PDC_base_ed *ed, PDC_uint32 *res_out,
			   PDC_charset char_set, const char *whatfn, int safe);

PDC_error_t PDCI_char_read(PDC_t *pdc, const PDC_base_csm *csm, PDC_base_ed *ed, PDC_char *c_out,
			   PDC_charset char_set, const char *whatfn, int safe);


PDC_error_t PDCI_string_FW_read(PDC_t *pdc, const PDC_base_csm *csm, size_t width,
				PDC_base_ed *ed, PDC_string *s_out,
				PDC_charset char_set, const char *whatfn, int safe);

PDC_error_t PDCI_string_read(PDC_t *pdc, const PDC_base_csm *csm, PDC_char stopChar,
			     PDC_base_ed *ed, PDC_string *s_out,
			     PDC_charset char_set, const char *whatfn, int safe);

PDC_error_t PDCI_string_ME_read(PDC_t *pdc, const PDC_base_csm *csm, const char *matchRegexp,
				PDC_base_ed *ed, PDC_string *s_out,
				PDC_charset char_set, const char *whatfn, int safe);

PDC_error_t PDCI_string_CME_read(PDC_t *pdc, const PDC_base_csm *csm, PDC_regexp_t *matchRegexp,
				 PDC_base_ed *ed, PDC_string *s_out,
				 PDC_charset char_set, const char *whatfn, int safe);

PDC_error_t PDCI_string_SE_read(PDC_t *pdc, const PDC_base_csm *csm, const char *stopRegexp,
				PDC_base_ed *ed, PDC_string *s_out,
				PDC_charset char_set, const char *whatfn, int safe);

PDC_error_t PDCI_string_CSE_read(PDC_t *pdc, const PDC_base_csm *csm, PDC_regexp_t *stopRegexp,
				 PDC_base_ed *ed, PDC_string *s_out,
				 PDC_charset char_set, const char *whatfn, int safe);

/* ================================================================================ */
/* INTERNAL WRITE ROUTINES (helpers) */


ssize_t PDCI_char_lit_write2io(PDC_t *pdc, Sfio_t *io, PDC_char c,
			       PDC_charset char_set, const char *whatfn, int safe);

ssize_t PDCI_char_lit_write2buf(PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, PDC_char c,
				PDC_charset char_set, const char *whatfn, int safe);

ssize_t PDCI_str_lit_write2io(PDC_t *pdc, Sfio_t *io, const PDC_string *s,
			      PDC_charset char_set, const char *whatfn, int safe);

ssize_t PDCI_str_lit_write2buf(PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, const PDC_string *s,
			       PDC_charset char_set, const char *whatfn, int safe);

ssize_t PDCI_Cstr_lit_write2io(PDC_t *pdc, Sfio_t *io, const char *s,
			       PDC_charset char_set, const char *whatfn, int safe);

ssize_t PDCI_Cstr_lit_write2buf(PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, const char *s,
				PDC_charset char_set, const char *whatfn, int safe);

/* ================================================================================ */
/* INTERNAL invalid_valfn FUNCTIONS (helpers) */

PDC_inv_valfn        PDCI_get_inv_valfn(PDC_t* pdc, PDC_inv_valfn_map_t *map, const char *type_name, int safe); 
PDC_inv_valfn        PDCI_set_inv_valfn(PDC_t* pdc, PDC_inv_valfn_map_t *map, const char *type_name, PDC_inv_valfn fn, int safe);
PDC_inv_valfn_map_t* PDCI_inv_valfn_map_create(PDC_t *pdc, int safe);
PDC_error_t          PDCI_inv_valfn_map_destroy(PDC_t *pdc, PDC_inv_valfn_map_t *map, int safe);

/* ================================================================================ */
/* INTERNAL char/string write FUNCTIONS (helpers) */

ssize_t PDCI_char_write2io (PDC_t *pdc, Sfio_t *io, const PDC_base_ed *ed,
			    PDC_char *c, PDC_charset char_set, const char *whatfn, int safe);
ssize_t PDCI_char_write2buf(PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, const PDC_base_ed *ed,
			    PDC_char *c, PDC_charset char_set, const char *whatfn, int safe);
ssize_t PDCI_string_FW_write2io(PDC_t *pdc, Sfio_t *io, size_t width, const PDC_base_ed *ed,
				PDC_string *s, PDC_charset char_set, const char *whatfn, int size);
ssize_t PDCI_string_FW_write2buf(PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full,
				 size_t width, const PDC_base_ed *ed,
				 PDC_string *s, PDC_charset char_set, const char *whatfn, int size);
ssize_t PDCI_string_write2io(PDC_t *pdc, Sfio_t *io, void *type_arg1, const PDC_base_ed *ed,
			     PDC_string *s, PDC_charset char_set, const char *inv_type, const char *whatfn, int safe);
ssize_t PDCI_string_write2buf(PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full,
			      void *type_arg1, const PDC_base_ed *ed, PDC_string *s,
			      PDC_charset char_set, const char *inv_type, const char *whatfn, int safe);

ssize_t PDCI_date_write2io(PDC_t *pdc, Sfio_t *io, void *type_arg1, const PDC_base_ed *ed,
			   PDC_uint32 *d, PDC_charset char_set, const char *inv_type, const char *whatfn, int safe);
ssize_t PDCI_date_write2buf(PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full,
			    void *type_arg1, const PDC_base_ed *ed, PDC_uint32 *d,
			    PDC_charset char_set, const char *inv_type, const char *whatfn, int safe);

/* ================================================================================
 * STRING COMPARISON
 */

#define PDC_string_eq(str1, str2) \
  ((str1)->len == (str2)->len && strncmp((str1)->str, (str2)->str, (str1)->len) == 0)

#define PDC_string_eq_Cstr(PDCstr, Cstr) \
  ((PDCstr)->len == strlen(Cstr) && strncmp((PDCstr)->str, (Cstr), (PDCstr)->len) == 0)

/* ================================================================================
 * CHAR/STRING SCAN FUNCTIONS
 */

#define PDC_char_lit_scan(pdc, c, s, eat_lit, c_out, offset_out) \
  PDCI_char_lit_scan(pdc, c, s, eat_lit, c_out, offset_out, PDCI_SAFE_DEF_CHARSET(pdc), "PDC_char_lit_scan", 1)

#define PDC_a_char_lit_scan(pdc, c, s, eat_lit, c_out, offset_out) \
  PDCI_char_lit_scan(pdc, c, s, eat_lit, c_out, offset_out, PDC_charset_ASCII, "PDC_a_char_lit_scan", 1)

#define PDC_e_char_lit_scan(pdc, c, s, eat_lit, c_out, offset_out) \
  PDCI_char_lit_scan(pdc, c, s, eat_lit, c_out, offset_out, PDC_charset_EBCDIC, "PDC_e_char_lit_scan", 1)

#define PDC_str_lit_scan(pdc, findStr, stopStr, eat_lit, str_out, offset_out) \
  PDCI_str_lit_scan(pdc, findStr, stopStr, eat_lit, str_out, offset_out, PDCI_SAFE_DEF_CHARSET(pdc), "PDC_str_lit_scan", 1)

#define PDC_a_str_lit_scan(pdc, findStr, stopStr, eat_lit, str_out, offset_out) \
  PDCI_str_lit_scan(pdc, findStr, stopStr, eat_lit, str_out, offset_out, PDC_charset_ASCII, "PDC_a_str_lit_scan", 1)

#define PDC_e_str_lit_scan(pdc, findStr, stopStr, eat_lit, str_out, offset_out) \
  PDCI_str_lit_scan(pdc, findStr, stopStr, eat_lit, str_out, offset_out, PDC_charset_EBCDIC, "PDC_e_str_lit_scan", 1)

#define PDC_Cstr_lit_scan(pdc, findStr, stopStr, eat_lit, str_out, offset_out) \
  PDCI_Cstr_lit_scan(pdc, findStr, stopStr, eat_lit, str_out, offset_out, PDCI_SAFE_DEF_CHARSET(pdc), "PDC_Cstr_lit_scan", 1)

#define PDC_a_Cstr_lit_scan(pdc, findStr, stopStr, eat_lit, str_out, offset_out) \
  PDCI_Cstr_lit_scan(pdc, findStr, stopStr, eat_lit, str_out, offset_out, PDC_charset_ASCII, "PDC_a_Cstr_lit_scan", 1)

#define PDC_e_Cstr_lit_scan(pdc, findStr, stopStr, eat_lit, str_out, offset_out) \
  PDCI_Cstr_lit_scan(pdc, findStr, stopStr, eat_lit, str_out, offset_out, PDC_charset_EBCDIC, "PDC_e_Cstr_lit_scan", 1)

/* ================================================================================
 * CHAR/STRING READ FUNCTIONS
 */

#define PDC_char_lit_read(pdc, csm, ed, c) \
  PDCI_char_lit_read(pdc, csm, ed, c, PDCI_SAFE_DEF_CHARSET(pdc), "PDC_char_lit_read", 1)

#define PDC_a_char_lit_read(pdc, csm, ed, c) \
  PDCI_char_lit_read(pdc, csm, ed, c, PDC_charset_ASCII, "PDC_a_char_lit_read", 1)

#define PDC_e_char_lit_read(pdc, csm, ed, c) \
  PDCI_char_lit_read(pdc, csm, ed, c, PDC_charset_EBCDIC, "PDC_e_char_lit_read", 1)

#define PDC_str_lit_read(pdc, csm, ed, s) \
  PDCI_str_lit_read(pdc, csm, ed, s, PDCI_SAFE_DEF_CHARSET(pdc), "PDC_str_lit_read", 1)

#define PDC_a_str_lit_read(pdc, csm, ed, s) \
  PDCI_str_lit_read(pdc, csm, ed, s, PDC_charset_ASCII, "PDC_a_str_lit_read", 1)

#define PDC_e_str_lit_read(pdc, csm, ed, s) \
  PDCI_str_lit_read(pdc, csm, ed, s, PDC_charset_EBCDIC, "PDC_e_str_lit_read", 1)

#define PDC_Cstr_lit_read(pdc, csm, ed, s) \
  PDCI_Cstr_lit_read(pdc, csm, ed, s, PDCI_SAFE_DEF_CHARSET(pdc), "PDC_Cstr_lit_read", 1)

#define PDC_a_Cstr_lit_read(pdc, csm, ed, s) \
  PDCI_Cstr_lit_read(pdc, csm, ed, s, PDC_charset_ASCII, "PDC_a_Cstr_lit_read", 1)

#define PDC_e_Cstr_lit_read(pdc, csm, ed, s) \
  PDCI_Cstr_lit_read(pdc, csm, ed, s, PDC_charset_EBCDIC, "PDC_e_Cstr_lit_read", 1)

#define PDC_countX(pdc, csm, x, eor_required, ed, res_out) \
  PDCI_countX(pdc, csm, x, eor_required, ed, res_out, PDCI_SAFE_DEF_CHARSET(pdc), "PDC_countX", 1)

#define PDC_a_countX(pdc, csm, x, eor_required, ed, res_out) \
  PDCI_countX(pdc, csm, x, eor_required, ed, res_out, PDC_charset_ASCII, "PDC_a_countX", 1)

#define PDC_e_countX(pdc, csm, x, eor_required, ed, res_out) \
  PDCI_countX(pdc, csm, x, eor_required, ed, res_out, PDC_charset_EBCDIC, "PDC_e_countX", 1)

#define PDC_countXtoY(pdc, csm, x, y, ed, res_out) \
  PDCI_countXtoY(pdc, csm, x, y, ed, res_out, PDCI_SAFE_DEF_CHARSET(pdc), "PDC_countXtoY", 1)

#define PDC_a_countXtoY(pdc, csm, x, y, ed, res_out) \
  PDCI_countXtoY(pdc, csm, x, y, ed, res_out, PDC_charset_ASCII, "PDC_a_countXtoY", 1)

#define PDC_e_countXtoY(pdc, csm, x, y, ed, res_out) \
  PDCI_countXtoY(pdc, csm, x, y, ed, res_out, PDC_charset_EBCDIC, "PDC_e_countXtoY", 1)

#define PDC_date_read(pdc, csm, stopChar, ed, res_out) \
  PDCI_date_read(pdc, csm, stopChar, ed, res_out, PDCI_SAFE_DEF_CHARSET(pdc), "PDC_date_read", 1)

#define PDC_a_date_read(pdc, csm, stopChar, ed, res_out) \
  PDCI_date_read(pdc, csm, stopChar, ed, res_out, PDC_charset_ASCII, "PDC_a_date_read", 1)

#define PDC_e_date_read(pdc, csm, stopChar, ed, res_out) \
  PDCI_date_read(pdc, csm, stopChar, ed, res_out, PDC_charset_EBCDIC, "PDC_e_date_read", 1)

#define PDC_char_read(pdc, csm, ed, c_out) \
  PDCI_char_read(pdc, csm, ed, c_out, PDCI_SAFE_DEF_CHARSET(pdc), "PDC_char_read", 1)

#define PDC_a_char_read(pdc, csm, ed, c_out) \
  PDCI_char_read(pdc, csm, ed, c_out, PDC_charset_ASCII, "PDC_a_char_read", 1)

#define PDC_e_char_read(pdc, csm, ed, c_out) \
  PDCI_char_read(pdc, csm, ed, c_out, PDC_charset_EBCDIC, "PDC_e_char_read", 1)

#define PDC_string_FW_read(pdc, csm, width, ed, s_out) \
  PDCI_string_FW_read(pdc, csm, width, ed, s_out, PDCI_SAFE_DEF_CHARSET(pdc), "PDC_string_FW_read", 1)

#define PDC_a_string_FW_read(pdc, csm, width, ed, s_out) \
  PDCI_string_FW_read(pdc, csm, width, ed, s_out, PDC_charset_ASCII, "PDC_a_string_FW_read", 1)

#define PDC_e_string_FW_read(pdc, csm, width, ed, s_out) \
  PDCI_string_FW_read(pdc, csm, width, ed, s_out, PDC_charset_EBCDIC, "PDC_e_string_FW_read", 1)


#define PDC_string_read(pdc, csm, stopChar, ed, s_out) \
  PDCI_string_read(pdc, csm, stopChar, ed, s_out, PDCI_SAFE_DEF_CHARSET(pdc), "PDC_string_read", 1)

#define PDC_a_string_read(pdc, csm, stopChar, ed, s_out) \
  PDCI_string_read(pdc, csm, stopChar, ed, s_out, PDC_charset_ASCII, "PDC_a_string_read", 1)

#define PDC_e_string_read(pdc, csm, stopChar, ed, s_out) \
  PDCI_string_read(pdc, csm, stopChar, ed, s_out, PDC_charset_EBCDIC, "PDC_e_string_read", 1)


#define PDC_string_ME_read(pdc, csm, matchRegexp, ed, s_out) \
  PDCI_string_ME_read(pdc, csm, matchRegexp, ed, s_out, PDCI_SAFE_DEF_CHARSET(pdc), "PDC_string_ME_read", 1)

#define PDC_a_string_ME_read(pdc, csm, matchRegexp, ed, s_out) \
  PDCI_string_ME_read(pdc, csm, matchRegexp, ed, s_out, PDC_charset_ASCII, "PDC_a_string_ME_read", 1)

#define PDC_e_string_ME_read(pdc, csm, matchRegexp, ed, s_out) \
  PDCI_string_ME_read(pdc, csm, matchRegexp, ed, s_out, PDC_charset_EBCDIC, "PDC_e_string_ME_read", 1)


#define PDC_string_CME_read(pdc, csm, matchRegexp, ed, s_out) \
  PDCI_string_CME_read(pdc, csm, matchRegexp, ed, s_out, PDCI_SAFE_DEF_CHARSET(pdc), "PDC_string_CME_read", 1)

#define PDC_a_string_CME_read(pdc, csm, matchRegexp, ed, s_out) \
  PDCI_string_CME_read(pdc, csm, matchRegexp, ed, s_out, PDC_charset_ASCII, "PDC_a_string_CME_read", 1)

#define PDC_e_string_CME_read(pdc, csm, matchRegexp, ed, s_out) \
  PDCI_string_CME_read(pdc, csm, matchRegexp, ed, s_out, PDC_charset_EBCDIC, "PDC_e_string_CME_read", 1)


#define PDC_string_SE_read(pdc, csm, stopRegexp, ed, s_out) \
  PDCI_string_SE_read(pdc, csm, stopRegexp, ed, s_out, PDCI_SAFE_DEF_CHARSET(pdc), "PDC_string_SE_read", 1)

#define PDC_a_string_SE_read(pdc, csm, stopRegexp, ed, s_out) \
  PDCI_string_SE_read(pdc, csm, stopRegexp, ed, s_out, PDC_charset_ASCII, "PDC_a_string_SE_read", 1)

#define PDC_e_string_SE_read(pdc, csm, stopRegexp, ed, s_out) \
  PDCI_string_SE_read(pdc, csm, stopRegexp, ed, s_out, PDC_charset_EBCDIC, "PDC_e_string_SE_read", 1)


#define PDC_string_CSE_read(pdc, csm, stopRegexp, ed, s_out) \
  PDCI_string_CSE_read(pdc, csm, stopRegexp, ed, s_out, PDCI_SAFE_DEF_CHARSET(pdc), "PDC_string_CSE_read", 1)

#define PDC_a_string_CSE_read(pdc, csm, stopRegexp, ed, s_out) \
  PDCI_string_CSE_read(pdc, csm, stopRegexp, ed, s_out, PDC_charset_ASCII, "PDC_a_string_CSE_read", 1)

#define PDC_e_string_CSE_read(pdc, csm, stopRegexp, ed, s_out) \
  PDCI_string_CSE_read(pdc, csm, stopRegexp, ed, s_out, PDC_charset_EBCDIC, "PDC_e_string_CSE_read", 1)


/* ================================================================================
 * DEFAULT STRING TO INTEGER READ FUNCTIONS
 */

#define PDC_int8_read(pdc, csm, ed, res_out) \
  PDCI_CHARSET_SWITCH(pdc, PDC, int8_read, (pdc, csm, ed, res_out))

#define PDC_int16_read(pdc, csm, ed, res_out) \
  PDCI_CHARSET_SWITCH(pdc, PDC, int16_read, (pdc, csm, ed, res_out))

#define PDC_int32_read(pdc, csm, ed, res_out) \
  PDCI_CHARSET_SWITCH(pdc, PDC, int32_read, (pdc, csm, ed, res_out))

#define PDC_int64_read(pdc, csm, ed, res_out) \
  PDCI_CHARSET_SWITCH(pdc, PDC, int64_read, (pdc, csm, ed, res_out))

#define PDC_uint8_read(pdc, csm, ed, res_out) \
  PDCI_CHARSET_SWITCH(pdc, PDC, uint8_read, (pdc, csm, ed, res_out))

#define PDC_uint16_read(pdc, csm, ed, res_out) \
  PDCI_CHARSET_SWITCH(pdc, PDC, uint16_read, (pdc, csm, ed, res_out))

#define PDC_uint32_read(pdc, csm, ed, res_out) \
  PDCI_CHARSET_SWITCH(pdc, PDC, uint32_read, (pdc, csm, ed, res_out))

#define PDC_uint64_read(pdc, csm, ed, res_out) \
  PDCI_CHARSET_SWITCH(pdc, PDC, uint64_read, (pdc, csm, ed, res_out))

#define PDC_int8_FW_read(pdc, csm, width, ed, res_out) \
  PDCI_CHARSET_SWITCH(pdc, PDC, int8_FW_read, (pdc, csm, width, ed, res_out))

#define PDC_int16_FW_read(pdc, csm, width, ed, res_out) \
  PDCI_CHARSET_SWITCH(pdc, PDC, int16_FW_read, (pdc, csm, width, ed, res_out))

#define PDC_int32_FW_read(pdc, csm, width, ed, res_out) \
  PDCI_CHARSET_SWITCH(pdc, PDC, int32_FW_read, (pdc, csm, width, ed, res_out))

#define PDC_int64_FW_read(pdc, csm, width, ed, res_out) \
  PDCI_CHARSET_SWITCH(pdc, PDC, int64_FW_read, (pdc, csm, width, ed, res_out))

#define PDC_uint8_FW_read(pdc, csm, width, ed, res_out) \
  PDCI_CHARSET_SWITCH(pdc, PDC, uint8_FW_read, (pdc, csm, width, ed, res_out))

#define PDC_uint16_FW_read(pdc, csm, width, ed, res_out) \
  PDCI_CHARSET_SWITCH(pdc, PDC, uint16_FW_read, (pdc, csm, width, ed, res_out))

#define PDC_uint32_FW_read(pdc, csm, width, ed, res_out) \
  PDCI_CHARSET_SWITCH(pdc, PDC, uint32_FW_read, (pdc, csm, width, ed, res_out))

#define PDC_uint64_FW_read(pdc, csm, width, ed, res_out) \
  PDCI_CHARSET_SWITCH(pdc, PDC, uint64_FW_read, (pdc, csm, width, ed, res_out))

/* ================================================================================
 * WRITE FUNCTIONS
 */

#define PDC_a_char_lit_write2io(pdc, io, c) \
  PDCI_char_lit_write2io(pdc, io, c, PDC_charset_ASCII, "PDC_a_char_lit_write2io", 1)

#define PDC_e_char_lit_write2io(pdc, io, c) \
  PDCI_char_lit_write2io(pdc, io, c, PDC_charset_EBCDIC, "PDC_e_char_lit_write2io", 1)

#define PDC_char_lit_write2io(pdc, io, c) \
  PDCI_char_lit_write2io(pdc, io, c, PDCI_SAFE_DEF_CHARSET(pdc), "PDC_char_lit_write2io", 1)

#define PDC_a_str_lit_write2io(pdc, io, s) \
  PDCI_str_lit_write2io(pdc, io, s, PDC_charset_ASCII, "PDC_a_str_lit_write2io", 1)

#define PDC_e_str_lit_write2io(pdc, io, s) \
  PDCI_str_lit_write2io(pdc, io, s, PDC_charset_EBCDIC, "PDC_e_str_lit_write2io", 1)

#define PDC_str_lit_write2io(pdc, io, s) \
  PDCI_str_lit_write2io(pdc, io, s, PDCI_SAFE_DEF_CHARSET(pdc), "PDC_str_lit_write2io", 1)

#define PDC_a_Cstr_lit_write2io(pdc, io, s) \
  PDCI_Cstr_lit_write2io(pdc, io, s, PDC_charset_ASCII, "PDC_a_Cstr_lit_write2io", 1)

#define PDC_e_Cstr_lit_write2io(pdc, io, s) \
  PDCI_Cstr_lit_write2io(pdc, io, s, PDC_charset_EBCDIC, "PDC_e_Cstr_lit_write2io", 1)

#define PDC_Cstr_lit_write2io(pdc, io, s) \
  PDCI_Cstr_lit_write2io(pdc, io, s, PDCI_SAFE_DEF_CHARSET(pdc), "PDC_Cstr_lit_write2io", 1)

#define PDC_a_char_lit_write2buf(pdc, buf, buf_len, buf_full, c) \
  PDCI_char_lit_write2buf(pdc, buf, buf_len, buf_full, c, PDC_charset_ASCII, "PDC_a_char_lit_write2buf", 1)

#define PDC_e_char_lit_write2buf(pdc, buf, buf_len, buf_full, c) \
  PDCI_char_lit_write2buf(pdc, buf, buf_len, buf_full, c, PDC_charset_EBCDIC, "PDC_e_char_lit_write2buf", 1)

#define PDC_char_lit_write2buf(pdc, buf, buf_len, buf_full, c) \
  PDCI_char_lit_write2buf(pdc, buf, buf_len, buf_full, c, PDCI_SAFE_DEF_CHARSET(pdc), "PDC_char_lit_write2buf", 1)

#define PDC_a_str_lit_write2buf(pdc, buf, buf_len, buf_full, s) \
  PDCI_str_lit_write2buf(pdc, buf, buf_len, buf_full, s, PDC_charset_ASCII, "PDC_a_str_lit_write2buf", 1)

#define PDC_e_str_lit_write2buf(pdc, buf, buf_len, buf_full, s) \
  PDCI_str_lit_write2buf(pdc, buf, buf_len, buf_full, s, PDC_charset_EBCDIC, "PDC_e_str_lit_write2buf", 1)

#define PDC_str_lit_write2buf(pdc, buf, buf_len, buf_full, s) \
  PDCI_str_lit_write2buf(pdc, buf, buf_len, buf_full, s, PDCI_SAFE_DEF_CHARSET(pdc), "PDC_str_lit_write2buf", 1)

#define PDC_a_Cstr_lit_write2buf(pdc, buf, buf_len, buf_full, s) \
  PDCI_Cstr_lit_write2buf(pdc, buf, buf_len, buf_full, s, PDC_charset_ASCII, "PDC_a_Cstr_lit_write2buf", 1)

#define PDC_e_Cstr_lit_write2buf(pdc, buf, buf_len, buf_full, s) \
  PDCI_Cstr_lit_write2buf(pdc, buf, buf_len, buf_full, s, PDC_charset_EBCDIC, "PDC_e_Cstr_lit_write2buf", 1)

#define PDC_Cstr_lit_write2buf(pdc, buf, buf_len, buf_full, s) \
  PDCI_Cstr_lit_write2buf(pdc, buf, buf_len, buf_full, s, PDCI_SAFE_DEF_CHARSET(pdc), "PDC_Cstr_lit_write2buf", 1)

#define PDC_char_write2io(pdc, io, ed, c) \
  PDCI_char_write2io(pdc, io, ed, c, PDCI_SAFE_DEF_CHARSET(pdc), "PDC_char_write2io", 1)

#define PDC_a_char_write2io(pdc, io, ed, c) \
  PDCI_char_write2io(pdc, io, ed, c, pdc_charset_ASCII, "PDC_a_char_write2io", 1)

#define PDC_e_char_write2io(pdc, io, ed, c) \
  PDCI_char_write2io(pdc, io, ed, c, pdc_charset_EBCDIC, "PDC_e_char_write2io", 1)

#define PDC_char_write2buf(pdc, buf, buf_len, buf_full, ed, c) \
  PDCI_char_write2buf(pdc, buf, buf_len, buf_full, ed, c, PDCI_SAFE_DEF_CHARSET(pdc), "PDC_char_write2buf", 1)

#define PDC_a_char_write2buf(pdc, buf, buf_len, buf_full, ed, c) \
  PDCI_char_write2buf(pdc, buf, buf_len, buf_full, ed, c, PDC_charset_ASCII, "PDC_a_char_write2buf", 1)

#define PDC_e_char_write2buf(pdc, buf, buf_len, buf_full, ed, c) \
  PDCI_char_write2buf(pdc, buf, buf_len, buf_full, ed, c, PDC_charset_EBCDIC, "PDC_e_char_write2buf", 1)

#define PDC_string_FW_write2io(pdc, io, width, ed, s) \
  PDCI_string_FW_write2io(pdc, io, width, ed, s, PDCI_SAFE_DEF_CHARSET(pdc), "PDC_string_FW_write2io", 1)

#define PDC_a_string_FW_write2io(pdc, io, width, ed, s) \
  PDCI_string_FW_write2io(pdc, io, width, ed, s, PDC_charset_ASCII, "PDC_a_string_FW_write2io", 1)

#define PDC_e_string_FW_write2io(pdc, io, width, ed, s) \
  PDCI_string_FW_write2io(pdc, io, width, ed, s, PDC_charset_EBCDIC, "PDC_e_string_FW_write2io", 1)

#define PDC_string_FW_write2buf(pdc, buf, buf_len, buf_full, width, ed, s) \
  PDCI_string_FW_write2buf(pdc, buf, buf_len, buf_full, width, ed, s, PDCI_SAFE_DEF_CHARSET(pdc), "PDC_string_FW_write2buf", 1)

#define PDC_a_string_FW_write2buf(pdc, buf, buf_len, buf_full, width, ed, s) \
  PDCI_string_FW_write2buf(pdc, buf, buf_len, buf_full, width, ed, s, PDC_charset_ASCII, "PDC_a_string_FW_write2buf", 1)

#define PDC_e_string_FW_write2buf(pdc, buf, buf_len, buf_full, width, ed, s) \
  PDCI_string_FW_write2buf(pdc, buf, buf_len, buf_full, width, ed, s, PDC_charset_EBCDIC, "PDC_e_string_FW_write2buf", 1)

#define PDC_string_write2io(pdc, io, stopChar, ed, s) \
  PDCI_string_write2io(pdc, io, ((void*)(stopChar)), ed, s, PDCI_SAFE_DEF_CHARSET(pdc), "PDC_string", "PDC_string_write2io", 1)

#define PDC_a_string_write2io(pdc, io, stopChar, ed, s) \
  PDCI_string_write2io(pdc, io, ((void*)(stopChar)), ed, s, PDC_charset_ASCII, "PDC_string", "PDC_a_string_write2io", 1)

#define PDC_e_string_write2io(pdc, io, stopChar, ed, s) \
  PDCI_string_write2io(pdc, io, ((void*)(stopChar)), ed, s, PDC_charset_EBCDIC, "PDC_string", "PDC_e_string_write2io", 1)

#define PDC_string_write2buf(pdc, buf, buf_len, buf_full, stopChar, ed, s) \
  PDCI_string_write2buf(pdc, buf, buf_len, buf_full, ((void*)(stopChar)), ed, s, PDCI_SAFE_DEF_CHARSET(pdc), "PDC_string", "PDC_string_write2buf", 1)

#define PDC_a_string_write2buf(pdc, buf, buf_len, buf_full, stopChar, ed, s) \
  PDCI_string_write2buf(pdc, buf, buf_len, buf_full, ((void*)(stopChar)), ed, s, PDC_charset_ASCII, "PDC_string", "PDC_a_string_write2buf", 1)

#define PDC_e_string_write2buf(pdc, buf, buf_len, buf_full, stopChar, ed, s) \
  PDCI_string_write2buf(pdc, buf, buf_len, buf_full, ((void*)(stopChar)), ed, s, PDC_charset_EBCDIC, "PDC_string", "PDC_e_string_write2buf", 1)

#define PDC_string_ME_write2io(pdc, io, matchRegexp, ed, s) \
  PDCI_string_write2io(pdc, io, ((void*)(matchRegexp)), ed, s, PDCI_SAFE_DEF_CHARSET(pdc), "PDC_string_ME", "PDC_string_ME_write2io", 1)

#define PDC_a_string_ME_write2io(pdc, io, matchRegexp, ed, s) \
  PDCI_string_write2io(pdc, io, ((void*)(matchRegexp)), ed, s, PDCI_charset_ASCII, "PDC_string_ME", "PDC_a_string_ME_write2io", 1)

#define PDC_e_string_ME_write2io(pdc, io, matchRegexp, ed, s) \
  PDCI_string_write2io(pdc, io, ((void*)(matchRegexp)), ed, s, PDCI_charset_EBCDIC, "PDC_string_ME", "PDC_e_string_ME_write2io", 1)

#define PDC_string_ME_write2buf(pdc, buf, buf_len, buf_full, matchRegexp, ed, s) \
  PDCI_string_write2buf(pdc, buf, buf_len, buf_full, ((void*)(matchRegexp)), ed, s, PDCI_SAFE_DEF_CHARSET(pdc), "PDC_string_ME", "PDC_ME_string_write2buf", 1)

#define PDC_a_string_ME_write2buf(pdc, buf, buf_len, buf_full, matchRegexp, ed, s) \
  PDCI_string_write2buf(pdc, buf, buf_len, buf_full, ((void*)(matchRegexp)), ed, s, PDC_charset_ASCII, "PDC_string_ME", "PDC_a_string_ME_write2buf", 1)

#define PDC_e_string_ME_write2buf(pdc, buf, buf_len, buf_full, matchRegexp, ed, s) \
  PDCI_string_write2buf(pdc, buf, buf_len, buf_full, ((void*)(matchRegexp)), ed, s, PDC_charset_EBCDIC, "PDC_string_ME", "PDC_e_string_ME_write2buf", 1)

#define PDC_string_CME_write2io(pdc, io, matchRegexp, ed, s) \
  PDCI_string_write2io(pdc, io, ((void*)(matchRegexp)), ed, s, PDCI_SAFE_DEF_CHARSET(pdc), "PDC_string_CME", "PDC_string_CME_write2io", 1)

#define PDC_a_string_CME_write2io(pdc, io, matchRegexp, ed, s) \
  PDCI_string_write2io(pdc, io, ((void*)(matchRegexp)), ed, s, PDCI_charset_ASCII, "PDC_string_CME", "PDC_a_string_CME_write2io", 1)

#define PDC_e_string_CME_write2io(pdc, io, matchRegexp, ed, s) \
  PDCI_string_write2io(pdc, io, ((void*)(matchRegexp)), ed, s, PDCI_charset_EBCDIC, "PDC_string_CME", "PDC_e_string_CME_write2io", 1)

#define PDC_string_CME_write2buf(pdc, buf, buf_len, buf_full, matchRegexp, ed, s) \
  PDCI_string_write2buf(pdc, buf, buf_len, buf_full, ((void*)(matchRegexp)), ed, s, PDCI_SAFE_DEF_CHARSET(pdc), "PDC_string_CME", "PDC_CME_string_write2buf", 1)

#define PDC_a_string_CME_write2buf(pdc, buf, buf_len, buf_full, matchRegexp, ed, s) \
  PDCI_string_write2buf(pdc, buf, buf_len, buf_full, ((void*)(matchRegexp)), ed, s, PDC_charset_ASCII, "PDC_string_CME", "PDC_a_string_CME_write2buf", 1)

#define PDC_e_string_CME_write2buf(pdc, buf, buf_len, buf_full, matchRegexp, ed, s) \
  PDCI_string_write2buf(pdc, buf, buf_len, buf_full, ((void*)(matchRegexp)), ed, s, PDC_charset_EBCDIC, "PDC_string_CME", "PDC_e_string_CME_write2buf", 1)

#define PDC_string_SE_write2io(pdc, io, stopRegexp, ed, s) \
  PDCI_string_write2io(pdc, io, ((void*)(stopRegexp)), ed, s, PDCI_SAFE_DEF_CHARSET(pdc), "PDC_string_SE", "PDC_string_SE_write2io", 1)

#define PDC_a_string_SE_write2io(pdc, io, stopRegexp, ed, s) \
  PDCI_string_write2io(pdc, io, ((void*)(stopRegexp)), ed, s, PDCI_charset_ASCII, "PDC_string_SE", "PDC_a_string_SE_write2io", 1)

#define PDC_e_string_SE_write2io(pdc, io, stopRegexp, ed, s) \
  PDCI_string_write2io(pdc, io, ((void*)(stopRegexp)), ed, s, PDCI_charset_EBCDIC, "PDC_string_SE", "PDC_e_string_SE_write2io", 1)

#define PDC_string_SE_write2buf(pdc, buf, buf_len, buf_full, stopRegexp, ed, s) \
  PDCI_string_write2buf(pdc, buf, buf_len, buf_full, ((void*)(stopRegexp)), ed, s, PDCI_SAFE_DEF_CHARSET(pdc), "PDC_string_SE", "PDC_SE_string_write2buf", 1)

#define PDC_a_string_SE_write2buf(pdc, buf, buf_len, buf_full, stopRegexp, ed, s) \
  PDCI_string_write2buf(pdc, buf, buf_len, buf_full, ((void*)(stopRegexp)), ed, s, PDC_charset_ASCII, "PDC_string_SE", "PDC_a_string_SE_write2buf", 1)

#define PDC_e_string_SE_write2buf(pdc, buf, buf_len, buf_full, stopRegexp, ed, s) \
  PDCI_string_write2buf(pdc, buf, buf_len, buf_full, ((void*)(stopRegexp)), ed, s, PDC_charset_EBCDIC, "PDC_string_SE", "PDC_e_string_SE_write2buf", 1)

#define PDC_string_CSE_write2io(pdc, io, stopRegexp, ed, s) \
  PDCI_string_write2io(pdc, io, ((void*)(stopRegexp)), ed, s, PDCI_SAFE_DEF_CHARSET(pdc), "PDC_string_CSE", "PDC_string_CSE_write2io", 1)

#define PDC_a_string_CSE_write2io(pdc, io, stopRegexp, ed, s) \
  PDCI_string_write2io(pdc, io, ((void*)(stopRegexp)), ed, s, PDCI_charset_ASCII, "PDC_string_CSE", "PDC_a_string_CSE_write2io", 1)

#define PDC_e_string_CSE_write2io(pdc, io, stopRegexp, ed, s) \
  PDCI_string_write2io(pdc, io, ((void*)(stopRegexp)), ed, s, PDCI_charset_EBCDIC, "PDC_string_CSE", "PDC_e_string_CSE_write2io", 1)

#define PDC_string_CSE_write2buf(pdc, buf, buf_len, buf_full, stopRegexp, ed, s) \
  PDCI_string_write2buf(pdc, buf, buf_len, buf_full, ((void*)(stopRegexp)), ed, s, PDCI_SAFE_DEF_CHARSET(pdc), "PDC_string_CSE", "PDC_CSE_string_write2buf", 1)

#define PDC_a_string_CSE_write2buf(pdc, buf, buf_len, buf_full, stopRegexp, ed, s) \
  PDCI_string_write2buf(pdc, buf, buf_len, buf_full, ((void*)(stopRegexp)), ed, s, PDC_charset_ASCII, "PDC_string_CSE", "PDC_a_string_CSE_write2buf", 1)

#define PDC_e_string_CSE_write2buf(pdc, buf, buf_len, buf_full, stopRegexp, ed, s) \
  PDCI_string_write2buf(pdc, buf, buf_len, buf_full, ((void*)(stopRegexp)), ed, s, PDC_charset_EBCDIC, "PDC_string_CSE", "PDC_e_string_CSE_write2buf", 1)

#define PDC_date_write2io(pdc, io, stopChar, ed, d) \
  PDCI_date_write2io(pdc, io, ((void*)(stopChar)), ed, d, PDCI_SAFE_DEF_CHARSET(pdc), "PDC_date", "PDC_date_write2io", 1)

#define PDC_a_date_write2io(pdc, io, stopChar, ed, d) \
  PDCI_date_write2io(pdc, io, ((void*)(stopChar)), ed, d, PDC_charset_ASCII, "PDC_date", "PDC_a_date_write2io", 1)

#define PDC_e_date_write2io(pdc, io, stopChar, ed, d) \
  PDCI_date_write2io(pdc, io, ((void*)(stopChar)), ed, d, PDC_charset_EBCDIC, "PDC_date", "PDC_e_date_write2io", 1)

#define PDC_date_write2buf(pdc, buf, buf_len, buf_full, stopChar, ed, d) \
  PDCI_date_write2buf(pdc, buf, buf_len, buf_full, ((void*)(stopChar)), ed, d, PDCI_SAFE_DEF_CHARSET(pdc), "PDC_date", "PDC_date_write2buf", 1)

#define PDC_a_date_write2buf(pdc, buf, buf_len, buf_full, stopChar, ed, d) \
  PDCI_date_write2buf(pdc, buf, buf_len, buf_full, ((void*)(stopChar)), ed, d, PDC_charset_ASCII, "PDC_date", "PDC_a_date_write2buf", 1)

#define PDC_e_date_write2buf(pdc, buf, buf_len, buf_full, stopChar, ed, d) \
  PDCI_date_write2buf(pdc, buf, buf_len, buf_full, ((void*)(stopChar)), ed, d, PDC_charset_EBCDIC, "PDC_date", "PDC_e_date_write2buf", 1)

#define PDC_int8_FW_write2io(pdc, io, width, ed, val) \
  PDCI_CHARSET_SWITCH(pdc, PDC, int8_FW_write2io, (pdc, io, width, ed, val))

#define PDC_int16_FW_write2io(pdc, io, width, ed, val) \
  PDCI_CHARSET_SWITCH(pdc, PDC, int16_FW_write2io, (pdc, io, width, ed, val))

#define PDC_int32_FW_write2io(pdc, io, width, ed, val) \
  PDCI_CHARSET_SWITCH(pdc, PDC, int32_FW_write2io, (pdc, io, width, ed, val))

#define PDC_int64_FW_write2io(pdc, io, width, ed, val) \
  PDCI_CHARSET_SWITCH(pdc, PDC, int64_FW_write2io, (pdc, io, width, ed, val))

#define PDC_uint8_FW_write2io(pdc, io, width, ed, val) \
  PDCI_CHARSET_SWITCH(pdc, PDC, uint8_FW_write2io, (pdc, io, width, ed, val))

#define PDC_uint16_FW_write2io(pdc, io, width, ed, val) \
  PDCI_CHARSET_SWITCH(pdc, PDC, uint16_FW_write2io, (pdc, io, width, ed, val))

#define PDC_uint32_FW_write2io(pdc, io, width, ed, val) \
  PDCI_CHARSET_SWITCH(pdc, PDC, uint32_FW_write2io, (pdc, io, width, ed, val))

#define PDC_uint64_FW_write2io(pdc, io, width, ed, val) \
  PDCI_CHARSET_SWITCH(pdc, PDC, uint64_FW_write2io, (pdc, io, width, ed, val))

#define PDC_int8_write2io(pdc, io, ed, val) \
  PDCI_CHARSET_SWITCH(pdc, PDC, int8_write2io, (pdc, io, ed, val))

#define PDC_int16_write2io(pdc, io, ed, val) \
  PDCI_CHARSET_SWITCH(pdc, PDC, int16_write2io, (pdc, io, ed, val))

#define PDC_int32_write2io(pdc, io, ed, val) \
  PDCI_CHARSET_SWITCH(pdc, PDC, int32_write2io, (pdc, io, ed, val))

#define PDC_int64_write2io(pdc, io, ed, val) \
  PDCI_CHARSET_SWITCH(pdc, PDC, int64_write2io, (pdc, io, ed, val))

#define PDC_uint8_write2io(pdc, io, ed, val) \
  PDCI_CHARSET_SWITCH(pdc, PDC, uint8_write2io, (pdc, io, ed, val))

#define PDC_uint16_write2io(pdc, io, ed, val) \
  PDCI_CHARSET_SWITCH(pdc, PDC, uint16_write2io, (pdc, io, ed, val))

#define PDC_uint32_write2io(pdc, io, ed, val) \
  PDCI_CHARSET_SWITCH(pdc, PDC, uint32_write2io, (pdc, io, ed, val))

#define PDC_uint64_write2io(pdc, io, ed, val) \
  PDCI_CHARSET_SWITCH(pdc, PDC, uint64_write2io, (pdc, io, ed, val))

#define PDC_int8_FW_write2buf(pdc, buf, buf_len, buf_full, width, ed, val) \
  PDCI_CHARSET_SWITCH(pdc, PDC, int8_FW_write2buf, (pdc, buf, buf_len, buf_full, width, ed, val))

#define PDC_int16_FW_write2buf(pdc, buf, buf_len, buf_full, width, ed, val) \
  PDCI_CHARSET_SWITCH(pdc, PDC, int16_FW_write2buf, (pdc, buf, buf_len, buf_full, width, ed, val))

#define PDC_int32_FW_write2buf(pdc, buf, buf_len, buf_full, width, ed, val) \
  PDCI_CHARSET_SWITCH(pdc, PDC, int32_FW_write2buf, (pdc, buf, buf_len, buf_full, width, ed, val))

#define PDC_int64_FW_write2buf(pdc, buf, buf_len, buf_full, width, ed, val) \
  PDCI_CHARSET_SWITCH(pdc, PDC, int64_FW_write2buf, (pdc, buf, buf_len, buf_full, width, ed, val))

#define PDC_uint8_FW_write2buf(pdc, buf, buf_len, buf_full, width, ed, val) \
  PDCI_CHARSET_SWITCH(pdc, PDC, uint8_FW_write2buf, (pdc, buf, buf_len, buf_full, width, ed, val))

#define PDC_uint16_FW_write2buf(pdc, buf, buf_len, buf_full, width, ed, val) \
  PDCI_CHARSET_SWITCH(pdc, PDC, uint16_FW_write2buf, (pdc, buf, buf_len, buf_full, width, ed, val))

#define PDC_uint32_FW_write2buf(pdc, buf, buf_len, buf_full, width, ed, val) \
  PDCI_CHARSET_SWITCH(pdc, PDC, uint32_FW_write2buf, (pdc, buf, buf_len, buf_full, width, ed, val))

#define PDC_uint64_FW_write2buf(pdc, buf, buf_len, buf_full, width, ed, val) \
  PDCI_CHARSET_SWITCH(pdc, PDC, uint64_FW_write2buf, (pdc, buf, buf_len, buf_full, width, ed, val))

#define PDC_int8_write2buf(pdc, buf, buf_len, buf_full, ed, val) \
  PDCI_CHARSET_SWITCH(pdc, PDC, int8_write2buf, (pdc, buf, buf_len, buf_full, ed, val))

#define PDC_int16_write2buf(pdc, buf, buf_len, buf_full, ed, val) \
  PDCI_CHARSET_SWITCH(pdc, PDC, int16_write2buf, (pdc, buf, buf_len, buf_full, ed, val))

#define PDC_int32_write2buf(pdc, buf, buf_len, buf_full, ed, val) \
  PDCI_CHARSET_SWITCH(pdc, PDC, int32_write2buf, (pdc, buf, buf_len, buf_full, ed, val))

#define PDC_int64_write2buf(pdc, buf, buf_len, buf_full, ed, val) \
  PDCI_CHARSET_SWITCH(pdc, PDC, int64_write2buf, (pdc, buf, buf_len, buf_full, ed, val))

#define PDC_uint8_write2buf(pdc, buf, buf_len, buf_full, ed, val) \
  PDCI_CHARSET_SWITCH(pdc, PDC, uint8_write2buf, (pdc, buf, buf_len, buf_full, ed, val))

#define PDC_uint16_write2buf(pdc, buf, buf_len, buf_full, ed, val) \
  PDCI_CHARSET_SWITCH(pdc, PDC, uint16_write2buf, (pdc, buf, buf_len, buf_full, ed, val))

#define PDC_uint32_write2buf(pdc, buf, buf_len, buf_full, ed, val) \
  PDCI_CHARSET_SWITCH(pdc, PDC, uint32_write2buf, (pdc, buf, buf_len, buf_full, ed, val))

#define PDC_uint64_write2buf(pdc, buf, buf_len, buf_full, ed, val) \
  PDCI_CHARSET_SWITCH(pdc, PDC, uint64_write2buf, (pdc, buf, buf_len, buf_full, ed, val))

/* ================================================================================ */
/* invalid_valfn FUNCTIONS */

#define PDC_get_inv_valfn(pdc, map, type_name) \
  PDCI_get_inv_valfn(pdc, map, type_name, 1)

#define PDC_set_inv_valfn(pdc, map, type_name, fn) \
  PDCI_set_inv_valfn(pdc, map, type_name, fn, 0)

#define PDC_inv_valfn_map_create(pdc) \
  PDCI_inv_valfn_map_create(pdc, 1)

#define PDC_inv_valfn_map_destroy(pdc, map) \
  PDCI_inv_valfn_map_destroy(pdc, map, 1)

#endif   /*   ! FOR_CKIT             */
#endif   /*   ! __LIBPADSC_IMPL_H__  */
