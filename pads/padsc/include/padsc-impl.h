#ifdef _USE_PROTO
#pragma prototyped
#endif
/*
 * padsc library interface: macro implementations of certain function calls
 *   
 * NOTE: C prototypes for these functions are given in padsc.h
 *
 * Kathleen Fisher, Robert Gruber
 * AT&T Labs Research
 */

#ifndef __PADSC_IMPL_H__
#define __PADSC_IMPL_H__

#ifndef FOR_CKIT

/* ================================================================================
 * Helper Macro
 * 
 * PDC_DEF_CHARSET(pdc) : produces pdc->disc->def_charset (or PDC_charset_INVALID)
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

#ifndef NDEBUG
/* safe versions */

#define PDCI_DEF_CHARSET(pdc) \
  ((pdc && pdc->disc) ? (pdc->disc->def_charset) : PDC_charset_INVALID)

#define PDCI_CHARSET_SWITCH(pdc,fn_pre,fn_post,args) \
 ((pdc && pdc->disc && pdc->disc->def_charset == PDC_charset_ASCII) \
    ?  fn_pre ## _a_ ## fn_post args \
    :  fn_pre ## _e_ ## fn_post args)

#else
/* unsafe versions */

#define PDCI_DEF_CHARSET(pdc) \
   (pdc->disc->def_charset)

#define PDCI_CHARSET_SWITCH(pdc,fn_pre,fn_post,args) \
 ((pdc->disc->def_charset == PDC_charset_ASCII) \
    ?  fn_pre ## _a_ ## fn_post args \
    :  fn_pre ## _e_ ## fn_post args)

#endif /* !NDEBUG */

/* Macros for setting or testing parse state (PS) pd->pstate */
/* These can be used with both PDC_base_pd and with generated structured parse descriptors */

#define PDC_PS_init(pd)          do { (pd)->pstate = 0; } while (0)
#define PDC_PS_setPanic(pd)      do { (pd)->pstate |= PDC_Panic; } while (0)
#define PDC_PS_unsetPanic(pd)    do { (pd)->pstate &= ~PDC_Panic; } while (0)
#define PDC_PS_isPanic(pd)       ((pd)->pstate & PDC_Panic)

/* ================================================================================ */
/* INTERNAL SCAN ROUTINES (helpers) */

#if PDC_CONFIG_READ_FUNCTIONS > 0

PDC_error_t PDCI_char_lit_scan(PDC_t *pdc, PDC_char c, PDC_char s, int eat_lit,
			       PDC_char *c_out, size_t *offset_out, PDC_charset char_set,
			       const char *whatfn);

PDC_error_t PDCI_str_lit_scan(PDC_t *pdc, const PDC_string *findStr, const PDC_string *stopStr, int eat_lit,
			      PDC_string **str_out, size_t *offset_out, PDC_charset char_set,
			      const char *whatfn);

PDC_error_t PDCI_Cstr_lit_scan(PDC_t *pdc, const char *findStr, const char *stopStr, int eat_lit,
			       const char **str_out, size_t *offset_out, PDC_charset char_set,
			       const char *whatfn);

#endif /* PDC_CONFIG_READ_FUNCTIONS */

/* ================================================================================ */
/* INTERNAL READ ROUTINES (helpers) */

#if PDC_CONFIG_READ_FUNCTIONS > 0

PDC_error_t PDCI_char_lit_read(PDC_t *pdc, const PDC_base_m *m, PDC_base_pd *pd,
			       PDC_char c, PDC_charset char_set,
			       const char* whatfn);

PDC_error_t PDCI_str_lit_read(PDC_t *pdc, const PDC_base_m *m, PDC_base_pd *pd, const PDC_string *s,
			      PDC_charset char_set, const char *whatfn);

PDC_error_t PDCI_Cstr_lit_read(PDC_t *pdc, const PDC_base_m *m, PDC_base_pd *pd, const char *s,
			      PDC_charset char_set, const char *whatfn);

PDC_error_t PDCI_countX(PDC_t *pdc, const PDC_base_m *m, PDC_uint8 x, int eor_required,
			PDC_base_pd *pd, PDC_int32 *res_out,
			PDC_charset char_set, const char *whatfn);


PDC_error_t PDCI_countXtoY(PDC_t *pdc, const PDC_base_m *m, PDC_uint8 x, PDC_uint8 y,
			   PDC_base_pd *pd, PDC_int32 *res_out,
			   PDC_charset char_set, const char *whatfn);

PDC_error_t PDCI_date_read(PDC_t *pdc, const PDC_base_m *m, PDC_char stopChar,
			   PDC_base_pd *pd, PDC_uint32 *res_out,
			   PDC_charset char_set, const char *whatfn);

PDC_error_t PDCI_char_read(PDC_t *pdc, const PDC_base_m *m, PDC_base_pd *pd, PDC_char *c_out,
			   PDC_charset char_set, const char *whatfn);


PDC_error_t PDCI_string_FW_read(PDC_t *pdc, const PDC_base_m *m, size_t width,
				PDC_base_pd *pd, PDC_string *s_out,
				PDC_charset char_set, const char *whatfn);

PDC_error_t PDCI_string_read(PDC_t *pdc, const PDC_base_m *m, PDC_char stopChar,
			     PDC_base_pd *pd, PDC_string *s_out,
			     PDC_charset char_set, const char *whatfn);

PDC_error_t PDCI_string_ME_read(PDC_t *pdc, const PDC_base_m *m, const char *matchRegexp,
				PDC_base_pd *pd, PDC_string *s_out,
				PDC_charset char_set, const char *whatfn);

PDC_error_t PDCI_string_CME_read(PDC_t *pdc, const PDC_base_m *m, PDC_regexp_t *matchRegexp,
				 PDC_base_pd *pd, PDC_string *s_out,
				 PDC_charset char_set, const char *whatfn);

PDC_error_t PDCI_string_SE_read(PDC_t *pdc, const PDC_base_m *m, const char *stopRegexp,
				PDC_base_pd *pd, PDC_string *s_out,
				PDC_charset char_set, const char *whatfn);

PDC_error_t PDCI_string_CSE_read(PDC_t *pdc, const PDC_base_m *m, PDC_regexp_t *stopRegexp,
				 PDC_base_pd *pd, PDC_string *s_out,
				 PDC_charset char_set, const char *whatfn);

#endif  /* PDC_CONFIG_READ_FUNCTIONS */

/* ================================================================================ */
/* INTERNAL WRITE ROUTINES (helpers) */


#if PDC_CONFIG_WRITE_FUNCTIONS > 0

ssize_t PDCI_char_lit_write2io(PDC_t *pdc, Sfio_t *io, PDC_char c,
			       PDC_charset char_set, const char *whatfn);

ssize_t PDCI_char_lit_write2buf(PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, PDC_char c,
				PDC_charset char_set, const char *whatfn);

ssize_t PDCI_str_lit_write2io(PDC_t *pdc, Sfio_t *io, const PDC_string *s,
			      PDC_charset char_set, const char *whatfn);

ssize_t PDCI_str_lit_write2buf(PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, const PDC_string *s,
			       PDC_charset char_set, const char *whatfn);

ssize_t PDCI_Cstr_lit_write2io(PDC_t *pdc, Sfio_t *io, const char *s,
			       PDC_charset char_set, const char *whatfn);

ssize_t PDCI_Cstr_lit_write2buf(PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, const char *s,
				PDC_charset char_set, const char *whatfn);
#endif /* PDC_CONFIG_WRITE_FUNCTIONS */

/* ================================================================================ */
/* INTERNAL char/string write FUNCTIONS (helpers) */

#if PDC_CONFIG_WRITE_FUNCTIONS > 0

ssize_t PDCI_char_write2io (PDC_t *pdc, Sfio_t *io, PDC_base_pd *pd,
			    PDC_char *c, PDC_charset char_set, const char *whatfn);
ssize_t PDCI_char_write2buf(PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, PDC_base_pd *pd,
			    PDC_char *c, PDC_charset char_set, const char *whatfn);
ssize_t PDCI_string_FW_write2io(PDC_t *pdc, Sfio_t *io, size_t width, PDC_base_pd *pd,
				PDC_string *s, PDC_charset char_set, const char *whatfn);
ssize_t PDCI_string_FW_write2buf(PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full,
				 size_t width, PDC_base_pd *pd,
				 PDC_string *s, PDC_charset char_set, const char *whatfn);
ssize_t PDCI_string_write2io(PDC_t *pdc, Sfio_t *io, void *type_arg1, PDC_base_pd *pd,
			     PDC_string *s, PDC_charset char_set, const char *inv_type, const char *whatfn);
ssize_t PDCI_string_write2buf(PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full,
			      void *type_arg1, PDC_base_pd *pd, PDC_string *s,
			      PDC_charset char_set, const char *inv_type, const char *whatfn);

ssize_t PDCI_date_write2io(PDC_t *pdc, Sfio_t *io, void *type_arg1, PDC_base_pd *pd,
			   PDC_uint32 *d, PDC_charset char_set, const char *inv_type, const char *whatfn);
ssize_t PDCI_date_write2buf(PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full,
			    void *type_arg1, PDC_base_pd *pd, PDC_uint32 *d,
			    PDC_charset char_set, const char *inv_type, const char *whatfn);

#endif /* PDC_CONFIG_WRITE_FUNCTIONS */

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

#if PDC_CONFIG_READ_FUNCTIONS > 0

#define PDC_char_lit_scan(pdc, c, s, eat_lit, c_out, offset_out) \
  PDCI_char_lit_scan(pdc, c, s, eat_lit, c_out, offset_out, PDCI_DEF_CHARSET(pdc), "PDC_char_lit_scan")

#define PDC_a_char_lit_scan(pdc, c, s, eat_lit, c_out, offset_out) \
  PDCI_char_lit_scan(pdc, c, s, eat_lit, c_out, offset_out, PDC_charset_ASCII, "PDC_a_char_lit_scan")

#define PDC_e_char_lit_scan(pdc, c, s, eat_lit, c_out, offset_out) \
  PDCI_char_lit_scan(pdc, c, s, eat_lit, c_out, offset_out, PDC_charset_EBCDIC, "PDC_e_char_lit_scan")

#define PDC_str_lit_scan(pdc, findStr, stopStr, eat_lit, str_out, offset_out) \
  PDCI_str_lit_scan(pdc, findStr, stopStr, eat_lit, str_out, offset_out, PDCI_DEF_CHARSET(pdc), "PDC_str_lit_scan")

#define PDC_a_str_lit_scan(pdc, findStr, stopStr, eat_lit, str_out, offset_out) \
  PDCI_str_lit_scan(pdc, findStr, stopStr, eat_lit, str_out, offset_out, PDC_charset_ASCII, "PDC_a_str_lit_scan")

#define PDC_e_str_lit_scan(pdc, findStr, stopStr, eat_lit, str_out, offset_out) \
  PDCI_str_lit_scan(pdc, findStr, stopStr, eat_lit, str_out, offset_out, PDC_charset_EBCDIC, "PDC_e_str_lit_scan")

#define PDC_Cstr_lit_scan(pdc, findStr, stopStr, eat_lit, str_out, offset_out) \
  PDCI_Cstr_lit_scan(pdc, findStr, stopStr, eat_lit, str_out, offset_out, PDCI_DEF_CHARSET(pdc), "PDC_Cstr_lit_scan")

#define PDC_a_Cstr_lit_scan(pdc, findStr, stopStr, eat_lit, str_out, offset_out) \
  PDCI_Cstr_lit_scan(pdc, findStr, stopStr, eat_lit, str_out, offset_out, PDC_charset_ASCII, "PDC_a_Cstr_lit_scan")

#define PDC_e_Cstr_lit_scan(pdc, findStr, stopStr, eat_lit, str_out, offset_out) \
  PDCI_Cstr_lit_scan(pdc, findStr, stopStr, eat_lit, str_out, offset_out, PDC_charset_EBCDIC, "PDC_e_Cstr_lit_scan")

#endif /* PDC_CONFIG_READ_FUNCTIONS */

/* ================================================================================
 * CHAR/STRING READ FUNCTIONS
 */

#if PDC_CONFIG_READ_FUNCTIONS > 0

#define PDC_char_lit_read(pdc, m, pd, c) \
  PDCI_char_lit_read(pdc, m, pd, c, PDCI_DEF_CHARSET(pdc), "PDC_char_lit_read")

#define PDC_a_char_lit_read(pdc, m, pd, c) \
  PDCI_char_lit_read(pdc, m, pd, c, PDC_charset_ASCII, "PDC_a_char_lit_read")

#define PDC_e_char_lit_read(pdc, m, pd, c) \
  PDCI_char_lit_read(pdc, m, pd, c, PDC_charset_EBCDIC, "PDC_e_char_lit_read")

#define PDC_str_lit_read(pdc, m, pd, s) \
  PDCI_str_lit_read(pdc, m, pd, s, PDCI_DEF_CHARSET(pdc), "PDC_str_lit_read")

#define PDC_a_str_lit_read(pdc, m, pd, s) \
  PDCI_str_lit_read(pdc, m, pd, s, PDC_charset_ASCII, "PDC_a_str_lit_read")

#define PDC_e_str_lit_read(pdc, m, pd, s) \
  PDCI_str_lit_read(pdc, m, pd, s, PDC_charset_EBCDIC, "PDC_e_str_lit_read")

#define PDC_Cstr_lit_read(pdc, m, pd, s) \
  PDCI_Cstr_lit_read(pdc, m, pd, s, PDCI_DEF_CHARSET(pdc), "PDC_Cstr_lit_read")

#define PDC_a_Cstr_lit_read(pdc, m, pd, s) \
  PDCI_Cstr_lit_read(pdc, m, pd, s, PDC_charset_ASCII, "PDC_a_Cstr_lit_read")

#define PDC_e_Cstr_lit_read(pdc, m, pd, s) \
  PDCI_Cstr_lit_read(pdc, m, pd, s, PDC_charset_EBCDIC, "PDC_e_Cstr_lit_read")

#define PDC_countX(pdc, m, x, eor_required, pd, res_out) \
  PDCI_countX(pdc, m, x, eor_required, pd, res_out, PDCI_DEF_CHARSET(pdc), "PDC_countX")

#define PDC_a_countX(pdc, m, x, eor_required, pd, res_out) \
  PDCI_countX(pdc, m, x, eor_required, pd, res_out, PDC_charset_ASCII, "PDC_a_countX")

#define PDC_e_countX(pdc, m, x, eor_required, pd, res_out) \
  PDCI_countX(pdc, m, x, eor_required, pd, res_out, PDC_charset_EBCDIC, "PDC_e_countX")

#define PDC_countXtoY(pdc, m, x, y, pd, res_out) \
  PDCI_countXtoY(pdc, m, x, y, pd, res_out, PDCI_DEF_CHARSET(pdc), "PDC_countXtoY")

#define PDC_a_countXtoY(pdc, m, x, y, pd, res_out) \
  PDCI_countXtoY(pdc, m, x, y, pd, res_out, PDC_charset_ASCII, "PDC_a_countXtoY")

#define PDC_e_countXtoY(pdc, m, x, y, pd, res_out) \
  PDCI_countXtoY(pdc, m, x, y, pd, res_out, PDC_charset_EBCDIC, "PDC_e_countXtoY")

#define PDC_date_read(pdc, m, stopChar, pd, res_out) \
  PDCI_date_read(pdc, m, stopChar, pd, res_out, PDCI_DEF_CHARSET(pdc), "PDC_date_read")

#define PDC_a_date_read(pdc, m, stopChar, pd, res_out) \
  PDCI_date_read(pdc, m, stopChar, pd, res_out, PDC_charset_ASCII, "PDC_a_date_read")

#define PDC_e_date_read(pdc, m, stopChar, pd, res_out) \
  PDCI_date_read(pdc, m, stopChar, pd, res_out, PDC_charset_EBCDIC, "PDC_e_date_read")

#define PDC_char_read(pdc, m, pd, c_out) \
  PDCI_char_read(pdc, m, pd, c_out, PDCI_DEF_CHARSET(pdc), "PDC_char_read")

#define PDC_a_char_read(pdc, m, pd, c_out) \
  PDCI_char_read(pdc, m, pd, c_out, PDC_charset_ASCII, "PDC_a_char_read")

#define PDC_e_char_read(pdc, m, pd, c_out) \
  PDCI_char_read(pdc, m, pd, c_out, PDC_charset_EBCDIC, "PDC_e_char_read")

#define PDC_string_FW_read(pdc, m, width, pd, s_out) \
  PDCI_string_FW_read(pdc, m, width, pd, s_out, PDCI_DEF_CHARSET(pdc), "PDC_string_FW_read")

#define PDC_a_string_FW_read(pdc, m, width, pd, s_out) \
  PDCI_string_FW_read(pdc, m, width, pd, s_out, PDC_charset_ASCII, "PDC_a_string_FW_read")

#define PDC_e_string_FW_read(pdc, m, width, pd, s_out) \
  PDCI_string_FW_read(pdc, m, width, pd, s_out, PDC_charset_EBCDIC, "PDC_e_string_FW_read")


#define PDC_string_read(pdc, m, stopChar, pd, s_out) \
  PDCI_string_read(pdc, m, stopChar, pd, s_out, PDCI_DEF_CHARSET(pdc), "PDC_string_read")

#define PDC_a_string_read(pdc, m, stopChar, pd, s_out) \
  PDCI_string_read(pdc, m, stopChar, pd, s_out, PDC_charset_ASCII, "PDC_a_string_read")

#define PDC_e_string_read(pdc, m, stopChar, pd, s_out) \
  PDCI_string_read(pdc, m, stopChar, pd, s_out, PDC_charset_EBCDIC, "PDC_e_string_read")


#define PDC_string_ME_read(pdc, m, matchRegexp, pd, s_out) \
  PDCI_string_ME_read(pdc, m, matchRegexp, pd, s_out, PDCI_DEF_CHARSET(pdc), "PDC_string_ME_read")

#define PDC_a_string_ME_read(pdc, m, matchRegexp, pd, s_out) \
  PDCI_string_ME_read(pdc, m, matchRegexp, pd, s_out, PDC_charset_ASCII, "PDC_a_string_ME_read")

#define PDC_e_string_ME_read(pdc, m, matchRegexp, pd, s_out) \
  PDCI_string_ME_read(pdc, m, matchRegexp, pd, s_out, PDC_charset_EBCDIC, "PDC_e_string_ME_read")


#define PDC_string_CME_read(pdc, m, matchRegexp, pd, s_out) \
  PDCI_string_CME_read(pdc, m, matchRegexp, pd, s_out, PDCI_DEF_CHARSET(pdc), "PDC_string_CME_read")

#define PDC_a_string_CME_read(pdc, m, matchRegexp, pd, s_out) \
  PDCI_string_CME_read(pdc, m, matchRegexp, pd, s_out, PDC_charset_ASCII, "PDC_a_string_CME_read")

#define PDC_e_string_CME_read(pdc, m, matchRegexp, pd, s_out) \
  PDCI_string_CME_read(pdc, m, matchRegexp, pd, s_out, PDC_charset_EBCDIC, "PDC_e_string_CME_read")


#define PDC_string_SE_read(pdc, m, stopRegexp, pd, s_out) \
  PDCI_string_SE_read(pdc, m, stopRegexp, pd, s_out, PDCI_DEF_CHARSET(pdc), "PDC_string_SE_read")

#define PDC_a_string_SE_read(pdc, m, stopRegexp, pd, s_out) \
  PDCI_string_SE_read(pdc, m, stopRegexp, pd, s_out, PDC_charset_ASCII, "PDC_a_string_SE_read")

#define PDC_e_string_SE_read(pdc, m, stopRegexp, pd, s_out) \
  PDCI_string_SE_read(pdc, m, stopRegexp, pd, s_out, PDC_charset_EBCDIC, "PDC_e_string_SE_read")


#define PDC_string_CSE_read(pdc, m, stopRegexp, pd, s_out) \
  PDCI_string_CSE_read(pdc, m, stopRegexp, pd, s_out, PDCI_DEF_CHARSET(pdc), "PDC_string_CSE_read")

#define PDC_a_string_CSE_read(pdc, m, stopRegexp, pd, s_out) \
  PDCI_string_CSE_read(pdc, m, stopRegexp, pd, s_out, PDC_charset_ASCII, "PDC_a_string_CSE_read")

#define PDC_e_string_CSE_read(pdc, m, stopRegexp, pd, s_out) \
  PDCI_string_CSE_read(pdc, m, stopRegexp, pd, s_out, PDC_charset_EBCDIC, "PDC_e_string_CSE_read")

#endif /* PDC_CONFIG_READ_FUNCTIONS */

/* ================================================================================
 * DEFAULT STRING TO INTEGER READ FUNCTIONS
 */

#if PDC_CONFIG_READ_FUNCTIONS > 0

#define PDC_int8_read(pdc, m, pd, res_out) \
  PDCI_CHARSET_SWITCH(pdc, PDC, int8_read, (pdc, m, pd, res_out))

#define PDC_int16_read(pdc, m, pd, res_out) \
  PDCI_CHARSET_SWITCH(pdc, PDC, int16_read, (pdc, m, pd, res_out))

#define PDC_int32_read(pdc, m, pd, res_out) \
  PDCI_CHARSET_SWITCH(pdc, PDC, int32_read, (pdc, m, pd, res_out))

#define PDC_int64_read(pdc, m, pd, res_out) \
  PDCI_CHARSET_SWITCH(pdc, PDC, int64_read, (pdc, m, pd, res_out))

#define PDC_uint8_read(pdc, m, pd, res_out) \
  PDCI_CHARSET_SWITCH(pdc, PDC, uint8_read, (pdc, m, pd, res_out))

#define PDC_uint16_read(pdc, m, pd, res_out) \
  PDCI_CHARSET_SWITCH(pdc, PDC, uint16_read, (pdc, m, pd, res_out))

#define PDC_uint32_read(pdc, m, pd, res_out) \
  PDCI_CHARSET_SWITCH(pdc, PDC, uint32_read, (pdc, m, pd, res_out))

#define PDC_uint64_read(pdc, m, pd, res_out) \
  PDCI_CHARSET_SWITCH(pdc, PDC, uint64_read, (pdc, m, pd, res_out))

#define PDC_int8_FW_read(pdc, m, width, pd, res_out) \
  PDCI_CHARSET_SWITCH(pdc, PDC, int8_FW_read, (pdc, m, width, pd, res_out))

#define PDC_int16_FW_read(pdc, m, width, pd, res_out) \
  PDCI_CHARSET_SWITCH(pdc, PDC, int16_FW_read, (pdc, m, width, pd, res_out))

#define PDC_int32_FW_read(pdc, m, width, pd, res_out) \
  PDCI_CHARSET_SWITCH(pdc, PDC, int32_FW_read, (pdc, m, width, pd, res_out))

#define PDC_int64_FW_read(pdc, m, width, pd, res_out) \
  PDCI_CHARSET_SWITCH(pdc, PDC, int64_FW_read, (pdc, m, width, pd, res_out))

#define PDC_uint8_FW_read(pdc, m, width, pd, res_out) \
  PDCI_CHARSET_SWITCH(pdc, PDC, uint8_FW_read, (pdc, m, width, pd, res_out))

#define PDC_uint16_FW_read(pdc, m, width, pd, res_out) \
  PDCI_CHARSET_SWITCH(pdc, PDC, uint16_FW_read, (pdc, m, width, pd, res_out))

#define PDC_uint32_FW_read(pdc, m, width, pd, res_out) \
  PDCI_CHARSET_SWITCH(pdc, PDC, uint32_FW_read, (pdc, m, width, pd, res_out))

#define PDC_uint64_FW_read(pdc, m, width, pd, res_out) \
  PDCI_CHARSET_SWITCH(pdc, PDC, uint64_FW_read, (pdc, m, width, pd, res_out))

#endif /* PDC_CONFIG_READ_FUNCTIONS */

/* ================================================================================
 * WRITE FUNCTIONS
 */

#if PDC_CONFIG_WRITE_FUNCTIONS > 0

#define PDC_a_char_lit_write2io(pdc, io, c) \
  PDCI_char_lit_write2io(pdc, io, c, PDC_charset_ASCII, "PDC_a_char_lit_write2io")

#define PDC_e_char_lit_write2io(pdc, io, c) \
  PDCI_char_lit_write2io(pdc, io, c, PDC_charset_EBCDIC, "PDC_e_char_lit_write2io")

#define PDC_char_lit_write2io(pdc, io, c) \
  PDCI_char_lit_write2io(pdc, io, c, PDCI_DEF_CHARSET(pdc), "PDC_char_lit_write2io")

#define PDC_a_str_lit_write2io(pdc, io, s) \
  PDCI_str_lit_write2io(pdc, io, s, PDC_charset_ASCII, "PDC_a_str_lit_write2io")

#define PDC_e_str_lit_write2io(pdc, io, s) \
  PDCI_str_lit_write2io(pdc, io, s, PDC_charset_EBCDIC, "PDC_e_str_lit_write2io")

#define PDC_str_lit_write2io(pdc, io, s) \
  PDCI_str_lit_write2io(pdc, io, s, PDCI_DEF_CHARSET(pdc), "PDC_str_lit_write2io")

#define PDC_a_Cstr_lit_write2io(pdc, io, s) \
  PDCI_Cstr_lit_write2io(pdc, io, s, PDC_charset_ASCII, "PDC_a_Cstr_lit_write2io")

#define PDC_e_Cstr_lit_write2io(pdc, io, s) \
  PDCI_Cstr_lit_write2io(pdc, io, s, PDC_charset_EBCDIC, "PDC_e_Cstr_lit_write2io")

#define PDC_Cstr_lit_write2io(pdc, io, s) \
  PDCI_Cstr_lit_write2io(pdc, io, s, PDCI_DEF_CHARSET(pdc), "PDC_Cstr_lit_write2io")

#define PDC_a_char_lit_write2buf(pdc, buf, buf_len, buf_full, c) \
  PDCI_char_lit_write2buf(pdc, buf, buf_len, buf_full, c, PDC_charset_ASCII, "PDC_a_char_lit_write2buf")

#define PDC_e_char_lit_write2buf(pdc, buf, buf_len, buf_full, c) \
  PDCI_char_lit_write2buf(pdc, buf, buf_len, buf_full, c, PDC_charset_EBCDIC, "PDC_e_char_lit_write2buf")

#define PDC_char_lit_write2buf(pdc, buf, buf_len, buf_full, c) \
  PDCI_char_lit_write2buf(pdc, buf, buf_len, buf_full, c, PDCI_DEF_CHARSET(pdc), "PDC_char_lit_write2buf")

#define PDC_a_str_lit_write2buf(pdc, buf, buf_len, buf_full, s) \
  PDCI_str_lit_write2buf(pdc, buf, buf_len, buf_full, s, PDC_charset_ASCII, "PDC_a_str_lit_write2buf")

#define PDC_e_str_lit_write2buf(pdc, buf, buf_len, buf_full, s) \
  PDCI_str_lit_write2buf(pdc, buf, buf_len, buf_full, s, PDC_charset_EBCDIC, "PDC_e_str_lit_write2buf")

#define PDC_str_lit_write2buf(pdc, buf, buf_len, buf_full, s) \
  PDCI_str_lit_write2buf(pdc, buf, buf_len, buf_full, s, PDCI_DEF_CHARSET(pdc), "PDC_str_lit_write2buf")

#define PDC_a_Cstr_lit_write2buf(pdc, buf, buf_len, buf_full, s) \
  PDCI_Cstr_lit_write2buf(pdc, buf, buf_len, buf_full, s, PDC_charset_ASCII, "PDC_a_Cstr_lit_write2buf")

#define PDC_e_Cstr_lit_write2buf(pdc, buf, buf_len, buf_full, s) \
  PDCI_Cstr_lit_write2buf(pdc, buf, buf_len, buf_full, s, PDC_charset_EBCDIC, "PDC_e_Cstr_lit_write2buf")

#define PDC_Cstr_lit_write2buf(pdc, buf, buf_len, buf_full, s) \
  PDCI_Cstr_lit_write2buf(pdc, buf, buf_len, buf_full, s, PDCI_DEF_CHARSET(pdc), "PDC_Cstr_lit_write2buf")

#define PDC_char_write2io(pdc, io, pd, c) \
  PDCI_char_write2io(pdc, io, pd, c, PDCI_DEF_CHARSET(pdc), "PDC_char_write2io")

#define PDC_a_char_write2io(pdc, io, pd, c) \
  PDCI_char_write2io(pdc, io, pd, c, pdc_charset_ASCII, "PDC_a_char_write2io")

#define PDC_e_char_write2io(pdc, io, pd, c) \
  PDCI_char_write2io(pdc, io, pd, c, pdc_charset_EBCDIC, "PDC_e_char_write2io")

#define PDC_char_write2buf(pdc, buf, buf_len, buf_full, pd, c) \
  PDCI_char_write2buf(pdc, buf, buf_len, buf_full, pd, c, PDCI_DEF_CHARSET(pdc), "PDC_char_write2buf")

#define PDC_a_char_write2buf(pdc, buf, buf_len, buf_full, pd, c) \
  PDCI_char_write2buf(pdc, buf, buf_len, buf_full, pd, c, PDC_charset_ASCII, "PDC_a_char_write2buf")

#define PDC_e_char_write2buf(pdc, buf, buf_len, buf_full, pd, c) \
  PDCI_char_write2buf(pdc, buf, buf_len, buf_full, pd, c, PDC_charset_EBCDIC, "PDC_e_char_write2buf")

#define PDC_string_FW_write2io(pdc, io, width, pd, s) \
  PDCI_string_FW_write2io(pdc, io, width, pd, s, PDCI_DEF_CHARSET(pdc), "PDC_string_FW_write2io")

#define PDC_a_string_FW_write2io(pdc, io, width, pd, s) \
  PDCI_string_FW_write2io(pdc, io, width, pd, s, PDC_charset_ASCII, "PDC_a_string_FW_write2io")

#define PDC_e_string_FW_write2io(pdc, io, width, pd, s) \
  PDCI_string_FW_write2io(pdc, io, width, pd, s, PDC_charset_EBCDIC, "PDC_e_string_FW_write2io")

#define PDC_string_FW_write2buf(pdc, buf, buf_len, buf_full, width, pd, s) \
  PDCI_string_FW_write2buf(pdc, buf, buf_len, buf_full, width, pd, s, PDCI_DEF_CHARSET(pdc), "PDC_string_FW_write2buf")

#define PDC_a_string_FW_write2buf(pdc, buf, buf_len, buf_full, width, pd, s) \
  PDCI_string_FW_write2buf(pdc, buf, buf_len, buf_full, width, pd, s, PDC_charset_ASCII, "PDC_a_string_FW_write2buf")

#define PDC_e_string_FW_write2buf(pdc, buf, buf_len, buf_full, width, pd, s) \
  PDCI_string_FW_write2buf(pdc, buf, buf_len, buf_full, width, pd, s, PDC_charset_EBCDIC, "PDC_e_string_FW_write2buf")

#define PDC_string_write2io(pdc, io, stopChar, pd, s) \
  PDCI_string_write2io(pdc, io, ((void*)(stopChar)), pd, s, PDCI_DEF_CHARSET(pdc), "PDC_string", "PDC_string_write2io")

#define PDC_a_string_write2io(pdc, io, stopChar, pd, s) \
  PDCI_string_write2io(pdc, io, ((void*)(stopChar)), pd, s, PDC_charset_ASCII, "PDC_string", "PDC_a_string_write2io")

#define PDC_e_string_write2io(pdc, io, stopChar, pd, s) \
  PDCI_string_write2io(pdc, io, ((void*)(stopChar)), pd, s, PDC_charset_EBCDIC, "PDC_string", "PDC_e_string_write2io")

#define PDC_string_write2buf(pdc, buf, buf_len, buf_full, stopChar, pd, s) \
  PDCI_string_write2buf(pdc, buf, buf_len, buf_full, ((void*)(stopChar)), pd, s, PDCI_DEF_CHARSET(pdc), "PDC_string", "PDC_string_write2buf")

#define PDC_a_string_write2buf(pdc, buf, buf_len, buf_full, stopChar, pd, s) \
  PDCI_string_write2buf(pdc, buf, buf_len, buf_full, ((void*)(stopChar)), pd, s, PDC_charset_ASCII, "PDC_string", "PDC_a_string_write2buf")

#define PDC_e_string_write2buf(pdc, buf, buf_len, buf_full, stopChar, pd, s) \
  PDCI_string_write2buf(pdc, buf, buf_len, buf_full, ((void*)(stopChar)), pd, s, PDC_charset_EBCDIC, "PDC_string", "PDC_e_string_write2buf")

#define PDC_string_ME_write2io(pdc, io, matchRegexp, pd, s) \
  PDCI_string_write2io(pdc, io, ((void*)(matchRegexp)), pd, s, PDCI_DEF_CHARSET(pdc), "PDC_string_ME", "PDC_string_ME_write2io")

#define PDC_a_string_ME_write2io(pdc, io, matchRegexp, pd, s) \
  PDCI_string_write2io(pdc, io, ((void*)(matchRegexp)), pd, s, PDCI_charset_ASCII, "PDC_string_ME", "PDC_a_string_ME_write2io")

#define PDC_e_string_ME_write2io(pdc, io, matchRegexp, pd, s) \
  PDCI_string_write2io(pdc, io, ((void*)(matchRegexp)), pd, s, PDCI_charset_EBCDIC, "PDC_string_ME", "PDC_e_string_ME_write2io")

#define PDC_string_ME_write2buf(pdc, buf, buf_len, buf_full, matchRegexp, pd, s) \
  PDCI_string_write2buf(pdc, buf, buf_len, buf_full, ((void*)(matchRegexp)), pd, s, PDCI_DEF_CHARSET(pdc), "PDC_string_ME", "PDC_ME_string_write2buf")

#define PDC_a_string_ME_write2buf(pdc, buf, buf_len, buf_full, matchRegexp, pd, s) \
  PDCI_string_write2buf(pdc, buf, buf_len, buf_full, ((void*)(matchRegexp)), pd, s, PDC_charset_ASCII, "PDC_string_ME", "PDC_a_string_ME_write2buf")

#define PDC_e_string_ME_write2buf(pdc, buf, buf_len, buf_full, matchRegexp, pd, s) \
  PDCI_string_write2buf(pdc, buf, buf_len, buf_full, ((void*)(matchRegexp)), pd, s, PDC_charset_EBCDIC, "PDC_string_ME", "PDC_e_string_ME_write2buf")

#define PDC_string_CME_write2io(pdc, io, matchRegexp, pd, s) \
  PDCI_string_write2io(pdc, io, ((void*)(matchRegexp)), pd, s, PDCI_DEF_CHARSET(pdc), "PDC_string_CME", "PDC_string_CME_write2io")

#define PDC_a_string_CME_write2io(pdc, io, matchRegexp, pd, s) \
  PDCI_string_write2io(pdc, io, ((void*)(matchRegexp)), pd, s, PDCI_charset_ASCII, "PDC_string_CME", "PDC_a_string_CME_write2io")

#define PDC_e_string_CME_write2io(pdc, io, matchRegexp, pd, s) \
  PDCI_string_write2io(pdc, io, ((void*)(matchRegexp)), pd, s, PDCI_charset_EBCDIC, "PDC_string_CME", "PDC_e_string_CME_write2io")

#define PDC_string_CME_write2buf(pdc, buf, buf_len, buf_full, matchRegexp, pd, s) \
  PDCI_string_write2buf(pdc, buf, buf_len, buf_full, ((void*)(matchRegexp)), pd, s, PDCI_DEF_CHARSET(pdc), "PDC_string_CME", "PDC_CME_string_write2buf")

#define PDC_a_string_CME_write2buf(pdc, buf, buf_len, buf_full, matchRegexp, pd, s) \
  PDCI_string_write2buf(pdc, buf, buf_len, buf_full, ((void*)(matchRegexp)), pd, s, PDC_charset_ASCII, "PDC_string_CME", "PDC_a_string_CME_write2buf")

#define PDC_e_string_CME_write2buf(pdc, buf, buf_len, buf_full, matchRegexp, pd, s) \
  PDCI_string_write2buf(pdc, buf, buf_len, buf_full, ((void*)(matchRegexp)), pd, s, PDC_charset_EBCDIC, "PDC_string_CME", "PDC_e_string_CME_write2buf")

#define PDC_string_SE_write2io(pdc, io, stopRegexp, pd, s) \
  PDCI_string_write2io(pdc, io, ((void*)(stopRegexp)), pd, s, PDCI_DEF_CHARSET(pdc), "PDC_string_SE", "PDC_string_SE_write2io")

#define PDC_a_string_SE_write2io(pdc, io, stopRegexp, pd, s) \
  PDCI_string_write2io(pdc, io, ((void*)(stopRegexp)), pd, s, PDCI_charset_ASCII, "PDC_string_SE", "PDC_a_string_SE_write2io")

#define PDC_e_string_SE_write2io(pdc, io, stopRegexp, pd, s) \
  PDCI_string_write2io(pdc, io, ((void*)(stopRegexp)), pd, s, PDCI_charset_EBCDIC, "PDC_string_SE", "PDC_e_string_SE_write2io")

#define PDC_string_SE_write2buf(pdc, buf, buf_len, buf_full, stopRegexp, pd, s) \
  PDCI_string_write2buf(pdc, buf, buf_len, buf_full, ((void*)(stopRegexp)), pd, s, PDCI_DEF_CHARSET(pdc), "PDC_string_SE", "PDC_SE_string_write2buf")

#define PDC_a_string_SE_write2buf(pdc, buf, buf_len, buf_full, stopRegexp, pd, s) \
  PDCI_string_write2buf(pdc, buf, buf_len, buf_full, ((void*)(stopRegexp)), pd, s, PDC_charset_ASCII, "PDC_string_SE", "PDC_a_string_SE_write2buf")

#define PDC_e_string_SE_write2buf(pdc, buf, buf_len, buf_full, stopRegexp, pd, s) \
  PDCI_string_write2buf(pdc, buf, buf_len, buf_full, ((void*)(stopRegexp)), pd, s, PDC_charset_EBCDIC, "PDC_string_SE", "PDC_e_string_SE_write2buf")

#define PDC_string_CSE_write2io(pdc, io, stopRegexp, pd, s) \
  PDCI_string_write2io(pdc, io, ((void*)(stopRegexp)), pd, s, PDCI_DEF_CHARSET(pdc), "PDC_string_CSE", "PDC_string_CSE_write2io")

#define PDC_a_string_CSE_write2io(pdc, io, stopRegexp, pd, s) \
  PDCI_string_write2io(pdc, io, ((void*)(stopRegexp)), pd, s, PDCI_charset_ASCII, "PDC_string_CSE", "PDC_a_string_CSE_write2io")

#define PDC_e_string_CSE_write2io(pdc, io, stopRegexp, pd, s) \
  PDCI_string_write2io(pdc, io, ((void*)(stopRegexp)), pd, s, PDCI_charset_EBCDIC, "PDC_string_CSE", "PDC_e_string_CSE_write2io")

#define PDC_string_CSE_write2buf(pdc, buf, buf_len, buf_full, stopRegexp, pd, s) \
  PDCI_string_write2buf(pdc, buf, buf_len, buf_full, ((void*)(stopRegexp)), pd, s, PDCI_DEF_CHARSET(pdc), "PDC_string_CSE", "PDC_CSE_string_write2buf")

#define PDC_a_string_CSE_write2buf(pdc, buf, buf_len, buf_full, stopRegexp, pd, s) \
  PDCI_string_write2buf(pdc, buf, buf_len, buf_full, ((void*)(stopRegexp)), pd, s, PDC_charset_ASCII, "PDC_string_CSE", "PDC_a_string_CSE_write2buf")

#define PDC_e_string_CSE_write2buf(pdc, buf, buf_len, buf_full, stopRegexp, pd, s) \
  PDCI_string_write2buf(pdc, buf, buf_len, buf_full, ((void*)(stopRegexp)), pd, s, PDC_charset_EBCDIC, "PDC_string_CSE", "PDC_e_string_CSE_write2buf")

#define PDC_date_write2io(pdc, io, stopChar, pd, d) \
  PDCI_date_write2io(pdc, io, ((void*)(stopChar)), pd, d, PDCI_DEF_CHARSET(pdc), "PDC_date", "PDC_date_write2io")

#define PDC_a_date_write2io(pdc, io, stopChar, pd, d) \
  PDCI_date_write2io(pdc, io, ((void*)(stopChar)), pd, d, PDC_charset_ASCII, "PDC_date", "PDC_a_date_write2io")

#define PDC_e_date_write2io(pdc, io, stopChar, pd, d) \
  PDCI_date_write2io(pdc, io, ((void*)(stopChar)), pd, d, PDC_charset_EBCDIC, "PDC_date", "PDC_e_date_write2io")

#define PDC_date_write2buf(pdc, buf, buf_len, buf_full, stopChar, pd, d) \
  PDCI_date_write2buf(pdc, buf, buf_len, buf_full, ((void*)(stopChar)), pd, d, PDCI_DEF_CHARSET(pdc), "PDC_date", "PDC_date_write2buf")

#define PDC_a_date_write2buf(pdc, buf, buf_len, buf_full, stopChar, pd, d) \
  PDCI_date_write2buf(pdc, buf, buf_len, buf_full, ((void*)(stopChar)), pd, d, PDC_charset_ASCII, "PDC_date", "PDC_a_date_write2buf")

#define PDC_e_date_write2buf(pdc, buf, buf_len, buf_full, stopChar, pd, d) \
  PDCI_date_write2buf(pdc, buf, buf_len, buf_full, ((void*)(stopChar)), pd, d, PDC_charset_EBCDIC, "PDC_date", "PDC_e_date_write2buf")

#define PDC_int8_FW_write2io(pdc, io, width, pd, val) \
  PDCI_CHARSET_SWITCH(pdc, PDC, int8_FW_write2io, (pdc, io, width, pd, val))

#define PDC_int16_FW_write2io(pdc, io, width, pd, val) \
  PDCI_CHARSET_SWITCH(pdc, PDC, int16_FW_write2io, (pdc, io, width, pd, val))

#define PDC_int32_FW_write2io(pdc, io, width, pd, val) \
  PDCI_CHARSET_SWITCH(pdc, PDC, int32_FW_write2io, (pdc, io, width, pd, val))

#define PDC_int64_FW_write2io(pdc, io, width, pd, val) \
  PDCI_CHARSET_SWITCH(pdc, PDC, int64_FW_write2io, (pdc, io, width, pd, val))

#define PDC_uint8_FW_write2io(pdc, io, width, pd, val) \
  PDCI_CHARSET_SWITCH(pdc, PDC, uint8_FW_write2io, (pdc, io, width, pd, val))

#define PDC_uint16_FW_write2io(pdc, io, width, pd, val) \
  PDCI_CHARSET_SWITCH(pdc, PDC, uint16_FW_write2io, (pdc, io, width, pd, val))

#define PDC_uint32_FW_write2io(pdc, io, width, pd, val) \
  PDCI_CHARSET_SWITCH(pdc, PDC, uint32_FW_write2io, (pdc, io, width, pd, val))

#define PDC_uint64_FW_write2io(pdc, io, width, pd, val) \
  PDCI_CHARSET_SWITCH(pdc, PDC, uint64_FW_write2io, (pdc, io, width, pd, val))

#define PDC_int8_write2io(pdc, io, pd, val) \
  PDCI_CHARSET_SWITCH(pdc, PDC, int8_write2io, (pdc, io, pd, val))

#define PDC_int16_write2io(pdc, io, pd, val) \
  PDCI_CHARSET_SWITCH(pdc, PDC, int16_write2io, (pdc, io, pd, val))

#define PDC_int32_write2io(pdc, io, pd, val) \
  PDCI_CHARSET_SWITCH(pdc, PDC, int32_write2io, (pdc, io, pd, val))

#define PDC_int64_write2io(pdc, io, pd, val) \
  PDCI_CHARSET_SWITCH(pdc, PDC, int64_write2io, (pdc, io, pd, val))

#define PDC_uint8_write2io(pdc, io, pd, val) \
  PDCI_CHARSET_SWITCH(pdc, PDC, uint8_write2io, (pdc, io, pd, val))

#define PDC_uint16_write2io(pdc, io, pd, val) \
  PDCI_CHARSET_SWITCH(pdc, PDC, uint16_write2io, (pdc, io, pd, val))

#define PDC_uint32_write2io(pdc, io, pd, val) \
  PDCI_CHARSET_SWITCH(pdc, PDC, uint32_write2io, (pdc, io, pd, val))

#define PDC_uint64_write2io(pdc, io, pd, val) \
  PDCI_CHARSET_SWITCH(pdc, PDC, uint64_write2io, (pdc, io, pd, val))

#define PDC_int8_FW_write2buf(pdc, buf, buf_len, buf_full, width, pd, val) \
  PDCI_CHARSET_SWITCH(pdc, PDC, int8_FW_write2buf, (pdc, buf, buf_len, buf_full, width, pd, val))

#define PDC_int16_FW_write2buf(pdc, buf, buf_len, buf_full, width, pd, val) \
  PDCI_CHARSET_SWITCH(pdc, PDC, int16_FW_write2buf, (pdc, buf, buf_len, buf_full, width, pd, val))

#define PDC_int32_FW_write2buf(pdc, buf, buf_len, buf_full, width, pd, val) \
  PDCI_CHARSET_SWITCH(pdc, PDC, int32_FW_write2buf, (pdc, buf, buf_len, buf_full, width, pd, val))

#define PDC_int64_FW_write2buf(pdc, buf, buf_len, buf_full, width, pd, val) \
  PDCI_CHARSET_SWITCH(pdc, PDC, int64_FW_write2buf, (pdc, buf, buf_len, buf_full, width, pd, val))

#define PDC_uint8_FW_write2buf(pdc, buf, buf_len, buf_full, width, pd, val) \
  PDCI_CHARSET_SWITCH(pdc, PDC, uint8_FW_write2buf, (pdc, buf, buf_len, buf_full, width, pd, val))

#define PDC_uint16_FW_write2buf(pdc, buf, buf_len, buf_full, width, pd, val) \
  PDCI_CHARSET_SWITCH(pdc, PDC, uint16_FW_write2buf, (pdc, buf, buf_len, buf_full, width, pd, val))

#define PDC_uint32_FW_write2buf(pdc, buf, buf_len, buf_full, width, pd, val) \
  PDCI_CHARSET_SWITCH(pdc, PDC, uint32_FW_write2buf, (pdc, buf, buf_len, buf_full, width, pd, val))

#define PDC_uint64_FW_write2buf(pdc, buf, buf_len, buf_full, width, pd, val) \
  PDCI_CHARSET_SWITCH(pdc, PDC, uint64_FW_write2buf, (pdc, buf, buf_len, buf_full, width, pd, val))

#define PDC_int8_write2buf(pdc, buf, buf_len, buf_full, pd, val) \
  PDCI_CHARSET_SWITCH(pdc, PDC, int8_write2buf, (pdc, buf, buf_len, buf_full, pd, val))

#define PDC_int16_write2buf(pdc, buf, buf_len, buf_full, pd, val) \
  PDCI_CHARSET_SWITCH(pdc, PDC, int16_write2buf, (pdc, buf, buf_len, buf_full, pd, val))

#define PDC_int32_write2buf(pdc, buf, buf_len, buf_full, pd, val) \
  PDCI_CHARSET_SWITCH(pdc, PDC, int32_write2buf, (pdc, buf, buf_len, buf_full, pd, val))

#define PDC_int64_write2buf(pdc, buf, buf_len, buf_full, pd, val) \
  PDCI_CHARSET_SWITCH(pdc, PDC, int64_write2buf, (pdc, buf, buf_len, buf_full, pd, val))

#define PDC_uint8_write2buf(pdc, buf, buf_len, buf_full, pd, val) \
  PDCI_CHARSET_SWITCH(pdc, PDC, uint8_write2buf, (pdc, buf, buf_len, buf_full, pd, val))

#define PDC_uint16_write2buf(pdc, buf, buf_len, buf_full, pd, val) \
  PDCI_CHARSET_SWITCH(pdc, PDC, uint16_write2buf, (pdc, buf, buf_len, buf_full, pd, val))

#define PDC_uint32_write2buf(pdc, buf, buf_len, buf_full, pd, val) \
  PDCI_CHARSET_SWITCH(pdc, PDC, uint32_write2buf, (pdc, buf, buf_len, buf_full, pd, val))

#define PDC_uint64_write2buf(pdc, buf, buf_len, buf_full, pd, val) \
  PDCI_CHARSET_SWITCH(pdc, PDC, uint64_write2buf, (pdc, buf, buf_len, buf_full, pd, val))

#endif /* PDC_CONFIG_WRITE_FUNCTIONS */

#endif   /*   ! FOR_CKIT             */
#endif   /*   ! __PADSC_IMPL_H__  */
