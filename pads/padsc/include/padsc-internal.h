#ifdef _USE_PROTO
#pragma prototyped
#endif
/*
 * padc library interface
 * 
 * Kathleen Fisher, Robert Gruber
 * AT&T Labs Research
 */

#ifndef __LIBPADSC_INTERNAL__
#define __LIBPADSC_INTERNAL__

/* ================================================================================ */
/* XXX THESE ARE REQUIRED FOR CKIT ON CERTAIN ARCHITECTURES ??? */

#ifdef __PREPROCESSOR_FIXES
typedef void * __builtin_va_list;
#define __THROW
/* extern int ftruncate (int __fd, long int __length) ; */

#endif

/* ================================================================================ */

#include "libpadsc.h"
#include "pdc_out_macros.h"

/* ================================================================================ */
/* NOT SURE WHERE THESE MACROS BELONG */

/* vmalloc macros:
 *   vmnewof copies existing bytes and zeroes extension
 *   vmoldof does not copy existing bytes and does not zero extension
 *   vmcpyoldof copies existing bytes but does not zero extension
 *
 * The first 2 are provided, the last one is here:
 */

#define vmcpyoldof(v,p,t,n,x) \
  (t*)vmresize((v), (p), sizeof(t)*(n)+(x), (VM_RSMOVE|VM_RSCOPY) )

/* ================================================================================ */
/* INTERNAL TYPE DEFINITIONS */

/* PDCI_stkElt_t: A stack elt has a cursor position cur, which is a
 * pointer to a PDC_IO_elt plus the number of byte remaining.  We also
 * record the spec flag passed to PDC_IO_checkpoint, to enable proper
 * de-bumping of pdc->speclev.
 */

/* type PDCI_stkElt_t: */
struct PDCI_stkElt_s {
  PDC_IO_elt_t  *elt;
  size_t        remain;  /* bytes remaining in current IO elt; determines cursor position */
  int           spec;    /* the spec flag passed to checkpoint */
};

/* ================================================================================ */
/* INTERNAL VERSIONS OF EXTERNAL IO FUNCTIONS */

PDC_error_t  PDC_IO_set_internal      (PDC_t *pdc, Sfio_t *io);
PDC_error_t  PDC_IO_fopen_internal    (PDC_t *pdc, char *path);
PDC_error_t  PDC_IO_close_internal    (PDC_t *pdc);
PDC_error_t  PDC_IO_next_rec_internal (PDC_t *pdc, size_t *skipped_bytes_out);

int          PDC_IO_at_EOR_internal   (PDC_t *pdc);
int          PDC_IO_at_EOF_internal   (PDC_t *pdc);

PDC_error_t  PDC_IO_getPos_internal   (PDC_t *pdc, PDC_pos_t *pos, int offset); 

/* ================================================================================ */ 
/* INTERNAL VERSIONS OF SCAN FUNCTIONS */

#define PDC_char_lit_scan_internal(pdc, c, s, eat_lit, c_out, offset_out) \
          PDCI_char_lit_scan(pdc, c, s, eat_lit, c_out, offset_out, pdc->disc->def_charclass, "[in PDC_char_lit_scan]")

#define PDC_a_char_lit_scan_internal(pdc, c, s, eat_lit, c_out, offset_out) \
          PDCI_char_lit_scan(pdc, c, s, eat_lit, c_out, offset_out, PDC_charclass_ASCII, "[in PDC_a_char_lit_scan]")

#define PDC_e_char_lit_scan_internal(pdc, c, s, eat_lit, c_out, offset_out) \
          PDCI_char_lit_scan(pdc, c, s, eat_lit, c_out, offset_out, PDC_charclass_EBCDIC, "[in PDC_e_char_lit_scan]")

#define PDC_str_lit_scan_internal(pdc, findStr, stopStr, eat_lit, str_out, offset_out) \
          PDCI_str_lit_scan(pdc, findStr, stopStr, eat_lit, str_out, offset_out, pdc->disc->def_charclass, "[in PDC_str_lit_scan]")

#define PDC_a_str_lit_scan_internal(pdc, findStr, stopStr, eat_lit, str_out, offset_out) \
          PDCI_str_lit_scan(pdc, findStr, stopStr, eat_lit, str_out, offset_out, PDC_charclass_ASCII, "[in PDC_a_str_lit_scan]")

#define PDC_e_str_lit_scan_internal(pdc, findStr, stopStr, eat_lit, str_out, offset_out) \
          PDCI_str_lit_scan(pdc, findStr, stopStr, eat_lit, str_out, offset_out, PDC_charclass_EBCDIC, "[in PDC_e_str_lit_scan]")

#define PDC_Cstr_lit_scan_internal(pdc, findStr, stopStr, eat_lit, str_out, offset_out) \
          PDCI_Cstr_lit_scan(pdc, findStr, stopStr, eat_lit, str_out, offset_out, pdc->disc->def_charclass, "[in PDC_Cstr_lit_scan]")


#define PDC_a_Cstr_lit_scan_internal(pdc, findStr, stopStr, eat_lit, str_out, offset_out) \
          (pdc, findStr, stopStr, eat_lit, str_out, offset_out, PDC_charclass_ASCII, "[in PDC_a_Cstr_lit_scan]")

#define PDC_e_Cstr_lit_scan_internal(pdc, findStr, stopStr, eat_lit, str_out, offset_out) \
          PDCI_Cstr_lit_scan(pdc, findStr, stopStr, eat_lit, str_out, offset_out, PDC_charclass_EBCDIC, "[in PDC_e_Cstr_lit_scan]")

/* ================================================================================ */ 
/* INTERNAL VERSIONS OF ALL BASE TYPE READ FUNCTIONS */

/* read functions that involve character class */

#define PDC_char_lit_read_internal(pdc, csm, ed, c) \
          PDCI_char_lit_read(pdc, csm, ed, c, pdc->disc->def_charclass, "[in PDC_char_lit_read]")

#define PDC_a_char_lit_read_internal(pdc, csm, ed, c) \
          PDCI_char_lit_read(pdc, csm, ed, c, PDC_charclass_ASCII, "[in PDC_a_char_lit_read]")

#define PDC_e_char_lit_read_internal(pdc, csm, ed, c) \
          PDCI_char_lit_read(pdc, csm, ed, c, PDC_charclass_EBCDIC, "[in PDC_e_char_lit_read]")


#define PDC_str_lit_read_internal(pdc, csm, ed, s) \
          PDCI_str_lit_read(pdc, csm, ed, s, pdc->disc->def_charclass, "[in PDC_str_lit_read]")

#define PDC_a_str_lit_read_internal(pdc, csm, ed, s) \
          PDCI_str_lit_read(pdc, csm, ed, s, PDC_charclass_ASCII, "[in PDC_a_str_lit_read]")

#define PDC_e_str_lit_read_internal(pdc, csm, ed, s) \
          PDCI_str_lit_read(pdc, csm, ed, s, PDC_charclass_EBCDIC, "[in PDC_e_str_lit_read]")


#define PDC_Cstr_lit_read_internal(pdc, csm, ed, s) \
          PDCI_Cstr_lit_read(pdc, csm, ed, s, pdc->disc->def_charclass, "[in PDC_Cstr_lit_read]")

#define PDC_a_Cstr_lit_read_internal(pdc, csm, ed, s) \
          PDCI_Cstr_lit_read(pdc, csm, ed, s, PDC_charclass_ASCII, "[in PDC_a_Cstr_lit_read]")

#define PDC_e_Cstr_lit_read_internal(pdc, csm, ed, s) \
          PDCI_Cstr_lit_read(pdc, csm, ed, s, PDC_charclass_EBCDIC, "[in PDC_e_Cstr_lit_read]")


#define PDC_countX_internal(pdc, csm, x, eor_required, ed, res_out) \
          PDCI_countX(pdc, csm, x, eor_required, ed, res_out, pdc->disc->def_charclass, "[in PDC_countX]")

#define PDC_a_countX_internal(pdc, csm, x, eor_required, ed, res_out) \
          PDCI_countX(pdc, csm, x, eor_required, ed, res_out, PDC_charclass_ASCII, "[in PDC_a_countX]")

#define PDC_e_countX_internal(pdc, csm, x, eor_required, ed, res_out) \
          PDCI_countX(pdc, csm, x, eor_required, ed, res_out, PDC_charclass_EBCDIC, "[in PDC_e_countX]")


#define PDC_countXtoY_internal(pdc, csm, x, y, ed, res_out) \
          PDCI_countXtoY(pdc, csm, x, y, ed, res_out, pdc->disc->def_charclass, "[in PDC_countXtoY]")

#define PDC_a_countXtoY_internal(pdc, csm, x, y, ed, res_out) \
          PDCI_countXtoY(pdc, csm, x, y, ed, res_out, PDC_charclass_ASCII, "[in PDC_a_countXtoY]")

#define PDC_e_countXtoY_internal(pdc, csm, x, y, ed, res_out) \
          PDCI_countXtoY(pdc, csm, x, y, ed, res_out, PDC_charclass_EBCDIC, "[in PDC_e_countXtoY]")


#define PDC_date_read_internal(pdc, csm, stopChar, ed, res_out) \
          PDCI_date_read(pdc, csm, stopChar, ed, res_out, pdc->disc->def_charclass, "[in PDC_date_read]")

#define PDC_a_date_read_internal(pdc, csm, stopChar, ed, res_out) \
          PDCI_date_read(pdc, csm, stopChar, ed, res_out, PDC_charclass_ASCII, "[in PDC_a_date_read]")

#define PDC_e_date_read_internal(pdc, csm, stopChar, ed, res_out) \
          PDCI_date_read(pdc, csm, stopChar, ed, res_out, PDC_charclass_EBCDIC, "[in PDC_e_date_read]")


#define PDC_char_read_internal(pdc, csm, ed, c_out) \
          PDCI_char_read(pdc, csm, ed, c_out, pdc->disc->def_charclass, "[in PDC_char_read]")

#define PDC_a_char_read_internal(pdc, csm, ed, c_out) \
          PDCI_char_read(pdc, csm, ed, c_out, PDC_charclass_ASCII, "[in PDC_a_char_read]")

#define PDC_e_char_read_internal(pdc, csm, ed, c_out) \
          PDCI_char_read(pdc, csm, ed, c_out, PDC_charclass_EBCDIC, "[in PDC_e_char_read]")


#define PDC_string_FW_read_internal(pdc, csm, width, ed, s_out) \
          PDCI_string_FW_read(pdc, csm, width, ed, s_out, pdc->disc->def_charclass, "[in PDC_string_FW_read]")

#define PDC_a_string_FW_read_internal(pdc, csm, width, ed, s_out) \
          PDCI_string_FW_read(pdc, csm, width, ed, s_out, PDC_charclass_ASCII, "[in PDC_a_string_FW_read]")

#define PDC_e_string_FW_read_internal(pdc, csm, width, ed, s_out) \
          PDCI_string_FW_read(pdc, csm, width, ed, s_out, PDC_charclass_EBCDIC, "[in PDC_e_string_FW_read]")


#define PDC_string_read_internal(pdc, csm, stopChar, ed, s_out) \
          PDCI_string_read(pdc, csm, stopChar, ed, s_out, pdc->disc->def_charclass, "[in PDC_string_read]")

#define PDC_a_string_read_internal(pdc, csm, stopChar, ed, s_out) \
          PDCI_string_read(pdc, csm, stopChar, ed, s_out, PDC_charclass_ASCII, "[in PDC_a_string_read]")

#define PDC_e_string_read_internal(pdc, csm, stopChar, ed, s_out) \
          PDCI_string_read(pdc, csm, stopChar, ed, s_out, PDC_charclass_EBCDIC, "[in PDC_e_string_read]")


#define PDC_string_ME_read_internal(pdc, csm, matchRegexp, ed, s_out) \
          PDCI_string_ME_read(pdc, csm, matchRegexp, ed, s_out, pdc->disc->def_charclass, "[in PDC_string_ME_read]")

#define PDC_a_string_ME_read_internal(pdc, csm, matchRegexp, ed, s_out) \
          PDCI_string_ME_read(pdc, csm, matchRegexp, ed, s_out, PDC_charclass_ASCII, "[in PDC_a_string_ME_read]")

#define PDC_e_string_ME_read_internal(pdc, csm, matchRegexp, ed, s_out) \
          PDCI_string_ME_read(pdc, csm, matchRegexp, ed, s_out, PDC_charclass_EBCDIC, "[in PDC_e_string_ME_read]")


#define PDC_string_CME_read_internal(pdc, csm, matchRegexp, ed, s_out) \
          PDCI_string_CME_read(pdc, csm, matchRegexp, ed, s_out, pdc->disc->def_charclass, "[in PDC_string_CME_read]")

#define PDC_a_string_CME_read_internal(pdc, csm, matchRegexp, ed, s_out) \
          PDCI_string_CME_read(pdc, csm, matchRegexp, ed, s_out, PDC_charclass_ASCII, "[in PDC_a_string_CME_read]")

#define PDC_e_string_CME_read_internal(pdc, csm, matchRegexp, ed, s_out) \
          PDCI_string_CME_read(pdc, csm, matchRegexp, ed, s_out, PDC_charclass_EBCDIC, "[in PDC_e_string_CME_read]")


#define PDC_string_SE_read_internal(pdc, csm, stopRegexp, ed, s_out) \
          PDCI_string_SE_read(pdc, csm, stopRegexp, ed, s_out, pdc->disc->def_charclass, "[in PDC_string_SE_read]")

#define PDC_a_string_SE_read_internal(pdc, csm, stopRegexp, ed, s_out) \
          PDCI_string_SE_read(pdc, csm, stopRegexp, ed, s_out, PDC_charclass_ASCII, "[in PDC_a_string_SE_read]")

#define PDC_e_string_SE_read_internal(pdc, csm, stopRegexp, ed, s_out) \
          PDCI_string_SE_read(pdc, csm, stopRegexp, ed, s_out, PDC_charclass_EBCDIC, "[in PDC_e_string_SE_read]")


#define PDC_string_CSE_read_internal(pdc, csm, stopRegexp, ed, s_out) \
          PDCI_string_CSE_read(pdc, csm, stopRegexp, ed, s_out, pdc->disc->def_charclass, "[in PDC_string_CSE_read]")

#define PDC_a_string_CSE_read_internal(pdc, csm, stopRegexp, ed, s_out) \
          PDCI_string_CSE_read(pdc, csm, stopRegexp, ed, s_out, PDC_charclass_ASCII, "[in PDC_a_string_CSE_read]")

#define PDC_e_string_CSE_read_internal(pdc, csm, stopRegexp, ed, s_out) \
          PDCI_string_CSE_read(pdc, csm, stopRegexp, ed, s_out, PDC_charclass_EBCDIC, "[in PDC_e_string_CSE_read]")



/* ================================================================================ */ 
/* HELPER MACRO TO DECLARE FAMILY OF FUNCTIONS */
/* N.B. First you must declare PDCI_FIRST_ARGS and PDCI_LAST_ARGS.  Follow macro with a semi */

#define PDCI_DECL_FAMILY(ret_type, fn_prefix, typ, fn_suffix, lastnm) \
ret_type fn_prefix ## typ ## 8 ## fn_suffix (PDCI_FIRST_ARGS, PDC_ ## typ ## 8 *lastnm PDCI_LAST_ARGS); \
ret_type fn_prefix ## typ ## 16 ## fn_suffix(PDCI_FIRST_ARGS, PDC_ ## typ ## 16 *lastnm PDCI_LAST_ARGS); \
ret_type fn_prefix ## typ ## 32 ## fn_suffix(PDCI_FIRST_ARGS, PDC_ ## typ ## 32 *lastnm PDCI_LAST_ARGS); \
ret_type fn_prefix ## typ ## 64 ## fn_suffix(PDCI_FIRST_ARGS, PDC_ ## typ  ## 64 *lastnm PDCI_LAST_ARGS); \
ret_type fn_prefix ## u ## typ ## 8 ## fn_suffix (PDCI_FIRST_ARGS, PDC_u ## typ ## 8 *lastnm PDCI_LAST_ARGS); \
ret_type fn_prefix ## u ## typ ## 16 ## fn_suffix(PDCI_FIRST_ARGS, PDC_u ## typ ## 16 *lastnm PDCI_LAST_ARGS); \
ret_type fn_prefix ## u ## typ ## 32 ## fn_suffix(PDCI_FIRST_ARGS, PDC_u ## typ ## 32 *lastnm PDCI_LAST_ARGS); \
ret_type fn_prefix ## u ## typ ## 64 ## fn_suffix(PDCI_FIRST_ARGS, PDC_u ## typ ## 64 *lastnm PDCI_LAST_ARGS) \

/* ================================================================================ */ 
/* Declarations of function families */

#undef PDCI_LAST_ARGS
#define PDCI_LAST_ARGS 

#undef PDCI_FIRST_ARGS
#define PDCI_FIRST_ARGS PDC_t *pdc, PDC_base_csm *csm, PDC_base_ed *ed
PDCI_DECL_FAMILY(PDC_error_t, PDC_a_, int, _read_internal, res_out);
PDCI_DECL_FAMILY(PDC_error_t, PDC_e_, int, _read_internal, res_out);
PDCI_DECL_FAMILY(PDC_error_t, PDC_b_, int, _read_internal, res_out);

#undef PDCI_FIRST_ARGS
#define PDCI_FIRST_ARGS PDC_t *pdc, PDC_base_csm *csm, size_t width, PDC_base_ed *ed
PDCI_DECL_FAMILY(PDC_error_t, PDC_a_, int, _FW_read_internal, res_out);
PDCI_DECL_FAMILY(PDC_error_t, PDC_e_, int, _FW_read_internal, res_out);

#undef PDCI_FIRST_ARGS
#define PDCI_FIRST_ARGS PDC_t *pdc, PDC_base_csm *csm, PDC_uint32 num_digits, PDC_base_ed *ed
PDCI_DECL_FAMILY(PDC_error_t, PDC_ebc_, int, _read_internal, res_out);
PDCI_DECL_FAMILY(PDC_error_t, PDC_bcd_, int, _read_internal, res_out);

#undef PDCI_FIRST_ARGS
#define PDCI_FIRST_ARGS PDC_t *pdc, PDC_base_csm *csm, PDC_uint32 num_bytes, PDC_base_ed *ed
PDCI_DECL_FAMILY(PDC_error_t, PDC_sbl_, int, _read_internal, res_out);
PDCI_DECL_FAMILY(PDC_error_t, PDC_sbh_, int, _read_internal, res_out);

#undef PDCI_FIRST_ARGS
#define PDCI_FIRST_ARGS PDC_t *pdc, PDC_base_csm *csm, PDC_uint32 num_digits, PDC_uint32 d_exp, PDC_base_ed *ed
PDCI_DECL_FAMILY(PDC_error_t, PDC_ecb_, fpoint, _read_internal, res_out);
PDCI_DECL_FAMILY(PDC_error_t, PDC_bcd_, fpoint, _read_internal, res_out);

#undef PDCI_FIRST_ARGS
#define PDCI_FIRST_ARGS PDC_t *pdc, PDC_base_csm *csm, PDC_uint32 num_bytes, PDC_uint32 d_exp, PDC_base_ed *ed
PDCI_DECL_FAMILY(PDC_error_t, PDC_sbl_, fpoint, _read_internal, res_out);
PDCI_DECL_FAMILY(PDC_error_t, PDC_sbh_, fpoint, _read_internal, res_out);

/* INTERNAL VERSIONS OF WRITE FUNCTIONS */

int PDC_a_char_lit_write_internal(PDC_t *pdc, PDC_byte* buf, size_t buf_len, int *buf_full, PDC_byte c);
int PDC_a_str_lit_write_internal (PDC_t *pdc, PDC_byte* buf, size_t buf_len, int *buf_full, PDC_string *s);
int PDC_a_Cstr_lit_write_internal(PDC_t *pdc, PDC_byte* buf, size_t buf_len, int *buf_full, const char *s);

#undef PDCI_FIRST_ARGS
#define PDCI_FIRST_ARGS PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, PDC_base_ed *ed
PDCI_DECL_FAMILY(int, PDC_a_, int, _write_internal, val);
PDCI_DECL_FAMILY(int, PDC_e_, int, _write_internal, val);
PDCI_DECL_FAMILY(int, PDC_b_, int, _write_internal, val);

#undef PDCI_FIRST_ARGS
#define PDCI_FIRST_ARGS PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, size_t width, PDC_base_ed *ed
PDCI_DECL_FAMILY(int, PDC_a_, int, _FW_write_internal, val);
PDCI_DECL_FAMILY(int, PDC_e_, int, _FW_write_internal, val);

#undef PDCI_FIRST_ARGS
#define PDCI_FIRST_ARGS PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, PDC_uint32 num_digits, PDC_base_ed *ed
PDCI_DECL_FAMILY(int, PDC_ebc_, int, _write_internal, val);
PDCI_DECL_FAMILY(int, PDC_bcd_, int, _write_internal, val);

#undef PDCI_FIRST_ARGS
#define PDCI_FIRST_ARGS PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, PDC_uint32 num_bytes, PDC_base_ed *ed
PDCI_DECL_FAMILY(int, PDC_sbl_, int, _write_internal, val);
PDCI_DECL_FAMILY(int, PDC_sbh_, int, _write_internal, val);

#undef PDCI_FIRST_ARGS
#define PDCI_FIRST_ARGS PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, PDC_uint32 num_digits, PDC_uint32 d_exp, PDC_base_ed *ed
PDCI_DECL_FAMILY(int, PDC_ebc_, fpoint, _write_internal, val);
PDCI_DECL_FAMILY(int, PDC_bcd_, fpoint, _write_internal, val);

#undef PDCI_FIRST_ARGS
#define PDCI_FIRST_ARGS PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, PDC_uint32 num_bytes, PDC_uint32 d_exp, PDC_base_ed *ed
PDCI_DECL_FAMILY(int, PDC_sbl_, fpoint, _write_internal, val);
PDCI_DECL_FAMILY(int, PDC_sbh_, fpoint, _write_internal, val);

/* ================================================================================ */ 
/* INTERNAL VERSIONS OF ACCUM REPORTING FUNCTIONS */

/* These functions take an argument, outstr, for 
 * the output target, and do not check
 * the pdc, prefix, or accumulator arguments for NULL values.
 */

#undef PDCI_FIRST_ARGS
#define PDCI_FIRST_ARGS PDC_t *pdc, Sfio_t *outstr, const char *prefix, const char *what, int nst

PDC_error_t PDC_int8_acc_report_internal   (PDCI_FIRST_ARGS, PDC_int8_acc *a);
PDC_error_t PDC_int16_acc_report_internal  (PDCI_FIRST_ARGS, PDC_int16_acc *a);
PDC_error_t PDC_int32_acc_report_internal  (PDCI_FIRST_ARGS, PDC_int32_acc *a);
PDC_error_t PDC_int64_acc_report_internal  (PDCI_FIRST_ARGS, PDC_int64_acc *a);
PDC_error_t PDC_uint8_acc_report_internal  (PDCI_FIRST_ARGS, PDC_uint8_acc *a);
PDC_error_t PDC_uint16_acc_report_internal (PDCI_FIRST_ARGS, PDC_uint16_acc *a);
PDC_error_t PDC_uint32_acc_report_internal (PDCI_FIRST_ARGS, PDC_uint32_acc *a);
PDC_error_t PDC_uint64_acc_report_internal (PDCI_FIRST_ARGS, PDC_uint64_acc *a);

PDC_error_t PDC_int32_acc_report_map_internal(PDCI_FIRST_ARGS, PDC_int32_map_fn  fn, PDC_int32_acc *a);

PDC_error_t PDC_nerr_acc_report_internal(PDC_t *pdc, Sfio_t *outstr, const char *prefix, const char *what, int nst,
					 PDC_int32_acc *a);

PDC_error_t PDC_string_acc_report_internal (PDCI_FIRST_ARGS, PDC_string_acc *a);
PDC_error_t PDC_char_acc_report_internal   (PDCI_FIRST_ARGS, PDC_char_acc *a);

PDC_error_t PDC_fpoint8_acc_report_internal   (PDCI_FIRST_ARGS, PDC_fpoint8_acc *a);
PDC_error_t PDC_fpoint16_acc_report_internal  (PDCI_FIRST_ARGS, PDC_fpoint16_acc *a);
PDC_error_t PDC_fpoint32_acc_report_internal  (PDCI_FIRST_ARGS, PDC_fpoint32_acc *a);
PDC_error_t PDC_fpoint64_acc_report_internal  (PDCI_FIRST_ARGS, PDC_fpoint64_acc *a);
PDC_error_t PDC_ufpoint8_acc_report_internal  (PDCI_FIRST_ARGS, PDC_ufpoint8_acc *a);
PDC_error_t PDC_ufpoint16_acc_report_internal (PDCI_FIRST_ARGS, PDC_ufpoint16_acc *a);
PDC_error_t PDC_ufpoint32_acc_report_internal (PDCI_FIRST_ARGS, PDC_ufpoint32_acc *a);
PDC_error_t PDC_ufpoint64_acc_report_internal (PDCI_FIRST_ARGS, PDC_ufpoint64_acc *a);

/* ********************************************************************************
 * Remainder of this file contains function decls for functions
 * purely internal to the library impl.  Note the use of the PDCI prefix
 * for these functions 
 * ********************************************************************************/

/* ================================================================================ */ 
/* INTERNAL ERROR REPORTING FUNCTIONS */

/*
 * PDCI_report_err: Report a parse error that occurred at location loc.
 *
 * Can also use for other errors that have error codes: loc can be NULL.
 *
 * See description of PDC_error_f for description of level
 *  
 *   XXX errCode's type should be an enum that describes the kind of error XXX ???
 *
 * The whatfn param is optional (can be NULL). If non-null, the text
 * is included in the error message (unlike format, which replaces the default message).
 * Typically used to pass a string of this form:   "[in <function>]"
 *
 * The <format, ...> args are for a printf-style description that augments
 * the default description based on errCode. 
 *
 * N.B. This call does nothing if either there is no disc error function
 *      or if the disc e_rep is PDC_errorRep_None
 */

PDC_error_t PDCI_report_err(PDC_t *pdc, int level, PDC_loc_t *loc,
			    PDC_errCode_t errCode, const char *whatfn, const char *format, ... );

/* ================================================================================ */
/* PURELY INTERNAL IO FUNCTIONS */

/* 
 * Note: all of the following act on the IO cursor of the top checkpoint
 *
 * PDCI_IO_install_io:    XXX_TODOC
 * PDCI_IO_needbytes:     XXX_TODOC
 * PDCI_IO_morebytes:     XXX_TODOC
 *
 * PDCI_IO_forward:
 *
 *   Move IO cursor forward num_bytes bytes, which should be <=
 *   (end-begin), where [begin,end] are from the last call to needbytes
 *   or morebytes.  This call can obliviate that [begin,end] data
 *   region so IO_forward should only be used after all relevant data
 *   bytes have been observed.  Causes fatal error if K would move
 *   beyond an EOR/EOF marker or beyond the last in-memory data byte.
 */

PDC_error_t  PDCI_IO_install_io(PDC_t *pdc, Sfio_t *io);

PDC_error_t  PDCI_IO_needbytes (PDC_t *pdc,
				PDC_byte **b_out, PDC_byte **p1_out, PDC_byte **p2_out, PDC_byte **e_out,
			        int *eor_out, int *eof_out, size_t *bytes_out);
PDC_error_t  PDCI_IO_morebytes (PDC_t *pdc, PDC_byte **b_out, PDC_byte **p1_out, PDC_byte **p2_out, PDC_byte **e_out,
				int *eor_out, int *eof_out, size_t *bytes_out);
PDC_error_t  PDCI_IO_forward   (PDC_t *pdc, size_t num_bytes);

/*
 * PDCI_IO_start_write:  Alloc a buffer buf associated with an output Sfio stream
 *                       that can be filled in using the internal write functions.
 *                       Must be paired with either commit_write or abort_write. 
 *                       is_rec specifies whether this is the start of a record write,
 *                       Param buf_len specifies how many bytes to allocate in buf, and
 *                       can be modified to a greater value if an existing buffer of
 *                       larger size is used.  Param set_buf is set to indicate whether the
 *                       stream's buffer has been set.  The same buf, io, set_buf, is_rec
 *                       must be passed to the paired commit_write or abort_write.
 *                       Returns NULL on failure, buf on success.
 *
 * PDCI_IO_commit_write: Write num_bytes bytes from buf to the stream, de-alloc buf.
 *                       Returns -1 on error, otherwise number of bytes written, which will
 *                       be greater than num_bytes if is_rec is non-zero and writing
 *                       a record happes to involve additional header/trailer bytes.
 * 
 * PDCI_IO_abort_write:  de-alloc buf without writing anything to io.
 */

PDC_byte*   PDCI_IO_start_write (PDC_t *pdc, Sfio_t *io, size_t *buf_len, int *set_buf, int is_rec);
int         PDCI_IO_commit_write(PDC_t *pdc, Sfio_t *io, PDC_byte *buf, size_t num_bytes, int set_buf, int is_rec);
PDC_error_t PDCI_IO_abort_write (PDC_t *pdc, Sfio_t *io, PDC_byte *buf, int set_buf, int is_rec);

/*
 * Other IO routines:
 *    PDCI_IO_getElt: if the specified elt is currently in an in-memory buffer,
 *                    sets (*elt_out) to point to elt and returns PDC_OK,
 *                    otherwise returns PDC_ERR.
 */

PDC_error_t PDCI_IO_getElt(PDC_t *pdc, size_t num, PDC_IO_elt_t **elt_out);

/* ================================================================================ */
/* INTERNAL SCAN ROUTINES (helpers) */

PDC_error_t PDCI_char_lit_scan(PDC_t *pdc, PDC_byte c, PDC_byte s, int eat_lit,
			       PDC_byte *c_out, size_t *offset_out, PDC_charclass char_class, const char *whatfn);

PDC_error_t PDCI_str_lit_scan(PDC_t *pdc, const PDC_string *findStr, const PDC_string *stopStr, int eat_lit,
			      PDC_string **str_out, size_t *offset_out, PDC_charclass char_class, const char *whatfn);

PDC_error_t PDCI_Cstr_lit_scan(PDC_t *pdc, const char *findStr, const char *stopStr, int eat_lit,
			       const char **str_out, size_t *offset_out, PDC_charclass char_class, const char *whatfn);

/* ================================================================================ */
/* INTERNAL READ ROUTINES (helpers) */

PDC_error_t PDCI_char_lit_read(PDC_t *pdc, PDC_base_csm *csm, PDC_base_ed *ed,
			       PDC_byte c, PDC_charclass char_class, const char* whatfn);

PDC_error_t PDCI_str_lit_read(PDC_t *pdc, PDC_base_csm *csm, PDC_base_ed *ed,
			      const PDC_string *s, PDC_charclass char_class, const char *whatfn);

PDC_error_t PDCI_Cstr_lit_read(PDC_t *pdc, PDC_base_csm *csm, PDC_base_ed *ed,
			       const char *s, PDC_charclass char_class, const char *whatfn);

PDC_error_t PDCI_countX(PDC_t *pdc, PDC_base_csm *csm, PDC_uint8 x, int eor_required,
			PDC_base_ed *ed, PDC_int32 *res_out, PDC_charclass char_class, const char *whatfn);

PDC_error_t PDCI_countXtoY(PDC_t *pdc, PDC_base_csm *csm, PDC_uint8 x, PDC_uint8 y,
			   PDC_base_ed *ed, PDC_int32 *res_out, PDC_charclass char_class, const char *whatfn);

PDC_error_t PDCI_date_read(PDC_t *pdc, PDC_base_csm *csm, PDC_byte stopChar,
			   PDC_base_ed *ed, PDC_uint32 *res_out, PDC_charclass char_class, const char *whatfn);

PDC_error_t PDCI_char_read(PDC_t *pdc, PDC_base_csm *csm, PDC_base_ed *ed, PDC_char *c_out, PDC_charclass char_class, const char *whatfn);

PDC_error_t PDCI_string_FW_read(PDC_t *pdc, PDC_base_csm *csm, size_t width,
				PDC_base_ed *ed, PDC_string *s_out, PDC_charclass char_class, const char *whatfn);

PDC_error_t PDCI_string_read(PDC_t *pdc, PDC_base_csm *csm, PDC_byte stopChar,
			     PDC_base_ed *ed, PDC_string *s_out, PDC_charclass char_class, const char *whatfn);

PDC_error_t PDCI_string_ME_read(PDC_t *pdc, PDC_base_csm *csm, const char *matchRegexp,
				PDC_base_ed *ed, PDC_string *s_out, PDC_charclass char_class, const char *whatfn);

PDC_error_t PDCI_string_CME_read(PDC_t *pdc, PDC_base_csm *csm, PDC_regexp_t *matchRegexp,
				 PDC_base_ed *ed, PDC_string *s_out, PDC_charclass char_class, const char *whatfn);

PDC_error_t PDCI_string_SE_read(PDC_t *pdc, PDC_base_csm *csm, const char *stopRegexp,
				PDC_base_ed *ed, PDC_string *s_out, PDC_charclass char_class, const char *whatfn);

PDC_error_t PDCI_string_CSE_read(PDC_t *pdc, PDC_base_csm *csm, PDC_regexp_t *stopRegexp,
				 PDC_base_ed *ed, PDC_string *s_out, PDC_charclass char_class, const char *whatfn);

/* ================================================================================ */
/* INTERNAL CONVERSION ROUTINES */

/* Various tables */
extern int PDCI_ascii_digit[];
extern int PDCI_ascii_is_digit[];
extern int PDCI_ascii_is_space[];
#define PDCI_is_a_digit(c) PDCI_ascii_is_digit[c]
#define PDCI_is_a_space(c) PDCI_ascii_is_space[c]

extern int PDCI_ebcdic_digit[];
extern int PDCI_ebcdic_is_digit[];
extern int PDCI_ebcdic_is_space[];
#define PDCI_is_e_digit(c) PDCI_ebcdic_is_digit[c]
#define PDCI_is_e_space(c) PDCI_ebcdic_is_space[c]

extern PDC_byte PDC_ea_tab[];
extern PDC_byte PDC_ae_tab[];
extern PDC_byte PDC_mod_ae_tab[];

extern int PDCI_bcd_hilo_digits[];
extern int PDCI_bcd_hi_digit[];
extern PDC_uint64 PDCI_10toThe[];

PDC_int8   PDCI_a2int8  (PDC_t *pdc, const PDC_byte *bytes, PDC_byte **ptr_out);
PDC_int16  PDCI_a2int16 (PDC_t *pdc, const PDC_byte *bytes, PDC_byte **ptr_out);
PDC_int32  PDCI_a2int32 (PDC_t *pdc, const PDC_byte *bytes, PDC_byte **ptr_out);
PDC_int64  PDCI_a2int64 (PDC_t *pdc, const PDC_byte *bytes, PDC_byte **ptr_out);

int PDCI_int8_2a (PDC_t *pdc, PDC_byte *outbuf, size_t outbuf_len, int *outbuf_full, PDC_int8 i);
int PDCI_int16_2a(PDC_t *pdc, PDC_byte *outbuf, size_t outbuf_len, int *outbuf_full, PDC_int16 i);
int PDCI_int32_2a(PDC_t *pdc, PDC_byte *outbuf, size_t outbuf_len, int *outbuf_full, PDC_int32 i);
int PDCI_int64_2a(PDC_t *pdc, PDC_byte *outbuf, size_t outbuf_len, int *outbuf_full, PDC_int64 i);

PDC_uint8  PDCI_a2uint8 (PDC_t *pdc, const PDC_byte *bytes, PDC_byte **ptr_out);
PDC_uint16 PDCI_a2uint16(PDC_t *pdc, const PDC_byte *bytes, PDC_byte **ptr_out);
PDC_uint32 PDCI_a2uint32(PDC_t *pdc, const PDC_byte *bytes, PDC_byte **ptr_out);
PDC_uint64 PDCI_a2uint64(PDC_t *pdc, const PDC_byte *bytes, PDC_byte **ptr_out);

int PDCI_uint8_2a (PDC_t *pdc, PDC_byte *outbuf, size_t outbuf_len, int *outbuf_full, PDC_uint8 u);
int PDCI_uint16_2a(PDC_t *pdc, PDC_byte *outbuf, size_t outbuf_len, int *outbuf_full, PDC_uint16 u);
int PDCI_uint32_2a(PDC_t *pdc, PDC_byte *outbuf, size_t outbuf_len, int *outbuf_full, PDC_uint32 u);
int PDCI_uint64_2a(PDC_t *pdc, PDC_byte *outbuf, size_t outbuf_len, int *outbuf_full, PDC_uint64 u);

PDC_int8   PDCI_e2int8  (PDC_t *pdc, const PDC_byte *bytes, PDC_byte **ptr_out);
PDC_int16  PDCI_e2int16 (PDC_t *pdc, const PDC_byte *bytes, PDC_byte **ptr_out);
PDC_int32  PDCI_e2int32 (PDC_t *pdc, const PDC_byte *bytes, PDC_byte **ptr_out);
PDC_int64  PDCI_e2int64 (PDC_t *pdc, const PDC_byte *bytes, PDC_byte **ptr_out);

int PDCI_int8_2e (PDC_t *pdc, PDC_byte *outbuf, size_t outbuf_len, int *outbuf_full, PDC_int8  i);
int PDCI_int16_2e(PDC_t *pdc, PDC_byte *outbuf, size_t outbuf_len, int *outbuf_full, PDC_int16 i);
int PDCI_int32_2e(PDC_t *pdc, PDC_byte *outbuf, size_t outbuf_len, int *outbuf_full, PDC_int32 i);
int PDCI_int64_2e(PDC_t *pdc, PDC_byte *outbuf, size_t outbuf_len, int *outbuf_full, PDC_int64 i);

PDC_uint8  PDCI_e2uint8 (PDC_t *pdc, const PDC_byte *bytes, PDC_byte **ptr_out);
PDC_uint16 PDCI_e2uint16(PDC_t *pdc, const PDC_byte *bytes, PDC_byte **ptr_out);
PDC_uint32 PDCI_e2uint32(PDC_t *pdc, const PDC_byte *bytes, PDC_byte **ptr_out);
PDC_uint64 PDCI_e2uint64(PDC_t *pdc, const PDC_byte *bytes, PDC_byte **ptr_out);

int PDCI_uint8_2e (PDC_t *pdc, PDC_byte *outbuf, size_t outbuf_len, int *outbuf_full, PDC_uint8  u);
int PDCI_uint16_2e(PDC_t *pdc, PDC_byte *outbuf, size_t outbuf_len, int *outbuf_full, PDC_uint16 u);
int PDCI_uint32_2e(PDC_t *pdc, PDC_byte *outbuf, size_t outbuf_len, int *outbuf_full, PDC_uint32 u);
int PDCI_uint64_2e(PDC_t *pdc, PDC_byte *outbuf, size_t outbuf_len, int *outbuf_full, PDC_uint64 u);

PDC_int8   PDCI_ebc2int8 (PDC_t *pdc, const PDC_byte *bytes, PDC_uint32 num_digits, PDC_byte **ptr_out);
PDC_int16  PDCI_ebc2int16(PDC_t *pdc, const PDC_byte *bytes, PDC_uint32 num_digits, PDC_byte **ptr_out);
PDC_int32  PDCI_ebc2int32(PDC_t *pdc, const PDC_byte *bytes, PDC_uint32 num_digits, PDC_byte **ptr_out);
PDC_int64  PDCI_ebc2int64(PDC_t *pdc, const PDC_byte *bytes, PDC_uint32 num_digits, PDC_byte **ptr_out);

int PDCI_int8_2ebc (PDC_t *pdc, PDC_byte *outbuf, size_t outbuf_len, int *outbuf_full, PDC_int8  i, PDC_uint32 num_digits);
int PDCI_int16_2ebc(PDC_t *pdc, PDC_byte *outbuf, size_t outbuf_len, int *outbuf_full, PDC_int16 i, PDC_uint32 num_digits);
int PDCI_int32_2ebc(PDC_t *pdc, PDC_byte *outbuf, size_t outbuf_len, int *outbuf_full, PDC_int32 i, PDC_uint32 num_digits);
int PDCI_int64_2ebc(PDC_t *pdc, PDC_byte *outbuf, size_t outbuf_len, int *outbuf_full, PDC_int64 i, PDC_uint32 num_digits);

PDC_uint8   PDCI_ebc2uint8 (PDC_t *pdc, const PDC_byte *bytes, PDC_uint32 num_digits, PDC_byte **ptr_out);
PDC_uint16  PDCI_ebc2uint16(PDC_t *pdc, const PDC_byte *bytes, PDC_uint32 num_digits, PDC_byte **ptr_out);
PDC_uint32  PDCI_ebc2uint32(PDC_t *pdc, const PDC_byte *bytes, PDC_uint32 num_digits, PDC_byte **ptr_out);
PDC_uint64  PDCI_ebc2uint64(PDC_t *pdc, const PDC_byte *bytes, PDC_uint32 num_digits, PDC_byte **ptr_out);

int PDCI_uint8_2ebc (PDC_t *pdc, PDC_byte *outbuf, size_t outbuf_len, int *outbuf_full, PDC_uint8  u, PDC_uint32 num_digits);
int PDCI_uint16_2ebc(PDC_t *pdc, PDC_byte *outbuf, size_t outbuf_len, int *outbuf_full, PDC_uint16 u, PDC_uint32 num_digits);
int PDCI_uint32_2ebc(PDC_t *pdc, PDC_byte *outbuf, size_t outbuf_len, int *outbuf_full, PDC_uint32 u, PDC_uint32 num_digits);
int PDCI_uint64_2ebc(PDC_t *pdc, PDC_byte *outbuf, size_t outbuf_len, int *outbuf_full, PDC_uint64 u, PDC_uint32 num_digits);

PDC_int8   PDCI_bcd2int8 (PDC_t *pdc, const PDC_byte *bytes, PDC_uint32 num_digits, PDC_byte **ptr_out);
PDC_int16  PDCI_bcd2int16(PDC_t *pdc, const PDC_byte *bytes, PDC_uint32 num_digits, PDC_byte **ptr_out);
PDC_int32  PDCI_bcd2int32(PDC_t *pdc, const PDC_byte *bytes, PDC_uint32 num_digits, PDC_byte **ptr_out);
PDC_int64  PDCI_bcd2int64(PDC_t *pdc, const PDC_byte *bytes, PDC_uint32 num_digits, PDC_byte **ptr_out);

int PDCI_int8_2bcd (PDC_t *pdc, PDC_byte *outbuf, size_t outbuf_len, int *outbuf_full, PDC_int8  i, PDC_uint32 num_digits);
int PDCI_int16_2bcd(PDC_t *pdc, PDC_byte *outbuf, size_t outbuf_len, int *outbuf_full, PDC_int16 i, PDC_uint32 num_digits);
int PDCI_int32_2bcd(PDC_t *pdc, PDC_byte *outbuf, size_t outbuf_len, int *outbuf_full, PDC_int32 i, PDC_uint32 num_digits);
int PDCI_int64_2bcd(PDC_t *pdc, PDC_byte *outbuf, size_t outbuf_len, int *outbuf_full, PDC_int64 i, PDC_uint32 num_digits);

PDC_uint8   PDCI_bcd2uint8 (PDC_t *pdc, const PDC_byte *bytes, PDC_uint32 num_digits, PDC_byte **ptr_out);
PDC_uint16  PDCI_bcd2uint16(PDC_t *pdc, const PDC_byte *bytes, PDC_uint32 num_digits, PDC_byte **ptr_out);
PDC_uint32  PDCI_bcd2uint32(PDC_t *pdc, const PDC_byte *bytes, PDC_uint32 num_digits, PDC_byte **ptr_out);
PDC_uint64  PDCI_bcd2uint64(PDC_t *pdc, const PDC_byte *bytes, PDC_uint32 num_digits, PDC_byte **ptr_out);

int PDCI_uint8_2bcd (PDC_t *pdc, PDC_byte *outbuf, size_t outbuf_len, int *outbuf_full, PDC_uint8  u, PDC_uint32 num_digits);
int PDCI_uint16_2bcd(PDC_t *pdc, PDC_byte *outbuf, size_t outbuf_len, int *outbuf_full, PDC_uint16 u, PDC_uint32 num_digits);
int PDCI_uint32_2bcd(PDC_t *pdc, PDC_byte *outbuf, size_t outbuf_len, int *outbuf_full, PDC_uint32 u, PDC_uint32 num_digits);
int PDCI_uint64_2bcd(PDC_t *pdc, PDC_byte *outbuf, size_t outbuf_len, int *outbuf_full, PDC_uint64 u, PDC_uint32 num_digits);

PDC_int8   PDCI_sbl2int8 (PDC_t *pdc, const PDC_byte *bytes, PDC_uint32 num_bytes, PDC_byte **ptr_out);
PDC_int16  PDCI_sbl2int16(PDC_t *pdc, const PDC_byte *bytes, PDC_uint32 num_bytes, PDC_byte **ptr_out);
PDC_int32  PDCI_sbl2int32(PDC_t *pdc, const PDC_byte *bytes, PDC_uint32 num_bytes, PDC_byte **ptr_out);
PDC_int64  PDCI_sbl2int64(PDC_t *pdc, const PDC_byte *bytes, PDC_uint32 num_bytes, PDC_byte **ptr_out);

int PDCI_int8_2sbl (PDC_t *pdc, PDC_byte *outbuf, size_t outbuf_len, int *outbuf_full, PDC_int8  i, PDC_uint32 num_bytes);
int PDCI_int16_2sbl(PDC_t *pdc, PDC_byte *outbuf, size_t outbuf_len, int *outbuf_full, PDC_int16 i, PDC_uint32 num_bytes);
int PDCI_int32_2sbl(PDC_t *pdc, PDC_byte *outbuf, size_t outbuf_len, int *outbuf_full, PDC_int32 i, PDC_uint32 num_bytes);
int PDCI_int64_2sbl(PDC_t *pdc, PDC_byte *outbuf, size_t outbuf_len, int *outbuf_full, PDC_int64 i, PDC_uint32 num_bytes);

PDC_uint8   PDCI_sbl2uint8 (PDC_t *pdc, const PDC_byte *bytes, PDC_uint32 num_bytes, PDC_byte **ptr_out);
PDC_uint16  PDCI_sbl2uint16(PDC_t *pdc, const PDC_byte *bytes, PDC_uint32 num_bytes, PDC_byte **ptr_out);
PDC_uint32  PDCI_sbl2uint32(PDC_t *pdc, const PDC_byte *bytes, PDC_uint32 num_bytes, PDC_byte **ptr_out);
PDC_uint64  PDCI_sbl2uint64(PDC_t *pdc, const PDC_byte *bytes, PDC_uint32 num_bytes, PDC_byte **ptr_out);

int PDCI_uint8_2sbl (PDC_t *pdc, PDC_byte *outbuf, size_t outbuf_len, int *outbuf_full, PDC_uint8  u, PDC_uint32 num_bytes);
int PDCI_uint16_2sbl(PDC_t *pdc, PDC_byte *outbuf, size_t outbuf_len, int *outbuf_full, PDC_uint16 u, PDC_uint32 num_bytes);
int PDCI_uint32_2sbl(PDC_t *pdc, PDC_byte *outbuf, size_t outbuf_len, int *outbuf_full, PDC_uint32 u, PDC_uint32 num_bytes);
int PDCI_uint64_2sbl(PDC_t *pdc, PDC_byte *outbuf, size_t outbuf_len, int *outbuf_full, PDC_uint64 u, PDC_uint32 num_bytes);

PDC_int8   PDCI_sbh2int8 (PDC_t *pdc, const PDC_byte *bytes, PDC_uint32 num_bytes, PDC_byte **ptr_out);
PDC_int16  PDCI_sbh2int16(PDC_t *pdc, const PDC_byte *bytes, PDC_uint32 num_bytes, PDC_byte **ptr_out);
PDC_int32  PDCI_sbh2int32(PDC_t *pdc, const PDC_byte *bytes, PDC_uint32 num_bytes, PDC_byte **ptr_out);
PDC_int64  PDCI_sbh2int64(PDC_t *pdc, const PDC_byte *bytes, PDC_uint32 num_bytes, PDC_byte **ptr_out);

int PDCI_int8_2sbh (PDC_t *pdc, PDC_byte *outbuf, size_t outbuf_len, int *outbuf_full, PDC_int8  i, PDC_uint32 num_bytes);
int PDCI_int16_2sbh(PDC_t *pdc, PDC_byte *outbuf, size_t outbuf_len, int *outbuf_full, PDC_int16 i, PDC_uint32 num_bytes);
int PDCI_int32_2sbh(PDC_t *pdc, PDC_byte *outbuf, size_t outbuf_len, int *outbuf_full, PDC_int32 i, PDC_uint32 num_bytes);
int PDCI_int64_2sbh(PDC_t *pdc, PDC_byte *outbuf, size_t outbuf_len, int *outbuf_full, PDC_int64 i, PDC_uint32 num_bytes);

PDC_uint8   PDCI_sbh2uint8 (PDC_t *pdc, const PDC_byte *bytes, PDC_uint32 num_bytes, PDC_byte **ptr_out);
PDC_uint16  PDCI_sbh2uint16(PDC_t *pdc, const PDC_byte *bytes, PDC_uint32 num_bytes, PDC_byte **ptr_out);
PDC_uint32  PDCI_sbh2uint32(PDC_t *pdc, const PDC_byte *bytes, PDC_uint32 num_bytes, PDC_byte **ptr_out);
PDC_uint64  PDCI_sbh2uint64(PDC_t *pdc, const PDC_byte *bytes, PDC_uint32 num_bytes, PDC_byte **ptr_out);

int PDCI_uint8_2sbh (PDC_t *pdc, PDC_byte *outbuf, size_t outbuf_len, int *outbuf_full, PDC_uint8  u, PDC_uint32 num_bytes);
int PDCI_uint16_2sbh(PDC_t *pdc, PDC_byte *outbuf, size_t outbuf_len, int *outbuf_full, PDC_uint16 u, PDC_uint32 num_bytes);
int PDCI_uint32_2sbh(PDC_t *pdc, PDC_byte *outbuf, size_t outbuf_len, int *outbuf_full, PDC_uint32 u, PDC_uint32 num_bytes);
int PDCI_uint64_2sbh(PDC_t *pdc, PDC_byte *outbuf, size_t outbuf_len, int *outbuf_full, PDC_uint64 u, PDC_uint32 num_bytes);

/* ================================================================================ */
/* INTERNAL MISC ROUTINES */


/* Internal version of PDC_regexp_compile, takes whatfn */
PDC_error_t
PDCI_regexp_compile(PDC_t *pdc, const char *regexp, PDC_regexp_t **regexp_out, const char *whatfn);

/*  PDCI_regexpMatch returns the number of characters in str that match regexp
 *  (or 0 if str does not match the regular expression).
 */
size_t PDCI_regexpMatch(PDC_t *pdc, PDC_regexp_t *regexp, PDC_byte *begin, PDC_byte *end, PDC_charclass char_class);

/* Accum impl helpers:
 *
 * PDCI_nst_prefix_what prints a heading to outstr 
 * based on *nst nesting level and
 * (unless *nst is -1) it increments the nesting level.
 */

void PDCI_nst_prefix_what(Sfio_t *outstr, int *nst, const char *prefix, const char *what);

/* 
 * PDCI_findfirst and PDCI_findlast are like strchr and strrchr except NULL does
 * not terminate the search: instead, begin/end bracket the search space, where
 * end is one byte beyond the last byte to check.
 */ 
PDC_byte *PDCI_findfirst(const PDC_byte *begin, const PDC_byte *end, PDC_byte b);
PDC_byte *PDCI_findlast(const PDC_byte *begin, const PDC_byte *end, PDC_byte b);


/* ================================================================================ */

#endif /*  __LIBPADSC_INTERNAL__  */
