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
/* INTERNAL VERSIONS OF ALL BASE TYPE READ FUNCTIONS */

PDC_error_t PDC_char_lit_read_internal(PDC_t *pdc, PDC_base_em *em,
				       PDC_base_ed *ed, PDC_byte c);

PDC_error_t PDC_str_lit_read_internal(PDC_t *pdc, PDC_base_em *em,
				      PDC_base_ed *ed, const PDC_string *s);

PDC_error_t PDC_countX_internal(PDC_t *pdc, PDC_base_em *em, PDC_uint8 x, int eor_required,
				PDC_base_ed *ed, PDC_int32 *res_out);

PDC_error_t PDC_countXtoY_internal(PDC_t *pdc, PDC_base_em *em, PDC_uint8 x, PDC_uint8 y,
				   PDC_base_ed *ed, PDC_int32 *res_out);

PDC_error_t PDC_a_date_read_internal(PDC_t *pdc, PDC_base_em *em, PDC_byte stopChar,
				     PDC_base_ed *ed, PDC_uint32 *res_out);

PDC_error_t PDC_e_date_read_internal(PDC_t *pdc, PDC_base_em *em, PDC_byte stopChar,
				     PDC_base_ed *ed, PDC_uint32 *res_out);

PDC_error_t PDC_a_char_read_internal (PDC_t *pdc, PDC_base_em *em, PDC_base_ed *ed, PDC_char *c_out);

PDC_error_t PDC_e_char_read_internal (PDC_t *pdc, PDC_base_em *em, PDC_base_ed *ed, PDC_char *c_out);

PDC_error_t PDC_a_string_FW_read_internal(PDC_t *pdc, PDC_base_em *em, size_t width,
					  PDC_base_ed *ed, PDC_string *s_out);

PDC_error_t PDC_a_string_read_internal(PDC_t *pdc, PDC_base_em *em, PDC_byte stopChar,
				       PDC_base_ed *ed, PDC_string *s_out);

PDC_error_t PDC_a_string_ME_read_internal(PDC_t *pdc, PDC_base_em *em, const char *matchRegexp,
					  PDC_base_ed *ed, PDC_string *s_out);

PDC_error_t PDC_a_string_CME_read_internal(PDC_t *pdc, PDC_base_em *em, PDC_regexp_t *matchRegexp,
					   PDC_base_ed *ed, PDC_string *s_out);

PDC_error_t PDC_a_string_SE_read_internal(PDC_t *pdc, PDC_base_em *em, const char *stopRegexp,
					  PDC_base_ed *ed, PDC_string *s_out);

PDC_error_t PDC_a_string_CSE_read_internal(PDC_t *pdc, PDC_base_em *em, PDC_regexp_t *stopRegexp,
					   PDC_base_ed *ed, PDC_string *s_out);

PDC_error_t PDC_e_string_FW_read_internal(PDC_t *pdc, PDC_base_em *em, size_t width,
					  PDC_base_ed *ed, PDC_string *s_out);

PDC_error_t PDC_e_string_read_internal(PDC_t *pdc, PDC_base_em *em, PDC_byte stopChar,
				       PDC_base_ed *ed, PDC_string *s_out);

PDC_error_t PDC_e_string_ME_read_internal(PDC_t *pdc, PDC_base_em *em, const char *matchRegexp,
					  PDC_base_ed *ed, PDC_string *s_out);

PDC_error_t PDC_e_string_CME_read_internal(PDC_t *pdc, PDC_base_em *em, PDC_regexp_t *matchRegexp,
					   PDC_base_ed *ed, PDC_string *s_out);

PDC_error_t PDC_e_string_SE_read_internal(PDC_t *pdc, PDC_base_em *em, const char *stopRegexp,
					  PDC_base_ed *ed, PDC_string *s_out);

PDC_error_t PDC_e_string_CSE_read_internal(PDC_t *pdc, PDC_base_em *em, PDC_regexp_t *stopRegexp,
					   PDC_base_ed *ed, PDC_string *s_out);

/* ================================================================================ */ 
/* HELPER MACRO TO DECLARE FAMILY OF FUNCTIONS */
/* N.B. First you must declare PDCI_FIRST_ARGS and PDCI_LAST_ARGS.  Follow macro with a semi */

#define PDCI_DECL_FAMILY(ret_type, fn_prefix, typ, fn_suffix) \
ret_type fn_prefix ## typ ## 8 ## fn_suffix (PDCI_FIRST_ARGS, PDC_ ## typ ## 8 *res_out PDCI_LAST_ARGS); \
ret_type fn_prefix ## typ ## 16 ## fn_suffix(PDCI_FIRST_ARGS, PDC_ ## typ ## 16 *res_out PDCI_LAST_ARGS); \
ret_type fn_prefix ## typ ## 32 ## fn_suffix(PDCI_FIRST_ARGS, PDC_ ## typ ## 32 *res_out PDCI_LAST_ARGS); \
ret_type fn_prefix ## typ ## 64 ## fn_suffix(PDCI_FIRST_ARGS, PDC_ ## typ  ## 64 *res_out PDCI_LAST_ARGS); \
ret_type fn_prefix ## u ## typ ## 8 ## fn_suffix (PDCI_FIRST_ARGS, PDC_u ## typ ## 8 *res_out PDCI_LAST_ARGS); \
ret_type fn_prefix ## u ## typ ## 16 ## fn_suffix(PDCI_FIRST_ARGS, PDC_u ## typ ## 16 *res_out PDCI_LAST_ARGS); \
ret_type fn_prefix ## u ## typ ## 32 ## fn_suffix(PDCI_FIRST_ARGS, PDC_u ## typ ## 32 *res_out PDCI_LAST_ARGS); \
ret_type fn_prefix ## u ## typ ## 64 ## fn_suffix(PDCI_FIRST_ARGS, PDC_u ## typ ## 64 *res_out PDCI_LAST_ARGS) \

/* ================================================================================ */ 
/* Declarations of function families */

#undef PDCI_LAST_ARGS
#define PDCI_LAST_ARGS 

#undef PDCI_FIRST_ARGS
#define PDCI_FIRST_ARGS PDC_t *pdc, PDC_base_em *em, PDC_base_ed *ed
PDCI_DECL_FAMILY(PDC_error_t, PDC_a_, int, _read_internal);
PDCI_DECL_FAMILY(PDC_error_t, PDC_e_, int, _read_internal);
PDCI_DECL_FAMILY(PDC_error_t, PDC_b_, int, _read_internal);

#undef PDCI_FIRST_ARGS
#define PDCI_FIRST_ARGS PDC_t *pdc, PDC_base_em *em, size_t width, PDC_base_ed *ed
PDCI_DECL_FAMILY(PDC_error_t, PDC_a_, int, _FW_read_internal);
PDCI_DECL_FAMILY(PDC_error_t, PDC_e_, int, _FW_read_internal);

#undef PDCI_FIRST_ARGS
#define PDCI_FIRST_ARGS PDC_t *pdc, PDC_base_em *em, PDC_uint32 digits, PDC_base_ed *ed
PDCI_DECL_FAMILY(PDC_error_t, PDC_ebc_, int, _read_internal);
PDCI_DECL_FAMILY(PDC_error_t, PDC_bcd_, int, _read_internal);

#undef PDCI_FIRST_ARGS
#define PDCI_FIRST_ARGS PDC_t *pdc, PDC_base_em *em, PDC_uint32 num_bytes, PDC_base_ed *ed
PDCI_DECL_FAMILY(PDC_error_t, PDC_sbl_, int, _read_internal);
PDCI_DECL_FAMILY(PDC_error_t, PDC_sbh_, int, _read_internal);

#undef PDCI_FIRST_ARGS
#define PDCI_FIRST_ARGS PDC_t *pdc, PDC_base_em *em, PDC_uint32 num_digits, PDC_uint32 d_exp, PDC_base_ed *ed
PDCI_DECL_FAMILY(PDC_error_t, PDC_ecb_, fpoint, _read_internal);
PDCI_DECL_FAMILY(PDC_error_t, PDC_bcd_, fpoint, _read_internal);

#undef PDCI_FIRST_ARGS
#define PDCI_FIRST_ARGS PDC_t *pdc, PDC_base_em *em, PDC_uint32 num_bytes, PDC_uint32 d_exp, PDC_base_ed *ed
PDCI_DECL_FAMILY(PDC_error_t, PDC_sbl_, fpoint, _read_internal);
PDCI_DECL_FAMILY(PDC_error_t, PDC_sbh_, fpoint, _read_internal);

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
 * Other IO routines:
 *    PDCI_IO_getElt: if the specified elt is currently in an in-memory buffer,
 *                    sets (*elt_out) to point to elt and returns PDC_OK,
 *                    otherwise returns PDC_ERR.
 */

PDC_error_t PDCI_IO_getElt(PDC_t *pdc, size_t num, PDC_IO_elt_t **elt_out);

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

int PDCI_int8_2a (PDC_t *pdc, Sfio_t *io, PDC_int8 i);
int PDCI_int16_2a(PDC_t *pdc, Sfio_t *io, PDC_int16 i);
int PDCI_int32_2a(PDC_t *pdc, Sfio_t *io, PDC_int32 i);
int PDCI_int64_2a(PDC_t *pdc, Sfio_t *io, PDC_int64 i);

PDC_uint8  PDCI_a2uint8 (PDC_t *pdc, const PDC_byte *bytes, PDC_byte **ptr_out);
PDC_uint16 PDCI_a2uint16(PDC_t *pdc, const PDC_byte *bytes, PDC_byte **ptr_out);
PDC_uint32 PDCI_a2uint32(PDC_t *pdc, const PDC_byte *bytes, PDC_byte **ptr_out);
PDC_uint64 PDCI_a2uint64(PDC_t *pdc, const PDC_byte *bytes, PDC_byte **ptr_out);

int PDCI_uint8_2a (PDC_t *pdc, Sfio_t *io, PDC_uint8 u);
int PDCI_uint16_2a(PDC_t *pdc, Sfio_t *io, PDC_uint16 u);
int PDCI_uint32_2a(PDC_t *pdc, Sfio_t *io, PDC_uint32 u);
int PDCI_uint64_2a(PDC_t *pdc, Sfio_t *io, PDC_uint64 u);

PDC_int8   PDCI_e2int8  (PDC_t *pdc, const PDC_byte *bytes, PDC_byte **ptr_out);
PDC_int16  PDCI_e2int16 (PDC_t *pdc, const PDC_byte *bytes, PDC_byte **ptr_out);
PDC_int32  PDCI_e2int32 (PDC_t *pdc, const PDC_byte *bytes, PDC_byte **ptr_out);
PDC_int64  PDCI_e2int64 (PDC_t *pdc, const PDC_byte *bytes, PDC_byte **ptr_out);

int PDCI_int8_2e (PDC_t *pdc, Sfio_t *io, PDC_int8  i);
int PDCI_int16_2e(PDC_t *pdc, Sfio_t *io, PDC_int16 i);
int PDCI_int32_2e(PDC_t *pdc, Sfio_t *io, PDC_int32 i);
int PDCI_int64_2e(PDC_t *pdc, Sfio_t *io, PDC_int64 i);

PDC_uint8  PDCI_e2uint8 (PDC_t *pdc, const PDC_byte *bytes, PDC_byte **ptr_out);
PDC_uint16 PDCI_e2uint16(PDC_t *pdc, const PDC_byte *bytes, PDC_byte **ptr_out);
PDC_uint32 PDCI_e2uint32(PDC_t *pdc, const PDC_byte *bytes, PDC_byte **ptr_out);
PDC_uint64 PDCI_e2uint64(PDC_t *pdc, const PDC_byte *bytes, PDC_byte **ptr_out);

int PDCI_uint8_2e (PDC_t *pdc, Sfio_t *io, PDC_uint8  u);
int PDCI_uint16_2e(PDC_t *pdc, Sfio_t *io, PDC_uint16 u);
int PDCI_uint32_2e(PDC_t *pdc, Sfio_t *io, PDC_uint32 u);
int PDCI_uint64_2e(PDC_t *pdc, Sfio_t *io, PDC_uint64 u);

PDC_int8   PDCI_ebc2int8 (PDC_t *pdc, const PDC_byte *bytes, PDC_uint32 num_digits, PDC_byte **ptr_out);
PDC_int16  PDCI_ebc2int16(PDC_t *pdc, const PDC_byte *bytes, PDC_uint32 num_digits, PDC_byte **ptr_out);
PDC_int32  PDCI_ebc2int32(PDC_t *pdc, const PDC_byte *bytes, PDC_uint32 num_digits, PDC_byte **ptr_out);
PDC_int64  PDCI_ebc2int64(PDC_t *pdc, const PDC_byte *bytes, PDC_uint32 num_digits, PDC_byte **ptr_out);

int PDCI_int8_2ebc (PDC_t *pdc, Sfio_t *io, PDC_int8  i, PDC_uint32 num_digits);
int PDCI_int16_2ebc(PDC_t *pdc, Sfio_t *io, PDC_int16 i, PDC_uint32 num_digits);
int PDCI_int32_2ebc(PDC_t *pdc, Sfio_t *io, PDC_int32 i, PDC_uint32 num_digits);
int PDCI_int64_2ebc(PDC_t *pdc, Sfio_t *io, PDC_int64 i, PDC_uint32 num_digits);

PDC_uint8   PDCI_ebc2uint8 (PDC_t *pdc, const PDC_byte *bytes, PDC_uint32 num_digits, PDC_byte **ptr_out);
PDC_uint16  PDCI_ebc2uint16(PDC_t *pdc, const PDC_byte *bytes, PDC_uint32 num_digits, PDC_byte **ptr_out);
PDC_uint32  PDCI_ebc2uint32(PDC_t *pdc, const PDC_byte *bytes, PDC_uint32 num_digits, PDC_byte **ptr_out);
PDC_uint64  PDCI_ebc2uint64(PDC_t *pdc, const PDC_byte *bytes, PDC_uint32 num_digits, PDC_byte **ptr_out);

int PDCI_uint8_2ebc (PDC_t *pdc, Sfio_t *io, PDC_uint8  u, PDC_uint32 num_digits);
int PDCI_uint16_2ebc(PDC_t *pdc, Sfio_t *io, PDC_uint16 u, PDC_uint32 num_digits);
int PDCI_uint32_2ebc(PDC_t *pdc, Sfio_t *io, PDC_uint32 u, PDC_uint32 num_digits);
int PDCI_uint64_2ebc(PDC_t *pdc, Sfio_t *io, PDC_uint64 u, PDC_uint32 num_digits);

PDC_int8   PDCI_bcd2int8 (PDC_t *pdc, const PDC_byte *bytes, PDC_uint32 num_digits, PDC_byte **ptr_out);
PDC_int16  PDCI_bcd2int16(PDC_t *pdc, const PDC_byte *bytes, PDC_uint32 num_digits, PDC_byte **ptr_out);
PDC_int32  PDCI_bcd2int32(PDC_t *pdc, const PDC_byte *bytes, PDC_uint32 num_digits, PDC_byte **ptr_out);
PDC_int64  PDCI_bcd2int64(PDC_t *pdc, const PDC_byte *bytes, PDC_uint32 num_digits, PDC_byte **ptr_out);

int PDCI_int8_2bcd (PDC_t *pdc, Sfio_t *io, PDC_int8  i, PDC_uint32 num_digits);
int PDCI_int16_2bcd(PDC_t *pdc, Sfio_t *io, PDC_int16 i, PDC_uint32 num_digits);
int PDCI_int32_2bcd(PDC_t *pdc, Sfio_t *io, PDC_int32 i, PDC_uint32 num_digits);
int PDCI_int64_2bcd(PDC_t *pdc, Sfio_t *io, PDC_int64 i, PDC_uint32 num_digits);

PDC_uint8   PDCI_bcd2uint8 (PDC_t *pdc, const PDC_byte *bytes, PDC_uint32 num_digits, PDC_byte **ptr_out);
PDC_uint16  PDCI_bcd2uint16(PDC_t *pdc, const PDC_byte *bytes, PDC_uint32 num_digits, PDC_byte **ptr_out);
PDC_uint32  PDCI_bcd2uint32(PDC_t *pdc, const PDC_byte *bytes, PDC_uint32 num_digits, PDC_byte **ptr_out);
PDC_uint64  PDCI_bcd2uint64(PDC_t *pdc, const PDC_byte *bytes, PDC_uint32 num_digits, PDC_byte **ptr_out);

int PDCI_uint8_2bcd (PDC_t *pdc, Sfio_t *io, PDC_uint8  u, PDC_uint32 num_digits);
int PDCI_uint16_2bcd(PDC_t *pdc, Sfio_t *io, PDC_uint16 u, PDC_uint32 num_digits);
int PDCI_uint32_2bcd(PDC_t *pdc, Sfio_t *io, PDC_uint32 u, PDC_uint32 num_digits);
int PDCI_uint64_2bcd(PDC_t *pdc, Sfio_t *io, PDC_uint64 u, PDC_uint32 num_digits);

PDC_int8   PDCI_sbl2int8 (PDC_t *pdc, const PDC_byte *bytes, PDC_uint32 num_bytes, PDC_byte **ptr_out);
PDC_int16  PDCI_sbl2int16(PDC_t *pdc, const PDC_byte *bytes, PDC_uint32 num_bytes, PDC_byte **ptr_out);
PDC_int32  PDCI_sbl2int32(PDC_t *pdc, const PDC_byte *bytes, PDC_uint32 num_bytes, PDC_byte **ptr_out);
PDC_int64  PDCI_sbl2int64(PDC_t *pdc, const PDC_byte *bytes, PDC_uint32 num_bytes, PDC_byte **ptr_out);

int PDCI_int8_2sbl (PDC_t *pdc, Sfio_t *io, PDC_int8  i, PDC_uint32 num_bytes);
int PDCI_int16_2sbl(PDC_t *pdc, Sfio_t *io, PDC_int16 i, PDC_uint32 num_bytes);
int PDCI_int32_2sbl(PDC_t *pdc, Sfio_t *io, PDC_int32 i, PDC_uint32 num_bytes);
int PDCI_int64_2sbl(PDC_t *pdc, Sfio_t *io, PDC_int64 i, PDC_uint32 num_bytes);

PDC_uint8   PDCI_sbl2uint8 (PDC_t *pdc, const PDC_byte *bytes, PDC_uint32 num_bytes, PDC_byte **ptr_out);
PDC_uint16  PDCI_sbl2uint16(PDC_t *pdc, const PDC_byte *bytes, PDC_uint32 num_bytes, PDC_byte **ptr_out);
PDC_uint32  PDCI_sbl2uint32(PDC_t *pdc, const PDC_byte *bytes, PDC_uint32 num_bytes, PDC_byte **ptr_out);
PDC_uint64  PDCI_sbl2uint64(PDC_t *pdc, const PDC_byte *bytes, PDC_uint32 num_bytes, PDC_byte **ptr_out);

int PDCI_uint8_2sbl (PDC_t *pdc, Sfio_t *io, PDC_uint8  u, PDC_uint32 num_bytes);
int PDCI_uint16_2sbl(PDC_t *pdc, Sfio_t *io, PDC_uint16 u, PDC_uint32 num_bytes);
int PDCI_uint32_2sbl(PDC_t *pdc, Sfio_t *io, PDC_uint32 u, PDC_uint32 num_bytes);
int PDCI_uint64_2sbl(PDC_t *pdc, Sfio_t *io, PDC_uint64 u, PDC_uint32 num_bytes);

PDC_int8   PDCI_sbh2int8 (PDC_t *pdc, const PDC_byte *bytes, PDC_uint32 num_bytes, PDC_byte **ptr_out);
PDC_int16  PDCI_sbh2int16(PDC_t *pdc, const PDC_byte *bytes, PDC_uint32 num_bytes, PDC_byte **ptr_out);
PDC_int32  PDCI_sbh2int32(PDC_t *pdc, const PDC_byte *bytes, PDC_uint32 num_bytes, PDC_byte **ptr_out);
PDC_int64  PDCI_sbh2int64(PDC_t *pdc, const PDC_byte *bytes, PDC_uint32 num_bytes, PDC_byte **ptr_out);

int PDCI_int8_2sbh (PDC_t *pdc, Sfio_t *io, PDC_int8  i, PDC_uint32 num_bytes);
int PDCI_int16_2sbh(PDC_t *pdc, Sfio_t *io, PDC_int16 i, PDC_uint32 num_bytes);
int PDCI_int32_2sbh(PDC_t *pdc, Sfio_t *io, PDC_int32 i, PDC_uint32 num_bytes);
int PDCI_int64_2sbh(PDC_t *pdc, Sfio_t *io, PDC_int64 i, PDC_uint32 num_bytes);

PDC_uint8   PDCI_sbh2uint8 (PDC_t *pdc, const PDC_byte *bytes, PDC_uint32 num_bytes, PDC_byte **ptr_out);
PDC_uint16  PDCI_sbh2uint16(PDC_t *pdc, const PDC_byte *bytes, PDC_uint32 num_bytes, PDC_byte **ptr_out);
PDC_uint32  PDCI_sbh2uint32(PDC_t *pdc, const PDC_byte *bytes, PDC_uint32 num_bytes, PDC_byte **ptr_out);
PDC_uint64  PDCI_sbh2uint64(PDC_t *pdc, const PDC_byte *bytes, PDC_uint32 num_bytes, PDC_byte **ptr_out);

int PDCI_uint8_2sbh (PDC_t *pdc, Sfio_t *io, PDC_uint8  u, PDC_uint32 num_bytes);
int PDCI_uint16_2sbh(PDC_t *pdc, Sfio_t *io, PDC_uint16 u, PDC_uint32 num_bytes);
int PDCI_uint32_2sbh(PDC_t *pdc, Sfio_t *io, PDC_uint32 u, PDC_uint32 num_bytes);
int PDCI_uint64_2sbh(PDC_t *pdc, Sfio_t *io, PDC_uint64 u, PDC_uint32 num_bytes);

/* ================================================================================ */
/* INTERNAL MISC ROUTINES */

/*  PDCI_regexpMatch returns the number of characters in str that match regexp
 *  (or 0 if str does not match the regular expression).  If ebcdic is non-zero, the
 *  chars between begin and end are EBCDIC chars, otherwise they are ASCII chars.
 */

size_t PDCI_regexpMatch(PDC_t *pdc, PDC_regexp_t *regexp, PDC_byte *begin, PDC_byte *end, int ebcdic);

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
