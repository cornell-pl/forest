#pragma prototyped
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

PDC_error_t  PDC_IO_fopen_internal    (PDC_t *pdc, char *path);
PDC_error_t  PDC_IO_fclose_internal   (PDC_t *pdc);
PDC_error_t  PDC_IO_next_rec_internal (PDC_t *pdc, size_t *skipped_bytes_out);

int          PDC_IO_at_EOR_internal   (PDC_t *pdc);
int          PDC_IO_at_EOF_internal   (PDC_t *pdc);

PDC_error_t  PDC_IO_getPos_internal   (PDC_t *pdc, PDC_pos_t *pos, int offset); 

/* ================================================================================ */ 
/* INTERNAL VERSIONS OF ALL BASE TYPE READ FUNCTIONS */

PDC_error_t PDC_char_lit_read_internal(PDC_t *pdc, PDC_base_em *em,
				       PDC_base_ed *ed, unsigned char c);

PDC_error_t PDC_str_lit_read_internal(PDC_t *pdc, PDC_base_em *em,
				      PDC_base_ed *ed, const PDC_string *s);

PDC_error_t PDC_countX_internal(PDC_t *pdc, PDC_base_em *em, PDC_uint8 x, int eor_required,
				PDC_base_ed *ed, PDC_int32 *res_out);

PDC_error_t PDC_countXtoY_internal(PDC_t *pdc, PDC_base_em *em, PDC_uint8 x, PDC_uint8 y,
				   PDC_base_ed *ed, PDC_int32 *res_out);

PDC_error_t PDC_adate_read_internal(PDC_t *pdc, PDC_base_em *em, unsigned char stopChar,
				    PDC_base_ed *ed, PDC_uint32 *res_out);

PDC_error_t PDC_astringFW_read_internal(PDC_t *pdc, PDC_base_em *em, size_t width,
					PDC_base_ed *ed, PDC_string *s_out);

PDC_error_t PDC_astring_read_internal(PDC_t *pdc, PDC_base_em *em, unsigned char stopChar,
				      PDC_base_ed *ed, PDC_string *s_out);

PDC_error_t PDC_astringSE_read_internal(PDC_t *pdc, PDC_base_em *em, const char *stopRegexp,
					PDC_base_ed *ed, PDC_string *s_out);

PDC_error_t PDC_astringCSE_read_internal(PDC_t *pdc, PDC_base_em *em, PDC_regexp_t *stopRegexp,
					 PDC_base_ed *ed, PDC_string *s_out);

PDC_error_t PDC_estringFW_read_internal(PDC_t *pdc, PDC_base_em *em, size_t width,
					PDC_base_ed *ed, PDC_string *s_out);

PDC_error_t PDC_estring_read_internal(PDC_t *pdc, PDC_base_em *em, unsigned char stopChar,
				      PDC_base_ed *ed, PDC_string *s_out);

PDC_error_t PDC_estringSE_read_internal(PDC_t *pdc, PDC_base_em *em, const char *stopRegexp,
					PDC_base_ed *ed, PDC_string *s_out);

PDC_error_t PDC_estringCSE_read_internal(PDC_t *pdc, PDC_base_em *em, PDC_regexp_t *stopRegexp,
					 PDC_base_ed *ed, PDC_string *s_out);

PDC_error_t PDC_aint8_read_internal (PDC_t *pdc, PDC_base_em *em,
				     PDC_base_ed *ed, PDC_int8 *res_out);

PDC_error_t PDC_aint16_read_internal(PDC_t *pdc, PDC_base_em *em,
				     PDC_base_ed *ed, PDC_int16 *res_out);

PDC_error_t PDC_aint32_read_internal(PDC_t *pdc, PDC_base_em *em,
				     PDC_base_ed *ed, PDC_int32 *res_out);

PDC_error_t PDC_aint64_read_internal(PDC_t *pdc, PDC_base_em *em,
				     PDC_base_ed *ed, PDC_int64 *res_out);

PDC_error_t PDC_auint8_read_internal (PDC_t *pdc, PDC_base_em *em,
				      PDC_base_ed *ed, PDC_uint8 *res_out);

PDC_error_t PDC_auint16_read_internal(PDC_t *pdc, PDC_base_em *em,
				      PDC_base_ed *ed, PDC_uint16 *res_out);

PDC_error_t PDC_auint32_read_internal(PDC_t *pdc, PDC_base_em *em,
				      PDC_base_ed *ed, PDC_uint32 *res_out);

PDC_error_t PDC_auint64_read_internal(PDC_t *pdc, PDC_base_em *em,
				      PDC_base_ed *ed, PDC_uint64 *res_out);

PDC_error_t PDC_aint8FW_read_internal (PDC_t *pdc, PDC_base_em *em, size_t width,
				       PDC_base_ed *ed, PDC_int8 *res_out);

PDC_error_t PDC_aint16FW_read_internal(PDC_t *pdc, PDC_base_em *em, size_t width,
				       PDC_base_ed *ed, PDC_int16 *res_out);

PDC_error_t PDC_aint32FW_read_internal(PDC_t *pdc, PDC_base_em *em, size_t width,
				       PDC_base_ed *ed, PDC_int32 *res_out);

PDC_error_t PDC_aint64FW_read_internal(PDC_t *pdc, PDC_base_em *em, size_t width,
				       PDC_base_ed *ed, PDC_int64 *res_out);

PDC_error_t PDC_auint8FW_read_internal (PDC_t *pdc, PDC_base_em *em, size_t width,
					PDC_base_ed *ed, PDC_uint8 *res_out);

PDC_error_t PDC_auint16FW_read_internal(PDC_t *pdc, PDC_base_em *em, size_t width,
					PDC_base_ed *ed, PDC_uint16 *res_out);

PDC_error_t PDC_auint32FW_read_internal(PDC_t *pdc, PDC_base_em *em, size_t width,
					PDC_base_ed *ed, PDC_uint32 *res_out);

PDC_error_t PDC_auint64FW_read_internal(PDC_t *pdc, PDC_base_em *em, size_t width,
					PDC_base_ed *ed, PDC_uint64 *res_out);

PDC_error_t PDC_eint8_read_internal (PDC_t *pdc, PDC_base_em *em,
				     PDC_base_ed *ed, PDC_int8 *res_out);

PDC_error_t PDC_eint16_read_internal(PDC_t *pdc, PDC_base_em *em,
				     PDC_base_ed *ed, PDC_int16 *res_out);

PDC_error_t PDC_eint32_read_internal(PDC_t *pdc, PDC_base_em *em,
				     PDC_base_ed *ed, PDC_int32 *res_out);

PDC_error_t PDC_eint64_read_internal(PDC_t *pdc, PDC_base_em *em,
				     PDC_base_ed *ed, PDC_int64 *res_out);

PDC_error_t PDC_euint8_read_internal (PDC_t *pdc, PDC_base_em *em,
				      PDC_base_ed *ed, PDC_uint8 *res_out);

PDC_error_t PDC_euint16_read_internal(PDC_t *pdc, PDC_base_em *em,
				      PDC_base_ed *ed, PDC_uint16 *res_out);

PDC_error_t PDC_euint32_read_internal(PDC_t *pdc, PDC_base_em *em,
				      PDC_base_ed *ed, PDC_uint32 *res_out);

PDC_error_t PDC_euint64_read_internal(PDC_t *pdc, PDC_base_em *em,
				      PDC_base_ed *ed, PDC_uint64 *res_out);

PDC_error_t PDC_eint8FW_read_internal (PDC_t *pdc, PDC_base_em *em, size_t width,
				       PDC_base_ed *ed, PDC_int8 *res_out);

PDC_error_t PDC_eint16FW_read_internal(PDC_t *pdc, PDC_base_em *em, size_t width,
				       PDC_base_ed *ed, PDC_int16 *res_out);

PDC_error_t PDC_eint32FW_read_internal(PDC_t *pdc, PDC_base_em *em, size_t width,
				       PDC_base_ed *ed, PDC_int32 *res_out);

PDC_error_t PDC_eint64FW_read_internal(PDC_t *pdc, PDC_base_em *em, size_t width,
				       PDC_base_ed *ed, PDC_int64 *res_out);

PDC_error_t PDC_euint8FW_read_internal (PDC_t *pdc, PDC_base_em *em, size_t width,
					PDC_base_ed *ed, PDC_uint8 *res_out);

PDC_error_t PDC_euint16FW_read_internal(PDC_t *pdc, PDC_base_em *em, size_t width,
					PDC_base_ed *ed, PDC_uint16 *res_out);

PDC_error_t PDC_euint32FW_read_internal(PDC_t *pdc, PDC_base_em *em, size_t width,
					PDC_base_ed *ed, PDC_uint32 *res_out);

PDC_error_t PDC_euint64FW_read_internal(PDC_t *pdc, PDC_base_em *em, size_t width,
					PDC_base_ed *ed, PDC_uint64 *res_out);

PDC_error_t PDC_bint8_read_internal (PDC_t *pdc, PDC_base_em *em,
				     PDC_base_ed *ed, PDC_int8 *res_out);

PDC_error_t PDC_bint16_read_internal(PDC_t *pdc, PDC_base_em *em,
				     PDC_base_ed *ed, PDC_int16 *res_out);

PDC_error_t PDC_bint32_read_internal(PDC_t *pdc, PDC_base_em *em,
				     PDC_base_ed *ed, PDC_int32 *res_out);

PDC_error_t PDC_bint64_read_internal(PDC_t *pdc, PDC_base_em *em,
				     PDC_base_ed *ed, PDC_int64 *res_out);

PDC_error_t PDC_buint8_read_internal (PDC_t *pdc, PDC_base_em *em,
				      PDC_base_ed *ed, PDC_uint8 *res_out);

PDC_error_t PDC_buint16_read_internal(PDC_t *pdc, PDC_base_em *em,
				      PDC_base_ed *ed, PDC_uint16 *res_out);

PDC_error_t PDC_buint32_read_internal(PDC_t *pdc, PDC_base_em *em,
				      PDC_base_ed *ed, PDC_uint32 *res_out);

PDC_error_t PDC_buint64_read_internal(PDC_t *pdc, PDC_base_em *em,
				      PDC_base_ed *ed, PDC_uint64 *res_out);

/* ================================================================================ */ 
/* INTERNAL VERSIONS OF ACCUM REPORTING FUNCTIONS */

/* These functions take an argument, outstr, for 
 * the output target, and do not check
 * the pdc, prefix, or accumulator arguments for NULL values.
 */

PDC_error_t PDC_int8_acc_report_internal   (PDC_t *pdc, Sfio_t *outstr, const char *prefix, const char *what,
					    int nst, PDC_int8_acc *a);
PDC_error_t PDC_int16_acc_report_internal  (PDC_t *pdc, Sfio_t *outstr, const char *prefix, const char *what,
					    int nst, PDC_int16_acc *a);
PDC_error_t PDC_int32_acc_report_internal  (PDC_t *pdc, Sfio_t *outstr, const char *prefix, const char *what,
					    int nst, PDC_int32_acc *a);
PDC_error_t PDC_int64_acc_report_internal  (PDC_t *pdc, Sfio_t *outstr, const char *prefix, const char *what,
					    int nst, PDC_int64_acc *a);
PDC_error_t PDC_uint8_acc_report_internal  (PDC_t *pdc, Sfio_t *outstr, const char *prefix, const char *what,
					    int nst, PDC_uint8_acc *a);
PDC_error_t PDC_uint16_acc_report_internal (PDC_t *pdc, Sfio_t *outstr, const char *prefix, const char *what,
					    int nst, PDC_uint16_acc *a);
PDC_error_t PDC_uint32_acc_report_internal (PDC_t *pdc, Sfio_t *outstr, const char *prefix, const char *what,
					    int nst, PDC_uint32_acc *a);
PDC_error_t PDC_uint64_acc_report_internal (PDC_t *pdc, Sfio_t *outstr, const char *prefix, const char *what,
					    int nst, PDC_uint64_acc *a);

PDC_error_t PDC_int32_acc_report_map_internal(PDC_t *pdc, Sfio_t *outstr, const char *prefix, const char *what,
					      int nst, PDC_int32_map_fn  fn, PDC_int32_acc *a);

PDC_error_t PDC_string_acc_report_internal (PDC_t *pdc, Sfio_t *outstr, const char *prefix, const char *what,
					    int nst, PDC_string_acc *a);
PDC_error_t PDC_char_acc_report_internal   (PDC_t *pdc, Sfio_t *outstr, const char *prefix, const char *what,
					    int nst, PDC_char_acc *a);

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
 * The <format, ...> args are for a printf-style description that augments
 * the default description based on errCode. 
 *
 * N.B. This call does nothing if either there is no disc error function
 *      or if the disc e_rep is PDC_errorRep_None
 */

PDC_error_t PDCI_report_err(PDC_t *pdc, int level, PDC_loc_t *loc,
			    PDC_errCode_t errCode, const char *format, ... );

/* ================================================================================ */
/* PURELY INTERNAL IO FUNCTIONS */

/* 
 * Note: all of the following act on the IO cursor of the top checkpoint
 *
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

PDC_error_t  PDCI_IO_needbytes (PDC_t *pdc,
				char **b_out, char **p1_out, char **p2_out, char **e_out,
			        int *eor_out, int *eof_out, size_t *bytes_out);
PDC_error_t  PDCI_IO_morebytes (PDC_t *pdc, char **b_out, char **p1_out, char **p2_out, char **e_out,
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
/* INTERNAL EBCDIC ROUTINES */

int is_e_digit(unsigned char c);
int is_e_space(unsigned char c);

long PDCI_estrtol(const char *str, char **ptr, int base);
long long PDCI_estrtoll(const char *str, char **ptr, int base);
unsigned long PDCI_estrtoul(const char *str, char **ptr, int base);
unsigned long long PDCI_estrtoull(const char *str, char **ptr, int base);

/* ================================================================================ */
/* INTERNAL MODIFIED CONVERSION ROUTINES */

/*
 * Wrappers for conversion routines that set errno to zero before
 * making the real call.  The unsigned wrappers also check for
 * "-<digits>" pattern and produce range error, rather than relying
 * on the real call to do the right thing.
 */
long PDCI_stringtol (const char *, char **, int);
long long PDCI_stringtoll(const char *, char **, int);
unsigned long PDCI_stringtoul (const char *, char **, int);
unsigned long long PDCI_stringtoull(const char *, char **, int);

/* ================================================================================ */
/* INTERNAL MISC ROUTINES */

/*  PDCI_regexpMatch returns the number of characters in str that match regexp
 *  (or 0 if str does not match the regular expression).  If ebcdic is non-zero, the
 *  chars between begin and end are EBCDIC chars, otherwise they are ASCII chars.
 */

size_t PDCI_regexpMatch(PDC_t *pdc, PDC_regexp_t *regexp, char *begin, char *end, int ebcdic);

/* Accum impl helpers:
 *
 * PDCI_nst_prefix_what prints a heading to outstr 
 * based on *nst nesting level and
 * (unless *nst is -1) it increments the nesting level.
 */

void PDCI_nst_prefix_what(Sfio_t *outstr, int *nst, const char *prefix, const char *what);

/* ================================================================================ */

#endif /*  __LIBPADSC_INTERNAL__  */
