#pragma prototyped
/*
 * padc library interface
 * 
 * Kathleen Fisher, Robert Gruber
 * AT&T Labs Research
 */

#ifndef __LIBPADSC_INTERNAL_H__
#define __LIBPADSC_INTERNAL_H__

#ifdef __PREPROCESSOR_FIXES
typedef void * __builtin_va_list;
#define __THROW
/* extern int ftruncate (int __fd, long int __length) ; */

#endif

#include "libpadsc.h"
#include "pdc_io_disc.h"
#include "pdc_out_macros.h"

/* ================================================================================ */
/* INTERNAL VERSIONS OF EXTERNAL IO FUNCTIONS */

PDC_error_t  PDC_IO_fopen_internal    (PDC_t *pdc, char *path, PDC_disc_t *disc);
PDC_error_t  PDC_IO_fclose_internal   (PDC_t *pdc, PDC_disc_t *disc);
PDC_error_t  PDC_IO_next_rec_internal (PDC_t *pdc, size_t *skipped_bytes_out, PDC_disc_t *disc);

int          PDC_IO_at_EOR_internal   (PDC_t *pdc, PDC_disc_t *disc);
int          PDC_IO_at_EOF_internal   (PDC_t *pdc, PDC_disc_t *disc);

PDC_error_t  PDC_IO_getPos_internal   (PDC_t *pdc, PDC_pos_t *pos, int offset, PDC_disc_t *disc); 

/* ================================================================================ */ 
/* INTERNAL VERSIONS OF ALL BASE TYPE READ FUNCTIONS */

PDC_error_t PDC_char_lit_read_internal(PDC_t *pdc, PDC_base_em *em,
				       PDC_base_ed *ed, unsigned char c, PDC_disc_t *disc);

PDC_error_t PDC_str_lit_read_internal(PDC_t *pdc, PDC_base_em *em,
				      PDC_base_ed *ed, const PDC_string *s, PDC_disc_t *disc);

PDC_error_t PDC_countXtoY_internal(PDC_t *pdc, PDC_base_em *em, PDC_uint8 x, PDC_uint8 y,
				   PDC_base_ed *ed, PDC_int32 *res_out, PDC_disc_t *disc);

PDC_error_t PDC_adate_read_internal(PDC_t *pdc, PDC_base_em *em, PDC_base_ed *ed, 
				    PDC_uint32 *res_out, PDC_disc_t *disc);

PDC_error_t PDC_string_fw_read_internal(PDC_t *pdc, PDC_base_em *em, size_t width,
					PDC_base_ed *ed, PDC_string *s_out, PDC_disc_t *disc);

PDC_error_t PDC_string_stopChar_read_internal(PDC_t *pdc, PDC_base_em *em, unsigned char stopChar,
					      PDC_base_ed *ed, PDC_string *s_out, PDC_disc_t *disc);

PDC_error_t PDC_string_stopRegexp_read_internal(PDC_t *pdc, PDC_base_em *em, PDC_regexp_t *stopRegexp,
						PDC_base_ed *ed, PDC_string *s_out, PDC_disc_t *disc);

PDC_error_t PDC_aint8_read_internal (PDC_t *pdc, PDC_base_em *em,
				     PDC_base_ed *ed, PDC_int8 *res_out, PDC_disc_t *disc);

PDC_error_t PDC_aint16_read_internal(PDC_t *pdc, PDC_base_em *em,
				     PDC_base_ed *ed, PDC_int16 *res_out, PDC_disc_t *disc);

PDC_error_t PDC_aint32_read_internal(PDC_t *pdc, PDC_base_em *em,
				     PDC_base_ed *ed, PDC_int32 *res_out, PDC_disc_t *disc);

PDC_error_t PDC_aint64_read_internal(PDC_t *pdc, PDC_base_em *em,
				     PDC_base_ed *ed, PDC_int64 *res_out, PDC_disc_t *disc);

PDC_error_t PDC_auint8_read_internal (PDC_t *pdc, PDC_base_em *em,
				      PDC_base_ed *ed, PDC_uint8 *res_out, PDC_disc_t *disc);

PDC_error_t PDC_auint16_read_internal(PDC_t *pdc, PDC_base_em *em,
				      PDC_base_ed *ed, PDC_uint16 *res_out, PDC_disc_t *disc);

PDC_error_t PDC_auint32_read_internal(PDC_t *pdc, PDC_base_em *em,
				      PDC_base_ed *ed, PDC_uint32 *res_out, PDC_disc_t *disc);

PDC_error_t PDC_auint64_read_internal(PDC_t *pdc, PDC_base_em *em,
				      PDC_base_ed *ed, PDC_uint64 *res_out, PDC_disc_t *disc);

PDC_error_t PDC_aint8_fw_read_internal (PDC_t *pdc, PDC_base_em *em, size_t width,
					PDC_base_ed *ed, PDC_int8 *res_out, PDC_disc_t *disc);

PDC_error_t PDC_aint16_fw_read_internal(PDC_t *pdc, PDC_base_em *em, size_t width,
					PDC_base_ed *ed, PDC_int16 *res_out, PDC_disc_t *disc);

PDC_error_t PDC_aint32_fw_read_internal(PDC_t *pdc, PDC_base_em *em, size_t width,
					PDC_base_ed *ed, PDC_int32 *res_out, PDC_disc_t *disc);

PDC_error_t PDC_aint64_fw_read_internal(PDC_t *pdc, PDC_base_em *em, size_t width,
					PDC_base_ed *ed, PDC_int64 *res_out, PDC_disc_t *disc);


PDC_error_t PDC_auint8_fw_read_internal (PDC_t *pdc, PDC_base_em *em, size_t width,
					 PDC_base_ed *ed, PDC_uint8 *res_out, PDC_disc_t *disc);

PDC_error_t PDC_auint16_fw_read_internal(PDC_t *pdc, PDC_base_em *em, size_t width,
					 PDC_base_ed *ed, PDC_uint16 *res_out, PDC_disc_t *disc);

PDC_error_t PDC_auint32_fw_read_internal(PDC_t *pdc, PDC_base_em *em, size_t width,
					 PDC_base_ed *ed, PDC_uint32 *res_out, PDC_disc_t *disc);

PDC_error_t PDC_auint64_fw_read_internal(PDC_t *pdc, PDC_base_em *em, size_t width,
					 PDC_base_ed *ed, PDC_uint64 *res_out, PDC_disc_t *disc);


PDC_error_t PDC_bint8_read_internal (PDC_t *pdc, PDC_base_em *em,
				     PDC_base_ed *ed, PDC_int8 *res_out, PDC_disc_t *disc);

PDC_error_t PDC_bint16_read_internal(PDC_t *pdc, PDC_base_em *em,
				     PDC_base_ed *ed, PDC_int16 *res_out, PDC_disc_t *disc);

PDC_error_t PDC_bint32_read_internal(PDC_t *pdc, PDC_base_em *em,
				     PDC_base_ed *ed, PDC_int32 *res_out, PDC_disc_t *disc);

PDC_error_t PDC_bint64_read_internal(PDC_t *pdc, PDC_base_em *em,
				     PDC_base_ed *ed, PDC_int64 *res_out, PDC_disc_t *disc);

PDC_error_t PDC_buint8_read_internal (PDC_t *pdc, PDC_base_em *em,
				      PDC_base_ed *ed, PDC_uint8 *res_out, PDC_disc_t *disc);

PDC_error_t PDC_buint16_read_internal(PDC_t *pdc, PDC_base_em *em,
				      PDC_base_ed *ed, PDC_uint16 *res_out, PDC_disc_t *disc);

PDC_error_t PDC_buint32_read_internal(PDC_t *pdc, PDC_base_em *em,
				      PDC_base_ed *ed, PDC_uint32 *res_out, PDC_disc_t *disc);

PDC_error_t PDC_buint64_read_internal(PDC_t *pdc, PDC_base_em *em,
				      PDC_base_ed *ed, PDC_uint64 *res_out, PDC_disc_t *disc);

/* ================================================================================ */ 
/* INTERNAL VERSIONS OF ACCUM REPORTING FUNCTIONS */

/* These functions take an argument, outstr, for 
 * the output target, and do not check
 * the pdc, prefix, or accumulator arguments for NULL values.
 */

PDC_error_t PDC_int8_acc_report_internal   (PDC_t *pdc, Sfio_t *outstr, const char *prefix, const char *what,
					    int nst, PDC_int8_acc *a, PDC_disc_t *disc);
PDC_error_t PDC_int16_acc_report_internal  (PDC_t *pdc, Sfio_t *outstr, const char *prefix, const char *what,
					    int nst, PDC_int16_acc *a, PDC_disc_t *disc);
PDC_error_t PDC_int32_acc_report_internal  (PDC_t *pdc, Sfio_t *outstr, const char *prefix, const char *what,
					    int nst, PDC_int32_acc *a, PDC_disc_t *disc);
PDC_error_t PDC_int64_acc_report_internal  (PDC_t *pdc, Sfio_t *outstr, const char *prefix, const char *what,
					    int nst, PDC_int64_acc *a, PDC_disc_t *disc);
PDC_error_t PDC_uint8_acc_report_internal  (PDC_t *pdc, Sfio_t *outstr, const char *prefix, const char *what,
					    int nst, PDC_uint8_acc *a, PDC_disc_t *disc);
PDC_error_t PDC_uint16_acc_report_internal (PDC_t *pdc, Sfio_t *outstr, const char *prefix, const char *what,
					    int nst, PDC_uint16_acc *a, PDC_disc_t *disc);
PDC_error_t PDC_uint32_acc_report_internal (PDC_t *pdc, Sfio_t *outstr, const char *prefix, const char *what,
					    int nst, PDC_uint32_acc *a, PDC_disc_t *disc);
PDC_error_t PDC_uint64_acc_report_internal (PDC_t *pdc, Sfio_t *outstr, const char *prefix, const char *what,
					    int nst, PDC_uint64_acc *a, PDC_disc_t *disc);

PDC_error_t PDC_int32_acc_report_map_internal(PDC_t *pdc, Sfio_t *outstr, const char *prefix, const char *what,
					      int nst, PDC_int32_map_fn  fn, PDC_int32_acc *a, PDC_disc_t *disc);

PDC_error_t PDC_string_acc_report_internal (PDC_t *pdc, Sfio_t *outstr, const char *prefix, const char *what,
					    int nst, PDC_string_acc *a, PDC_disc_t *disc);
PDC_error_t PDC_char_acc_report_internal   (PDC_t *pdc, Sfio_t *outstr, const char *prefix, const char *what,
					    int nst, PDC_char_acc *a, PDC_disc_t *disc);

/* ********************************************************************************
 * Remainder of this file contains function decls for functions and types
 * purely internal to the library impl.  Note the use of the PDCI prefix
 * for these functions 
 * ********************************************************************************/

/* ================================================================================ */
/* INTERNAL TYPE DEFINITIONS */

/* PDCI_stkElt_t: A stack elt has a cursor position cur, which is a
 * pointer to a PDC_IO_elt plus the number of byte remaining.  We also
 * record the spec flag passed to PDC_IO_checkpoint, to enable proper
 * de-bumping of pdc->speclev.
 */
typedef struct PDCI_stkElt_s {
  PDC_IO_elt_t  *elt;
  size_t        remain;  /* bytes remaining in current IO elt; determines cursor position */
  int           spec;    /* the spec flag passed to checkpoint */
} PDCI_stkElt_t;

struct PDC_s {
  const char        *id;       /* interface id */
  PDC_disc_t        *disc;     /* user-supplied disc (can be null) */
  Vmalloc_t         *vm;       /* vm handle */
  Sfio_t            *tmp;      /* tmp sfprintf area */
  RMM_t             *rmm_z;    /* rbuf memory mgr -- zeroes allocated memory */ 
  RMM_t             *rmm_nz;   /* rbuf memory mgr -- does not zero allocated memory */ 
  /* The following are all related to IO state / checkpointing */
  char              *path;     /* original path -- eventually want to support a set of input files */
  Sfio_t            *io;       /* sfio stream */
  char              *sfbuf;    /* buffer that is installed in any sfio that is opened */
  PDC_IO_elt_t      *head;     /* head of list of input elts */
  PDCI_stkElt_t     *stack;    /* stack - resized dynamically */
  size_t            salloc;    /* total elts allocated for stack */
  size_t            top;       /* index of top stack elt */
  unsigned int      speclev;   /* speculative nesting level */
  char              dummy[1];  /* used for error case */
};

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

PDC_error_t PDCI_report_err(PDC_t *pdc, PDC_disc_t *disc, int level, PDC_loc_t *loc,
			    PDC_errCode_t errCode, const char *format, ... );

/* ================================================================================ */
/* INTERNAL SCAN FUNCTIONS */

/* PDCI_char_lit_scan:
 *
 * EFFECT:  scan for either goal character c or stop character s,
 *          move IO cursor just beyond.  disc controls maximum
 *          scan distance.  Hitting eor or eof considered to be an error.
 *          N.B. If there is mixed binary and ascii data, scanning
 *          can 'find' an ascii char in a binary field.  Be careful!
 *          Do not use 0 to mean EOF.  If there is no stop char,
 *          use the same char for both the c and s params.
 *
 * RETURNS: PDC_error_t
 *         PDC_OK    => goal/stop char found, IO cursor now points just beyond char
 *                      if c_out, *c_out set to the char that was found
 *                      if offset_out, *offset_out set to the distance scanned to find that char
 *                      (0 means the IO cursor was pointing at the found char)
 *         PDC_ERR   => char not found, IO cursor unchanged
 * 
 * PDCI_string_lit_scan: same as char_lit_scan execpt a goal string and
 * stop string are given.  In this case, if there is no stop string, a
 * NULL stop string should be used.  On PDC_OK, *str_out points to
 * either findStr or stopStr (depending on which was found), *offset_out
 * is the distance scanned to find the string, and the IO cursor
 * points just beyond the string that was found.  On PDC_ERR, the
 * IO cursor is unchanged.
 */

PDC_error_t PDCI_char_lit_scan(PDC_t *pdc, unsigned char c, unsigned char s, 
			       unsigned char *c_out, size_t *offset_out,
			       PDC_disc_t *disc);

PDC_error_t PDCI_str_lit_scan(PDC_t *pdc, const PDC_string *findStr, const PDC_string *stopStr,
			      PDC_string **str_out, size_t *offset_out, PDC_disc_t *disc);

/* ================================================================================ */
/* PURELY INTERNAL IO FUNCTIONS */

/*
 * The checkpoint API: if any of these return PDC_ERR, there is an
 * internal error -- the calling code should probably exit the program
 * as continuing could lead to unspecified behavior / crash.
 *
 * If a non-zero speculative flag is passed to checkpoint, then the
 * speculative nesting level  is incremented by one.  Once the checkpoint
 * is removed by either commit or restore, the nesting level is
 * decremented by one.  PDC_spec_level gives the current nesting level.
 */
PDC_error_t  PDCI_IO_checkpoint (PDC_t *pdc, int speculative, PDC_disc_t *disc);
PDC_error_t  PDCI_IO_commit     (PDC_t *pdc, PDC_disc_t *disc);
PDC_error_t  PDCI_IO_restore    (PDC_t *pdc, PDC_disc_t *disc);
unsigned int PDCI_spec_level    (PDC_t *pdc, PDC_disc_t *disc);

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

PDC_error_t  PDCI_IO_needbytes (PDC_t *pdc, char **b_out, char **p1_out, char **p2_out, char **e_out,
			        int *eor_out, int *eof_out, size_t *bytes_out, PDC_disc_t *disc);
PDC_error_t  PDCI_IO_morebytes (PDC_t *pdc, char **b_out, char **p1_out, char **p2_out, char **e_out,
				int *eor_out, int *eof_out, size_t *bytes_out, PDC_disc_t *disc);
PDC_error_t  PDCI_IO_forward   (PDC_t *pdc, size_t num_bytes, PDC_disc_t *disc);

/*
 * Other IO routines:
 *    PDCI_IO_getElt: if the specified elt is currently in an in-memory buffer,
 *                    sets (*elt_out) to point to elt and returns PDC_OK,
 *                    otherwise returns PDC_ERR.
 */

PDC_error_t PDCI_IO_getElt(PDC_t *pdc, size_t num, PDC_IO_elt_t **elt_out, PDC_disc_t *disc);

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
/*
 *    PDCI_fmtChar: produce a ptr to a string that is a pretty-print (escaped) formated for char c
 *        N.B. Resulting string should be printed immediately then not used again, e.g.,
 *        PDC_report_err( .. .. , "Missing separator: %s", PDC_fmtChar(010)); 
 * 
 *    PDCI_fmtStr   : same thing for a PDC_string
 *    PDCI_fmtStrL  : same thing for a char * string / length
 *    PDCI_fmtQChar/PDC_fmtQStr/PDC_fmtQStrL : adds quote marks
 */
char *PDCI_fmtChar(char c);
char *PDCI_fmtStr(const PDC_string *s);
char *PDCI_fmtStrL(const char *s, size_t len);
char *PDCI_fmtQChar(char c);
char *PDCI_fmtQStr(const PDC_string *s);
char *PDCI_fmtQStrL(const char *s, size_t len);

/*  PDCI_regexpMatch returns the number of characters in str that match regexp
 *  (or 0 if str does not match the regular expression).
 */

size_t PDCI_regexpMatch(PDC_t *pdc, PDC_regexp_t *regexp, char *begin, char *end, PDC_disc_t *disc);

/* Accum impl helpers:
 *
 * PDCI_nst_prefix_what prints a heading to outstr 
 * based on *nst nesting level and
 * (unless *nst is -1) it increments the nesting level.
 */

void PDCI_nst_prefix_what(Sfio_t *outstr, int *nst, const char *prefix, const char *what);

/* ================================================================================ */

#endif /*  __LIBPADSC_INTERNAL_H__  */
