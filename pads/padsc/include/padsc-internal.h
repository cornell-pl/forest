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
typedef void* __builtin_va_list;
#define __THROW
/* extern int ftruncate (int __fd, long int __length) ; */

#endif

#include <ast.h>
#include <vmalloc.h>
#include <sfstr.h>
#include <error.h>
#include "libpadsc.h"
#include "rbuf.h"

/* ================================================================================ */
/* TYPE DEFINITIONS */

/*
 * used for list of input lines, one per line retained in memory
 * the input lines are pdc->iline[0] .. pdc->iline[pdc->itail].
 * For the tail line, boff,eoff are offsets into sfio-managed space
 * (base = pdc->sfbuf); otherwise they are offsets into the
 * pdc->buf shadow space 
 */
typedef struct PDC_IO_line_s {
  size_t  lnum;    /* line # */
  size_t  boffset; /* offset of first char in line */
  size_t  eoffset; /* offset 1 beyond last char in line */
} PDC_IO_line_t;

/*
 * a stack elt has a cursor position cur, which is an
 * offset within IO line ilines[idx] -- cur should be
 * between ilines[idx].boff and ilines[idx].eoff
 */
typedef struct PDC_stkElt_s {
  size_t      idx;   /* index of IO line */
  size_t      cur;   /* cursor position */
} PDC_stkElt_t;

struct PDC_s {
  const char*       id;      /* interface id */
  PDC_disc_t*       disc;    /* user-supplied discipline (can be null) */
  Vmalloc_t*        vm;      /* vm handle */
  Sfio_t*           tmp;     /* tmp sfprintf area */
  RMM_t*            rmm_z;   /* rbuf memory mgr -- zeroes allocated memory */ 
  RMM_t*            rmm_nz;  /* rbuf memory mgr -- does not zero allocated memory */ 
  /* The following are all IO state */
  Sfio_t*           io;      /* io stream */
  int               eof;     /* found eof? */ 
  size_t            lnum;    /* last line # read */ 
  char*             path;    /* original path -- eventually want to support a set of input files */
  char*             sfbuf;   /* sfio buffer ptr returned from sfgetr */
  PDC_IO_line_t*    ilines;  /* list of input lines in memory - resized dynamically */
  size_t            ialloc;  /* total lines allocated for ilines */
  size_t            itail;   /* indx of tail of iline list */
  PDC_stkElt_t*     stack;   /* stack - resized dynamically */
  size_t            salloc;  /* total elts allocated for stack */
  size_t            top;     /* index of top stack elt */
  char*             buf;     /* shadow buffer space - resized dynamically */
  size_t            balloc;  /* total chars allocated for buf */
  size_t            bchars;  /* total chars copied into buf so far */
};

/* ================================================================================ */
/* RMM FUNCTIONS */

RMM_t* PDC_rmm_zero  (PDC_t* pdc, PDC_disc_t* disc);  /* get rbuf memory mgr that zeroes allocated memory */
RMM_t* PDC_rmm_nozero(PDC_t* pdc, PDC_disc_t* disc);  /* get rbuf memory mgr that does not zero allocated memory */

/* ================================================================================ */
/* INTERNAL IO FUNCTIONS */

/*
 * The checkpoint API: if any of these return PDC_ERROR, there is an
 * internal error -- the calling code should probably exit the program
 * as continuing could lead to unspecified behavior / crash.
 */
PDC_error_t  PDC_IO_checkpoint  (PDC_t* pdc, PDC_disc_t* disc);
PDC_error_t  PDC_IO_commit      (PDC_t* pdc, PDC_disc_t* disc);
PDC_error_t  PDC_IO_restore     (PDC_t* pdc, PDC_disc_t* disc);

/* 
 * Note: all of the following act on the IO cursor of the top checkpoint
 */
PDC_error_t  PDC_get_loc        (PDC_t* pdc, PDC_loc_t* l, PDC_disc_t* disc); /* sets l begin/end pos to current IO pos */
PDC_error_t  PDC_get_loc2end    (PDC_t* pdc, PDC_loc_t* l, PDC_disc_t* disc); /* sets l begin pos to current IO pos and
										 l end pos to end of line pos */
PDC_error_t  PDC_get_beginLoc   (PDC_t* pdc, PDC_loc_t* l, PDC_disc_t* disc); /* sets l begin pos to current IO pos */
PDC_error_t  PDC_get_endLoc     (PDC_t* pdc, PDC_loc_t* l, PDC_disc_t* disc); /* sets l end pos to current IO pos */
int          PDC_IO_is_EOF      (PDC_t* pdc, PDC_disc_t* disc);
int          PDC_IO_peek_EOF    (PDC_t* pdc, PDC_disc_t* disc);
PDC_error_t  PDC_IO_getchar     (PDC_t* pdc, unsigned char* ct, int obeyPanicStop, PDC_disc_t* disc);
PDC_error_t  PDC_IO_getchars    (PDC_t* pdc, size_t num_chars, char** b_out, char** e_out, PDC_disc_t* disc);
PDC_error_t  PDC_IO_back        (PDC_t* pdc, size_t num_chars, PDC_disc_t* disc);
PDC_error_t  PDC_IO_refill      (PDC_t* pdc, PDC_disc_t* disc);

/*
 * Other IO routines:
 *    PDC_IO_getLineBuf: if the specified line is currently in an in-memory buffer, sets
 *                          + *buf_out to point to first char of line in that buffer
 *                          + *len_out to the length of the line (including newline)
 *                       and returns PDC_OK, otherwise returns PDC_ERROR.
 *    PDC_IO_initialize : initialize IO state to 'nothing read/no checkpoints'
 */

PDC_error_t PDC_IO_getLineBuf(PDC_t* pdc, size_t line, char** buf_out, size_t* len_out, PDC_disc_t* disc);
PDC_error_t PDC_IO_initialize(PDC_t* pdc, PDC_disc_t* disc);

/* ================================================================================ */ 
/* INTERNAL ERROR REPORTING FUNCTIONS */

/*
 * PDC_report_err: Report a parse error that occurred at location loc;
 *
 * level should be one of:
 *      -K : negative # is used for debugging messages
 *       ERROR_INFO  : informative, no prefixes appended to message
 *       ERROR_ERROR : warning message, program library name is added as prefix
 *       ERROR_FATAL : fatal error, program should exit 
 * One can or in the following flags (as in ERROR_INFO|ERROR_PROMPT):
 *       ERROR_PROMPT : do not emit a newline
 *  
 *   XXX errCode's type should be an enum that describes the kind of error XXX ???
 *
 * The <format, ...> args are for a printf-style description that augments
 * the default description based on errCode. 
 *
 * N.B. This call does nothing if either there is no discipline error function
 *      or if the discipline e_rep is PDC_errorRep_None
 */

PDC_error_t PDC_report_err(PDC_t* pdc, PDC_disc_t* disc, int level, PDC_loc_t* loc,
			   PDC_errCode_t errCode, const char* format, ... );

/*
 *  PDC_errorf: default discipline error reporting routine
 */
int           PDC_errorf(PDC_t* pdc, PDC_disc_t* disc, int level, ...);

/* ================================================================================ */
/* SCAN FUNCTIONS */

/* PDC_char_lit_scan:
 *
 * EFFECT:  scan for either goal character c or stop character s,
 *          move IO cursor just beyond.  Discipline controls maximum
 *          scan distance.  Hitting eof considered to be an error.
 *          N.B. If there is mixed binary and ascii data, scanning
 *          can 'find' an ascii char in a binary field.  Be careful!
 *          Do not use 0 to mean EOF.  If there is no stop char,
 *          use the same char for both the c and s params.
 *
 * RETURNS: PDC_error_t
 *             OK    => goal/stop char found, IO cursor now points just beyond char
 *                      if c_out, *c_out set to the char that was found
 *                      if offset_out, *offset_out set to the distance scanned to find that char
 *                      (0 means the IO cursor was pointing at the found char)
 *             ERROR => char not found, IO cursor unchanged
 */

PDC_error_t PDC_char_lit_scan(PDC_t* pdc, unsigned char c, unsigned char s, 
			      unsigned char* c_out, size_t* offset_out,
			      PDC_disc_t* disc);

PDC_error_t PDC_str_lit_scan(PDC_t* pdc, const PDC_string* findStr, const PDC_string* stopStr,
			     PDC_string** str_out, size_t* offset_out, PDC_disc_t* disc);

/* ================================================================================ */
/* READ FUNCTIONS */

/* PDC_char_lit_read:
 *
 * EFFECT: verify IO cursor points to specified char, move IO cursor just beyond
 *
 * RETURNS: PDC_error_t
 *            OK    => IO cursor did point to char, now points just beyond char
 *            ERROR => IO cursor does not point to char (cursor unchanged)
 */

PDC_error_t PDC_char_lit_read(PDC_t* pdc, PDC_base_em* em,
			      PDC_base_ed* ed, unsigned char c, PDC_disc_t* disc);

PDC_error_t PDC_str_lit_read(PDC_t* pdc, PDC_base_em* em,
			     PDC_base_ed* ed, const PDC_string* s, PDC_disc_t* disc);

/* ================================================================================ */
/* MISC ROUTINES */
/*
 *    PDC_fmtChar: produce a ptr to a string that is a pretty-print (escaped) formated for char c
 *        N.B. Resulting string should be printed immediately then not used again, e.g.,
 *        PDC_report_err( ..xxx.. , "Missing separator: %s", PDC_fmtChar(010)); 
 * 
 *    PDC_fmtStr  : same thing for a PDC_string
 *    PDC_fmtStrL : same thing for a char* string / length
 */
char*       PDC_fmtChar(char c);
char*       PDC_fmtStr(const PDC_string* s);
char*       PDC_fmtStrL(const char* s, size_t len);

/* ================================================================================ */
/* OUTPUT MACROS  */

#define PDC_DBG_FLAGS      -2
#define PDC_TRACE_FLAGS    -4
#define PDC_WARN_FLAGS      2
#define PDC_SYSERR_FLAGS    ERROR_SYSTEM|2

#define NULL_STMT do { } while (0)

#ifndef NDEBUG
#  define DBG(pdc, format)                                   FMT_ERR_MSG (pdc, format, PDC_DBG_FLAGS)
#  define DBG1(pdc, format, arg1)                            FMT_ERR_MSG1(pdc, format, PDC_DBG_FLAGS, arg1)
#  define DBG2(pdc, format, arg1, arg2)                      FMT_ERR_MSG2(pdc, format, PDC_DBG_FLAGS, arg1, arg2)
#  define DBG3(pdc, format, arg1, arg2, arg3)                FMT_ERR_MSG3(pdc, format, PDC_DBG_FLAGS, arg1, arg2, arg3)
#  define DBG4(pdc, format, arg1, arg2, arg3, arg4)          FMT_ERR_MSG4(pdc, format, PDC_DBG_FLAGS, arg1, arg2, arg3, arg4)
#  define DBG5(pdc, format, arg1, arg2, arg3, arg4, arg5)    FMT_ERR_MSG5(pdc, format, PDC_DBG_FLAGS, arg1, arg2, arg3, arg4, arg5)

#  define TRACE(pdc, format)                                 FMT_ERR_MSG (pdc, format, PDC_TRACE_FLAGS)
#  define TRACE1(pdc, format, arg1)                          FMT_ERR_MSG1(pdc, format, PDC_TRACE_FLAGS, arg1)
#  define TRACE2(pdc, format, arg1, arg2)                    FMT_ERR_MSG2(pdc, format, PDC_TRACE_FLAGS, arg1, arg2)
#  define TRACE3(pdc, format, arg1, arg2, arg3)              FMT_ERR_MSG3(pdc, format, PDC_TRACE_FLAGS, arg1, arg2, arg3)
#  define TRACE4(pdc, format, arg1, arg2, arg3, arg4)        FMT_ERR_MSG4(pdc, format, PDC_TRACE_FLAGS, arg1, arg2, arg3, arg4)
#  define TRACE5(pdc, format, arg1, arg2, arg3, arg4, arg5)  FMT_ERR_MSG5(pdc, format, PDC_TRACE_FLAGS, arg1, arg2, arg3, arg4, arg5)
#else
#  define DBG(pdc, format)                                   NULL_STMT
#  define DBG1(pdc, format, arg1)                            NULL_STMT
#  define DBG2(pdc, format, arg1, arg2)                      NULL_STMT
#  define DBG3(pdc, format, arg1, arg2, arg3)                NULL_STMT
#  define DBG4(pdc, format, arg1, arg2, arg3, arg4)          NULL_STMT
#  define DBG5(pdc, format, arg1, arg2, arg3, arg4, arg5)    NULL_STMT

#  define TRACE(pdc, format)                                 NULL_STMT
#  define TRACE1(pdc, format, arg1)                          NULL_STMT
#  define TRACE2(pdc, format, arg1, arg2)                    NULL_STMT
#  define TRACE3(pdc, format, arg1, arg2, arg3)              NULL_STMT
#  define TRACE4(pdc, format, arg1, arg2, arg3, arg4)        NULL_STMT
#  define TRACE5(pdc, format, arg1, arg2, arg3, arg4, arg5)  NULL_STMT
#endif

#define WARN(pdc, format)                                  FMT_ERR_MSG (pdc, format, PDC_WARN_FLAGS)
#define WARN1(pdc, format, arg1)                           FMT_ERR_MSG1(pdc, format, PDC_WARN_FLAGS, arg1)
#define WARN2(pdc, format, arg1, arg2)                     FMT_ERR_MSG2(pdc, format, PDC_WARN_FLAGS, arg1, arg2)
#define WARN3(pdc, format, arg1, arg2, arg3)               FMT_ERR_MSG3(pdc, format, PDC_WARN_FLAGS, arg1, arg2, arg3)
#define WARN4(pdc, format, arg1, arg2, arg3, arg4)         FMT_ERR_MSG4(pdc, format, PDC_WARN_FLAGS, arg1, arg2, arg3, arg4)
#define WARN5(pdc, format, arg1, arg2, arg3, arg4, arg5)   FMT_ERR_MSG5(pdc, format, PDC_WARN_FLAGS, arg1, arg2, arg3, arg4, arg5)

#define SYSERR(pdc, format)                                FMT_ERR_MSG (pdc, format, PDC_SYSERR_FLAGS)
#define SYSERR1(pdc, format, arg1)                         FMT_ERR_MSG1(pdc, format, PDC_SYSERR_FLAGS, arg1)
#define SYSERR2(pdc, format, arg1, arg2)                   FMT_ERR_MSG2(pdc, format, PDC_SYSERR_FLAGS, arg1, arg2)
#define SYSERR3(pdc, format, arg1, arg2, arg3)             FMT_ERR_MSG3(pdc, format, PDC_SYSERR_FLAGS, arg1, arg2, arg3)
#define SYSERR4(pdc, format, arg1, arg2, arg3, arg4)       FMT_ERR_MSG4(pdc, format, PDC_SYSERR_FLAGS, arg1, arg2, arg3, arg4)
#define SYSERR5(pdc, format, arg1, arg2, arg3, arg4, arg5) FMT_ERR_MSG5(pdc, format, PDC_SYSERR_FLAGS, arg1, arg2, arg3, arg4, arg5)

#define FMT_ERR_MSG(pdc, format, erlev) \
  if (disc->errorf) {(*disc->errorf)(pdc, disc, erlev, format);}
#define FMT_ERR_MSG1(pdc, format, erlev, arg1) \
  if (disc->errorf) {(*disc->errorf)(pdc, disc, erlev, format, arg1);}
#define FMT_ERR_MSG2(pdc, format, erlev, arg1, arg2) \
  if (disc->errorf) {(*disc->errorf)(pdc, disc, erlev, format, arg1, arg2);}
#define FMT_ERR_MSG3(pdc, format, erlev, arg1, arg2, arg3) \
  if (disc->errorf) {(*disc->errorf)(pdc, disc, erlev, format, arg1, arg2, arg3);}
#define FMT_ERR_MSG4(pdc, format, erlev, arg1, arg2, arg3, arg4) \
  if (disc->errorf) {(*disc->errorf)(pdc, disc, erlev, format, arg1, arg2, arg3, arg4);}
#define FMT_ERR_MSG5(pdc, format, erlev, arg1, arg2, arg3, arg4, arg5) \
  if (disc->errorf) {(*disc->errorf)(pdc, disc, erlev, format, arg1, arg2, arg3, arg4, arg5);}

/* ================================================================================ */

#endif /*  __LIBPADSC_INTERNAL_H__  */
