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
  const char*       id;     /* interface id */
  PDC_disc_t*       disc;   /* user-supplied discipline (can be null) */
  Vmalloc_t*        vm;     /* vm handle */
  Sfio_t*           tmp;    /* tmp sfprintf area */
  /* The following are all IO state */
  Sfio_t*           io;      /* io stream */
  int               eof;     /* hit eof? */ 
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

/*
 * PDC_rbuf_t: resizable buffer
 */
typedef struct PDC_rbuf_s {
  int               elementSize;
  int               numElements;
  int               numAllocated;
  void*             buffer;
} PDC_rbuf_t;

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
PDC_error_t  PDC_get_loc        (PDC_t* pdc, PDC_loc_t* l, PDC_disc_t* disc);
int          PDC_IO_is_EOF      (PDC_t* pdc, PDC_disc_t* disc);
int          PDC_IO_peek_EOF    (PDC_t* pdc, PDC_disc_t* disc);
PDC_error_t  PDC_IO_getchar     (PDC_t* pdc, unsigned char* ct, int panicking, PDC_disc_t* disc);
PDC_error_t  PDC_IO_back        (PDC_t* pdc, size_t num_chars, PDC_disc_t* disc);
PDC_error_t  PDC_IO_refill      (PDC_t* pdc, PDC_disc_t* disc);

/*
 * Other IO routines:
 *    PDC_IO_getLineBuf: if the specified line is currently in an in-memory buffer,
 *                       sets *buf_out to point to first char of line in that buffer
 *                       and returns PDC_OK, otherwise returns PDC_ERROR.
 */

PDC_error_t PDC_IO_getLineBuf(PDC_t* pdc, size_t line, char** buf_out, PDC_disc_t* disc);

/* ================================================================================ */
/* RBUF: RESIZABLE ALLLOC'D SPACE */

PDC_error_t   PDC_RBUF_alloc   (PDC_t* pdc, int elSize, int numElements, 
				PDC_rbuf_t** rbuf_out, void** buf_out, PDC_disc_t* disc);
PDC_error_t   PDC_RBUF_grow    (PDC_t* pdc, PDC_rbuf_t* rbuf, int numRequired, 
				void** buf_out, PDC_disc_t* disc);
PDC_error_t   PDC_RBUF_getBuf  (PDC_t* pdc, PDC_rbuf_t* rbuf, void** buf_out, PDC_disc_t* disc);
PDC_error_t   PDC_RBUF_free    (PDC_t* pdc, PDC_rbuf_t* rbuf, int dealloc_buf, void** buf_out, PDC_disc_t* disc);

PDC_error_t   PDC_freeBuf      (PDC_t* pdc, void* buf, PDC_disc_t* disc);

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

PDC_error_t PDC_report_err(PDC_t* pdc, PDC_disc_t* disc, int level, PDC_loc_t* loc, int errCode,
			   const char* format, ... );

/*
 *  PDC_errorf: default discipline error reporting routine
 */
int           PDC_errorf(PDC_t* pdc, PDC_disc_t* disc, int level, ...);

/* ================================================================================ */
/* SCAN FUNCTIONS */

/* PDC_char_lit_scan:
 *
 * EFFECT:  scan forward, move IO cursor just beyond a specified char
 *
 * RETURNS: PDC_OK if char found (IO cursor now points just beyond char)
 *
 *          PDC_ERROR if char not found (IO cursor has moved arbitrarily forward;
 *                                       exactly how far is determined by p_stop
 *                                       as found in the user-specified discipline)
 * 
 * ** NB: ** em and er currently ignored
 */

PDC_error_t PDC_char_lit_scan(PDC_t* pdc, PDC_base_em* em,
			      PDC_base_ed* ed, unsigned char c, PDC_disc_t* disc);

/* ================================================================================ */
/* READ FUNCTIONS */

/* PDC_char_lit_read:
 *
 * EFFECT: verify IO cursor points to specified char, move IO cursor just beyond
 *
 * RETURNS: PDC_ERROR if cursor does not point to char (IO cursor unchanged)
 *          PDC_OK if cursor does point to char (IO cursor now points just beyond char)
 *
 * ** NB: ** em and er currently ignored
 */

PDC_error_t PDC_char_lit_read(PDC_t* pdc, PDC_base_em* em,
			      PDC_base_ed* ed, unsigned char c, PDC_disc_t* disc);

/* ================================================================================ */
/* MISC ROUTINES */
/*
 *    PDC_fmtChar: produce a ptr to a string that is a pretty-print (escaped) formated for char c
 *        N.B. Resulting string should be printed immediately then not used again, e.g.,
 *        PDC_report_err( ..xxx.. , "Missing separator: %s", PDC_fmtChar(010)); 
 */
char*       PDC_fmtChar(char c);

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
