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
 * PDC_IO_t: io state : this will change, something simple for now 
 *   (line based input is assumed to work)
 */
typedef struct PDC_IO_s {
  Sfio_t*           io;   /* io stream */
  char*             path; /* original path -- eventually want to support a set of input files */
  size_t            line; /* line count */
  int               eof;  /* hit EOF ? */ 
  char*             buf;  /* input buffer : initialized on file open */
  char*             b;    /* b, c, e point into buf, where b,e mark begin/end */
  char*             e;    /* of cur input line, e one beyond last char in line */
  char*             c;    /* thus c == e means current line has been read */
} PDC_IO_t;

struct PDC_s {
  const char*       id;     /* interface id */
  PDC_disc_t*       disc;   /* user-supplied discipline (can be null) */
  Vmalloc_t*        vm;     /* vm handle */
  PDC_IO_t          iost;   /* io state */
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

PDC_error_t  PDC_get_loc       (PDC_t* pdc, PDC_loc_t* l, PDC_disc_t* disc);
int          PDC_IO_is_EOF     (PDC_t* pdc, PDC_disc_t* disc);

PDC_error_t  PDC_IO_getchar    (PDC_t* pdc, unsigned char* ct, int panicking, PDC_disc_t* disc);
PDC_error_t  PDC_IO_back       (PDC_t* pdc, size_t num_chars, PDC_disc_t* disc);

PDC_error_t  PDC_IO_checkpoint (PDC_t* pdc, PDC_disc_t* disc);
PDC_error_t  PDC_IO_commit     (PDC_t* pdc, PDC_disc_t* disc);
PDC_error_t  PDC_IO_restore    (PDC_t* pdc, PDC_disc_t* disc);

PDC_error_t  PDC_IO_refill     (PDC_t* pdc, PDC_disc_t* disc);

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
 *   XXX errCode's type should be an enum that describes the kind of error XXX ???
 *   The varargs are for a printf-style format, arg1, ... 
 *   (printf-style) description that augments the default
 *   description based on errCode.
 * N.B. This call does nothing if either (disc && !disc->errorf) or
 * or if (!disc && !pdc->disc->errorf).
 */
PDC_error_t   PDC_report_err(PDC_t* pdc, PDC_disc_t* disc, PDC_loc_t* loc, int errCode, /* format, args */ ...);

/*
 *  PDC_errorf, PDC_errorvf: error reporting impls
 */
int           PDC_errorf(PDC_t* pdc, PDC_disc_t* disc, int level, ...);
int           PDC_errorvf(PDC_t* pdc, PDC_disc_t* disc, int level, va_list ap);

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
			      PDC_base_ed* er, unsigned char c, PDC_disc_t* disc);

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
			      PDC_base_ed* er, unsigned char c, PDC_disc_t* disc);

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
