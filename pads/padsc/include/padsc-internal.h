#pragma prototyped
/*
 * padc library interface
 * 
 * Kathleen Fisher, Robert Gruber
 * AT&T Labs Research
 */

#ifndef __LIBPADSC_INTERNAL_H__
#define __LIBPADSC_INTERNAL_H__

#include <ast.h>
#include <vmalloc.h>
#include <sfstr.h>
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
  Vmalloc_t*        vm;     /* vm handle */
  PDC_disc_t*       disc;   /* user-supplied discipline (can be null) */
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
 *   format is an additional printf-formatted description that augments the default
 *   description based on errCode and the optional args go with the format arg.
 * N.B. This call does nothing if either (disc && !disc->errorf) or
 * or if (!disc && !pdc->disc->errorf).
 */
PDC_error_t   PDC_report_err(PDC_t* pdc, PDC_disc_t* disc, PDC_loc_t* loc,
			     int errCode, const char* format, ...);

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

#endif /*  __LIBPADSC_INTERNAL_H__  */
