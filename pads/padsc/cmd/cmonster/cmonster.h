#ifdef _USE_PROTO
#pragma prototyped
#endif
/*
 * Decls for impl of cmonster ('cookie monster') command
 *
 * Kathleen Fisher, Robert Gruber
 * AT&T Labs Research
 */

#ifndef __CMONSTER_H__
#define __CMONSTER_H__

#include "libpadsc.h"
#include "cmdline.h"

/* Predeclare some types */
typedef struct CMR_s CMR_t;
typedef struct CMR_cookie_s CMR_cookie_t;
typedef struct CMR_tmentry_s CMR_tmentry_t;

/* A cmonster handle, type CMR_t: */
struct CMR_s {
  PDC_t           *pdc;
};

/* Signature of a cookie monster read/write function */
typedef PDC_error_t (*CMR_rwfn)(CMR_t *cmr, CMR_cookie_t *cookie, PDC_byte *begin);

/* Representation of a cookie, type CMR_cookie_t: */
struct CMR_cookie_s {
  PDC_uint32       num;
  PDC_uint64       offset;
  PDC_uint64       size;
  PDC_uint32      *params[10];
  const char      *tname;
  CMR_rwfn         rwfn;
};

/* A typemap entry, type CMR_tmentry_t: */
struct CMR_tmentry_s {
  const char      *tname;
  CMR_rwfn         rwfn;
};

extern CMR_tmentry_t tmap[];

/* io discipline helpers */

/* returns -1 on usage error, 0 otherwise */
/* (*iodisc_out) set to result of iodisc open call (may be NULL) */
int CMR_open_iodisc(CMDLINE_iodisc_spec *ispec, PDC_IO_disc_t ** iodisc_out);

#endif  /* __CMONSTER_H__  */

