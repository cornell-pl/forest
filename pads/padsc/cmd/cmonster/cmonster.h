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

#include "padsc-internal.h"
#include "cmdline.h"

/* -------------------------------------------------------------------------------- */
/* types */

/* A cmonster handle, type CM_t: */
struct CM_s {
  PDC_t           *pdc;
  Vmalloc_t       *vm;
  Sfio_t          *inf;
  Sfio_t          *outf;
  Sfio_t          *errf;
  Sfio_t          *padslf;
  size_t           outbuf_sz;
  PDC_byte        *outbuf;
  PDC_byte        *outbuf_end;
  PDC_byte        *outbuf_cursor; /* cursor always in range [outbuf, outbuf_end] */
};

/* -------------------------------------------------------------------------------- */
/* Static state */
extern CM_tmentry_t tmap[];

/* -------------------------------------------------------------------------------- */
/* io discipline helpers */

/* returns -1 on usage error, 0 otherwise */
/* (*iodisc_out) set to result of iodisc open call (may be NULL) */
int CM_open_iodisc(CM_t *cm, CM_dspec *dspec, PDC_IO_disc_t **iodisc_out);

/* -------------------------------------------------------------------------------- */
/* helper macros */

/* the param signature of a rw_fn: */ 
#define CM_RW_FN_PARAMS \
  CM_t *cm, CM_query *qy, PDC_byte *begin, PDC_byte *end  

/* rw_fn function name for ty, e.g., CM_RW_FN_NM(a_int8) */
#define CM_RW_FN_NM(ty) \
CM_ ## ty ## _rw

/* rw_fn function decl for ty, e.g., CM_RW_FN_DECL(a_int8) */
#define CM_RW_FN_DECL(ty) \
PDC_error_t CM_RW_FN_NM(ty) (CM_RW_FN_PARAMS)

/* the param signature of a sval_fn: */ 
#define CM_SVAL_FN_PARAMS \
  CM_t *cm, CM_query *qy, PDC_byte *begin, PDC_byte *end, PDC_int32 *res_out  

/* sval_fn function name for ty, e.g., CM_SVAL_FN_NM(a_int8) */
#define CM_SVAL_FN_NM(ty) \
CM_ ## ty ## _sval

/* sval_fn function decl for ty, e.g., CM_SVAL_FN_DECL(a_int32) */
#define CM_SVAL_FN_DECL(ty) \
PDC_error_t CM_SVAL_FN_NM(ty) (CM_SVAL_FN_PARAMS)

/* out_sz_fn function name for ty, e.g., CM_OUT_SZ_FN_NM(a_int8) */
#define CM_OUT_SZ_FN_NM(ty) \
CM_ ## ty ## _out_sz

/* -------------------------------------------------------------------------------- */

#endif  /* __CMONSTER_H__  */

