#ifdef _USE_PROTO
#pragma prototyped
#endif
/*
 * Map from type name to read and write functions for cmonster.
 * This file is included directly by cmonster.h
 * 
 * Kathleen Fisher, Robert Gruber
 * AT&T Labs Research
 */

#ifndef __TMAP_H__
#define __TMAP_H__

/* Declare all of the rwfn: */

#define CMR_RW_DECL(fn) \
PDC_error_t fn(CMR_t *cmr, CMR_cookie_t *cookie, PDC_byte *begin)

CMR_RW_DECL(CMR_int8_rw);
CMR_RW_DECL(CMR_int16_rw);
CMR_RW_DECL(CMR_int32_rw);
CMR_RW_DECL(CMR_int64_rw);
CMR_RW_DECL(CMR_uint8_rw);
CMR_RW_DECL(CMR_uint16_rw);
CMR_RW_DECL(CMR_uint32_rw);
CMR_RW_DECL(CMR_uint64_rw);

CMR_tmentry_t tmap[] = {
  { "PDC_int8",         CMR_int8_rw },
  { "PDC_int16",        CMR_int16_rw },
  { "PDC_int32",        CMR_int32_rw },
  { "PDC_int64",        CMR_int64_rw },
  { "PDC_uint8",        CMR_uint8_rw },
  { "PDC_uint16",       CMR_uint16_rw },
  { "PDC_uint32",       CMR_uint32_rw },
  { "PDC_uint64",       CMR_uint64_rw },
  /* END MARKER */
  { 0, 0 }
};

#endif  /* __TMAP_H__  */

