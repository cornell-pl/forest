#pragma prototyped
/*
 * rbuf interface - internal
 * 
 * Kathleen Fisher, Robert Gruber
 * AT&T Labs Research
 */

#ifndef __RBUF_INTERNAL_H__
#define __RBUF_INTERNAL_H__

#include "rbuf.h"

struct RBuf_s {
  RMM_t*       mgr;
  void*        buf;
  size_t       bufSize;
  size_t       eltSize;
  size_t       numElts;
  size_t       extraBytes;
  size_t       maxEltHint;
};

struct RMM_s {
  void*          vm;
  RMM_allin1_fn  fn;
};

#endif  /*  __RBUF_INTERNAL_H__  */
