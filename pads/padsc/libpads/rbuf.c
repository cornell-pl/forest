#pragma prototyped
/*
 * rbuf implementation
 * 
 * Kathleen Fisher, Robert Gruber
 * AT&T Labs Research
 */

#include "rbuf-internal.h"
#include "vmalloc.h"

/* ================================================================================ */
/* RMM : default allin1 functions and disciplines */

void* RMM_allin1_zero(void* vm, void* data, size_t size)
{
  error(2, "RMM_allin1_zero called with vm %p, data %p, size %ld", vm, data, size);
  if (!vm) {
    return (void*)vmopen(Vmdcheap, Vmbest, 0);
  }
  if (data || size) {
    return (void*)vmresize((Vmalloc_t*)vm, data, size, VM_RSMOVE|VM_RSCOPY|VM_RSZERO);
  }
  vmclose((Vmalloc_t*)vm);
  return 0;
}

void* RMM_allin1_nozero(void* vm, void* data, size_t size)
{
  error(2, "RMM_allin1_nozero called with vm %p, data %p, size %ld", vm, data, size);
  if (!vm) {
    return (void*)vmopen(Vmdcheap, Vmbest, 0);
  }
  if (data || size) {
    return (void*)vmresize((Vmalloc_t*)vm, data, size, VM_RSMOVE|VM_RSCOPY);
  }
  vmclose((Vmalloc_t*)vm);
  return 0;
}

RMM_disc_t RMM_zero_disc   = { RMM_allin1_zero };
RMM_disc_t RMM_nozero_disc = { RMM_allin1_nozero };

RMM_disc_t* RMM_zero_disc_ptr   = &RMM_zero_disc;
RMM_disc_t* RMM_nozero_disc_ptr = &RMM_nozero_disc;

/* ================================================================================ */
/* RMM : open/close */

RMM_t*
RMM_open(RMM_disc_t* disc)
{
  void* vm;
  RMM_t* res;

  if (!disc) {
    disc = RMM_zero_disc_ptr;
  }
  if (!(vm = disc->allin1(0, 0, 0))) {
    return 0;
  }
  if (!(res = (RMM_t*)disc->allin1(vm, 0, sizeof(RMM_t)))) {
    return 0;
  }
  res->vm = vm;
  res->fn = disc->allin1;
  return res;
}

int
RMM_close(RMM_t* mgr)
{
  if (!mgr || !mgr->vm || !mgr->fn) {
    return -1; /* failure */
  }
  mgr->fn(mgr->vm, 0, 0); /* frees vm region / all vm-allocated objects, including mgr itself */
  return 0; /* success */
}

/* ================================================================================ */
/* RMM : new/free/reserve functions */

RBuf_t*
RMM_new_rbuf (RMM_t* mgr)
{
  RBuf_t* res;

  if (!mgr || !mgr->vm || !mgr->fn) {
    return 0; /* failure */
  }
  if (!(res = (RBuf_t*)mgr->fn(mgr->vm, 0, sizeof(RBuf_t)))) {
    return 0; /* failure */
  }
  res->mgr = mgr;
  res->buf = 0;
  res->bufSize = res->eltSize = res->numElts = res->maxEltHint = 0;
  return res;
}

int
RMM_free_rbuf(RBuf_t* rbuf)
{
  RMM_t* mgr;
  if (!rbuf) {
    return -2; /* failure - other */
  }
  mgr = rbuf->mgr;
  if (!mgr || !mgr->vm || !mgr->fn) {
    return -1; /* failure - no manager */
  }
  if (rbuf->buf) {
    mgr->fn(mgr->vm, rbuf->buf, 0); /* free buf */
    rbuf->buf = 0;
  }
  rbuf->mgr = 0; /* prevent future rbuf_free from doing anything */
  mgr->fn(mgr->vm, rbuf, 0); /* free rbuf */
  return 0; /* success */
}

int
RMM_free_rbuf_keep_buf(RBuf_t* rbuf, void** buf_out, RMM_t** mgr_out)
{
  RMM_t* mgr;
  if (!rbuf) {
    return -2; /* failure - other */
  }
  mgr = rbuf->mgr;
  if (!mgr || !mgr->vm || !mgr->fn) {
    return -1; /* failure - no manager */
  }
  if (mgr_out) {
    *mgr_out = mgr;
  }
  if (buf_out) {
    *buf_out = rbuf->buf;
  }
  rbuf->mgr = 0; /* prevent future rbuf_free from doing anything */
  rbuf->buf = 0; /* ditto for future rbuf access calls */
  mgr->fn(mgr->vm, rbuf, 0); /* free rbuf */
  return 0; /* success */
}

int
RMM_free_buf(RMM_t* mgr, void* buf)
{
  if (!mgr || !mgr->vm || !mgr->fn) {
    return -1; /* failure - no manager */
  }
  mgr->fn(mgr->vm, buf, 0); /* free buf */
  return 0; /* success */
}

int
RBuf_reserve(RBuf_t* rbuf, void** buf_out, size_t eltSize,
	     size_t numElts, size_t maxEltHint)
{
  RMM_t* mgr;
  size_t new_size;
  size_t targ_size;

  if (!rbuf) {
    return -3; /* failure - other */
  }
  mgr = rbuf->mgr;
  if (!mgr || !mgr->vm || !mgr->fn) {
    return -1; /* failure - no manager */
  }
  targ_size = (eltSize * numElts);
  if (rbuf->buf && (targ_size <= rbuf->bufSize)) {
    return 0; /* trivial success */
  }
  if (!rbuf->buf) {
    if ((maxEltHint > numElts) && (maxEltHint <= 2*numElts)) {
      new_size = eltSize * maxEltHint;
    } else {
      new_size = targ_size;
    }
  } else {
    new_size = rbuf->bufSize;
    while (new_size < targ_size) {
      new_size *= 2;
    }
    /* adjust downward if hint tells us we would grow too much */
    if (maxEltHint >= numElts) {
      targ_size = eltSize * maxEltHint;
      if (targ_size < new_size) {
	new_size = targ_size;
      }
    }
  }
  if (!(rbuf->buf = mgr->fn(mgr->vm, rbuf->buf, new_size))) { /* resize buf */
    return -2; /* failure - out of space */
  }
  rbuf->bufSize    = new_size;
  rbuf->eltSize    = eltSize;
  rbuf->numElts    = numElts;
  rbuf->maxEltHint = maxEltHint;
  if (buf_out) {
    *buf_out = rbuf->buf;
  }
  return 0; /* success */
}

/* ================================================================================ */
/* RBuf : accessor functions */

size_t
RBuf_bufSize(RBuf_t* rbuf)
{
  if (!rbuf) {
    return 0;
  }
  return rbuf->bufSize;
}

size_t
RBuf_numElts(RBuf_t* rbuf)
{
  if (!rbuf) {
    return 0;
  }
  return rbuf->numElts;
}

size_t
RBuf_eltSize(RBuf_t* rbuf)
{
  if (!rbuf) {
    return 0;
  }
  return rbuf->eltSize;
}

size_t
RBuf_maxEltHint(RBuf_t* rbuf)
{
  if (!rbuf) {
    return 0;
  }
  return rbuf->maxEltHint;
}

void*
RBuf_get_buf(RBuf_t* rbuf)
{
  if (!rbuf) {
    return 0;
  }
  return rbuf->buf;
}

void*
RBuf_get_elt(RBuf_t* rbuf, size_t index)
{
  if (!rbuf) {
    return 0;
  }
  return (void*) ((char*)rbuf->buf + (index * rbuf->eltSize));
}

/* ================================================================================ */

