#ifdef _USE_PROTO
#pragma prototyped
#endif
/*
 * rbuf interface
 *         RBuf : Resizable Buffer
 *         RMM  : RBuf Memory Manager
 * 
 * Kathleen Fisher, Robert Gruber
 * AT&T Labs Research
 */

#ifndef __RBUF_H__
#define __RBUF_H__

#include <ast_common.h>

/* ================================================================================

General description of this wrapper library

Variable names used in this description:
RBuf_t*  rbuf  -- handle to a resizable buffer
RMM_t*   mgr   -- handle to an Rbuf memory manager
void*    buf   -- pointer to a buffer that is (or was) associated with an rbuf

An rbuf is a small data structure that keeps track of information
about a resizable buffer, buf, which can be allocated, resized, and
released using operations on the rbuf.  Since buf is resizable, it is
always allocated (and resized) using a memory manager; it is never on
the stack.  In contrast, rbuf is a simple fixed-sized data structure
that could be stack allocated or heap allocated.  However, for
simplicity we currently require that it is always heap allocated.

It is important to remember that the lifetime of an rbuf is not always the
same as the lifetime of its associated buf: buf can 'live on' even after the
associated rbuf has been freed.

In the common case, an rbuf and its buf are freed at the
same time, as in this sample code:

void sample1(RMM_t* mgr) {
  char*  buf;
  RBuf_t* rbuf;
  rbuf = RMM_new_rbuf(mgr);         -- heap alloc a new rbuf managed by mgr
  RBuf_RESERVE(rbuf, buf, char, 12);  -- reserve space for 12 chars +
                                          store associated buffer ptr in buf
  sprintf(buf, "hello world");
  ...use buf...
  RMM_free_rbuf(rbuf)                 -- frees both rbuf and associated buffer buf
}

If it is necessary to keep a pointer to buf beyond the
lifetime of rbuf, then one must keep track of the manager
and eventually free buf, as in this sample code:

char* sample2_helper(RMM_t* mgr) {
  char*  buf;
  RBuf_t* rbuf;
  rbuf = RMM_new_rbuf(mgr);
  RBuf_RESERVE(rbuf, buf, char, 12);
  sprintf(buf, "hello world");
  RMM_free_rbuf_keep_buf(rbuf, 0, 0); -- frees rbuf but *not* associated buffer buf
  return buf;
}

void sample2(RMM_t* mgr) {
  char* buf;
  buf = sample2_helper(mgr);
  ...use buf...
  RMM_free_buf(mgr, (void*)buf)       -- finally frees buf
}

Documentation for each routine:
 
================================================================================
RMM_t* RMM_open(RMM_disc_t* disc)
================================================================================
 A rbuf manager is really a wrapper for an underlying storage manager.
 RMM_open creates a new manager, and the underlying alloc/resize/free
 mechanisms can be specified using the disc argument.  If disc is nil,
 the default discipline, RMM_zero_disc, is used (see below).

 Returns: 0 on failure, valid pointer on success

 At the moment, the discipline specifies exactly one thing:
 a single allin1 function that takes 3 args and does all of the following functions:

    allin1(0,0,0)	open new region
    allin1(r,0,0)	free region
    allin1(r,0,n)	allocate n bytes
    allin1(r,p,0)	free p
    allin1(r,p,n)	realloc p to n bytes

 Two allin1 functions are provided:

    RMM_allin1_zero, RMM_allin1_nozero

 Each use the vmalloc memory manager with Vmdcheap/Vmbest, where
 RMM_allin1_zero zeroes all memory that is (re)allocated and the other does not.

 Two handles to disciplines that use these functions are also provided:
 RMM_zero_disc_ptr and RMM_nozero_disc_ptr; they point to disciplines 
 RMM_zero_disc and RMM_nozero_disc, respectively.
 RMM_zero_disc is the default discipline if none is specified.

================================================================================
int RMM_close(RMM_t* mgr);
================================================================================
 RMM_close closes the specified manager, releasing all storage
 associated with that manager, including any rbuf/buffer storage
 that was allocated by that manager.

 Return codes:
    0 => success
   -1 => failure

================================================================================
 Dynamic allocation of an rbuf:
   RBuf_t*   RMM_new_rbuf (RMM_t* mgr);
================================================================================
 Returns: 0 on failure, valid pointer on success

================================================================================
int RMM_free_rbuf(RBuf_t* rbuf);
int RMM_free_rbuf_keep_buf(RBuf_t* rbuf, void** buf_out, RMM_t** mgr_out);
================================================================================
 Frees the space allocated for rbuf.  Note that the rbuf knows who its manager
 is... this is really a call on a manager, thus the RMM_ prefix.

 RMM_free_rbuf frees the buffer associated with rbuf, 
 while RMM_free_rbuf_keep_buf does not. For the latter:
   . If buf_out is non-nil, *buf_out is set to point to this buffer
   . If mgr_out is non-nil, *mgr_out is set to point to the manager
       that should be used later to free this buffer (using RMM_free_buf).

 Return codes:
    0 => success
   -1 => no manager
   -2 => other error

================================================================================
int RMM_free_buf(RMM_t* mgr, void* buf);
================================================================================
  Frees a buffer that *used to be* associated with an rbuf.  Should not
  be used unless the rbuf has already been freed using RMM_free_rbuf_keep_buf.

 Return codes:
    0 => success
   -1 => error


================================================================================
int RBuf_reserve(Rbuf_t* rbuf, void** buf_out, size_t eltSize,
                 size_t numElts, size_t maxEltHint)
================================================================================
  RBuf_reserve ensures that the buffer associated with rbuf has space for
  at least numElts elements of size eltSize.  If necessary, it reallocates
  the space and copies the previous buffer.  If buf_out is non-nil,
  *buf_out is set to point to the associated buffer.  

 Return codes:
    0 => success
   -1 => no manager
   -2 => out of space
   -3 => other error

  RBuf_reserve is free to allocate more space than is requested, and is not
  required to free up space if the specified space requirement is less than the
  space that has already been allocated.  The argument maxEltHint is a hint as
  to future growth, which may or may not have an impact on how quickly the
  buffer space grows as new space is demanded.

  Since the buffer pointer associated with rbuf can change across
  calls to RBuf_reserve, it is very important that the user code should
  update its buffer pointer on each call.  Example:

    char*  buf;
    RBuf_t* rbuf;
    rbuf = RMM_new_rbuf(mgr);
    RBuf_reserve(rbuf, (void**)&buf, sizeof(char), 12, 0, 0);
    sprintf(buf, "hello world");
    RBuf_reserve(rbuf, 0, sizeof(char), 20, 0, 0);  -- ERROR, should have passed in &buf since buf ptr can change!
    sprintf(buf, "hello big bad world");            -- if buf has changed, this could cause a core dump

================================================================================
RBuf_RESERVE(rbuf, buf_ptr, type, numElts)
================================================================================
RBuf_RESERVE is a macro that calls RBuf_reserve.  The rewrite that occurs
is that the above call is turned into:

RBuf_reserve(rbuf, (void**)&(buf_ptr), sizeof(type), numElts, numElts)

For example, these 2 calls are equivalent:

  RBuf_RESERVE(rbuf, buf, char, 12);
  RBuf_reserve(rbuf, (void**)&buf, sizeof(char), 12, 12);

Note that if you want to specify maxEltHint that is different from numElts,
you cannot use this macro.

================================================================================
RBuf state access functions:

  size_t RBuf_bufSize   (RBuf_t* rbuf)
  size_t RBuf_numElts   (RBuf_t* rbuf)
  size_t RBuf_eltSize   (RBuf_t* rbuf)
  size_t RBuf_maxEltHint(RBuf_t* rbuf)

  void* RBuf_get_buf  (RBuf_t* rbuf)
  void* RBuf_get_elt  (RBuf_t* rbuf, size_t index)
================================================================================
Return values:
   RBuf_bufSize:    the actual number of bytes currently allocated
   RBuf_numelts:    numElts    as given in latest RBuf_reserve call
   RBuf_eltSize:    eltSize    as given in latest RBuf_reserve call
   RBuf_maxEltHint: maxEltHint as given in latest RBuf_reserve call

   RBuf_get_buf:   pointer to start of buffer
   RBuf_get_elt:   returns pointer to element (index+1) in the buffer

N.B.: get_elt uses numElts/eltSize as given in the latest
      RBuf_reserve call to compute the necessary offset into the buffer.

================================================================================
RBuf access macros:
  type* RBuf_GET_BUF  (rbuf, type)
  type* RBuf_GET_ELT  (rbuf, index, type)
================================================================================
These macros take a type as last argument and return a
pointer that has been cast to be a pointer to data of the specified type.
They are otherwise the same as the corresponding routines.


================================================================================
*/

struct RBuf_s;
struct RMM_s;
typedef struct RBuf_s RBuf_t;
typedef struct RMM_s RMM_t;

typedef void* (*RMM_allin1_fn)(void* vm, void* data, size_t size);

typedef struct RMM_disc_t {
  RMM_allin1_fn allin1;
} RMM_disc_t;

extern void* RMM_allin1_zero  (void* vm, void* data, size_t size);
extern void* RMM_allin1_nozero(void* vm, void* data, size_t size);

extern RMM_disc_t RMM_zero_disc;
extern RMM_disc_t RMM_nozero_disc;

extern RMM_disc_t* RMM_zero_disc_ptr;
extern RMM_disc_t* RMM_nozero_disc_ptr;

RMM_t*    RMM_open(RMM_disc_t* disc);
int       RMM_close(RMM_t* mgr);

RBuf_t*   RMM_new_rbuf (RMM_t* mgr);

int       RMM_free_rbuf(RBuf_t* rbuf);
int       RMM_free_rbuf_keep_buf(RBuf_t* rbuf, void** buf_out, RMM_t** mgr_out);

int       RMM_free_buf(RMM_t* mgr, void* buf);

int       RBuf_reserve(RBuf_t* rbuf, void** buf_out, size_t eltSize,
		       size_t numElts, size_t maxEltHint);

size_t    RBuf_bufSize   (RBuf_t* rbuf);
size_t    RBuf_numElts   (RBuf_t* rbuf);
size_t    RBuf_eltSize   (RBuf_t* rbuf);
size_t    RBuf_maxEltHint(RBuf_t* rbuf);

void*     RBuf_get_buf  (RBuf_t* rbuf);
void*     RBuf_get_elt  (RBuf_t* rbuf, size_t index);

/* ================================================================================ */
/* MACROS */

#define RBuf_RESERVE(rbuf, buf_ptr, type, numElts) \
  RBuf_reserve((rbuf), (void**)&(buf_ptr), sizeof(type), (numElts), (numElts))

#define RBuf_GET_BUF(rbuf, type) \
  (type)* RBuf_get_buf(rbuf)

#define RBuf_GET_ELT(rbuf, index, type) \
  (type)* RBuf_get_elt(rbuf, index)

#define RBuf_CPY_SRC2DEST(src, dest, bytes, mgr) do { \
  void *src_buf, *dest_buf; \
  if (!(dest)) { \
    (dest) = RMM_new_rbuf(mgr); \
  } \
  RBuf_reserve((dest), &dest_buf, 1, (bytes), (bytes)); \
  src_buf = RBuf_get_buf(src); \
  memcpy(dest_buf, src_buf, (bytes)); \
} while (0)

#endif  /*  __RBUF_H__  */
