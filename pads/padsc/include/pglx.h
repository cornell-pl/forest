#ifdef _USE_PROTO
#pragma prototyped
#endif
/*
 * public API, galax-pads
 * 
 * Kathleen Fisher, Robert Gruber
 * AT&T Labs Research
 */

#ifndef __PGLX_H__
#define __PGLX_H__

/* generic calls from galax to pads */

#include "libpadsc.h"       /* Need to include ast stuff before caml stuff for some reason */

/* XXX_RESTORE: */
/* #include "caml/mlvalues.h"  */ /* Need value */

/* XXX_REMOVE next 4 lines: */
#ifndef FAKE_CAML_VALUE
#define FAKE_CAML_VALUE
typedef void* value;
#endif /* FAKE_CAML_VALUE */

void**      PGLX_generic_children    (void *ocaml_n);
void*       PGLX_generic_parent      (void *ocaml_n);
value       PGLX_generic_typed_value (void *ocaml_n);
const char* PGLX_generic_string_value(void *ocaml_n);
const char* PGLX_generic_name        (void *ocaml_n);

void        PGLX_node_free           (void *ocaml_n);

#endif  /*   __PGLX_H__   */
