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

#ifdef USE_GALAX

#include "caml/mlvalues.h"  /* Need value */

#else

/* XXX_REMOVE next 4 lines: */
#ifndef FAKE_CAML_VALUE
#define FAKE_CAML_VALUE
typedef void* value;
#endif /* FAKE_CAML_VALUE */

#endif /* USE_GALAX */

/*
 * XXX TEMPORARY:
 */
void walk_children(void *n, int indent);

void**      PGLX_generic_children    (void *ocaml_n);
void*       PGLX_generic_parent      (void *ocaml_n);
value       PGLX_generic_typed_value (void *ocaml_n);
const char* PGLX_generic_string_value(void *ocaml_n);
const char* PGLX_generic_name        (void *ocaml_n);

void        PGLX_node_free           (void *ocaml_n);     /* free node ocaml_n */
void        PGLX_nodelist_free       (void **child_list); /* free a void ** list returned from generic_children */
/* Note: PGLX_nodelist_free does not free the child nodes, just the list that refers to them */

#endif  /*   __PGLX_H__   */
