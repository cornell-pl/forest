#include "pglx-internal.h"
/* include mary's stuff for c to ocaml rep functions */
/* ocaml header files can be found in /usr/common/lib/ocaml/caml */
#include <caml/fail.h>      /* exception */


/* Generic functions */
/* PDCI_node_rep_t ** PGLX_generic_children (PDCI_node_rep_t *node); */
void** PGLX_generic_children (void *ocaml_n)
{
  PDCI_node_rep_t *n = (PDCI_node_rep_t *) ocaml_n; 
  PDCI_NODE_VT_CHECK(n, "PGLX_generic_children");
  return (void **) n->vt.children(n);
}

void* PGLX_generic_parent (void *ocaml_n)
{
  PDCI_node_rep_t *n = (PDCI_node_rep_t *) ocaml_n; 
  PDCI_NODE_CHECK(n, "PGLX_generic_parent");
  return (void *)n->parent;
}

/* Return value is: 
   1. atomicValue -- a Caml object or 
   2. a union of PADS base types, which is converted
      to a Caml object on the Caml side.
 */

value PGLX_generic_typed_value (void * ocaml_n)
{
  PDCI_node_rep_t *n = (PDCI_node_rep_t *) ocaml_n; 
  PDCI_NODE_VT_CHECK(n, "PGLX_generic_typed_value");
  return n->vt.typed_value(n);
}

const char* PGLX_generic_string_value(void *ocaml_nn){
  PDCI_node_rep_t *n = (PDCI_node_rep_t *) ocaml_n; 
  PDCI_NODE_CHECK(n, "PGLX_generic_string_value");
  return "Not yet implemented";
}




/* HELPERS */

/* Helper functions */
/* Error function used for many cases */
value PDCI_error_typed_value(PDCI_node_rep_t *node){
  failwith("NOT_A_VALUE: typed_value called on structured type.");
  return 0;  /* will never get here*/
} 

/* Children functions for structured_pd, sequenced_pd */
/* A structured_pd has four children (nerr, errCode, loc, panic) */
PDCI_node_t ** PDCI_structured_pd_children(PDCI_node_rep_t *self){
  PDCI_structured_pd *pd = (PDCI_structured_pd *) self->rep;
  PDCI_node_rep_t **result;
  if (!(result = PDCI_NEW_NODE_PTR_LIST(pdc, 4))) {
    failwith("ALLOC_ERROR: in PDCI_structured_pd_children");
  }
  /* the following mk calls raise an exception on alloc error */
  PDCI_MK_TNODE(result[0], PDC_uint32_val_vtable, self, "nerr", &(pd->nerr));
  PDCI_MK_TNODE(result[1], PDC_errCode_t_vtable,self,"errCode",&(pd->errCode));
  PDCI_MK_TNODE(result[2], PDC_loc_t_vtable, self, "loc", &(pd->loc));
  PDCI_MK_TNODE(result[3], PDC_int32_val_vtable, self, "panic", &(pd->panic));
  return result;
}

/* A sequenced_pd has six children 
  (nerr, errCode, loc, panic, neerr, firstError) */
PDCI_node_t ** PDCI_sequenced_pd_children(PDCI_node_rep_t *self){
  PDCI_sequenced_pd *pd = (PDCI_sequenced_pd *) self->rep;
  PDCI_node_rep_t **result;
  if (!(result = PDCI_NEW_NODE_PTR_LIST(pdc, 6))) {
    failwith("ALLOC_ERROR: in PDCI_sequenced_pd_children");
  }
  /* the following mk calls raise an exception on alloc error */
  PDCI_MK_TNODE(result[0], PDC_uint32_val_vtable, self, "nerr", &(pd->nerr));
  PDCI_MK_TNODE(result[1], PDC_errCode_t_vtable,self,"errCode",&(pd->errCode));
  PDCI_MK_TNODE(result[2], PDC_loc_t_vtable, self, "loc",   &(pd->loc));
  PDCI_MK_TNODE(result[3], PDC_int32_val_vtable, self, "panic", &(pd->panic));
  PDCI_MK_TNODE(result[4], PDC_int32_val_vtable, self, "neerr", &(pd->neerr));
  PDCI_MK_TNODE(result[5], PDC_int32_val_vtable, self, "firstErr", &(pd->firstError));
  return result;
}


/* Helper vtables */
const PDCI_vtable_t
PDCI_structured_pd_vtable = {PDCI_structured_pd_children, 
			     PDCI_error_typed_value,
			     0};

const PDCI_vtable_t
PDCI_sequenced_pd_vtable = {PDCI_sequenced_pd_children, 
			    PDCI_error_typed_value,
			    0};

/* Bob: we need vtable for each base type in parse descriptors:
   PDC_errCode_t, PDC_loc_t, PDC_pos_t, ...? 
   PDC_flags_t, PDCI_flag.  */


/* BASE TYPE SUPPORT */
/* 1. Define a vtable and val_vtable for each base type */
/* 2. Define bty_typed_value */
/* 3. Define bty_val_typed_value */

value PDC_uint32_typed_value (void * ocaml_n)
{
  PDCI_node_rep_t *n = (PDCI_node_rep_t *) ocaml_n; 
  PDC_uint32 r = *((PDC_uint32 *)n->rep);
 
  /*
  call macro provided my mary to create an ocaml value.
  macro will come from/follow pattern from c_api portion of galax.
  galax/galapi/c_api
  whether to gc protect it is open question.
  Xavier's email is relevant.
  ...
  */
}
