#include "pglx.h"
#include "pglx-internal.h"

/* ocaml header files can be found in /usr/common/lib/ocaml/caml */
#include "caml/fail.h"      /* exception */
/* include mary's stuff for c to ocaml rep functions */

/* ================================================================================
 * PUBLIC GALAX->PADS CALLS (see pglx.h) */ 

void** PGLX_generic_children (void *ocaml_n)
{
  PDCI_node_t *n = (PDCI_node_t *) ocaml_n; 
  if (!n)
    failwith("INVALID_PARAM: n null in " "PGLX_generic_children");
  if (!n->vt)
    failwith("INVALID_PARAM: n->vt null in " "PGLX_generic_children");
  PDCI_NODE_VT_CHECK(n, "PGLX_generic_children");
  return (void **) ((n->vt->children)(n));
}

void* PGLX_generic_parent (void *ocaml_n)
{
  PDCI_node_t *n = (PDCI_node_t *) ocaml_n; 
  PDCI_NODE_CHECK(n, "PGLX_generic_parent");
  return (void *) (n->parent);
}

/* Return value TBD */

value PGLX_generic_typed_value (void * ocaml_n)
{
  PDCI_node_t *n = (PDCI_node_t *) ocaml_n; 
  PDCI_NODE_VT_CHECK(n, "PGLX_generic_typed_value");
  return (n->vt->typed_value)(n);
}

const char* PGLX_generic_string_value(void *ocaml_n)
{
  PDCI_node_t *n = (PDCI_node_t *) ocaml_n; 
  PDCI_NODE_CHECK(n, "PGLX_generic_string_value");
  (n = n); /* XXX_REMOVE */
  return "Not yet implemented";
}

const char* PGLX_generic_name(void *ocaml_n){
  PDCI_node_t *n = (PDCI_node_t *) ocaml_n; 
  PDCI_NODE_CHECK(n, "PGLX_generic_name");
  return n->name;
}

void PGLX_node_free(void *ocaml_n)
{
  PDCI_node_t *n = (PDCI_node_t *) ocaml_n;
  PDCI_FREE_NODE(n->pdc, n);
}

/* ================================================================================
 * INTERNAL */

/* HELPERS */

/* Helper functions */
/* Error function used for many cases */
value PDCI_error_typed_value(PDCI_node_t *node)
{
  failwith("NOT_A_VALUE: typed_value called on structured type.");
  return 0;  /* will never get here*/
} 

/* Children functions for structured_pd, sequenced_pd */
/* A structured_pd has four children (nerr, errCode, loc, panic) */
#undef WHATFN
#define WHATFN "PDCI_structured_pd_children"
PDCI_node_t ** PDCI_structured_pd_children(PDCI_node_t *self)
{
  PDCI_structured_pd *pd = (PDCI_structured_pd *) self->rep;
  PDCI_node_t **result;
  if (!(result = PDCI_NEW_NODE_PTR_LIST(self->pdc, 4))) {
    failwith("ALLOC_ERROR: in PDCI_structured_pd_children");
  }
  
  /* the following mk calls raise an exception on alloc error */
  do {
    if (!(result[0] = PDCI_NEW_NODE((self)->pdc))) {
      failwith("ALLOC_ERROR: in " WHATFN);
    }
    result[0]->vt     = (&PDC_uint32_val_vtable);
    result[0]->pdc    = (self)->pdc;
    result[0]->parent = (self);
    result[0]->m      = (void *)(0);
    result[0]->pd     = (void *)(0);
    result[0]->rep    = (&(pd->nerr));
    result[0]->name   = ("nerr");
  } while (0);
  PDCI_MK_TNODE(result[0], &PDC_uint32_val_vtable, self, "nerr",    &(pd->nerr),    WHATFN);
  PDCI_MK_TNODE(result[1], &PDC_uint32_val_vtable, self, "errCode", &(pd->errCode), WHATFN);
  PDCI_MK_TNODE(result[2], &PDC_loc_t_vtable,      self, "loc",     &(pd->loc),     WHATFN);
  PDCI_MK_TNODE(result[3], &PDC_uint32_val_vtable, self, "panic",   &(pd->panic),   WHATFN);
  return result;
}

/* A sequenced_pd has six children 
  (nerr, errCode, loc, panic, neerr, firstError) */
#undef WHATFN
#define WHATFN "PDCI_sequenced_pd_children"
PDCI_node_t ** PDCI_sequenced_pd_children(PDCI_node_t *self)
{
  PDCI_sequenced_pd *pd = (PDCI_sequenced_pd *) self->rep;
  PDCI_node_t **result;
  if (!(result = PDCI_NEW_NODE_PTR_LIST(self->pdc, 6))) {
    failwith("ALLOC_ERROR: in PDCI_sequenced_pd_children");
  }
  /* the following mk calls raise an exception on alloc error */
  PDCI_MK_TNODE(result[0], &PDC_uint32_val_vtable, self, "nerr",     &(pd->nerr),       WHATFN);
  PDCI_MK_TNODE(result[1], &PDC_uint32_val_vtable, self, "errCode",  &(pd->errCode),    WHATFN);
  PDCI_MK_TNODE(result[2], &PDC_loc_t_vtable,      self, "loc",      &(pd->loc),        WHATFN);
  PDCI_MK_TNODE(result[3], &PDC_uint32_val_vtable, self, "panic",    &(pd->panic),      WHATFN);
  PDCI_MK_TNODE(result[4], &PDC_uint32_val_vtable, self, "neerr",    &(pd->neerr),      WHATFN);
  PDCI_MK_TNODE(result[5], &PDC_uint32_val_vtable, self, "firstErr", &(pd->firstError), WHATFN);
  return result;
}

/* All base types have two children, pd and val. */
/* The routine that constructed self has already placed all */
/* the necarry stuff in self->base_pd, self->base_val, and self->base_vt */
#undef WHATFN
#define WHATFN "PDCI_basetype_children"
PDCI_node_t ** PDCI_basetype_children(PDCI_node_t *self)
{
  PDCI_node_t **result;
  if (!(result = PDCI_NEW_NODE_PTR_LIST(self->pdc, 2))) {
    failwith("ALLOC_ERROR: in " WHATFN);
  }
  /* the following mk calls raise an exception on alloc error */
  PDCI_MK_TNODE(result[0], &PDC_base_pd_vtable, self, "pd",  self->base_pd,  WHATFN);
  PDCI_MK_TNODE(result[1], self->base_vt,        self, "val", self->base_val, WHATFN);
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

const PDCI_vtable_t
PDCI_basetype_vtable = {PDCI_basetype_children,
			PDCI_error_typed_value,
			0};

const PDCI_vtable_t
PDCI_base_pd_vtable = {PDC_base_pd_children,
		       PDCI_error_typed_value,
		       0};

const PDCI_vtable_t
PDCI_loc_t_vtable = {PDC_loc_t_children,
		     PDCI_error_typed_value,
		     0};

const PDCI_vtable_t
PDCI_pos_t_vtable = {PDC_pos_t_children,
		     PDCI_error_typed_value,
		     0};

/* BASE TYPE SUPPORT */
/* 1. Define a vtable and val_vtable for each base type */
/* 2. Define bty_typed_value */
/* 3. Define bty_val_typed_value */

value PDC_uint32_typed_value (void * ocaml_n)
{
  PDCI_node_t *n = (PDCI_node_t *) ocaml_n; 
  PDC_uint32 r = *((PDC_uint32 *)n->rep);
 
  /*
  call macro provided my mary to create an ocaml value.
  macro will come from/follow pattern from c_api portion of galax.
  galax/galapi/c_api
  whether to gc protect it is open question.
  Xavier's email is relevant.
  ...
  */
  /* XXX_TODO */
  r = r; /* XXX_REMOVE */
  return 0;
}
