/*
 * Implementation of
 *    public and private APIs, galax-pads
 * 
 * Kathleen Fisher, Robert Gruber
 * AT&T Labs Research
 */

#include "pglx.h"
#include "pglx-internal.h"


/* ocaml header files can be found in /usr/common/lib/ocaml/caml */

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

/* Helper macros  */

#define PDCI_IMPL_BASE_VT(ty) \
/* base types have two children, pd and val. */ \
PDCI_node_t ** ty ## _children(PDCI_node_t *self) \
{ \
  ty           *rep = (ty*)self->rep; \
  PDC_base_pd  *pd  = (PDC_base_pd*)self->pd; \
  PDCI_node_t **result; \
  if (!(result = PDCI_NEW_NODE_PTR_LIST(self->pdc, 2))) { \
    failwith("ALLOC_ERROR: in " PDCI_MacroArg2String(ty) "_children"); \
  } \
  /* the following mk calls raise an exception on alloc error */ \
  PDCI_MK_TNODE(result[0], & PDC_base_pd_vtable,  self, "pd",  pd,  PDCI_MacroArg2String(ty) "_children"); \
  PDCI_MK_TNODE(result[1], & ty ## _val_vtable,   self, "val", rep, PDCI_MacroArg2String(ty) "_children"); \
  return result; \
} \
 \
const PDCI_vtable_t ty ## _vtable = {ty ## _children, \
				     PDCI_error_typed_value, \
				     0}

#define PDCI_IMPL_BASE_VAL_VT(ty) \
/* node->rep is a pointer to a ty */ \
value ty ## _typed_value (PDCI_node_t *node) \
{ \
  /* XXX_TODO: invoke Galax-provided macro for producing a value containing a ty */ \
  /* For now, raise exception containing the value as as string. */ \
  ty          *r   = (ty*)node->rep; \
  PDC_base_pd *pd  = (PDC_base_pd*)node->pd; \
  PDC_base_pd  tpd; \
  if (!pd) { \
    pd = &tpd; \
    pd->errCode = PDC_NO_ERR; \
  } \
  sfstrset(node->pdc->tmp2, 0); \
  if (-1 == ty ## _write2io(node->pdc, node->pdc->tmp2, pd, r)) { \
    failwith("UNEXPECTED_IO_FAILURE in base type typed_value function"); \
  } \
  failwith(sfstruse(node->pdc->tmp2)); \
  return 0;  /* will never get here*/ \
} \
 \
const PDCI_vtable_t ty ## _val_vtable = {PDCI_no_children, \
				         ty ## _typed_value, \
				         0}

/* For the case where a base type requires an arg, such as stop char for PDC_string */
#define PDCI_IMPL_BASE_VAL_VT_ARG1(ty, ty_arg1) \
/* node->rep is a pointer to a ty */ \
value ty ## _typed_value (PDCI_node_t *node) \
{ \
  /* XXX_TODO: invoke Galax-provided macro for producing a value containing a ty */ \
  /* For now, raise exception containing the value as a string. */ \
  ty          *r   = (ty*)node->rep; \
  PDC_base_pd *pd  = (PDC_base_pd*)node->pd; \
  PDC_base_pd  tpd; \
  if (!pd) { \
    pd = &tpd; \
    pd->errCode = PDC_NO_ERR; \
  } \
  sfstrset(node->pdc->tmp2, 0); \
  if (-1 == ty ## _write2io(node->pdc, node->pdc->tmp2, ty_arg1, pd, r)) { \
    failwith("UNEXPECTED_IO_FAILURE in base type typed_value function"); \
  } \
  failwith(sfstruse(node->pdc->tmp2)); \
  return 0;  /* will never get here*/ \
} \
 \
const PDCI_vtable_t ty ## _val_vtable = {PDCI_no_children, \
				         ty ## _typed_value, \
				         0}

/* HELPERS */

/* ---------------------------
 * Some children functions
 * --------------------------- */

/* A pos_t has 3 children (byte, num, and unit) */
#undef WHATFN
#define WHATFN "PDC_pos_t_children"
PDCI_node_t ** PDC_pos_t_children(PDCI_node_t *self)
{
  PDC_pos_t *pos = (PDC_pos_t *) self->rep;
  PDCI_node_t **result;
  if (!(result = PDCI_NEW_NODE_PTR_LIST(self->pdc, 3))) {
    failwith("ALLOC_ERROR: in " WHATFN);
  }
  PDCI_MK_TNODE(result[0], &PDC_int32_val_vtable,   self, "byte",    &(pos->byte),     WHATFN);
  PDCI_MK_TNODE(result[1], &PDC_int32_val_vtable,   self, "num",     &(pos->num),      WHATFN);
  PDCI_MK_TNODE(result[2], &PDCI_Cstr_val_vtable,   self, "unit",    (char*)pos->unit, WHATFN);
  return result;
}

/* A loc_t has 2 children (b and e) */
#undef WHATFN
#define WHATFN "PDC_loc_t_children"
PDCI_node_t ** PDC_loc_t_children(PDCI_node_t *self)
{
  PDC_loc_t *loc = (PDC_loc_t *) self->rep;
  PDCI_node_t **result;
  if (!(result = PDCI_NEW_NODE_PTR_LIST(self->pdc, 2))) {
    failwith("ALLOC_ERROR: in " WHATFN);
  }
  PDCI_MK_TNODE(result[0], &PDC_pos_t_vtable,      self, "b",     &(loc->b),     WHATFN);
  PDCI_MK_TNODE(result[1], &PDC_pos_t_vtable,      self, "e",     &(loc->e),     WHATFN);
  return result;
}

/* A base_pd has three children (pstate, errCode, loc) */
#undef WHATFN
#define WHATFN "PDC_base_pd_children"
PDCI_node_t ** PDC_base_pd_children(PDCI_node_t *self)
{
  int            i = 0;
  PDC_base_pd   *pd = (PDC_base_pd *) self->rep;
  PDCI_node_t  **result;

  if (!(result = PDCI_NEW_NODE_PTR_LIST(self->pdc, 3))) {
    failwith("ALLOC_ERROR: in " WHATFN);
  }
  PDCI_MK_TNODE(result[i], &PDC_uint32_val_vtable, self, "pstate",  &(pd->pstate),  WHATFN); i++;
  PDCI_MK_TNODE(result[i], &PDC_uint32_val_vtable, self, "errCode", &(pd->errCode), WHATFN); i++;
  if (pd->errCode >= 100) {
    PDCI_MK_TNODE(result[i], &PDC_loc_t_vtable,    self, "loc",     &(pd->loc),     WHATFN); i++;
  } else {
    result[i] = 0; i++;
  }
  return result;
}

/* A structured_pd has four children (pstate, errCode, loc, nerr) */
#undef WHATFN
#define WHATFN "PDCI_structured_pd_children"
PDCI_node_t ** PDCI_structured_pd_children(PDCI_node_t *self)
{
  int                  i = 0;
  PDCI_structured_pd  *pd = (PDCI_structured_pd *) self->rep;
  PDCI_node_t        **result;

  if (!(result = PDCI_NEW_NODE_PTR_LIST(self->pdc, 4))) {
    failwith("ALLOC_ERROR: in " WHATFN);
  }
  /* the following mk calls raise an exception on alloc error */
  PDCI_MK_TNODE(result[i], &PDC_uint32_val_vtable, self, "pstate",  &(pd->pstate),  WHATFN); i++;
  PDCI_MK_TNODE(result[i], &PDC_uint32_val_vtable, self, "errCode", &(pd->errCode), WHATFN); i++;
  if (pd->errCode >= 100) {
    PDCI_MK_TNODE(result[i], &PDC_loc_t_vtable,    self, "loc",     &(pd->loc),     WHATFN); i++;
  } else {
    result[i] = 0; i++;
  }
  PDCI_MK_TNODE(result[i], &PDC_uint32_val_vtable, self, "nerr",    &(pd->nerr),    WHATFN); i++;
  return result;
}

/* A sequenced_pd has six children 
  (pstate, errCode, loc, nerr, neerr, firstError) */
#undef WHATFN
#define WHATFN "PDCI_sequenced_pd_children"
PDCI_node_t ** PDCI_sequenced_pd_children(PDCI_node_t *self)
{
  int                 i = 0;
  PDCI_sequenced_pd  *pd = (PDCI_sequenced_pd *) self->rep;
  PDCI_node_t       **result;

  if (!(result = PDCI_NEW_NODE_PTR_LIST(self->pdc, 6))) {
    failwith("ALLOC_ERROR: in " WHATFN);
  }
  /* the following mk calls raise an exception on alloc error */
  PDCI_MK_TNODE(result[i], &PDC_uint32_val_vtable, self, "pstate",   &(pd->pstate),     WHATFN); i++;
  PDCI_MK_TNODE(result[i], &PDC_uint32_val_vtable, self, "errCode",  &(pd->errCode),    WHATFN); i++;
  if (pd->errCode >= 100) {
    PDCI_MK_TNODE(result[i], &PDC_loc_t_vtable,    self, "loc",     &(pd->loc),     WHATFN); i++;
  } else {
    result[i] = 0; i++;
  }
  PDCI_MK_TNODE(result[i], &PDC_uint32_val_vtable, self, "nerr",     &(pd->nerr),       WHATFN); i++;
  PDCI_MK_TNODE(result[i], &PDC_uint32_val_vtable, self, "neerr",    &(pd->neerr),      WHATFN); i++;
  PDCI_MK_TNODE(result[i], &PDC_uint32_val_vtable, self, "firstErr", &(pd->firstError), WHATFN); i++;
  return result;
}

/* Used for any node with no children */
#undef WHATFN
#define WHATFN "PDCI_no_children"
PDCI_node_t ** PDCI_no_children(PDCI_node_t *self)
{
  PDCI_node_t **result;
  if (!(result = PDCI_NEW_NODE_PTR_LIST(self->pdc, 0))) {
    failwith("ALLOC_ERROR: in " WHATFN);
  }
  return result;
}

/* ---------------------------
 * Some typed_value functions
 * --------------------------- */

/* Error function used for many cases */
value PDCI_error_typed_value(PDCI_node_t *node)
{
  failwith("NOT_A_VALUE: typed_value called on structured type.");
  return 0;  /* will never get here*/
} 

/* node->rep is a C-style string (const char *) */
value PDCI_Cstr_typed_value(PDCI_node_t *node)
{
  const char * s = (const char *)node->rep;
  /* XXX_TODO: invoke Galax-provided macro for producing a value containing a Galax string */
  /* For now, raise exception containing the value as a string. */
  sfstrset(node->pdc->tmp1, 0);
  if (-1 == sfprintf(node->pdc->tmp1, "%s", s)) {
    failwith("UNEXPECTED_IO_FAILURE in PDCI_Cstr_typed_value");
  }
  failwith(sfstruse(node->pdc->tmp1));
  return 0;  /* will never get here*/
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
PDC_base_pd_vtable = {PDC_base_pd_children,
		      PDCI_error_typed_value,
		      0};

const PDCI_vtable_t
PDC_loc_t_vtable = {PDC_loc_t_children,
		    PDCI_error_typed_value,
		    0};

const PDCI_vtable_t
PDC_pos_t_vtable = {PDC_pos_t_children,
		    PDCI_error_typed_value,
		    0};

const PDCI_vtable_t
PDCI_Cstr_val_vtable = {PDCI_no_children,
			PDCI_Cstr_typed_value,
			0};

/* Impl some base type children and typed_value functions and
   associated vtable/val_vtable pairs */

PDCI_IMPL_BASE_VT(PDC_char);
PDCI_IMPL_BASE_VAL_VT(PDC_char);

PDCI_IMPL_BASE_VT(PDC_string);
PDCI_IMPL_BASE_VAL_VT_ARG1(PDC_string, ' ');

PDCI_IMPL_BASE_VT(PDC_int8);
PDCI_IMPL_BASE_VAL_VT(PDC_int8);

PDCI_IMPL_BASE_VT(PDC_int16);
PDCI_IMPL_BASE_VAL_VT(PDC_int16);

PDCI_IMPL_BASE_VT(PDC_int32);
PDCI_IMPL_BASE_VAL_VT(PDC_int32);

PDCI_IMPL_BASE_VT(PDC_int64);
PDCI_IMPL_BASE_VAL_VT(PDC_int64);

PDCI_IMPL_BASE_VT(PDC_uint8);
PDCI_IMPL_BASE_VAL_VT(PDC_uint8);

PDCI_IMPL_BASE_VT(PDC_uint16);
PDCI_IMPL_BASE_VAL_VT(PDC_uint16);

PDCI_IMPL_BASE_VT(PDC_uint32);
/* PDCI_IMPL_BASE_VAL_VT(PDC_uint32); */

PDCI_IMPL_BASE_VT(PDC_uint64);
PDCI_IMPL_BASE_VAL_VT(PDC_uint64);

/* XXX EXPANDED PDCI_IMPL_BASE_VAL_VT(PDC_uint32) here in case
 * XXX someone wants to play with doing a real implementation of
 * XXX PDC_uint32_typed_value
 */

/* node->rep is a pointer to a PDC_uint32 */
value PDC_uint32_typed_value (PDCI_node_t *node)
{
  /* XXX_TODO: invoke Galax-provided macro for producing a value containing an unsigned int */
  /* For now, raise exception containing the value as a string */
  PDC_uint32  *r   = (PDC_uint32*)node->rep;
  PDC_base_pd *pd  = (PDC_base_pd*)node->pd;
  PDC_base_pd  tpd;
  if (!pd) {
    pd = &tpd;
    pd->errCode = PDC_NO_ERR;
  }
  sfstrset(node->pdc->tmp2, 0);
  if (-1 == PDC_uint32_write2io(node->pdc, node->pdc->tmp2, pd, r)) {
    failwith("UNEXPECTED_IO_FAILURE in base type typed_value function");
  }
  failwith(sfstruse(node->pdc->tmp2));
  return 0;  /* will never get here*/
}

const PDCI_vtable_t PDC_uint32_val_vtable = {PDCI_no_children,
					     PDC_uint32_typed_value,
					     0};

