/*
 * Implementation of
 *    public and private APIs, galax-pads
 * 
 * Kathleen Fisher, Robert Gruber
 * AT&T Labs Research
 */

#include "pglx.h"
#include "pglx-internal.h"
#include <stdio.h>

/* ocaml header files can be found in /usr/common/lib/ocaml/caml */

/*
 * XXX TEMPORARY:
 */

static const char *walk_children_spaces = "                                                                                                                        ";

void walk_children(void *n, int indent) {
  void      **children, **iter;
  void       *child;

  if (strcmp(PGLX_generic_kind(n), "element") == 0 || strcmp(PGLX_generic_kind(n), "document") == 0) {
    const char *n_name = PGLX_generic_name(n);
    error(0, "%.*s<%s>",
	  indent, walk_children_spaces, n_name);
    children = PGLX_generic_children(n); 
    for (iter = children, child = *iter; child; child = *++iter) {
      walk_children(child, indent+4);
      /*      PGLX_node_free(child); */
    }
    error(0, "%.*s</%s>",
	  indent, walk_children_spaces, n_name);
  } else {
    error(0, "%.*s%s",
	  indent, walk_children_spaces, PGLX_generic_string_value(n));
  }
}

/* ================================================================================
 * PUBLIC GALAX->PADS CALLS (see pglx.h) */ 

nodeRepArray PGLX_generic_children (nodeRep ocaml_n)
{
  PDCI_node_t *n = (PDCI_node_t *) ocaml_n; 
  if (!n)
    failwith("PADS/Galax INVALID_PARAM: n null in " "PGLX_generic_children");
  if (!n->vt)
    failwith("PADS/Galax INVALID_PARAM: n->vt null in " "PGLX_generic_children");
  PDCI_NODE_VT_CHECK(n, "PGLX_generic_children");
  return (nodeRepArray) ((n->vt->children)(n));
}

nodeRep PGLX_generic_kth_child (nodeRep ocaml_n, childIndex idx)
{
  PDCI_node_t *n = (PDCI_node_t *) ocaml_n; 
  if (!n)
    failwith("PADS/Galax INVALID_PARAM: n null in " "PGLX_generic_kth_child");
  if (!n->vt)
    failwith("PADS/Galax INVALID_PARAM: n->vt null in " "PGLX_generic_kth_child");
  PDCI_NODE_VT_CHECK(n, "PGLX_generic_kth_child");
  return (nodeRep) ((n->vt->kth_child)(n, idx));
}

nodeRep PGLX_generic_kth_child_named (nodeRep ocaml_n, childIndex idx, const char *name)
{
  PDCI_node_t *n = (PDCI_node_t *) ocaml_n; 
  if (!n)
    failwith("PADS/Galax INVALID_PARAM: n null in " "PGLX_generic_kth_child_named");
  if (!n->vt)
    failwith("PADS/Galax INVALID_PARAM: n->vt null in " "PGLX_generic_kth_child_named");
  PDCI_NODE_VT_CHECK(n, "PGLX_generic_kth_child_named");
  return (nodeRep) ((n->vt->kth_child_named)(n, idx, name));
}

nodeRep PGLX_generic_parent (nodeRep ocaml_n)
{
  PDCI_node_t *n = (PDCI_node_t *) ocaml_n; 
  PDCI_NODE_CHECK(n, "PGLX_generic_parent");
  return (nodeRep ) (n->parent);
}

/* Return value TBD */

item PGLX_generic_typed_value (nodeRep  ocaml_n)
{
  PDCI_node_t *n = (PDCI_node_t *) ocaml_n; 
  PDCI_NODE_VT_CHECK(n, "PGLX_generic_typed_value");
  return (n->vt->typed_value)(n);
}

const char* PGLX_generic_string_value(nodeRep ocaml_n)
{
  PDCI_node_t *n = (PDCI_node_t *) ocaml_n; 
  PDCI_NODE_CHECK(n, "PGLX_generic_string_value");
  return (n->vt->string_value)(n);
}

const char* PGLX_generic_name(nodeRep ocaml_n){
  PDCI_node_t *n = (PDCI_node_t *) ocaml_n; 
  PDCI_NODE_CHECK(n, "PGLX_generic_name");
  return n->name;
}

const char* PGLX_generic_kind(nodeRep ocaml_n){
  PDCI_node_t *n = (PDCI_node_t *) ocaml_n; 
  PDCI_NODE_CHECK(n, "PGLX_generic_kind");
  return n->kind;
}

void PGLX_node_free(nodeRep ocaml_n)
{
  PDCI_FREE_NODE(ocaml_n);
}

void PGLX_nodelist_free(nodeRepArray list)
{
  PDCI_FREE_NODE_PTR_LIST(list);
}

/* ================================================================================
 * INTERNAL */

/* Helper macros  */


#define PDCI_IMPL_BASE_VT(ty) \
/* base types have two children, pd and val. */ \
PDCI_node_t ** ty ## _children(PDCI_node_t *self) \
{ \
  ty        *rep = (ty*)self->rep; \
  Pbase_pd  *pd  = (Pbase_pd*)self->pd; \
  PDCI_node_t **result; \
  if (!(result = PDCI_NEW_NODE_PTR_LIST(2))) { \
    failwith("PADS/Galax ALLOC_ERROR: in " PDCI_MacroArg2String(ty) "_children"); \
  } \
  /* the following mk calls raise an exception on alloc error */ \
  PDCI_MK_TNODE(result[0], & Pbase_pd_vtable,  self, "pd",  pd,  PDCI_MacroArg2String(ty) "_children"); \
  if (pd->errCode == P_NO_ERR || pd->errCode == P_USER_CONSTRAINT_VIOLATION) { \
    PDCI_MK_TNODE(result[1], & ty ## _val_vtable,   self, "val", rep, PDCI_MacroArg2String(ty) "_children"); \
  } \
  return result; \
} \
 \
PDCI_node_t * ty ## _kth_child(PDCI_node_t *self, childIndex idx) \
{ \
  ty        *rep = (ty*)self->rep; \
  Pbase_pd  *pd  = (Pbase_pd*)self->pd; \
  PDCI_node_t *result = 0; \
  /* the mk calls below raise an exception on alloc error */ \
  switch (idx) { \
    case 0: \
      PDCI_MK_TNODE(result, & Pbase_pd_vtable,  self, "pd",  pd,  PDCI_MacroArg2String(ty) "_kth_child"); \
      break; \
    case 1: \
      if (pd->errCode == P_NO_ERR || pd->errCode == P_USER_CONSTRAINT_VIOLATION) { \
        PDCI_MK_TNODE(result, & ty ## _val_vtable,   self, "val", rep, PDCI_MacroArg2String(ty) "_kth_child"); \
      } \
      break; \
  } \
  return result; \
} \
 \
PDCI_node_t * ty ## _kth_child_named(PDCI_node_t *self, childIndex idx, const char *name) \
{ \
  ty        *rep = (ty*)self->rep; \
  Pbase_pd  *pd  = (Pbase_pd*)self->pd; \
  PDCI_node_t *result = 0; \
  /* the only valid idx is 0  */ \
  if (idx) return 0; \
  /* the mk calls below raise an exception on alloc error */ \
  if (strcmp(name, "pd") == 0) { \
    PDCI_MK_TNODE(result, & Pbase_pd_vtable,  self, "pd",  pd,  PDCI_MacroArg2String(ty) "_kth_child_named"); \
  } else if (strcmp(name, "val") == 0) { \
      if (pd->errCode == P_NO_ERR || pd->errCode == P_USER_CONSTRAINT_VIOLATION) { \
        PDCI_MK_TNODE(result, & ty ## _val_vtable,   self, "val", rep, PDCI_MacroArg2String(ty) "_kth_child_named"); \
      } \
  } \
  return result; \
} \
 \
const PDCI_vtable_t ty ## _vtable = {ty ## _children, \
				     ty ## _kth_child, \
				     ty ## _kth_child_named, \
				     PDCI_error_typed_value, \
				     PDCI_not_impl_yet_string_value}

#define PDCI_IMPL_TYPED_VALUE(ty) \
item ty ## _typed_value (PDCI_node_t *node) \
{ \
  item         res = 0; \
  ty           *r   = (ty*)node->rep; \
  Pbase_pd  *pd  = (Pbase_pd*)node->pd; \
  Pbase_pd   tpd; \
  if (!pd) { \
    pd = &tpd; \
    pd->errCode = P_NO_ERR; \
  } \
  sfstrset(node->pads->tmp2, 0); \
  if (-1 == ty ## _write2io(node->pads, node->pads->tmp2, pd, r)) { \
    failwith("PADS/Galax UNEXPECTED_IO_FAILURE in " PDCI_MacroArg2String(ty) "_typed_value"); \
  } \
  if (galax_atomicUntyped(sfstruse(node->pads->tmp2), &res)) { \
    failwith("PADS/Galax UNEXPECTED_GALAX_VALUE_WRAP_FAILURE in " PDCI_MacroArg2String(ty) "_typed_value"); \
  } \
  return res; \
}

#define PDCI_IMPL_TYPED_VALUE_INT(ty) \
item ty ## _typed_value (PDCI_node_t *node) \
{ \
  item       res = 0; \
  int        r   = *((ty*)node->rep); \
  Pbase_pd  *pd  = (Pbase_pd*)node->pd; \
  Pbase_pd   tpd; \
  if (!pd) { \
    pd = &tpd; \
    pd->errCode = P_NO_ERR; \
  } \
  if (galax_atomicInt(r, &res)) { \
    failwith("PADS/Galax UNEXPECTED_GALAX_VALUE_WRAP_FAILURE in " PDCI_MacroArg2String(ty) "_typed_value"); \
  } \
  return res; \
}

/* XXX should use long long */
#define PDCI_IMPL_TYPED_VALUE_INTEGER(ty) \
item ty ## _typed_value (PDCI_node_t *node) \
{ \
  item         res = 0; \
  int          r   = *((ty*)node->rep); \
  Pbase_pd  *pd  = (Pbase_pd*)node->pd; \
  Pbase_pd   tpd; \
  if (!pd) { \
    pd = &tpd; \
    pd->errCode = P_NO_ERR; \
  } \
  if (galax_atomicInteger(r, &res)) { \
    failwith("PADS/Galax UNEXPECTED_GALAX_VALUE_WRAP_FAILURE in " PDCI_MacroArg2String(ty) "_typed_value"); \
  } \
  return res; \
}

#define PDCI_IMPL_BASE_VAL_VT(ty) \
/* val node has one child, a text node with no name */ \
PDCI_node_t ** ty ## _val_children(PDCI_node_t *self) \
{ \
  PDCI_node_t **result; \
  if (!(result = PDCI_NEW_NODE_PTR_LIST(1))) { \
    failwith("PADS/Galax ALLOC_ERROR: in " PDCI_MacroArg2String(ty) "_val_children"); \
  } \
  /* the following mk call raises an exception on alloc error */ \
  PDCI_MK_TEXTNODE(result[0], & ty ## _text_vtable,  self, PDCI_MacroArg2String(ty) "_val_children"); \
  return result; \
} \
 \
PDCI_node_t * ty ## _val_kth_child(PDCI_node_t *self, childIndex idx) \
{ \
  PDCI_node_t *result = 0; \
  /* the only valid idx is 0  */ \
  if (idx) return 0; \
  /* the following mk call raises an exception on alloc error */ \
  PDCI_MK_TEXTNODE(result, & ty ## _text_vtable,  self, PDCI_MacroArg2String(ty) "_val_kth_child"); \
  return result; \
} \
 \
const char * ty ## _string_value (PDCI_node_t *node) \
{ \
  ty        *r   = (ty*)node->rep; \
  Pbase_pd  *pd  = (Pbase_pd*)node->pd; \
  Pbase_pd   tpd; \
  if (!pd) { \
    pd = &tpd; \
    pd->errCode = P_NO_ERR; \
  } \
  sfstrset(node->pads->tmp2, 0); \
  if (-1 == ty ## _write2io(node->pads, node->pads->tmp2, pd, r)) { \
    failwith("PADS/Galax UNEXPECTED_IO_FAILURE in " PDCI_MacroArg2String(ty) "_typed_value"); \
  } \
  return (sfstruse(node->pads->tmp2)); \
} \
 \
item ty ## _text_typed_value (PDCI_node_t *node) \
{ \
  item         res = 0; \
  ty           *r   = (ty*)node->rep; \
  Pbase_pd  *pd  = (Pbase_pd*)node->pd; \
  Pbase_pd   tpd; \
  if (!pd) { \
    pd = &tpd; \
    pd->errCode = P_NO_ERR; \
  } \
  sfstrset(node->pads->tmp2, 0); \
  if (-1 == ty ## _write2io(node->pads, node->pads->tmp2, pd, r)) { \
    failwith("PADS/Galax UNEXPECTED_IO_FAILURE in " PDCI_MacroArg2String(ty) "_typed_value"); \
  } \
  if (galax_atomicUntyped(sfstruse(node->pads->tmp2), &res)) { \
    failwith("PADS/Galax UNEXPECTED_GALAX_VALUE_WRAP_FAILURE in " PDCI_MacroArg2String(ty) "_typed_value"); \
  } \
  return res; \
} \
 \
const PDCI_vtable_t ty ## _val_vtable = {ty ## _val_children, \
				         ty ## _val_kth_child, \
				         PDCI_no_kth_child_named, /* no named children */ \
				         ty ## _typed_value, \
				         ty ## _string_value}; \
 \
const PDCI_vtable_t ty ## _text_vtable = {PDCI_no_children, \
				          PDCI_no_kth_child, \
				          PDCI_no_kth_child_named, \
				          ty ## _text_typed_value, \
				          ty ## _string_value}

/* For the case where a base type requires an arg, such as stop char for Pstring */

#define PDCI_IMPL_TYPED_VALUE_ARG1(ty, ty_arg1) \
item ty ## _typed_value (PDCI_node_t *node) \
{ \
  item         res = 0; \
  ty           *r   = (ty*)node->rep; \
  Pbase_pd  *pd  = (Pbase_pd*)node->pd; \
  Pbase_pd   tpd; \
  if (!pd) { \
    pd = &tpd; \
    pd->errCode = P_NO_ERR; \
  } \
  sfstrset(node->pads->tmp2, 0); \
  if (-1 == ty ## _write2io(node->pads, node->pads->tmp2, ty_arg1, pd, r)) { \
    failwith("PADS/Galax UNEXPECTED_IO_FAILURE in " PDCI_MacroArg2String(ty) "_typed_value"); \
  } \
  if (galax_atomicUntyped(sfstruse(node->pads->tmp2), &res)) { \
    failwith("PADS/Galax UNEXPECTED_GALAX_VALUE_WRAP_FAILURE in " PDCI_MacroArg2String(ty) "_typed_value"); \
  } \
  return res; \
}

#define PDCI_IMPL_BASE_VAL_VT_ARG1(ty, ty_arg1) \
/* val node has one child, a text node with no name */ \
PDCI_node_t ** ty ## _val_children(PDCI_node_t *self) \
{ \
  PDCI_node_t **result; \
  if (!(result = PDCI_NEW_NODE_PTR_LIST(1))) { \
    failwith("PADS/Galax ALLOC_ERROR: in " PDCI_MacroArg2String(ty) "_val_children"); \
  } \
  /* the following mk call raises an exception on alloc error */ \
  PDCI_MK_TEXTNODE(result[0], & ty ## _text_vtable,  self, PDCI_MacroArg2String(ty) "_val_children"); \
  return result; \
} \
 \
PDCI_node_t * ty ## _val_kth_child(PDCI_node_t *self, childIndex idx) \
{ \
  PDCI_node_t *result = 0; \
  /* the only valid idx is 0  */ \
  if (idx) return 0; \
  /* the following mk call raises an exception on alloc error */ \
  PDCI_MK_TEXTNODE(result, & ty ## _text_vtable,  self, PDCI_MacroArg2String(ty) "_val_kth_child"); \
  return result; \
} \
 \
const char * ty ## _string_value (PDCI_node_t *node) \
{ \
  ty           *r   = (ty*)node->rep; \
  Pbase_pd  *pd  = (Pbase_pd*)node->pd; \
  Pbase_pd   tpd; \
  if (!pd) { \
    pd = &tpd; \
    pd->errCode = P_NO_ERR; \
  } \
  sfstrset(node->pads->tmp2, 0); \
  if (-1 == ty ## _write2io(node->pads, node->pads->tmp2, ty_arg1, pd, r)) { \
    failwith("PADS/Galax UNEXPECTED_IO_FAILURE in " PDCI_MacroArg2String(ty) "_typed_value"); \
  } \
  return (sfstruse(node->pads->tmp2)); \
} \
 \
item ty ## _text_typed_value (PDCI_node_t *node) \
{ \
  item         res = 0; \
  ty           *r   = (ty*)node->rep; \
  Pbase_pd  *pd  = (Pbase_pd*)node->pd; \
  Pbase_pd   tpd; \
  if (!pd) { \
    pd = &tpd; \
    pd->errCode = P_NO_ERR; \
  } \
  sfstrset(node->pads->tmp2, 0); \
  if (-1 == ty ## _write2io(node->pads, node->pads->tmp2, ty_arg1, pd, r)) { \
    failwith("PADS/Galax UNEXPECTED_IO_FAILURE in " PDCI_MacroArg2String(ty) "_typed_value"); \
  } \
  if (galax_atomicUntyped(sfstruse(node->pads->tmp2), &res)) { \
    failwith("PADS/Galax UNEXPECTED_GALAX_VALUE_WRAP_FAILURE in " PDCI_MacroArg2String(ty) "_typed_value"); \
  } \
  return res; \
} \
 \
const PDCI_vtable_t ty ## _val_vtable = {ty ## _val_children, \
				         ty ## _val_kth_child, \
				         PDCI_no_kth_child_named, /* no named children */ \
				         ty ## _typed_value, \
				         ty ## _string_value}; \
 \
const PDCI_vtable_t ty ## _text_vtable = {PDCI_no_children, \
				          PDCI_no_kth_child, \
				          PDCI_no_kth_child_named, \
				          ty ## _text_typed_value, \
				          ty ## _string_value}

/* HELPERS */

/* ---------------------------
 * Some children functions
 * --------------------------- */

/* A pos_t has 3 children (byte, num, and sfio-offset) */
#undef WHATFN
#define WHATFN "Ppos_t_children"
PDCI_node_t ** Ppos_t_children(PDCI_node_t *self)
{
  Ppos_t *pos = (Ppos_t *) self->rep;
  PDCI_node_t **result;
  if (!(result = PDCI_NEW_NODE_PTR_LIST(3))) {
    failwith("PADS/Galax ALLOC_ERROR: in " WHATFN);
  }
  PDCI_MK_TNODE(result[0], &Pint32_val_vtable,   self, "byte",    &(pos->byte),     WHATFN);
  PDCI_MK_TNODE(result[1], &Pint32_val_vtable,   self, "num",     &(pos->num),      WHATFN);
  /*  PDCI_MK_TNODE(result[2], &Puint64_val_vtable,  self, "offset",  (Puint64)(pos->offset), WHATFN); */
  return result;
}

#undef WHATFN
#define WHATFN "Ppos_t_kth_child"
PDCI_node_t * Ppos_t_kth_child(PDCI_node_t *self, childIndex idx)
{
  Ppos_t *pos = (Ppos_t *) self->rep;
  PDCI_node_t *result = 0;
  switch (idx) {
  case 0:
    PDCI_MK_TNODE(result, &Pint32_val_vtable,   self, "byte",    &(pos->byte),     WHATFN);
    break;
  case 1:
    PDCI_MK_TNODE(result, &Pint32_val_vtable,   self, "num",     &(pos->num),      WHATFN);
    break;
  case 2:
    /*  PDCI_MK_TNODE(result[2], &Puint64_val_vtable,  self, "offset",  (Puint64)(pos->offset), WHATFN); */
    break;
  }
  return result;
}

#undef WHATFN
#define WHATFN "Ppos_t_kth_child_named"
PDCI_node_t * Ppos_t_kth_child_named(PDCI_node_t *self, childIndex idx, const char *name)
{
  Ppos_t *pos = (Ppos_t *) self->rep;
  PDCI_node_t *result = 0;
  /* the only valid idx is 0 */
  if (idx) return 0;
  if (strcmp(name, "byte") == 0) {
    PDCI_MK_TNODE(result, &Pint32_val_vtable,   self, "byte",    &(pos->byte),     WHATFN);
  } else if (strcmp(name, "num") == 0) {
    PDCI_MK_TNODE(result, &Pint32_val_vtable,   self, "num",     &(pos->num),      WHATFN);
  } else if (strcmp(name, "offset") == 0) {
    /*  PDCI_MK_TNODE(result[2], &Puint64_val_vtable,  self, "offset",  (Puint64)(pos->offset), WHATFN); */
  }
  return result;
}

/* A loc_t has 2 children (b and e) */
#undef WHATFN
#define WHATFN "Ploc_t_children"
PDCI_node_t ** Ploc_t_children(PDCI_node_t *self)
{
  Ploc_t *loc = (Ploc_t *) self->rep;
  PDCI_node_t **result;
  if (!(result = PDCI_NEW_NODE_PTR_LIST(2))) {
    failwith("PADS/Galax ALLOC_ERROR: in " WHATFN);
  }
  PDCI_MK_TNODE(result[0], &Ppos_t_vtable,      self, "b",     &(loc->b),     WHATFN);
  PDCI_MK_TNODE(result[1], &Ppos_t_vtable,      self, "e",     &(loc->e),     WHATFN);
  return result;
}

#undef WHATFN
#define WHATFN "Ploc_t_kth_child"
PDCI_node_t * Ploc_t_kth_child(PDCI_node_t *self, childIndex idx)
{
  Ploc_t *loc = (Ploc_t *) self->rep;
  PDCI_node_t *result = 0;
  switch (idx) {
  case 0:
    PDCI_MK_TNODE(result, &Ppos_t_vtable,      self, "b",     &(loc->b),     WHATFN);
    break;
  case 1:
    PDCI_MK_TNODE(result, &Ppos_t_vtable,      self, "e",     &(loc->e),     WHATFN);
    break;
  }
  return result;
}

#undef WHATFN
#define WHATFN "Ploc_t_kth_child_named"
PDCI_node_t * Ploc_t_kth_child_named(PDCI_node_t *self, childIndex idx, const char *name)
{
  Ploc_t *loc = (Ploc_t *) self->rep;
  PDCI_node_t *result = 0;
  /* the only valid idx is 0 */
  if (idx) return 0;
  if (strcmp(name, "b") == 0) {
    PDCI_MK_TNODE(result, &Ppos_t_vtable,      self, "b",     &(loc->b),     WHATFN);
  } else if (strcmp(name, "e") == 0) {
    PDCI_MK_TNODE(result, &Ppos_t_vtable,      self, "e",     &(loc->e),     WHATFN);
  }
  return result;
}

/* A base_pd has three children (pstate, errCode, loc) */
#undef WHATFN
#define WHATFN "Pbase_pd_children"
PDCI_node_t ** Pbase_pd_children(PDCI_node_t *self)
{
  int            i = 0;
  Pbase_pd      *pd = (Pbase_pd *) self->rep;
  PDCI_node_t  **result;

  if (!(result = PDCI_NEW_NODE_PTR_LIST(3))) {
    failwith("PADS/Galax ALLOC_ERROR: in " WHATFN);
  }
  PDCI_MK_TNODE(result[i], &Puint32_val_vtable, self, "pstate",  &(pd->pstate),  WHATFN); i++;
  PDCI_MK_TNODE(result[i], &Puint32_val_vtable, self, "errCode", &(pd->errCode), WHATFN); i++;
  if (pd->errCode >= 100) {
    PDCI_MK_TNODE(result[i], &Ploc_t_vtable,    self, "loc",     &(pd->loc),     WHATFN); i++;
  } else {
    result[i] = 0; i++;
  }
  return result;
}

#undef WHATFN
#define WHATFN "Pbase_pd_kth_child"
PDCI_node_t * Pbase_pd_kth_child(PDCI_node_t *self, childIndex idx)
{
  Pbase_pd     *pd = (Pbase_pd *) self->rep;
  PDCI_node_t  *result = 0;
  switch (idx) {
  case 0:
    PDCI_MK_TNODE(result, &Puint32_val_vtable, self, "pstate",  &(pd->pstate),  WHATFN);
    break;
  case 1:
    PDCI_MK_TNODE(result, &Puint32_val_vtable, self, "errCode", &(pd->errCode), WHATFN);
    break;
  case 2:
    if (pd->errCode >= 100) {
      PDCI_MK_TNODE(result, &Ploc_t_vtable,    self, "loc",     &(pd->loc),     WHATFN);
    }
    break;
  }
  return result;
}

#undef WHATFN
#define WHATFN "Pbase_pd_kth_child_named"
PDCI_node_t * Pbase_pd_kth_child_named(PDCI_node_t *self, childIndex idx, const char *name)
{
  Pbase_pd     *pd = (Pbase_pd *) self->rep;
  PDCI_node_t  *result = 0;
  /* the only valid idx is 0 */
  if (idx) return 0;
  if (strcmp(name, "pstate") == 0) {
    PDCI_MK_TNODE(result, &Puint32_val_vtable, self, "pstate",  &(pd->pstate),  WHATFN);
  } else if (strcmp(name, "errCode") == 0) {
    PDCI_MK_TNODE(result, &Puint32_val_vtable, self, "errCode", &(pd->errCode), WHATFN);
  } else if (strcmp(name, "loc") == 0) {
    if (pd->errCode >= 100) {
      PDCI_MK_TNODE(result, &Ploc_t_vtable,    self, "loc",     &(pd->loc),     WHATFN);
    }
  }
  return result;
}

/* A structured_pd has four children (pstate, nerr, errCode, loc) */
#undef WHATFN
#define WHATFN "PDCI_structured_pd_children"
PDCI_node_t ** PDCI_structured_pd_children(PDCI_node_t *self)
{
  int                  i = 0;
  PDCI_structured_pd  *pd = (PDCI_structured_pd *) self->rep;
  PDCI_node_t        **result;

  if (!(result = PDCI_NEW_NODE_PTR_LIST(4))) {
    failwith("PADS/Galax ALLOC_ERROR: in " WHATFN);
  }
  /* the following mk calls raise an exception on alloc error */
  PDCI_MK_TNODE(result[i], &Puint32_val_vtable, self, "pstate",  &(pd->pstate),  WHATFN); i++;
  PDCI_MK_TNODE(result[i], &Puint32_val_vtable, self, "nerr",    &(pd->nerr),    WHATFN); i++;
  PDCI_MK_TNODE(result[i], &Puint32_val_vtable, self, "errCode", &(pd->errCode), WHATFN); i++;
  if (pd->errCode >= 100) {
    PDCI_MK_TNODE(result[i], &Ploc_t_vtable,    self, "loc",     &(pd->loc),     WHATFN); i++;
  }
  return result;
}

#undef WHATFN
#define WHATFN "PDCI_structured_pd_kth_child"
PDCI_node_t * PDCI_structured_pd_kth_child(PDCI_node_t *self, childIndex idx)
{
  PDCI_structured_pd  *pd = (PDCI_structured_pd *) self->rep;
  PDCI_node_t         *result = 0;
  /* the following mk calls raise an exception on alloc error */
  switch (idx) {
  case 0:
    PDCI_MK_TNODE(result, &Puint32_val_vtable, self, "pstate",  &(pd->pstate),  WHATFN);
    break;
  case 1:
    PDCI_MK_TNODE(result, &Puint32_val_vtable, self, "nerr",    &(pd->nerr),    WHATFN);
    break;
  case 2:
    PDCI_MK_TNODE(result, &Puint32_val_vtable, self, "errCode", &(pd->errCode), WHATFN);
    break;
  case 3:
    if (pd->errCode >= 100) {
      PDCI_MK_TNODE(result, &Ploc_t_vtable,    self, "loc",     &(pd->loc),     WHATFN);
    }
    break;
  }
  return result;
}

#undef WHATFN
#define WHATFN "PDCI_structured_pd_kth_child_named"
PDCI_node_t * PDCI_structured_pd_kth_child_named(PDCI_node_t *self, childIndex idx, const char * name)
{
  PDCI_structured_pd  *pd = (PDCI_structured_pd *) self->rep;
  PDCI_node_t         *result = 0;
  /* the only valid idx is 0 */
  if (idx) return 0;
  /* the following mk calls raise an exception on alloc error */
  if (strcmp(name, "pstate") == 0) {
    PDCI_MK_TNODE(result, &Puint32_val_vtable, self, "pstate",  &(pd->pstate),  WHATFN);
  } else if (strcmp(name, "nerr") == 0) {
    PDCI_MK_TNODE(result, &Puint32_val_vtable, self, "nerr",    &(pd->nerr),    WHATFN);
  } else if (strcmp(name, "errCode") == 0) {
    PDCI_MK_TNODE(result, &Puint32_val_vtable, self, "errCode", &(pd->errCode), WHATFN);
  } else if (strcmp(name, "loc") == 0) {
    if (pd->errCode >= 100) {
      PDCI_MK_TNODE(result, &Ploc_t_vtable,    self, "loc",     &(pd->loc),     WHATFN);
    }
  }
  return result;
}

/* A sequenced_pd has six children 
  (pstate, nerr, errCode, loc, neerr, firstError) */
#undef WHATFN
#define WHATFN "PDCI_sequenced_pd_children"
PDCI_node_t ** PDCI_sequenced_pd_children(PDCI_node_t *self)
{
  int                 i = 0;
  PDCI_sequenced_pd  *pd = (PDCI_sequenced_pd *) self->rep;
  PDCI_node_t       **result;

  if (!(result = PDCI_NEW_NODE_PTR_LIST(6))) {
    failwith("PADS/Galax ALLOC_ERROR: in " WHATFN);
  }
  /* the following mk calls raise an exception on alloc error */
  PDCI_MK_TNODE(result[i], &Puint32_val_vtable, self, "pstate",   &(pd->pstate),     WHATFN); i++;
  PDCI_MK_TNODE(result[i], &Puint32_val_vtable, self, "nerr",     &(pd->nerr),       WHATFN); i++;
  PDCI_MK_TNODE(result[i], &Puint32_val_vtable, self, "errCode",  &(pd->errCode),    WHATFN); i++;
  if (pd->errCode >= 100) {
    PDCI_MK_TNODE(result[i], &Ploc_t_vtable,    self, "loc",     &(pd->loc),     WHATFN); i++;
  } else {
    result[i] = 0; i++;
  }
  PDCI_MK_TNODE(result[i], &Puint32_val_vtable, self, "neerr",    &(pd->neerr),      WHATFN); i++;
  PDCI_MK_TNODE(result[i], &Puint32_val_vtable, self, "firstErr", &(pd->firstError), WHATFN); i++;
  return result;
}

#undef WHATFN
#define WHATFN "PDCI_sequenced_pd_kth_child"
PDCI_node_t * PDCI_sequenced_pd_kth_child(PDCI_node_t *self, childIndex idx)
{
  PDCI_sequenced_pd  *pd = (PDCI_sequenced_pd *) self->rep;
  PDCI_node_t        *result = 0;
  /* the following mk calls raise an exception on alloc error */
  if (pd->errCode < 100) { /* do not include loc */
    switch (idx) {
    case 0:
      PDCI_MK_TNODE(result, &Puint32_val_vtable, self, "pstate",   &(pd->pstate),     WHATFN);
      break;
    case 1:
      PDCI_MK_TNODE(result, &Puint32_val_vtable, self, "nerr",     &(pd->nerr),       WHATFN);
      break;
    case 2:
      PDCI_MK_TNODE(result, &Puint32_val_vtable, self, "errCode",  &(pd->errCode),    WHATFN);
      break;
    case 3:
      PDCI_MK_TNODE(result, &Puint32_val_vtable, self, "neerr",    &(pd->neerr),      WHATFN);
      break;
    case 4:
      PDCI_MK_TNODE(result, &Puint32_val_vtable, self, "firstErr", &(pd->firstError), WHATFN);
      break;
    }
  } else { /* include loc */
    switch (idx) {
    case 0:
      PDCI_MK_TNODE(result, &Puint32_val_vtable, self, "pstate",   &(pd->pstate),     WHATFN);
      break;
    case 1:
      PDCI_MK_TNODE(result, &Puint32_val_vtable, self, "nerr",     &(pd->nerr),       WHATFN);
      break;
    case 2:
      PDCI_MK_TNODE(result, &Puint32_val_vtable, self, "errCode",  &(pd->errCode),    WHATFN);
      break;
    case 3:
      PDCI_MK_TNODE(result, &Ploc_t_vtable,      self, "loc",      &(pd->loc),        WHATFN);
      break;
    case 4:
      PDCI_MK_TNODE(result, &Puint32_val_vtable, self, "neerr",    &(pd->neerr),      WHATFN);
      break;
    case 5:
      PDCI_MK_TNODE(result, &Puint32_val_vtable, self, "firstErr", &(pd->firstError), WHATFN);
      break;
    }
  }
  return result;
}

/* Used for any node with no children */
/* 
   We have to accommodate Galax here.  For nodes that contain only typed values, 
   Galax expects children() to return a text node containing the typed
   value as a string.  This may be onerous, but necessary for the moment.
*/
#undef WHATFN
#define WHATFN "PDCI_no_children"
PDCI_node_t ** PDCI_no_children(PDCI_node_t *self)
{
  PDCI_node_t **result;

  if (!(result = PDCI_NEW_NODE_PTR_LIST(1))) {
      failwith("PADS/Galax ALLOC_ERROR: in " WHATFN);
  }
  return result;
}

PDCI_node_t * PDCI_no_kth_child(PDCI_node_t *self, childIndex idx)
{
  return 0;
}

PDCI_node_t * PDCI_no_kth_child_named(PDCI_node_t *self, childIndex idx, const char *name)
{
  return 0;
}

/* ---------------------------
 * Some typed_value functions
 * --------------------------- */

/* Error function used for many cases */
item PDCI_error_typed_value(PDCI_node_t *node)
{
  failwith("PADS/Galax NOT_A_VALUE: typed_value called on structured type.");
  return 0;  /* will never get here*/
} 

/* node->rep is a C-style string (const char *) */
item PDCI_cstr_typed_value(PDCI_node_t *node)
{
  item        res = 0;
  char        *s   = (char *)node->rep;
  if (galax_atomicUntyped(s, &res)) {
    failwith("PADS/Galax UNEXPECTED_GALAX_VALUE_WRAP_FAILURE in Pcstr_typed_value");
  }
  return res;
}

/* ---------------------------
 * Some string_value functions
 * --------------------------- */

const char * PDCI_not_impl_yet_string_value(PDCI_node_t *node)
{
  return ""; /* not meaningful to concat the leaves of pads data? */
}

/* Helper vtables */

const PDCI_vtable_t
PDCI_structured_pd_vtable = {PDCI_structured_pd_children, 
			     PDCI_structured_pd_kth_child, 
			     PDCI_structured_pd_kth_child_named, 
			     PDCI_error_typed_value,
			     PDCI_not_impl_yet_string_value};

const PDCI_vtable_t
PDCI_sequenced_pd_vtable = {PDCI_sequenced_pd_children, 
			    PDCI_sequenced_pd_kth_child, 
			    PDCI_sequenced_pd_kth_child_named, 
			    PDCI_error_typed_value,
			    PDCI_not_impl_yet_string_value};

const PDCI_vtable_t
Pbase_pd_vtable = {Pbase_pd_children,
		   Pbase_pd_kth_child,
		   Pbase_pd_kth_child_named,
		   PDCI_error_typed_value,
		   PDCI_not_impl_yet_string_value};

const PDCI_vtable_t
Ploc_t_vtable = {Ploc_t_children,
		 Ploc_t_kth_child,
		 Ploc_t_kth_child_named,
		 PDCI_error_typed_value,
		 PDCI_not_impl_yet_string_value};

const PDCI_vtable_t
Ppos_t_vtable = {Ppos_t_children,
		 Ppos_t_kth_child,
		 Ppos_t_kth_child_named,
		 PDCI_error_typed_value,
		 PDCI_not_impl_yet_string_value};

const PDCI_vtable_t
PDCI_cstr_val_vtable = {PDCI_no_children,
			PDCI_no_kth_child,
			PDCI_no_kth_child_named,
			PDCI_cstr_typed_value,
			PDCI_not_impl_yet_string_value};

/* Impl some base type children and typed_value functions and
   associated vtable/val_vtable pairs */

PDCI_IMPL_BASE_VT(Pchar);
PDCI_IMPL_BASE_VAL_VT(Pchar);

PDCI_IMPL_BASE_VT(Pstring);
PDCI_IMPL_BASE_VAL_VT_ARG1(Pstring, ' ');

PDCI_IMPL_BASE_VT(Pint8);
PDCI_IMPL_TYPED_VALUE_INT(Pint8);
PDCI_IMPL_BASE_VAL_VT(Pint8);

PDCI_IMPL_BASE_VT(Pint16);
PDCI_IMPL_TYPED_VALUE_INT(Pint16);
PDCI_IMPL_BASE_VAL_VT(Pint16);

PDCI_IMPL_BASE_VT(Pint32);
PDCI_IMPL_TYPED_VALUE_INTEGER(Pint32);
PDCI_IMPL_BASE_VAL_VT(Pint32);

PDCI_IMPL_BASE_VT(Pint64);
PDCI_IMPL_TYPED_VALUE_INTEGER(Pint64);
PDCI_IMPL_BASE_VAL_VT(Pint64);

PDCI_IMPL_BASE_VT(Puint8);
PDCI_IMPL_TYPED_VALUE_INT(Puint8);
PDCI_IMPL_BASE_VAL_VT(Puint8);

PDCI_IMPL_BASE_VT(Puint16);
PDCI_IMPL_TYPED_VALUE_INT(Puint16);
PDCI_IMPL_BASE_VAL_VT(Puint16);

PDCI_IMPL_BASE_VT(Puint32);
PDCI_IMPL_TYPED_VALUE_INTEGER(Puint32);
PDCI_IMPL_BASE_VAL_VT(Puint32);

PDCI_IMPL_BASE_VT(Puint64);
PDCI_IMPL_TYPED_VALUE_INTEGER(Puint64);
PDCI_IMPL_BASE_VAL_VT(Puint64);

item Pchar_typed_value (PDCI_node_t *node)
{
  item         res = 0;
  Pchar        c   = *((char*)node->rep);
  Pbase_pd  *pd  = (Pbase_pd*)node->pd;
  Pbase_pd   tpd;
  if (!pd) {
    pd = &tpd;
    pd->errCode = P_NO_ERR;
  }
  sfstrset(node->pads->tmp2, 0);
  sfprintf(node->pads->tmp2, "%c", c);
  if (galax_atomicString(sfstruse(node->pads->tmp2), &res)) {
    failwith("PADS/Galax UNEXPECTED_GALAX_VALUE_WRAP_FAILURE in Pchar_typed_value");
  }
  return res;
}

item Pstring_typed_value (PDCI_node_t *node)
{
  item         res = 0;
  Pstring *ps = (Pstring*)node->rep;
  Pbase_pd  *pd  = (Pbase_pd*)node->pd;
  Pbase_pd   tpd;
  if (!pd) {
    pd = &tpd;
    pd->errCode = P_NO_ERR;
  }
  sfstrset(node->pads->tmp2, 0);
  sfprintf(node->pads->tmp2, "%.*s", ps->len, ps->str);
  if (galax_atomicString(sfstruse(node->pads->tmp2), &res)) {
    failwith("PADS/Galax UNEXPECTED_GALAX_VALUE_WRAP_FAILURE in Pstring_typed_value");
  }
  return res;
}

/*

 char,string -> atomicString

Integer types:
 everything < 32 is atomicInt
 32,64 are atomicInteger

 fpoint,floats,doubles are atomicFloat

*/
