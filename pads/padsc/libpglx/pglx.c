/*
 * Implementation of
 *    public and private APIs, galax-pads
 * 
 * Kathleen Fisher, Robert Gruber
 * AT&T Labs Research
 */

#include "pads-internal.h"
#include "pglx-internal.h"
#include "pglx.h"
#include "caml/fail.h"
#include <stdio.h>


/* ocaml header files can be found in /usr/common/lib/ocaml/caml */


/* ================================================================================
 * PUBLIC GALAX->PADS CALLS (see pglx.h) */ 

/*
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
*/

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
  PDCI_node_t *n = (PDCI_node_t *) ocaml_n; 
  PDCI_NODE_CHECK(n, "PGLX_node_free");
  PDCI_FREE_NODE(n);
}

void PGLX_nodelist_free(nodeRepArray list)
{
  PDCI_FREE_NODE_PTR_LIST(list);
}

/* ================================================================================
 * INTERNAL */

/* Helper macros  */

#define PDCI_SND_INIT_DEF(ty) \
PDCI_node_t * ty ## _sndNode_init(PDCI_node_t *self,          \
				  PDCI_smart_elt_info_t *elt, \
				  PDCI_gen_t gen, 	      \
				  PDCI_childIndex_t idx)      \
{                                                             \
  PDCI_SND_INIT(ty,self,elt,gen,idx);			      \
  return self;						      \
}							      

#define PDCI_IMPL_BASE_VT(ty) \
\
PDCI_node_t * ty ## _node_new(PDCI_node_t *parent, \
			 const char *name, \
			 void* m, void* pd, void* rep,\
			 const char *kind,const char *whatfn)\
{\
  PDCI_node_t *result;\
  PDCI_MK_NODE (result,& ty ## _node_vtable,\
		parent,name,m,pd,rep,kind,#ty "_node_new");\
  return result;\
}\
\
PDCI_node_t * ty ## _cachedNode_init(PDCI_node_t *node){\
\
  /* Setup the virtual table */ \
  node->vt = & ty ## _cachedNode_vtable;\
\
  /* Setup node-type specific fields */ \
  node->child_cache = (PDCI_node_t **)PDCI_NEW_LIST(2);\
  if(node->child_cache == NULL)\
    failwith ("PADS/Galax ALLOC_ERROR: in " PDCI_MacroArg2String(ty) "_cachedNode_init");  \
\
  return node;\
}\
\
PDCI_SND_INIT_DEF(ty)\
\
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
  PDCI_MK_TNODE(result[0], & Pbase_pd_node_vtable,  self, "pd",  pd,  PDCI_MacroArg2String(ty) "_children"); \
  if (pd->errCode == P_NO_ERR || pd->errCode == P_USER_CONSTRAINT_VIOLATION) { \
    PDCI_MK_TNODE(result[1], & ty ## _val_node_vtable,   self, "val", rep, PDCI_MacroArg2String(ty) "_children"); \
  } \
  return result; \
} \
 \
PDCI_node_t * ty ## _node_kthChild(PDCI_node_t *self, childIndex idx) \
{ \
  ty        *rep = (ty*)self->rep; \
  Pbase_pd  *pd  = (Pbase_pd*)self->pd; \
  PDCI_node_t *result = 0; \
  /* the mk calls below raise an exception on alloc error */ \
  switch (idx) { \
    case 0: \
      result = Pbase_pd_node_new(self, "pd",  pd,  PDCI_MacroArg2String(ty) "_node_kthChild"); \
      break; \
    case 1: \
      if (pd->errCode == P_NO_ERR || pd->errCode == P_USER_CONSTRAINT_VIOLATION) { \
        result = ty ## _val_node_new(self, "val", rep, PDCI_MacroArg2String(ty) "_node_kthChild"); \
      } \
      break; \
  } \
  return result; \
} \
 \
PDCI_node_t * ty ## _node_kthChildNamed(PDCI_node_t *self, childIndex idx, const char *name) \
{ \
  PDCI_node_t *result = 0; \
  /* the only valid idx is 0  */ \
  if (idx) return result; \
  /* the mk calls below raise an exception on alloc error */ \
  if (strcmp(name, "pd") == 0)          idx = 0; \
  else if (strcmp(name, "val") == 0)    idx = 1; \
  else return result; /* no such child */ \
\
  return (self->vt->kth_child)(self,idx); \
} \
 \
PDCI_node_t * ty ## _cachedNode_kthChild(PDCI_node_t *self, childIndex idx) \
{ \
  PDCI_node_t *result = 0; \
\
  if (idx >= 2) \
    return result; \
\
  result = self->child_cache[idx]; \
  if (result == NULL){ \
    /* create a new node for the kth child */ \
    result = ty ## _node_kthChild(self,idx); \
\
    /* initialize the node to be a cachedNode */ \
    (result->vt->cachedNode_init)(result);\
\
    /* cache the result */ \
    self->child_cache[idx] = result; \
  } \
\
  return PDCI_ALIAS_NODE(result);\
} \
\
\
PDCI_node_t * ty ## _sndNode_kthChild(PDCI_node_t *self, childIndex idx) \
{ \
  ty        *rep = (ty*)self->rep; \
  Pbase_pd  *pd  = (Pbase_pd*)self->pd; \
  PDCI_node_t *result = 0; \
  /* the mk calls below raise an exception on alloc error */ \
  switch (idx) { \
    case 0: \
      result = Pbase_pd_node_new(self, "pd",  pd,  PDCI_MacroArg2String(ty) "_sndNode_kthChild"); \
      Pbase_pd_sndNode_init(result,self->ancestor,self->ancestor_gen,idx);\
      break; \
    case 1: \
\
      /* Make sure that the node is valid before attempting to access its contents. */ \
      PDCI_sndNode_validate(self);\
      rep = self->rep; \
      pd = self->pd;\
\
      if (pd->errCode == P_NO_ERR || pd->errCode == P_USER_CONSTRAINT_VIOLATION) { \
        result = ty ## _val_node_new(self, "val", rep, PDCI_MacroArg2String(ty) "_sndNode_kthChild"); \
        ty ## _val_sndNode_init(result,self->ancestor,self->ancestor_gen,idx); \
      } \
      break; \
  } \
  return result; \
} \
\
const PDCI_vtable_t ty ## _node_vtable = {ty ## _cachedNode_init, \
				     ty ## _node_kthChild, \
				     ty ## _node_kthChildNamed, \
                                     PDCI_node_free, \
				     PDCI_error_typed_value, \
				     PDCI_not_impl_yet_string_value};\
\
const PDCI_vtable_t ty ## _cachedNode_vtable = {PDCI_error_cachedNode_init, \
					    ty ## _cachedNode_kthChild, \
					    ty ## _node_kthChildNamed, \
					    PDCI_cachedNode_free, \
					    PDCI_error_typed_value, \
					    PDCI_not_impl_yet_string_value}; \
\
const PDCI_vtable_t ty ## _sndNode_vtable = {PDCI_error_cachedNode_init, \
					    ty ## _sndNode_kthChild, \
					    ty ## _node_kthChildNamed, \
					    PDCI_node_free, \
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
}\
\
item ty ## _sndNode_typed_value (PDCI_node_t *node) \
{ \
  item         res = 0; \
  ty           *r;\
  Pbase_pd  *pd;\
  Pbase_pd   tpd; \
\
  /* Make sure that the node is valid before attempting to access its contents. */ \
  PDCI_sndNode_validate(node);\
  r   = (ty*)node->rep; \
  pd  = (Pbase_pd*)node->pd; \
\
  if (!pd) { \
    pd = &tpd; \
    pd->errCode = P_NO_ERR; \
  } \
  sfstrset(node->pads->tmp2, 0); \
  if (-1 == ty ## _write2io(node->pads, node->pads->tmp2, pd, r)) { \
    failwith("PADS/Galax UNEXPECTED_IO_FAILURE in " PDCI_MacroArg2String(ty) "_sndNode_typed_value"); \
  } \
  if (galax_atomicUntyped(sfstruse(node->pads->tmp2), &res)) { \
    failwith("PADS/Galax UNEXPECTED_GALAX_VALUE_WRAP_FAILURE in " PDCI_MacroArg2String(ty) "_sndNode_typed_value"); \
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
}\
\
item ty ## _sndNode_typed_value (PDCI_node_t *node) \
{ \
  item       res = 0; \
  int        r;\
  Pbase_pd  *pd;\
  Pbase_pd   tpd; \
\
  /* Make sure that the node is valid before attempting to access its contents. */ \
  PDCI_sndNode_validate(node);\
  r = *((ty*)node->rep); \
  pd  = (Pbase_pd*)node->pd; \
\
  if (!pd) { \
    pd = &tpd; \
    pd->errCode = P_NO_ERR; \
  } \
  if (galax_atomicInt(r, &res)) { \
    failwith("PADS/Galax UNEXPECTED_GALAX_VALUE_WRAP_FAILURE in " PDCI_MacroArg2String(ty) "_sndNode_typed_value"); \
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
}\
\
item ty ## _sndNode_typed_value (PDCI_node_t *node) \
{ \
  item         res = 0; \
  int          r;\
  Pbase_pd  *pd;\
  Pbase_pd   tpd; \
\
  /* Make sure that the node is valid before attempting to access its contents. */ \
  PDCI_sndNode_validate(node);\
  r = *((ty*)node->rep); \
  pd  = (Pbase_pd*)node->pd; \
\
  if (!pd) { \
    pd = &tpd; \
    pd->errCode = P_NO_ERR; \
  } \
  if (galax_atomicInteger(r, &res)) { \
    failwith("PADS/Galax UNEXPECTED_GALAX_VALUE_WRAP_FAILURE in " PDCI_MacroArg2String(ty) "_sndNode_typed_value"); \
  } \
  return res; \
}

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
}\
\
item ty ## _sndNode_typed_value (PDCI_node_t *node) \
{ \
  item         res = 0; \
  ty           *r;\
  Pbase_pd  *pd;\
  Pbase_pd   tpd; \
\
  /* Make sure that the node is valid before attempting to access its contents. */ \
  PDCI_sndNode_validate(node);\
  r   = (ty*)node->rep; \
  pd  = (Pbase_pd*)node->pd; \
\
  if (!pd) { \
    pd = &tpd; \
    pd->errCode = P_NO_ERR; \
  } \
  sfstrset(node->pads->tmp2, 0); \
  if (-1 == ty ## _write2io(node->pads, node->pads->tmp2, ty_arg1, pd, r)) { \
    failwith("PADS/Galax UNEXPECTED_IO_FAILURE in " PDCI_MacroArg2String(ty) "_sndNode_typed_value"); \
  } \
  if (galax_atomicUntyped(sfstruse(node->pads->tmp2), &res)) { \
    failwith("PADS/Galax UNEXPECTED_GALAX_VALUE_WRAP_FAILURE in " PDCI_MacroArg2String(ty) "_sndNode_typed_value"); \
  } \
  return res; \
}

#define PDCI_NO_CHILD_CN_INIT_DEF(ty)\
\
PDCI_node_t * ty ## _cachedNode_init(PDCI_node_t *node){\
\
  /* Setup the virtual table */ \
  node->vt = & ty ## _cachedNode_vtable;\
\
  /*  The node has no children, hence no cache */ \
  node->child_cache = NULL; \
\
  return node;\
}

/* val node has one child, a text node with no name */ 
#define PDCI_IMPL_BASE_VAL_FUNS(ty) \
\
PDCI_node_t * ty ## _val_node_new(PDCI_node_t *parent, \
			 const char *name, \
			 void* rep,const char *whatfn)\
{\
  PDCI_node_t *result;\
  PDCI_MK_TNODE (result,& ty ## _val_node_vtable,\
		parent,name,rep,#ty "_val_node_new");\
  return result;\
}\
\
PDCI_node_t * ty ## _text_node_new(PDCI_node_t *parent,const char *whatfn)\
{\
  PDCI_node_t *result;\
  PDCI_MK_TEXTNODE (result,& ty ## _text_node_vtable,parent,#ty "_text_node_new");\
  return result;\
}\
\
PDCI_node_t * ty ## _val_cachedNode_init(PDCI_node_t *node){\
\
  /* Setup the virtual table */ \
  node->vt = & ty ## _val_cachedNode_vtable;\
\
  /*  Setup node-type specific fields */ \
  node->child_cache = (PDCI_node_t **)PDCI_NEW_LIST(1);\
  if(node->child_cache == NULL)\
    failwith ("PADS/Galax ALLOC_ERROR: in " PDCI_MacroArg2String(ty) "_val_cachedNode_init");  \
\
  return node;\
}\
\
PDCI_NO_CHILD_CN_INIT_DEF(ty ## _text)\
PDCI_SND_INIT_DEF(ty ## _val) \
PDCI_SND_INIT_DEF(ty ## _text) \
\
PDCI_node_t ** ty ## _val_children(PDCI_node_t *self) \
{ \
  PDCI_node_t **result; \
  if (!(result = PDCI_NEW_NODE_PTR_LIST(1))) { \
    failwith("PADS/Galax ALLOC_ERROR: in " PDCI_MacroArg2String(ty) "_val_children"); \
  } \
  /* the following mk call raises an exception on alloc error */ \
  PDCI_MK_TEXTNODE(result[0], & ty ## _text_node_vtable,  self, PDCI_MacroArg2String(ty) "_val_children"); \
  return result; \
} \
 \
PDCI_node_t * ty ## _val_node_kthChild(PDCI_node_t *self, childIndex idx) \
{ \
  PDCI_node_t *result = 0; \
  /* the only valid idx is 0  */ \
  if (idx) return 0; \
  /* the following mk call raises an exception on alloc error */ \
  /* PDCI_MK_TEXTNODE(result, & ty ## _text_vtable,  self, PDCI_MacroArg2String(ty) "_val_node_kthChild"); */ \
  result = ty ## _text_node_new(self, PDCI_MacroArg2String(ty) "_val_node_kthChild"); \
  return result; \
} \
 \
PDCI_node_t * ty ## _val_cachedNode_kthChild(PDCI_node_t *self, childIndex idx) \
{ \
  PDCI_node_t *result = 0; \
\
  if (idx >= 1) \
    return result; \
\
  result = self->child_cache[idx]; \
  if (result == NULL){ \
    /* create a new node for the kth child */ \
    result = ty ## _node_kthChild(self,idx); \
\
    /* initialize the node to be a cachedNode */ \
    (result->vt->cachedNode_init)(result);\
\
    /* cache the result */ \
    self->child_cache[idx] = result; \
  } \
\
  return PDCI_ALIAS_NODE(result);\
} \
 \
\
PDCI_node_t * ty ## _val_sndNode_kthChild(PDCI_node_t *self, childIndex idx) \
{ \
  PDCI_node_t *result = 0; \
\
  /* the only valid idx is 0  */ \
  if (idx) return 0; \
\
  result = ty ## _text_node_new(self, PDCI_MacroArg2String(ty) "_val_sndNode_kthChild"); \
  ty ## _text_sndNode_init(result,self->ancestor,self->ancestor_gen,idx); \
  return result; \
}

#define PDCI_DEF_BASE_VAL_VT(ty)\
const PDCI_vtable_t ty ## _val_node_vtable = {ty ## _val_cachedNode_init, \
					      ty ## _val_node_kthChild, \
					      PDCI_node_no_kthChildNamed, /* no named children */ \
					      PDCI_node_free,\
					      ty ## _typed_value, \
					      ty ## _string_value}; \
					      \
const PDCI_vtable_t ty ## _val_cachedNode_vtable = {PDCI_error_cachedNode_init, \
		          			    ty ## _val_cachedNode_kthChild, \
						    PDCI_node_no_kthChildNamed, /* no named children */ \
						    PDCI_cachedNode_free,\
						    ty ## _typed_value, \
						    ty ## _string_value}; \
						    \
const PDCI_vtable_t ty ## _val_sndNode_vtable = {PDCI_error_cachedNode_init, \
		          			    ty ## _val_sndNode_kthChild, \
						    PDCI_node_no_kthChildNamed, /* no named children */ \
						    PDCI_node_free,\
						    ty ## _sndNode_typed_value, \
						    ty ## _sndNode_string_value}; \
						    \
const PDCI_vtable_t ty ## _text_node_vtable = {ty ## _text_cachedNode_init,\
					       PDCI_node_no_kthChild, \
					       PDCI_node_no_kthChildNamed, \
					       PDCI_node_free,\
					       ty ## _text_typed_value, \
					       ty ## _string_value};\
					       \
const PDCI_vtable_t ty ## _text_cachedNode_vtable = {PDCI_error_cachedNode_init, \
						     PDCI_node_no_kthChild, /* no children */\
						     PDCI_node_no_kthChildNamed, /* no children */\
						     PDCI_cachedNode_free,  \
						     ty ## _text_typed_value, \
						     ty ## _string_value};\
\
const PDCI_vtable_t ty ## _text_sndNode_vtable = {PDCI_error_cachedNode_init, \
						     PDCI_node_no_kthChild, /* no children */\
						     PDCI_node_no_kthChildNamed, /* no children */\
						     PDCI_node_free,  \
						     ty ## _text_sndNode_typed_value, \
						     ty ## _sndNode_string_value}

#define PDCI_IMPL_BASE_VAL_VT(ty) \
PDCI_IMPL_BASE_VAL_FUNS(ty) \
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
    failwith("PADS/Galax UNEXPECTED_IO_FAILURE in " PDCI_MacroArg2String(ty) "_string_value"); \
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
    failwith("PADS/Galax UNEXPECTED_IO_FAILURE in " PDCI_MacroArg2String(ty) "_text_typed_value"); \
  } \
  if (galax_atomicUntyped(sfstruse(node->pads->tmp2), &res)) { \
    failwith("PADS/Galax UNEXPECTED_GALAX_VALUE_WRAP_FAILURE in " PDCI_MacroArg2String(ty) "_text_typed_value"); \
  } \
  return res; \
} \
const char * ty ## _sndNode_string_value (PDCI_node_t *node) \
{ \
  ty        *r;\
  Pbase_pd  *pd;\
  Pbase_pd   tpd; \
\
  /* Make sure that the node is valid before attempting to access its contents. */ \
  PDCI_sndNode_validate(node);\
  r   = (ty*)node->rep; \
  pd  = (Pbase_pd*)node->pd; \
\
  if (!pd) { \
    pd = &tpd; \
    pd->errCode = P_NO_ERR; \
  } \
  sfstrset(node->pads->tmp2, 0); \
  if (-1 == ty ## _write2io(node->pads, node->pads->tmp2, pd, r)) { \
    failwith("PADS/Galax UNEXPECTED_IO_FAILURE in " PDCI_MacroArg2String(ty) "_sndNode_string_value"); \
  } \
  return (sfstruse(node->pads->tmp2)); \
} \
 \
item ty ## _text_sndNode_typed_value (PDCI_node_t *node) \
{ \
  item         res = 0; \
  ty           *r;\
  Pbase_pd  *pd;\
  Pbase_pd   tpd; \
\
  /* Make sure that the node is valid before attempting to access its contents. */ \
  PDCI_sndNode_validate(node);\
  r   = (ty*)node->rep; \
  pd  = (Pbase_pd*)node->pd; \
\
  if (!pd) { \
    pd = &tpd; \
    pd->errCode = P_NO_ERR; \
  } \
  sfstrset(node->pads->tmp2, 0); \
  if (-1 == ty ## _write2io(node->pads, node->pads->tmp2, pd, r)) { \
    failwith("PADS/Galax UNEXPECTED_IO_FAILURE in " PDCI_MacroArg2String(ty) "_text_sndNode_typed_value"); \
  } \
  if (galax_atomicUntyped(sfstruse(node->pads->tmp2), &res)) { \
    failwith("PADS/Galax UNEXPECTED_GALAX_VALUE_WRAP_FAILURE in " PDCI_MacroArg2String(ty) "_text_sndNode_typed_value"); \
  } \
  return res; \
} \
PDCI_DEF_BASE_VAL_VT(ty)

#define PDCI_IMPL_BASE_VAL_VT_ARG1(ty, ty_arg1) \
PDCI_IMPL_BASE_VAL_FUNS(ty)\
\
const char * ty ## _string_value (PDCI_node_t *node) \
{ \
  ty           *r   = (ty*)node->rep; \
  Pbase_pd  *pd  = (Pbase_pd*)node->pd; \
  Pbase_pd   tpd; \
  if (!pd) { /* Must be a parse-descriptor node */ \
    pd = &tpd; \
    pd->errCode = P_NO_ERR; \
  } \
  sfstrset(node->pads->tmp2, 0); \
  if (-1 == ty ## _write2io(node->pads, node->pads->tmp2, ty_arg1, pd, r)) { \
    failwith("PADS/Galax UNEXPECTED_IO_FAILURE in " PDCI_MacroArg2String(ty) "_string_value"); \
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
    failwith("PADS/Galax UNEXPECTED_IO_FAILURE in " PDCI_MacroArg2String(ty) "_text_typed_value"); \
  } \
  if (galax_atomicUntyped(sfstruse(node->pads->tmp2), &res)) { \
    failwith("PADS/Galax UNEXPECTED_GALAX_VALUE_WRAP_FAILURE in " PDCI_MacroArg2String(ty) "_text_typed_value"); \
  } \
  return res; \
} \
const char * ty ## _sndNode_string_value (PDCI_node_t *node) \
{ \
  ty           *r;\
  Pbase_pd  *pd;\
  Pbase_pd   tpd; \
\
  /* Make sure that the node is valid before attempting to access its contents. */ \
  PDCI_sndNode_validate(node);\
  r   = (ty*)node->rep; \
  pd  = (Pbase_pd*)node->pd; \
\
  if (!pd) { /* Must be a parse-descriptor node */ \
    pd = &tpd; \
    pd->errCode = P_NO_ERR; \
  } \
  sfstrset(node->pads->tmp2, 0); \
  if (-1 == ty ## _write2io(node->pads, node->pads->tmp2, ty_arg1, pd, r)) { \
    failwith("PADS/Galax UNEXPECTED_IO_FAILURE in " PDCI_MacroArg2String(ty) "_sndNode_string_value"); \
  } \
  return (sfstruse(node->pads->tmp2)); \
} \
 \
item ty ## _text_sndNode_typed_value (PDCI_node_t *node) \
{ \
  item         res = 0; \
  ty           *r;\
  Pbase_pd  *pd;\
  Pbase_pd   tpd; \
\
  /* Make sure that the node is valid before attempting to access its contents. */ \
  PDCI_sndNode_validate(node);\
  r   = (ty*)node->rep; \
  pd  = (Pbase_pd*)node->pd; \
\
  if (!pd) { \
    pd = &tpd; \
    pd->errCode = P_NO_ERR; \
  } \
  sfstrset(node->pads->tmp2, 0); \
  if (-1 == ty ## _write2io(node->pads, node->pads->tmp2, ty_arg1, pd, r)) { \
    failwith("PADS/Galax UNEXPECTED_IO_FAILURE in " PDCI_MacroArg2String(ty) "_text_sndNode_typed_value"); \
  } \
  if (galax_atomicUntyped(sfstruse(node->pads->tmp2), &res)) { \
    failwith("PADS/Galax UNEXPECTED_GALAX_VALUE_WRAP_FAILURE in " PDCI_MacroArg2String(ty) "_text_sndNode_typed_value"); \
  } \
  return res; \
} \
 \
PDCI_DEF_BASE_VAL_VT(ty)

/*
 * XXX TEMPORARY:
 */

static const char *walk_children_spaces = "                                                                                                                        ";

void walk_children(void *n, int indent) {
  void       *child = NULL;
  int         iter;
  

  if (strcmp(PGLX_generic_name(n), "pd") == 0)
    return;

  if (strcmp(PGLX_generic_kind(n), "element") == 0 || strcmp(PGLX_generic_kind(n), "document") == 0) {
    const char *n_name = PGLX_generic_name(n);
    error(0, "%.*s<%s>",
	  indent, walk_children_spaces, n_name);
    child = PGLX_generic_kth_child(n,0);
    for (iter = 1; child; iter++) {
      walk_children(child, indent+4);
      PGLX_node_free(child); 
      child = PGLX_generic_kth_child(n,iter);
    }
    error(0, "%.*s</%s>",
	  indent, walk_children_spaces, n_name);
  } else {
    error(0, "%.*s%s",
	  indent, walk_children_spaces, PGLX_generic_string_value(n));
  }
}

/* HELPERS */

/* ---------------------------
 * Some children functions
 * --------------------------- */

/* A pos_t has 3 children (byte, num, and sfio-offset) */
#undef NUM_CHILDREN
#define NUM_CHILDREN 3
PDCI_node_t * Ppos_t_node_new(PDCI_node_t *parent, 
			      const char *name, 
			      void* rep,const char *whatfn)
{
  PDCI_node_t *result;
  PDCI_MK_TNODE (result, (& Ppos_t_node_vtable), parent, name, rep, "Ppos_t_node_new");
  return result;
}

PDCI_node_t * Ppos_t_cachedNode_init(PDCI_node_t *node){

  /* Setup the virtual table */ 
  node->vt = & Ppos_t_cachedNode_vtable;

  /*  Setup node-type specific fields */ 
  node->child_cache = (PDCI_node_t **)PDCI_NEW_LIST(NUM_CHILDREN);
  if(node->child_cache == NULL)
    failwith ("PADS/Galax ALLOC_ERROR: in Ppos_t_cachedNode_init");  

  return node;
}

PDCI_SND_INIT_DEF(Ppos_t)

#undef WHATFN
#define WHATFN "Ppos_t_children"
PDCI_node_t ** Ppos_t_children(PDCI_node_t *self)
{
  Ppos_t *pos = (Ppos_t *) self->rep;
  PDCI_node_t **result;
  if (!(result = PDCI_NEW_NODE_PTR_LIST(NUM_CHILDREN))) {
    failwith("PADS/Galax ALLOC_ERROR: in " WHATFN);
  }
  PDCI_MK_TNODE(result[0], &Pint32_val_node_vtable,   self, "byte",    &(pos->byte),     WHATFN);
  PDCI_MK_TNODE(result[1], &Pint32_val_node_vtable,   self, "num",     &(pos->num),      WHATFN);
  /*  PDCI_MK_TNODE(result[2], &Puint64_val_vtable,  self, "offset",  (Puint64)(pos->offset), WHATFN); */
  return result;
}

#undef WHATFN
#define WHATFN "Ppos_t_node_kthChild"
PDCI_node_t * Ppos_t_node_kthChild(PDCI_node_t *self, childIndex idx)
{
  Ppos_t *pos = (Ppos_t *) self->rep;
  PDCI_node_t *result = 0;
  switch (idx) {
  case 0:
    // PDCI_MK_TNODE(result, &Pint32_val_vtable,   self, "byte",    &(pos->byte),     WHATFN);
    result = Pint32_val_node_new(self, "byte", &(pos->byte), WHATFN);
    break;
  case 1:
    // PDCI_MK_TNODE(result, &Pint32_val_vtable,   self, "num",     &(pos->num),      WHATFN);
    result = Pint32_val_node_new(self, "num", &(pos->num), WHATFN);
    break;
  case 2:
    /*  PDCI_MK_TNODE(result[2], &Puint64_val_vtable,  self, "offset",  (Puint64)(pos->offset), WHATFN); */
    break;
  }
  return result;
}

#undef WHATFN
#define WHATFN "Ppos_t_node_kthChildNamed"
PDCI_node_t * Ppos_t_node_kthChildNamed(PDCI_node_t *self, childIndex idx, const char *name)
{
  PDCI_node_t *result = 0;

  /* the only valid idx is 0 */
  if (idx) return result;

  if (strcmp(name, "byte") == 0)        idx = 0;
  else if (strcmp(name, "num") == 0)    idx = 1;
/*else if (strcmp(name, "offset") == 0) idx = 2; */
  else  return result;

  return (self->vt->kth_child)(self,idx);
}

PDCI_node_t *  Ppos_t_cachedNode_kthChild(PDCI_node_t *self, childIndex idx) 
{ 
  PDCI_node_t *result = 0; 

  if (idx >= NUM_CHILDREN) 
    return result;   

  result = self->child_cache[idx]; 
  if (result == NULL){ 
    /* create a new node for the kth child */ 
    result = Ppos_t_node_kthChild(self,idx); 

    /* initialize the node to be a cachedNode */ 
    (result->vt->cachedNode_init)(result);

    /* cache the result */ 
    self->child_cache[idx] = result; 
  } 

  return PDCI_ALIAS_NODE(result);
} 

#undef WHATFN
#define WHATFN "Ppos_t_sndNode_kthChild"
PDCI_node_t * Ppos_t_sndNode_kthChild(PDCI_node_t *self, childIndex idx)
{
  Ppos_t *pos = (Ppos_t *) self->rep;
  PDCI_node_t *result = 0;

  // check the validaty of the data
  if (pos == NULL || !PDCI_sndNode_is_valid(self)){
    // in case the right-side of the || got us here:
    self->rep = NULL;  
    self->pd  = NULL;

    switch (idx) {
    case 0:
      result = Pint32_val_node_new(self, "byte", NULL, WHATFN);
      Pint32_val_sndNode_init(result,self->ancestor,self->ancestor_gen,idx);
      break;
    case 1:
      result = Pint32_val_node_new(self, "num", NULL, WHATFN);
      Pint32_val_sndNode_init(result,self->ancestor,self->ancestor_gen,idx);
      break;
    case 2:
      /* PDCI_MK_TNODE(result[2], &Puint64_val_vtable,  self, "offset",  (Puint64)(pos->offset), WHATFN); */
      break;
    }
  }else{
    switch (idx) {
    case 0:
      result = Pint32_val_node_new(self, "byte", &(pos->byte), WHATFN);
      Pint32_val_sndNode_init(result,self->ancestor,self->ancestor_gen,idx);
      break;
    case 1:
      result = Pint32_val_node_new(self, "num", &(pos->num), WHATFN);
      Pint32_val_sndNode_init(result,self->ancestor,self->ancestor_gen,idx);
      break;
    case 2:
      /*  PDCI_MK_TNODE(result[2], &Puint64_val_vtable,  self, "offset",  (Puint64)(pos->offset), WHATFN); */
      break;
    }
  }
  return result;
}


/* A loc_t has 2 children (b and e) */
#undef NUM_CHILDREN
#define NUM_CHILDREN 2
PDCI_node_t * Ploc_t_node_new(PDCI_node_t *parent, 
			      const char *name, 
			      void* rep,const char *whatfn)
{
  PDCI_node_t *result;
  PDCI_MK_TNODE (result,& Ploc_t_node_vtable,
		parent,name,rep,"Ploc_t_node_new");
  return result;
}

PDCI_node_t * Ploc_t_cachedNode_init(PDCI_node_t *node){

  /* Setup the virtual table */ 
  node->vt = & Ploc_t_cachedNode_vtable;

  /*  Setup node-type specific fields */ 
  node->child_cache = (PDCI_node_t **)PDCI_NEW_LIST(NUM_CHILDREN);
  if(node->child_cache == NULL)
    failwith ("PADS/Galax ALLOC_ERROR: in Ploc_t_cachedNode_init");  

  return node;
}

PDCI_SND_INIT_DEF(Ploc_t)

#undef WHATFN
#define WHATFN "Ploc_t_children"
PDCI_node_t ** Ploc_t_children(PDCI_node_t *self)
{
  Ploc_t *loc = (Ploc_t *) self->rep;
  PDCI_node_t **result;
  if (!(result = PDCI_NEW_NODE_PTR_LIST(NUM_CHILDREN))) {
    failwith("PADS/Galax ALLOC_ERROR: in " WHATFN);
  }
  PDCI_MK_TNODE(result[0], &Ppos_t_node_vtable,      self, "b",     &(loc->b),     WHATFN);
  PDCI_MK_TNODE(result[1], &Ppos_t_node_vtable,      self, "e",     &(loc->e),     WHATFN);
  return result;
}

#undef WHATFN
#define WHATFN "Ploc_t_node_kthChild"
PDCI_node_t * Ploc_t_node_kthChild(PDCI_node_t *self, childIndex idx)
{
  Ploc_t *loc = (Ploc_t *) self->rep;
  PDCI_node_t *result = 0;
  switch (idx) {
  case 0:
    // PDCI_MK_TNODE(result, &Ppos_t_node_vtable,      self, "b",     &(loc->b),     WHATFN);
    result = Ppos_t_node_new(self, "b",     &(loc->b),     WHATFN);
    break;
  case 1:
    result =  Ppos_t_node_new(self, "e",     &(loc->e),     WHATFN);
    break;
  }
  return result;
}

#undef WHATFN
#define WHATFN "Ploc_t_node_kthChildNamed"
PDCI_node_t * Ploc_t_node_kthChildNamed(PDCI_node_t *self, childIndex idx, const char *name)
{
  PDCI_node_t *result = 0;

  /* the only valid idx is 0 */
  if (idx) return result;

  if (strcmp(name, "b") == 0)       idx = 0;
  else if (strcmp(name, "e") == 0)  idx = 1;
  else return result;
 
  return (self->vt->kth_child)(self,idx);  
}

PDCI_node_t *  Ploc_t_cachedNode_kthChild(PDCI_node_t *self, childIndex idx) 
{ 
  PDCI_node_t *result = 0; 

  if (idx >= NUM_CHILDREN) 
    return result;   

  result = self->child_cache[idx]; 
  if (result == NULL){ 
    /* create a new node for the kth child */ 
    result = Ploc_t_node_kthChild(self,idx); 

    /* initialize the node to be a cachedNode */ 
    (result->vt->cachedNode_init)(result);

    /* cache the result */ 
    self->child_cache[idx] = result; 
  } 

  return PDCI_ALIAS_NODE(result);
} 

#undef WHATFN
#define WHATFN "Ploc_t_sndNode_kthChild"
PDCI_node_t * Ploc_t_sndNode_kthChild(PDCI_node_t *self, childIndex idx)
{
  Ploc_t *loc = (Ploc_t *) self->rep;
  PDCI_node_t *result = 0;

  // check the validaty of the data
  if (loc == NULL || !PDCI_sndNode_is_valid(self)){
    // in case the right-side of the || got us here:
    self->rep = NULL;  
    self->pd  = NULL;

    switch (idx) {
    case 0:
      result = Ppos_t_node_new(self, "b", NULL,     WHATFN);
      Ppos_t_sndNode_init(result,self->ancestor,self->ancestor_gen,idx);
      break;
    case 1:
      result =  Ppos_t_node_new(self, "e", NULL,     WHATFN);
      Ppos_t_sndNode_init(result,self->ancestor,self->ancestor_gen,idx);
      break;
    }
  }else{
    switch (idx) {
    case 0:
      result = Ppos_t_node_new(self, "b",     &(loc->b),     WHATFN);
      Ppos_t_sndNode_init(result,self->ancestor,self->ancestor_gen,idx);
      break;
    case 1:
      result =  Ppos_t_node_new(self, "e",     &(loc->e),     WHATFN);
      Ppos_t_sndNode_init(result,self->ancestor,self->ancestor_gen,idx);
      break;
    }
  }
  return result;
}

/* A base_pd has three children (pstate, errCode, loc) */
#undef NUM_CHILDREN
#define NUM_CHILDREN 3
PDCI_node_t * Pbase_pd_node_new(PDCI_node_t *parent, 
				const char *name, 
				void* rep,const char *whatfn)
{
  PDCI_node_t *result;
  PDCI_MK_TNODE (result,& Pbase_pd_node_vtable,
		parent,name,rep,"Pbase_pd_node_new");
  return result;
}

PDCI_node_t * Pbase_pd_cachedNode_init(PDCI_node_t *node){

  /* Setup the virtual table */ 
  node->vt = & Pbase_pd_cachedNode_vtable;

  /*  Setup node-type specific fields */ 
  node->child_cache = (PDCI_node_t **)PDCI_NEW_LIST(NUM_CHILDREN);
  if(node->child_cache == NULL)
    failwith ("PADS/Galax ALLOC_ERROR: in Pbase_pd_cachedNode_init");  

  return node;
}

PDCI_SND_INIT_DEF(Pbase_pd)

#undef WHATFN
#define WHATFN "Pbase_pd_children"
PDCI_node_t ** Pbase_pd_children(PDCI_node_t *self)
{
  int            i = 0;
  Pbase_pd      *pd = (Pbase_pd *) self->rep;
  PDCI_node_t  **result;

  if (pd->errCode >= 100) {
    if (!(result = PDCI_NEW_NODE_PTR_LIST(NUM_CHILDREN))) {
      failwith("PADS/Galax ALLOC_ERROR: in " WHATFN);
    }
    PDCI_MK_TNODE(result[i], &Puint32_val_node_vtable, self, "pstate",  &(pd->pstate),  WHATFN); i++;
    PDCI_MK_TNODE(result[i], &Puint32_val_node_vtable, self, "errCode", &(pd->errCode), WHATFN); i++;
    PDCI_MK_TNODE(result[i], &Ploc_t_node_vtable,    self, "loc",     &(pd->loc),     WHATFN); i++;
  } else{
    if (!(result = PDCI_NEW_NODE_PTR_LIST(NUM_CHILDREN - 1))) {
      failwith("PADS/Galax ALLOC_ERROR: in " WHATFN);
    }
    PDCI_MK_TNODE(result[i], &Puint32_val_node_vtable, self, "pstate",  &(pd->pstate),  WHATFN); i++;
    PDCI_MK_TNODE(result[i], &Puint32_val_node_vtable, self, "errCode", &(pd->errCode), WHATFN); i++;
  }
  return result;
}

#undef WHATFN
#define WHATFN "Pbase_pd_node_kthChild"
PDCI_node_t * Pbase_pd_node_kthChild(PDCI_node_t *self, childIndex idx)
{
  Pbase_pd     *pd = (Pbase_pd *) self->rep;
  PDCI_node_t  *result = 0;
  switch (idx) {
  case 0:
    //    PDCI_MK_TNODE(result, &Puint32_val_node_vtable, self, "pstate",  &(pd->pstate),  WHATFN);
    result = Puint32_val_node_new(self, "pstate",  &(pd->pstate),  WHATFN);
    break;
  case 1:
    // PDCI_MK_TNODE(result, &Puint32_val_node_vtable, self, "errCode", &(pd->errCode), WHATFN);
    result = Puint32_val_node_new(self, "errCode", &(pd->errCode), WHATFN);
    break;
  case 2:
    if (pd->errCode >= 100) {
      //      PDCI_MK_TNODE(result, &Ploc_t_node_vtable,    self, "loc",     &(pd->loc),     WHATFN);
      result = Ploc_t_node_new(self, "loc",     &(pd->loc),     WHATFN);
    }
    break;
  }
  return result;
}

#undef WHATFN
#define WHATFN "Pbase_pd_node_kthChildNamed"
PDCI_node_t * Pbase_pd_node_kthChildNamed(PDCI_node_t *self, childIndex idx, const char *name)
{
  Pbase_pd     *pd = (Pbase_pd *) self->rep;
  PDCI_node_t  *result = 0;

  /* the only valid idx is 0 */
  if (idx) return 0;

  if (strcmp(name, "pstate") == 0)           idx = 0;
  else if (strcmp(name, "errCode") == 0)     idx = 1;
  else if (strcmp(name, "loc") == 0 
	   && pd->errCode >= 100)            idx = 2;
  else return result;

  return (self->vt->kth_child)(self,idx);
}

PDCI_node_t *  Pbase_pd_cachedNode_kthChild(PDCI_node_t *self, childIndex idx) 
{ 
  Pbase_pd        *pd = (Pbase_pd*)self->rep; 
  PDCI_node_t *result = 0; 

  if ((pd->errCode >= 100 && idx >= NUM_CHILDREN) 
      || (pd->errCode < 100 && idx >= NUM_CHILDREN - 1)) 
    return result;   

  result = self->child_cache[idx]; 
  if (result == NULL){ 
    /* create a new node for the kth child */ 
    result = Pbase_pd_node_kthChild(self,idx); 

    /* initialize the node to be a cachedNode */ 
    (result->vt->cachedNode_init)(result);

    /* cache the result */ 
    self->child_cache[idx] = result; 
  } 

  return PDCI_ALIAS_NODE(result);
} 

#undef WHATFN
#define WHATFN "Pbase_pd_sndNode_kthChild"
PDCI_node_t * Pbase_pd_sndNode_kthChild(PDCI_node_t *self, childIndex idx)
{
  Pbase_pd     *pd = (Pbase_pd *) self->rep;
  PDCI_node_t  *result = 0;

  // check the validaty of the data
  if (pd == NULL || !PDCI_sndNode_is_valid(self)){
    // in case the right-side of the || got us here:
    self->rep = NULL;  
    self->pd  = NULL;

    switch (idx) {
    case 0:
      result = Puint32_val_node_new(self, "pstate",  NULL,  WHATFN);
      Puint32_val_sndNode_init(result,self->ancestor,self->ancestor_gen,idx);
      break;
    case 1:
      result = Puint32_val_node_new(self, "errCode", NULL, WHATFN);
      Puint32_val_sndNode_init(result,self->ancestor,self->ancestor_gen,idx);
      break;
    case 2:
      /* We need to know the value of errCode, so force validation. */
      if (P_ERR == PDCI_sndNode_make_valid(self)){
	failwith("PADS/Galax failed to page node into memory in " WHATFN); 
      }
      pd = self->rep;

      if (pd->errCode >= 100) {
	result = Ploc_t_node_new(self, "loc",  NULL,     WHATFN);
	Ploc_t_sndNode_init(result,self->ancestor,self->ancestor_gen,idx);
      }
      break;
    }
  }else{
    switch (idx) {
    case 0:
      result = Puint32_val_node_new(self, "pstate",  &(pd->pstate),  WHATFN);
      Puint32_val_sndNode_init(result,self->ancestor,self->ancestor_gen,idx);
      break;
    case 1:
      result = Puint32_val_node_new(self, "errCode", &(pd->errCode), WHATFN);
      Puint32_val_sndNode_init(result,self->ancestor,self->ancestor_gen,idx);
      break;
    case 2:
      if (pd->errCode >= 100) {
	result = Ploc_t_node_new(self, "loc",     &(pd->loc),     WHATFN);
	Ploc_t_sndNode_init(result,self->ancestor,self->ancestor_gen,idx);
      }
      break;
    }
  }
  return result;
}

/* A structured_pd has four children (pstate, nerr, errCode, loc) */
#undef NUM_CHILDREN
#define NUM_CHILDREN 4
PDCI_node_t * PDCI_structured_pd_node_new(PDCI_node_t *parent, 
				const char *name, 
				void* rep,const char *whatfn)
{
  PDCI_node_t *result;
  PDCI_MK_TNODE (result,& PDCI_structured_pd_node_vtable,
		parent,name,rep,"PDCI_structured_pd_node_new");
  return result;
}

PDCI_node_t * PDCI_structured_pd_cachedNode_init(PDCI_node_t *node){

  /* Setup the virtual table */ 
  node->vt = & PDCI_structured_pd_cachedNode_vtable;

  /*  Setup node-type specific fields */ 
  node->child_cache = (PDCI_node_t **)PDCI_NEW_LIST(NUM_CHILDREN);
  if(node->child_cache == NULL)
    failwith ("PADS/Galax ALLOC_ERROR: in PDCI_structured_pd_cachedNode_init");  

  return node;
}

PDCI_SND_INIT_DEF(PDCI_structured_pd)

#undef WHATFN
#define WHATFN "PDCI_structured_pd_children"
PDCI_node_t ** PDCI_structured_pd_children(PDCI_node_t *self)
{
  int                  i = 0;
  PDCI_structured_pd  *pd = (PDCI_structured_pd *) self->rep;
  PDCI_node_t        **result;

  if (pd->errCode >= 100) {
    if (!(result = PDCI_NEW_NODE_PTR_LIST(NUM_CHILDREN))) {
      failwith("PADS/Galax ALLOC_ERROR: in " WHATFN);
    }
    /* the following mk calls raise an exception on alloc error */
    PDCI_MK_TNODE(result[i], &Puint32_val_node_vtable, self, "pstate",  &(pd->pstate),  WHATFN); i++;
    PDCI_MK_TNODE(result[i], &Puint32_val_node_vtable, self, "nerr",    &(pd->nerr),    WHATFN); i++;
    PDCI_MK_TNODE(result[i], &Puint32_val_node_vtable, self, "errCode", &(pd->errCode), WHATFN); i++;
    PDCI_MK_TNODE(result[i], &Ploc_t_node_vtable,    self, "loc",     &(pd->loc),     WHATFN); i++;
  } else {
    if (!(result = PDCI_NEW_NODE_PTR_LIST(NUM_CHILDREN - 1))) {
      failwith("PADS/Galax ALLOC_ERROR: in " WHATFN);
    }
    /* the following mk calls raise an exception on alloc error */
    PDCI_MK_TNODE(result[i], &Puint32_val_node_vtable, self, "pstate",  &(pd->pstate),  WHATFN); i++;
    PDCI_MK_TNODE(result[i], &Puint32_val_node_vtable, self, "nerr",    &(pd->nerr),    WHATFN); i++;
    PDCI_MK_TNODE(result[i], &Puint32_val_node_vtable, self, "errCode", &(pd->errCode), WHATFN); i++;
  }
  return result;
}

#undef WHATFN
#define WHATFN "PDCI_structured_pd_node_kthChild"
PDCI_node_t * PDCI_structured_pd_node_kthChild(PDCI_node_t *self, childIndex idx)
{
  PDCI_structured_pd  *pd = (PDCI_structured_pd *) self->rep;
  PDCI_node_t         *result = 0;
  /* the following mk calls raise an exception on alloc error */
  switch (idx) {
  case 0:
    // PDCI_MK_TNODE(result, &Puint32_val_node_vtable, self, "pstate",  &(pd->pstate),  WHATFN);
    result = Puint32_val_node_new(self, "pstate",  &(pd->pstate),  WHATFN);
    break;
  case 1:
    result = Puint32_val_node_new(self, "nerr",    &(pd->nerr),    WHATFN);
    break;
  case 2:
    //    PDCI_MK_TNODE(result, &Puint32_val_node_vtable, self, "errCode", &(pd->errCode), WHATFN);
    result = Puint32_val_node_new(self, "errCode",    &(pd->errCode),    WHATFN);
    break;
  case 3:
    if (pd->errCode >= 100) {
      // PDCI_MK_TNODE(result, &Ploc_t_node_vtable,    self, "loc",     &(pd->loc),     WHATFN);
      result = Ploc_t_node_new(self, "loc",     &(pd->loc),     WHATFN);
    }
    break;
  }
  return result;
}

#undef WHATFN
#define WHATFN "PDCI_structured_pd_node_kthChildNamed"
PDCI_node_t * PDCI_structured_pd_node_kthChildNamed(PDCI_node_t *self, childIndex idx, const char * name)
{
  PDCI_structured_pd  *pd = (PDCI_structured_pd *) self->rep;
  PDCI_node_t         *result = 0;

  /* the only valid idx is 0 */
  if (idx) return 0;

  /* the following mk calls raise an exception on alloc error */
  if (strcmp(name, "pstate") == 0)            idx = 0;
  else if (strcmp(name, "nerr") == 0)         idx = 1;
  else if (strcmp(name, "errCode") == 0)      idx = 2;
  else if (strcmp(name, "loc") == 0         
           && pd->errCode >= 100)             idx = 3;
  else return result;

  return (self->vt->kth_child)(self,idx);
}

PDCI_node_t *  PDCI_structured_pd_cachedNode_kthChild(PDCI_node_t *self, childIndex idx) 
{ 
  PDCI_structured_pd        *pd = (PDCI_structured_pd*)self->rep; 
  PDCI_node_t *result = 0; 

  if ((pd->errCode >= 100 && idx >= NUM_CHILDREN) 
      || (pd->errCode < 100 && idx >= NUM_CHILDREN - 1)) 
    return result;   

  result = self->child_cache[idx]; 
  if (result == NULL){ 
    /* create a new node for the kth child */ 
    result = PDCI_structured_pd_node_kthChild(self,idx); 

    /* initialize the node to be a cachedNode */ 
    (result->vt->cachedNode_init)(result);

    /* cache the result */ 
    self->child_cache[idx] = result; 
  } 

  return PDCI_ALIAS_NODE(result);
} 

#undef WHATFN
#define WHATFN "PDCI_structured_pd_sndNode_kthChild"
PDCI_node_t * PDCI_structured_pd_sndNode_kthChild(PDCI_node_t *self, childIndex idx)
{
  PDCI_structured_pd  *pd = (PDCI_structured_pd *) self->rep;
  PDCI_node_t         *result = 0;

  // check the validaty of the data
  if (pd == NULL || !PDCI_sndNode_is_valid(self)){
    // in case the right-side of the || got us here:
    self->rep = NULL;  
    self->pd  = NULL;

    switch (idx) {
    case 0:
      result = Puint32_val_node_new(self, "pstate",  NULL,  WHATFN);
      Puint32_val_sndNode_init(result,self->ancestor,self->ancestor_gen,idx);
      break;
    case 1:
      result = Puint32_val_node_new(self, "nerr", NULL,    WHATFN);
      Puint32_val_sndNode_init(result,self->ancestor,self->ancestor_gen,idx);
      break;
    case 2:
      result = Puint32_val_node_new(self, "errCode", NULL,    WHATFN);
      Puint32_val_sndNode_init(result,self->ancestor,self->ancestor_gen,idx);
      break;
    case 3:
      /* We need to know the value of errCode, so force validation. */
      if (P_ERR == PDCI_sndNode_make_valid(self)){
	failwith("PADS/Galax failed to page node into memory in " WHATFN); 
      }
      pd = self->rep;

      if (pd->errCode >= 100) {
	result = Ploc_t_node_new(self, "loc", &(pd->loc),     WHATFN);
	Ploc_t_sndNode_init(result,self->ancestor,self->ancestor_gen,idx);
      }
      break;
    }
  }else{
    switch (idx) {
    case 0:
      result = Puint32_val_node_new(self, "pstate",  &(pd->pstate),  WHATFN);
      Puint32_val_sndNode_init(result,self->ancestor,self->ancestor_gen,idx);
      break;
    case 1:
      result = Puint32_val_node_new(self, "nerr",    &(pd->nerr),    WHATFN);
      Puint32_val_sndNode_init(result,self->ancestor,self->ancestor_gen,idx);
      break;
    case 2:
      result = Puint32_val_node_new(self, "errCode",    &(pd->errCode),    WHATFN);
      Puint32_val_sndNode_init(result,self->ancestor,self->ancestor_gen,idx);
      break;
    case 3:
      if (pd->errCode >= 100) {
	result = Ploc_t_node_new(self, "loc",     &(pd->loc),     WHATFN);
	Ploc_t_sndNode_init(result,self->ancestor,self->ancestor_gen,idx);
      }
      break;
    }
  }
  return result;
}

/* A sequenced_pd has six children 
  (pstate, nerr, errCode, neerr, firstError, loc) */

#undef NUM_CHILDREN
#define NUM_CHILDREN 6
PDCI_node_t * PDCI_sequenced_pd_node_new(PDCI_node_t *parent, 
				const char *name, 
				void* rep,const char *whatfn)
{
  PDCI_node_t *result;
  PDCI_MK_TNODE (result,& PDCI_sequenced_pd_node_vtable,
		parent,name,rep,"PDCI_sequenced_pd_node_new");
  return result;
}

PDCI_node_t * PDCI_sequenced_pd_cachedNode_init(PDCI_node_t *node){

  /* Setup the virtual table */ 
  node->vt = & PDCI_sequenced_pd_cachedNode_vtable;

  /*  Setup node-type specific fields */ 
  node->child_cache = (PDCI_node_t **)PDCI_NEW_LIST(NUM_CHILDREN);
  if(node->child_cache == NULL)
    failwith ("PADS/Galax ALLOC_ERROR: in PDCI_sequenced_pd_cachedNode_init");  

  return node;
}

PDCI_SND_INIT_DEF(PDCI_sequenced_pd)

#undef WHATFN
#define WHATFN "PDCI_sequenced_pd_children"
PDCI_node_t ** PDCI_sequenced_pd_children(PDCI_node_t *self)
{
  int                 i = 0;
  PDCI_sequenced_pd  *pd = (PDCI_sequenced_pd *) self->rep;
  PDCI_node_t       **result;

  if (!(result = PDCI_NEW_NODE_PTR_LIST(NUM_CHILDREN))) {
    failwith("PADS/Galax ALLOC_ERROR: in " WHATFN);
  }
  /* the following mk calls raise an exception on alloc error */
  PDCI_MK_TNODE(result[i], &Puint32_val_node_vtable, self, "pstate",   &(pd->pstate),     WHATFN); i++;
  PDCI_MK_TNODE(result[i], &Puint32_val_node_vtable, self, "nerr",     &(pd->nerr),       WHATFN); i++;
  PDCI_MK_TNODE(result[i], &Puint32_val_node_vtable, self, "errCode",  &(pd->errCode),    WHATFN); i++;
  PDCI_MK_TNODE(result[i], &Puint32_val_node_vtable, self, "neerr",    &(pd->neerr),      WHATFN); i++;
  PDCI_MK_TNODE(result[i], &Puint32_val_node_vtable, self, "firstError", &(pd->firstError), WHATFN); i++;
  if (pd->errCode >= 100) {
    PDCI_MK_TNODE(result[i], &Ploc_t_node_vtable,    self, "loc",     &(pd->loc),     WHATFN); i++;
  }
  return result;
}

#undef WHATFN
#define WHATFN "PDCI_sequenced_pd_node_kthChild"
PDCI_node_t * PDCI_sequenced_pd_node_kthChild(PDCI_node_t *self, childIndex idx)
{
  PDCI_sequenced_pd  *pd = (PDCI_sequenced_pd *) self->rep;
  PDCI_node_t        *result = 0;

  switch (idx) {
  case 0:
    result = Puint32_val_node_new(self, "pstate",   &(pd->pstate),     WHATFN);
    break;
  case 1:
    result = Puint32_val_node_new(self, "nerr",     &(pd->nerr),       WHATFN);
    break;
  case 2:
    result = Puint32_val_node_new(self, "errCode",  &(pd->errCode),    WHATFN);
    break;
  case 3:
    result = Puint32_val_node_new(self, "neerr",    &(pd->neerr),      WHATFN);
    break;
  case 4:
    result = Puint32_val_node_new(self, "firstError", &(pd->firstError), WHATFN);
    break;
  case 5:
    if (pd->errCode >= 100) { /* return loc as child */
      result = Ploc_t_node_new(     self, "loc",      &(pd->loc),        WHATFN);
    }
    break;
  }
  return result;
}

#undef WHATFN
#define WHATFN "PDCI_sequenced_pd_node_kthChildNamed"
PDCI_node_t * PDCI_sequenced_pd_node_kthChildNamed(PDCI_node_t *self, childIndex idx, const char* name)
{
  PDCI_sequenced_pd  *pd = (PDCI_sequenced_pd *) self->rep;
  PDCI_node_t        *result = 0;

  /* the only valid idx is 0 */
  if (idx) return 0;

  if (strcmp(name, "pstate") == 0)            idx = 0;
  else if (strcmp(name, "nerr") == 0)         idx = 1;
  else if (strcmp(name, "errCode") == 0)      idx = 2;
  else if (strcmp(name, "neerr") == 0)        idx = 3;
  else if (strcmp(name, "firstError") == 0)     idx = 4;
  else if (pd->errCode >= 100 
	   && strcmp(name, "loc") == 0)       idx = 5;
  else return result;

  return (self->vt->kth_child)(self,idx);
}

PDCI_node_t *  PDCI_sequenced_pd_cachedNode_kthChild(PDCI_node_t *self, childIndex idx) 
{ 
  PDCI_sequenced_pd        *pd = (PDCI_sequenced_pd*)self->rep; 
  PDCI_node_t *result = 0; 

  // Ensure that idx is within the (array) bounds of the cache.
  if ((pd->errCode >= 100 && idx >= NUM_CHILDREN) 
      || (pd->errCode < 100 && idx >= NUM_CHILDREN - 1)) 
    return result;   

  result = self->child_cache[idx]; 
  if (result == NULL){ 
    /* create a new node for the kth child */ 
    result = PDCI_sequenced_pd_node_kthChild(self,idx); 

    /* initialize the node to be a cachedNode */ 
    (result->vt->cachedNode_init)(result);

    /* cache the result */ 
    self->child_cache[idx] = result; 
  } 

  return PDCI_ALIAS_NODE(result);
} 

#undef WHATFN
#define WHATFN "PDCI_sequenced_pd_sndNode_kthChild"
PDCI_node_t * PDCI_sequenced_pd_sndNode_kthChild(PDCI_node_t *self, childIndex idx)
{
  PDCI_sequenced_pd  *pd = (PDCI_sequenced_pd *) self->rep;
  PDCI_node_t        *result = 0;

  // check the validaty of the data
  if (pd == NULL || !PDCI_sndNode_is_valid(self)){
    // in case the right-side of the || got us here:
    self->rep = NULL;  
    self->pd  = NULL;

    switch (idx) {
    case 0:
      result = Puint32_val_node_new(self, "pstate",   NULL,     WHATFN);
      Puint32_val_sndNode_init(result,self->ancestor,self->ancestor_gen,idx);
      break;
    case 1:
      result = Puint32_val_node_new(self, "nerr",     NULL,       WHATFN);
      Puint32_val_sndNode_init(result,self->ancestor,self->ancestor_gen,idx);
      break;
    case 2:
      result = Puint32_val_node_new(self, "errCode",  NULL,    WHATFN);
      Puint32_val_sndNode_init(result,self->ancestor,self->ancestor_gen,idx);
      break;
    case 3:
      result = Puint32_val_node_new(self, "neerr",    NULL,      WHATFN);
      Puint32_val_sndNode_init(result,self->ancestor,self->ancestor_gen,idx);
      break;
    case 4:
      result = Puint32_val_node_new(self, "firstError", NULL, WHATFN);
      Puint32_val_sndNode_init(result,self->ancestor,self->ancestor_gen,idx);
      break;
    case 5:
      /* We need to know the value of errCode, so force validation. */
      if (P_ERR == PDCI_sndNode_make_valid(self)){
	failwith("PADS/Galax failed to page node into memory in " WHATFN); 
      }
      pd = self->rep;

      if (pd->errCode >= 100) {
	result = Ploc_t_node_new(     self, "loc",&(pd->loc),        WHATFN);
	Ploc_t_sndNode_init(result,self->ancestor,self->ancestor_gen,idx);
      }
      break;
    }
  }else{
    switch (idx) {
    case 0:
      result = Puint32_val_node_new(self, "pstate",   &(pd->pstate),     WHATFN);
      Puint32_val_sndNode_init(result,self->ancestor,self->ancestor_gen,idx);
      break;
    case 1:
      result = Puint32_val_node_new(self, "nerr",     &(pd->nerr),       WHATFN);
      Puint32_val_sndNode_init(result,self->ancestor,self->ancestor_gen,idx);
      break;
    case 2:
      result = Puint32_val_node_new(self, "errCode",  &(pd->errCode),    WHATFN);
      Puint32_val_sndNode_init(result,self->ancestor,self->ancestor_gen,idx);
      break;
    case 3:
      result = Puint32_val_node_new(self, "neerr",    &(pd->neerr),      WHATFN);
      Puint32_val_sndNode_init(result,self->ancestor,self->ancestor_gen,idx);
      break;
    case 4:
      result = Puint32_val_node_new(self, "firstError", &(pd->firstError), WHATFN);
      Puint32_val_sndNode_init(result,self->ancestor,self->ancestor_gen,idx);
      break;
    case 5:
      if (pd->errCode >= 100) { /* return loc as child */
	result = Ploc_t_node_new(     self, "loc",      &(pd->loc),        WHATFN);
	Ploc_t_sndNode_init(result,self->ancestor,self->ancestor_gen,idx);
      }
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

PDCI_node_t * PDCI_node_no_kthChild(PDCI_node_t *self, childIndex idx)
{
  return 0;
}

PDCI_node_t * PDCI_node_no_kthChildNamed(PDCI_node_t *self, childIndex idx, const char *name)
{
  return 0;
}

/* ---------------------------
 * Some general-use functions
 * --------------------------- */

void PDCI_node_free(PDCI_node_t *node){
  NodeMM_free((NodeMM_t *)node->pads->ext1, node);
}
 
void PDCI_cachedNode_free(PDCI_node_t *node)
  {/* ignore. cached nodes aren't freed. */} 

/* Error function used for many cases */
item PDCI_error_typed_value(PDCI_node_t *node)
{
  failwith("PADS/Galax NOT_A_VALUE: typed_value called on structured type.");
  return 0;  /* will never get here*/
} 

/* Error function used for cachedNode vtables, as cachedNodes should never
   be reinitialized. */
PDCI_node_t *PDCI_error_cachedNode_init(PDCI_node_t *node)
{
  failwith("PADS/Galax Error: Attempting to re-initialize cachedNode.");
  return 0;  /* will never get here*/
} 

/* Misc. XXX_value functions. */

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

/* node->rep is a C-style string (const char *) */
item PDCI_cstr_sndNode_typed_value(PDCI_node_t *node)
{
  item        res = 0;
  char        *s;

  /* Make sure that the node is valid before attempting to access its contents. */
  PDCI_sndNode_validate(node);
  s   = (char *)node->rep;

  if (galax_atomicUntyped(s, &res)) {
    failwith("PADS/Galax UNEXPECTED_GALAX_VALUE_WRAP_FAILURE in Pcstr_typed_value");
  }
  return res;
}

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

item Pchar_sndNode_typed_value (PDCI_node_t *node)
{
  item         res = 0;
  Pchar        c;
  Pbase_pd  *pd;
  Pbase_pd   tpd;

  /* Make sure that the node is valid before attempting to access its contents. */
  PDCI_sndNode_validate(node);
  c   = *((char*)node->rep);
  pd  = (Pbase_pd*)node->pd;

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

item Pstring_sndNode_typed_value (PDCI_node_t *node)
{
  item         res = 0;
  Pstring *ps;
  Pbase_pd  *pd;
  Pbase_pd   tpd;

  /* Make sure that the node is valid before attempting to access its contents. */
  PDCI_sndNode_validate(node);
  ps = (Pstring*)node->rep;
  pd  = (Pbase_pd*)node->pd;

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

/* ---------------------------
 * Some string_value functions
 * --------------------------- */

const char * PDCI_not_impl_yet_string_value(PDCI_node_t *node)
{
  return ""; /* not meaningful to concat the leaves of pads data? */
}

/* Helper vtables */

#define PDCI_DEF_HELPER_VT(ty) \
const PDCI_vtable_t \
ty ## _node_vtable = {ty ## _cachedNode_init, \
			     ty ## _node_kthChild, \
			     ty ## _node_kthChildNamed, \
                             PDCI_node_free, \
			     PDCI_error_typed_value, \
			     PDCI_not_impl_yet_string_value};\
\
const PDCI_vtable_t \
ty ## _cachedNode_vtable = {PDCI_error_cachedNode_init, \
			     ty ## _cachedNode_kthChild, \
			     ty ## _node_kthChildNamed, \
                             PDCI_cachedNode_free, \
			     PDCI_error_typed_value, \
			     PDCI_not_impl_yet_string_value};\
\
const PDCI_vtable_t \
ty ## _sndNode_vtable = {PDCI_error_cachedNode_init,\
			     ty ## _sndNode_kthChild, \
			     ty ## _node_kthChildNamed, \
                             PDCI_node_free, \
			     PDCI_error_typed_value, \
			     PDCI_not_impl_yet_string_value}

PDCI_DEF_HELPER_VT(PDCI_structured_pd);
PDCI_DEF_HELPER_VT(PDCI_sequenced_pd);
PDCI_DEF_HELPER_VT(Pbase_pd);
PDCI_DEF_HELPER_VT(Ploc_t);
PDCI_DEF_HELPER_VT(Ppos_t);

PDCI_NO_CHILD_CN_INIT_DEF(PDCI_cstr_val)

const PDCI_vtable_t
PDCI_cstr_val_node_vtable = {PDCI_cstr_val_cachedNode_init,
			PDCI_node_no_kthChild,
			PDCI_node_no_kthChildNamed,
                        PDCI_node_free, \
			PDCI_cstr_typed_value,
			PDCI_not_impl_yet_string_value};

const PDCI_vtable_t
PDCI_cstr_val_cachedNode_vtable = {PDCI_error_cachedNode_init,
				   PDCI_node_no_kthChild,
				   PDCI_node_no_kthChildNamed,
				   PDCI_cachedNode_free,
				   PDCI_cstr_typed_value,
				   PDCI_not_impl_yet_string_value};

const PDCI_vtable_t
PDCI_cstr_val_sndNode_vtable = {PDCI_error_cachedNode_init,
				PDCI_node_no_kthChild,
				PDCI_node_no_kthChildNamed,
				PDCI_node_free,
				PDCI_cstr_sndNode_typed_value,
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

/*

 char,string -> atomicString

Integer types:
 everything < 32 is atomicInt
 32,64 are atomicInteger

 fpoint,floats,doubles are atomicFloat

*/
