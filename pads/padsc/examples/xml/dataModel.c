/* QUESTIONS

     1. Is it illegal to name a Pstruct/Punion/etc member 'pd' ?

 */

/* Bob: we need vtable for each base type in parse descriptors:
   PDC_errCode_t, PDC_loc_t, PDC_pos_t, ...? 
   PDC_flags_t, PDCI_flag.  We also need a vtable for each
   base type. */

/* put where CKIT can see: */

#ifdef FOR_CKIT
void PDCI_NODE_CHECK(PDCI_node_rep_t *n, const char *whatfn);
void PDCI_NODE_NV_CHECK(PDCI_node_rep_t *n, const char *whatfn);
PDCI_node_rep_t *PDCI_NEW_NODE(PDC_t *pdc);
PDCI_node_rep_t **PDCI_NEW_NODE_PTR_LIST(PDC_t *pdc, int num);
void PDCI_FREE_NODE(PDC_t *pdc, PDCI_node_rep_t *n);
void PDCI_FREE_NODE_PTR_LIST(PDC_t *pdc, PDCI_node_rep_t **list);
#endif

/* put where CKIT cannot see: */ 

#define PDCI_NEW_NODE(pdc) \
  vmnewof(pdc->vm, 0, PDCI_node_rep_t, 1, 0)

#define PDCI_NEW_NODE_PTR_LIST(pdc, num) \
  vmnewof(pdc->vm, 0, PDCI_node_rep_t*, num, 0)

#define PDCI_FREE_NODE(pdc, n) \
  vmfree(pdc->vm, n)

#define PDCI_FREE_NODE_PTR_LIST(pdc, list) \
  vmfree(pdc->vm, list)

#ifndef NDEBUG
#define PDCI_NODE_CHECK(n, whatfn) \
do { \
  if (!n) \
    failwith("INVALID_PARAM: n null in " whatfn); \
} while (0)

#define PDCI_NODE_VT_CHECK(n, whatfn) \
do { \
  if (!n) \
    failwith("INVALID_PARAM: n null in " whatfn); \
  if (!n->vt) \
    failwith("INVALID_PARAM: n->vt null in " whatfn); \
} while (0)

#else

#define PDCI_NODE_CHECK      PDCI_NULL_STMT
#define PDCI_NODE_VT_CHECK   PDCI_NULL_STMT

#endif

/* Questions:
 * Giving parse descriptors and field the same names means we
 * get collisions on queries like: give me all the b's that are
 * greater than one.
 */

/* libpadsc-galax.h/c */
/* define unknown_vtable for unknown types */
/* define foo_vtable for all base types foo*/

/* header files can be found in /usr/common/lib/ocaml/caml */
#include <caml/mlvalues.h>  /* value type */
#include <caml/fail.h>      /* exception */
#include <libpads-galax.h>  /* header file for this file */

typedef struct PDCI_node_rep_s PDCI_node_rep_t;

typedef PDCI_node_rep_t ** (* PDCI_childrenVT) (PDCI_node_rep_t *node); 
typedef value (* PDCI_typeValueVT) (PDCI_node_rep_t *node); 
typedef const char * (* PDCI_stringValueVT) (PDCI_node_rep_t *node);

/* leave these for later */
typedef size_t (* PDCI_write2ioVT) (Sfio_t *fp, PDCI_node_rep_t * node);
typedef size_t (* PDCI_write2bufVT) (PDC_byte *buf, size_t buf_len, 
				     PDCI_node_rep_t * node, int *buf_full);

typedef struct PDCI_vtable_s {
  PDCI_childrenVT      children;
  PDCI_typedValueVT    typed_value;
  PDCI_stringValueVT   string_value;
} PDCI_vtable_t;

struct PDCI_node_rep_s {
  PDCI_vtable_t   *vt;
  PDC_t           *pdc;
  PDCI_node_rep_t *parent;
  void            *m;
  void            *pd;
  void            *rep;
  const char      *name;
};
/* Generic vtables */
/* need one vtable for each pd of each kind of structured type:
   struct, union, array, enum, typedef */
const PDCI_vtable_t
PDCI_struct_pd_vtable = {PDCI_struct_pd_childrenVT, 
			 PDCI_error_typed_valueVT,
			 0};

/* 'Generic' forms of generated pd types  */

/* NB all generated struct pd types must BEGIN with the declarations given here: */
typedef struct PDCI_struct_pd_s {
  int nerr;
  PDC_errCode_t errCode;
  PDC_loc_t loc;
  int panic;
} PDCI_struct_pd_t;

/* TODO: decls for PDCI_union_pd_t, PDCI_array_pd_t, and so on */

/* Generic functions */
/* PDCI_node_rep_t ** PDCI_generic_children (PDCI_node_rep_t *node); */
void**
PDCI_generic_children (void *ocaml_n)
{
  PDCI_node_rep_t *n = (PDCI_node_rep_t *) ocaml_n; 
  PDCI_NODE_VT_CHECK(n, "PDCI_generic_children");
  return (void **) n->vt.children(n);
}

void*
PDCI_generic_parent (void *ocaml_n)
{
  PDCI_node_rep_t *n = (PDCI_node_rep_t *) ocaml_n; 
  PDCI_NODE_CHECK(n, "PDCI_generic_parent");
  return (void *)n->parent;
}

/* Return value is: 
   1. atomicValue -- a Caml object or 
   2. a union of PADS base types, which is converted
      to a Caml object on the Caml side.
 */

value
PDCI_generic_typed_value (void * ocaml_n)
{
  PDCI_node_rep_t *n = (PDCI_node_rep_t *) ocaml_n; 
  PDCI_NODE_VT_CHECK(n, "PDCI_generic_typed_value");
  return n->vt.typed_value(n);
}

const char*
PDCI_generic_string_value(void *ocaml_nn){
  PDCI_node_rep_t *n = (PDCI_node_rep_t *) ocaml_n; 
  PDCI_NODE_CHECK(n, "PDCI_generic_string_value");
  return "Not yet implemented";
}

/* Error function used for many cases */
value
PDCI_error_typed_value(PDCI_node_rep_t *node){
  failwith("NOT_A_VALUE: typed_value called on structured type.");
  return 0;  /* will never get here*/
} 

/* Children functions for struct_pd, union_pd, etc. */

/* A struct_pd has four children (nerr, errCode, loc, panic) */
PDCI_node_t **
PDCI_struct_pd_children(PDCI_node_rep_t *self){
  PDCI_struct_pd *pd = (PDCI_struct_pd *) self->rep;
  PDCI_node_rep_t **result;
  if (!(result = PDCI_NEW_NODE_PTR_LIST(pdc, 4))) {
    failwith("ALLOC_ERROR: in PDCI_struct_pd_children");
  }
  /* the following mk calls raise an exception on alloc error */
  result[0] = PDCI_uint32_mk_node(self, "nerr", pd->nerr);
  result[1] = PDCI_errCode_mk_node(self, "errCode", pd->errCode);
  result[2] = PDCI_loc_mk_node(self, "loc", pd->loc);
  result[3] = PDCI_int32_mk_node(self, "panic", pd->panic);
  return result;
}

/*****************************************************************/
/* in generated p1-galax.h file for foo pads declaration*/
PDCI_node_rep_t ** foo_children(PDCI_node_rep_t *node);
/*foo_typed_value: always uses error version in library for structured types*/
/*foo_string_value: not yet implemented. */
/* do we need a function to create nodes? 
   PDCI_node_rep_t * foo_mk_node(PDC_t *pdc, PDCI_node)_ */

extern const PDCI_vtable_t foo_vtable;

/*****************************************************************/
/* in generated p1-galax.c file for foo pads declaration */
/* struct case */
#include <foo-galax.h>
/* assume foo is a struct */
const PDCI_vtable_t foo_vtable = {foo_children, PDCI_error_typed_value, 0};

/* NOTE: <numChildren> = padsc compiler-computed constant 
   (number of full fields + number of computed fields + 1) */

PDCI_node_rep_t** foo_children(PDCI_node_rep_t *self){
  foo *rep = (foo *) self->rep;
  foo_pd *pd = (foo_pd *) self->pd;
  foo_m *m = (foo_m *) self->m;
  PDCI_node_rep_t *current;
  PDCI_node_rep_t** result; 
  if (!(result = PDCI_NEW_NODE_PTR_LIST(self->pdc, <numChildren>))) {
    failwith("ALLOC_ERROR: in foo_children");
  };
  /* parse descriptor child */
  if (!(current = result[0] = PDCI_NEW_NODE(pdc))) {
    failwith("ALLOC_ERROR: in foo_children");
  }
  current->vt     = PDCI_struct_pd_vtable;
  current->pdc    = self->pdc;
  current->parent = self;
  current->m      = (void *)0;
  current->pd     = (void *)0;
  current->rep    = (void *)pd;
  current->name   = "pd";
  
  /* now do normal fields: assume first field is bar b */
  current = &(self->children[1]);
  current->vt = bar_vtable;
  current->pdc = self->pdc;
  current->parent = self;
  current->m = (void *)&(m->b);
  current->pd = (void *)&(pd->b);
  current->rep = (void *)&(rep->b);
  current->name = "b";
  /* ... repeat for all other fields ... */
  return PDC_OK;
}

/* struct, union, array, enum case */
size_t foo_write2io(Sfio_t *fp, PDCI_node_rep_t *node){
  foo *rep = (foo *) node->rep;
  foo_pd *pd = (foo_pd *) node->pd;
  return foo_write2io(node->pdc, fp, pd, rep);
}

size_t foo_write2buf(PDC_byte *buf, size_t buf_len, PDCI_node_rep_t * node, int *buf_full){
  foo *rep = (foo *) node->rep;
  foo_pd *pd = (foo_pd *) node->pd;
  return foo_write2buf(node->pdc, buf, buf_len, buf_full, pd, rep);
}


/* typedef case, base type baz*/
const PDCI_vtable_t fooTD_vtable = {baz_children, baz_typedValue, baz_write2IO, baz_write2Buffer};

/* union case fooUn */
const PDCI_vtable_t fooUn_vtable = {fooUn_children, fooUn_typedValue, fooUn_write2IO, fooUn_write2Buffer};


PDC_error_t fooUn_children(PDCI_node_rep_t *self){
  foo *rep = (foo *) self->rep;
  foo_pd *pd = (foo_pd *) self->pd;
  foo_m *m = (foo_m *) self->m;
  PDCI_node_rep_t *current;
  self->numChildren =2;
  if (!(self->children = vmnewof(self->pdc->vm, 0, PDCI_node_rep_t, self->numChildren, 0))){
    return PDC_ERR;
  };
  /* set children m, pd, and rep */
  /* parse descriptor child */
  current = &(self->children[0]);
  current->vt = foo_pd_vtable;
  current->pdc = self->pdc;
  current->parent = self;
  current->m = (void *)0;
  current->pd = (void *)pd;
  current->rep = (void *)0;
  current->name = "parseDesc";
  
  /* fill in second child using tag to determine child */
  current = &(self->children[1]);
  current->pdc = self->pdc;
  current->parent = self;

  switch rep->tag {
  case b:{
    current->vt = bar_vtable;
    current->m = (void *)&(m->b);
    current->pd = (void *)&(pd->val.b);
    current->rep = (void *)&(rep->val.b);
    current->name = "b";
    break;
  }
  ...
  }
  return PDC_OK;
}

/* enum case fooEnum */
const PDCI_vtable_t fooEnum_vtable = {fooEnum_children, fooEnum_typedValue, 
				      fooEnum_write2IO, fooEnum_write2Buffer};

PDC_error_t fooUn_children(PDCI_node_rep_t *self){
  foo *rep = (foo *) self->rep;
  foo_pd *pd = (foo_pd *) self->pd;
  foo_m *m = (foo_m *) self->m;
  PDCI_node_rep_t *current;
  self->numChildren =2;
  if (!(self->children = vmnewof(self->pdc->vm, 0, PDCI_node_rep_t, self->numChildren, 0))){
    return PDC_ERR;
  };
  /* set children m, pd, and rep */
  /* parse descriptor child */
  current = &(self->children[0]);
  current->vt = foo_pd_vtable;
  current->pdc = self->pdc;
  current->parent = self;
  current->m = (void *)0;
  current->pd = (void *)pd;
  current->rep = (void *)0;
  current->name = "parseDesc";
  
  current = &(self->children[1]);
  current->pdc = self->pdc;
  current->parent = self;

  current->vt = bar_vtable;
  current->m = (void *)&(m->b);
  current->pd = (void *)&(pd->val.b);
  current->rep = (void *)&(rep->val.b);
  current->name = "b";

  return PDC_OK;
}

/* enum case */
PDC_error_t foo_typed_value(PDCI_node_rep_t *node){
   foo *rep = (foo *) node->rep;
   node->val = create_xsstring(enum_2str(*rep));
   return PDC_OK;
}

/* enum case: write functions are above */
