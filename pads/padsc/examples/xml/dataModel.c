/* Bob: we need vtable for each base type in parse descriptors:
   PDC_errCode_t, PDC_loc_t, PDC_pos_t, ...? 
   PDC_flags_t, PDCI_flag.  We also need a vtable for each
   base type. */



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

typedef PDC_node_rep_t ** (* childrenVT) (PDC_node_rep_t * node); 
typedef value (* typeValueVT) (PDC_node_rep_t * node); 
typedef const char * (* stringValueVT) (PDC_node_rep_t *node);

/* leave these for later */
typedef size_t (* write2ioVT) (Sfio_t *fp, PDC_node_rep_t * node);
typedef size_t (* write2bufVT) (PDC_byte *buf, size_t buf_len, 
				PDC_node_rep_t * node, int *buf_full);

typedef struct PDC_vtable_s {
  childrenVT      children;
  typedValueVT    typed_value;
  stringValueVT   string_value;
} PDC_vtable_t;

typedef struct PDC_node_rep_s PDC_node_rep_t;

struct PDC_node_rep_s {
  PDC_vtable_t   *vt;
  PDC_t          *pdc;
  PDC_node_rep_t *parent;
  void           *m;
  void           *pd;
  void           *rep;
  const char     *name;
};
/* Generic vtables */
/* need one vtable for each pd of each kind of structured type:
   struct, union, array, enum, typedef */
const PDC_vtable_t struct_pd_vtable = {struct_pd_childrenVT, 
				       PDCI_error_typed_valueVT,
                                       0}; 


/* Generic functions */
/* PDC_node_rep_t ** generic_children (PDC_node_rep_t * node); */
void **generic_children (void * ocamln)
{
  PDC_node_rep_t *n = (PDC_node_rep_t *) ocamln; 
#ifndef NDEBUG
  if (!n) { failwith("INVALID_PARAM: n null in generic_children"); }
  if (!n->vt) 
    { failwith("INVALID_PARAM: n->vt null in generic_children"); }
#endif
  return (void **) n->vt.children(n);
}

void *generic_parent (void * ocamln)
{
  PDC_node_rep_t *n = (PDC_node_rep_t *) ocamln; 
#ifndef NDEBUG
  if (!n) { failwith("INVALID_PARAM: n null in generic_parent"); }
#endif
  return (void *)n->parent;
}
/* Return value is: 

   1. atomicValue -- a Caml object or 
   2. a union of PADS base types, which is converted
   to a Caml object on the Caml side. */
value generic_typed_value (void * ocamln)
{
  PDC_node_rep_t *n = (PDC_node_rep_t *) ocamln; 
#ifndef NDEBUG
  if (!n) {failwith("INVALID_PARAM: n null in generic_typed_value"); }
  if (!n->vt) 
    {failwith("INVALID_PARAM: n->vt null in generic_typed_value");}
#endif
  return n->vt.typed_value(n);
}


const char * generic_string_value(void *ocamln){
  PDC_node_rep_t *n = (PDC_node_rep_t *) ocamln; 
#ifndef NDEBUG
  if (!n) { failwith("INVALID_PARAM: n null in generic_string_value");}
#endif
  return "Not yet implemented";
}

/* struct, union, array case */
value PDCI_error_typed_valueVT(PDC_node_rep_t *node){
  failwith("NOT_A_VALUE: typed_value called on structured type.");
  return 0;  /* will never get here*/
} 

/*****************************************************************/
/* in generated p1-galax.h file for foo pads declaration*/
PDC_node_rep_t ** foo_children(PDC_node_rep_t *node);
/*foo_typed_value: always uses error version in library for structured types*/
/*foo_string_value: not yet implemented. */
/* do we need a function to create nodes? 
   PDC_node_rep_t * foo_mk_node(PDC_t *pdc, PDC_node)_ */

extern const PDC_vtable_t foo_vtable;

/*****************************************************************/
/* in generated p1-galax.c file for foo pads declaration */
/* struct case */
#include <foo-galax.h>
/* assume foo is a struct */
const PDC_vtable_t foo_vtable = {foo_children, PDCI_error_typed_value, 0};

PDC_node_rep_t** foo_children(PDC_node_rep_t *self){
  foo *rep = (foo *) self->rep;
  foo_pd *pd = (foo_pd *) self->pd;
  foo_m *m = (foo_m *) self->m;
  PDC_node_rep_t *current;
  int numChildren = <Compiler-supplied constant: 
                     number of full fields + number of computed fields + 1>;
  PDC_node_rep_t** result; 
  /* vmnewof call must be changed so we can call it*/
  if (!(result = (PDC_node_rep_t**)PDCI_newof(self->pdc->vm, 0, 
					      sizeof(PDC_node_rep_t*), 
					      numChildren, 0))){
    failwith("ALLOC_ERROR: in foo_children");
  };
  /* set children m, pd, and rep */
  /* parse descriptor child */
  result[0] = (PDC_node_rep_t *)PDCI_newof(self->pdc->vm, 0, 
					   sizeof(PDC_node_rep_t), 1, 0);
  current = result[0];
  current->vt = foo_pd_vtable;
  current->pdc = self->pdc;
  current->self = self;
  current->m = (void *)0;
  current->pd = (void *)0;
  current->rep = (void *)pd;
  current->name = "parseDesc";
  
  /* now do normal fields: assume first field is bar b */
  current = &(self->children[1]);
  current->vt = bar_vtable;
  current->pdc = self->pdc;
  current->self = self;
  current->m = (void *)&(m->b);
  current->pd = (void *)&(pd->b);
  current->rep = (void *)&(rep->b);
  current->name = "b";
  /* ... repeat for all other fields ... */
  return PDC_OK;
}

PDC_error_t foo_pd_childrenVT(PDC_node_rep_t *parent){
  foo_pd *pd = (foo_pd *) parent->rep;
  PDC_node_rep_t *current;
  parent->numChildren =<Compiler-supplied constant: 
                        number of fields in struct + standard pd fields>;
  if (!(parent->children = vmnewof(parent->pdc->vm, 0, PDC_node_rep_t, parent->numChildren, 0))){
    return PDC_ERR;
  };
  /* standard prelude, nerr, to start */
  current = &(parent->children[0]);
  current->vt = PDC_uint32_vtable;  /* provided by some internal PDC library */
  current->pdc = parent->pdc;
  current->parent = parent;
  current->m = (void *)0;
  current->pd = (void *)0;
  current->rep = (void *)&(pd->nerr);
  current->name = "nerr";
  /* repeat for each field in the prelude */

  /* now do named fields: assume first field is bar b, (k+1)th field of pd */
  current = &(parent->children[k]);
  current->vt = bar_pd_vtable;
  current->pdc = parent->pdc;
  current->parent = parent;
  current->m = (void *)0;
  current->pd = (void *)0;
  current->rep = (void *)&(pd->b);
  current->name = "b";
  /* ... repeat for all other fields ... */
  return PDC_OK;
}

/* struct, union, array, enum case */
size_t foo_write2ioVT(Sfio_t *fp, PDC_node_rep_t *node){
  foo *rep = (foo *) node->rep;
  foo_pd *pd = (foo_pd *) node->pd;
  return foo_write2io(node->pdc, fp, pd, rep);
}

size_t foo_write2bufVT (PDC_byte *buf, size_t buf_len, PDC_node_rep_t * node, int *buf_full){
  foo *rep = (foo *) node->rep;
  foo_pd *pd = (foo_pd *) node->pd;
  return foo_write2buf(node->pdc, buf, buf_len, buf_full, pd, rep);
}


/* typedef case, base type baz*/
const PDC_vtable_t fooTD_vtable = {baz_children, baz_typedValue, baz_write2IO, baz_write2Buffer};

/* union case fooUn */
const PDC_vtable_t fooUn_vtable = {fooUn_children, fooUn_typedValue, fooUn_write2IO, fooUn_write2Buffer};


PDC_error_t fooUn_children(PDC_node_rep_t *parent){
  foo *rep = (foo *) parent->rep;
  foo_pd *pd = (foo_pd *) parent->pd;
  foo_m *m = (foo_m *) parent->m;
  PDC_node_rep_t *current;
  parent->numChildren =2;
  if (!(parent->children = vmnewof(parent->pdc->vm, 0, PDC_node_rep_t, parent->numChildren, 0))){
    return PDC_ERR;
  };
  /* set children m, pd, and rep */
  /* parse descriptor child */
  current = &(parent->children[0]);
  current->vt = foo_pd_vtable;
  current->pdc = parent->pdc;
  current->parent = parent;
  current->m = (void *)0;
  current->pd = (void *)pd;
  current->rep = (void *)0;
  current->name = "parseDesc";
  
  /* fill in second child using tag to determine child */
  current = &(parent->children[1]);
  current->pdc = parent->pdc;
  current->parent = parent;

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
const PDC_vtable_t fooEnum_vtable = {fooEnum_children, fooEnum_typedValue, 
				     fooEnum_write2IO, fooEnum_write2Buffer};

PDC_error_t fooUn_children(PDC_node_rep_t *parent){
  foo *rep = (foo *) parent->rep;
  foo_pd *pd = (foo_pd *) parent->pd;
  foo_m *m = (foo_m *) parent->m;
  PDC_node_rep_t *current;
  parent->numChildren =2;
  if (!(parent->children = vmnewof(parent->pdc->vm, 0, PDC_node_rep_t, parent->numChildren, 0))){
    return PDC_ERR;
  };
  /* set children m, pd, and rep */
  /* parse descriptor child */
  current = &(parent->children[0]);
  current->vt = foo_pd_vtable;
  current->pdc = parent->pdc;
  current->parent = parent;
  current->m = (void *)0;
  current->pd = (void *)pd;
  current->rep = (void *)0;
  current->name = "parseDesc";
  
  current = &(parent->children[1]);
  current->pdc = parent->pdc;
  current->parent = parent;

  current->vt = bar_vtable;
  current->m = (void *)&(m->b);
  current->pd = (void *)&(pd->val.b);
  current->rep = (void *)&(rep->val.b);
  current->name = "b";

  return PDC_OK;
}

/* enum case */
PDC_error_t foo_typed_valueVT(PDC_node_rep_t *node){
   foo *rep = (foo *) node->rep;
   node->val = create_xsstring(enum_2str(*rep));
   return PDC_OK;
}

/* enum case: write functions are above */
