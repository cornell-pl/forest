/* Questions:
 * Giving parse descriptors and field the same names means we
 * get collisions on queries like: give me all the b's that are
 * greater than one.
 */

/* libpadsc-xml.h/c */
/* define unknown_vtable for unknown types */
/* define foo_vtable for all base types foo*/

#include "CAML/CStuff.h"  /* value type */
#include <pads-galax.h>
#include <caml/fail.h>

typedef PDC_error_t (* childrenVT) (PDC_node_rep_t * node);
typedef PDC_error_t (* typeValueVT) (PDC_node_rep_t * node);
typedef size_t (* write2ioVT) (Sfio_t *fp, PDC_node_rep_t * node);
typedef size_t (* write2bufVT) (PDC_byte *buf, size_t buf_len, PDC_node_rep_t * node, int *buf_full);
/* need vtable for each base type in parse descriptors:
   PDC_errCode_t, PDC_loc_t, PDC_pos_t, ...? 
   PDC_flags_t, PDCI_flag*/

typedef struct PDC_vtable_s {
  childrenVT      children;
  typedValueVT    typed_value;
  write2ioVT      write2io; 
  write2bufVT     write2buf; 
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

struct PDC_base_rep_s {
  
}; 


/* Generic functions */
void **generic_children (void * ocamln)
{
  PDC_node_rep_t *n = (PDC_node_rep_t *) ocamln; 
  if (!n) return { failwith("Bad node representation in generic_children"); }
  return n->vt.children(n);
}
void *generic_parent (void * ocamln)
{
  PDC_node_rep_t *n = (PDC_node_rep_t *) ocamln; 
  if (!n) return { failwith("Bad node representation in generic_parent"); }
  return n->parent;
}
/* Return value is: 

   1. atomicValue -- a Caml object or 
   2. a union of PADS base types, which is converted
   to a Caml object on the Caml side. */
generic_typed_value (void * ocamln)
{
  PDC_node_rep_t *n = (PDC_node_rep_t *) ocamln; 
  if (!n) return { failwith("Bad node representation in generic_typed_value"); }
  return n->vt.typed_value(n);
}

/* struct, union, array case */
PDC_error_t PDCI_error_typed_valueVT(PDC_node_rep_t *node){
  return PDC_ERR;
} 

ssize_t PDCI_null_write2ioVT(PDC_node_rep_t *node){
  return 0;  /* eventually generate type specific parse descriptor calls */
} 

ssize_t PDCI_null_write2bufVT(PDC_node_rep_t *node){
  return 0;   /* eventually generate type specific parse descriptor calls */
} 

/* in generated .h file for foo pads declaration*/
PDC_error_t foo_children(PDC_node_rep_t *node);
PDC_error_t foo_typed_value(PDC_node_rep_t *node); 
size_t foo_write2ioVT(Sfio_t *fp, PDC_node_rep_t *node);
size_t foo_write2bufVT (PDC_byte *buf, size_t buf_len, PDC_node_rep_t * node, int *buf_full);

extern const PDC_vtable_t foo_vtable;
extern const PDC_vtable_t foo_pd_vtable;

/* in generated .c file for foo pads declaration */
/* struct case */
const PDC_vtable_t foo_vtable = {foo_childrenVT, PDCI_error__typed_valueVT, 
				 foo_write2ioVT, foo_write2bufVT};
const PDC_vtable_t foo_pd_vtable = {foo_pd_childrenVT, PDCI_error__typed_valueVT, 
				    PDCI_null_write2ioVT, PDCI_null_write2bufVT};


PDC_error_t foo_childrenVT(PDC_node_rep_t *parent){
  foo *rep = (foo *) parent->rep;
  foo_pd *pd = (foo_pd *) parent->pd;
  foo_m *m = (foo_m *) parent->m;
  PDC_node_rep_t *current;
  parent->numChildren =<Compiler-supplied constant: 
                        number of full fields + number of computed fields + 1>;
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
  current->pd = (void *)0;
  current->rep = (void *)pd;
  current->name = "parseDesc";
  
  /* now do normal fields: assume first field is bar b */
  current = &(parent->children[1]);
  current->vt = bar_vtable;
  current->pdc = parent->pdc;
  current->parent = parent;
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
