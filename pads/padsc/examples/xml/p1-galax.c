/*****************************************************************/
/* in generated p1-galax.c file from pads file p1.p */

#include "p1-galax.h"
#include "caml/fail.h"      /* exception */

/* struct case */
/* assume fooStruct is a struct in p1.p*/

/* NOTE: <numChildren> = padsc compiler-computed constant 
   (number of full fields + number of computed fields + 1) 
   for the parse descriptor.*/

PDCI_node_t** fooStruct_children(PDCI_node_t *self){
  fooStruct    *rep = (fooStruct *)    self->rep;
  fooStruct_pd *pd  = (fooStruct_pd *) self->pd;
  fooStruct_m  *m   = (fooStruct_m *)  self->m;
  PDCI_node_t** result; 
  if (!(result = PDCI_NEW_NODE_PTR_LIST(self->pdc, /* <numChildren>, e.g.,  */ 2 ))) {
    failwith("ALLOC_ERROR: in fooStruct_children");
  };
  /* parse descriptor child */
  PDCI_MK_TNODE(result[0], &PDCI_structured_pd_vtable, self, "pd", pd, "fooStruct_children");
  
  /* now do normal fields: assume first field is bar b */
  PDCI_MK_NODE(result[1], &bar_vtable, self, "b", &(m->b), &(pd->b), &(rep->b), "fooStruct_children");

  /* ... repeat for all other fields ... */
  return result;
}

const PDCI_vtable_t fooStruct_vtable = {fooStruct_children, 
					PDCI_error_typed_value, 0};


/* ENUM CASE fooEnum */
PDCI_node_t** fooEnum_children(PDCI_node_t *self){
  char  *Cstr;
  fooEnum     *rep  = (fooEnum *)     self->rep;
  P_base_pd *pd   = (P_base_pd *) self->pd;
  PDCI_node_t** result;
  if (!(result = PDCI_NEW_NODE_PTR_LIST(self->pdc, 2))) {
    failwith("ALLOC_ERROR: in fooEnum_children");
  };
  /* parse descriptor child */
  PDCI_MK_TNODE(result[0], &P_base_pd_vtable, self, "pd", pd, "fooEnum_children");
  /* string val child */
  Cstr = (char*)fooEnum2str(*rep);
  PDCI_MK_TNODE(result[0], &PDCI_Cstr_val_vtable, self, "val", Cstr, "fooEnum_children");
  return result;
}

const PDCI_vtable_t fooEnum_vtable = {fooEnum_children, 
				      PDCI_error_typed_value, 0};


/* TYPEDEF CASE, typedef fooBase fooTy*/
PDCI_node_t** fooTy_children(PDCI_node_t *self){
  fooTy    *rep = (fooTy *)    self->rep;
  fooTy_pd *pd  = (fooTy_pd *) self->pd;
  fooTy_m  *m   = (fooTy_m *)  self->m;
  PDCI_node_t** result; 
  if (!(result = PDCI_NEW_NODE_PTR_LIST(self->pdc, 2))) {
    failwith("ALLOC_ERROR: in fooTy_children");
  };
  /* parse descriptor child */
  PDCI_MK_TNODE(result[0], &PDCI_structured_pd_vtable, self, "pd", pd, "fooTy_children");
  
  /* base child*/
  PDCI_MK_NODE(result[1], &fooBase_vtable,self,"base",&(m->base),&(pd->base), rep, "fooTy_children");

  return result;
}

const PDCI_vtable_t fooTy_vtable = {fooTy_children,
				    PDCI_error_typed_value, 0}; 

/* UNION CASE fooUnion */
PDCI_node_t** fooUnion_children(PDCI_node_t *self){
  fooUnion    *rep = (fooUnion *)    self->rep;
  fooUnion_pd *pd  = (fooUnion_pd *) self->pd;
  fooUnion_m  *m   = (fooUnion_m *)  self->m;
  PDCI_node_t** result; 

  const char *branch = fooUnion_tag2str(rep->tag);

  if (!(result = PDCI_NEW_NODE_PTR_LIST(self->pdc, 2))) {
    failwith("ALLOC_ERROR: in fooUnion_children");
  };
  /* parse descriptor child */
  PDCI_MK_TNODE(result[0], &PDCI_structured_pd_vtable, self, "pd", pd, "fooUnion_children");

  switch (rep->tag){
  case tag1: 
  /* handle branches: assume first branch is tagty1 tag1 */
    PDCI_MK_NODE(result[1], &tagty1_vtable,self,branch,
		 &(m->tag1),&(pd->val.tag1),&(rep->val.tag1), "fooUnion_children");
    break;

  case tag2: 
    /* as above */
    break;

  /* ... repeat for all other branches ... */

  /* handle error case */
  case fooUnion_err: 
    result[1] = 0;  /* we have no valid value */
    break;
  }
  return result;
}
const PDCI_vtable_t fooUnion_vtable = {fooUnion_children, 
				       PDCI_error_typed_value, 0}; 


/* ARRAY CASE fooArray with element type fooElement */

PDCI_node_t** fooArray_children(PDCI_node_t *self){
  fooArray    *rep = (fooArray *)    self->rep;
  fooArray_pd *pd  = (fooArray_pd *) self->pd;
  fooArray_m  *m   = (fooArray_m *)  self->m;
  PDCI_node_t** result; 
  int i;
  if (!(result = PDCI_NEW_NODE_PTR_LIST(self->pdc, rep->length + 2))) {
    failwith("ALLOC_ERROR: in fooArray_children");
  };
  /* parse descriptor child */
  PDCI_MK_TNODE(result[0], &PDCI_sequenced_pd_vtable, self, "pd", pd, "fooArray_children");
  PDCI_MK_TNODE(result[1], &P_uint32_val_vtable, self, "length", &(rep->length), "fooArray_children");  
  /* now do elements  */
  for (i = 0; i<rep->length; i++){
    PDCI_MK_NODE(result[i+2],&fooElement_vtable,self,
		 "elt",&(m->element),&(pd->elts[i]), &(rep->elts[i]), "fooArray_children");
  }

  return result;
}

const PDCI_vtable_t fooArray_vtable = {fooArray_children, 
				       PDCI_error_typed_value, 0};


