/*****************************************************************/
/* in generated p1-galax.c file from pads file p1.p */
#include "p1-galax.h"
/* struct case */
/* assume fooStruct is a struct in p1.p*/

/* NOTE: <numChildren> = padsc compiler-computed constant 
   (number of full fields + number of computed fields + 1) 
   for the parse descriptor.*/

PDCI_node_rep_t** fooStruct_children(PDCI_node_rep_t *self){
  fooStruct    *rep = (fooStruct *)    self->rep;
  fooStruct_pd *pd  = (fooStruct_pd *) self->pd;
  fooStruct_m  *m   = (fooStruct_m *)  self->m;
  PDCI_node_rep_t** result; 
  if (!(result = PDCI_NEW_NODE_PTR_LIST(self->pdc, <numChildren>))) {
    failwith("ALLOC_ERROR: in fooStruct_children");
  };
  /* parse descriptor child */
  PDCI_MK_TNODE(result[0], PDCI_structured_pd_vtable, self, "pd", pd);
  
  /* now do normal fields: assume first field is bar b */
  PDCI_MK_NODE(result[1],bar_vtable,self,"b",&(m->b),&(pd->b), &(rep->b));

  /* ... repeat for all other fields ... */
  return result;
}

const PDCI_vtable_t fooStruct_vtable = {fooStruct_children, 
					PDCI_error_typed_value, 0};


/* ENUM CASE fooEnum */
PDCI_node_rep_t** fooEnum_children(PDCI_node_rep_t *self){
  PDCI_node_rep_t** result;
  fooEnum *temp = (fooEnum *)self->val;
  self->val = fooEnum2str(*temp);
  result = PDCI_Cstring_children(self);
  self->val = temp; 
  return result;
}

const PDCI_vtable_t fooEnum_vtable = {fooEnum_children, 
				      PDCI_error_typed_value, 0};


/* TYPEDEF CASE, typedef fooBase fooTy*/
PDCI_node_rep_t** fooTy_children(PDCI_node_rep_t *self){
  fooTy    *rep = (fooTy *)    self->rep;
  fooTy_pd *pd  = (fooTy_pd *) self->pd;
  fooTy_m  *m   = (fooTy_m *)  self->m;
  PDCI_node_rep_t** result; 
  if (!(result = PDCI_NEW_NODE_PTR_LIST(self->pdc, 2))) {
    failwith("ALLOC_ERROR: in fooTy_children");
  };
  /* parse descriptor child */
  PDCI_MK_TNODE(result[0], PDCI_structured_pd_vtable, self, "pd", pd);
  
  /* base child*/
  PDCI_MK_NODE(result[1],fooBase_vtable,self,"base",&(m->base),&(pd->base), rep);

  return result;
}

const PDCI_vtable_t fooTy_vtable = {fooTy_children,
				    PDCI_error_typed_value, 0}; 

/* UNION CASE fooUnion */
PDCI_node_rep_t** fooUnion_children(PDCI_node_rep_t *self){
  fooUnion    *rep = (fooUnion *)    self->rep;
  fooUnion_pd *pd  = (fooUnion_pd *) self->pd;
  fooUnion_m  *m   = (fooUnion_m *)  self->m;
  PDCI_node_rep_t** result; 

  const char *branch = fooUnion_tag2str(rep->tag);

  if (!(result = PDCI_NEW_NODE_PTR_LIST(self->pdc, 2))) {
    failwith("ALLOC_ERROR: in fooUnion_children");
  };
  /* parse descriptor child */
  PDCI_MK_TNODE(result[0], PDCI_structured_pd_vtable, self, "pd", pd);

  switch (rep->tag){
  case tag1: 
  /* handle branches: assume first branch is tagty1 tag1 */
    PDCI_MK_NODE(result[1],tagty1_vtable,self,branch,
		 &(m->tag1),&(pd->val.tag1),&(rep->val.tag1));
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

PDCI_node_rep_t** fooArray_children(PDCI_node_rep_t *self){
  fooArray    *rep = (fooArray *)    self->rep;
  fooArray_pd *pd  = (fooArray_pd *) self->pd;
  fooArray_m  *m   = (fooArray_m *)  self->m;
  PDCI_node_rep_t** result; 
  if (!(result = PDCI_NEW_NODE_PTR_LIST(self->pdc, rep->length + 2))) {
    failwith("ALLOC_ERROR: in fooArray_children");
  };
  /* parse descriptor child */
  PDCI_MK_TNODE(result[0], PDCI_sequenced_pd_vtable, self, "pd", pd);
  PDCI_MK_TNODE(result[1], PDC_uint32_val_vtable, self, "length", &(rep->length));  
  /* now do elements  */
  for (i = 0; i<rep->length; i++){
    PDCI_MK_NODE(result[i+2],fooElement_vtable,self,
		 "elt",&(m->element),&(pd->elts[i]), &(rep->elts[i]));
  }

  return result;
}

const PDCI_vtable_t fooArray_vtable = {fooArray_children, 
				       PDCI_error_typed_value, 0};


