#include "pglx.h"


/*
nodeRep root(void) { 
  printf("root() should never be called directly in pads_pglx.c\n"); 
  exit(-1); 
}
*/

const char *name(nodeRep x) { 
  return PGLX_generic_name(x);
}

/* This is wrong but OK for now.  We need to return text nodes
   as the children of nodes with typed content. */
const char *kind(nodeRep x) { 
  return PGLX_generic_kind(x);
}
/* *** */
atomicValue typed_value(nodeRep x) { 
  return PGLX_generic_typed_value(x); 
}

/* *** 
nodeRepArray children(nodeRep x) { 
  return PGLX_generic_children(x);
}
*/

nodeRepOpt kth_child(nodeRep x, childIndex idx){
  return PGLX_generic_kth_child(x,idx);
}

nodeRepOpt kth_child_named(nodeRep x, childIndex idx, const char *name){
  return PGLX_generic_kth_child_named(x,idx,name);
}

nodeRep parent(nodeRep x) { 
  return PGLX_generic_parent(x);
}

padsNID get_id(nodeRep x){
  return PGLX_generic_get_id(x);
}

void free_node(nodeRep *x) {
  PGLX_node_free(*x);
}
