#include "pads-galax.h"

/* Generic functions */
/* PDCI_node_rep_t ** PGLX_generic_children (PDCI_node_rep_t *node); */
void** PGLX_generic_children (void *ocaml_n)
{
  PDCI_node_rep_t *n = (PDCI_node_rep_t *) ocaml_n; 
  PDCI_NODE_VT_CHECK(n, "PGLX_generic_children");
  return (void **) n->vt.children(n);
}

void* PGLX_generic_parent (void *ocaml_n)
{
  PDCI_node_rep_t *n = (PDCI_node_rep_t *) ocaml_n; 
  PDCI_NODE_CHECK(n, "PGLX_generic_parent");
  return (void *)n->parent;
}

/* Return value is: 
   1. atomicValue -- a Caml object or 
   2. a union of PADS base types, which is converted
      to a Caml object on the Caml side.
 */

value PGLX_generic_typed_value (void * ocaml_n)
{
  PDCI_node_rep_t *n = (PDCI_node_rep_t *) ocaml_n; 
  PDCI_NODE_VT_CHECK(n, "PGLX_generic_typed_value");
  return n->vt.typed_value(n);
}

const char* PGLX_generic_string_value(void *ocaml_nn){
  PDCI_node_rep_t *n = (PDCI_node_rep_t *) ocaml_n; 
  PDCI_NODE_CHECK(n, "PGLX_generic_string_value");
  return "Not yet implemented";
}


