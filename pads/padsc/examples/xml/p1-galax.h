/*****************************************************************/
/* in generated p1-galax.h file from pads file p1.p */
/*typed_value: always uses error version in library for structured types*/
/*string_value: not yet implemented. */

/* assume fooStruct is a struct in p1.p*/
PDCI_node_rep_t ** fooStruct_children(PDCI_node_rep_t *node);
extern constPDCI_vtable_t fooStruct_vtable;

/* assume fooEnum is an enum in p1.p*/
PDCI_node_rep_t ** fooEnum_children(PDCI_node_rep_t *node);
extern const PDCI_vtable_t fooEnum_vtable;

/* assume fooTy is a typedef in p1.p*/
PDCI_node_rep_t ** fooTy_children(PDCI_node_rep_t *self);
extern const PDCI_vtable_t fooTy_vtable; 

/* assume fooUnion is a union in p1.p*/
PDCI_node_rep_t** fooUnion_children(PDCI_node_rep_t *self);
extern const PDCI_vtable_t fooUnion_vtable; 

/* assume fooArray is an array in p1.p*/
PDCI_node_rep_t** fooArray_children(PDCI_node_rep_t *self);
extern const PDCI_vtable_t fooArray_vtable;


