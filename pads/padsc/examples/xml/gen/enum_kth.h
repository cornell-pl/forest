#ifndef __ENUM_KTH__H__
#define __ENUM_KTH__H__
PDCI_node_t *bar_kth_child (PDCI_node_t *self, PDCI_childIndex_t idx);

PDCI_node_t *bar_kth_child_named (PDCI_node_t *self, PDCI_childIndex_t idx, const char *name);

PDCI_node_t *barArray_kth_child (PDCI_node_t *self, PDCI_childIndex_t idx);

PDCI_node_t *barArray_kth_child_named (PDCI_node_t *self, PDCI_childIndex_t idx, const char *name);
#endif /*  __ENUM_KTH__H__  */
