
#ifndef __NODE_MM_H__
#define __NODE_MM_H__

/* !!! The type PDCI_node_t must be defined before including this file. !!! */

typedef struct NodeMM_s NodeMM_t;

NodeMM_t *NodeMM_newMM();

void NodeMM_freeMM(NodeMM_t *mm);

void NodeMM_init(NodeMM_t *mm);

PDCI_node_t *NodeMM_alloc(NodeMM_t *mm);

void NodeMM_free(NodeMM_t *mm, PDCI_node_t *n);

// If the array is longer than MAX_INT, then free it in pieces.
void NodeMM_freeArray(NodeMM_t *mm, PDCI_node_t **nArray, unsigned int length);

PDCI_node_t *NodeMM_get_alias(PDCI_node_t *n);

#endif /* __NODE_MM_H__ */
