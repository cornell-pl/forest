//#include <stdlib.h>
#include "pglx-free-list.h"
#include "node_mm.h"

#define FREE_LIST_MAX_SIZE 10

/* This should be modified to work the way the rest of the system works,
   but in the meantime ... */
#define MM_FATAL(s)\
  printf("Fatal Error in Node Memory Manager: %s.\n",(s));\
  exit(-1)

struct NodeMM_s {
  char init;
  FreeList_head_t list;
};

NodeMM_t *NodeMM_newMM(){
  /* We depend on calloc to ensure that init is initially 0 */
  return (NodeMM_t *)calloc(1, sizeof(NodeMM_t));
}

void NodeMM_freeMM(NodeMM_t *mm){
  free(mm);
}

void NodeMM_init(NodeMM_t *mm){
  if (!mm->init){
    FreeList_init(&(mm->list), FREE_LIST_MAX_SIZE);  
    mm->init = 1;
  }
  
}

PDCI_node_t *NodeMM_get_alias(PDCI_node_t *n){
  n->rc++;
  return n;
}

PDCI_node_t *NodeMM_alloc(NodeMM_t *mm){
  if (!mm->init){
    MM_FATAL("Node Memory Manager not initialized");
    return 0; //never reached.
  }

  PDCI_node_t *n = FreeList_alloc(&(mm->list));
  n->rc = 1;
  return n;
}

void NodeMM_free(NodeMM_t *mm, PDCI_node_t *n){
  if (!mm->init){
    MM_FATAL("Node Memory Manager not initialized");
  }

  // Decrement and check the reference count
  if (--(n->rc) == 0){
    if (n->parent != NULL)
      PDCI_FREE_NODE(n->parent);
    FreeList_add(&(mm->list), n);
  }
}

// If the array is longer than MAX_INT, then free it in pieces.
void NodeMM_freeArray(NodeMM_t *mm, PDCI_node_t **nArray, unsigned int length){
  int i;
  for (i=0; i < length; i++)
    NodeMM_free(mm,nArray[i]);
}

#undef FREE_LIST_MAX_SIZE
