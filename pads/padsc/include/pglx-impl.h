#define PDCI_MK_NODE(result, vt, parent, name, m, pd, rep) \
  do {  \
    if (!(result = PDCI_NEW_NODE(parent->pdc))) { \
      failwith("ALLOC_ERROR: in PDCI_MK_NODE"); \
    } \
    result->vt     = vt; \
    result->pdc    = parent->pdc; \
    result->parent = parent; \
    result->m      = (void *)m; \
    result->pd     = (void *)pd; \
    result->rep    = rep; \
    result->name   = name; \
  } while (0)


#define  PDCI_MK_TNODE(result, vt, parent, name, rep) \
   PDCI_MK_NODE(result, vt, parent, name, 0,0,rep)

/* TODO: BASE TYPE: make macro for each base type */

#define PDCI_NEW_NODE(pdc) \
  vmnewof(pdc->vm, 0, PDCI_node_t, 1, 0)

#define PDCI_NEW_NODE_PTR_LIST(pdc, num) \
  vmnewof(pdc->vm, 0, PDCI_node_t*, num +1, 0)

#define PDCI_FREE_NODE(pdc, n) \
  vmfree(pdc->vm, n)

#define PDCI_FREE_NODE_PTR_LIST(pdc, list) \
  vmfree(pdc->vm, list)

#ifndef NDEBUG
#define PDCI_NODE_CHECK(n, whatfn) \
do { \
  if (!n) \
    failwith("INVALID_PARAM: n null in " whatfn); \
} while (0)

#define PDCI_NODE_VT_CHECK(n, whatfn) \
do { \
  if (!n) \
    failwith("INVALID_PARAM: n null in " whatfn); \
  if (!n->vt) \
    failwith("INVALID_PARAM: n->vt null in " whatfn); \
} while (0)

#else

#define PDCI_NODE_CHECK(n, whatfn)      PDC_NULL_STMT
#define PDCI_NODE_VT_CHECK(n, whatfn)   PDC_NULL_STMT

#endif
