/*
 * macros, galax-pads
 * 
 * Kathleen Fisher, Robert Gruber
 * AT&T Labs Research
 */

#ifndef __PGLX_IMPL_H__
#define __PGLX_IMPL_H__


#define PDCI_MK_NODE(resultIN, vtIN, parentIN, nameIN, mIN, pdIN, repIN, kindIN, whatfn) \
  do {  \
    if (!(resultIN = PDCI_NEW_NODE())) { \
      failwith("PADS/Galax ALLOC_ERROR: in " whatfn); \
    } \
    resultIN->vt     = (vtIN); \
    resultIN->pads    = (parentIN)->pads; \
    resultIN->parent = (parentIN); \
    resultIN->m      = (void *)(mIN); \
    resultIN->pd     = (void *)(pdIN); \
    resultIN->rep    = (repIN); \
    resultIN->name   = (nameIN); \
    resultIN->kind   = (kindIN); \
  } while (0)

#define PDCI_MK_TOP_NODE(resultIN, vtIN, padsIN, nameIN, mIN, pdIN, repIN, whatfn) \
  do {  \
    if (!(resultIN = PDCI_NEW_NODE())) { \
      failwith("PADS/Galax ALLOC_ERROR: in " whatfn); \
    } \
    resultIN->vt     = (vtIN); \
    resultIN->pads    = padsIN; \
    resultIN->parent = NULL; \
    resultIN->m      = (void *)(mIN); \
    resultIN->pd     = (void *)(pdIN); \
    resultIN->rep    = (repIN); \
    resultIN->name   = (nameIN); \
    resultIN->kind   = "document"; \
  } while (0)

#define PDCI_MK_TOP_NODE_NORET(resultIN, vtIN, padsIN, nameIN, mIN, pdIN, repIN, whatfn) \
  do {  \
    resultIN = PDCI_NEW_NODE(); \
    if (resultIN) { \
      resultIN->vt     = (vtIN); \
      resultIN->pads    = padsIN; \
      resultIN->parent = NULL; \
      resultIN->m      = (void *)(mIN); \
      resultIN->pd     = (void *)(pdIN); \
      resultIN->rep    = (repIN); \
      resultIN->name   = (nameIN); \
      resultIN->kind   = "document"; \
    } \
  } while (0)

#define  PDCI_MK_TNODE(resultIN, vtIN, parentIN, nameIN, repIN, whatfn) \
  PDCI_MK_NODE(resultIN, vtIN, parentIN, nameIN, 0, 0, repIN,           "element", whatfn)

#define  PDCI_MK_TEXTNODE(resultIN, vtIN, parentIN, whatfn) \
  PDCI_MK_NODE(resultIN, vtIN, parentIN, "",     0, 0, (parentIN)->rep, "text", whatfn)

/* TODO: BASE TYPE: make macro for each base type */

#define PDCI_NEW_NODE() \
  ((PDCI_node_t*)calloc(1, sizeof(PDCI_node_t)))

#define PDCI_NEW_NODE_PTR_LIST(num) \
  ((PDCI_node_t**)calloc((num)+1, sizeof(void*)))

#define PDCI_FREE_NODE(n) \
  free(n)

#define PDCI_FREE_NODE_PTR_LIST(list) \
  free(list)

#ifndef NDEBUG
#define PDCI_NODE_CHECK(n, whatfn) \
do { \
  if (!(n)) \
    failwith("PADS/Galax INVALID_PARAM: " PDCI_MacroArg2String(n) " null in " whatfn); \
} while (0)

#define PDCI_NODE_VT_CHECK(n, whatfn) \
do { \
  if (!n) \
    failwith("PADS/Galax INVALID_PARAM: " PDCI_MacroArg2String(n) " null in " whatfn); \
  if (!n->vt) \
    failwith("PADS/Galax INVALID_PARAM: " PDCI_MacroArg2String(n) "->vt null in " whatfn); \
} while (0)

#else

#define PDCI_NODE_CHECK(n, whatfn)      P_NULL_STMT
#define PDCI_NODE_VT_CHECK(n, whatfn)   P_NULL_STMT

#endif

#endif /*   __PGLX_IMPL_H__    */
