/*
 * macros, galax-pads
 * 
 * Kathleen Fisher, Robert Gruber
 * AT&T Labs Research
 */

#ifndef __PGLX_IMPL_H__
#define __PGLX_IMPL_H__


#define PDCI_MK_NODE(resultIN, vtIN, parentIN, nameIN, mIN, pdIN, repIN, whatfn) \
  do {  \
    if (!(resultIN = PDCI_NEW_NODE((parentIN)->pdc))) { \
      failwith("ALLOC_ERROR: in " whatfn); \
    } \
    resultIN->vt     = (vtIN); \
    resultIN->pdc    = (parentIN)->pdc; \
    resultIN->parent = (parentIN); \
    resultIN->m      = (void *)(mIN); \
    resultIN->pd     = (void *)(pdIN); \
    resultIN->rep    = (repIN); \
    resultIN->name   = (nameIN); \
  } while (0)

#define PDCI_MK_TOP_NODE(resultIN, vtIN, pdcIN, nameIN, mIN, pdIN, repIN, whatfn) \
  do {  \
    if (!(resultIN = PDCI_NEW_NODE(pdcIN))) { \
      failwith("ALLOC_ERROR: in " whatfn); \
    } \
    resultIN->vt     = (vtIN); \
    resultIN->pdc    = pdcIN; \
    resultIN->parent = NULL; \
    resultIN->m      = (void *)(mIN); \
    resultIN->pd     = (void *)(pdIN); \
    resultIN->rep    = (repIN); \
    resultIN->name   = (nameIN); \
  } while (0)

#define PDCI_MK_TOP_NODE_NORET(resultIN, vtIN, pdcIN, nameIN, mIN, pdIN, repIN, whatfn) \
  do {  \
    resultIN = PDCI_NEW_NODE(pdcIN); \
    if (resultIN) { \
      resultIN->vt     = (vtIN); \
      resultIN->pdc    = pdcIN; \
      resultIN->parent = NULL; \
      resultIN->m      = (void *)(mIN); \
      resultIN->pd     = (void *)(pdIN); \
      resultIN->rep    = (repIN); \
      resultIN->name   = (nameIN); \
    } \
  } while (0)

#define  PDCI_MK_TNODE(resultIN, vtIN, parentIN, nameIN, repIN, whatfn) \
  PDCI_MK_NODE(resultIN, vtIN, parentIN, nameIN, 0, 0, repIN, whatfn)

/* TODO: BASE TYPE: make macro for each base type */

#define PDCI_NEW_NODE(pdc) \
  vmnewof((pdc)->vm, 0, PDCI_node_t, 1, 0)

#define PDCI_NEW_NODE_PTR_LIST(pdc, num) \
  vmnewof((pdc)->vm, 0, PDCI_node_t*, (num)+1, 0)

#define PDCI_FREE_NODE(pdc, n) \
  vmfree((pdc)->vm, n)

#define PDCI_FREE_NODE_PTR_LIST(pdc, list) \
  vmfree((pdc)->vm, list)

#ifndef NDEBUG
#define PDCI_NODE_CHECK(n, whatfn) \
do { \
  if (!(n)) \
    failwith("INVALID_PARAM: " PDCI_MacroArg2String(n) " null in " whatfn); \
} while (0)

#define PDCI_NODE_VT_CHECK(n, whatfn) \
do { \
  if (!n) \
    failwith("INVALID_PARAM: " PDCI_MacroArg2String(n) " null in " whatfn); \
  if (!n->vt) \
    failwith("INVALID_PARAM: " PDCI_MacroArg2String(n) "->vt null in " whatfn); \
} while (0)

#else

#define PDCI_NODE_CHECK(n, whatfn)      PDC_NULL_STMT
#define PDCI_NODE_VT_CHECK(n, whatfn)   PDC_NULL_STMT

#endif

#endif /*   __PGLX_IMPL_H__    */
