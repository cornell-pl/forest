/*
 * macros, galax-pads
 * 
 * Kathleen Fisher, Robert Gruber
 * AT&T Labs Research
 */

#ifndef __PGLX_IMPL_H__
#define __PGLX_IMPL_H__

#define PDCI_INIT_NODE(resultIN, vtIN, padsIN, parentIN, nameIN, mIN, pdIN, repIN, kindIN) \
  do {  \
    resultIN->vt     = (vtIN); \
    resultIN->pads    = (padsIN); \
    resultIN->parent = PDCI_ALIAS_NODE(parentIN); \
    resultIN->m      = (void *)(mIN); \
    resultIN->pd     = (void *)(pdIN); \
    resultIN->rep    = (repIN);   \
    resultIN->name   = (nameIN);  \
    resultIN->kind   = (kindIN);  \
  } while (0)

#define PDCI_MK_NODE(resultIN, vtIN, parentIN, nameIN, mIN, pdIN, repIN, kindIN, whatfn) \
  do {  \
    if (!(resultIN = PDCI_NEW_NODE((parentIN)->pads))) { \
      failwith("PADS/Galax ALLOC_ERROR: in " whatfn); \
    } \
    PDCI_INIT_NODE(resultIN,vtIN,(parentIN)->pads,parentIN,nameIN,mIN,pdIN,repIN,kindIN); \
  } while (0)

#define PDCI_MK_TOP_NODE(resultIN, vtIN, padsIN, nameIN, mIN, pdIN, repIN, whatfn) \
  do {  \
    if (!(resultIN = PDCI_NEW_NODE(padsIN))) { \
      failwith("PADS/Galax ALLOC_ERROR: in " whatfn); \
    } \
    PDCI_INIT_NODE(resultIN,vtIN,padsIN,NULL,nameIN,mIN,pdIN,repIN,"document"); \
  } while (0)

#define PDCI_MK_TOP_NODE_NORET(resultIN, vtIN, padsIN, nameIN, mIN, pdIN, repIN, whatfn) \
  do {  \
    resultIN = PDCI_NEW_NODE(padsIN); \
    if (resultIN) { \
      PDCI_INIT_NODE(resultIN,vtIN,padsIN,NULL,nameIN,mIN,pdIN,repIN,"document"); \
    } \
  } while (0)

#define  PDCI_MK_TNODE(resultIN, vtIN, parentIN, nameIN, repIN, whatfn) \
  PDCI_MK_NODE(resultIN, vtIN, parentIN, nameIN, 0, 0, repIN,           "element", whatfn)

#define  PDCI_MK_TEXTNODE(resultIN, vtIN, parentIN, whatfn) \
  PDCI_MK_NODE(resultIN, vtIN, parentIN, "",     0, 0, (parentIN)->rep, "text", whatfn)

/* TODO: BASE TYPE: make macro for each base type */

#define PDCI_NEW_NODE(pads) (NodeMM_alloc((NodeMM_t *)(pads->ext1)))

#define PDCI_NEW_LIST(num) \
  ((PDCI_node_t**)calloc((num), sizeof(void*)))
 
#define PDCI_NEW_NODE_PTR_LIST(num) \
  PDCI_NEW_LIST((num) + 1)

#define PDCI_ALIAS_NODE(n) ((n) == NULL ? NULL : NodeMM_get_alias(n))

#define PDCI_FREE_NODE(n) (((n)->vt->free)(n)) 

#define PDCI_FREE_NODE_PTR_LIST(list) \
  free(list)

#define PDCI_SND_INIT(ty,self,elt,gen,path)   \
do{                                           \
   /* Setup the virtual table */              \
  (self)->vt = & ty ## _sndNode_vtable;     \
					      \
  /* Setup node-type specific fields  */      \
  (self)->ancestor = (elt);		      \
  (self)->ancestor_gen = (gen);		      \
  (self)->path = (path);	              \
}while(0)

extern const PDCI_path_t PDCI_emptyPath;
#define PDCI_EMPTY_PATH (PDCI_emptyPath)

/* child <= ty_PATH_MASK */
#define PDCI_PATH_ADD(ty,p,child)\
  {((p).path |((child) << (p).length)),(p).length+(ty ## _pathWidth)}

#define PDCI_PATH_REMOVE(ty,pIN,childOUT,pOUT)     \
do { 					      \
  (childOUT) = (pIN).path & ty ## _pathMask;      \
  (pOUT).path   = (pIN).path >> ty ## _pathWidth;	      \
  (pOUT).length = (pIN).length-(ty ## _pathWidth);	      \
}while (0)

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
