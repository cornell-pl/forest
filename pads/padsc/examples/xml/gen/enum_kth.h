#ifndef __ENUM_KTH__H__
#define __ENUM_KTH__H__
PDCI_node_t *barArray_dummySmartNode_init(PDCI_node_t *self);

PDCI_node_t *barArray_seqSmartNode_init(PDCI_node_t *self, unsigned int max_elts);

Perror_t barArray_read_init (P_t *pads,barArray_m *m,barArray_pd *pd,barArray *rep);
Perror_t barArray_read_all(P_t *pads,barArray_m *m,barArray_pd *pd,barArray *rep);
int barArray_read_one(P_t *pads,barArray_m *m,barArray_pd *pd,barArray *rep, PDCI_childIndex_t idx);
Perror_t barArray_read_start(P_t *pads,barArray_pd *pd);
Perror_t barArray_read_finish(P_t *pads,barArray_pd *pd,barArray *rep);

extern PDCI_vtable_t const barArray_dummySmartNode_vtable;
extern PDCI_vtable_t const barArray_seqSmartNode_vtable;

/*
extern PDCI_vtable_t const barArray_cachedNode_vtable;

PDCI_node_t *bar_node_new(PDCI_node_t *parent,
			 const char *name, 
			 void* m, void* pd, void* rep,
			 const char *kind,
			 const char *whatfn);
 
PDCI_node_t *bar_cachedNode_init(PDCI_node_t *node);

PDCI_node_t *bar_node_kthChild (PDCI_node_t *self, PDCI_childIndex_t idx);

PDCI_node_t *bar_node_kthChildNamed (PDCI_node_t *self, PDCI_childIndex_t idx, const char *name);

PDCI_node_t *bar_cachedNode_kthChild (PDCI_node_t *self, PDCI_childIndex_t idx);

extern PDCI_vtable_t const bar_cachedNode_vtable;
extern PDCI_vtable_t const bar_sndNode_vtable;

PDCI_node_t *barArray_node_new(PDCI_node_t *parent,
			       const char *name, 
			       void* m, void* pd, void* rep,
			       const char *kind,
			       const char *whatfn);
 
PDCI_node_t *barArray_cachedNode_init(PDCI_node_t *node);

PDCI_node_t *barArray_node_kthChild (PDCI_node_t *self, PDCI_childIndex_t idx);

PDCI_node_t *barArray_node_kthChildNamed (PDCI_node_t *self, PDCI_childIndex_t idx, const char *name);

PDCI_node_t *barArray_cachedNode_kthChild (PDCI_node_t *self, PDCI_childIndex_t idx);
*/

#endif /*  __ENUM_KTH__H__  */
