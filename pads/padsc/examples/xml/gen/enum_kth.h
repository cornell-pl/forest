#ifndef __ENUM_KTH__H__
#define __ENUM_KTH__H__
//////////////////////////////////////
// Type: PDCI_smart_array_info_t
//
// Information used by the elt_read function implemented for arrays.
//

typedef struct barArray_info_s barArray_info_t; 

PDCI_node_t *barArray_dummySmartNode_init(PDCI_node_t *self);

barArray_info_t *barArray_info_init(P_t *pads,int max_elts);

PDCI_node_t *barArray_seqSmartNode_init(PDCI_node_t *self, unsigned int max_elts);

extern PDCI_vtable_t const barArray_dummySmartNode_vtable;
extern PDCI_vtable_t const barArray_seqSmartNode_vtable;

struct barArray_info_s {

  /******************************* Generic fields **************************/
  PDCI_smart_elt_info_t     *tmap;         // alias of _internal's buf; reset on growth
  RBuf_t                    *_internal;    // growable rbuf for tmap
  PDCI_childIndex_t         *liveList;       // list of live elements, used to choose an evictee.
  RBuf_t                    *_internal_live;    // growable rbuf for invMap.

  unsigned int               max_elts;         // maximum number of elements allowed in-memory at once.

  // parse state
  Sfoff_t                    first_offset; // offset for idx 0
  Sfoff_t                    next_offset;  // offset for next_idx

  PDCI_childIndex_t          next_idx_read;    // first unread index
  PDCI_childIndex_t          next_idx_create;  // first uncreated index

  /* live is used as a "producer" and 
   * evict is used as a "consumer".
   */
  unsigned int               next_idx_live;    // next available space in the live list
  unsigned int               next_idx_evict;   // next eviction candidate of the live list.
  unsigned int               live_count;
  /******************************* Type-specific fields **************************/
  /* Stores the beginning io location of the array. */
  Ploc_t beginLoc;
  
  
};

#endif /*  __ENUM_KTH__H__  */

/*
Perror_t barArray_read_init (P_t *pads,barArray_m *m,barArray_pd *pd,barArray *rep);
Perror_t barArray_read_all(P_t *pads,barArray_m *m,barArray_pd *pd,barArray *rep);
int barArray_read_one(P_t *pads,barArray_m *m,barArray_pd *pd,barArray *rep, PDCI_childIndex_t idx);
Perror_t barArray_read_start(P_t *pads,barArray_pd *pd);
Perror_t barArray_read_finish(P_t *pads,barArray_pd *pd,barArray *rep);

*/
