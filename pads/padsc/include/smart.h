/*
 * smart nodes types.
 * 
 * Robert Gruber, Yitzhak Mandelbaum
 * AT&T Labs Research
 */

#ifndef __SMART_H__
#define __SMART_H__


typedef unsigned long PDCI_gen_t;// XXX: Soundness not guaranteed, due to potential 
                                 // wrapping of gen counts.

// ======================================================================
// Core Data Structure Typedefs
// ======================================================================

typedef struct PDCI_smart_elt_info_s     PDCI_smart_elt_info_t;
typedef struct PDCI_smart_node_s         PDCI_smart_node_t;
typedef struct PDCI_smart_array_info_s   PDCI_smart_array_info_t; 
typedef PDCI_smart_node_t                PDCI_manager_t;

// ======================================================================
// Functions needed by smart nodes
// ======================================================================

// Type: PDCI_smart_elt_read_fn
//
// The smart elt read function allocates and initializes info->rep, info->pd, and info->mask,
// (parent->m is the smart node mask, read_elt_fn pulls out the elt mask after doing appropriate cast)
// (parent->pd is used to track read state so far / final state if finalized is true) 
// and then reads the element. 
// Additionally, this function will deal with paging out other elements, if necessary. 
//
// Input: info->idx, info->offset should be valid,
//        node->next_idx tells us whether we are re-reading or reading for the first time
//          when re-reading, we do not bump node->contents.pd.(err/nerr) beccause
//          they were already bumped on the first read, and we do not need to check
//          for termination conditions because they were already checked.
// 

typedef  Perror_t (*PDCI_smart_elt_read_fn)(PDCI_node_t *node, P_t *pads, PDCI_smart_elt_info_t *info);


// Type: PDCI_smart_elt_free_fn
//
// Because the above allocates them, we need a way to free info->rep, info->pd, info->mask

typedef  Perror_t (*PDCI_smart_elt_free_fn)(P_t *pads, PDCI_smart_elt_info_t *info);


// Type: PDCI_offset_walk_fn
//
// Walk PADS representation rep, pd and m to find rep, pd and m of structure at offset.

typedef Perror_t  (* PDCI_path_walk_fn) (P_t *pads, void *m, void *pd, void *rep, PDCI_path_t path,
					 void **m_out, void **pd_out, void **rep_out);


// Type: PDCI_smart_failure_fn
//
// Report that something very wrong happened.
// elt_info can be NULL if failure not associated with a particular elt

typedef void   (* PDCI_smart_failure_fn) (P_t *pads, PDCI_smart_node_t *node, 
					  PDCI_smart_elt_info_t *info,
					  const char *descr);


// ======================================================================
// Core Data Structures
// ======================================================================


//////////////////////////////////////
// Type: PDCI_smart_node_t
//
// Construction Notes:
//   smart array -
//        P_PS_setPartial(&(content.pd))
//   ...
//

struct PDCI_smart_node_s {

  // Parts of a PDCI_node_t that are used by the smart node:
  //     contents.mask:        is used (both the array-level mask and the elt-level mask are used)
  //     contents.pd:
  //         pstate : tells us if panic is happening, tells us if final state has been reached
  //         nerr:    number of errors so far
  //         neerr:   number of element errors so far (for arrays)
  //         etc.
  //         elts/_internal: used as a cache of in-memory element pds. Unlike normal arrays,
  //                         the length does not reflect the true length of the array.
  //         length: length of the elts cache.
  //    contents.rep
  //         elts/_internal: used as a cache of in-memory elements. Unlike normal arrays,
  //                         the length does not reflect the true length of the array.
  //         length: length of the elts cache.
  //
  //PDCI_node_t                contents;  // includes elt mask passed to read_elt_fn

  // information needed to manage elements
  PDCI_smart_elt_read_fn     elt_read;
  PDCI_smart_elt_free_fn     elt_free;
  PDCI_path_walk_fn          elt_path_walk;
  PDCI_smart_failure_fn      handle_failure;
  void                      *elt_state;  // for arrays, this is PDCI_smart_array_t 
};


//////////////////////////////////////
// Type: PDCI_smart_array_info_t
//
// Information used by the elt_read function implemented for arrays.
//

struct PDCI_smart_array_info_s {
  // INV: tmap[0] ... tmap[next_idx_create-1] are all allocated elements.
  // INV: tmap[0] ... tmap[next_idx_read-1] are all valid elements.
  // INV: Forall valid idx, tmap[idx].rep != NULL iff tmap[idx] is in memory.
  // INV: Forall valid idx, tmap[idx].pd != NULL iff tmap[idx] is in memory.
  PDCI_smart_elt_info_t     *tmap;         // alias of _internal's buf; reset on growth
  RBuf_t                    *_internal;    // growable rbuf for tmap
  PDCI_childIndex_t         *invMap;       // map from rep element indexes to corresponding smart element indexes.
  RBuf_t                    *_internal_inv;    // growable rbuf for invMap.

  unsigned int               max_elts;         // maximum number of elements allowed in-memory at once.

  // parse state
  Sfoff_t                    first_offset; // offset for idx 0
  Sfoff_t                    next_offset;  // offset for next_idx

  PDCI_childIndex_t          next_idx_read;    // first unread index
  PDCI_childIndex_t          next_idx_create;  // first uncreated index
  unsigned int               next_idx_evict;   // next eviction candidate 

  // ...
  
};

//////////////////////////////////////
// Type: PDCI_smart_elt_info_s
//
// Information stored for each smart element of a smart node.
//

struct PDCI_smart_elt_info_s {

  // always valid (and all we need to read into memory -- 
  //               plus some stuff at smart array level):

  PDCI_node_t         *parent;  // the smart node that contains this element.
  PDCI_childIndex_t    idx;
  Sfoff_t              offset; // can be used as the key if stored in a hash table.
  PDCI_gen_t           gen; 
  void                *m; 

  // only valid if data is in memory:

  void                *rep;
  void                *pd;
};

// ======================================================================
// Macros
// ======================================================================

#define PDCI_NEW_SMART_NODE(padsIN)\
  ((PDCI_smart_node_t *)calloc(1,sizeof(PDCI_smart_node_t)))

#define PDCI_INIT_SMART_NODE(resultIN, readIN, freeIN, walkIN, failIN, stateIN) \
  do {  \
    (resultIN)->elt_read = (readIN); \
    (resultIN)->elt_free = (freeIN); \
    (resultIN)->elt_path_walk = (walkIN); \
    (resultIN)->handle_failure = (failIN); \
    (resultIN)->elt_state = (stateIN); \
  } while (0)

#define PDCI_MK_SMART_NODE(resultIN, padsIN, readIN, freeIN, walkIN, failIN, stateIN, whatfn) \
  do {  \
    if (!(resultIN = PDCI_NEW_SMART_NODE(padsIN))) { \
      failwith("PADS/Galax ALLOC_ERROR: in " whatfn); \
    } \
    PDCI_INIT_SMART_NODE(resultIN, readIN, freeIN, walkIN, failIN, stateIN); \
  } while (0)

#define PDCI_INIT_SMART_ELT(eltIN, parentIN, idxIN, offsetIN, genIN, repIN, pdIN, mIN) \
do {\
  (eltIN).parent = (parentIN);\
  (eltIN).idx    = (idxIN);\
  (eltIN).offset = (offsetIN);\
  (eltIN).gen    = (genIN);\
  (eltIN).rep    = (repIN);\
  (eltIN).pd     = (pdIN);\
  (eltIN).m      = (mIN);\
} while (0)

// ======================================================================
// Function Headers
// ======================================================================

//
// Check whether *node refers to an in-memory element.
//
int PDCI_sndNode_is_valid(PDCI_node_t *node);

//
// Make *node valid. This function includes all the logic for 
// ensuring that node is a valid reference (e.g. all flags are set 
// correctly, everything is in memory that should be, etc.)
// Use only if !PDCI_node_is_valid(node)
//
// In: node
// Out: *node.
//
Perror_t PDCI_sndNode_make_valid(PDCI_node_t *node);

// Regardless of current state of node, node will be valid on return from function.
void PDCI_sndNode_validate(PDCI_node_t *node);

#endif /*   __SMART_H__    */
