#ifndef FOR_CKIT
#include "pglx-impl.h"
#endif

#ifdef FOR_CKIT
void PDCI_NODE_CHECK(PDCI_node_rep_t *n, const char *whatfn);
void PDCI_NODE_VT_CHECK(PDCI_node_rep_t *n, const char *whatfn);
PDCI_node_rep_t *PDCI_NEW_NODE(PDC_t *pdc);
PDCI_node_rep_t **PDCI_NEW_NODE_PTR_LIST(PDC_t *pdc, int num);
void PDCI_FREE_NODE(PDC_t *pdc, PDCI_node_rep_t *n);
void PDCI_FREE_NODE_PTR_LIST(PDC_t *pdc, PDCI_node_rep_t **list);

void PDCI_MK_TNODE(PDCI_node_rep_t *result,
		   PDCI_vtable_t *vt,
		   PDCI_node_rep_t *parent,
		   const char *name, 
		   PDCI_structured_pd_t* val);
void  PDCI_MK_NODE(PDCI_node_rep_t *result,
		   PDCI_vtable_t *vt,
		   PDCI_node_rep_t *parent,
		   const char *name, 
		   void* m, void* pd,
		   void* rep);

#endif

typedef struct PDCI_node_rep_s PDCI_node_rep_t;

struct PDCI_node_rep_s {
  PDCI_vtable_t   *vt;
  PDC_t           *pdc;
  PDCI_node_rep_t *parent;
  void            *m;
  void            *pd;
  void            *rep;
  const char      *name;
};

typedef PDCI_node_rep_t ** (* PDCI_childrenVT) (PDCI_node_rep_t *node); 
typedef value (* PDCI_typeValueVT) (PDCI_node_rep_t *node); 
typedef const char * (* PDCI_stringValueVT) (PDCI_node_rep_t *node);

typedef struct PDCI_vtable_s {
  PDCI_childrenVT      children;
  PDCI_typedValueVT    typed_value;
  PDCI_stringValueVT   string_value;
} PDCI_vtable_t;


/* PARSE DESCRIPTOR SUPPORT */
/* NB all generated structured pd types must BEGIN with the declarations given here: */
typedef struct PDCI_structured_pd_s {
  int nerr;
  PDC_errCode_t errCode;
  PDC_loc_t loc;
  int panic;
} PDCI_structured_pd_t;

/* NB all generated sequenced pd types must BEGIN with the declarations given here: */
typedef struct PDCI_sequenced_pd_s {
  int nerr;
  PDC_errCode_t errCode;
  PDC_loc_t loc;
  int panic;
  int neerr;		        
  int firstError;		
} PDCI_sequenced_pd_t;



/* HELPERS */
/* Helper functions */
/* Error function used for many cases */
value PDCI_error_typed_value(PDCI_node_rep_t *node);
PDCI_node_t ** PDCI_structured_pd_children(PDCI_node_rep_t *self);
PDCI_node_t ** PDCI_sequenced_pd_children(PDCI_node_rep_t *self);

/* Helper vtables */
extern const PDCI_vtable_t PDCI_structured_pd_vtable;
extern const PDCI_vtable_t PDCI_sequenced_pd_vtable;

/* Bob: we need vtable for each base type in parse descriptors:
   PDC_errCode_t, PDC_loc_t, PDC_pos_t, ...? 
   PDC_flags_t, PDCI_flag.  */


/* BASE TYPE SUPPORT */
/* declare bt_vtable for all base types bt*/
/* declare all of the base type make functions */
/* where bty is any PADS base type, eg PDC_uint32 */
#ifdef FOR_CKIT
void bty_mk_node(PDCI_node_rep_t *result,
		 PDCI_node_rep_t *parent,
		 const char *name, 
		 bty_m m, bty_pd pd, bty val);
#endif
