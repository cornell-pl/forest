#include "libpadsc-internal.h"
#ifndef FOR_CKIT
#  include "pglx-impl.h"
#endif

/* ================================================================================
 * Predeclare some types */

typedef struct PDCI_node_s          PDCI_node_t;
typedef struct PDCI_vtable_s        PDCI_vtable_t;
typedef struct PDCI_structured_pd_s PDCI_structured_pd;
typedef struct PDCI_sequenced_pd_s PDCI_sequenced_pd;

/* ================================================================================
 * HELPER MACROS */

/* These macros are defind in pglx-impl.h.  Here we give prototypes for CKIT: */ 
#ifdef FOR_CKIT
void PDCI_NODE_CHECK(PDCI_node_t *n, const char *whatfn);
void PDCI_NODE_VT_CHECK(PDCI_node_t *n, const char *whatfn);
PDCI_node_t *PDCI_NEW_NODE(PDC_t *pdc);
PDCI_node_t **PDCI_NEW_NODE_PTR_LIST(PDC_t *pdc, int num);
void PDCI_FREE_NODE(PDC_t *pdc, PDCI_node_t *n);
void PDCI_FREE_NODE_PTR_LIST(PDC_t *pdc, PDCI_node_t **list);

void PDCI_MK_TNODE(PDCI_node_t *result,
		   PDCI_vtable_t *vt,
		   PDCI_node_t *parent,
		   const char *name, 
		   PDCI_structured_pd* val,
		   const char *whatfn);
void  PDCI_MK_NODE(PDCI_node_t *result,
		   PDCI_vtable_t *vt,
		   PDCI_node_t *parent,
		   const char *name, 
		   void* m, void* pd,
		   void* rep,
		   const char *whatfn);
#endif

/* Helper macros that we always want expanded */

#define PDCI_DECL_VT(ty) extern const PDCI_vtable_t ty ## _vtable
#define PDCI_DECL_VAL_VT(ty) extern const PDCI_vtable_t ty ## _val_vtable

/* ================================================================================
 * TYPES */

/* prototypes for vtable functions */
typedef PDCI_node_t **      (* PDCI_children_fn)    (PDCI_node_t *node); 
typedef value               (* PDCI_typed_value_fn)   (PDCI_node_t *node); 
typedef const char *        (* PDCI_string_value_fn) (PDCI_node_t *node);

/* Type PDCI_node_t: */
struct PDCI_node_s {
  const PDCI_vtable_t   *vt;
  PDC_t                 *pdc;
  PDCI_node_t           *parent;
  void                  *m;
  void                  *pd;
  void                  *rep;
  const char            *name;
  /* the following are only used by base type nodes */
  const PDCI_vtable_t   *base_vt;
  PDC_base_pd           *base_pd;
  void                  *base_val;
};

/* Type PDCI_vtable_t: */
struct PDCI_vtable_s {
  PDCI_children_fn       children;
  PDCI_typed_value_fn    typed_value;
  PDCI_string_value_fn   string_value;
};

/* PARSE DESCRIPTOR SUPPORT */
/* NB all generated structured pd types must BEGIN with the declarations given here: */

/* type PDCI_structured_pd: */
struct PDCI_structured_pd_s {
  int nerr;
  PDC_errCode_t errCode;
  PDC_loc_t loc;
  int panic;
};

/* NB all generated sequenced pd types must BEGIN with the declarations given here: */

/* Type PDCI_sequenced_pd_t: */
struct PDCI_sequenced_pd_s {
  int nerr;
  PDC_errCode_t errCode;
  PDC_loc_t loc;
  int panic;
  int neerr;		        
  int firstError;		
};

/* ================================================================================
 * Helper functions */

/* Error function used for many cases */
value PDCI_error_typed_value(PDCI_node_t *node);

/* Children functions */

PDCI_node_t ** PDC_base_pd_children(PDCI_node_t *self);
PDCI_node_t ** PDC_loc_t_children(PDCI_node_t *self);
PDCI_node_t ** PDC_pos_t_children(PDCI_node_t *self);

PDCI_node_t ** PDCI_structured_pd_children(PDCI_node_t *self);
PDCI_node_t ** PDCI_sequenced_pd_children(PDCI_node_t *self);
PDCI_node_t ** PDCI_no_children(PDCI_node_t *self);

/* ================================================================================
 * VTABLES */

/* Special vtables */

PDCI_DECL_VT(PDCI_structured_pd);
PDCI_DECL_VT(PDCI_sequenced_pd);
PDCI_DECL_VT(PDCI_basetype);

PDCI_DECL_VT(PDC_base_pd);
PDCI_DECL_VT(PDC_loc_t);
PDCI_DECL_VT(PDC_pos_t);

PDCI_DECL_VT(PDCI_Cstr);

/* Base type vtables: need two vtables for all base types,
   one at level of pd/val, one for the actual value */

#ifdef FOR_CKIT
/* CKIT needs to think all of these are actual base table constants */

PDCI_DECL_VT(PDC_char);
PDCI_DECL_VT(PDC_a_char);
PDCI_DECL_VT(PDC_e_char);

PDCI_DECL_VT(PDC_string);
PDCI_DECL_VT(PDC_string_ME);
PDCI_DECL_VT(PDC_string_CME);
PDCI_DECL_VT(PDC_string_SE);
PDCI_DECL_VT(PDC_string_CSE);

PDCI_DECL_VT(PDC_a_string);
PDCI_DECL_VT(PDC_a_string_ME);
PDCI_DECL_VT(PDC_a_string_CME);
PDCI_DECL_VT(PDC_a_string_SE);
PDCI_DECL_VT(PDC_a_string_CSE);

PDCI_DECL_VT(PDC_e_string);
PDCI_DECL_VT(PDC_e_string_ME);
PDCI_DECL_VT(PDC_e_string_CME);
PDCI_DECL_VT(PDC_e_string_SE);
PDCI_DECL_VT(PDC_e_string_CSE);

PDCI_DECL_VT(PDC_date);
PDCI_DECL_VT(PDC_a_date);
PDCI_DECL_VT(PDC_e_date);

PDCI_DECL_VT(PDC_int8);
PDCI_DECL_VT(PDC_int16);
PDCI_DECL_VT(PDC_int32);
PDCI_DECL_VT(PDC_int64);
PDCI_DECL_VT(PDC_uint8);
PDCI_DECL_VT(PDC_uint16);
PDCI_DECL_VT(PDC_uint32);
PDCI_DECL_VT(PDC_uint64);

#else 
/* all of the above can just be defined to be PDCI_basetype_vtable */

#define PDC_char_vtable PDCI_basetype_vtable
#define PDC_a_char_vtable PDCI_basetype_vtable
#define PDC_e_char_vtable PDCI_basetype_vtable

#define PDC_string_vtable PDCI_basetype_vtable
#define PDC_string_ME_vtable PDCI_basetype_vtable
#define PDC_string_CME_vtable PDCI_basetype_vtable
#define PDC_string_SE_vtable PDCI_basetype_vtable
#define PDC_string_CSE_vtable PDCI_basetype_vtable

#define PDC_a_string_vtable PDCI_basetype_vtable
#define PDC_a_string_ME_vtable PDCI_basetype_vtable
#define PDC_a_string_CME_vtable PDCI_basetype_vtable
#define PDC_a_string_SE_vtable PDCI_basetype_vtable
#define PDC_a_string_CSE_vtable PDCI_basetype_vtable

#define PDC_e_string_vtable PDCI_basetype_vtable
#define PDC_e_string_ME_vtable PDCI_basetype_vtable
#define PDC_e_string_CME_vtable PDCI_basetype_vtable
#define PDC_e_string_SE_vtable PDCI_basetype_vtable
#define PDC_e_string_CSE_vtable PDCI_basetype_vtable

#define PDC_date_vtable PDCI_basetype_vtable
#define PDC_a_date_vtable PDCI_basetype_vtable
#define PDC_e_date_vtable PDCI_basetype_vtable

#define PDC_int8_vtable PDCI_basetype_vtable
#define PDC_int16_vtable PDCI_basetype_vtable
#define PDC_int32_vtable PDCI_basetype_vtable
#define PDC_int64_vtable PDCI_basetype_vtable
#define PDC_uint8_vtable PDCI_basetype_vtable
#define PDC_uint16_vtable PDCI_basetype_vtable
#define PDC_uint32_vtable PDCI_basetype_vtable
#define PDC_uint64_vtable PDCI_basetype_vtable

#endif /* FOR_CKIT */

/* We need one _val_vtable for each in-memory format.
   All of the PADS types that share an in-memory format 
   can share a vtable */

/* The required _val_vtable */
PDCI_DECL_VAL_VT(PDC_char);
PDCI_DECL_VAL_VT(PDC_string);
PDCI_DECL_VAL_VT(PDC_date);
PDCI_DECL_VAL_VT(PDC_int8);
PDCI_DECL_VAL_VT(PDC_int16);
PDCI_DECL_VAL_VT(PDC_int32);
PDCI_DECL_VAL_VT(PDC_int64);
PDCI_DECL_VAL_VT(PDC_uint8);
PDCI_DECL_VAL_VT(PDC_uint16);
PDCI_DECL_VAL_VT(PDC_uint32);
PDCI_DECL_VAL_VT(PDC_uint64);

/* The cases where we can use vtable sharing */
#ifdef FOR_CKIT
PDCI_DECL_VAL_VT(PDC_a_char);
PDCI_DECL_VAL_VT(PDC_e_char);

PDCI_DECL_VAL_VT(PDC_string_ME);
PDCI_DECL_VAL_VT(PDC_string_CME);
PDCI_DECL_VAL_VT(PDC_string_SE);
PDCI_DECL_VAL_VT(PDC_string_CSE);

PDCI_DECL_VAL_VT(PDC_a_string);
PDCI_DECL_VAL_VT(PDC_a_string_ME);
PDCI_DECL_VAL_VT(PDC_a_string_CME);
PDCI_DECL_VAL_VT(PDC_a_string_SE);
PDCI_DECL_VAL_VT(PDC_a_string_CSE);

PDCI_DECL_VAL_VT(PDC_e_string);
PDCI_DECL_VAL_VT(PDC_e_string_ME);
PDCI_DECL_VAL_VT(PDC_e_string_CME);
PDCI_DECL_VAL_VT(PDC_e_string_SE);
PDCI_DECL_VAL_VT(PDC_e_string_CSE);

PDCI_DECL_VAL_VT(PDC_a_date);
PDCI_DECL_VAL_VT(PDC_e_date);

#else
#define PDC_a_char_val_vtable         PDC_char_val_vtable
#define PDC_e_char_val_vtable         PDC_char_val_vtable

#define PDC_string_ME_val_vtable      PDC_string_val_vtable
#define PDC_string_CME_val_vtable     PDC_string_val_vtable
#define PDC_string_SE_val_vtable      PDC_string_val_vtable
#define PDC_string_CSE_val_vtable     PDC_string_val_vtable

#define PDC_a_string_val_vtable       PDC_string_val_vtable
#define PDC_a_string_ME_val_vtable    PDC_string_val_vtable
#define PDC_a_string_CME_val_vtable   PDC_string_val_vtable
#define PDC_a_string_SE_val_vtable    PDC_string_val_vtable
#define PDC_a_string_CSE_val_vtable   PDC_string_val_vtable

#define PDC_e_string_val_vtable       PDC_string_val_vtable
#define PDC_e_string_ME_val_vtable    PDC_string_val_vtable
#define PDC_e_string_CME_val_vtable   PDC_string_val_vtable
#define PDC_e_string_SE_val_vtable    PDC_string_val_vtable
#define PDC_e_string_CSE_val_vtable   PDC_string_val_vtable

#define PDC_a_date_val_vtable         PDC_a_date_val_vtable
#define PDC_e_date_val_vtable         PDC_e_date_val_vtable

#endif /* FOR_CKIT */

