#ifdef _USE_PROTO
#pragma prototyped
#endif
/*
 * internal APIs, galax-pads
 * 
 * Kathleen Fisher, Robert Gruber
 * AT&T Labs Research
 */

#ifndef __PGLX_INTERNAL_H__
#define __PGLX_INTERNAL_H__

#include "pads-internal.h"

#include "galax.h"              /* Need to pack/unpack Galax atomic values */

/* ================================================================================
 * Predeclare some types */

typedef struct PDCI_node_s          PDCI_node_t;
typedef struct PDCI_vtable_s        PDCI_vtable_t;
typedef struct PDCI_structured_pd_s PDCI_structured_pd;
typedef struct PDCI_sequenced_pd_s  PDCI_sequenced_pd;

/* ================================================================================
 * HELPER MACROS */

/* These macros are defind in pglx-impl.h.  Here we give prototypes for CKIT: */ 

#ifndef FOR_CKIT
#  include "pglx-impl.h"

#else

void PDCI_NODE_CHECK(PDCI_node_t *n, const char *whatfn);
void PDCI_NODE_VT_CHECK(PDCI_node_t *n, const char *whatfn);
PDCI_node_t *PDCI_NEW_NODE();
PDCI_node_t **PDCI_NEW_NODE_PTR_LIST(unsigned int num);
void PDCI_FREE_NODE(PDCI_node_t *n);
void PDCI_FREE_NODE_PTR_LIST(PDCI_node_t **list);

void PDCI_MK_TNODE(PDCI_node_t *result,
		   const PDCI_vtable_t *vt,
		   PDCI_node_t *parent,
		   const char *name, 
		   void* val, /* rep* */
		   const char *whatfn);

void PDCI_MK_TEXTNODE(PDCI_node_t *result,
		      const PDCI_vtable_t *vt,
		      PDCI_node_t *parent,
		      void* val, /* rep* */
		      const char *whatfn);

void  PDCI_MK_NODE(PDCI_node_t *result,
		   const PDCI_vtable_t *vt,
		   PDCI_node_t *parent,
		   const char *name, 
		   void* m, void* pd,
		   void* rep,
		   const char *kind,
		   const char *whatfn);

void  PDCI_MK_TOP_NODE(PDCI_node_t *result,
		       const PDCI_vtable_t *vt,
		       P_t *pads,
		       const char *name, 
		       void* m, void* pd,
		       void* rep,
		       const char *whatfn);
#endif

/* Helper macros that we always want expanded */

#define PDCI_DECL_VT(ty) \
extern const PDCI_vtable_t ty ## _vtable

#define PDCI_DECL_VAL_VT(ty) \
PDCI_node_t ** ty ## _val_children(PDCI_node_t *node); \
PDCI_node_t  * ty ## _val_kth_child(PDCI_node_t *node, childIndex idx); \
PDCI_node_t  * ty ## _val_kth_child_named(PDCI_node_t *node, childIndex idx, const char *name); \
item ty ## _typed_value(PDCI_node_t *node); \
const char * ty ## _string_value(PDCI_node_t *node); \
extern const PDCI_vtable_t ty ## _val_vtable; \
extern const PDCI_vtable_t ty ## _text_vtable

/* ================================================================================
 * TYPES */

/* prototypes for vtable functions */
typedef PDCI_node_t **      (* PDCI_children_fn)         (PDCI_node_t *node); 
typedef PDCI_node_t *       (* PDCI_kth_child_fn)        (PDCI_node_t *node, childIndex idx); 
typedef PDCI_node_t *       (* PDCI_kth_child_named_fn)  (PDCI_node_t *node, childIndex idx, const char *name); 
typedef item                (* PDCI_typed_value_fn)      (PDCI_node_t *node); 
typedef const char *        (* PDCI_string_value_fn)     (PDCI_node_t *node);

/* Type PDCI_node_t: */
struct PDCI_node_s {
  const PDCI_vtable_t   *vt;
  P_t                   *pads;
  PDCI_node_t           *parent;
  void                  *m;
  void                  *pd;
  void                  *rep;
  const char            *name;
  const char            *kind;
};

/* Type PDCI_vtable_t: */
struct PDCI_vtable_s {
  PDCI_children_fn          children;
  PDCI_kth_child_fn         kth_child;
  PDCI_kth_child_named_fn   kth_child_named;
  PDCI_typed_value_fn       typed_value;
  PDCI_string_value_fn      string_value;
};

/* PARSE DESCRIPTOR SUPPORT */
/* NB all generated structured pd types must BEGIN with the declarations given here: */

/* type PDCI_structured_pd: */
struct PDCI_structured_pd_s {
  Pflags_t     pstate;
  int          nerr;
  PerrCode_t   errCode;
  Ploc_t       loc;
};

/* NB all generated sequenced pd types must BEGIN with the declarations given here: */

/* Type PDCI_sequenced_pd_t: */
struct PDCI_sequenced_pd_s {
  Pflags_t    pstate;
  int         nerr;
  PerrCode_t  errCode;
  Ploc_t      loc;
  int         neerr;		        
  int         firstError;		
};

/* ================================================================================
 * Helper functions */

/* children functions that return an array - will go away */

PDCI_node_t ** Pbase_pd_children(PDCI_node_t *self);
PDCI_node_t ** Ploc_t_children(PDCI_node_t *self);
PDCI_node_t ** Ppos_t_children(PDCI_node_t *self);
PDCI_node_t ** PDCI_structured_pd_children(PDCI_node_t *self);
PDCI_node_t ** PDCI_sequenced_pd_children(PDCI_node_t *self);
PDCI_node_t ** PDCI_no_children(PDCI_node_t *self);

/* Kth child functions */

PDCI_node_t  * Pbase_pd_kth_child(PDCI_node_t *self, childIndex idx);
PDCI_node_t  * Ploc_t_kth_child(PDCI_node_t *self, childIndex idx);
PDCI_node_t  * Ppos_t_kth_child(PDCI_node_t *self, childIndex idx);
PDCI_node_t  * PDCI_structured_pd_kth_child(PDCI_node_t *self, childIndex idx);
PDCI_node_t  * PDCI_sequenced_pd_kth_child(PDCI_node_t *self, childIndex idx);

/* Kth child named functions */

PDCI_node_t  * Pbase_pd_kth_child_named(PDCI_node_t *self, childIndex idx, const char *name);
PDCI_node_t  * Ploc_t_kth_child_named(PDCI_node_t *self, childIndex idx, const char *name);
PDCI_node_t  * Ppos_t_kth_child_named(PDCI_node_t *self, childIndex idx, const char *name);
PDCI_node_t  * PDCI_structured_pd_kth_child_named(PDCI_node_t *self, childIndex idx, const char *name);
PDCI_node_t  * PDCI_sequenced_pd_kth_child_named(PDCI_node_t *self, childIndex idx, const char *name);

/* Helpers for nodes with no children */
PDCI_node_t  * PDCI_no_kth_child(PDCI_node_t *self, childIndex idx);
PDCI_node_t  * PDCI_no_kth_child_named(PDCI_node_t *self, childIndex idx, const char *name);

/* Typed Value functions */

item PDCI_error_typed_value(PDCI_node_t *node); /* Error function used for many cases */
item PDCI_cstr_typed_value (PDCI_node_t *node); /* node->rep is a C-style string (const char *) */

/* String Value functions */
const char * PDCI_not_impl_yet_string_value(PDCI_node_t *node);

/* ================================================================================
 * VTABLES */

/* Special vtables */

PDCI_DECL_VT(PDCI_structured_pd);
PDCI_DECL_VT(PDCI_sequenced_pd);

PDCI_DECL_VT(Pbase_pd);
PDCI_DECL_VT(Ploc_t);
PDCI_DECL_VT(Ppos_t);

/* Special val_vtables */

PDCI_DECL_VAL_VT(PDCI_cstr);

/* Base type vtables */
PDCI_DECL_VT(Pchar);
PDCI_DECL_VT(Pa_char);
PDCI_DECL_VT(Pe_char);

PDCI_DECL_VT(Pstring);
PDCI_DECL_VT(Pstring_ME);
PDCI_DECL_VT(Pstring_CME);
PDCI_DECL_VT(Pstring_SE);
PDCI_DECL_VT(Pstring_CSE);

PDCI_DECL_VT(Pa_string);
PDCI_DECL_VT(Pa_string_ME);
PDCI_DECL_VT(Pa_string_CME);
PDCI_DECL_VT(Pa_string_SE);
PDCI_DECL_VT(Pa_string_CSE);

PDCI_DECL_VT(Pe_string);
PDCI_DECL_VT(Pe_string_ME);
PDCI_DECL_VT(Pe_string_CME);
PDCI_DECL_VT(Pe_string_SE);
PDCI_DECL_VT(Pe_string_CSE);

PDCI_DECL_VT(Pint8);
PDCI_DECL_VT(Pint16);
PDCI_DECL_VT(Pint32);
PDCI_DECL_VT(Pint64);
PDCI_DECL_VT(Puint8);
PDCI_DECL_VT(Puint16);
PDCI_DECL_VT(Puint32);
PDCI_DECL_VT(Puint64);

/* We need one _val_vtable for each in-memory format.
   All of the PADS types that share an in-memory format 
   can share a vtable */

/* The required _val_vtable */
PDCI_DECL_VAL_VT(Pchar);
PDCI_DECL_VAL_VT(Pstring);
PDCI_DECL_VAL_VT(Pint8);
PDCI_DECL_VAL_VT(Pint16);
PDCI_DECL_VAL_VT(Pint32);
PDCI_DECL_VAL_VT(Pint64);
PDCI_DECL_VAL_VT(Puint8);
PDCI_DECL_VAL_VT(Puint16);
PDCI_DECL_VAL_VT(Puint32);
PDCI_DECL_VAL_VT(Puint64);

/* The cases where we can use vtable sharing */
#ifdef FOR_CKIT
PDCI_DECL_VAL_VT(Pa_char);
PDCI_DECL_VAL_VT(Pe_char);

PDCI_DECL_VAL_VT(Pstring_ME);
PDCI_DECL_VAL_VT(Pstring_CME);
PDCI_DECL_VAL_VT(Pstring_SE);
PDCI_DECL_VAL_VT(Pstring_CSE);

PDCI_DECL_VAL_VT(Pa_string);
PDCI_DECL_VAL_VT(Pa_string_ME);
PDCI_DECL_VAL_VT(Pa_string_CME);
PDCI_DECL_VAL_VT(Pa_string_SE);
PDCI_DECL_VAL_VT(Pa_string_CSE);

PDCI_DECL_VAL_VT(Pe_string);
PDCI_DECL_VAL_VT(Pe_string_ME);
PDCI_DECL_VAL_VT(Pe_string_CME);
PDCI_DECL_VAL_VT(Pe_string_SE);
PDCI_DECL_VAL_VT(Pe_string_CSE);

#else
#define Pa_char_val_vtable         Pchar_val_vtable
#define Pe_char_val_vtable         Pchar_val_vtable

#define Pstring_ME_val_vtable      Pstring_val_vtable
#define Pstring_CME_val_vtable     Pstring_val_vtable
#define Pstring_SE_val_vtable      Pstring_val_vtable
#define Pstring_CSE_val_vtable     Pstring_val_vtable

#define Pa_string_val_vtable       Pstring_val_vtable
#define Pa_string_ME_val_vtable    Pstring_val_vtable
#define Pa_string_CME_val_vtable   Pstring_val_vtable
#define Pa_string_SE_val_vtable    Pstring_val_vtable
#define Pa_string_CSE_val_vtable   Pstring_val_vtable

#define Pe_string_val_vtable       Pstring_val_vtable
#define Pe_string_ME_val_vtable    Pstring_val_vtable
#define Pe_string_CME_val_vtable   Pstring_val_vtable
#define Pe_string_SE_val_vtable    Pstring_val_vtable
#define Pe_string_CSE_val_vtable   Pstring_val_vtable

#endif /* FOR_CKIT */

#endif  /*   __PGX_INTERNAL_H__   */

