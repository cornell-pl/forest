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

typedef struct PDCI_path_s          PDCI_path_t;
typedef unsigned long               PDCI_childIndex_t;
typedef struct PDCI_node_s          PDCI_node_t;
typedef struct PDCI_vtable_s        PDCI_vtable_t;
typedef struct PDCI_structured_pd_s PDCI_structured_pd;
typedef struct PDCI_sequenced_pd_s  PDCI_sequenced_pd;

#include "node_mm.h"
#include "path_walk.h"
#include "smart.h"

/* ================================================================================
 * HELPER MACROS */

/* These macros are defined in pglx-impl.h.  Here we give prototypes for CKIT: */ 

#ifndef FOR_CKIT
#  include "pglx-impl.h"
#  include "pglx-codegen-macros-gen.h"
#else

void PDCI_NODE_CHECK(PDCI_node_t *n, const char *whatfn);
void PDCI_NODE_VT_CHECK(PDCI_node_t *n, const char *whatfn);
PDCI_node_t *PDCI_NEW_NODE(P_t *pads);
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
typedef int type_t;
typedef int field_t;

void NODE_NEW_BODY(type_t ty);
PDCI_node_t *NODE_NEW_RET();

void CACHED_NODE_INIT_BODY(type_t ty, int NUM_CHILDREN);
PDCI_node_t *CACHED_NODE_INIT_RET();

void SND_NODE_INIT_BODY(type_t ty);
PDCI_node_t
    *SND_NODE_INIT_RET();

void STR_NODE_KTH_CHILD_BODY_BEGIN(type_t ty);
void STR_NODE_KTH_CHILD_BODY_END();
PDCI_node_t *STR_NODE_KTH_CHILD_RET();

void NODE_KC_CASE(type_t ty, int fieldNumIN, type_t fieldTy, field_t fieldNameIN);
void NODE_KC_CASE_COMP(type_t ty, int fieldNumIN, type_t fieldTy, field_t fieldNameIN);
void STR_NODE_KTH_CHILD_NAMED_BODY(type_t ty, ...);
PDCI_node_t
    *STR_NODE_KTH_CHILD_NAMED_RET();

void CACHED_NODE_KTH_CHILD_BODY(type_t ty, int NUM_CHILDREN);
PDCI_node_t 
    *CACHED_NODE_KTH_CHILD_RET();

void STR_SND_NODE_KTH_CHILD_BODY_BEGIN(type_t ty);
void STR_SND_NODE_KTH_CHILD_BODY_END();
PDCI_node_t
    *STR_SND_NODE_KTH_CHILD_RET();
void SND_NODE_KC_CASE(type_t ty, int fieldNumIN,type_t fieldTy, field_t fieldNameIN);
void SND_NODE_KC_CASE_COMP(type_t ty, int fieldNumIN,type_t fieldTy, field_t fieldNameIN);

void STR_NODE_PATH_WALK_BODY_BEGIN();
void STR_NODE_PATH_WALK_BODY_END();
Perror_t STR_NODE_PATH_WALK_RET();

int  NODE_PW_CASE(fieldNumIN,fieldTy,fieldNameIN);

void VTABLE_DEFS(type_t ty);

void ARR_NODE_KTH_CHILD_BODY(type_t ty, type_t childTy);
PDCI_node_t 
    *ARR_NODE_KTH_CHILD_RET();

void ARR_NODE_KTH_CHILD_NAMED_BODY(type_t ty);
PDCI_node_t
    *ARR_NODE_KTH_CHILD_NAMED_RET();

int  ARR_LENGTH(type_t ty);

void ARR_SND_NODE_KTH_CHILD_BODY(type_t ty, type_t childTy);
PDCI_node_t *
     ARR_SND_NODE_KTH_CHILD_RET();

void ARR_NODE_PATH_WALK_BODY(type_t childTy);
Perror_t
     ARR_NODE_PATH_WALK_RET();
#endif
/* Helper macros that we always want expanded */

#define PDCI_DECL_NEW(ty) \
PDCI_node_t * ty ## _node_new(PDCI_node_t *parent, \
			 const char *name, \
			 void* m, void* pd, void* rep,\
			 const char *kind,const char *whatfn); \
\
PDCI_node_t * ty ## _sndNode_init(PDCI_node_t *self,          \
				  PDCI_smart_elt_info_t *elt, \
				  PDCI_gen_t gen, 	      \
				  PDCI_childIndex_t idx)

#define PDCI_DECL_VT(ty) \
extern const PDCI_vtable_t ty ## _node_vtable; \
extern const PDCI_vtable_t ty ## _cachedNode_vtable; \
extern const PDCI_vtable_t ty ## _sndNode_vtable

#define PDCI_DECL_NEW_VT(ty)\
PDCI_DECL_NEW(ty);\
PDCI_DECL_VT(ty)

#define PDCI_DECL_VAL_VT(ty) \
PDCI_node_t * ty ## _val_node_new(PDCI_node_t *parent, const char *name, \
			 void* rep, const char *whatfn);\
PDCI_node_t * ty ## _text_node_new(PDCI_node_t *parent, const char *whatfn);\
PDCI_node_t  * ty ## _val_cachedNode_init(PDCI_node_t *node); \
PDCI_node_t  * ty ## _text_cachedNode_init(PDCI_node_t *node); \
PDCI_node_t  * ty ## _val_sndNode_init(PDCI_node_t *node, PDCI_smart_elt_info_t *elt, \
				     PDCI_gen_t gen, PDCI_childIndex_t idx); \
PDCI_node_t  * ty ## _text_sndNode_init(PDCI_node_t *node, PDCI_smart_elt_info_t *elt, \
				     PDCI_gen_t gen, PDCI_childIndex_t idx); \
PDCI_node_t  * ty ## _val_node_kthChild(PDCI_node_t *node, PDCI_childIndex_t idx); \
PDCI_node_t  * ty ## _val_node_kthChildNamed(PDCI_node_t *node, PDCI_childIndex_t idx, const char *name); \
item ty ## _typed_value(PDCI_node_t *node); \
const char * ty ## _string_value(PDCI_node_t *node); \
extern const PDCI_vtable_t ty ## _val_node_vtable; \
extern const PDCI_vtable_t ty ## _val_cachedNode_vtable; \
extern const PDCI_vtable_t ty ## _val_sndNode_vtable; \
extern const PDCI_vtable_t ty ## _text_node_vtable; \
extern const PDCI_vtable_t ty ## _text_cachedNode_vtable; \
extern const PDCI_vtable_t ty ## _text_sndNode_vtable

/* ================================================================================
 * TYPES */

/* prototypes for vtable functions */
typedef PDCI_node_t **       (* PDCI_children_fn)  (PDCI_node_t *node); 
typedef PDCI_node_t *       (* PDCI_cachedNode_init_fn)  (PDCI_node_t *node); 
typedef PDCI_node_t *       (* PDCI_kth_child_fn)        (PDCI_node_t *node, PDCI_childIndex_t idx); 
typedef PDCI_node_t *       (* PDCI_kth_child_named_fn)  (PDCI_node_t *node, PDCI_childIndex_t idx, const char *name); 
typedef void                (* PDCI_free_fn)             (PDCI_node_t *node);
typedef item                (* PDCI_typed_value_fn)      (PDCI_node_t *node); 
typedef const char *        (* PDCI_string_value_fn)     (PDCI_node_t *node);

/* Type PDCI_node_t: */
struct PDCI_node_s {
  const PDCI_vtable_t     *vt;
  P_t                     *pads;

  void                    *m;
  void                    *pd;
  void                    *rep;
  const char              *name;
  const char              *kind;
  PDCI_node_t             *parent;

  // reference counting
  unsigned long            rc;

  // Used by smart node descendents

  /* 
   *  ancestor: pointer to ancestor element.
   *  ancestor_gen: generation of ancestor data.
   */     
  PDCI_smart_elt_info_t   *ancestor;     
  PDCI_gen_t               ancestor_gen;

  /*
   * index of object in relation to parent
   */
  PDCI_childIndex_t        idx;
  
  // Used by smart nodes:
  PDCI_smart_node_t       *snExt;

  // Used by caching nodes
  PDCI_node_t           **child_cache;  
};

/* Type PDCI_vtable_t: */
struct PDCI_vtable_s {
#ifdef OLD_PGLX
  PDCI_children_fn          children;
#else
  PDCI_cachedNode_init_fn    cachedNode_init;
  PDCI_kth_child_fn         kth_child;
  PDCI_kth_child_named_fn   kth_child_named;
  PDCI_free_fn              free;
#endif
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

/* Node new functions */

PDCI_node_t * Pbase_pd_node_new(PDCI_node_t *parent, const char *name, 
			      void* rep, const char *whatfn);
PDCI_node_t * Ploc_t_node_new(PDCI_node_t *parent, const char *name, 
			      void* rep, const char *whatfn);
PDCI_node_t * Ppos_t_node_new(PDCI_node_t *parent, const char *name, 
			      void* rep, const char *whatfn);
PDCI_node_t * PDCI_structured_pd_node_new(PDCI_node_t *parent, const char *name, 
			      void* rep, const char *whatfn);
PDCI_node_t * PDCI_sequenced_pd_node_new(PDCI_node_t *parent, const char *name, 
			      void* rep, const char *whatfn);

/* cachedNode init functions */

PDCI_node_t  * Pbase_pd_cachedNode_init(PDCI_node_t *self);
PDCI_node_t  * Ploc_t_cachedNode_init(PDCI_node_t *self);
PDCI_node_t  * Ppos_t_cachedNode_init(PDCI_node_t *self);
PDCI_node_t  * PDCI_structured_pd_cachedNode_init(PDCI_node_t *self);
PDCI_node_t  * PDCI_sequenced_pd_cachedNode_init(PDCI_node_t *self);

/* sndNode init functions */

PDCI_node_t  * Pbase_pd_sndNode_init(PDCI_node_t *self, PDCI_smart_elt_info_t *elt, 
				     PDCI_gen_t gen, PDCI_childIndex_t idx);
PDCI_node_t  * Ploc_t_sndNode_init(PDCI_node_t *self, PDCI_smart_elt_info_t *elt, 
				     PDCI_gen_t gen, PDCI_childIndex_t idx);
PDCI_node_t  * Ppos_t_sndNode_init(PDCI_node_t *self, PDCI_smart_elt_info_t *elt, 
				     PDCI_gen_t gen, PDCI_childIndex_t idx);
PDCI_node_t  * PDCI_structured_pd_sndNode_init(PDCI_node_t *self, PDCI_smart_elt_info_t *elt, 
				     PDCI_gen_t gen, PDCI_childIndex_t idx);
PDCI_node_t  * PDCI_sequenced_pd_sndNode_init(PDCI_node_t *self, PDCI_smart_elt_info_t *elt, 
				     PDCI_gen_t gen, PDCI_childIndex_t idx);

/* children functions that return an array - will go away */

PDCI_node_t ** Pbase_pd_children(PDCI_node_t *self);
PDCI_node_t ** Ploc_t_children(PDCI_node_t *self);
PDCI_node_t ** Ppos_t_children(PDCI_node_t *self);
PDCI_node_t ** PDCI_structured_pd_children(PDCI_node_t *self);
PDCI_node_t ** PDCI_sequenced_pd_children(PDCI_node_t *self);
PDCI_node_t ** PDCI_no_children(PDCI_node_t *self);

/* Node Kth child functions */

PDCI_node_t  * Pbase_pd_node_kthChild(PDCI_node_t *self, PDCI_childIndex_t idx);
PDCI_node_t  * Ploc_t_node_kthChild(PDCI_node_t *self, PDCI_childIndex_t idx);
PDCI_node_t  * Ppos_t_node_kthChild(PDCI_node_t *self, PDCI_childIndex_t idx);
PDCI_node_t  * PDCI_structured_pd_node_kthChild(PDCI_node_t *self, PDCI_childIndex_t idx);
PDCI_node_t  * PDCI_sequenced_pd_node_kthChild(PDCI_node_t *self, PDCI_childIndex_t idx);

/* Node Kth child named functions */

PDCI_node_t  * Pbase_pd_node_kthChildNamed(PDCI_node_t *self, PDCI_childIndex_t idx, const char *name);
PDCI_node_t  * Ploc_t_node_kthChildNamed(PDCI_node_t *self, PDCI_childIndex_t idx, const char *name);
PDCI_node_t  * Ppos_t_node_kthChildNamed(PDCI_node_t *self, PDCI_childIndex_t idx, const char *name);
PDCI_node_t  * PDCI_structured_pd_node_kthChildNamed(PDCI_node_t *self, PDCI_childIndex_t idx, const char *name);
PDCI_node_t  * PDCI_sequenced_pd_node_kthChildNamed(PDCI_node_t *self, PDCI_childIndex_t idx, const char *name);

/* cachedNode Kth child functions */

PDCI_node_t  * Pbase_pd_cachedNode_kthChild(PDCI_node_t *self, PDCI_childIndex_t idx);
PDCI_node_t  * Ploc_t_cachedNode_kthChild(PDCI_node_t *self, PDCI_childIndex_t idx);
PDCI_node_t  * Ppos_t_cachedNode_kthChild(PDCI_node_t *self, PDCI_childIndex_t idx);
PDCI_node_t  * PDCI_structured_pd_cachedNode_kthChild(PDCI_node_t *self, PDCI_childIndex_t idx);
PDCI_node_t  * PDCI_sequenced_pd_cachedNode_kthChild(PDCI_node_t *self, PDCI_childIndex_t idx);

/* sndNode Kth child functions */

PDCI_node_t  * Pbase_pd_sndNode_kthChild(PDCI_node_t *self, PDCI_childIndex_t idx);
PDCI_node_t  * Ploc_t_sndNode_kthChild(PDCI_node_t *self, PDCI_childIndex_t idx);
PDCI_node_t  * Ppos_t_sndNode_kthChild(PDCI_node_t *self, PDCI_childIndex_t idx);
PDCI_node_t  * PDCI_structured_pd_sndNode_kthChild(PDCI_node_t *self, PDCI_childIndex_t idx);
PDCI_node_t  * PDCI_sequenced_pd_sndNode_kthChild(PDCI_node_t *self, PDCI_childIndex_t idx);

/* Helpers for nodes with no children */
PDCI_node_t  * PDCI_node_no_kthChild(PDCI_node_t *self, PDCI_childIndex_t idx);
PDCI_node_t  * PDCI_node_no_kthChildNamed(PDCI_node_t *self, PDCI_childIndex_t idx, const char *name);


/* Error function used to prevent reinitialization of cachedNode caches. */
PDCI_node_t *PDCI_error_cachedNode_init(PDCI_node_t *node); 

/* General-purpose memory management vtable functions. */
void PDCI_node_free(PDCI_node_t *node);
void PDCI_cachedNode_free(PDCI_node_t *node);

/* Error and Typed Value functions */
PDCI_node_t *PDCI_error_cachedNode_init(PDCI_node_t *node);
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
PDCI_DECL_NEW_VT(Pchar);
PDCI_DECL_NEW_VT(Pa_char);
PDCI_DECL_NEW_VT(Pe_char);

PDCI_DECL_NEW_VT(Pstring);
PDCI_DECL_NEW_VT(Pstring_ME);
PDCI_DECL_NEW_VT(Pstring_CME);
PDCI_DECL_NEW_VT(Pstring_SE);
PDCI_DECL_NEW_VT(Pstring_CSE);

PDCI_DECL_NEW_VT(Pa_string);
PDCI_DECL_NEW_VT(Pa_string_ME);
PDCI_DECL_NEW_VT(Pa_string_CME);
PDCI_DECL_NEW_VT(Pa_string_SE);
PDCI_DECL_NEW_VT(Pa_string_CSE);

PDCI_DECL_NEW_VT(Pe_string);
PDCI_DECL_NEW_VT(Pe_string_ME);
PDCI_DECL_NEW_VT(Pe_string_CME);
PDCI_DECL_NEW_VT(Pe_string_SE);
PDCI_DECL_NEW_VT(Pe_string_CSE);

PDCI_DECL_NEW_VT(Pint8);
PDCI_DECL_NEW_VT(Pint16);
PDCI_DECL_NEW_VT(Pint32);
PDCI_DECL_NEW_VT(Pint64);
PDCI_DECL_NEW_VT(Puint8);
PDCI_DECL_NEW_VT(Puint16);
PDCI_DECL_NEW_VT(Puint32);
PDCI_DECL_NEW_VT(Puint64);

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

