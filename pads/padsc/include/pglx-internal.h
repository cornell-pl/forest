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

#include "padsc-internal.h"

#ifdef USE_GALAX
#include "glx.h"              /* Need to pack/unpack Galax atomic values */
#else

#ifndef FAKE_CAML_VALUE
#define FAKE_CAML_VALUE
typedef void* item;
#endif /* FAKE_CAML_VALUE */

/* make all the value types be value */
#define atomicString item
#define atomicBoolean item
#define atomicInt item
#define atomicInteger item
#define atomicDecimal item
#define atomicFloat item
#define atomicDouble item
#define atomicAnyURI item
#define atomicValue item

#define glx_err int

/* XXX_REMOVE next 14 lines: */
#ifdef FOR_CKIT
void* failwith(const char *);
glx_err glx_atomicString(char *s, atomicString *); 
glx_err glx_atomicBoolean(int b, atomicBoolean *);
glx_err glx_atomicInt(int i, atomicInt *); 
glx_err glx_atomicInteger(int i, atomicInteger *); 
glx_err glx_atomicDecimal(int i, atomicDecimal *); 
glx_err glx_atomicFloat(double f, atomicFloat *); 
glx_err glx_atomicDouble(double f, atomicDouble *); 
glx_err glx_atomicAnyURI(char* u, atomicAnyURI *);
glx_err glx_string_of_atomicValue(atomicValue, char **);
#else
#define prev_failwith(s) do { \
  char *s1 = (s); \
  char *s2 = (char*)malloc(strlen(s1) + 1); \
  strcpy(s2, s1); \
  return (void*)s2; \
} while(0)
#define failwith(s) error(ERROR_FATAL, "%s", s)

#define glx_atomicString(s, outval) fake_glx_atomicString(s, outval)
#define glx_atomicBoolean(b, outval)
#define glx_atomicInt(i, outval) fake_glx_atomicInt(i, outval)
#define glx_atomicInteger(i, outval) fake_glx_atomicInteger(i, outval)
#define glx_atomicDecimal(i, outval) fake_glx_atomicDecimal(i, outval)
#define glx_atomicFloat(f, outval) fake_glx_atomicFloat(f, outval)
#define glx_atomicDouble(f, outval) fake_glx_atomicDouble(f, outval)
#define glx_atomicAnyURI(u, outval) fake_glx_atomicAnyURI(u, outval)
#define glx_string_of_atomicValue(atomicValue, outval) fake_glx_string_of_atomicValue(atomicValue, outval)

glx_err fake_glx_atomicString(char *s, atomicString *); 
glx_err fake_glx_atomicBoolean(int b, atomicBoolean *);
glx_err fake_glx_atomicInt(int i, atomicInt *); 
glx_err fake_glx_atomicInteger(int i, atomicInteger *); 
glx_err fake_glx_atomicDecimal(int i, atomicDecimal *); 
glx_err fake_glx_atomicFloat(double f, atomicFloat *); 
glx_err fake_glx_atomicDouble(double f, atomicDouble *); 
glx_err fake_glx_atomicAnyURI(char* u, atomicAnyURI *);
glx_err fake_glx_string_of_atomicValue(atomicValue, char **);

#endif
#endif /* USE_GALAX */

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
PDCI_node_t *PDCI_NEW_NODE();
PDCI_node_t **PDCI_NEW_NODE_PTR_LIST(unsigned int num);
void PDCI_FREE_NODE(PDCI_node_t *n);
void PDCI_FREE_NODE_PTR_LIST(PDCI_node_t **list);

void PDCI_MK_TNODE(PDCI_node_t *result,
		   const PDCI_vtable_t *vt,
		   PDCI_node_t *parent,
		   const char *name, 
		   void* val, /* PDCI_structured_pd* val, */
		   const char *whatfn);

void  PDCI_MK_NODE(PDCI_node_t *result,
		   const PDCI_vtable_t *vt,
		   PDCI_node_t *parent,
		   const char *name, 
		   void* m, void* pd,
		   void* rep,
		   const char *whatfn);

void  PDCI_MK_TOP_NODE(PDCI_node_t *result,
		       const PDCI_vtable_t *vt,
		       PDC_t *pdc,
		       const char *name, 
		       void* m, void* pd,
		       void* rep,
		       const char *whatfn);
#endif

/* Helper macros that we always want expanded */

#define PDCI_DECL_VT(ty) \
extern const PDCI_vtable_t ty ## _vtable

#define PDCI_DECL_VAL_VT(ty) \
item ty ## typed_value(PDCI_node_t *node); \
extern const PDCI_vtable_t ty ## _val_vtable

/* ================================================================================
 * TYPES */

/* prototypes for vtable functions */
typedef PDCI_node_t **      (* PDCI_children_fn)      (PDCI_node_t *node); 
typedef item                (* PDCI_typed_value_fn)   (PDCI_node_t *node); 
typedef const char *        (* PDCI_string_value_fn)  (PDCI_node_t *node);

/* Type PDCI_node_t: */
struct PDCI_node_s {
  const PDCI_vtable_t   *vt;
  PDC_t                 *pdc;
  PDCI_node_t           *parent;
  void                  *m;
  void                  *pd;
  void                  *rep;
  const char            *name;
  const char            *kind;
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
  PDC_flags_t     pstate;
  PDC_errCode_t   errCode;
  PDC_loc_t       loc;
  int             nerr;
};

/* NB all generated sequenced pd types must BEGIN with the declarations given here: */

/* Type PDCI_sequenced_pd_t: */
struct PDCI_sequenced_pd_s {
  PDC_flags_t    pstate;
  PDC_errCode_t  errCode;
  PDC_loc_t      loc;
  int            nerr;
  int            neerr;		        
  int            firstError;		
};

/* ================================================================================
 * Helper functions */

/* Children functions */

PDCI_node_t ** PDC_base_pd_children(PDCI_node_t *self);
PDCI_node_t ** PDC_loc_t_children(PDCI_node_t *self);
PDCI_node_t ** PDC_pos_t_children(PDCI_node_t *self);

PDCI_node_t ** PDCI_structured_pd_children(PDCI_node_t *self);
PDCI_node_t ** PDCI_sequenced_pd_children(PDCI_node_t *self);
PDCI_node_t ** PDCI_no_children(PDCI_node_t *self);

/* Typed Value functions */

item PDCI_error_typed_value(PDCI_node_t *node); /* Error function used for many cases */
item PDCI_Cstr_typed_value (PDCI_node_t *node); /* node->rep is a C-style string (const char *) */

/* ================================================================================
 * VTABLES */

/* Special vtables */

PDCI_DECL_VT(PDCI_structured_pd);
PDCI_DECL_VT(PDCI_sequenced_pd);

PDCI_DECL_VT(PDC_base_pd);
PDCI_DECL_VT(PDC_loc_t);
PDCI_DECL_VT(PDC_pos_t);

/* Special val_vtables */

PDCI_DECL_VAL_VT(PDCI_Cstr);

/* Base type vtables */
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

PDCI_DECL_VT(PDC_int8);
PDCI_DECL_VT(PDC_int16);
PDCI_DECL_VT(PDC_int32);
PDCI_DECL_VT(PDC_int64);
PDCI_DECL_VT(PDC_uint8);
PDCI_DECL_VT(PDC_uint16);
PDCI_DECL_VT(PDC_uint32);
PDCI_DECL_VT(PDC_uint64);

/* We need one _val_vtable for each in-memory format.
   All of the PADS types that share an in-memory format 
   can share a vtable */

/* The required _val_vtable */
PDCI_DECL_VAL_VT(PDC_char);
PDCI_DECL_VAL_VT(PDC_string);
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

#endif /* FOR_CKIT */

#endif  /*   __PGX_INTERNAL_H__   */

