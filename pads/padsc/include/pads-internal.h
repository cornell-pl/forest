#ifdef _USE_PROTO
#pragma prototyped
#endif
/*
 * padc library interface -- internal functions
 * 
 * Kathleen Fisher, Robert Gruber
 * AT&T Labs Research
 */

#ifndef __PADS_INTERNAL__
#define __PADS_INTERNAL__

/* ================================================================================ */

#include "pads.h"
#include "out_macros.h"

/* ================================================================================ */
/* NOT SURE WHERE THESE MACROS BELONG */

/* vmalloc macros:
 *   vmnewof copies existing bytes and zeroes extension
 *   vmoldof does not copy existing bytes and does not zero extension
 *   vmcpyoldof copies existing bytes but does not zero extension
 *
 * The first 2 are provided, the last one is here:
 */

#define vmcpyoldof(v,p,t,n,x) \
  (t*)vmresize((v), (p), sizeof(t)*(n)+(x), (VM_RSMOVE|VM_RSCOPY) )

/* ================================================================================
 * MACROS used by generated code as well as library code
 */

#ifndef PDCI_MacroArg2String
#define PDCI_MacroArg2String(s) #s
#endif

/* ----------------------
 * P_REGEXP_DECL macros
 * ----------------------
 */

#define P_REGEXP_DECL_NULL(my_regexp) \
  Pregexp_t my_regexp = { 0 }

/* ----------------------
 * P_STRING_DECL macros
 * ----------------------
 * (note final args 0, 1 ==> initial null rbuf ptr, sharing set)
 */

#define P_STRING_DECL_NULL(var_nm) \
  Pstring var_nm = { 0, 0, 0, 0 }

#define P_STRING_DECL_LIT(var_nm, str_lit) \
  Pstring var_nm = { str_lit, sizeof(str_lit), 0, 1 }

#define P_STRING_DECL_CSTR(var_nm, cstr_expr) \
  Pstring var_nm = { cstr_expr, strlen(cstr_expr), 0, 1 }

#define P_STRING_DECL_CSTR_LEN(var_nm, char_ptr_expr, length_expr) \
  Pstring var_nm = { char_ptr_expr, length_expr, 0, 1 }

/* --------------------------------
 * Macros that need CKIT prototypes
 * --------------------------------
 */

#ifdef FOR_CKIT
/* Prototypes for CKIT */

void PDCI_DISC_INIT_CHECKS(char * whatfn);
void PDCI_DISC_INIT_CHECKS_RET_0(char * whatfn);
void PDCI_DISC_INIT_CHECKS_RET_VOID(char * whatfn);
void PDCI_DISC_INIT_CHECKS_RET_SSIZE(char * whatfn);

void PDCI_IODISC_INIT_CHECKS(char * whatfn);
void PDCI_IODISC_INIT_CHECKS_RET_0(char * whatfn);
void PDCI_IODISC_INIT_CHECKS_RET_VOID(char * whatfn);
void PDCI_IODISC_INIT_CHECKS_RET_SSIZE(char * whatfn);

void PDCI_NULLPARAM_CHECK(char *, void *);
void PDCI_NULLPARAM_CHECK_RET_0(char *, void *);
void PDCI_NULLPARAM_CHECK_RET_VOID(char *, void *);
void PDCI_NULLPARAM_CHECK_RET_SSIZE(char *, void *);

void PDCI_DISC_0P_CHECKS(const char *whatfn);
void PDCI_DISC_1P_CHECKS(const char *whatfn, void *p1);
void PDCI_DISC_2P_CHECKS(const char *whatfn, void *p1, void *p2);
void PDCI_DISC_3P_CHECKS(const char *whatfn, void *p1, void *p2, void *p3);
void PDCI_DISC_4P_CHECKS(const char *whatfn, void *p1, void *p2, void *p3, void *p4);

void PDCI_DISC_0P_CHECKS_RET_0(const char *whatfn);
void PDCI_DISC_1P_CHECKS_RET_0(const char *whatfn, void *p1);
void PDCI_DISC_2P_CHECKS_RET_0(const char *whatfn, void *p1, void *p2);
void PDCI_DISC_3P_CHECKS_RET_0(const char *whatfn, void *p1, void *p2, void *p3);
void PDCI_DISC_4P_CHECKS_RET_0(const char *whatfn, void *p1, void *p2, void *p3, void *p4);

void PDCI_DISC_0P_CHECKS_RET_VOID(const char *whatfn);
void PDCI_DISC_1P_CHECKS_RET_VOID(const char *whatfn, void *p1);
void PDCI_DISC_2P_CHECKS_RET_VOID(const char *whatfn, void *p1, void *p2);
void PDCI_DISC_3P_CHECKS_RET_VOID(const char *whatfn, void *p1, void *p2, void *p3);
void PDCI_DISC_4P_CHECKS_RET_VOID(const char *whatfn, void *p1, void *p2, void *p3, void *p4);

void PDCI_DISC_0P_CHECKS_RET_SSIZE(const char *whatfn);
void PDCI_DISC_1P_CHECKS_RET_SSIZE(const char *whatfn, void *p1);
void PDCI_DISC_2P_CHECKS_RET_SSIZE(const char *whatfn, void *p1, void *p2);
void PDCI_DISC_3P_CHECKS_RET_SSIZE(const char *whatfn, void *p1, void *p2, void *p3);
void PDCI_DISC_4P_CHECKS_RET_SSIZE(const char *whatfn, void *p1, void *p2, void *p3, void *p4);

void PDCI_IODISC_0P_CHECKS(const char *whatfn);
void PDCI_IODISC_1P_CHECKS(const char *whatfn, void *p1);
void PDCI_IODISC_2P_CHECKS(const char *whatfn, void *p1, void *p2);
void PDCI_IODISC_3P_CHECKS(const char *whatfn, void *p1, void *p2, void *p3);
void PDCI_IODISC_4P_CHECKS(const char *whatfn, void *p1, void *p2, void *p3, void *p4);

void PDCI_IODISC_0P_CHECKS_RET_SSIZE(const char *whatfn);
void PDCI_IODISC_1P_CHECKS_RET_SSIZE(const char *whatfn, void *p1);
void PDCI_IODISC_2P_CHECKS_RET_SSIZE(const char *whatfn, void *p1, void *p2);
void PDCI_IODISC_3P_CHECKS_RET_SSIZE(const char *whatfn, void *p1, void *p2, void *p3);
void PDCI_IODISC_4P_CHECKS_RET_SSIZE(const char *whatfn, void *p1, void *p2, void *p3, void *p4);

void PDCI_READFN_WIDTH_CHECK(const char *whatfn, const char *elt_descr, size_t width);

Pinv_valfn PDCI_GET_INV_VALFN(P_t *, const char *);

void PDCI_fill_mask(Pbase_m* mask, Pbase_m m, size_t mask_size);

#else
/* The actual impls */

#ifndef NDEBUG
/* DEBUG VERSIONS */

#define PDCI_DISC_INIT_CHECKS_RET(whatfn, ret) \
  do { \
    if (!pads)  { \
      P_WARN1(&Pdefault_disc, "%s: null pads param", whatfn); \
      ret; \
    } \
    if (!pads->disc) { \
      P_WARN1(&Pdefault_disc, "%s: null pads->disc", whatfn); \
      ret; \
    } \
    P_TRACE1(pads->disc, "%s called", whatfn); \
  } while (0)

#define PDCI_DISC_INIT_CHECKS(whatfn) \
     PDCI_DISC_INIT_CHECKS_RET(whatfn, return P_ERR)

#define PDCI_DISC_INIT_CHECKS_RET_0(whatfn) \
     PDCI_DISC_INIT_CHECKS_RET(whatfn, return 0)

#define PDCI_DISC_INIT_CHECKS_RET_VOID(whatfn) \
     PDCI_DISC_INIT_CHECKS_RET(whatfn, return)

#define PDCI_DISC_INIT_CHECKS_RET_SSIZE(whatfn) \
     PDCI_DISC_INIT_CHECKS_RET(whatfn, return -1)

#define PDCI_IODISC_INIT_CHECKS_RET(whatfn, ret) \
  do { \
    if (!pads)  { \
      P_WARN1(&Pdefault_disc, "%s: null pads param", whatfn); \
      ret; \
    } \
    if (!pads->disc) { \
      P_WARN1(&Pdefault_disc, "%s: null pads->disc", whatfn); \
      ret; \
    } \
    P_TRACE1(pads->disc, "%s called", whatfn); \
    if (!pads->disc->io_disc) { \
      P_WARN1(pads->disc, "%s: IO discipline not installed", whatfn); \
      ret; \
    } \
  } while (0)

#define PDCI_IODISC_INIT_CHECKS(whatfn) \
     PDCI_IODISC_INIT_CHECKS_RET(whatfn, return P_ERR)

#define PDCI_IODISC_INIT_CHECKS_RET_0(whatfn) \
     PDCI_IODISC_INIT_CHECKS_RET(whatfn, return 0)

#define PDCI_IODISC_INIT_CHECKS_RET_VOID(whatfn) \
     PDCI_IODISC_INIT_CHECKS_RET(whatfn, return)

#define PDCI_IODISC_INIT_CHECKS_RET_SSIZE(whatfn) \
     PDCI_IODISC_INIT_CHECKS_RET(whatfn, return -1)

/* Assumes pads and disc already checked */
#define PDCI_NULLPARAM_CHECK_RET(whatfn, param, ret) \
  do { \
    if (!(param))  { \
      P_WARN1(pads->disc, "%s: param " PDCI_MacroArg2String(param) " must not be NULL", whatfn); \
      ret; \
    } \
  } while (0)

#define PDCI_NULLPARAM_CHECK(whatfn, param) \
     PDCI_NULLPARAM_CHECK_RET(whatfn, param, return P_ERR)

#define PDCI_NULLPARAM_CHECK_RET_0(whatfn, param) \
     PDCI_NULLPARAM_CHECK_RET(whatfn, param, return 0)

#define PDCI_NULLPARAM_CHECK_RET_VOID(whatfn, param) \
     PDCI_NULLPARAM_CHECK_RET(whatfn, param, return)

#define PDCI_NULLPARAM_CHECK_RET_SSIZE(whatfn, param) \
     PDCI_NULLPARAM_CHECK_RET(whatfn, param, return -1)

#define PDCI_DISC_0P_CHECKS(whatfn) \
     PDCI_DISC_INIT_CHECKS(whatfn)

#define PDCI_DISC_1P_CHECKS(whatfn, p1) \
do { \
  PDCI_DISC_INIT_CHECKS (whatfn); \
  PDCI_NULLPARAM_CHECK (whatfn, p1); \
} while (0)

#define PDCI_DISC_2P_CHECKS(whatfn, p1, p2) \
do { \
  PDCI_DISC_INIT_CHECKS (whatfn); \
  PDCI_NULLPARAM_CHECK (whatfn, p1); \
  PDCI_NULLPARAM_CHECK (whatfn, p2); \
} while (0)

#define PDCI_DISC_3P_CHECKS(whatfn, p1, p2, p3) \
do { \
  PDCI_DISC_INIT_CHECKS (whatfn); \
  PDCI_NULLPARAM_CHECK (whatfn, p1); \
  PDCI_NULLPARAM_CHECK (whatfn, p2); \
  PDCI_NULLPARAM_CHECK (whatfn, p3); \
} while (0)

#define PDCI_DISC_4P_CHECKS(whatfn, p1, p2, p3, p4) \
do { \
  PDCI_DISC_INIT_CHECKS (whatfn); \
  PDCI_NULLPARAM_CHECK (whatfn, p1); \
  PDCI_NULLPARAM_CHECK (whatfn, p2); \
  PDCI_NULLPARAM_CHECK (whatfn, p3); \
  PDCI_NULLPARAM_CHECK (whatfn, p4); \
} while (0)

#define PDCI_DISC_0P_CHECKS_RET_0(whatfn) \
     PDCI_DISC_INIT_CHECKS_RET_0(whatfn)

#define PDCI_DISC_1P_CHECKS_RET_0(whatfn, p1) \
do { \
  PDCI_DISC_INIT_CHECKS_RET_0 (whatfn); \
  PDCI_NULLPARAM_CHECK_RET_0 (whatfn, p1); \
} while (0)

#define PDCI_DISC_2P_CHECKS_RET_0(whatfn, p1, p2) \
do { \
  PDCI_DISC_INIT_CHECKS_RET_0 (whatfn); \
  PDCI_NULLPARAM_CHECK_RET_0 (whatfn, p1); \
  PDCI_NULLPARAM_CHECK_RET_0 (whatfn, p2); \
} while (0)

#define PDCI_DISC_3P_CHECKS_RET_0(whatfn, p1, p2, p3) \
do { \
  PDCI_DISC_INIT_CHECKS_RET_0 (whatfn); \
  PDCI_NULLPARAM_CHECK_RET_0 (whatfn, p1); \
  PDCI_NULLPARAM_CHECK_RET_0 (whatfn, p2); \
  PDCI_NULLPARAM_CHECK_RET_0 (whatfn, p3); \
} while (0)

#define PDCI_DISC_4P_CHECKS_RET_0(whatfn, p1, p2, p3, p4) \
do { \
  PDCI_DISC_INIT_CHECKS_RET_0 (whatfn); \
  PDCI_NULLPARAM_CHECK_RET_0 (whatfn, p1); \
  PDCI_NULLPARAM_CHECK_RET_0 (whatfn, p2); \
  PDCI_NULLPARAM_CHECK_RET_0 (whatfn, p3); \
  PDCI_NULLPARAM_CHECK_RET_0 (whatfn, p4); \
} while (0)

#define PDCI_DISC_0P_CHECKS_RET_VOID(whatfn) \
     PDCI_DISC_INIT_CHECKS_RET_VOID(whatfn)

#define PDCI_DISC_1P_CHECKS_RET_VOID(whatfn, p1) \
do { \
  PDCI_DISC_INIT_CHECKS_RET_VOID (whatfn); \
  PDCI_NULLPARAM_CHECK_RET_VOID (whatfn, p1); \
} while (0)

#define PDCI_DISC_2P_CHECKS_RET_VOID(whatfn, p1, p2) \
do { \
  PDCI_DISC_INIT_CHECKS_RET_VOID (whatfn); \
  PDCI_NULLPARAM_CHECK_RET_VOID (whatfn, p1); \
  PDCI_NULLPARAM_CHECK_RET_VOID (whatfn, p2); \
} while (0)

#define PDCI_DISC_3P_CHECKS_RET_VOID(whatfn, p1, p2, p3) \
do { \
  PDCI_DISC_INIT_CHECKS_RET_VOID (whatfn); \
  PDCI_NULLPARAM_CHECK_RET_VOID (whatfn, p1); \
  PDCI_NULLPARAM_CHECK_RET_VOID (whatfn, p2); \
  PDCI_NULLPARAM_CHECK_RET_VOID (whatfn, p3); \
} while (0)

#define PDCI_DISC_4P_CHECKS_RET_VOID(whatfn, p1, p2, p3, p4) \
do { \
  PDCI_DISC_INIT_CHECKS_RET_VOID (whatfn); \
  PDCI_NULLPARAM_CHECK_RET_VOID (whatfn, p1); \
  PDCI_NULLPARAM_CHECK_RET_VOID (whatfn, p2); \
  PDCI_NULLPARAM_CHECK_RET_VOID (whatfn, p3); \
  PDCI_NULLPARAM_CHECK_RET_VOID (whatfn, p4); \
} while (0)

#define PDCI_DISC_0P_CHECKS_RET_SSIZE(whatfn) \
     PDCI_DISC_INIT_CHECKS_RET_SSIZE(whatfn)

#define PDCI_DISC_1P_CHECKS_RET_SSIZE(whatfn, p1) \
do { \
  PDCI_DISC_INIT_CHECKS_RET_SSIZE (whatfn); \
  PDCI_NULLPARAM_CHECK_RET_SSIZE (whatfn, p1); \
} while (0) \

#define PDCI_DISC_2P_CHECKS_RET_SSIZE(whatfn, p1, p2) \
do { \
  PDCI_DISC_INIT_CHECKS_RET_SSIZE (whatfn); \
  PDCI_NULLPARAM_CHECK_RET_SSIZE (whatfn, p1); \
  PDCI_NULLPARAM_CHECK_RET_SSIZE (whatfn, p2); \
} while (0)

#define PDCI_DISC_3P_CHECKS_RET_SSIZE(whatfn, p1, p2, p3) \
do { \
  PDCI_DISC_INIT_CHECKS_RET_SSIZE (whatfn); \
  PDCI_NULLPARAM_CHECK_RET_SSIZE (whatfn, p1); \
  PDCI_NULLPARAM_CHECK_RET_SSIZE (whatfn, p2); \
  PDCI_NULLPARAM_CHECK_RET_SSIZE (whatfn, p3); \
} while (0)

#define PDCI_DISC_4P_CHECKS_RET_SSIZE(whatfn, p1, p2, p3, p4) \
do { \
  PDCI_DISC_INIT_CHECKS_RET_SSIZE (whatfn); \
  PDCI_NULLPARAM_CHECK_RET_SSIZE (whatfn, p1); \
  PDCI_NULLPARAM_CHECK_RET_SSIZE (whatfn, p2); \
  PDCI_NULLPARAM_CHECK_RET_SSIZE (whatfn, p3); \
  PDCI_NULLPARAM_CHECK_RET_SSIZE (whatfn, p4); \
} while (0)

#define PDCI_IODISC_0P_CHECKS(whatfn) \
     PDCI_IODISC_INIT_CHECKS(whatfn)

#define PDCI_IODISC_1P_CHECKS(whatfn, p1) \
do { \
  PDCI_IODISC_INIT_CHECKS (whatfn); \
  PDCI_NULLPARAM_CHECK (whatfn, p1); \
} while (0)

#define PDCI_IODISC_2P_CHECKS(whatfn, p1, p2) \
do { \
  PDCI_IODISC_INIT_CHECKS (whatfn); \
  PDCI_NULLPARAM_CHECK (whatfn, p1); \
  PDCI_NULLPARAM_CHECK (whatfn, p2); \
} while (0)

#define PDCI_IODISC_3P_CHECKS(whatfn, p1, p2, p3) \
do { \
  PDCI_IODISC_INIT_CHECKS (whatfn); \
  PDCI_NULLPARAM_CHECK (whatfn, p1); \
  PDCI_NULLPARAM_CHECK (whatfn, p2); \
  PDCI_NULLPARAM_CHECK (whatfn, p3); \
} while (0)

#define PDCI_IODISC_4P_CHECKS(whatfn, p1, p2, p3, p4) \
do { \
  PDCI_IODISC_INIT_CHECKS (whatfn); \
  PDCI_NULLPARAM_CHECK (whatfn, p1); \
  PDCI_NULLPARAM_CHECK (whatfn, p2); \
  PDCI_NULLPARAM_CHECK (whatfn, p3); \
  PDCI_NULLPARAM_CHECK (whatfn, p4); \
} while (0)

#define PDCI_IODISC_0P_CHECKS_RET_SSIZE(whatfn) \
     PDCI_IODISC_INIT_CHECKS_RET_SSIZE(whatfn)

#define PDCI_IODISC_1P_CHECKS_RET_SSIZE(whatfn, p1) \
do { \
  PDCI_IODISC_INIT_CHECKS_RET_SSIZE (whatfn); \
  PDCI_NULLPARAM_CHECK_RET_SSIZE (whatfn, p1); \
} while (0)

#define PDCI_IODISC_2P_CHECKS_RET_SSIZE(whatfn, p1, p2) \
do { \
  PDCI_IODISC_INIT_CHECKS_RET_SSIZE (whatfn); \
  PDCI_NULLPARAM_CHECK_RET_SSIZE (whatfn, p1); \
  PDCI_NULLPARAM_CHECK_RET_SSIZE (whatfn, p2); \
} while (0)

#define PDCI_IODISC_3P_CHECKS_RET_SSIZE(whatfn, p1, p2, p3) \
do { \
  PDCI_IODISC_INIT_CHECKS_RET_SSIZE (whatfn); \
  PDCI_NULLPARAM_CHECK_RET_SSIZE (whatfn, p1); \
  PDCI_NULLPARAM_CHECK_RET_SSIZE (whatfn, p2); \
  PDCI_NULLPARAM_CHECK_RET_SSIZE (whatfn, p3); \
} while (0)

#define PDCI_IODISC_4P_CHECKS_RET_SSIZE(whatfn, p1, p2, p3, p4) \
do { \
  PDCI_IODISC_INIT_CHECKS_RET_SSIZE (whatfn); \
  PDCI_NULLPARAM_CHECK_RET_SSIZE (whatfn, p1); \
  PDCI_NULLPARAM_CHECK_RET_SSIZE (whatfn, p2); \
  PDCI_NULLPARAM_CHECK_RET_SSIZE (whatfn, p3); \
  PDCI_NULLPARAM_CHECK_RET_SSIZE (whatfn, p4); \
} while (0)

#define PDCI_READFN_WIDTH_CHECK(whatfn, elt_descr, width) \
do { \
  if (width <= 0) { \
    if (pads->speclev == 0) { \
      P_WARN2(pads->disc, "UNEXPECTED PARAM VALUE: %s called with %s width <= 0", whatfn, elt_descr); \
    } \
    PDCI_READFN_SET_NULLSPAN_LOC(0); \
    PDCI_READFN_RET_ERRCODE_WARN(whatfn, 0, P_BAD_PARAM); \
  } \
} while (0)

#else
/* NO-DEBUG VERSIONS */

#define PDCI_DISC_INIT_CHECKS(whatfn)                          P_NULL_STMT
#define PDCI_DISC_INIT_CHECKS_RET_0(whatfn)                    P_NULL_STMT
#define PDCI_DISC_INIT_CHECKS_RET_VOID(whatfn)                 P_NULL_STMT
#define PDCI_DISC_INIT_CHECKS_RET_SSIZE(whatfn)                P_NULL_STMT

#define PDCI_IODISC_INIT_CHECKS(whatfn)                        P_NULL_STMT
#define PDCI_IODISC_INIT_CHECKS_RET_0(whatfn)                  P_NULL_STMT
#define PDCI_IODISC_INIT_CHECKS_RET_VOID(whatfn)               P_NULL_STMT
#define PDCI_IODISC_INIT_CHECKS_RET_SSIZE(whatfn)              P_NULL_STMT

#define PDCI_NULLPARAM_CHECK(whatfn, param)                    P_NULL_STMT
#define PDCI_NULLPARAM_CHECK_RET_0(whatfn, param)              P_NULL_STMT
#define PDCI_NULLPARAM_CHECK_RET_VOID(whatfn, param)           P_NULL_STMT
#define PDCI_NULLPARAM_CHECK_RET_SSIZE(whatfn, param)          P_NULL_STMT

#define PDCI_DISC_0P_CHECKS(whatfn)                            P_NULL_STMT
#define PDCI_DISC_1P_CHECKS(whatfn, p1)                        P_NULL_STMT
#define PDCI_DISC_2P_CHECKS(whatfn, p1, p2)                    P_NULL_STMT
#define PDCI_DISC_3P_CHECKS(whatfn, p1, p2, p3)                P_NULL_STMT
#define PDCI_DISC_4P_CHECKS(whatfn, p1, p2, p3, p4)            P_NULL_STMT

#define PDCI_DISC_0P_CHECKS_RET_0(whatfn)                   P_NULL_STMT
#define PDCI_DISC_1P_CHECKS_RET_0(whatfn, p1)               P_NULL_STMT
#define PDCI_DISC_2P_CHECKS_RET_0(whatfn, p1, p2)           P_NULL_STMT
#define PDCI_DISC_3P_CHECKS_RET_0(whatfn, p1, p2, p3)       P_NULL_STMT
#define PDCI_DISC_4P_CHECKS_RET_0(whatfn, p1, p2, p3, p4)   P_NULL_STMT

#define PDCI_DISC_0P_CHECKS_RET_VOID(whatfn)                   P_NULL_STMT
#define PDCI_DISC_1P_CHECKS_RET_VOID(whatfn, p1)               P_NULL_STMT
#define PDCI_DISC_2P_CHECKS_RET_VOID(whatfn, p1, p2)           P_NULL_STMT
#define PDCI_DISC_3P_CHECKS_RET_VOID(whatfn, p1, p2, p3)       P_NULL_STMT
#define PDCI_DISC_4P_CHECKS_RET_VOID(whatfn, p1, p2, p3, p4)   P_NULL_STMT

#define PDCI_DISC_0P_CHECKS_RET_SSIZE(whatfn)                  P_NULL_STMT
#define PDCI_DISC_1P_CHECKS_RET_SSIZE(whatfn, p1)              P_NULL_STMT
#define PDCI_DISC_2P_CHECKS_RET_SSIZE(whatfn, p1, p2)          P_NULL_STMT
#define PDCI_DISC_3P_CHECKS_RET_SSIZE(whatfn, p1, p2, p3)      P_NULL_STMT
#define PDCI_DISC_4P_CHECKS_RET_SSIZE(whatfn, p1, p2, p3, p4)  P_NULL_STMT

#define PDCI_IODISC_0P_CHECKS(whatfn)                          P_NULL_STMT
#define PDCI_IODISC_1P_CHECKS(whatfn, p1)                      P_NULL_STMT
#define PDCI_IODISC_2P_CHECKS(whatfn, p1, p2)                  P_NULL_STMT
#define PDCI_IODISC_3P_CHECKS(whatfn, p1, p2, p3)              P_NULL_STMT
#define PDCI_IODISC_4P_CHECKS(whatfn, p1, p2, p3, p4)          P_NULL_STMT

#define PDCI_IODISC_0P_CHECKS_RET_SSIZE(whatfn)                          P_NULL_STMT
#define PDCI_IODISC_1P_CHECKS_RET_SSIZE(whatfn, p1)                      P_NULL_STMT
#define PDCI_IODISC_2P_CHECKS_RET_SSIZE(whatfn, p1, p2)                  P_NULL_STMT
#define PDCI_IODISC_3P_CHECKS_RET_SSIZE(whatfn, p1, p2, p3)              P_NULL_STMT
#define PDCI_IODISC_4P_CHECKS_RET_SSIZE(whatfn, p1, p2, p3, p4)          P_NULL_STMT

#define PDCI_READFN_WIDTH_CHECK(whatfn, elt_descr, width)                P_NULL_STMT

#endif /* !NDEBUG */

#define PDCI_GET_INV_VALFN(pads,type_name) \
  (pads->disc->inv_valfn_map ? P_get_inv_valfn(pads, pads->disc->inv_valfn_map, type_name) : 0)

#define PDCI_fill_mask(mask, m, sz) do { \
  if ((m) == 0) { \
    memset((void*)(mask), 0, (sz)); \
  } else { \
    int i; \
    for (i = 0; i < (sz)/sizeof(Pbase_m); i++) { \
      ((Pbase_m*)(mask))[i] = (m); \
    } \
  } \
} while (0)

#endif /* FOR_CKIT */

/* ================================================================================ */
/* INTERNAL TYPE DEFINITIONS */

/* PDCI_stkElt_t: A stack elt has a cursor position cur, which is a
 * pointer to a Pio_elt plus the number of byte remaining.  We also
 * record the spec flag passed to P_io_checkpoint, to enable proper
 * de-bumping of pads->speclev.
 */

/* type PDCI_stkElt_t: */
struct PDCI_stkElt_s {
  Pio_elt_t  *elt;
  size_t        remain;  /* bytes remaining in current IO elt; determines cursor position */
  int           spec;    /* the spec flag passed to checkpoint */
};

/* ================================================================================ */
/* INTERNAL VERSIONS OF EXTERNAL IO FUNCTIONS */

/* These do not have the same API as external versions; they take a whatfn arg */

#if P_CONFIG_WRITE_FUNCTIONS > 0
Pbyte*    PDCI_io_write_start  (P_t *pads, Sfio_t *io, size_t *buf_len, int *set_buf, const char *whatfn);
ssize_t      PDCI_io_write_commit (P_t *pads, Sfio_t *io, Pbyte *buf, int set_buf, size_t num_bytes, const char *whatfn);
void         PDCI_io_write_abort  (P_t *pads, Sfio_t *io, Pbyte *buf, int set_buf, const char *whatfn);

ssize_t      PDCI_io_rec_write2io        (P_t *pads, Sfio_t *io, Pbyte *buf, size_t rec_data_len, const char *whatfn);
ssize_t      PDCI_io_rec_open_write2buf  (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, const char *whatfn);
ssize_t      PDCI_io_rec_close_write2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full,
					  Pbyte *rec_start, size_t num_bytes, const char *whatfn);

ssize_t      PDCI_io_rblk_write2io       (P_t *pads, Sfio_t *io, Pbyte *buf, size_t blk_data_len, Puint32 num_recs,
					  const char *whatfn);
ssize_t      PDCI_io_rblk_open_write2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, const char *whatfn);
ssize_t      PDCI_io_rblk_close_write2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full,
					  Pbyte *blk_start, size_t num_bytes, Puint32 num_recs, const char *whatfn);
#endif

/* ================================================================================ */ 
/* HELPER MACRO TO DECLARE FAMILY OF FUNCTIONS */
/* N.B. First you must declare PDCI_FIRST_ARGS and PDCI_LAST_ARGS.  Follow macro with a semi */

#define PDCI_DECL_FAMILY(ret_type, fn_prefix, typ, fn_suffix, lastnm) \
ret_type fn_prefix ## typ ## 8 ## fn_suffix (PDCI_FIRST_ARGS, P_ ## typ ## 8 *lastnm PDCI_LAST_ARGS); \
ret_type fn_prefix ## typ ## 16 ## fn_suffix(PDCI_FIRST_ARGS, P_ ## typ ## 16 *lastnm PDCI_LAST_ARGS); \
ret_type fn_prefix ## typ ## 32 ## fn_suffix(PDCI_FIRST_ARGS, P_ ## typ ## 32 *lastnm PDCI_LAST_ARGS); \
ret_type fn_prefix ## typ ## 64 ## fn_suffix(PDCI_FIRST_ARGS, P_ ## typ  ## 64 *lastnm PDCI_LAST_ARGS); \
ret_type fn_prefix ## u ## typ ## 8 ## fn_suffix (PDCI_FIRST_ARGS, P_u ## typ ## 8 *lastnm PDCI_LAST_ARGS); \
ret_type fn_prefix ## u ## typ ## 16 ## fn_suffix(PDCI_FIRST_ARGS, P_u ## typ ## 16 *lastnm PDCI_LAST_ARGS); \
ret_type fn_prefix ## u ## typ ## 32 ## fn_suffix(PDCI_FIRST_ARGS, P_u ## typ ## 32 *lastnm PDCI_LAST_ARGS); \
ret_type fn_prefix ## u ## typ ## 64 ## fn_suffix(PDCI_FIRST_ARGS, P_u ## typ ## 64 *lastnm PDCI_LAST_ARGS) \

#define PDCI_DECL_FAMILY_LASTCONST(ret_type, fn_prefix, typ, fn_suffix, lastnm) \
ret_type fn_prefix ## typ ## 8 ## fn_suffix (PDCI_FIRST_ARGS, const P_ ## typ ## 8 *lastnm PDCI_LAST_ARGS); \
ret_type fn_prefix ## typ ## 16 ## fn_suffix(PDCI_FIRST_ARGS, const P_ ## typ ## 16 *lastnm PDCI_LAST_ARGS); \
ret_type fn_prefix ## typ ## 32 ## fn_suffix(PDCI_FIRST_ARGS, const P_ ## typ ## 32 *lastnm PDCI_LAST_ARGS); \
ret_type fn_prefix ## typ ## 64 ## fn_suffix(PDCI_FIRST_ARGS, const P_ ## typ  ## 64 *lastnm PDCI_LAST_ARGS); \
ret_type fn_prefix ## u ## typ ## 8 ## fn_suffix (PDCI_FIRST_ARGS, const P_u ## typ ## 8 *lastnm PDCI_LAST_ARGS); \
ret_type fn_prefix ## u ## typ ## 16 ## fn_suffix(PDCI_FIRST_ARGS, const P_u ## typ ## 16 *lastnm PDCI_LAST_ARGS); \
ret_type fn_prefix ## u ## typ ## 32 ## fn_suffix(PDCI_FIRST_ARGS, const P_u ## typ ## 32 *lastnm PDCI_LAST_ARGS); \
ret_type fn_prefix ## u ## typ ## 64 ## fn_suffix(PDCI_FIRST_ARGS, const P_u ## typ ## 64 *lastnm PDCI_LAST_ARGS) \

/* ================================================================================ */ 
/* INTERNAL VERSIONS OF ACCUM REPORTING FUNCTIONS */

/* These functions take an argument, outstr, for 
 * the output target, and do not check
 * the pads, prefix, or accumulator arguments for NULL values.
 */

#undef PDCI_FIRST_ARGS
#define PDCI_FIRST_ARGS P_t *pads, Sfio_t *outstr, const char *prefix, const char *what, int nst

/* we always need these 4 functions */
Perror_t Pint32_acc_report2io  (PDCI_FIRST_ARGS, Pint32_acc *a);
Perror_t Puint32_acc_report2io (PDCI_FIRST_ARGS, Puint32_acc *a);
Perror_t Pint32_acc_map_report2io(PDCI_FIRST_ARGS, Pint32_map_fn  fn, Pint32_acc *a);
Perror_t P_nerr_acc_report2io(P_t *pads, Sfio_t *outstr, const char *prefix, const char *what, int nst,
				   Puint32_acc *a);

#if P_CONFIG_ACCUM_FUNCTIONS > 0
Perror_t Pint8_acc_report2io   (PDCI_FIRST_ARGS, Pint8_acc *a);
Perror_t Pint16_acc_report2io  (PDCI_FIRST_ARGS, Pint16_acc *a);
Perror_t Pint64_acc_report2io  (PDCI_FIRST_ARGS, Pint64_acc *a);
Perror_t Puint8_acc_report2io  (PDCI_FIRST_ARGS, Puint8_acc *a);
Perror_t Puint16_acc_report2io (PDCI_FIRST_ARGS, Puint16_acc *a);
Perror_t Puint64_acc_report2io (PDCI_FIRST_ARGS, Puint64_acc *a);

Perror_t Pstring_acc_report2io (PDCI_FIRST_ARGS, Pstring_acc *a);
Perror_t Pchar_acc_report2io   (PDCI_FIRST_ARGS, Pchar_acc *a);

Perror_t Pfpoint8_acc_report2io   (PDCI_FIRST_ARGS, Pfpoint8_acc *a);
Perror_t Pfpoint16_acc_report2io  (PDCI_FIRST_ARGS, Pfpoint16_acc *a);
Perror_t Pfpoint32_acc_report2io  (PDCI_FIRST_ARGS, Pfpoint32_acc *a);
Perror_t Pfpoint64_acc_report2io  (PDCI_FIRST_ARGS, Pfpoint64_acc *a);
Perror_t Pufpoint8_acc_report2io  (PDCI_FIRST_ARGS, Pufpoint8_acc *a);
Perror_t Pufpoint16_acc_report2io (PDCI_FIRST_ARGS, Pufpoint16_acc *a);
Perror_t Pufpoint32_acc_report2io (PDCI_FIRST_ARGS, Pufpoint32_acc *a);
Perror_t Pufpoint64_acc_report2io (PDCI_FIRST_ARGS, Pufpoint64_acc *a);

#endif /* P_CONFIG_ACCUM_FUNCTIONS */

/* ********************************************************************************
 * Remainder of this file contains function decls for functions
 * purely internal to the library impl.  Note the use of the PDCI prefix
 * for these functions 
 * ********************************************************************************/

/* ================================================================================ */ 
/* INTERNAL ERROR REPORTING FUNCTIONS */

/*
 * PDCI_report_err: Report a parse error that occurred at location loc.
 *
 * Can also use for other errors that have error codes: loc can be NULL.
 *
 * See description of Perror_f for description of level
 *  
 *   XXX errCode's type should be an enum that describes the kind of error XXX ???
 *
 * The whatfn param is optional (can be NULL). If non-null, the report
 * include a prefix of the form "[in <whatfn>]"
 *
 * The <format, ...> args are for a printf-style description that augments
 * the default description based on errCode. 
 *
 * N.B. This call does nothing if either there is no disc error function
 *      or if the disc e_rep is PerrorRep_None
 */

Perror_t PDCI_report_err(P_t *pads, int level, Ploc_t *loc,
			    PerrCode_t errCode, const char *whatfn, const char *format, ... );

/* ================================================================================ */
/* PURELY INTERNAL IO FUNCTIONS */

/* 
 * Note: all of the following act on the IO cursor of the top checkpoint
 *
 * PDCI_io_install_io:    XXX_TODOC
 * 
 * PDCI_io_need_K_bytes:
 *
 *   Used when the calling function knows exactly K byte are required.
 *   need_K_bytes will either set (*b_out)/(*e_out) to cover exactly a K
 *   byte span or will set them to cover the largest span < K bytes that is
 *   possible (due to hitting eor or eof).  Also sets (*bor_out) to status of
 *   begin byte, and (*eor_out)/(*eof_out) to status of the end byte.
 *
 * PDCI_io_need_some_bytes:
 *
 *   Used when the calling function needs to determine the required byte scope
 *   based on the bytes themselves.  Has same out params as need_K_bytes, plus a
 *   (*g_out) [goal out] that is set as discussed below.  If (*e_out) < (*g_out),
 *   then eor was not hit but eof WAS hit before the desired goal.  NB
 *   (*g_out) should only be used for comparison to (*e_out).  A goal
 *   is set by choosing one of the following enum values for the goal param:
 *   PDCI_goal_match, PDCI_goal_scan, PDCI_goal_panic, PDCI_goal_numeric,
 *   PDCI_goal_specific.
 *
 *     PDCI_goal_match:
 *       Need sufficient bytes for a literal or regular expression match.
 *
 *       (a) For record-based disciplines:
 *            The goal is to find eor or to find pads->disc->match_max
 *            characters, if pads->disc->match_max != 0.  If eor or the goal # of
 *            characters is found, then (*g_out) is set to the same character as
 *            (*e_out).  If eof is found prior to reaching the goal, (*g_out) is
 *            set to either (*e_out)+PDCI_GOAL_HUGE [when pads->disc->match_max == 0] or
 *            (*b_out)+pads->disc->match_max [when pads->disc->match_max != 0]...
 *            (*g_out) > (*e_out) either way.
 *
 *       (b) For non-record-based disciplines:
 *            The goal is to find pads->disc->match_max characters, or if
 *            pads->disc->match_max == 0, then to try find the default built-in
 *            value for match_max, but to allow the actual bytes found to
 *            stretch to a larger value (use a soft rather than hard limit).  If
 *            the goal # of characters is found, (*g_out) is set to the same
 *            character as (*e_out).  Otherwise, (*g_out) is set to (*b_out)
 *            + hard/soft goal... (*g_out) > (*e_out).
 *
 *        For both (a) and (b), note that if !eor and !eof, it must be the case
 *        that hard/soft match_max forced a limit on the scope, otherwise it did not.
 *        (*g_out) - (*e_out) gives the missing # of characters, where if 
 *        the difference is PDCI_GOAL_HUGE then this is due to a record-based discipline
 *        combined with a 0 pads->disc->match_max.
 *
 *     PDCI_goal_scan:
 *       Need sufficient bytes for a normal scan to find a terminating
 *         literal or regular expression.  Same description as PDCI_goal_match,
 *         except using pads->disc->scan_max / the built-in scan_max default.
 *
 *     PDCI_goal_panic:
 *       Need sufficient bytes for a panic scan to find a 'synchronizing'
 *         literal or regular expression.  Same description as PDCI_goal_match,
 *         except using pads->disc->panic_max / the built-in panic_max default.
 *
 *     PDCI_goal_numeric:
 *       Need sufficient bytes for a character-based number, e.g., need to parse
 *       an integer, float32 or float64, etc.  In this case end-of-record is used as
 *       the goal for record-based IO disciplines, otherwise a built-in soft goal
 *       for numerics.
 *
 *     PDCI_goal_specific:
 *       Need to set a specific goal using the specific param.  When this goal
 *       is used, the specific param must have a value > 0, and it is used as
 *       a hard limit.  P_ERR is returned immediately without setting any out
 *       params if this goal is used with specific == 0.
 *
 * PDCI_io_forward:
 *
 *   Move IO cursor forward num_bytes bytes, which should be <=
 *   (end-begin), where [begin,end] are from the last call to needbytes
 *   or morebytes.  This call can obliviate that [begin,end] data
 *   region so IO_forward should only be used after all relevant data
 *   bytes have been observed.  Causes fatal error if K would move
 *   beyond an EOR/EOF marker or beyond the last in-memory data byte. 
 */

#define PDCI_GOAL_HUGE            999999

typedef enum P_goal_e {
  PDCI_goal_match,
  PDCI_goal_scan,
  PDCI_goal_panic,
  PDCI_goal_numeric,
  PDCI_goal_specific
} PDCI_goal_t;

Perror_t  PDCI_io_install_io(P_t *pads, Sfio_t *io);

Perror_t  PDCI_io_need_K_bytes (P_t *pads, size_t K,
				   Pbyte **b_out, Pbyte **e_out,
				   int *bor_out, int *eor_out, int *eof_out);

Perror_t  PDCI_io_need_some_bytes (P_t *pads, PDCI_goal_t goal, size_t specific,
				      Pbyte **b_out, Pbyte **e_out, Pbyte **g_out,
				      int *bor_out, int *eor_out, int *eof_out);

Perror_t  PDCI_io_forward   (P_t *pads, size_t num_bytes);


/*
 * The following is used to obtain one record's worth of bytes at a time.
 * The usage look something like this:
 *   int eor = 0, eof = 0, skip_rec = 0;
 *   Pbyte *begin = 0, *end = 0;
 *   size_t skipped_bytes = 0;
 *   while (!eof && P_OK == PDCI_io_need_rec_bytes(pads, skip_rec, &begin, &end, &eor, &eof, &skipped_bytes)) {
 *      skip_rec = 1;
 *          -- after first call, subsequent calls advance the IO cursor past the current record
 *          -- skipped_bytes is set to # of bytes skipped
 *       ...use bytes which go from begin to end (rec_len = end-begin)
 *   }
 */
Perror_t  PDCI_io_need_rec_bytes(P_t *pads, int skip_rec,
				    Pbyte **b_out, Pbyte **e_out,
				    int *eor_out, int *eof_out, size_t *skipped_bytes_out);
/*
 * Other IO routines:
 *    PDCI_io_getElt: if the specified elt is currently in an in-memory buffer,
 *                    sets (*elt_out) to point to elt and returns P_OK,
 *                    otherwise returns P_ERR.
 */

Perror_t PDCI_io_getElt(P_t *pads, size_t num, Pio_elt_t **elt_out);

/* ================================================================================ */
/* INTERNAL CONVERSION ROUTINES */

/* Various tables */
extern int PDCI_ascii_digit[];
extern int PDCI_ascii_is_digit[];
extern int PDCI_ascii_is_space[];
#define PDCI_is_a_digit(c) PDCI_ascii_is_digit[c]
#define PDCI_is_a_space(c) PDCI_ascii_is_space[c]

extern int PDCI_ebcdic_digit[];
extern int PDCI_ebcdic_is_digit[];
extern int PDCI_ebcdic_is_space[];
#define PDCI_is_e_digit(c) PDCI_ebcdic_is_digit[c]
#define PDCI_is_e_space(c) PDCI_ebcdic_is_space[c]

extern Pbyte P_ea_tab[];
extern Pbyte P_ae_tab[];
extern Pbyte P_mod_ea_tab[];
extern Pbyte P_mod_ae_tab[];

extern int PDCI_bcd_hilo_digits[];
extern int PDCI_bcd_hi_digit[];
extern Puint64 PDCI_10toThe[];

Pint8   PDCI_a2int8  (P_t *pads, const Pbyte *bytes, Pbyte **ptr_out);
Pint16  PDCI_a2int16 (P_t *pads, const Pbyte *bytes, Pbyte **ptr_out);
Pint32  PDCI_a2int32 (P_t *pads, const Pbyte *bytes, Pbyte **ptr_out);
Pint64  PDCI_a2int64 (P_t *pads, const Pbyte *bytes, Pbyte **ptr_out);

Pint8   PDCI_a2int8_norange  (P_t *pads, const Pbyte *bytes, Pbyte **ptr_out);
Pint16  PDCI_a2int16_norange (P_t *pads, const Pbyte *bytes, Pbyte **ptr_out);
Pint32  PDCI_a2int32_norange (P_t *pads, const Pbyte *bytes, Pbyte **ptr_out);
Pint64  PDCI_a2int64_norange (P_t *pads, const Pbyte *bytes, Pbyte **ptr_out);

ssize_t PDCI_int8_2a_buf (P_t *pads, Pbyte *outbuf, size_t outbuf_len, int *outbuf_full, Pint8  i);
ssize_t PDCI_int16_2a_buf(P_t *pads, Pbyte *outbuf, size_t outbuf_len, int *outbuf_full, Pint16 i);
ssize_t PDCI_int32_2a_buf(P_t *pads, Pbyte *outbuf, size_t outbuf_len, int *outbuf_full, Pint32 i);
ssize_t PDCI_int64_2a_buf(P_t *pads, Pbyte *outbuf, size_t outbuf_len, int *outbuf_full, Pint64 i);

ssize_t PDCI_int8_2a_io (P_t *pads, Sfio_t *io, Pint8  i);
ssize_t PDCI_int16_2a_io(P_t *pads, Sfio_t *io, Pint16 i);
ssize_t PDCI_int32_2a_io(P_t *pads, Sfio_t *io, Pint32 i);
ssize_t PDCI_int64_2a_io(P_t *pads, Sfio_t *io, Pint64 i);

ssize_t PDCI_int8_2a_FW_buf (P_t *pads, Pbyte *outbuf, size_t outbuf_len, int *outbuf_full, Pint8  i, size_t width);
ssize_t PDCI_int16_2a_FW_buf(P_t *pads, Pbyte *outbuf, size_t outbuf_len, int *outbuf_full, Pint16 i, size_t width);
ssize_t PDCI_int32_2a_FW_buf(P_t *pads, Pbyte *outbuf, size_t outbuf_len, int *outbuf_full, Pint32 i, size_t width);
ssize_t PDCI_int64_2a_FW_buf(P_t *pads, Pbyte *outbuf, size_t outbuf_len, int *outbuf_full, Pint64 i, size_t width);

ssize_t PDCI_int8_2a_FW_io (P_t *pads, Sfio_t *io, Pint8  i, size_t width);
ssize_t PDCI_int16_2a_FW_io(P_t *pads, Sfio_t *io, Pint16 i, size_t width);
ssize_t PDCI_int32_2a_FW_io(P_t *pads, Sfio_t *io, Pint32 i, size_t width);
ssize_t PDCI_int64_2a_FW_io(P_t *pads, Sfio_t *io, Pint64 i, size_t width);

Puint8  PDCI_a2uint8 (P_t *pads, const Pbyte *bytes, Pbyte **ptr_out);
Puint16 PDCI_a2uint16(P_t *pads, const Pbyte *bytes, Pbyte **ptr_out);
Puint32 PDCI_a2uint32(P_t *pads, const Pbyte *bytes, Pbyte **ptr_out);
Puint64 PDCI_a2uint64(P_t *pads, const Pbyte *bytes, Pbyte **ptr_out);

Puint8  PDCI_a2uint8_norange (P_t *pads, const Pbyte *bytes, Pbyte **ptr_out);
Puint16 PDCI_a2uint16_norange(P_t *pads, const Pbyte *bytes, Pbyte **ptr_out);
Puint32 PDCI_a2uint32_norange(P_t *pads, const Pbyte *bytes, Pbyte **ptr_out);
Puint64 PDCI_a2uint64_norange(P_t *pads, const Pbyte *bytes, Pbyte **ptr_out);

ssize_t PDCI_uint8_2a_buf (P_t *pads, Pbyte *outbuf, size_t outbuf_len, int *outbuf_full, Puint8  u);
ssize_t PDCI_uint16_2a_buf(P_t *pads, Pbyte *outbuf, size_t outbuf_len, int *outbuf_full, Puint16 u);
ssize_t PDCI_uint32_2a_buf(P_t *pads, Pbyte *outbuf, size_t outbuf_len, int *outbuf_full, Puint32 u);
ssize_t PDCI_uint64_2a_buf(P_t *pads, Pbyte *outbuf, size_t outbuf_len, int *outbuf_full, Puint64 u);

ssize_t PDCI_uint8_2a_io (P_t *pads, Sfio_t *io, Puint8  u);
ssize_t PDCI_uint16_2a_io(P_t *pads, Sfio_t *io, Puint16 u);
ssize_t PDCI_uint32_2a_io(P_t *pads, Sfio_t *io, Puint32 u);
ssize_t PDCI_uint64_2a_io(P_t *pads, Sfio_t *io, Puint64 u);

ssize_t PDCI_uint8_2a_FW_buf (P_t *pads, Pbyte *outbuf, size_t outbuf_len, int *outbuf_full, Puint8  u, size_t width);
ssize_t PDCI_uint16_2a_FW_buf(P_t *pads, Pbyte *outbuf, size_t outbuf_len, int *outbuf_full, Puint16 u, size_t width);
ssize_t PDCI_uint32_2a_FW_buf(P_t *pads, Pbyte *outbuf, size_t outbuf_len, int *outbuf_full, Puint32 u, size_t width);
ssize_t PDCI_uint64_2a_FW_buf(P_t *pads, Pbyte *outbuf, size_t outbuf_len, int *outbuf_full, Puint64 u, size_t width);

ssize_t PDCI_uint8_2a_FW_io (P_t *pads, Sfio_t *io, Puint8  u, size_t width);
ssize_t PDCI_uint16_2a_FW_io(P_t *pads, Sfio_t *io, Puint16 u, size_t width);
ssize_t PDCI_uint32_2a_FW_io(P_t *pads, Sfio_t *io, Puint32 u, size_t width);
ssize_t PDCI_uint64_2a_FW_io(P_t *pads, Sfio_t *io, Puint64 u, size_t width);

Pint8   PDCI_e2int8  (P_t *pads, const Pbyte *bytes, Pbyte **ptr_out);
Pint16  PDCI_e2int16 (P_t *pads, const Pbyte *bytes, Pbyte **ptr_out);
Pint32  PDCI_e2int32 (P_t *pads, const Pbyte *bytes, Pbyte **ptr_out);
Pint64  PDCI_e2int64 (P_t *pads, const Pbyte *bytes, Pbyte **ptr_out);

Pint8   PDCI_e2int8_norange  (P_t *pads, const Pbyte *bytes, Pbyte **ptr_out);
Pint16  PDCI_e2int16_norange (P_t *pads, const Pbyte *bytes, Pbyte **ptr_out);
Pint32  PDCI_e2int32_norange (P_t *pads, const Pbyte *bytes, Pbyte **ptr_out);
Pint64  PDCI_e2int64_norange (P_t *pads, const Pbyte *bytes, Pbyte **ptr_out);

ssize_t PDCI_int8_2e_buf (P_t *pads, Pbyte *outbuf, size_t outbuf_len, int *outbuf_full, Pint8  i);
ssize_t PDCI_int16_2e_buf(P_t *pads, Pbyte *outbuf, size_t outbuf_len, int *outbuf_full, Pint16 i);
ssize_t PDCI_int32_2e_buf(P_t *pads, Pbyte *outbuf, size_t outbuf_len, int *outbuf_full, Pint32 i);
ssize_t PDCI_int64_2e_buf(P_t *pads, Pbyte *outbuf, size_t outbuf_len, int *outbuf_full, Pint64 i);

ssize_t PDCI_int8_2e_io (P_t *pads, Sfio_t *io, Pint8  i);
ssize_t PDCI_int16_2e_io(P_t *pads, Sfio_t *io, Pint16 i);
ssize_t PDCI_int32_2e_io(P_t *pads, Sfio_t *io, Pint32 i);
ssize_t PDCI_int64_2e_io(P_t *pads, Sfio_t *io, Pint64 i);

ssize_t PDCI_int8_2e_FW_buf (P_t *pads, Pbyte *outbuf, size_t outbuf_len, int *outbuf_full, Pint8  i, size_t width);
ssize_t PDCI_int16_2e_FW_buf(P_t *pads, Pbyte *outbuf, size_t outbuf_len, int *outbuf_full, Pint16 i, size_t width);
ssize_t PDCI_int32_2e_FW_buf(P_t *pads, Pbyte *outbuf, size_t outbuf_len, int *outbuf_full, Pint32 i, size_t width);
ssize_t PDCI_int64_2e_FW_buf(P_t *pads, Pbyte *outbuf, size_t outbuf_len, int *outbuf_full, Pint64 i, size_t width);

ssize_t PDCI_int8_2e_FW_io (P_t *pads, Sfio_t *io, Pint8  i, size_t width);
ssize_t PDCI_int16_2e_FW_io(P_t *pads, Sfio_t *io, Pint16 i, size_t width);
ssize_t PDCI_int32_2e_FW_io(P_t *pads, Sfio_t *io, Pint32 i, size_t width);
ssize_t PDCI_int64_2e_FW_io(P_t *pads, Sfio_t *io, Pint64 i, size_t width);

Puint8  PDCI_e2uint8 (P_t *pads, const Pbyte *bytes, Pbyte **ptr_out);
Puint16 PDCI_e2uint16(P_t *pads, const Pbyte *bytes, Pbyte **ptr_out);
Puint32 PDCI_e2uint32(P_t *pads, const Pbyte *bytes, Pbyte **ptr_out);
Puint64 PDCI_e2uint64(P_t *pads, const Pbyte *bytes, Pbyte **ptr_out);

Puint8  PDCI_e2uint8_norange (P_t *pads, const Pbyte *bytes, Pbyte **ptr_out);
Puint16 PDCI_e2uint16_norange(P_t *pads, const Pbyte *bytes, Pbyte **ptr_out);
Puint32 PDCI_e2uint32_norange(P_t *pads, const Pbyte *bytes, Pbyte **ptr_out);
Puint64 PDCI_e2uint64_norange(P_t *pads, const Pbyte *bytes, Pbyte **ptr_out);

ssize_t PDCI_uint8_2e_buf (P_t *pads, Pbyte *outbuf, size_t outbuf_len, int *outbuf_full, Puint8  u);
ssize_t PDCI_uint16_2e_buf(P_t *pads, Pbyte *outbuf, size_t outbuf_len, int *outbuf_full, Puint16 u);
ssize_t PDCI_uint32_2e_buf(P_t *pads, Pbyte *outbuf, size_t outbuf_len, int *outbuf_full, Puint32 u);
ssize_t PDCI_uint64_2e_buf(P_t *pads, Pbyte *outbuf, size_t outbuf_len, int *outbuf_full, Puint64 u);

ssize_t PDCI_uint8_2e_io (P_t *pads, Sfio_t *io, Puint8  u);
ssize_t PDCI_uint16_2e_io(P_t *pads, Sfio_t *io, Puint16 u);
ssize_t PDCI_uint32_2e_io(P_t *pads, Sfio_t *io, Puint32 u);
ssize_t PDCI_uint64_2e_io(P_t *pads, Sfio_t *io, Puint64 u);

ssize_t PDCI_uint8_2e_FW_buf (P_t *pads, Pbyte *outbuf, size_t outbuf_len, int *outbuf_full, Puint8  u, size_t width);
ssize_t PDCI_uint16_2e_FW_buf(P_t *pads, Pbyte *outbuf, size_t outbuf_len, int *outbuf_full, Puint16 u, size_t width);
ssize_t PDCI_uint32_2e_FW_buf(P_t *pads, Pbyte *outbuf, size_t outbuf_len, int *outbuf_full, Puint32 u, size_t width);
ssize_t PDCI_uint64_2e_FW_buf(P_t *pads, Pbyte *outbuf, size_t outbuf_len, int *outbuf_full, Puint64 u, size_t width);

ssize_t PDCI_uint8_2e_FW_io (P_t *pads, Sfio_t *io, Puint8  u, size_t width);
ssize_t PDCI_uint16_2e_FW_io(P_t *pads, Sfio_t *io, Puint16 u, size_t width);
ssize_t PDCI_uint32_2e_FW_io(P_t *pads, Sfio_t *io, Puint32 u, size_t width);
ssize_t PDCI_uint64_2e_FW_io(P_t *pads, Sfio_t *io, Puint64 u, size_t width);

Pint8   PDCI_ebc2int8 (P_t *pads, const Pbyte *bytes, Puint32 num_digits, Pbyte **ptr_out);
Pint16  PDCI_ebc2int16(P_t *pads, const Pbyte *bytes, Puint32 num_digits, Pbyte **ptr_out);
Pint32  PDCI_ebc2int32(P_t *pads, const Pbyte *bytes, Puint32 num_digits, Pbyte **ptr_out);
Pint64  PDCI_ebc2int64(P_t *pads, const Pbyte *bytes, Puint32 num_digits, Pbyte **ptr_out);

Pint8   PDCI_ebc2int8_norange (P_t *pads, const Pbyte *bytes, Puint32 num_digits, Pbyte **ptr_out);
Pint16  PDCI_ebc2int16_norange(P_t *pads, const Pbyte *bytes, Puint32 num_digits, Pbyte **ptr_out);
Pint32  PDCI_ebc2int32_norange(P_t *pads, const Pbyte *bytes, Puint32 num_digits, Pbyte **ptr_out);
Pint64  PDCI_ebc2int64_norange(P_t *pads, const Pbyte *bytes, Puint32 num_digits, Pbyte **ptr_out);

ssize_t PDCI_int8_2ebc_buf (P_t *pads, Pbyte *outbuf, size_t outbuf_len, int *outbuf_full, Pint8  i, Puint32 num_digits);
ssize_t PDCI_int16_2ebc_buf(P_t *pads, Pbyte *outbuf, size_t outbuf_len, int *outbuf_full, Pint16 i, Puint32 num_digits);
ssize_t PDCI_int32_2ebc_buf(P_t *pads, Pbyte *outbuf, size_t outbuf_len, int *outbuf_full, Pint32 i, Puint32 num_digits);
ssize_t PDCI_int64_2ebc_buf(P_t *pads, Pbyte *outbuf, size_t outbuf_len, int *outbuf_full, Pint64 i, Puint32 num_digits);

ssize_t PDCI_int8_2ebc_io (P_t *pads, Sfio_t *io, Pint8  i, Puint32 num_digits);
ssize_t PDCI_int16_2ebc_io(P_t *pads, Sfio_t *io, Pint16 i, Puint32 num_digits);
ssize_t PDCI_int32_2ebc_io(P_t *pads, Sfio_t *io, Pint32 i, Puint32 num_digits);
ssize_t PDCI_int64_2ebc_io(P_t *pads, Sfio_t *io, Pint64 i, Puint32 num_digits);

Puint8   PDCI_ebc2uint8 (P_t *pads, const Pbyte *bytes, Puint32 num_digits, Pbyte **ptr_out);
Puint16  PDCI_ebc2uint16(P_t *pads, const Pbyte *bytes, Puint32 num_digits, Pbyte **ptr_out);
Puint32  PDCI_ebc2uint32(P_t *pads, const Pbyte *bytes, Puint32 num_digits, Pbyte **ptr_out);
Puint64  PDCI_ebc2uint64(P_t *pads, const Pbyte *bytes, Puint32 num_digits, Pbyte **ptr_out);

Puint8   PDCI_ebc2uint8_norange (P_t *pads, const Pbyte *bytes, Puint32 num_digits, Pbyte **ptr_out);
Puint16  PDCI_ebc2uint16_norange(P_t *pads, const Pbyte *bytes, Puint32 num_digits, Pbyte **ptr_out);
Puint32  PDCI_ebc2uint32_norange(P_t *pads, const Pbyte *bytes, Puint32 num_digits, Pbyte **ptr_out);
Puint64  PDCI_ebc2uint64_norange(P_t *pads, const Pbyte *bytes, Puint32 num_digits, Pbyte **ptr_out);

ssize_t PDCI_uint8_2ebc_buf (P_t *pads, Pbyte *outbuf, size_t outbuf_len, int *outbuf_full, Puint8  u, Puint32 num_digits);
ssize_t PDCI_uint16_2ebc_buf(P_t *pads, Pbyte *outbuf, size_t outbuf_len, int *outbuf_full, Puint16 u, Puint32 num_digits);
ssize_t PDCI_uint32_2ebc_buf(P_t *pads, Pbyte *outbuf, size_t outbuf_len, int *outbuf_full, Puint32 u, Puint32 num_digits);
ssize_t PDCI_uint64_2ebc_buf(P_t *pads, Pbyte *outbuf, size_t outbuf_len, int *outbuf_full, Puint64 u, Puint32 num_digits);

ssize_t PDCI_uint8_2ebc_io (P_t *pads, Sfio_t *io, Puint8  u, Puint32 num_digits);
ssize_t PDCI_uint16_2ebc_io(P_t *pads, Sfio_t *io, Puint16 u, Puint32 num_digits);
ssize_t PDCI_uint32_2ebc_io(P_t *pads, Sfio_t *io, Puint32 u, Puint32 num_digits);
ssize_t PDCI_uint64_2ebc_io(P_t *pads, Sfio_t *io, Puint64 u, Puint32 num_digits);

Pint8   PDCI_bcd2int8 (P_t *pads, const Pbyte *bytes, Puint32 num_digits, Pbyte **ptr_out);
Pint16  PDCI_bcd2int16(P_t *pads, const Pbyte *bytes, Puint32 num_digits, Pbyte **ptr_out);
Pint32  PDCI_bcd2int32(P_t *pads, const Pbyte *bytes, Puint32 num_digits, Pbyte **ptr_out);
Pint64  PDCI_bcd2int64(P_t *pads, const Pbyte *bytes, Puint32 num_digits, Pbyte **ptr_out);

Pint8   PDCI_bcd2int8_norange (P_t *pads, const Pbyte *bytes, Puint32 num_digits, Pbyte **ptr_out);
Pint16  PDCI_bcd2int16_norange(P_t *pads, const Pbyte *bytes, Puint32 num_digits, Pbyte **ptr_out);
Pint32  PDCI_bcd2int32_norange(P_t *pads, const Pbyte *bytes, Puint32 num_digits, Pbyte **ptr_out);
Pint64  PDCI_bcd2int64_norange(P_t *pads, const Pbyte *bytes, Puint32 num_digits, Pbyte **ptr_out);

ssize_t PDCI_int8_2bcd_buf (P_t *pads, Pbyte *outbuf, size_t outbuf_len, int *outbuf_full, Pint8  i, Puint32 num_digits);
ssize_t PDCI_int16_2bcd_buf(P_t *pads, Pbyte *outbuf, size_t outbuf_len, int *outbuf_full, Pint16 i, Puint32 num_digits);
ssize_t PDCI_int32_2bcd_buf(P_t *pads, Pbyte *outbuf, size_t outbuf_len, int *outbuf_full, Pint32 i, Puint32 num_digits);
ssize_t PDCI_int64_2bcd_buf(P_t *pads, Pbyte *outbuf, size_t outbuf_len, int *outbuf_full, Pint64 i, Puint32 num_digits);

ssize_t PDCI_int8_2bcd_io (P_t *pads, Sfio_t *io, Pint8  i, Puint32 num_digits);
ssize_t PDCI_int16_2bcd_io(P_t *pads, Sfio_t *io, Pint16 i, Puint32 num_digits);
ssize_t PDCI_int32_2bcd_io(P_t *pads, Sfio_t *io, Pint32 i, Puint32 num_digits);
ssize_t PDCI_int64_2bcd_io(P_t *pads, Sfio_t *io, Pint64 i, Puint32 num_digits);

Puint8   PDCI_bcd2uint8 (P_t *pads, const Pbyte *bytes, Puint32 num_digits, Pbyte **ptr_out);
Puint16  PDCI_bcd2uint16(P_t *pads, const Pbyte *bytes, Puint32 num_digits, Pbyte **ptr_out);
Puint32  PDCI_bcd2uint32(P_t *pads, const Pbyte *bytes, Puint32 num_digits, Pbyte **ptr_out);
Puint64  PDCI_bcd2uint64(P_t *pads, const Pbyte *bytes, Puint32 num_digits, Pbyte **ptr_out);

Puint8   PDCI_bcd2uint8_norange (P_t *pads, const Pbyte *bytes, Puint32 num_digits, Pbyte **ptr_out);
Puint16  PDCI_bcd2uint16_norange(P_t *pads, const Pbyte *bytes, Puint32 num_digits, Pbyte **ptr_out);
Puint32  PDCI_bcd2uint32_norange(P_t *pads, const Pbyte *bytes, Puint32 num_digits, Pbyte **ptr_out);
Puint64  PDCI_bcd2uint64_norange(P_t *pads, const Pbyte *bytes, Puint32 num_digits, Pbyte **ptr_out);

ssize_t PDCI_uint8_2bcd_buf (P_t *pads, Pbyte *outbuf, size_t outbuf_len, int *outbuf_full, Puint8  u, Puint32 num_digits);
ssize_t PDCI_uint16_2bcd_buf(P_t *pads, Pbyte *outbuf, size_t outbuf_len, int *outbuf_full, Puint16 u, Puint32 num_digits);
ssize_t PDCI_uint32_2bcd_buf(P_t *pads, Pbyte *outbuf, size_t outbuf_len, int *outbuf_full, Puint32 u, Puint32 num_digits);
ssize_t PDCI_uint64_2bcd_buf(P_t *pads, Pbyte *outbuf, size_t outbuf_len, int *outbuf_full, Puint64 u, Puint32 num_digits);

ssize_t PDCI_uint8_2bcd_io (P_t *pads, Sfio_t *io, Puint8  u, Puint32 num_digits);
ssize_t PDCI_uint16_2bcd_io(P_t *pads, Sfio_t *io, Puint16 u, Puint32 num_digits);
ssize_t PDCI_uint32_2bcd_io(P_t *pads, Sfio_t *io, Puint32 u, Puint32 num_digits);
ssize_t PDCI_uint64_2bcd_io(P_t *pads, Sfio_t *io, Puint64 u, Puint32 num_digits);

Pint8   PDCI_sbl2int8 (P_t *pads, const Pbyte *bytes, Puint32 num_bytes, Pbyte **ptr_out);
Pint16  PDCI_sbl2int16(P_t *pads, const Pbyte *bytes, Puint32 num_bytes, Pbyte **ptr_out);
Pint32  PDCI_sbl2int32(P_t *pads, const Pbyte *bytes, Puint32 num_bytes, Pbyte **ptr_out);
Pint64  PDCI_sbl2int64(P_t *pads, const Pbyte *bytes, Puint32 num_bytes, Pbyte **ptr_out);

/* Above functions do not have range errors, so we do not need separate _norange versions */
#define PDCI_sbl2int8_norange  PDCI_sbl2int8
#define PDCI_sbl2int16_norange PDCI_sbl2int16
#define PDCI_sbl2int32_norange PDCI_sbl2int32
#define PDCI_sbl2int64_norange PDCI_sbl2int64

ssize_t PDCI_int8_2sbl_buf (P_t *pads, Pbyte *outbuf, size_t outbuf_len, int *outbuf_full, Pint8  i, Puint32 num_bytes);
ssize_t PDCI_int16_2sbl_buf(P_t *pads, Pbyte *outbuf, size_t outbuf_len, int *outbuf_full, Pint16 i, Puint32 num_bytes);
ssize_t PDCI_int32_2sbl_buf(P_t *pads, Pbyte *outbuf, size_t outbuf_len, int *outbuf_full, Pint32 i, Puint32 num_bytes);
ssize_t PDCI_int64_2sbl_buf(P_t *pads, Pbyte *outbuf, size_t outbuf_len, int *outbuf_full, Pint64 i, Puint32 num_bytes);

ssize_t PDCI_int8_2sbl_io (P_t *pads, Sfio_t *io, Pint8  i, Puint32 num_bytes);
ssize_t PDCI_int16_2sbl_io(P_t *pads, Sfio_t *io, Pint16 i, Puint32 num_bytes);
ssize_t PDCI_int32_2sbl_io(P_t *pads, Sfio_t *io, Pint32 i, Puint32 num_bytes);
ssize_t PDCI_int64_2sbl_io(P_t *pads, Sfio_t *io, Pint64 i, Puint32 num_bytes);

Puint8   PDCI_sbl2uint8 (P_t *pads, const Pbyte *bytes, Puint32 num_bytes, Pbyte **ptr_out);
Puint16  PDCI_sbl2uint16(P_t *pads, const Pbyte *bytes, Puint32 num_bytes, Pbyte **ptr_out);
Puint32  PDCI_sbl2uint32(P_t *pads, const Pbyte *bytes, Puint32 num_bytes, Pbyte **ptr_out);
Puint64  PDCI_sbl2uint64(P_t *pads, const Pbyte *bytes, Puint32 num_bytes, Pbyte **ptr_out);

/* Above functions do not have range errors, so we do not need separate _norange versions */
#define PDCI_sbl2uint8_norange  PDCI_sbl2uint8
#define PDCI_sbl2uint16_norange PDCI_sbl2uint16
#define PDCI_sbl2uint32_norange PDCI_sbl2uint32
#define PDCI_sbl2uint64_norange PDCI_sbl2uint64

ssize_t PDCI_uint8_2sbl_buf (P_t *pads, Pbyte *outbuf, size_t outbuf_len, int *outbuf_full, Puint8  u, Puint32 num_bytes);
ssize_t PDCI_uint16_2sbl_buf(P_t *pads, Pbyte *outbuf, size_t outbuf_len, int *outbuf_full, Puint16 u, Puint32 num_bytes);
ssize_t PDCI_uint32_2sbl_buf(P_t *pads, Pbyte *outbuf, size_t outbuf_len, int *outbuf_full, Puint32 u, Puint32 num_bytes);
ssize_t PDCI_uint64_2sbl_buf(P_t *pads, Pbyte *outbuf, size_t outbuf_len, int *outbuf_full, Puint64 u, Puint32 num_bytes);

ssize_t PDCI_uint8_2sbl_io (P_t *pads, Sfio_t *io, Puint8  u, Puint32 num_bytes);
ssize_t PDCI_uint16_2sbl_io(P_t *pads, Sfio_t *io, Puint16 u, Puint32 num_bytes);
ssize_t PDCI_uint32_2sbl_io(P_t *pads, Sfio_t *io, Puint32 u, Puint32 num_bytes);
ssize_t PDCI_uint64_2sbl_io(P_t *pads, Sfio_t *io, Puint64 u, Puint32 num_bytes);

Pint8   PDCI_sbh2int8 (P_t *pads, const Pbyte *bytes, Puint32 num_bytes, Pbyte **ptr_out);
Pint16  PDCI_sbh2int16(P_t *pads, const Pbyte *bytes, Puint32 num_bytes, Pbyte **ptr_out);
Pint32  PDCI_sbh2int32(P_t *pads, const Pbyte *bytes, Puint32 num_bytes, Pbyte **ptr_out);
Pint64  PDCI_sbh2int64(P_t *pads, const Pbyte *bytes, Puint32 num_bytes, Pbyte **ptr_out);

/* Above functions do not have range errors, so we do not need separate _norange versions */
#define PDCI_sbh2int8_norange  PDCI_sbh2int8
#define PDCI_sbh2int16_norange PDCI_sbh2int16
#define PDCI_sbh2int32_norange PDCI_sbh2int32
#define PDCI_sbh2int64_norange PDCI_sbh2int64

ssize_t PDCI_int8_2sbh_buf (P_t *pads, Pbyte *outbuf, size_t outbuf_len, int *outbuf_full, Pint8  i, Puint32 num_bytes);
ssize_t PDCI_int16_2sbh_buf(P_t *pads, Pbyte *outbuf, size_t outbuf_len, int *outbuf_full, Pint16 i, Puint32 num_bytes);
ssize_t PDCI_int32_2sbh_buf(P_t *pads, Pbyte *outbuf, size_t outbuf_len, int *outbuf_full, Pint32 i, Puint32 num_bytes);
ssize_t PDCI_int64_2sbh_buf(P_t *pads, Pbyte *outbuf, size_t outbuf_len, int *outbuf_full, Pint64 i, Puint32 num_bytes);

ssize_t PDCI_int8_2sbh_io (P_t *pads, Sfio_t *io, Pint8  i, Puint32 num_bytes);
ssize_t PDCI_int16_2sbh_io(P_t *pads, Sfio_t *io, Pint16 i, Puint32 num_bytes);
ssize_t PDCI_int32_2sbh_io(P_t *pads, Sfio_t *io, Pint32 i, Puint32 num_bytes);
ssize_t PDCI_int64_2sbh_io(P_t *pads, Sfio_t *io, Pint64 i, Puint32 num_bytes);

Puint8   PDCI_sbh2uint8 (P_t *pads, const Pbyte *bytes, Puint32 num_bytes, Pbyte **ptr_out);
Puint16  PDCI_sbh2uint16(P_t *pads, const Pbyte *bytes, Puint32 num_bytes, Pbyte **ptr_out);
Puint32  PDCI_sbh2uint32(P_t *pads, const Pbyte *bytes, Puint32 num_bytes, Pbyte **ptr_out);
Puint64  PDCI_sbh2uint64(P_t *pads, const Pbyte *bytes, Puint32 num_bytes, Pbyte **ptr_out);

/* Above functions do not have range errors, so we do not need separate _norange versions */
#define PDCI_sbh2uint8_norange  PDCI_sbh2uint8
#define PDCI_sbh2uint16_norange PDCI_sbh2uint16
#define PDCI_sbh2uint32_norange PDCI_sbh2uint32
#define PDCI_sbh2uint64_norange PDCI_sbh2uint64

ssize_t PDCI_uint8_2sbh_buf (P_t *pads, Pbyte *outbuf, size_t outbuf_len, int *outbuf_full, Puint8  u, Puint32 num_bytes);
ssize_t PDCI_uint16_2sbh_buf(P_t *pads, Pbyte *outbuf, size_t outbuf_len, int *outbuf_full, Puint16 u, Puint32 num_bytes);
ssize_t PDCI_uint32_2sbh_buf(P_t *pads, Pbyte *outbuf, size_t outbuf_len, int *outbuf_full, Puint32 u, Puint32 num_bytes);
ssize_t PDCI_uint64_2sbh_buf(P_t *pads, Pbyte *outbuf, size_t outbuf_len, int *outbuf_full, Puint64 u, Puint32 num_bytes);

ssize_t PDCI_uint8_2sbh_io (P_t *pads, Sfio_t *io, Puint8  u, Puint32 num_bytes);
ssize_t PDCI_uint16_2sbh_io(P_t *pads, Sfio_t *io, Puint16 u, Puint32 num_bytes);
ssize_t PDCI_uint32_2sbh_io(P_t *pads, Sfio_t *io, Puint32 u, Puint32 num_bytes);
ssize_t PDCI_uint64_2sbh_io(P_t *pads, Sfio_t *io, Puint64 u, Puint32 num_bytes);

/* ================================================================================ */
/* INTERNAL MISC TYPES + ROUTINES */

Perror_t PDCI_regexp_compile_cstr(P_t *pads, const char *regexp_str, Pregexp_t *regexp, const char *whatfn);
Perror_t PDCI_regexp_compile(P_t *pads, const Pstring *regexp_str, Pregexp_t *regexp, const char *whatfn);
Perror_t PDCI_regexp_cleanup(P_t *pads, Pregexp_t *regexp, const char *whatfn);
int         PDCI_regexp_match(P_t *pads, Pregexp_t *regexp, Pbyte *begin, Pbyte *end,
			      regflags_t e_flags, Pcharset char_set);

/*  PDCI_regexp_match returns 0 on success (match), non-zero on failure.
 *  On success, the offset of the matched characters is given by:
 *       regexp->match[0].rm_so, regexp->match[0].rm_eo where the difference
 *  gives the match length.
 *
 *  The region to match against is bound by begin/end, where end-begin gives the number of
 *  bytes in the region (*end is not included in the match).
 *
 *  e_flags should be set as follows:
 *        set REG_LEFT    if the match must include the first character (*begin)
 *        set REG_NOTBOL  if begin is not at bor (beginning of record)
 *        set REG_NOTEOL  if end is not at eor (end of record)
 */

/* Accum impl helpers:
 *
 * PDCI_nst_prefix_what prints a heading to outstr 
 * based on *nst nesting level and
 * (unless *nst is -1) it increments the nesting level.
 */

void PDCI_nst_prefix_what(Sfio_t *outstr, int *nst, const char *prefix, const char *what);

/* 
 * PDCI_findfirst and PDCI_findlast are like strchr and strrchr except NULL does
 * not terminate the search: instead, begin/end bracket the search space, where
 * end is one byte beyond the last byte to check.
 */ 
Pbyte *PDCI_findfirst(const Pbyte *begin, const Pbyte *end, Pbyte b);
Pbyte *PDCI_findlast(const Pbyte *begin, const Pbyte *end, Pbyte b);

/* ================================================================================ */

#endif /*  __PADS_INTERNAL__  */
