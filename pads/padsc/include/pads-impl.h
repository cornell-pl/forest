#ifdef _USE_PROTO
#pragma prototyped
#endif
/*
 * PADS library interface: macro implementations of certain function calls
 *   
 * NOTE: C prototypes for these functions are given in pads.h
 *
 * Kathleen Fisher, Robert Gruber
 * AT&T Labs Research
 */

#ifndef __PADS_IMPL_H__
#define __PADS_IMPL_H__

/* Regexp support (that the compiler needs know about ???) */

Perror_t PDCI_regexp_compile_cstr(P_t *pads, const char *regexp_str, Pregexp_t *regexp,
				  const char *err_prefix, const char *whatfn);
Perror_t PDCI_regexp_compile(P_t *pads, const Pstring *regexp_str, Pregexp_t *regexp,
			     const char *err_prefix, const char *whatfn);
Perror_t PDCI_regexp_cleanup(P_t *pads, Pregexp_t *regexp, const char *whatfn);

#ifndef FOR_CKIT

/* ================================================================================
 * Helper Macro
 * 
 * P_DEF_CHARSET(pads) : produces pads->disc->def_charset (or Pcharset_INVALID)
 *
 * PDCI_CHARSET_SWITCH(pads,fn_pre, fn_post, ,args) selects from a set of
 * functions based on pads->disc->def_charset.  It inserts an appropriate
 * charset marker ("a_", "e_", etc.) between fn_pre and fn_post to form a
 * function name, and calls that function with the specified args, which must be
 * given in parens.  If charset is not valid it will still choose one
 * of the possible functions -- that function should detect and report the problem.
 *
 * Example:
 *
 *    PDCI_CHARSET_SWITCH(pads, my, fun, (pads, x, y))
 *
 * Is converted to one of these forms:
 * 
 *    mya_fun(pads, x, y)
 *    mye_fun(pads, x, y)
 *    etc. 
 */

#ifndef NDEBUG
/* safe versions */

#define PDCI_DEF_CHARSET(pads) \
  ((pads && pads->disc) ? (pads->disc->def_charset) : Pcharset_INVALID)

#define PDCI_CHARSET_SWITCH(pads,fn_pre,fn_post,args) \
 ((pads && pads->disc && pads->disc->def_charset == Pcharset_ASCII) \
    ?  fn_pre ## a_ ## fn_post args \
    :  fn_pre ## e_ ## fn_post args)

#else
/* unsafe versions */

#define PDCI_DEF_CHARSET(pads) \
   (pads->disc->def_charset)

#define PDCI_CHARSET_SWITCH(pads,fn_pre,fn_post,args) \
 ((pads->disc->def_charset == Pcharset_ASCII) \
    ?  fn_pre ## a_ ## fn_post args \
    :  fn_pre ## e_ ## fn_post args)

#endif /* !NDEBUG */

/* Macros for setting or testing parse state (PS) pd->pstate */
/* These can be used with both Pbase_pd and with generated structured parse descriptors */

#define P_PS_init(pd)          do { (pd)->pstate = 0; } while (0)

#define P_PS_setPanic(pd)      do { (pd)->pstate |= P_Panic; } while (0)
#define P_PS_unsetPanic(pd)    do { (pd)->pstate &= ~P_Panic; } while (0)
#define P_PS_isPanic(pd)       ((pd)->pstate & P_Panic)

#define P_PS_setPartial(pd)      do { (pd)->pstate |= P_Partial; } while (0)
#define P_PS_unsetPartial(pd)    do { (pd)->pstate &= ~P_Partial; } while (0)
#define P_PS_isPartial(pd)       ((pd)->pstate & P_Partial)

/* Identifier suport macros. */

#ifdef USE_GALAX

/* The maximum number of nodes in a pd (including itself) is 14, */
/* which is the numer of nodes in the pd of an array with errors. */
/* In addition, arrays have a length field, which makes 15. */
#  define PDCI_MAX_ID_INCR 15

#  define PDCI_ID_RESET(padsIN, valIN) \
     do{ \
       (padsIN)->stack[(padsIN)->top].id_gen = (valIN); \
     }while(0)

#  define PDCI_ID_NEW(idOUT, padsIN) \
do{ \
  (idOUT) = (padsIN)->stack[(padsIN)->top].id_gen; \
  (padsIN)->stack[(padsIN)->top].id_gen += PDCI_MAX_ID_INCR; \
}while(0)

#  define PDCI_SAVE_PD_ID(idtmpIN, pdIN)          PDCI_id_t idtmpIN = (pdIN)->_id_
#  define PDCI_REST_PD_ID(idtmpIN, pdIN)          (pdIN)->_id_ = idtmpIN
#  define PDCI_IO_RESTORE_KEEP_ID_GEN(fn_nmIN, padsIN) \
     do{ \
       PDCI_id_t id_tmp = (padsIN)->stack[(padsIN)->top].id_gen; \
       if (P_ERR == P_io_restore(padsIN)) { \
         PDCI_report_err(padsIN, P_LEV_FATAL, 0, P_RESTORE_ERR, fn_nmIN, 0); \
       } \
       (padsIN)->stack[(padsIN)->top].id_gen = id_tmp; \
     }while(0)

#else

#  define PDCI_SAVE_PD_ID(idtmpIN, pdIN)          P_NULL_STMT
#  define PDCI_REST_PD_ID(idtmpIN, pdIN)          P_NULL_STMT
#  define PDCI_IO_RESTORE_KEEP_ID_GEN(fn_nmIN, padsIN) \
     do{ \
       if (P_ERR == P_io_restore(padsIN)) { \
         PDCI_report_err(padsIN, P_LEV_FATAL, 0, P_RESTORE_ERR, fn_nmIN, 0); \
       } \
     }while(0)

#endif

/* Parse descriptor macros */

#define PD_COMMON_INIT(pd) do { \
  memset((void*)(pd), 0, sizeof(Pbase_pd)); \
} while (0)

#define PD_COMMON_INIT_NO_ERR(pd) do { \
  memset((void*)(pd), 0, sizeof(Pbase_pd)); \
  (pd)->errCode = P_NO_ERR; \
} while (0)

#ifndef USE_GALAX
# define PD_PGLX_INIT(padsIN,pdIN) 
#else
# define PD_PGLX_INIT(padsIN,pdIN) \
  do { \
    PDCI_ID_NEW(((pdIN)->_id_),padsIN);\
  } while (0)
#endif

#define PD_COMMON_READ_INIT(padsIN,pdIN) \
do { \
  PDCI_READFN_BEGINLOC(padsIN,(pdIN)->loc); \
  PD_PGLX_INIT(padsIN,pdIN);\
} while (0)

#define Pbase_pd_init(pd) PD_COMMON_INIT(pd)
#define Pbase_pd_init_no_err(pd) PD_COMMON_INIT_NO_ERR(pd)

/* ================================================================================
 * POS MACROS
 */

#define P_POS_EQ(pos1, pos2) ((pos1).offset == (pos2).offset)
/* HELPER: P_POS_GT tests whether pos1 is greater than pos2 */
/* #define P_POS_GT(pos1, pos2) ((pos1).num > (pos2).num || ((pos1).num > (pos2).num && (pos1).byte > (pos2).byte)) */
#define P_POS_GT(pos1, pos2) ((pos1).offset > (pos2).offset)

/* ================================================================================ */
/* ACCUM FUNCTION MACROS
 */

#if P_CONFIG_ACCUM_FUNCTIONS > 0

#define Ptimestamp_explicit_acc_init(pads, a)         Puint32_acc_init(pads, a)
#define Ptimestamp_explicit_acc_reset(pads, a)        Puint32_acc_reset(pads, a)
#define Ptimestamp_explicit_acc_cleanup(pads, a)      Puint32_acc_cleanup(pads, a)
#define Ptimestamp_explicit_acc_add(pads, a, pd, val) Puint32_acc_add(pads, a, pd, val)

#define Ptimestamp_acc_init(pads, a)         Puint32_acc_init(pads, a)
#define Ptimestamp_acc_reset(pads, a)        Puint32_acc_reset(pads, a)
#define Ptimestamp_acc_cleanup(pads, a)      Puint32_acc_cleanup(pads, a)
#define Ptimestamp_acc_add(pads, a, pd, val) Puint32_acc_add(pads, a, pd, val)

#define Pdate_acc_init(pads, a)         Puint32_acc_init(pads, a)
#define Pdate_acc_reset(pads, a)        Puint32_acc_reset(pads, a)
#define Pdate_acc_cleanup(pads, a)      Puint32_acc_cleanup(pads, a)
#define Pdate_acc_add(pads, a, pd, val) Puint32_acc_add(pads, a, pd, val)

#define Ptime_acc_init(pads, a)         Puint32_acc_init(pads, a)
#define Ptime_acc_reset(pads, a)        Puint32_acc_reset(pads, a)
#define Ptime_acc_cleanup(pads, a)      Puint32_acc_cleanup(pads, a)
#define Ptime_acc_add(pads, a, pd, val) Puint32_acc_add(pads, a, pd, val)

Perror_t PDCI_date_time_acc_report(P_t *pads, const char *prefix, const char *what,
				   int nst, Puint32_acc *a,
				   const char *whatfn, const char *def_what,
				   const char *format, const char *format_descr,
				   Tm_zone_t *tzone, const char *tzone_descr);

Perror_t PDCI_date_time_acc_report2io(P_t *pads, Sfio_t *outstr, const char *prefix,
				      const char *what, int nst, Puint32_acc *a,
				      const char *whatfn, const char *def_what,
				      const char *format, const char *format_descr,
				      Tm_zone_t *tzone, const char *tzone_descr);

#define Ptimestamp_explicit_acc_report(pads, prefix, what, nst, a) \
  PDCI_date_time_acc_report(pads, prefix, what, nst, a, \
			    "Ptimestamp_explicit_acc_report", "timestamp",  \
			    pads->disc->out_formats.timestamp, "pads->disc->out_formats.timestamp", \
                            pads->out_zone, "default output time zone")

#define Ptimestamp_acc_report(pads, prefix, what, nst, a) \
  PDCI_date_time_acc_report(pads, prefix, what, nst, a, \
			    "Ptimestamp_acc_report", "timestamp", 	\
			    pads->disc->out_formats.timestamp, "pads->disc->out_formats.timestamp", \
                            pads->out_zone, "default output time zone")

#define Pdate_acc_report(pads, prefix, what, nst, a) \
  PDCI_date_time_acc_report(pads, prefix, what, nst, a, \
			    "Pdate_acc_report", "date", \
			    pads->disc->out_formats.date, "pads->disc->out_formats.date", \
                            pads->out_zone, "default output time zone")

#define Ptime_acc_report(pads, prefix, what, nst, a) \
  PDCI_date_time_acc_report(pads, prefix, what, nst, a, \
			    "Ptime_acc_report", "time", \
			    pads->disc->out_formats.time, "pads->disc->out_formats.time", \
                            pads->out_zone, "default output time zone")

#define Ptimestamp_explicit_acc_report2io(pads, outstr, prefix, what, nst, a) \
  PDCI_date_time_acc_report2io(pads, outstr, prefix, what, nst, a,	\
			       "Ptimestamp_explicit_acc_report2io", "timestamp", \
			       pads->disc->out_formats.timestamp, "pads->disc->out_formats.timestamp", \
			       pads->out_zone, "default output time zone")

#define Ptimestamp_acc_report2io(pads, outstr, prefix, what, nst, a)	\
  PDCI_date_time_acc_report2io(pads, outstr, prefix, what, nst, a,	\
			       "Ptimestamp_acc_report2io", "timestamp",	\
			       pads->disc->out_formats.timestamp, "pads->disc->out_formats.timestamp", \
			       pads->out_zone, "default output time zone")

#define Pdate_acc_report2io(pads, outstr, prefix, what, nst, a)		\
  PDCI_date_time_acc_report2io(pads, outstr, prefix, what, nst, a,	\
			       "Pdate_acc_report2io", "date",		\
			       pads->disc->out_formats.date, "pads->disc->out_formats.date", \
			       pads->out_zone, "default output time zone")

#define Ptime_acc_report2io(pads, outstr, prefix, what, nst, a)		\
  PDCI_date_time_acc_report2io(pads, outstr, prefix, what, nst, a,	\
			       "Ptime_acc_report2io", "time",		\
			       pads->disc->out_formats.time, "pads->disc->out_formats.time", \
			       pads->out_zone, "default output time zone")

#endif

/* ================================================================================ */
/* IMPL.h FILES FOR BUILT-IN TYPES
 */

#include "ptypes/impl/Pb_int-impl.h"
#include "ptypes/impl/Pbcd_fpoint-impl.h"
#include "ptypes/impl/Pbcd_int-impl.h"
#include "ptypes/impl/Pchar-impl.h"
#include "ptypes/impl/Pchar_lit-impl.h"
#include "ptypes/impl/Pcount-impl.h"
#include "ptypes/impl/Pcstr_lit-impl.h"
#include "ptypes/impl/Pdate-impl.h"
#include "ptypes/impl/Pebc_fpoint-impl.h"
#include "ptypes/impl/Pebc_int-impl.h"
#include "ptypes/impl/Pfloat-impl.h"
#include "ptypes/impl/Pint-impl.h"
#include "ptypes/impl/Pip-impl.h"
#include "ptypes/impl/Pre-impl.h"
#include "ptypes/impl/Psb_fpoint-impl.h"
#include "ptypes/impl/Psb_int-impl.h"
#include "ptypes/impl/Pstr_lit-impl.h"
#include "ptypes/impl/Pstring-impl.h"

#endif   /*   ! FOR_CKIT             */
#endif   /*   ! __PADS_IMPL_H__  */
