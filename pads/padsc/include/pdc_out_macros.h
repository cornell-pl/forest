#pragma prototyped
/*
 * PDC output macros
 * 
 * Kathleen Fisher, Robert Gruber
 * AT&T Labs Research
 */

#ifndef __PDC_OUT_MACROS_H__
#define __PDC_OUT_MACROS_H__

/* ================================================================================ */
/* OUTPUT MACROS  */

#define PDC_DBG_FLAGS      -2
#define PDC_TRACE_FLAGS    -4
#define PDC_WARN_FLAGS      2
#define PDC_SYSERR_FLAGS    ERROR_SYSTEM|2

#define PDC_NULL_STMT do { } while (0)

#ifndef NDEBUG
# define PDC_DBG(pdc, disc, fmt)                        PDC_FMT_ERR_MSG (pdc, disc, fmt, PDC_DBG_FLAGS)
# define PDC_DBG1(pdc, disc, fmt, a1)                   PDC_FMT_ERR_MSG1(pdc, disc, fmt, PDC_DBG_FLAGS, a1)
# define PDC_DBG2(pdc, disc, fmt, a1, a2)               PDC_FMT_ERR_MSG2(pdc, disc, fmt, PDC_DBG_FLAGS, a1, a2)
# define PDC_DBG3(pdc, disc, fmt, a1, a2, a3)           PDC_FMT_ERR_MSG3(pdc, disc, fmt, PDC_DBG_FLAGS, a1, a2, a3)
# define PDC_DBG4(pdc, disc, fmt, a1, a2, a3, a4)       PDC_FMT_ERR_MSG4(pdc, disc, fmt, PDC_DBG_FLAGS, a1, a2, a3, a4)
# define PDC_DBG5(pdc, disc, fmt, a1, a2, a3, a4, a5)   PDC_FMT_ERR_MSG5(pdc, disc, fmt, PDC_DBG_FLAGS, a1, a2, a3, a4, a5)

# define PDC_TRACE(pdc, disc, fmt)                      PDC_FMT_ERR_MSG (pdc, disc, fmt, PDC_TRACE_FLAGS)
# define PDC_TRACE1(pdc, disc, fmt, a1)                 PDC_FMT_ERR_MSG1(pdc, disc, fmt, PDC_TRACE_FLAGS, a1)
# define PDC_TRACE2(pdc, disc, fmt, a1, a2)             PDC_FMT_ERR_MSG2(pdc, disc, fmt, PDC_TRACE_FLAGS, a1, a2)
# define PDC_TRACE3(pdc, disc, fmt, a1, a2, a3)         PDC_FMT_ERR_MSG3(pdc, disc, fmt, PDC_TRACE_FLAGS, a1, a2, a3)
# define PDC_TRACE4(pdc, disc, fmt, a1, a2, a3, a4)     PDC_FMT_ERR_MSG4(pdc, disc, fmt, PDC_TRACE_FLAGS, a1, a2, a3, a4)
# define PDC_TRACE5(pdc, disc, fmt, a1, a2, a3, a4, a5) PDC_FMT_ERR_MSG5(pdc, disc, fmt, PDC_TRACE_FLAGS, a1, a2, a3, a4, a5)
#else
# define PDC_DBG(pdc, disc, fmt)                        PDC_NULL_STMT
# define PDC_DBG1(pdc, disc, fmt, a1)                   PDC_NULL_STMT
# define PDC_DBG2(pdc, disc, fmt, a1, a2)               PDC_NULL_STMT
# define PDC_DBG3(pdc, disc, fmt, a1, a2, a3)           PDC_NULL_STMT
# define PDC_DBG4(pdc, disc, fmt, a1, a2, a3, a4)       PDC_NULL_STMT
# define PDC_DBG5(pdc, disc, fmt, a1, a2, a3, a4, a5)   PDC_NULL_STMT

# define PDC_TRACE(pdc, disc, fmt)                      PDC_NULL_STMT
# define PDC_TRACE1(pdc, disc, fmt, a1)                 PDC_NULL_STMT
# define PDC_TRACE2(pdc, disc, fmt, a1, a2)             PDC_NULL_STMT
# define PDC_TRACE3(pdc, disc, fmt, a1, a2, a3)         PDC_NULL_STMT
# define PDC_TRACE4(pdc, disc, fmt, a1, a2, a3, a4)     PDC_NULL_STMT
# define PDC_TRACE5(pdc, disc, fmt, a1, a2, a3, a4, a5) PDC_NULL_STMT
#endif

#define PDC_WARN(pdc, disc, fmt)                        PDC_FMT_ERR_MSG (pdc, disc, fmt, PDC_WARN_FLAGS)
#define PDC_WARN1(pdc, disc, fmt, a1)                   PDC_FMT_ERR_MSG1(pdc, disc, fmt, PDC_WARN_FLAGS, a1)
#define PDC_WARN2(pdc, disc, fmt, a1, a2)               PDC_FMT_ERR_MSG2(pdc, disc, fmt, PDC_WARN_FLAGS, a1, a2)
#define PDC_WARN3(pdc, disc, fmt, a1, a2, a3)           PDC_FMT_ERR_MSG3(pdc, disc, fmt, PDC_WARN_FLAGS, a1, a2, a3)
#define PDC_WARN4(pdc, disc, fmt, a1, a2, a3, a4)       PDC_FMT_ERR_MSG4(pdc, disc, fmt, PDC_WARN_FLAGS, a1, a2, a3, a4)
#define PDC_WARN5(pdc, disc, fmt, a1, a2, a3, a4, a5)   PDC_FMT_ERR_MSG5(pdc, disc, fmt, PDC_WARN_FLAGS, a1, a2, a3, a4, a5)

#define PDC_SYSERR(pdc, disc, fmt)                      PDC_FMT_ERR_MSG (pdc, disc, fmt, PDC_SYSERR_FLAGS)
#define PDC_SYSERR1(pdc, disc, fmt, a1)                 PDC_FMT_ERR_MSG1(pdc, disc, fmt, PDC_SYSERR_FLAGS, a1)
#define PDC_SYSERR2(pdc, disc, fmt, a1, a2)             PDC_FMT_ERR_MSG2(pdc, disc, fmt, PDC_SYSERR_FLAGS, a1, a2)
#define PDC_SYSERR3(pdc, disc, fmt, a1, a2, a3)         PDC_FMT_ERR_MSG3(pdc, disc, fmt, PDC_SYSERR_FLAGS, a1, a2, a3)
#define PDC_SYSERR4(pdc, disc, fmt, a1, a2, a3, a4)     PDC_FMT_ERR_MSG4(pdc, disc, fmt, PDC_SYSERR_FLAGS, a1, a2, a3, a4)
#define PDC_SYSERR5(pdc, disc, fmt, a1, a2, a3, a4, a5) PDC_FMT_ERR_MSG5(pdc, disc, fmt, PDC_SYSERR_FLAGS, a1, a2, a3, a4, a5)

#define PDC_FMT_ERR_MSG(pdc, disc, fmt, erlev) \
  if (disc && disc->errorf) {(*disc->errorf)(pdc, disc, erlev, fmt);}
#define PDC_FMT_ERR_MSG1(pdc, disc, fmt, erlev, a1) \
  if (disc && disc->errorf) {(*disc->errorf)(pdc, disc, erlev, fmt, a1);}
#define PDC_FMT_ERR_MSG2(pdc, disc, fmt, erlev, a1, a2) \
  if (disc && disc->errorf) {(*disc->errorf)(pdc, disc, erlev, fmt, a1, a2);}
#define PDC_FMT_ERR_MSG3(pdc, disc, fmt, erlev, a1, a2, a3) \
  if (disc && disc->errorf) {(*disc->errorf)(pdc, disc, erlev, fmt, a1, a2, a3);}
#define PDC_FMT_ERR_MSG4(pdc, disc, fmt, erlev, a1, a2, a3, a4) \
  if (disc && disc->errorf) {(*disc->errorf)(pdc, disc, erlev, fmt, a1, a2, a3, a4);}
#define PDC_FMT_ERR_MSG5(pdc, disc, fmt, erlev, a1, a2, a3, a4, a5) \
  if (disc && disc->errorf) {(*disc->errorf)(pdc, disc, erlev, fmt, a1, a2, a3, a4, a5);}

/* ================================================================================ */

#endif /*  __PDC_OUT_MACROS_H__  */
