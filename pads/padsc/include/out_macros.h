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

#define PDC_LEV_INFO      ERROR_INFO
#define PDC_LEV_WARN      ERROR_WARNING
#define PDC_LEV_ERR       ERROR_ERROR
#define PDC_LEV_FATAL     ERROR_FATAL
#define PDC_LEV_MASK      ERROR_LEVEL

#define PDC_FLG_PROMPT    ERROR_PROMPT
#define PDC_FLG_SYSERR    ERROR_SYSTEM
#define PDC_FLG_LIBRARY   ERROR_LIBRARY

#define PDC_GET_LEV(flags) (flags & PDC_LEV_MASK)
#define PDC_GET_FLG(flags) (flags & ~PDC_LEV_MASK)

#define PDC_DBG_FLAGS      -2
#define PDC_TRACE_FLAGS    -4
#define PDC_WARN_FLAGS     PDC_LEV_WARN
#define PDC_SYSERR_FLAGS   PDC_LEV_ERR|PDC_FLG_SYSERR
#define PDC_FATAL_FLAGS    PDC_LEV_FATAL

#define PDC_NULL_STMT do { } while (0)

#ifndef NDEBUG
# define PDC_DBG(pdc,disc,fmt)                   PDC_FMT_ERR_MSG ((pdc),(disc),(fmt),PDC_DBG_FLAGS)
# define PDC_DBG1(pdc,disc,fmt,a1)               PDC_FMT_ERR_MSG1((pdc),(disc),(fmt),PDC_DBG_FLAGS,(a1))
# define PDC_DBG2(pdc,disc,fmt,a1,a2)            PDC_FMT_ERR_MSG2((pdc),(disc),(fmt),PDC_DBG_FLAGS,(a1),(a2))
# define PDC_DBG3(pdc,disc,fmt,a1,a2,a3)         PDC_FMT_ERR_MSG3((pdc),(disc),(fmt),PDC_DBG_FLAGS,(a1),(a2),(a3))
# define PDC_DBG4(pdc,disc,fmt,a1,a2,a3,a4)      PDC_FMT_ERR_MSG4((pdc),(disc),(fmt),PDC_DBG_FLAGS,(a1),(a2),(a3),(a4))
# define PDC_DBG5(pdc,disc,fmt,a1,a2,a3,a4,a5)   PDC_FMT_ERR_MSG5((pdc),(disc),(fmt),PDC_DBG_FLAGS,(a1),(a2),(a3),(a4),(a5))

# define PDC_TRACE(pdc,disc,fmt)                 PDC_FMT_ERR_MSG ((pdc),(disc),(fmt),PDC_TRACE_FLAGS)
# define PDC_TRACE1(pdc,disc,fmt,a1)             PDC_FMT_ERR_MSG1((pdc),(disc),(fmt),PDC_TRACE_FLAGS,(a1))
# define PDC_TRACE2(pdc,disc,fmt,a1,a2)          PDC_FMT_ERR_MSG2((pdc),(disc),(fmt),PDC_TRACE_FLAGS,(a1),(a2))
# define PDC_TRACE3(pdc,disc,fmt,a1,a2,a3)       PDC_FMT_ERR_MSG3((pdc),(disc),(fmt),PDC_TRACE_FLAGS,(a1),(a2),(a3))
# define PDC_TRACE4(pdc,disc,fmt,a1,a2,a3,a4)    PDC_FMT_ERR_MSG4((pdc),(disc),(fmt),PDC_TRACE_FLAGS,(a1),(a2),(a3),(a4))
# define PDC_TRACE5(pdc,disc,fmt,a1,a2,a3,a4,a5) PDC_FMT_ERR_MSG5((pdc),(disc),(fmt),PDC_TRACE_FLAGS,(a1),(a2),(a3),(a4),(a5))
#else
# define PDC_DBG(pdc,disc,fmt)                   PDC_NULL_STMT
# define PDC_DBG1(pdc,disc,fmt,a1)               PDC_NULL_STMT
# define PDC_DBG2(pdc,disc,fmt,a1,a2)            PDC_NULL_STMT
# define PDC_DBG3(pdc,disc,fmt,a1,a2,a3)         PDC_NULL_STMT
# define PDC_DBG4(pdc,disc,fmt,a1,a2,a3,a4)      PDC_NULL_STMT
# define PDC_DBG5(pdc,disc,fmt,a1,a2,a3,a4,a5)   PDC_NULL_STMT

# define PDC_TRACE(pdc,disc,fmt)                 PDC_NULL_STMT
# define PDC_TRACE1(pdc,disc,fmt,a1)             PDC_NULL_STMT
# define PDC_TRACE2(pdc,disc,fmt,a1,a2)          PDC_NULL_STMT
# define PDC_TRACE3(pdc,disc,fmt,a1,a2,a3)       PDC_NULL_STMT
# define PDC_TRACE4(pdc,disc,fmt,a1,a2,a3,a4)    PDC_NULL_STMT
# define PDC_TRACE5(pdc,disc,fmt,a1,a2,a3,a4,a5) PDC_NULL_STMT
#endif

#define PDC_WARN(pdc,disc,fmt)                   PDC_FMT_ERR_MSG ((pdc),(disc),(fmt),PDC_WARN_FLAGS)
#define PDC_WARN1(pdc,disc,fmt,a1)               PDC_FMT_ERR_MSG1((pdc),(disc),(fmt),PDC_WARN_FLAGS,(a1))
#define PDC_WARN2(pdc,disc,fmt,a1,a2)            PDC_FMT_ERR_MSG2((pdc),(disc),(fmt),PDC_WARN_FLAGS,(a1),(a2))
#define PDC_WARN3(pdc,disc,fmt,a1,a2,a3)         PDC_FMT_ERR_MSG3((pdc),(disc),(fmt),PDC_WARN_FLAGS,(a1),(a2),(a3))
#define PDC_WARN4(pdc,disc,fmt,a1,a2,a3,a4)      PDC_FMT_ERR_MSG4((pdc),(disc),(fmt),PDC_WARN_FLAGS,(a1),(a2),(a3),(a4))
#define PDC_WARN5(pdc,disc,fmt,a1,a2,a3,a4,a5)   PDC_FMT_ERR_MSG5((pdc),(disc),(fmt),PDC_WARN_FLAGS,(a1),(a2),(a3),(a4),(a5))

#define PDC_SYSERR(pdc,disc,fmt)                 PDC_FMT_ERR_MSG ((pdc),(disc),(fmt),PDC_SYSERR_FLAGS)
#define PDC_SYSERR1(pdc,disc,fmt,a1)             PDC_FMT_ERR_MSG1((pdc),(disc),(fmt),PDC_SYSERR_FLAGS,(a1))
#define PDC_SYSERR2(pdc,disc,fmt,a1,a2)          PDC_FMT_ERR_MSG2((pdc),(disc),(fmt),PDC_SYSERR_FLAGS,(a1),(a2))
#define PDC_SYSERR3(pdc,disc,fmt,a1,a2,a3)       PDC_FMT_ERR_MSG3((pdc),(disc),(fmt),PDC_SYSERR_FLAGS,(a1),(a2),(a3))
#define PDC_SYSERR4(pdc,disc,fmt,a1,a2,a3,a4)    PDC_FMT_ERR_MSG4((pdc),(disc),(fmt),PDC_SYSERR_FLAGS,(a1),(a2),(a3),(a4))
#define PDC_SYSERR5(pdc,disc,fmt,a1,a2,a3,a4,a5) PDC_FMT_ERR_MSG5((pdc),(disc),(fmt),PDC_SYSERR_FLAGS,(a1),(a2),(a3),(a4),(a5))

#define PDC_FATAL(pdc,disc,fmt)                  PDC_FMT_ERR_MSG ((pdc),(disc),("FATAL: " fmt),PDC_FATAL_FLAGS)
#define PDC_FATAL1(pdc,disc,fmt,a1)              PDC_FMT_ERR_MSG1((pdc),(disc),("FATAL: " fmt),PDC_FATAL_FLAGS,(a1))
#define PDC_FATAL2(pdc,disc,fmt,a1,a2)           PDC_FMT_ERR_MSG2((pdc),(disc),("FATAL: " fmt),PDC_FATAL_FLAGS,(a1),(a2))
#define PDC_FATAL3(pdc,disc,fmt,a1,a2,a3)        PDC_FMT_ERR_MSG3((pdc),(disc),("FATAL: " fmt),PDC_FATAL_FLAGS,(a1),(a2),(a3))
#define PDC_FATAL4(pdc,disc,fmt,a1,a2,a3,a4)     PDC_FMT_ERR_MSG4((pdc),(disc),("FATAL: " fmt),PDC_FATAL_FLAGS,(a1),(a2),(a3),(a4))
#define PDC_FATAL5(pdc,disc,fmt,a1,a2,a3,a4,a5)  PDC_FMT_ERR_MSG5((pdc),(disc),("FATAL: " fmt),PDC_FATAL_FLAGS,(a1),(a2),(a3),(a4),(a5))

#define PDC_FMT_ERR_MSG(pdc,disc,fmt,erlev) \
  do { if ((disc) && (disc)->errorf) {(*(disc)->errorf)(NiL, erlev, fmt);} } while (0)
#define PDC_FMT_ERR_MSG1(pdc,disc,fmt,erlev,a1) \
  do { if ((disc) && (disc)->errorf) {(*(disc)->errorf)(NiL, erlev, fmt, a1);} } while (0)
#define PDC_FMT_ERR_MSG2(pdc,disc,fmt,erlev,a1,a2) \
  do { if ((disc) && (disc)->errorf) {(*(disc)->errorf)(NiL, erlev, fmt, a1, a2);} } while (0)
#define PDC_FMT_ERR_MSG3(pdc,disc,fmt,erlev,a1,a2,a3) \
  do { if ((disc) && (disc)->errorf) {(*(disc)->errorf)(NiL, erlev, fmt, a1, a2, a3);} } while (0)
#define PDC_FMT_ERR_MSG4(pdc,disc,fmt,erlev,a1,a2,a3,a4) \
  do { if ((disc) && (disc)->errorf) {(*(disc)->errorf)(NiL, erlev, fmt, a1, a2, a3, a4);} } while (0)
#define PDC_FMT_ERR_MSG5(pdc,disc,fmt,erlev,a1,a2,a3,a4,a5) \
  do { if ((disc) && (disc)->errorf) {(*(disc)->errorf)(NiL, erlev, fmt, a1, a2, a3, a4, a5);} } while (0)

/* ================================================================================ */

#endif /*  __PDC_OUT_MACROS_H__  */
