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
# define PDC_DBG(disc,fmt)                   PDC_FMT_ERR_MSG ((disc),(fmt),PDC_DBG_FLAGS)
# define PDC_DBG1(disc,fmt,a1)               PDC_FMT_ERR_MSG1((disc),(fmt),PDC_DBG_FLAGS,(a1))
# define PDC_DBG2(disc,fmt,a1,a2)            PDC_FMT_ERR_MSG2((disc),(fmt),PDC_DBG_FLAGS,(a1),(a2))
# define PDC_DBG3(disc,fmt,a1,a2,a3)         PDC_FMT_ERR_MSG3((disc),(fmt),PDC_DBG_FLAGS,(a1),(a2),(a3))
# define PDC_DBG4(disc,fmt,a1,a2,a3,a4)      PDC_FMT_ERR_MSG4((disc),(fmt),PDC_DBG_FLAGS,(a1),(a2),(a3),(a4))
# define PDC_DBG5(disc,fmt,a1,a2,a3,a4,a5)   PDC_FMT_ERR_MSG5((disc),(fmt),PDC_DBG_FLAGS,(a1),(a2),(a3),(a4),(a5))

# define PDC_TRACE(disc,fmt)                 PDC_FMT_ERR_MSG ((disc),(fmt),PDC_TRACE_FLAGS)
# define PDC_TRACE1(disc,fmt,a1)             PDC_FMT_ERR_MSG1((disc),(fmt),PDC_TRACE_FLAGS,(a1))
# define PDC_TRACE2(disc,fmt,a1,a2)          PDC_FMT_ERR_MSG2((disc),(fmt),PDC_TRACE_FLAGS,(a1),(a2))
# define PDC_TRACE3(disc,fmt,a1,a2,a3)       PDC_FMT_ERR_MSG3((disc),(fmt),PDC_TRACE_FLAGS,(a1),(a2),(a3))
# define PDC_TRACE4(disc,fmt,a1,a2,a3,a4)    PDC_FMT_ERR_MSG4((disc),(fmt),PDC_TRACE_FLAGS,(a1),(a2),(a3),(a4))
# define PDC_TRACE5(disc,fmt,a1,a2,a3,a4,a5) PDC_FMT_ERR_MSG5((disc),(fmt),PDC_TRACE_FLAGS,(a1),(a2),(a3),(a4),(a5))
#else
# define PDC_DBG(disc,fmt)                   PDC_NULL_STMT
# define PDC_DBG1(disc,fmt,a1)               PDC_NULL_STMT
# define PDC_DBG2(disc,fmt,a1,a2)            PDC_NULL_STMT
# define PDC_DBG3(disc,fmt,a1,a2,a3)         PDC_NULL_STMT
# define PDC_DBG4(disc,fmt,a1,a2,a3,a4)      PDC_NULL_STMT
# define PDC_DBG5(disc,fmt,a1,a2,a3,a4,a5)   PDC_NULL_STMT

# define PDC_TRACE(disc,fmt)                 PDC_NULL_STMT
# define PDC_TRACE1(disc,fmt,a1)             PDC_NULL_STMT
# define PDC_TRACE2(disc,fmt,a1,a2)          PDC_NULL_STMT
# define PDC_TRACE3(disc,fmt,a1,a2,a3)       PDC_NULL_STMT
# define PDC_TRACE4(disc,fmt,a1,a2,a3,a4)    PDC_NULL_STMT
# define PDC_TRACE5(disc,fmt,a1,a2,a3,a4,a5) PDC_NULL_STMT
#endif

#define PDC_WARN(disc,fmt)                   PDC_FMT_ERR_MSG ((disc),(fmt),PDC_WARN_FLAGS)
#define PDC_WARN1(disc,fmt,a1)               PDC_FMT_ERR_MSG1((disc),(fmt),PDC_WARN_FLAGS,(a1))
#define PDC_WARN2(disc,fmt,a1,a2)            PDC_FMT_ERR_MSG2((disc),(fmt),PDC_WARN_FLAGS,(a1),(a2))
#define PDC_WARN3(disc,fmt,a1,a2,a3)         PDC_FMT_ERR_MSG3((disc),(fmt),PDC_WARN_FLAGS,(a1),(a2),(a3))
#define PDC_WARN4(disc,fmt,a1,a2,a3,a4)      PDC_FMT_ERR_MSG4((disc),(fmt),PDC_WARN_FLAGS,(a1),(a2),(a3),(a4))
#define PDC_WARN5(disc,fmt,a1,a2,a3,a4,a5)   PDC_FMT_ERR_MSG5((disc),(fmt),PDC_WARN_FLAGS,(a1),(a2),(a3),(a4),(a5))

#define PDC_SYSERR(disc,fmt)                 PDC_FMT_ERR_MSG ((disc),(fmt),PDC_SYSERR_FLAGS)
#define PDC_SYSERR1(disc,fmt,a1)             PDC_FMT_ERR_MSG1((disc),(fmt),PDC_SYSERR_FLAGS,(a1))
#define PDC_SYSERR2(disc,fmt,a1,a2)          PDC_FMT_ERR_MSG2((disc),(fmt),PDC_SYSERR_FLAGS,(a1),(a2))
#define PDC_SYSERR3(disc,fmt,a1,a2,a3)       PDC_FMT_ERR_MSG3((disc),(fmt),PDC_SYSERR_FLAGS,(a1),(a2),(a3))
#define PDC_SYSERR4(disc,fmt,a1,a2,a3,a4)    PDC_FMT_ERR_MSG4((disc),(fmt),PDC_SYSERR_FLAGS,(a1),(a2),(a3),(a4))
#define PDC_SYSERR5(disc,fmt,a1,a2,a3,a4,a5) PDC_FMT_ERR_MSG5((disc),(fmt),PDC_SYSERR_FLAGS,(a1),(a2),(a3),(a4),(a5))

#define PDC_FATAL(disc,fmt)                  PDC_FMT_ERR_MSG ((disc),("FATAL: " fmt),PDC_FATAL_FLAGS)
#define PDC_FATAL1(disc,fmt,a1)              PDC_FMT_ERR_MSG1((disc),("FATAL: " fmt),PDC_FATAL_FLAGS,(a1))
#define PDC_FATAL2(disc,fmt,a1,a2)           PDC_FMT_ERR_MSG2((disc),("FATAL: " fmt),PDC_FATAL_FLAGS,(a1),(a2))
#define PDC_FATAL3(disc,fmt,a1,a2,a3)        PDC_FMT_ERR_MSG3((disc),("FATAL: " fmt),PDC_FATAL_FLAGS,(a1),(a2),(a3))
#define PDC_FATAL4(disc,fmt,a1,a2,a3,a4)     PDC_FMT_ERR_MSG4((disc),("FATAL: " fmt),PDC_FATAL_FLAGS,(a1),(a2),(a3),(a4))
#define PDC_FATAL5(disc,fmt,a1,a2,a3,a4,a5)  PDC_FMT_ERR_MSG5((disc),("FATAL: " fmt),PDC_FATAL_FLAGS,(a1),(a2),(a3),(a4),(a5))

#define PDC_FMT_ERR_MSG(disc,fmt,erlev) \
  do { if (disc && disc->errorf) {disc->errorf(NiL, erlev, fmt);} } while (0)
#define PDC_FMT_ERR_MSG1(disc,fmt,erlev,a1) \
  do { if (disc && disc->errorf) {disc->errorf(NiL, erlev, fmt, a1);} } while (0)
#define PDC_FMT_ERR_MSG2(disc,fmt,erlev,a1,a2) \
  do { if (disc && disc->errorf) {disc->errorf(NiL, erlev, fmt, a1, a2);} } while (0)
#define PDC_FMT_ERR_MSG3(disc,fmt,erlev,a1,a2,a3) \
  do { if (disc && disc->errorf) {disc->errorf(NiL, erlev, fmt, a1, a2, a3);} } while (0)
#define PDC_FMT_ERR_MSG4(disc,fmt,erlev,a1,a2,a3,a4) \
  do { if (disc && disc->errorf) {disc->errorf(NiL, erlev, fmt, a1, a2, a3, a4);} } while (0)
#define PDC_FMT_ERR_MSG5(disc,fmt,erlev,a1,a2,a3,a4,a5) \
  do { if (disc && disc->errorf) {disc->errorf(NiL, erlev, fmt, a1, a2, a3, a4, a5);} } while (0)

/* ================================================================================ */

#endif /*  __PDC_OUT_MACROS_H__  */
