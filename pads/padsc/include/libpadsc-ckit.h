#pragma prototyped
/*
 * PDC ckit prototypes for output macros
 * 
 * Kathleen Fisher, Robert Gruber
 * AT&T Labs Research
 */

#ifndef __LIBPADSC_CKIT_H__
#define __LIBPADSC_CKIT_H__

#undef PDC_LEV_INFO
#undef PDC_LEV_WARN
#undef PDC_LEV_ERR
#undef PDC_LEV_FATAL
#undef PDC_LEV_MASK

#undef PDC_FLG_PROMPT
#undef PDC_FLG_SYSERR
#undef PDC_FLG_LIBRARY


#undef PDC_GET_LEV
#undef PDC_GET_FLG

#undef PDC_DBG
#undef PDC_DBG1
#undef PDC_DBG2
#undef PDC_DBG3
#undef PDC_DBG4
#undef PDC_DBG5

#undef PDC_TRACE
#undef PDC_TRACE1
#undef PDC_TRACE2
#undef PDC_TRACE3
#undef PDC_TRACE4
#undef PDC_TRACE5

#undef PDC_WARN
#undef PDC_WARN1
#undef PDC_WARN2
#undef PDC_WARN3
#undef PDC_WARN4
#undef PDC_WARN5

#undef PDC_SYSERR
#undef PDC_SYSERR1
#undef PDC_SYSERR2
#undef PDC_SYSERR3
#undef PDC_SYSERR4
#undef PDC_SYSERR5

#undef PDC_FATAL
#undef PDC_FATAL1
#undef PDC_FATAL2
#undef PDC_FATAL3
#undef PDC_FATAL4
#undef PDC_FATAL5

/* ================================================================================ */
/* OUTPUT MACROS  */

extern int PDC_LEV_INFO;
extern int PDC_LEV_WARN;
extern int PDC_LEV_ERR;
extern int PDC_LEV_FATAL;
extern int PDC_LEV_MASK;

extern int PDC_FLG_PROMPT;
extern int PDC_FLG_SYSERR;
extern int PDC_FLG_LIBRARY;

int PDC_GET_LEV(int flags);
int PDC_GET_FLG(int flags);

void PDC_DBG(PDC_disc_t *t, char * fmt);
void PDC_DBG1(PDC_disc_t *t, char * fmt,...);
void PDC_DBG2(PDC_disc_t *t, char * fmt,...);
void PDC_DBG3(PDC_disc_t *t, char * fmt,...);
void PDC_DBG4(PDC_disc_t *t, char * fmt,...);
void PDC_DBG5(PDC_disc_t *t, char * fmt,...);

void PDC_TRACE(PDC_disc_t *t, char * fmt);
void PDC_TRACE1(PDC_disc_t *t, char * fmt,...);
void PDC_TRACE2(PDC_disc_t *t, char * fmt,...);
void PDC_TRACE3(PDC_disc_t *t, char * fmt,...);
void PDC_TRACE4(PDC_disc_t *t, char * fmt,...);
void PDC_TRACE5(PDC_disc_t *t, char * fmt,...);

void PDC_WARN(PDC_disc_t *t, char * fmt);
void PDC_WARN1(PDC_disc_t *t, char * fmt,...);
void PDC_WARN2(PDC_disc_t *t, char * fmt,...);
void PDC_WARN3(PDC_disc_t *t, char * fmt,...);
void PDC_WARN4(PDC_disc_t *t, char * fmt,...);
void PDC_WARN5(PDC_disc_t *t, char * fmt,...);

void PDC_SYSERR(PDC_disc_t *t, char * fmt);
void PDC_SYSERR1(PDC_disc_t *t, char * fmt,...);
void PDC_SYSERR2(PDC_disc_t *t, char * fmt,...);
void PDC_SYSERR3(PDC_disc_t *t, char * fmt,...);
void PDC_SYSERR4(PDC_disc_t *t, char * fmt,...);
void PDC_SYSERR5(PDC_disc_t *t, char * fmt,...);

void PDC_FATAL(PDC_disc_t *t, char * fmt);
void PDC_FATAL1(PDC_disc_t *t, char * fmt,...);
void PDC_FATAL2(PDC_disc_t *t, char * fmt,...);
void PDC_FATAL3(PDC_disc_t *t, char * fmt,...);
void PDC_FATAL4(PDC_disc_t *t, char * fmt,...);
void PDC_FATAL5(PDC_disc_t *t, char * fmt,...);

/* ================================================================================ */

#endif /*  __LIBPADSC_CKIT__  */
