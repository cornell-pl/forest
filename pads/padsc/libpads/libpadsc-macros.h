#pragma prototyped
/*
 * macros
 * 
 * Kathleen Fisher, Robert Gruber
 * AT&T Labs Research
 */

#ifndef __LIBPADSC_MACROS_H__
#define __LIBPADSC_MACROS_H__

#ifndef MacroArg2String
#define MacroArg2String(s) #s
#endif

#define PDC_DISC_INIT_CHECKS \
  do { \
    if (!pdc)  { return PDC_ERROR; } \
    if (!disc) { disc = pdc->disc; } \
  } while (0)

#endif  /*  __LIBPADSC_MACROS_H__  */
