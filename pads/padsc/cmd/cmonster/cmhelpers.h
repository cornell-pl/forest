#ifdef _USE_PROTO
#pragma prototyped
#endif
/*
 * Helpers
 *
 * Kathleen Fisher, Robert Gruber
 * AT&T Labs Research
 */

#ifndef __CMHELPERS_H__
#define __CMHELPERS_H__

#include "padsc-internal.h"
#include "cmdline.h"

void describe_params(CM_params *ps);
void describe_query(CM_query *q);
void describe_queries(CM_queries *qs);
void describe_c_cookie(CM_c_cookie *c);
void describe_arm(CM_arm *a);
void describe_s_cookie(CM_s_cookie *s);
void describe_cookie(CM_cookie *cspec);

#endif /*  !__CMHELPERS_H__  */
