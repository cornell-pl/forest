#ifdef _USE_PROTO
#pragma prototyped
#endif
/*
 * Helpers
 *
 * Kathleen Fisher, Robert Gruber
 * AT&T Labs Research
 */

#ifndef __HELPERS_H__
#define __HELPERS_H__

#include "padsc-internal.h"
#include "cmdline.h"

/* -------------------------------------------------------------------------------- */

void describe_params(CM_params *ps);
void describe_query(CM_query *q);
void describe_queries(CM_queries *qs);
void describe_c_cookie(CM_c_cookie *c);
void describe_arm(CM_arm *a);
void describe_s_cookie(CM_s_cookie *s);
void describe_cookie(CM_cspec *cspec);

size_t out_sz_c_cookie(CM_c_cookie *c);
size_t out_sz_s_cookie(CM_s_cookie *s);
size_t out_sz_cookie(CM_cspec *cspec);

PDC_error_t rw_c_cookie(CM_t *cm, CM_c_cookie *c, PDC_byte *begin, PDC_byte *end);
PDC_error_t rw_s_cookie(CM_t *cm, CM_s_cookie *c, PDC_byte *begin, PDC_byte *end);

/* -------------------------------------------------------------------------------- */

#define USAGE \
"\n  Usage: cmonster ispec cspec" \
"\n      where ispec specifies an IO discipline and its creation params" \
"\n      and cpsec is a cookie specification that selects one or more data items" \
"\n  " \
"\n  Use 'cmonster -h' for details" \
"\n"

#define DETAILED_USAGE \
"\n  Usage: cmonster ispec cspec" \
"\n      where ispec specifies an IO discipline and its creation params" \
"\n      and cpsec is a cookie specification that selects one or more data items" \
"\n  " \
"\n  -----" \
"\n  ispec" \
"\n  -----" \
"\n    An ispec specifies an IO discipline and its creation params." \
"\n    The set of supported IO disciplines (with example params) is as follows:" \
"\n  " \
"\n      fwrec(:0,24,1:)" \
"\n          Specifies a fixed width record with 0 'leader' bytes," \
"\n          24 data bytes, and 1 'trailer' byte (25 bytes total)." \
"\n  " \
"\n      fwrec_noseek(:0,24,1:)" \
"\n          Like fwrec, but the stream is assumed to not support seeking." \
"\n  " \
"\n      nlrec(:0:)" \
"\n          Specifies newline(ASCII)-terminated records." \
"\n          The argument is a block size hint (see below). " \
"\n  " \
"\n      nlrec_noseek(:0:)" \
"\n          Like nlrec, but the stream is assumed to not support seeking." \
"\n  " \
"\n      ctrec(:10,0:)" \
"\n          Specifies character(ASCII)-terminated records.  The first argument" \
"\n          is the term. character, the second is a block size hint (see below)." \
"\n          In this case character 10 is a newline, so this ispec is equivalent" \
"\n          to nlrec(:0:)" \
"\n  " \
"\n      ctrec_noseek(:10,0:)" \
"\n          Like ctrec, but the stream is assumed to not support seeking." \
"\n  " \
"\n      vlrec(:0,0:)" \
"\n          Specifies an IBM-style variable-length record format.  The first argument" \
"\n          specifies whether the records are grouped into blocks (i.e., whether the input" \
"\n          is variable-length blocks of variable-length records). 0 indicates non-blocked." \
"\n          The second argument is a hint as to the average record length." \
"\n          Use of 0 indicates 'no hint.'" \
"\n  " \
"\n      vlrec_noseek(:0,0:)" \
"\n          Like vlrec, but the stream is assumed to not support seeking." \
"\n  " \
"\n  " \
"\n    Block size hint: a block size hint suggests an appropriate block size for the IO" \
"\n    discipline to use 'under the covers' when doing IO.  Use of 0 indicates 'no hint.'" \
"\n    The hint may be ignored. " \
"\n  " \
"\n  -----" \
"\n  cspec" \
"\n  -----" \
"\n    Legal switch tags must fit in a PDC_int32, and the switch query must use" \
"\n    a character type (Pchar, Pa_char, Pe_char) or a numeric type whose in-memory" \
"\n    representation is PDC_int32 (Pint32_FW, Pa_int32_FW, Pe_int32_FW, Pb_int32," \
"\n    Pebc_int32, Pbcd_int32, Psbl_int32, Psbh_int32)." \
"\n  " \
"\n    TODO: describe cspec details" \
"\n  " \
"\n"

/* -------------------------------------------------------------------------------- */

#endif /*  !__HELPERS_H__  */
