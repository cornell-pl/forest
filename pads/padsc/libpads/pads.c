/* DEFGEN(libpadsc-gen.c) */
/*
 * library routines for library that goes with padsc
 *
 * Kathleen Fisher, Robert Gruber
 * AT&T Labs Research
 */

#gen_include "libpadsc-internal.h"
#gen_include "libpadsc-macros-gen.h"

static const char id[] = "\n@(#)$Id: pads.c,v 1.40 2002-10-21 16:17:43 gruber Exp $\0\n";

static const char lib[] = "padsc";

/* ================================================================================ */
/* MODIFIED CONVERSION ROUTINES */

long
PDC_strtol(const char* str, char** ptr, int base)
{
  errno = 0;
  return strtol(str, ptr, base);
}

long long
PDC_strtoll(const char* str, char** ptr, int base)
{
  errno = 0;
  return strtoll(str, ptr, base);
}

unsigned long
PDC_strtoul(const char* str, char** ptr, int base)
{
  const char* tmp = str;
  errno = 0;
  while (isspace(*tmp)) {
    tmp++;
  }
  if (*tmp++ == '-') {
    if (isdigit(*tmp)) { /* treat as range error */
      while (isdigit(*tmp)) {
	tmp++;
      }
      (*ptr) = (char*)tmp;
      errno = ERANGE;
      return 0;
    }
    (*ptr) = 0; /* indicates prefix error */
    return 0;
  }
  return strtoul(str, ptr, base);
}

unsigned long
PDC_strtoull(const char* str, char** ptr, int base)
{
  const char* tmp = str;
  errno = 0;
  while (isspace(*tmp)) {
    tmp++;
  }
  if (*tmp++ == '-') {
    if (isdigit(*tmp)) { /* treat as range error */
      while (isdigit(*tmp)) {
	tmp++;
      }
      (*ptr) = (char*)tmp;
      errno = ERANGE;
      return 0;
    }
    (*ptr) = 0; /* indicates prefix error */
    return 0;
  }
  return strtoull(str, ptr, base);
}

/* ********************* BEGIN_MACROS(libpadsc-macros-gen.h) ********************** */
/*
 * Some generally useful macros
 * 
 * Kathleen Fisher, Robert Gruber
 * AT&T Labs Research
 */

#ifndef MacroArg2String
#define MacroArg2String(s) #s
#endif
/* ********************************** END_HEADER ********************************** */
#define PDC_DISC_INIT_CHECKS
  do {
    if (!pdc)  { return PDC_ERROR; }
    if (!disc) { disc = pdc->disc; }
  } while (0)
/* END_MACRO */

/* set loc begin/end to current IO pos */
#define HANDLE_ERR_CURPOS(errcode)
  do {
    if (*em < PDC_Ignore) {
      ed->errCode = errcode;
      PDC_get_loc(pdc, &(ed->loc), disc);
    }
    return PDC_ERROR;
  } while (0)
/* END_MACRO */

/* set loc begin to current IO pos, end pos to end of current line */
#define HANDLE_ERR_CUR2ENDPOS(errcode)
  do {
    if (*em < PDC_Ignore) {
      ed->errCode = errcode;
      PDC_get_loc2end(pdc, &(ed->loc), disc);
    }
    return PDC_ERROR;
  } while (0)
/* END_MACRO */

/* set loc end to current IO pos */
#define HANDLE_ERR_ENDPOS(errcode)
  do {
    if (*em < PDC_Ignore) {
      ed->errCode = errcode;
      PDC_get_endLoc(pdc, &(ed->loc), disc);
    }
    return PDC_ERROR;
  } while (0)
/* END_MACRO */

/* move begin/end positions backwards by specified amounts */
#define HANDLE_ERR_MODPOS(errcode, adj1, adj2)
  do {
    if (*em < PDC_Ignore) {
      ed->errCode = errcode;
      PDC_get_loc(pdc, &(ed->loc), disc);
      ed->loc.beginChar -= (adj1);
      ed->loc.endChar   -= (adj2);
    }
    return PDC_ERROR;
  } while (0)
/* END_MACRO */

/*
 * Starting alloc size for strings, even if initial string is smaller;
 * saves on later alloc calls when PDC_string field is re-used many
 * times with strings of different lengths.
 */ 
#define PDC_STRING_HINT 128
/* END_MACRO */

/*
 * If *em is CheckAndSet, create a string copy of the string
 * that goes from begin to end-1.  Must have a no_space label.
 */
#define PDC_STR_COPY(s_out, begin, end)
  do {
    if (*em == PDC_CheckAndSet && s_out) {
      size_t wdth = end-begin; 
      char* buf;
      if (!s_out->rbuf) {
	if (!(s_out->rbuf = RMM_new_rbuf(pdc->rmm_nz))) {
	  goto no_space;
	}
      }
      if (RBuf_reserve(s_out->rbuf, (void**)&buf, sizeof(char), wdth+1, PDC_STRING_HINT)) {
       goto no_space;
      }
      memcpy(buf, begin, wdth);
      buf[wdth] = 0;
      s_out->str = buf;
      s_out->len = wdth;
    }
  } while (0)
/* END_MACRO */

/* Useful constants */

#define PDC_HALFMIN_INT64   -4611686018427387904LL
#define PDC_HALFMAX_INT64    4611686018427387903LL
#define PDC_HALFMAX_UINT64   9223372036854775807ULL
/* END_MACRO */

/* Fold Points : when should the running int64 / uint64 sum be folded into the average? */

#define PDC_FOLD_MIN_INT8    -9223372036854775680LL  /* PDC_MIN_INT64 - PDC_MIN_INT8  */
#define PDC_FOLD_MAX_INT8     9223372036854775680LL  /* PDC_MAX_INT64 - PDC_MAX_INT8  */
#define PDC_FOLD_MIN_INT16   -9223372036854743040LL  /* PDC_MIN_INT64 - PDC_MIN_INT16 */
#define PDC_FOLD_MAX_INT16    9223372036854743040LL  /* PDC_MAX_INT64 - PDC_MAX_INT16 */
#define PDC_FOLD_MIN_INT32   -9223372034707292160LL  /* PDC_MIN_INT64 - PDC_MIN_INT32 */
#define PDC_FOLD_MAX_INT32    9223372034707292160LL  /* PDC_MAX_INT64 - PDC_MAX_INT32 */

#define PDC_FOLD_MAX_UINT8   18446744073709551488ULL  /* PDC_MAX_UINT64 - PDC_MAX_UINT8  */
#define PDC_FOLD_MAX_UINT16  18446744073709518848ULL  /* PDC_MAX_UINT64 - PDC_MAX_UINT16 */
#define PDC_FOLD_MAX_UINT32  18446744069414584320ULL  /* PDC_MAX_UINT64 - PDC_MAX_UINT32 */
/* END_MACRO */

/* Macros that test whether folding should occur, given new val v and running sum s */

#define PDC_FOLDTEST_INT8(v, s)  (((s) < PDC_FOLD_MIN_INT8)  || ((s) > PDC_FOLD_MAX_INT8))
#define PDC_FOLDTEST_INT16(v, s) (((s) < PDC_FOLD_MIN_INT16) || ((s) > PDC_FOLD_MAX_INT16))
#define PDC_FOLDTEST_INT32(v, s) (((s) < PDC_FOLD_MIN_INT32) || ((s) > PDC_FOLD_MAX_INT32))
#define PDC_FOLDTEST_INT32(v, s) (((s) < PDC_FOLD_MIN_INT32) || ((s) > PDC_FOLD_MAX_INT32))
#define PDC_FOLDTEST_INT64(v, s) ( (((s) < 0) && ((v) < PDC_HALFMIN_INT64)) ||
				   (((v) < 0) && ((s) < PDC_HALFMIN_INT64)) ||
				   (((s) > 0) && ((v) > PDC_HALFMAX_INT64)) ||
				   (((v) > 0) && ((s) > PDC_HALFMAX_INT64)) )
#define PDC_FOLDTEST_UINT8(v, s)  ((s) > PDC_FOLD_MAX_UINT8)
#define PDC_FOLDTEST_UINT16(v, s) ((s) > PDC_FOLD_MAX_UINT16)
#define PDC_FOLDTEST_UINT32(v, s) ((s) > PDC_FOLD_MAX_UINT32)
#define PDC_FOLDTEST_UINT64(v, s) ( ((s) > PDC_HALFMAX_UINT64) || ((v) > PDC_HALFMAX_UINT64) )

/* ********************************* BEGIN_TRAILER ******************************** */
/* ********************************** END_MACROS ********************************** */

/* ****************** BEGIN_MACROS(libpadsc-read-macros-gen.h) ******************** */
/*
 * Macros that help implement read functions
 * 
 * Kathleen Fisher, Robert Gruber
 * AT&T Labs Research
 */

/* ********************************** END_HEADER ********************************** */
#define PDC_AINT_READ_FN(fn_name, targ_type, int_type, strtonum_fn, invalid_err, opt_tmp_test)
fn_name(PDC_t* pdc, PDC_base_em* em,
	PDC_base_ed* ed, targ_type* res_out, PDC_disc_t* disc)
{
  int_type        tmp;   /* tmp num */
  char*           tcp;   /* tmp char* */
  char*           begin; /* cursor at beginning of call */
  char*           end;   /* cursor just beyond EOR */
  PDC_base_em     emt = PDC_CheckAndSet;
  PDC_base_ed     edt;
  PDC_DISC_INIT_CHECKS;
  TRACE(pdc, MacroArg2String(fn_name) " called");
  if (!em) {
    em = &emt;
  }
  if (!ed) {
    ed = &edt;
  }
  if (PDC_ERROR == PDC_Internal_IO_needchars(pdc, 0, &begin, &end, disc)) { /* 0 means do not obey panicStop */
    goto at_eof_err; /* hit EOF */
  }
  if (!(disc->flags & PDC_WSPACE_OK) && isspace(*begin)) {
    goto bad_prefix_err;
  }
  tmp = strtonum_fn(begin, &tcp, 10);
  if (tcp==0 || tcp==begin) {
    goto bad_prefix_err;
  }
  PDC_IO_forward(pdc, tcp-begin, disc);
  if (errno==ERANGE opt_tmp_test) {
    goto range_err;
  }
  /* success */
  if (res_out && *em == PDC_CheckAndSet) {
    (*res_out) = (targ_type)tmp;
  }
  ed->errCode = PDC_NO_ERROR;
  return PDC_OK;

 at_eof_err:
  HANDLE_ERR_CURPOS(PDC_AT_EOF); 

 bad_prefix_err:
  HANDLE_ERR_CURPOS(invalid_err);

 range_err:
  HANDLE_ERR_MODPOS(PDC_RANGE, tcp-begin, 1); /* mod pos to start/end of number */
}
/* END_MACRO */

#define PDC_AINT_FW_READ_FN(fn_name, targ_type, int_type, strtonum_fn, invalid_err, opt_tmp_test)
PDC_error_t
fn_name(PDC_t* pdc, PDC_base_em* em, size_t width,
	PDC_base_ed* ed, targ_type* res_out, PDC_disc_t* disc)
{
  unsigned char   ct;    /* char tmp */
  int_type        tmp;   /* tmp num */
  char*           tcp;   /* tmp char* */
  char*           begin; /* cursor at beginning of call */
  char*           end;   /* cursor just beyond width chars */
  PDC_base_em     emt = PDC_CheckAndSet;
  PDC_base_ed     edt;

  PDC_DISC_INIT_CHECKS;
  TRACE(pdc, MacroArg2String(fn_name) " called");
  if (!em) {
    em = &emt;
  }
  if (!ed) {
    ed = &edt;
  }
  if (width <= 0) {
    WARN(pdc, "UNEXPECTED PARAM VALUE: " MacroArg2String(fn_name) " called with width <= 0");
    return PDC_ERROR; /* XXX mis-use of API -- unrecoverable/panic/advance cursor ??? */
  }
  /* ensure there are width chars available */
  if (PDC_ERROR == PDC_IO_getchars(pdc, width, &begin, &end, disc)) {
    goto width_not_avail;
  }
  if (!(disc->flags & PDC_WSPACE_OK) && isspace(*begin)) {
    goto wspace_err;
  }
  ct = *end;    /* save */
  *end = 0;     /* null */
  tmp = strtonum_fn(begin, &tcp, 10);
  *end = ct;    /* restore */
  if (tcp==0 || tcp==begin) {
    goto bad_prefix_err;
  }
  while (tcp < end && (disc->flags & PDC_WSPACE_OK) && isspace(*tcp)) {
    tcp++;
  }
  if (tcp != end) {
    goto bad_suffix_err;
  }
  if (errno==ERANGE opt_tmp_test) {
    goto range_err;
  }
  /* success */
  if (res_out && *em == PDC_CheckAndSet) {
    (*res_out) = (targ_type)tmp;
  }
  ed->errCode = PDC_NO_ERROR;
  return PDC_OK;

 width_not_avail:
  HANDLE_ERR_CUR2ENDPOS(PDC_WIDTH_NOT_AVAILABLE);

 wspace_err:
 bad_prefix_err:
 bad_suffix_err:
  HANDLE_ERR_MODPOS(invalid_err, width, 1); /* mod pos to start/end of number */

 range_err:
  HANDLE_ERR_MODPOS(PDC_RANGE, width, 1); /* mod pos to start/end of number */
}
/* END_MACRO */

#define PDC_BINT_READ_FN(fn_name, targ_type, width, swapmem_op)
PDC_error_t
fn_name(PDC_t* pdc, PDC_base_em* em,
	PDC_base_ed* ed, targ_type* res_out, PDC_disc_t* disc)
{
  char*           begin; /* cursor at beginning of call */
  PDC_base_em     emt = PDC_CheckAndSet;
  PDC_base_ed     edt;

  if (!disc) {
    disc = pdc->disc;
  }
  PDC_DISC_INIT_CHECKS;
  TRACE(pdc, MacroArg2String(fn_name) " called");
  if (!em) {
    em = &emt;
  }
  if (!ed) {
    ed = &edt;
  }
  /* ensure there are width chars available */
  if (PDC_ERROR == PDC_IO_getchars(pdc, width, &begin, 0, disc)) {
    goto width_not_avail;
  }
  /* success */
  if (res_out && *em == PDC_CheckAndSet) {
    if (disc->m_endian != disc->d_endian) {
      swapmem(swapmem_op, begin, res_out, width);
    } else {
      swapmem(0, begin, res_out, width);
    }
  }
  ed->errCode = PDC_NO_ERROR;
  return PDC_OK;

 width_not_avail:
  HANDLE_ERR_CUR2ENDPOS(PDC_WIDTH_NOT_AVAILABLE);
}
/* END_MACRO */

/* ********************************* BEGIN_TRAILER ******************************** */
/* ********************************** END_MACROS ********************************** */

/* ****************** BEGIN_MACROS(libpadsc-acc-macros-gen.h) ********************* */
/*
 * Macros that help implement accum functions
 * 
 * Kathleen Fisher, Robert Gruber
 * AT&T Labs Research
 */
/* ********************************** END_HEADER ********************************** */

#define PDC_INT_ACCUM(int_type, int_descr, num_bytes, fmt, fold_test)

typedef struct int_type ## _dt_key_s {
  int_type     val;
  PDC_uint64   cnt;
} int_type ## _dt_key_t;

typedef struct int_type ## _dt_elt_s {
  int_type ## _dt_key_t   key;
  Dtlink_t         link;
} int_type ## _dt_elt_t;

/*
 * Order set comparison function: only used at the end to rehash
 * the (formerly unordered) set.  Since same val only occurs
 * once, ptr equivalence produces key equivalence.
 *   different keys: sort keys by cnt field, break tie with vals
 */
int
int_type ## _dt_elt_oset_cmp(Dt_t* dt, int_type ## _dt_key_t* a, int_type ## _dt_key_t* b, Dtdisc_t* disc)
{
  NoP(dt);
  NoP(disc);
  if (a == b) { /* same key */
    return 0;
  }
  if (a->cnt == b->cnt) { /* same count, do val comparison */
    return (a->val < b->val) ? -1 : 1;
  }
  /* different counts */
  return (a->cnt > b->cnt) ? -1 : 1;
}

/*
 * Unordered set comparison function: all that matters is val equality
 * (0 => equal, 1 => not equal)
 */
int
int_type ## _dt_elt_set_cmp(Dt_t* dt, int_type ## _dt_key_t* a, int_type ## _dt_key_t* b, Dtdisc_t* disc)
{
  NoP(dt);
  NoP(disc);
  if (a->val == b->val) {
    return 0;
  }
  return 1;
}

void*
int_type ## _dt_elt_make(Dt_t* dt, int_type ## _dt_elt_t* a, Dtdisc_t* disc)
{
  int_type ## _dt_elt_t* b;
  if ((b = oldof(0, int_type ## _dt_elt_t, 1, 0))) {
    b->key.val  = a->key.val;
    b->key.cnt  = a->key.cnt;
  }
  return b;
}

void
int_type ## _dt_elt_free(Dt_t* dt, int_type ## _dt_elt_t* a, Dtdisc_t* disc)
{
  free(a);
}

static Dtdisc_t int_type ## _acc_dt_set_disc = {
  offsetof(int_type ## _dt_elt_t, key),     /* key     */
  num_bytes,                                /* size    */
  offsetof(int_type ## _dt_elt_t, link),    /* link    */
  (Dtmake_f)int_type ## _dt_elt_make,       /* makef   */
  (Dtfree_f)int_type ## _dt_elt_free,       /* freef   */
  (Dtcompar_f)int_type ## _dt_elt_set_cmp,  /* comparf */
  NiL,                                      /* hashf   */
  NiL,                                      /* memoryf */
  NiL                                       /* eventf  */
};

static Dtdisc_t int_type ## _acc_dt_oset_disc = {
  offsetof(int_type ## _dt_elt_t, key),     /* key     */
  num_bytes,                                /* size    */
  offsetof(int_type ## _dt_elt_t, link),    /* link    */
  (Dtmake_f)int_type ## _dt_elt_make,       /* makef   */
  (Dtfree_f)int_type ## _dt_elt_free,       /* freef   */
  (Dtcompar_f)int_type ## _dt_elt_oset_cmp, /* comparf */
  NiL,                                      /* hashf   */
  NiL,                                      /* memoryf */
  NiL                                       /* eventf  */
};

PDC_error_t
int_type ## _acc_init(PDC_t* pdc, int_type ## _acc* a, PDC_disc_t* disc)
{
  PDC_DISC_INIT_CHECKS;
  TRACE(pdc, MacroArg2String(int_type) "_acc_init called");
  if (!a) {
    return PDC_ERROR;
  }
  if (!(a->dict = dtopen(&int_type ## _acc_dt_set_disc, Dtset))) {
    return PDC_ERROR;
  }
  a->good = a->bad = a->fold = a->psum = a->avg = a->min = a->max = 0;
  return PDC_OK;
}

PDC_error_t
int_type ## _acc_reset(PDC_t* pdc, int_type ## _acc* a, PDC_disc_t* disc)
{
  PDC_DISC_INIT_CHECKS;
  TRACE(pdc, MacroArg2String(int_type) "_acc_reset called");
  if (!a || !a->dict) {
    return PDC_ERROR;
  }
  dtclear(a->dict);
  a->good = a->bad = a->fold = a->psum = a->avg = a->min = a->max = 0;
  return PDC_OK;
}

PDC_error_t
int_type ## _acc_cleanup(PDC_t* pdc, int_type ## _acc* a, PDC_disc_t* disc)
{
  PDC_DISC_INIT_CHECKS;
  TRACE(pdc, MacroArg2String(int_type) "_acc_cleanup called");
  if (!a) {
    return PDC_ERROR;
  }
  if (a->dict) {
    dtclose(a->dict);
    a->dict = 0;
  }
  return PDC_OK;
}

void
int_type ## _acc_fold_psum(int_type ## _acc* a) {
  double pavg, navg;
  PDC_uint64 recent = a->good - a->fold;
  if (recent == 0) {
    return;
  }
  pavg = a->psum / (double)recent;
  navg = ((a->avg * a->fold) + (pavg * recent))/(double)a->good;
  /* could test for change between a->avg and navg */
  a->avg = navg;
  a->psum = 0;
  a->fold += recent;
}

PDC_error_t
int_type ## _acc_add(PDC_t* pdc, int_type ## _acc* a, PDC_base_ed* ed, int_type* val, PDC_disc_t* disc)
{
  int_type               v          = (*val);
  int_type ## _dt_elt_t  insert_elt;
  int_type ## _dt_key_t  lookup_key;
  int_type ## _dt_elt_t* tmp1;
  PDC_DISC_INIT_CHECKS;
  TRACE(pdc, MacroArg2String(int_type) "_acc_add called");
  if (!a || !a->dict || !ed || !val) {
    return PDC_ERROR;
  }
  if (ed->errCode != 0) {
    (a->bad)++;
    return PDC_OK;
  }
  if (fold_test(v, a->psum)) {
    int_type ## _acc_fold_psum(a);
  }
  a->psum += v;
  (a->good)++;
  if (a->good == 1) {
    a->min = a->max = v;
  } else if (v < a->min) {
    a->min = v;
  } else if (v > a->max) {
    a->max = v;
  }
  if (dtsize(a->dict) < PDC_ACC_MAX2TRACK) {
    insert_elt.key.val = v;
    insert_elt.key.cnt = 0;
    if (!(tmp1 = dtinsert(a->dict, &insert_elt))) {
      WARN(pdc, "** PADC internal error: dtinsert failed (out of memory?) **");
      return PDC_ERROR;
    }
    (tmp1->key.cnt)++;
  } else {
    lookup_key.val = v;
    lookup_key.cnt = 0;
    if ((tmp1 = dtmatch(a->dict, (Void_t*)&lookup_key))) {
      (tmp1->key.cnt)++;
    }
  }
  return PDC_OK;
}

PDC_error_t
int_type ## _acc_report_internal(PDC_t* pdc, Sfio_t* outstr, const char* prefix, const char* what, int nst,
				 int_type ## _acc* a, PDC_disc_t* disc)
{
  const char*           cmt = ""; 
  int                   i = 0, sz, rp;
  PDC_uint64            cnt_sum = 0;
  double                bad_pcnt;
  double                cnt_sum_pcnt;
  double                elt_pcnt;
  Void_t*               velt;
  int_type ## _dt_elt_t* elt;

  if (!prefix || *prefix == 0) {
    prefix = "<top>";
  }
  if (!what) {
    what = int_descr;
  }
  PDC_nst_prefix_what(outstr, &nst, prefix, what);
  if (a->good == 0) {
    bad_pcnt = (a->bad == 0) ? 0.0 : 100.0;
  } else {
    bad_pcnt = a->bad / (double)(a->good + a->bad);
  }
  sfprintf(outstr, "good vals: %10llu    bad vals: %10llu    pcnt-bad: %8.3lf\n",
	   a->good, a->bad, bad_pcnt);
  if (a->good == 0) {
    return PDC_OK;
  }
  int_type ## _acc_fold_psum(a);
  sz = dtsize(a->dict);
  rp = (sz < PDC_ACC_REPORT_K) ? sz : PDC_ACC_REPORT_K;
  if (sz == PDC_ACC_MAX2TRACK) {
    cmt = " (* hit tracking limit *) ";
  }
  dtdisc(a->dict,   &int_type ## _acc_dt_oset_disc, DT_SAMEHASH); /* change cmp function */
  dtmethod(a->dict, Dtoset); /* change to ordered set -- establishes an ordering */
  sfprintf(outstr, "  Characterizing %s:  min %" fmt, what, a->min);
  sfprintf(outstr, " max %" fmt, a->max);
  sfprintf(outstr, " avg %lf\n", a->avg);

  sfprintf(outstr, "    => distribution of top %d values out of %d distinct values%s:\n",
	   rp, sz, cmt);
  for (velt = dtfirst(a->dict); velt && i < PDC_ACC_REPORT_K; velt = dtnext(a->dict, velt), i++) {
    elt = (int_type ## _dt_elt_t*)velt;
    cnt_sum += elt->key.cnt;
    elt_pcnt = ((double)100.0 * elt->key.cnt)/a->good;
    sfprintf(outstr, "        val: %10" fmt, elt->key.val);
    sfprintf(outstr, " count: %10llu  pcnt-of-good-vals: %8.3lf\n", elt->key.cnt, elt_pcnt);

  }
  cnt_sum_pcnt = ((double)100.0 * cnt_sum)/a->good;
  sfprintf(outstr,   ". . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .\n");
  sfprintf(outstr,   "        SUMMING         count: %10llu  pcnt-of-good-vals: %8.3lf\n",
	   cnt_sum, cnt_sum_pcnt);
  /* revert to unordered set in case more inserts will occur after this report */
  dtmethod(a->dict, Dtset); /* change to unordered set */
  dtdisc(a->dict,   &int_type ## _acc_dt_set_disc, DT_SAMEHASH); /* change cmp function */
  return PDC_OK;
}

PDC_error_t
int_type ## _acc_report(PDC_t* pdc, const char* prefix, const char* what, int nst,
			int_type ## _acc* a, PDC_disc_t* disc)
{
  Sfio_t *tmpstr;
  PDC_error_t res;
  PDC_DISC_INIT_CHECKS;
  TRACE(pdc, MacroArg2String(int_type) "_acc_report called");
  if (!a) {
    return PDC_ERROR;
  }
  if (!disc->errorf) {
    return PDC_OK;
  }
  if (!(tmpstr = sfstropen ())) { 
    return PDC_ERROR;
  }
  res = int_type ## _acc_report_internal(pdc, tmpstr, prefix, what, nst, a, disc);
  if (res == PDC_OK) {
    disc->errorf(pdc, disc, 0, "%s", sfstruse(tmpstr));
  }
  sfstrclose (tmpstr);
  return res;
}
/* END_MACRO */

#define PDC_INT_ACCUM_REPORT_MAP(int_type, int_descr, fmt)
PDC_error_t
int_type ## _acc_report_map_internal(PDC_t* pdc, Sfio_t* outstr, const char* prefix, const char* what,  int nst,
				     int_type ## _map_fn fn, int_type ## _acc* a, PDC_disc_t* disc)
{
  size_t                pad;
  const char*           mapped_min;
  const char*           mapped_max;
  const char*           mapped_val;
  const char*           cmt = ""; 
  int                   i, sz, rp;
  PDC_uint64            cnt_sum = 0;
  double                bad_pcnt;
  double                cnt_sum_pcnt;
  double                elt_pcnt;
  Void_t*               velt;
  int_type ## _dt_elt_t* elt;

  if (!prefix || *prefix == 0) {
    prefix = "<top>";
  }
  if (!what) {
    what = int_descr;
  }
  PDC_nst_prefix_what(outstr, &nst, prefix, what);
  if (a->good == 0) {
    bad_pcnt = (a->bad == 0) ? 0.0 : 100.0;
  } else {
    bad_pcnt = a->bad / (double)(a->good + a->bad);
  }
  sfprintf(outstr, "good vals: %10llu    bad vals: %10llu    pcnt-bad: %8.3lf\n",
	   a->good, a->bad, bad_pcnt);
  if (a->good == 0) {
    return PDC_OK;
  }
  int_type ## _acc_fold_psum(a);
  sz = dtsize(a->dict);
  rp = (sz < PDC_ACC_REPORT_K) ? sz : PDC_ACC_REPORT_K;
  if (sz == PDC_ACC_MAX2TRACK) {
    cmt = " (* hit tracking limit *) ";
  }
  dtdisc(a->dict,   &int_type ## _acc_dt_oset_disc, DT_SAMEHASH); /* change cmp function */
  dtmethod(a->dict, Dtoset); /* change to ordered set -- establishes an ordering */
  mapped_min = fn(a->min);
  mapped_max = fn(a->max);
  sfprintf(outstr, "  Characterizing %s:  min %" fmt, what, a->min);
  sfprintf(outstr, " (%s) max %" fmt, mapped_min, a->max);
  sfprintf(outstr, " (%s)\n", mapped_max);
  sfprintf(outstr, "    => distribution of top %d values out of %d distinct values%s:\n",
	   rp, sz, cmt);
  sz = rp = 0;
  for (i = 0, velt = dtfirst(a->dict); velt && i < PDC_ACC_REPORT_K; velt = dtnext(a->dict, velt), i++) {
    elt = (int_type ## _dt_elt_t*)velt;
    sz = strlen(fn(elt->key.val));
    if (sz > rp) {
      rp = sz; 
    }
  }
  for (i = 0, velt = dtfirst(a->dict); velt && i < PDC_ACC_REPORT_K; velt = dtnext(a->dict, velt), i++) {
    elt = (int_type ## _dt_elt_t*)velt;
    cnt_sum += elt->key.cnt;
    elt_pcnt = ((double)100.0 * elt->key.cnt)/a->good;
    mapped_val = fn(elt->key.val);
    sfprintf(outstr, "        val: %5" fmt, elt->key.val);
    sfprintf(outstr, " (%s) ", mapped_val);
    pad = rp-strlen(mapped_val);
    sfprintf(outstr, "%-.*s", pad,
	     "                                                                                ");
    sfprintf(outstr, "  count: %10llu  pcnt-of-good-vals: %8.3lf\n", elt->key.cnt, elt_pcnt);
  }
  cnt_sum_pcnt = ((double)100.0 * cnt_sum)/a->good;
  sfprintf(outstr,   "%-.*s", rp,
	   ". . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .");
  sfprintf(outstr,   " . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .\n        SUMMING");
  sfprintf(outstr,   "%-.*s", rp,
	   "                                                                                ");
  sfprintf(outstr,   "         count: %10llu  pcnt-of-good-vals: %8.3lf\n", cnt_sum, cnt_sum_pcnt);
  /* revert to unordered set in case more inserts will occur after this report */
  dtmethod(a->dict, Dtset); /* change to unordered set */
  dtdisc(a->dict,   &int_type ## _acc_dt_set_disc, DT_SAMEHASH); /* change cmp function */
  return PDC_OK;
}

PDC_error_t
int_type ## _acc_report_map(PDC_t* pdc, const char* prefix, const char* what, int nst,
			    int_type ## _map_fn fn, int_type ## _acc* a, PDC_disc_t* disc)
{
  Sfio_t *tmpstr;
  PDC_error_t res;
  PDC_DISC_INIT_CHECKS;
  TRACE(pdc, MacroArg2String(int_type) "_acc_mapped_report called");
  if (!a) {
    return PDC_ERROR;
  }
  if (!disc->errorf) {
    return PDC_OK;
  }
  if (!(tmpstr = sfstropen ())) { 
    return PDC_ERROR;
  }
  res = int_type ## _acc_report_map_internal(pdc, tmpstr, prefix, what, nst, fn, a, disc);
  if (res == PDC_OK) {
    disc->errorf(pdc, disc, 0, "%s", sfstruse(tmpstr));
  }
  sfstrclose (tmpstr);
  return res;
}
/* END_MACRO */

/* ********************************* BEGIN_TRAILER ******************************** */
/* ********************************** END_MACROS ********************************** */


/* ********************** BEGIN_MACGEN(libpadsc-read-gen.c) *********************** */
/*
 * Generated read functions
 * 
 * Kathleen Fisher, Robert Gruber
 * AT&T Labs Research
 */

#gen_include "libpadsc-internal.h"
#gen_include "libpadsc-macros-gen.h"
/* ********************************** END_HEADER ********************************** */
#gen_include "libpadsc-read-macros-gen.h"

/* ================================================================================ */
/* VARIABLE-WIDTH ASCII INTEGER READ FUNCTIONS */

/*
 * PDC_AINT_READ_FN(fn_name, targ_type, int_type, strtonum_fn, invalid_err, opt_tmp_test)
 */

PDC_AINT_READ_FN(PDC_aint8_read,  PDC_int8,  long,      PDC_strtol,  PDC_INVALID_AINT,
 || tmp < PDC_MIN_INT8  || tmp > PDC_MAX_INT8);

PDC_AINT_READ_FN(PDC_aint16_read, PDC_int16, long,      PDC_strtol,  PDC_INVALID_AINT,
 || tmp < PDC_MIN_INT16 || tmp > PDC_MAX_INT16);

PDC_AINT_READ_FN(PDC_aint32_read, PDC_int32, long,      PDC_strtol,  PDC_INVALID_AINT, );

PDC_AINT_READ_FN(PDC_aint64_read, PDC_int64, long long, PDC_strtoll, PDC_INVALID_AINT, );

PDC_AINT_READ_FN(PDC_auint8_read,  PDC_uint8,  unsigned long,      PDC_strtoul,  PDC_INVALID_AUINT,
 || tmp > PDC_MAX_UINT8);

PDC_AINT_READ_FN(PDC_auint16_read, PDC_uint16, unsigned long,      PDC_strtoul,  PDC_INVALID_AUINT,
 || tmp > PDC_MAX_UINT16);

PDC_AINT_READ_FN(PDC_auint32_read, PDC_uint32, unsigned long,      PDC_strtoul,  PDC_INVALID_AUINT, );

PDC_AINT_READ_FN(PDC_auint64_read, PDC_uint64, unsigned long long, PDC_strtoull, PDC_INVALID_AUINT, );

/* ================================================================================ */
/* FIXED-WIDTH ASCII INTEGER READ FUNCTIONS */

/*
 * PDC_AINT_FW_READ_FN(fn_name, targ_type, int_type, strtonum_fn, invalid_err, opt_tmp_test)
 */

PDC_AINT_FW_READ_FN(PDC_aint8_fw_read,  PDC_int8,  long,      PDC_strtol,  PDC_INVALID_AINT,
 || tmp < PDC_MIN_INT8  || tmp > PDC_MAX_INT8);

PDC_AINT_FW_READ_FN(PDC_aint16_fw_read, PDC_int16, long,      PDC_strtol,  PDC_INVALID_AINT,
 || tmp < PDC_MIN_INT16 || tmp > PDC_MAX_INT16);

PDC_AINT_FW_READ_FN(PDC_aint32_fw_read, PDC_int32, long,      PDC_strtol,  PDC_INVALID_AINT, );

PDC_AINT_FW_READ_FN(PDC_aint64_fw_read, PDC_int64, long long, PDC_strtoll, PDC_INVALID_AINT, );

PDC_AINT_FW_READ_FN(PDC_auint8_fw_read,  PDC_uint8,  unsigned long,      PDC_strtoul,  PDC_INVALID_AUINT,
 || tmp > PDC_MAX_UINT8);

PDC_AINT_FW_READ_FN(PDC_auint16_fw_read, PDC_uint16, unsigned long,      PDC_strtoul,  PDC_INVALID_AUINT,
 || tmp > PDC_MAX_UINT16);

PDC_AINT_FW_READ_FN(PDC_auint32_fw_read, PDC_uint32, unsigned long,      PDC_strtoul,  PDC_INVALID_AUINT, );

PDC_AINT_FW_READ_FN(PDC_auint64_fw_read, PDC_uint64, unsigned long long, PDC_strtoull, PDC_INVALID_AUINT, );

/* ================================================================================ */
/* BINARY INTEGER READ FUNCTIONS */

/*
 * PDC_BINT_READ_FN(fn_name, targ_type, width, swapmem_op)
 *
 * swapmem ops:
 *    0 -> straight copy
 *    1 -> reverse each byte in each string of 2 bytes
 *    3 -> reverse each byte in each string of 4 bytes
 *    4 -> swap upper/lower 4 bytes in each 8 byte value
 *    7 -> reverse each byte in each string of 8 bytes
 */

PDC_BINT_READ_FN(PDC_bint8_read,   PDC_int8,   1, 0);

PDC_BINT_READ_FN(PDC_buint8_read,  PDC_uint8,  1, 0);

PDC_BINT_READ_FN(PDC_bint16_read,  PDC_int16,  2, 1);

PDC_BINT_READ_FN(PDC_buint16_read, PDC_uint16, 2, 1);

PDC_BINT_READ_FN(PDC_bint32_read,  PDC_int32,  4, 3);

PDC_BINT_READ_FN(PDC_buint32_read, PDC_uint32, 4, 3);

PDC_BINT_READ_FN(PDC_bint64_read,  PDC_int64,  8, 7);

PDC_BINT_READ_FN(PDC_buint64_read, PDC_uint64, 8, 7);

/* ********************************* BEGIN_TRAILER ******************************** */
/* ********************************** END_MACGEN ********************************** */

/* ********************** BEGIN_MACGEN(libpadsc-acc-gen.c) ************************ */
/*
 * Generated accumulator functions
 * 
 * Kathleen Fisher, Robert Gruber
 * AT&T Labs Research
 */

#gen_include "libpadsc-internal.h"
#gen_include "libpadsc-macros-gen.h"

/* track up to 1000 values, report the top 10 */
#define PDC_ACC_MAX2TRACK      1000
#define PDC_ACC_REPORT_K         10

/* ********************************** END_HEADER ********************************** */
#gen_include "libpadsc-acc-macros-gen.h"

/* PDC_INT_ACCUM(int_type, int_descr, num_bytes, fmt, fold_test) */

PDC_INT_ACCUM(PDC_int8,   "int8",   1, "d",   PDC_FOLDTEST_INT8);

PDC_INT_ACCUM(PDC_uint8,  "uint8",  1, "u",   PDC_FOLDTEST_UINT8);

PDC_INT_ACCUM(PDC_int16,  "int16",  2, "d",   PDC_FOLDTEST_INT16);

PDC_INT_ACCUM(PDC_uint16, "uint16", 2, "u",   PDC_FOLDTEST_UINT16);

PDC_INT_ACCUM(PDC_int32,  "int32",  4, "ld",  PDC_FOLDTEST_INT32);

PDC_INT_ACCUM(PDC_uint32, "uint32", 4, "lu",  PDC_FOLDTEST_UINT32);

PDC_INT_ACCUM(PDC_int64,  "int64",  8, "lld", PDC_FOLDTEST_INT64);

PDC_INT_ACCUM(PDC_uint64, "uint64", 8, "llu", PDC_FOLDTEST_UINT64);


/* PDC_INT_ACCUM_REPORT_MAP(int_type, int_descr, fmt) */

PDC_INT_ACCUM_REPORT_MAP(PDC_int32, "int32", "ld");


/* ********************************* BEGIN_TRAILER ******************************** */

typedef struct PDC_string_dt_key_s {
  PDC_uint64  cnt;
  size_t      len;
  char*       str;
} PDC_string_dt_key_t;

typedef struct PDC_string_dt_elt_s {
  PDC_string_dt_key_t  key;
  Dtlink_t             link;
  char                 buf[1];
} PDC_string_dt_elt_t;

unsigned int
PDC_string_dt_elt_hash(Dt_t* dt, Void_t* key, Dtdisc_t* disc)
{
  PDC_string_dt_key_t* k = (PDC_string_dt_key_t*)key;
  NoP(dt);
  NoP(disc);
  return dtstrhash(0, k->str, k->len);
}

/*
 * Order set comparison function: only used at the end to rehash
 * the (formerly unordered) set.  Since same string only occurs
 * once, ptr equivalence produces key equivalence.
 *   different keys: sort keys by cnt field, break tie with string vals
 */
int
PDC_string_dt_elt_oset_cmp(Dt_t* dt, PDC_string_dt_key_t* a, PDC_string_dt_key_t* b, Dtdisc_t* disc)
{
  size_t min_len;
  int    res;
  NoP(dt);
  NoP(disc);
  if (a == b) { /* same key */
    return 0;
  }
  if (a->cnt == b->cnt) { /* same count, so do lexicographic comparison */
    min_len = (a->len < b->len) ? a->len : b->len;
    if ((res = strncmp(a->str, b->str, min_len))) {
      return res;
    }
    return (a->len < b->len) ? -1 : 1;
  }
  /* different counts */
  return (a->cnt > b->cnt) ? -1 : 1;
}

/*
 * Unordered set comparison function: all that matters is string equality
 * (0 => equal, 1 => not equal)
 */
int
PDC_string_dt_elt_set_cmp(Dt_t* dt, PDC_string_dt_key_t* a, PDC_string_dt_key_t* b, Dtdisc_t* disc)
{
  NoP(dt);
  NoP(disc);
  if (a->len == b->len && strncmp(a->str, b->str, a->len) == 0) {
    return 0;
  }
  return 1;
}

void*
PDC_string_dt_elt_make(Dt_t* dt, PDC_string_dt_elt_t* a, Dtdisc_t* disc)
{
  PDC_string_dt_elt_t* b;
  NoP(dt);
  NoP(disc);
  if ((b = oldof(0, PDC_string_dt_elt_t, 1, a->key.len))) {
    memcpy(b->buf, a->key.str, a->key.len);
    b->key.cnt = a->key.cnt;
    b->key.len = a->key.len;
    b->key.str = b->buf;
  }
  return b;
}

void
PDC_string_dt_elt_free(Dt_t* dt, PDC_string_dt_elt_t* a, Dtdisc_t* disc)
{
  free(a);
}

static Dtdisc_t PDC_string_acc_dt_set_disc = {
  offsetof(PDC_string_dt_elt_t, key),     /* key     */
  0,				          /* size    */
  offsetof(PDC_string_dt_elt_t, link),    /* link    */
  (Dtmake_f)PDC_string_dt_elt_make,       /* makef   */
  (Dtfree_f)PDC_string_dt_elt_free,       /* freef */
  (Dtcompar_f)PDC_string_dt_elt_set_cmp,  /* comparf */
  (Dthash_f)PDC_string_dt_elt_hash,       /* hashf   */
  NiL,				          /* memoryf */
  NiL				          /* eventf  */
};

static Dtdisc_t PDC_string_acc_dt_oset_disc = {
  offsetof(PDC_string_dt_elt_t, key),     /* key     */
  0,				          /* size    */
  offsetof(PDC_string_dt_elt_t, link),    /* link    */
  (Dtmake_f)PDC_string_dt_elt_make,       /* makef   */
  (Dtfree_f)PDC_string_dt_elt_free,       /* freef */
  (Dtcompar_f)PDC_string_dt_elt_oset_cmp, /* comparf */
  (Dthash_f)PDC_string_dt_elt_hash,       /* hashf   */
  NiL,				          /* memoryf */
  NiL				          /* eventf  */
};

PDC_error_t
PDC_string_acc_init(PDC_t* pdc, PDC_string_acc* a, PDC_disc_t* disc)
{
  PDC_DISC_INIT_CHECKS;
  TRACE(pdc, "PDC_string_acc_init called");
  if (!a) {
    return PDC_ERROR;
  }
  if (!(a->dict = dtopen(&PDC_string_acc_dt_set_disc, Dtset))) {
    return PDC_ERROR;
  }
  return PDC_uint32_acc_init(pdc, &(a->len_accum), disc);
}

PDC_error_t
PDC_string_acc_reset(PDC_t* pdc, PDC_string_acc* a, PDC_disc_t* disc)
{
  PDC_DISC_INIT_CHECKS;
  TRACE(pdc, "PDC_string_acc_reset called");
  if (!a || !a->dict) {
    return PDC_ERROR;
  }
  dtclear(a->dict);
  return PDC_uint32_acc_reset(pdc, &(a->len_accum), disc);
}

PDC_error_t
PDC_string_acc_cleanup(PDC_t* pdc, PDC_string_acc* a, PDC_disc_t* disc)
{
  PDC_DISC_INIT_CHECKS;
  TRACE(pdc, "PDC_string_acc_cleanup called");
  if (!a) {
    return PDC_ERROR;
  }
  if (a->dict) {
    dtclose(a->dict);
    a->dict = 0;
  }
  return PDC_uint32_acc_cleanup(pdc, &(a->len_accum), disc);
}

PDC_error_t
PDC_string_acc_add(PDC_t* pdc, PDC_string_acc* a, PDC_base_ed* ed, PDC_string* val, PDC_disc_t* disc)
{
  PDC_string_dt_elt_t insert_elt;
  PDC_string_dt_key_t lookup_key;
  PDC_string_dt_elt_t* tmp1;
  PDC_DISC_INIT_CHECKS;
  TRACE(pdc, "PDC_string_acc_add called");
  if (!a || !a->dict || !ed || !val) {
    return PDC_ERROR;
  }
  if (PDC_ERROR == PDC_uint32_acc_add(pdc, &(a->len_accum), ed, &(val->len), disc)) {
    return PDC_ERROR;
  }
  if (ed->errCode != 0) {
    return PDC_OK;
  }
  if (dtsize(a->dict) < PDC_ACC_MAX2TRACK) {
    insert_elt.key.str = val->str;
    insert_elt.key.len = val->len;
    insert_elt.key.cnt = 0;
    if (!(tmp1 = dtinsert(a->dict, &insert_elt))) {
      WARN(pdc, "** PADC internal error: dtinsert failed (out of memory?) **");
      return PDC_ERROR;
    }
    (tmp1->key.cnt)++;
  }
  else {
    lookup_key.str = val->str;
    lookup_key.len = val->len;
    lookup_key.cnt = 0;
    if ((tmp1 = dtmatch(a->dict, (Void_t*)&lookup_key))) {
      (tmp1->key.cnt)++;
    }
  }
  return PDC_OK;
}

PDC_error_t
PDC_string_acc_report_internal(PDC_t* pdc, Sfio_t* outstr, const char* prefix, const char* what, int nst,
			       PDC_string_acc* a, PDC_disc_t* disc)
{
  size_t                pad;
  const char*           cmt = ""; 
  int                   i = 0, sz, rp;
  PDC_uint64            cnt_sum = 0;
  double                cnt_sum_pcnt;
  double                elt_pcnt;
  Void_t*               velt;
  PDC_string_dt_elt_t*  elt;

  if (!prefix || *prefix == 0) {
    prefix = "<top>";
  }
  if (!what) {
    what = "string";
  }
  PDC_nst_prefix_what(outstr, &nst, prefix, what);
  if (PDC_ERROR == PDC_uint32_acc_report_internal(pdc, outstr, "String lengths", "lengths", -1, &(a->len_accum), disc)) {
    return PDC_ERROR;
  }
  if (a->len_accum.good == 0) {
    return PDC_OK;
  }
  /* rehash tree to get keys ordered by count */
  sz = dtsize(a->dict);
  rp = (sz < PDC_ACC_REPORT_K) ? sz : PDC_ACC_REPORT_K;
  if (sz == PDC_ACC_MAX2TRACK) {
    cmt = " (* hit tracking limit *) ";
  }
  dtdisc(a->dict, &PDC_string_acc_dt_oset_disc, DT_SAMEHASH); /* change cmp function */
  dtmethod(a->dict, Dtoset); /* change to ordered set -- establishes an ordering */
  sfprintf(outstr, "\n  Characterizing strings:\n");
  sfprintf(outstr, "    => distribution of top %d strings out of %d distinct strings%s:\n",
	   rp, sz, cmt);
  for (velt = dtfirst(a->dict); velt && i < PDC_ACC_REPORT_K; velt = dtnext(a->dict, velt), i++) {
    elt = (PDC_string_dt_elt_t*)velt;
    cnt_sum += elt->key.cnt;
    elt_pcnt = ((double)100.0 * elt->key.cnt)/a->len_accum.good;
    sfprintf(outstr, "        val: [");
    sfprintf(outstr, "%-.*s", elt->key.len, elt->key.str);
    sfprintf(outstr, "]");
    pad = a->len_accum.max - elt->key.len;
    sfprintf(outstr, "%-.*s", pad,
	     "                                                                                ");
    sfprintf(outstr, " count: %10llu  pcnt-of-good-vals: %8.3lf\n", elt->key.cnt, elt_pcnt);
  }
  cnt_sum_pcnt = ((double)100.0 * cnt_sum)/a->len_accum.good;
  sfprintf(outstr, ". . . . . . . .");
  pad = a->len_accum.max;
  sfprintf(outstr, "%-.*s", pad,
	   " . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .");
  sfprintf(outstr, " . . . . . . . . . . . . . . . . . . . . . . .\n");

  sfprintf(outstr, "        SUMMING");
  sfprintf(outstr, "%-.*s", pad,
	   "                                                                                ");
  sfprintf(outstr, " count: %10llu  pcnt-of-good-vals: %8.3lf\n", cnt_sum, cnt_sum_pcnt);
  /* revert to unordered set in case more inserts will occur after this report */
  dtmethod(a->dict, Dtset); /* change to unordered set */
  dtdisc(a->dict, &PDC_string_acc_dt_set_disc, DT_SAMEHASH); /* change cmp function */
  return PDC_OK;
};

PDC_error_t
PDC_string_acc_report(PDC_t* pdc, const char* prefix, const char* what, int nst, PDC_string_acc* a, PDC_disc_t* disc)
{
  Sfio_t *tmpstr;
  PDC_error_t res;
  PDC_DISC_INIT_CHECKS;
  TRACE(pdc, "PDC_string_acc_report called");
  if (!a) {
    return PDC_ERROR;
  }
  if (!disc->errorf) {
    return PDC_OK;
  }
  if (!(tmpstr = sfstropen ())) { 
    return PDC_ERROR;
  }
  res = PDC_string_acc_report_internal(pdc, tmpstr, prefix, what, nst, a, disc);
  if (res == PDC_OK) {
    disc->errorf(pdc, disc, 0, "%s", sfstruse(tmpstr));
  }
  sfstrclose (tmpstr);
  return res;
}

static const char* PDC_hdr_lines[] = {
  "*****************************************************************************************************\n",
  "+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n",
  "=====================================================================================================\n",
  "-----------------------------------------------------------------------------------------------------\n",
  ".....................................................................................................\n",
  "* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\n",
  "+ + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + +\n",
  "= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =\n",
  "- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -\n"
};

void
PDC_nst_prefix_what(Sfio_t* outstr, int* nst, const char* prefix, const char* what)
{
  if (prefix) {
    if ((*nst) >= 0) {
      int idx = (*nst) % 9;
      sfprintf(outstr, "\n%s", PDC_hdr_lines[idx]);
      sfprintf(outstr, "%s : %s\n", prefix, what);
      sfprintf(outstr, "%s", PDC_hdr_lines[idx]);
      (*nst)++;
    } else {
      sfprintf(outstr, "%s: ", prefix);
    }
  }
}

/* ********************************** END_MACGEN ********************************** */

/* ================================================================================ */ 
/* IMPL CONSTANTS */

#define PDC_initStkElts      8
#define PDC_initInpBufs      8
#define PDC_initBufSize   1024

/* ================================================================================ */ 
/* ERROR REPORTING FUNCTIONS */

int
PDC_errorf(PDC_t* pdc, PDC_disc_t* disc, int level, ...)
{
  va_list ap;
  va_start(ap, level);
  if (!disc) {
    disc = pdc->disc;
  }
  errorv((disc && pdc) ? *((char**)pdc) : (char*)pdc, (disc || level < 0) ? level : (level | ERROR_LIBRARY), ap);
  va_end(ap);
  return 0;
}

PDC_error_t
PDC_report_err(PDC_t* pdc, PDC_disc_t* disc, int level, PDC_loc_t* loc,
	       PDC_errCode_t errCode, const char* format, ...)
{
  PDC_error_f errorf;
  char*   severity = "error";
  char*   tmpstr1;
  char*   tmpstr2;

  PDC_DISC_INIT_CHECKS;
  TRACE(pdc, "PDC_report_err called");
  errorf = disc->errorf;
  if (level & ERROR_FATAL) {
    severity = "FATAL error";
    if (!errorf) { /* need an error function anyway for fatal case */
      errorf = PDC_errorf;
    }
  } else if (pdc->speclev > 0 || disc->e_rep == PDC_errorRep_None || !errorf) {
    return PDC_OK;
  }
  if (disc->e_rep == PDC_errorRep_Min) {
    if (loc) {
      errorf(pdc, disc, level, "%s at line %d char %d : errCode %d", severity, loc->beginLine, loc->beginChar, errCode);
    } else {
      errorf(pdc, disc, level, "%s : errCode %d", severity, errCode);
    }
    return PDC_OK;
  }
  sfstrset(pdc->tmp, 0);
  if (format && strlen(format)) {
    va_list ap;
    if (loc) {
      sfprintf(pdc->tmp, "%s at line %d char %d : ", severity, loc->beginLine, loc->beginChar);
    } else {
      sfprintf(pdc->tmp, "%s : ", severity);
    }
    va_start(ap, format);
    sfvprintf(pdc->tmp, format, ap);
    va_end(ap);
  } else {
    const char* msg = "** internal error **";

    switch (errCode) {
    case PDC_NO_ERROR:
      msg = "(errcode indicates no error)";
      break;
    case PDC_OUT_OF_MEMORY:
      msg = "Failed to allocate memory";
      break;
    case PDC_SYS_ERROR:
      msg = "System error";
      break;
    case PDC_INTERNAL_ERROR:
      msg = "Internal error";
      break;
    case PDC_CHKPOINT_FAILURE:
      msg = "Checkpoint failure (misuse of libpadsc IO checkpoint facility)";
      break;
    case PDC_COMMIT_FAILURE:
      msg = "Commit failure (misuse of libpadsc IO checkpoint facility)";
      break;
    case PDC_RESTORE_FAILURE:
      msg = "Restore failure (misuse of libpadsc IO checkpoint facility)";
      break;
    case PDC_ALLOC_FAILURE:
      msg = "Memory alloc failure (out of space)";
      break;
    case PDC_PANIC_SKIPPED:
      msg = "Data element parsing skipped: in panic mode due to earlier error(s)";
      break;
    case PDC_USER_CONSTRAINT_VIOLATION:
      msg = "User constraint violation";
      break;
    case PDC_MISSING_LITERAL:
      msg = "Missing literal";
      break;
    case PDC_ARRAY_ELEM_ERR:
      msg = "Array element error";
      break;
    case PDC_ARRAY_SEP_ERR:
      msg = "Arrey seperator error";
      break;
    case PDC_ARRAY_TERM_ERR:
      msg = "Arrey terminator error";
      break;
    case PDC_ARRAY_SIZE_ERR:
      msg = "Array size error";
      break;
    case PDC_ARRAY_USER_CONSTRAINT_ERR:
      msg = "Array user constraint violation";
      break;
    case PDC_ARRAY_MIN_BIGGER_THAN_MAX_ERR:
      msg = "Array min bigger than array max";
      break;
    case PDC_ARRAY_MIN_NEGATIVE:
      msg = "Negative number used for array min";
      break;
    case PDC_ARRAY_MAX_NEGATIVE:
      msg = "Negative number used for array max";
      break;
    case PDC_ARRAY_EXTRA_BEFORE_SEP:
      msg = "Unexpected extra data before array element separator";
      break;
    case PDC_ARRAY_EXTRA_BEFORE_TERM:
      msg = "Unexpected extra data before array element terminator";
      break;
    case PDC_STRUCT_FIELD_ERR:
      msg = "Structure field error";
      break;
    case PDC_UNION_MATCH_FAILURE:
      msg = "Union match failure";
      break;
    case PDC_ENUM_MATCH_FAILURE:
      msg = "Enum match failure";
      break;
    case PDC_TYPEDEF_CONSTRAINT_ERR:
      msg = "Typedef constraint error";
      break;
    case PDC_AT_EOF:
      msg = "Unexpected end of file (field too short?) failure";
      break;
    case PDC_AT_EOR:
      msg = "Unexpected end of record (field too short?) failure";
      break;
    case PDC_RANGE:
      msg = "Number out of range error";
      break;
    case PDC_INVALID_AINT:
      msg = "Invalid ascii integer";
      break;
    case PDC_INVALID_AUINT:
      msg = "Invalid ascii unsigned integer";
      break;
    case PDC_INVALID_BINT:
      msg = "Invalid binary integer";
      break;
    case PDC_INVALID_BUINT:
      msg = "Invalid binary unsigned integer";
      break;
    case PDC_CHAR_LIT_NOT_FOUND:
      msg = "Expected character literal not found";
      break;
    case PDC_STR_LIT_NOT_FOUND:
      msg = "Expected string literal not found";
      break;
    case PDC_REGEXP_NOT_FOUND:
      msg = "Match for regular expression not found";
      break;
    case PDC_INVALID_REGEXP:
      msg = "Invalid regular expression";
      break;
    case PDC_WIDTH_NOT_AVAILABLE:
      msg = "Specified width not available (EOR/EOF encountered)";
      break;
    default:
      sfprintf(pdc->tmp, "*** unknown error code: %d ***", errCode);
      msg = "";
      break;
    }
    if (loc) {
      sfprintf(pdc->tmp, "%s at line %d char %d : %s ", severity, loc->beginLine, loc->beginChar, msg);
    } else {
      sfprintf(pdc->tmp, "%s : %s ", severity, msg);
    }
  }
  if (loc && (disc->e_rep == PDC_errorRep_Max)) {
    char* buf;
    size_t len;
    if (PDC_OK == PDC_IO_getLineBuf(pdc, loc->endLine, &buf, &len, disc)) {
      size_t minc = (loc->beginLine == loc->endLine) ? loc->beginChar : 1;
      size_t maxc = loc->endChar;
      if (buf[len-1] == '\n') {
	len--;
      }
      if (len <= 0) {
	sfprintf(pdc->tmp, "\n[LINE %d](**EMPTY**)", loc->endLine);
      } else {
	if (maxc > len) {
	  maxc = len;
	}
	if (minc > maxc) {
	  minc = maxc;
	}
	if (maxc > 0) {
	  if (minc < maxc) {
	    tmpstr1 = PDC_fmtStrL(buf,minc-1);
	    tmpstr2 = PDC_fmtStrL(buf+minc-1,maxc-minc+1);
	    sfprintf(pdc->tmp, "\n[LINE %d]%s>>>%s<<<", loc->endLine, tmpstr1, tmpstr2);
	  } else {
	    tmpstr1 = PDC_fmtStrL(buf, maxc);
	    sfprintf(pdc->tmp, "\n[LINE %d]%s<<<", loc->endLine, tmpstr1);
	  }
	}
      }
    }
  }
  errorf(pdc, disc, level, "%s", sfstruse(pdc->tmp));
  return PDC_OK;
}

/* ================================================================================ */
/* SCAN FUNCTIONS */

PDC_error_t
PDC_char_lit_scan(PDC_t* pdc, unsigned char c, unsigned char s,
		  unsigned char* c_out, size_t* offset_out,
		  PDC_disc_t* disc)
{
  unsigned char ct;

  PDC_DISC_INIT_CHECKS;
  TRACE1(pdc, "PDC_char_lit_scan called for char = %s", PDC_fmtChar(c));
  if (offset_out) {
    (*offset_out) = 0;
  }
  if (PDC_ERROR == PDC_IO_checkpoint(pdc, 0, disc)) {
    return PDC_ERROR; /* XXX out of space -- unrecoverable error */
  }
  while (PDC_OK == PDC_IO_getchar(pdc, 1, &ct, disc)) { /* 1 means obey panicStop */
    if ((c == ct) || (s == ct)) {
      if (c_out) {
	(*c_out) = ct;
      }
      if (PDC_ERROR == PDC_IO_commit(pdc, disc)) {
	return PDC_ERROR; /* XXX internal error -- unrecoverable error */
      }
      return PDC_OK;  /* IO cursor is one beyond c/s */
    }
    if (offset_out) {
      (*offset_out)++;
    }
  }
  /* restore IO cursor to original position and return error */
  if (PDC_ERROR == PDC_IO_restore(pdc, disc)) {
    /* XXX unrecoverable error -- should call discipline unrecov error handler */
  }
  return PDC_ERROR;
}

/* XXX_TODO currently only examines one line */
PDC_error_t
PDC_str_lit_scan(PDC_t* pdc, const PDC_string* findStr, const PDC_string* stopStr,
		 PDC_string** str_out, size_t* offset_out, PDC_disc_t* disc) 
{
  char            *begin, *end, *ptr;
  size_t          remain;

  PDC_DISC_INIT_CHECKS;
  TRACE2(pdc, "PDC_str_lit_scan called for findStr = %s stopStre = %s",
	 PDC_fmtStr(findStr), PDC_fmtStr(stopStr));
  if (offset_out) {
    (*offset_out) = 0;
  }
  if (!findStr || findStr->len == 0) {
    WARN(pdc, "PDC_str_lit_scan : null/empty findStr specified");
    return PDC_ERROR;
  }
  if (PDC_ERROR == PDC_IO_checkpoint(pdc, 0, disc)) {
    return PDC_ERROR; /* XXX out of space -- unrecoverable error */
  }
  if (PDC_ERROR == PDC_Internal_IO_needchars(pdc, 0, &begin, &end, disc)) { /* 0 means do not obey panicStop */
    goto not_found; /* hit EOF */
  }
  for (ptr = begin, remain=end-begin; ptr < end; ptr++, remain--) {
    if (remain < findStr->len) {
      break;
    }
    if (strncmp(findStr->str,ptr,findStr->len) == 0) {
      if (str_out) {
	(*str_out) = (PDC_string*)findStr;
      }
      PDC_IO_forward(pdc, findStr->len, disc); /* XXX len - 1 ??? XXX */
      if (PDC_ERROR == PDC_IO_commit(pdc, disc)) {
	return PDC_ERROR; /* XXX internal error -- unrecoverable error */
      }
      return PDC_OK; /* IO cursor one beyond findStr */
    }
    if (stopStr && (remain >= stopStr->len) &&
	(strncmp(stopStr->str,ptr,stopStr->len) == 0)) {
      if (str_out) {
	(*str_out) = (PDC_string*)stopStr;
      }
      PDC_IO_forward(pdc, stopStr->len, disc); /* XXX len - 1 ??? XXX */
      if (PDC_ERROR == PDC_IO_commit(pdc, disc)) {
	return PDC_ERROR; /* XXX internal error -- unrecoverable error */
      }
      return PDC_OK; /* IO cursor one beyond findStr */
    }
    if (offset_out) {
      (*offset_out)++;
    }
  }

 not_found:
  if (PDC_ERROR == PDC_IO_restore(pdc, disc)) {
    /* XXX unrecoverable error -- should call discipline unrecov error handler */
  }
  return PDC_ERROR;
}

/* ================================================================================ */
/* LITERAL READ FUNCTIONS */

PDC_error_t
PDC_char_lit_read(PDC_t* pdc, PDC_base_em* em,
		  PDC_base_ed* ed, unsigned char c, PDC_disc_t* disc)
{
  unsigned char   ct;
  PDC_base_em     emt = PDC_CheckAndSet;
  PDC_base_ed     edt;

  PDC_DISC_INIT_CHECKS;
  TRACE1(pdc, "PDC_char_lit_read called for char = %s", PDC_fmtChar(c));
  if (!em) {
    em = &emt;
  }
  if (!ed) {
    ed = &edt;
  }
  if (PDC_OK == PDC_IO_getchar(pdc, 0, &ct, disc)) { /* 0 means do not obey panicStop */
    if ((c == ct) || (*em == PDC_Ignore)) {
      ed->errCode = PDC_NO_ERROR;
      return PDC_OK;  /* IO cursor is one beyond c */
    }
    /* wrong char -- put it back */
    PDC_IO_back(pdc, 1, disc);
  }
  HANDLE_ERR_CURPOS(PDC_CHAR_LIT_NOT_FOUND);
}

PDC_error_t
PDC_str_lit_read(PDC_t* pdc, PDC_base_em* em,
		 PDC_base_ed* ed, const PDC_string* s, PDC_disc_t* disc)
{
  char            *begin, *end;
  PDC_base_em     emt = PDC_CheckAndSet;
  PDC_base_ed     edt;

  PDC_DISC_INIT_CHECKS;
  TRACE1(pdc, "PDC_str_lit_read called for str = %s", PDC_fmtStr(s));
  if (!em) {
    em = &emt;
  }
  if (!ed) {
    ed = &edt;
  }
  if (s->len <= 0) {
    WARN(pdc, "UNEXPECTED PARAM VALUE: PDC_str_lit_read called with s->len <= 0");
    goto not_found;
  }
  if (PDC_ERROR == PDC_IO_getchars(pdc, s->len, &begin, &end, disc)) {
    goto not_found;
  }
  if ((*em == PDC_Ignore) || (strncmp(begin, s->str, s->len) == 0)) {
    ed->errCode = PDC_NO_ERROR;
    return PDC_OK;    /* found it */
  }
  /* string did not match */
  PDC_IO_back(pdc, s->len, disc);

 not_found:
  HANDLE_ERR_CURPOS(PDC_STR_LIT_NOT_FOUND);
}

/* ================================================================================ */
/* DATE/TIME READ FUNCTIONS */

PDC_error_t
PDC_adate_read (PDC_t* pdc, PDC_base_em* em, PDC_base_ed* ed, 
		PDC_uint32* res_out, PDC_disc_t* disc)
{
  /* TODO */
  return PDC_ERROR;
}


/* ================================================================================ */
/* STRING READ FUNCTIONS */

/* related helper functions */

PDC_error_t
PDC_string_ed_init(PDC_t* pdc, PDC_string_ed* ed, PDC_disc_t* disc)
{
  PDC_DISC_INIT_CHECKS;
  return PDC_OK;
}

PDC_error_t
PDC_string_ed_cleanup(PDC_t* pdc, PDC_string_ed* ed, PDC_disc_t* disc)
{
  PDC_DISC_INIT_CHECKS;
  return PDC_OK;
}

PDC_error_t
PDC_string_init(PDC_t* pdc, PDC_string* s, PDC_disc_t* disc)
{
  if (!s) {
    return PDC_ERROR;
  }
  bzero(s, sizeof(PDC_string));
  return PDC_OK;
}

PDC_error_t
PDC_string_cleanup(PDC_t* pdc, PDC_string* s, PDC_disc_t* disc)
{
  if (!s) {
    return PDC_ERROR;
  }
  RMM_free_rbuf(s->rbuf);
  return PDC_OK;
}

PDC_error_t
PDC_string_copy(PDC_t* pdc, PDC_string* targ, const char* src, size_t len, PDC_disc_t* disc)
{
  PDC_DISC_INIT_CHECKS;
  TRACE(pdc, "PDC_string_copy called");
  if (!src || !targ) {
    /* XXX REPORT ERROR */
    return PDC_ERROR;
  }
  if (!targ->rbuf) {
    if (!(targ->rbuf = RMM_new_rbuf(pdc->rmm_nz))) {
      goto no_space;
    }
  }
  if (RBuf_reserve(targ->rbuf, (void**)&(targ->str), sizeof(char), len+1, 0)) {
    goto no_space;
  }
  memcpy(targ->str, src, len);
  targ->str[len] = 0;
  targ->len = len;
  return PDC_OK;
 no_space:
  /* XXX REPORT ERROR */
  return PDC_ERROR;
}

PDC_error_t
PDC_string_fw_read(PDC_t* pdc, PDC_base_em* em, size_t width,
		   PDC_base_ed* ed, PDC_string* s_out, PDC_disc_t* disc)
{
  char            *begin, *end;
  PDC_base_em     emt = PDC_CheckAndSet;
  PDC_base_ed     edt;

  PDC_DISC_INIT_CHECKS;
  TRACE(pdc, "PDC_string_fw_read called");
  if (!em) {
    em = &emt;
  }
  if (!ed) {
    ed = &edt;
  }
  if (width <= 0) {
    WARN(pdc, "UNEXPECTED PARAM VALUE: PDC_string_fw_read called with width <= 0");
    goto width_not_avail;
  }
  /* ensure there are width chars available */
  if (PDC_ERROR == PDC_IO_getchars(pdc, width, &begin, &end, disc)) {
    goto width_not_avail;
  }
  /* success */
  PDC_STR_COPY(s_out, begin, end);
  ed->errCode = PDC_NO_ERROR;
  return PDC_OK;

 width_not_avail:
  HANDLE_ERR_CUR2ENDPOS(PDC_WIDTH_NOT_AVAILABLE);

 no_space:
  HANDLE_ERR_CURPOS(PDC_OUT_OF_MEMORY);
}

PDC_error_t
PDC_string_stopChar_read(PDC_t* pdc, PDC_base_em* em, unsigned char stopChar,
			 PDC_base_ed* ed, PDC_string* s_out, PDC_disc_t* disc)
{
  char            *begin, *end, *ptr;
  PDC_base_em     emt = PDC_CheckAndSet;
  PDC_base_ed     edt;

  PDC_DISC_INIT_CHECKS;
  TRACE(pdc, "PDC_string_stopChar_read called");
  if (!em) {
    em = &emt;
  }
  if (!ed) {
    ed = &edt;
  }
  if (PDC_ERROR == PDC_IO_checkpoint(pdc, 0, disc)) {
    goto no_space;
  }
  if (PDC_ERROR == PDC_Internal_IO_needchars(pdc, 0, &begin, &end, disc)) { /* 0 means do not obey panicStop */
    goto not_found; /* hit EOF */
  }
  for (ptr = begin; ptr < end; ptr++) {
    if (*ptr == stopChar) {
      /* success */
      PDC_STR_COPY(s_out, begin, ptr);
      PDC_IO_forward(pdc, ptr-begin, disc);
      if (PDC_ERROR == PDC_IO_commit(pdc, disc)) {
	return PDC_ERROR; /* XXX internal error -- unrecoverable error */
      }
      ed->errCode = PDC_NO_ERROR;
      return PDC_OK;
    }
  }
  /* hit EOF/EOR before hitting stopChar */

 not_found:
  if (PDC_ERROR == PDC_IO_restore(pdc, disc)) {
    /* XXX unrecoverable error -- should call discipline unrecov error handler */
  }
  HANDLE_ERR_CUR2ENDPOS(PDC_CHAR_LIT_NOT_FOUND);

 no_space:
  HANDLE_ERR_CURPOS(PDC_OUT_OF_MEMORY);
}

/* XXX not supporting multi-line strings yet XXX */
PDC_error_t
PDC_string_stopRegexp_read(PDC_t* pdc, PDC_base_em* em, const char* stopRegexp,
			   PDC_base_ed* ed, PDC_string* s_out, PDC_disc_t* disc)
{
  int             len;
  char            *begin, *end, *ptr;
  const char      *stopCharSetBegin, *stopCharSetEnd, *ptr2;
  PDC_base_em     emt = PDC_CheckAndSet;
  PDC_base_ed     edt;

  PDC_DISC_INIT_CHECKS;
  TRACE(pdc, "PDC_string_stopRegexp_read called");
  if (!em) {
    em = &emt;
  }
  if (!ed) {
    ed = &edt;
  }
  if (!stopRegexp) {
    WARN(pdc, "astringSRE : null regexp specified");
    goto invalid_regexp;
  }
  len = strlen(stopRegexp);
  if ((len < 3) || stopRegexp[0] != '[' || stopRegexp[len-1] != ']') {
    WARN1(pdc, "astringSRE : invalid regexp: %s, currently only support a stopRegexp of the form [<chars>], i.e., a simple stop char set", stopRegexp);
    goto invalid_regexp;
  }
  stopCharSetBegin = stopRegexp + 1;          /* first stop char */
  stopCharSetEnd   = stopRegexp + (len - 1);  /* one past last stop char */
  if (PDC_ERROR == PDC_IO_checkpoint(pdc, 0, disc)) {
    goto no_space;
  }
  if (PDC_ERROR == PDC_Internal_IO_needchars(pdc, 0, &begin, &end, disc)) { /* 0 means do not obey panicStop */
    goto not_found; /* hit EOF */
  }
  for (ptr = begin; ptr < end; ptr++) {
    for (ptr2 = stopCharSetBegin; ptr2 < stopCharSetEnd;  ptr2++) {
      if (*ptr == *ptr2) { /* success */
	PDC_STR_COPY(s_out, begin, ptr);
	PDC_IO_forward(pdc, ptr-begin, disc);
	if (PDC_ERROR == PDC_IO_commit(pdc, disc)) {
	  return PDC_ERROR; /* XXX internal error -- unrecoverable error */
	}
	ed->errCode = PDC_NO_ERROR;
	return PDC_OK;
      }
    }
  }
  /* hit EOF/EOR before hitting a stopChar */

 not_found:
  if (PDC_ERROR == PDC_IO_restore(pdc, disc)) {
    /* XXX unrecoverable error -- should call discipline unrecov error handler */
  }
  HANDLE_ERR_CUR2ENDPOS(PDC_REGEXP_NOT_FOUND);

 no_space:
  HANDLE_ERR_CURPOS(PDC_OUT_OF_MEMORY);

 invalid_regexp:
  HANDLE_ERR_CURPOS(PDC_INVALID_REGEXP);
}

/* ================================================================================ */
/* MISC READ ROUTINES */

PDC_error_t
PDC_countXtoY(PDC_t* pdc, PDC_base_em* em, PDC_uint8 x, PDC_uint8 y,
	      PDC_base_ed* ed, PDC_int32* res_out, PDC_disc_t* disc)
{
  unsigned char   ct;
  PDC_base_em     emt = PDC_CheckAndSet;
  PDC_base_ed     edt;

  PDC_DISC_INIT_CHECKS;
  TRACE2(pdc, "PDC_countXtoY called for x = %s y = %s", PDC_fmtChar(x), PDC_fmtChar(y));
  if (!em) {
    em = &emt;
  }
  if (!ed) {
    ed = &edt;
  }
  if (res_out) {
    (*res_out) = 0;
  }
  if (PDC_ERROR == PDC_IO_checkpoint(pdc, 0, disc)) {
    HANDLE_ERR_CURPOS(PDC_OUT_OF_MEMORY);
  }
  while (PDC_OK == PDC_IO_getchar(pdc, 1, &ct, disc)) { /* 1 means obey panicStop */
    if (y == ct) { /* success */
      if (PDC_ERROR == PDC_IO_restore(pdc, disc)) {
	HANDLE_ERR_CURPOS(PDC_INTERNAL_ERROR);
      }
      ed->errCode = PDC_NO_ERROR;
      return PDC_OK;
    }
    if (x == ct && res_out) {
      (*res_out)++;
    }
  }
  /* y not found */
  if (PDC_ERROR == PDC_IO_restore(pdc, disc)) {
    HANDLE_ERR_CURPOS(PDC_INTERNAL_ERROR);
  }
  HANDLE_ERR_CURPOS(PDC_CHAR_LIT_NOT_FOUND);
}

/* ================================================================================ */
/* IO FUNCTIONS */

/*
 * PDC_IO_refill: only call either to initialize IO during fopen
 * or when all prior input has been parsed.  Returns PDC_ERROR
 * if there is an error condition on the stream / stream is at eof.
 */
PDC_error_t
PDC_IO_refill(PDC_t* pdc, PDC_disc_t* disc)
{
  ssize_t         readlen;
  PDC_IO_line_t*  readline;
  PDC_IO_line_t*  latestline  = &(pdc->ilines[pdc->itail]);
  PDC_stkElt_t*   tp          = &(pdc->stack[pdc->top]);

  PDC_DISC_INIT_CHECKS;
  TRACE(pdc, "PDC_IO_refill called");
#ifndef NDEBUG
  if (tp->idx != pdc->itail) {
    /* Something is very wrong */
    WARN(pdc, "XXX internal error: IO_refill called when tp->idx != pdc->itail");
    return PDC_ERROR;
  }
  if (tp->cur < latestline->eoffset) {
    /* Something is very wrong */
    WARN(pdc, "XXX internal error: IO_refill called when tp->cur < latestline->eoffset");
    return PDC_ERROR;
  }
#endif
  if (pdc->eof) {
    return PDC_ERROR;
  }
  if (pdc->top == 0) {
    /* can revert to state where ilines has only 1 line */
    pdc->bchars  = 0;
    pdc->itail   = 0;
  } else {
    /* must copy sfbuf data and update latestline before proceeding with sfgetr */
    if (++(pdc->itail) >= pdc->ialloc) {
      DBG2(pdc, "XXX Growing from %d to %d iline slots", pdc->ialloc, 2*pdc->ialloc);
      pdc->ialloc *= 2;
      if (!(pdc->ilines = vmnewof(pdc->vm, pdc->ilines, PDC_IO_line_t, pdc->ialloc, 0))) {
	WARN(pdc, "out of space [input line tracking]");
	goto at_eof; /* pretend we hit eof */
      }
    }
    if (pdc->bchars + (latestline->eoffset - latestline->boffset) > pdc->balloc) {
      while (pdc->bchars + (latestline->eoffset - latestline->boffset) > pdc->balloc) {
	pdc->balloc *= 2;
      }
      if (!(pdc->buf = vmnewof(pdc->vm, pdc->buf, char, pdc->balloc, 0))) {
	WARN(pdc, "out of space [shadow buf]");
	goto at_eof; /* pretend we hit eof */
      }
    }
    memcpy(pdc->buf + pdc->bchars, pdc->sfbuf, latestline->eoffset - latestline->boffset);
    latestline->boffset = pdc->bchars;
    pdc->bchars += (latestline->eoffset - latestline->boffset);
    latestline->eoffset = pdc->bchars;
  }
  /* try to read a line of input into readline */
  tp->idx         = pdc->itail;
  readline        = &(pdc->ilines[pdc->itail]);
  readline->boffset  = readline->eoffset = tp->cur = 0; /* useful if reading fails */
  readline->lnum  = ++(pdc->lnum);
  pdc->sfbuf = sfgetr(pdc->io, '\n', 0);
  readlen = sfvalue(pdc->io);
  if (sferror(pdc->io)) {
    SYSERR(pdc, "Error reading IO stream");
    goto at_eof;
  }
  if (!pdc->sfbuf) { /* check for partial read */
    pdc->sfbuf = sfgetr(pdc->io, 0, SF_LASTR);
    if (sferror(pdc->io)) {
      SYSERR(pdc, "Error reading IO stream");
      goto at_eof; /* pretend we hit eof */
    }
    if (!pdc->sfbuf || readlen == 0) {      /* hit EOF */
      goto at_eof;
    }
    pdc->eof = 1; /* cursor not at eof, but no need to read another line */
  }
  readline->boffset = tp->cur = 0;
  readline->eoffset = readlen;
  { /* XXX_REMOVE THIS WHOLE SCOPE */
    if (*(pdc->sfbuf + readlen - 1) == '\n') { readlen--; }
    WARN2(pdc, "XXX_REMOVE line %d: %s", pdc->lnum, PDC_fmtStrL(pdc->sfbuf, readlen));
  }
  return PDC_OK;

 at_eof:
  pdc->eof = 1;
  if (pdc->itail > 0) { /* better to point to end of a real line than start of zero-length line */
    (pdc->itail)--; /* drop zero length line */
    tp->idx    = pdc->itail;
    readline   = &(pdc->ilines[pdc->itail]);
    tp->cur    = readline->eoffset; /* point to end of line */
  }
  return PDC_ERROR;
}

PDC_error_t
PDC_get_loc(PDC_t* pdc, PDC_loc_t* l, PDC_disc_t* disc)
{
  PDC_stkElt_t*   tp          = &(pdc->stack[pdc->top]);
  PDC_IO_line_t*  tpline      = &(pdc->ilines[tp->idx]);

  PDC_DISC_INIT_CHECKS;
  if (!l) {
    WARN(pdc, "PDC_get_loc called with null loc ptr");
    return PDC_ERROR;
  }
  l->beginLine = l->endLine = tpline->lnum;
  l->beginChar = l->endChar = (tp->cur - tpline->boffset) + 1;
  return PDC_OK;
}

PDC_error_t
PDC_get_loc2end(PDC_t* pdc, PDC_loc_t* l, PDC_disc_t* disc)
{
  PDC_stkElt_t*   tp          = &(pdc->stack[pdc->top]);
  PDC_IO_line_t*  tpline      = &(pdc->ilines[tp->idx]);

  PDC_DISC_INIT_CHECKS;
  if (!l) {
    WARN(pdc, "PDC_get_loc2end called with null loc ptr");
    return PDC_ERROR;
  }
  l->beginLine = l->endLine = tpline->lnum;
  l->beginChar = (tp->cur - tpline->boffset) + 1;
  l->endChar = tpline->eoffset - tpline->boffset;
  return PDC_OK;
}

PDC_error_t
PDC_get_beginLoc(PDC_t* pdc, PDC_loc_t* l, PDC_disc_t* disc)
{
  PDC_stkElt_t*   tp          = &(pdc->stack[pdc->top]);
  PDC_IO_line_t*  tpline      = &(pdc->ilines[tp->idx]);

  PDC_DISC_INIT_CHECKS;
  if (!l) {
    WARN(pdc, "PDC_get_beginLoc called with null loc ptr");
    return PDC_ERROR;
  }
  l->beginLine = tpline->lnum;
  l->beginChar = (tp->cur - tpline->boffset) + 1;
  return PDC_OK;
}

PDC_error_t
PDC_get_endLoc(PDC_t* pdc, PDC_loc_t* l, PDC_disc_t* disc)
{
  PDC_stkElt_t*   tp          = &(pdc->stack[pdc->top]);
  PDC_IO_line_t*  tpline      = &(pdc->ilines[tp->idx]);

  PDC_DISC_INIT_CHECKS;
  if (!l) {
    WARN(pdc, "PDC_getEnd_loc called with null loc ptr");
    return PDC_ERROR;
  }
  l->endLine = tpline->lnum;
  l->endChar = (tp->cur - tpline->boffset) + 1;
  return PDC_OK;
}

int
PDC_IO_is_EOF(PDC_t* pdc, PDC_disc_t* disc) {
  /* eof *and* top's line index has reached tail index *and* top's char cursor has reached last char in line */
  return (pdc->eof && pdc->stack[pdc->top].idx == pdc->itail && pdc->stack[pdc->top].cur >= pdc->ilines[pdc->itail].eoffset);
}

int
PDC_IO_peek_EOF(PDC_t* pdc, PDC_disc_t* disc) {
  unsigned char ct;

  PDC_DISC_INIT_CHECKS;
  TRACE(pdc, "PDC_IO_peek_EOF called");
  if (PDC_ERROR == PDC_IO_getchar(pdc, 0, &ct, disc)) { /* 0 means do not obey panicStop */
    return 1;
  }
  PDC_IO_back(pdc, 1, disc);
  return 0;
}

PDC_error_t
PDC_IO_getchar(PDC_t* pdc, int obeyPanicStop, unsigned char* ct_out, PDC_disc_t* disc)
{
  PDC_stkElt_t*   tp;
  PDC_IO_line_t*  tpline;
  char* base;

  PDC_DISC_INIT_CHECKS;
  TRACE(pdc, "PDC_IO_getchar called");
  if (PDC_Internal_IO_needchar(pdc, obeyPanicStop, &tp, &tpline, disc)) {
    return PDC_ERROR; /* at panic stop or at EOF */
  }
  if (ct_out) {
    base = (tp->idx == pdc->itail) ? pdc->sfbuf : pdc->buf;
    (*ct_out) = *(base + tp->cur);
  }
  (tp->cur)++;
  return PDC_OK;
}

/*
 * XXX currently only works within single input line XXX
 */
PDC_error_t
PDC_IO_getchars(PDC_t* pdc, size_t num_chars, char** b_out, char** e_out, PDC_disc_t* disc)
{
  PDC_stkElt_t*   tp;
  PDC_IO_line_t*  tpline;
  char* base;

  PDC_DISC_INIT_CHECKS;
  TRACE(pdc, "PDC_IO_getchars called");
  if (PDC_Internal_IO_needchar(pdc, 0, &tp, &tpline, disc)) { /* hit EOF */
    return PDC_ERROR;
  }
  if (tp->cur + num_chars > tpline->eoffset) {
    /* not enough chars left on line; leave IO cursor as-is  */
    return PDC_ERROR;
  }
  /* set *b_out, *e_out to beginning and just past ending of requested num_chars */
  base = (tp->idx == pdc->itail) ? pdc->sfbuf : pdc->buf;
  if (b_out) {
    (*b_out) = base + tp->cur;
  }
  if (e_out) {
    (*e_out) = base + (tp->cur + num_chars);
  }
  /* advance IO cursor num_chars */
  tp->cur += num_chars;
  return PDC_OK;
}

PDC_error_t
PDC_IO_back(PDC_t* pdc, size_t num_chars, PDC_disc_t* disc)
{
  PDC_stkElt_t*   tp          = &(pdc->stack[pdc->top]);
  PDC_IO_line_t*  tpline      = &(pdc->ilines[tp->idx]);
  size_t avail_this_line, todo;

  PDC_DISC_INIT_CHECKS;
  TRACE(pdc, "PDC_IO_back called");
  todo = num_chars;
  while (1) {
    avail_this_line = tp->cur - tpline->boffset;
    if (todo <= avail_this_line) { /* all set */
      tp->cur -= todo;
      DBG1(pdc, "PDC_IO_BACK: moved back %d chars", num_chars);
      return PDC_OK;
    }
    if (tp->idx > 0) { /* backup to end of prev line and continue loop */
      (tp->idx)--;
      tpline = &(pdc->ilines[tp->idx]);
      tp->cur = tpline->eoffset;
      todo -= avail_this_line;
      continue;
    }
    /* At first line in memory but not good enough. */
    /* Backup to offset zero on this line, return error. */
    tp->cur = 0;
    todo -= avail_this_line;
    break;
  }
  WARN2(pdc, "XXX_CHANGE_TO_DBG PDC_IO_BACK: requested move back %d, but could only move back %d chars", num_chars, num_chars - todo);
  return PDC_ERROR;
}

PDC_error_t
PDC_IO_forward(PDC_t* pdc, size_t num_chars, PDC_disc_t* disc)
{
  PDC_stkElt_t*   tp          = &(pdc->stack[pdc->top]);
  PDC_IO_line_t*  tpline      = &(pdc->ilines[tp->idx]);
  size_t avail_this_line, todo;

  PDC_DISC_INIT_CHECKS;
  TRACE(pdc, "PDC_IO_forward called");
  todo = num_chars;
  while (1) {
    avail_this_line = tpline->eoffset - tp->cur;
    if (todo <= avail_this_line) { /* all set */
      tp->cur += todo;
      DBG1(pdc, "PDC_IO_FORWARD: moved forward %d chars", num_chars);
      return PDC_OK;
    }
    if (tp->idx < pdc->itail) { /* advance to next in-memory input line */
      (tp->idx)++;
      tp->cur = 0;
      tpline = &(pdc->ilines[tp->idx]);
      todo -= avail_this_line;
      continue;
    }
    /* At last line in memory; attempting to move forward over 
     * data that has not been read yet -- error!
     * Advance to end of line and return error.
     */
    tp->cur = tpline->eoffset;
    todo -= avail_this_line;
    break;
  }
  WARN2(pdc, "XXX_CHANGE_TO_DBG PDC_IO_FORWARD: requested move forward %d, but could only move forward %d chars", num_chars, num_chars - todo);
  return PDC_ERROR;
}

PDC_error_t
PDC_IO_fopen(PDC_t* pdc, char* path, PDC_disc_t* disc)
{
  PDC_DISC_INIT_CHECKS;
  TRACE1(pdc, "PDC_IO_fopen called for path = %s", path);
  if (!path) {
    WARN(pdc, "fopen called with null path");
    return PDC_ERROR;
  }
  if (pdc->io) {
    WARN(pdc, "fopen called while previous file still open");
    return PDC_ERROR;
  }
  if (!(pdc->io = sfopen(NiL, path, "r"))) {
    SYSERR1(pdc, "Failed to open file \"%s\"", path);
    return PDC_ERROR;
  }
  if (!(pdc->path = vmnewof(pdc->vm, 0, char, strlen(path) + 1, 0))) {
    WARN(pdc, "out of space [string to record file path]");
    PDC_IO_fclose(pdc, disc);
    return PDC_ERROR;
  }
  strcpy(pdc->path, path);
  PDC_IO_initialize(pdc, disc); /* sets state to 'nothing read/no checkpoints */
  PDC_IO_refill(pdc, disc);     /* get line of input */
  return PDC_OK;
}

PDC_error_t
PDC_IO_fclose(PDC_t* pdc, PDC_disc_t* disc)
{

  PDC_DISC_INIT_CHECKS;
  TRACE(pdc, "PDC_IO_fclose called");
  if (!pdc->io) {
    return PDC_ERROR;
  }
  if (pdc->io) {
    sfclose(pdc->io);
    pdc->io = 0;
    pdc->eof = 1;
  }
  if (!pdc->vm || !pdc->path) {
    return PDC_ERROR;
  }
  vmfree(pdc->vm, pdc->path);
  pdc->path = 0;
  return PDC_OK;
}

PDC_error_t
PDC_IO_checkpoint(PDC_t* pdc, int speculative, PDC_disc_t* disc)
{
  PDC_DISC_INIT_CHECKS;
  TRACE(pdc, "PDC_IO_checkpoint called");
  if (++(pdc->top) >= pdc->salloc) {
    DBG2(pdc, "XXX Growing from %d to %d checkpoint stack slots", pdc->salloc, 2*pdc->salloc);
    pdc->salloc *= 2;
    if (!(pdc->stack = vmnewof(pdc->vm, pdc->stack, PDC_stkElt_t, pdc->salloc, 0))) {
      WARN(pdc, "out of space [input cursor stack]");
      return PDC_ERROR;
    }
  }
  pdc->stack[pdc->top].idx  = pdc->stack[pdc->top - 1].idx;
  pdc->stack[pdc->top].cur  = pdc->stack[pdc->top - 1].cur;
  pdc->stack[pdc->top].spec = speculative;
  if (speculative) {
    (pdc->speclev)++;
  }
  return PDC_OK;
}

PDC_error_t
PDC_IO_restore(PDC_t* pdc, PDC_disc_t* disc)
{
  PDC_DISC_INIT_CHECKS;
  TRACE(pdc, "PDC_IO_restore called");
  if (pdc->top <= 0) {
    WARN(pdc, "Internal error: PDC_IO_restore called when stack top <= 0");
    return PDC_ERROR;
  }
  if (pdc->stack[pdc->top].spec) {
    (pdc->speclev)--;
  }
  /* this discards all changes since the latest checkpoint */ 
  (pdc->top)--;
  return PDC_OK;
}

unsigned int
PDC_spec_level(PDC_t* pdc, PDC_disc_t* disc)
{
  PDC_DISC_INIT_CHECKS;
  TRACE(pdc, "PDC_spec_level called");
  return pdc->speclev;
}

PDC_error_t
PDC_IO_commit(PDC_t* pdc, PDC_disc_t* disc)
{
  PDC_DISC_INIT_CHECKS;
  TRACE(pdc, "PDC_IO_commit called");
  if (pdc->top <= 0) {
    WARN(pdc, "Internal error: PDC_IO_commit called when stack top <= 0");
    return PDC_ERROR;
  }
  if (pdc->stack[pdc->top].spec) {
    (pdc->speclev)--;
  }
  /* propagate changes up to next level */
  pdc->stack[pdc->top - 1].idx = pdc->stack[pdc->top].idx;
  pdc->stack[pdc->top - 1].cur = pdc->stack[pdc->top].cur;
  (pdc->top)--;
  return PDC_OK;
}

PDC_error_t
PDC_IO_getLineBuf(PDC_t* pdc, size_t line, char** buf_out, size_t* len_out, PDC_disc_t* disc) {
  char* base;
  int i;

  PDC_DISC_INIT_CHECKS;
  TRACE(pdc, "PDC_IO_getLineBuf called");
  if (!buf_out) {
    WARN(pdc, "PDC_IO_getLineBuf called with null buf_out");
    return PDC_ERROR;
  }
  if (!len_out) {
    WARN(pdc, "PDC_IO_getLineBuf called with null len_out");
    return PDC_ERROR;
  }
  for (i = 0; i <= pdc->itail; i++) {
    if (pdc->ilines[i].lnum == line) {
      base = (i == pdc->itail) ? pdc->sfbuf : pdc->buf;
      (*buf_out) = base + pdc->ilines[i].boffset;
      (*len_out) = pdc->ilines[i].eoffset - pdc->ilines[i].boffset;
      return PDC_OK;
    }
  }
  return PDC_ERROR;
}


PDC_error_t
PDC_IO_initialize(PDC_t* pdc, PDC_disc_t* disc)
{
  PDC_stkElt_t*   tp          = &(pdc->stack[0]);
  PDC_IO_line_t*  tpline      = &(pdc->ilines[0]);

  PDC_DISC_INIT_CHECKS;
  TRACE(pdc, "PDC_IO_initialize called");
  pdc->top        = 0;
  pdc->itail      = 0;
  pdc->eof        = 0;
  pdc->lnum       = 0;
  tp->idx         = 0;
  tp->cur         = 0;
  tpline->lnum    = 0;
  tpline->boffset = 0;
  tpline->eoffset = 0;
  return PDC_OK;
}

/* ================================================================================ */ 
/* INTERNAL IO FUNCTIONS */

PDC_error_t
PDC_Internal_IO_needchar(PDC_t* pdc, int obeyPanicStop, PDC_stkElt_t** tp_out, PDC_IO_line_t** tpline_out, PDC_disc_t* disc)
{
  PDC_stkElt_t*   tp      = &(pdc->stack[pdc->top]);
  PDC_IO_line_t*  tpline  = &(pdc->ilines[tp->idx]);

  TRACE(pdc, "PDC_Internal_IO_needchar called");
  while (1) {
    if (PDC_IO_is_EOF(pdc, disc)) { /* already hit EOF */
      return PDC_ERROR;
    }
    if (tp->cur < tpline->eoffset) { /* still more chars on current line */
      break;
    }
    if (obeyPanicStop && (disc->p_stop == PDC_Line_Stop)) {
      /* do not move beyond newline char / line end */
      return PDC_ERROR;
    }
    if (tp->idx < pdc->itail) { /* advance to next in-memory input line */
      (tp->idx)++;
      tp->cur = 0;
      tpline = &(pdc->ilines[tp->idx]);
    } else {
      /* hit end of in-memory input lines, must get next line */
      if (PDC_ERROR == PDC_IO_refill(pdc, disc)) {
	return PDC_ERROR;
      }
    }
    /* go to top of loop to do eof/eol checks again */
  }
  (*tp_out) = tp;
  (*tpline_out) = tpline;
  return PDC_OK;
}

PDC_error_t
PDC_Internal_IO_needchars(PDC_t* pdc, int obeyPanicStop, char** b_out, char** e_out, PDC_disc_t* disc)
{
  PDC_stkElt_t*   tp      = &(pdc->stack[pdc->top]);
  PDC_IO_line_t*  tpline  = &(pdc->ilines[tp->idx]);

  TRACE(pdc, "PDC_Internal_IO_needchars called");
  while (1) {
    if (PDC_IO_is_EOF(pdc, disc)) { /* already hit EOF */
      return PDC_ERROR;
    }
    if (tp->cur < tpline->eoffset) { /* still more chars on current line */
      break;
    }
    if (obeyPanicStop && (disc->p_stop == PDC_Line_Stop)) {
      /* do not move beyond newline char / line end */
      return PDC_ERROR;
    }
    if (tp->idx < pdc->itail) { /* advance to next in-memory input line */
      (tp->idx)++;
      tp->cur = 0;
      tpline = &(pdc->ilines[tp->idx]);
    } else {
      /* hit end of in-memory input lines, must get next line */
      if (PDC_ERROR == PDC_IO_refill(pdc, disc)) {
	return PDC_ERROR;
      }
    }
    /* go to top of loop to do eof/eol checks again */
  }
  (*b_out) = (tp->idx == pdc->itail) ? pdc->sfbuf : pdc->buf;
  (*e_out) = (*b_out) + tpline->eoffset;
  (*b_out) += tp->cur;
  return PDC_OK;
}

/* ================================================================================ */
/* TOP-LEVEL LIBRARY FUNCTIONS */

/* The default discipline */
PDC_disc_t PDC_default_disc = {
  PDC_VERSION,
  (PDC_flags_t)PDC_NULL_CTL_FLAG,
  PDC_Line_Stop,
  PDC_errorf,
  PDC_errorRep_Max,
  PDC_bigEndian,
  PDC_bigEndian
};

PDC_error_t
PDC_open(PDC_disc_t* disc, PDC_t** pdc_out)
{
  Vmalloc_t*    vm;
  PDC_t*        pdc;

  if (!disc) {
    disc = &PDC_default_disc;
  }
  TRACE(NiL, "PDC_open called");
  if (!pdc_out) {
    WARN(NiL, "PDC_open called with null pdc_out");
    return PDC_ERROR;
  }
  if (!(vm = vmopen(Vmdcheap, Vmbest, 0))) {
    WARN(NiL, "out of space [vm]");
    return PDC_ERROR;
  }
  if (!(pdc = vmnewof(vm, 0, PDC_t, 1, 0))) {
    WARN(NiL, "out of space [padsc library data]");
    vmclose(vm);
    return PDC_ERROR;
  }
  if (!(pdc->tmp = sfstropen())) {
    WARN(NiL, "out of space [sfstr]");
    vmclose(vm);
    return PDC_ERROR;
  }
  if (!(pdc->rmm_z = RMM_open(RMM_zero_disc_ptr))) {
    WARN(NiL, "out of space [rbuf memory mgr]");
    vmclose(vm);
    return PDC_ERROR;
  }
  if (!(pdc->rmm_nz = RMM_open(RMM_nozero_disc_ptr))) {
    WARN(NiL, "out of space [rbuf memory mgr]");
    vmclose(vm);
    return PDC_ERROR;
  } 
  pdc->id          = lib;
  pdc->vm          = vm;
  pdc->disc        = disc;
  pdc->itail  = 0;
  pdc->ialloc = PDC_initInpBufs;
  if (!(pdc->ilines = vmnewof(vm, 0, PDC_IO_line_t, pdc->ialloc, 0))) {
    WARN(pdc, "out of space [input line tracking]");
    vmclose(vm);
    return PDC_ERROR;
  }
  pdc->top    = 0;
  pdc->salloc = PDC_initInpBufs;
  if (!(pdc->stack = vmnewof(vm, 0, PDC_stkElt_t, pdc->salloc, 0))) {
    WARN(pdc, "out of space [input cursor stack]");
    vmclose(vm);
    return PDC_ERROR;
  }
  pdc->balloc = PDC_initBufSize;
  if (!(pdc->buf = vmnewof(vm, 0, char, pdc->balloc, 0))) {
    WARN(pdc, "out of space [shadow buf]");
    vmclose(vm);
    return PDC_ERROR;
  }
  pdc->bchars  = 0;
  pdc->speclev = 0;
  (*pdc_out) = pdc;
  return PDC_OK;
}

PDC_error_t
PDC_close(PDC_t* pdc, PDC_disc_t* disc)
{

  PDC_DISC_INIT_CHECKS;
  TRACE(pdc, "PDC_close called");
  if (pdc->rmm_z) {
    RMM_close(pdc->rmm_z);
  }
  if (pdc->rmm_nz) {
    RMM_close(pdc->rmm_nz);
  }
  if (!pdc->vm) {
    return PDC_ERROR;
  }
  vmclose(pdc->vm); /* frees everything alloc'd using vm */
  return PDC_OK;
}

/* ================================================================================ */
/* INTERNAL PDC FUNCTIONS */

RMM_t*
PDC_rmm_zero  (PDC_t* pdc, PDC_disc_t* disc)
{
  return pdc->rmm_z;
}

RMM_t*
PDC_rmm_nozero(PDC_t* pdc, PDC_disc_t* disc)
{
  return pdc->rmm_nz;
}

/* ================================================================================ */
/* MISC ROUTINES -- INTERNAL */

char*
PDC_fmtChar(char c) {
  return fmtquote(&c, NiL, NiL, 1, 0);
}

char*
PDC_fmtStr(const PDC_string* s) {
  return fmtquote(s->str, NiL, NiL, s->len, 0);
}

char*
PDC_fmtStrL(const char* s, size_t len) {
  return fmtquote(s, NiL, NiL, len, 0);
}

/* ================================================================================ */
