#pragma prototyped
/*
 * accum library routines for library that goes with padsc
 *
 * Kathleen Fisher, Robert Gruber
 * AT&T Labs Research
 */

#include "libpadsc-internal.h"
#include "libpadsc-macros.h"

/* ================================================================================ */
/*
 * PDC_int32_acc -- internal stuff
 */

typedef struct PDC_int32_dt_elt_s {
  PDC_int32    val;
  PDC_uint32   cnt;
  Dtlink_t     link;
} PDC_int32_dt_elt_t;

int PDC_int32_dt_elt_describe(Dt_t* dt, Void_t* a, Void_t* data)
{
  PDC_int32_dt_elt_t* elt = (PDC_int32_dt_elt_t*)a;
  NoP(data);
  error(0, "  dt_elt: val %3d cnt %3d", elt->val, elt->cnt);
  return 0;
}

int
PDC_int32_dt_elt_cmp(Dt_t* dt, PDC_int32_dt_elt_t* a, PDC_int32_dt_elt_t* b, Dtdisc_t* disc)
{
  NoP(dt);
  NoP(disc);
  if (a->val < b->val)
    return -1;
  if (a->val > b->val)
    return 1;
  return 0;
}

void*
PDC_int32_dt_elt_make(Dt_t* dt, PDC_int32_dt_elt_t* a, Dtdisc_t* disc)
{
   PDC_int32_dt_elt_t* b;
  if ((b = oldof(0, PDC_int32_dt_elt_t, 1, 0))) {
    b->val = a->val;
    b->cnt = a->cnt;
  }
  return b;
}

void
PDC_int32_dt_elt_free(Dt_t* dt, PDC_int32_dt_elt_t* a, Dtdisc_t* disc)
{
  free(a);
}

static Dtdisc_t PDC_int32_acc_dt_disc = {
  0,                                   /* key     */
  0,                                   /* size    */
  offsetof(PDC_int32_dt_elt_t, link),  /* link    */
  (Dtmake_f)PDC_int32_dt_elt_make,     /* makef   */
  (Dtfree_f)PDC_int32_dt_elt_free,     /* freef   */
  (Dtcompar_f)PDC_int32_dt_elt_cmp,    /* comparf */
  NiL,                                 /* hashf   */
  NiL,                                 /* memoryf */
  NiL                                  /* eventf  */
};

/* ================================================================================ */
/*
 * PDC_int32_acc -- library-level functions
 */

PDC_error_t
PDC_int32_acc_init(PDC_t* pdc, PDC_int32_acc* a, PDC_disc_t* disc)
{
  PDC_DISC_INIT_CHECKS;
  TRACE(pdc, "PDC_int32_acc_init called");
  if (!a) {
    return PDC_ERROR;
  }
  if (!(a->dict = dtopen(&PDC_int32_acc_dt_disc, Dtoset))) {
    return PDC_ERROR;
  }
  a->good = a->bad = a->fold = a->psum = a->avg = a->min = a->max = 0;
  return PDC_OK;
}

PDC_error_t
PDC_int32_acc_reset(PDC_t* pdc, PDC_int32_acc* a, PDC_disc_t* disc)
{
  PDC_DISC_INIT_CHECKS;
  TRACE(pdc, "PDC_int32_acc_reset called");
  if (!a || !a->dict) {
    return PDC_ERROR;
  }
  dtclear(a->dict);
  a->good = a->bad = a->fold = a->psum = a->avg = a->min = a->max = 0;
  return PDC_OK;
}

PDC_error_t
PDC_int32_acc_cleanup(PDC_t* pdc, PDC_int32_acc* a, PDC_disc_t* disc)
{
  PDC_DISC_INIT_CHECKS;
  TRACE(pdc, "PDC_int32_acc_cleanup called");
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
PDC_int32_acc_fold_psum(PDC_int32_acc* a) {
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
PDC_int32_acc_add(PDC_t* pdc, PDC_int32_acc* a, PDC_base_ed* ed, PDC_int32* val, PDC_disc_t* disc)
{
  PDC_int32           v          = (*val);
  PDC_int32_dt_elt_t  insert_val = { 0 };
  PDC_int32_dt_elt_t* tmp1;
  PDC_DISC_INIT_CHECKS;
  TRACE(pdc, "PDC_int32_acc_add called");
  if (!a || !a->dict || !ed || !val) {
    return PDC_ERROR;
  }
  if (ed->errCode != 0) {
    (a->bad)++;
    return PDC_OK;
  }
  a->psum += v;
  a->good++;
  if (a->good == 1) {
    a->min = a->max = v;
  } else if (v < a->min) {
    a->min = v;
  } else if (v > a->max) {
    a->max = v;
  }
  if (a->good % 1000 == 0) {
    PDC_int32_acc_fold_psum(a);
  }
  if (dtsize(a->dict) < 10) {
    insert_val.val = v;
    if (!(tmp1 = dtinsert(a->dict, &insert_val))) {
      WARN(pdc, "** PADC internal error: dtinsert failed (out of memory?) **");
      return PDC_ERROR;
    }
    tmp1->cnt++;
  } else if ((tmp1 = dtmatch(a->dict, (Void_t*)&v))) {
    tmp1->cnt++;
  }
  return PDC_OK;
}

PDC_error_t
PDC_int32_acc_report(PDC_t* pdc, const char* prefix, PDC_int32_acc* a, PDC_disc_t* disc)
{
  PDC_uint64 cnt_sum = 0;
  double     cnt_sum_pcnt;
  Void_t* velt;
  PDC_DISC_INIT_CHECKS;
  TRACE(pdc, "PDC_int32_acc_report called");
  if (!a) {
    return PDC_ERROR;
  }
  if (!disc->errorf) {
    return PDC_OK;
  }
  if (!a->good) {
    disc->errorf(pdc, disc, 0,
		 "%s: int32 field with 0 good vals %ld bad vals ==> ** NO GOOD VALS **",
		 prefix, a->bad);
    return PDC_OK;
  }
  PDC_int32_acc_fold_psum(a);
  sfstrset(pdc->tmp, 0);
  sfprintf(pdc->tmp, "%s: int32 field with %ld good vals %ld bad vals; for good vals: min %d max %d avg %lf\n",
	   prefix, a->good, a->bad, a->min, a->max, a->avg);
  sfprintf(pdc->tmp, "  Distribution of first 10 values encountered:\n");
  for (velt = dtfirst(a->dict); velt; velt = dtnext(a->dict, velt)) {
    double elt_pcnt;
    PDC_int32_dt_elt_t* elt = (PDC_int32_dt_elt_t*)velt;
    cnt_sum += elt->cnt;
    elt_pcnt = (100.0 * elt->cnt)/a->good;
    sfprintf(pdc->tmp, "        val: %10d  count: %10d  pcnt-of-good-vals: %7.3lf\n",
	     elt->val, elt->cnt, elt_pcnt);
  }
  cnt_sum_pcnt = (100.0 * cnt_sum)/a->good;
  sfprintf(pdc->tmp,   "        SUMMING          count: %10d  pcnt-of-good-vals: %7.3lf",
	   cnt_sum, cnt_sum_pcnt);
  disc->errorf(pdc, disc, 0, "%s", sfstruse(pdc->tmp));
  return PDC_OK;
}

/* ================================================================================ */
