/*
 * switch value functions for cmonster
 *
 * Kathleen Fisher, Robert Gruber
 * AT&T Labs Research
 */

#include "cmonster.h"

#define CM_SVAL_FN_IMPL(ty, targty, readcall) \
CM_SVAL_FN_DECL(ty) \
{ \
  targty         targ; \
  PDC_base_pd    pd; \
  PDC_base_m     m = PDC_CheckAndSet; /* could optimize and just use PDC_Set */ \
  PDC_error_t    er; \
  int            buf_full; \
 \
  size_t avail_in   = end - (begin + qy->off); \
  size_t remain_out = cm->outbuf_end - cm->outbuf_cursor; \
 \
  error(0, "swval_fn for %s called", qy->entry->tname); \
  error(0, "  outbuf has %lu bytes remaining", (unsigned long)remain_out); \
  error(0, "  input has %lu bytes available starting at offset %lu", \
	(unsigned long)avail_in, (unsigned long)qy->off); \
  if (qy->out_sz > remain_out) { \
    error(0, \
	  "  Error: qy requires %lu output bytes but outbuf has only %lu bytes\n" \
	  "   remaining.  Skipping this data item.", \
	  (unsigned long)qy->out_sz, (unsigned long)remain_out); \
    return PDC_ERR; \
  } \
  if (qy->in_sz > avail_in) { \
    error(0, \
	  "  Error: qy requires %lu input bytes but input record has only %lu bytes\n" \
	  "  available starting at offset %lu.  Skipping this data item.", \
	  (unsigned long)qy->in_sz, (unsigned long)avail_in, (unsigned long)qy->off); \
    return PDC_ERR; \
  } \
 \
  PDC_IO_checkpoint(cm->pdc, 0); /* could use 1 to supress error msgs */ \
  PDCI_IO_forward(cm->pdc, qy->off); \
  er = readcall; \
  PDC_IO_restore(cm->pdc); \
  if (PDC_ERR == er) { \
    error(0, \
	  "  Error: read function for type %s at offset %lu failed\n" \
	  "  Skipping this data item.", (unsigned long)qy->off, qy->entry->tname); \
    return PDC_ERR; \
  } \
  (*res_out) = (PDC_int32)targ; \
  if (qy->out_sz) { /* write the val */ \
    if (4 != PDC_sbl_int32_write2buf(cm->pdc, cm->outbuf_cursor, remain_out, &buf_full, 4, &pd, res_out)) { \
      error(ERROR_FATAL, "\n*** FATAL: Unexpected error calling PDC_sbl_int32_write2buf"); \
    } \
    error(0, "  advancing outbuf_cursor by %lu bytes", (unsigned long)qy->out_sz); \
    cm->outbuf_cursor += qy->out_sz; \
  } \
  return PDC_OK; \
}


#undef ty
#undef targty
#undef readcall
#define ty        a_int32_FW
#define targty    PDC_int32
#define readcall  PDC_a_int32_FW_read(cm->pdc, &m, qy->params.elts[0], &pd, &targ)

CM_SVAL_FN_DECL(ty)
{
  targty         targ;
  PDC_base_pd    pd;
  PDC_base_m     m = PDC_CheckAndSet; /* could optimize and just use PDC_Set */
  PDC_error_t    er;
  int            buf_full;

  size_t avail_in   = end - (begin + qy->off);
  size_t remain_out = cm->outbuf_end - cm->outbuf_cursor;

  error(0, "swval_fn for %s called", qy->entry->tname);
  error(0, "  outbuf has %lu bytes remaining", (unsigned long)remain_out);
  error(0, "  input has %lu bytes available starting at offset %lu",
	(unsigned long)avail_in, (unsigned long)qy->off);
  if (qy->out_sz > remain_out) {
    error(0,
	  "  Error: qy requires %lu output bytes but outbuf has only %lu bytes\n"
	  "   remaining.  Skipping this data item.",
	  (unsigned long)qy->out_sz, (unsigned long)remain_out);
    return PDC_ERR;
  }
  if (qy->in_sz > avail_in) {
    error(0,
	  "  Error: qy requires %lu input bytes but input record has only %lu bytes\n"
	  "  available starting at offset %lu.  Skipping this data item.",
	  (unsigned long)qy->in_sz, (unsigned long)avail_in, (unsigned long)qy->off);
    return PDC_ERR;
  }

  PDC_IO_checkpoint(cm->pdc, 0); /* could use 1 to supress error msgs */
  if (qy->off) {
    PDCI_IO_forward(cm->pdc, qy->off);
  }
  er = readcall;
  PDC_IO_restore(cm->pdc);
  if (PDC_ERR == er) {
    error(0,
	  "  Error: read function for type %s at offset %lu failed\n"
	  "  Skipping this data item.", (unsigned long)qy->off, qy->entry->tname);
    return PDC_ERR;
  }
  (*res_out) = (PDC_int32)targ;
  error(0, "  ==> switch val = %ld", (unsigned long)(*res_out));
  if (qy->out_sz) { /* write the val */
    if (4 != PDC_sbl_int32_write2buf(cm->pdc, cm->outbuf_cursor, remain_out, &buf_full, 4, &pd, res_out)) {
      error(ERROR_FATAL, "\n*** FATAL: Unexpected error calling PDC_sbl_int32_write2buf");
    }
    error(0, "  advancing outbuf_cursor by %lu bytes", (unsigned long)qy->out_sz);
    cm->outbuf_cursor += qy->out_sz;
  }
  return PDC_OK;
}
#undef ty
#undef targty
#undef readcall

CM_SVAL_FN_IMPL(int32_FW,   PDC_int32, PDC_int32_FW_read(cm->pdc, &m, qy->params.elts[0], &pd, &targ));
//CM_SVAL_FN_IMPL(a_int32_FW, PDC_int32, PDC_a_int32_FW_read(cm->pdc, &m, qy->params.elts[0], &pd, &targ));
CM_SVAL_FN_IMPL(e_int32_FW, PDC_int32, PDC_e_int32_FW_read(cm->pdc, &m, qy->params.elts[0], &pd, &targ));
CM_SVAL_FN_IMPL(b_int32,    PDC_int32, PDC_b_int32_read(cm->pdc, &m, &pd, &targ));
CM_SVAL_FN_IMPL(ebc_int32,  PDC_int32, PDC_ebc_int32_read(cm->pdc, &m, qy->params.elts[0], &pd, &targ));
CM_SVAL_FN_IMPL(bcd_int32,  PDC_int32, PDC_bcd_int32_read(cm->pdc, &m, qy->params.elts[0], &pd, &targ));
CM_SVAL_FN_IMPL(sbl_int32,  PDC_int32, PDC_sbl_int32_read(cm->pdc, &m, qy->params.elts[0], &pd, &targ));
CM_SVAL_FN_IMPL(sbh_int32,  PDC_int32, PDC_sbh_int32_read(cm->pdc, &m, qy->params.elts[0], &pd, &targ));
CM_SVAL_FN_IMPL(char,       PDC_char,  PDC_char_read(cm->pdc, &m, &pd, &targ));
CM_SVAL_FN_IMPL(a_char,     PDC_char,  PDC_a_char_read(cm->pdc, &m, &pd, &targ));
CM_SVAL_FN_IMPL(e_char,     PDC_char,  PDC_e_char_read(cm->pdc, &m, &pd, &targ));

