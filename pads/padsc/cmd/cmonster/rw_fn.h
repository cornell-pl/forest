#ifdef _USE_PROTO
#pragma prototyped
#endif
/*
 * Helpers
 *
 * Kathleen Fisher, Robert Gruber
 * AT&T Labs Research
 */

#ifndef __RW_FN_H__
#define __RW_FN_H__

#define CM_RW_FN_IMPL(fn_nm, params, targ_decl, read_call, assign_stmt, dbg_val_write, write2buf_nm, write_sz, write_call) \
PDC_error_t fn_nm ( params ) \
{ \
  targ_decl; \
  PDC_base_pd    pd; \
  PDC_base_m     m = PDC_CheckAndSet; /* could optimize and just use PDC_Set */ \
  PDC_error_t    er; \
  int            buf_full; \
 \
  size_t avail_in   = end - (begin + qy->off); \
  size_t remain_out = cm->outbuf_end - cm->outbuf_cursor; \
 \
  sfprintf(cm->errf, PDCI_MacroArg2String(fn_nm) " for type %s called\n", qy->entry->tname); \
  sfprintf(cm->errf, "  outbuf has %lu bytes remaining\n", (unsigned long)remain_out); \
  sfprintf(cm->errf, "  input has %lu bytes available starting at offset %lu\n", \
	(unsigned long)avail_in, (unsigned long)qy->off); \
  if (qy->out_sz > remain_out) { \
    sfprintf(cm->errf, \
	  "  Error: qy requires %lu output bytes but outbuf has only %lu bytes\n" \
	  "   remaining.  Skipping this data item.\n", \
	  (unsigned long)qy->out_sz, (unsigned long)remain_out); \
    return PDC_ERR; \
  } \
  if (qy->in_sz > avail_in) { \
    sfprintf(cm->errf, \
	  "  Error: qy requires %lu input bytes but input record has only %lu bytes\n\n" \
	  "  available starting at offset %lu.  Skipping this data item.\n", \
	  (unsigned long)qy->in_sz, (unsigned long)avail_in, (unsigned long)qy->off); \
    return PDC_ERR; \
  } \
  PDC_IO_checkpoint(cm->pdc, 0); /* could use 1 to supress error msgs */ \
  if (qy->off) { \
    PDCI_IO_forward(cm->pdc, qy->off); \
  } \
  er = read_call; \
  PDC_IO_restore(cm->pdc); \
  if (PDC_ERR == er) { \
    sfprintf(cm->errf, \
	  "  Error: read function for type %s at offset %lu failed\n" \
	  "  Skipping this data item.\n", (unsigned long)qy->off, qy->entry->tname); \
    return PDC_ERR; \
  } \
  assign_stmt; \
  dbg_val_write; \
  if (qy->out_sz) { /* write the val */ \
    if (write_sz != write_call) { \
      sfprintf(cm->errf, "\n*** FATAL: Unexpected error calling " write2buf_nm "\n\n"); \
      abort(); \
    } \
    sfprintf(cm->errf, "  advancing outbuf_cursor by %lu bytes\n", (unsigned long)qy->out_sz); \
    cm->outbuf_cursor += qy->out_sz; \
  } \
  return PDC_OK; \
}

#endif  /*  !__RW_FN_H__  */
