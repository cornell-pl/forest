#ifndef MacroArg2String
#define MacroArg2String(s) #s
#endif

/* ================================================================================ */ 
/* PRELIMINARY HELPER MACROS */

/* set loc begin/end to current IO pos */ \
#define HANDLE_ERR_CURPOS(errcode) \
  do { \
    if (*em < PDC_Ignore) { \
      ed->errCode = errcode; \
      PDC_get_loc(pdc, &(ed->loc), disc); \
    } \
    return PDC_ERROR; \
  } while (0)

/* set loc begin to current IO pos, end pos to end of current line */ \
#define HANDLE_ERR_CUR2ENDPOS(errcode) \
  do { \
    if (*em < PDC_Ignore) { \
      ed->errCode = errcode; \
      PDC_get_loc2end(pdc, &(ed->loc), disc); \
    } \
    return PDC_ERROR; \
  } while (0)

/* set loc end to current IO pos */
#define HANDLE_ERR_ENDPOS(errcode) \
  do { \
    if (*em < PDC_Ignore) { \
      ed->errCode = errcode; \
      PDC_get_endLoc(pdc, &(ed->loc), disc); \
    } \
    return PDC_ERROR; \
  } while (0)

/* move begin/end positions backwards by specified amounts */
#define HANDLE_ERR_MODPOS(errcode, adj1, adj2) \
  do { \
    if (*em < PDC_Ignore) { \
      ed->errCode = errcode; \
      PDC_get_loc(pdc, &(ed->loc), disc); \
      ed->loc.beginChar -= (adj1); \
      ed->loc.endChar   -= (adj2); \
    } \
    return PDC_ERROR; \
  } while (0)

/*
 * If *em is CheckAndSet, create a string copy of the string
 * that goes from begin to end-1.  Must have a no_space label.
 */
#define PDC_STR_COPY(s_out, begin, end) \
  do { \
    if (*em == PDC_CheckAndSet && s_out) { \
      size_t wdth = end-begin;  \
      char* buf; \
      RBuf_t* rbuf; \
      if (!(rbuf = RMM_new_rbuf(pdc->rmm_nz))) { \
        goto no_space; \
      } \
      if (RBuf_RESERVE(rbuf, buf, char, wdth+1)) { \
       RMM_free_rbuf(rbuf); \
       goto no_space; \
      } \
      memcpy(buf, begin, wdth); \
      buf[wdth] = 0; \
      RMM_free_rbuf_keep_buf(rbuf, 0, 0); \
      s_out->str = buf; \
      s_out->len = wdth; \
    } \
  } while (0)

/* ================================================================================ */ 
/* READ MACROS */

#define PDC_AINT_FW_READ_FN(fn_name, targ_type, int_type, strtonum_fn, invalid_err, opt_tmp_test) \
PDC_error_t \
fn_name(PDC_t* pdc, PDC_base_em* em, size_t width, \
	PDC_base_ed* ed, targ_type* res_out, PDC_disc_t* disc) \
{ \
  unsigned char   ct;    /* char tmp */ \
  int_type        tmp;   /* tmp num */ \
  char*           tcp;   /* tmp char* */ \
  char*           begin; /* cursor at beginning of call */ \
  char*           end;   /* cursor just beyond width chars */ \
  PDC_base_em     emt = PDC_CheckAndSet; \
  PDC_base_ed     edt = {0}; \
 \
  if (!disc) { \
    disc = pdc->disc; \
  } \
  TRACE(pdc, MacroArg2String(fn_name) " called"); \
  if (!em) { \
    em = &emt; \
  } \
  if (!ed) { \
    ed = &edt; \
  } \
  if (width <= 0) { \
    WARN(pdc, "UNEXPECTED PARAM VALUE: " MacroArg2String(fn_name) " called with width <= 0"); \
    return PDC_ERROR; /* XXX mis-use of API -- unrecoverable/panic/advance cursor ??? */ \
  } \
  /* ensure there are width chars available */ \
  if (PDC_ERROR == PDC_IO_getchars(pdc, width, &begin, &end, disc)) { \
    goto width_not_avail; \
  } \
  if (!(disc->flags & PDC_WSPACE_OK) && isspace(*begin)) { \
    goto wspace_err; \
  } \
  ct = *end;    /* save */ \
  *end = 0;     /* null */ \
  tmp = strtonum_fn(begin, &tcp, 10); \
  *end = ct;    /* restore */ \
  if (tcp==0 || tcp==begin) { \
    goto bad_prefix_err; \
  } \
  while (tcp < end && (disc->flags & PDC_WSPACE_OK) && isspace(*tcp)) { \
    tcp++; \
  } \
  if (tcp != end) { \
    goto bad_suffix_err; \
  } \
  if (errno==ERANGE opt_tmp_test) { \
    goto range_err; \
  } \
  /* success */ \
  if (res_out && *em == PDC_CheckAndSet) { \
    *res_out = (targ_type)tmp; \
  } \
  return PDC_OK; \
 \
 width_not_avail: \
  HANDLE_ERR_CUR2ENDPOS(PDC_WIDTH_NOT_AVAILABLE); \
 \
 wspace_err: \
 bad_prefix_err: \
 bad_suffix_err: \
  HANDLE_ERR_MODPOS(invalid_err, width, 1); /* mod pos to start/end of number */ \
 \
 range_err: \
  HANDLE_ERR_MODPOS(PDC_RANGE, width, 1); /* mod pos to start/end of number */ \
}

#define PDC_BINT_READ_FN(fn_name, targ_type, width, swapmem_op) \
PDC_error_t \
fn_name(PDC_t* pdc, PDC_base_em* em, \
	PDC_base_ed* ed, targ_type* res_out, PDC_disc_t* disc) \
{ \
  char*           begin; /* cursor at beginning of call */ \
  PDC_base_em     emt = PDC_CheckAndSet; \
  PDC_base_ed     edt = {0}; \
 \
  if (!disc) { \
    disc = pdc->disc; \
  } \
  TRACE(pdc, MacroArg2String(fn_name) " called"); \
  if (!em) { \
    em = &emt; \
  } \
  if (!ed) { \
    ed = &edt; \
  } \
  /* ensure there are width chars available */ \
  if (PDC_ERROR == PDC_IO_getchars(pdc, width, &begin, 0, disc)) { \
    goto width_not_avail; \
  } \
  /* success */ \
  if (res_out && *em == PDC_CheckAndSet) { \
    if (disc->m_endian != disc->d_endian) { \
      swapmem(swapmem_op, begin, res_out, width); \
    } else { \
      swapmem(0, begin, res_out, width); \
    } \
  } \
  return PDC_OK; \
 \
 width_not_avail: \
  HANDLE_ERR_CUR2ENDPOS(PDC_WIDTH_NOT_AVAILABLE); \
}
