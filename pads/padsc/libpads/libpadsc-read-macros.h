#ifndef MacroArg2String
#define MacroArg2String(s) #s
#endif

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
    if (PDC_IO_peek_EOF(pdc, disc)) { \
      goto at_eof_err; \
    } \
    goto at_eol_err; \
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
 at_eof_err: \
  if (*em < PDC_Ignore) { \
    ed->errCode = PDC_AT_EOF; \
    /* set loc begin/end to current IO pos = 1 past end char */ \
    PDC_get_loc(pdc, &(ed->loc), disc); \
  } \
  return PDC_ERROR; \
 \
 at_eol_err: \
  if (*em < PDC_Ignore) { \
    ed->errCode = PDC_AT_EOL; \
    /* set loc begin/end to current IO pos = 1 past end of line */ \
    PDC_get_loc(pdc, &(ed->loc), disc); \
  } \
  return PDC_ERROR; \
 \
 wspace_err: \
 bad_prefix_err: \
 bad_suffix_err: \
  if (*em < PDC_Ignore) { \
    ed->errCode = invalid_err; \
    PDC_get_loc(pdc, &(ed->loc), disc); /* set loc begin/end to current IO pos */ \
    ed->loc.beginChar -= width;         /* move loc begin to start of number */  \
    ed->loc.endChar--;                  /* move loc end to end of number */  \
  } \
  return PDC_ERROR; \
 \
 range_err: \
  if (*em < PDC_Ignore) { \
    ed->errCode = PDC_RANGE; \
    PDC_get_loc(pdc, &(ed->loc), disc); /* set loc begin/end to current IO pos */ \
    ed->loc.beginChar -= width;         /* move loc begin to start of number */  \
    ed->loc.endChar--;                  /* move loc end to end of number */  \
  } \
  return PDC_ERROR; \
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
    if (PDC_IO_peek_EOF(pdc, disc)) { \
      goto at_eof_err; \
    } \
    goto at_eol_err; \
  } \
  /* success */ \
  if (res_out && *em == PDC_CheckAndSet) { \
    swapmem(swapmem_op, begin, res_out, width); \
  } \
  return PDC_OK; \
 \
 at_eof_err: \
  if (*em < PDC_Ignore) { \
    ed->errCode = PDC_AT_EOF; \
    /* set loc begin/end to current IO pos = 1 past end char */ \
    PDC_get_loc(pdc, &(ed->loc), disc); \
  } \
  return PDC_ERROR; \
 \
 at_eol_err: \
  if (*em < PDC_Ignore) { \
    ed->errCode = PDC_AT_EOL; \
    /* set loc begin/end to current IO pos = 1 past end of line */ \
    PDC_get_loc(pdc, &(ed->loc), disc); \
  } \
  return PDC_ERROR; \
}
