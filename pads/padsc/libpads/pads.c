## This source file is run through srcgen.pl to produce 
## a number of generated files:
##
##    libpadsc-macros-gen.h      : generally useful macros
##    libpadsc-read-macros-gen.h : macros that help implement read  functions
##    libpadsc-acc-macros-gen.h  : macros that help implement accum functions
##    libpadsc-misc-macros-gen.h : macros that help implement misc  functions
## 
##    libpadsc-read-gen.c        : generated read  functions
##    libpadsc-acc-gen.c         : generated accum functions
##    libpadsc-misc-gen.c        : generated misc  functions
##    libpadsc-gen.c             : the rest of the libpadsc library
##
/* ********************* BEGIN_MACROS(libpadsc-macros-gen.h) ********************** */
/*
 * Some generally useful macros
 * 
 * Kathleen Fisher, Robert Gruber
 * AT&T Labs Research
 */

#ifndef PDCI_MacroArg2String
#define PDCI_MacroArg2String(s) #s
#endif
/* ********************************** END_HEADER ********************************** */

/* ================================================================================ */
/* MACROS USED BY READ FUNCTIONS
 *
 * These macros assume em/ed have been set up
 */

/* eoff is one byte beyond last error byte, so sub 1 */
#define PDCI_READFN_SET_LOC_BE(boff, eoff)
  do {
    PDC_IO_getPos_internal(pdc, &(ed->loc.b), (boff));
    PDC_IO_getPos_internal(pdc, &(ed->loc.e), (eoff)-1);
  } while (0)
/* END_MACRO */

#define PDCI_READFN_SET_NULLSPAN_LOC(boff)
  do {
    PDC_IO_getPos_internal(pdc, &(ed->loc.b), (boff));
    ed->loc.e = ed->loc.b;
    if (ed->loc.e.byte) {
      (ed->loc.e.byte)--;
    }
  } while (0)
/* END_MACRO */

/* Assumes ed->loc has already been set */
#define PDCI_READFN_RET_ERRCODE_WARN(whatfn, msg, errcode)
  do {
    if (pdc->speclev == 0 && (*em < PDC_Ignore)) {
      ed->errCode = (errcode);
      if (!pdc->inestlev) {
	PDCI_report_err(pdc, PDC_WARN_FLAGS, &(ed->loc), (errcode), (whatfn), (msg));
      }
    }
    return PDC_ERR;
  } while (0)
/* END_MACRO */

/* Assumes ed->loc and ed->errCode have already been set */
#define PDCI_READFN_RET_EXIST_ERRCODE_WARN(whatfn, msg)
  do {
    if (pdc->speclev == 0 && (*em < PDC_Ignore)) {
      if (!pdc->inestlev) {
	PDCI_report_err(pdc, PDC_WARN_FLAGS, &(ed->loc), ed->errCode, (whatfn), (msg));
      }
    }
    return PDC_ERR;
  } while (0)
/* END_MACRO */

/* Assumes ed->loc has already been set, warning already issued */
#define PDCI_READFN_RET_ERRCODE_NOWARN(errcode)
  do {
    if (pdc->speclev == 0 && (*em < PDC_Ignore)) {
      ed->errCode = (errcode);
    }
    return PDC_ERR;
  } while (0)
/* END_MACRO */

/* Does not use ed->loc */
#define PDCI_READFN_RET_ERRCODE_FATAL(whatfn, msg, errcode)
  do {
    if (pdc->speclev == 0 && (*em < PDC_Ignore)) {
      ed->errCode = (errcode);
      PDCI_report_err(pdc, PDC_FATAL_FLAGS, 0, (errcode), (whatfn), (msg));
    }
    return PDC_ERR;
  } while (0)
/* END_MACRO */

/*
 * Starting alloc size for strings, even if initial string is smaller;
 * saves on later alloc calls when PDC_string field is re-used many
 * times with strings of different lengths.
 */ 
#define PDCI_STRING_HINT 128
/* END_MACRO */

/* PDC_string_mk_copy -- inline version.  Caller must provide fatal_alloc_err target */
#define PDCI_STR_CPY(s, b, wdth)
  do {
    if (!(s)->rbuf) {
      if (!((s)->rbuf = RMM_new_rbuf(pdc->rmm_nz))) {
	goto fatal_alloc_err;
      }
    }
    if (RBuf_reserve((s)->rbuf, (void**)&((s)->str), sizeof(char), (wdth)+1, PDCI_STRING_HINT)) {
      goto fatal_alloc_err;
    }
    strncpy((s)->str, (b), (wdth));
    (s)->str[wdth] = 0;
    (s)->len = (wdth);
    /* if ((s)->sharing) { PDC_WARN1(pdc->disc, "XXX_REMOVE copy: string %p is no longer sharing", (void*)(s)); } */
    (s)->sharing = 0;
  } while (0)
/* END_MACRO */

/* PDC_string_preserve -- inline version.  Caller must provide fatal_alloc_err target */
#define PDCI_STR_PRESERVE(s)
  do {
    char *shared_str;
    /* PDC_WARN3(pdc->disc, "XXX_REMOVE [%s:%d] preserve called on shared string %p", __FILE__, __LINE__, (void*)(s)); */
    /* if (!(s)->sharing) { PDC_WARN3(pdc->disc, "XXX_REMOVE [%s:%d] ... but string %p was not shared",__FILE__, __LINE__, (void*)(s)); } */
    if ((s)->sharing) {
      shared_str = (s)->str;
      PDCI_STR_CPY((s), shared_str, (s)->len);
    }
  } while (0)
/* END_MACRO */

/* Set up str sharing */
#define PDCI_STR_SHARE(s, b, wdth)
  do {
    (s)->str = (char*)(b);
    (s)->len = wdth;
    (s)->sharing = 1;
    /* PDC_WARN1(pdc->disc, "XXX_REMOVE string %p is now sharing", (void*)(s)); */
  } while (0)
/* END_MACRO */

/* If *em is CheckAndSet, point to or copy (depending on pdc->disc->copy_strings)
 * the string that goes from b to e-1.
 * Caller must provide fatal_alloc_err target
 */
#define PDCI_STR_SET(s, b, e)
  do {
    if (*em == PDC_CheckAndSet && (s)) {
      size_t wdth = (e)-(b); 
      if (pdc->disc->copy_strings) {
	PDCI_STR_CPY((s), (b), wdth);
      } else {
	PDCI_STR_SHARE((s), (b), wdth);
      }
    }
  } while (0)
/* END_MACRO */

/* copy and convert from ASCII to EBCDIC at same time.  Caller must provide fatal_alloc_err target */
#define PDCI_A2E_STR_CPY(s, b, wdth)
  do {
    int i;
    if (!(s)->rbuf) {
      if (!((s)->rbuf = RMM_new_rbuf(pdc->rmm_nz))) {
	goto fatal_alloc_err;
      }
    }
    if (RBuf_reserve((s)->rbuf, (void**)&((s)->str), sizeof(char), (wdth)+1, PDCI_STRING_HINT)) {
      goto fatal_alloc_err;
    }
    for (i = 0; i < wdth; i++) {
      (s)->str[i] = PDC_mod_ae_tab[(int)((b)[i])];
    }
    (s)->str[wdth] = 0;
    (s)->len = (wdth);
    /* if ((s)->sharing) { PDC_WARN1(pdc->disc, "XXX_REMOVE copy: string %p is no longer sharing", (void*)(s)); } */
    (s)->sharing = 0;
  } while (0)
/* END_MACRO */

/* ================================================================================ */
/* MACROS USED BY OTHER FUNCTIONS
 * i.e., functions that do not have ed and em params
 */

#define PDCI_DISC_INIT_CHECKS(prefix)
  do {
    if (!pdc)  {
      PDC_WARN(&PDC_default_disc, prefix ": null pdc param");
      return PDC_ERR;
    }
    if (!pdc->disc) {
      PDC_WARN(&PDC_default_disc, prefix ": null pdc->disc");
      return PDC_ERR;
    }
    PDC_TRACE(pdc->disc, prefix " called");
  } while (0)
/* END_MACRO */

#define PDCI_IODISC_INIT_CHECKS(prefix)
  do {
    if (!pdc)  {
      PDC_WARN(&PDC_default_disc, prefix ": null pdc param");
      return PDC_ERR;
    }
    if (!pdc->disc) {
      PDC_WARN(&PDC_default_disc, prefix ": null pdc->disc");
      return PDC_ERR;
    }
    PDC_TRACE(pdc->disc, prefix " called");
    if (!pdc->disc->io_disc) {
      PDC_WARN(pdc->disc, prefix ": IO discipline not installed");
      return PDC_ERR;
    }
  } while (0)
/* END_MACRO */

/* Assumes pdc and disc already checked */
#define PDCI_NULLPARAM_CHECK(prefix, param)
  do {
    if (!(param))  {
      PDC_WARN(pdc->disc, prefix ": param " PDCI_MacroArg2String(param) " must not be NULL");
      return PDC_ERR;
    }
  } while (0)
/* END_MACRO */

/* ================================================================================ */
/* MACROS USED BY ACCUM FUNCTIONS */
 
/* Useful constants */

#define PDCI_HALFMIN_INT64   -4611686018427387904LL
#define PDCI_HALFMAX_INT64    4611686018427387903LL
#define PDCI_HALFMAX_UINT64   9223372036854775807ULL
/* END_MACRO */

/* Fold Points : when should the running int64 / uint64 sum be folded into the average? */

#define PDCI_FOLD_MIN_INT8    -9223372036854775680LL  /* PDC_MIN_INT64 - PDC_MIN_INT8  */
#define PDCI_FOLD_MAX_INT8     9223372036854775680LL  /* PDC_MAX_INT64 - PDC_MAX_INT8  */
#define PDCI_FOLD_MIN_INT16   -9223372036854743040LL  /* PDC_MIN_INT64 - PDC_MIN_INT16 */
#define PDCI_FOLD_MAX_INT16    9223372036854743040LL  /* PDC_MAX_INT64 - PDC_MAX_INT16 */
#define PDCI_FOLD_MIN_INT32   -9223372034707292160LL  /* PDC_MIN_INT64 - PDC_MIN_INT32 */
#define PDCI_FOLD_MAX_INT32    9223372034707292160LL  /* PDC_MAX_INT64 - PDC_MAX_INT32 */

#define PDCI_FOLD_MAX_UINT8   18446744073709551488ULL  /* PDC_MAX_UINT64 - PDC_MAX_UINT8  */
#define PDCI_FOLD_MAX_UINT16  18446744073709518848ULL  /* PDC_MAX_UINT64 - PDC_MAX_UINT16 */
#define PDCI_FOLD_MAX_UINT32  18446744069414584320ULL  /* PDC_MAX_UINT64 - PDC_MAX_UINT32 */
/* END_MACRO */

/* Macros that test whether folding should occur, given new val v and running sum s */

#define PDCI_FOLDTEST_INT8(v, s)  (((s) < PDCI_FOLD_MIN_INT8)  || ((s) > PDCI_FOLD_MAX_INT8))
#define PDCI_FOLDTEST_INT16(v, s) (((s) < PDCI_FOLD_MIN_INT16) || ((s) > PDCI_FOLD_MAX_INT16))
#define PDCI_FOLDTEST_INT32(v, s) (((s) < PDCI_FOLD_MIN_INT32) || ((s) > PDCI_FOLD_MAX_INT32))
#define PDCI_FOLDTEST_INT32(v, s) (((s) < PDCI_FOLD_MIN_INT32) || ((s) > PDCI_FOLD_MAX_INT32))
#define PDCI_FOLDTEST_INT64(v, s) ( (((s) < 0) && ((v) < PDCI_HALFMIN_INT64)) ||
				   (((v) < 0) && ((s) < PDCI_HALFMIN_INT64)) ||
				   (((s) > 0) && ((v) > PDCI_HALFMAX_INT64)) ||
				   (((v) > 0) && ((s) > PDCI_HALFMAX_INT64)) )
#define PDCI_FOLDTEST_UINT8(v, s)  ((s) > PDCI_FOLD_MAX_UINT8)
#define PDCI_FOLDTEST_UINT16(v, s) ((s) > PDCI_FOLD_MAX_UINT16)
#define PDCI_FOLDTEST_UINT32(v, s) ((s) > PDCI_FOLD_MAX_UINT32)
#define PDCI_FOLDTEST_UINT64(v, s) ( ((s) > PDCI_HALFMAX_UINT64) || ((v) > PDCI_HALFMAX_UINT64) )
/* END_MACRO */

/* ================================================================================ */
/* DOUBLY-LINKED LIST HELPER MACROS */

#define PDC_SOME_ELTS(head) ((head)->next != (head))
#define PDC_FIRST_ELT(head) ((head)->next)
#define PDC_LAST_ELT(head)  ((head)->prev)
/* END_MACRO */

#define PDC_REMOVE_ELT(elt)
  do {
    (elt)->prev->next = (elt)->next;
    (elt)->next->prev = (elt)->prev;
  } while (0)
/* END_MACRO */

#define PDC_APPEND_ELT(head, elt)
  do {
    (elt)->prev = (head)->prev;
    (elt)->next = (head);
    (elt)->prev->next = (elt);
    (elt)->next->prev = (elt);
  } while (0)
/* END_MACRO */

#define PDC_PREPEND_ELT(head, elt)
  do {
    (elt)->prev = (head);
    (elt)->next = (head)->next;
    (elt)->prev->next = (elt);
    (elt)->next->prev = (elt);
  } while (0)
/* END_MACRO */

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

#define PDCI_AE_INT_READ_FN(fn_name, targ_type, bytes2num_fn, invalid_err, isspace_fn, isdigit_fn)

PDC_error_t
fn_name ## _internal (PDC_t *pdc, PDC_base_em *em,
		      PDC_base_ed *ed, targ_type *res_out)
{
  targ_type       tmp;   /* tmp num */
  PDC_byte        *begin, *p1, *p2, *end;
  int             eor, eof;
  size_t          bytes;

  PDC_TRACE(pdc->disc, PDCI_MacroArg2String(fn_name) "_internal called" );
  if (PDC_ERR == PDCI_IO_needbytes(pdc, &begin, &p1, &p2, &end, &eor, &eof, &bytes)) {
    goto fatal_nb_io_err;
  }
  if (bytes == 0) {
    goto at_eor_or_eof_err;
  }
  switch (*em) {
  case PDC_Ignore:
    {
      /* move beyond anything that looks like an ascii number, return PDC_ERR if none such */
      if (isspace_fn(*p1) && !(pdc->disc->flags & PDC_WSPACE_OK)) {
	return PDC_ERR;
      }
      while (isspace_fn(*p1)) { /* skip spaces, if any */
	p1++;
	if (p1 == end) {
	  if (eor|eof) { return PDC_ERR; } /* did not find a digit */
	  if (PDC_ERR == PDCI_IO_morebytes(pdc, &begin, &p1, &p2, &end, &eor, &eof, &bytes)) {
	    goto fatal_mb_io_err;
	  }
	  if (bytes == 0) { return PDC_ERR; } /* all spaces, did not find a digit */
	}
      }
      if ('-' == (*p1) || '+' == (*p1)) { /* skip +/-, if any */
	p1++;
	if (p1 == end) {
	  if (eor|eof) { return PDC_ERR; } /* did not find a digit */
	  if (PDC_ERR == PDCI_IO_morebytes(pdc, &begin, &p1, &p2, &end, &eor, &eof, &bytes)) {
	    goto fatal_mb_io_err;
	  }
	  if (bytes == 0) { return PDC_ERR; } /* did not find a digit */
	}
      }
      if (!isdigit_fn(*p1)) {
	return PDC_ERR; /* did not find a digit */
      }
      /* all set: skip digits, move IO cursor, and return PDC_OK */
      while (isdigit_fn(*p1)) {
	p1++;
	if (p1 == end && !(eor|eof)) {
	  if (PDC_ERR == PDCI_IO_morebytes(pdc, &begin, &p1, &p2, &end, &eor, &eof, &bytes)) {
	    goto fatal_mb_io_err;
	  }
	  if (bytes == 0) { break; }
	}
      }
      if (PDC_ERR == PDCI_IO_forward(pdc, p1-begin)) {
	goto fatal_forward_err;
      }
      ed->errCode = PDC_NO_ERR;
      return PDC_OK;
    }
  case PDC_Check:
  case PDC_CheckAndSet:
    {
      if (isspace_fn(*p1) && !(pdc->disc->flags & PDC_WSPACE_OK)) {
	goto invalid_wspace;
      }
      while (!(eor|eof)) { /* find a non-space */ 
	while (isspace_fn(*p1)) { p1++; }
	if (p1 < end) { break; } 
	if (PDC_ERR == PDCI_IO_morebytes(pdc, &begin, &p1, &p2, &end, &eor, &eof, &bytes)) {
	  goto fatal_mb_io_err;
	}
	if (bytes == 0) { /* all spaces! */
	  goto invalid;
	}
      }
      if (!(eor|eof) && ('-' == (*p1) || '+' == (*p1))) { p1++; }
      while (!(eor|eof)) { /* find a non-digit */
	while (isdigit_fn(*p1)) { p1++; }
	if (p1 < end) { break; }
	if (PDC_ERR == PDCI_IO_morebytes(pdc, &begin, &p1, &p2, &end, &eor, &eof, &bytes)) {
	  goto fatal_mb_io_err;
	}
      }
      /* Either eor|eof, or found non-digit before end.  Thus, */
      /* the range [begin, end] is now set up for the strtonum function */
      tmp = bytes2num_fn(pdc, begin, &p1);
      if (errno == EINVAL) goto invalid;
      if (errno == ERANGE) goto range_err;
      /* success */
      if (PDC_ERR == PDCI_IO_forward(pdc, p1-begin)) {
	goto fatal_forward_err;
      }
      if (res_out && *em == PDC_CheckAndSet) {
	(*res_out) = tmp;
      }
      ed->errCode = PDC_NO_ERR;
      return PDC_OK;
    }
  }

 at_eor_or_eof_err:
  PDCI_READFN_SET_NULLSPAN_LOC(0);
  PDCI_READFN_RET_ERRCODE_WARN("[in " PDCI_MacroArg2String(fn_name) "]", 0, eor ? PDC_AT_EOR : PDC_AT_EOF);

 invalid_wspace:
  PDCI_READFN_SET_LOC_BE(0, 1);
  PDCI_READFN_RET_ERRCODE_WARN("[in " PDCI_MacroArg2String(fn_name) "]", "spaces not allowed in a_int field unless flag PDC_WSPACE_OK is set", invalid_err);

 invalid:
  PDCI_READFN_SET_LOC_BE(0, p1-begin);
  PDCI_READFN_RET_ERRCODE_WARN("[in " PDCI_MacroArg2String(fn_name) "]", 0, invalid_err);

 range_err:
  /* range error still consumes the number */
  PDCI_READFN_SET_LOC_BE(0, p1-begin);
  if (PDC_ERR == PDCI_IO_forward(pdc, p1-begin)) {
    goto fatal_forward_err;
  }
  PDCI_READFN_RET_ERRCODE_WARN("[in " PDCI_MacroArg2String(fn_name) "]", 0, PDC_RANGE);

 fatal_nb_io_err:
  PDCI_READFN_RET_ERRCODE_FATAL("[in " PDCI_MacroArg2String(fn_name) "_internal]", "IO error (nb)", PDC_IO_ERR);

 fatal_mb_io_err:
  PDCI_READFN_RET_ERRCODE_FATAL("[in " PDCI_MacroArg2String(fn_name) "_internal]", "IO error (mb)", PDC_IO_ERR);

 fatal_forward_err:
  PDCI_READFN_RET_ERRCODE_FATAL("[in " PDCI_MacroArg2String(fn_name) "_internal]", "IO_forward error", PDC_FORWARD_ERR);
}

PDC_error_t
fn_name(PDC_t *pdc, PDC_base_em *em,
	PDC_base_ed *ed, targ_type *res_out)
{
  PDC_base_em     emt = PDC_CheckAndSet;
  PDC_base_ed     edt;

  if (!em) {
    em = &emt;
  }
  if (!ed) {
    ed = &edt;
  }
  PDCI_IODISC_INIT_CHECKS( PDCI_MacroArg2String(fn_name) );
  return fn_name ## _internal (pdc, em, ed, res_out);
}
/* END_MACRO */

#define PDCI_AE_INT_FW_READ_FN(fn_name, targ_type, bytes2num_fn, invalid_err, isspace_fn)
PDC_error_t
fn_name ## _internal (PDC_t *pdc, PDC_base_em *em, size_t width,
		      PDC_base_ed *ed, targ_type *res_out)
{
  PDC_byte        ct;    /* char tmp */
  targ_type       tmp;   /* tmp num */
  PDC_byte        *begin, *p1, *p2, *end;
  int             eor, eof;
  size_t          bytes;

  PDC_TRACE(pdc->disc, PDCI_MacroArg2String(fn_name) "_internal called" );
  if (width <= 0) {
    PDC_WARN(pdc->disc, "UNEXPECTED PARAM VALUE: " PDCI_MacroArg2String(fn_name) " called with width <= 0");
    goto bad_param_err;
  }
  if (PDC_ERR == PDCI_IO_needbytes(pdc, &begin, &p1, &p2, &end, &eor, &eof, &bytes)) {
    goto fatal_nb_io_err;
  }
  while (end-begin < width) {
    if ((eor|eof) || bytes == 0) {
      goto width_not_avail;
    }
    if (PDC_ERR == PDCI_IO_morebytes(pdc, &begin, &p1, &p2, &end, &eor, &eof, &bytes)) {
      goto fatal_mb_io_err;
    }
  }
  /* end-begin >= width */
  if (*em == PDC_Check || *em == PDC_CheckAndSet) {
    end = begin + width;
    if (isspace_fn(*begin) && !(pdc->disc->flags & PDC_WSPACE_OK)) {
      goto invalid_wspace;
    }
    ct = *end;    /* save */
    *end = 0;     /* null */
    tmp = bytes2num_fn(pdc, begin, &p1);
    *end = ct;    /* restore */
    if (errno == EINVAL) goto invalid;
    if (p1 < end && isspace_fn(*p1)) {
      if (!(pdc->disc->flags & PDC_WSPACE_OK)) {
	goto invalid_wspace;
      }
      do { p1++; } while (p1 < end && isspace_fn(*p1));
    }
    if (p1 != end) {
      goto invalid;
    }
    if (errno == ERANGE) goto range_err;
  }
  /* success */
  if (PDC_ERR == PDCI_IO_forward(pdc, width)) {
    goto fatal_forward_err;
  }
  if (res_out && *em == PDC_CheckAndSet) {
    (*res_out) = tmp;
  }
  ed->errCode = PDC_NO_ERR;
  return PDC_OK;

 bad_param_err:
  PDCI_READFN_SET_NULLSPAN_LOC(0);
  PDCI_READFN_RET_ERRCODE_WARN("[in " PDCI_MacroArg2String(fn_name) "]", 0, PDC_BAD_PARAM);

 width_not_avail:
  /* FW field: eat the space whether or not there is an error */
  PDCI_READFN_SET_LOC_BE(0, end-begin);
  if (PDC_ERR == PDCI_IO_forward(pdc, end-begin)) {
    goto fatal_forward_err;
  }
  PDCI_READFN_RET_ERRCODE_WARN("[in " PDCI_MacroArg2String(fn_name) "]", 0, PDC_WIDTH_NOT_AVAILABLE);

 invalid:
  /* FW field: eat the space whether or not there is an error */
  PDCI_READFN_SET_LOC_BE(0, width);
  if (PDC_ERR == PDCI_IO_forward(pdc, width)) {
    goto fatal_forward_err;
  }
  PDCI_READFN_RET_ERRCODE_WARN("[in " PDCI_MacroArg2String(fn_name) "]", 0, invalid_err);

 invalid_wspace:
  /* FW field: eat the space whether or not there is an error */
  PDCI_READFN_SET_LOC_BE(0, width);
  if (PDC_ERR == PDCI_IO_forward(pdc, width)) {
    goto fatal_forward_err;
  }
  PDCI_READFN_RET_ERRCODE_WARN("[in " PDCI_MacroArg2String(fn_name) "]", "spaces not allowed in a_int field unless flag PDC_WSPACE_OK is set", invalid_err);

 range_err:
  /* FW field: eat the space whether or not there is an error */
  PDCI_READFN_SET_LOC_BE(0, width);
  if (PDC_ERR == PDCI_IO_forward(pdc, width)) {
    goto fatal_forward_err;
  }
  PDCI_READFN_RET_ERRCODE_WARN("[in " PDCI_MacroArg2String(fn_name) "]", 0, PDC_RANGE);

  /* fatal_alloc_err:
     PDCI_READFN_RET_ERRCODE_FATAL("[in " PDCI_MacroArg2String(fn_name) "_internal]", "Memory alloc err", PDC_ALLOC_ERR); */

 fatal_nb_io_err:
  PDCI_READFN_RET_ERRCODE_FATAL("[in " PDCI_MacroArg2String(fn_name) "_internal]", "IO error (nb)", PDC_IO_ERR);

 fatal_mb_io_err:
  PDCI_READFN_RET_ERRCODE_FATAL("[in " PDCI_MacroArg2String(fn_name) "_internal]", "IO error (mb)", PDC_IO_ERR);

 fatal_forward_err:
  PDCI_READFN_RET_ERRCODE_FATAL("[in " PDCI_MacroArg2String(fn_name) "_internal]", "IO_forward error", PDC_FORWARD_ERR);
}

PDC_error_t
fn_name(PDC_t *pdc, PDC_base_em *em, size_t width,
	PDC_base_ed *ed, targ_type *res_out)
{
  PDC_base_em     emt = PDC_CheckAndSet;
  PDC_base_ed     edt;

  if (!em) {
    em = &emt;
  }
  if (!ed) {
    ed = &edt;
  }
  PDCI_IODISC_INIT_CHECKS( PDCI_MacroArg2String(fn_name) );
  return fn_name ## _internal (pdc, em, width, ed, res_out);
}
/* END_MACRO */

#define PDCI_B_INT_READ_FN(fn_name, targ_type, width, swapmem_op)
PDC_error_t
fn_name ## _internal (PDC_t *pdc, PDC_base_em *em,
		      PDC_base_ed *ed, targ_type *res_out)
{
  PDC_byte        *begin, *p1, *p2, *end;
  int             eor, eof;
  size_t          bytes;

  PDC_TRACE(pdc->disc, PDCI_MacroArg2String(fn_name) "_internal called" );
  if (PDC_ERR == PDCI_IO_needbytes(pdc, &begin, &p1, &p2, &end, &eor, &eof, &bytes)) {
    goto fatal_nb_io_err;
  }
  while (end-begin < width) {
    if ((eor|eof) || bytes == 0) {
      goto width_not_avail;
    }
    if (PDC_ERR == PDCI_IO_morebytes(pdc, &begin, &p1, &p2, &end, &eor, &eof, &bytes)) {
      goto fatal_mb_io_err;
    }
  }
  /* end-begin >= width */
  if (res_out && *em == PDC_CheckAndSet) {
    if (pdc->m_endian != pdc->disc->d_endian) {
      swapmem(swapmem_op, begin, res_out, width);
    } else {
      swapmem(0, begin, res_out, width);
    }
  }
  if (PDC_ERR == PDCI_IO_forward(pdc, width)) {
    goto fatal_forward_err;
  }
  ed->errCode = PDC_NO_ERR;
  return PDC_OK;

 width_not_avail:
  PDCI_READFN_SET_LOC_BE(0, end-begin);
  PDCI_READFN_RET_ERRCODE_WARN("[in " PDCI_MacroArg2String(fn_name) "]", 0, PDC_WIDTH_NOT_AVAILABLE);

 fatal_nb_io_err:
  PDCI_READFN_RET_ERRCODE_FATAL("[in " PDCI_MacroArg2String(fn_name) "_internal]", "IO error (nb)", PDC_IO_ERR);

 fatal_mb_io_err:
  PDCI_READFN_RET_ERRCODE_FATAL("[in " PDCI_MacroArg2String(fn_name) "_internal]", "IO error (mb)", PDC_IO_ERR);

 fatal_forward_err:
  PDCI_READFN_RET_ERRCODE_FATAL("[in " PDCI_MacroArg2String(fn_name) "_internal]", "IO_forward error", PDC_FORWARD_ERR);
}

PDC_error_t
fn_name(PDC_t *pdc, PDC_base_em *em,
	PDC_base_ed *ed, targ_type *res_out)
{
  PDC_base_em     emt = PDC_CheckAndSet;
  PDC_base_ed     edt;

  if (!em) {
    em = &emt;
  }
  if (!ed) {
    ed = &edt;
  }
  PDCI_IODISC_INIT_CHECKS( PDCI_MacroArg2String(fn_name) );
  return fn_name ## _internal (pdc, em, ed, res_out);
}
/* END_MACRO */

#define PDCI_EBCBCDSB_INT_READ_FN(fn_name, targ_type, bytes2num_fn, invalid_err, width)
PDC_error_t
fn_name ## _internal (PDC_t *pdc, PDC_base_em *em, PDC_uint32 num_digits_or_bytes,
		      PDC_base_ed *ed, targ_type *res_out)
{
  targ_type       tmp;   /* tmp num */
  PDC_byte        *begin, *p1, *p2, *end;
  int             eor, eof;
  size_t          bytes;

  PDC_TRACE(pdc->disc, PDCI_MacroArg2String(fn_name) "_internal called" );
  if (PDC_ERR == PDCI_IO_needbytes(pdc, &begin, &p1, &p2, &end, &eor, &eof, &bytes)) {
    goto fatal_nb_io_err;
  }
  while (end-begin < width) {
    if ((eor|eof) || bytes == 0) {
      goto width_not_avail;
    }
    if (PDC_ERR == PDCI_IO_morebytes(pdc, &begin, &p1, &p2, &end, &eor, &eof, &bytes)) {
      goto fatal_mb_io_err;
    }
  }
  /* end-begin >= width */
  if (*em == PDC_Check || *em == PDC_CheckAndSet) {
    tmp = bytes2num_fn(pdc, begin, num_digits_or_bytes, &p1);
    if (errno) goto invalid_range_dom;
  }
  /* success */
  if (PDC_ERR == PDCI_IO_forward(pdc, width)) {
    goto fatal_forward_err;
  }
  if (res_out && *em == PDC_CheckAndSet) {
    (*res_out) = tmp;
  }
  ed->errCode = PDC_NO_ERR;
  return PDC_OK;

 invalid_range_dom:
  /* FW field: eat the space whether or not there is an error */
  PDCI_READFN_SET_LOC_BE(0, width);
  if (PDC_ERR == PDCI_IO_forward(pdc, width)) {
    goto fatal_forward_err;
  }
  switch (errno) {
  case EINVAL:
    PDCI_READFN_RET_ERRCODE_WARN("[in " PDCI_MacroArg2String(fn_name) "]", 0, invalid_err);
  case ERANGE:
    PDCI_READFN_RET_ERRCODE_WARN("[in " PDCI_MacroArg2String(fn_name) "]", 0, PDC_RANGE);
  case EDOM:
    PDCI_READFN_RET_ERRCODE_WARN("[in " PDCI_MacroArg2String(fn_name) "]", 0, PDC_BAD_PARAM);
  }

 width_not_avail:
  PDCI_READFN_SET_LOC_BE(0, end-begin);
  PDCI_READFN_RET_ERRCODE_WARN("[in " PDCI_MacroArg2String(fn_name) "]", 0, PDC_WIDTH_NOT_AVAILABLE);

 fatal_nb_io_err:
  PDCI_READFN_RET_ERRCODE_FATAL("[in " PDCI_MacroArg2String(fn_name) "_internal]", "IO error (nb)", PDC_IO_ERR);

 fatal_mb_io_err:
  PDCI_READFN_RET_ERRCODE_FATAL("[in " PDCI_MacroArg2String(fn_name) "_internal]", "IO error (mb)", PDC_IO_ERR);

 fatal_forward_err:
  PDCI_READFN_RET_ERRCODE_FATAL("[in " PDCI_MacroArg2String(fn_name) "_internal]", "IO_forward error", PDC_FORWARD_ERR);
}

PDC_error_t
fn_name(PDC_t *pdc, PDC_base_em *em, PDC_uint32 num_digits_or_bytes,
	PDC_base_ed *ed, targ_type *res_out)
{
  PDC_base_em     emt = PDC_CheckAndSet;
  PDC_base_ed     edt;

  if (!em) {
    em = &emt;
  }
  if (!ed) {
    ed = &edt;
  }
  PDCI_IODISC_INIT_CHECKS( PDCI_MacroArg2String(fn_name) );
  return fn_name ## _internal (pdc, em, num_digits_or_bytes, ed, res_out);
}
/* END_MACRO */

#define PDCI_EBCBCDSB_FPOINT_READ_FN(fn_name, targ_type, internal_numerator_read_fn, width, dexp_max)
PDC_error_t
fn_name ## _internal (PDC_t *pdc, PDC_base_em *em, PDC_uint32 num_digits_or_bytes, PDC_uint32 d_exp,
		      PDC_base_ed *ed, targ_type *res_out)
{
  targ_type       tmp;   /* tmp num */

  PDC_TRACE(pdc->disc, PDCI_MacroArg2String(fn_name) "_internal called" );
  (pdc->inestlev)++;
  if (PDC_ERR == internal_numerator_read_fn(pdc, em, num_digits_or_bytes, ed, &(tmp.num))) {
    /* ed filled in already, IO cursor advanced if appropriate */
    (pdc->inestlev)--;
    PDCI_READFN_RET_EXIST_ERRCODE_WARN("[in " PDCI_MacroArg2String(fn_name) "]", 0);
  }
  (pdc->inestlev)--;
  /* so far so good, IO cursor has been advanced, ed->errCode set to PDC_NO_ERR */
  if (d_exp > dexp_max) {
    PDCI_READFN_SET_LOC_BE(-width, 0);
    PDCI_READFN_RET_ERRCODE_WARN("[in " PDCI_MacroArg2String(fn_name) "]", 0, PDC_BAD_PARAM);
  }
  if (res_out && *em == PDC_CheckAndSet) {
    tmp.denom = PDCI_10toThe[d_exp];
    (*res_out) = tmp;
  }
  return PDC_OK;
}

PDC_error_t
fn_name(PDC_t *pdc, PDC_base_em *em, PDC_uint32 num_digits_or_bytes, PDC_uint32 d_exp,
	PDC_base_ed *ed, targ_type *res_out)
{
  PDC_base_em     emt = PDC_CheckAndSet;
  PDC_base_ed     edt;

  if (!em) {
    em = &emt;
  }
  if (!ed) {
    ed = &edt;
  }
  PDCI_IODISC_INIT_CHECKS( PDCI_MacroArg2String(fn_name) );
  return fn_name ## _internal (pdc, em, num_digits_or_bytes, d_exp, ed, res_out);
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

#define PDCI_INT_ACCUM(int_type, int_descr, num_bytes, fmt, fold_test)

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
int_type ## _dt_elt_oset_cmp(Dt_t *dt, int_type ## _dt_key_t *a, int_type ## _dt_key_t *b, Dtdisc_t *disc)
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
int_type ## _dt_elt_set_cmp(Dt_t *dt, int_type ## _dt_key_t *a, int_type ## _dt_key_t *b, Dtdisc_t *disc)
{
  NoP(dt);
  NoP(disc);
  if (a->val == b->val) {
    return 0;
  }
  return 1;
}

void*
int_type ## _dt_elt_make(Dt_t *dt, int_type ## _dt_elt_t *a, Dtdisc_t *disc)
{
  int_type ## _dt_elt_t *b;
  if ((b = oldof(0, int_type ## _dt_elt_t, 1, 0))) {
    b->key.val  = a->key.val;
    b->key.cnt  = a->key.cnt;
  }
  return b;
}

void
int_type ## _dt_elt_free(Dt_t *dt, int_type ## _dt_elt_t *a, Dtdisc_t *disc)
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
int_type ## _acc_init(PDC_t *pdc, int_type ## _acc *a)
{
  PDCI_DISC_INIT_CHECKS( PDCI_MacroArg2String(int_type) "_acc_init" );
  PDCI_NULLPARAM_CHECK( PDCI_MacroArg2String(int_type) "_acc_init" , a );
  if (!(a->dict = dtopen(&int_type ## _acc_dt_set_disc, Dtset))) {
    return PDC_ERR;
  }
  a->good = a->bad = a->fold = a->tracked = a->psum = a->avg = a->min = a->max = 0;
  return PDC_OK;
}

PDC_error_t
int_type ## _acc_reset(PDC_t *pdc, int_type ## _acc *a)
{
  PDCI_DISC_INIT_CHECKS( PDCI_MacroArg2String(int_type) "_acc_reset" );
  PDCI_NULLPARAM_CHECK( PDCI_MacroArg2String(int_type) "_acc_reset" , a );
  if (!a->dict) {
    return PDC_ERR;
  }
  dtclear(a->dict);
  a->good = a->bad = a->fold = a->tracked = a->psum = a->avg = a->min = a->max = 0;
  return PDC_OK;
}

PDC_error_t
int_type ## _acc_cleanup(PDC_t *pdc, int_type ## _acc *a)
{
  PDCI_DISC_INIT_CHECKS( PDCI_MacroArg2String(int_type) "_acc_cleanup" );
  PDCI_NULLPARAM_CHECK( PDCI_MacroArg2String(int_type) "_acc_cleanup" , a );
  if (a->dict) {
    dtclose(a->dict);
    a->dict = 0;
  }
  return PDC_OK;
}

void
int_type ## _acc_fold_psum(int_type ## _acc *a) {
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
int_type ## _acc_add(PDC_t *pdc, int_type ## _acc *a, PDC_base_ed *ed, int_type *val)
{
  int_type               v          = (*val);
  int_type ## _dt_elt_t  insert_elt;
  int_type ## _dt_key_t  lookup_key;
  int_type ## _dt_elt_t  *tmp1;
  PDCI_DISC_INIT_CHECKS( PDCI_MacroArg2String(int_type) "_acc_add" );
  PDCI_NULLPARAM_CHECK( PDCI_MacroArg2String(int_type) "_acc_add" , a );
  PDCI_NULLPARAM_CHECK( PDCI_MacroArg2String(int_type) "_acc_add" , ed );
  PDCI_NULLPARAM_CHECK( PDCI_MacroArg2String(int_type) "_acc_add" , val );
  if (!a->dict) {
    return PDC_ERR;
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
  if (dtsize(a->dict) < PDCI_ACC_MAX2TRACK) {
    insert_elt.key.val = v;
    insert_elt.key.cnt = 0;
    if (!(tmp1 = dtinsert(a->dict, &insert_elt))) {
      PDC_WARN(pdc->disc, "** PADC internal error: dtinsert failed (out of memory?) **");
      return PDC_ERR;
    }
    (tmp1->key.cnt)++;
    (a->tracked)++;
  } else {
    lookup_key.val = v;
    lookup_key.cnt = 0;
    if ((tmp1 = dtmatch(a->dict, (Void_t*)&lookup_key))) {
      (tmp1->key.cnt)++;
      (a->tracked)++;
    }
  }
  return PDC_OK;
}

PDC_error_t
int_type ## _acc_report_internal(PDC_t *pdc, Sfio_t *outstr, const char *prefix, const char *what, int nst,
				 int_type ## _acc *a)
{
  int                   i = 0, sz, rp;
  PDC_uint64            cnt_sum = 0;
  double                bad_pcnt;
  double                track_pcnt;
  double                cnt_sum_pcnt;
  double                elt_pcnt;
  Void_t                *velt;
  int_type ## _dt_elt_t *elt;

  PDC_TRACE(pdc->disc, PDCI_MacroArg2String(int_type) "_acc_report_internal called" );
  if (!prefix || *prefix == 0) {
    prefix = "<top>";
  }
  if (!what) {
    what = int_descr;
  }
  PDCI_nst_prefix_what(outstr, &nst, prefix, what);
  if (a->good == 0) {
    bad_pcnt = (a->bad == 0) ? 0.0 : 100.0;
  } else {
    bad_pcnt = 100.0 * (a->bad / (double)(a->good + a->bad));
  }
  sfprintf(outstr, "good vals: %10llu    bad vals: %10llu    pcnt-bad: %8.3lf\n",
	   a->good, a->bad, bad_pcnt);
  if (a->good == 0) {
    return PDC_OK;
  }
  int_type ## _acc_fold_psum(a);
  sz = dtsize(a->dict);
  rp = (sz < PDCI_ACC_REPORT_K) ? sz : PDCI_ACC_REPORT_K;
  dtdisc(a->dict,   &int_type ## _acc_dt_oset_disc, DT_SAMEHASH); /* change cmp function */
  dtmethod(a->dict, Dtoset); /* change to ordered set -- establishes an ordering */
  sfprintf(outstr, "  Characterizing %s:  min %" fmt, what, a->min);
  sfprintf(outstr, " max %" fmt, a->max);
  sfprintf(outstr, " avg %.3lf\n", a->avg);
  sfprintf(outstr, "    => distribution of top %d values out of %d distinct values:\n", rp, sz);
  if (sz == PDCI_ACC_MAX2TRACK && a->good > a->tracked) {
    track_pcnt = 100.0 * (a->tracked/(double)a->good);
    sfprintf(outstr, "        (* hit tracking limit, tracked %.3lf pcnt of all values *) \n", track_pcnt);
  }
  for (velt = dtfirst(a->dict); velt && i < PDCI_ACC_REPORT_K; velt = dtnext(a->dict, velt), i++) {
    elt = (int_type ## _dt_elt_t*)velt;
    cnt_sum += elt->key.cnt;
    elt_pcnt = 100.0 * (elt->key.cnt/(double)a->good);
    sfprintf(outstr, "        val: %10" fmt, elt->key.val);
    sfprintf(outstr, " count: %10llu  pcnt-of-good-vals: %8.3lf\n", elt->key.cnt, elt_pcnt);

  }
  cnt_sum_pcnt = 100.0 * (cnt_sum/(double)a->good);
  sfprintf(outstr,   ". . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .\n");
  sfprintf(outstr,   "        SUMMING         count: %10llu  pcnt-of-good-vals: %8.3lf\n",
	   cnt_sum, cnt_sum_pcnt);
  /* revert to unordered set in case more inserts will occur after this report */
  dtmethod(a->dict, Dtset); /* change to unordered set */
  dtdisc(a->dict,   &int_type ## _acc_dt_set_disc, DT_SAMEHASH); /* change cmp function */
  return PDC_OK;
}

PDC_error_t
int_type ## _acc_report(PDC_t *pdc, const char *prefix, const char *what, int nst,
			int_type ## _acc *a)
{
  Sfio_t *tmpstr;
  PDC_error_t res;
  PDCI_DISC_INIT_CHECKS( PDCI_MacroArg2String(int_type) "_acc_report" );
  PDCI_NULLPARAM_CHECK( PDCI_MacroArg2String(int_type) "_acc_report" , a );
  if (!pdc->disc->errorf) {
    return PDC_OK;
  }
  if (!(tmpstr = sfstropen ())) { 
    return PDC_ERR;
  }
  res = int_type ## _acc_report_internal(pdc, tmpstr, prefix, what, nst, a);
  if (res == PDC_OK) {
    pdc->disc->errorf(NiL, 0, "%s", sfstruse(tmpstr));
  }
  sfstrclose (tmpstr);
  return res;
}
/* END_MACRO */

#define PDCI_INT_ACCUM_REPORT_MAP(int_type, int_descr, fmt)
PDC_error_t
int_type ## _acc_report_map_internal(PDC_t *pdc, Sfio_t *outstr, const char *prefix, const char *what,  int nst,
				     int_type ## _map_fn fn, int_type ## _acc *a)
{
  size_t                pad;
  const char            *mapped_min;
  const char            *mapped_max;
  const char            *mapped_val;
  int                   i, sz, rp;
  PDC_uint64            cnt_sum = 0;
  double                bad_pcnt;
  double                track_pcnt;
  double                cnt_sum_pcnt;
  double                elt_pcnt;
  Void_t                *velt;
  int_type ## _dt_elt_t *elt;

  PDC_TRACE(pdc->disc, PDCI_MacroArg2String(int_type) "_acc_report_map_internal called" );
  if (!prefix || *prefix == 0) {
    prefix = "<top>";
  }
  if (!what) {
    what = int_descr;
  }
  PDCI_nst_prefix_what(outstr, &nst, prefix, what);
  if (a->good == 0) {
    bad_pcnt = (a->bad == 0) ? 0.0 : 100.0;
  } else {
    bad_pcnt = 100.0 * (a->bad / (double)(a->good + a->bad));
  }
  sfprintf(outstr, "good vals: %10llu    bad vals: %10llu    pcnt-bad: %8.3lf\n",
	   a->good, a->bad, bad_pcnt);
  if (a->good == 0) {
    return PDC_OK;
  }
  int_type ## _acc_fold_psum(a);
  sz = dtsize(a->dict);
  rp = (sz < PDCI_ACC_REPORT_K) ? sz : PDCI_ACC_REPORT_K;
  dtdisc(a->dict,   &int_type ## _acc_dt_oset_disc, DT_SAMEHASH); /* change cmp function */
  dtmethod(a->dict, Dtoset); /* change to ordered set -- establishes an ordering */
  mapped_min = fn(a->min);
  mapped_max = fn(a->max);
  sfprintf(outstr, "  Characterizing %s:  min %s (%5" fmt, what, mapped_min, a->min);
  sfprintf(outstr, ")  max %s (%5" fmt, mapped_max, a->max);
  sfprintf(outstr, ")\n");
  sfprintf(outstr, "    => distribution of top %d values out of %d distinct values:\n", rp, sz);
  if (sz == PDCI_ACC_MAX2TRACK && a->good > a->tracked) {
    track_pcnt = 100.0 * (a->tracked/(double)a->good);
    sfprintf(outstr, "        (* hit tracking limit, tracked %.3lf pcnt of all values *) \n", track_pcnt);
  }
  sz = rp = 0;
  for (i = 0, velt = dtfirst(a->dict); velt && i < PDCI_ACC_REPORT_K; velt = dtnext(a->dict, velt), i++) {
    elt = (int_type ## _dt_elt_t*)velt;
    sz = strlen(fn(elt->key.val));
    if (sz > rp) {
      rp = sz; 
    }
  }
  for (i = 0, velt = dtfirst(a->dict); velt && i < PDCI_ACC_REPORT_K; velt = dtnext(a->dict, velt), i++) {
    elt = (int_type ## _dt_elt_t*)velt;
    cnt_sum += elt->key.cnt;
    elt_pcnt = 100.0 * (elt->key.cnt/(double)a->good);
    mapped_val = fn(elt->key.val);
    sfprintf(outstr, "        val: %s (%5" fmt, mapped_val, elt->key.val);
    sfprintf(outstr, ") ");
    pad = rp-strlen(mapped_val);
    sfprintf(outstr, "%-.*s", pad,
	     "                                                                                ");
    sfprintf(outstr, "  count: %10llu  pcnt-of-good-vals: %8.3lf\n", elt->key.cnt, elt_pcnt);
  }
  cnt_sum_pcnt = 100.0 * (cnt_sum/(double)a->good);
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
int_type ## _acc_report_map(PDC_t *pdc, const char *prefix, const char *what, int nst,
			    int_type ## _map_fn fn, int_type ## _acc *a)
{
  Sfio_t *tmpstr;
  PDC_error_t res;
  PDCI_DISC_INIT_CHECKS( PDCI_MacroArg2String(int_type) "_acc_report_map" );
  PDCI_NULLPARAM_CHECK( PDCI_MacroArg2String(int_type) "_acc_report_map" , a );
  if (!pdc->disc->errorf) {
    return PDC_OK;
  }
  if (!(tmpstr = sfstropen ())) { 
    return PDC_ERR;
  }
  res = int_type ## _acc_report_map_internal(pdc, tmpstr, prefix, what, nst, fn, a);
  if (res == PDC_OK) {
    pdc->disc->errorf(NiL, 0, "%s", sfstruse(tmpstr));
  }
  sfstrclose (tmpstr);
  return res;
}
/* END_MACRO */

/* ********************************* BEGIN_TRAILER ******************************** */
/* ********************************** END_MACROS ********************************** */

/* ****************** BEGIN_MACROS(libpadsc-misc-macros-gen.h) ********************* */
/*
 * Macros that help implement accum functions
 * 
 * Kathleen Fisher, Robert Gruber
 * AT&T Labs Research
 */
/* ********************************** END_HEADER ********************************** */

#define PDCI_A2INT(fn_name, rev_fn_name, targ_type, int_min, int_max)
targ_type
fn_name(PDC_t *pdc, const PDC_byte *bytes, PDC_byte **ptr_out)
{
  int digit;
  int  neg = 0, range_err = 0;
  targ_type res = 0;

  while (PDCI_is_a_space(*bytes)) {
    bytes++;
  }
  if (*bytes == '+') {
    bytes++;
  } else if (*bytes == '-') {
    bytes++;
    neg = 1;
  }
  if (!PDCI_is_a_digit(*bytes)) {
    if (ptr_out) {
      (*ptr_out) = (PDC_byte*)bytes;
    }
    errno = EINVAL;
    return int_min;
  }
  while ((digit = PDCI_ascii_digit[*bytes]) != -1) {
    if (res < int_min ## _DIV10) {
      range_err = 1;
    }
    res = (res << 3) + (res << 1); /* res *= 10 */
    if (res < int_min + digit) {
      range_err = 1;
    }
    res -= digit;
    bytes++;
  }
  if (ptr_out) {
    (*ptr_out) = (PDC_byte*)bytes;
  }
  if (range_err) {
    errno = ERANGE;
    return neg ? int_min : int_max;
  }
  errno = 0;
  return neg ? res : - res;
}

int
rev_fn_name(PDC_t *pdc, Sfio_t *io, targ_type i)
{
  errno = 0;
  return sfprintf(io, "%I*d", sizeof(i), i);
}
/* END_MACRO */

#define PDCI_A2UINT(fn_name, rev_fn_name, targ_type, int_max)
targ_type
fn_name(PDC_t *pdc, const PDC_byte *bytes, PDC_byte **ptr_out)
{
  int digit;
  int  range_err = 0;
  targ_type res = 0;

  while (PDCI_is_a_space(*bytes)) {
    bytes++;
  }
  if (*bytes == '+') {
    bytes++;
  } else if (*bytes == '-') {
    bytes++;
    range_err = 1;
  }
  if (!PDCI_is_a_digit(*bytes)) {
    if (ptr_out) {
      (*ptr_out) = (PDC_byte*)bytes;
    }
    errno = EINVAL;
    return int_max;
  }
  while ((digit = PDCI_ascii_digit[*bytes]) != -1) {
    if (res > int_max ## _DIV10) {
      range_err = 1;
    }
    res = (res << 3) + (res << 1); /* res *= 10 */
    if (res > int_max - digit) {
      range_err = 1;
    }
    res += digit;
    bytes++;
  }
  if (ptr_out) {
    (*ptr_out) = (PDC_byte*)bytes;
  }
  if (range_err) {
    errno = ERANGE;
    return int_max;
  }
  errno = 0;
  return res;
}

int
rev_fn_name(PDC_t *pdc, Sfio_t *io, targ_type u)
{
  errno = 0;
  return sfprintf(io, "%I*u", sizeof(u), u);
}
/* END_MACRO */

#define PDCI_E2INT(fn_name, rev_fn_name, targ_type, int_min, int_max)
targ_type
fn_name(PDC_t *pdc, const PDC_byte *bytes, PDC_byte **ptr_out)
{
  int digit;
  int  neg = 0, range_err = 0;
  targ_type res = 0;

  while (PDCI_is_e_space(*bytes)) {
    bytes++;
  }
  if (*bytes == PDC_EBCDIC_PLUS) {
    bytes++;
  } else if (*bytes == PDC_EBCDIC_MINUS) {
    bytes++;
    neg = 1;
  }
  if (!PDCI_is_e_digit(*bytes)) {
    if (ptr_out) {
      (*ptr_out) = (PDC_byte*)bytes;
    }
    errno = EINVAL;
    return int_min;
  }
  while ((digit = PDCI_ebcdic_digit[*bytes]) != -1) {
    if (res < int_min ## _DIV10) {
      range_err = 1;
    }
    res = (res << 3) + (res << 1); /* res *= 10 */
    if (res < int_min + digit) {
      range_err = 1;
    }
    res -= digit;
    bytes++;
  }
  if (ptr_out) {
    (*ptr_out) = (PDC_byte*)bytes;
  }
  if (range_err) {
    errno = ERANGE;
    return neg ? int_min : int_max;
  }
  errno = 0;
  return neg ? res : - res;
}

int
rev_fn_name(PDC_t *pdc, Sfio_t *io, targ_type i)
{
  int j, writelen;
  char *buf;

  errno = 0;
  sfstrset(pdc->tmp, 0);
  if (-1 == (writelen = sfprintf(pdc->tmp, "%I*d", sizeof(i), i))) return -1;
  buf = sfstruse(pdc->tmp);
  for (j = 0; j < writelen; j++) {
    buf[j] = PDC_mod_ae_tab[(int)(buf[j])];
  }
  return sfwrite(io, buf, writelen);
}
/* END_MACRO */

#define PDCI_E2UINT(fn_name, rev_fn_name, targ_type, int_max)
targ_type
fn_name(PDC_t *pdc, const PDC_byte *bytes, PDC_byte **ptr_out)
{
  int digit;
  int range_err = 0;
  targ_type res = 0;

  while (PDCI_is_e_space(*bytes)) {
    bytes++;
  }
  if (*bytes == PDC_EBCDIC_PLUS) {
    bytes++;
  } else if (*bytes == PDC_EBCDIC_MINUS) {
    bytes++;
    range_err = 1;
  }
  if (!PDCI_is_e_digit(*bytes)) {
    if (ptr_out) {
      (*ptr_out) = (PDC_byte*)bytes;
    }
    errno = EINVAL;
    return int_max;
  }
  while ((digit = PDCI_ebcdic_digit[*bytes]) != -1) {
    if (res > int_max ## _DIV10) {
      range_err = 1;
    }
    res = (res << 3) + (res << 1); /* res *= 10 */
    if (res > int_max - digit) {
      range_err = 1;
    }
    res += digit;
    bytes++;
  }
  if (ptr_out) {
    (*ptr_out) = (PDC_byte*)bytes;
  }
  if (range_err) {
    errno = ERANGE;
    return int_max;
  }
  errno = 0;
  return res;
}

int
rev_fn_name(PDC_t *pdc, Sfio_t *io, targ_type u)
{
  int j, writelen;
  char *buf;

  errno = 0;
  sfstrset(pdc->tmp, 0);
  if (-1 == (writelen = sfprintf(pdc->tmp, "%I*u", sizeof(u), u))) return -1;
  buf = sfstruse(pdc->tmp);
  for (j = 0; j < writelen; j++) {
    buf[j] = PDC_mod_ae_tab[(int)(buf[j])];
  }
  return sfwrite(io, buf, writelen);
}
/* END_MACRO */

#define PDCI_EBC2INT(fn_name, rev_fn_name, targ_type, int_min, int_max, nd_max, act_nd_max)
targ_type
fn_name(PDC_t *pdc, const PDC_byte *bytes, PDC_uint32 num_digits, PDC_byte **ptr_out)
{
  PDC_int32 n = num_digits;
  targ_type res = 0;
  int neg, digit;

  if (ptr_out) {
    (*ptr_out) = (PDC_byte*)bytes + n;
  }
  if (n == 0 || n > nd_max) {
    errno = EDOM;
    return int_min;
  }
  neg = ((bytes[n-1]&0xF0) == 0xD0); /* look at sign nibble; C,F >=0; D < 0 */
  while (--n >= 0) {
    if ((digit = (0xF & *bytes)) > 9) {
      errno = EINVAL;
      return int_min;
    }
    if (res < int_min ## _DIV10) {
      goto range_err;
    }
    res = (res << 3) + (res << 1); /* res *= 10 */
    if (res < int_min + digit) {
      goto range_err;
    }
    res -= digit;
    bytes++;
  }
  errno = 0;
  return neg ? res : - res;
 range_err:
  errno = ERANGE;
  return neg ? int_min : int_max;
}

int
rev_fn_name(PDC_t *pdc, Sfio_t *io, targ_type i, PDC_uint32 num_digits)
{
  PDC_int32 n = num_digits;
  PDC_byte  ebc[30];
  int       neg = (i < 0);
  targ_type lim;

  if (n == 0 || n > nd_max) {
    errno = EDOM;
    return -1;
  }
  if (n < act_nd_max) {
    lim = PDCI_10toThe[n];
    if (i >= lim || (-i) >= lim) {
      errno = ERANGE;
      return -1;
    }
  }
  if (neg) {
    while (--n >= 0) {
      ebc[n] = 0xF0 | (-(i % 10));
      i /= 10;
    }
    ebc[num_digits-1] &= 0xDF; /* force sign nibble to negative */
  } else {
    while (--n >= 0) {
      ebc[n] = 0xF0 | (i % 10);
      i /= 10;
    }
  }
  errno = 0;
  return sfwrite(io, ebc, num_digits);
}
/* END_MACRO */

#define PDCI_EBC2UINT(fn_name, rev_fn_name, targ_type, int_max, nd_max)
targ_type
fn_name(PDC_t *pdc, const PDC_byte *bytes, PDC_uint32 num_digits, PDC_byte **ptr_out)
{
  PDC_int32 n = num_digits;
  targ_type res = 0;
  int digit;

  if (ptr_out) {
    (*ptr_out) = (PDC_byte*)bytes + n;
  }
  if (n == 0 || n > nd_max) {
    errno = EDOM;
    return int_max;
  }
  if ((bytes[n-1]&0xF0) == 0xD0) { /* look at sign nibble; C,F >=0; D < 0 */
    goto range_err;
  }
  while (--n >= 0) {
    if ((digit = (0xF & *bytes)) > 9) {
      errno = EINVAL;
      return int_max;
    }
    if (res > int_max ## _DIV10) {
      goto range_err;
    }
    res = (res << 3) + (res << 1); /* res *= 10 */
    if (res > int_max - digit) {
      goto range_err;
    }
    res += digit;
    bytes++;
  }
  errno = 0;
  return res;
 range_err:
  errno = ERANGE;
  return int_max;
}

int
rev_fn_name(PDC_t *pdc, Sfio_t *io, targ_type u, PDC_uint32 num_digits)
{
  PDC_int32 n = num_digits;
  PDC_byte  ebc[30];
  targ_type lim;

  if (n == 0 || n > nd_max) {
    errno = EDOM;
    return -1;
  }
  if (n < nd_max) {
    lim = PDCI_10toThe[n];
    if (u >= lim) {
      errno = ERANGE;
      return -1;
    }
  }
  while (--n >= 0) {
    ebc[n] = 0xF0 | (u % 10);
    u /= 10;
  }
  errno = 0;
  return sfwrite(io, ebc, num_digits);
}
/* END_MACRO */


#define PDCI_BCD2INT(fn_name, rev_fn_name, targ_type, int_min, int_max, nd_max, act_nd_max)
targ_type
fn_name(PDC_t *pdc, const PDC_byte *bytes, PDC_uint32 num_digits, PDC_byte **ptr_out)
{
  int  digit, two_digits;
  int  neg = 0;
  PDC_int32 num_bytes = ((num_digits+1) / 2);
  targ_type res = 0;

  if (ptr_out) {
    (*ptr_out) = (PDC_byte*)bytes + num_bytes;
  }
  neg = ((num_digits % 2 == 1) && ((bytes[num_bytes-1]&0xF) == 0xD)); /* look at sign nibble; C,F >=0; D < 0 */
  if (num_digits == 0 || num_digits > nd_max) {
    errno = EDOM;
    return int_min;
  }
  while (num_digits >= 2) {
    if (-1 == (two_digits = PDCI_bcd_hilo_digits[*bytes])) {
      if (ptr_out) {
	(*ptr_out) = (PDC_byte*)bytes;
      }
      errno = EINVAL;
      return int_min;
    }
    if (res < int_min ## _DIV100) {
      goto range_err;
    }
    res *= 100;
    if (res < int_min + two_digits) {
      goto range_err;
    }
    res -= two_digits;
    bytes++;
    num_digits -= 2;
  }
  if (num_digits) {
    if (-1 == (digit = PDCI_bcd_hi_digit[*bytes])) {
      errno = EINVAL;
      return int_min;
    }
    if (res < int_min ## _DIV10) {
      goto range_err;
    }
    res = (res << 3) + (res << 1); /* res *= 10 */
    if (res < int_min + digit) {
      goto range_err;
    }
    res -= digit;
    bytes++;
  }
  errno = 0;
  return neg ? res : - res;
 range_err:
  errno = ERANGE;
  return neg ? int_min : int_max;
}

int
rev_fn_name(PDC_t *pdc, Sfio_t *io, targ_type i, PDC_uint32 num_digits)
{
  PDC_byte  bcd[30];
  PDC_int32 num_bytes;
  int       x, n;
  int       neg = (i < 0);
  int       oddbytes = (num_digits % 2 == 1);
  targ_type lim;

  if (num_digits == 0 || num_digits > nd_max) {
    errno = EDOM;
    return -1;
  }
  if (num_digits < act_nd_max) {
    lim = PDCI_10toThe[num_digits];
    if (i >= lim || (-i) >= lim) {
      errno = ERANGE;
      return -1;
    }
  }
  num_bytes = ((num_digits+1) / 2);
  n = num_bytes - 1;
  if (neg) {
    if (!oddbytes) {  /* must use odd number of digits for negative number */
      errno = EDOM;
      return -1;
    }
    bcd[n] = ((-(i%10))<<4) | 0xD; /* force sign nibble to negative */
    n--;
    i /= 10;
    while (n >= 0) {
      x = -(i % 100);
      i /= 100;
      bcd[n--] = (x%10) | ((x/10)<<4);
    }
  } else { /* i positive */
    if (oddbytes) {
      bcd[n] = ((i%10)<<4);
      n--;
      i /= 10;
    }
    while (n >= 0) {
      x = i % 100;
      i /= 100;
      bcd[n--] = (x%10) | ((x/10)<<4);
    }
  }
  errno = 0;
  return sfwrite(io, bcd, num_bytes);
}
/* END_MACRO */

#define PDCI_BCD2UINT(fn_name, rev_fn_name, targ_type, int_max, nd_max)
targ_type
fn_name(PDC_t *pdc, const PDC_byte *bytes, PDC_uint32 num_digits, PDC_byte **ptr_out)
{
  int  digit, two_digits;
  PDC_int32 num_bytes = ((num_digits+1) / 2);
  targ_type res = 0;

  if (ptr_out) {
    (*ptr_out) = (PDC_byte*)bytes + num_bytes;
  }
  if (num_digits == 0 || num_digits > nd_max) {
    errno = EDOM;
    return int_max;
  }
  while (num_digits >= 2) {
    if (-1 == (two_digits = PDCI_bcd_hilo_digits[*bytes])) {
      if (ptr_out) {
	(*ptr_out) = (PDC_byte*)bytes;
      }
      errno = EINVAL;
      return int_max;
    }
    if (res > int_max ## _DIV100) {
      goto range_err;
    }
    res *= 100;
    if (res > int_max - two_digits) {
      goto range_err;
    }
    res += two_digits;
    bytes++;
    num_digits -= 2;
  }
  if (num_digits) {
    if (-1 == (digit = PDCI_bcd_hi_digit[*bytes])) {
      errno = EINVAL;
      return int_max;
    }
    if (res > int_max ## _DIV10) {
      goto range_err;
    }
    res = (res << 3) + (res << 1); /* res *= 10 */
    if (res > int_max - digit) {
      goto range_err;
    }
    res += digit;
    bytes++;
  }
  errno = 0;
  return res;
 range_err:
  errno = ERANGE;
  return int_max;
}

int
rev_fn_name(PDC_t *pdc, Sfio_t *io, targ_type u, PDC_uint32 num_digits)
{
  PDC_byte  bcd[30];
  PDC_int32 num_bytes;
  int       x, n;
  targ_type lim;

  if (num_digits == 0 || num_digits > nd_max) {
    errno = EDOM;
    return -1;
  }
  if (num_digits < nd_max) {
    lim = PDCI_10toThe[num_digits];
    if (u >= lim) {
      errno = ERANGE;
      return -1;
    }
  }
  num_bytes = ((num_digits+1) / 2);
  n = num_bytes - 1;
  if (num_digits % 2 == 1) {
    bcd[n--] = ((u%10)<<4);
    u /= 10;
  }
  while (n >= 0) {
    x = u % 100;
    u /= 100;
    bcd[n--] = (x%10) | ((x/10)<<4);
  }
  errno = 0;
  return sfwrite(io, bcd, num_bytes);
}
/* END_MACRO */

#define PDCI_SB2INT(fn_name, rev_fn_name, targ_type, sb_endian, int_min, int_max, nb_max)
targ_type
fn_name(PDC_t *pdc, const PDC_byte *bytes, PDC_uint32 num_bytes, PDC_byte **ptr_out)
{
  PDC_int32 n = num_bytes;
  targ_type res = 0;
  PDC_byte *resbytes = (PDC_byte*)(&res);

  if (ptr_out) {
    (*ptr_out) = (PDC_byte*)bytes + num_bytes;
  }
  if (n == 0 || n > nb_max) {
    errno = EDOM;
    return int_min;
  }
  if (pdc->m_endian == sb_endian) {
    /* on-disk order same as in-memory rep */
    memcpy(resbytes, bytes, n);
  } else {
    /* must reverse the order */
    while (--n >= 0) {
      resbytes[n] = *bytes++;
    }
  }
  errno = 0;
  return res;
}

int
rev_fn_name(PDC_t *pdc, Sfio_t *io, targ_type i, PDC_uint32 num_bytes)
{
  PDC_int32 n = num_bytes;
  PDC_byte  sb[30];
  PDC_byte *ibytes = (PDC_byte*)(&i);

  if (n == 0 || n > 8) {
    errno = EDOM;
    return -1;
  };
  if (i > PDC_MAX_FOR_NB[n] || i < PDC_MIN_FOR_NB[n]) {
    errno = ERANGE;
    return -1;
  }
  if (pdc->m_endian == sb_endian) {
    /* on-disk order same as in-memory rep */
    memcpy(sb, ibytes, n);
  } else {
    /* must reverse the order */
    while (--n >= 0) {
      sb[n] = *ibytes++;
    }
  }
  errno = 0;
  return sfwrite(io, sb, num_bytes);
}
/* END_MACRO */

#define PDCI_SB2UINT(fn_name, rev_fn_name, targ_type, sb_endian, int_max, nb_max)
targ_type
fn_name(PDC_t *pdc, const PDC_byte *bytes, PDC_uint32 num_bytes, PDC_byte **ptr_out)
{
  PDC_int32 n = num_bytes;
  targ_type res = 0;
  PDC_byte *resbytes = (PDC_byte*)(&res);

  if (ptr_out) {
    (*ptr_out) = (PDC_byte*)bytes + n;
  }
  if (n == 0 || n > nb_max) {
    errno = EDOM;
    return int_max;
  }
  if (pdc->m_endian == sb_endian) {
    /* on-disk order same as in-memory rep */
    memcpy(resbytes, bytes, n);
  } else {
    /* must reverse the order */
    while (--n >= 0) {
      resbytes[n] = *bytes++;
    }
  }
  errno = 0;
  return res;
}

int
rev_fn_name(PDC_t *pdc, Sfio_t *io, targ_type u, PDC_uint32 num_bytes)
{
  PDC_int32 n = num_bytes;
  PDC_byte sb[30];
  PDC_byte *ubytes = (PDC_byte*)(&u);

  if (n == 0 || n > 8) {
    errno = EDOM;
    return -1;
  };
  if (u > PDC_UMAX_FOR_NB[n]) {
    errno = ERANGE;
    return -1;
  }
  if (pdc->m_endian == sb_endian) {
    /* on-disk order same as in-memory rep */
    memcpy(sb, ubytes, n);
  } else {
    /* must reverse the order */
    while (--n >= 0) {
      sb[n] = *ubytes++;
    }
  }
  errno = 0;
  return sfwrite(io, sb, num_bytes);
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
 * PDCI_AE_INT_READ_FN(fn_name, targ_type, bytes2num_fn, invalid_err, isspace_fn, isdigit_fn)
 */

PDCI_AE_INT_READ_FN(PDC_a_int8_read,   PDC_int8,   PDCI_a2int8,   PDC_INVALID_A_NUM,  PDCI_is_a_space, PDCI_is_a_digit);
PDCI_AE_INT_READ_FN(PDC_a_int16_read,  PDC_int16,  PDCI_a2int16,  PDC_INVALID_A_NUM,  PDCI_is_a_space, PDCI_is_a_digit);
PDCI_AE_INT_READ_FN(PDC_a_int32_read,  PDC_int32,  PDCI_a2int32,  PDC_INVALID_A_NUM,  PDCI_is_a_space, PDCI_is_a_digit);
PDCI_AE_INT_READ_FN(PDC_a_int64_read,  PDC_int64,  PDCI_a2int64,  PDC_INVALID_A_NUM,  PDCI_is_a_space, PDCI_is_a_digit);
PDCI_AE_INT_READ_FN(PDC_a_uint8_read,  PDC_uint8,  PDCI_a2uint8,  PDC_INVALID_A_NUM, PDCI_is_a_space, PDCI_is_a_digit);
PDCI_AE_INT_READ_FN(PDC_a_uint16_read, PDC_uint16, PDCI_a2uint16, PDC_INVALID_A_NUM, PDCI_is_a_space, PDCI_is_a_digit);
PDCI_AE_INT_READ_FN(PDC_a_uint32_read, PDC_uint32, PDCI_a2uint32, PDC_INVALID_A_NUM, PDCI_is_a_space, PDCI_is_a_digit);
PDCI_AE_INT_READ_FN(PDC_a_uint64_read, PDC_uint64, PDCI_a2uint64, PDC_INVALID_A_NUM, PDCI_is_a_space, PDCI_is_a_digit);

/* ================================================================================ */
/* FIXED-WIDTH ASCII INTEGER READ FUNCTIONS */

/*
 * PDCI_AE_INT_FW_READ_FN(fn_name, targ_type, bytes2num_fn, invalid_err, isspace_fn)
 */

PDCI_AE_INT_FW_READ_FN(PDC_a_int8_FW_read,   PDC_int8,   PDCI_a2int8,   PDC_INVALID_A_NUM,  PDCI_is_a_space);
PDCI_AE_INT_FW_READ_FN(PDC_a_int16_FW_read,  PDC_int16,  PDCI_a2int16,  PDC_INVALID_A_NUM,  PDCI_is_a_space);
PDCI_AE_INT_FW_READ_FN(PDC_a_int32_FW_read,  PDC_int32,  PDCI_a2int32,  PDC_INVALID_A_NUM,  PDCI_is_a_space);
PDCI_AE_INT_FW_READ_FN(PDC_a_int64_FW_read,  PDC_int64,  PDCI_a2int64,  PDC_INVALID_A_NUM,  PDCI_is_a_space);
PDCI_AE_INT_FW_READ_FN(PDC_a_uint8_FW_read,  PDC_uint8,  PDCI_a2uint8,  PDC_INVALID_A_NUM, PDCI_is_a_space);
PDCI_AE_INT_FW_READ_FN(PDC_a_uint16_FW_read, PDC_uint16, PDCI_a2uint16, PDC_INVALID_A_NUM, PDCI_is_a_space);
PDCI_AE_INT_FW_READ_FN(PDC_a_uint32_FW_read, PDC_uint32, PDCI_a2uint32, PDC_INVALID_A_NUM, PDCI_is_a_space);
PDCI_AE_INT_FW_READ_FN(PDC_a_uint64_FW_read, PDC_uint64, PDCI_a2uint64, PDC_INVALID_A_NUM, PDCI_is_a_space);

/* ================================================================================ */
/* VARIABLE-WIDTH EBCDIC CHAR ENCODING INTEGER READ FUNCTIONS */

/*
 * PDCI_AE_INT_READ_FN(fn_name, targ_type, bytes2num_fn, invalid_err, isspace_fn, isdigit_fn)
 */

PDCI_AE_INT_READ_FN(PDC_e_int8_read,   PDC_int8,   PDCI_e2int8,   PDC_INVALID_E_NUM,  PDCI_is_e_space, PDCI_is_e_digit);
PDCI_AE_INT_READ_FN(PDC_e_int16_read,  PDC_int16,  PDCI_e2int16,  PDC_INVALID_E_NUM,  PDCI_is_e_space, PDCI_is_e_digit);
PDCI_AE_INT_READ_FN(PDC_e_int32_read,  PDC_int32,  PDCI_e2int32,  PDC_INVALID_E_NUM,  PDCI_is_e_space, PDCI_is_e_digit);
PDCI_AE_INT_READ_FN(PDC_e_int64_read,  PDC_int64,  PDCI_e2int64,  PDC_INVALID_E_NUM,  PDCI_is_e_space, PDCI_is_e_digit);
PDCI_AE_INT_READ_FN(PDC_e_uint8_read,  PDC_uint8,  PDCI_e2uint8,  PDC_INVALID_E_NUM, PDCI_is_e_space, PDCI_is_e_digit);
PDCI_AE_INT_READ_FN(PDC_e_uint16_read, PDC_uint16, PDCI_e2uint16, PDC_INVALID_E_NUM, PDCI_is_e_space, PDCI_is_e_digit);
PDCI_AE_INT_READ_FN(PDC_e_uint32_read, PDC_uint32, PDCI_e2uint32, PDC_INVALID_E_NUM, PDCI_is_e_space, PDCI_is_e_digit);
PDCI_AE_INT_READ_FN(PDC_e_uint64_read, PDC_uint64, PDCI_e2uint64, PDC_INVALID_E_NUM, PDCI_is_e_space, PDCI_is_e_digit);

/* ================================================================================ */
/* FIXED-WIDTH EBCDIC CHAR ENCODING INTEGER READ FUNCTIONS */

/*
 * PDCI_AE_INT_FW_READ_FN(fn_name, targ_type, bytes2num_fn, invalid_err, isspace_fn)
 */

PDCI_AE_INT_FW_READ_FN(PDC_e_int8_FW_read,   PDC_int8,   PDCI_e2int8,   PDC_INVALID_E_NUM,  PDCI_is_e_space);
PDCI_AE_INT_FW_READ_FN(PDC_e_int16_FW_read,  PDC_int16,  PDCI_e2int16,  PDC_INVALID_E_NUM,  PDCI_is_e_space);
PDCI_AE_INT_FW_READ_FN(PDC_e_int32_FW_read,  PDC_int32,  PDCI_e2int32,  PDC_INVALID_E_NUM,  PDCI_is_e_space);
PDCI_AE_INT_FW_READ_FN(PDC_e_int64_FW_read,  PDC_int64,  PDCI_e2int64,  PDC_INVALID_E_NUM,  PDCI_is_e_space);
PDCI_AE_INT_FW_READ_FN(PDC_e_uint8_FW_read,  PDC_uint8,  PDCI_e2uint8,  PDC_INVALID_E_NUM, PDCI_is_e_space);
PDCI_AE_INT_FW_READ_FN(PDC_e_uint16_FW_read, PDC_uint16, PDCI_e2uint16, PDC_INVALID_E_NUM, PDCI_is_e_space);
PDCI_AE_INT_FW_READ_FN(PDC_e_uint32_FW_read, PDC_uint32, PDCI_e2uint32, PDC_INVALID_E_NUM, PDCI_is_e_space);
PDCI_AE_INT_FW_READ_FN(PDC_e_uint64_FW_read, PDC_uint64, PDCI_e2uint64, PDC_INVALID_E_NUM, PDCI_is_e_space);

/* ================================================================================ */
/* BINARY INTEGER READ FUNCTIONS */

/*
 * PDCI_B_INT_READ_FN(fn_name, targ_type, width, swapmem_op)
 *
 * swapmem ops:
 *    0 -> straight copy
 *    1 -> reverse each byte in each string of 2 bytes
 *    3 -> reverse each byte in each string of 4 bytes
 *    4 -> swap upper/lower 4 bytes in each 8 byte value
 *    7 -> reverse each byte in each string of 8 bytes
 */

PDCI_B_INT_READ_FN(PDC_b_int8_read,   PDC_int8,   1, 0);
PDCI_B_INT_READ_FN(PDC_b_uint8_read,  PDC_uint8,  1, 0);
PDCI_B_INT_READ_FN(PDC_b_int16_read,  PDC_int16,  2, 1);
PDCI_B_INT_READ_FN(PDC_b_uint16_read, PDC_uint16, 2, 1);
PDCI_B_INT_READ_FN(PDC_b_int32_read,  PDC_int32,  4, 3);
PDCI_B_INT_READ_FN(PDC_b_uint32_read, PDC_uint32, 4, 3);
PDCI_B_INT_READ_FN(PDC_b_int64_read,  PDC_int64,  8, 7);
PDCI_B_INT_READ_FN(PDC_b_uint64_read, PDC_uint64, 8, 7);

/* ================================================================================ */
/* EBC, BCD, SBL, SBH NUMERIC ENCODING INTEGER READ FUNCTIONS */

/*
 * PDCI_EBCBCDSB_INT_READ_FN(fn_name, targ_type, bytes2num_fn, invalid_err, width)
 */

PDCI_EBCBCDSB_INT_READ_FN(PDC_ebc_int8_read,   PDC_int8,   PDCI_ebc2int8,   PDC_INVALID_EBC_NUM, num_digits_or_bytes);
PDCI_EBCBCDSB_INT_READ_FN(PDC_ebc_int16_read,  PDC_int16,  PDCI_ebc2int16,  PDC_INVALID_EBC_NUM, num_digits_or_bytes);
PDCI_EBCBCDSB_INT_READ_FN(PDC_ebc_int32_read,  PDC_int32,  PDCI_ebc2int32,  PDC_INVALID_EBC_NUM, num_digits_or_bytes);
PDCI_EBCBCDSB_INT_READ_FN(PDC_ebc_int64_read,  PDC_int64,  PDCI_ebc2int64,  PDC_INVALID_EBC_NUM, num_digits_or_bytes);
PDCI_EBCBCDSB_INT_READ_FN(PDC_ebc_uint8_read,  PDC_uint8,  PDCI_ebc2uint8,  PDC_INVALID_EBC_NUM, num_digits_or_bytes);
PDCI_EBCBCDSB_INT_READ_FN(PDC_ebc_uint16_read, PDC_uint16, PDCI_ebc2uint16, PDC_INVALID_EBC_NUM, num_digits_or_bytes);
PDCI_EBCBCDSB_INT_READ_FN(PDC_ebc_uint32_read, PDC_uint32, PDCI_ebc2uint32, PDC_INVALID_EBC_NUM, num_digits_or_bytes);
PDCI_EBCBCDSB_INT_READ_FN(PDC_ebc_uint64_read, PDC_uint64, PDCI_ebc2uint64, PDC_INVALID_EBC_NUM, num_digits_or_bytes);

PDCI_EBCBCDSB_INT_READ_FN(PDC_bcd_int8_read,   PDC_int8,   PDCI_bcd2int8,   PDC_INVALID_BCD_NUM, ((num_digits_or_bytes+1)/2));
PDCI_EBCBCDSB_INT_READ_FN(PDC_bcd_int16_read,  PDC_int16,  PDCI_bcd2int16,  PDC_INVALID_BCD_NUM, ((num_digits_or_bytes+1)/2));
PDCI_EBCBCDSB_INT_READ_FN(PDC_bcd_int32_read,  PDC_int32,  PDCI_bcd2int32,  PDC_INVALID_BCD_NUM, ((num_digits_or_bytes+1)/2));
PDCI_EBCBCDSB_INT_READ_FN(PDC_bcd_int64_read,  PDC_int64,  PDCI_bcd2int64,  PDC_INVALID_BCD_NUM, ((num_digits_or_bytes+1)/2));
PDCI_EBCBCDSB_INT_READ_FN(PDC_bcd_uint8_read,  PDC_uint8,  PDCI_bcd2uint8,  PDC_INVALID_BCD_NUM, ((num_digits_or_bytes+1)/2));
PDCI_EBCBCDSB_INT_READ_FN(PDC_bcd_uint16_read, PDC_uint16, PDCI_bcd2uint16, PDC_INVALID_BCD_NUM, ((num_digits_or_bytes+1)/2));
PDCI_EBCBCDSB_INT_READ_FN(PDC_bcd_uint32_read, PDC_uint32, PDCI_bcd2uint32, PDC_INVALID_BCD_NUM, ((num_digits_or_bytes+1)/2));
PDCI_EBCBCDSB_INT_READ_FN(PDC_bcd_uint64_read, PDC_uint64, PDCI_bcd2uint64, PDC_INVALID_BCD_NUM, ((num_digits_or_bytes+1)/2));

PDCI_EBCBCDSB_INT_READ_FN(PDC_sbl_int8_read,   PDC_int8,   PDCI_sbl2int8,   PDC_UNEXPECTED_ERR, num_digits_or_bytes);
PDCI_EBCBCDSB_INT_READ_FN(PDC_sbl_int16_read,  PDC_int16,  PDCI_sbl2int16,  PDC_UNEXPECTED_ERR, num_digits_or_bytes);
PDCI_EBCBCDSB_INT_READ_FN(PDC_sbl_int32_read,  PDC_int32,  PDCI_sbl2int32,  PDC_UNEXPECTED_ERR, num_digits_or_bytes);
PDCI_EBCBCDSB_INT_READ_FN(PDC_sbl_int64_read,  PDC_int64,  PDCI_sbl2int64,  PDC_UNEXPECTED_ERR, num_digits_or_bytes);
PDCI_EBCBCDSB_INT_READ_FN(PDC_sbl_uint8_read,  PDC_uint8,  PDCI_sbl2uint8,  PDC_UNEXPECTED_ERR, num_digits_or_bytes);
PDCI_EBCBCDSB_INT_READ_FN(PDC_sbl_uint16_read, PDC_uint16, PDCI_sbl2uint16, PDC_UNEXPECTED_ERR, num_digits_or_bytes);
PDCI_EBCBCDSB_INT_READ_FN(PDC_sbl_uint32_read, PDC_uint32, PDCI_sbl2uint32, PDC_UNEXPECTED_ERR, num_digits_or_bytes);
PDCI_EBCBCDSB_INT_READ_FN(PDC_sbl_uint64_read, PDC_uint64, PDCI_sbl2uint64, PDC_UNEXPECTED_ERR, num_digits_or_bytes);

PDCI_EBCBCDSB_INT_READ_FN(PDC_sbh_int8_read,   PDC_int8,   PDCI_sbh2int8,   PDC_UNEXPECTED_ERR, num_digits_or_bytes);
PDCI_EBCBCDSB_INT_READ_FN(PDC_sbh_int16_read,  PDC_int16,  PDCI_sbh2int16,  PDC_UNEXPECTED_ERR, num_digits_or_bytes);
PDCI_EBCBCDSB_INT_READ_FN(PDC_sbh_int32_read,  PDC_int32,  PDCI_sbh2int32,  PDC_UNEXPECTED_ERR, num_digits_or_bytes);
PDCI_EBCBCDSB_INT_READ_FN(PDC_sbh_int64_read,  PDC_int64,  PDCI_sbh2int64,  PDC_UNEXPECTED_ERR, num_digits_or_bytes);
PDCI_EBCBCDSB_INT_READ_FN(PDC_sbh_uint8_read,  PDC_uint8,  PDCI_sbh2uint8,  PDC_UNEXPECTED_ERR, num_digits_or_bytes);
PDCI_EBCBCDSB_INT_READ_FN(PDC_sbh_uint16_read, PDC_uint16, PDCI_sbh2uint16, PDC_UNEXPECTED_ERR, num_digits_or_bytes);
PDCI_EBCBCDSB_INT_READ_FN(PDC_sbh_uint32_read, PDC_uint32, PDCI_sbh2uint32, PDC_UNEXPECTED_ERR, num_digits_or_bytes);
PDCI_EBCBCDSB_INT_READ_FN(PDC_sbh_uint64_read, PDC_uint64, PDCI_sbh2uint64, PDC_UNEXPECTED_ERR, num_digits_or_bytes);

/* ================================================================================ */
/* EBC, BCD, SBL, SBH NUMERIC ENCODING FIXED POINT READ FUNCTIONS */

/*
 * PDCI_EBCBCDSB_FPOINT_READ_FN(fn_name, targ_type, internal_numerator_read_fn, width, dexp_max)
 */

PDCI_EBCBCDSB_FPOINT_READ_FN(PDC_ebc_fpoint8_read,   PDC_fpoint8,   PDC_ebc_int8_read_internal,   num_digits_or_bytes,  2)
PDCI_EBCBCDSB_FPOINT_READ_FN(PDC_ebc_fpoint16_read,  PDC_fpoint16,  PDC_ebc_int16_read_internal,  num_digits_or_bytes,  4)
PDCI_EBCBCDSB_FPOINT_READ_FN(PDC_ebc_fpoint32_read,  PDC_fpoint32,  PDC_ebc_int32_read_internal,  num_digits_or_bytes,  9)
PDCI_EBCBCDSB_FPOINT_READ_FN(PDC_ebc_fpoint64_read,  PDC_fpoint64,  PDC_ebc_int64_read_internal,  num_digits_or_bytes, 19)
PDCI_EBCBCDSB_FPOINT_READ_FN(PDC_ebc_ufpoint8_read,  PDC_ufpoint8,  PDC_ebc_uint8_read_internal,  num_digits_or_bytes,  2)
PDCI_EBCBCDSB_FPOINT_READ_FN(PDC_ebc_ufpoint16_read, PDC_ufpoint16, PDC_ebc_uint16_read_internal, num_digits_or_bytes,  4)
PDCI_EBCBCDSB_FPOINT_READ_FN(PDC_ebc_ufpoint32_read, PDC_ufpoint32, PDC_ebc_uint32_read_internal, num_digits_or_bytes,  9)
PDCI_EBCBCDSB_FPOINT_READ_FN(PDC_ebc_ufpoint64_read, PDC_ufpoint64, PDC_ebc_uint64_read_internal, num_digits_or_bytes, 19)

PDCI_EBCBCDSB_FPOINT_READ_FN(PDC_bcd_fpoint8_read,   PDC_fpoint8,   PDC_bcd_int8_read_internal,   ((num_digits_or_bytes+1)/2),  2)
PDCI_EBCBCDSB_FPOINT_READ_FN(PDC_bcd_fpoint16_read,  PDC_fpoint16,  PDC_bcd_int16_read_internal,  ((num_digits_or_bytes+1)/2),  4)
PDCI_EBCBCDSB_FPOINT_READ_FN(PDC_bcd_fpoint32_read,  PDC_fpoint32,  PDC_bcd_int32_read_internal,  ((num_digits_or_bytes+1)/2),  9)
PDCI_EBCBCDSB_FPOINT_READ_FN(PDC_bcd_fpoint64_read,  PDC_fpoint64,  PDC_bcd_int64_read_internal,  ((num_digits_or_bytes+1)/2), 19)
PDCI_EBCBCDSB_FPOINT_READ_FN(PDC_bcd_ufpoint8_read,  PDC_ufpoint8,  PDC_bcd_uint8_read_internal,  ((num_digits_or_bytes+1)/2),  2)
PDCI_EBCBCDSB_FPOINT_READ_FN(PDC_bcd_ufpoint16_read, PDC_ufpoint16, PDC_bcd_uint16_read_internal, ((num_digits_or_bytes+1)/2),  4)
PDCI_EBCBCDSB_FPOINT_READ_FN(PDC_bcd_ufpoint32_read, PDC_ufpoint32, PDC_bcd_uint32_read_internal, ((num_digits_or_bytes+1)/2),  9)
PDCI_EBCBCDSB_FPOINT_READ_FN(PDC_bcd_ufpoint64_read, PDC_ufpoint64, PDC_bcd_uint64_read_internal, ((num_digits_or_bytes+1)/2), 19)

PDCI_EBCBCDSB_FPOINT_READ_FN(PDC_sbl_fpoint8_read,   PDC_fpoint8,   PDC_sbl_int8_read_internal,   num_digits_or_bytes,  2)
PDCI_EBCBCDSB_FPOINT_READ_FN(PDC_sbl_fpoint16_read,  PDC_fpoint16,  PDC_sbl_int16_read_internal,  num_digits_or_bytes,  4)
PDCI_EBCBCDSB_FPOINT_READ_FN(PDC_sbl_fpoint32_read,  PDC_fpoint32,  PDC_sbl_int32_read_internal,  num_digits_or_bytes,  9)
PDCI_EBCBCDSB_FPOINT_READ_FN(PDC_sbl_fpoint64_read,  PDC_fpoint64,  PDC_sbl_int64_read_internal,  num_digits_or_bytes, 19)
PDCI_EBCBCDSB_FPOINT_READ_FN(PDC_sbl_ufpoint8_read,  PDC_ufpoint8,  PDC_sbl_uint8_read_internal,  num_digits_or_bytes,  2)
PDCI_EBCBCDSB_FPOINT_READ_FN(PDC_sbl_ufpoint16_read, PDC_ufpoint16, PDC_sbl_uint16_read_internal, num_digits_or_bytes,  4)
PDCI_EBCBCDSB_FPOINT_READ_FN(PDC_sbl_ufpoint32_read, PDC_ufpoint32, PDC_sbl_uint32_read_internal, num_digits_or_bytes,  9)
PDCI_EBCBCDSB_FPOINT_READ_FN(PDC_sbl_ufpoint64_read, PDC_ufpoint64, PDC_sbl_uint64_read_internal, num_digits_or_bytes, 19)

PDCI_EBCBCDSB_FPOINT_READ_FN(PDC_sbh_fpoint8_read,   PDC_fpoint8,   PDC_sbh_int8_read_internal,   num_digits_or_bytes,  2)
PDCI_EBCBCDSB_FPOINT_READ_FN(PDC_sbh_fpoint16_read,  PDC_fpoint16,  PDC_sbh_int16_read_internal,  num_digits_or_bytes,  4)
PDCI_EBCBCDSB_FPOINT_READ_FN(PDC_sbh_fpoint32_read,  PDC_fpoint32,  PDC_sbh_int32_read_internal,  num_digits_or_bytes,  9)
PDCI_EBCBCDSB_FPOINT_READ_FN(PDC_sbh_fpoint64_read,  PDC_fpoint64,  PDC_sbh_int64_read_internal,  num_digits_or_bytes, 19)
PDCI_EBCBCDSB_FPOINT_READ_FN(PDC_sbh_ufpoint8_read,  PDC_ufpoint8,  PDC_sbh_uint8_read_internal,  num_digits_or_bytes,  2)
PDCI_EBCBCDSB_FPOINT_READ_FN(PDC_sbh_ufpoint16_read, PDC_ufpoint16, PDC_sbh_uint16_read_internal, num_digits_or_bytes,  4)
PDCI_EBCBCDSB_FPOINT_READ_FN(PDC_sbh_ufpoint32_read, PDC_ufpoint32, PDC_sbh_uint32_read_internal, num_digits_or_bytes,  9)
PDCI_EBCBCDSB_FPOINT_READ_FN(PDC_sbh_ufpoint64_read, PDC_ufpoint64, PDC_sbh_uint64_read_internal, num_digits_or_bytes, 19)

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
#define PDCI_ACC_MAX2TRACK      1000
#define PDCI_ACC_REPORT_K         10

/* ********************************** END_HEADER ********************************** */
#gen_include "libpadsc-acc-macros-gen.h"

/* PDCI_INT_ACCUM(int_type, int_descr, num_bytes, fmt, fold_test) */

PDCI_INT_ACCUM(PDC_int8,   "int8",   1, "d",   PDCI_FOLDTEST_INT8);
PDCI_INT_ACCUM(PDC_uint8,  "uint8",  1, "u",   PDCI_FOLDTEST_UINT8);
PDCI_INT_ACCUM(PDC_int16,  "int16",  2, "d",   PDCI_FOLDTEST_INT16);
PDCI_INT_ACCUM(PDC_uint16, "uint16", 2, "u",   PDCI_FOLDTEST_UINT16);
PDCI_INT_ACCUM(PDC_int32,  "int32",  4, "ld",  PDCI_FOLDTEST_INT32);
PDCI_INT_ACCUM(PDC_uint32, "uint32", 4, "lu",  PDCI_FOLDTEST_UINT32);
PDCI_INT_ACCUM(PDC_int64,  "int64",  8, "lld", PDCI_FOLDTEST_INT64);
PDCI_INT_ACCUM(PDC_uint64, "uint64", 8, "llu", PDCI_FOLDTEST_UINT64);


/* PDCI_INT_ACCUM_REPORT_MAP(int_type, int_descr, fmt) */

PDCI_INT_ACCUM_REPORT_MAP(PDC_int32, "int32", "ld");


/* ********************************* BEGIN_TRAILER ******************************** */

typedef struct PDCI_string_dt_key_s {
  PDC_uint64  cnt;
  size_t      len;
  char        *str;
} PDCI_string_dt_key_t;

typedef struct PDCI_string_dt_elt_s {
  PDCI_string_dt_key_t  key;
  Dtlink_t              link;
  char                  buf[1];
} PDCI_string_dt_elt_t;

unsigned int
PDCI_string_dt_elt_hash(Dt_t *dt, Void_t *key, Dtdisc_t *disc)
{
  PDCI_string_dt_key_t *k = (PDCI_string_dt_key_t*)key;
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
PDCI_string_dt_elt_oset_cmp(Dt_t *dt, PDCI_string_dt_key_t *a, PDCI_string_dt_key_t *b, Dtdisc_t *disc)
{
  size_t min_len;
  int res;
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
PDCI_string_dt_elt_set_cmp(Dt_t *dt, PDCI_string_dt_key_t *a, PDCI_string_dt_key_t *b, Dtdisc_t *disc)
{
  NoP(dt);
  NoP(disc);
  if (a->len == b->len && strncmp(a->str, b->str, a->len) == 0) {
    return 0;
  }
  return 1;
}

void*
PDCI_string_dt_elt_make(Dt_t *dt, PDCI_string_dt_elt_t *a, Dtdisc_t *disc)
{
  PDCI_string_dt_elt_t *b;
  NoP(dt);
  NoP(disc);
  if ((b = oldof(0, PDCI_string_dt_elt_t, 1, a->key.len))) {
    memcpy(b->buf, a->key.str, a->key.len);
    b->key.cnt = a->key.cnt;
    b->key.len = a->key.len;
    b->key.str = b->buf;
  }
  return b;
}

void
PDCI_string_dt_elt_free(Dt_t *dt, PDCI_string_dt_elt_t *a, Dtdisc_t *disc)
{
  free(a);
}

static Dtdisc_t PDCI_string_acc_dt_set_disc = {
  offsetof(PDCI_string_dt_elt_t, key),     /* key     */
  0,				           /* size    */
  offsetof(PDCI_string_dt_elt_t, link),    /* link    */
  (Dtmake_f)PDCI_string_dt_elt_make,       /* makef   */
  (Dtfree_f)PDCI_string_dt_elt_free,       /* freef */
  (Dtcompar_f)PDCI_string_dt_elt_set_cmp,  /* comparf */
  (Dthash_f)PDCI_string_dt_elt_hash,       /* hashf   */
  NiL,				           /* memoryf */
  NiL				           /* eventf  */
};

static Dtdisc_t PDCI_string_acc_dt_oset_disc = {
  offsetof(PDCI_string_dt_elt_t, key),     /* key     */
  0,				           /* size    */
  offsetof(PDCI_string_dt_elt_t, link),    /* link    */
  (Dtmake_f)PDCI_string_dt_elt_make,       /* makef   */
  (Dtfree_f)PDCI_string_dt_elt_free,       /* freef */
  (Dtcompar_f)PDCI_string_dt_elt_oset_cmp, /* comparf */
  (Dthash_f)PDCI_string_dt_elt_hash,       /* hashf   */
  NiL,				           /* memoryf */
  NiL				           /* eventf  */
};

PDC_error_t
PDC_string_acc_init(PDC_t *pdc, PDC_string_acc *a)
{
  PDCI_DISC_INIT_CHECKS("PDC_string_acc_init");
  PDCI_NULLPARAM_CHECK("PDC_string_acc_init", a);
  if (!(a->dict = dtopen(&PDCI_string_acc_dt_set_disc, Dtset))) {
    return PDC_ERR;
  }
  a->tracked = 0;
  return PDC_uint32_acc_init(pdc, &(a->len_accum));
}

PDC_error_t
PDC_string_acc_reset(PDC_t *pdc, PDC_string_acc *a)
{
  PDCI_DISC_INIT_CHECKS("PDC_string_acc_reset");
  PDCI_NULLPARAM_CHECK("PDC_string_acc_reset", a);
  if (!a->dict) {
    return PDC_ERR;
  }
  dtclear(a->dict);
  a->tracked = 0;
  return PDC_uint32_acc_reset(pdc, &(a->len_accum));
}

PDC_error_t
PDC_string_acc_cleanup(PDC_t *pdc, PDC_string_acc *a)
{
  PDCI_DISC_INIT_CHECKS("PDC_string_acc_cleanup");
  PDCI_NULLPARAM_CHECK("PDC_string_acc_cleanup", a);
  if (a->dict) {
    dtclose(a->dict);
    a->dict = 0;
  }
  return PDC_uint32_acc_cleanup(pdc, &(a->len_accum));
}

PDC_error_t
PDC_string_acc_add(PDC_t *pdc, PDC_string_acc *a, PDC_base_ed *ed, PDC_string *val)
{
  PDCI_string_dt_elt_t  insert_elt;
  PDCI_string_dt_key_t  lookup_key;
  PDCI_string_dt_elt_t  *tmp1;
  PDCI_DISC_INIT_CHECKS("PDC_string_acc_add");
  PDCI_NULLPARAM_CHECK("PDC_string_acc_add", a);
  PDCI_NULLPARAM_CHECK("PDC_string_acc_add", ed);
  PDCI_NULLPARAM_CHECK("PDC_string_acc_add", val);
  if (!a->dict) {
    return PDC_ERR;
  }
  if (PDC_ERR == PDC_uint32_acc_add(pdc, &(a->len_accum), ed, &(val->len))) {
    return PDC_ERR;
  }
  if (ed->errCode != 0) {
    return PDC_OK;
  }
  if (dtsize(a->dict) < PDCI_ACC_MAX2TRACK) {
    insert_elt.key.str = val->str;
    insert_elt.key.len = val->len;
    insert_elt.key.cnt = 0;
    if (!(tmp1 = dtinsert(a->dict, &insert_elt))) {
      PDC_WARN(pdc->disc, "** PADC internal error: dtinsert failed (out of memory?) **");
      return PDC_ERR;
    }
    (tmp1->key.cnt)++;
    (a->tracked)++;
  } else {
    lookup_key.str = val->str;
    lookup_key.len = val->len;
    lookup_key.cnt = 0;
    if ((tmp1 = dtmatch(a->dict, (Void_t*)&lookup_key))) {
      (tmp1->key.cnt)++;
      (a->tracked)++;
    }
  }
  return PDC_OK;
}

PDC_error_t
PDC_string_acc_report_internal(PDC_t *pdc, Sfio_t *outstr, const char *prefix, const char *what, int nst,
			       PDC_string_acc *a)
{
  size_t                 pad;
  int                    i = 0, sz, rp;
  PDC_uint64             cnt_sum = 0;
  double                 track_pcnt;
  double                 cnt_sum_pcnt;
  double                 elt_pcnt;
  Void_t                 *velt;
  PDCI_string_dt_elt_t   *elt;

  PDC_TRACE(pdc->disc, "PDC_string_acc_report_internal called");
  if (!prefix || *prefix == 0) {
    prefix = "<top>";
  }
  if (!what) {
    what = "string";
  }
  PDCI_nst_prefix_what(outstr, &nst, prefix, what);
  if (PDC_ERR == PDC_uint32_acc_report_internal(pdc, outstr, "String lengths", "lengths", -1, &(a->len_accum))) {
    return PDC_ERR;
  }
  if (a->len_accum.good == 0) {
    return PDC_OK;
  }
  /* rehash tree to get keys ordered by count */
  sz = dtsize(a->dict);
  rp = (sz < PDCI_ACC_REPORT_K) ? sz : PDCI_ACC_REPORT_K;
  dtdisc(a->dict, &PDCI_string_acc_dt_oset_disc, DT_SAMEHASH); /* change cmp function */
  dtmethod(a->dict, Dtoset); /* change to ordered set -- establishes an ordering */
  sfprintf(outstr, "\n  Characterizing strings:\n");
  sfprintf(outstr, "    => distribution of top %d strings out of %d distinct strings:\n", rp, sz);
  if (sz == PDCI_ACC_MAX2TRACK && a->len_accum.good > a->tracked) {
    track_pcnt = 100.0 * (a->tracked/(double)a->len_accum.good);
    sfprintf(outstr, "        (* hit tracking limit, tracked %.3lf pcnt of all values *) \n", track_pcnt);
  }
  for (velt = dtfirst(a->dict); velt && i < PDCI_ACC_REPORT_K; velt = dtnext(a->dict, velt), i++) {
    elt = (PDCI_string_dt_elt_t*)velt;
    cnt_sum += elt->key.cnt;
    elt_pcnt = 100.0 * (elt->key.cnt/(double)a->len_accum.good);
    sfprintf(outstr, "        val: ");
    sfprintf(outstr, "%-.*s", elt->key.len+2, PDC_qfmt_Cstr(elt->key.str, elt->key.len));
    sfprintf(outstr, "");
    pad = a->len_accum.max - elt->key.len;
    sfprintf(outstr, "%-.*s", pad,
	     "                                                                                ");
    sfprintf(outstr, " count: %10llu  pcnt-of-good-vals: %8.3lf\n", elt->key.cnt, elt_pcnt);
  }
  cnt_sum_pcnt = 100.0 * (cnt_sum/(double)a->len_accum.good);
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
  dtdisc(a->dict, &PDCI_string_acc_dt_set_disc, DT_SAMEHASH); /* change cmp function */
  return PDC_OK;
}

PDC_error_t
PDC_string_acc_report(PDC_t *pdc, const char *prefix, const char *what, int nst, PDC_string_acc *a)
{
  Sfio_t *tmpstr;
  PDC_error_t res;
  PDCI_DISC_INIT_CHECKS("PDC_string_acc_report");
  PDCI_NULLPARAM_CHECK("PDC_string_acc_report", a);
  if (!pdc->disc->errorf) {
    return PDC_OK;
  }
  if (!(tmpstr = sfstropen ())) { 
    return PDC_ERR;
  }
  res = PDC_string_acc_report_internal(pdc, tmpstr, prefix, what, nst, a);
  if (res == PDC_OK) {
    pdc->disc->errorf(NiL, 0, "%s", sfstruse(tmpstr));
  }
  sfstrclose (tmpstr);
  return res;
}

PDC_error_t
PDC_char_acc_init(PDC_t *pdc, PDC_char_acc *a)
{
  return PDC_uint8_acc_init(pdc, a);
}

PDC_error_t
PDC_char_acc_reset(PDC_t *pdc, PDC_char_acc *a)
{
  return PDC_uint8_acc_reset(pdc, a);
}

PDC_error_t
PDC_char_acc_cleanup(PDC_t *pdc, PDC_char_acc *a)
{
  return PDC_uint8_acc_cleanup(pdc, a);
}

PDC_error_t
PDC_char_acc_add(PDC_t *pdc, PDC_char_acc *a, PDC_base_ed *ed, PDC_uint8 *val)
{
  return PDC_uint8_acc_add(pdc, a, ed, val);
}

PDC_error_t
PDC_char_acc_report_internal(PDC_t *pdc, Sfio_t *outstr, const char *prefix, const char *what, int nst,
			     PDC_char_acc *a)
{
  int                   i = 0, sz, rp;
  PDC_uint64            cnt_sum = 0;
  double                bad_pcnt;
  double                track_pcnt;
  double                cnt_sum_pcnt;
  double                elt_pcnt;
  Void_t                *velt;
  PDC_uint8_dt_elt_t    *elt;

  PDC_TRACE(pdc->disc, "PDC_char_acc_report_internal called");
  if (!prefix || *prefix == 0) {
    prefix = "<top>";
  }
  if (!what) {
    what = "char";
  }
  PDCI_nst_prefix_what(outstr, &nst, prefix, what);
  if (a->good == 0) {
    bad_pcnt = (a->bad == 0) ? 0.0 : 100.0;
  } else {
    bad_pcnt = 100.0 * (a->bad / (double)(a->good + a->bad));
  }
  sfprintf(outstr, "good vals: %10llu    bad vals: %10llu    pcnt-bad: %8.3lf\n",
	   a->good, a->bad, bad_pcnt);
  if (a->good == 0) {
    return PDC_OK;
  }
  PDC_uint8_acc_fold_psum(a);
  sz = dtsize(a->dict);
  rp = (sz < PDCI_ACC_REPORT_K) ? sz : PDCI_ACC_REPORT_K;
  dtdisc(a->dict,   &PDC_uint8_acc_dt_oset_disc, DT_SAMEHASH); /* change cmp function */
  dtmethod(a->dict, Dtoset); /* change to ordered set -- establishes an ordering */
  sfprintf(outstr, "  Characterizing %s:  min %s", what, PDC_qfmt_char(a->min));
  sfprintf(outstr, " max %s\n", PDC_qfmt_char(a->max));

  sfprintf(outstr, "    => distribution of top %d values out of %d distinct values:\n", rp, sz);
  if (sz == PDCI_ACC_MAX2TRACK && a->good > a->tracked) {
    track_pcnt = 100.0 * (a->tracked/(double)a->good);
    sfprintf(outstr, "        (* hit tracking limit, tracked %.3lf pcnt of all values *) \n", track_pcnt);
  }
  for (velt = dtfirst(a->dict); velt && i < PDCI_ACC_REPORT_K; velt = dtnext(a->dict, velt), i++) {
    elt = (PDC_uint8_dt_elt_t*)velt;
    cnt_sum += elt->key.cnt;
    elt_pcnt = 100.0 * (elt->key.cnt/(double)a->good);
    sfprintf(outstr, "        val: %6s", PDC_qfmt_char(elt->key.val));
    sfprintf(outstr, " count: %10llu  pcnt-of-good-vals: %8.3lf\n", elt->key.cnt, elt_pcnt);

  }
  cnt_sum_pcnt = 100.0 * (cnt_sum/(double)a->good);
  sfprintf(outstr,   ". . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .\n");
  sfprintf(outstr,   "        SUMMING     count: %10llu  pcnt-of-good-vals: %8.3lf\n",
	   cnt_sum, cnt_sum_pcnt);
  /* revert to unordered set in case more inserts will occur after this report */
  dtmethod(a->dict, Dtset); /* change to unordered set */
  dtdisc(a->dict,   &PDC_uint8_acc_dt_set_disc, DT_SAMEHASH); /* change cmp function */
  return PDC_OK;
}

PDC_error_t
PDC_char_acc_report(PDC_t *pdc, const char *prefix, const char *what, int nst,
		    PDC_char_acc *a)
{
  Sfio_t *tmpstr;
  PDC_error_t res;
  PDCI_DISC_INIT_CHECKS("PDC_char_acc_report");
  PDCI_NULLPARAM_CHECK("PDC_char_acc_report", a);
  if (!pdc->disc->errorf) {
    return PDC_OK;
  }
  if (!(tmpstr = sfstropen ())) { 
    return PDC_ERR;
  }
  res = PDC_char_acc_report_internal(pdc, tmpstr, prefix, what, nst, a);
  if (res == PDC_OK) {
    pdc->disc->errorf(NiL, 0, "%s", sfstruse(tmpstr));
  }
  sfstrclose (tmpstr);
  return res;
}

/* ================================================================================ */ 
/* ACCUM IMPL HELPERS */

static const char *PDCI_hdr_strings[] = {
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
PDCI_nst_prefix_what(Sfio_t *outstr, int *nst, const char *prefix, const char *what)
{
  if (prefix) {
    if ((*nst) >= 0) {
      int idx = (*nst) % 9;
      sfprintf(outstr, "\n%s", PDCI_hdr_strings[idx]);
      sfprintf(outstr, "%s : %s\n", prefix, what);
      sfprintf(outstr, "%s", PDCI_hdr_strings[idx]);
      (*nst)++;
    } else {
      sfprintf(outstr, "%s: ", prefix);
    }
  }
}

/* ********************************** END_MACGEN ********************************** */

/* ********************** BEGIN_MACGEN(libpadsc-misc-gen.c) *********************** */
/*
 * Generated misc functions
 * 
 * Kathleen Fisher, Robert Gruber
 * AT&T Labs Research
 */

#gen_include "libpadsc-internal.h"
#gen_include "libpadsc-macros-gen.h"

/* ================================================================================ */
/* USEFUL CONVERSION CONSTANTS */

#define PDC_MIN_INT8_DIV10                       -12
#define PDC_MIN_INT8_DIV100                       -1

#define PDC_MAX_UINT8_DIV10                      25U
#define PDC_MAX_UINT8_DIV100                      2U

#define PDC_MIN_INT16_DIV10                    -3276
#define PDC_MIN_INT16_DIV100                    -327

#define PDC_MAX_UINT16_DIV10                   6553U
#define PDC_MAX_UINT16_DIV100                   655U

#define PDC_MIN_INT32_DIV10              -214748364L
#define PDC_MIN_INT32_DIV100              -21474836L

#define PDC_MAX_UINT32_DIV10             429496729UL
#define PDC_MAX_UINT32_DIV100             42949672UL

#define PDC_MIN_INT64_DIV10    -922337203685477580LL
#define PDC_MIN_INT64_DIV100    -92233720368547758LL

#define PDC_MAX_UINT64_DIV10  1844674407370955161ULL
#define PDC_MAX_UINT64_DIV100  184467440737095516ULL

static PDC_int64 PDC_MIN_FOR_NB[] = {
  0,
  PDC_MIN_INT8,
  PDC_MIN_INT16,
  PDC_MIN_INT24,
  PDC_MIN_INT32,
  PDC_MIN_INT40,
  PDC_MIN_INT48,
  PDC_MIN_INT56,
  PDC_MIN_INT64
};

static PDC_int64 PDC_MAX_FOR_NB[] = {
  0,
  PDC_MAX_INT8,
  PDC_MAX_INT16,
  PDC_MAX_INT24,
  PDC_MAX_INT32,
  PDC_MAX_INT40,
  PDC_MAX_INT48,
  PDC_MAX_INT56,
  PDC_MAX_INT64
};

static PDC_uint64 PDC_UMAX_FOR_NB[] = {
  0,
  PDC_MAX_UINT8,
  PDC_MAX_UINT16,
  PDC_MAX_UINT24,
  PDC_MAX_UINT32,
  PDC_MAX_UINT40,
  PDC_MAX_UINT48,
  PDC_MAX_UINT56,
  PDC_MAX_UINT64
};

/* ********************************** END_HEADER ********************************** */
#gen_include "libpadsc-misc-macros-gen.h"

/* PDCI_A2INT(fn_name, rev_fn_name, targ_type, int_min, int_max) */
PDCI_A2INT(PDCI_a2int8,  PDCI_int8_2a,  PDC_int8,  PDC_MIN_INT8,  PDC_MAX_INT8)
PDCI_A2INT(PDCI_a2int16, PDCI_int16_2a, PDC_int16, PDC_MIN_INT16, PDC_MAX_INT16)
PDCI_A2INT(PDCI_a2int32, PDCI_int32_2a, PDC_int32, PDC_MIN_INT32, PDC_MAX_INT32)
PDCI_A2INT(PDCI_a2int64, PDCI_int64_2a, PDC_int64, PDC_MIN_INT64, PDC_MAX_INT64)

/* PDCI_A2UINT(fn_name, targ_type, int_max) */
PDCI_A2UINT(PDCI_a2uint8,  PDCI_uint8_2a,  PDC_uint8,  PDC_MAX_UINT8)
PDCI_A2UINT(PDCI_a2uint16, PDCI_uint16_2a, PDC_uint16, PDC_MAX_UINT16)
PDCI_A2UINT(PDCI_a2uint32, PDCI_uint32_2a, PDC_uint32, PDC_MAX_UINT32)
PDCI_A2UINT(PDCI_a2uint64, PDCI_uint64_2a, PDC_uint64, PDC_MAX_UINT64)

/* PDCI_E2INT(fn_name, rev_fn_name, targ_type, int_min, int_max) */
PDCI_E2INT(PDCI_e2int8,  PDCI_int8_2e,  PDC_int8,  PDC_MIN_INT8,  PDC_MAX_INT8)
PDCI_E2INT(PDCI_e2int16, PDCI_int16_2e, PDC_int16, PDC_MIN_INT16, PDC_MAX_INT16)
PDCI_E2INT(PDCI_e2int32, PDCI_int32_2e, PDC_int32, PDC_MIN_INT32, PDC_MAX_INT32)
PDCI_E2INT(PDCI_e2int64, PDCI_int64_2e, PDC_int64, PDC_MIN_INT64, PDC_MAX_INT64)

/* PDCI_E2UINT(fn_name, rev_fn_name, targ_type, int_max) */
PDCI_E2UINT(PDCI_e2uint8,  PDCI_uint8_2e,  PDC_uint8,  PDC_MAX_UINT8)
PDCI_E2UINT(PDCI_e2uint16, PDCI_uint16_2e, PDC_uint16, PDC_MAX_UINT16)
PDCI_E2UINT(PDCI_e2uint32, PDCI_uint32_2e, PDC_uint32, PDC_MAX_UINT32)
PDCI_E2UINT(PDCI_e2uint64, PDCI_uint64_2e, PDC_uint64, PDC_MAX_UINT64)

/* PDCI_EBC2INT(fn_name, rev_fn_name, targ_type, int_min, int_max, nd_max, act_nd_max) */
PDCI_EBC2INT(PDCI_ebc2int8,  PDCI_int8_2ebc,  PDC_int8,  PDC_MIN_INT8,  PDC_MAX_INT8,   3, 3)
PDCI_EBC2INT(PDCI_ebc2int16, PDCI_int16_2ebc, PDC_int16, PDC_MIN_INT16, PDC_MAX_INT16,  5, 5)
PDCI_EBC2INT(PDCI_ebc2int32, PDCI_int32_2ebc, PDC_int32, PDC_MIN_INT32, PDC_MAX_INT32, 10, 10)
PDCI_EBC2INT(PDCI_ebc2int64, PDCI_int64_2ebc, PDC_int64, PDC_MIN_INT64, PDC_MAX_INT64, 19, 19)

/* PDCI_EBC2UINT(fn_name, rev_fn_name, targ_type, int_max, nd_max) */
PDCI_EBC2UINT(PDCI_ebc2uint8,  PDCI_uint8_2ebc,  PDC_uint8,  PDC_MAX_UINT8,   3)
PDCI_EBC2UINT(PDCI_ebc2uint16, PDCI_uint16_2ebc, PDC_uint16, PDC_MAX_UINT16,  5)
PDCI_EBC2UINT(PDCI_ebc2uint32, PDCI_uint32_2ebc, PDC_uint32, PDC_MAX_UINT32, 10)
PDCI_EBC2UINT(PDCI_ebc2uint64, PDCI_uint64_2ebc, PDC_uint64, PDC_MAX_UINT64, 20)

/* PDCI_BCD2INT(fn_name, rev_fn_name, targ_type, int_min, int_max, nd_max, act_nd_max) */
PDCI_BCD2INT(PDCI_bcd2int8,  PDCI_int8_2bcd,  PDC_int8,  PDC_MIN_INT8,  PDC_MAX_INT8,   3, 3)
PDCI_BCD2INT(PDCI_bcd2int16, PDCI_int16_2bcd, PDC_int16, PDC_MIN_INT16, PDC_MAX_INT16,  5, 5)
PDCI_BCD2INT(PDCI_bcd2int32, PDCI_int32_2bcd, PDC_int32, PDC_MIN_INT32, PDC_MAX_INT32, 11, 10)
PDCI_BCD2INT(PDCI_bcd2int64, PDCI_int64_2bcd, PDC_int64, PDC_MIN_INT64, PDC_MAX_INT64, 19, 19)

/* PDCI_BCD2UINT(fn_name, rev_fn_name, targ_type, int_max, nd_max) */
PDCI_BCD2UINT(PDCI_bcd2uint8,  PDCI_uint8_2bcd,  PDC_uint8,  PDC_MAX_UINT8,   3)
PDCI_BCD2UINT(PDCI_bcd2uint16, PDCI_uint16_2bcd, PDC_uint16, PDC_MAX_UINT16,  5)
PDCI_BCD2UINT(PDCI_bcd2uint32, PDCI_uint32_2bcd, PDC_uint32, PDC_MAX_UINT32, 10)
PDCI_BCD2UINT(PDCI_bcd2uint64, PDCI_uint64_2bcd, PDC_uint64, PDC_MAX_UINT64, 20)

/* PDCI_SB2INT(fn_name, rev_fn_name, targ_type, sb_endian, int_min, int_max, nb_max) */
PDCI_SB2INT(PDCI_sbl2int8,  PDCI_int8_2sbl,  PDC_int8,  PDC_littleEndian, PDC_MIN_INT8,  PDC_MAX_INT8,  1)
PDCI_SB2INT(PDCI_sbl2int16, PDCI_int16_2sbl, PDC_int16, PDC_littleEndian, PDC_MIN_INT16, PDC_MAX_INT16, 2)
PDCI_SB2INT(PDCI_sbl2int32, PDCI_int32_2sbl, PDC_int32, PDC_littleEndian, PDC_MIN_INT32, PDC_MAX_INT32, 4)
PDCI_SB2INT(PDCI_sbl2int64, PDCI_int64_2sbl, PDC_int64, PDC_littleEndian, PDC_MIN_INT64, PDC_MAX_INT64, 8)

/* PDCI_SB2UINT(fn_name, rev_fn_name, targ_type, sb_endian, int_max, nb_max) */
PDCI_SB2UINT(PDCI_sbl2uint8,  PDCI_uint8_2sbl,  PDC_uint8,  PDC_littleEndian, PDC_MAX_UINT8,  1)
PDCI_SB2UINT(PDCI_sbl2uint16, PDCI_uint16_2sbl, PDC_uint16, PDC_littleEndian, PDC_MAX_UINT16, 2)
PDCI_SB2UINT(PDCI_sbl2uint32, PDCI_uint32_2sbl, PDC_uint32, PDC_littleEndian, PDC_MAX_UINT32, 4)
PDCI_SB2UINT(PDCI_sbl2uint64, PDCI_uint64_2sbl, PDC_uint64, PDC_littleEndian, PDC_MAX_UINT64, 8)

/* PDCI_SB2INT(fn_name, rev_fn_name, targ_type, sb_endian, int_min, int_max, nb_max) */
PDCI_SB2INT(PDCI_sbh2int8,  PDCI_int8_2sbh,  PDC_int8,  PDC_bigEndian, PDC_MIN_INT8,  PDC_MAX_INT8,  1)
PDCI_SB2INT(PDCI_sbh2int16, PDCI_int16_2sbh, PDC_int16, PDC_bigEndian, PDC_MIN_INT16, PDC_MAX_INT16, 2)
PDCI_SB2INT(PDCI_sbh2int32, PDCI_int32_2sbh, PDC_int32, PDC_bigEndian, PDC_MIN_INT32, PDC_MAX_INT32, 4)
PDCI_SB2INT(PDCI_sbh2int64, PDCI_int64_2sbh, PDC_int64, PDC_bigEndian, PDC_MIN_INT64, PDC_MAX_INT64, 8)

/* PDCI_SB2UINT(fn_name, rev_fn_name, targ_type, sb_endian, int_max, nb_max) */
PDCI_SB2UINT(PDCI_sbh2uint8,  PDCI_uint8_2sbh,  PDC_uint8,  PDC_bigEndian, PDC_MAX_UINT8,  1)
PDCI_SB2UINT(PDCI_sbh2uint16, PDCI_uint16_2sbh, PDC_uint16, PDC_bigEndian, PDC_MAX_UINT16, 2)
PDCI_SB2UINT(PDCI_sbh2uint32, PDCI_uint32_2sbh, PDC_uint32, PDC_bigEndian, PDC_MAX_UINT32, 4)
PDCI_SB2UINT(PDCI_sbh2uint64, PDCI_uint64_2sbh, PDC_uint64, PDC_bigEndian, PDC_MAX_UINT64, 8)

/* ********************************* BEGIN_TRAILER ******************************** */
/* ********************************** END_MACGEN ********************************** */
/* DEFGEN(libpadsc-gen.c) */
/*
 * library routines for library that goes with padsc
 *
 * Kathleen Fisher, Robert Gruber
 * AT&T Labs Research
 */

#gen_include "libpadsc-internal.h"
#gen_include "libpadsc-macros-gen.h"

static const char id[] = "\n@(#)$Id: pads.c,v 1.66 2003-04-25 20:46:14 gruber Exp $\0\n";

static const char lib[] = "padsc";

/* ================================================================================ */ 
/* IMPL CONSTANTS */

#define PDCI_initStkElts      8

/* ================================================================================
 * ASCII CHAR TABLES
 */

/* ASCII digits are 0x3[0-9] */
int PDCI_ascii_digit[256] = {
  /* 0x0? */ -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
  /* 0x1? */ -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
  /* 0x2? */ -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
  /* 0x3? */  0,  1,  2,  3,  4,  5,  6,  7,  8,  9, -1, -1, -1, -1, -1, -1,
  /* 0x4? */ -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
  /* 0x5? */ -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
  /* 0x6? */ -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
  /* 0x7? */ -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
  /* 0x8? */ -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
  /* 0x9? */ -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
  /* 0xA? */ -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
  /* 0xB? */ -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
  /* 0xC? */ -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
  /* 0xD? */ -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
  /* 0xE? */ -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
  /* 0xF? */ -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
};

/* ASCII digits are 0x3[0-9] */
int PDCI_ascii_is_digit[256] = {
  /* 0x0? */  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
  /* 0x1? */  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
  /* 0x2? */  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
  /* 0x3? */  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  0,  0,  0,  0,  0,  0,
  /* 0x4? */  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
  /* 0x5? */  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
  /* 0x6? */  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
  /* 0x7? */  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
  /* 0x8? */  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
  /* 0x9? */  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
  /* 0xA? */  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
  /* 0xB? */  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
  /* 0xC? */  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
  /* 0xD? */  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
  /* 0xE? */  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
  /* 0xF? */  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
};

/* ASCII spaces : 0x09:HT, 0x0A:LF, 0x0B:VT, 0x0C:FF, 0x0D:CR, 0x20:SP */
int PDCI_ascii_is_space[256] = {
  /* 0x0? */  0,  0,  0,  0,  0,  0,  0,  0,  0,  1,  1,  1,  1,  1,  0,  0,
  /* 0x1? */  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
  /* 0x2? */  1,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
  /* 0x3? */  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
  /* 0x4? */  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
  /* 0x5? */  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
  /* 0x6? */  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
  /* 0x7? */  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
  /* 0x8? */  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
  /* 0x9? */  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
  /* 0xA? */  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
  /* 0xB? */  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
  /* 0xC? */  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
  /* 0xD? */  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
  /* 0xE? */  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
  /* 0xF? */  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
};

/* ================================================================================
 * EBCDIC CHAR TABLES : tables for EBCDIC char conversion
 *   -- from Andrew Hume (ng_ebcdic.c)
 *
 * ================================================================================ */

/* not-sign 0xac -> circumflex 0x5e */
 /* non-spacing macron 0xaf -> tilde 0x7e */
PDC_byte PDC_ea_tab[256] =
{
  /* 0x0? */ 0x00, 0x01, 0x02, 0x03, '?',  0x09, '?',  0x7f, '?',  '?',  '?',  0x0b, 0x0c, 0x0d, 0x0e, 0x0f,
  /* 0x1? */ 0x10, 0x11, 0x12, 0x13, '?',  '?',  0x08,'?',   0x18, 0x09, '?',  '?',  0x1c, 0x1d, 0x1e, 0x1f,
  /* 0x2? */ '?',  '?',  '?',  '?',  '?',  0x0a, 0x17, 0x1b, '?',  '?',  '?',  '?',  '?',  0x05, 0x06, 0x07,
  /* 0x3? */ '?',  '?',  0x16, '?',  '?',  '?',  '?',  0x04, '?',  '?',  '?',  '?',  0x14, 0x15, '?',  0x1a,
  /* 0x4? */ 0x20, '?',  '?',  '?',  '?',  '?',  '?', '?',   '?',  '?',  0x5b, 0x2e, 0x3c, 0x28, 0x2b, 0x21,
  /* 0x5? */ 0x26, '?',  '?',  '?',  '?',  '?',  '?', '?',   '?',  '?',  0x5d, 0x24, 0x2a, 0x29, 0x3b, 0x5e,
  /* 0x6? */ 0x2d, 0x2f, '?',  '?',  '?',  '?',  '?', '?',   '?',  '?',  0x7c, 0x2c, 0x25, 0x5f, 0x3e, 0x3f,
  /* 0x7? */ '?',  '?',  '?',  '?',  '?',  '?',  '?', '?',   '?',  0x60, 0x3a, 0x23, 0x40, 0x27, 0x3d, 0x22,
  /* 0x8? */ '?',  0x61, 0x62, 0x63, 0x64, 0x65, 0x66, 0x67, 0x68, 0x69, '?',  '?',  '?',  '?',  '?', '?',
  /* 0x9? */ '?',  0x6a, 0x6b, 0x6c, 0x6d, 0x6e, 0x6f, 0x70, 0x71, 0x72, '?',  '?',  '?',  '?',  '?', '?',
  /* 0xA? */ '?',  0x7e, 0x73, 0x74, 0x75, 0x76, 0x77, 0x78, 0x79, 0x7a, '?',  '?',  '?',  '?',  '?', '?',
  /* 0xB? */ '?',  '?',  '?',  '?',  '?',  '?',  '?', '?',   '?',  '?',  '?',  '?',  '?',  '?',  '?', '?',
  /* 0xC? */ 0x7b, 0x41, 0x42, 0x43, 0x44, 0x45, 0x46, 0x47, 0x48, 0x49, '?',  '?',  '?',  '?',  '?', '?',
  /* 0xD? */ 0x7d, 0x4a, 0x4b, 0x4c, 0x4d, 0x4e, 0x4f, 0x50, 0x51, 0x52, '?',  '?',  '?',  '?',  '?', '?',
  /* 0xE? */ 0x5c, '?',  0x53, 0x54, 0x55, 0x56, 0x57, 0x58, 0x59, 0x5a, '?',  '?',  '?',  '?',  '?', '?',
  /* 0xF? */ 0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37, 0x38, 0x39, '?',  '?',  '?',  '?',  '?', '?',
};

PDC_byte PDC_ae_tab[256] =
{
  /* 0x0? */ 0x00, 0x01, 0x02, 0x03, 0x37, 0x2d, 0x2e, 0x2f, 0x16, 0x19, 0x25, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f, 
  /* 0x1? */ 0x10, 0x11, 0x12, 0x13, 0x3c, 0x3d, 0x32, 0x26, 0x18, '?',  0x3f, 0x27, 0x1c, 0x1d, 0x1e, 0x1f, 
  /* 0x2? */ 0x40, 0x4f, 0x7f, 0x7b, 0x5b, 0x6c, 0x50, 0x7d, 0x4d, 0x5d, 0x5c, 0x4e, 0x6b, 0x60, 0x4b, 0x61, 
  /* 0x3? */ 0xf0, 0xf1, 0xf2, 0xf3, 0xf4, 0xf5, 0xf6, 0xf7, 0xf8, 0xf9, 0x7a, 0x5e, 0x4c, 0x7e, 0x6e, 0x6f, 
  /* 0x4? */ 0x7c, 0xc1, 0xc2, 0xc3, 0xc4, 0xc5, 0xc6, 0xc7, 0xc8, 0xc9, 0xd1, 0xd2, 0xd3, 0xd4, 0xd5, 0xd6, 
  /* 0x5? */ 0xd7, 0xd8, 0xd9, 0xe2, 0xe3, 0xe4, 0xe5, 0xe6, 0xe7, 0xe8, 0xe9, 0x4a, 0xe0, 0x5a, 0x5f, 0x6d, 
  /* 0x6? */ 0x79, 0x81, 0x82, 0x83, 0x84, 0x85, 0x86, 0x87, 0x88, 0x89, 0x91, 0x92, 0x93, 0x94, 0x95, 0x96, 
  /* 0x7? */ 0x97, 0x98, 0x99, 0xa2, 0xa3, 0xa4, 0xa5, 0xa6, 0xa7, 0xa8, 0xa9, 0xc0, 0x6a, 0xd0, 0xa1, 0x07, 
  /* 0x8? */ '?',  '?',  '?',  '?',  '?',  '?',  '?',  '?',  '?',  '?',  '?',  '?',  '?',  '?',  '?',  '?',  
  /* 0x9? */ '?',  '?',  '?',  '?',  '?',  '?',  '?',  '?',  '?',  '?',  '?',  '?',  '?',  '?',  '?',  '?',  
  /* 0xA? */ '?',  '?',  '?',  '?',  '?',  '?',  '?',  '?',  '?',  '?',  '?',  '?',  '?',  '?',  '?',  '?',  
  /* 0xB? */ '?',  '?',  '?',  '?',  '?',  '?',  '?',  '?',  '?',  '?',  '?',  '?',  '?',  '?',  '?',  '?',  
  /* 0xC? */ '?',  '?',  '?',  '?',  '?',  '?',  '?',  '?',  '?',  '?',  '?',  '?',  '?',  '?',  '?',  '?',  
  /* 0xD? */ '?',  '?',  '?',  '?',  '?',  '?',  '?',  '?',  '?',  '?',  '?',  '?',  '?',  '?',  '?',  '?',  
  /* 0xE? */ '?',  '?',  '?',  '?',  '?',  '?',  '?',  '?',  '?',  '?',  '?',  '?',  '?',  '?',  '?',  '?',  
  /* 0xF? */ '?',  '?',  '?',  '?',  '?',  '?',  '?',  '?',  '?',  '?',  '?',  '?',  '?',  '?',  '?',  '?',  
};

/* replaced '?' with 0xff
 * '?' equals EBCDIC SUB (substitute) character, would rather
 * use 0xff which is unspecified
 */
PDC_byte PDC_mod_ae_tab[256] =
{
  /* 0x0? */ 0x00, 0x01, 0x02, 0x03, 0x37, 0x2d, 0x2e, 0x2f, 0x16, 0x19, 0x25, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f, 
  /* 0x1? */ 0x10, 0x11, 0x12, 0x13, 0x3c, 0x3d, 0x32, 0x26, 0x18, 0xff, 0x3f, 0x27, 0x1c, 0x1d, 0x1e, 0x1f, 
  /* 0x2? */ 0x40, 0x4f, 0x7f, 0x7b, 0x5b, 0x6c, 0x50, 0x7d, 0x4d, 0x5d, 0x5c, 0x4e, 0x6b, 0x60, 0x4b, 0x61, 
  /* 0x3? */ 0xf0, 0xf1, 0xf2, 0xf3, 0xf4, 0xf5, 0xf6, 0xf7, 0xf8, 0xf9, 0x7a, 0x5e, 0x4c, 0x7e, 0x6e, 0x6f, 
  /* 0x4? */ 0x7c, 0xc1, 0xc2, 0xc3, 0xc4, 0xc5, 0xc6, 0xc7, 0xc8, 0xc9, 0xd1, 0xd2, 0xd3, 0xd4, 0xd5, 0xd6, 
  /* 0x5? */ 0xd7, 0xd8, 0xd9, 0xe2, 0xe3, 0xe4, 0xe5, 0xe6, 0xe7, 0xe8, 0xe9, 0x4a, 0xe0, 0x5a, 0x5f, 0x6d, 
  /* 0x6? */ 0x79, 0x81, 0x82, 0x83, 0x84, 0x85, 0x86, 0x87, 0x88, 0x89, 0x91, 0x92, 0x93, 0x94, 0x95, 0x96, 
  /* 0x7? */ 0x97, 0x98, 0x99, 0xa2, 0xa3, 0xa4, 0xa5, 0xa6, 0xa7, 0xa8, 0xa9, 0xc0, 0x6a, 0xd0, 0xa1, 0x07, 
  /* 0x8? */ 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 
  /* 0x9? */ 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 
  /* 0xA? */ 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 
  /* 0xB? */ 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 
  /* 0xC? */ 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 
  /* 0xD? */ 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 
  /* 0xE? */ 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 
  /* 0xF? */ 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 
};

/* EBCDIC digits are 0xC[0-9], 0xD[0-9], 0XF[0-9] */
/* aka 192-201, 208-217, 240-249 */
int PDCI_ebcdic_digit[256] = {
  /* 0x0? */ -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
  /* 0x1? */ -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
  /* 0x2? */ -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
  /* 0x3? */ -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
  /* 0x4? */ -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
  /* 0x5? */ -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
  /* 0x6? */ -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
  /* 0x7? */ -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
  /* 0x8? */ -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
  /* 0x9? */ -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
  /* 0xA? */ -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
  /* 0xB? */ -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
  /* 0xC? */  0,  1,  2,  3,  4,  5,  6,  7,  8,  9, -1, -1, -1, -1, -1, -1,
  /* 0xD? */  0,  1,  2,  3,  4,  5,  6,  7,  8,  9, -1, -1, -1, -1, -1, -1,
  /* 0xE? */ -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
  /* 0xF? */  0,  1,  2,  3,  4,  5,  6,  7,  8,  9, -1, -1, -1, -1, -1, -1,
};

/* EBCDIC digits are 0xC[0-9], 0xD[0-9], 0XF[0-9] */
int PDCI_ebcdic_is_digit[256] = {
  /* 0x0? */  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
  /* 0x1? */  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
  /* 0x2? */  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
  /* 0x3? */  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
  /* 0x4? */  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
  /* 0x5? */  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
  /* 0x6? */  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
  /* 0x7? */  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
  /* 0x8? */  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
  /* 0x9? */  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
  /* 0xA? */  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
  /* 0xB? */  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
  /* 0xC? */  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  0,  0,  0,  0,  0,  0,
  /* 0xD? */  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  0,  0,  0,  0,  0,  0,
  /* 0xE? */  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
  /* 0xF? */  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  0,  0,  0,  0,  0,  0,
};

/* EBCDIC spaces : 0x05:HT, 0x0B:VT, 0x0C:FF, 0x0D:CR, 0x15:NL, 0x40:SP */
int PDCI_ebcdic_is_space[256] = {
  /* 0x0? */  0,  0,  0,  0,  0,  1,  0,  0,  0,  0,  0,  1,  1,  1,  0,  0,
  /* 0x1? */  0,  0,  0,  0,  0,  1,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
  /* 0x2? */  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
  /* 0x3? */  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
  /* 0x4? */  1,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
  /* 0x5? */  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
  /* 0x6? */  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
  /* 0x7? */  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
  /* 0x8? */  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
  /* 0x9? */  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
  /* 0xA? */  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
  /* 0xB? */  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
  /* 0xC? */  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
  /* 0xD? */  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
  /* 0xE? */  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
  /* 0xF? */  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
};

/* ================================================================================
 * BCD TABLES : tables for BCD conversion
 *     -- from Andrew Hume (ng_bcd.c)
 *
 * ================================================================================ */

int PDCI_bcd_hilo_digits[256] = {
  /* 0x0? */  0,  1,  2,  3,  4,  5,  6,  7,  8,  9, -1, -1, -1, -1, -1, -1,
  /* 0x1? */ 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, -1, -1, -1, -1, -1, -1,
  /* 0x2? */ 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, -1, -1, -1, -1, -1, -1,
  /* 0x3? */ 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, -1, -1, -1, -1, -1, -1,
  /* 0x4? */ 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, -1, -1, -1, -1, -1, -1,
  /* 0x5? */ 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, -1, -1, -1, -1, -1, -1,
  /* 0x6? */ 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, -1, -1, -1, -1, -1, -1,
  /* 0x7? */ 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, -1, -1, -1, -1, -1, -1,
  /* 0x8? */ 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, -1, -1, -1, -1, -1, -1,
  /* 0x9? */ 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, -1, -1, -1, -1, -1, -1,
  /* 0xA? */ -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
  /* 0xB? */ -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
  /* 0xC? */ -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
  /* 0xD? */ -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
  /* 0xE? */ -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
  /* 0xF? */ -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
};

int PDCI_bcd_hi_digit[256] = {
  /* 0x0? */  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
  /* 0x1? */  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,
  /* 0x2? */  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,
  /* 0x3? */  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,
  /* 0x4? */  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,
  /* 0x5? */  5,  5,  5,  5,  5,  5,  5,  5,  5,  5,  5,  5,  5,  5,  5,  5,
  /* 0x6? */  6,  6,  6,  6,  6,  6,  6,  6,  6,  6,  6,  6,  6,  6,  6,  6,
  /* 0x7? */  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,
  /* 0x8? */  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,
  /* 0x9? */  9,  9,  9,  9,  9,  9,  9,  9,  9,  9,  9,  9,  9,  9,  9,  9,
  /* 0xA? */ -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
  /* 0xB? */ -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
  /* 0xC? */ -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
  /* 0xD? */ -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
  /* 0xE? */ -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
  /* 0xF? */ -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
};

#if 0
/* XXX the only valid 2nd nible is  C, D, or F, so an alternate
 * XXX form of the above would be: */
int PDCI_bcd_hi_digit[256] = {
  /* 0x0? */ -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,  0,  0, -1,  0,
  /* 0x1? */ -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,  1,  1, -1,  1,
  /* 0x2? */ -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,  2,  2, -1,  2,
  /* 0x3? */ -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,  3,  3, -1,  3,
  /* 0x4? */ -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,  4,  4, -1,  4,
  /* 0x5? */ -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,  5,  5, -1,  5,
  /* 0x6? */ -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,  6,  6, -1,  6,
  /* 0x7? */ -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,  7,  7, -1,  7,
  /* 0x8? */ -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,  8,  8, -1,  8,
  /* 0x9? */ -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,  9,  9, -1,  9,
  /* 0xA? */ -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
  /* 0xB? */ -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
  /* 0xC? */ -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
  /* 0xD? */ -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
  /* 0xE? */ -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
  /* 0xF? */ -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
};
#endif

/* ================================================================================
 * MISC TABLES
 * ================================================================================ */

PDC_uint64 PDCI_10toThe[] = {
  /* 10^0  = */                          1ULL,
  /* 10^1  = */                         10ULL,
  /* 10^2  = */                        100ULL,
  /* 10^3  = */                       1000ULL,
  /* 10^4  = */                      10000ULL,
  /* 10^5  = */                     100000ULL,
  /* 10^6  = */                    1000000ULL,
  /* 10^7  = */                   10000000ULL,
  /* 10^8  = */                  100000000ULL,
  /* 10^9  = */                 1000000000ULL,
  /* 10^10 = */                10000000000ULL,
  /* 10^11 = */               100000000000ULL,
  /* 10^12 = */              1000000000000ULL,
  /* 10^13 = */             10000000000000ULL,
  /* 10^14 = */            100000000000000ULL,
  /* 10^15 = */           1000000000000000ULL,
  /* 10^16 = */          10000000000000000ULL,
  /* 10^17 = */         100000000000000000ULL,
  /* 10^18 = */        1000000000000000000ULL,
  /* 10^19 = */       10000000000000000000ULL
};

/********************************************************************************
 * EXTERNAL FUNCTIONS (see libpadsc.h)
 ********************************************************************************/

/* ================================================================================ */ 
/* EXTERNAL ERROR REPORTING FUNCTIONS */

int
PDC_errorf(const char *libnm, int level, ...)
{
  va_list ap;
  va_start(ap, level);
  errorv(libnm, (libnm ? level|ERROR_LIBRARY : level), ap);
  va_end(ap);
  return 0;
}

/* ================================================================================ */
/* EXTERNAL LIBRARY TOP-LEVEL OPEN/CLOSE FUNCTIONS */

/* The default disc */
PDC_disc_t PDC_default_disc = {
  PDC_VERSION,
  (PDC_flags_t)PDC_NULL_CTL_FLAG,
  0, /* string read functions do not copy strings */
  0, /* stop_regexp disabled    */
  0, /* no stop_maxlen disabled */
  PDC_errorf,
  PDC_errorRep_Max,
  PDC_littleEndian,
  0 /* a default IO discipline is installed on PDC_open */
};

PDC_error_t
PDC_open(PDC_t **pdc_out, PDC_disc_t *disc, PDC_IO_disc_t *io_disc)
{
  Vmalloc_t    *vm;
  PDC_t        *pdc;
  PDC_int32     testint = 2;

  PDC_TRACE(&PDC_default_disc, "PDC_open called");
  if (!pdc_out) {
    PDC_WARN(&PDC_default_disc, "PDC_open: param pdc_out must not be NULL");
    return PDC_ERR;
  }
  if (!(vm = vmopen(Vmdcheap, Vmbest, 0))) {
    goto fatal_alloc_err;
  }
  if (!disc) { /* copy the default discipline */
    if (!(disc = vmnewof(vm, 0, PDC_disc_t, 1, 0))) {
      disc = &PDC_default_disc;
      goto fatal_alloc_err;
    }
    (*disc) = PDC_default_disc;
  }
  if (io_disc) {
    disc->io_disc = io_disc;
  } else if (!disc->io_disc) {
    PDC_WARN(disc, "Installing default IO discipline : newline-terminated records");
    if (!(disc->io_disc = PDC_ctrec_noseek_make('\n', 0))) {
      PDC_FATAL(disc, "Unexpected failure to install default IO discipline");
    }
  }
  if (!(pdc = vmnewof(vm, 0, PDC_t, 1, 0))) {
    goto fatal_alloc_err;
  }
#if 1
  /* allocate a 1 MB + 1 byte buffer to use with sfio */
  if (!(pdc->sfbuf = vmoldof(vm, 0, PDC_byte, 1024 * 1024, 1))) {
    goto fatal_alloc_err;
  } 
#endif
  pdc->inestlev = 0;
  if (!(pdc->tmp = sfstropen())) {
    goto fatal_alloc_err;
  }
  if (!(pdc->rmm_z = RMM_open(RMM_zero_disc_ptr))) {
    goto fatal_alloc_err;
  }
  if (!(pdc->rmm_nz = RMM_open(RMM_nozero_disc_ptr))) {
    goto fatal_alloc_err;
  }
  pdc->m_endian = (((char*)(&testint))[0]) ? PDC_littleEndian : PDC_bigEndian;
  pdc->id          = lib;
  pdc->vm          = vm;
  pdc->disc        = disc;
  if (!(pdc->head = vmnewof(vm, 0, PDC_IO_elt_t, 1, 0))) {
    goto fatal_alloc_err;
  }
  pdc->head->next = pdc->head;
  pdc->head->prev = pdc->head;

  pdc->salloc = PDCI_initStkElts;
  if (!(pdc->stack = vmnewof(vm, 0, PDCI_stkElt_t, pdc->salloc, 0))) {
    goto fatal_alloc_err;
  }
  PDC_string_init(pdc, &pdc->stmp1);
  PDC_string_init(pdc, &pdc->stmp2);
  /* These fields are 0/NiL due to zero-based alloc of pdc:
   *   path, io_state, top, buf, balloc, bchars, speclev
   */
  (*pdc_out) = pdc;
  return PDC_OK;

 fatal_alloc_err:
  PDC_FATAL(disc, "out of space error during PDC_open");
  if (pdc) {
    if (pdc->rmm_z) {
      RMM_close(pdc->rmm_z);
    }
    if (pdc->rmm_nz) {
      RMM_close(pdc->rmm_nz);
    }
    if (pdc->tmp) {
      sfstrclose(pdc->tmp);
    }
  }
  if (vm) {
    vmclose(vm);
  }
  return PDC_ERR;
}

PDC_error_t
PDC_close(PDC_t *pdc)
{

  PDCI_DISC_INIT_CHECKS("PDC_close");
  PDC_string_cleanup(pdc, &pdc->stmp1);
  PDC_string_cleanup(pdc, &pdc->stmp2);
  if (pdc->disc->io_disc) {
    pdc->disc->io_disc->unmake_fn(pdc, pdc->disc->io_disc);
  }
  if (pdc->rmm_z) {
    RMM_close(pdc->rmm_z);
  }
  if (pdc->rmm_nz) {
    RMM_close(pdc->rmm_nz);
  }
  if (pdc->tmp) {
    sfstrclose(pdc->tmp);
  }
  if (pdc->vm) {
    vmclose(pdc->vm); /* frees everything alloc'd using vm */
  }
  return PDC_OK;
}


/* ================================================================================ */
/* EXTERNAL DISCIPLINE GET/SET FUNCTIONS */

PDC_disc_t *
PDC_get_disc(PDC_t *pdc)
{
  return (pdc ? pdc->disc : 0);
}

PDC_error_t
PDC_set_disc(PDC_t *pdc, PDC_disc_t *new_disc, int xfer_io)
{
  PDCI_DISC_INIT_CHECKS("PDC_set_disc");
  PDCI_NULLPARAM_CHECK("PDC_set_disc", new_disc);
  if (xfer_io) {
    if (new_disc->io_disc) {
      PDC_WARN(pdc->disc, "PDC_set_disc: Cannot transfer IO discipline when new_disc->io_disc is non-NULL");
      return PDC_ERR;
    }
    new_disc->io_disc = pdc->disc->io_disc;
    pdc->disc->io_disc = 0;
  }
  pdc->disc = new_disc;
  return PDC_OK;
}

PDC_error_t
PDC_set_IO_disc(PDC_t* pdc, PDC_IO_disc_t* new_io_disc)
{
  PDCI_stkElt_t    *bot       = &(pdc->stack[0]);
  PDC_IO_elt_t     *io_elt    = bot->elt;
  size_t           io_remain  = bot->remain;

  PDCI_DISC_INIT_CHECKS("PDC_set_disc");
  PDCI_NULLPARAM_CHECK("PDC_set_disc", new_io_disc);
  if (pdc->top != 0) {
    PDC_WARN(pdc->disc, "PDC_set_IO_disc: cannot change IO discipline "
	     "in the middle of a speculative read function (e.g., union, ...)");
    return PDC_ERR;
  }
  if (pdc->io && pdc->disc->io_disc) {
    /* do a clean sfclose */
    if (PDC_ERR == pdc->disc->io_disc->sfclose_fn(pdc, pdc->disc->io_disc, io_elt, io_remain)) {
      /* XXX perhaps it was not open?? */
    }
  }
  if (pdc->disc->io_disc) {
    /* unmake the previous discipline */
    if (PDC_ERR == pdc->disc->io_disc->unmake_fn(pdc, pdc->disc->io_disc)) {
      /* XXX report an error ??? */
    }
  }
  pdc->disc->io_disc = new_io_disc;
  if (pdc->io) {
    if (PDC_ERR == pdc->disc->io_disc->sfopen_fn(pdc, new_io_disc, pdc->io, pdc->head)) {
      /* XXX report an error ??? */
    }
  }
  return PDC_OK;
}

/* ================================================================================ */
/* EXTERNAL RMM ACCESSORS */

RMM_t *
PDC_rmm_zero(PDC_t *pdc)
{
  return (pdc ? pdc->rmm_z : 0);
}

RMM_t *
PDC_rmm_nozero(PDC_t *pdc)
{
  return (pdc ? pdc->rmm_nz : 0);
}

/* ================================================================================ */
/* EXTERNAL IO FUNCTIONS */

PDC_error_t
PDC_IO_set(PDC_t *pdc, Sfio_t *io)
{
  PDCI_DISC_INIT_CHECKS("PDC_IO_set");
  PDCI_NULLPARAM_CHECK("PDC_IO_set", io);
  return PDC_IO_set_internal(pdc, io);
}

PDC_error_t
PDC_IO_fopen(PDC_t *pdc, char *path)
{
  PDCI_DISC_INIT_CHECKS("PDC_IO_fopen");
  PDCI_NULLPARAM_CHECK("PDC_IO_fopen", path);
  return PDC_IO_fopen_internal(pdc, path);
}

PDC_error_t
PDC_IO_close(PDC_t *pdc)
{
  PDCI_DISC_INIT_CHECKS("PDC_IO_close");
  return PDC_IO_close_internal(pdc);
}

PDC_error_t
PDC_IO_next_rec(PDC_t *pdc, size_t *skipped_bytes_out) {
  size_t  tmp_skipped_bytes;

  PDCI_IODISC_INIT_CHECKS("PDC_IO_next_rec");
  if (!skipped_bytes_out) {
    skipped_bytes_out = &tmp_skipped_bytes;
  }
  return PDC_IO_next_rec_internal(pdc, skipped_bytes_out);
}

int
PDC_IO_at_EOR(PDC_t *pdc) {
  PDCI_DISC_INIT_CHECKS("PDC_IO_at_EOR");
  return PDC_IO_at_EOR_internal(pdc);
}

int
PDC_IO_at_EOF(PDC_t *pdc) {
  PDCI_DISC_INIT_CHECKS("PDC_IO_at_EOF");
  return PDC_IO_at_EOF_internal(pdc);
}

PDC_error_t
PDC_IO_getPos(PDC_t *pdc, PDC_pos_t *pos, int offset)
{
  PDCI_DISC_INIT_CHECKS("PDC_IO_getPos");
  PDCI_NULLPARAM_CHECK("PDC_IO_getPos", pos);
  return PDC_IO_getPos_internal(pdc, pos, offset);
}

PDC_error_t
PDC_IO_getLocB(PDC_t *pdc, PDC_loc_t *loc, int offset)
{
  PDCI_DISC_INIT_CHECKS("PDC_IO_getLocB");
  PDCI_NULLPARAM_CHECK("PDC_IO_getLocB", loc);
  return PDC_IO_getPos_internal(pdc, &(loc->b), offset);
}

PDC_error_t
PDC_IO_getLocE(PDC_t *pdc, PDC_loc_t *loc, int offset)
{
  PDCI_DISC_INIT_CHECKS("PDC_IO_getLocE");
  PDCI_NULLPARAM_CHECK("PDC_IO_getLocE", loc);
  return PDC_IO_getPos_internal(pdc, &(loc->e), offset);
}

PDC_error_t
PDC_IO_getLoc(PDC_t *pdc, PDC_loc_t *loc, int offset)
{
  PDCI_DISC_INIT_CHECKS("PDC_IO_getLoc");
  PDCI_NULLPARAM_CHECK("PDC_IO_getLoc", loc);
  if (PDC_ERR == PDC_IO_getPos_internal(pdc, &(loc->b), offset)) {
    return PDC_ERR;
  }
  loc->e = loc->b;
  if (loc->e.byte) {
    (loc->e.byte)--;
  }
  return PDC_OK;
}

/* ================================================================================ */
/* EXTERNAL IO CHECKPOINT API */

PDC_error_t
PDC_IO_checkpoint(PDC_t *pdc, int speculative)
{
  if (!pdc || !pdc->disc) { return PDC_ERR; }
  PDC_TRACE(pdc->disc, "PDC_IO_checkpoint called");
  if (++(pdc->top) >= pdc->salloc) {
    PDCI_stkElt_t *stack_next;
    size_t salloc_next = 2 * pdc->salloc;
    /* PDC_DBG2(pdc->disc, "XXX_REMOVE Growing from %d to %d checkpoint stack slots", pdc->salloc, salloc_next); */
    if (!(stack_next = vmnewof(pdc->vm, pdc->stack, PDCI_stkElt_t, salloc_next, 0))) {
      PDC_FATAL(pdc->disc, "out of space [input cursor stack]");
      return PDC_ERR;
    }
    pdc->stack  = stack_next;
    pdc->salloc = salloc_next;
  }
  pdc->stack[pdc->top].elt     = pdc->stack[pdc->top - 1].elt;
  pdc->stack[pdc->top].remain  = pdc->stack[pdc->top - 1].remain;
  pdc->stack[pdc->top].spec    = speculative;
  if (speculative) {
    (pdc->speclev)++;
  }
  return PDC_OK;
}

PDC_error_t
PDC_IO_restore(PDC_t *pdc)
{
  if (!pdc || !pdc->disc) { return PDC_ERR; }
  PDC_TRACE(pdc->disc, "PDC_IO_restore called");
  if (pdc->top <= 0) {
    PDC_WARN(pdc->disc, "Internal error: PDC_IO_restore called when stack top <= 0");
    return PDC_ERR;
  }
  if (pdc->stack[pdc->top].spec) {
    (pdc->speclev)--;
  }
  /* this discards all changes since the latest checkpoint */ 
  (pdc->top)--;
  return PDC_OK;
}

PDC_error_t
PDC_IO_commit(PDC_t *pdc)
{
  if (!pdc || !pdc->disc) { return PDC_ERR; }
  PDC_TRACE(pdc->disc, "PDC_IO_commit called");
  if (pdc->top <= 0) {
    PDC_WARN(pdc->disc, "Internal error: PDC_IO_commit called when stack top <= 0");
    return PDC_ERR;
  }
  if (pdc->stack[pdc->top].spec) {
    (pdc->speclev)--;
  }
  /* propagate changes to elt/remain up to next level */
  pdc->stack[pdc->top - 1].elt    = pdc->stack[pdc->top].elt;
  pdc->stack[pdc->top - 1].remain = pdc->stack[pdc->top].remain;
  (pdc->top)--;
  return PDC_OK;
}

unsigned int
PDC_spec_level(PDC_t *pdc)
{
  if (!pdc || !pdc->disc) { return PDC_ERR; }
  PDC_TRACE(pdc->disc, "PDC_spec_level called");
  return pdc->speclev;
}

/* ================================================================================ */
/* EXTERNAL LITERAL READ FUNCTIONS */

PDC_error_t
PDC_a_char_lit_read(PDC_t *pdc, PDC_base_em *em,
		   PDC_base_ed *ed, PDC_byte c)
{
  PDC_base_em     emt = PDC_CheckAndSet;
  PDC_base_ed     edt;

  if (!em) {
    em = &emt;
  }
  if (!ed) {
    ed = &edt;
  }
  PDCI_IODISC_INIT_CHECKS("PDC_a_char_lit_read");
  return PDC_char_lit_read_internal(pdc, em, ed, c);
}

PDC_error_t
PDC_a_str_lit_read(PDC_t *pdc, PDC_base_em *em,
		  PDC_base_ed *ed, const PDC_string *s)
{
  PDC_base_em     emt = PDC_CheckAndSet;
  PDC_base_ed     edt;

  if (!em) {
    em = &emt;
  }
  if (!ed) {
    ed = &edt;
  }
  PDCI_IODISC_INIT_CHECKS("PDC_a_str_lit_read");
  return PDC_str_lit_read_internal(pdc, em, ed, s);
}

PDC_error_t
PDC_countX(PDC_t *pdc, PDC_base_em *em, PDC_uint8 x, int eor_required,
	   PDC_base_ed *ed, PDC_int32 *res_out)
{
  PDC_base_em     emt = PDC_CheckAndSet;
  PDC_base_ed     edt;

  if (!em) {
    em = &emt;
  }
  if (!ed) {
    ed = &edt;
  }
  PDCI_IODISC_INIT_CHECKS("PDC_countX");
  PDCI_NULLPARAM_CHECK("PDC_countX", res_out);
  return PDC_countX_internal(pdc, em, x, eor_required, ed, res_out);
}

PDC_error_t
PDC_countXtoY(PDC_t *pdc, PDC_base_em *em, PDC_uint8 x, PDC_uint8 y,
	      PDC_base_ed *ed, PDC_int32 *res_out)
{
  PDC_base_em     emt = PDC_CheckAndSet;
  PDC_base_ed     edt;

  if (!em) {
    em = &emt;
  }
  if (!ed) {
    ed = &edt;
  }
  PDCI_IODISC_INIT_CHECKS("PDC_countXtoY");
  PDCI_NULLPARAM_CHECK("PDC_countXtoY", res_out);
  return PDC_countXtoY_internal(pdc, em, x, y, ed, res_out);
}

/* ================================================================================ */
/* EXTERNAL DATE/TIME READ FUNCTIONS */

PDC_error_t
PDC_a_date_read(PDC_t *pdc, PDC_base_em *em, PDC_byte stopChar,
		PDC_base_ed *ed, PDC_uint32 *res_out)
{
  PDC_base_em     emt = PDC_CheckAndSet;
  PDC_base_ed     edt;

  if (!em) {
    em = &emt;
  }
  if (!ed) {
    ed = &edt;
  }
  PDCI_IODISC_INIT_CHECKS("PDC_a_date_read");
  return PDC_a_date_read_internal(pdc, em, stopChar, ed, res_out);
}

PDC_error_t
PDC_e_date_read(PDC_t *pdc, PDC_base_em *em, PDC_byte stopChar,
		PDC_base_ed *ed, PDC_uint32 *res_out)
{
  PDC_base_em     emt = PDC_CheckAndSet;
  PDC_base_ed     edt;

  if (!em) {
    em = &emt;
  }
  if (!ed) {
    ed = &edt;
  }
  PDCI_IODISC_INIT_CHECKS("PDC_e_date_read");
  return PDC_e_date_read_internal(pdc, em, stopChar, ed, res_out);
}

/* ================================================================================ */
/* EXTERNAL STRING READ FUNCTIONS */

/*
 * Related helper functions
 */

PDC_error_t
PDC_string_init(PDC_t *pdc, PDC_string *s)
{
  if (!s) {
    return PDC_ERR;
  }
  bzero(s, sizeof(PDC_string));
  return PDC_OK;
}

PDC_error_t
PDC_string_cleanup(PDC_t *pdc, PDC_string *s)
{
  if (!s) {
    return PDC_ERR;
  }
  /* if (s->sharing) { PDC_WARN1(pdc->disc, "XXX_REMOVE cleanup: string %p is no longer sharing", (void*)s); } */
  s->sharing = 0;
  RMM_free_rbuf(s->rbuf);
  return PDC_OK;
}

PDC_error_t
PDC_string_mk_share(PDC_t *pdc, PDC_string *targ, const char *src, size_t len)
{
  PDCI_DISC_INIT_CHECKS("PDC_string_mk_share");
  PDCI_NULLPARAM_CHECK("PDC_string_mk_share", src);
  PDCI_NULLPARAM_CHECK("PDC_string_mk_share", targ);
  PDCI_STR_SHARE(targ, src, len);
  return PDC_OK;
}

PDC_error_t
PDC_string_mk_copy(PDC_t *pdc, PDC_string *targ, const char *src, size_t len)
{
  PDCI_DISC_INIT_CHECKS("PDC_string_mk_copy");
  PDCI_NULLPARAM_CHECK("PDC_string_mk_copy", src);
  PDCI_NULLPARAM_CHECK("PDC_string_mk_copy", targ);
  PDCI_STR_CPY(targ, src, len);
  return PDC_OK;

 fatal_alloc_err:
  PDC_FATAL(pdc->disc, "PDC_string_mk_copy: out of space");
  return PDC_ERR;
}

PDC_error_t
PDC_string_preserve(PDC_t *pdc, PDC_string *s)
{
  PDCI_DISC_INIT_CHECKS("PDC_string_preserve");
  PDCI_NULLPARAM_CHECK("PDC_string_preserve", s);
  PDCI_STR_PRESERVE(s);
  return PDC_OK;

 fatal_alloc_err:
  PDC_FATAL(pdc->disc, "PDC_string_preserve: out of space");
  return PDC_ERR;
}

PDC_error_t
PDC_string_copy(PDC_t *pdc, PDC_string *targ, const PDC_string *src)
{
  PDCI_DISC_INIT_CHECKS("PDC_string_copy");
  PDCI_NULLPARAM_CHECK("PDC_string_copy", src);
  PDCI_NULLPARAM_CHECK("PDC_string_copy", targ);
  PDCI_STR_CPY(targ, src->str, src->len);
  return PDC_OK;

 fatal_alloc_err:
  PDC_FATAL(pdc->disc, "PDC_string_copy: out of space");
  return PDC_ERR;
}

PDC_error_t
PDC_string_ed_init(PDC_t *pdc, PDC_base_ed *ed)
{
  PDCI_DISC_INIT_CHECKS("PDC_string_ed_init");
  return PDC_OK;
}

PDC_error_t
PDC_string_ed_cleanup(PDC_t *pdc, PDC_base_ed *ed)
{
  PDCI_DISC_INIT_CHECKS("PDC_string_ed_cleanup");
  return PDC_OK;
}

/*
 * The string read functions
 */

PDC_error_t
PDC_a_string_FW_read(PDC_t *pdc, PDC_base_em *em, size_t width,
		   PDC_base_ed *ed, PDC_string *s_out)
{
  PDC_base_em     emt = PDC_CheckAndSet;
  PDC_base_ed     edt;

  if (!em) {
    em = &emt;
  }
  if (!ed) {
    ed = &edt;
  }
  PDCI_IODISC_INIT_CHECKS("PDC_a_string_FW_read");
  return PDC_a_string_FW_read_internal(pdc, em, width, ed, s_out);
}

PDC_error_t
PDC_a_string_read(PDC_t *pdc, PDC_base_em *em, PDC_byte stopChar,
		 PDC_base_ed *ed, PDC_string *s_out)
{
  PDC_base_em     emt = PDC_CheckAndSet;
  PDC_base_ed     edt;

  if (!em) {
    em = &emt;
  }
  if (!ed) {
    ed = &edt;
  }
  PDCI_IODISC_INIT_CHECKS("PDC_a_string_read");
  return PDC_a_string_read_internal(pdc, em, stopChar, ed, s_out);
}

PDC_error_t
PDC_a_string_SE_read(PDC_t *pdc, PDC_base_em *em, const char *stopRegexp,
		   PDC_base_ed *ed, PDC_string *s_out)
{
  PDC_base_em     emt = PDC_CheckAndSet;
  PDC_base_ed     edt;

  if (!em) {
    em = &emt;
  }
  if (!ed) {
    ed = &edt;
  }
  PDCI_IODISC_INIT_CHECKS("PDC_a_string_SE_read");
  PDCI_NULLPARAM_CHECK("PDC_a_string_SE_read", stopRegexp);
  return PDC_a_string_SE_read_internal(pdc, em, stopRegexp, ed, s_out);
}

PDC_error_t
PDC_a_string_CSE_read(PDC_t *pdc, PDC_base_em *em, PDC_regexp_t *stopRegexp,
		    PDC_base_ed *ed, PDC_string *s_out)
{
  PDC_base_em     emt = PDC_CheckAndSet;
  PDC_base_ed     edt;

  if (!em) {
    em = &emt;
  }
  if (!ed) {
    ed = &edt;
  }
  PDCI_IODISC_INIT_CHECKS("PDC_a_string_CSE_read");
  PDCI_NULLPARAM_CHECK("PDC_a_string_CSE_read", stopRegexp);
  return PDC_a_string_CSE_read_internal(pdc, em, stopRegexp, ed, s_out);
}

PDC_error_t
PDC_e_string_FW_read(PDC_t *pdc, PDC_base_em *em, size_t width,
		   PDC_base_ed *ed, PDC_string *s_out)
{
  PDC_base_em     emt = PDC_CheckAndSet;
  PDC_base_ed     edt;

  if (!em) {
    em = &emt;
  }
  if (!ed) {
    ed = &edt;
  }
  PDCI_IODISC_INIT_CHECKS("PDC_e_string_FW_read");
  return PDC_e_string_FW_read_internal(pdc, em, width, ed, s_out);
}

PDC_error_t
PDC_e_string_read(PDC_t *pdc, PDC_base_em *em, PDC_byte stopChar,
		 PDC_base_ed *ed, PDC_string *s_out)
{
  PDC_base_em     emt = PDC_CheckAndSet;
  PDC_base_ed     edt;

  if (!em) {
    em = &emt;
  }
  if (!ed) {
    ed = &edt;
  }
  PDCI_IODISC_INIT_CHECKS("PDC_e_string_read");
  return PDC_e_string_read_internal(pdc, em, stopChar, ed, s_out);
}

PDC_error_t
PDC_e_string_SE_read(PDC_t *pdc, PDC_base_em *em, const char *stopRegexp,
		   PDC_base_ed *ed, PDC_string *s_out)
{
  PDC_base_em     emt = PDC_CheckAndSet;
  PDC_base_ed     edt;

  if (!em) {
    em = &emt;
  }
  if (!ed) {
    ed = &edt;
  }
  PDCI_IODISC_INIT_CHECKS("PDC_e_string_SE_read");
  PDCI_NULLPARAM_CHECK("PDC_e_string_SE_read", stopRegexp);
  return PDC_e_string_SE_read_internal(pdc, em, stopRegexp, ed, s_out);
}

PDC_error_t
PDC_e_string_CSE_read(PDC_t *pdc, PDC_base_em *em, PDC_regexp_t *stopRegexp,
		    PDC_base_ed *ed, PDC_string *s_out)
{
  PDC_base_em     emt = PDC_CheckAndSet;
  PDC_base_ed     edt;

  if (!em) {
    em = &emt;
  }
  if (!ed) {
    ed = &edt;
  }
  PDCI_IODISC_INIT_CHECKS("PDC_e_string_CSE_read");
  PDCI_NULLPARAM_CHECK("PDC_e_string_CSE_read", stopRegexp);
  return PDC_e_string_CSE_read_internal(pdc, em, stopRegexp, ed, s_out);
}

/* ================================================================================ */
/* EXTERNAL REGULAR EXPRESSION SUPPORT */

/* type PDC_regexp_t: */
struct PDC_regexp_s {
  int         invert;
  size_t      min;     /* 0 means no min */
  size_t      max;     /* 0 means no max */
  int         or_eor;  /* regexp ended with '|EOR' ? */
  char        charset[1];
};

PDC_error_t
PDC_regexp_compile(PDC_t *pdc, const char *regexp, PDC_regexp_t **regexp_out)
{
  PDC_regexp_t *res;
  size_t        last;
  int           eor = 0, just_eor = 0;

  PDCI_DISC_INIT_CHECKS("PDC_regexp_compile");

  if (!regexp_out) {
    PDC_WARN(pdc->disc, "PDC_regexp_compile: regexp_out cannot be NULL");
    return PDC_ERR;
  }
  if (!regexp || !(*regexp)) {
    PDC_WARN(pdc->disc, "PDC_regexp_compile: null regular expression specified");
    return PDC_ERR;
  }
  last = strlen(regexp) - 1;
  if (strcmp(regexp, "EOR") == 0) {
    eor = just_eor = 1;
    goto done;
  }
  /* check for [x]|EOR  */
  if (last > 5 && regexp[last] == 'R' && regexp[last-1] == 'O' && regexp[last-2] == 'E' && regexp[last-3] == '|') {
    eor = 1;
    last -= 4;
  }
  if (last < 2 || regexp[0] != '[' || regexp[last] != ']') {
    PDC_WARN1(pdc->disc,
	      "PDC_regexp_compile: Invalid regular expression: %s\n"
	      "    currently only support the forms \"EOR\", \"[<chars>]\", and \"[<chars>]|EOR\"",
	      PDC_qfmt_Cstr(regexp, strlen(regexp)));
    return PDC_ERR;
  }

 done:
  if (!(res = vmnewof(pdc->vm, 0, PDC_regexp_t, 1, last-1))) {
    PDC_FATAL(pdc->disc, "Out of space [regexp]");
    return PDC_ERR;
  }
  res->invert = 0;
  res->min    = 1;
  res->max    = 1;
  res->or_eor = eor;
  if (just_eor) {
    res->charset[0] = 0;
  } else {
    strncpy(res->charset, regexp+1, last-1);
    res->charset[last-1] = 0;
  }
  (*regexp_out) = res;
  return PDC_OK;
}

PDC_error_t
PDC_regexp_free(PDC_t *pdc, PDC_regexp_t *regexp)
{
  PDCI_DISC_INIT_CHECKS("PDC_regexp_free");
  if (regexp) {
    vmfree(pdc->vm, regexp);
  }
  return PDC_OK;
}

/* ================================================================================ */
/* EXTERNAL MISC ROUTINES */

/* helper for PDC_endian: */
const char *
PDC_Endian2String(PDC_endian e)
{
  return (e == PDC_bigEndian) ? "Big Endian" : "Little Endian";
}

char*
PDC_fmt_char(char c) {
  return fmtquote(&c, NiL, NiL, 1, 0);
}

char*
PDC_qfmt_char(char c) {
  return fmtquote(&c, "\'", "\'", 1, 1);
}

char*
PDC_fmt_str(const PDC_string *s) {
  return fmtquote(s->str, NiL, NiL, s->len, 0);
}

char*
PDC_qfmt_str(const PDC_string *s) {
  return fmtquote(s->str, "\"", "\"", s->len, 1);
}

char*
PDC_fmt_Cstr(const char *s, size_t len) {
  return fmtquote(s, NiL, NiL, len, 0);
}

char*
PDC_qfmt_Cstr(const char *s, size_t len) {
  return fmtquote(s, "\"", "\"", len, 1);
}

/*
 * Note: swapmem ops documented with binary read functions
 * Here we use in-place swap, which is safe with gsf's swapmem
 */

PDC_error_t
PDC_swap_bytes(PDC_byte *bytes, size_t num_bytes)
{
  if (!bytes) {
    PDC_WARN(&PDC_default_disc, "PDC_swap_bytes: param bytes must not be NULL");
    return PDC_ERR;
  }

  switch (num_bytes) {
  case 2:
    swapmem(1, bytes, bytes, num_bytes);
    return PDC_OK;
  case 4:
    swapmem(3, bytes, bytes, num_bytes);
    return PDC_OK;
  case 8:
    swapmem(7, bytes, bytes, num_bytes);
    return PDC_OK;
  }
  PDC_WARN1(&PDC_default_disc, "PDC_swap_bytes: invalid num_bytes (%d), use 2, 4, or 8", num_bytes);
  return PDC_ERR;
}

/*
 * XXX dummy going away eventually
 */
PDC_error_t
PDC_dummy_read(PDC_t *pdc, PDC_base_em *em, PDC_int32 dummy_val, PDC_base_ed *ed, PDC_int32 *res_out)
{
  PDCI_DISC_INIT_CHECKS("PDC_dummy_read");
  PDCI_NULLPARAM_CHECK("PDC_dummy_read", res_out);
  (*res_out) = dummy_val;
  ed->errCode = PDC_NO_ERR;
  return PDC_OK;
}

PDC_error_t
PDC_dummy_read_internal(PDC_t *pdc, PDC_base_em *em, PDC_int32 dummy_val, PDC_base_ed *ed, PDC_int32 *res_out)
{
  (*res_out) = dummy_val;
  ed->errCode = PDC_NO_ERR;
  return PDC_OK;
}

/* ================================================================================ */
/* SCAN FUNCTIONS */

PDC_error_t
PDC_a_char_lit_scan(PDC_t *pdc, PDC_byte c, PDC_byte s, int eat_lit,
		   PDC_byte *c_out, size_t *offset_out)
{
  PDC_byte        *begin, *p1, *p2, *end;
  int             eor, eof;
  size_t          bytes;
  int             matchlen = -1;

  PDCI_IODISC_INIT_CHECKS("PDC_a_char_lit_scan");
  PDC_TRACE3(pdc->disc, "PDC_a_char_lit_scan args: c %s stop %s eat %d", PDC_qfmt_char(c), PDC_qfmt_char(s), eat_lit);
  if (offset_out) {
    (*offset_out) = 0;
  }
  if (pdc->disc->stop_regexp) {
    matchlen = pdc->disc->stop_regexp->max;
  }
  if (PDC_ERR == PDCI_IO_needbytes(pdc, &begin, &p1, &p2, &end, &eor, &eof, &bytes)) {
    return PDC_ERR;
  }
  while (1) {
    if (p1 == end) {
      if (eor|eof) {
	break;
      }
      if (PDC_ERR == PDCI_IO_morebytes(pdc, &begin, &p1, &p2, &end, &eor, &eof, &bytes)) {
	return PDC_ERR;
      }
      if (bytes == 0) {
	break;
      }
      /* longer regexp match may work now */
      if (matchlen == 0) { /* no limit on match size, back up all the way */
	p1 = begin;
      } else if (matchlen > 1) { 
	p1 -= (matchlen - 1);
      }
      continue;
    }
    /* p1 < end */
    if (c == (*p1) || s == (*p1)) {
      if (c_out) {
	(*c_out) = (*p1);
      }
      if (offset_out) {
	(*offset_out) = (p1-begin);
      }
      if (eat_lit) {
	p1++; /* advance beyond char found */
      }
      if ((p1-begin) && PDC_ERR == PDCI_IO_forward(pdc, p1-begin)) {
	PDC_FATAL(pdc->disc, "Internal error : unexpected failure of PDCI_IO_forward");
      }
      return PDC_OK;
    }
    if (pdc->disc->stop_maxlen && ((p1-begin) >= pdc->disc->stop_maxlen)) {
      PDC_WARN(pdc->disc, "PDC_a_char_lit_scan: scan terminated early due to disc->stop_maxlen");
      break;
    }
    if (pdc->disc->stop_regexp && PDCI_regexpMatch(pdc, pdc->disc->stop_regexp, p1, end, 0)) {
      PDC_WARN(pdc->disc, "PDC_a_char_lit_scan: scan terminated early due to disc->stop_regexp");
      break;
    }
    p1++;
  }
  return PDC_ERR;
}

PDC_error_t
PDC_a_str_lit_scan(PDC_t *pdc, const PDC_string *findStr, const PDC_string *stopStr, int eat_lit,
		  PDC_string **str_out, size_t *offset_out) 
{
  PDC_byte        *begin, *p1, *p2, *end;
  int             eor, eof;
  size_t          bytes;
  int             matchlen = -1;

  PDCI_IODISC_INIT_CHECKS("PDC_a_str_lit_scan");
  PDC_TRACE3(pdc->disc, "PDC_a_str_lit_scan args: findStr = %s stopStre = %s eat = %d",
	     PDC_qfmt_str(findStr), PDC_qfmt_str(stopStr), eat_lit);
  if (offset_out) {
    (*offset_out) = 0;
  }
  if (!findStr || findStr->len == 0) {
    PDC_WARN(pdc->disc, "PDC_a_str_lit_scan : null/empty findStr specified");
    return PDC_ERR;
  }
  if (pdc->disc->stop_regexp) {
    matchlen = pdc->disc->stop_regexp->max;
  }
  if (matchlen != 0 && stopStr && matchlen < stopStr->len) {
    matchlen = stopStr->len;
  }
  if (PDC_ERR == PDCI_IO_needbytes(pdc, &begin, &p1, &p2, &end, &eor, &eof, &bytes)) {
    return PDC_ERR;
  }
  while (1) {
    if (p1 + findStr->len > end) {
      if (eor|eof) {
	break;
      }
      if (PDC_ERR == PDCI_IO_morebytes(pdc, &begin, &p1, &p2, &end, &eor, &eof, &bytes)) {
	return PDC_ERR;
      }
      if (bytes == 0) {
	break;
      }
      /* longer regexp match or stopStr match may work now */
      if (matchlen == 0) { /* no limit on match size, back up all the way */
	p1 = begin;
      } else if (matchlen > findStr->len) {
	p1 -= (matchlen - findStr->len);
      }
      continue;
    }
    /* p1 + findStr->len <= end */
    if (strncmp((char*)p1, findStr->str, findStr->len) == 0) {
      if (str_out) {
	(*str_out) = (PDC_string*)findStr;
      }
      if (offset_out) {
	(*offset_out) = (p1-begin);
      }
      if (eat_lit) {
	p1 += findStr->len; /* advance beyond findStr */
      }
      if ((p1-begin) && PDC_ERR == PDCI_IO_forward(pdc, p1-begin)) {
	PDC_FATAL(pdc->disc, "Internal error : unexpected failure of PDCI_IO_forward");
      }
      return PDC_OK;
    }
    if (stopStr && (p1 + stopStr->len <= end) &&
	strncmp((char*)p1, stopStr->str, stopStr->len) == 0) {
      if (str_out) {
	(*str_out) = (PDC_string*)stopStr;
      }
      if (offset_out) {
	(*offset_out) = (p1-begin);
      }
      p1 += stopStr->len; /* advance beyond stopStr */
      if (PDC_ERR == PDCI_IO_forward(pdc, p1-begin)) {
	PDC_FATAL(pdc->disc, "Internal error : unexpected failure of PDCI_IO_forward");
      }
      return PDC_OK;
    }
    if (pdc->disc->stop_maxlen && ((p1-begin) >= pdc->disc->stop_maxlen)) {
      PDC_WARN(pdc->disc, "PDC_a_str_lit_scan: scan terminated early due to disc->stop_maxlen");
      break;
    }
    if (pdc->disc->stop_regexp && PDCI_regexpMatch(pdc, pdc->disc->stop_regexp, p1, end, 0)) {
      PDC_WARN(pdc->disc, "PDC_a_str_lit_scan: scan terminated early due to disc->stop_regexp");
      break;
    }
    p1++;
  }
  return PDC_ERR;
}

/* ================================================================================ */
/* EBCDIC LITERAL READ and SCAN FUNCTIONS */

PDC_error_t
PDC_e_char_lit_read(PDC_t *pdc, PDC_base_em *em,
		   PDC_base_ed *ed, PDC_byte c)
{
  PDC_base_em     emt = PDC_CheckAndSet;
  PDC_base_ed     edt;

  if (!em) {
    em = &emt;
  }
  if (!ed) {
    ed = &edt;
  }
  PDCI_IODISC_INIT_CHECKS("PDC_e_char_lit_read");
  c = PDC_mod_ae_tab[(int)c]; /* convert to EBCDIC char */
  return PDC_char_lit_read_internal(pdc, em, ed, c);
}

PDC_error_t
PDC_e_str_lit_read(PDC_t *pdc, PDC_base_em *em,
		  PDC_base_ed *ed, const PDC_string *s)
{
  PDC_error_t     res;
  PDC_string      *es = &pdc->stmp1;
  PDC_base_em     emt = PDC_CheckAndSet;
  PDC_base_ed     edt;

  if (!em) {
    em = &emt;
  }
  if (!ed) {
    ed = &edt;
  }
  PDCI_IODISC_INIT_CHECKS("PDC_e_str_lit_read");
  PDCI_A2E_STR_CPY(es, s->str, s->len);
  res = PDC_str_lit_read_internal(pdc, em, ed, es);
  return res;

 fatal_alloc_err:
  PDCI_READFN_RET_ERRCODE_FATAL("[in PDC_e_str_lit_read]", "Memory alloc error", PDC_ALLOC_ERR);
}

PDC_error_t
PDC_e_char_lit_scan(PDC_t *pdc, PDC_byte c, PDC_byte s, int eat_lit,
		   PDC_byte *c_out, size_t *offset_out)
{
  c = PDC_mod_ae_tab[(int)c]; /* convert to EBCDIC char */
  s = PDC_mod_ae_tab[(int)s]; /* convert to EBCDIC char */
  return PDC_a_char_lit_scan(pdc, c, s, eat_lit, c_out, offset_out);
}

PDC_error_t
PDC_e_str_lit_scan(PDC_t *pdc, const PDC_string *findStr, const PDC_string *stopStr, int eat_lit,
		  PDC_string **str_out, size_t *offset_out) 
{
  PDC_error_t     res;
  PDC_string      *efindStr = &pdc->stmp1;
  PDC_string      *estopStr = &pdc->stmp2;
  PDCI_A2E_STR_CPY(efindStr, findStr->str, findStr->len);
  PDCI_A2E_STR_CPY(estopStr, stopStr->str, stopStr->len);
  res = PDC_a_str_lit_scan(pdc, efindStr, estopStr, eat_lit, str_out, offset_out);
  if (str_out && (*str_out)) {
    /* make str_out point to the ASCII version of the string that was found */
    if ((*str_out) == efindStr) {
      (*str_out) = (PDC_string*)findStr;
    } else {
      (*str_out) = (PDC_string*)stopStr;
    }
  }
  return res;

 fatal_alloc_err:
  PDC_FATAL(pdc->disc, "PDC_e_str_lit_scan: out of space");
  return PDC_ERR;
}

/********************************************************************************
 * INTERNAL FUNCTIONS (see libpadsc-internal.h)
 ********************************************************************************/

/* ================================================================================ */ 
/* INTERNAL ERROR REPORTING FUNCTIONS */

PDC_error_t
PDCI_report_err(PDC_t *pdc, int level, PDC_loc_t *loc,
		PDC_errCode_t errCode, const char *whatfn, const char *format, ...)
{
  PDC_error_f pdc_errorf;
  char    *severity = "Error";
  char    *msg      = "** unknown error code **";
  char    *tmpstr1, *tmpstr2, *tmpstr3;
  size_t  tmplen1, tmplen2, tmplen3;
  int     nullspan = 0;

  PDC_TRACE(pdc->disc, "PDCI_report_err called");
  if (!whatfn) whatfn = "";
  pdc_errorf = pdc->disc->errorf;
  if (PDC_GET_LEV(level) == PDC_LEV_FATAL) {
    severity = "FATAL error";
    if (!pdc_errorf) { /* need an error function anyway for fatal case */
      pdc_errorf = PDC_errorf;
    }
  } else if (pdc->speclev > 0 || pdc->disc->e_rep == PDC_errorRep_None || !pdc_errorf) {
    return PDC_OK;
  }
  if (errCode == PDC_NO_ERR) {
    severity = "Note";
  }
  if (loc && loc->b.num == loc->e.num && loc->e.byte == loc->b.byte -1 ) {
    nullspan = 1;
  }
  sfstrset(pdc->tmp, 0);
  if (pdc->disc->e_rep == PDC_errorRep_Min) {
    if (loc) {
      pdc_errorf(NiL, level, "%s %s: %s %d char %d: errCode %d",
		 severity, whatfn, loc->b.unit, loc->b.num, loc->b.byte, errCode);
    } else {
      pdc_errorf(NiL, level, "%s %s: errCode %d", severity, whatfn, errCode);
    }
    return PDC_OK;
  }
  if (format && strlen(format)) {
    va_list ap;
    if (loc) {
      sfprintf(pdc->tmp, "%s %s: %s %d char %d : ", severity, whatfn, loc->b.unit, loc->b.num, loc->b.byte);
    } else {
      sfprintf(pdc->tmp, "%s %s: ", severity, whatfn);
    }
    va_start(ap, format);
    sfvprintf(pdc->tmp, format, ap);
    va_end(ap);
  } else {
    switch (errCode) {
    case PDC_NO_ERR:
      msg = "";
      break;
    case PDC_UNEXPECTED_ERR:
      msg = "XXX Unexpected error (should not happen)";
      break;
    case PDC_BAD_PARAM:
      msg = "Invalid argument value used in libpadsc library call";
      break;
    case PDC_SYS_ERR:
      msg = "System error";
      break;
    case PDC_CHKPOINT_ERR:
      msg = "Checkpoint error (misuse of libpadsc IO checkpoint facility)";
      break;
    case PDC_COMMIT_ERR:
      msg = "Commit error (misuse of libpadsc IO checkpoint facility)";
      break;
    case PDC_RESTORE_ERR:
      msg = "Restore error (misuse of libpadsc IO checkpoint facility)";
      break;
    case PDC_ALLOC_ERR:
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
    case PDC_STRUCT_EXTRA_BEFORE_SEP:
      msg = "Unexpected extra data before field separator in struct";
      break;
    case PDC_STRUCT_FIELD_ERR:
      msg = "Structure field error";
      break;
    case PDC_UNION_MATCH_ERR:
      msg = "Union match failure";
      break;
    case PDC_ENUM_MATCH_ERR:
      msg = "Enum match failure";
      break;
    case PDC_TYPEDEF_CONSTRAINT_ERR:
      msg = "Typedef constraint error";
      break;
    case PDC_AT_EOF:
      msg = "Unexpected end of file (field too short?)";
      break;
    case PDC_AT_EOR:
      msg = "Unexpected end of record (field too short?)";
      break;
    case PDC_EXTRA_BEFORE_EOR:
      msg = "Unexpected extra data before EOR";
      break;
    case PDC_EOF_BEFORE_EOR:
      msg = "EOF encountered prior to expected EOR";
      break;
    case PDC_RANGE:
      msg = "Number out of range error";
      break;
    case PDC_INVALID_A_NUM:
      msg = "Invalid ASCII character encoding of a number";
      break;
    case PDC_INVALID_E_NUM:
      msg = "Invalid EBCDIC character encoding of a number";
      break;
    case PDC_INVALID_EBC_NUM:
      msg = "Invalid EBCDIC numeric encoding";
      break;
    case PDC_INVALID_BCD_NUM:
      msg = "Invalid BCD numeric encoding";
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
    case PDC_INVALID_DATE:
      msg = "Invalid date";
      break;
    default:
      sfprintf(pdc->tmp, "*** unknown error code: %d ***", errCode);
      msg = "";
      break;
    }
    if (loc) {
      if (loc->b.num != loc->e.num) {
	sfprintf(pdc->tmp, "%s %s: from %s %d char %d to %s %d char %d: %s ",
		 severity, whatfn,
		 loc->b.unit, loc->b.num, loc->b.byte, 
		 loc->e.unit, loc->e.num, loc->e.byte,
		 msg);
      } else if (nullspan) {
	sfprintf(pdc->tmp, "%s %s: at %s %d just before char %d: %s",
		 severity, whatfn,
		 loc->b.unit, loc->b.num, loc->b.byte,
		 msg);
      } else if (loc->b.byte == loc->e.byte) {
	sfprintf(pdc->tmp, "%s %s: at %s %d at char %d : %s ",
		 severity, whatfn,
		 loc->b.unit, loc->b.num, loc->b.byte,
		 msg);
      } else {
	sfprintf(pdc->tmp, "%s %s: at %s %d from char %d to char %d: %s ",
		 severity, whatfn,
		 loc->b.unit, loc->b.num, loc->b.byte, loc->e.byte,
		 msg);
      }
    } else {
      sfprintf(pdc->tmp, "%s %s: %s ", severity, whatfn, msg);
    }
  }
  if (loc && (pdc->disc->e_rep == PDC_errorRep_Max)) {
    PDC_IO_elt_t *elt1, *elt2;
    if (loc->b.num != loc->e.num) {
      if (PDC_OK == PDCI_IO_getElt(pdc, loc->b.num, &elt1)) {
	sfprintf(pdc->tmp, "\n[%s %d]", loc->b.unit, loc->b.num);
	if (elt1->len == 0) {
	  sfprintf(pdc->tmp, "(**EMPTY**)>>>");
	} else {
	  tmplen1 = loc->b.byte - 1;
	  tmplen2 = elt1->len - tmplen1;
	  tmpstr1 = PDC_fmt_Cstr((char*)elt1->begin,           tmplen1);
	  tmpstr2 = PDC_fmt_Cstr((char*)elt1->begin + tmplen1, tmplen2);
	  sfprintf(pdc->tmp, "%s>>>%s", tmpstr1, tmpstr2);
	}
      }
      if (PDC_OK == PDCI_IO_getElt(pdc, loc->e.num, &elt2)) {
	if (!elt1) {
	  sfprintf(pdc->tmp, "\n[%s %d]: ... >>>(char pos %d) ...",
		   loc->b.unit, loc->b.num, loc->b.byte);
	}
	sfprintf(pdc->tmp, "\n[%s %d]", loc->e.unit, loc->e.num);
	if (elt2->len == 0) {
	  sfprintf(pdc->tmp, "(**EMPTY**)<<<");
	} else {
	  tmplen1 = loc->e.byte;
	  tmplen2 = elt2->len - tmplen1;
	  tmpstr1 = PDC_fmt_Cstr((char*)elt2->begin,           tmplen1);
	  tmpstr2 = PDC_fmt_Cstr((char*)elt2->begin + tmplen1, tmplen2);
	  sfprintf(pdc->tmp, "%s<<<%s", tmpstr1, tmpstr2);
	}
      }
    } else { /* same elt */
      if (PDC_OK == PDCI_IO_getElt(pdc, loc->e.num, &elt1)) {
	sfprintf(pdc->tmp, "\n[%s %d]", loc->e.unit, loc->e.num);
	if (elt1->len == 0) {
	  sfprintf(pdc->tmp, ">>>(**EMPTY**)<<<");
	} else if (nullspan) {
	  tmplen1 = loc->b.byte - 1;
	  tmplen2 = elt1->len - tmplen1;
	  tmpstr1 = PDC_fmt_Cstr((char*)elt1->begin,           tmplen1);
	  tmpstr2 = PDC_fmt_Cstr((char*)elt1->begin + tmplen1, tmplen2);
	  sfprintf(pdc->tmp, "%s>>><<<%s", tmpstr1, tmpstr2);
	} else {
	  tmplen1 = loc->b.byte - 1;
	  tmplen3 = elt1->len - loc->e.byte;
	  tmplen2 = elt1->len - tmplen1 - tmplen3;
	  tmpstr1 = PDC_fmt_Cstr((char*)elt1->begin,                     tmplen1);
	  tmpstr2 = PDC_fmt_Cstr((char*)elt1->begin + tmplen1,           tmplen2);
	  tmpstr3 = PDC_fmt_Cstr((char*)elt1->begin + tmplen1 + tmplen2, tmplen3);
	  sfprintf(pdc->tmp, "%s>>>%s<<<%s", tmpstr1, tmpstr2, tmpstr3);
	}
      }
    }
  }
  pdc_errorf(NiL, level, "%s", sfstruse(pdc->tmp));
  return PDC_OK;
}

/* ================================================================================ */
/* INTERNAL VERSIONS OF EXTERNAL IO FUNCTIONS */

PDC_error_t
PDCI_IO_install_io(PDC_t *pdc, Sfio_t *io)
{
  PDCI_stkElt_t    *tp        = &(pdc->stack[0]);
  PDC_IO_elt_t     *next_elt;
  Void_t           *buf;

  /* XXX_TODO handle case where pdc->io is already set, io_discipline already open, etc */
  pdc->io = io;
#if 1
  /* tell sfio to use pdc->sfbuf but only let it know about sizeof(sfbuf)-1 space */
  buf = sfsetbuf(pdc->io, (Void_t*)1, 0);
  if (!buf) {
    sfsetbuf(pdc->io, (Void_t*)pdc->sfbuf, 1024 * 1024);
  } else if (buf == (Void_t*)pdc->sfbuf) {
    /* PDC_WARN(pdc->disc, "XXX_REMOVE pdc->sfbuf has already been installed so not installing it again"); */
  } else {
    /* PDC_WARN(pdc->disc, "XXX_REMOVE An unknown buffer has already been installed so not installing pdc->sfbuf\n"
                 "  (could be due to use of sfungetc)"); */
  }
#endif

  /* AT PRESENT we only support switching io at a very simply boundary:
   *    1. no checkpoint established
   *    2. not performing a speculative read (redundant based on 1)
   *    3. no nested internal calls in progress
   *    ...
   */
  if (PDC_SOME_ELTS(pdc->head)) {
    PDC_FATAL(pdc->disc, "Internal error: new io is being installed when pdc->head list is non-empty\n"
	      "Should not happen if IO discipline close is working properly");
  }
  if (pdc->top != 0) {
    PDC_FATAL(pdc->disc, "Switching io during IO checkpoint not supported yet");
  }
  if (pdc->speclev != 0) {
    PDC_FATAL(pdc->disc, "Switching io during speculative read not supported yet");
  }
  if (pdc->inestlev != 0) {
    PDC_FATAL(pdc->disc, "Switching io during internal call nesting not supported yet");
  }

  /* open IO discipline */
  if (PDC_ERR == pdc->disc->io_disc->sfopen_fn(pdc, pdc->disc->io_disc, pdc->io, pdc->head)) {
    return PDC_ERR;
  }
  /* perform first read */
  if (PDC_ERR == pdc->disc->io_disc->read_fn(pdc, pdc->disc->io_disc, 0, 0, &next_elt)) {
    return PDC_ERR;
  }
  tp->elt = PDC_FIRST_ELT(pdc->head);
  if (tp->elt == pdc->head || tp->elt != next_elt) {
    PDC_FATAL(pdc->disc, "Internal error : IO read function failure in PDCI_IO_install_io");
  }
  tp->remain = tp->elt->len;
  return PDC_OK;
}

PDC_error_t
PDC_IO_set_internal(PDC_t *pdc, Sfio_t *io)
{
  PDC_TRACE(pdc->disc, "PDC_IO_set_internal called");
  if (!pdc->disc->io_disc) {
    PDC_WARN(pdc->disc, "IO_set called with no IO discipline installed");
    return PDC_ERR;
  }
  if (pdc->io) {
    if (pdc->io == io) {
      PDC_DBG(pdc->disc, "PDC_IO_set_internal: same io installed more than once, ignoring this call");
      return PDC_OK;
    }
    if (pdc->path) {
      PDC_WARN(pdc->disc, "IO_set called with previous installed io due to fopen; closing");
    }
    PDC_IO_close_internal(pdc);
    /* path and io are no longer set */
  }
  return PDCI_IO_install_io(pdc, io);
}

PDC_error_t
PDC_IO_fopen_internal(PDC_t *pdc, char *path)
{
  Sfio_t           *io; 

  PDC_TRACE(pdc->disc, "PDC_IO_fopen_internal called");
  if (!pdc->disc->io_disc) {
    PDC_WARN(pdc->disc, "IO_fopen called with previous installed io due to fopen; closing");
    return PDC_ERR;
  }
  if (pdc->io) {
    if (pdc->path) {
      PDC_WARN(pdc->disc, "IO_fopen called while previous file still open; closing");
    }
    PDC_IO_close_internal(pdc);
    /* path and io are no longer set */
  }
  if (strcmp(path, "/dev/stdin") == 0) {
    return PDC_IO_set_internal(pdc, sfstdin);
  }
  if (!(pdc->path = vmnewof(pdc->vm, 0, char, strlen(path) + 1, 0))) {
    PDC_FATAL(pdc->disc, "out of space [string to record file path]");
    return PDC_ERR;
  }
  strcpy(pdc->path, path);
  if (!(io = sfopen(NiL, path, "r"))) {
    PDC_SYSERR1(pdc->disc, "Failed to open file \"%s\"", path);
    vmfree(pdc->vm, pdc->path);
    pdc->path = 0;
    return PDC_ERR;
  }
  return PDCI_IO_install_io(pdc, io);
}

PDC_error_t
PDC_IO_close_internal(PDC_t *pdc)
{
  PDCI_stkElt_t    *bot       = &(pdc->stack[0]);
  PDC_IO_elt_t     *io_elt    = bot->elt;
  size_t           io_remain  = bot->remain;

  PDC_TRACE(pdc->disc, "PDC_IO_close_internal called");
  if (!pdc->io) {
    return PDC_ERR;
  }
  /* close IO discpline */
  if (pdc->disc->io_disc) {
    pdc->disc->io_disc->sfclose_fn(pdc, pdc->disc->io_disc, io_elt, io_remain);
  }
  if (pdc->path) {
    sfclose(pdc->io);
  }
  if (pdc->vm && pdc->path) {
    vmfree(pdc->vm, pdc->path);
  }
  pdc->io = 0;
  pdc->path = 0;
  return PDC_OK;
}

PDC_error_t
PDC_IO_next_rec_internal(PDC_t *pdc, size_t *skipped_bytes_out) {
  PDCI_stkElt_t    *tp        = &(pdc->stack[pdc->top]);
  PDCI_stkElt_t    *bot       = &(pdc->stack[0]);
  PDC_IO_elt_t     *io_elt    = bot->elt;
  size_t           io_remain  = bot->remain;
  PDC_IO_elt_t     *next_elt;
  int              prev_eor;

  PDC_TRACE(pdc->disc, "PDC_IO_next_rec_internal called");
  (*skipped_bytes_out) = 0;
  if (pdc->disc->io_disc->uses_eor == 0) {
    PDC_WARN(pdc->disc, "PDC_IO_next_rec called when pdc->disc->io_disc does not support records");
    return PDC_ERR;
  }
  if (pdc->top == 0) { /* no need to preserve any bytes on read_fn call */ 
    io_elt    = 0;
    io_remain = 0;
  }
  while (1) {
    prev_eor = tp->elt->eor;
    (*skipped_bytes_out) += tp->remain;
    tp->remain = 0;
    if (tp->elt->eof) {
      return PDC_ERR;
    }
    /* advance IO cursor */
    if (tp->elt->next != pdc->head) {
      tp->elt = tp->elt->next;
    } else {
      /* use IO disc read_fn */
      if (PDC_ERR == pdc->disc->io_disc->read_fn(pdc, pdc->disc->io_disc, io_elt, io_remain, &next_elt)) {
	/* perhaps put an eof rec in */
	tp->elt = PDC_LAST_ELT(pdc->head);
	tp->remain = 0;
	return PDC_ERR;
      }
      if (next_elt == pdc->head) { /* should not happen */
	PDC_FATAL(pdc->disc, "Internal error, PDC_IO_next_rec_internal observed incorrect read_fn behavior");
	return PDC_ERR;
      }
      tp->elt = next_elt;
    }
    tp->remain = tp->elt->len;
    if (prev_eor) { /* we just advanced past an EOR */
      break;
    }
    /* just advanced past a partial read -- continue while loop */
  }
  return PDC_OK;
}

int
PDC_IO_at_EOR_internal(PDC_t *pdc) {
  PDCI_stkElt_t    *tp        = &(pdc->stack[pdc->top]);
  PDC_IO_elt_t     *tpelt     = tp->elt;
  PDC_TRACE(pdc->disc, "PDC_IO_at_EOR_internal called");
  return (tp->remain == 0 && tpelt && tpelt->eor) ? 1 : 0;
}

int
PDC_IO_at_EOF_internal(PDC_t *pdc) {
  PDCI_stkElt_t    *tp        = &(pdc->stack[pdc->top]);
  PDC_IO_elt_t     *tpelt     = tp->elt;
  PDC_TRACE(pdc->disc, "PDC_IO_at_EOF_internal called");
  return (tp->remain == 0 && tpelt && tpelt->eof) ? 1 : 0;
}

PDC_error_t
PDC_IO_getPos_internal(PDC_t *pdc, PDC_pos_t *pos, int offset)
{
  PDCI_stkElt_t    *tp        = &(pdc->stack[pdc->top]);
  PDC_IO_elt_t     *elt       = tp->elt;
  size_t           remain     = tp->remain;
  size_t           avail;
  PDC_TRACE(pdc->disc, "PDC_IO_getPos_internal called");
  /* invariant: remain should be in range [1, elt->len]; should only be 0 if elt->len is 0 */
  if (offset > 0) {
    while (1) {
      if (remain > offset) {
	remain -= offset;
	goto done;
      }
      offset -= remain;
      while (1) {
	if (elt->eof) {
	  remain = 0;
	  goto done;
	}
	elt = elt->next;
	if (elt == pdc->head) {
	  pos->num = 0;
	  pos->byte = 0;
	  pos->unit = "*pos not found*";
	  return PDC_ERR;
	}
	if (elt->len) {
	  break;
	}
      }
      remain = elt->len;
      /* now at first byte of next elt */
    }
  } else if (offset < 0) {
    offset = - offset;
    while (1) {
      avail = elt->len - remain;
      if (avail >= offset) {
	remain += offset;
	goto done;
      }
      offset -= avail; /* note offset still > 0 */
      while (1) {
	elt = elt->prev;
	if (elt == pdc->head) {
	  pos->num = 0;
	  pos->byte = 0;
	  pos->unit = "*pos not found*";
	  return PDC_ERR;
	}
	if (elt->len) {
	  break;
	}
      }
      remain = 1;
      offset--;
      /* now at last byte of prev elt */
    }
  }

 done:
  pos->num  = elt->num;
  if (elt->len) {
    pos->byte = elt->len - remain + 1;
  } else {
    pos->byte = 0;
  }
  pos->unit = elt->unit;
  return PDC_OK;
}

/* ================================================================================ */
/* PURELY INTERNAL IO FUNCTIONS */

PDC_error_t
PDCI_IO_needbytes(PDC_t *pdc,
		  PDC_byte **b_out, PDC_byte **p1_out, PDC_byte **p2_out, PDC_byte **e_out,
		  int *eor_out, int *eof_out, size_t *bytes_out)
{
  PDCI_stkElt_t   *tp       = &(pdc->stack[pdc->top]);
  PDC_IO_elt_t    *elt      = tp->elt;

  PDC_TRACE(pdc->disc, "PDCI_IO_needbytes called");
  (*bytes_out) = tp->remain;
  (*b_out) = (*p1_out) = (*p2_out) = (elt->end - tp->remain);

  while (!(elt->eor|elt->eof) && elt->next != pdc->head) {
    elt = elt->next;
    (*bytes_out) += elt->len;
  }
  (*eor_out)   =  elt->eor;
  (*eof_out)   =  elt->eof;
  (*e_out)     =  elt->end;
  return PDC_OK;
}

PDC_error_t
PDCI_IO_morebytes(PDC_t *pdc, PDC_byte **b_out, PDC_byte **p1_out, PDC_byte **p2_out, PDC_byte **e_out,
		  int *eor_out, int *eof_out, size_t *bytes_out)
{
  PDC_IO_elt_t     *lastelt   = PDC_LAST_ELT(pdc->head);
  PDCI_stkElt_t    *bot       = &(pdc->stack[0]);
  PDC_IO_elt_t     *io_elt    = bot->elt;
  PDC_IO_elt_t     *next_elt;
  size_t           io_remain  = bot->remain;
  size_t           offset;
  PDC_byte         *prev_lastelt_end;

  if (lastelt->eor|lastelt->eof) {
    PDC_FATAL(pdc->disc, "Internal error, PDCI_IO_morebytes called when lastelt eor or eof is set");
    return PDC_ERR;
  }
  if ((prev_lastelt_end = lastelt->end) != (*e_out)) {
    PDC_FATAL(pdc->disc, "Internal error, PDCI_IO_morebytes called when lastelt->end != (*e_out)");
    return PDC_ERR;
  }
  if (PDC_ERR == pdc->disc->io_disc->read_fn(pdc, pdc->disc->io_disc, io_elt, io_remain, &next_elt)) {
    (*bytes_out) = 0;
    (*eof_out)   = 1;
    return PDC_ERR;
  }
  if (lastelt->next != next_elt || next_elt == pdc->head) { /* should not happen */
    PDC_FATAL(pdc->disc, "Internal error, PDCI_IO_morebytes observed incorrect read_fn behavior");
    (*bytes_out) = 0;
    (*eof_out)   = 1;
    return PDC_ERR;
  }
  if (lastelt->end > prev_lastelt_end) { /* things shifted to higher mem loc */
    offset = lastelt->end - prev_lastelt_end;
    (*b_out)  += offset;
    (*p1_out) += offset;
    (*p2_out) += offset;
    (*e_out)  += offset;
  } else if (prev_lastelt_end > lastelt->end) { /* things shifted to lower mem loc */
    offset = prev_lastelt_end - lastelt->end;
    (*b_out)  -= offset;
    (*p1_out) -= offset;
    (*p2_out) -= offset;
    (*e_out)  -= offset;
  }
  lastelt = lastelt->next;
  (*bytes_out) = lastelt->len;
  (*e_out)    += lastelt->len;
  while (!(lastelt->eor|lastelt->eof) && lastelt->next != pdc->head) {
    /* read_fn added more than one element */
    lastelt = lastelt->next;
    (*bytes_out) += lastelt->len;
    (*e_out)     += lastelt->len;
  }
  (*eor_out)   = lastelt->eor;
  (*eof_out)   = lastelt->eof;
  return PDC_OK;
}

PDC_error_t
PDCI_IO_forward(PDC_t *pdc, size_t num_bytes)
{
  PDCI_stkElt_t    *tp        = &(pdc->stack[pdc->top]);
  size_t todo                 = num_bytes;
  PDCI_stkElt_t    *bot       = &(pdc->stack[0]);
  PDC_IO_elt_t     *io_elt    = bot->elt;
  size_t           io_remain  = bot->remain;
  PDC_IO_elt_t     *next_elt;

  PDC_TRACE(pdc->disc, "PDCI_IO_forward called");
  /* should be able to move forward without reading new bytes or advancing past EOR/EOF */
  while (todo > 0) {
    if (tp->remain == 0) {
      if (tp->elt->eor|tp->elt->eof) {
	PDC_FATAL(pdc->disc, "Internal error, PDCI_IO_forward hit EOR OR EOF");
      }
      if (tp->elt->next == pdc->head) {
	PDC_FATAL(pdc->disc, "Internal error, PDCI_IO_forward would need to read bytes from io stream");
	return PDC_ERR;
      }
      tp->elt = tp->elt->next;
      tp->remain = tp->elt->len;
      continue;
    }
    if (todo <= tp->remain) {
      tp->remain -= todo;
      todo = 0;
      break;
    }
    /* current IO rec gets us partway */
    todo -= tp->remain;
    tp->remain = 0;
  }
  /* success */
  if (tp->remain || (tp->elt->eor|tp->elt->eof)) {
    return PDC_OK;
  }
  /* at end of a non-EOR, non-EOF elt: advance now */
  if (tp->elt->next != pdc->head) {
    tp->elt = tp->elt->next;
    tp->remain = tp->elt->len;
    return PDC_OK;
  }
  /* need to read some data -- use IO disc read_fn */
  if (pdc->top == 0) { /* no need to preserve any bytes on read_fn call */ 
    io_elt    = 0;
    io_remain = 0;
  }
  if (PDC_ERR == pdc->disc->io_disc->read_fn(pdc, pdc->disc->io_disc, io_elt, io_remain, &next_elt)) {
    /* perhaps put an eof rec in */
    tp->elt = PDC_LAST_ELT(pdc->head);
    tp->remain = 0;
    return PDC_ERR;
  }
  if (next_elt == pdc->head) { /* should not happen */
    PDC_FATAL(pdc->disc, "Internal error, PDCI_IO_forward observed incorrect read_fn behavior");
    return PDC_ERR;
  }
  tp->elt = next_elt;
  tp->remain = tp->elt->len;
  return PDC_OK;
}

PDC_error_t
PDCI_IO_getElt(PDC_t *pdc, size_t num, PDC_IO_elt_t **elt_out) {
  PDC_IO_elt_t *elt;

  PDC_TRACE(pdc->disc, "PDCI_IO_getElt called");
  PDCI_NULLPARAM_CHECK("PDCI_IO_getElt", elt_out);
  for (elt = PDC_FIRST_ELT(pdc->head); elt != pdc->head; elt = elt->next) {
    if (elt->num == num) {
      (*elt_out) = elt;
      return PDC_OK;
    }
  }
  return PDC_ERR;
}

/* ================================================================================ */
/* INTERNAL VERSIONS OF EXTERNAL LITERAL READ FUNCTIONS */

PDC_error_t
PDC_char_lit_read_internal(PDC_t *pdc, PDC_base_em *em,
			   PDC_base_ed *ed, PDC_byte c)
{
  PDC_byte        *begin, *p1, *p2, *end;
  int             eor, eof;
  size_t          bytes;

  PDC_TRACE1(pdc->disc, "PDC_char_lit_read_internal called, arg: %s", PDC_qfmt_char(c));
  if (PDC_ERR == PDCI_IO_needbytes(pdc, &begin, &p1, &p2, &end, &eor, &eof, &bytes)) {
    goto fatal_nb_io_err;
  }
  if (bytes == 0) {
    goto at_eor_or_eof_err;
  }
  if ((*em == PDC_Ignore) || (c == (*begin))) {
    if (PDC_ERR == PDCI_IO_forward(pdc, 1)) {
      goto fatal_forward_err;
    }
    ed->errCode = PDC_NO_ERR;
    return PDC_OK;  /* IO cursor is one beyond c */
  }
  goto not_found;

 at_eor_or_eof_err:
  PDCI_READFN_SET_NULLSPAN_LOC(0);
  PDCI_READFN_RET_ERRCODE_WARN("[in PDC_char_lit_read]", 0, eor ? PDC_AT_EOR : PDC_AT_EOF);

 not_found:
  PDCI_READFN_SET_LOC_BE(0, 1);
  if (*em == PDC_CheckAndSet) {
    PDCI_READFN_RET_ERRCODE_WARN("[in PDC_char_lit_read]", 0, PDC_CHAR_LIT_NOT_FOUND);
  }
  PDCI_READFN_RET_ERRCODE_NOWARN(PDC_CHAR_LIT_NOT_FOUND);

 fatal_nb_io_err:
  PDCI_READFN_RET_ERRCODE_FATAL("[in PDC_char_lit_read_internal]", "IO error (nb)", PDC_IO_ERR);

 fatal_forward_err:
  PDCI_READFN_RET_ERRCODE_FATAL("[in PDC_char_lit_read_internal]", "IO_forward error", PDC_FORWARD_ERR);
}

PDC_error_t
PDC_str_lit_read_internal(PDC_t *pdc, PDC_base_em *em,
			  PDC_base_ed *ed, const PDC_string *s)
{
  PDC_byte        *begin, *p1, *p2, *end;
  int             eor, eof;
  size_t          bytes;

  PDC_TRACE1(pdc->disc, "PDC_str_lit_read_internal called, arg: %s", PDC_qfmt_str(s));
  if (s->len <= 0) {
    PDC_WARN(pdc->disc, "UNEXPECTED PARAM VALUE: PDC_str_lit_read_internal called with s->len <= 0");
    goto bad_param_err;
  }
  if (PDC_ERR == PDCI_IO_needbytes(pdc, &begin, &p1, &p2, &end, &eor, &eof, &bytes)) {
    goto fatal_nb_io_err;
  }
  while (end-begin < s->len) {
    if ((eor|eof) || bytes == 0) {
      goto width_not_avail;
    }
    if (PDC_ERR == PDCI_IO_morebytes(pdc, &begin, &p1, &p2, &end, &eor, &eof, &bytes)) {
      goto fatal_mb_io_err;
    }
  }
  /* end-begin >= s->len */
  if ((*em == PDC_Ignore) || (strncmp((char*)begin, s->str, s->len) == 0)) {
    if (PDC_ERR == PDCI_IO_forward(pdc, s->len)) {
      goto fatal_forward_err;
    }
    ed->errCode = PDC_NO_ERR;
    return PDC_OK;    /* found it */
  }
  goto not_found;

 bad_param_err:
  PDCI_READFN_SET_NULLSPAN_LOC(0);
  PDCI_READFN_RET_ERRCODE_WARN("[in PDC_str_lit_read]", 0, PDC_BAD_PARAM);

 width_not_avail:
  PDCI_READFN_SET_LOC_BE(0, end-begin);
  if (*em == PDC_CheckAndSet) {
    PDCI_READFN_RET_ERRCODE_WARN("[in PDC_str_lit_read]", 0, PDC_STR_LIT_NOT_FOUND);
  }
  PDCI_READFN_RET_ERRCODE_NOWARN(PDC_STR_LIT_NOT_FOUND);

 not_found:
  PDCI_READFN_SET_LOC_BE(0, s->len);
  if (*em == PDC_CheckAndSet) {
    PDCI_READFN_RET_ERRCODE_WARN("[in PDC_str_lit_read]", 0, PDC_STR_LIT_NOT_FOUND);
  }
  PDCI_READFN_RET_ERRCODE_NOWARN(PDC_STR_LIT_NOT_FOUND);

 fatal_nb_io_err:
  PDCI_READFN_RET_ERRCODE_FATAL("[in PDC_str_lit_read_internal]", "IO error (nb)", PDC_IO_ERR);

 fatal_mb_io_err:
  PDCI_READFN_RET_ERRCODE_FATAL("[in PDC_str_lit_read_internal]", "IO error (mb)", PDC_IO_ERR);

 fatal_forward_err:
  PDCI_READFN_RET_ERRCODE_FATAL("[in PDC_str_lit_read_internal]", "IO_forward error", PDC_FORWARD_ERR);
}

PDC_error_t
PDC_countX_internal(PDC_t *pdc, PDC_base_em *em, PDC_uint8 x, int eor_required,
		    PDC_base_ed *ed, PDC_int32 *res_out)
{
  PDC_int32       count = 0;
  PDC_byte        *begin, *p1, *p2, *end;
  int             eor, eof;
  size_t          bytes;
  int             matchlen = -1;
  PDC_byte        *tmp;

  PDC_TRACE2(pdc->disc, "PDC_countX_internal called, args: x = %s eor_required = %d", PDC_qfmt_char(x), eor_required);
  (*res_out) = 0;
  if (pdc->disc->stop_regexp) {
    matchlen = pdc->disc->stop_regexp->max;
  }
  if (PDC_ERR == PDCI_IO_needbytes(pdc, &begin, &p1, &p2, &end, &eor, &eof, &bytes)) {
    goto fatal_nb_io_err;
  }
  while (1) {
    if (p1 == end) {
      if (eor|eof) {
	break;
      }
      if (PDC_ERR == PDCI_IO_morebytes(pdc, &begin, &p1, &p2, &end, &eor, &eof, &bytes)) {
	goto fatal_mb_io_err;
      }
      if (bytes == 0) {
	break;
      } 
      /* longer regexp match may work now */
      if (matchlen == 0) {
	p1 = begin;
	count = 0;
      } else if (matchlen > 1) {
	p1 -= (matchlen - 1);
	/* fix count to avoid double-counting */
	for (tmp = p1; tmp < (end-bytes); tmp++) {
	  if (x == (*tmp)) {
	    count--;
	  }
	}
      }
      continue;
    }
    /* p1 < end */
    if (x == (*p1)) {
      count++;
    }
    if (pdc->disc->stop_maxlen && ((p1-begin) >= pdc->disc->stop_maxlen)) {
      PDC_WARN(pdc->disc, "PDC_countX: scan terminated early due to disc->stop_maxlen");
      break;
    }
    if (pdc->disc->stop_regexp && PDCI_regexpMatch(pdc, pdc->disc->stop_regexp, p1, end, 0)) {
      PDC_WARN(pdc->disc, "PDC_countX: scan terminated early due to disc->stop_regexp");
      break;
    }
    p1++;
  }
  if (eor_required && !eor && eof) { /* EOF encountered first, error */
    PDCI_READFN_SET_LOC_BE(0, p1-begin);
    PDCI_READFN_RET_ERRCODE_WARN("[in PDC_countXtoY]", 0, PDC_EOF_BEFORE_EOR);
  }
  /* hit EOR/EOF/stop restriction */
  (*res_out) = count;
  ed->errCode = PDC_NO_ERR;
  return PDC_OK;

 fatal_nb_io_err:
  PDCI_READFN_RET_ERRCODE_FATAL("[in PDC_countX_internal]", "IO error (nb)", PDC_IO_ERR);

 fatal_mb_io_err:
  PDCI_READFN_RET_ERRCODE_FATAL("[in PDC_countX_internal]", "IO error (mb)", PDC_IO_ERR);
}

PDC_error_t
PDC_countXtoY_internal(PDC_t *pdc, PDC_base_em *em, PDC_uint8 x, PDC_uint8 y,
		       PDC_base_ed *ed, PDC_int32 *res_out)
{
  PDC_int32       count = 0;
  PDC_byte        *begin, *p1, *p2, *end;
  int             eor, eof;
  size_t          bytes;
  int             matchlen = -1;
  PDC_byte        *tmp;

  PDC_TRACE2(pdc->disc, "PDC_countXtoY_internal called, args: x = %s y = %s", PDC_qfmt_char(x), PDC_qfmt_char(y));
  (*res_out) = 0;
  if (pdc->disc->stop_regexp) {
    matchlen = pdc->disc->stop_regexp->max;
  }
  if (PDC_ERR == PDCI_IO_needbytes(pdc, &begin, &p1, &p2, &end, &eor, &eof, &bytes)) {
    goto fatal_nb_io_err;
  }
  while (1) {
    if (p1 == end) {
      if (eor|eof) {
	break;
      }
      if (PDC_ERR == PDCI_IO_morebytes(pdc, &begin, &p1, &p2, &end, &eor, &eof, &bytes)) {
	goto fatal_mb_io_err;
      }
      if (bytes == 0) {
	break;
      } 
      /* longer regexp match may work now */
      if (matchlen == 0) {
	p1 = begin;
	count = 0;
      } else if (matchlen > 1) {
	p1 -= (matchlen - 1);
	/* fix count to avoid double-counting */
	for (tmp = p1; tmp < (end-bytes); tmp++) {
	  if (x == (*tmp)) {
	    count--;
	  }
	}
      }
      continue;
    }
    /* p1 < end */
    if (y == (*p1)) { /* success */
      (*res_out) = count;
      ed->errCode = PDC_NO_ERR;
      return PDC_OK;
    }
    if (x == (*p1)) {
      count++;
    }
    if (pdc->disc->stop_maxlen && ((p1-begin) >= pdc->disc->stop_maxlen)) {
      PDC_WARN(pdc->disc, "PDC_countXtoY: scan terminated early due to disc->stop_maxlen");
      break;
    }
    if (pdc->disc->stop_regexp && PDCI_regexpMatch(pdc, pdc->disc->stop_regexp, p1, end, 0)) {
      PDC_WARN(pdc->disc, "PDC_countXtoY: scan terminated early due to disc->stop_regexp");
      break;
    }
    p1++;
  }
  goto not_found; /* y not found */

 not_found:
  PDCI_READFN_SET_LOC_BE(0, p1-begin);
  PDCI_READFN_RET_ERRCODE_WARN("[in PDC_countXtoY]", 0, PDC_CHAR_LIT_NOT_FOUND);

 fatal_nb_io_err:
  PDCI_READFN_RET_ERRCODE_FATAL("[in PDC_countXtoY_internal]", "IO error (nb)", PDC_IO_ERR);

 fatal_mb_io_err:
  PDCI_READFN_RET_ERRCODE_FATAL("[in PDC_countXtoY_internal]", "IO error (mb)", PDC_IO_ERR);
}

/* ================================================================================ */
/* INTERNAL VERSIONS OF EXTERNAL DATE/TIME READ FUNCTIONS */

PDC_error_t
PDC_a_date_read_internal(PDC_t *pdc, PDC_base_em *em, PDC_byte stopChar,
			 PDC_base_ed *ed, PDC_uint32 *res_out)
{
  PDC_string      *s = &pdc->stmp1;
  time_t          tm;
  PDC_byte        *tmp;
  size_t          width;

  PDC_TRACE(pdc->disc, "PDC_a_date_read_internal called");
  if (PDC_ERR == PDC_a_string_read_internal(pdc, em, stopChar, ed, s)) {
    return PDC_ERR;
  }
  PDCI_STR_PRESERVE(s); /* this ensures s.str is null terminated */
  width = s->len;
  tm = tmdate(s->str, (char**)&tmp, NiL);
  if (!tmp || (char*)tmp - s->str != width) {
    PDCI_READFN_SET_LOC_BE(-width, 0);
    PDCI_READFN_RET_ERRCODE_WARN("[in PDC_a_date_read]", 0, PDC_INVALID_DATE);
  }
  (*res_out) = tm;
  PDC_DBG3(pdc->disc, "PDC_a_date_read_internal converted string %s => %s (secs = %lu)", PDC_qfmt_str(s), fmttime("%K", (time_t)(*res_out)), (unsigned long)(*res_out));
  return PDC_OK;

 fatal_alloc_err:
  PDCI_READFN_RET_ERRCODE_FATAL("[in PDC_a_date_read_internal]", "Memory alloc error", PDC_ALLOC_ERR);
}

PDC_error_t
PDC_e_date_read_internal(PDC_t *pdc, PDC_base_em *em, PDC_byte stopChar,
			 PDC_base_ed *ed, PDC_uint32 *res_out)
{
  PDC_string      *s = &pdc->stmp1;
  time_t          tm;
  PDC_byte        *tmp;
  size_t          width;

  PDC_TRACE(pdc->disc, "PDC_e_date_read_internal called");
  if (PDC_ERR == PDC_e_string_read_internal(pdc, em, stopChar, ed, s)) {
    return PDC_ERR;
  }
  PDCI_STR_PRESERVE(s); /* this ensures s.str is null terminated */
  width = s->len;
  tm = tmdate(s->str, (char**)&tmp, NiL);
  if (!tmp || (char*)tmp - s->str != width) {
    PDCI_READFN_SET_LOC_BE(-width, 0);
    PDCI_READFN_RET_ERRCODE_WARN("[in PDC_e_date_read]", 0, PDC_INVALID_DATE);
  }
  (*res_out) = tm;
  PDC_DBG3(pdc->disc, "PDC_e_date_read_internal converted string %s => %s (secs = %lu)", PDC_qfmt_str(s), fmttime("%K", (time_t)(*res_out)), (unsigned long)(*res_out));
  return PDC_OK;

 fatal_alloc_err:
  PDCI_READFN_RET_ERRCODE_FATAL("[in PDC_e_date_read_internal]", "Memory alloc error", PDC_ALLOC_ERR);
}

/* ================================================================================ */
/* INTERNAL VERSIONS OF EXTERNAL STRING READ FUNCTIONS */

PDC_error_t
PDC_a_string_FW_read_internal(PDC_t *pdc, PDC_base_em *em, size_t width,
			    PDC_base_ed *ed, PDC_string *s_out)
{
  PDC_byte        *begin, *p1, *p2, *end;
  int             eor, eof;
  size_t          bytes;

  PDC_TRACE(pdc->disc, "PDC_a_string_FW_read_internal called");
  if (width <= 0) {
    PDC_WARN(pdc->disc, "UNEXPECTED PARAM VALUE: PDC_a_string_FW_read called with width <= 0");
    goto bad_param_err;
  }
  /* ensure there are width chars available */
  if (PDC_ERR == PDCI_IO_needbytes(pdc, &begin, &p1, &p2, &end, &eor, &eof, &bytes)) {
    goto fatal_nb_io_err;
  }
  while (end-begin < width) {
    if ((eor|eof) || bytes == 0) {
      goto width_not_avail;
    }
    if (PDC_ERR == PDCI_IO_morebytes(pdc, &begin, &p1, &p2, &end, &eor, &eof, &bytes)) {
      goto fatal_mb_io_err;
    }
  }
  /* end-begin >= width */
  end = begin + width;
  PDCI_STR_SET(s_out, (char*)begin, (char*)end);
  if (PDC_ERR == PDCI_IO_forward(pdc, width)) {
    goto fatal_forward_err;
  }
  ed->errCode = PDC_NO_ERR;
  return PDC_OK;

 bad_param_err:
  PDCI_READFN_SET_NULLSPAN_LOC(0);
  PDCI_READFN_RET_ERRCODE_WARN("[in PDC_a_string_FW_read]", 0, PDC_BAD_PARAM);

 width_not_avail:
  PDCI_READFN_SET_LOC_BE(0, end-begin);
  PDCI_READFN_RET_ERRCODE_WARN("[in PDC_a_string_FW_read]", 0, PDC_WIDTH_NOT_AVAILABLE);

 fatal_alloc_err:
  PDCI_READFN_RET_ERRCODE_FATAL("[in PDC_a_string_FW_read_internal]", "Memory alloc error", PDC_ALLOC_ERR);

 fatal_nb_io_err:
  PDCI_READFN_RET_ERRCODE_FATAL("[in PDC_a_string_FW_read_internal]", "IO error (nb)", PDC_IO_ERR);

 fatal_mb_io_err:
  PDCI_READFN_RET_ERRCODE_FATAL("[in PDC_a_string_FW_read_internal]", "IO error (mb)", PDC_IO_ERR);

 fatal_forward_err:
  PDCI_READFN_RET_ERRCODE_FATAL("[in PDC_a_string_FW_read_internal]", "IO_forward error", PDC_FORWARD_ERR);
}

PDC_error_t
PDC_a_string_read_internal(PDC_t *pdc, PDC_base_em *em, PDC_byte stopChar,
			  PDC_base_ed *ed, PDC_string *s_out)
{
  PDC_byte        *begin, *p1, *p2, *end;
  int             eor, eof;
  size_t          bytes;
  int             matchlen = -1;

  PDC_TRACE(pdc->disc, "PDC_a_string_read_internal called");
  if (pdc->disc->stop_regexp) {
    matchlen = pdc->disc->stop_regexp->max;
  }
  if (PDC_ERR == PDCI_IO_needbytes(pdc, &begin, &p1, &p2, &end, &eor, &eof, &bytes)) {
    goto fatal_nb_io_err;
  }
  while (1) {
    if (p1 == end) {
      if (stopChar && (eor|eof)) {
	break;
      }
      if (stopChar || !(eor|eof)) {
	if (PDC_ERR == PDCI_IO_morebytes(pdc, &begin, &p1, &p2, &end, &eor, &eof, &bytes)) {
	  goto fatal_mb_io_err;
	}
	if (bytes == 0) {
	  break;
	}
	/* longer regexp match may work now */
	if (matchlen == 0) { /* no limit on match size, back up all the way */
	  p1 = begin;
	} else if (matchlen > 1) {
	  p1 -= (matchlen - 1);
	}
	continue;
      }
    }
    /* (p1 < end) OR (p1 == end, stopChar is 0, eor|eof set) */
    if (p1 == end || stopChar == (*p1)) {
      /* success */
      PDCI_STR_SET(s_out, (char*)begin, (char*)p1);
      if (PDC_ERR == PDCI_IO_forward(pdc, p1-begin)) {
	goto fatal_forward_err;
      }
      ed->errCode = PDC_NO_ERR;
      return PDC_OK;
    }
    if (pdc->disc->stop_maxlen && ((p1-begin) >= pdc->disc->stop_maxlen)) {
      PDC_WARN(pdc->disc, "PDC_a_string_read: scan terminated early due to disc->stop_maxlen");
      break;
    }
    if (pdc->disc->stop_regexp && PDCI_regexpMatch(pdc, pdc->disc->stop_regexp, p1, end, 0)) {
      PDC_WARN(pdc->disc, "PDC_a_string_read: scan terminated early due to disc->stop_regexp");
      break;
    }
    p1++;
  }
  goto not_found;

 not_found:
  PDCI_READFN_SET_LOC_BE(0, p1-begin);
  PDCI_READFN_RET_ERRCODE_WARN("[in PDC_a_string_read]", 0, PDC_CHAR_LIT_NOT_FOUND);

 fatal_alloc_err:
  PDCI_READFN_RET_ERRCODE_FATAL("[in PDC_a_string_read_internal]", "Memory alloc error", PDC_ALLOC_ERR);

 fatal_nb_io_err:
  PDCI_READFN_RET_ERRCODE_FATAL("[in PDC_a_string_read_internal]", "IO error (nb)", PDC_IO_ERR);

 fatal_mb_io_err:
  PDCI_READFN_RET_ERRCODE_FATAL("[in PDC_a_string_read_internal]", "IO error (mb)", PDC_IO_ERR);

 fatal_forward_err:
  PDCI_READFN_RET_ERRCODE_FATAL("[in PDC_a_string_read_internal]", "IO_forward error", PDC_FORWARD_ERR);
}

PDC_error_t
PDC_a_string_SE_read_internal(PDC_t *pdc, PDC_base_em *em, const char *stopRegexp,
			    PDC_base_ed *ed, PDC_string *s_out)
{
  PDC_regexp_t *compiled_exp;
  if (PDC_ERR == PDC_regexp_compile(pdc, stopRegexp, &compiled_exp)) {
    goto bad_exp;
  }
  return PDC_a_string_CSE_read_internal(pdc, em, compiled_exp, ed, s_out);

 bad_exp:
  PDCI_READFN_SET_NULLSPAN_LOC(0);
  /* regexp_compile already issued a warning */
  /* PDCI_READFN_RET_ERRCODE_WARN("[in PDC_a_string_SE_read]", 0, PDC_INVALID_REGEXP); */
  PDCI_READFN_RET_ERRCODE_NOWARN(PDC_INVALID_REGEXP);
}

PDC_error_t
PDC_a_string_CSE_read_internal(PDC_t *pdc, PDC_base_em *em, PDC_regexp_t *stopRegexp,
			     PDC_base_ed *ed, PDC_string *s_out)
{
  PDC_byte        *begin, *p1, *p2, *end;
  int             eor, eof;
  size_t          bytes;
  int             matchlen;

  PDC_TRACE(pdc->disc, "PDC_a_string_CSE_read_internal called");
  matchlen = stopRegexp->max;
  if (matchlen && pdc->disc->stop_regexp) {
    if (pdc->disc->stop_regexp->max == 0) {
      matchlen = 0;
    } else if (matchlen < pdc->disc->stop_regexp->max) {
      matchlen = pdc->disc->stop_regexp->max;
    }
  }
  if (PDC_ERR == PDCI_IO_needbytes(pdc, &begin, &p1, &p2, &end, &eor, &eof, &bytes)) {
    goto fatal_nb_io_err;
  }
  while (1) {
    if (p1 == end) {
      if (eor && stopRegexp->or_eor) { /* EOR is valid stop */
	PDCI_STR_SET(s_out, (char*)begin, (char*)p1);
	if (PDC_ERR == PDCI_IO_forward(pdc, p1-begin)) {
	  goto fatal_forward_err;
	}
	ed->errCode = PDC_NO_ERR;
	return PDC_OK;
      }
      if (eor|eof) {
	break;
      }
      if (PDC_ERR == PDCI_IO_morebytes(pdc, &begin, &p1, &p2, &end, &eor, &eof, &bytes)) {
	goto fatal_mb_io_err;
      }
      if (bytes == 0) {
	break;
      }
      /* longer regexp match may work now */
      if (matchlen == 0) { /* no limit on match size, back up all the way */
	p1 = begin;
      } else if (matchlen > 1) {
	p1 -= (matchlen - 1);
      }
      continue;
    }
    /* p1 < end */
    if (PDCI_regexpMatch(pdc, stopRegexp, p1, end, 0)) {
      PDCI_STR_SET(s_out, (char*)begin, (char*)p1);
      if (PDC_ERR == PDCI_IO_forward(pdc, p1-begin)) {
	goto fatal_forward_err;
      }
      ed->errCode = PDC_NO_ERR;
      return PDC_OK;
    }
    if (pdc->disc->stop_maxlen && ((p1-begin) >= pdc->disc->stop_maxlen)) {
      PDC_WARN(pdc->disc, "PDC_a_string_CSE_read: scan terminated early due to disc->stop_maxlen");
      break;
    }
    if (pdc->disc->stop_regexp && PDCI_regexpMatch(pdc, pdc->disc->stop_regexp, p1, end, 0)) {
      PDC_WARN(pdc->disc, "PDC_a_string_CSE_read: scan terminated early due to disc->stop_regexp");
      break;
    }
    p1++;
  }
  goto not_found;

 not_found:
  PDCI_READFN_SET_LOC_BE(0, p1-begin);
  PDCI_READFN_RET_ERRCODE_WARN("[in PDC_a_string_CSE_read]", 0, PDC_REGEXP_NOT_FOUND);

 fatal_alloc_err:
  PDCI_READFN_RET_ERRCODE_FATAL("[in PDC_a_string_CSE_read_internal]", "Memory alloc error", PDC_ALLOC_ERR);

 fatal_nb_io_err:
  PDCI_READFN_RET_ERRCODE_FATAL("[in PDC_a_string_CSE_read_internal]", "IO error (nb)", PDC_IO_ERR);

 fatal_mb_io_err:
  PDCI_READFN_RET_ERRCODE_FATAL("[in PDC_a_string_CSE_read_internal]", "IO error (mb)", PDC_IO_ERR);

 fatal_forward_err:
  PDCI_READFN_RET_ERRCODE_FATAL("[in PDC_a_string_CSE_read_internal]", "IO_forward error", PDC_FORWARD_ERR);
}

PDC_error_t
PDC_e_string_FW_read_internal(PDC_t *pdc, PDC_base_em *em, size_t width,
			    PDC_base_ed *ed, PDC_string *s_out)
{
  PDC_byte        *begin, *p1, *p2, *end;
  int             eor, eof;
  size_t          bytes;

  PDC_TRACE(pdc->disc, "PDC_e_string_FW_read_internal called");
  if (width <= 0) {
    PDC_WARN(pdc->disc, "UNEXPECTED PARAM VALUE: PDC_e_string_FW_read called with width <= 0");
    goto bad_param_err;
  }
  /* ensure there are width chars available */
  if (PDC_ERR == PDCI_IO_needbytes(pdc, &begin, &p1, &p2, &end, &eor, &eof, &bytes)) {
    goto fatal_nb_io_err;
  }
  while (end-begin < width) {
    if ((eor|eof) || bytes == 0) {
      goto width_not_avail;
    }
    if (PDC_ERR == PDCI_IO_morebytes(pdc, &begin, &p1, &p2, &end, &eor, &eof, &bytes)) {
      goto fatal_mb_io_err;
    }
  }
  /* end-begin >= width */
  end = begin + width;
  PDCI_STR_SET(s_out, (char*)begin, (char*)end);
  /* convert EBCDIC to ASCII */  
  for (p2 = (PDC_byte*)s_out->str, end = p2 + s_out->len; p2 < end; p2++) {
    (*p2) = PDC_ea_tab[(int)(*p2)];
  }
  if (PDC_ERR == PDCI_IO_forward(pdc, width)) {
    goto fatal_forward_err;
  }
  ed->errCode = PDC_NO_ERR;
  return PDC_OK;

 bad_param_err:
  PDCI_READFN_SET_NULLSPAN_LOC(0);
  PDCI_READFN_RET_ERRCODE_WARN("[in PDC_e_string_FW_read]", 0, PDC_BAD_PARAM);

 width_not_avail:
  PDCI_READFN_SET_LOC_BE(0, end-begin);
  PDCI_READFN_RET_ERRCODE_WARN("[in PDC_e_string_FW_read]", 0, PDC_WIDTH_NOT_AVAILABLE);

 fatal_alloc_err:
  PDCI_READFN_RET_ERRCODE_FATAL("[in PDC_e_string_FW_read_internal]", "Memory alloc error", PDC_ALLOC_ERR);

 fatal_nb_io_err:
  PDCI_READFN_RET_ERRCODE_FATAL("[in PDC_e_string_FW_read_internal]", "IO error (nb)", PDC_IO_ERR);

 fatal_mb_io_err:
  PDCI_READFN_RET_ERRCODE_FATAL("[in PDC_e_string_FW_read_internal]", "IO error (mb)", PDC_IO_ERR);

 fatal_forward_err:
  PDCI_READFN_RET_ERRCODE_FATAL("[in PDC_e_string_FW_internal]", "IO_forward error", PDC_FORWARD_ERR);
}

PDC_error_t
PDC_e_string_read_internal(PDC_t *pdc, PDC_base_em *em, PDC_byte stopChar,
			  PDC_base_ed *ed, PDC_string *s_out)
{
  PDC_byte        *begin, *p1, *p2, *end;
  int             eor, eof;
  size_t          bytes;
  int             matchlen = -1;
  PDC_byte        estopChar = PDC_mod_ae_tab[(int)stopChar];

  PDC_TRACE(pdc->disc, "PDC_e_string_read_internal called");
  if (pdc->disc->stop_regexp) {
    matchlen = pdc->disc->stop_regexp->max;
  }
  if (PDC_ERR == PDCI_IO_needbytes(pdc, &begin, &p1, &p2, &end, &eor, &eof, &bytes)) {
    goto fatal_nb_io_err;
  }
  while (1) {
    if (p1 == end) {
      if (estopChar && (eor|eof)) {
	break;
      }
      if (estopChar || !(eor|eof)) {
	if (PDC_ERR == PDCI_IO_morebytes(pdc, &begin, &p1, &p2, &end, &eor, &eof, &bytes)) {
	  goto fatal_mb_io_err;
	}
	if (bytes == 0) {
	  break;
	}
	/* longer regexp match may work now */
	if (matchlen == 0) { /* no limit on match size, back up all the way */
	  p1 = begin;
	} else if (matchlen > 1) {
	  p1 -= (matchlen - 1);
	}
	continue;
      }
    }
    /* (p1 < end) OR (p1 == end, estopChar is 0, eor|eof set) */
    if (p1 == end || estopChar == (*p1)) {
      /* success */
      PDCI_STR_SET(s_out, (char*)begin, (char*)p1);
      /* convert EBCDIC to ASCII */
      for (p2 = (PDC_byte*)s_out->str, end = p2 + s_out->len; p2 < end; p2++) {
	(*p2) = PDC_ea_tab[(int)(*p2)];
      }
      if (PDC_ERR == PDCI_IO_forward(pdc, p1-begin)) {
	goto fatal_forward_err;
      }
      ed->errCode = PDC_NO_ERR;
      return PDC_OK;
    }
    if (pdc->disc->stop_maxlen && ((p1-begin) >= pdc->disc->stop_maxlen)) {
      PDC_WARN(pdc->disc, "PDC_e_string_read: scan terminated early due to disc->stop_maxlen");
      break;
    }
    if (pdc->disc->stop_regexp && PDCI_regexpMatch(pdc, pdc->disc->stop_regexp, p1, end, 1)) {
      PDC_WARN(pdc->disc, "PDC_e_string_read: scan terminated early due to disc->stop_regexp");
      break;
    }
    p1++;
  }
  goto not_found;

 not_found:
  PDCI_READFN_SET_LOC_BE(0, p1-begin);
  PDCI_READFN_RET_ERRCODE_WARN("[in PDC_e_string_read]", 0, PDC_CHAR_LIT_NOT_FOUND);

 fatal_alloc_err:
  PDCI_READFN_RET_ERRCODE_FATAL("[in PDC_e_string_read_internal]", "Memory alloc error", PDC_ALLOC_ERR);

 fatal_nb_io_err:
  PDCI_READFN_RET_ERRCODE_FATAL("[in PDC_e_string_read_internal]", "IO error (nb)", PDC_IO_ERR);

 fatal_mb_io_err:
  PDCI_READFN_RET_ERRCODE_FATAL("[in PDC_e_string_read_internal]", "IO error (mb)", PDC_IO_ERR);

 fatal_forward_err:
  PDCI_READFN_RET_ERRCODE_FATAL("[in PDC_e_string_read_internal]", "IO_forward error", PDC_FORWARD_ERR);
}

PDC_error_t
PDC_e_string_SE_read_internal(PDC_t *pdc, PDC_base_em *em, const char *stopRegexp,
			    PDC_base_ed *ed, PDC_string *s_out)
{
  PDC_regexp_t *compiled_exp;
  if (PDC_ERR == PDC_regexp_compile(pdc, stopRegexp, &compiled_exp)) {
    goto bad_exp;
  }
  return PDC_e_string_CSE_read_internal(pdc, em, compiled_exp, ed, s_out);

 bad_exp:
  PDCI_READFN_SET_NULLSPAN_LOC(0);
  /* regexp_compile already issued a warning */
  /* PDCI_READFN_RET_ERRCODE_WARN("[in PDC_e_string_SE_read]", 0, PDC_INVALID_REGEXP); */
  PDCI_READFN_RET_ERRCODE_NOWARN(PDC_INVALID_REGEXP);
}

PDC_error_t
PDC_e_string_CSE_read_internal(PDC_t *pdc, PDC_base_em *em, PDC_regexp_t *stopRegexp,
			     PDC_base_ed *ed, PDC_string *s_out)
{
  PDC_byte        *begin, *p1, *p2, *end;
  int             eor, eof;
  size_t          bytes;
  int             matchlen;

  PDC_TRACE(pdc->disc, "PDC_e_string_CSE_read_internal called");
  matchlen = stopRegexp->max;
  if (matchlen && pdc->disc->stop_regexp) {
    if (pdc->disc->stop_regexp->max == 0) {
      matchlen = 0;
    } else if (matchlen < pdc->disc->stop_regexp->max) {
      matchlen = pdc->disc->stop_regexp->max;
    }
  }
  if (PDC_ERR == PDCI_IO_needbytes(pdc, &begin, &p1, &p2, &end, &eor, &eof, &bytes)) {
    goto fatal_nb_io_err;
  }
  while (1) {
    if (p1 == end) {
      if (eor && stopRegexp->or_eor) { /* EOR is valid stop */
	PDCI_STR_SET(s_out, (char*)begin, (char*)p1);
	/* convert EBCDIC to ASCII */
	for (p2 = (PDC_byte*)s_out->str, end = p2 + s_out->len; p2 < end; p2++) {
	  (*p2) = PDC_ea_tab[(int)(*p2)];
	}
	if (PDC_ERR == PDCI_IO_forward(pdc, p1-begin)) {
	  goto fatal_forward_err;
	}
	ed->errCode = PDC_NO_ERR;
	return PDC_OK;
      }
      if (eor|eof) {
	break;
      }
      if (PDC_ERR == PDCI_IO_morebytes(pdc, &begin, &p1, &p2, &end, &eor, &eof, &bytes)) {
	goto fatal_mb_io_err;
      }
      if (bytes == 0) {
	break;
      }
      /* longer regexp match may work now */
      if (matchlen == 0) { /* no limit on match size, back up all the way */
	p1 = begin;
      } else if (matchlen > 1) {
	p1 -= (matchlen - 1);
      }
      continue;
    }
    /* p1 < end */
    if (PDCI_regexpMatch(pdc, stopRegexp, p1, end, 1)) {
      PDCI_STR_SET(s_out, (char*)begin, (char*)p1);
      /* convert EBCDIC to ASCII */
      for (p2 = (PDC_byte*)s_out->str, end = p2 + s_out->len; p2 < end; p2++) {
	(*p2) = PDC_ea_tab[(int)(*p2)];
      }
      if (PDC_ERR == PDCI_IO_forward(pdc, p1-begin)) {
	goto fatal_forward_err;
      }
      ed->errCode = PDC_NO_ERR;
      return PDC_OK;
    }
    if (pdc->disc->stop_maxlen && ((p1-begin) >= pdc->disc->stop_maxlen)) {
      PDC_WARN(pdc->disc, "PDC_e_string_CSE_read: scan terminated early due to disc->stop_maxlen");
      break;
    }
    if (pdc->disc->stop_regexp && PDCI_regexpMatch(pdc, pdc->disc->stop_regexp, p1, end, 1)) {
      PDC_WARN(pdc->disc, "PDC_e_string_CSE_read: scan terminated early due to disc->stop_regexp");
      break;
    }
    p1++;
  }
  goto not_found;

 not_found:
  PDCI_READFN_SET_LOC_BE(0, p1-begin);
  PDCI_READFN_RET_ERRCODE_WARN("[in PDC_e_string_CSE_read]", 0, PDC_REGEXP_NOT_FOUND);

 fatal_alloc_err:
  PDCI_READFN_RET_ERRCODE_FATAL("[in PDC_e_string_CSE_read_internal]", "Memory alloc error", PDC_ALLOC_ERR);

 fatal_nb_io_err:
  PDCI_READFN_RET_ERRCODE_FATAL("[in PDC_e_string_CSE_read_internal]", "IO error (nb)", PDC_IO_ERR);

 fatal_mb_io_err:
  PDCI_READFN_RET_ERRCODE_FATAL("[in PDC_e_string_CSE_read_internal]", "IO error (mb)", PDC_IO_ERR);

 fatal_forward_err:
  PDCI_READFN_RET_ERRCODE_FATAL("[in PDC_e_string_CSE_read_internal]", "IO_forward error", PDC_FORWARD_ERR);
}


/* ================================================================================ */
/* INTERNAL MISC ROUTINES */

size_t
PDCI_regexpMatch(PDC_t *pdc, PDC_regexp_t *regexp, PDC_byte *begin, PDC_byte *end, int ebcdic)
{
  PDC_byte match_char; 
  if (!begin || !begin[0]) {
    return 0;
  }
  match_char = ebcdic ? PDC_ea_tab[(int)begin[0]] : begin[0];
  if (strchr(regexp->charset, match_char)) {
    return 1; /* match length is 1 char */
  }
  return 0;
}

PDC_byte*
PDCI_findfirst(const PDC_byte *begin, const PDC_byte *end, PDC_byte b)
{
  begin--;
  while (++begin < end) {
    if (*begin == b) return (PDC_byte*)begin;
  }
  return 0;
}

PDC_byte*
PDCI_findlast(const PDC_byte *begin, const PDC_byte *end, PDC_byte b)
{
  while (--end >= begin) {
    if (*end == b) return (PDC_byte*)end;
  }
  return 0;
}

/* ================================================================================ */
