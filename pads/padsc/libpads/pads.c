## This source file is run through srcgen.pl to produce 
## a number of generated files:
##
##    libpadsc-macros-gen.h       : generally useful macros
##    libpadsc-read-macros-gen.h  : macros that help implement read  functions
##    libpadsc-write-macros-gen.h : macros that help implement write functions
##    libpadsc-acc-macros-gen.h   : macros that help implement accum functions
##    libpadsc-misc-macros-gen.h  : macros that help implement misc  functions
## 
##    libpadsc-read-gen.c         : generated read  functions
##    libpadsc-write-gen.c        : generated write functions
##    libpadsc-cobol-read-gen.c   : generated cobol read  functions
##    libpadsc-cobol-write-gen.c  : generated cobol write functions
##    libpadsc-acc-gen.c          : generated accum functions
##    libpadsc-misc-gen.c         : generated misc  functions
##    libpadsc-cobol-only-gen.c   : stubs for functions not required by cobol-only version of library
##    libpadsc-gen.c              : the rest of the libpadsc library
##
/* ********************* BEGIN_MACROS(libpadsc-macros-gen.h) ********************** */
/*
 * Some generally useful macros
 * 
 * Kathleen Fisher, Robert Gruber
 * AT&T Labs Research
 */

/* ********************************** END_HEADER ********************************** */

/* ================================================================================ */
/* MACROS USED BY READ FUNCTIONS
 *
 * These macros assume csm/ed have been set up
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
    if (pdc->speclev == 0 && (*csm < PDC_Ignore)) {
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
    if (pdc->speclev == 0 && (*csm < PDC_Ignore)) {
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
    if (pdc->speclev == 0 && (*csm < PDC_Ignore)) {
      ed->errCode = (errcode);
    }
    return PDC_ERR;
  } while (0)
/* END_MACRO */

/* Does not use ed->loc */
#define PDCI_READFN_RET_ERRCODE_FATAL(whatfn, msg, errcode)
  do {
    if (pdc->speclev == 0 && (*csm < PDC_Ignore)) {
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

/* PDC_string_Cstr_copy -- inline version.  Caller must provide fatal_alloc_err target */
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

/* Fill string s with n copies of c.  Caller must provide fatal_alloc_err target */
#define PDCI_STRFILL(s, c, n)
  do {
    if (!(s)->rbuf) {
      if (!((s)->rbuf = RMM_new_rbuf(pdc->rmm_nz))) {
	goto fatal_alloc_err;
      }
    }
    if (RBuf_reserve((s)->rbuf, (void**)&((s)->str), sizeof(char), (n)+1, PDCI_STRING_HINT)) {
      goto fatal_alloc_err;
    }
    memset((s)->str, (c), (n));
    (s)->str[n] = 0;
    (s)->len = (n);
    /* if ((s)->sharing) { PDC_WARN1(pdc->disc, "XXX_REMOVE fill: string %p is no longer sharing", (void*)(s)); } */
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

/* If *csm is CheckAndSet, point to or copy (depending on pdc->disc->copy_strings)
 * the string that goes from b to e-1.
 * Caller must provide fatal_alloc_err target
 */
#define PDCI_A_STR_SET(s, b, e)
  do {
    if (*csm == PDC_CheckAndSet && (s)) {
      size_t wdth = (e)-(b); 
      if (pdc->disc->copy_strings) {
	PDCI_STR_CPY((s), (b), wdth);
      } else {
	PDCI_STR_SHARE((s), (b), wdth);
      }
    }
  } while (0)
/* END_MACRO */

/* If *csm is CheckAndSet, copy (always copy for EBCDIC)
 * the string that goes from b to e-1.
 * Caller must provide fatal_alloc_err target
 */
#define PDCI_E_STR_SET(s, b, e)
  do {
    if (*csm == PDC_CheckAndSet && (s)) {
      size_t wdth = (e)-(b); 
      PDCI_STR_CPY((s), (b), wdth);
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
/* MACROS USED BY ACCUM FUNCTIONS */
 
/* Useful constants */

#define PDCI_HALFMIN_INT64   -4611686018427387904LL
#define PDCI_HALFMAX_INT64    4611686018427387903LL
#define PDCI_HALFMAX_UINT64   9223372036854775807ULL
#define PDCI_LARGE_NEG_DBL   -4611686018427387904.0
#define PDCI_LARGE_POS_DBL    4611686018427387903.0
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

#define PDCI_AE_INT_READ_FN(fn_pref, targ_type, bytes2num_fn, invalid_err, isspace_fn, isdigit_fn)

PDC_error_t
fn_pref ## _read_internal(PDC_t *pdc, const PDC_base_csm *csm,
			  PDC_base_ed *ed, targ_type *res_out)
{
  targ_type       tmp;   /* tmp num */
  PDC_byte        *begin, *p1, *p2, *end;
  int             eor, eof;
  size_t          bytes;

  PDC_TRACE(pdc->disc, PDCI_MacroArg2String(fn_pref) "_read_internal called" );
  if (PDC_ERR == PDCI_IO_needbytes(pdc, &begin, &p1, &p2, &end, &eor, &eof, &bytes)) {
    goto fatal_nb_io_err;
  }
  if (bytes == 0) {
    goto at_eor_or_eof_err;
  }
  switch (*csm) {
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
      if (errno == EINVAL) {
	if (p1 != end) p1++; /* move to just beyond offending char */
	goto invalid;
      }
      if (errno == ERANGE) goto range_err;
      /* success */
      if (PDC_ERR == PDCI_IO_forward(pdc, p1-begin)) {
	goto fatal_forward_err;
      }
      if (res_out && *csm == PDC_CheckAndSet) {
	(*res_out) = tmp;
      }
      ed->errCode = PDC_NO_ERR;
      return PDC_OK;
    }
  }

 at_eor_or_eof_err:
  PDCI_READFN_SET_NULLSPAN_LOC(0);
  PDCI_READFN_RET_ERRCODE_WARN(PDCI_MacroArg2String(fn_pref) "_read", 0, eor ? PDC_AT_EOR : PDC_AT_EOF);

 invalid_wspace:
  PDCI_READFN_SET_LOC_BE(0, 1);
  PDCI_READFN_RET_ERRCODE_WARN(PDCI_MacroArg2String(fn_pref) "_read", "spaces not allowed in a_int field unless flag PDC_WSPACE_OK is set", invalid_err);

 invalid:
  PDCI_READFN_SET_LOC_BE(0, p1-begin);
  PDCI_READFN_RET_ERRCODE_WARN(PDCI_MacroArg2String(fn_pref) "_read", 0, invalid_err);

 range_err:
  /* range error still consumes the number */
  PDCI_READFN_SET_LOC_BE(0, p1-begin);
  if (PDC_ERR == PDCI_IO_forward(pdc, p1-begin)) {
    goto fatal_forward_err;
  }
  PDCI_READFN_RET_ERRCODE_WARN(PDCI_MacroArg2String(fn_pref) "_read", 0, PDC_RANGE);

 fatal_nb_io_err:
  PDCI_READFN_RET_ERRCODE_FATAL(PDCI_MacroArg2String(fn_pref) "_read_internal", "IO error (nb)", PDC_IO_ERR);

 fatal_mb_io_err:
  PDCI_READFN_RET_ERRCODE_FATAL(PDCI_MacroArg2String(fn_pref) "_read_internal", "IO error (mb)", PDC_IO_ERR);

 fatal_forward_err:
  PDCI_READFN_RET_ERRCODE_FATAL(PDCI_MacroArg2String(fn_pref) "_read_internal", "IO_forward error", PDC_FORWARD_ERR);
}

PDC_error_t
fn_pref ## _read(PDC_t *pdc, const PDC_base_csm *csm,
		 PDC_base_ed *ed, targ_type *res_out)
{
  PDC_base_csm    csmt = PDC_CheckAndSet;
  PDC_base_ed     edt;

  if (!csm) {
    csm = &csmt;
  }
  if (!ed) {
    ed = &edt;
  }
  PDCI_IODISC_INIT_CHECKS( PDCI_MacroArg2String(fn_pref) "_read" );
  return fn_pref ## _read_internal(pdc, csm, ed, res_out);
}
/* END_MACRO */

#define PDCI_AE_INT_FW_READ_FN(fn_name, targ_type, bytes2num_fn, invalid_err, isspace_fn)

PDC_error_t
fn_name ## _internal(PDC_t *pdc, const PDC_base_csm *csm, size_t width,
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
  if (*csm == PDC_Check || *csm == PDC_CheckAndSet) {
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
  if (res_out && *csm == PDC_CheckAndSet) {
    (*res_out) = tmp;
  }
  ed->errCode = PDC_NO_ERR;
  return PDC_OK;

 bad_param_err:
  PDCI_READFN_SET_NULLSPAN_LOC(0);
  PDCI_READFN_RET_ERRCODE_WARN(PDCI_MacroArg2String(fn_name), 0, PDC_BAD_PARAM);

 width_not_avail:
  /* FW field: eat the space whether or not there is an error */
  PDCI_READFN_SET_LOC_BE(0, end-begin);
  if (PDC_ERR == PDCI_IO_forward(pdc, end-begin)) {
    goto fatal_forward_err;
  }
  PDCI_READFN_RET_ERRCODE_WARN(PDCI_MacroArg2String(fn_name), 0, PDC_WIDTH_NOT_AVAILABLE);

 invalid:
  /* FW field: eat the space whether or not there is an error */
  PDCI_READFN_SET_LOC_BE(0, width);
  if (PDC_ERR == PDCI_IO_forward(pdc, width)) {
    goto fatal_forward_err;
  }
  PDCI_READFN_RET_ERRCODE_WARN(PDCI_MacroArg2String(fn_name), 0, invalid_err);

 invalid_wspace:
  /* FW field: eat the space whether or not there is an error */
  PDCI_READFN_SET_LOC_BE(0, width);
  if (PDC_ERR == PDCI_IO_forward(pdc, width)) {
    goto fatal_forward_err;
  }
  PDCI_READFN_RET_ERRCODE_WARN(PDCI_MacroArg2String(fn_name), "spaces not allowed in a_int field unless flag PDC_WSPACE_OK is set", invalid_err);

 range_err:
  /* FW field: eat the space whether or not there is an error */
  PDCI_READFN_SET_LOC_BE(0, width);
  if (PDC_ERR == PDCI_IO_forward(pdc, width)) {
    goto fatal_forward_err;
  }
  PDCI_READFN_RET_ERRCODE_WARN(PDCI_MacroArg2String(fn_name), 0, PDC_RANGE);

  /* fatal_alloc_err:
     PDCI_READFN_RET_ERRCODE_FATAL(PDCI_MacroArg2String(fn_name) "_internal", "Memory alloc err", PDC_ALLOC_ERR); */

 fatal_nb_io_err:
  PDCI_READFN_RET_ERRCODE_FATAL(PDCI_MacroArg2String(fn_name) "_internal", "IO error (nb)", PDC_IO_ERR);

 fatal_mb_io_err:
  PDCI_READFN_RET_ERRCODE_FATAL(PDCI_MacroArg2String(fn_name) "_internal", "IO error (mb)", PDC_IO_ERR);

 fatal_forward_err:
  PDCI_READFN_RET_ERRCODE_FATAL(PDCI_MacroArg2String(fn_name) "_internal", "IO_forward error", PDC_FORWARD_ERR);
}

PDC_error_t
fn_name(PDC_t *pdc, const PDC_base_csm *csm, size_t width,
	PDC_base_ed *ed, targ_type *res_out)
{
  PDC_base_csm    csmt = PDC_CheckAndSet;
  PDC_base_ed     edt;

  if (!csm) {
    csm = &csmt;
  }
  if (!ed) {
    ed = &edt;
  }
  PDCI_IODISC_INIT_CHECKS( PDCI_MacroArg2String(fn_name) );
  return fn_name ## _internal(pdc, csm, width, ed, res_out);
}
/* END_MACRO */

#define PDCI_B1_INT_READ_FN(fn_name, targ_type)

PDC_error_t
fn_name ## _internal(PDC_t *pdc, const PDC_base_csm *csm,
		     PDC_base_ed *ed, targ_type *res_out)
{
  PDC_byte        *begin, *p1, *p2, *end;
  int             eor, eof;
  size_t          bytes;

  PDC_TRACE(pdc->disc, PDCI_MacroArg2String(fn_name) "_internal called" );
  if (PDC_ERR == PDCI_IO_needbytes(pdc, &begin, &p1, &p2, &end, &eor, &eof, &bytes)) {
    goto fatal_nb_io_err;
  }
  if (bytes == 0) {
    goto width_not_avail;
  }
  /* end-begin >= width */
  if (res_out && *csm == PDC_CheckAndSet) {
    (*res_out) = *begin;
  }
  if (PDC_ERR == PDCI_IO_forward(pdc, 1)) {
    goto fatal_forward_err;
  }
  ed->errCode = PDC_NO_ERR;
  return PDC_OK;

 width_not_avail:
  PDCI_READFN_SET_LOC_BE(0, end-begin);
  PDCI_READFN_RET_ERRCODE_WARN(PDCI_MacroArg2String(fn_name), 0, PDC_WIDTH_NOT_AVAILABLE);

 fatal_nb_io_err:
  PDCI_READFN_RET_ERRCODE_FATAL(PDCI_MacroArg2String(fn_name) "_internal", "IO error (nb)", PDC_IO_ERR);

 fatal_forward_err:
  PDCI_READFN_RET_ERRCODE_FATAL(PDCI_MacroArg2String(fn_name) "_internal", "IO_forward error", PDC_FORWARD_ERR);
}


PDC_error_t
fn_name(PDC_t *pdc, const PDC_base_csm *csm,
	PDC_base_ed *ed, targ_type *res_out)
{
  PDC_base_csm    csmt = PDC_CheckAndSet;
  PDC_base_ed     edt;

  if (!csm) {
    csm = &csmt;
  }
  if (!ed) {
    ed = &edt;
  }
  PDCI_IODISC_INIT_CHECKS( PDCI_MacroArg2String(fn_name) );
  return fn_name ## _internal(pdc, csm, ed, res_out);
}
/* END_MACRO */

#define PDCI_B_INT_READ_FN(fn_name, targ_type, width, swapmem_op)

PDC_error_t
fn_name ## _internal(PDC_t *pdc, const PDC_base_csm *csm,
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
  if (res_out && *csm == PDC_CheckAndSet) {
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
  PDCI_READFN_RET_ERRCODE_WARN(PDCI_MacroArg2String(fn_name), 0, PDC_WIDTH_NOT_AVAILABLE);

 fatal_nb_io_err:
  PDCI_READFN_RET_ERRCODE_FATAL(PDCI_MacroArg2String(fn_name) "_internal", "IO error (nb)", PDC_IO_ERR);

 fatal_mb_io_err:
  PDCI_READFN_RET_ERRCODE_FATAL(PDCI_MacroArg2String(fn_name) "_internal", "IO error (mb)", PDC_IO_ERR);

 fatal_forward_err:
  PDCI_READFN_RET_ERRCODE_FATAL(PDCI_MacroArg2String(fn_name) "_internal", "IO_forward error", PDC_FORWARD_ERR);
}

PDC_error_t
fn_name(PDC_t *pdc, const PDC_base_csm *csm,
	PDC_base_ed *ed, targ_type *res_out)
{
  PDC_base_csm    csmt = PDC_CheckAndSet;
  PDC_base_ed     edt;

  if (!csm) {
    csm = &csmt;
  }
  if (!ed) {
    ed = &edt;
  }
  PDCI_IODISC_INIT_CHECKS( PDCI_MacroArg2String(fn_name) );
  return fn_name ## _internal(pdc, csm, ed, res_out);
}
/* END_MACRO */

#define PDCI_EBCBCDSB_INT_READ_FN(fn_name, targ_type, bytes2num_fn, invalid_err, width)

PDC_error_t
fn_name ## _internal(PDC_t *pdc, const PDC_base_csm *csm, PDC_uint32 num_digits_or_bytes,
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
  if (*csm == PDC_Check || *csm == PDC_CheckAndSet) {
    tmp = bytes2num_fn(pdc, begin, num_digits_or_bytes, &p1);
    if (errno) goto invalid_range_dom;
  }
  /* success */
  if (PDC_ERR == PDCI_IO_forward(pdc, width)) {
    goto fatal_forward_err;
  }
  if (res_out && *csm == PDC_CheckAndSet) {
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
    PDCI_READFN_RET_ERRCODE_WARN(PDCI_MacroArg2String(fn_name), 0, invalid_err);
  case ERANGE:
    PDCI_READFN_RET_ERRCODE_WARN(PDCI_MacroArg2String(fn_name), 0, PDC_RANGE);
  case EDOM:
    PDCI_READFN_RET_ERRCODE_WARN(PDCI_MacroArg2String(fn_name), 0, PDC_BAD_PARAM);
  }

 width_not_avail:
  PDCI_READFN_SET_LOC_BE(0, end-begin);
  PDCI_READFN_RET_ERRCODE_WARN(PDCI_MacroArg2String(fn_name), 0, PDC_WIDTH_NOT_AVAILABLE);

 fatal_nb_io_err:
  PDCI_READFN_RET_ERRCODE_FATAL(PDCI_MacroArg2String(fn_name) "_internal", "IO error (nb)", PDC_IO_ERR);

 fatal_mb_io_err:
  PDCI_READFN_RET_ERRCODE_FATAL(PDCI_MacroArg2String(fn_name) "_internal", "IO error (mb)", PDC_IO_ERR);

 fatal_forward_err:
  PDCI_READFN_RET_ERRCODE_FATAL(PDCI_MacroArg2String(fn_name) "_internal", "IO_forward error", PDC_FORWARD_ERR);
}

PDC_error_t
fn_name(PDC_t *pdc, const PDC_base_csm *csm, PDC_uint32 num_digits_or_bytes,
	PDC_base_ed *ed, targ_type *res_out)
{
  PDC_base_csm    csmt = PDC_CheckAndSet;
  PDC_base_ed     edt;

  if (!csm) {
    csm = &csmt;
  }
  if (!ed) {
    ed = &edt;
  }
  PDCI_IODISC_INIT_CHECKS( PDCI_MacroArg2String(fn_name) );
  return fn_name ## _internal(pdc, csm, num_digits_or_bytes, ed, res_out);
}
/* END_MACRO */

#define PDCI_EBCBCDSB_FPOINT_READ_FN(fn_name, targ_type, internal_numerator_read_fn, width, dexp_max)

PDC_error_t
fn_name ## _internal(PDC_t *pdc, const PDC_base_csm *csm, PDC_uint32 num_digits_or_bytes, PDC_uint32 d_exp,
		     PDC_base_ed *ed, targ_type *res_out)
{
  targ_type       tmp;   /* tmp num */

  PDC_TRACE(pdc->disc, PDCI_MacroArg2String(fn_name) "_internal called" );
  (pdc->inestlev)++;
  if (PDC_ERR == internal_numerator_read_fn(pdc, csm, num_digits_or_bytes, ed, &(tmp.num))) {
    /* ed filled in already, IO cursor advanced if appropriate */
    (pdc->inestlev)--;
    PDCI_READFN_RET_EXIST_ERRCODE_WARN(PDCI_MacroArg2String(fn_name), 0);
  }
  (pdc->inestlev)--;
  /* so far so good, IO cursor has been advanced, ed->errCode set to PDC_NO_ERR */
  if (d_exp > dexp_max) {
    PDCI_READFN_SET_LOC_BE(-width, 0);
    PDCI_READFN_RET_ERRCODE_WARN(PDCI_MacroArg2String(fn_name), 0, PDC_BAD_PARAM);
  }
  if (res_out && *csm == PDC_CheckAndSet) {
    tmp.denom = PDCI_10toThe[d_exp];
    (*res_out) = tmp;
  }
  return PDC_OK;
}

PDC_error_t
fn_name(PDC_t *pdc, const PDC_base_csm *csm, PDC_uint32 num_digits_or_bytes, PDC_uint32 d_exp,
	PDC_base_ed *ed, targ_type *res_out)
{
  PDC_base_csm    csmt = PDC_CheckAndSet;
  PDC_base_ed     edt;

  if (!csm) {
    csm = &csmt;
  }
  if (!ed) {
    ed = &edt;
  }
  PDCI_IODISC_INIT_CHECKS( PDCI_MacroArg2String(fn_name) );
  return fn_name ## _internal(pdc, csm, num_digits_or_bytes, d_exp, ed, res_out);
}
/* END_MACRO */

/* ********************************* BEGIN_TRAILER ******************************** */
/* ********************************** END_MACROS ********************************** */
/* ****************** BEGIN_MACROS(libpadsc-write-macros-gen.h) ******************* */
/*
 * Macros that help implement write functions
 * 
 * Kathleen Fisher, Robert Gruber
 * AT&T Labs Research
 */

/* ********************************** END_HEADER ********************************** */

#define PDCI_A_INT_WRITE_FN(fn_pref, targ_type, fmt, inv_type, inv_val)

ssize_t
fn_pref ## _write2buf_internal(PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, const PDC_base_ed *ed, const targ_type *in)
{
  targ_type     t;
  int           writelen;
  PDC_inv_valfn fn;
  void         *type_args[1] = { 0 };

  if (ed->errCode == PDC_NO_ERR) {
    t = *in;
  } else {
    fn = PDCI_GET_INV_VALFN(pdc, inv_type);
    if (!fn || (PDC_ERR == fn(pdc, (void*)ed, (void*)&t, type_args))) {
      t = (ed->errCode == PDC_USER_CONSTRAINT_VIOLATION) ? *in : inv_val;
    }
  }
  sfstrset(pdc->tmp1, 0);
  writelen = sfprintf(pdc->tmp1, fmt, t);
  if (writelen <= 0) return writelen;
  if (writelen > buf_len) {
    (*buf_full) = 1;
    return -1;
  }
  memcpy(buf, sfstruse(pdc->tmp1), writelen);
  return writelen;
}

ssize_t
fn_pref ## _write2io_internal(PDC_t *pdc, Sfio_t *io, const PDC_base_ed *ed, const targ_type *in)
{
  targ_type     t;
  PDC_inv_valfn fn;
  void         *type_args[1] = { 0 };

  if (ed->errCode == PDC_NO_ERR) {
    t = *in;
  } else {
    fn = PDCI_GET_INV_VALFN(pdc, inv_type);
    if (!fn || (PDC_ERR == fn(pdc, (void*)ed, (void*)&t, type_args))) {
      t = (ed->errCode == PDC_USER_CONSTRAINT_VIOLATION) ? *in : inv_val;
    }
  }
  return sfprintf(io, fmt, t);
}

ssize_t
fn_pref ## _write2buf(PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, const PDC_base_ed *ed, const targ_type *in)
{
  PDCI_DISC_INIT_CHECKS_RET_SSIZE( PDCI_MacroArg2String(fn_pref) "_write2buf" );
  PDCI_NULLPARAM_CHECK_RET_SSIZE( PDCI_MacroArg2String(fn_pref) "_write2buf" , buf );
  PDCI_NULLPARAM_CHECK_RET_SSIZE( PDCI_MacroArg2String(fn_pref) "_write2buf" , buf_full );
  PDCI_NULLPARAM_CHECK_RET_SSIZE( PDCI_MacroArg2String(fn_pref) "_write2buf" , in );

  return fn_pref ## _write2buf_internal(pdc, buf, buf_len, buf_full, ed, in);
}

ssize_t
fn_pref ## _write2io(PDC_t *pdc, Sfio_t *io, const PDC_base_ed *ed, const targ_type *val)
{
  PDCI_DISC_INIT_CHECKS_RET_SSIZE( PDCI_MacroArg2String(fn_pref) "_write2io" );
  PDCI_NULLPARAM_CHECK_RET_SSIZE( PDCI_MacroArg2String(fn_pref) "_write2io" , io );
  PDCI_NULLPARAM_CHECK_RET_SSIZE( PDCI_MacroArg2String(fn_pref) "_write2io" , val );

  return fn_pref ## _write2io_internal(pdc, io, ed, val);
}
/* END_MACRO */

#define PDCI_E_INT_WRITE_FN(fn_pref, targ_type, num2pre, inv_type, inv_val)

ssize_t
fn_pref ## _write2buf_internal(PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, const PDC_base_ed *ed, const targ_type *in)
{
  targ_type     t;
  PDC_inv_valfn fn;
  void         *type_args[1] = { 0 };

  if (ed->errCode == PDC_NO_ERR) {
    t = *in;
  } else {
    fn = PDCI_GET_INV_VALFN(pdc, inv_type);
    if (!fn || (PDC_ERR == fn(pdc, (void*)ed, (void*)&t, type_args))) {
      t = (ed->errCode == PDC_USER_CONSTRAINT_VIOLATION) ? *in : inv_val;
    }
  }
  return num2pre ## _buf (pdc, buf, buf_len, buf_full, t);
}

ssize_t
fn_pref ## _write2io_internal(PDC_t *pdc, Sfio_t *io, const PDC_base_ed *ed, const targ_type *in)
{
  targ_type     t;
  PDC_inv_valfn fn;
  void         *type_args[1] = { 0 };

  if (ed->errCode == PDC_NO_ERR) {
    t = *in;
  } else {
    fn = PDCI_GET_INV_VALFN(pdc, inv_type);
    if (!fn || (PDC_ERR == fn(pdc, (void*)ed, (void*)&t, type_args))) {
      t = (ed->errCode == PDC_USER_CONSTRAINT_VIOLATION) ? *in : inv_val;
    }
  }
  return num2pre ## _io (pdc, io, t);
}

ssize_t
fn_pref ## _write2buf(PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, const PDC_base_ed *ed, const targ_type *in)
{
  PDCI_DISC_INIT_CHECKS_RET_SSIZE( PDCI_MacroArg2String(fn_pref) "_write2buf" );
  PDCI_NULLPARAM_CHECK_RET_SSIZE( PDCI_MacroArg2String(fn_pref) "_write2buf" , buf );
  PDCI_NULLPARAM_CHECK_RET_SSIZE( PDCI_MacroArg2String(fn_pref) "_write2buf" , buf_full );
  PDCI_NULLPARAM_CHECK_RET_SSIZE( PDCI_MacroArg2String(fn_pref) "_write2buf" , in );

  return fn_pref ## _write2buf_internal(pdc, buf, buf_len, buf_full, ed, in);
}

ssize_t
fn_pref ## _write2io(PDC_t *pdc, Sfio_t *io, const PDC_base_ed *ed, const targ_type *val)
{
  PDCI_DISC_INIT_CHECKS_RET_SSIZE( PDCI_MacroArg2String(fn_pref) "_write2io" );
  PDCI_NULLPARAM_CHECK_RET_SSIZE( PDCI_MacroArg2String(fn_pref) "_write2io" , io );
  PDCI_NULLPARAM_CHECK_RET_SSIZE( PDCI_MacroArg2String(fn_pref) "_write2io" , val );

  return fn_pref ## _write2io_internal(pdc, io, ed, val);
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
  DTOFFSET(int_type ## _dt_elt_t, key),     /* key     */
  num_bytes,                                /* size    */
  DTOFFSET(int_type ## _dt_elt_t, link),    /* link    */
  (Dtmake_f)int_type ## _dt_elt_make,       /* makef   */
  (Dtfree_f)int_type ## _dt_elt_free,       /* freef   */
  (Dtcompar_f)int_type ## _dt_elt_set_cmp,  /* comparf */
  NiL,                                      /* hashf   */
  NiL,                                      /* memoryf */
  NiL                                       /* eventf  */
};

static Dtdisc_t int_type ## _acc_dt_oset_disc = {
  DTOFFSET(int_type ## _dt_elt_t, key),     /* key     */
  num_bytes,                                /* size    */
  DTOFFSET(int_type ## _dt_elt_t, link),    /* link    */
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
  memset((void*)a, 0, sizeof(*a));
  if (!(a->dict = dtopen(&int_type ## _acc_dt_set_disc, Dtset))) {
    return PDC_ERR;
  }
  return PDC_OK;
}

PDC_error_t
int_type ## _acc_reset(PDC_t *pdc, int_type ## _acc *a)
{
  Dt_t        *dict;

  PDCI_DISC_INIT_CHECKS( PDCI_MacroArg2String(int_type) "_acc_reset" );
  PDCI_NULLPARAM_CHECK( PDCI_MacroArg2String(int_type) "_acc_reset" , a );
  if (!(dict = a->dict)) {
    return PDC_ERR;
  }
  memset((void*)a, 0, sizeof(*a));
  dtclear(dict);
  a->dict = dict;
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

double
int_type ## _acc_avg(PDC_t *pdc, int_type ## _acc *a) {
  int_type ## _acc_fold_psum(a);
  return a->avg;
}

int_type
int_type ## _acc_ravg(PDC_t *pdc, int_type ## _acc *a) {
  int_type res;
  int_type ## _acc_fold_psum(a);
  if (a->avg >= 0) {
    res = (a->avg + 0.5); /* truncate( avg + 0.5) */ 
  } else {
    res = (a->avg - 0.5); /* truncate( avg - 0.5) */ 
  }
  return res;
}

PDC_error_t
int_type ## _acc_add(PDC_t *pdc, int_type ## _acc *a, const PDC_base_ed *ed, const int_type *val)
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
      PDC_WARN(pdc->disc, "** PADSC internal error: dtinsert failed (out of memory?) **");
      return PDC_ERR;
    }
    (tmp1->key.cnt)++;
    (a->tracked)++;
  } else {
    lookup_key.val = v;
    lookup_key.cnt = 0;
    if ((tmp1 = dtmatch(a->dict, &lookup_key))) {
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

#define PDCI_FPOINT_ACCUM(fpoint_type, fpoint_descr, floatORdouble, fpoint2floatORdouble)

typedef struct fpoint_type ## _dt_key_s {
  floatORdouble  val;
  PDC_uint64     cnt;
} fpoint_type ## _dt_key_t;

typedef struct fpoint_type ## _dt_elt_s {
  fpoint_type ## _dt_key_t key;
  Dtlink_t link;
} fpoint_type ## _dt_elt_t;

/*
 * Order set comparison function: only used at the end to rehash
 * the (formerly unordered) set.  Since same val only occurs
 * once, ptr equivalence produces key equivalence.
 *   different keys: sort keys by cnt field, break tie with vals
 */
int
fpoint_type ## _dt_elt_oset_cmp(Dt_t *dt, fpoint_type ## _dt_key_t *a, fpoint_type ## _dt_key_t *b, Dtdisc_t *disc)
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
fpoint_type ## _dt_elt_set_cmp(Dt_t *dt, fpoint_type ## _dt_key_t *a, fpoint_type ## _dt_key_t *b, Dtdisc_t *disc)
{
  NoP(dt);
  NoP(disc);
  if (a->val == b->val) {
    return 0;
  }
  return 1;
}

void*
fpoint_type ## _dt_elt_make(Dt_t *dt, fpoint_type ## _dt_elt_t *a, Dtdisc_t *disc)
{
  fpoint_type ## _dt_elt_t *b;
  if ((b = oldof(0, fpoint_type ## _dt_elt_t, 1, 0))) {
    b->key.val  = a->key.val;
    b->key.cnt  = a->key.cnt;
  }
  return b;
}

void
fpoint_type ## _dt_elt_free(Dt_t *dt, fpoint_type ## _dt_elt_t *a, Dtdisc_t *disc)
{
  free(a);
}

static Dtdisc_t fpoint_type ## _acc_dt_set_disc = {
  DTOFFSET(fpoint_type ## _dt_elt_t, key),     /* key     */
  sizeof(floatORdouble),                       /* size    */
  DTOFFSET(fpoint_type ## _dt_elt_t, link),    /* link    */
  (Dtmake_f)fpoint_type ## _dt_elt_make,       /* makef   */
  (Dtfree_f)fpoint_type ## _dt_elt_free,       /* freef   */
  (Dtcompar_f)fpoint_type ## _dt_elt_set_cmp,  /* comparf */
  NiL,                                         /* hashf   */
  NiL,                                         /* memoryf */
  NiL                                          /* eventf  */
};

static Dtdisc_t fpoint_type ## _acc_dt_oset_disc = {
  DTOFFSET(fpoint_type ## _dt_elt_t, key),     /* key     */
  sizeof(floatORdouble),                       /* size    */
  DTOFFSET(fpoint_type ## _dt_elt_t, link),    /* link    */
  (Dtmake_f)fpoint_type ## _dt_elt_make,       /* makef   */
  (Dtfree_f)fpoint_type ## _dt_elt_free,       /* freef   */
  (Dtcompar_f)fpoint_type ## _dt_elt_oset_cmp, /* comparf */
  NiL,                                         /* hashf   */
  NiL,                                         /* memoryf */
  NiL                                          /* eventf  */
};

PDC_error_t
fpoint_type ## _acc_init(PDC_t *pdc, fpoint_type ## _acc *a)
{
  PDCI_DISC_INIT_CHECKS( PDCI_MacroArg2String(fpoint_type) "_acc_init" );
  PDCI_NULLPARAM_CHECK( PDCI_MacroArg2String(fpoint_type) "_acc_init" , a );
  memset((void*)a, 0, sizeof(*a));
  if (!(a->dict = dtopen(&fpoint_type ## _acc_dt_set_disc, Dtset))) {
    return PDC_ERR;
  }
  return PDC_OK;
}

PDC_error_t
fpoint_type ## _acc_reset(PDC_t *pdc, fpoint_type ## _acc *a)
{
  Dt_t        *dict;

  PDCI_DISC_INIT_CHECKS( PDCI_MacroArg2String(fpoint_type) "_acc_reset" );
  PDCI_NULLPARAM_CHECK( PDCI_MacroArg2String(fpoint_type) "_acc_reset" , a );
  if (!(dict = a->dict)) {
    return PDC_ERR;
  }
  memset((void*)a, 0, sizeof(*a));
  dtclear(dict);
  a->dict = dict;
  return PDC_OK;
}

PDC_error_t
fpoint_type ## _acc_cleanup(PDC_t *pdc, fpoint_type ## _acc *a)
{
  PDCI_DISC_INIT_CHECKS( PDCI_MacroArg2String(fpoint_type) "_acc_cleanup" );
  PDCI_NULLPARAM_CHECK( PDCI_MacroArg2String(fpoint_type) "_acc_cleanup" , a );
  if (a->dict) {
    dtclose(a->dict);
    a->dict = 0;
  }
  return PDC_OK;
}

void
fpoint_type ## _acc_fold_psum(fpoint_type ## _acc *a) {
  floatORdouble pavg, navg;
  PDC_uint64 recent = a->good - a->fold;
  if (recent == 0) {
    return;
  }
  pavg = a->psum / (floatORdouble)recent;
  navg = ((a->avg * a->fold) + (pavg * recent))/(floatORdouble)a->good;
  /* could test for change between a->avg and navg */
  a->avg = navg;
  a->psum = 0;
  a->fold += recent;
}

floatORdouble
fpoint_type ## _acc_avg(PDC_t *pdc, fpoint_type ## _acc *a) {
  fpoint_type ## _acc_fold_psum(a);
  return a->avg;
}

PDC_error_t
fpoint_type ## _acc_add(PDC_t *pdc, fpoint_type ## _acc *a, const PDC_base_ed *ed, const fpoint_type *val)
{
  floatORdouble             v          = fpoint2floatORdouble(*val);
  fpoint_type ## _dt_elt_t  insert_elt;
  fpoint_type ## _dt_key_t  lookup_key;
  fpoint_type ## _dt_elt_t  *tmp1;
  PDCI_DISC_INIT_CHECKS( PDCI_MacroArg2String(fpoint_type) "_acc_add" );
  PDCI_NULLPARAM_CHECK( PDCI_MacroArg2String(fpoint_type) "_acc_add" , a );
  PDCI_NULLPARAM_CHECK( PDCI_MacroArg2String(fpoint_type) "_acc_add" , ed );
  PDCI_NULLPARAM_CHECK( PDCI_MacroArg2String(fpoint_type) "_acc_add" , val );
  if (!a->dict) {
    return PDC_ERR;
  }
  if (ed->errCode != 0) {
    (a->bad)++;
    return PDC_OK;
  }
  if ( (v > 0 && a->psum > PDCI_LARGE_POS_DBL) ||
       (v < 0 && a->psum < PDCI_LARGE_NEG_DBL) ) {
    fpoint_type ## _acc_fold_psum(a);
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
      PDC_WARN(pdc->disc, "** PADSC internal error: dtinsert failed (out of memory?) **");
      return PDC_ERR;
    }
    (tmp1->key.cnt)++;
    (a->tracked)++;
  } else {
    lookup_key.val = v;
    lookup_key.cnt = 0;
    if ((tmp1 = dtmatch(a->dict, &lookup_key))) {
      (tmp1->key.cnt)++;
      (a->tracked)++;
    }
  }
  return PDC_OK;
}

PDC_error_t
fpoint_type ## _acc_report_internal(PDC_t *pdc, Sfio_t *outstr, const char *prefix, const char *what, int nst,
				    fpoint_type ## _acc *a)
{
  int                   i = 0, sz, rp;
  PDC_uint64            cnt_sum = 0;
  floatORdouble         bad_pcnt;
  floatORdouble         track_pcnt;
  floatORdouble         cnt_sum_pcnt;
  floatORdouble         elt_pcnt;
  Void_t                *velt;
  fpoint_type ## _dt_elt_t *elt;

  PDC_TRACE(pdc->disc, PDCI_MacroArg2String(fpoint_type) "_acc_report_internal called" );
  if (!prefix || *prefix == 0) {
    prefix = "<top>";
  }
  if (!what) {
    what = fpoint_descr;
  }
  PDCI_nst_prefix_what(outstr, &nst, prefix, what);
  if (a->good == 0) {
    bad_pcnt = (a->bad == 0) ? 0.0 : 100.0;
  } else {
    bad_pcnt = 100.0 * (a->bad / (floatORdouble)(a->good + a->bad));
  }
  sfprintf(outstr, "good vals: %10llu    bad vals: %10llu    pcnt-bad: %8.3lf\n",
	   a->good, a->bad, bad_pcnt);
  if (a->good == 0) {
    return PDC_OK;
  }
  fpoint_type ## _acc_fold_psum(a);
  sz = dtsize(a->dict);
  rp = (sz < PDCI_ACC_REPORT_K) ? sz : PDCI_ACC_REPORT_K;
  dtdisc(a->dict,   &fpoint_type ## _acc_dt_oset_disc, DT_SAMEHASH); /* change cmp function */
  dtmethod(a->dict, Dtoset); /* change to ordered set -- establishes an ordering */
  sfprintf(outstr, "  Characterizing %s:  min %.5lf", what, a->min);
  sfprintf(outstr, " max %.5lf", a->max);
  sfprintf(outstr, " avg %.3lf\n", a->avg);
  sfprintf(outstr, "    => distribution of top %d values out of %d distinct values:\n", rp, sz);
  if (sz == PDCI_ACC_MAX2TRACK && a->good > a->tracked) {
    track_pcnt = 100.0 * (a->tracked/(floatORdouble)a->good);
    sfprintf(outstr, "        (* hit tracking limit, tracked %.3lf pcnt of all values *) \n", track_pcnt);
  }
  for (velt = dtfirst(a->dict); velt && i < PDCI_ACC_REPORT_K; velt = dtnext(a->dict, velt), i++) {
    elt = (fpoint_type ## _dt_elt_t*)velt;
    cnt_sum += elt->key.cnt;
    elt_pcnt = 100.0 * (elt->key.cnt/(floatORdouble)a->good);
    sfprintf(outstr, "        val: %10.5lf", elt->key.val);
    sfprintf(outstr, " count: %10llu  pcnt-of-good-vals: %8.3lf\n", elt->key.cnt, elt_pcnt);

  }
  cnt_sum_pcnt = 100.0 * (cnt_sum/(floatORdouble)a->good);
  sfprintf(outstr,   ". . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .\n");
  sfprintf(outstr,   "        SUMMING         count: %10llu  pcnt-of-good-vals: %8.3lf\n",
	   cnt_sum, cnt_sum_pcnt);
  /* revert to unordered set in case more inserts will occur after this report */
  dtmethod(a->dict, Dtset); /* change to unordered set */
  dtdisc(a->dict,   &fpoint_type ## _acc_dt_set_disc, DT_SAMEHASH); /* change cmp function */
  return PDC_OK;
}

PDC_error_t
fpoint_type ## _acc_report(PDC_t *pdc, const char *prefix, const char *what, int nst,
			   fpoint_type ## _acc *a)
{
  Sfio_t *tmpstr;
  PDC_error_t res;
  PDCI_DISC_INIT_CHECKS( PDCI_MacroArg2String(fpoint_type) "_acc_report" );
  PDCI_NULLPARAM_CHECK( PDCI_MacroArg2String(fpoint_type) "_acc_report" , a );
  if (!pdc->disc->errorf) {
    return PDC_OK;
  }
  if (!(tmpstr = sfstropen ())) { 
    return PDC_ERR;
  }
  res = fpoint_type ## _acc_report_internal(pdc, tmpstr, prefix, what, nst, a);
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

#define PDCI_A2INT(fn_name, rev_fn_name, targ_type, fmt, int_min, int_max)
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

ssize_t
rev_fn_name ## _buf (PDC_t *pdc, PDC_byte *outbuf, size_t outbuf_len, int *outbuf_full, targ_type i)
{
  int  writelen;
  char *buf;

  errno = 0;
  sfstrset(pdc->tmp1, 0);
  writelen = sfprintf(pdc->tmp1, fmt, i);
  if (writelen <= 0) return writelen;
  if (writelen > outbuf_len) {
    if (outbuf_full) { (*outbuf_full) = 1; }
    return -1;
  }
  buf = sfstruse(pdc->tmp1);
  memcpy(outbuf, buf, writelen);
  return writelen;
}

ssize_t
rev_fn_name ## _io(PDC_t *pdc, Sfio_t *io, targ_type i)
{
  errno = 0;
  return sfprintf(io, fmt, i);
}
/* END_MACRO */

#define PDCI_A2UINT(fn_name, rev_fn_name, targ_type, fmt, int_max)
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

ssize_t
rev_fn_name ## _buf (PDC_t *pdc, PDC_byte *outbuf, size_t outbuf_len, int *outbuf_full, targ_type u)
{
  int  writelen;
  char *buf;

  errno = 0;
  sfstrset(pdc->tmp1, 0);
  writelen = sfprintf(pdc->tmp1, fmt, u);
  if (writelen <= 0) return writelen;
  if (writelen > outbuf_len) {
    if (outbuf_full) { (*outbuf_full) = 1; }
    return -1;
  }
  buf = sfstruse(pdc->tmp1);
  memcpy(outbuf, buf, writelen);
  return writelen;
}

ssize_t
rev_fn_name ## _io (PDC_t *pdc, Sfio_t *io, targ_type u)
{
  errno = 0;
  return sfprintf(io, fmt, u);
}
/* END_MACRO */

#define PDCI_E2INT(fn_name, rev_fn_name, targ_type, fmt, int_min, int_max)
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

ssize_t
rev_fn_name ## _buf (PDC_t *pdc, PDC_byte *outbuf, size_t outbuf_len, int *outbuf_full, targ_type i)
{
  int j, writelen;
  char *buf;

  errno = 0;
  sfstrset(pdc->tmp1, 0);
  writelen = sfprintf(pdc->tmp1, fmt, i);
  if (writelen <= 0) return writelen;
  if (writelen > outbuf_len) {
    if (outbuf_full) { (*outbuf_full) = 1; }
    return -1;
  }
  buf = sfstruse(pdc->tmp1);
  for (j = 0; j < writelen; j++) {
    outbuf[j] = PDC_mod_ae_tab[(int)(buf[j])];
  }
  return writelen;
}

ssize_t
rev_fn_name ## _io (PDC_t *pdc, Sfio_t *io, targ_type i)
{
  int j, writelen;
  char *buf;

  errno = 0;
  sfstrset(pdc->tmp1, 0);
  if (-1 == (writelen = sfprintf(pdc->tmp1, fmt, i))) return -1;
  buf = sfstruse(pdc->tmp1);
  for (j = 0; j < writelen; j++) {
    buf[j] = PDC_mod_ae_tab[(int)(buf[j])];
  }
  return sfwrite(io, buf, writelen);
}
/* END_MACRO */

#define PDCI_E2UINT(fn_name, rev_fn_name, targ_type, fmt, int_max)
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

ssize_t
rev_fn_name ## _buf (PDC_t *pdc, PDC_byte *outbuf, size_t outbuf_len, int *outbuf_full, targ_type u)
{
  int j, writelen;
  char *buf;

  errno = 0;
  sfstrset(pdc->tmp1, 0);
  writelen = sfprintf(pdc->tmp1, fmt, u);
  if (writelen <= 0) return writelen;
  if (writelen > outbuf_len) {
    if (outbuf_full) { (*outbuf_full) = 1; }
    return -1;
  }
  buf = sfstruse(pdc->tmp1);
  for (j = 0; j < writelen; j++) {
    outbuf[j] = PDC_mod_ae_tab[(int)(buf[j])];
  }
  return writelen;
}

ssize_t
rev_fn_name ## _io (PDC_t *pdc, Sfio_t *io, targ_type u)
{
  int j, writelen;
  char *buf;

  errno = 0;
  sfstrset(pdc->tmp1, 0);
  if (-1 == (writelen = sfprintf(pdc->tmp1, fmt, u))) return -1;
  buf = sfstruse(pdc->tmp1);
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

ssize_t
rev_fn_name ## _buf (PDC_t *pdc, PDC_byte *outbuf, size_t outbuf_len, int *outbuf_full, targ_type i, PDC_uint32 num_digits)
{
  PDC_int32 n = num_digits;
  PDC_byte  ebc[30];
  targ_type lim;

  if (num_digits > outbuf_len) {
    if (outbuf_full) { (*outbuf_full) = 1; }
    errno = EDOM;
    return -1;
  }
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
  if (i < 0) {
    i = -i;
    while (--n >= 0) {
      ebc[n] = 0xF0 | (i % 10);
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
  memcpy(outbuf, ebc, num_digits);
  return num_digits;
}

ssize_t
rev_fn_name ## _io (PDC_t *pdc, Sfio_t *io, targ_type i, PDC_uint32 num_digits)
{
  PDC_int32 n = num_digits;
  PDC_byte  ebc[30];
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
  if (i < 0) {
    i = -i;
    while (--n >= 0) {
      ebc[n] = 0xF0 | (i % 10);
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

ssize_t
rev_fn_name ## _buf (PDC_t *pdc, PDC_byte *outbuf, size_t outbuf_len, int *outbuf_full, targ_type u, PDC_uint32 num_digits)
{
  PDC_int32 n = num_digits;
  PDC_byte  ebc[30];
  targ_type lim;

  if (num_digits > outbuf_len) {
    if (outbuf_full) { (*outbuf_full) = 1; }
    errno = EDOM;
    return -1;
  }
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
  memcpy(outbuf, ebc, num_digits);
  return num_digits;
}

ssize_t
rev_fn_name ## _io (PDC_t *pdc, Sfio_t *io, targ_type u, PDC_uint32 num_digits)
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

ssize_t
rev_fn_name ## _buf (PDC_t *pdc, PDC_byte *outbuf, size_t outbuf_len, int *outbuf_full, targ_type i, PDC_uint32 num_digits)
{
  PDC_byte  bcd[30];
  PDC_int32 num_bytes;
  int       x, n;
  int       oddbytes = (num_digits % 2 == 1);
  targ_type lim;

  num_bytes = ((num_digits+1) / 2);
  if (num_bytes > outbuf_len) {
    if (outbuf_full) { (*outbuf_full) = 1; }
    errno = EDOM;
    return -1;
  }
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
  n = num_bytes - 1;
  if (i < 0) {
    if (!oddbytes) {  /* must use odd number of digits for negative number */
      errno = EDOM;
      return -1;
    }
    i = -i;
    bcd[n] = ((i%10)<<4) | 0xD; /* force sign nibble to negative */
    n--;
    i /= 10;
    while (n >= 0) {
      x = i % 100;
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
  memcpy(outbuf, bcd, num_bytes);
  return num_bytes;
}

ssize_t
rev_fn_name ## _io (PDC_t *pdc, Sfio_t *io, targ_type i, PDC_uint32 num_digits)
{
  PDC_byte  bcd[30];
  PDC_int32 num_bytes;
  int       x, n;
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
  if (i < 0) {
    if (!oddbytes) {  /* must use odd number of digits for negative number */
      errno = EDOM;
      return -1;
    }
    i = -i;
    bcd[n] = ((i%10)<<4) | 0xD; /* force sign nibble to negative */
    n--;
    i /= 10;
    while (n >= 0) {
      x = i % 100;
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

ssize_t
rev_fn_name ## _buf (PDC_t *pdc, PDC_byte *outbuf, size_t outbuf_len, int *outbuf_full, targ_type u, PDC_uint32 num_digits)
{
  PDC_byte  bcd[30];
  PDC_int32 num_bytes;
  int       x, n;
  targ_type lim;

  num_bytes = ((num_digits+1) / 2);
  if (num_bytes > outbuf_len) {
    if (outbuf_full) { (*outbuf_full) = 1; }
    errno = EDOM;
    return -1;
  }
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
  memcpy(outbuf, bcd, num_bytes);
  return num_bytes;
}

ssize_t
rev_fn_name ## _io (PDC_t *pdc, Sfio_t *io, targ_type u, PDC_uint32 num_digits)
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

ssize_t
rev_fn_name ## _buf (PDC_t *pdc, PDC_byte *outbuf, size_t outbuf_len, int *outbuf_full, targ_type i, PDC_uint32 num_bytes)
{
  PDC_int32 n = num_bytes;
  PDC_byte *ibytes = (PDC_byte*)(&i);

  if (num_bytes > outbuf_len) {
    if (outbuf_full) { (*outbuf_full) = 1; }
    errno = EDOM;
    return -1;
  }
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
    memcpy(outbuf, ibytes, num_bytes);
  } else {
    /* must reverse the order */
    while (--n >= 0) {
      outbuf[n] = *ibytes++;
    }
  }
  errno = 0;
  return num_bytes;
}

ssize_t
rev_fn_name ## _io (PDC_t *pdc, Sfio_t *io, targ_type i, PDC_uint32 num_bytes)
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

ssize_t
rev_fn_name ## _buf (PDC_t *pdc, PDC_byte *outbuf, size_t outbuf_len, int *outbuf_full, targ_type u, PDC_uint32 num_bytes)
{
  PDC_int32 n = num_bytes;
  PDC_byte *ubytes = (PDC_byte*)(&u);

  if (num_bytes > outbuf_len) {
    if (outbuf_full) { (*outbuf_full) = 1; }
    errno = EDOM;
    return -1;
  }
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
    memcpy(outbuf, ubytes, num_bytes);
  } else {
    /* must reverse the order */
    while (--n >= 0) {
      outbuf[n] = *ubytes++;
    }
  }
  errno = 0;
  return num_bytes;
}

ssize_t
rev_fn_name ## _io (PDC_t *pdc, Sfio_t *io, targ_type u, PDC_uint32 num_bytes)
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
 * PDCI_AE_INT_READ_FN(fn_pref, targ_type, bytes2num_fn, invalid_err, isspace_fn, isdigit_fn)
 */

PDCI_AE_INT_READ_FN(PDC_a_int8,   PDC_int8,   PDCI_a2int8,   PDC_INVALID_A_NUM, PDCI_is_a_space, PDCI_is_a_digit);
PDCI_AE_INT_READ_FN(PDC_a_int16,  PDC_int16,  PDCI_a2int16,  PDC_INVALID_A_NUM, PDCI_is_a_space, PDCI_is_a_digit);
PDCI_AE_INT_READ_FN(PDC_a_int32,  PDC_int32,  PDCI_a2int32,  PDC_INVALID_A_NUM, PDCI_is_a_space, PDCI_is_a_digit);
PDCI_AE_INT_READ_FN(PDC_a_int64,  PDC_int64,  PDCI_a2int64,  PDC_INVALID_A_NUM, PDCI_is_a_space, PDCI_is_a_digit);
PDCI_AE_INT_READ_FN(PDC_a_uint8,  PDC_uint8,  PDCI_a2uint8,  PDC_INVALID_A_NUM, PDCI_is_a_space, PDCI_is_a_digit);
PDCI_AE_INT_READ_FN(PDC_a_uint16, PDC_uint16, PDCI_a2uint16, PDC_INVALID_A_NUM, PDCI_is_a_space, PDCI_is_a_digit);
PDCI_AE_INT_READ_FN(PDC_a_uint32, PDC_uint32, PDCI_a2uint32, PDC_INVALID_A_NUM, PDCI_is_a_space, PDCI_is_a_digit);
PDCI_AE_INT_READ_FN(PDC_a_uint64, PDC_uint64, PDCI_a2uint64, PDC_INVALID_A_NUM, PDCI_is_a_space, PDCI_is_a_digit);

/* ================================================================================ */
/* FIXED-WIDTH ASCII INTEGER READ FUNCTIONS */

/*
 * PDCI_AE_INT_FW_READ_FN(fn_name, targ_type, bytes2num_fn, invalid_err, isspace_fn)
 */

PDCI_AE_INT_FW_READ_FN(PDC_a_int8_FW_read,   PDC_int8,   PDCI_a2int8,   PDC_INVALID_A_NUM, PDCI_is_a_space);
PDCI_AE_INT_FW_READ_FN(PDC_a_int16_FW_read,  PDC_int16,  PDCI_a2int16,  PDC_INVALID_A_NUM, PDCI_is_a_space);
PDCI_AE_INT_FW_READ_FN(PDC_a_int32_FW_read,  PDC_int32,  PDCI_a2int32,  PDC_INVALID_A_NUM, PDCI_is_a_space);
PDCI_AE_INT_FW_READ_FN(PDC_a_int64_FW_read,  PDC_int64,  PDCI_a2int64,  PDC_INVALID_A_NUM, PDCI_is_a_space);
PDCI_AE_INT_FW_READ_FN(PDC_a_uint8_FW_read,  PDC_uint8,  PDCI_a2uint8,  PDC_INVALID_A_NUM, PDCI_is_a_space);
PDCI_AE_INT_FW_READ_FN(PDC_a_uint16_FW_read, PDC_uint16, PDCI_a2uint16, PDC_INVALID_A_NUM, PDCI_is_a_space);
PDCI_AE_INT_FW_READ_FN(PDC_a_uint32_FW_read, PDC_uint32, PDCI_a2uint32, PDC_INVALID_A_NUM, PDCI_is_a_space);
PDCI_AE_INT_FW_READ_FN(PDC_a_uint64_FW_read, PDC_uint64, PDCI_a2uint64, PDC_INVALID_A_NUM, PDCI_is_a_space);

/* ================================================================================ */
/* BINARY INTEGER READ FUNCTIONS */

/*
 * PDCI_B1_INT_READ_FN(fn_name, targ_type)
 *   read 1 byte
 */

PDCI_B1_INT_READ_FN(PDC_b_int8_read,   PDC_int8  );
PDCI_B1_INT_READ_FN(PDC_b_uint8_read,  PDC_uint8 );

/*
 * PDCI_B_INT_READ_FN(fn_name, targ_type, width, swapmem_op)
 *   read width bytes
 *
 * swapmem ops:
 *    0 -> straight copy
 *    1 -> reverse each byte in each string of 2 bytes
 *    3 -> reverse each byte in each string of 4 bytes
 *    4 -> swap upper/lower 4 bytes in each 8 byte value
 *    7 -> reverse each byte in each string of 8 bytes
 */

PDCI_B_INT_READ_FN(PDC_b_int16_read,  PDC_int16,  2, 1);
PDCI_B_INT_READ_FN(PDC_b_uint16_read, PDC_uint16, 2, 1);
PDCI_B_INT_READ_FN(PDC_b_int32_read,  PDC_int32,  4, 3);
PDCI_B_INT_READ_FN(PDC_b_uint32_read, PDC_uint32, 4, 3);
PDCI_B_INT_READ_FN(PDC_b_int64_read,  PDC_int64,  8, 7);
PDCI_B_INT_READ_FN(PDC_b_uint64_read, PDC_uint64, 8, 7);

/* ********************************* BEGIN_TRAILER ******************************** */

/*
 * XXX dummy going away eventually
 */
PDC_error_t
PDC_dummy_read(PDC_t *pdc, const PDC_base_csm *csm, PDC_int32 dummy_val, PDC_base_ed *ed, PDC_int32 *res_out)
{
  PDCI_DISC_INIT_CHECKS("PDC_dummy_read");
  PDCI_NULLPARAM_CHECK("PDC_dummy_read", res_out);
  (*res_out) = dummy_val;
  ed->errCode = PDC_NO_ERR;
  return PDC_OK;
}

PDC_error_t
PDC_dummy_read_internal(PDC_t *pdc, const PDC_base_csm *csm, PDC_int32 dummy_val, PDC_base_ed *ed, PDC_int32 *res_out)
{
  (*res_out) = dummy_val;
  ed->errCode = PDC_NO_ERR;
  return PDC_OK;
}


/* ********************************** END_MACGEN ********************************** */
/* ******************* BEGIN_MACGEN(libpadsc-cobol-read-gen.c) ******************** */
/*
 * Generated COBOL read functions
 * 
 * Kathleen Fisher, Robert Gruber
 * AT&T Labs Research
 */

#gen_include "libpadsc-internal.h"
#gen_include "libpadsc-macros-gen.h"

/* ********************************** END_HEADER ********************************** */
#gen_include "libpadsc-read-macros-gen.h"

/* ================================================================================ */
/* VARIABLE-WIDTH EBCDIC CHAR ENCODING INTEGER READ FUNCTIONS */

/*
 * PDCI_AE_INT_READ_FN(fn_pref, targ_type, bytes2num_fn, invalid_err, isspace_fn, isdigit_fn)
 */

PDCI_AE_INT_READ_FN(PDC_e_int8,   PDC_int8,   PDCI_e2int8,   PDC_INVALID_E_NUM, PDCI_is_e_space, PDCI_is_e_digit);
PDCI_AE_INT_READ_FN(PDC_e_int16,  PDC_int16,  PDCI_e2int16,  PDC_INVALID_E_NUM, PDCI_is_e_space, PDCI_is_e_digit);
PDCI_AE_INT_READ_FN(PDC_e_int32,  PDC_int32,  PDCI_e2int32,  PDC_INVALID_E_NUM, PDCI_is_e_space, PDCI_is_e_digit);
PDCI_AE_INT_READ_FN(PDC_e_int64,  PDC_int64,  PDCI_e2int64,  PDC_INVALID_E_NUM, PDCI_is_e_space, PDCI_is_e_digit);
PDCI_AE_INT_READ_FN(PDC_e_uint8,  PDC_uint8,  PDCI_e2uint8,  PDC_INVALID_E_NUM, PDCI_is_e_space, PDCI_is_e_digit);
PDCI_AE_INT_READ_FN(PDC_e_uint16, PDC_uint16, PDCI_e2uint16, PDC_INVALID_E_NUM, PDCI_is_e_space, PDCI_is_e_digit);
PDCI_AE_INT_READ_FN(PDC_e_uint32, PDC_uint32, PDCI_e2uint32, PDC_INVALID_E_NUM, PDCI_is_e_space, PDCI_is_e_digit);
PDCI_AE_INT_READ_FN(PDC_e_uint64, PDC_uint64, PDCI_e2uint64, PDC_INVALID_E_NUM, PDCI_is_e_space, PDCI_is_e_digit);

/* ================================================================================ */
/* FIXED-WIDTH EBCDIC CHAR ENCODING INTEGER READ FUNCTIONS */

/*
 * PDCI_AE_INT_FW_READ_FN(fn_name, targ_type, bytes2num_fn, invalid_err, isspace_fn)
 */

PDCI_AE_INT_FW_READ_FN(PDC_e_int8_FW_read,   PDC_int8,   PDCI_e2int8,   PDC_INVALID_E_NUM, PDCI_is_e_space);
PDCI_AE_INT_FW_READ_FN(PDC_e_int16_FW_read,  PDC_int16,  PDCI_e2int16,  PDC_INVALID_E_NUM, PDCI_is_e_space);
PDCI_AE_INT_FW_READ_FN(PDC_e_int32_FW_read,  PDC_int32,  PDCI_e2int32,  PDC_INVALID_E_NUM, PDCI_is_e_space);
PDCI_AE_INT_FW_READ_FN(PDC_e_int64_FW_read,  PDC_int64,  PDCI_e2int64,  PDC_INVALID_E_NUM, PDCI_is_e_space);
PDCI_AE_INT_FW_READ_FN(PDC_e_uint8_FW_read,  PDC_uint8,  PDCI_e2uint8,  PDC_INVALID_E_NUM, PDCI_is_e_space);
PDCI_AE_INT_FW_READ_FN(PDC_e_uint16_FW_read, PDC_uint16, PDCI_e2uint16, PDC_INVALID_E_NUM, PDCI_is_e_space);
PDCI_AE_INT_FW_READ_FN(PDC_e_uint32_FW_read, PDC_uint32, PDCI_e2uint32, PDC_INVALID_E_NUM, PDCI_is_e_space);
PDCI_AE_INT_FW_READ_FN(PDC_e_uint64_FW_read, PDC_uint64, PDCI_e2uint64, PDC_INVALID_E_NUM, PDCI_is_e_space);

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
/* ********************* BEGIN_MACGEN(libpadsc-write-gen.c) *********************** */
/*
 * Generated write functions
 * 
 * Kathleen Fisher, Robert Gruber
 * AT&T Labs Research
 */

#gen_include "libpadsc-internal.h"
#gen_include "libpadsc-macros-gen.h"

/* ********************************** END_HEADER ********************************** */
#gen_include "libpadsc-write-macros-gen.h"

/* ================================================================================ */
/* VARIABLE-WIDTH ASCII INTEGER WRITE FUNCTIONS */

/*
 * PDCI_A_INT_WRITE_FN(fn_pref, targ_type, fmt, inv_type, inv_val)
 */

PDCI_A_INT_WRITE_FN(PDC_a_int8,   PDC_int8,   "%I1d",   "PDC_int8",   PDC_INT8_DEF_INV_VAL);
PDCI_A_INT_WRITE_FN(PDC_a_int16,  PDC_int16,  "%I2d",   "PDC_int16",  PDC_INT16_DEF_INV_VAL);
PDCI_A_INT_WRITE_FN(PDC_a_int32,  PDC_int32,  "%I4d",   "PDC_int32",  PDC_INT32_DEF_INV_VAL);
PDCI_A_INT_WRITE_FN(PDC_a_int64,  PDC_int64,  "%I8d",   "PDC_int64",  PDC_INT64_DEF_INV_VAL);
PDCI_A_INT_WRITE_FN(PDC_a_uint8,  PDC_uint8,  "%I1u",   "PDC_uint8",  PDC_UINT8_DEF_INV_VAL);
PDCI_A_INT_WRITE_FN(PDC_a_uint16, PDC_uint16, "%I2u",   "PDC_uint16", PDC_UINT16_DEF_INV_VAL);
PDCI_A_INT_WRITE_FN(PDC_a_uint32, PDC_uint32, "%I4u",   "PDC_uint32", PDC_UINT32_DEF_INV_VAL);
PDCI_A_INT_WRITE_FN(PDC_a_uint64, PDC_uint64, "%I8u",   "PDC_uint64", PDC_UINT64_DEF_INV_VAL);

/* ********************************* BEGIN_TRAILER ******************************** */
/* ********************************** END_MACGEN ********************************** */
/* ****************** BEGIN_MACGEN(libpadsc-cobol-write-gen.c) ******************** */
/*
 * Generated COBOL write functions
 * 
 * Kathleen Fisher, Robert Gruber
 * AT&T Labs Research
 */

#gen_include "libpadsc-internal.h"
#gen_include "libpadsc-macros-gen.h"

/* ********************************** END_HEADER ********************************** */
#gen_include "libpadsc-write-macros-gen.h"

/* ================================================================================ */
/* VARIABLE-WIDTH EBCDIC CHAR ENCODING INTEGER WRITE FUNCTIONS */

/*
 * PDCI_E_INT_WRITE_FN(fn_pref, targ_type, num2pre, inv_type, inv_val)
 */

PDCI_E_INT_WRITE_FN(PDC_e_int8,   PDC_int8,   PDCI_int8_2e,   "PDC_int8",   PDC_INT8_DEF_INV_VAL);
PDCI_E_INT_WRITE_FN(PDC_e_int16,  PDC_int16,  PDCI_int16_2e,  "PDC_int16",  PDC_INT16_DEF_INV_VAL);
PDCI_E_INT_WRITE_FN(PDC_e_int32,  PDC_int32,  PDCI_int32_2e,  "PDC_int32",  PDC_INT32_DEF_INV_VAL);
PDCI_E_INT_WRITE_FN(PDC_e_int64,  PDC_int64,  PDCI_int64_2e,  "PDC_int64",  PDC_INT64_DEF_INV_VAL);
PDCI_E_INT_WRITE_FN(PDC_e_uint8,  PDC_uint8,  PDCI_uint8_2e,  "PDC_uint8",  PDC_UINT8_DEF_INV_VAL);
PDCI_E_INT_WRITE_FN(PDC_e_uint16, PDC_uint16, PDCI_uint16_2e, "PDC_uint16", PDC_UINT16_DEF_INV_VAL);
PDCI_E_INT_WRITE_FN(PDC_e_uint32, PDC_uint32, PDCI_uint32_2e, "PDC_uint32", PDC_UINT32_DEF_INV_VAL);
PDCI_E_INT_WRITE_FN(PDC_e_uint64, PDC_uint64, PDCI_uint64_2e, "PDC_uint64", PDC_UINT64_DEF_INV_VAL);

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

/* PDCI_FPOINT_ACCUM(fpoint_type, fpoint_descr, floatORdouble, fpoint2floatORdouble) */

PDCI_FPOINT_ACCUM(PDC_fpoint8,   "fpoint8",   float,  PDC_FPOINT2FLT);
PDCI_FPOINT_ACCUM(PDC_ufpoint8,  "ufpoint8",  float,  PDC_FPOINT2FLT);
PDCI_FPOINT_ACCUM(PDC_fpoint16,  "fpoint16",  float,  PDC_FPOINT2FLT);
PDCI_FPOINT_ACCUM(PDC_ufpoint16, "ufpoint16", float,  PDC_FPOINT2FLT);
PDCI_FPOINT_ACCUM(PDC_fpoint32,  "fpoint32",  float,  PDC_FPOINT2FLT);
PDCI_FPOINT_ACCUM(PDC_ufpoint32, "ufpoint32", float,  PDC_FPOINT2FLT);
PDCI_FPOINT_ACCUM(PDC_fpoint64,  "fpoint64",  double, PDC_FPOINT2DBL);
PDCI_FPOINT_ACCUM(PDC_ufpoint64, "ufpoint64", double, PDC_FPOINT2DBL);

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
  DTOFFSET(PDCI_string_dt_elt_t, key),     /* key     */
  0,				           /* size    */
  DTOFFSET(PDCI_string_dt_elt_t, link),    /* link    */
  (Dtmake_f)PDCI_string_dt_elt_make,       /* makef   */
  (Dtfree_f)PDCI_string_dt_elt_free,       /* freef */
  (Dtcompar_f)PDCI_string_dt_elt_set_cmp,  /* comparf */
  (Dthash_f)PDCI_string_dt_elt_hash,       /* hashf   */
  NiL,				           /* memoryf */
  NiL				           /* eventf  */
};

static Dtdisc_t PDCI_string_acc_dt_oset_disc = {
  DTOFFSET(PDCI_string_dt_elt_t, key),     /* key     */
  0,				           /* size    */
  DTOFFSET(PDCI_string_dt_elt_t, link),    /* link    */
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
PDC_string_acc_add(PDC_t *pdc, PDC_string_acc *a, const PDC_base_ed *ed, const PDC_string *val)
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
      PDC_WARN(pdc->disc, "** PADSC internal error: dtinsert failed (out of memory?) **");
      return PDC_ERR;
    }
    (tmp1->key.cnt)++;
    (a->tracked)++;
  } else {
    lookup_key.str = val->str;
    lookup_key.len = val->len;
    lookup_key.cnt = 0;
    if ((tmp1 = dtmatch(a->dict, &lookup_key))) {
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
PDC_char_acc_add(PDC_t *pdc, PDC_char_acc *a, const PDC_base_ed *ed, const PDC_uint8 *val)
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
  sfprintf(outstr, " max %s", PDC_qfmt_char(a->max));
  sfprintf(outstr, " (based on ASCII encoding)\n");

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

PDC_error_t
PDC_nerr_acc_report_internal(PDC_t *pdc, Sfio_t *outstr, const char *prefix, const char *what, int nst,
			     PDC_int32_acc *a)
{
  int                   i = 0, sz, rp;
  PDC_uint64            cnt_sum = 0;
  double                track_pcnt;
  double                cnt_sum_pcnt;
  double                elt_pcnt;
  Void_t                *velt;
  PDC_int32_dt_elt_t *elt;

  PDC_TRACE(pdc->disc, "PDC_nerr_acc_report_internal called");
  if (!prefix || *prefix == 0) {
    prefix = "<top>";
  }
  if (!what) {
    what = "nerr";
  }
  PDCI_nst_prefix_what(outstr, &nst, prefix, what);
  sfprintf(outstr, "total vals: %10llu\n", a->good);
  if (a->bad) {
    PDC_WARN(pdc->disc, "** UNEXPECTED: PDC_nerr_acc_report called with bad values (all nerr are valid).  Ignoring bad.");
  }
  if (a->good == 0) {
    return PDC_OK;
  }
  PDC_int32_acc_fold_psum(a);
  sz = dtsize(a->dict);
  rp = (sz < PDCI_ACC_REPORT_K) ? sz : PDCI_ACC_REPORT_K;
  dtdisc(a->dict,   &PDC_int32_acc_dt_oset_disc, DT_SAMEHASH); /* change cmp function */
  dtmethod(a->dict, Dtoset); /* change to ordered set -- establishes an ordering */
  sfprintf(outstr, "  Characterizing %s:  min %ld", what, a->min);
  sfprintf(outstr, " max %ld", a->max);
  sfprintf(outstr, " avg %.3lf\n", a->avg);
  sfprintf(outstr, "    => distribution of top %d values out of %d distinct values:\n", rp, sz);
  if (sz == PDCI_ACC_MAX2TRACK && a->good > a->tracked) {
    track_pcnt = 100.0 * (a->tracked/(double)a->good);
    sfprintf(outstr, "        (* hit tracking limit, tracked %.3lf pcnt of all values *) \n", track_pcnt);
  }
  for (velt = dtfirst(a->dict); velt && i < PDCI_ACC_REPORT_K; velt = dtnext(a->dict, velt), i++) {
    elt = (PDC_int32_dt_elt_t*)velt;
    cnt_sum += elt->key.cnt;
    elt_pcnt = 100.0 * (elt->key.cnt/(double)a->good);
    sfprintf(outstr, "        val: %10ld", elt->key.val);
    sfprintf(outstr, " count: %10llu pcnt-of-total-vals: %8.3lf\n", elt->key.cnt, elt_pcnt);
  }
  cnt_sum_pcnt = 100.0 * (cnt_sum/(double)a->good);
  sfprintf(outstr,   ". . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .\n");
  sfprintf(outstr,   "        SUMMING         count: %10llu pcnt-of-total-vals: %8.3lf\n",
	   cnt_sum, cnt_sum_pcnt);
  /* revert to unordered set in case more inserts will occur after this report */
  dtmethod(a->dict, Dtset); /* change to unordered set */
  dtdisc(a->dict,   &PDC_int32_acc_dt_set_disc, DT_SAMEHASH); /* change cmp function */
  return PDC_OK;
}

PDC_error_t
PDC_nerr_acc_report(PDC_t *pdc, const char *prefix, const char *what, int nst,
		    PDC_int32_acc *a)
{
  Sfio_t *tmpstr;
  PDC_error_t res;
  PDCI_DISC_INIT_CHECKS("PDC_nerr_acc_report");
  PDCI_NULLPARAM_CHECK("PDC_nerr_acc_report", a);

  if (!pdc->disc->errorf) {
    return PDC_OK;
  }
  if (!(tmpstr = sfstropen ())) { 
    return PDC_ERR;
  }
  res = PDC_nerr_acc_report_internal(pdc, tmpstr, prefix, what, nst, a);
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

/* PDCI_A2INT(fn_name, rev_fn_name, targ_type, fmt, int_min, int_max) */
PDCI_A2INT(PDCI_a2int8,  PDCI_int8_2a,  PDC_int8,  "%I1d", PDC_MIN_INT8,  PDC_MAX_INT8)
PDCI_A2INT(PDCI_a2int16, PDCI_int16_2a, PDC_int16, "%I2d", PDC_MIN_INT16, PDC_MAX_INT16)
PDCI_A2INT(PDCI_a2int32, PDCI_int32_2a, PDC_int32, "%I4d", PDC_MIN_INT32, PDC_MAX_INT32)
PDCI_A2INT(PDCI_a2int64, PDCI_int64_2a, PDC_int64, "%I8d", PDC_MIN_INT64, PDC_MAX_INT64)

/* PDCI_A2UINT(fn_name, rev_fn_name, targ_type, fmt, int_max) */
PDCI_A2UINT(PDCI_a2uint8,  PDCI_uint8_2a,  PDC_uint8,  "%I1u", PDC_MAX_UINT8)
PDCI_A2UINT(PDCI_a2uint16, PDCI_uint16_2a, PDC_uint16, "%I2u", PDC_MAX_UINT16)
PDCI_A2UINT(PDCI_a2uint32, PDCI_uint32_2a, PDC_uint32, "%I4u", PDC_MAX_UINT32)
PDCI_A2UINT(PDCI_a2uint64, PDCI_uint64_2a, PDC_uint64, "%I8u", PDC_MAX_UINT64)

/* PDCI_E2INT(fn_name, rev_fn_name, targ_type, fmt, int_min, int_max) */
PDCI_E2INT(PDCI_e2int8,  PDCI_int8_2e,  PDC_int8,  "%I1d", PDC_MIN_INT8,  PDC_MAX_INT8)
PDCI_E2INT(PDCI_e2int16, PDCI_int16_2e, PDC_int16, "%I2d", PDC_MIN_INT16, PDC_MAX_INT16)
PDCI_E2INT(PDCI_e2int32, PDCI_int32_2e, PDC_int32, "%I4d", PDC_MIN_INT32, PDC_MAX_INT32)
PDCI_E2INT(PDCI_e2int64, PDCI_int64_2e, PDC_int64, "%I8d", PDC_MIN_INT64, PDC_MAX_INT64)

/* PDCI_E2UINT(fn_name, rev_fn_name, targ_type, fmt, int_max) */
PDCI_E2UINT(PDCI_e2uint8,  PDCI_uint8_2e,  PDC_uint8,  "%I1u", PDC_MAX_UINT8)
PDCI_E2UINT(PDCI_e2uint16, PDCI_uint16_2e, PDC_uint16, "%I2u", PDC_MAX_UINT16)
PDCI_E2UINT(PDCI_e2uint32, PDCI_uint32_2e, PDC_uint32, "%I4u", PDC_MAX_UINT32)
PDCI_E2UINT(PDCI_e2uint64, PDCI_uint64_2e, PDC_uint64, "%I8u", PDC_MAX_UINT64)

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
/* ******************* BEGIN_MACGEN(libpadsc-cobol-only-gen.c) ******************** */
/*
 * Generated stub functions for COBOL-only version of library
 * 
 * Kathleen Fisher, Robert Gruber
 * AT&T Labs Research
 */

#gen_include "libpadsc-internal.h"
#gen_include "libpadsc-macros-gen.h"
/* ================================================================================ */

/* ********************************** END_HEADER ********************************** */
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

static const char id[] = "\n@(#)$Id: pads.c,v 1.83 2003-05-29 04:49:53 gruber Exp $\0\n";

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

#if 0 
/* HUME version */
int PDCI_bcd_hilo_digits[256] = {
  /* 0x0? */  0,  1,  2,  3,  4,  5,  6,  7,  8,  9,  0,  0,  0,  0,  0,  0,
  /* 0x1? */ 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 10, 10, 10, 10, 10, 10,
  /* 0x2? */ 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 20, 20, 20, 20, 20, 20,
  /* 0x3? */ 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 30, 30, 30, 30, 30, 30,
  /* 0x4? */ 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 40, 40, 40, 40, 40, 40,
  /* 0x5? */ 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 50, 50, 50, 50, 50, 50,
  /* 0x6? */ 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 60, 60, 60, 60, 60, 60,
  /* 0x7? */ 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 70, 70, 70, 70, 70, 70,
  /* 0x8? */ 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 80, 80, 80, 80, 80, 80,
  /* 0x9? */ 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 90, 90, 90, 90, 90, 90,
  /* 0xA? */  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
  /* 0xB? */  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
  /* 0xC? */  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
  /* 0xD? */  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
  /* 0xE? */  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
  /* 0xF? */  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
};
#endif

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
/* HUME version */
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
  /* 0xA? */  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
  /* 0xB? */  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
  /* 0xC? */  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
  /* 0xD? */  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
  /* 0xE? */  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
  /* 0xF? */  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
};
#endif

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

/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 * EXTERNAL FUNCTIONS (see libpadsc.h)
 * ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 */

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
  PDC_charset_ASCII,
  0, /* string read functions do not copy strings */
  0, /* stop_regexp disabled    */
  0, /* no stop_maxlen (disabled) */
  PDC_errorf,
  PDC_errorRep_Max,
  PDC_littleEndian,
  0, /* by default, no inv_valfn map */
  0  /* a default IO discipline is installed on PDC_open */
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
  /* allocate a 1 MB + 1 byte buffer to use with sfio */
  if (!(pdc->sfbuf = vmoldof(vm, 0, PDC_byte, 1024 * 1024, 1))) {
    goto fatal_alloc_err;
  }
  pdc->outbuf_len = 1024 * 64;
  pdc->outbuf_res = 1024 * 32;
  if (!(pdc->outbuf = vmoldof(vm, 0, PDC_byte, pdc->outbuf_len, 1))) {
    goto fatal_alloc_err;
  }
  pdc->inestlev = 0;
  if (!(pdc->tmp1 = sfstropen())) {
    goto fatal_alloc_err;
  }
  if (!(pdc->tmp2 = sfstropen())) {
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
    if (pdc->tmp1) {
      sfstrclose(pdc->tmp1);
    }
    if (pdc->tmp2) {
      sfstrclose(pdc->tmp2);
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
  if (pdc->tmp1) {
    sfstrclose(pdc->tmp1);
  }
  if (pdc->tmp2) {
    sfstrclose(pdc->tmp2);
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
/* EXTERNAL inv_val FUNCTIONS */

/* Type PDC_inv_valfn_map_t: */
struct PDC_inv_valfn_map_s {
  Dt_t *dt;
};

typedef struct PDCI_inv_valfn_elt_s {
  Dtlink_t        link;
  const char     *key;
  PDC_inv_valfn   val;
} PDCI_inv_valfn_elt_t;

void*
PDCI_inv_valfn_elt_make(Dt_t *dt, PDCI_inv_valfn_elt_t *a, Dtdisc_t *disc)
{
  PDCI_inv_valfn_elt_t *b;
  if ((b = oldof(0, PDCI_inv_valfn_elt_t, 1, 0))) {
    b->key  = a->key;
    b->val  = a->val;
  }
  return b;
}

void
PDCI_inv_valfn_elt_free(Dt_t *dt, PDCI_inv_valfn_elt_t *a, Dtdisc_t *disc)
{
  free(a);
}

static Dtdisc_t PDCI_inv_valfn_map_disc = {
  DTOFFSET(PDCI_inv_valfn_elt_t, key),      /* key     */
  -1,                                       /* size    */
  DTOFFSET(PDCI_inv_valfn_elt_t, link),     /* link    */
  (Dtmake_f)PDCI_inv_valfn_elt_make,        /* makef   */
  (Dtfree_f)PDCI_inv_valfn_elt_free,        /* freef   */
  NiL,                                      /* comparf */
  NiL,                                      /* hashf   */
  NiL,                                      /* memoryf */
  NiL                                       /* eventf  */
};

PDC_inv_valfn_map_t*
PDCI_inv_valfn_map_create(PDC_t *pdc, int safe)
{
  PDC_inv_valfn_map_t *map; 

  if (safe) {
    PDCI_DISC_INIT_CHECKS_RET_0("PDC_inv_valfn_map_create");
  }
  if (!pdc->vm) {
    PDC_WARN(pdc->disc, "PDC_inv_valfn_map_create: pdc handle not initialized properly");
    return 0;
  }
  if (!(map = vmnewof(pdc->vm, 0, PDC_inv_valfn_map_t, 1, 0))) {
    goto alloc_err;
  }
  if (!(map->dt = dtopen(&PDCI_inv_valfn_map_disc, Dtset))) {
    vmfree(pdc->vm, map);
    goto alloc_err;
  }
  return map;

 alloc_err:
  PDCI_report_err(pdc, PDC_FATAL_FLAGS, 0, PDC_ALLOC_ERR, "PDC_inv_valfn_map_create", "Memory alloc error");
  return 0;
}

PDC_error_t
PDCI_inv_valfn_map_destroy(PDC_t *pdc, PDC_inv_valfn_map_t *map, int safe)
{
  if (safe) {
    PDCI_DISC_INIT_CHECKS("PDC_inv_valfn_map_destroy");
    PDCI_NULLPARAM_CHECK("PDC_inv_valfn_map_destroy", map);
  }
  if (map->dt) {
    dtclose(map->dt);
    map->dt = 0;
  }
  if (pdc->vm) {
    vmfree(pdc->vm, map);
  }
  return PDC_OK;
}

PDC_error_t
PDCI_inv_valfn_map_clear(PDC_t *pdc, PDC_inv_valfn_map_t *map, int safe)
{
  if (safe) {
    PDCI_DISC_INIT_CHECKS("PDC_inv_valfn_map_clear");
    PDCI_NULLPARAM_CHECK("PDC_inv_valfn_map_destroy", map);
  }
  if (map->dt) {
    dtclear(map->dt);
    return PDC_OK;
  }
  return PDC_ERR;
}

PDC_inv_valfn
PDCI_get_inv_valfn(PDC_t* pdc, PDC_inv_valfn_map_t *map, const char *type_name, int safe)
{
  PDCI_inv_valfn_elt_t *tmp;

  if (safe) {
    PDCI_DISC_INIT_CHECKS_RET_0("PDC_get_inv_valfn");
    PDCI_NULLPARAM_CHECK_RET_0("PDC_get_inv_valfn", map);
    PDCI_NULLPARAM_CHECK_RET_0("PDC_get_inv_valfn", type_name);
    if (!map->dt) {
      PDC_WARN(pdc->disc, "PDC_get_inv_valfn: map not initialized properly");
      return 0;
    }
  }
  if ((tmp = dtmatch(map->dt, type_name))) {
    return tmp->val;
  }
  return 0;
}
 
PDC_inv_valfn
PDCI_set_inv_valfn(PDC_t* pdc, PDC_inv_valfn_map_t *map, const char *type_name, PDC_inv_valfn fn, int safe)
{
  PDC_inv_valfn          res = 0;
  PDCI_inv_valfn_elt_t  *tmp;
  PDCI_inv_valfn_elt_t   insert_elt;

  if (safe) {
    PDCI_DISC_INIT_CHECKS_RET_0("PDC_set_inv_valfn");
    PDCI_NULLPARAM_CHECK_RET_0("PDC_set_inv_valfn", map);
    PDCI_NULLPARAM_CHECK_RET_0("PDC_set_inv_valfn", type_name);
    if (!map->dt) {
      PDC_WARN(pdc->disc, "PDC_set_inv_valfn: map not initialized properly");
      return 0;
    }
  }
  if ((tmp = dtmatch(map->dt, type_name))) {
    res = tmp->val;
    tmp->val = fn;
    return res;
  }
  if (fn) {
    insert_elt.key = type_name;
    insert_elt.val = fn;
    if (!(tmp = dtinsert(map->dt, &insert_elt))) {
      PDC_WARN(pdc->disc, "** PADSC internal error: dtinsert failed (out of memory?) **");
    }
  }
  return 0;
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
PDC_IO_fopen(PDC_t *pdc, const char *path)
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
  PDCI_DISC_INIT_CHECKS_RET_0("PDC_IO_at_EOR");
  return PDC_IO_at_EOR_internal(pdc);
}

int
PDC_IO_at_EOF(PDC_t *pdc) {
  PDCI_DISC_INIT_CHECKS_RET_0("PDC_IO_at_EOF");
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

PDC_byte*
PDC_IO_write_start(PDC_t *pdc, Sfio_t *io, size_t *buf_len, int *set_buf)
{
  PDCI_DISC_INIT_CHECKS_RET_0("PDC_IO_write_start");
  PDCI_NULLPARAM_CHECK_RET_0("PDC_IO_write_start", io);
  PDCI_NULLPARAM_CHECK_RET_0("PDC_IO_write_start", buf_len);
  PDCI_NULLPARAM_CHECK_RET_0("PDC_IO_write_start", set_buf);
  return PDCI_IO_write_start(pdc, io, buf_len, set_buf, "PDC_IO_write_start");
}

ssize_t
PDC_IO_write_commit(PDC_t *pdc, Sfio_t *io, PDC_byte *buf, int set_buf, size_t num_bytes)
{
  PDCI_DISC_INIT_CHECKS_RET_SSIZE("PDC_IO_write_commit");
  PDCI_NULLPARAM_CHECK_RET_SSIZE("PDC_IO_write_commit", io);
  PDCI_NULLPARAM_CHECK_RET_SSIZE("PDC_IO_write_commit", buf);
  return PDCI_IO_write_commit(pdc, io, buf, set_buf, num_bytes, "PDC_IO_write_commit");
}

void
PDC_IO_write_abort (PDC_t *pdc, Sfio_t *io, PDC_byte *buf, int set_buf)
{
  PDCI_DISC_INIT_CHECKS_RET_VOID("PDC_IO_write_abort");
  PDCI_NULLPARAM_CHECK_RET_VOID("PDC_IO_write_abort", io);
  PDCI_NULLPARAM_CHECK_RET_VOID("PDC_IO_write_abort", buf);
  PDCI_IO_write_abort(pdc, io, buf, set_buf, "PDC_IO_write_abort");
}

ssize_t
PDC_IO_rec_write2io(PDC_t *pdc, Sfio_t *io, PDC_byte *buf, size_t rec_data_len)
{
  PDCI_IODISC_INIT_CHECKS_RET_SSIZE("PDC_IO_rec_write2io");
  PDCI_NULLPARAM_CHECK_RET_SSIZE("PDC_IO_rec_write2io", io);
  PDCI_NULLPARAM_CHECK_RET_SSIZE("PDC_IO_rec_write2io", buf);
  return PDCI_IO_rec_write2io(pdc, io, buf, rec_data_len, "PDC_IO_rec_write2io");
}

ssize_t
PDC_IO_rec_open_write2buf(PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full)
{
  PDCI_IODISC_INIT_CHECKS_RET_SSIZE("PDC_IO_rec_open_write2buf");
  PDCI_NULLPARAM_CHECK_RET_SSIZE("PDC_IO_rec_open_write2buf", buf);
  PDCI_NULLPARAM_CHECK_RET_SSIZE("PDC_IO_rec_open_write2buf", buf_full);
  return PDCI_IO_rec_open_write2buf(pdc, buf, buf_len, buf_full, "PDC_IO_rec_open_write2buf");
}

ssize_t
PDC_IO_rec_close_write2buf(PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full,
			   PDC_byte *rec_start, size_t num_bytes) 
{
  PDCI_IODISC_INIT_CHECKS_RET_SSIZE("PDC_IO_rec_close_write2buf");
  PDCI_NULLPARAM_CHECK_RET_SSIZE("PDC_IO_rec_close_write2buf", buf);
  PDCI_NULLPARAM_CHECK_RET_SSIZE("PDC_IO_rec_close_write2buf", buf_full);
  PDCI_NULLPARAM_CHECK_RET_SSIZE("PDC_IO_rec_close_write2buf", rec_start);
  return PDCI_IO_rec_close_write2buf(pdc, buf, buf_len, buf_full, rec_start, num_bytes, "PDC_IO_rec_close_write2buf");
}

ssize_t
PDC_IO_rblk_write2io(PDC_t *pdc, Sfio_t *io, PDC_byte *buf, size_t blk_data_len, PDC_uint32 num_recs)
{
  PDCI_IODISC_INIT_CHECKS_RET_SSIZE("PDC_IO_rblk_write2io");
  PDCI_NULLPARAM_CHECK_RET_SSIZE("PDC_IO_rblk_write2io", io);
  PDCI_NULLPARAM_CHECK_RET_SSIZE("PDC_IO_rblk_write2io", buf);
  return PDCI_IO_rblk_write2io(pdc, io, buf, blk_data_len, num_recs, "PDC_IO_rblk_write2io");
}

ssize_t
PDC_IO_rblk_open_write2buf(PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full)
{
  PDCI_IODISC_INIT_CHECKS_RET_SSIZE("PDC_IO_rblk_open_write2buf");
  PDCI_NULLPARAM_CHECK_RET_SSIZE("PDC_IO_rblk_open_write2buf", buf);
  PDCI_NULLPARAM_CHECK_RET_SSIZE("PDC_IO_rblk_open_write2buf", buf_full);
  return PDCI_IO_rblk_open_write2buf(pdc, buf, buf_len, buf_full, "PDC_IO_rblk_open_write2buf");
}

ssize_t
PDC_IO_rblk_close_write2buf(PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full,
			    PDC_byte *blk_start, size_t num_bytes, PDC_uint32 num_recs)
{
  PDCI_IODISC_INIT_CHECKS_RET_SSIZE("PDC_IO_rblk_close_write2buf");
  PDCI_NULLPARAM_CHECK_RET_SSIZE("PDC_IO_rblk_close_write2buf", buf);
  PDCI_NULLPARAM_CHECK_RET_SSIZE("PDC_IO_rblk_close_write2buf", buf_full);
  PDCI_NULLPARAM_CHECK_RET_SSIZE("PDC_IO_rblk_close_write2buf", blk_start);
  return PDCI_IO_rblk_close_write2buf(pdc, buf, buf_len, buf_full, blk_start, num_bytes, num_recs, "PDC_IO_rblk_close_write2buf");
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
/* PDC_string helper functions */

PDC_error_t
PDC_string_init(PDC_t *pdc, PDC_string *s)
{
  if (!s) {
    return PDC_ERR;
  }
  memset((void*)s, 0, sizeof(*s));
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
PDC_string_share(PDC_t *pdc, PDC_string *targ, const PDC_string *src)
{
  PDCI_DISC_INIT_CHECKS("PDC_string_share");
  PDCI_NULLPARAM_CHECK("PDC_string_share", src);
  PDCI_NULLPARAM_CHECK("PDC_string_share", targ);
  PDCI_STR_SHARE(targ, src->str, src->len);
  return PDC_OK;
}

PDC_error_t
PDC_string_Cstr_share(PDC_t *pdc, PDC_string *targ, const char *src, size_t len)
{
  PDCI_DISC_INIT_CHECKS("PDC_string_Cstr_share");
  PDCI_NULLPARAM_CHECK("PDC_string_Cstr_share", src);
  PDCI_NULLPARAM_CHECK("PDC_string_Cstr_share", targ);
  PDCI_STR_SHARE(targ, src, len);
  return PDC_OK;
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
PDC_string_Cstr_copy(PDC_t *pdc, PDC_string *targ, const char *src, size_t len)
{
  PDCI_DISC_INIT_CHECKS("PDC_string_Cstr_copy");
  PDCI_NULLPARAM_CHECK("PDC_string_Cstr_copy", src);
  PDCI_NULLPARAM_CHECK("PDC_string_Cstr_copy", targ);
  PDCI_STR_CPY(targ, src, len);
  return PDC_OK;

 fatal_alloc_err:
  PDC_FATAL(pdc->disc, "PDC_string_Cstr_copy: out of space");
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

PDC_error_t
PDC_string_ed_copy(PDC_t *pdc, PDC_base_ed *targ, const PDC_base_ed *src)
{
  PDCI_DISC_INIT_CHECKS("PDC_string_ed_copy");
  PDCI_NULLPARAM_CHECK("PDC_string_ed_copy", src);
  PDCI_NULLPARAM_CHECK("PDC_string_ed_copy", targ);
  (*targ) = (*src);
  return PDC_OK;
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
  PDCI_DISC_INIT_CHECKS("PDC_regexp_compile");

  return PDCI_regexp_compile(pdc, regexp, regexp_out, "PDC_regexp_compile");
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

/* helpers for enumeration types */

const char *
PDC_base_csm2str(PDC_base_csm e)
{
  switch (e)
    {
    case PDC_CheckAndSet:
      return "PDC_CheckAndSet";
    case PDC_Check:
      return "PDC_Check";
    case PDC_Ignore:
      return "PDC_Ignore";
    default:
      break;
    }
  return "*Invalid PDC_base_csm value*";
}

const char *
PDC_errorRep2str(PDC_errorRep e)
{
  switch (e)
    {
    case PDC_errorRep_Max:
      return "PDC_errorRep_Max";
    case PDC_errorRep_Med:
      return "PDC_errorRep_Med";
    case PDC_errorRep_Min:
      return "PDC_errorRep_Min";
    case PDC_errorRep_None:
      return "PDC_errorRep_None";
    default:
      break;
    }
  return "*Invalid PDC_errorRep value*";
}

const char *
PDC_endian2str(PDC_endian e)
{
  switch (e)
    {
    case PDC_bigEndian:
      return "PDC_bigEndian";
    case PDC_littleEndian:
      return "PDC_littleEndian";
    default:
      break;
    }
  return "*Invalid PDC_endian value*";
}

const char *
PDC_charset2str(PDC_charset e)
{
  switch (e)
    {
    case PDC_charset_ASCII:
      return "PDC_charset_ASCII";
    case PDC_charset_EBCDIC:
      return "PDC_charset_EBCDIC";
    default:
      break;
    }
  return "*Invalid PDC_charset value*";
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

/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 * INTERNAL FUNCTIONS (see libpadsc-internal.h)
 * ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*/

/* ================================================================================ */ 
/* INTERNAL ERROR REPORTING FUNCTIONS */

PDC_error_t
PDCI_report_err(PDC_t *pdc, int level, PDC_loc_t *loc,
		PDC_errCode_t errCode, const char *whatfn, const char *format, ...)
{
  PDC_error_f pdc_errorf;
  char    *severity = "Error";
  char    *msg      = "** unknown error code **";
  char    *infn, *tmpstr1, *tmpstr2, *tmpstr3;
  size_t  tmplen1, tmplen2, tmplen3;
  int     nullspan = 0;

  PDC_TRACE(pdc->disc, "PDCI_report_err called");
  if (!whatfn) {
    infn = "";
  } else {
    sfstrset(pdc->tmp2, 0);
    sfprintf(pdc->tmp2, "[in %s]", whatfn);
    infn = sfstruse(pdc->tmp2);
  }
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
  sfstrset(pdc->tmp1, 0);
  if (pdc->disc->e_rep == PDC_errorRep_Min) {
    if (loc) {
      pdc_errorf(NiL, level, "%s %s: %s %d char %d: errCode %d",
		 severity, infn, loc->b.unit, loc->b.num, loc->b.byte, errCode);
    } else {
      pdc_errorf(NiL, level, "%s %s: errCode %d", severity, infn, errCode);
    }
    return PDC_OK;
  }
  if (format && strlen(format)) {
    va_list ap;
    if (loc) {
      sfprintf(pdc->tmp1, "%s %s: %s %d char %d : ", severity, infn, loc->b.unit, loc->b.num, loc->b.byte);
    } else {
      sfprintf(pdc->tmp1, "%s %s: ", severity, infn);
    }
    va_start(ap, format);
    sfvprintf(pdc->tmp1, format, ap);
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
    case PDC_INVALID_CHARSET:
      msg = "Invalid PDC_charset value";
      break;
    case PDC_INVALID_WIDTH:
      msg = "Invalid fixed width arg: does not match width of PDC_string arg";
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
      sfprintf(pdc->tmp1, "** unknown error code: %d **", errCode);
      msg = "";
      break;
    }
    if (loc) {
      if (loc->b.num != loc->e.num) {
	sfprintf(pdc->tmp1, "%s %s: from %s %d char %d to %s %d char %d: %s ",
		 severity, infn,
		 loc->b.unit, loc->b.num, loc->b.byte, 
		 loc->e.unit, loc->e.num, loc->e.byte,
		 msg);
      } else if (nullspan) {
	sfprintf(pdc->tmp1, "%s %s: at %s %d just before char %d: %s",
		 severity, infn,
		 loc->b.unit, loc->b.num, loc->b.byte,
		 msg);
      } else if (loc->b.byte == loc->e.byte) {
	sfprintf(pdc->tmp1, "%s %s: at %s %d at char %d : %s ",
		 severity, infn,
		 loc->b.unit, loc->b.num, loc->b.byte,
		 msg);
      } else {
	sfprintf(pdc->tmp1, "%s %s: at %s %d from char %d to char %d: %s ",
		 severity, infn,
		 loc->b.unit, loc->b.num, loc->b.byte, loc->e.byte,
		 msg);
      }
    } else {
      sfprintf(pdc->tmp1, "%s %s: %s ", severity, infn, msg);
    }
  }
  if (loc && (pdc->disc->e_rep == PDC_errorRep_Max)) {
    PDC_IO_elt_t *elt1, *elt2;
    if (loc->b.num != loc->e.num) {
      if (PDC_OK == PDCI_IO_getElt(pdc, loc->b.num, &elt1)) {
	sfprintf(pdc->tmp1, "\n[%s %d]", loc->b.unit, loc->b.num);
	if (elt1->len == 0) {
	  sfprintf(pdc->tmp1, "(**EMPTY**)>>>");
	} else {
	  tmplen1 = loc->b.byte - 1;
	  tmplen2 = elt1->len - tmplen1;
	  tmpstr1 = PDC_fmt_Cstr((char*)elt1->begin,           tmplen1);
	  tmpstr2 = PDC_fmt_Cstr((char*)elt1->begin + tmplen1, tmplen2);
	  sfprintf(pdc->tmp1, "%s>>>%s", tmpstr1, tmpstr2);
	}
      }
      if (PDC_OK == PDCI_IO_getElt(pdc, loc->e.num, &elt2)) {
	if (!elt1) {
	  sfprintf(pdc->tmp1, "\n[%s %d]: ... >>>(char pos %d) ...",
		   loc->b.unit, loc->b.num, loc->b.byte);
	}
	sfprintf(pdc->tmp1, "\n[%s %d]", loc->e.unit, loc->e.num);
	if (elt2->len == 0) {
	  sfprintf(pdc->tmp1, "(**EMPTY**)<<<");
	} else {
	  tmplen1 = loc->e.byte;
	  tmplen2 = elt2->len - tmplen1;
	  tmpstr1 = PDC_fmt_Cstr((char*)elt2->begin,           tmplen1);
	  tmpstr2 = PDC_fmt_Cstr((char*)elt2->begin + tmplen1, tmplen2);
	  sfprintf(pdc->tmp1, "%s<<<%s", tmpstr1, tmpstr2);
	}
      }
    } else { /* same elt */
      if (PDC_OK == PDCI_IO_getElt(pdc, loc->e.num, &elt1)) {
	sfprintf(pdc->tmp1, "\n[%s %d]", loc->e.unit, loc->e.num);
	if (elt1->len == 0) {
	  sfprintf(pdc->tmp1, ">>>(**EMPTY**)<<<");
	} else if (nullspan) {
	  tmplen1 = loc->b.byte - 1;
	  tmplen2 = elt1->len - tmplen1;
	  tmpstr1 = PDC_fmt_Cstr((char*)elt1->begin,           tmplen1);
	  tmpstr2 = PDC_fmt_Cstr((char*)elt1->begin + tmplen1, tmplen2);
	  sfprintf(pdc->tmp1, "%s>>><<<%s", tmpstr1, tmpstr2);
	} else {
	  tmplen1 = loc->b.byte - 1;
	  tmplen3 = elt1->len - loc->e.byte;
	  tmplen2 = elt1->len - tmplen1 - tmplen3;
	  tmpstr1 = PDC_fmt_Cstr((char*)elt1->begin,                     tmplen1);
	  tmpstr2 = PDC_fmt_Cstr((char*)elt1->begin + tmplen1,           tmplen2);
	  tmpstr3 = PDC_fmt_Cstr((char*)elt1->begin + tmplen1 + tmplen2, tmplen3);
	  sfprintf(pdc->tmp1, "%s>>>%s<<<%s", tmpstr1, tmpstr2, tmpstr3);
	}
      }
    }
  }
  pdc_errorf(NiL, level, "%s", sfstruse(pdc->tmp1));
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
  /* tell sfio to use pdc->sfbuf but only let it know about sizeof(sfbuf)-1 space */
  buf = sfsetbuf(pdc->io, (Void_t*)1, 0);
  if (!buf) {
    sfsetbuf(pdc->io, pdc->sfbuf, 1024 * 1024);
  } else if (buf == (Void_t*)pdc->sfbuf) {
    /* PDC_WARN(pdc->disc, "XXX_REMOVE pdc->sfbuf has already been installed so not installing it again"); */
  } else {
    /* PDC_WARN(pdc->disc, "XXX_REMOVE An unknown buffer has already been installed so not installing pdc->sfbuf\n"
                 "  (could be due to use of sfungetc)"); */
  }
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
PDC_IO_fopen_internal(PDC_t *pdc, const char *path)
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
  if (pdc->disc->io_disc->rec_based == 0) {
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

PDC_byte*
PDCI_IO_write_start(PDC_t *pdc, Sfio_t *io, size_t *buf_len, int *set_buf, const char *whatfn)
{
  PDC_byte  *buf;
  ssize_t    n, nm;

  PDC_TRACE(pdc->disc, "PDCI_IO_write_start called");
  if (!sfsetbuf(io, (Void_t *)1, 0))  {
    sfsetbuf(io, pdc->outbuf, pdc->outbuf_len);
    (*set_buf) = 1;
  } else {
    (*set_buf) = 0;
  }
  n = (*buf_len);
  nm = -1 * n;
  if (!(buf = (PDC_byte*)sfreserve(io, nm, SF_LOCKR))) {
    PDCI_report_err(pdc, PDC_FATAL_FLAGS, 0, PDC_IO_ERR, whatfn, "sfreserve failed");
    if (*set_buf) {
      sfsetbuf(io, (Void_t*)0, 0); /* undo sfsetbuf */
    }
    return 0;
  }
  nm = sfvalue(io);
  if (nm < (*buf_len)) {
    PDCI_report_err(pdc, PDC_FATAL_FLAGS, 0, PDC_IO_ERR, whatfn, "sfreserve returned insufficient bytes");
    sfwrite(io, (Void_t*)buf, 0); /* release sfreserve */
    if (*set_buf) {
      sfsetbuf(io, (Void_t*)0, 0); /* undo sfsetbuf */
    }
    return 0;
  }
  if (nm > (*buf_len)) {
    (*buf_len) = nm;
  }
  return buf;
}

ssize_t
PDCI_IO_write_commit(PDC_t *pdc, Sfio_t *io, PDC_byte *buf, int set_buf, size_t num_bytes, const char *whatfn)
{
  ssize_t n;

  PDC_TRACE(pdc->disc, "PDCI_IO_write_commit called");
  n = sfwrite(io, (Void_t*)buf, num_bytes);
  if (set_buf) {
    sfsetbuf(io, (Void_t*)0, 0); /* undo sfsetbuf */
  }
  if (n != num_bytes) {
    PDC_WARN1(pdc->disc, "%s: low-level sfwrite failure", whatfn);
    if (n > 0) {
      /* XXX_TODO try to back up ??? */
    }
    return -1;
  }
  return n;
}

void
PDCI_IO_write_abort(PDC_t *pdc, Sfio_t *io, PDC_byte *buf, int set_buf, const char *whatfn)
{
  PDC_TRACE(pdc->disc, "PDCI_IO_write_abort called");
  sfwrite(io, (Void_t*)buf, 0); /* release sfreserve */
  if (set_buf) {
    sfsetbuf(io, (Void_t*)0, 0); /* undo sfsetbuf */
  }
}

ssize_t
PDCI_IO_rec_write2io(PDC_t *pdc, Sfio_t *io, PDC_byte *buf, size_t rec_data_len, const char *whatfn)
{
  PDC_IO_disc_t *iodisc = pdc->disc->io_disc;
  PDC_byte      *iobuf, *iobuf_cursor;
  size_t         num_bytes, iobuf_len;
  int            set_buf = 0;
  ssize_t        tlen;

  PDC_TRACE(pdc->disc, "PDCI_IO_rec_write2io called");
  if (!iodisc->rec_based) {
    PDC_WARN1(pdc->disc, "%s: pdc->disc->io_disc must support records to use this function", whatfn);
    return -1;
  }
  num_bytes = rec_data_len + iodisc->rec_obytes;
  iobuf_len = num_bytes + iodisc->rec_cbytes + 1;
  iobuf = PDCI_IO_write_start(pdc, io, &iobuf_len, &set_buf, whatfn);
  if (!iobuf) {
    /* write_start reported the error */
    /* don't have to abort because write_start failed */
    return -1;
  }
  iobuf_cursor = iobuf + iodisc->rec_obytes;
  memcpy(iobuf_cursor, buf, rec_data_len);
  iobuf_cursor += rec_data_len;
  if (-1 == (tlen = iodisc->rec_close_fn(pdc, iodisc, iobuf_cursor, iobuf, num_bytes))) {
    PDC_WARN1(pdc->disc, "%s: internal error, failed to write record", whatfn);
    PDCI_IO_write_abort(pdc, io, iobuf, set_buf, whatfn);
    return -1;
  }
  iobuf_len = num_bytes + tlen;
  return PDCI_IO_write_commit(pdc, io, iobuf, iobuf_len, set_buf, whatfn);
}

ssize_t
PDCI_IO_rec_open_write2buf(PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, const char *whatfn)
{
  PDC_IO_disc_t *iodisc = pdc->disc->io_disc;

  PDC_TRACE(pdc->disc, "PDCI_IO_rec_open_write2buf called");
  if (!iodisc->rec_based) {
    PDC_WARN1(pdc->disc, "%s: pdc->disc->io_disc must support records to use this function", whatfn);
    return -1;
  }
  if (buf_len < iodisc->rec_obytes) {
    (*buf_full) = 1;
    return -1;
  }
  return iodisc->rec_obytes;
}

ssize_t
PDCI_IO_rec_close_write2buf(PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full,
			    PDC_byte *rec_start, size_t num_bytes, const char *whatfn)
{
  PDC_IO_disc_t *iodisc = pdc->disc->io_disc;

  PDC_TRACE(pdc->disc, "PDCI_IO_rec_close_write2buf called");
  if (!iodisc->rec_based) {
    PDC_WARN1(pdc->disc, "%s: pdc->disc->io_disc must support records to use this function", whatfn);
    return -1;
  }
  if (buf_len < iodisc->rec_cbytes) {
    (*buf_full) = 1;
    return -1;
  }
  return iodisc->rec_close_fn(pdc, iodisc, buf, rec_start, num_bytes);
}

ssize_t
PDCI_IO_rblk_write2io(PDC_t *pdc, Sfio_t *io, PDC_byte *buf, size_t blk_data_len, PDC_uint32 num_recs, const char *whatfn)
{
  PDC_IO_disc_t *iodisc = pdc->disc->io_disc;
  PDC_byte      *iobuf, *iobuf_cursor;
  size_t         num_bytes, iobuf_len;
  int            set_buf = 0;
  ssize_t        tlen;

  PDC_TRACE(pdc->disc, "PDCI_IO_rblk_write2io called");
  if (!iodisc->has_rblks) {
    PDC_WARN1(pdc->disc, "%s: pdc->disc->io_disc must support record blocks to use this function", whatfn);
    return -1;
  }
  num_bytes = blk_data_len + iodisc->blk_obytes;
  iobuf_len = num_bytes + iodisc->blk_cbytes + 1;
  iobuf = PDCI_IO_write_start(pdc, io, &iobuf_len, &set_buf, whatfn);
  if (!iobuf) {
    /* write_start reported the error */
    /* don't have to abort because write_start failed */
    return -1;
  }
  iobuf_cursor = iobuf + iodisc->blk_obytes;
  memcpy(iobuf_cursor, buf, blk_data_len);
  iobuf_cursor += blk_data_len;
  if (-1 == (tlen = iodisc->blk_close_fn(pdc, iodisc, iobuf_cursor, iobuf, num_bytes, num_recs))) {
    PDC_WARN1(pdc->disc, "%s: internal error, failed to write block of records", whatfn);
    PDCI_IO_write_abort(pdc, io, iobuf, set_buf, whatfn);
    return -1;
  }
  iobuf_len = num_bytes + tlen;
  return PDCI_IO_write_commit(pdc, io, iobuf, iobuf_len, set_buf, whatfn);
}

ssize_t
PDCI_IO_rblk_open_write2buf(PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, const char *whatfn)
{
  PDC_IO_disc_t *iodisc = pdc->disc->io_disc;

  PDC_TRACE(pdc->disc, "PDCI_IO_rblk_open_write2buf called");
  if (!iodisc->has_rblks) {
    PDC_WARN1(pdc->disc, "%s: pdc->disc->io_disc must support record blocks to use this function", whatfn);
    return -1;
  }
  if (buf_len < iodisc->blk_obytes) {
    (*buf_full) = 1;
    return -1;
  }
  return iodisc->blk_obytes;
}

ssize_t
PDCI_IO_rblk_close_write2buf(PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full,
			     PDC_byte *blk_start, size_t num_bytes, PDC_uint32 num_recs, const char *whatfn)
{
  PDC_IO_disc_t *iodisc = pdc->disc->io_disc;

  PDC_TRACE(pdc->disc, "PDCI_IO_rblk_close_write2buf called");
  if (!iodisc->has_rblks) {
    PDC_WARN1(pdc->disc, "%s: pdc->disc->io_disc must support record blocks to use this function", whatfn);
    return -1;
  }
  if (buf_len < iodisc->blk_cbytes) {
    (*buf_full) = 1;
    return -1;
  }
  return iodisc->blk_close_fn(pdc, iodisc, buf, blk_start, num_bytes, num_recs);
}


/* ================================================================================ */
/* CHARSET INTERNAL SCAN FUNCTIONS */

PDC_error_t
PDCI_char_lit_scan(PDC_t *pdc, PDC_char c, PDC_char s, int eat_lit,
		   PDC_char *c_out, size_t *offset_out, PDC_charset char_set, const char *whatfn, int safe)
{
  PDC_byte       *begin, *p1, *p2, *end;
  int             eor, eof;
  size_t          bytes;
  int             matchlen = -1;

  if (safe) {
    PDCI_IODISC_INIT_CHECKS(whatfn);
  }
  PDC_TRACE6(pdc->disc, "PDCI_char_lit_scan args: c %s stop %s eat %d, char_set = %s, whatfn = %s, safe = %d",
	     PDC_qfmt_char(c), PDC_qfmt_char(s), eat_lit, PDC_charset2str(char_set), whatfn, safe);
  switch (char_set)
    {
    case PDC_charset_ASCII:
      break;
    case PDC_charset_EBCDIC:
      c = PDC_mod_ae_tab[(int)c]; /* convert to EBCDIC char */
      s = PDC_mod_ae_tab[(int)s]; /* convert to EBCDIC char */
      break;
    default:
      goto invalid_charset;
    }
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
	goto fatal_forward_err;
      }
      return PDC_OK;
    }
    if (pdc->disc->stop_maxlen && ((p1-begin) >= pdc->disc->stop_maxlen)) {
      if (pdc->speclev == 0) {
	PDC_WARN1(pdc->disc, "%s: scan terminated early due to disc->stop_maxlen", whatfn);
      }
      break;
    }
    if (pdc->disc->stop_regexp && PDCI_regexpMatch(pdc, pdc->disc->stop_regexp, p1, end, char_set)) {
      if (pdc->speclev == 0) {
	PDC_WARN1(pdc->disc, "%s: scan terminated early due to disc->stop_regexp", whatfn);
      }
      break;
    }
    p1++;
  }
  return PDC_ERR;

 invalid_charset:
  PDCI_report_err(pdc, PDC_WARN_FLAGS, 0, PDC_INVALID_CHARSET, whatfn, 0);
  return PDC_ERR;

 fatal_forward_err:
  PDCI_report_err(pdc, PDC_FATAL_FLAGS, 0, PDC_FORWARD_ERR, whatfn, "IO_forward error");
  return PDC_ERR;
}

PDC_error_t
PDCI_str_lit_scan(PDC_t *pdc, const PDC_string *findStr, const PDC_string *stopStr, int eat_lit,
		  PDC_string **str_out, size_t *offset_out, PDC_charset char_set,
		  const char *whatfn, int safe) 
{
  PDC_byte        *begin, *p1, *p2, *end;
  int             eor, eof;
  size_t          bytes;
  int             matchlen = -1;
  PDC_string     *tmp_findStr = (PDC_string*)findStr;
  PDC_string     *tmp_stopStr = (PDC_string*)stopStr;

  if (safe) {
    PDCI_IODISC_INIT_CHECKS(whatfn);
    PDCI_NULLPARAM_CHECK(whatfn, findStr);
  }

  PDC_TRACE6(pdc->disc, "PDCI_str_lit_scan args: findStr = %s stopStre = %s eat = %d, char_set = %s, whatfn = %s, safe = %d",
	     PDC_qfmt_str(findStr), PDC_qfmt_str(stopStr), eat_lit, PDC_charset2str(char_set), whatfn, safe);
  if (offset_out) {
    (*offset_out) = 0;
  }
  if (!tmp_findStr || tmp_findStr->len == 0) {
    PDC_WARN1(pdc->disc, "%s: null/empty findStr specified", whatfn);
    return PDC_ERR;
  }
  switch (char_set)
    {
    case PDC_charset_ASCII:
      break;
    case PDC_charset_EBCDIC:
      tmp_findStr = &pdc->stmp1;
      PDCI_A2E_STR_CPY(tmp_findStr, findStr->str, findStr->len);
      if (tmp_stopStr) {
	tmp_stopStr = &pdc->stmp2;
	PDCI_A2E_STR_CPY(tmp_stopStr, stopStr->str, stopStr->len);
      }
      break;
    default:
      goto invalid_charset;
    }
  if (pdc->disc->stop_regexp) {
    matchlen = pdc->disc->stop_regexp->max;
  }
  if (matchlen != 0 && tmp_stopStr && matchlen < tmp_stopStr->len) {
    matchlen = tmp_stopStr->len;
  }
  if (PDC_ERR == PDCI_IO_needbytes(pdc, &begin, &p1, &p2, &end, &eor, &eof, &bytes)) {
    return PDC_ERR;
  }
  while (1) {
    if (p1 + tmp_findStr->len > end) {
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
      } else if (matchlen > tmp_findStr->len) {
	p1 -= (matchlen - tmp_findStr->len);
      }
      continue;
    }
    /* p1 + tmp_findStr->len <= end */
    if (strncmp((char*)p1, tmp_findStr->str, tmp_findStr->len) == 0) {
      if (str_out) {
	(*str_out) = (PDC_string*)findStr; /* note: original string */
      }
      if (offset_out) {
	(*offset_out) = (p1-begin);
      }
      if (eat_lit) {
	p1 += tmp_findStr->len; /* advance beyond findStr */
      }
      if ((p1-begin) && PDC_ERR == PDCI_IO_forward(pdc, p1-begin)) {
	goto fatal_forward_err;
      }
      return PDC_OK;
    }
    if (tmp_stopStr && (p1 + tmp_stopStr->len <= end) &&
	strncmp((char*)p1, tmp_stopStr->str, tmp_stopStr->len) == 0) {
      if (str_out) {
	(*str_out) = (PDC_string*)stopStr; /* note: original string */
      }
      if (offset_out) {
	(*offset_out) = (p1-begin);
      }
      p1 += tmp_stopStr->len; /* advance beyond stopStr */
      if (PDC_ERR == PDCI_IO_forward(pdc, p1-begin)) {
	goto fatal_forward_err;
      }
      return PDC_OK;
    }
    if (pdc->disc->stop_maxlen && ((p1-begin) >= pdc->disc->stop_maxlen)) {
      if (pdc->speclev == 0) {
	PDC_WARN1(pdc->disc, "%s: scan terminated early due to disc->stop_maxlen", whatfn);
      }
      break;
    }
    if (pdc->disc->stop_regexp && PDCI_regexpMatch(pdc, pdc->disc->stop_regexp, p1, end, char_set)) {
      if (pdc->speclev == 0) {
	PDC_WARN1(pdc->disc, "%s: scan terminated early due to disc->stop_regexp", whatfn);
      }
      break;
    }
    p1++;
  }
  return PDC_ERR;

 invalid_charset:
  PDCI_report_err(pdc, PDC_WARN_FLAGS, 0, PDC_INVALID_CHARSET, whatfn, 0);
  return PDC_ERR;

 fatal_forward_err:
  PDCI_report_err(pdc, PDC_FATAL_FLAGS, 0, PDC_FORWARD_ERR, whatfn, "IO_forward error");
  return PDC_ERR;

 fatal_alloc_err:
  PDCI_report_err(pdc, PDC_FATAL_FLAGS, 0, PDC_ALLOC_ERR, whatfn, "Memory alloc error");
  return PDC_ERR;
}

PDC_error_t
PDCI_Cstr_lit_scan(PDC_t *pdc, const char *findStr, const char *stopStr, int eat_lit,
		   const char **str_out, size_t *offset_out, PDC_charset char_set,
		   const char *whatfn, int safe)
{
  PDC_string findS, stopS;
  PDC_string *findS_ptr = 0, *stopS_ptr = 0;
  PDC_string *outS_ptr;

  if (findStr) {
    findS.str = (char*)findStr;
    findS.len = strlen(findStr);
    findS_ptr = &findS;
  }
  if (stopStr) {
    stopS.str = (char*)stopStr;
    stopS.len = strlen(stopStr);
    stopS_ptr = &stopS;
  }
  if (str_out) {
    if (PDC_ERR == PDCI_str_lit_scan(pdc, findS_ptr, stopS_ptr, eat_lit, &outS_ptr, offset_out, char_set, whatfn, safe)) {
      return PDC_ERR;
    }
    (*str_out) = (outS_ptr == findS_ptr) ? findStr : stopStr;
    return PDC_OK;
  }
  return PDCI_str_lit_scan(pdc, findS_ptr, stopS_ptr, eat_lit, 0, offset_out, char_set, whatfn, safe);
}

/* ================================================================================ */
/* CHARSET INTERNAL READ ROUTINES */

PDC_error_t
PDCI_char_lit_read(PDC_t *pdc, const PDC_base_csm *csm,
		   PDC_base_ed *ed, PDC_char c, PDC_charset char_set,
		   const char *whatfn, int safe)
{
  PDC_byte        *begin, *p1, *p2, *end;
  int              eor, eof;
  size_t           bytes;
  PDC_base_csm     csmt;
  PDC_base_ed      edt;

  if (safe) {
    PDCI_IODISC_INIT_CHECKS(whatfn);
    if (!csm) {
      csmt = PDC_CheckAndSet;
      csm = &csmt;
    }
    if (!ed) {
      ed = &edt;
    }
  }
  PDC_TRACE4(pdc->disc, "PDCI_char_lit_read called, arg: %s, char_set %s, whatfn = %s, safe = %d",
	     PDC_qfmt_char(c), PDC_charset2str(char_set), whatfn, safe);
  switch (char_set)
    {
    case PDC_charset_ASCII:
      break;
    case PDC_charset_EBCDIC:
      c = PDC_mod_ae_tab[(int)c]; /* convert to EBCDIC char */
      break;
    default:
      goto invalid_charset;
    }
  if (PDC_ERR == PDCI_IO_needbytes(pdc, &begin, &p1, &p2, &end, &eor, &eof, &bytes)) {
    goto fatal_nb_io_err;
  }
  if (bytes == 0) {
    goto at_eor_or_eof_err;
  }
  if ((*csm == PDC_Ignore) || (c == (*begin))) {
    if (PDC_ERR == PDCI_IO_forward(pdc, 1)) {
      goto fatal_forward_err;
    }
    ed->errCode = PDC_NO_ERR;
    return PDC_OK;  /* IO cursor is one beyond c */
  }
  goto not_found;

 invalid_charset:
  PDCI_READFN_SET_NULLSPAN_LOC(0);
  PDCI_READFN_RET_ERRCODE_WARN(whatfn, 0, PDC_INVALID_CHARSET);

 at_eor_or_eof_err:
  PDCI_READFN_SET_NULLSPAN_LOC(0);
  PDCI_READFN_RET_ERRCODE_WARN(whatfn, 0, eor ? PDC_AT_EOR : PDC_AT_EOF);

 not_found:
  PDCI_READFN_SET_LOC_BE(0, 1);
  if (*csm == PDC_CheckAndSet) {
    PDCI_READFN_RET_ERRCODE_WARN(whatfn, 0, PDC_CHAR_LIT_NOT_FOUND);
  }
  PDCI_READFN_RET_ERRCODE_NOWARN(PDC_CHAR_LIT_NOT_FOUND);

 fatal_nb_io_err:
  PDCI_READFN_RET_ERRCODE_FATAL(whatfn, "IO error (nb)", PDC_IO_ERR);

 fatal_forward_err:
  PDCI_READFN_RET_ERRCODE_FATAL(whatfn, "IO_forward error", PDC_FORWARD_ERR);
}

PDC_error_t
PDCI_str_lit_read(PDC_t *pdc, const PDC_base_csm *csm,
		  PDC_base_ed *ed, const PDC_string *s, PDC_charset char_set, const char *whatfn, int safe)
{
  PDC_byte        *begin, *p1, *p2, *end;
  PDC_string      *es;
  int              eor, eof;
  size_t           bytes;
  PDC_base_csm     csmt;
  PDC_base_ed      edt;

  if (safe) {
    PDCI_IODISC_INIT_CHECKS(whatfn);
    PDCI_NULLPARAM_CHECK(whatfn, s);
    if (!csm) {
      csmt = PDC_CheckAndSet;
      csm = &csmt;
    }
    if (!ed) {
      ed = &edt;
    }
  }
  PDC_TRACE4(pdc->disc, "PDCI_str_lit_read called, arg: %s, char_set %s, whatfn = %s, safe = %d",
	     PDC_qfmt_str(s), PDC_charset2str(char_set), whatfn, safe);
  switch (char_set)
    {
    case PDC_charset_ASCII:
      break;
    case PDC_charset_EBCDIC:
      es = &pdc->stmp1;
      PDCI_A2E_STR_CPY(es, s->str, s->len);
      s = es;
      break;
    default:
      goto invalid_charset;
    }
  if (s->len <= 0) {
    PDC_WARN1(pdc->disc, "%s: UNEXPECTED PARAM VALUE: s->len <= 0", whatfn);
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
  if ((*csm == PDC_Ignore) || (strncmp((char*)begin, s->str, s->len) == 0)) {
    if (PDC_ERR == PDCI_IO_forward(pdc, s->len)) {
      goto fatal_forward_err;
    }
    ed->errCode = PDC_NO_ERR;
    return PDC_OK;    /* found it */
  }
  goto not_found;

 invalid_charset:
  PDCI_READFN_SET_NULLSPAN_LOC(0);
  PDCI_READFN_RET_ERRCODE_WARN(whatfn, 0, PDC_INVALID_CHARSET);

 bad_param_err:
  PDCI_READFN_SET_NULLSPAN_LOC(0);
  PDCI_READFN_RET_ERRCODE_WARN(whatfn, 0, PDC_BAD_PARAM);

 width_not_avail:
  PDCI_READFN_SET_LOC_BE(0, end-begin);
  if (*csm == PDC_CheckAndSet) {
    PDCI_READFN_RET_ERRCODE_WARN(whatfn, 0, PDC_STR_LIT_NOT_FOUND);
  }
  PDCI_READFN_RET_ERRCODE_NOWARN(PDC_STR_LIT_NOT_FOUND);

 not_found:
  PDCI_READFN_SET_LOC_BE(0, s->len);
  if (*csm == PDC_CheckAndSet) {
    PDCI_READFN_RET_ERRCODE_WARN(whatfn, 0, PDC_STR_LIT_NOT_FOUND);
  }
  PDCI_READFN_RET_ERRCODE_NOWARN(PDC_STR_LIT_NOT_FOUND);

 fatal_nb_io_err:
  PDCI_READFN_RET_ERRCODE_FATAL(whatfn, "IO error (nb)", PDC_IO_ERR);

 fatal_mb_io_err:
  PDCI_READFN_RET_ERRCODE_FATAL(whatfn, "IO error (mb)", PDC_IO_ERR);

 fatal_forward_err:
  PDCI_READFN_RET_ERRCODE_FATAL(whatfn, "IO_forward error", PDC_FORWARD_ERR);

 fatal_alloc_err:
  PDCI_READFN_RET_ERRCODE_FATAL(whatfn, "Memory alloc error", PDC_ALLOC_ERR);
}

PDC_error_t
PDCI_Cstr_lit_read(PDC_t *pdc, const PDC_base_csm *csm,
		   PDC_base_ed *ed, const char *s, PDC_charset char_set, const char *whatfn, int safe)
{
  PDC_string  p_s;
  PDC_string *p_s_ptr = 0;

  if (s) {
    p_s.str = (char*)s;
    p_s.len = strlen(s);
    p_s_ptr = &p_s;
  }
  return PDCI_str_lit_read(pdc, csm, ed, p_s_ptr, char_set, whatfn, safe);
}

PDC_error_t
PDCI_countX(PDC_t *pdc, const PDC_base_csm *csm, PDC_uint8 x, int eor_required,
	    PDC_base_ed *ed, PDC_int32 *res_out, PDC_charset char_set, const char *whatfn, int safe)
{
  PDC_int32       count = 0;
  PDC_byte       *begin, *p1, *p2, *end;
  int             eor, eof;
  size_t          bytes;
  int             matchlen = -1;
  PDC_byte       *tmp;
  PDC_base_csm    csmt;
  PDC_base_ed     edt;

  if (safe) {
    PDCI_IODISC_INIT_CHECKS(whatfn);
    PDCI_NULLPARAM_CHECK(whatfn, res_out);
    if (!csm) {
      csmt = PDC_CheckAndSet;
      csm = &csmt;
    }
    if (!ed) {
      ed = &edt;
    }
  }
  PDC_TRACE5(pdc->disc, "PDCI_countX called, args: x = %s eor_required = %d, char_set %s, whatfn = %s, safe = %d",
	     PDC_qfmt_char(x), eor_required, PDC_charset2str(char_set), whatfn, safe);
  switch (char_set)
    {
    case PDC_charset_ASCII:
      break;
    case PDC_charset_EBCDIC:
      x = PDC_mod_ae_tab[(int)x]; /* convert to EBCDIC char */
      break;
    default:
      goto invalid_charset;
    }
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
      if (pdc->speclev == 0) {
	PDC_WARN1(pdc->disc, "%s: scan terminated early due to disc->stop_maxlen", whatfn);
      }
      break;
    }
    if (pdc->disc->stop_regexp && PDCI_regexpMatch(pdc, pdc->disc->stop_regexp, p1, end, char_set)) {
      if (pdc->speclev == 0) {
	PDC_WARN1(pdc->disc, "%s: scan terminated early due to disc->stop_regexp", whatfn);
      }
      break;
    }
    p1++;
  }
  if (eor_required && !eor && eof) { /* EOF encountered first, error */
    PDCI_READFN_SET_LOC_BE(0, p1-begin);
    PDCI_READFN_RET_ERRCODE_WARN(whatfn, 0, PDC_EOF_BEFORE_EOR);
  }
  /* hit EOR/EOF/stop restriction */
  (*res_out) = count;
  ed->errCode = PDC_NO_ERR;
  return PDC_OK;

 invalid_charset:
  PDCI_READFN_SET_NULLSPAN_LOC(0);
  PDCI_READFN_RET_ERRCODE_WARN(whatfn, 0, PDC_INVALID_CHARSET);

 fatal_nb_io_err:
  PDCI_READFN_RET_ERRCODE_FATAL(whatfn, "IO error (nb)", PDC_IO_ERR);

 fatal_mb_io_err:
  PDCI_READFN_RET_ERRCODE_FATAL(whatfn, "IO error (mb)", PDC_IO_ERR);
}

PDC_error_t
PDCI_countXtoY(PDC_t *pdc, const PDC_base_csm *csm, PDC_uint8 x, PDC_uint8 y,
	       PDC_base_ed *ed, PDC_int32 *res_out, PDC_charset char_set, const char *whatfn, int safe)
{
  PDC_int32       count = 0;
  PDC_byte       *begin, *p1, *p2, *end;
  int             eor, eof;
  size_t          bytes;
  int             matchlen = -1;
  PDC_byte       *tmp;
  PDC_base_csm    csmt;
  PDC_base_ed     edt;

  if (safe) {
    PDCI_IODISC_INIT_CHECKS(whatfn);
    PDCI_NULLPARAM_CHECK(whatfn, res_out);
    if (!csm) {
      csmt = PDC_CheckAndSet;
      csm = &csmt;
    }
    if (!ed) {
      ed = &edt;
    }
  }
  PDC_TRACE5(pdc->disc, "PDCI_countXtoY called, args: x = %s y = %s, char_set %s, whatfn = %s, safe = %d",
	     PDC_qfmt_char(x), PDC_qfmt_char(y), PDC_charset2str(char_set), whatfn, safe);
  switch (char_set)
    {
    case PDC_charset_ASCII:
      break;
    case PDC_charset_EBCDIC:
      x = PDC_mod_ae_tab[(int)x]; /* convert to EBCDIC char */
      y = PDC_mod_ae_tab[(int)y]; /* convert to EBCDIC char */
      break;
    default:
      goto invalid_charset;
    }
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
      if (pdc->speclev == 0) {
	PDC_WARN1(pdc->disc, "%s: scan terminated early due to disc->stop_maxlen", whatfn);
      }
      break;
    }
    if (pdc->disc->stop_regexp && PDCI_regexpMatch(pdc, pdc->disc->stop_regexp, p1, end, char_set)) {
      if (pdc->speclev == 0) {
	PDC_WARN1(pdc->disc, "%s: scan terminated early due to disc->stop_regexp", whatfn);
      }
      break;
    }
    p1++;
  }
  goto not_found; /* y not found */

 invalid_charset:
  PDCI_READFN_SET_NULLSPAN_LOC(0);
  PDCI_READFN_RET_ERRCODE_WARN(whatfn, 0, PDC_INVALID_CHARSET);

 not_found:
  PDCI_READFN_SET_LOC_BE(0, p1-begin);
  PDCI_READFN_RET_ERRCODE_WARN(whatfn, 0, PDC_CHAR_LIT_NOT_FOUND);

 fatal_nb_io_err:
  PDCI_READFN_RET_ERRCODE_FATAL(whatfn, "IO error (nb)", PDC_IO_ERR);

 fatal_mb_io_err:
  PDCI_READFN_RET_ERRCODE_FATAL(whatfn, "IO error (mb)", PDC_IO_ERR);
}

PDC_error_t
PDCI_date_read(PDC_t *pdc, const PDC_base_csm *csm, PDC_char stopChar,
	       PDC_base_ed *ed, PDC_uint32 *res_out, PDC_charset char_set, const char *whatfn, int safe)
{
  PDC_string     *s = &pdc->stmp1;
  time_t          tm;
  PDC_byte       *tmp;
  size_t          width;
  PDC_base_csm    csmt;
  PDC_base_ed     edt;

  if (safe) { 
    PDCI_IODISC_INIT_CHECKS(whatfn);
    if (!csm) {
      csmt = PDC_CheckAndSet;
      csm = &csmt;
    }
    if (!ed) {
      ed = &edt;
    }
  }
  PDC_TRACE4(pdc->disc, "PDCI_date_read called, args: stopChar %s char_set %s, whatfn = %s, safe = %d",
	     PDC_qfmt_char(stopChar), PDC_charset2str(char_set), whatfn, safe);
  if (PDC_ERR == PDCI_string_read(pdc, csm, stopChar, ed, s, char_set, whatfn, 0)) {
    return PDC_ERR;
  }
  PDCI_STR_PRESERVE(s); /* this ensures s.str is null terminated */
  width = s->len;
  tm = tmdate(s->str, (char**)&tmp, NiL);
  if (!tmp || (char*)tmp - s->str != width) {
    PDCI_READFN_SET_LOC_BE(-width, 0);
    PDCI_READFN_RET_ERRCODE_WARN(whatfn, 0, PDC_INVALID_DATE);
  }
  (*res_out) = tm;
  PDC_DBG4(pdc->disc, "%s: converted string %s => %s (secs = %lu)", whatfn, PDC_qfmt_str(s), fmttime("%K", (time_t)(*res_out)), (unsigned long)(*res_out));
  return PDC_OK;

 fatal_alloc_err:
  PDCI_READFN_RET_ERRCODE_FATAL(whatfn, "Memory alloc error", PDC_ALLOC_ERR);
}

PDC_error_t
PDCI_char_read(PDC_t *pdc, const PDC_base_csm *csm,
	       PDC_base_ed *ed, PDC_char *c_out, PDC_charset char_set,
	       const char *whatfn, int safe)
{
  PDC_byte       *begin, *p1, *p2, *end;
  int             eor, eof;
  size_t          bytes;
  PDC_base_csm    csmt;
  PDC_base_ed     edt;

  if (safe) {
    PDCI_IODISC_INIT_CHECKS(whatfn);
    if (!csm) {
      csmt = PDC_CheckAndSet;
      csm = &csmt;
    }
    if (!ed) {
      ed = &edt;
    }
  }
  PDC_TRACE3(pdc->disc, "PDCI_char_read called, char_set = %s, whatfn = %s, safe = %d",
	     PDC_charset2str(char_set), whatfn, safe);
  if (PDC_ERR == PDCI_IO_needbytes(pdc, &begin, &p1, &p2, &end, &eor, &eof, &bytes)) {
    goto fatal_nb_io_err;
  }
  if (bytes == 0) {
    goto width_not_avail;
  }
  if (c_out && *csm == PDC_CheckAndSet) {
    switch (char_set)
      {
      case PDC_charset_ASCII:
	(*c_out) = *begin;
	break;
      case PDC_charset_EBCDIC:
	(*c_out) = PDC_ea_tab[(int)(*begin)];
	break;
      default:
	goto invalid_charset;
      }
  }
  if (PDC_ERR == PDCI_IO_forward(pdc, 1)) {
    goto fatal_forward_err;
  }
  ed->errCode = PDC_NO_ERR;
  return PDC_OK;

 invalid_charset:
  PDCI_READFN_SET_NULLSPAN_LOC(0);
  PDCI_READFN_RET_ERRCODE_WARN(whatfn, 0, PDC_INVALID_CHARSET);

 width_not_avail:
  PDCI_READFN_SET_LOC_BE(0, end-begin);
  PDCI_READFN_RET_ERRCODE_WARN(whatfn, 0, PDC_WIDTH_NOT_AVAILABLE);

 fatal_nb_io_err:
  PDCI_READFN_RET_ERRCODE_FATAL(whatfn, "IO error (nb)", PDC_IO_ERR);

 fatal_forward_err:
  PDCI_READFN_RET_ERRCODE_FATAL(whatfn, "IO_forward error", PDC_FORWARD_ERR);
}

PDC_error_t
PDCI_string_FW_read(PDC_t *pdc, const PDC_base_csm *csm, size_t width,
		    PDC_base_ed *ed, PDC_string *s_out, PDC_charset char_set,
		    const char *whatfn, int safe)
{
  PDC_byte        *begin, *p1, *p2, *end;
  int             eor, eof;
  size_t          bytes;
  PDC_base_csm    csmt;
  PDC_base_ed     edt;

  if (safe) {
    PDCI_IODISC_INIT_CHECKS(whatfn);
    if (!csm) {
      csmt = PDC_CheckAndSet;
      csm = &csmt;
    }
    if (!ed) {
      ed = &edt;
    }
  }
  PDC_TRACE3(pdc->disc, "PDCI_string_FW_read called, char_set = %s, whatfn = %s, safe = %d",
	     PDC_charset2str(char_set), whatfn, safe);
  if (width <= 0) {
    PDC_WARN1(pdc->disc, "%s: UNEXPECTED PARAM VALUE: width <= 0", whatfn);
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
  switch (char_set)
    {
    case PDC_charset_ASCII:
      PDCI_A_STR_SET(s_out, (char*)begin, (char*)end);
      break;
    case PDC_charset_EBCDIC:
      PDCI_E_STR_SET(s_out, (char*)begin, (char*)end);
      /* convert EBCDIC to ASCII */  
      for (p2 = (PDC_byte*)s_out->str, end = p2 + s_out->len; p2 < end; p2++) {
	(*p2) = PDC_ea_tab[(int)(*p2)];
      }
      break;
    default:
      goto invalid_charset;
    }
  if (PDC_ERR == PDCI_IO_forward(pdc, width)) {
    goto fatal_forward_err;
  }
  ed->errCode = PDC_NO_ERR;
  return PDC_OK;

 invalid_charset:
  PDCI_READFN_SET_NULLSPAN_LOC(0);
  PDCI_READFN_RET_ERRCODE_WARN(whatfn, 0, PDC_INVALID_CHARSET);

 bad_param_err:
  PDCI_READFN_SET_NULLSPAN_LOC(0);
  PDCI_READFN_RET_ERRCODE_WARN(whatfn, 0, PDC_BAD_PARAM);

 width_not_avail:
  PDCI_READFN_SET_LOC_BE(0, end-begin);
  PDCI_READFN_RET_ERRCODE_WARN(whatfn, 0, PDC_WIDTH_NOT_AVAILABLE);

 fatal_alloc_err:
  PDCI_READFN_RET_ERRCODE_FATAL(whatfn, "Memory alloc error", PDC_ALLOC_ERR);

 fatal_nb_io_err:
  PDCI_READFN_RET_ERRCODE_FATAL(whatfn, "IO error (nb)", PDC_IO_ERR);

 fatal_mb_io_err:
  PDCI_READFN_RET_ERRCODE_FATAL(whatfn, "IO error (mb)", PDC_IO_ERR);

 fatal_forward_err:
  PDCI_READFN_RET_ERRCODE_FATAL(whatfn, "IO_forward error", PDC_FORWARD_ERR);
}

PDC_error_t
PDCI_string_read(PDC_t *pdc, const PDC_base_csm *csm, PDC_char stopChar,
		 PDC_base_ed *ed, PDC_string *s_out, PDC_charset char_set,
		 const char *whatfn, int safe)
{
  PDC_byte        *begin, *p1, *p2, *end;
  int             eor, eof;
  size_t          bytes;
  int             matchlen = -1;
  PDC_base_csm    csmt;
  PDC_base_ed     edt;

  if (safe) {
    PDCI_IODISC_INIT_CHECKS(whatfn);
    if (!csm) {
      csmt = PDC_CheckAndSet;
      csm = &csmt;
    }
    if (!ed) {
      ed = &edt;
    }
  }
  PDC_TRACE3(pdc->disc, "PDCI_string_read called, char_set = %s, whatfn = %s, safe = %d",
	     PDC_charset2str(char_set), whatfn, safe);
  switch (char_set)
    {
    case PDC_charset_ASCII:
      break;
    case PDC_charset_EBCDIC:
      stopChar = PDC_mod_ae_tab[(int)stopChar]; /* convert to EBCDIC char */
      break;
    default:
      goto invalid_charset;
    }
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
      switch (char_set)
	{
	case PDC_charset_ASCII:
	  PDCI_A_STR_SET(s_out, (char*)begin, (char*)p1);
	  break;
	case PDC_charset_EBCDIC:
	  PDCI_E_STR_SET(s_out, (char*)begin, (char*)p1);
	  /* convert EBCDIC to ASCII */
	  for (p2 = (PDC_byte*)s_out->str, end = p2 + s_out->len; p2 < end; p2++) {
	    (*p2) = PDC_ea_tab[(int)(*p2)];
	  }
	  break;
	default:
	  goto invalid_charset;
	}
      if (PDC_ERR == PDCI_IO_forward(pdc, p1-begin)) {
	goto fatal_forward_err;
      }
      ed->errCode = PDC_NO_ERR;
      return PDC_OK;
    }
    if (pdc->disc->stop_maxlen && ((p1-begin) >= pdc->disc->stop_maxlen)) {
      if (pdc->speclev == 0) {
	PDC_WARN1(pdc->disc, "%s: scan terminated early due to disc->stop_maxlen", whatfn);
      }
      break;
    }
    if (pdc->disc->stop_regexp && PDCI_regexpMatch(pdc, pdc->disc->stop_regexp, p1, end, char_set)) {
      if (pdc->speclev == 0) {
	PDC_WARN1(pdc->disc, "%s: scan terminated early due to disc->stop_regexp", whatfn);
      }
      break;
    }
    p1++;
  }
  goto not_found;

 invalid_charset:
  PDCI_READFN_SET_NULLSPAN_LOC(0);
  PDCI_READFN_RET_ERRCODE_WARN(whatfn, 0, PDC_INVALID_CHARSET);

 not_found:
  PDCI_READFN_SET_LOC_BE(0, p1-begin);
  PDCI_READFN_RET_ERRCODE_WARN(whatfn, 0, PDC_CHAR_LIT_NOT_FOUND);

 fatal_alloc_err:
  PDCI_READFN_RET_ERRCODE_FATAL(whatfn, "Memory alloc error", PDC_ALLOC_ERR);

 fatal_nb_io_err:
  PDCI_READFN_RET_ERRCODE_FATAL(whatfn, "IO error (nb)", PDC_IO_ERR);

 fatal_mb_io_err:
  PDCI_READFN_RET_ERRCODE_FATAL(whatfn, "IO error (mb)", PDC_IO_ERR);

 fatal_forward_err:
  PDCI_READFN_RET_ERRCODE_FATAL(whatfn, "IO_forward error", PDC_FORWARD_ERR);
}

PDC_error_t
PDCI_string_SE_read(PDC_t *pdc, const PDC_base_csm *csm, const char *stopRegexp,
		    PDC_base_ed *ed, PDC_string *s_out, PDC_charset char_set,
		    const char *whatfn, int safe)
{
  PDC_regexp_t   *compiled_exp;
  PDC_base_csm    csmt;
  PDC_base_ed     edt;

  if (safe) {
    PDCI_IODISC_INIT_CHECKS(whatfn);
    PDCI_NULLPARAM_CHECK(whatfn, stopRegexp);
    if (!csm) {
      csmt = PDC_CheckAndSet;
      csm = &csmt;
    }
    if (!ed) {
      ed = &edt;
    }
  }
  if (PDC_ERR == PDCI_regexp_compile(pdc, stopRegexp, &compiled_exp, whatfn)) {
    goto bad_exp;
  }
  return PDCI_string_CSE_read(pdc, csm, compiled_exp, ed, s_out, char_set, whatfn, 0);

 bad_exp:
  PDCI_READFN_SET_NULLSPAN_LOC(0);
  /* regexp_compile already issued a warning */
  /* PDCI_READFN_RET_ERRCODE_WARN("PDC_e_string_SE_read", 0, PDC_INVALID_REGEXP); */
  PDCI_READFN_RET_ERRCODE_NOWARN(PDC_INVALID_REGEXP);
}

PDC_error_t
PDCI_string_CSE_read(PDC_t *pdc, const PDC_base_csm *csm, PDC_regexp_t *stopRegexp,
		     PDC_base_ed *ed, PDC_string *s_out, PDC_charset char_set,
		     const char *whatfn, int safe)
{
  PDC_byte        *begin, *p1, *p2, *end;
  int             eor, eof;
  size_t          bytes;
  int             matchlen;
  PDC_base_csm    csmt;
  PDC_base_ed     edt;

  if (safe) {
    PDCI_IODISC_INIT_CHECKS(whatfn);
    PDCI_NULLPARAM_CHECK(whatfn, stopRegexp);
    if (!csm) {
      csmt = PDC_CheckAndSet;
      csm = &csmt;
    }
    if (!ed) {
      ed = &edt;
    }
  }
  PDC_TRACE3(pdc->disc, "PDCI_string_CSE_read called, char_set = %s, whatfn = %s, safe = %d",
	     PDC_charset2str(char_set), whatfn, safe);
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
	goto found;
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
    if (PDCI_regexpMatch(pdc, stopRegexp, p1, end, char_set)) {
      goto found;
    }
    if (pdc->disc->stop_maxlen && ((p1-begin) >= pdc->disc->stop_maxlen)) {
      if (pdc->speclev == 0) {
	PDC_WARN1(pdc->disc, "%s: scan terminated early due to disc->stop_maxlen", whatfn);
      }
      break;
    }
    if (pdc->disc->stop_regexp && PDCI_regexpMatch(pdc, pdc->disc->stop_regexp, p1, end, char_set)) {
      if (pdc->speclev == 0) {
	PDC_WARN1(pdc->disc, "%s: scan terminated early due to disc->stop_regexp", whatfn);
      }
      break;
    }
    p1++;
  }
  goto not_found;

 found:
  switch (char_set) 
    {
    case PDC_charset_ASCII:
      PDCI_A_STR_SET(s_out, (char*)begin, (char*)p1);
      break;
    case PDC_charset_EBCDIC:
      PDCI_E_STR_SET(s_out, (char*)begin, (char*)p1);
      /* convert EBCDIC to ASCII */
      for (p2 = (PDC_byte*)s_out->str, end = p2 + s_out->len; p2 < end; p2++) {
	(*p2) = PDC_ea_tab[(int)(*p2)];
      }
      break;
    default:
      goto invalid_charset;
    }
  if (PDC_ERR == PDCI_IO_forward(pdc, p1-begin)) {
    goto fatal_forward_err;
  }
  ed->errCode = PDC_NO_ERR;
  return PDC_OK;

 invalid_charset:
  PDCI_READFN_SET_NULLSPAN_LOC(0);
  PDCI_READFN_RET_ERRCODE_WARN(whatfn, 0, PDC_INVALID_CHARSET);

 not_found:
  PDCI_READFN_SET_LOC_BE(0, p1-begin);
  PDCI_READFN_RET_ERRCODE_WARN(whatfn, 0, PDC_REGEXP_NOT_FOUND);

 fatal_alloc_err:
  PDCI_READFN_RET_ERRCODE_FATAL(whatfn, "Memory alloc error", PDC_ALLOC_ERR);

 fatal_nb_io_err:
  PDCI_READFN_RET_ERRCODE_FATAL(whatfn, "IO error (nb)", PDC_IO_ERR);

 fatal_mb_io_err:
  PDCI_READFN_RET_ERRCODE_FATAL(whatfn, "IO error (mb)", PDC_IO_ERR);

 fatal_forward_err:
  PDCI_READFN_RET_ERRCODE_FATAL(whatfn, "IO_forward error", PDC_FORWARD_ERR);
}

/* ================================================================================ */
/* CHARSET INTERNAL WRITE ROUTINES */

ssize_t
PDCI_char_lit_write2io(PDC_t *pdc, Sfio_t *io, PDC_char c,
		       PDC_charset char_set, const char *whatfn, int safe)
{
  if (safe) {
    PDCI_DISC_INIT_CHECKS_RET_SSIZE(whatfn);
    PDCI_NULLPARAM_CHECK_RET_SSIZE(whatfn, io);
  }
  PDC_TRACE4(pdc->disc, "PDCI_char_lit_write2io args: c %s, char_set = %s, whatfn = %s, safe = %d",
	     PDC_qfmt_char(c), PDC_charset2str(char_set), whatfn, safe);
  switch (char_set)
    {
    case PDC_charset_ASCII:
      break;
    case PDC_charset_EBCDIC:
      c = PDC_mod_ae_tab[(int)c]; /* convert to EBCDIC char */
      break;
    default:
      goto invalid_charset;
    }
  if (c != sfputc(io, c)) {
    PDC_WARN1(pdc->disc, "%s: low-level sfputc failure", whatfn);
    return -1;
  }
  return 1;

 invalid_charset:
  PDCI_report_err(pdc, PDC_WARN_FLAGS, 0, PDC_INVALID_CHARSET, whatfn, 0);
  return -1;
}

ssize_t
PDCI_char_lit_write2buf(PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, PDC_char c,
			PDC_charset char_set, const char *whatfn, int safe)
{
  if (safe) {
    PDCI_DISC_INIT_CHECKS_RET_SSIZE(whatfn);
    PDCI_NULLPARAM_CHECK_RET_SSIZE(whatfn, buf);
    PDCI_NULLPARAM_CHECK_RET_SSIZE(whatfn, buf_full);
  }
  PDC_TRACE4(pdc->disc, "PDCI_char_lit_write2buf args: c %s, char_set = %s, whatfn = %s, safe = %d",
	     PDC_qfmt_char(c), PDC_charset2str(char_set), whatfn, safe);
  switch (char_set)
    {
    case PDC_charset_ASCII:
      break;
    case PDC_charset_EBCDIC:
      c = PDC_mod_ae_tab[(int)c]; /* convert to EBCDIC char */
      break;
    default:
      goto invalid_charset;
    }
  if (1 > buf_len) {
    (*buf_full) = 1;
    return -1;
  }
  *buf = c;
  return 1;

 invalid_charset:
  PDCI_report_err(pdc, PDC_WARN_FLAGS, 0, PDC_INVALID_CHARSET, whatfn, 0);
  return -1;
}

ssize_t
PDCI_str_lit_write2io(PDC_t *pdc, Sfio_t *io, const PDC_string *s,
		      PDC_charset char_set, const char *whatfn, int safe)
{
  ssize_t         n;
  PDC_string     *tmp_s = (PDC_string*)s;

  if (safe) {
    PDCI_DISC_INIT_CHECKS_RET_SSIZE(whatfn);
    PDCI_NULLPARAM_CHECK_RET_SSIZE(whatfn, io);
    PDCI_NULLPARAM_CHECK_RET_SSIZE(whatfn, s);
  }
  PDC_TRACE4(pdc->disc, "PDCI_str_lit_write2io args: s %s, char_set = %s, whatfn = %s, safe = %d",
	     PDC_qfmt_str(s), PDC_charset2str(char_set), whatfn, safe);
  switch (char_set)
    {
    case PDC_charset_ASCII:
      break;
    case PDC_charset_EBCDIC:
      tmp_s = &pdc->stmp1;
      PDCI_A2E_STR_CPY(tmp_s, s->str, s->len);
      break;
    default:
      goto invalid_charset;
    }
  n = sfwrite(io, (Void_t*)tmp_s->str, tmp_s->len);
  if (n != tmp_s->len) {
    PDC_WARN1(pdc->disc, "%s: low-level sfwrite failure", whatfn);
    if (n > 0) {
      /* XXX_TODO try to back up ??? */
    }
    return -1;
  }
  return n;

 invalid_charset:
  PDCI_report_err(pdc, PDC_WARN_FLAGS, 0, PDC_INVALID_CHARSET, whatfn, 0);
  return -1;

 fatal_alloc_err:
  PDCI_report_err(pdc, PDC_FATAL_FLAGS, 0, PDC_ALLOC_ERR, whatfn, "Memory alloc error");
  return -1;
}

ssize_t
PDCI_str_lit_write2buf(PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, const PDC_string *s,
		       PDC_charset char_set, const char *whatfn, int safe)
{
  PDC_string     *tmp_s = (PDC_string*)s;

  if (safe) {
    PDCI_DISC_INIT_CHECKS_RET_SSIZE(whatfn);
    PDCI_NULLPARAM_CHECK_RET_SSIZE(whatfn, buf);
    PDCI_NULLPARAM_CHECK_RET_SSIZE(whatfn, buf_full);
    PDCI_NULLPARAM_CHECK_RET_SSIZE(whatfn, s);
  }
  PDC_TRACE4(pdc->disc, "PDCI_str_lit_write2buf args: s %s, char_set = %s, whatfn = %s, safe = %d",
	     PDC_qfmt_str(s), PDC_charset2str(char_set), whatfn, safe);
  switch (char_set)
    {
    case PDC_charset_ASCII:
      break;
    case PDC_charset_EBCDIC:
      tmp_s = &pdc->stmp1;
      PDCI_A2E_STR_CPY(tmp_s, s->str, s->len);
      break;
    default:
      goto invalid_charset;
    }
  if (tmp_s->len > buf_len) {
    (*buf_full) = 1;
    return -1;
  }
  memcpy(buf, tmp_s->str, tmp_s->len);
  return tmp_s->len;

 invalid_charset:
  PDCI_report_err(pdc, PDC_WARN_FLAGS, 0, PDC_INVALID_CHARSET, whatfn, 0);
  return -1;

 fatal_alloc_err:
  PDCI_report_err(pdc, PDC_FATAL_FLAGS, 0, PDC_ALLOC_ERR, whatfn, "Memory alloc error");
  return -1;
}

ssize_t
PDCI_Cstr_lit_write2io(PDC_t *pdc, Sfio_t *io, const char *s,
		       PDC_charset char_set, const char *whatfn, int safe)
{
  ssize_t         n;
  PDC_string      stack_s;
  PDC_string     *tmp_s = &stack_s;

  if (safe) {
    PDCI_DISC_INIT_CHECKS_RET_SSIZE(whatfn);
    PDCI_NULLPARAM_CHECK_RET_SSIZE(whatfn, io);
    PDCI_NULLPARAM_CHECK_RET_SSIZE(whatfn, s);
  }
  PDC_TRACE4(pdc->disc, "PDCI_Cstr_lit_write2io args: s %s, char_set = %s, whatfn = %s, safe = %d",
	     PDC_qfmt_Cstr(s, strlen(s)), PDC_charset2str(char_set), whatfn, safe);
  stack_s.str = (char*)s;
  stack_s.len = strlen(s);
  switch (char_set)
    {
    case PDC_charset_ASCII:
      break;
    case PDC_charset_EBCDIC:
      tmp_s = &pdc->stmp1;
      PDCI_A2E_STR_CPY(tmp_s, s, stack_s.len);
      break;
    default:
      goto invalid_charset;
    }
  n = sfwrite(io, (Void_t*)tmp_s->str, tmp_s->len);
  if (n != tmp_s->len) {
    PDC_WARN1(pdc->disc, "%s: low-level sfwrite failure", whatfn);
    if (n > 0) {
      /* XXX_TODO try to back up ??? */
    }
    return -1;
  }
  return n;

 invalid_charset:
  PDCI_report_err(pdc, PDC_WARN_FLAGS, 0, PDC_INVALID_CHARSET, whatfn, 0);
  return -1;

 fatal_alloc_err:
  PDCI_report_err(pdc, PDC_FATAL_FLAGS, 0, PDC_ALLOC_ERR, whatfn, "Memory alloc error");
  return -1;
}

ssize_t
PDCI_Cstr_lit_write2buf(PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, const char *s,
			PDC_charset char_set, const char *whatfn, int safe)
{
  PDC_string      stack_s;
  PDC_string     *tmp_s = &stack_s;

  if (safe) {
    PDCI_DISC_INIT_CHECKS_RET_SSIZE(whatfn);
    PDCI_NULLPARAM_CHECK_RET_SSIZE(whatfn, buf);
    PDCI_NULLPARAM_CHECK_RET_SSIZE(whatfn, buf_full);
    PDCI_NULLPARAM_CHECK_RET_SSIZE(whatfn, s);
  }
  PDC_TRACE4(pdc->disc, "PDCI_Cstr_lit_write2buf args: s %s, char_set = %s, whatfn = %s, safe = %d",
	     PDC_qfmt_Cstr(s, strlen(s)), PDC_charset2str(char_set), whatfn, safe);
  stack_s.str = (char*)s;
  stack_s.len = strlen(s);
  switch (char_set)
    {
    case PDC_charset_ASCII:
      break;
    case PDC_charset_EBCDIC:
      tmp_s = &pdc->stmp1;
      PDCI_A2E_STR_CPY(tmp_s, s, stack_s.len);
      break;
    default:
      goto invalid_charset;
    }
  if (tmp_s->len > buf_len) {
    (*buf_full) = 1;
    return -1;
  }
  memcpy(buf, tmp_s->str, tmp_s->len);
  return tmp_s->len;

 invalid_charset:
  PDCI_report_err(pdc, PDC_WARN_FLAGS, 0, PDC_INVALID_CHARSET, whatfn, 0);
  return -1;

 fatal_alloc_err:
  PDCI_report_err(pdc, PDC_FATAL_FLAGS, 0, PDC_ALLOC_ERR, whatfn, "Memory alloc error");
  return -1;
}

ssize_t
PDCI_char_write2io(PDC_t *pdc, Sfio_t *io, const PDC_base_ed *ed, PDC_byte *in,
		   PDC_charset char_set, const char *whatfn, int safe)
{
  PDC_char      c;
  PDC_inv_valfn fn;
  void         *type_args[1] = { 0 };

  if (safe) {
    PDCI_DISC_INIT_CHECKS_RET_SSIZE(whatfn);
    PDCI_NULLPARAM_CHECK_RET_SSIZE(whatfn, io);
  }
  PDC_TRACE4(pdc->disc, "PDCI_char_write2io args: c %s, char_set = %s, whatfn = %s, safe = %d",
	     PDC_qfmt_char(*in), PDC_charset2str(char_set), whatfn, safe);
  if (ed->errCode == PDC_NO_ERR) {
    c = *in;
  } else {
    fn = PDCI_GET_INV_VALFN(pdc, "PDC_char");
    if (!fn || (PDC_ERR == fn(pdc, (void*)ed, (void*)&c, type_args))) {
      c = (ed->errCode == PDC_USER_CONSTRAINT_VIOLATION) ? *in : PDC_CHAR_DEF_INV_VAL;
    }
  }
  switch (char_set)
    {
    case PDC_charset_ASCII:
      break;
    case PDC_charset_EBCDIC:
      c = PDC_mod_ae_tab[(int)c]; /* convert to EBCDIC char */
      break;
    default:
      goto invalid_charset;
    }
  if (c != sfputc(io, c)) {
    PDC_WARN1(pdc->disc, "%s: low-level sfputc failure", whatfn);
    return -1;
  }
  return 1;

 invalid_charset:
  PDCI_report_err(pdc, PDC_WARN_FLAGS, 0, PDC_INVALID_CHARSET, whatfn, 0);
  return -1;
}

ssize_t
PDCI_char_write2buf(PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full,
		    const PDC_base_ed *ed, PDC_byte *in,
		    PDC_charset char_set, const char *whatfn, int safe)
{
  PDC_char      c;
  PDC_inv_valfn fn;
  void         *type_args[1] = { 0 };

  if (safe) {
    PDCI_DISC_INIT_CHECKS_RET_SSIZE(whatfn);
    PDCI_NULLPARAM_CHECK_RET_SSIZE(whatfn, buf);
    PDCI_NULLPARAM_CHECK_RET_SSIZE(whatfn, buf_full);
  }
  PDC_TRACE4(pdc->disc, "PDCI_char_write2buf args: c %s, char_set = %s, whatfn = %s, safe = %d",
	     PDC_qfmt_char(*in), PDC_charset2str(char_set), whatfn, safe);
  if (ed->errCode == PDC_NO_ERR) {
    c = *in;
  } else {
    fn = PDCI_GET_INV_VALFN(pdc, "PDC_char");
    if (!fn || (PDC_ERR == fn(pdc, (void*)ed, (void*)&c, type_args))) {
      c = (ed->errCode == PDC_USER_CONSTRAINT_VIOLATION) ? *in : PDC_CHAR_DEF_INV_VAL;
    }
  }
  switch (char_set)
    {
    case PDC_charset_ASCII:
      break;
    case PDC_charset_EBCDIC:
      c = PDC_mod_ae_tab[(int)c]; /* convert to EBCDIC char */
      break;
    default:
      goto invalid_charset;
    }
  if (1 > buf_len) {
    (*buf_full) = 1;
    return -1;
  }
  *buf = c;
  return 1;

 invalid_charset:
  PDCI_report_err(pdc, PDC_WARN_FLAGS, 0, PDC_INVALID_CHARSET, whatfn, 0);
  return -1;
}

ssize_t
PDCI_string_write2io(PDC_t *pdc, Sfio_t *io, PDC_char stopChar, const PDC_base_ed *ed, PDC_string *s,
		     PDC_charset char_set, const char *whatfn, int safe)
{
  ssize_t         n;
  PDC_string     *tmp_s = (PDC_string*)s;
  PDC_inv_valfn   fn;
  void           *type_args[1] = { 0 };

  if (safe) {
    PDCI_DISC_INIT_CHECKS_RET_SSIZE(whatfn);
    PDCI_NULLPARAM_CHECK_RET_SSIZE(whatfn, io);
    PDCI_NULLPARAM_CHECK_RET_SSIZE(whatfn, s);
  }
  PDC_TRACE4(pdc->disc, "PDCI_string_write2io args: s %s, char_set = %s, whatfn = %s, safe = %d",
	     PDC_qfmt_str(s), PDC_charset2str(char_set), whatfn, safe);
  if (ed->errCode != PDC_NO_ERR) {
    fn = PDCI_GET_INV_VALFN(pdc, "PDC_string");
    if (!fn || (PDC_ERR == fn(pdc, (void*)ed, (void*)s, type_args))) {
      if (ed->errCode != PDC_USER_CONSTRAINT_VIOLATION) {
	s->len = 0;
      }
    }
  }
  switch (char_set)
    {
    case PDC_charset_ASCII:
      break;
    case PDC_charset_EBCDIC:
      tmp_s = &pdc->stmp1;
      PDCI_A2E_STR_CPY(tmp_s, s->str, s->len);
      break;
    default:
      goto invalid_charset;
    }
  n = sfwrite(io, (Void_t*)tmp_s->str, tmp_s->len);
  if (n != tmp_s->len) {
    PDC_WARN1(pdc->disc, "%s: low-level sfwrite failure", whatfn);
    if (n > 0) {
      /* XXX_TODO try to back up ??? */
    }
    return -1;
  }
  return n;

 invalid_charset:
  PDCI_report_err(pdc, PDC_WARN_FLAGS, 0, PDC_INVALID_CHARSET, whatfn, 0);
  return -1;

 fatal_alloc_err:
  PDCI_report_err(pdc, PDC_FATAL_FLAGS, 0, PDC_ALLOC_ERR, whatfn, "Memory alloc error");
  return -1;
}

ssize_t
PDCI_string_write2buf(PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full,
		      PDC_char stopChar, const PDC_base_ed *ed, PDC_string *s,
		      PDC_charset char_set, const char *whatfn, int safe)
{
  PDC_string     *tmp_s = (PDC_string*)s;
  PDC_inv_valfn   fn;
  void           *type_args[1] = { 0 };

  if (safe) {
    PDCI_DISC_INIT_CHECKS_RET_SSIZE(whatfn);
    PDCI_NULLPARAM_CHECK_RET_SSIZE(whatfn, buf);
    PDCI_NULLPARAM_CHECK_RET_SSIZE(whatfn, buf_full);
    PDCI_NULLPARAM_CHECK_RET_SSIZE(whatfn, s);
  }
  PDC_TRACE4(pdc->disc, "PDCI_string_write2buf args: s %s, char_set = %s, whatfn = %s, safe = %d",
	     PDC_qfmt_str(s), PDC_charset2str(char_set), whatfn, safe);
  if (ed->errCode != PDC_NO_ERR) {
    fn = PDCI_GET_INV_VALFN(pdc, "PDC_string");
    if (!fn || (PDC_ERR == fn(pdc, (void*)ed, (void*)s, type_args))) {
      if (ed->errCode != PDC_USER_CONSTRAINT_VIOLATION) {
	s->len = 0;
      }
    }
  }
  switch (char_set)
    {
    case PDC_charset_ASCII:
      break;
    case PDC_charset_EBCDIC:
      tmp_s = &pdc->stmp1;
      PDCI_A2E_STR_CPY(tmp_s, s->str, s->len);
      break;
    default:
      goto invalid_charset;
    }
  if (tmp_s->len > buf_len) {
    (*buf_full) = 1;
    return -1;
  }
  memcpy(buf, tmp_s->str, tmp_s->len);
  return tmp_s->len;

 invalid_charset:
  PDCI_report_err(pdc, PDC_WARN_FLAGS, 0, PDC_INVALID_CHARSET, whatfn, 0);
  return -1;

 fatal_alloc_err:
  PDCI_report_err(pdc, PDC_FATAL_FLAGS, 0, PDC_ALLOC_ERR, whatfn, "Memory alloc error");
  return -1;
}

ssize_t
PDCI_string_FW_write2io(PDC_t *pdc, Sfio_t *io,
			size_t width, const PDC_base_ed *ed, PDC_string *s,
			PDC_charset char_set, const char *whatfn, int safe)
{
  ssize_t         n;
  PDC_string     *tmp_s = (PDC_string*)s;
  PDC_inv_valfn   fn;
  void           *type_args[2] = { (void*)(&width), 0 };

  if (safe) {
    PDCI_DISC_INIT_CHECKS_RET_SSIZE(whatfn);
    PDCI_NULLPARAM_CHECK_RET_SSIZE(whatfn, io);
    PDCI_NULLPARAM_CHECK_RET_SSIZE(whatfn, s);
  }
  PDC_TRACE4(pdc->disc, "PDCI_string_write2io args: s %s, char_set = %s, whatfn = %s, safe = %d",
	     PDC_qfmt_str(s), PDC_charset2str(char_set), whatfn, safe);
  if (ed->errCode != PDC_NO_ERR) {
    fn = PDCI_GET_INV_VALFN(pdc, "PDC_string_FW");
    if (!fn || (PDC_ERR == fn(pdc, (void*)ed, (void*)s, type_args))) {
      if (ed->errCode != PDC_USER_CONSTRAINT_VIOLATION) {
	PDCI_STRFILL(s, PDC_CHAR_DEF_INV_VAL, width);
      }
    }
  }
  if (s->len != width) {
    goto invalid_width;
  }
  switch (char_set)
    {
    case PDC_charset_ASCII:
      break;
    case PDC_charset_EBCDIC:
      tmp_s = &pdc->stmp1;
      PDCI_A2E_STR_CPY(tmp_s, s->str, s->len);
      break;
    default:
      goto invalid_charset;
    }
  n = sfwrite(io, (Void_t*)tmp_s->str, tmp_s->len);
  if (n != tmp_s->len) {
    PDC_WARN1(pdc->disc, "%s: low-level sfwrite failure", whatfn);
    if (n > 0) {
      /* XXX_TODO try to back up ??? */
    }
    return -1;
  }
  return n;

 invalid_width:
  PDCI_report_err(pdc, PDC_WARN_FLAGS, 0, PDC_INVALID_WIDTH, whatfn, 0);
  return -1;

 invalid_charset:
  PDCI_report_err(pdc, PDC_WARN_FLAGS, 0, PDC_INVALID_CHARSET, whatfn, 0);
  return -1;

 fatal_alloc_err:
  PDCI_report_err(pdc, PDC_FATAL_FLAGS, 0, PDC_ALLOC_ERR, whatfn, "Memory alloc error");
  return -1;
}

ssize_t
PDCI_string_FW_write2buf(PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full,
			 size_t width, const PDC_base_ed *ed, PDC_string *s,
			 PDC_charset char_set, const char *whatfn, int safe)
{
  PDC_string     *tmp_s = (PDC_string*)s;
  PDC_inv_valfn   fn;
  void           *type_args[2] = { (void*)(&width), 0 };

  if (safe) {
    PDCI_DISC_INIT_CHECKS_RET_SSIZE(whatfn);
    PDCI_NULLPARAM_CHECK_RET_SSIZE(whatfn, buf);
    PDCI_NULLPARAM_CHECK_RET_SSIZE(whatfn, buf_full);
    PDCI_NULLPARAM_CHECK_RET_SSIZE(whatfn, s);
  }
  PDC_TRACE4(pdc->disc, "PDCI_string_write2buf args: s %s, char_set = %s, whatfn = %s, safe = %d",
	     PDC_qfmt_str(s), PDC_charset2str(char_set), whatfn, safe);
  if (ed->errCode != PDC_NO_ERR) {
    fn = PDCI_GET_INV_VALFN(pdc, "PDC_string_FW");
    if (!fn || (PDC_ERR == fn(pdc, (void*)ed, (void*)s, type_args))) {
      if (ed->errCode != PDC_USER_CONSTRAINT_VIOLATION) {
	PDCI_STRFILL(s, PDC_CHAR_DEF_INV_VAL, width);
      }
    }
  }
  if (s->len != width) {
    goto invalid_width;
  }
  switch (char_set)
    {
    case PDC_charset_ASCII:
      break;
    case PDC_charset_EBCDIC:
      tmp_s = &pdc->stmp1;
      PDCI_A2E_STR_CPY(tmp_s, s->str, s->len);
      break;
    default:
      goto invalid_charset;
    }
  if (tmp_s->len > buf_len) {
    (*buf_full) = 1;
    return -1;
  }
  memcpy(buf, tmp_s->str, tmp_s->len);
  return tmp_s->len;

 invalid_width:
  PDCI_report_err(pdc, PDC_WARN_FLAGS, 0, PDC_INVALID_WIDTH, whatfn, 0);
  return -1;

 invalid_charset:
  PDCI_report_err(pdc, PDC_WARN_FLAGS, 0, PDC_INVALID_CHARSET, whatfn, 0);
  return -1;

 fatal_alloc_err:
  PDCI_report_err(pdc, PDC_FATAL_FLAGS, 0, PDC_ALLOC_ERR, whatfn, "Memory alloc error");
  return -1;
}

/* ================================================================================ */
/* INTERNAL MISC ROUTINES */

PDC_error_t
PDCI_regexp_compile(PDC_t *pdc, const char *regexp, PDC_regexp_t **regexp_out, const char *whatfn)
{
  PDC_regexp_t *res;
  size_t        last;
  int           eor = 0, just_eor = 0;

  if (!regexp_out) {
    PDC_WARN1(pdc->disc, "%s: regexp_out cannot be NULL", whatfn);
    return PDC_ERR;
  }
  if (!regexp || !(*regexp)) {
    PDC_WARN1(pdc->disc, "%s, null regular expression specified", whatfn);
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
    PDC_WARN2(pdc->disc,
	      "%s: Invalid regular expression: %s\n"
	      "    currently only support the forms \"EOR\", \"[<chars>]\", and \"[<chars>]|EOR\"",
	      whatfn, PDC_qfmt_Cstr(regexp, strlen(regexp)));
    return PDC_ERR;
  }

 done:
  if (!(res = vmnewof(pdc->vm, 0, PDC_regexp_t, 1, last-1))) {
    PDC_FATAL1(pdc->disc, "%s: Memory alloc error [regexp]", whatfn);
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

size_t
PDCI_regexpMatch(PDC_t *pdc, PDC_regexp_t *regexp, PDC_byte *begin, PDC_byte *end, PDC_charset char_set)
{
  PDC_byte match_char; 
  if (!begin || !begin[0]) {
    return 0;
  }
  switch (char_set)
    {
    case PDC_charset_ASCII:
      match_char = begin[0];
      break;
    case PDC_charset_EBCDIC:
      match_char = PDC_ea_tab[(int)begin[0]];
      break;
    default:
      /* should not get here, calling function should already have vetted char_set */
      return 0;
    }
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
