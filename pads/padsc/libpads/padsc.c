## This source file is run through srcgen.pl to produce 
## a number of generated files:
##
##    libpadsc-macros-gen.h      : generally useful macros
##    libpadsc-read-macros-gen.h : macros that help implement read functions
##    libpadsc-acc-macros-gen.h  : macros that help implement accum functions
## 
##    libpadsc-read-gen.c        : generated read functions
##    libpadsc-acc-gen.c         : generated accum functions
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
#define PDCI_READFN_RET_ERRCODE_WARN(msg, errcode)
  do {
    if (pdc->speclev == 0 && (*em < PDC_Ignore)) {
      ed->errCode = (errcode);
      PDCI_report_err(pdc, PDC_WARN_FLAGS, &(ed->loc), (errcode), (msg));
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
#define PDCI_READFN_RET_ERRCODE_FATAL(msg, errcode)
  do {
    if (pdc->speclev == 0 && (*em < PDC_Ignore)) {
      ed->errCode = (errcode);
      PDCI_report_err(pdc, PDC_FATAL_FLAGS, 0, (errcode), (msg));
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

/*
 * If *em is CheckAndSet, create a string copy of the string
 * that goes from begin to end-1.  Must have a no_space label.
 */
 #define PDCI_STR_COPY(s_out, begin, end)
  do {
    if (*em == PDC_CheckAndSet && s_out) {
      size_t wdth = end-begin; 
      char *buf;
      if (!s_out->rbuf) {
	if (!(s_out->rbuf = RMM_new_rbuf(pdc->rmm_nz))) {
	  goto fatal_alloc_err;
	}
      }
      if (RBuf_reserve(s_out->rbuf, (void**)&buf, sizeof(char), wdth+1, PDCI_STRING_HINT)) {
       goto fatal_alloc_err;
      }
      strncpy(buf, begin, wdth);
      s_out->str = buf;
      s_out->len = wdth;
    }
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
    if (!param)  {
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
#define PDCI_AINT_READ_FN(fn_name, targ_type, int_type, strtonum_fn, invalid_err, opt_tmp_test)
fn_name ## _internal (PDC_t *pdc, PDC_base_em *em,
		      PDC_base_ed *ed, targ_type *res_out)
{
  int_type        tmp;   /* tmp num */
  char            *begin, *p1, *p2, *end;
  int             eor, eof;
  size_t          bytes;

  PDC_TRACE(pdc->disc, PDCI_MacroArg2String(fn_name) "_internal called" );
  if (PDC_ERR == PDCI_IO_needbytes(pdc, &begin, &p1, &p2, &end, &eor, &eof, &bytes)) {
    goto fatal_nb_io_err;
  }
  if (bytes == 0) {
    goto at_eor_or_eof_err;
  }
  if (isspace(*p1) && !(pdc->disc->flags & PDC_WSPACE_OK)) {
    goto invalid_wspace;
  }
  while (!(eor|eof)) { /* find a non-space */ 
    while (isspace(*p1)) { p1++; }
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
    while (isdigit(*p1)) { p1++; }
    if (p1 < end) { break; }
    if (PDC_ERR == PDCI_IO_morebytes(pdc, &begin, &p1, &p2, &end, &eor, &eof, &bytes)) {
      goto fatal_mb_io_err;
    }
  }
  /* Either eor|eof, or found non-digit before end.  Thus, */
  /* the range [begin, end] is now set up for the strtonum function */
  tmp = strtonum_fn(begin, &p1, 10);
  if (p1==0 || p1==begin) {
    p1 = begin;
    while (isspace(*p1)) { p1++; }
    if ('-' == (*p1) || '+' == (*p1)) { p1++; }
    while (isdigit(*p1)) { p1++; }
    goto invalid;
  }
  if (errno==ERANGE opt_tmp_test) {
    goto range_err;
  }
  /* success */
  if (PDC_ERR == PDCI_IO_forward(pdc, p1-begin)) {
    goto fatal_forward_err;
  }
  if (res_out && *em == PDC_CheckAndSet) {
    (*res_out) = (targ_type)tmp;
  }
  ed->errCode = PDC_NO_ERR;
  return PDC_OK;

 at_eor_or_eof_err:
  PDCI_READFN_SET_NULLSPAN_LOC(0);
  PDCI_READFN_RET_ERRCODE_WARN(0, eor ? PDC_AT_EOR : PDC_AT_EOF);

 invalid_wspace:
  PDCI_READFN_SET_LOC_BE(0, 1);
  PDCI_READFN_RET_ERRCODE_WARN("spaces not allowed in aint field unless flag PDC_WSPACE_OK is set", invalid_err);

 invalid:
  PDCI_READFN_SET_LOC_BE(0, p1-begin);
  PDCI_READFN_RET_ERRCODE_WARN(0, invalid_err);

 range_err:
  /* range error still consumes the number */
  PDCI_READFN_SET_LOC_BE(0, p1-begin);
  if (PDC_ERR == PDCI_IO_forward(pdc, p1-begin)) {
    goto fatal_forward_err;
  }
  PDCI_READFN_RET_ERRCODE_WARN(0, PDC_RANGE);

 fatal_nb_io_err:
  PDCI_READFN_RET_ERRCODE_FATAL("IO error in " PDCI_MacroArg2String(fn_name) "_internal (nb)", PDC_IO_ERR);

 fatal_mb_io_err:
  PDCI_READFN_RET_ERRCODE_FATAL("IO error in " PDCI_MacroArg2String(fn_name) "_internal (mb)", PDC_IO_ERR);

 fatal_forward_err:
  PDCI_READFN_RET_ERRCODE_FATAL("Internal IO_forward error in " PDCI_MacroArg2String(fn_name) "_internal", PDC_FORWARD_ERR);
}

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

#define PDCI_AINT_FW_READ_FN(fn_name, targ_type, int_type, strtonum_fn, invalid_err, opt_tmp_test)
PDC_error_t
fn_name ## _internal (PDC_t *pdc, PDC_base_em *em, size_t width,
		      PDC_base_ed *ed, targ_type *res_out)
{
  unsigned char   ct;    /* char tmp */
  int_type        tmp;   /* tmp num */
  char            *begin, *p1, *p2, *end;
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
  end = begin + width;
  if (isspace(*begin) && !(pdc->disc->flags & PDC_WSPACE_OK)) {
    goto invalid_wspace;
  }
  ct = *end;    /* save */
  *end = 0;     /* null */
  tmp = strtonum_fn(begin, &p1, 10);
  if (p1==0 || p1==begin) {
    *end = ct;    /* restore */
    goto invalid;
  }
  *end = ct;    /* restore */
  while (isspace(*p1) && !(pdc->disc->flags & PDC_WSPACE_OK)) {
    goto invalid_wspace;
  }
  while (p1 < end && isspace(*p1)) { p1++; } 
  if (p1 != end) {
    goto invalid;
  }
  if (errno==ERANGE opt_tmp_test) {
    goto range_err;
  }
  /* success */
  if (PDC_ERR == PDCI_IO_forward(pdc, width)) {
    goto fatal_forward_err;
  }
  if (res_out && *em == PDC_CheckAndSet) {
    (*res_out) = (targ_type)tmp;
  }
  ed->errCode = PDC_NO_ERR;
  return PDC_OK;

 bad_param_err:
  PDCI_READFN_SET_NULLSPAN_LOC(0);
  PDCI_READFN_RET_ERRCODE_WARN(0, PDC_BAD_PARAM);

 width_not_avail:
  /* FW field: eat the space whether or not there is an error */
  PDCI_READFN_SET_LOC_BE(0, end-begin);
  if (PDC_ERR == PDCI_IO_forward(pdc, end-begin)) {
    goto fatal_forward_err;
  }
  PDCI_READFN_RET_ERRCODE_WARN(0, PDC_WIDTH_NOT_AVAILABLE);

 invalid:
  /* FW field: eat the space whether or not there is an error */
  PDCI_READFN_SET_LOC_BE(0, width);
  if (PDC_ERR == PDCI_IO_forward(pdc, width)) {
    goto fatal_forward_err;
  }
  PDCI_READFN_RET_ERRCODE_WARN(0, invalid_err);

 invalid_wspace:
  /* FW field: eat the space whether or not there is an error */
  PDCI_READFN_SET_LOC_BE(0, width);
  if (PDC_ERR == PDCI_IO_forward(pdc, width)) {
    goto fatal_forward_err;
  }
  PDCI_READFN_RET_ERRCODE_WARN("spaces not allowed in aint field unless flag PDC_WSPACE_OK is set", invalid_err);

 range_err:
  /* FW field: eat the space whether or not there is an error */
  PDCI_READFN_SET_LOC_BE(0, width);
  if (PDC_ERR == PDCI_IO_forward(pdc, width)) {
    goto fatal_forward_err;
  }
  PDCI_READFN_RET_ERRCODE_WARN(0, PDC_RANGE);

 fatal_alloc_err:
  PDCI_READFN_RET_ERRCODE_FATAL("Memory alloc error in " PDCI_MacroArg2String(fn_name) "_internal", PDC_ALLOC_ERR);

 fatal_nb_io_err:
  PDCI_READFN_RET_ERRCODE_FATAL("IO error in " PDCI_MacroArg2String(fn_name) "_internal (nb)", PDC_IO_ERR);

 fatal_mb_io_err:
  PDCI_READFN_RET_ERRCODE_FATAL("IO error in " PDCI_MacroArg2String(fn_name) "_internal (mb)", PDC_IO_ERR);

 fatal_forward_err:
  PDCI_READFN_RET_ERRCODE_FATAL("Internal IO_forward error in " PDCI_MacroArg2String(fn_name) "_internal", PDC_FORWARD_ERR);
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

#define PDCI_BINT_READ_FN(fn_name, targ_type, width, swapmem_op)
PDC_error_t
fn_name ## _internal (PDC_t *pdc, PDC_base_em *em,
		      PDC_base_ed *ed, targ_type *res_out)
{
  char            *begin, *p1, *p2, *end;
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
    if (pdc->disc->m_endian != pdc->disc->d_endian) {
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
  PDCI_READFN_RET_ERRCODE_WARN(0, PDC_WIDTH_NOT_AVAILABLE);

 fatal_nb_io_err:
  PDCI_READFN_RET_ERRCODE_FATAL("IO error in " PDCI_MacroArg2String(fn_name) "_internal (nb)", PDC_IO_ERR);

 fatal_mb_io_err:
  PDCI_READFN_RET_ERRCODE_FATAL("IO error in " PDCI_MacroArg2String(fn_name) "_internal (mb)", PDC_IO_ERR);

 fatal_forward_err:
  PDCI_READFN_RET_ERRCODE_FATAL("Internal IO_forward error in " PDCI_MacroArg2String(fn_name) "_internal", PDC_FORWARD_ERR);
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
  a->good = a->bad = a->fold = a->psum = a->avg = a->min = a->max = 0;
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
  a->good = a->bad = a->fold = a->psum = a->avg = a->min = a->max = 0;
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
int_type ## _acc_report_internal(PDC_t *pdc, Sfio_t *outstr, const char *prefix, const char *what, int nst,
				 int_type ## _acc *a)
{
  const char            *cmt = ""; 
  int                   i = 0, sz, rp;
  PDC_uint64            cnt_sum = 0;
  double                bad_pcnt;
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
    bad_pcnt = a->bad / (double)(a->good + a->bad);
  }
  sfprintf(outstr, "good vals: %10llu    bad vals: %10llu    pcnt-bad: %8.3lf\n",
	   a->good, a->bad, bad_pcnt);
  if (a->good == 0) {
    return PDC_OK;
  }
  int_type ## _acc_fold_psum(a);
  sz = dtsize(a->dict);
  rp = (sz < PDCI_ACC_REPORT_K) ? sz : PDCI_ACC_REPORT_K;
  if (sz == PDCI_ACC_MAX2TRACK) {
    cmt = " (* hit tracking limit *) ";
  }
  dtdisc(a->dict,   &int_type ## _acc_dt_oset_disc, DT_SAMEHASH); /* change cmp function */
  dtmethod(a->dict, Dtoset); /* change to ordered set -- establishes an ordering */
  sfprintf(outstr, "  Characterizing %s:  min %" fmt, what, a->min);
  sfprintf(outstr, " max %" fmt, a->max);
  sfprintf(outstr, " avg %lf\n", a->avg);

  sfprintf(outstr, "    => distribution of top %d values out of %d distinct values%s:\n",
	   rp, sz, cmt);
  for (velt = dtfirst(a->dict); velt && i < PDCI_ACC_REPORT_K; velt = dtnext(a->dict, velt), i++) {
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
  const char            *cmt = ""; 
  int                   i, sz, rp;
  PDC_uint64            cnt_sum = 0;
  double                bad_pcnt;
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
    bad_pcnt = a->bad / (double)(a->good + a->bad);
  }
  sfprintf(outstr, "good vals: %10llu    bad vals: %10llu    pcnt-bad: %8.3lf\n",
	   a->good, a->bad, bad_pcnt);
  if (a->good == 0) {
    return PDC_OK;
  }
  int_type ## _acc_fold_psum(a);
  sz = dtsize(a->dict);
  rp = (sz < PDCI_ACC_REPORT_K) ? sz : PDCI_ACC_REPORT_K;
  if (sz == PDCI_ACC_MAX2TRACK) {
    cmt = " (* hit tracking limit *) ";
  }
  dtdisc(a->dict,   &int_type ## _acc_dt_oset_disc, DT_SAMEHASH); /* change cmp function */
  dtmethod(a->dict, Dtoset); /* change to ordered set -- establishes an ordering */
  mapped_min = fn(a->min);
  mapped_max = fn(a->max);
  sfprintf(outstr, "  Characterizing %s:  min %s (%5" fmt, what, mapped_min, a->min);
  sfprintf(outstr, ")  max %s (%5" fmt, mapped_max, a->max);
  sfprintf(outstr, ")\n");
  sfprintf(outstr, "    => distribution of top %d values out of %d distinct values%s:\n",
	   rp, sz, cmt);
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
    elt_pcnt = ((double)100.0 * elt->key.cnt)/a->good;
    mapped_val = fn(elt->key.val);
    sfprintf(outstr, "        val: %s (%5" fmt, mapped_val, elt->key.val);
    sfprintf(outstr, ") ");
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
 * PDCI_AINT_READ_FN(fn_name, targ_type, int_type, strtonum_fn, invalid_err, opt_tmp_test)
 */

PDCI_AINT_READ_FN(PDC_aint8_read,  PDC_int8,  long,      PDCI_strtol,  PDC_INVALID_AINT,
 || tmp < PDC_MIN_INT8  || tmp > PDC_MAX_INT8);

PDCI_AINT_READ_FN(PDC_aint16_read, PDC_int16, long,      PDCI_strtol,  PDC_INVALID_AINT,
 || tmp < PDC_MIN_INT16 || tmp > PDC_MAX_INT16);

PDCI_AINT_READ_FN(PDC_aint32_read, PDC_int32, long,      PDCI_strtol,  PDC_INVALID_AINT, );

PDCI_AINT_READ_FN(PDC_aint64_read, PDC_int64, long long, PDCI_strtoll, PDC_INVALID_AINT, );

PDCI_AINT_READ_FN(PDC_auint8_read,  PDC_uint8,  unsigned long,      PDCI_strtoul,  PDC_INVALID_AUINT,
 || tmp > PDC_MAX_UINT8);

PDCI_AINT_READ_FN(PDC_auint16_read, PDC_uint16, unsigned long,      PDCI_strtoul,  PDC_INVALID_AUINT,
 || tmp > PDC_MAX_UINT16);

PDCI_AINT_READ_FN(PDC_auint32_read, PDC_uint32, unsigned long,      PDCI_strtoul,  PDC_INVALID_AUINT, );

PDCI_AINT_READ_FN(PDC_auint64_read, PDC_uint64, unsigned long long, PDCI_strtoull, PDC_INVALID_AUINT, );

/* ================================================================================ */
/* FIXED-WIDTH ASCII INTEGER READ FUNCTIONS */

/*
 * PDCI_AINT_FW_READ_FN(fn_name, targ_type, int_type, strtonum_fn, invalid_err, opt_tmp_test)
 */

PDCI_AINT_FW_READ_FN(PDC_aint8_fw_read,  PDC_int8,  long,      PDCI_strtol,  PDC_INVALID_AINT,
 || tmp < PDC_MIN_INT8  || tmp > PDC_MAX_INT8);

PDCI_AINT_FW_READ_FN(PDC_aint16_fw_read, PDC_int16, long,      PDCI_strtol,  PDC_INVALID_AINT,
 || tmp < PDC_MIN_INT16 || tmp > PDC_MAX_INT16);

PDCI_AINT_FW_READ_FN(PDC_aint32_fw_read, PDC_int32, long,      PDCI_strtol,  PDC_INVALID_AINT, );

PDCI_AINT_FW_READ_FN(PDC_aint64_fw_read, PDC_int64, long long, PDCI_strtoll, PDC_INVALID_AINT, );

PDCI_AINT_FW_READ_FN(PDC_auint8_fw_read,  PDC_uint8,  unsigned long,      PDCI_strtoul,  PDC_INVALID_AUINT,
 || tmp > PDC_MAX_UINT8);

PDCI_AINT_FW_READ_FN(PDC_auint16_fw_read, PDC_uint16, unsigned long,      PDCI_strtoul,  PDC_INVALID_AUINT,
 || tmp > PDC_MAX_UINT16);

PDCI_AINT_FW_READ_FN(PDC_auint32_fw_read, PDC_uint32, unsigned long,      PDCI_strtoul,  PDC_INVALID_AUINT, );

PDCI_AINT_FW_READ_FN(PDC_auint64_fw_read, PDC_uint64, unsigned long long, PDCI_strtoull, PDC_INVALID_AUINT, );

/* ================================================================================ */
/* BINARY INTEGER READ FUNCTIONS */

/*
 * PDCI_BINT_READ_FN(fn_name, targ_type, width, swapmem_op)
 *
 * swapmem ops:
 *    0 -> straight copy
 *    1 -> reverse each byte in each string of 2 bytes
 *    3 -> reverse each byte in each string of 4 bytes
 *    4 -> swap upper/lower 4 bytes in each 8 byte value
 *    7 -> reverse each byte in each string of 8 bytes
 */

PDCI_BINT_READ_FN(PDC_bint8_read,   PDC_int8,   1, 0);

PDCI_BINT_READ_FN(PDC_buint8_read,  PDC_uint8,  1, 0);

PDCI_BINT_READ_FN(PDC_bint16_read,  PDC_int16,  2, 1);

PDCI_BINT_READ_FN(PDC_buint16_read, PDC_uint16, 2, 1);

PDCI_BINT_READ_FN(PDC_bint32_read,  PDC_int32,  4, 3);

PDCI_BINT_READ_FN(PDC_buint32_read, PDC_uint32, 4, 3);

PDCI_BINT_READ_FN(PDC_bint64_read,  PDC_int64,  8, 7);

PDCI_BINT_READ_FN(PDC_buint64_read, PDC_uint64, 8, 7);

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
PDC_string_acc_report_internal(PDC_t *pdc, Sfio_t *outstr, const char *prefix, const char *what, int nst,
			       PDC_string_acc *a)
{
  size_t                 pad;
  const char             *cmt = ""; 
  int                    i = 0, sz, rp;
  PDC_uint64             cnt_sum = 0;
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
  if (sz == PDCI_ACC_MAX2TRACK) {
    cmt = " (* hit tracking limit *) ";
  }
  dtdisc(a->dict, &PDCI_string_acc_dt_oset_disc, DT_SAMEHASH); /* change cmp function */
  dtmethod(a->dict, Dtoset); /* change to ordered set -- establishes an ordering */
  sfprintf(outstr, "\n  Characterizing strings:\n");
  sfprintf(outstr, "    => distribution of top %d strings out of %d distinct strings%s:\n",
	   rp, sz, cmt);
  for (velt = dtfirst(a->dict); velt && i < PDCI_ACC_REPORT_K; velt = dtnext(a->dict, velt), i++) {
    elt = (PDCI_string_dt_elt_t*)velt;
    cnt_sum += elt->key.cnt;
    elt_pcnt = ((double)100.0 * elt->key.cnt)/a->len_accum.good;
    sfprintf(outstr, "        val: ");
    sfprintf(outstr, "%-.*s", elt->key.len+2, PDCI_fmtQStrL(elt->key.str, elt->key.len));
    sfprintf(outstr, "");
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
  const char            *cmt = ""; 
  int                   i = 0, sz, rp;
  PDC_uint64            cnt_sum = 0;
  double                bad_pcnt;
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
    bad_pcnt = a->bad / (double)(a->good + a->bad);
  }
  sfprintf(outstr, "good vals: %10llu    bad vals: %10llu    pcnt-bad: %8.3lf\n",
	   a->good, a->bad, bad_pcnt);
  if (a->good == 0) {
    return PDC_OK;
  }
  PDC_uint8_acc_fold_psum(a);
  sz = dtsize(a->dict);
  rp = (sz < PDCI_ACC_REPORT_K) ? sz : PDCI_ACC_REPORT_K;
  if (sz == PDCI_ACC_MAX2TRACK) {
    cmt = " (* hit tracking limit *) ";
  }
  dtdisc(a->dict,   &PDC_uint8_acc_dt_oset_disc, DT_SAMEHASH); /* change cmp function */
  dtmethod(a->dict, Dtoset); /* change to ordered set -- establishes an ordering */
  sfprintf(outstr, "  Characterizing %s:  min %s", what, PDCI_fmtQChar(a->min));
  sfprintf(outstr, " max %s\n", PDCI_fmtQChar(a->max));

  sfprintf(outstr, "    => distribution of top %d values out of %d distinct values%s:\n",
	   rp, sz, cmt);
  for (velt = dtfirst(a->dict); velt && i < PDCI_ACC_REPORT_K; velt = dtnext(a->dict, velt), i++) {
    elt = (PDC_uint8_dt_elt_t*)velt;
    cnt_sum += elt->key.cnt;
    elt_pcnt = ((double)100.0 * elt->key.cnt)/a->good;
    sfprintf(outstr, "        val: %6s", PDCI_fmtQChar(elt->key.val));
    sfprintf(outstr, " count: %10llu  pcnt-of-good-vals: %8.3lf\n", elt->key.cnt, elt_pcnt);

  }
  cnt_sum_pcnt = ((double)100.0 * cnt_sum)/a->good;
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
/* DEFGEN(libpadsc-gen.c) */
/*
 * library routines for library that goes with padsc
 *
 * Kathleen Fisher, Robert Gruber
 * AT&T Labs Research
 */

#gen_include "libpadsc-internal.h"
#gen_include "libpadsc-macros-gen.h"

static const char id[] = "\n@(#)$Id: padsc.c,v 1.49 2002-11-18 14:46:41 kfisher Exp $\0\n";

static const char lib[] = "padsc";

/* ================================================================================ */ 
/* IMPL CONSTANTS */

#define PDCI_initStkElts      8

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
  0, /* stop_regexp disabled    */
  0, /* no stop_maxlen disabled */
  PDC_errorf,
  PDC_errorRep_Max,
  PDC_bigEndian,
  PDC_bigEndian,
  0 /* a default IO discipline is installed on PDC_open */
};

PDC_error_t
PDC_open(PDC_t **pdc_out, PDC_disc_t *disc, PDC_IO_disc_t *io_disc)
{
  Vmalloc_t    *vm;
  PDC_t        *pdc;

  PDC_TRACE(&PDC_default_disc, "PDC_open called");
  if (!pdc_out) {
    PDC_WARN(&PDC_default_disc, "PDC_open called with null pdc_out");
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
    PDC_WARN(disc, "Installing default IO discipline nlrec_noseek");
    if (!(disc->io_disc = PDC_nlrec_noseek_make(0))) {
      PDC_FATAL(disc, "Unexpected failure to install default IO discipline");
    }
  }
  if (!(pdc = vmnewof(vm, 0, PDC_t, 1, 0))) {
    goto fatal_alloc_err;
  }
#if 1
  /* allocate a 1 MB + 1 byte buffer to use with sfio */
  if (!(pdc->sfbuf = vmoldof(vm, 0, char, 1024 * 1024, 1))) {
    goto fatal_alloc_err;
  } 
#endif
  if (!(pdc->tmp = sfstropen())) {
    goto fatal_alloc_err;
  }
  if (!(pdc->rmm_z = RMM_open(RMM_zero_disc_ptr))) {
    goto fatal_alloc_err;
  }
  if (!(pdc->rmm_nz = RMM_open(RMM_nozero_disc_ptr))) {
    goto fatal_alloc_err;
  }
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
  /* These fields are 0/NiL due to zero-based alloc of pdc:
   *   path, io_state, top, buf, balloc, bchars, speclev
   */
  (*pdc_out) = pdc;
  return PDC_OK;

 fatal_alloc_err:
  PDC_FATAL(disc, "out of space error during PDC_open");
 any_err:
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
    /* do an clean sfclose */
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
PDC_IO_fopen(PDC_t *pdc, char *path)
{
  PDCI_DISC_INIT_CHECKS("PDC_IO_fopen");
  PDCI_NULLPARAM_CHECK("PDC_IO_fopen", path);
  return PDC_IO_fopen_internal(pdc, path);
}

PDC_error_t
PDC_IO_fclose(PDC_t *pdc)
{
  PDCI_DISC_INIT_CHECKS("PDC_IO_fclose");
  return PDC_IO_fclose_internal(pdc);
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
/* EXTERNAL LITERAL READ FUNCTIONS */

PDC_error_t
PDC_char_lit_read(PDC_t *pdc, PDC_base_em *em,
		  PDC_base_ed *ed, unsigned char c)
{
  PDC_base_em     emt = PDC_CheckAndSet;
  PDC_base_ed     edt;

  if (!em) {
    em = &emt;
  }
  if (!ed) {
    ed = &edt;
  }
  PDCI_IODISC_INIT_CHECKS("PDC_char_lit_read");
  return PDC_char_lit_read_internal(pdc, em, ed, c);
}

PDC_error_t
PDC_str_lit_read(PDC_t *pdc, PDC_base_em *em,
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
  PDCI_IODISC_INIT_CHECKS("PDC_str_lit_read");
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
PDC_adate_read (PDC_t *pdc, PDC_base_em *em, PDC_base_ed *ed, 
		PDC_uint32 *res_out)
{
  PDC_base_em     emt = PDC_CheckAndSet;
  PDC_base_ed     edt;

  if (!em) {
    em = &emt;
  }
  if (!ed) {
    ed = &edt;
  }
  PDCI_IODISC_INIT_CHECKS("PDC_adate_read");
  return PDC_adate_read_internal(pdc, em, ed, res_out);
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
  RMM_free_rbuf(s->rbuf);
  return PDC_OK;
}

PDC_error_t
PDC_string_copy(PDC_t *pdc, PDC_string *targ, const char *src, size_t len)
{
  PDCI_DISC_INIT_CHECKS("PDC_string_copy");
  PDCI_NULLPARAM_CHECK("PDC_string_copy", src);
  PDCI_NULLPARAM_CHECK("PDC_string_copy", targ);
  if (!targ->rbuf) {
    if (!(targ->rbuf = RMM_new_rbuf(pdc->rmm_nz))) {
      goto fatal_alloc_err;
    }
  }
  if (RBuf_reserve(targ->rbuf, (void**)&(targ->str), sizeof(char), len+1, 0)) {
    goto fatal_alloc_err;
  }
  strncpy(targ->str, src, len);
  targ->len = len;
  return PDC_OK;

 fatal_alloc_err:
  PDC_FATAL(pdc->disc, "PDC_string_copy: out of space");
  return PDC_ERR;
}

PDC_error_t
PDC_string_ed_init(PDC_t *pdc, PDC_string_ed *ed)
{
  PDCI_DISC_INIT_CHECKS("PDC_string_ed_init");
  return PDC_OK;
}

PDC_error_t
PDC_string_ed_cleanup(PDC_t *pdc, PDC_string_ed *ed)
{
  PDCI_DISC_INIT_CHECKS("PDC_string_ed_cleanup");
  return PDC_OK;
}

/*
 * The string read functions
 */

PDC_error_t
PDC_astringFW_read(PDC_t *pdc, PDC_base_em *em, size_t width,
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
  PDCI_IODISC_INIT_CHECKS("PDC_astringFW_read");
  return PDC_astringFW_read_internal(pdc, em, width, ed, s_out);
}

PDC_astring_read(PDC_t *pdc, PDC_base_em *em, unsigned char stopChar,
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
  PDCI_IODISC_INIT_CHECKS("PDC_astring_read");
  return PDC_astring_read_internal(pdc, em, stopChar, ed, s_out);
}

PDC_error_t
PDC_astringSE_read(PDC_t *pdc, PDC_base_em *em, const char *stopRegexp,
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
  PDCI_IODISC_INIT_CHECKS("PDC_astringSE_read");
  PDCI_NULLPARAM_CHECK("PDC_astringSE_read", stopRegexp);
  return PDC_astringSE_read_internal(pdc, em, stopRegexp, ed, s_out);
}

PDC_error_t
PDC_astringCSE_read(PDC_t *pdc, PDC_base_em *em, PDC_regexp_t *stopRegexp,
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
  PDCI_IODISC_INIT_CHECKS("PDC_astringCSE_read");
  PDCI_NULLPARAM_CHECK("PDC_astringCSE_read", stopRegexp);
  return PDC_astringCSE_read_internal(pdc, em, stopRegexp, ed, s_out);
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
	      PDCI_fmtQStrL(regexp, strlen(regexp)));
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

/*
 * Note: swapmem ops documented with binary read functions
 * Here we use in-place swap, which is safe with gsf's swapmem
 */

PDC_error_t
PDC_swap_bytes(PDC_t *pdc, char *bytes, size_t num_bytes)
{
  PDCI_DISC_INIT_CHECKS("PDC_swap_bytes");

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
  PDC_WARN1(pdc->disc, "PDC_swap_bytes: invalid num_bytes (%d), use 2, 4, or 8", num_bytes);
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
PDC_char_lit_scan(PDC_t *pdc, unsigned char c, unsigned char s, int eat_lit,
		  unsigned char *c_out, size_t *offset_out)
{
  char            *begin, *p1, *p2, *end;
  int             eor, eof;
  size_t          bytes;
  int             matchlen = -1;

  PDCI_IODISC_INIT_CHECKS("PDC_char_lit_scan");
  PDC_TRACE3(pdc->disc, "PDC_char_lit_scan args: c %s stop %s eat %d", PDCI_fmtQChar(c), PDCI_fmtQChar(s), eat_lit);
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
      PDC_WARN(pdc->disc, "PDC_char_lit_scan: scan terminated early due to disc->stop_maxlen");
      break;
    }
    if (pdc->disc->stop_regexp && PDCI_regexpMatch(pdc, pdc->disc->stop_regexp, p1, end)) {
      PDC_WARN(pdc->disc, "PDC_char_lit_scan: scan terminated early due to disc->stop_regexp");
      break;
    }
    p1++;
  }
  return PDC_ERR;
}

PDC_error_t
PDC_str_lit_scan(PDC_t *pdc, const PDC_string *findStr, const PDC_string *stopStr, int eat_lit,
		 PDC_string **str_out, size_t *offset_out) 
{
  char            *begin, *p1, *p2, *end;
  int             eor, eof;
  size_t          bytes;
  int             matchlen = -1;

  PDCI_IODISC_INIT_CHECKS("PDC_str_lit_scan");
  PDC_TRACE3(pdc->disc, "PDC_str_lit_scan args: findStr = %s stopStre = %s eat = %d",
	     PDCI_fmtQStr(findStr), PDCI_fmtQStr(stopStr), eat_lit);
  if (offset_out) {
    (*offset_out) = 0;
  }
  if (!findStr || findStr->len == 0) {
    PDC_WARN(pdc->disc, "PDC_str_lit_scan : null/empty findStr specified");
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
    if (strncmp(p1, findStr->str, findStr->len) == 0) {
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
	strncmp(p1, stopStr->str, stopStr->len) == 0) {
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
      PDC_WARN(pdc->disc, "PDC_str_lit_scan: scan terminated early due to disc->stop_maxlen");
      break;
    }
    if (pdc->disc->stop_regexp && PDCI_regexpMatch(pdc, pdc->disc->stop_regexp, p1, end)) {
      PDC_WARN(pdc->disc, "PDC_str_lit_scan: scan terminated early due to disc->stop_regexp");
      break;
    }
    p1++;
  }
  return PDC_ERR;
}

/********************************************************************************
 * INTERNAL FUNCTIONS (see libpadsc-internal.h)
 ********************************************************************************/

/* ================================================================================ */ 
/* INTERNAL ERROR REPORTING FUNCTIONS */

PDC_error_t
PDCI_report_err(PDC_t *pdc, int level, PDC_loc_t *loc,
		PDC_errCode_t errCode, const char *format, ...)
{
  PDC_error_f pdc_errorf;
  char    *severity = "Error";
  char    *msg      = "** unknown error code **";
  char    *tmpstr1, *tmpstr2, *tmpstr3;
  size_t  tmplen1, tmplen2, tmplen3;
  int     nullspan = 0;

  PDC_TRACE(pdc->disc, "PDCI_report_err called");
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
      pdc_errorf(NiL, level, "%s : %s %d char %d : errCode %d",
		 severity, loc->b.unit, loc->b.num, loc->b.byte, errCode);
    } else {
      pdc_errorf(NiL, level, "%s : errCode %d", severity, errCode);
    }
    return PDC_OK;
  }
  if (format && strlen(format)) {
    va_list ap;
    if (loc) {
      sfprintf(pdc->tmp, "%s : %s %d char %d : ", severity, loc->b.unit, loc->b.num, loc->b.byte);
    } else {
      sfprintf(pdc->tmp, "%s : ", severity);
    }
    va_start(ap, format);
    sfvprintf(pdc->tmp, format, ap);
    va_end(ap);
  } else {
    switch (errCode) {
    case PDC_NO_ERR:
      msg = "";
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
    case PDC_INVALID_AINT:
      msg = "Invalid ASCII integer";
      break;
    case PDC_INVALID_AUINT:
      msg = "Invalid ASCII unsigned integer";
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
      if (loc->b.num != loc->e.num) {
	sfprintf(pdc->tmp, "%s from %s %d char %d to %s %d char %d: %s ",
		 severity,
		 loc->b.unit, loc->b.num, loc->b.byte, 
		 loc->e.unit, loc->e.num, loc->e.byte,
		 msg);
      } else if (nullspan) {
	sfprintf(pdc->tmp, "%s at %s %d just before char %d: %s",
		 severity,
		 loc->b.unit, loc->b.num, loc->b.byte,
		 msg);
      } else if (loc->b.byte == loc->e.byte) {
	sfprintf(pdc->tmp, "%s at %s %d at char %d : %s ",
		 severity,
		 loc->b.unit, loc->b.num, loc->b.byte,
		 msg);
      } else {
	sfprintf(pdc->tmp, "%s at %s %d from char %d to char %d: %s ",
		 severity,
		 loc->b.unit, loc->b.num, loc->b.byte, loc->e.byte,
		 msg);
      }
    } else {
      sfprintf(pdc->tmp, "%s : %s ", severity, msg);
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
	  tmpstr1 = PDCI_fmtStrL(elt1->begin,           tmplen1);
	  tmpstr2 = PDCI_fmtStrL(elt1->begin + tmplen1, tmplen2);
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
	  tmpstr1 = PDCI_fmtStrL(elt2->begin,           tmplen1);
	  tmpstr2 = PDCI_fmtStrL(elt2->begin + tmplen1, tmplen2);
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
	  tmpstr1 = PDCI_fmtStrL(elt1->begin,           tmplen1);
	  tmpstr2 = PDCI_fmtStrL(elt1->begin + tmplen1, tmplen2);
	  sfprintf(pdc->tmp, "%s>>><<<%s", tmpstr1, tmpstr2);
	} else {
	  tmplen1 = loc->b.byte - 1;
	  tmplen3 = elt1->len - loc->e.byte;
	  tmplen2 = elt1->len - tmplen1 - tmplen3;
	  tmpstr1 = PDCI_fmtStrL(elt1->begin,                     tmplen1);
	  tmpstr2 = PDCI_fmtStrL(elt1->begin + tmplen1,           tmplen2);
	  tmpstr3 = PDCI_fmtStrL(elt1->begin + tmplen1 + tmplen2, tmplen3);
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
PDC_IO_fopen_internal(PDC_t *pdc, char *path)
{
  PDCI_stkElt_t    *tp        = &(pdc->stack[0]);
  PDC_IO_elt_t     *next_elt;

  PDC_TRACE(pdc->disc, "PDC_IO_fopen_internal called");
  if (pdc->io) {
    PDC_WARN(pdc->disc, "fopen called while previous file still open");
    return PDC_ERR;
  }
  if (!pdc->disc->io_disc) {
    PDC_WARN(pdc->disc, "fopen called with no IO discipline installed");
    return PDC_ERR;
  }
  if (!(pdc->io = sfopen(NiL, path, "r"))) {
    PDC_SYSERR1(pdc->disc, "Failed to open file \"%s\"", path);
    return PDC_ERR;
  }
#if 1
  /* tell sfio to use pdc->sfbuf but only let it know about 1 MB of space */
  sfsetbuf(pdc->io, (Void_t*)pdc->sfbuf, 1024 * 1024);
#endif
  if (!(pdc->path = vmnewof(pdc->vm, 0, char, strlen(path) + 1, 0))) {
    PDC_FATAL(pdc->disc, "out of space [string to record file path]");
    PDC_IO_fclose(pdc);
    return PDC_ERR;
  }
  strcpy(pdc->path, path);

  /* set state to nothing read/no checkpoints */
  pdc->top        = 0;
  pdc->speclev    = 0;

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
    PDC_FATAL(pdc->disc, "Internal error : IO read function failure in PDC_IO_fopen_internal");
  }
  tp->remain = tp->elt->len;
  return PDC_OK;
}

PDC_error_t
PDC_IO_fclose_internal(PDC_t *pdc)
{
  PDC_TRACE(pdc->disc, "PDC_IO_fclose_internal called");
  if (!pdc->io) {
    return PDC_ERR;
  }
  /* close IO discpline */
  if (pdc->disc->io_disc) {
    pdc->disc->io_disc->sfclose_fn(pdc, pdc->disc->io_disc, 0, 0); /* do not both returning bytes to io stream */
  }
  if (pdc->io) {
    sfclose(pdc->io);
    pdc->io = 0;
  }
  if (!pdc->vm || !pdc->path) {
    return PDC_ERR;
  }
  vmfree(pdc->vm, pdc->path);
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
PDCI_IO_needbytes(PDC_t *pdc, char **b_out, char **p1_out, char **p2_out, char **e_out,
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
PDCI_IO_morebytes(PDC_t *pdc, char **b_out, char **p1_out, char **p2_out, char **e_out,
		  int *eor_out, int *eof_out, size_t *bytes_out)
{
  PDC_IO_elt_t     *lastelt   = PDC_LAST_ELT(pdc->head);
  PDCI_stkElt_t    *bot       = &(pdc->stack[0]);
  PDC_IO_elt_t     *io_elt    = bot->elt;
  PDC_IO_elt_t     *next_elt;
  size_t           io_remain  = bot->remain;
  size_t           offset;
  char             *prev_lastelt_end;

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
PDCI_IO_checkpoint(PDC_t *pdc, int speculative)
{
  PDC_TRACE(pdc->disc, "PDCI_IO_checkpoint called");
  if (++(pdc->top) >= pdc->salloc) {
    PDCI_stkElt_t *stack_next;
    size_t salloc_next = 2 * pdc->salloc;
    PDC_DBG2(pdc->disc, "XXX Growing from %d to %d checkpoint stack slots", pdc->salloc, salloc_next);
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
PDCI_IO_restore(PDC_t *pdc)
{
  PDC_TRACE(pdc->disc, "PDCI_IO_restore called");
  if (pdc->top <= 0) {
    PDC_WARN(pdc->disc, "Internal error: PDCI_IO_restore called when stack top <= 0");
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
PDCI_IO_commit(PDC_t *pdc)
{
  PDC_TRACE(pdc->disc, "PDCI_IO_commit called");
  if (pdc->top <= 0) {
    PDC_WARN(pdc->disc, "Internal error: PDCI_IO_commit called when stack top <= 0");
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
PDCI_spec_level(PDC_t *pdc)
{
  PDC_TRACE(pdc->disc, "PDCI_spec_level called");
  return pdc->speclev;
}

PDC_error_t
PDCI_IO_getElt(PDC_t *pdc, size_t num, PDC_IO_elt_t **elt_out) {
  PDC_IO_elt_t *elt;

  PDC_TRACE(pdc->disc, "PDCI_IO_getElt called");
  if (!elt_out) {
    PDC_WARN(pdc->disc, "PDCI_IO_getElt called with null elt_out");
    return PDC_ERR;
  }
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
			   PDC_base_ed *ed, unsigned char c)
{
  char            *begin, *p1, *p2, *end;
  int             eor, eof;
  size_t          bytes;

  PDC_TRACE1(pdc->disc, "PDC_char_lit_read_internal called, arg: %s", PDCI_fmtQChar(c));
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
  PDCI_READFN_RET_ERRCODE_WARN(0, eor ? PDC_AT_EOR : PDC_AT_EOF);

 not_found:
  PDCI_READFN_SET_LOC_BE(0, 1);
  if (*em == PDC_CheckAndSet) {
    PDCI_READFN_RET_ERRCODE_WARN(0, PDC_CHAR_LIT_NOT_FOUND);
  }
  PDCI_READFN_RET_ERRCODE_NOWARN(PDC_CHAR_LIT_NOT_FOUND);

 fatal_nb_io_err:
  PDCI_READFN_RET_ERRCODE_FATAL("IO error in PDC_char_lit_read_internal (nb)", PDC_IO_ERR);

 fatal_forward_err:
  PDCI_READFN_RET_ERRCODE_FATAL("Internal IO_forward error in PDC_char_lit_read_internal", PDC_FORWARD_ERR);
}

PDC_error_t
PDC_str_lit_read_internal(PDC_t *pdc, PDC_base_em *em,
			  PDC_base_ed *ed, const PDC_string *s)
{
  char            *begin, *p1, *p2, *end;
  int             eor, eof;
  size_t          bytes;

  PDC_TRACE1(pdc->disc, "PDC_str_lit_read_internal called, arg: %s", PDCI_fmtQStr(s));
  if (s->len <= 0) {
    PDC_WARN(pdc->disc, "UNEXPECTED PARAM VALUE: PDC_str_lit_read called with s->len <= 0");
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
  if ((*em == PDC_Ignore) || (strncmp(begin, s->str, s->len) == 0)) {
    if (PDC_ERR == PDCI_IO_forward(pdc, s->len)) {
      goto fatal_forward_err;
    }
    ed->errCode = PDC_NO_ERR;
    return PDC_OK;    /* found it */
  }
  goto not_found;

 bad_param_err:
  PDCI_READFN_SET_NULLSPAN_LOC(0);
  PDCI_READFN_RET_ERRCODE_WARN(0, PDC_BAD_PARAM);

 width_not_avail:
  PDCI_READFN_SET_LOC_BE(0, end-begin);
  if (*em == PDC_CheckAndSet) {
    PDCI_READFN_RET_ERRCODE_WARN(0, PDC_STR_LIT_NOT_FOUND);
  }
  PDCI_READFN_RET_ERRCODE_NOWARN(PDC_STR_LIT_NOT_FOUND);

 not_found:
  PDCI_READFN_SET_LOC_BE(0, s->len);
  if (*em == PDC_CheckAndSet) {
    PDCI_READFN_RET_ERRCODE_WARN(0, PDC_STR_LIT_NOT_FOUND);
  }
  PDCI_READFN_RET_ERRCODE_NOWARN(PDC_STR_LIT_NOT_FOUND);

 fatal_nb_io_err:
  PDCI_READFN_RET_ERRCODE_FATAL("IO error in PDC_str_lit_read_internal (nb)", PDC_IO_ERR);

 fatal_mb_io_err:
  PDCI_READFN_RET_ERRCODE_FATAL("IO error in PDC_str_lit_read_internal (mb)", PDC_IO_ERR);

 fatal_forward_err:
  PDCI_READFN_RET_ERRCODE_FATAL("Internal IO_forward error in PDC_str_lit_read_internal", PDC_FORWARD_ERR);
}

PDC_error_t
PDC_countX_internal(PDC_t *pdc, PDC_base_em *em, PDC_uint8 x, int eor_required,
		    PDC_base_ed *ed, PDC_int32 *res_out)
{
  PDC_int32       count = 0;
  char            *begin, *p1, *p2, *end;
  int             eor, eof;
  size_t          bytes;
  int             matchlen = -1;
  char            *tmp;

  PDC_TRACE2(pdc->disc, "PDC_countX_internal called, args: x = %s eor_required = %d", PDCI_fmtQChar(x), eor_required);
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
    if (pdc->disc->stop_regexp && PDCI_regexpMatch(pdc, pdc->disc->stop_regexp, p1, end)) {
      PDC_WARN(pdc->disc, "PDC_countX: scan terminated early due to disc->stop_regexp");
      break;
    }
    p1++;
  }
  if (eor_required && !eor && eof) { /* EOF encountered first, error */
    PDCI_READFN_SET_LOC_BE(0, p1-begin);
    PDCI_READFN_RET_ERRCODE_WARN(0, PDC_EOF_BEFORE_EOR);
  }
  /* hit EOR/EOF/stop restriction */
  (*res_out) = count;
  ed->errCode = PDC_NO_ERR;
  return PDC_OK;

 not_found:

 fatal_nb_io_err:
  PDCI_READFN_RET_ERRCODE_FATAL("IO error in PDC_countXtoY_internal (nb)", PDC_IO_ERR);

 fatal_mb_io_err:
  PDCI_READFN_RET_ERRCODE_FATAL("IO error in PDC_countXtoY_internal (mb)", PDC_IO_ERR);
}

PDC_error_t
PDC_countXtoY_internal(PDC_t *pdc, PDC_base_em *em, PDC_uint8 x, PDC_uint8 y,
		       PDC_base_ed *ed, PDC_int32 *res_out)
{
  PDC_int32       count = 0;
  char            *begin, *p1, *p2, *end;
  int             eor, eof;
  size_t          bytes;
  int             matchlen = -1;
  char            *tmp;

  PDC_TRACE2(pdc->disc, "PDC_countXtoY_internal called, args: x = %s y = %s", PDCI_fmtQChar(x), PDCI_fmtQChar(y));
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
    if (pdc->disc->stop_regexp && PDCI_regexpMatch(pdc, pdc->disc->stop_regexp, p1, end)) {
      PDC_WARN(pdc->disc, "PDC_countXtoY: scan terminated early due to disc->stop_regexp");
      break;
    }
    p1++;
  }
  goto not_found; /* y not found */

 not_found:
  PDCI_READFN_SET_LOC_BE(0, p1-begin);
  PDCI_READFN_RET_ERRCODE_WARN(0, PDC_CHAR_LIT_NOT_FOUND);

 fatal_nb_io_err:
  PDCI_READFN_RET_ERRCODE_FATAL("IO error in PDC_countXtoY_internal (nb)", PDC_IO_ERR);

 fatal_mb_io_err:
  PDCI_READFN_RET_ERRCODE_FATAL("IO error in PDC_countXtoY_internal (mb)", PDC_IO_ERR);
}

/* ================================================================================ */
/* INTERNAL VERSIONS OF EXTERNAL DATE/TIME READ FUNCTIONS */

PDC_error_t
PDC_adate_read_internal(PDC_t *pdc, PDC_base_em *em, PDC_base_ed *ed, 
			PDC_uint32 *res_out)
{
  PDC_TRACE(pdc->disc, "PDC_adate_read_internal called");
  /* TODO */
  return PDC_ERR;
}

/* ================================================================================ */
/* INTERNAL VERSIONS OF EXTERNAL STRING READ FUNCTIONS */

PDC_error_t
PDC_astringFW_read_internal(PDC_t *pdc, PDC_base_em *em, size_t width,
			    PDC_base_ed *ed, PDC_string *s_out)
{
  char            *begin, *p1, *p2, *end;
  int             eor, eof;
  size_t          bytes;

  PDC_TRACE(pdc->disc, "PDC_astringFW_read_internal called");
  if (width <= 0) {
    PDC_WARN(pdc->disc, "UNEXPECTED PARAM VALUE: PDC_astringFW_read called with width <= 0");
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
  PDCI_STR_COPY(s_out, begin, end);
  if (PDC_ERR == PDCI_IO_forward(pdc, width)) {
    goto fatal_forward_err;
  }
  ed->errCode = PDC_NO_ERR;
  return PDC_OK;

 bad_param_err:
  PDCI_READFN_SET_NULLSPAN_LOC(0);
  PDCI_READFN_RET_ERRCODE_WARN(0, PDC_BAD_PARAM);

 width_not_avail:
  PDCI_READFN_SET_LOC_BE(0, end-begin);
  PDCI_READFN_RET_ERRCODE_WARN(0, PDC_WIDTH_NOT_AVAILABLE);

 fatal_alloc_err:
  PDCI_READFN_RET_ERRCODE_FATAL("Memory alloc error in PDC_astringFW_read_internal", PDC_ALLOC_ERR);

 fatal_nb_io_err:
  PDCI_READFN_RET_ERRCODE_FATAL("IO error in PDC_astringFW_read_internal (nb)", PDC_IO_ERR);

 fatal_mb_io_err:
  PDCI_READFN_RET_ERRCODE_FATAL("IO error in PDC_astringFW_read_internal (mb)", PDC_IO_ERR);

 fatal_forward_err:
  PDCI_READFN_RET_ERRCODE_FATAL("Internal IO_forward error in PDC_astringFW_read_internal", PDC_FORWARD_ERR);
}

PDC_error_t
PDC_astring_read_internal(PDC_t *pdc, PDC_base_em *em, unsigned char stopChar,
			  PDC_base_ed *ed, PDC_string *s_out)
{
  char            *begin, *p1, *p2, *end;
  int             eor, eof;
  size_t          bytes;
  int             matchlen = -1;

  PDC_TRACE(pdc->disc, "PDC_astring_read_internal called");
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
      if (matchlen == 0) { /* no limit on match size, back up all the way */
	p1 = begin;
      } else if (matchlen > 1) {
	p1 -= (matchlen - 1);
      }
      continue;
    }
    /* p1 < end */
    if (stopChar == (*p1)) {
      /* success */
      PDCI_STR_COPY(s_out, begin, p1);
      if (PDC_ERR == PDCI_IO_forward(pdc, p1-begin)) {
	goto fatal_forward_err;
      }
      ed->errCode = PDC_NO_ERR;
      return PDC_OK;
    }
    if (pdc->disc->stop_maxlen && ((p1-begin) >= pdc->disc->stop_maxlen)) {
      PDC_WARN(pdc->disc, "PDC_astring_read: scan terminated early due to disc->stop_maxlen");
      break;
    }
    if (pdc->disc->stop_regexp && PDCI_regexpMatch(pdc, pdc->disc->stop_regexp, p1, end)) {
      PDC_WARN(pdc->disc, "PDC_astring_read: scan terminated early due to disc->stop_regexp");
      break;
    }
    p1++;
  }
  goto not_found;

 not_found:
  PDCI_READFN_SET_LOC_BE(0, p1-begin);
  PDCI_READFN_RET_ERRCODE_WARN(0, PDC_CHAR_LIT_NOT_FOUND);

 fatal_alloc_err:
  PDCI_READFN_RET_ERRCODE_FATAL("Memory alloc error in PDC_astring_read_internal", PDC_ALLOC_ERR);

 fatal_nb_io_err:
  PDCI_READFN_RET_ERRCODE_FATAL("IO error in PDC_astring_read_internal (nb)", PDC_IO_ERR);

 fatal_mb_io_err:
  PDCI_READFN_RET_ERRCODE_FATAL("IO error in PDC_astring_read_internal (mb)", PDC_IO_ERR);

 fatal_forward_err:
  PDCI_READFN_RET_ERRCODE_FATAL("Internal IO_forward error in PDC_astring_read_internal", PDC_FORWARD_ERR);
}

PDC_error_t
PDC_astringSE_read_internal(PDC_t *pdc, PDC_base_em *em, const char *stopRegexp,
			    PDC_base_ed *ed, PDC_string *s_out)
{
  PDC_regexp_t *compiled_exp;
  if (PDC_ERR == PDC_regexp_compile(pdc, stopRegexp, &compiled_exp)) {
    goto bad_exp;
  }
  return PDC_astringCSE_read_internal(pdc, em, compiled_exp, ed, s_out);

 bad_exp:
  PDCI_READFN_SET_NULLSPAN_LOC(0);
  PDCI_READFN_RET_ERRCODE_NOWARN(PDC_INVALID_REGEXP);  /* regexp_compile already issued a warning */
}

PDC_error_t
PDC_astringCSE_read_internal(PDC_t *pdc, PDC_base_em *em, PDC_regexp_t *stopRegexp,
			     PDC_base_ed *ed, PDC_string *s_out)
{
  char            *begin, *p1, *p2, *end;
  int             eor, eof;
  size_t          bytes;
  int             matchlen;

  PDC_TRACE(pdc->disc, "PDC_astringCSE_read_internal called");
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
	PDCI_STR_COPY(s_out, begin, p1);
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
    if (PDCI_regexpMatch(pdc, stopRegexp, p1, end)) {
      PDCI_STR_COPY(s_out, begin, p1);
      if (PDC_ERR == PDCI_IO_forward(pdc, p1-begin)) {
	goto fatal_forward_err;
      }
      ed->errCode = PDC_NO_ERR;
      return PDC_OK;
    }
    if (pdc->disc->stop_maxlen && ((p1-begin) >= pdc->disc->stop_maxlen)) {
      PDC_WARN(pdc->disc, "PDC_astringCSE_read: scan terminated early due to disc->stop_maxlen");
      break;
    }
    if (pdc->disc->stop_regexp && PDCI_regexpMatch(pdc, pdc->disc->stop_regexp, p1, end)) {
      PDC_WARN(pdc->disc, "PDC_astringCSE_read: scan terminated early due to disc->stop_regexp");
      break;
    }
    p1++;
  }
  goto not_found;

 not_found:
  PDCI_READFN_SET_LOC_BE(0, p1-begin);
  PDCI_READFN_RET_ERRCODE_WARN(0, PDC_REGEXP_NOT_FOUND);

 fatal_alloc_err:
  PDCI_READFN_RET_ERRCODE_FATAL("Memory alloc error in PDC_astringCSE_read_internal", PDC_ALLOC_ERR);

 fatal_nb_io_err:
  PDCI_READFN_RET_ERRCODE_FATAL("IO error in PDC_astringCSE_read_internal (nb)", PDC_IO_ERR);

 fatal_mb_io_err:
  PDCI_READFN_RET_ERRCODE_FATAL("IO error in PDC_astringCSE_read_internal (mb)", PDC_IO_ERR);

 fatal_forward_err:
  PDCI_READFN_RET_ERRCODE_FATAL("Internal IO_forward error in PDC_astringCSE_read_internal", PDC_FORWARD_ERR);
}

/* ================================================================================ */
/* INTERNAL MODIFIED CONVERSION ROUTINES */

long
PDCI_strtol(const char *str, char **ptr, int base)
{
  errno = 0;
  return strtol(str, ptr, base);
}

long long
PDCI_strtoll(const char *str, char **ptr, int base)
{
  errno = 0;
  return strtoll(str, ptr, base);
}

unsigned long
PDCI_strtoul(const char *str, char **ptr, int base)
{
  const char *tmp = str;
  errno = 0;
  while (isspace(*tmp)) {
    tmp++;
  }
  if (*(tmp++) == '-') {
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
PDCI_strtoull(const char *str, char **ptr, int base)
{
  const char *tmp = str;
  errno = 0;
  while (isspace(*tmp)) {
    tmp++;
  }
  if (*(tmp++) == '-') {
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

/* ================================================================================ */
/* INTERNAL MISC ROUTINES */

char*
PDCI_fmtChar(char c) {
  return fmtquote(&c, NiL, NiL, 1, 0);
}

char*
PDCI_fmtQChar(char c) {
  return fmtquote(&c, "\'", "\'", 1, 1);
}

char*
PDCI_fmtStr(const PDC_string *s) {
  return fmtquote(s->str, NiL, NiL, s->len, 0);
}

char*
PDCI_fmtQStr(const PDC_string *s) {
  return fmtquote(s->str, "\"", "\"", s->len, 1);
}

char*
PDCI_fmtStrL(const char *s, size_t len) {
  return fmtquote(s, NiL, NiL, len, 0);
}

char*
PDCI_fmtQStrL(const char *s, size_t len) {
  return fmtquote(s, "\"", "\"", len, 1);
}


size_t
PDCI_regexpMatch(PDC_t *pdc, PDC_regexp_t *regexp, char *begin, char *end)
{
  if (!begin || !begin[0]) {
    return 0;
  }
  if (strchr(regexp->charset, begin[0])) {
    return 1; /* match length is 1 char */
  }
  return 0;
}

/* ================================================================================ */
