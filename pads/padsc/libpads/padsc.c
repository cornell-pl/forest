#pragma prototyped
/*
 * library routines for library that goes with padsc
 *
 * Kathleen Fisher, Robert Gruber
 * AT&T Labs Research
 */

#include "libpadsc-internal.h"
#include <ctype.h>

static const char id[] = "\n@(#)$Id: padsc.c,v 1.2 2002-08-17 21:01:36 gruber Exp $\0\n";

static const char lib[] = "padsc";

/* ================================================================================ */ 
/* INTERNAL ERROR REPORTING FUNCTIONS */

int
PDC_errorvf(PDC_t* pdc, PDC_disc_t* disc, int level, va_list ap)
{
  if (!disc) {
    disc = pdc->disc;
  }
  errorv((disc && pdc) ? *((char**)pdc) : (char*)pdc, (disc || level < 0) ? level : (level | ERROR_LIBRARY), ap);
  return 0;
}

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
PDC_report_err(PDC_t* pdc, PDC_disc_t* disc, PDC_loc_t* loc, int errCode, ...)
{
  va_list ap;
  const char* msg = "** unknown error code **";

  if (!disc) {
    disc = pdc->disc;
  }
  TRACE(pdc, "PDC_report_err called");
  if (!disc->errorf || !disc->errorvf) {
    return PDC_OK;
  }
  switch (errCode) {
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
  case PDC_STRUCT_FIELD_ERR:
    msg = "Structure field error";
    break;
  case PDC_UNION_MATCH_FAILURE:
    msg = "Union match failure";
    break;
  case PDC_ENUM_MATCH_FAILURE:
    msg = "Enum match failure";
    break;
  }
  disc->errorf(pdc, disc, 2, msg);
  va_start(ap, errCode);
  disc->errorvf(pdc, disc, 2, ap);
  va_end(ap);
  return PDC_OK;
}

/* ================================================================================ */
/* SCAN FUNCTIONS */

PDC_error_t
PDC_char_lit_scan(PDC_t* pdc, PDC_base_em* em,
		  PDC_base_ed* er, unsigned char c, PDC_disc_t* disc)
{
  unsigned char ct;

  if (!disc) {
    disc = pdc->disc;
  }
  TRACE1(pdc, "PDC_char_lit_scan called for char = %c", c);
  while (PDC_OK == PDC_IO_getchar(pdc, &ct, 1, disc)) { /* 1 means panicking */
    if (c == ct) {
      return PDC_OK;  /* IO cursor is one beyond c */
    }
  }
  /* IO cursor moved an arbitrary amount, limited by disc->p_stop setting */
  return PDC_ERROR;
}

/* ================================================================================ */
/* READ FUNCTIONS */

PDC_error_t
PDC_char_lit_read(PDC_t* pdc, PDC_base_em* em,
		  PDC_base_ed* er, unsigned char c, PDC_disc_t* disc)
{
  unsigned char ct;

  if (!disc) {
    disc = pdc->disc;
  }
  TRACE1(pdc, "PDC_char_lit_read called for char = %c", c);
  if (PDC_OK == PDC_IO_getchar(pdc, &ct, 0, disc)) {
    if (c == ct) {
      return PDC_OK;  /* IO cursor is one beyond c */
    }
    /* wrong char -- put it back */
    PDC_IO_back(pdc, 1, disc);
  }
  return PDC_ERROR;
}

PDC_error_t
PDC_auint32_read(PDC_t* pdc, PDC_base_em* em,
		 PDC_base_ed* ed, PDC_uint32* res_out, PDC_disc_t* disc)
{
  unsigned long tul; /* tmp unsigned long */
  char*         tcp; /* tmp char* */
  char*         begin = pdc->iost.c; /* IO cursor at beginning of call */
  PDC_base_em   emt = PDC_CheckAndSet;
  PDC_base_ed   edt = {0};

  if (!disc) {
    disc = pdc->disc;
  }
  TRACE(pdc, "PDC_auint32_read called");
  if (!em) {
    em = &emt;
  }
  if (!ed) {
    ed = &edt;
  }
  while (!pdc->iost.eof && pdc->iost.c >= pdc->iost.e) { /* while need to fill */
    if (PDC_ERROR == PDC_IO_refill(pdc, disc)) {
      /* eof has been set, so treat it as eof case */
      goto at_eof_err;
    }
  }
  if (pdc->iost.eof) {
    goto at_eof_err;
  }
  if (isspace(*(pdc->iost.c))) {  /* strtol allows white space so must do this check first */
    goto bad_prefix_err;
  }
  tul = strtoul(pdc->iost.c, &tcp, 10);
  if (tcp==0 || tcp==pdc->iost.c) {
    goto bad_prefix_err;
  }
  pdc->iost.c = tcp; /* advance IO cursor */
  if (errno==ERANGE) {
    goto range_err;
  }
  /* success */
  if (res_out && *em == PDC_CheckAndSet) {
    *res_out = (PDC_uint32)tul;
  }
  return PDC_OK;

 at_eof_err:
  if (*em < PDC_Ignore) {
    ed->errCode = PDC_AT_EOF;
    ed->loc.lineNum = pdc->iost.line;
    ed->loc.posNum = (pdc->iost.e - pdc->iost.b) + 1;  /* pos 1 past last char in line */
    ed->loc.begin = ed->loc.end = pdc->iost.e; /* loc 1 past last char in line */
  }
  return PDC_ERROR;

 bad_prefix_err:
  if (*em < PDC_Ignore) {
    ed->errCode = PDC_INVALID_AUINT;
    ed->loc.lineNum = pdc->iost.line;
    ed->loc.posNum = (pdc->iost.c - pdc->iost.b) + 1;  /* pos in line of IO cursor */
    ed->loc.begin = ed->loc.end = begin; /* loc of IO cursor */
  }
  return PDC_ERROR;

 range_err:
  if (*em < PDC_Ignore) {
    ed->errCode = PDC_RANGE;
    ed->loc.lineNum = pdc->iost.line;
    ed->loc.posNum = (pdc->iost.c - pdc->iost.b) + 1;  /* loc of IO cursor */
    ed->loc.begin = begin; /* beginning of acsii integer */
    ed->loc.end = pdc->iost.c; /* new loc of IO cursor */
  }
  return PDC_ERROR;
}

/* ================================================================================ */
/* IO FUNCTIONS */

/*
 * PDC_IO_refill: only call when either to initialize IO
 * or when all prior input has been parsed.  Returns PDC_ERROR
 * if there is an error condition on the stream.  This means
 * the caller must check EOF status after then call, as
 * a PDC_OK result does not indicate the EOF status.
 */
PDC_error_t
PDC_IO_refill(PDC_t* pdc, PDC_disc_t* disc)
{
  ssize_t readlen = 0;

  if (!disc) {
    disc = pdc->disc;
  }
  if (pdc->iost.eof) {
    return PDC_OK;
  }
  pdc->iost.buf = sfgetr(pdc->iost.io, '\n', 0);
  readlen = sfvalue(pdc->iost.io);
  if (sferror(pdc->iost.io)) {
    SYSERR(pdc, "Error reading IO stream");
    pdc->iost.eof = 1;
    return PDC_ERROR;
  }
  if (!pdc->iost.buf) { /* check for partial read */
    pdc->iost.buf = sfgetr(pdc->iost.io, 0, SF_LASTR);
    if (sferror(pdc->iost.io)) {
      SYSERR(pdc, "Error reading IO stream");
      pdc->iost.eof = 1;
      return PDC_ERROR;
    }
    if (!pdc->iost.buf) {
      /* hit EOF */
      pdc->iost.eof = 1;
      return PDC_ERROR;
    }
  }
  pdc->iost.b = pdc->iost.c = pdc->iost.buf;
  pdc->iost.e = pdc->iost.buf + readlen;
  pdc->iost.line++;
  if (*(pdc->iost.e - 1) == '\n') { readlen--; } /* XXX_REMOVE */
  WARN3(pdc, "XXX_REMOVE line %d: %-.*s", pdc->iost.line, readlen, pdc->iost.b);
  return PDC_OK;
}

PDC_error_t
PDC_get_loc(PDC_t* pdc, PDC_loc_t* l, PDC_disc_t* disc)
{

  if (!disc) {
    disc = pdc->disc;
  }
  if (!l) {
    WARN(pdc, "PDC_get_loc called with null loc ptr");
    return PDC_ERROR;
  }
  /* XXX the following works for eof case ??? */
  l->lineNum = pdc->iost.line;
  l->posNum  = (pdc->iost.c - pdc->iost.b) + 1;
  l->begin = l->end = pdc->iost.c;
  return PDC_OK;
}

int
PDC_IO_is_EOF(PDC_t* pdc, PDC_disc_t* disc) {
  return (pdc->iost.eof);
}

PDC_error_t
PDC_IO_getchar(PDC_t* pdc, unsigned char* ct, int panicking, PDC_disc_t* disc)
{

  if (!disc) {
    disc = pdc->disc;
  }
  TRACE(pdc, "PDC_IO_getchar called");
  if (pdc->iost.eof) { /* already hit EOF */
    return PDC_ERROR;
  }
  if (panicking && (disc->p_stop == PDC_Line_Stop) && (pdc->iost.c >= pdc->iost.e)) {
    /* do not move beyond newline char / line end */
    return PDC_ERROR;
  }
  if (pdc->iost.c < pdc->iost.e) { /* still more chars on current line */
    *ct = *(pdc->iost.c++);
    return PDC_OK;
  }
  if ((PDC_ERROR == PDC_IO_refill(pdc, disc)) || pdc->iost.eof) {
    return PDC_ERROR;
  }
  *ct = *(pdc->iost.c++);
  return PDC_OK;
}

PDC_error_t
PDC_IO_back(PDC_t* pdc, size_t num_chars, PDC_disc_t* disc)
{
  char* s;

  if (!disc) {
    disc = pdc->disc;
  }
  TRACE(pdc, "PDC_IO_back called");
  if (!pdc->iost.buf) { /* never initialized! */
    return PDC_ERROR;
  }
  s = pdc->iost.c - num_chars;
  if (s < pdc->iost.b) { /* request goes too far back */
    return PDC_ERROR;
  }
  pdc->iost.c = s;
  WARN1(pdc, "XXX_REMOVE    PDC_IO_BACK: moved back %d chars", num_chars);
  return PDC_OK;
}

PDC_error_t
PDC_IO_fopen(PDC_t* pdc, char* path, PDC_disc_t* disc)
{
  if (!disc) {
    disc = pdc->disc;
  }
  TRACE1(pdc, "PDC_IO_fopen called for path = %s", path);
  if (!path) {
    WARN(pdc, "fopen called with null path");
    return PDC_ERROR;
  }
  if (!(pdc->iost.io = sfopen(NiL, path, "r"))) {
    SYSERR1(pdc, "Failed to open file \"%s\"", path);
  }
  if (!(pdc->iost.path = vmnewof(pdc->vm, 0, char, strlen(path) + 1, 0))) {
    WARN(pdc, "out of space [string to record file path]");
    PDC_IO_fclose(pdc, disc);
    return PDC_ERROR;
  }
  strcpy(pdc->iost.path, path);
  /* get line of input */
  PDC_IO_refill(pdc, disc);
  return PDC_OK;
}

PDC_error_t
PDC_IO_fclose(PDC_t* pdc, PDC_disc_t* disc)
{

  if (!disc) {
    disc = pdc->disc;
  }
  TRACE(pdc, "PDC_IO_fclose called");
  if (!pdc->iost.io) {
    return PDC_ERROR;
  }
  if (pdc->iost.io) {
    sfclose(pdc->iost.io);
    pdc->iost.io = 0;
    pdc->iost.eof = 1;
  }
  if (!pdc->vm || !pdc->iost.path) {
    return PDC_ERROR;
  }
  vmfree(pdc->vm, pdc->iost.path);
  pdc->iost.path = 0;
  return PDC_OK;
}

/* ================================================================================ */
/* TOP-LEVEL LIBRARY FUNCTIONS */

/* The default discipline */
static PDC_disc_t PDC_default_disc = {
  PDC_VERSION,
  PDC_errorf,
  PDC_errorvf,
  PDC_Line_Stop
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
  if (!(vm = vmopen(Vmdcheap, Vmbest, 0))) {
    WARN(NiL, "out of space [vm]");
    return PDC_ERROR;
  }
  if (!(pdc = vmnewof(vm, 0, PDC_t, 1, 0))) {
    WARN(NiL, "out of space [dss]");
    vmclose(vm);
    return PDC_ERROR;
  }
  pdc->id    = lib;
  pdc->vm    = vm;
  pdc->disc  = disc;
  (*pdc_out) = pdc;
  return PDC_OK;
}

PDC_error_t
PDC_close(PDC_t* pdc, PDC_disc_t* disc)
{

  if (!disc) {
    disc = pdc->disc;
  }
  TRACE(pdc, "PDC_close called");
  if (!pdc->vm) {
    return PDC_ERROR;
  }
  vmclose(pdc->vm);
  return PDC_OK;
}

/* ================================================================================ */
