#pragma prototyped
/*
 * library routines for library that goes with padsc
 *
 * Kathleen Fisher, Robert Gruber
 * AT&T Labs Research
 */

#include "libpadsc-internal.h"
#include <ctype.h>

static const char id[] = "\n@(#)$Id: pads.c,v 1.4 2002-08-22 16:44:31 gruber Exp $\0\n";

static const char lib[] = "padsc";

/* ================================================================================ */ 
/* IMPL CONSTANTS */

#define PDC_initStkElts      8
#define PDC_initInpBufs      8
#define PDC_initBufSize   1024

/* ================================================================================ */ 
/* INTERNAL ERROR REPORTING FUNCTIONS */

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
PDC_report_err(PDC_t* pdc, PDC_disc_t* disc, int level, PDC_loc_t* loc, int errCode, const char* format, ...)
{
  if (!disc) {
    disc = pdc->disc;
  }
  TRACE(pdc, "PDC_report_err called");
  if (disc->e_rep == PDC_errorRep_None || !disc->errorf) {
    return PDC_OK;
  }
  if (disc->e_rep == PDC_errorRep_Min) {
    disc->errorf(pdc, disc, level, "line %d char %d errCode %d", loc->beginLine, loc->beginChar, errCode);
    return PDC_OK;
  }
  if (format && strlen(format)) {
    va_list ap;
    sfprintf(pdc->tmp, "line %d char %d : ", loc->beginLine, loc->beginChar);
    va_start(ap, format);
    sfvprintf(pdc->tmp, format, ap);
    va_end(ap);
  } else {
    const char* msg = "** unknown error code **";

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
    case PDC_ARRAY_MIN_BIGGER_THAN_MAX_ERR:
      msg = "Array min bigger than array max";
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
    case PDC_AT_EOF:
      msg = "Unexpected EOF failure";
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
    case PDC_CHAR_LIT_NOT_FOUND:
      msg = "Expected character literal not found";
      break;
    }
    sfprintf(pdc->tmp, "line %d char %d : %s ", loc->beginLine, loc->beginChar, msg);
  }
  if (disc->e_rep == PDC_errorRep_Max) {
    char* buf;
    if (PDC_OK == PDC_IO_getLineBuf(pdc, loc->endLine, &buf, disc)) {
      size_t minc = (loc->beginLine == loc->endLine) ? loc->beginChar : 1;
      size_t maxc = loc->endChar;
      if (minc < maxc) {
	sfprintf(pdc->tmp, "\n[LINE %d]%-.*s>>>%-.*s<<<", loc->endLine, minc-1, buf, maxc-minc+1, buf+minc-1);
      } else {
	sfprintf(pdc->tmp, "\n[LINE %d]%-.*s<<<", loc->endLine, maxc, buf);
      }
    }
  }
  disc->errorf(pdc, disc, level, "%s", sfstruse(pdc->tmp));
  return PDC_OK;
}

/* ================================================================================ */
/* SCAN FUNCTIONS */

PDC_error_t
PDC_char_lit_scan(PDC_t* pdc, PDC_base_em* em,
		  PDC_base_ed* ed, unsigned char c, PDC_disc_t* disc)
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
		  PDC_base_ed* ed, unsigned char c, PDC_disc_t* disc)
{
  unsigned char   ct;
  PDC_base_em     emt = PDC_CheckAndSet;
  PDC_base_ed     edt = {0};

  if (!disc) {
    disc = pdc->disc;
  }
  TRACE1(pdc, "PDC_char_lit_read called for char = %c", c);
  if (!em) {
    em = &emt;
  }
  if (!ed) {
    ed = &edt;
  }
  if (PDC_OK == PDC_IO_getchar(pdc, &ct, 0, disc)) {
    if (c == ct) {
      return PDC_OK;  /* IO cursor is one beyond c */
    }
    /* wrong char -- put it back */
    PDC_IO_back(pdc, 1, disc);
  }
  if (*em < PDC_Ignore) {
    ed->errCode = PDC_CHAR_LIT_NOT_FOUND;
    PDC_get_loc(pdc, &(ed->loc), disc); /* set loc begin/end to current IO pos */
  }
  return PDC_ERROR;
}

PDC_error_t
PDC_auint32_read(PDC_t* pdc, PDC_base_em* em,
		 PDC_base_ed* ed, PDC_uint32* res_out, PDC_disc_t* disc)
{
  PDC_stkElt_t*   tp;    /* stack top */
  unsigned char   ct;    /* char tmp */
  unsigned long   tul;   /* tmp unsigned long */
  char*           tcp;   /* tmp char* */
  char*           begin; /* IO cursor at beginning of call */
  PDC_base_em     emt = PDC_CheckAndSet;
  PDC_base_ed     edt = {0};

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
  /* peek at first char -- ensures we are on line with at least one char to parse */
  if (PDC_ERROR == PDC_IO_getchar(pdc, &ct, 0, disc)) {
    goto at_eof_err;
  }
  PDC_IO_back(pdc, 1, disc);
  if (isspace(ct)) {  /* strtol allows white space so must do this check first */
    goto bad_prefix_err;
  }
  tp = &(pdc->stack[pdc->top]);
  begin = (tp->idx == pdc->itail) ? pdc->sfbuf : pdc->buf;
  begin += tp->cur;
  tul = strtoul(begin, &tcp, 10);
  if (tcp==0 || tcp==begin) {
    goto bad_prefix_err;
  }
  tp->cur += (tcp - begin); /* advance IO cursor */
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
    /* set loc begin/end to current IO pos = 1 past end char */
    PDC_get_loc(pdc, &(ed->loc), disc);
  }
  return PDC_ERROR;

 bad_prefix_err:
  if (*em < PDC_Ignore) {
    ed->errCode = PDC_INVALID_AUINT;
    PDC_get_loc(pdc, &(ed->loc), disc); /* set loc begin/end to current IO pos */
  }
  return PDC_ERROR;

 range_err:
  if (*em < PDC_Ignore) {
    ed->errCode = PDC_RANGE;
    PDC_get_loc(pdc, &(ed->loc), disc); /* set loc begin/end to current IO pos */
    ed->loc.beginChar -= (tcp - begin);  /* move loc begin to start of number */ 
  }
  return PDC_ERROR;
}

/* ================================================================================ */
/* IO FUNCTIONS */

/*
 * PDC_IO_refill: only call either to initialize IO during fopen
 * or when all prior input has been parsed.  Returns PDC_ERROR
 * if there is an error condition on the stream.  This means
 * the caller must check EOF status after then call, as
 * a PDC_OK result does not indicate the EOF status.
 */
PDC_error_t
PDC_IO_refill(PDC_t* pdc, PDC_disc_t* disc)
{
  ssize_t         readlen;
  PDC_IO_line_t*  readline;
  PDC_IO_line_t*  latestline  = &(pdc->ilines[pdc->itail]);
  PDC_stkElt_t*   tp          = &(pdc->stack[pdc->top]);

  if (!disc) {
    disc = pdc->disc;
  }
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
    return PDC_OK;
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
	pdc->eof = 1;
	return PDC_ERROR;
      }
    }
    if (pdc->bchars + (latestline->eoffset - latestline->boffset) > pdc->balloc) {
      while (pdc->bchars + (latestline->eoffset - latestline->boffset) > pdc->balloc) {
	pdc->balloc *= 2;
      }
      if (!(pdc->buf = vmnewof(pdc->vm, pdc->buf, char, pdc->balloc, 0))) {
	WARN(pdc, "out of space [shadow buf]");
	pdc->eof = 1;
	return PDC_ERROR;
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
    pdc->eof = 1;
    return PDC_ERROR;
  }
  if (!pdc->sfbuf) { /* check for partial read */
    pdc->sfbuf = sfgetr(pdc->io, 0, SF_LASTR);
    if (sferror(pdc->io)) {
      SYSERR(pdc, "Error reading IO stream");
      pdc->eof = 1;
      return PDC_ERROR;
    }
    if (!pdc->sfbuf || readlen == 0) {
      /* hit EOF */
      pdc->eof = 1;
      return PDC_ERROR;
    }
  }
  readline->boffset = tp->cur = 0;
  readline->eoffset = readlen;
  if (*(pdc->sfbuf + readlen - 1) == '\n') { readlen--; } /* XXX_REMOVE */
  WARN3(pdc, "XXX_REMOVE line %d: %-.*s", pdc->lnum, readlen, pdc->sfbuf);
  return PDC_OK;
}

PDC_error_t
PDC_get_loc(PDC_t* pdc, PDC_loc_t* l, PDC_disc_t* disc)
{
  PDC_stkElt_t*   tp          = &(pdc->stack[pdc->top]);
  PDC_IO_line_t*  tpline      = &(pdc->ilines[tp->idx]);

  if (!disc) {
    disc = pdc->disc;
  }
  if (!l) {
    WARN(pdc, "PDC_get_loc called with null loc ptr");
    return PDC_ERROR;
  }
  l->beginLine = l->endLine = tpline->lnum;
  l->beginChar = l->endChar = (tp->cur - tpline->boffset) + 1;
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

  if (!disc) {
    disc = pdc->disc;
  }
  TRACE(pdc, "PDC_IO_peek_EOF called");
  if (PDC_ERROR == PDC_IO_getchar(pdc, &ct, 0, disc)) {
    return 1;
  }
  PDC_IO_back(pdc, 1, disc);
  return 0;
}

PDC_error_t
PDC_IO_getchar(PDC_t* pdc, unsigned char* ct, int panicking, PDC_disc_t* disc)
{
  PDC_stkElt_t*   tp          = &(pdc->stack[pdc->top]);
  PDC_IO_line_t*  tpline      = &(pdc->ilines[tp->idx]);
  char* base;

  if (!disc) {
    disc = pdc->disc;
  }
  TRACE(pdc, "PDC_IO_getchar called");
  while (1) {
    if (PDC_IO_is_EOF(pdc, disc)) { /* already hit EOF */
      return PDC_ERROR;
    }
    if (panicking && (disc->p_stop == PDC_Line_Stop) && (tp->cur >= tpline->eoffset)) {
      /* do not move beyond newline char / line end */
      return PDC_ERROR;
    }
    if (tp->cur < tpline->eoffset) { /* still more chars on current line */
      break;
    }
    if (tp->idx < pdc->itail) { /* advance to next in-memory input line */
      tp->idx++;
      tp->cur = 0;
      tpline = &(pdc->ilines[tp->idx]);
    } else {
      /* hit end of in-memory input lines, must get next line */
      if (PDC_ERROR == PDC_IO_refill(pdc, disc)) {
	return PDC_ERROR;
      }
    }
    /* go to top of loop to do eof and panic checks again */
  }
  base = (tp->idx == pdc->itail) ? pdc->sfbuf : pdc->buf;
  *ct = *(base + (tp->cur++));
  return PDC_OK;
}

PDC_error_t
PDC_IO_back(PDC_t* pdc, size_t num_chars, PDC_disc_t* disc)
{
  PDC_stkElt_t*   tp          = &(pdc->stack[pdc->top]);
  PDC_IO_line_t*  tpline      = &(pdc->ilines[tp->idx]);
  size_t avail_this_line, todo;

  if (!disc) {
    disc = pdc->disc;
  }
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
      tp->idx--;
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
  pdc->top = 0;  /* reset checkpoint stack */
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
PDC_IO_checkpoint(PDC_t* pdc, PDC_disc_t* disc)
{
  if (!disc) {
    disc = pdc->disc;
  }
  TRACE(pdc, "PDC_IO_checkpoint called");
  if (++(pdc->top) >= pdc->salloc) {
    DBG2(pdc, "XXX Growing from %d to %d checkpoint stack slots", pdc->salloc, 2*pdc->salloc);
    pdc->salloc *= 2;
    if (!(pdc->stack = vmnewof(pdc->vm, pdc->stack, PDC_stkElt_t, pdc->salloc, 0))) {
      WARN(pdc, "out of space [input cursor stack]");
      return PDC_ERROR;
    }
  }
  pdc->stack[pdc->top] = pdc->stack[pdc->top - 1];
  return PDC_OK;
}

PDC_error_t
PDC_IO_restore(PDC_t* pdc, PDC_disc_t* disc)
{
  if (!disc) {
    disc = pdc->disc;
  }
  TRACE(pdc, "PDC_IO_restore called");
  if (pdc->top <= 0) {
    WARN(pdc, "Internal error: PDC_IO_restore called when stack top <= 0");
    return PDC_ERROR;
  }
  /* this discards all changes since the latest checkpoint */ 
  pdc->stack[pdc->top] = pdc->stack[pdc->top - 1];
  return PDC_OK;
}

PDC_error_t
PDC_IO_commit(PDC_t* pdc, PDC_disc_t* disc)
{
  if (!disc) {
    disc = pdc->disc;
  }
  TRACE(pdc, "PDC_IO_commit called");
  if (pdc->top <= 0) {
    WARN(pdc, "Internal error: PDC_IO_commit called when stack top <= 0");
    return PDC_ERROR;
  }
  /* propagate changes up to next level */
  pdc->stack[pdc->top - 1] = pdc->stack[pdc->top];
  pdc->top--;
  return PDC_OK;
}

PDC_error_t
PDC_IO_getLineBuf(PDC_t* pdc, size_t line, char** buf_out, PDC_disc_t* disc) {
  char* base;
  int i;

  if (!disc) {
    disc = pdc->disc;
  }
  TRACE(pdc, "PDC_IO_getLineBuf called");
  for (i = 0; i <= pdc->itail; i++) {
    if (pdc->ilines[i].lnum == line) {
      base = (i == pdc->itail) ? pdc->sfbuf : pdc->buf;
      (*buf_out) = base + pdc->ilines[i].boffset;
      return PDC_OK;
    }
  }
  return PDC_ERROR;
}



/* ================================================================================ */
/* TOP-LEVEL LIBRARY FUNCTIONS */

/* The default discipline */
static PDC_disc_t PDC_default_disc = {
  PDC_VERSION,
  PDC_errorf,
  PDC_errorRep_Max,
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
    WARN(NiL, "out of space [padsc library data]");
    vmclose(vm);
    return PDC_ERROR;
  }
  if (!(pdc->tmp = sfstropen())) {
    WARN(NiL, "out of space [sfstr]");
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
  pdc->bchars = 0;
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
  vmclose(pdc->vm); /* frees everything alloc'd using vm */
  return PDC_OK;
}

/* ================================================================================ */
/* MISC ROUTINES */

char*
PDC_fmtChar(char c) {
  char buf[2];
  buf[0] = c;
  buf[1] = 0;
  return (fmtesc(buf));
}

/* ================================================================================ */
