#pragma prototyped
/*
 * library routines for library that goes with padsc
 *
 * Kathleen Fisher, Robert Gruber
 * AT&T Labs Research
 */

#include "libpadsc-internal.h"

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
  while (PDC_OK == PDC_IO_getchar(pdc, &ct, 1, disc)) { /* 1 means panicking */
    if (c == ct) {
      return PDC_OK;  /* IO cursor is one beyond c */
    }
  }
  /* IO cursor moved an arbitrary amount, limited by disc->p_stop setting */
  return PDC_ERROR;
}

/* ================================================================================ */
/* IO FUNCTIONS */

PDC_error_t
PDC_IO_getchar(PDC_t* pdc, unsigned char* ct, int panicking, PDC_disc_t* disc) {
  if (!disc) {
    disc = pdc->disc;
  }
  if (pdc->iost.eof) { /* already hit EOF */
    return PDC_ERROR;
  }
  if (panicking && (disc->p_stop == PDC_Line_Stop) && (pdc->iost.c == pdc->iost.e - 1)) {
    /* do not move beyond newline char */
    return PDC_ERROR;
  }
  if (pdc->iost.c < pdc->iost.e) { /* still more chars on current line */
    *ct = *(pdc->iost.c++);
    return PDC_OK;
  }
  /* attempt to read a new line of input */
  if (!(pdc->iost.buf = sfgetr(pdc->iost.io, '\n', 0))) {
    /* hit EOF */
    pdc->iost.eof = 1;
    return PDC_ERROR;
  }
  pdc->iost.b = pdc->iost.c = pdc->iost.buf;
  pdc->iost.e = pdc->iost.buf + sfvalue(pdc->iost.io);
  pdc->iost.line++;
  *ct = *(pdc->iost.c++);
  return PDC_OK;
}

PDC_error_t
PDC_IO_back(PDC_t* pdc, size_t num_chars, PDC_disc_t* disc) {
  char* s;

  if (!disc) {
    disc = pdc->disc;
  }
  if (!pdc->iost.buf) { /* never initialized! */
    return PDC_ERROR;
  }
  s = pdc->iost.c - num_chars;
  if (s < pdc->iost.b) { /* request goes too far back */
    return PDC_ERROR;
  }
  pdc->iost.c = s;
  return PDC_OK;
}

PDC_error_t
PDC_IO_fopen(PDC_t* pdc, char* path, PDC_disc_t* disc) {
  if (!disc) {
    disc = pdc->disc;
  }
  if (!path) {
    if (disc->errorf) {
      disc->errorf(pdc, disc, 2, "fopen called with null path");
    }
    return PDC_ERROR;
  }
  if (!(pdc->iost.io = sfopen(NiL, path, "r"))) {
    if (disc->errorf) {
      disc->errorf(pdc, disc, 2, "Could not open file %s", path);
    }
  }
  if (!(pdc->iost.path = vmnewof(pdc->vm, 0, char, strlen(path) + 1, 0))) {
    if (disc->errorf) {
      disc->errorf(pdc, disc, 2, "out of space [string to record file path]");
    }
    PDC_IO_fclose(pdc, disc);
    return PDC_ERROR;
  }
  strcpy(pdc->iost.path, path);
  /* get first line of input */
  if (!(pdc->iost.buf = sfgetr(pdc->iost.io, '\n', 0))) {
    /* hit EOF */
    pdc->iost.eof = 1;
    return PDC_OK; /* OK but not very useful ! */
  }
  pdc->iost.b = pdc->iost.c = pdc->iost.buf;
  pdc->iost.e = pdc->iost.buf + sfvalue(pdc->iost.io);
  pdc->iost.line++;
  return PDC_OK;
}

PDC_error_t
PDC_IO_fclose(PDC_t* pdc, PDC_disc_t* disc) {
  if (!disc) {
    disc = pdc->disc;
  }
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

PDC_error_t
PDC_open(PDC_disc_t* disc, PDC_t** pdc_out) {
  /* XXX_TODO */
  return PDC_OK;
}

PDC_error_t
PDC_close(PDC_t* pdc, PDC_disc_t* disc) {
  if (!disc) {
    disc = pdc->disc;
  }
  /* XXX_TODO */
  return PDC_OK;
}

/* ================================================================================ */
