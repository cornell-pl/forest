/*
 *  libtest1: Test fixed width ascii read functions
 */


#include "libpadsc-internal.h" /* for testing - normally do not include internal */

int main(int argc, char** argv) {
  /* int             ctr; */
  /* size_t          n; */
  /* unsigned char   c; */
  /* int             i; */
  PDC_string      s;
  PDC_t*          pdc;
  PDC_base_em     em = PDC_CheckAndSet;
  PDC_base_ed     ed;
  PDC_disc_t      my_disc = PDC_default_disc;

  if (PDC_ERROR == PDC_open(0, &pdc)) {
    error(2, "*** PDC_open failed ***");
    exit(-1);
  }
  if (PDC_ERROR == PDC_IO_fopen(pdc, "../ex_data.libtest4", &my_disc)) {
    error(2, "*** PDC_IO_fopen failed ***");
    exit(-1);
  }

  /*
   * XXX Process the data here XXX
   */
  while (1) {
    if (PDC_IO_peek_EOF(pdc, &my_disc)) {
      error(0, "Main program found eof");
      break;
    }
    /* try to read line with 2 strings term by vbar 1 string term by newline */
    if (PDC_ERROR == PDC_string_stopChar_read(pdc, &em, '|', &ed, &s, &my_disc)) {
      PDC_report_err (pdc, &my_disc, 0, &ed.loc, ed.errCode, "Cannot find vbar stop char");
      goto find_newline;
    } else {
      error(0, "Read string term by vbar: %s (length %d)", s.str, s.len);
    }
    if (PDC_ERROR == PDC_char_lit_read(pdc, &em, &ed, '|', 0)) {
      PDC_report_err (pdc, &my_disc, 0, &ed.loc, ed.errCode, 0);
      goto find_newline;
    }
    if (PDC_ERROR == PDC_string_stopChar_read(pdc, &em, '|', &ed, &s, &my_disc)) {
      PDC_report_err (pdc, &my_disc, 0, &ed.loc, ed.errCode, "Cannot find vbar stop char");
      goto find_newline;
    } else {
      error(0, "Read string term by vbar: %s (length %d)", s.str, s.len);
    }
    if (PDC_ERROR == PDC_char_lit_read(pdc, &em, &ed, '|', 0)) {
      PDC_report_err (pdc, &my_disc, 0, &ed.loc, ed.errCode, 0);
      goto find_newline;
    }
    if (PDC_ERROR == PDC_string_stopRegexp_read(pdc, &em, "[\nX]", &ed, &s, &my_disc)) {
      PDC_report_err (pdc, &my_disc, 0, &ed.loc, ed.errCode, "Cannot find newline or X stop chars, ending program");
      break;
    } else {
      error(0, "Read string term by newline or X : %s (length %d)", s.str, s.len);
    }
  find_newline:
    if (PDC_ERROR == PDC_char_lit_scan(pdc, '\n', '\n', 0, 0, &my_disc)) {
      error(2, "Could not find newline, ending program");
      break;
    }
  }

  if (PDC_ERROR == PDC_IO_fclose(pdc, &my_disc)) {
    error(2, "*** PDC_IO_fclose failed ***");
    exit(-1);
  }

  if (PDC_ERROR == PDC_close(pdc, &my_disc)) {
    error(2, "*** PDC_close failed ***");
    exit(-1);
  }

  return 0;
}
