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
  PDC_IO_disc_t*  io_disc;
  PDC_base_em     em = PDC_CheckAndSet;
  PDC_base_ed     ed;
  PDC_disc_t      my_disc = PDC_default_disc;
  size_t          bytes_skipped;
  unsigned long   ultmp;
  PDC_regexp_t    *my_regexp;

  printf("\nUsing PADSC IO discipline nlrec\n\n");
  io_disc = PDC_nlrec_make(0);

  if (PDC_ERR == PDC_open(&pdc, &my_disc, io_disc)) {
    error(2, "*** PDC_open failed ***");
    exit(-1);
  }
  if (PDC_ERR == PDC_IO_fopen(pdc, "../ex_data.libtest4")) {
    error(2, "*** PDC_IO_fopen failed ***");
    exit(-1);
  }

  if (PDC_ERR == PDC_regexp_compile(pdc, "[X]|EOR", &my_regexp)) {
    error(2, "** unexpected regexp compile failure **");
    exit(-1);
  }

  /*
   * XXX Process the data here XXX
   */
  while (1) {
    if (PDC_IO_at_EOF(pdc)) {
      error(0, "Main program found eof");
      break;
    }
    /* try to read line with 2 strings term by vbar 1 string term by EOR */
    if (PDC_ERR == PDC_string_stopChar_read(pdc, &em, '|', &ed, &s)) {
      goto find_EOR;
    } else {
      error(0, "Read string term by vbar: %s (length %d)", s.str, s.len);
    }
    if (PDC_ERR == PDC_char_lit_read(pdc, &em, &ed, '|')) {
      PDCI_report_err (pdc, 0, &ed.loc, ed.errCode, 0);
      goto find_EOR;
    }
    if (PDC_ERR == PDC_string_stopChar_read(pdc, &em, '|', &ed, &s)) {
      goto find_EOR;
    } else {
      error(0, "Read string term by vbar: %s (length %d)", s.str, s.len);
    }
    if (PDC_ERR == PDC_char_lit_read(pdc, &em, &ed, '|')) {
      PDCI_report_err (pdc, 0, &ed.loc, ed.errCode, 0);
      goto find_EOR;
    }
    if (PDC_ERR == PDC_string_stopRegexp_read(pdc, &em, my_regexp, &ed, &s)) {
      break;
    } else {
      error(0, "Read string term by EOR or X : %s (length %d)", s.str, s.len);
    }
  find_EOR:
    if (PDC_ERR == PDC_IO_next_rec(pdc, &bytes_skipped)) {
      error(2, "Could not find EOR (newline), ending program");
      goto done;
    }
    ultmp = bytes_skipped;
    error(0, "bytes_skipped to find EOR/newline = %ld", ultmp);
  }

 done:
  if (PDC_ERR == PDC_IO_fclose(pdc)) {
    error(2, "*** PDC_IO_fclose failed ***");
    exit(-1);
  }

  if (PDC_ERR == PDC_close(pdc)) {
    error(2, "*** PDC_close failed ***");
    exit(-1);
  }

  return 0;
}
