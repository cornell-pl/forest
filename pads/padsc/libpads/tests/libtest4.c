/*
 *  libtest1: Test fixed width ascii read functions
 */


#include "padsc-internal.h" /* for testing - normally do not include internal */

int main(int argc, char** argv) {
  /* int             ctr; */
  /* size_t          n; */
  /* unsigned char   c; */
  /* int             i; */
  PDC_string      s;
  PDC_t*          pdc;
  PDC_IO_disc_t*  io_disc;
  PDC_base_m      m    = PDC_CheckAndSet;
  PDC_base_pd     pd;
  PDC_disc_t      my_disc = PDC_default_disc;
  size_t          bytes_skipped;
  unsigned long   ultmp;
  PDC_regexp_t    *my_regexp;

  error(0, "\nUsing PADSC IO discipline nlrec\n\n");
  io_disc = PDC_nlrec_make(0);

  if (PDC_ERR == PDC_open(&pdc, &my_disc, io_disc)) {
    error(2, "*** PDC_open failed ***");
    exit(-1);
  }
  if (PDC_ERR == PDC_IO_fopen(pdc, "../../data/ex_data.libtest4")) {
    error(2, "*** PDC_IO_fopen failed ***");
    exit(-1);
  }

  if (PDC_ERR == PDC_regexp_compile(pdc, "[X]|EOR", &my_regexp)) {
    error(2, "** unexpected regexp compile failure **");
    exit(-1);
  }

  PDC_string_init(pdc, &s);

  /*
   * XXX Process the data here XXX
   */
  while (1) {
    if (PDC_IO_at_EOF(pdc)) {
      error(0, "Main program found eof");
      break;
    }
    /* try to read line with 2 strings term by vbar 1 string term by EOR */
    if (PDC_ERR == PDC_a_string_read(pdc, &m, '|', &pd, &s)) {
      goto find_EOR;
    } else {
      error(0, "Read string term by vbar: %s (length %d)", PDC_fmt_str(&s), s.len);
    }
    if (PDC_ERR == PDC_a_char_lit_read(pdc, &m, &pd, '|')) {
      PDCI_report_err (pdc, 0, &pd.loc, pd.errCode, 0, 0);
      goto find_EOR;
    }
    if (PDC_ERR == PDC_a_string_read(pdc, &m, '|', &pd, &s)) {
      goto find_EOR;
    } else {
      error(0, "Read string term by vbar: %s (length %d)", PDC_fmt_str(&s), s.len);
    }
    if (PDC_ERR == PDC_a_char_lit_read(pdc, &m, &pd, '|')) {
      PDCI_report_err (pdc, 0, &pd.loc, pd.errCode, 0, 0);
      goto find_EOR;
    }
    if (PDC_ERR == PDC_a_string_CSE_read(pdc, &m, my_regexp, &pd, &s)) {
      break;
    } else {
      error(0, "Read string term by EOR or X : %s (length %d)", PDC_fmt_str(&s), s.len);
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
  PDC_string_cleanup(pdc, &s);

  if (PDC_ERR == PDC_IO_close(pdc)) {
    error(2, "*** PDC_IO_close failed ***");
    exit(-1);
  }

  if (PDC_ERR == PDC_close(pdc)) {
    error(2, "*** PDC_close failed ***");
    exit(-1);
  }

  return 0;
}
