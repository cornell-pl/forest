/*
 *  libtest1: Test fixed width ascii read functions
 */


#include "libpadsc-internal.h" /* for testing - normally do not include internal */

int main(int argc, char** argv) {
  PDC_string      s1, s2, s3, s4, s5, s6;
  PDC_string      *str1, *str2, *str3;
  PDC_t*          pdc;
  PDC_IO_disc_t*  io_disc;
  PDC_base_em     em = PDC_CheckAndSet;
  PDC_base_ed     ed;
  PDC_disc_t      my_disc = PDC_default_disc;
  size_t          bytes_skipped;
  unsigned long   ultmp;
  PDC_regexp_t    *my_regexp;
  int             ctr = 0;


  /*  error(0, "\nUsing PADSC IO discipline fwec_noseek\n\n"); */
  /*  io_disc = PDC_fwrec_noseek_make(0, 20, 1); */

  error(0, "\nUsing PADSC IO discipline nlrec_noseek with block size 42\n\n");
  io_disc = PDC_nlrec_noseek_make(42);

  if (PDC_ERR == PDC_open(&pdc, &my_disc, io_disc)) {
    error(2, "*** PDC_open failed ***");
    exit(-1);
  }
  if (PDC_ERR == PDC_IO_fopen(pdc, "../../data/ex_data.string_share_test")) {
    error(2, "*** PDC_IO_fopen failed ***");
    exit(-1);
  }

  if (PDC_ERR == PDC_regexp_compile(pdc, "[X]|EOR", &my_regexp)) {
    error(2, "** unexpected regexp compile failure **");
    exit(-1);
  }
  PDC_string_init(pdc, &s1);
  PDC_string_init(pdc, &s2);
  PDC_string_init(pdc, &s3);
  PDC_string_init(pdc, &s4);
  PDC_string_init(pdc, &s5);
  PDC_string_init(pdc, &s6);

  /*
   * XXX Process the data here XXX
   */

  /* XXX_REMOVE: */  /*   PDC_IO_checkpoint(pdc, 0); */
  /* XXX_REMOVE: */  /*   pdc->disc->copy_strings = 1; */
  while (1) {
    ctr++;
    switch (ctr % 2) {
    case 0:
      str1 = &s1;
      str2 = &s2;
      str3 = &s3;
      break;
    case 1:
      str1 = &s4;
      str2 = &s5;
      str3 = &s6;
      break;
    }
    if (PDC_IO_at_EOF(pdc)) {
      error(0, "Main program found eof");
      break;
    }
    /* try to read line with 2 strings term by vbar 1 string term by EOR */
    if (PDC_ERR == PDC_astring_read(pdc, &em, '|', &ed, str1)) {
      goto find_EOR;
    } else {
      error(0, "Read string term by vbar: %s (length %d)", PDC_fmt_str(str1), str1->len);
    }
    if (PDC_ERR == PDC_char_lit_read(pdc, &em, &ed, '|')) {
      PDCI_report_err (pdc, 0, &ed.loc, ed.errCode, 0);
      goto find_EOR;
    }
    if (PDC_ERR == PDC_astring_read(pdc, &em, '|', &ed, str2)) {
      goto find_EOR;
    } else {
      error(0, "Read string term by vbar: %s (length %d)", PDC_fmt_str(str2), str2->len);
    }
    if (PDC_ERR == PDC_char_lit_read(pdc, &em, &ed, '|')) {
      PDCI_report_err (pdc, 0, &ed.loc, ed.errCode, 0);
      goto find_EOR;
    }
    if (PDC_ERR == PDC_astringCSE_read(pdc, &em, my_regexp, &ed, str3)) {
      break;
    } else {
      error(0, "Read string term by EOR or X : %s (length %d)", PDC_fmt_str(str3), str3->len);
    }
  find_EOR:
    if (PDC_ERR == PDC_IO_next_rec(pdc, &bytes_skipped)) {
      error(2, "Could not find EOR (newline), ending program");
      goto done;
    }
    ultmp = bytes_skipped;
    error(0, "bytes_skipped to find EOR/newline = %ld", ultmp);
  }
  /* XXX_REMOVE */ /*   PDC_IO_commit(pdc); */

 done:
  PDC_string_cleanup(pdc, &s1);
  PDC_string_cleanup(pdc, &s2);
  PDC_string_cleanup(pdc, &s3);

  if (PDC_ERR == PDC_IO_fclose(pdc)) {
    error(2, "*** PDC_IO_fclose failed ***");
    exit(-1);
  }

  PDC_string_cleanup(pdc, &s4);
  PDC_string_cleanup(pdc, &s5);
  PDC_string_cleanup(pdc, &s6);

  if (PDC_ERR == PDC_close(pdc)) {
    error(2, "*** PDC_close failed ***");
    exit(-1);
  }

  return 0;
}
