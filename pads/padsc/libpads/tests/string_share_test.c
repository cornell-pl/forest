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
  size_t          bytes_skipped;
  unsigned long   ultmp;
  PDC_regexp_t    *my_regexp;
  int             ctr = 0;
  int             norec = 0; 

  if (argc != 2) {
    goto usage;
  }

  if (strcmp(argv[1], "fwrec") == 0) {
    io_disc = PDC_fwrec_make(0, 20, 1);
  } else if (strcmp(argv[1], "nlrec") == 0) {
    io_disc = PDC_nlrec_make(21);
  } else if (strcmp(argv[1], "norec") == 0) {
    io_disc = PDC_norec_make(21);
    norec = 1;
  } else if (strcmp(argv[1], "fwrec_noseek") == 0) {
    io_disc = PDC_fwrec_noseek_make(0, 20, 1); /* 4 6-char ints, newline */ 
  } else if (strcmp(argv[1], "nlrec_noseek") == 0) {
    io_disc = PDC_nlrec_noseek_make(21);
  } else if (strcmp(argv[1], "norec_noseek") == 0) {
    io_disc = PDC_norec_noseek_make(21);
    norec = 1;
  } else {
    goto usage;
  }
  if (!io_disc) {
    error(ERROR_FATAL, "\nFailed to install IO discipline %s", argv[1]);
  } else {
    error(0, "\nInstalled IO discipline %s block size 21", argv[1]);
  }

  if (PDC_ERR == PDC_open(&pdc, 0, io_disc)) {
    error(2, "*** PDC_open failed ***");
    return -1;
  }
  if (PDC_ERR == PDC_IO_fopen(pdc, "../../data/ex_data.string_share_test")) {
    error(2, "*** PDC_IO_fopen failed ***");
    return -1;
  }

  if (norec) {
    if (PDC_ERR == PDC_regexp_compile(pdc, "[X\n]", &my_regexp)) {
      error(2, "** unexpected regexp compile failure **");
      return -1;
    }
  } else {
    if (PDC_ERR == PDC_regexp_compile(pdc, "[X]|EOR", &my_regexp)) {
      error(2, "** unexpected regexp compile failure **");
      return -1;
    }
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

  /* XXX_REMOVE */   /* PDC_IO_checkpoint(pdc, 0);   */
  /* XXX_REMOVE */   /* pdc->disc->copy_strings = 1; */
  while (1) {
    if (ctr % 10 == 0) {
      str1 = &s1;
      str2 = &s2;
      str3 = &s3;
    } else {
      str1 = &s4;
      str2 = &s5;
      str3 = &s6;
    }
    ctr++;
    if (PDC_IO_at_EOF(pdc)) {
      error(0, "Main program found eof");
      break;
    }
    /* try to read line with 2 strings term by vbar 1 string term by EOR */
    if (PDC_ERR == PDC_a_string_read(pdc, &em, '|', &ed, str1)) {
      goto find_EOR;
    } else {
      error(0, "Read string term by vbar: %s (length %d)", PDC_fmt_str(str1), str1->len);
    }
    if (PDC_ERR == PDC_a_char_lit_read(pdc, &em, &ed, '|')) {
      PDCI_report_err (pdc, 0, &ed.loc, ed.errCode, 0, 0);
      goto find_EOR;
    }
    if (PDC_ERR == PDC_a_string_read(pdc, &em, '|', &ed, str2)) {
      goto find_EOR;
    } else {
      error(0, "Read string term by vbar: %s (length %d)", PDC_fmt_str(str2), str2->len);
    }
    if (PDC_ERR == PDC_a_char_lit_read(pdc, &em, &ed, '|')) {
      PDCI_report_err (pdc, 0, &ed.loc, ed.errCode, 0, 0);
      goto find_EOR;
    }
    if (PDC_ERR == PDC_a_string_CSE_read(pdc, &em, my_regexp, &ed, str3)) {
      break;
    } else {
      error(0, "Read string term by EOR(newline) or X : %s (length %d)", PDC_fmt_str(str3), str3->len);
    }
  find_EOR:
    if (norec) {
      if (PDC_ERR == PDC_a_char_lit_scan(pdc, '\n', '\n', 1, 0, &bytes_skipped)) {
	error(2, "Could not find EOR (newline), ending program");
	goto done;
      }
    } else {
      if (PDC_ERR == PDC_IO_next_rec(pdc, &bytes_skipped)) {
	error(2, "Could not find EOR (newline), ending program");
	goto done;
      }
    }
    ultmp = bytes_skipped;
    error(0, "bytes_skipped to find EOR/newline = %ld", ultmp);
  }
  /* XXX_REMOVE */ /* PDC_IO_commit(pdc); */

 done:
  PDC_string_cleanup(pdc, &s1);
  PDC_string_cleanup(pdc, &s2);
  PDC_string_cleanup(pdc, &s3);

  if (PDC_ERR == PDC_IO_close(pdc)) {
    error(2, "*** PDC_IO_close failed ***");
    return -1;
  }

  PDC_string_cleanup(pdc, &s4);
  PDC_string_cleanup(pdc, &s5);
  PDC_string_cleanup(pdc, &s6);

  if (PDC_ERR == PDC_close(pdc)) {
    error(2, "*** PDC_close failed ***");
    return -1;
  }

  return 0;

 usage:
  error(2, "\nUsage: %s <io-disc-name>\n\n\twhere <io-disc-name> is one of: fwrec, nlrec, norec, fwrec_noseek, nlrec_noseek, norec_noseek\n", argv[0]);
  return -1;
}
