/*
 *  adate_test: Test adate_read
 */


#include "padsc-internal.h" /* for testing - normally do not include internal */

int main(int argc, char** argv) {
  PDC_t*          pdc;
  PDC_IO_disc_t*  io_disc;
  PDC_base_m      m = PDC_CheckAndSet;
  PDC_base_pd     pd;
  size_t          bytes_skipped;
  unsigned long   ultmp;
  PDC_uint32      tm;

  error(0, "\nUsing PADSC IO discipline nlrec\n\n");
  io_disc = PDC_nlrec_make(0);

  if (PDC_ERR == PDC_open(&pdc, 0, io_disc)) {
    error(2, "*** PDC_open failed ***");
    exit(-1);
  }
  if (PDC_ERR == PDC_IO_fopen(pdc, "../../data/ex_data.adate_test")) {
    error(2, "*** PDC_IO_fopen failed ***");
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
    /* try to read line with 1 date term by vbar, 1 date term by EOR */
    if (PDC_ERR == PDC_a_date_read(pdc, &m, '|', &pd, &tm)) {
      if (pd.errCode != PDC_INVALID_DATE) {
	goto find_EOR;
      }
    } else {
      error(0, "Read date term by vbar: %s (secs = %lu)", fmttime("%K", (time_t)tm), (unsigned long)tm);
    }
    if (PDC_ERR == PDC_a_char_lit_read(pdc, &m, &pd, '|')) {
      PDCI_report_err (pdc, 0, &pd.loc, pd.errCode, 0, 0);
      goto find_EOR;
    }
    if (PDC_ERR == PDC_a_date_read(pdc, &m, 0, &pd, &tm)) {
      if (pd.errCode != PDC_INVALID_DATE) {
	goto find_EOR;
      }
    } else {
      error(0, "Read date term by NULL: %s (secs = %lu)", fmttime("%K", (time_t)tm), (unsigned long)tm);
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
