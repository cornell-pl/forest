/*
 * ebcdic_libtest4: 
 *
 *    tests:
 *        io discipline ctrec_noseek(PDC_EBCDIC_NEWLINE)
 *        PDC_e_string_read
 *        PDC_e_char_lit_read
 *        PDC_e_string_CSE_read
 *        PDC_e_string_FW_read 
 *        PDC_e_string_SE_read
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
  PDC_base_csm    csm = PDC_CheckAndSet;
  PDC_base_ed     ed;
  PDC_disc_t      my_disc = PDC_default_disc;
  size_t          bytes_skipped;
  unsigned long   ultmp;
  PDC_regexp_t    *my_regexp;

  error(0, "\nUsing PADSC IO discipline ctrec with cterm PDC_EBCDIC_NEWLINE\n\n");
  io_disc = PDC_ctrec_noseek_make(PDC_EBCDIC_NEWLINE, 0);

  if (PDC_ERR == PDC_open(&pdc, &my_disc, io_disc)) {
    error(2, "*** PDC_open failed ***");
    exit(-1);
  }
  if (PDC_ERR == PDC_IO_fopen(pdc, "../../data/ex_data.libtest4.ebcdic")) {
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
    if (PDC_ERR == PDC_e_string_read(pdc, &csm, '|', &ed, &s)) {
      goto find_EOR1;
    } else {
      error(0, "Read string term by vbar: %s (length %d)", PDC_fmt_str(&s), s.len);
    }
    if (PDC_ERR == PDC_e_char_lit_read(pdc, &csm, &ed, '|')) {
      PDCI_report_err (pdc, 0, &ed.loc, ed.errCode, 0, 0);
      goto find_EOR1;
    }
    if (PDC_ERR == PDC_e_string_read(pdc, &csm, '|', &ed, &s)) {
      goto find_EOR1;
    } else {
      error(0, "Read string term by vbar: %s (length %d)", PDC_fmt_str(&s), s.len);
    }
    if (PDC_ERR == PDC_e_char_lit_read(pdc, &csm, &ed, '|')) {
      PDCI_report_err (pdc, 0, &ed.loc, ed.errCode, 0, 0);
      goto find_EOR1;
    }
    if (PDC_ERR == PDC_e_string_CSE_read(pdc, &csm, my_regexp, &ed, &s)) {
      break;
    } else {
      error(0, "Read string term by EOR or X : %s (length %d)", PDC_fmt_str(&s), s.len);
    }
  find_EOR1:
    if (PDC_ERR == PDC_IO_next_rec(pdc, &bytes_skipped)) {
      error(2, "Could not find EOR (newline), ending program");
      goto done;
    }
    ultmp = bytes_skipped;
    error(0, "bytes_skipped to find EOR/newline = %ld", ultmp);
    if (PDC_IO_at_EOF(pdc)) {
      error(0, "Main program found eof");
      break;
    }

    /* try to read line with 2 strings term by vbar 1 string term by EOR */
    if (PDC_ERR == PDC_e_string_FW_read(pdc, &csm, 4, &ed, &s)) {
      goto find_EOR2;
    } else {
      error(0, "Read string term by vbar: %s (length %d)", PDC_fmt_str(&s), s.len);
    }
    if (PDC_ERR == PDC_e_char_lit_read(pdc, &csm, &ed, '|')) {
      PDCI_report_err (pdc, 0, &ed.loc, ed.errCode, 0, 0);
      goto find_EOR2;
    }
    if (PDC_ERR == PDC_e_string_read(pdc, &csm, '|', &ed, &s)) {
      goto find_EOR2;
    } else {
      error(0, "Read string term by vbar: %s (length %d)", PDC_fmt_str(&s), s.len);
    }
    if (PDC_ERR == PDC_e_char_lit_read(pdc, &csm, &ed, '|')) {
      PDCI_report_err (pdc, 0, &ed.loc, ed.errCode, 0, 0);
      goto find_EOR2;
    }
    if (PDC_ERR == PDC_e_string_SE_read(pdc, &csm, "[X]|EOR", &ed, &s)) {
      break;
    } else {
      error(0, "Read string term by EOR or X : %s (length %d)", PDC_fmt_str(&s), s.len);
    }
  find_EOR2:
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
