/*
 *  re_scan_test: Test  PDC_RE_scan2 
 */


#include "padsc-internal.h" /* for testing - normally do not include internal */

int main(int argc, char** argv) {
  PDC_string      s;
  PDC_t*          pdc;
  PDC_IO_disc_t*  io_disc;
  PDC_disc_t      my_disc = PDC_default_disc;
  size_t          offset, bytes_skipped;
  int             line, f_found, try;
  unsigned long   ultmp;
  PDC_REGEXP_DECL_NULL(my_regexp_f);
  PDC_REGEXP_DECL_NULL(my_regexp_s);

  error(0, "\nUsing PADSC IO discipline nlrec\n\n");
  io_disc = PDC_nlrec_make(0);

  for (try = 0; try < 2; try++) {
    error(0, "\nTry using scan%d\n", try+1);

    if (PDC_ERR == PDC_open(&pdc, &my_disc, io_disc)) {
      error(ERROR_FATAL, "*** PDC_open failed ***");
    }
    if (PDC_ERR == PDC_IO_fopen(pdc, "../../data/re_scan_test.dat")) {
      error(ERROR_FATAL, "*** PDC_IO_fopen failed ***");
    }

    if (PDC_ERR == PDC_regexp_compile_Cstr(pdc, "/[|]+/", &my_regexp_f)) {
      error(ERROR_FATAL, "** unexpected regexp compile failure **");
    }

    if (PDC_ERR == PDC_regexp_compile_Cstr(pdc, "/([X]+)?$/", &my_regexp_s)) {
      error(ERROR_FATAL, "** unexpected regexp compile failure **");
    }

    PDC_string_init(pdc, &s);

  /*
   * XXX Process the data here XXX
   */
    for (line = 0; 1; line++) {
      if (PDC_IO_at_EOF(pdc)) {
	error(0, "Main program found eof");
	break;
      }
      /* try to scan line with 2 strings term by vbar 1 string term by EOR */
      if (try == 0) {

	/* try using scan1 */

	if (PDC_ERR == PDC_RE_scan1(pdc, &my_regexp_f, 1, 0, &offset)) {
	  error(0, "line %d: UNEXPECTED, scan1 did not find vbars", line);
	  goto next_line;
	}
	error(0, "line %d: scan1 found vbars at offset %d", line, offset);
	if (PDC_ERR == PDC_RE_scan1(pdc, &my_regexp_f, 1, 0, &offset)) {
	  error(0, "line %d: UNEXPECTED, scan1 did not find vbars", line);
	  goto next_line;
	}
	error(0, "line %d: scan1 found vbars at offset %d", line, offset);
	if (PDC_ERR == PDC_RE_scan1(pdc, &my_regexp_s, 0, 0, &offset)) {
	  error(0, "line %d: UNEXPECTED, scan1 did not find term", line);
	  goto next_line;
	}
	error(0, "line %d: scan1 found term at offset %d", line, offset);

      } else {

	/* try using scan2 */

	if (PDC_ERR == PDC_RE_scan2(pdc, &my_regexp_f, &my_regexp_s, 1, 0, 0, &f_found, &offset)) {
	  error(0, "line %d: UNEXPECTED, scan2 did not find sep or term", line);
	  goto next_line;
	}
	if (f_found) {
	  error(0, "line %d: scan2 found vbars at offset %d", line, offset);
	} else {
	  error(0, "line %d: UNEXPECTED, scan2 found term at offset %d", line, offset);
	  goto next_line;
	}
	if (PDC_ERR == PDC_RE_scan2(pdc, &my_regexp_f, &my_regexp_s, 1, 0, 0, &f_found, &offset)) {
	  error(0, "line %d: UNEXPECTED, scan2 did not find sep or term", line);
	  goto next_line;
	}
	if (f_found) {
	  error(0, "line %d: scan2 found vbars at offset %d", line, offset);
	} else {
	  error(0, "line %d: UNEXPECTED, scan2 found term at offset %d", line, offset);
	  goto next_line;
	}
	if (PDC_ERR == PDC_RE_scan2(pdc, &my_regexp_f, &my_regexp_s, 1, 0, 0, &f_found, &offset)) {
	  error(0, "line %d: UNEXPECTED, scan2 did not find sep or term", line);
	  goto next_line;
	}
	if (f_found) {
	  error(0, "line %d: UNEXPECTED scan2 found extra vbars at offset %d", line, offset);
	} else {
	  error(0, "line %d: scan2 found term at offset %d", line, offset);
	}
      }

    next_line:
      if (PDC_ERR == PDC_IO_next_rec(pdc, &bytes_skipped)) {
	error(2, "Could not find EOR (newline), ending program");
	goto done;
      }
      ultmp = bytes_skipped;
      error(0, "bytes_skipped to find EOR/newline = %ld", ultmp);
    }

  done:
    PDC_string_cleanup(pdc, &s);
    PDC_regexp_cleanup(pdc, &my_regexp_f);
    PDC_regexp_cleanup(pdc, &my_regexp_s);

    if (PDC_ERR == PDC_IO_close(pdc)) {
      error(ERROR_FATAL, "*** PDC_IO_close failed ***");
    }

    if (PDC_ERR == PDC_close(pdc)) {
      error(ERROR_FATAL, "*** PDC_close failed ***");
    }

  }

  return 0;
}
