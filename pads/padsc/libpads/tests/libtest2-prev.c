/*
 *  libtest1: Test fixed width ascii read functions
 */


#include "libpadsc-internal.h" /* for testing - normally do not include internal */

int main(int argc, char** argv) {
  /* int             ctr; */
  /* size_t          n; */
  /* unsigned char   c; */
  char fname[1000];
  char* h;
  int             i;
  PDC_t*          pdc;
  PDC_int8        i1;
  PDC_int16       i2;
  PDC_int32       i4;
  PDC_int64       i8;
  PDC_uint8       ui1;
  PDC_uint16      ui2;
  PDC_uint32      ui4;
  PDC_uint64      ui8;
  PDC_base_em     em = PDC_CheckAndSet;
  PDC_base_ed     ed;
  PDC_disc_t      my_disc = PDC_default_disc;

  if (argc == 2) {
    h = argv[1];
  } else {
    h = getenv("HOSTSHORT");
  }
  sprintf(fname, "../ex_data.libtest2.%s", h);
  printf("fname = %s\n", fname);

  if (PDC_ERR == PDC_open(0, &pdc)) {
    error(2, "*** PDC_open failed ***");
    exit(-1);
  }

  /* ================================================================================ */
  /* Test 1: read 8 1byte ints */

  if (PDC_ERR == PDC_IO_fopen(pdc, fname, &my_disc)) {
    error(2, "*** PDC_IO_fopen failed ***");
    exit(-1);
  }

  while (1) {
    if (PDC_IO_peek_EOF(pdc, &my_disc)) {
      error(0, "Main program found eof");
      break;
    }
    for (i = 0; i < 8; i++) {
      if (PDC_ERR == PDC_bint8_read(pdc, &em, &ed, &i1, &my_disc)) {
	PDCI_report_err (pdc, &my_disc, 0, &ed.loc, ed.errCode, 0);
      } else {
	error(0, "Read binary integer : %ld", i1);
      }
    }
    if (PDC_ERR == PDCI_char_lit_scan(pdc, '\n', '\n', 0, 0, 0, &my_disc)) {
      error(2, "Could not find newline, ending program");
      break;
    }
  }

  if (PDC_ERR == PDC_IO_fclose(pdc, &my_disc)) {
    error(2, "*** PDC_IO_fclose failed ***");
    exit(-1);
  }

  /* ================================================================================ */
  /* Test 2: read 4 2byte ints */

  if (PDC_ERR == PDC_IO_fopen(pdc, fname, &my_disc)) {
    error(2, "*** PDC_IO_fopen failed ***");
    exit(-1);
  }

  while (1) {
    if (PDC_IO_peek_EOF(pdc, &my_disc)) {
      error(0, "Main program found eof");
      break;
    }
    for (i = 0; i < 4; i++) {
      if (PDC_ERR == PDC_bint16_norev_read(pdc, &em, &ed, &i2, &my_disc)) {
	PDCI_report_err (pdc, &my_disc, 0, &ed.loc, ed.errCode, 0);
      } else {
	error(0, "Read binary integer : %ld", i2);
      }
    }
    if (PDC_ERR == PDCI_char_lit_scan(pdc, '\n', '\n', 0, 0, 0, &my_disc)) {
      error(2, "Could not find newline, ending program");
      break;
    }
  }

  if (PDC_ERR == PDC_IO_fclose(pdc, &my_disc)) {
    error(2, "*** PDC_IO_fclose failed ***");
    exit(-1);
  }

  /* ================================================================================ */
  /* Test 3: read 2 4byte ints */

  if (PDC_ERR == PDC_IO_fopen(pdc, fname, &my_disc)) {
    error(2, "*** PDC_IO_fopen failed ***");
    exit(-1);
  }

  while (1) {
    if (PDC_IO_peek_EOF(pdc, &my_disc)) {
      error(0, "Main program found eof");
      break;
    }
    for (i = 0; i < 2; i++) {
      if (PDC_ERR == PDC_bint32_norev_read(pdc, &em, &ed, &i4, &my_disc)) {
	PDCI_report_err (pdc, &my_disc, 0, &ed.loc, ed.errCode, 0);
      } else {
	error(0, "Read binary integer : %ld", i4);
      }
    }
    if (PDC_ERR == PDCI_char_lit_scan(pdc, '\n', '\n', 0, 0, 0, &my_disc)) {
      error(2, "Could not find newline, ending program");
      break;
    }
  }

  if (PDC_ERR == PDC_IO_fclose(pdc, &my_disc)) {
    error(2, "*** PDC_IO_fclose failed ***");
    exit(-1);
  }

  /* ================================================================================ */
  /* Test 4: read 1 8byte int */

  if (PDC_ERR == PDC_IO_fopen(pdc, fname, &my_disc)) {
    error(2, "*** PDC_IO_fopen failed ***");
    exit(-1);
  }

  while (1) {
    if (PDC_IO_peek_EOF(pdc, &my_disc)) {
      error(0, "Main program found eof");
      break;
    }
    for (i = 0; i < 1; i++) {
      if (PDC_ERR == PDC_bint64_norev_noswap_read(pdc, &em, &ed, &i8, &my_disc)) {
	PDCI_report_err (pdc, &my_disc, 0, &ed.loc, ed.errCode, 0);
      } else {
	error(0, "Read binary integer : %lld", i8);
      }
    }
    if (PDC_ERR == PDCI_char_lit_scan(pdc, '\n', '\n', 0, 0, 0, &my_disc)) {
      error(2, "Could not find newline, ending program");
      break;
    }
  }

  if (PDC_ERR == PDC_IO_fclose(pdc, &my_disc)) {
    error(2, "*** PDC_IO_fclose failed ***");
    exit(-1);
  }

  /* ================================================================================ */
  /* Test 5: read 8 1byte uints */

  if (PDC_ERR == PDC_IO_fopen(pdc, fname, &my_disc)) {
    error(2, "*** PDC_IO_fopen failed ***");
    exit(-1);
  }

  while (1) {
    if (PDC_IO_peek_EOF(pdc, &my_disc)) {
      error(0, "Main program found eof");
      break;
    }
    for (i = 0; i < 8; i++) {
      if (PDC_ERR == PDC_buint8_read(pdc, &em, &ed, &ui1, &my_disc)) {
	PDCI_report_err (pdc, &my_disc, 0, &ed.loc, ed.errCode, 0);
      } else {
	error(0, "Read binary integer : %lu", ui1);
      }
    }
    if (PDC_ERR == PDCI_char_lit_scan(pdc, '\n', '\n', 0, 0, 0, &my_disc)) {
      error(2, "Could not find newline, ending program");
      break;
    }
  }

  if (PDC_ERR == PDC_IO_fclose(pdc, &my_disc)) {
    error(2, "*** PDC_IO_fclose failed ***");
    exit(-1);
  }

  /* ================================================================================ */
  /* Test 6: read 4 2byte uints */

  if (PDC_ERR == PDC_IO_fopen(pdc, fname, &my_disc)) {
    error(2, "*** PDC_IO_fopen failed ***");
    exit(-1);
  }

  while (1) {
    if (PDC_IO_peek_EOF(pdc, &my_disc)) {
      error(0, "Main program found eof");
      break;
    }
    for (i = 0; i < 4; i++) {
      if (PDC_ERR == PDC_buint16_norev_read(pdc, &em, &ed, &ui2, &my_disc)) {
	PDCI_report_err (pdc, &my_disc, 0, &ed.loc, ed.errCode, 0);
      } else {
	error(0, "Read binary integer : %lu", ui2);
      }
    }
    if (PDC_ERR == PDCI_char_lit_scan(pdc, '\n', '\n', 0, 0, 0, &my_disc)) {
      error(2, "Could not find newline, ending program");
      break;
    }
  }

  if (PDC_ERR == PDC_IO_fclose(pdc, &my_disc)) {
    error(2, "*** PDC_IO_fclose failed ***");
    exit(-1);
  }

  /* ================================================================================ */
  /* Test 7: read 2 4byte uints */

  if (PDC_ERR == PDC_IO_fopen(pdc, fname, &my_disc)) {
    error(2, "*** PDC_IO_fopen failed ***");
    exit(-1);
  }

  while (1) {
    if (PDC_IO_peek_EOF(pdc, &my_disc)) {
      error(0, "Main program found eof");
      break;
    }
    for (i = 0; i < 2; i++) {
      if (PDC_ERR == PDC_buint32_norev_read(pdc, &em, &ed, &ui4, &my_disc)) {
	PDCI_report_err (pdc, &my_disc, 0, &ed.loc, ed.errCode, 0);
      } else {
	error(0, "Read binary integer : %lu", ui4);
      }
    }
    if (PDC_ERR == PDCI_char_lit_scan(pdc, '\n', '\n', 0, 0, 0, &my_disc)) {
      error(2, "Could not find newline, ending program");
      break;
    }
  }

  if (PDC_ERR == PDC_IO_fclose(pdc, &my_disc)) {
    error(2, "*** PDC_IO_fclose failed ***");
    exit(-1);
  }

  /* ================================================================================ */
  /* Test 8: read 1 8byte uint */

  if (PDC_ERR == PDC_IO_fopen(pdc, fname, &my_disc)) {
    error(2, "*** PDC_IO_fopen failed ***");
    exit(-1);
  }

  while (1) {
    if (PDC_IO_peek_EOF(pdc, &my_disc)) {
      error(0, "Main program found eof");
      break;
    }
    for (i = 0; i < 1; i++) {
      if (PDC_ERR == PDC_buint64_norev_noswap_read(pdc, &em, &ed, &ui8, &my_disc)) {
	PDCI_report_err (pdc, &my_disc, 0, &ed.loc, ed.errCode, 0);
      } else {
	error(0, "Read binary integer : %llu", ui8);
      }
    }
    if (PDC_ERR == PDCI_char_lit_scan(pdc, '\n', '\n', 0, 0, 0, &my_disc)) {
      error(2, "Could not find newline, ending program");
      break;
    }
  }

  if (PDC_ERR == PDC_IO_fclose(pdc, &my_disc)) {
    error(2, "*** PDC_IO_fclose failed ***");
    exit(-1);
  }

  /* ================================================================================ */
  /* DONE */

  if (PDC_ERR == PDC_close(pdc, &my_disc)) {
    error(2, "*** PDC_close failed ***");
    exit(-1);
  }

  return 0;
}
