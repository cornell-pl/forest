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
  int rev = 0;
  int swap = 0;
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

  if (argc >= 2) {
    h = argv[1];
  } else {
    h = getenv("HOSTSHORT");
  }
  if (argc >= 3 && ((strcmp(argv[2], "rev") == 0) || (strcmp(argv[2], "revswap") == 0))) {
    rev = 1;
  }
  if (argc >= 3 && ((strcmp(argv[2], "swap") == 0) || (strcmp(argv[2], "revswap") == 0))) {
    swap = 1;
  }
  sprintf(fname, "../ex_data.libtest2.%s", h);
  printf("fname = %s    rev = %d    swap = %d\n", fname, rev, swap);

  if (PDC_ERROR == PDC_open(0, &pdc)) {
    error(2, "*** PDC_open failed ***");
    exit(-1);
  }

  if (PDC_ERROR == PDC_IO_fopen(pdc, fname, &my_disc)) {
    error(2, "*** PDC_IO_fopen failed ***");
    exit(-1);
  }

  while (1) {
    if (PDC_IO_peek_EOF(pdc, &my_disc)) {
      error(0, "Main program found eof");
      break;
    }

    if (PDC_ERROR == PDC_bint8_read(pdc, &em, &ed, &i1, &my_disc)) {
      PDC_report_err (pdc, &my_disc, 0, &ed.loc, ed.errCode, 0);
      goto check_newline;
    } else {
      error(0, "Read bint8  : %ld", i1);
    }
    if (PDC_ERROR == PDC_buint8_read(pdc, &em, &ed, &ui1, &my_disc)) {
      PDC_report_err (pdc, &my_disc, 0, &ed.loc, ed.errCode, 0);
      goto check_newline;
    } else {
      error(0, "Read buint8 : %lu", ui1);
    }

    if (rev == 0) {
      if (PDC_ERROR == PDC_bint16_norev_read(pdc, &em, &ed, &i2, &my_disc)) {
	PDC_report_err (pdc, &my_disc, 0, &ed.loc, ed.errCode, 0);
	goto check_newline;
      } else {
	error(0, "Read bint16  : %ld", i2);
      }
      if (PDC_ERROR == PDC_buint16_norev_read(pdc, &em, &ed, &ui2, &my_disc)) {
	PDC_report_err (pdc, &my_disc, 0, &ed.loc, ed.errCode, 0);
	goto check_newline;
      } else {
	error(0, "Read buint16 : %lu", ui2);
      }
      if (PDC_ERROR == PDC_bint32_norev_read(pdc, &em, &ed, &i4, &my_disc)) {
	PDC_report_err (pdc, &my_disc, 0, &ed.loc, ed.errCode, 0);
	goto check_newline;
      } else {
	error(0, "Read bint32  : %ld", i4);
      }
      if (PDC_ERROR == PDC_buint32_norev_read(pdc, &em, &ed, &ui4, &my_disc)) {
	PDC_report_err (pdc, &my_disc, 0, &ed.loc, ed.errCode, 0);
	goto check_newline;
      } else {
	error(0, "Read buint32 : %lu", ui4);
      }
      if (swap == 0) {
	if (PDC_ERROR == PDC_bint64_norev_noswap_read(pdc, &em, &ed, &i8, &my_disc)) {
	  PDC_report_err (pdc, &my_disc, 0, &ed.loc, ed.errCode, 0);
	  goto check_newline;
	} else {
	  error(0, "Read bint64  : %lld", i8);
	}
	if (PDC_ERROR == PDC_buint64_norev_noswap_read(pdc, &em, &ed, &ui8, &my_disc)) {
	  PDC_report_err (pdc, &my_disc, 0, &ed.loc, ed.errCode, 0);
	  goto check_newline;
	} else {
	  error(0, "Read buint64 : %llu", ui8);
	}
      } else {
	if (PDC_ERROR == PDC_bint64_norev_swap_read(pdc, &em, &ed, &i8, &my_disc)) {
	  PDC_report_err (pdc, &my_disc, 0, &ed.loc, ed.errCode, 0);
	  goto check_newline;
	} else {
	  error(0, "Read bint64  : %lld", i8);
	}
	if (PDC_ERROR == PDC_buint64_norev_swap_read(pdc, &em, &ed, &ui8, &my_disc)) {
	  PDC_report_err (pdc, &my_disc, 0, &ed.loc, ed.errCode, 0);
	  goto check_newline;
	} else {
	  error(0, "Read buint64 : %llu", ui8);
	}
      }
    } else {
      if (PDC_ERROR == PDC_bint16_rev_read(pdc, &em, &ed, &i2, &my_disc)) {
	PDC_report_err (pdc, &my_disc, 0, &ed.loc, ed.errCode, 0);
	goto check_newline;
      } else {
	error(0, "Read bint16  : %ld", i2);
      }
      if (PDC_ERROR == PDC_buint16_rev_read(pdc, &em, &ed, &ui2, &my_disc)) {
	PDC_report_err (pdc, &my_disc, 0, &ed.loc, ed.errCode, 0);
	goto check_newline;
      } else {
	error(0, "Read buint16 : %lu", ui2);
      }

      if (PDC_ERROR == PDC_bint32_rev_read(pdc, &em, &ed, &i4, &my_disc)) {
	PDC_report_err (pdc, &my_disc, 0, &ed.loc, ed.errCode, 0);
	goto check_newline;
      } else {
	error(0, "Read bint32  : %ld", i4);
      }
      if (PDC_ERROR == PDC_buint32_rev_read(pdc, &em, &ed, &ui4, &my_disc)) {
	PDC_report_err (pdc, &my_disc, 0, &ed.loc, ed.errCode, 0);
	goto check_newline;
      } else {
	error(0, "Read buint32 : %lu", ui4);
      }
      if (swap == 0) { 
	if (PDC_ERROR == PDC_bint64_rev_noswap_read(pdc, &em, &ed, &i8, &my_disc)) {
	  PDC_report_err (pdc, &my_disc, 0, &ed.loc, ed.errCode, 0);
	  goto check_newline;
	} else {
	  error(0, "Read bint64  : %lld", i8);
	}
	if (PDC_ERROR == PDC_buint64_rev_noswap_read(pdc, &em, &ed, &ui8, &my_disc)) {
	  PDC_report_err (pdc, &my_disc, 0, &ed.loc, ed.errCode, 0);
	  goto check_newline;
	} else {
	  error(0, "Read buint64 : %llu", ui8);
	}
      } else {
	printf("revswap not supported yet\n");
      }
    }

  check_newline:
    PDC_get_loc(pdc, &ed.loc, &my_disc);
    error(0, "Searching for newline char.  Start loc line %d char %d", ed.loc.beginLine, ed.loc.beginChar);
    if (PDC_ERROR == PDC_char_lit_scan(pdc, '\n', '\n', 0, 0, &my_disc)) {
      error(2, "Could not find newline, ending program");
      break;
    }
    PDC_get_loc(pdc, &ed.loc, &my_disc);
    error(0, "Found newline.  Now loc line %d char %d", ed.loc.beginLine, ed.loc.beginChar);
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
