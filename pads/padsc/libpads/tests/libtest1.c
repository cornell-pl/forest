/*
 *  libtest1: Test fixed width ascii read functions
 */


#include "libpadsc-internal.h" /* for testing - normally do not include internal */

int main(int argc, char** argv) {
  /* int             ctr; */
  /* size_t          n; */
  /* unsigned char   c; */
  int             i;
  PDC_t*          pdc;
  PDC_IO_disc_t*  io_disc;
  PDC_int8        i1;
  PDC_base_em     em = PDC_CheckAndSet;
  PDC_base_ed     ed;
  PDC_disc_t      my_disc = PDC_default_disc;
  size_t          bytes_skipped;
  unsigned long   ultmp;

  my_disc.flags |= (PDC_flags_t)PDC_WSPACE_OK;

  if (argc != 2) {
    goto usage;
  }

  if (strcmp(argv[1], "fwrec") == 0) {
    io_disc = PDC_fwrec_make(0, 24, 1); /* 4 6-char ints, newline */ 
  } else if (strcmp(argv[1], "nlrec") == 0) {
    io_disc = PDC_nlrec_make(0);
  } else if (strcmp(argv[1], "norec") == 0) {
    io_disc = PDC_norec_make(0);
  } else if (strcmp(argv[1], "fwrec_noseek") == 0) {
    io_disc = PDC_fwrec_noseek_make(0, 24, 1); /* 4 6-char ints, newline */ 
  } else if (strcmp(argv[1], "nlrec_noseek") == 0) {
    io_disc = PDC_nlrec_noseek_make(0);
  } else if (strcmp(argv[1], "norec_noseek") == 0) {
    io_disc = PDC_norec_noseek_make(0);
  } else {
    goto usage;
  }
  if (!io_disc) {
    error(ERROR_FATAL, "\nFailed to install IO discipline %s", argv[1]);
  } else {
    error(0, "\nInstalled IO discipline %s", argv[1]);
  }

  if (PDC_ERR == PDC_open(&pdc, &my_disc, io_disc)) {
    error(2, "*** PDC_open failed ***");
    exit(-1);
  }
  if (PDC_ERR == PDC_IO_fopen(pdc, "../../data/ex_data.libtest1")) {
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
    /* try to read 4 fixed width integers (width 6) */
    for (i = 0; i < 4; i++) {
      if (PDC_OK == PDC_a_int8_FW_read(pdc, &em, 6, &ed, &i1)) {
	error(0, "Read ascii integer of width 6: %ld", i1);
      }
    }
    if (strncmp(argv[1], "norec", 5) == 0) {
      if (PDC_ERR == PDC_a_char_lit_scan(pdc, '\n', '\n', 1, 0, &bytes_skipped)) {
	error(2, "Could not find EOR (newline), ending program");
	break;
      }
    } else {
      if (PDC_ERR == PDC_IO_next_rec(pdc, &bytes_skipped)) {
	error(2, "Could not find EOR (newline), ending program");
	break;
      }
    }
    ultmp = bytes_skipped;
    error(0, "next_rec returned bytes_skipped = %ld", ultmp);
  }

  if (PDC_ERR == PDC_IO_fclose(pdc)) {
    error(2, "*** PDC_IO_fclose failed ***");
    exit(-1);
  }

  if (PDC_ERR == PDC_close(pdc)) {
    error(2, "*** PDC_close failed ***");
    exit(-1);
  }

  return 0;

 usage:
  error(2, "\nUsage: %s <io-disc-name>\n\n\twhere <io-disc-name> is one of: fwrec, nlrec, norec, fwrec_noseek, nlrec_noseek, norec_noseek\n", argv[0]);
  return -1;
}
