/*
 *  io_openclsoe_test: Test io open/close functionality
 */


#include "libpadsc-internal.h" /* for testing - normally do not include internal */

int main(int argc, char** argv) {
  Sfio_t         *io;
  int             i;
  PDC_t*          pdc;
  PDC_IO_disc_t*  io_disc;
  PDC_int8        i1;
  PDC_base_m      m    = PDC_CheckAndSet;
  PDC_base_pd     pd;
  PDC_disc_t      my_disc = PDC_default_disc;
  size_t          bytes_skipped;
  unsigned long   ultmp;
  const char     *fname = "../../data/ex_data.io_openclose_test.small";

  my_disc.flags |= (PDC_flags_t)PDC_WSPACE_OK;

  if ((argc != 2) && (argc != 3)) {
    goto usage;
  }
  if (argc == 3) {
    fname = argv[2];
    if (strcmp(fname, "large") == 0) {
      fname = "../../data/ex_data.io_openclose_test.large";
    }
    if (strcmp(fname, "-") == 0) {
      fname = "/dev/stdin";
    }
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
    error(ERROR_FATAL, "Failed to install IO discipline %s", argv[1]);
  } else {
    error(0, "Installed IO discipline %s", argv[1]);
  }

  if (PDC_ERR == PDC_open(&pdc, &my_disc, io_disc)) {
    error(ERROR_FATAL, "*** PDC_open failed ***");
  }
  if (strcmp(fname, "/dev/stdin") == 0) {
    io = sfstdin;
  } else {
    if (!(io = sfopen(NiL, fname, "r"))) {
      error(ERROR_FATAL, "Failed to open file \"%s\"", fname);
    }
    if (sfset(io, 0, 0) & SF_PUBLIC) {
      error(0, "SF_PUBLIC is set");
    }
    if (sfset(io, 0, 0) & SF_SHARE) {
      error(0, "SF_SHARE is set");
    }
  }

  if (PDC_ERR == PDC_IO_set(pdc, io)) {
    error(ERROR_FATAL, "PDC_IO_set failed");
  }

  /*
   * XXX Process the data here XXX
   */
  while (1) {
    /* uninstall then install io each time through the loop */
    if (PDC_ERR == PDC_IO_close(pdc)) {
      error(ERROR_FATAL, "PDC_IO_close failed");
    }
    if (PDC_ERR == PDC_IO_set(pdc, io)) {
      error(ERROR_FATAL, "PDC_IO_set failed");
    }
    if (PDC_IO_at_EOF(pdc)) {
      error(0, "Main program found eof");
      break;
    }
    /* try to read 4 fixed width integers (width 6) */
    for (i = 0; i < 4; i++) {
      if (PDC_OK == PDC_a_int8_FW_read(pdc, &m, 6, &pd, &i1)) {
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

  if (PDC_ERR == PDC_IO_close(pdc)) {
    error(ERROR_FATAL, "*** PDC_IO_close failed ***");
  }

  if (PDC_ERR == PDC_close(pdc)) {
    error(ERROR_FATAL, "*** PDC_close failed ***");
  }

  return 0;

 usage:
  error(2, "\nUsage: %s <io-disc-name> [ <fname> ]\n\n\twhere <io-disc-name> is one of: "
	"fwrec, nlrec, norec, fwrec_noseek, nlrec_noseek, norec_noseek\n"
	"  Note: fname can also be /dev/stdin or alias 'large' for large data set", argv[0]);
  return -1;
}
