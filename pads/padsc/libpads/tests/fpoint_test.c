#include "libpadsc-internal.h" /* for testing - normally do not include internal */

static PDC_uint64 PDCI_10toThe[] = {
  /* 10^0  = */                          1ULL,
  /* 10^1  = */                         10ULL,
  /* 10^2  = */                        100ULL,
  /* 10^3  = */                       1000ULL,
  /* 10^4  = */                      10000ULL,
  /* 10^5  = */                     100000ULL,
  /* 10^6  = */                    1000000ULL,
  /* 10^7  = */                   10000000ULL,
  /* 10^8  = */                  100000000ULL,
  /* 10^9  = */                 1000000000ULL,
  /* 10^10 = */                10000000000ULL,
  /* 10^11 = */               100000000000ULL,
  /* 10^12 = */              1000000000000ULL,
  /* 10^13 = */             10000000000000ULL,
  /* 10^14 = */            100000000000000ULL,
  /* 10^15 = */           1000000000000000ULL,
  /* 10^16 = */          10000000000000000ULL,
  /* 10^17 = */         100000000000000000ULL,
  /* 10^18 = */        1000000000000000000ULL,
  /* 10^19 = */       10000000000000000000ULL
};

#define NEXT_REC do {\
  if (strncmp(argv[1], "norec", 5) == 0) { \
    if (PDC_ERR == PDC_e_char_lit_scan(pdc, '\n', '\n', 1, 0, &bytes_skipped)) { \
      error(2, "Could not find EOR (newline), ending program"); \
      goto done; \
    } \
  } else { \
    if (PDC_ERR == PDC_IO_next_rec(pdc, &bytes_skipped)) { \
      error(2, "Could not find EOR (newline), ending program"); \
      goto done; \
    } \
  } \
  ultmp = bytes_skipped; \
  error(0, "next_rec returned bytes_skipped = %ld", ultmp); \
} while (0)

int main(int argc, char** argv) {
  PDC_t*          pdc;
  PDC_IO_disc_t*  io_disc;
  PDC_disc_t      my_disc = PDC_default_disc;
  PDC_base_em     em = PDC_CheckAndSet;
  PDC_base_ed     ed;
  size_t          bytes_skipped;
  PDC_fpoint64    fp;
  PDC_ufpoint64   ufp;
  int             w, n, d;
  unsigned long   ultmp;

  my_disc.flags |= (PDC_flags_t)PDC_WSPACE_OK;

  if (argc != 2) {
    goto usage;
  }

  if (strcmp(argv[1], "fwrec") == 0) {
    io_disc = PDC_fwrec_make(0, 20, 1); /* 1 20-echar int, newline */ 
  } else if (strcmp(argv[1], "ctrec") == 0) {
    io_disc = PDC_ctrec_make(PDC_EBCDIC_NEWLINE, 0);
  } else if (strcmp(argv[1], "norec") == 0) {
    io_disc = PDC_norec_make(0);
  } else if (strcmp(argv[1], "fwrec_noseek") == 0) {
    io_disc = PDC_fwrec_noseek_make(0, 20, 1); /* 1 20-echar int, newline */ 
  } else if (strcmp(argv[1], "ctrec_noseek") == 0) {
    io_disc = PDC_ctrec_noseek_make(PDC_EBCDIC_NEWLINE, 0);
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
    return -1;
  }
  if (PDC_ERR == PDC_IO_fopen(pdc, "../../data/ex_data.fpoint_test")) {
    error(2, "*** PDC_IO_fopen failed ***");
    return -1;
  }

  /* read fpoint once for each legal n/d combo making up each legal width */
  for (w = 1; w < 19; w++) {
    for (n = w; n >= 0; n--) {
      d = w-n;
      if (PDC_OK == PDC_ufpoint_read(pdc, &em, n, d, &ed, &ufp)) {
	error(0, "Read fpoint(%d, %d) with num = %llu denom = %llu", n, d, ufp.num, ufp.denom);
	if (ufp.denom != PDCI_10toThe[d]) {
	  error(0, "XXX failure: denom should be %llu", PDCI_10toThe[d]);
	  return -1;
	} 
      } else { return -1; }
      if (foo) { foo; }
      NEXT_REC;
    }
  }

 done:
  if (PDC_ERR == PDC_IO_fclose(pdc)) {
    error(2, "*** PDC_IO_fclose failed ***");
    return -1;
  }

  if (PDC_ERR == PDC_close(pdc)) {
    error(2, "*** PDC_close failed ***");
    return -1;
  }

  return 0;

 usage:
  error(2, "\nUsage: %s <io-disc-name>\n\n\twhere <io-disc-name> is one of: fwrec, ctrec, norec,"
	" fwrec_noseek, ctrec_noseek, norec_noseek\n", argv[0]);
  return -1;
}
