/*
 *  bcd_test: tests
 *       PDC_bcd_fpoint8_read
 *       PDC_bcd_ufpoint8_read
 *       PDC_bcd_fpoint16_read
 *       PDC_bcd_ufpoint16_read
 *       PDC_bcd_fpoint32_read
 *       PDC_bcd_ufpoint32_read
 *       PDC_bcd_fpoint64_read
 *       PDC_bcd_ufpoint64_read
 */

#include "libpadsc-internal.h" /* for testing - normally do not include internal */

#define NEXT_REC do {\
  if (strncmp(argv1, "norec", 5) == 0) { \
    if (PDC_ERR == PDC_e_char_lit_scan(pdc, '\n', '\n', 1, 0, &bytes_skipped)) { \
      error(2, "Could not find EOR (0xFF), ending program"); \
      goto done; \
    } \
  } else { \
    if (PDC_ERR == PDC_IO_next_rec(pdc, &bytes_skipped)) { \
      error(2, "Could not find EOR (OxFF), ending program"); \
      goto done; \
    } \
  } \
  ultmp = bytes_skipped; \
  error(0, "next_rec returned bytes_skipped = %ld", ultmp); \
} while (0)

int main(int argc, char** argv) {
  PDC_t*          pdc;
  PDC_IO_disc_t*  io_disc;
  PDC_fpoint8     f8;
  PDC_fpoint16    f16;
  PDC_fpoint32    f32;
  PDC_fpoint64    f64;
  PDC_ufpoint8    uf8;
  PDC_ufpoint16   uf16;
  PDC_ufpoint32   uf32;
  PDC_ufpoint64   uf64;
  PDC_disc_t      my_disc = PDC_default_disc;
  PDC_base_csm    csm = PDC_CheckAndSet;
  PDC_base_ed     ed;
  size_t          bytes_skipped;
  unsigned long   ultmp;
  const char     *argv1;

  my_disc.flags |= (PDC_flags_t)PDC_WSPACE_OK;

  if ((argc != 1) && (argc != 2)) {
    goto usage;
  }

  if (argc == 1) {
    argv1 = "fwrec_noseek";
  } else {
    argv1 = argv[1];
  }
  if (strcmp(argv1, "fwrec") == 0) {
    io_disc = PDC_fwrec_make(0, 20, 1);
  } else if (strcmp(argv1, "ctrec") == 0) {
    io_disc = PDC_ctrec_make(0xFF, 0);
  } else if (strcmp(argv1, "norec") == 0) {
    io_disc = PDC_norec_make(0);
  } else if (strcmp(argv1, "fwrec_noseek") == 0) {
    io_disc = PDC_fwrec_noseek_make(0, 20, 1);
  } else if (strcmp(argv1, "ctrec_noseek") == 0) {
    io_disc = PDC_ctrec_noseek_make(0xFF, 0);
  } else if (strcmp(argv1, "norec_noseek") == 0) {
    io_disc = PDC_norec_noseek_make(0);
  } else {
    goto usage;
  }
  if (!io_disc) {
    error(ERROR_FATAL, "\nFailed to install IO discipline %s", argv1);
  } else {
    error(0, "\nInstalled IO discipline %s", argv1);
  }

  if (PDC_ERR == PDC_open(&pdc, &my_disc, io_disc)) {
    error(2, "*** PDC_open failed ***");
    return -1;
  }
  if (PDC_ERR == PDC_IO_fopen(pdc, "../../data/ex_data.bcd_test")) {
    error(2, "*** PDC_IO_fopen failed ***");
    return -1;
  }

  if (PDC_OK == PDC_bcd_fpoint8_read(pdc, &csm, 3, 0, &ed, &f8)) {
    error(0, "Read bcd fpoint: num %ld denom %lu", (long)f8.num, (unsigned long)f8.denom);
    if (f8.num != 0) {
      error(0, "XXX failure: should be %ld XXX", (long)0);
      return -1;
    }
    if (f8.denom != 1) {
      error(0, "XXX failure: denom should be 1 XXX");
      return -1;
    } 
  } else { return -1; } 
  NEXT_REC;

  if (PDC_OK == PDC_bcd_fpoint8_read(pdc, &csm, 3, 1, &ed, &f8)) {
    error(0, "Read bcd fpoint: num %ld denom %lu", (long)f8.num, (unsigned long)f8.denom);
    if (f8.num != PDC_MIN_INT8) {
      error(0, "XXX failure: should be %ld XXX", (long)PDC_MIN_INT8);
      return -1;
    }
    if (f8.denom != 10) {
      error(0, "XXX failure: denom should be 10 XXX");
      return -1;
    } 
  } else { return -1; } 
  NEXT_REC;

  if (PDC_OK == PDC_bcd_fpoint8_read(pdc, &csm, 3, 2, &ed, &f8)) {
    error(0, "Read bcd fpoint: num %ld denom %lu", (long)f8.num, (unsigned long)f8.denom);
    if (f8.num != PDC_MAX_INT8) {
      error(0, "XXX failure: should be %ld XXX", (long)PDC_MAX_INT8);
      return -1;
    }
    if (f8.denom != 100) {
      error(0, "XXX failure: denom should be 100 XXX");
      return -1;
    } 
  } else { return -1; } 
  NEXT_REC;

  if (PDC_OK == PDC_bcd_ufpoint8_read(pdc, &csm, 3, 2, &ed, &uf8)) {
    error(0, "Read bcd ufpoint: num %lu denom %lu", (unsigned long)uf8.num, (unsigned long)uf8.denom);
    if (uf8.num != PDC_MAX_UINT8) {
      error(0, "XXX failure: should be %lu XXX", (unsigned long)PDC_MAX_UINT8);
      return -1;
    }
    if (uf8.denom != 100) {
      error(0, "XXX failure: denom should be 100 XXX");
      return -1;
    } 
  } else { return -1; } 
  NEXT_REC;

  if (PDC_OK == PDC_bcd_fpoint16_read(pdc, &csm, 5, 4, &ed, &f16)) {
    error(0, "Read bcd fpoint: num %ld denom %lu", (long)f16.num, (unsigned long)f16.denom);
    if (f16.num != PDC_MIN_INT16) {
      error(0, "XXX failure: should be %ld XXX", (long)PDC_MIN_INT16);
      return -1;
    }
    if (f16.denom != 10000) {
      error(0, "XXX failure: denom should be 10,000 XXX");
      return -1;
    } 
  } else { return -1; } 
  NEXT_REC;

  if (PDC_OK == PDC_bcd_fpoint16_read(pdc, &csm, 5, 4, &ed, &f16)) {
    error(0, "Read bcd fpoint: num %ld denom %lu", (long)f16.num, (unsigned long)f16.denom);
    if (f16.num != PDC_MAX_INT16) {
      error(0, "XXX failure: should be %ld XXX", (long)PDC_MAX_INT16);
      return -1;
    }
    if (f16.denom != 10000) {
      error(0, "XXX failure: denom should be 10,000 XXX");
      return -1;
    } 
  } else { return -1; } 
  NEXT_REC;

  if (PDC_OK == PDC_bcd_ufpoint16_read(pdc, &csm, 5, 4, &ed, &uf16)) {
    error(0, "Read bcd ufpoint: num %lu denom %lu", (unsigned long)uf16.num, (unsigned long)uf16.denom);
    if (uf16.num != PDC_MAX_UINT16) {
      error(0, "XXX failure: should be %lu XXX", (unsigned long)PDC_MAX_UINT16);
      return -1;
    }
    if (uf16.denom != 10000) {
      error(0, "XXX failure: denom should be 10,000 XXX");
      return -1;
    } 
  } else { return -1; } 
  NEXT_REC;

  if (PDC_OK == PDC_bcd_fpoint32_read(pdc, &csm, 11, 9, &ed, &f32)) {
    error(0, "Read bcd fpoint: num %ld denom %lu", (long)f32.num, (unsigned long)f32.denom);
    if (f32.num != PDC_MIN_INT32) {
      error(0, "XXX failure: should be %ld XXX", (long)PDC_MIN_INT32);
      return -1;
    }
    if (f32.denom != 1000000000UL) {
      error(0, "XXX failure: denom should be 1,000,000,000 XXX");
      return -1;
    } 
  } else { return -1; } 
  NEXT_REC;

  if (PDC_OK == PDC_bcd_fpoint32_read(pdc, &csm, 10, 9, &ed, &f32)) {
    error(0, "Read bcd fpoint: num %ld denom %lu", (long)f32.num, (unsigned long)f32.denom);
    if (f32.num != PDC_MAX_INT32) {
      error(0, "XXX failure: should be %ld XXX", (long)PDC_MAX_INT32);
      return -1;
    }
    if (f32.denom != 1000000000UL) {
      error(0, "XXX failure: denom should be 1,000,000,000 XXX");
      return -1;
    } 
  } else { return -1; } 
  NEXT_REC;

  if (PDC_OK == PDC_bcd_ufpoint32_read(pdc, &csm, 10, 9, &ed, &uf32)) {
    error(0, "Read bcd ufpoint: num %lu denom %lu", (unsigned long)uf32.num, (unsigned long)uf32.denom);
    if (uf32.num != PDC_MAX_UINT32) {
      error(0, "XXX failure: should be %lu XXX", (unsigned long)PDC_MAX_UINT32);
      return -1;
    }
    if (uf32.denom != 1000000000UL) {
      error(0, "XXX failure: denom should be 1,000,000,000 XXX");
      return -1;
    } 
  } else { return -1; } 
  NEXT_REC;

  if (PDC_OK == PDC_bcd_fpoint64_read(pdc, &csm, 19, 19, &ed, &f64)) {
    error(0, "Read bcd fpoint: num %lld denom %llu", (long long)f64.num, (unsigned long long)f64.denom);
    if (f64.num != PDC_MIN_INT64) {
      error(0, "XXX failure: should be %lld XXX", (long long)PDC_MIN_INT64);
      return -1;
    }
    if (f64.denom != 10000000000000000000ULL) {
      error(0, "XXX failure: denom should be 10,000,000,000,000,000,000 XXX");
      return -1;
    } 
  } else { return -1; } 
  NEXT_REC;

  if (PDC_OK == PDC_bcd_fpoint64_read(pdc, &csm, 19, 19, &ed, &f64)) {
    error(0, "Read bcd fpoint: num %lld denom %llu", (long long)f64.num, (unsigned long long)f64.denom);
    if (f64.num != PDC_MAX_INT64) {
      error(0, "XXX failure: should be %lld XXX", (long long)PDC_MAX_INT64);
      return -1;
    }
    if (f64.denom != 10000000000000000000ULL) {
      error(0, "XXX failure: denom should be 10,000,000,000,000,000,000 XXX");
      return -1;
    } 
  } else { return -1; } 
  NEXT_REC;

  if (PDC_OK == PDC_bcd_ufpoint64_read(pdc, &csm, 20, 19, &ed, &uf64)) {
    error(0, "Read bcd ufpoint: num %llu denom %llu", (unsigned long long)uf64.num, (unsigned long long)uf64.denom);
    if (uf64.num != PDC_MAX_UINT64) {
      error(0, "XXX failure: should be %llu XXX", (unsigned long long)PDC_MAX_UINT64);
      return -1;
    }
    if (uf64.denom != 10000000000000000000ULL) {
      error(0, "XXX failure: denom should be 10,000,000,000,000,000,000 XXX");
      return -1;
    } 
  } else { return -1; } 
  NEXT_REC;

 done:
  if (PDC_ERR == PDC_IO_close(pdc)) {
    error(2, "*** PDC_IO_close failed ***");
    return -1;
  }

  if (PDC_ERR == PDC_close(pdc)) {
    error(2, "*** PDC_close failed ***");
    return -1;
  }

  return 0;

 usage:
  error(2, "\nUsage: %s [ <io-disc-name> ]\n\n\twhere <io-disc-name> is one of: fwrec, ctrec, norec, fwrec_noseek, ctrec_noseek, norec_noseek\n", argv[0]);
  return -1;
}
