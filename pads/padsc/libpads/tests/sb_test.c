/*
 *  sb_test: tests
 *       PDC_sbl_int8_read
 *       PDC_sbl_uint8_read
 *       PDC_sbl_int16_read
 *       PDC_sbl_uint16_read
 *       PDC_sbl_int32_read
 *       PDC_sbl_uint32_read
 *       PDC_sbl_int64_read
 *       PDC_sbl_uint64_read
 *
 *       PDC_sbh_int8_read
 *       PDC_sbh_uint8_read
 *       PDC_sbh_int16_read
 *       PDC_sbh_uint16_read
 *       PDC_sbh_int32_read
 *       PDC_sbh_uint32_read
 *       PDC_sbh_int64_read
 *       PDC_sbh_uint64_read
 */

#include "libpadsc-internal.h" /* for testing - normally do not include internal */

#define NEXT_REC do {\
  if (PDC_ERR == PDC_IO_next_rec(pdc, &bytes_skipped)) { \
    error(2, "Could not find EOR (newline), ending program"); \
    goto done; \
  } \
  ultmp = bytes_skipped; \
  error(0, "next_rec returned bytes_skipped = %ld", ultmp); \
} while (0)

int main(int argc, char** argv) {
  PDC_t*          pdc;
  PDC_IO_disc_t*  io_disc;
  PDC_int8        i8;
  PDC_int16       i16;
  PDC_int32       i32;
  PDC_int64       i64;
  PDC_uint8       u8;
  PDC_uint16      u16;
  PDC_uint32      u32;
  PDC_uint64      u64;
  PDC_disc_t      my_disc = PDC_default_disc;
  PDC_base_m      m       = PDC_CheckAndSet;
  PDC_base_ed     ed;
  size_t          bytes_skipped;
  unsigned long        ultmp;

  my_disc.flags |= (PDC_flags_t)PDC_WSPACE_OK;

  if ((argc != 1) && (argc != 2)) {
    goto usage;
  }
  if (argc == 1) {
    io_disc = PDC_fwrec_noseek_make(0, 16, 0);
  } else if (strcmp(argv[1], "fwrec") == 0) {
    io_disc = PDC_fwrec_make(0, 16, 0);
  } else if (strcmp(argv[1], "fwrec_noseek") == 0) {
    io_disc = PDC_fwrec_noseek_make(0, 16, 0);
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
  if (PDC_ERR == PDC_IO_fopen(pdc, "../../data/ex_data.sb_test")) {
    error(2, "*** PDC_IO_fopen failed ***");
    return -1;
  }

  if (PDC_OK == PDC_sbl_int8_read(pdc, &m, 1, &ed, &i8)) {
    error(0, "Read sbl integer: %ld", (long)i8);
    if (i8 != 0) {
      error(0, "XXX failure: should be %ld XXX", (long)0);
      return -1;
    }
  } else { return -1; } 
  NEXT_REC;

  if (PDC_OK == PDC_sbl_int8_read(pdc, &m, 1, &ed, &i8)) {
    error(0, "Read sbl integer: %ld", (long)i8);
    if (i8 != PDC_MIN_INT8) {
      error(0, "XXX failure: should be %ld XXX", (long)PDC_MIN_INT8);
      /* return -1; */
    }
  } else { return -1; } 
  NEXT_REC;

  if (PDC_OK == PDC_sbl_int8_read(pdc, &m, 1, &ed, &i8)) {
    error(0, "Read sbl integer: %ld", (long)i8);
    if (i8 != PDC_MAX_INT8) {
      error(0, "XXX failure: should be %ld XXX", (long)PDC_MAX_INT8);
      /* return -1; */
    }
  } else { return -1; } 
  NEXT_REC;

  if (PDC_OK == PDC_sbl_uint8_read(pdc, &m, 1, &ed, &u8)) {
    error(0, "Read sbl integer: %lu", (unsigned long)u8);
    if (u8 != PDC_MAX_UINT8) {
      error(0, "XXX failure: should be %lu XXX", (unsigned long)PDC_MAX_UINT8);
      /* return -1; */
    }
  } else { return -1; } 
  NEXT_REC;

  if (PDC_OK == PDC_sbl_int16_read(pdc, &m, 2, &ed, &i16)) {
    error(0, "Read sbl integer: %ld", (long)i16);
    if (i16 != PDC_MIN_INT16) {
      error(0, "XXX failure: should be %ld XXX", (long)PDC_MIN_INT16);
      /* return -1; */
    }
  } else { return -1; } 
  NEXT_REC;

  if (PDC_OK == PDC_sbl_int16_read(pdc, &m, 2, &ed, &i16)) {
    error(0, "Read sbl integer: %ld", (long)i16);
    if (i16 != PDC_MAX_INT16) {
      error(0, "XXX failure: should be %ld XXX", (long)PDC_MAX_INT16);
      /* return -1; */
    }
  } else { return -1; } 
  NEXT_REC;

  if (PDC_OK == PDC_sbl_uint16_read(pdc, &m, 2, &ed, &u16)) {
    error(0, "Read sbl integer: %lu", (unsigned long)u16);
    if (u16 != PDC_MAX_UINT16) {
      error(0, "XXX failure: should be %lu XXX", (unsigned long)PDC_MAX_UINT16);
      /* return -1; */
    }
  } else { return -1; } 
  NEXT_REC;

  if (PDC_OK == PDC_sbl_int32_read(pdc, &m, 4, &ed, &i32)) {
    error(0, "Read sbl integer: %ld", (long)i32);
    if (i32 != PDC_MIN_INT32) {
      error(0, "XXX failure: should be %ld XXX", (long)PDC_MIN_INT32);
      /* return -1; */
    }
  } else { return -1; } 
  NEXT_REC;

  if (PDC_OK == PDC_sbl_int32_read(pdc, &m, 4, &ed, &i32)) {
    error(0, "Read sbl integer: %ld", (long)i32);
    if (i32 != PDC_MAX_INT32) {
      error(0, "XXX failure: should be %ld XXX", (long)PDC_MAX_INT32);
      /* return -1; */
    }
  } else { return -1; } 
  NEXT_REC;

  if (PDC_OK == PDC_sbl_uint32_read(pdc, &m, 4, &ed, &u32)) {
    error(0, "Read sbl integer: %lu", (unsigned long)u32);
    if (u32 != PDC_MAX_UINT32) {
      error(0, "XXX failure: should be %lu XXX", (unsigned long)PDC_MAX_UINT32);
      /* return -1; */
    }
  } else { return -1; } 
  NEXT_REC;

  if (PDC_OK == PDC_sbl_int64_read(pdc, &m, 8, &ed, &i64)) {
    error(0, "Read sbl integer: %lld", (long long)i64);
    if (i64 != PDC_MIN_INT64) {
      error(0, "XXX failure: should be %lld XXX", (long long)PDC_MIN_INT64);
      /* return -1; */
    }
  } else { return -1; } 
  NEXT_REC;

  if (PDC_OK == PDC_sbl_int64_read(pdc, &m, 8, &ed, &i64)) {
    error(0, "Read sbl integer: %lld", (long long)i64);
    if (i64 != PDC_MAX_INT64) {
      error(0, "XXX failure: should be %lld XXX", (long long)PDC_MAX_INT64);
      /* return -1; */
    }
  } else { return -1; } 
  NEXT_REC;

  if (PDC_OK == PDC_sbl_uint64_read(pdc, &m, 8, &ed, &u64)) {
    error(0, "Read sbl integer: %llu", (unsigned long long)u64);
    if (u64 != PDC_MAX_UINT64) {
      error(0, "XXX failure: should be %llu XXX", (unsigned long long)PDC_MAX_UINT64);
      /* return -1; */
    }
  } else { return -1; } 
  NEXT_REC;

  if (PDC_OK == PDC_sbh_int8_read(pdc, &m, 1, &ed, &i8)) {
    error(0, "Read sbh integer: %ld", (long)i8);
    if (i8 != 0) {
      error(0, "XXX failure: should be %ld XXX", (long)0);
      return -1;
    }
  } else { return -1; } 
  NEXT_REC;

  if (PDC_OK == PDC_sbh_int8_read(pdc, &m, 1, &ed, &i8)) {
    error(0, "Read sbh integer: %ld", (long)i8);
    if (i8 != PDC_MIN_INT8) {
      error(0, "XXX failure: should be %ld XXX", (long)PDC_MIN_INT8);
      /* return -1; */
    }
  } else { return -1; } 
  NEXT_REC;

  if (PDC_OK == PDC_sbh_int8_read(pdc, &m, 1, &ed, &i8)) {
    error(0, "Read sbh integer: %ld", (long)i8);
    if (i8 != PDC_MAX_INT8) {
      error(0, "XXX failure: should be %ld XXX", (long)PDC_MAX_INT8);
      /* return -1; */
    }
  } else { return -1; } 
  NEXT_REC;

  if (PDC_OK == PDC_sbh_uint8_read(pdc, &m, 1, &ed, &u8)) {
    error(0, "Read sbh integer: %lu", (unsigned long)u8);
    if (u8 != PDC_MAX_UINT8) {
      error(0, "XXX failure: should be %lu XXX", (unsigned long)PDC_MAX_UINT8);
      /* return -1; */
    }
  } else { return -1; } 
  NEXT_REC;

  if (PDC_OK == PDC_sbh_int16_read(pdc, &m, 2, &ed, &i16)) {
    error(0, "Read sbh integer: %ld", (long)i16);
    if (i16 != PDC_MIN_INT16) {
      error(0, "XXX failure: should be %ld XXX", (long)PDC_MIN_INT16);
      /* return -1; */
    }
  } else { return -1; } 
  NEXT_REC;

  if (PDC_OK == PDC_sbh_int16_read(pdc, &m, 2, &ed, &i16)) {
    error(0, "Read sbh integer: %ld", (long)i16);
    if (i16 != PDC_MAX_INT16) {
      error(0, "XXX failure: should be %ld XXX", (long)PDC_MAX_INT16);
      /* return -1; */
    }
  } else { return -1; } 
  NEXT_REC;

  if (PDC_OK == PDC_sbh_uint16_read(pdc, &m, 2, &ed, &u16)) {
    error(0, "Read sbh integer: %lu", (unsigned long)u16);
    if (u16 != PDC_MAX_UINT16) {
      error(0, "XXX failure: should be %lu XXX", (unsigned long)PDC_MAX_UINT16);
      /* return -1; */
    }
  } else { return -1; } 
  NEXT_REC;

  if (PDC_OK == PDC_sbh_int32_read(pdc, &m, 4, &ed, &i32)) {
    error(0, "Read sbh integer: %ld", (long)i32);
    if (i32 != PDC_MIN_INT32) {
      error(0, "XXX failure: should be %ld XXX", (long)PDC_MIN_INT32);
      /* return -1; */
    }
  } else { return -1; } 
  NEXT_REC;

  if (PDC_OK == PDC_sbh_int32_read(pdc, &m, 4, &ed, &i32)) {
    error(0, "Read sbh integer: %ld", (long)i32);
    if (i32 != PDC_MAX_INT32) {
      error(0, "XXX failure: should be %ld XXX", (long)PDC_MAX_INT32);
      /* return -1; */
    }
  } else { return -1; } 
  NEXT_REC;

  if (PDC_OK == PDC_sbh_uint32_read(pdc, &m, 4, &ed, &u32)) {
    error(0, "Read sbh integer: %lu", (unsigned long)u32);
    if (u32 != PDC_MAX_UINT32) {
      error(0, "XXX failure: should be %lu XXX", (unsigned long)PDC_MAX_UINT32);
      /* return -1; */
    }
  } else { return -1; } 
  NEXT_REC;

  if (PDC_OK == PDC_sbh_int64_read(pdc, &m, 8, &ed, &i64)) {
    error(0, "Read sbh integer: %lld", (long long)i64);
    if (i64 != PDC_MIN_INT64) {
      error(0, "XXX failure: should be %lld XXX", (long long)PDC_MIN_INT64);
      /* return -1; */
    }
  } else { return -1; } 
  NEXT_REC;

  if (PDC_OK == PDC_sbh_int64_read(pdc, &m, 8, &ed, &i64)) {
    error(0, "Read sbh integer: %lld", (long long)i64);
    if (i64 != PDC_MAX_INT64) {
      error(0, "XXX failure: should be %lld XXX", (long long)PDC_MAX_INT64);
      /* return -1; */
    }
  } else { return -1; } 
  NEXT_REC;

  if (PDC_OK == PDC_sbh_uint64_read(pdc, &m, 8, &ed, &u64)) {
    error(0, "Read sbh integer: %llu", (unsigned long long)u64);
    if (u64 != PDC_MAX_UINT64) {
      error(0, "XXX failure: should be %llu XXX", (unsigned long long)PDC_MAX_UINT64);
      /* return -1; */
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
  error(2, "\nUsage: %s [ <io-disc-name> ]\n\n\twhere <io-disc-name> is one of: fwrec, fwrec_noseek\n", argv[0]);
  return -1;
}
