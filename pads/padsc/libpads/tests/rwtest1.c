/*
 *  rwtest1: Test var-length ascii read/write functions
 */


#include "libpadsc-internal.h" /* for testing - normally do not include internal */
#ifndef PDCI_MacroArg2String
#define PDCI_MacroArg2String(s) #s
#endif

#define CHECK_EOF do { \
  if (PDC_IO_at_EOF(pdc)) { \
    error(0, "Main program found eof"); \
    break; \
  } \
} while(0)

#define NEXT_REC do { \
  if (strncmp(argv[1], "norec", 5) == 0) { \
    if (PDC_ERR == PDC_a_char_lit_scan(pdc, '\n', '\n', 1, 0, &bytes_skipped)) { \
      error(2|ERROR_FATAL, "Could not find EOR (newline), ending program"); \
    } \
  } else { \
    if (PDC_ERR == PDC_IO_next_rec(pdc, &bytes_skipped)) { \
      error(2|ERROR_FATAL, "Could not find EOR (newline), ending program"); \
    } \
  } \
  if (bytes_skipped != 0) { \
    error(2|ERROR_FATAL, "Bytes skipped should be 0, got %d, ending program", bytes_skipped); \
  } \
} while(0)

#define READ1(int_type, ivar, expect, fmt) do { \
  CHECK_EOF; \
  if (PDC_ERR == PDC_a_ ## int_type ## _read(pdc, &m, &pd, &ivar)) { \
    error(2|ERROR_FATAL, "Failed to read " PDCI_MacroArg2String(int_type)); \
  } \
  if (ivar != expect) { \
    error(2|ERROR_FATAL, "Expected %" fmt ", found %" fmt, expect, ivar); \
  } \
  NEXT_REC; \
} while(0)

#define WRITE1(int_type, ivar) do { \
  if (-1 == PDC_a_ ## int_type ## _write2io(pdc, io, &pd, &ivar)) { \
    error(2|ERROR_FATAL, "Failed to write " PDCI_MacroArg2String(int_type)); \
  } \
  if (-1 == sfprintf(io, "\n")) { \
    error(2|ERROR_FATAL, "Failed to write newline"); \
  } \
} while(0)

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
  PDC_base_m      m     = PDC_CheckAndSet;
  PDC_base_pd     pd;
  PDC_disc_t      my_disc = PDC_default_disc;
  size_t          bytes_skipped;
  Sfio_t         *io;
  char           *outf = "tmp/rwtest.write";

  my_disc.flags |= (PDC_flags_t)PDC_WSPACE_OK;

  if (argc != 2) {
    goto usage;
  }

  if (strcmp(argv[1], "nlrec") == 0) {
    io_disc = PDC_nlrec_make(0);
  } else if (strcmp(argv[1], "norec") == 0) {
    io_disc = PDC_norec_make(0);
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
    error(2|ERROR_FATAL, "*** PDC_open failed ***");
  }
  if (PDC_ERR == PDC_IO_fopen(pdc, "../../data/ex_data.rwtest1")) {
    error(2|ERROR_FATAL, "*** PDC_IO_fopen failed ***");
  }

  if (!(io = sfopen(NiL, outf, "w"))) {
    PDC_SYSERR1(pdc->disc, "Failed to open output file \"%s\" for writing", outf);
  }
  /*
   * XXX Process the data here XXX
   */
  while (1) {
    /* read then write all the min/max integers */
    READ1 (int8,   i8,  PDC_MIN_INT8,   "d");
    WRITE1(int8,   i8);
    READ1 (int8,   i8,  PDC_MAX_INT8,   "d");
    WRITE1(int8,   i8);
    READ1 (uint8,  u8,  PDC_MAX_UINT8,  "u");
    WRITE1(uint8,  u8);

    READ1 (int16,  i16, PDC_MIN_INT16,   "d");
    WRITE1(int16,  i16);
    READ1 (int16,  i16, PDC_MAX_INT16,   "d");
    WRITE1(int16,  i16);
    READ1 (uint16, u16, PDC_MAX_UINT16,  "u");
    WRITE1(uint16, u16);

    READ1 (int32,  i32, PDC_MIN_INT32,   "ld");
    WRITE1(int32,  i32);
    READ1 (int32,  i32, PDC_MAX_INT32,   "ld");
    WRITE1(int32,  i32);
    READ1 (uint32, u32, PDC_MAX_UINT32,  "lu");
    WRITE1(uint32, u32);

    READ1 (int64,  i64, PDC_MIN_INT64,   "lld");
    WRITE1(int64,  i64);
    READ1 (int64,  i64, PDC_MAX_INT64,   "lld");
    WRITE1(int64,  i64);
    READ1 (uint64, u64, PDC_MAX_UINT64,  "llu");
    WRITE1(uint64, u64);

    if (PDC_ERR == PDC_IO_close(pdc)) {
      error(2|ERROR_FATAL, "*** PDC_IO_close failed ***");
    }

    sfclose(io);

    if (PDC_ERR == PDC_IO_fopen(pdc, outf)) {
      error(2|ERROR_FATAL, "*** PDC_IO_fopen failed for file %s ***", outf);
    }

    /* read all the min/max integers */
    READ1 (int8,   i8,  PDC_MIN_INT8,   "d");
    READ1 (int8,   i8,  PDC_MAX_INT8,   "d");
    READ1 (uint8,  u8,  PDC_MAX_UINT8,  "u");

    READ1 (int16,  i16, PDC_MIN_INT16,   "d");
    READ1 (int16,  i16, PDC_MAX_INT16,   "d");
    READ1 (uint16, u16, PDC_MAX_UINT16,  "u");

    READ1 (int32,  i32, PDC_MIN_INT32,   "ld");
    READ1 (int32,  i32, PDC_MAX_INT32,   "ld");
    READ1 (uint32, u32, PDC_MAX_UINT32,  "lu");

    READ1 (int64,  i64, PDC_MIN_INT64,   "lld");
    READ1 (int64,  i64, PDC_MAX_INT64,   "lld");
    READ1 (uint64, u64, PDC_MAX_UINT64,  "llu");
    break;
  }

  if (PDC_ERR == PDC_IO_close(pdc)) {
    error(2|ERROR_FATAL, "*** PDC_IO_close failed ***");
  }

  if (PDC_ERR == PDC_close(pdc)) {
    error(2|ERROR_FATAL, "*** PDC_close failed ***");
  }

  return 0;

 usage:
  error(2, "\nUsage: %s <io-disc-name>\n\n\twhere <io-disc-name> is one of: nlrec, norec, nlrec_noseek, norec_noseek\n", argv[0]);
  return -1;
}
