/*
 *  libtest1: Test fixed width ascii read functions
 */


#include "libpadsc-internal.h" /* for testing - normally do not include internal */
#include <stdio.h>

/* Remove comments to see classic example of a case where mixing binary data and newlines can fail */
/* #define USE_NLREC */

int main(int argc, char** argv) {
  /* int             ctr; */
  /* size_t          n; */
  /* unsigned char   c; */
  char fname[1000];
  char* h;
  int rev = 0;
  PDC_t*          pdc;
  PDC_IO_disc_t*  io_disc;
  PDC_int8        i1;
  PDC_int16       i2;
  PDC_int32       i4;
  PDC_int64       i8;
  PDC_uint8       ui1;
  PDC_uint16      ui2;
  PDC_uint32      ui4;
  PDC_uint64      ui8;
  PDC_base_m      m    = PDC_CheckAndSet;
  PDC_base_pd     pd;
  PDC_disc_t      my_disc = PDC_default_disc;
  size_t          bytes_skipped;
  unsigned long   ultmp;

#ifdef USE_NLREC
  error(0, "\nUsing PADSC IO discipline nlrec_noseek\n\n");
  io_disc = PDC_nlrec_noseek_make(0);
#else
  error(0, "\nUsing PADSC IO discipline norec\n\n");
  io_disc = PDC_norec_make(0);
#endif

  if (argc >= 2) {
    h = argv[1];
  } else {
    h = getenv("AST_ARCH");
  }
  if (argc >= 3 && strcmp(argv[2], "rev") == 0) {
    rev = 1;
  }
  sprintf(fname, "../../data/ex_data.libtest2.%s", h);
  error(0, "fname = %s    rev = %d\n", fname, rev);

  if (PDC_ERR == PDC_open(&pdc, &my_disc, io_disc)) {
    error(2, "*** PDC_open failed ***");
    exit(-1);
  }

  switch (pdc->m_endian) {
  case PDC_bigEndian:
    my_disc.d_endian = rev ? PDC_littleEndian : PDC_bigEndian;
    break;
  case PDC_littleEndian:
    my_disc.d_endian = rev ? PDC_bigEndian : PDC_littleEndian;
    break;
  }

  if (PDC_ERR == PDC_IO_fopen(pdc, fname)) {
    error(2, "*** PDC_IO_fopen failed ***");
    exit(-1);
  }

  while (1) {
    if (PDC_IO_at_EOF(pdc)) {
      error(0, "Main program found eof");
      break;
    }

    if (PDC_ERR == PDC_b_int8_read(pdc, &m, &pd, &i1)) {
      goto check_newline;
    } else {
      error(0, "Read bint8  : %ld", i1);
    }
    if (PDC_ERR == PDC_b_uint8_read(pdc, &m, &pd, &ui1)) {
      goto check_newline;
    } else {
      error(0, "Read buint8 : %lu", ui1);
    }

    if (PDC_ERR == PDC_b_int16_read(pdc, &m, &pd, &i2)) {
      goto check_newline;
    } else {
      error(0, "Read bint16  : %ld", i2);
    }
    if (PDC_ERR == PDC_b_uint16_read(pdc, &m, &pd, &ui2)) {
      goto check_newline;
    } else {
      error(0, "Read buint16 : %lu", ui2);
    }
    if (PDC_ERR == PDC_b_int32_read(pdc, &m, &pd, &i4)) {
      goto check_newline;
    } else {
      error(0, "Read bint32  : %ld", i4);
    }
    if (PDC_ERR == PDC_b_uint32_read(pdc, &m, &pd, &ui4)) {
      goto check_newline;
    } else {
      error(0, "Read buint32 : %lu", ui4);
    }
    if (PDC_ERR == PDC_b_int64_read(pdc, &m, &pd, &i8)) {
      goto check_newline;
    } else {
      error(0, "Read bint64  : %lld", i8);
    }
    if (PDC_ERR == PDC_b_uint64_read(pdc, &m, &pd, &ui8)) {
      goto check_newline;
    } else {
      error(0, "Read buint64 : %llu", ui8);
    }

  check_newline:
#ifdef USE_NLREC
    if (PDC_ERR == PDC_IO_next_rec(pdc, &bytes_skipped)) {
      error(2, "Could not find EOR (newline), ending program");
      goto done;
    }
#else
    if (PDC_ERR == PDC_a_char_lit_scan(pdc, '\n', '\n', 1, 0, &bytes_skipped)) {
      error(2, "Could not find newline, ending program");
      break;
    }
#endif
    ultmp = bytes_skipped;
    error(0, "bytes_skipped to find EOR/newline = %ld", ultmp);
  }

#ifdef USE_NLREC
 done:
#endif
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
