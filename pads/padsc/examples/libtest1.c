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
  PDC_int8        i1;
  PDC_base_em     em = PDC_CheckAndSet;
  PDC_base_ed     ed;
  PDC_disc_t      my_disc = PDC_default_disc;

  my_disc.flags |= (PDC_flags_t)PDC_WSPACE_OK;

  if (PDC_ERROR == PDC_open(0, &pdc)) {
    error(2, "*** PDC_open failed ***");
    exit(-1);
  }
  if (PDC_ERROR == PDC_IO_fopen(pdc, "../ex_data.libtest1", &my_disc)) {
    error(2, "*** PDC_IO_fopen failed ***");
    exit(-1);
  }

  /*
   * XXX Process the data here XXX
   */
  while (1) {
    if (PDC_IO_peek_EOF(pdc, &my_disc)) {
      error(0, "Main program found eof");
      break;
    }
    /* try to read 4 fixed width integers (width 6) */
    for (i = 0; i < 4; i++) {
      if (PDC_ERROR == PDC_aint8_fw_read(pdc, &em, 6, &ed, &i1, &my_disc)) {
	PDC_report_err (pdc, &my_disc, 0, &ed.loc, ed.errCode, 0);
      } else {
	error(0, "Read ascii integer of width 6: %ld", i1);
      }
    }
    if (PDC_ERROR == PDC_char_lit_scan(pdc, '\n', '\n', 0, 0, &my_disc)) {
      error(2, "Could not find newline, ending program");
      break;
    }
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
