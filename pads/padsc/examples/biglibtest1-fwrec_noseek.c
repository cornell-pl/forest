/*
 *  libtest1: Test fixed width ascii read functions
 */


#include "libpadsc-internal.h" /* for testing - normally do not include internal */

int main(int argc, char** argv) {
  /* int             ctr; */
  /* size_t          n; */
  /* unsigned char   c; */
  int             i;
  unsigned long   count = 0;
  PDC_t*          pdc;
  PDC_int32        i1;
  PDC_base_em     em = PDC_CheckAndSet;
  PDC_base_ed     ed;
  PDC_disc_t      my_disc = PDC_default_disc;
  size_t          bytes_skipped;

  my_disc.flags |= (PDC_flags_t)PDC_WSPACE_OK;
  my_disc.e_rep = PDC_errorRep_Min;
  PDC_fwrec_noseek_install(&my_disc, 24, 1); /* 4 6-char ints, newline */ 

  if (PDC_ERR == PDC_open(&my_disc, &pdc)) {
    error(2, "*** PDC_open failed ***");
    exit(-1);
  }
  if (PDC_ERR == PDC_IO_fopen(pdc, "../ex_data.libtest1-big", &my_disc)) {
    error(2, "*** PDC_IO_fopen failed ***");
    exit(-1);
  }

  /*
   * XXX Process the data here XXX
   */
  while (1) {
    count++;
    if (PDC_IO_at_EOF(pdc, &my_disc)) {
      break;
    }
    /* try to read 4 fixed width integers (width 6) */
    for (i = 0; i < 4; i++) {
      PDC_aint32_fw_read(pdc, &em, 6, &ed, &i1, &my_disc);
    }
    if (PDC_ERR == PDC_IO_next_rec(pdc, &bytes_skipped, &my_disc)) {
      break;
    }
  }

  printf("\n%lu\n\n", count);

  if (PDC_ERR == PDC_IO_fclose(pdc, &my_disc)) {
    error(2, "*** PDC_IO_fclose failed ***");
    exit(-1);
  }

  if (PDC_ERR == PDC_close(pdc, &my_disc)) {
    error(2, "*** PDC_close failed ***");
    exit(-1);
  }
  return 0;
}
