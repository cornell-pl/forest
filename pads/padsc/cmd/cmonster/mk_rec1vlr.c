/*
 * Write to stdout a data file with vlrec format.
 * Data is read from stdin using io_disc nlrec
 * and the format described in rec1.p (in_rec) and it
 * is written out using the format described in
 * rec1.p (out_rec).
 */

#include "rec1.h"

int main(int argc, char** argv) {
  PDC_t                  *pdc;
  PDC_disc_t              my_disc;
  PDC_IO_disc_t          *in_io_disc, *out_io_disc;

  in_file                 rep;
  in_file_m               m;
  in_file_pd              pd;

  in_file                *in_rep  = &rep;
  in_file_m              *in_m    = &m;
  in_file_pd             *in_pd   = &pd;

  out_file               *out_rep = (out_file*)   &rep;
  out_file_pd            *out_pd  = (out_file_pd*)&pd;

  in_io_disc = PDC_nlrec_make(0);
  if (!in_io_disc) {
    error(ERROR_FATAL, "\nFailed to make IO discipline nlrec");
  }
  out_io_disc = PDC_vlrec_make(0, 0);
  if (!out_io_disc) {
    error(ERROR_FATAL, "\nFailed to make IO discipline vlrec");
  }

  my_disc = PDC_default_disc;
  my_disc.flags |= (PDC_flags_t)PDC_WSPACE_OK;
  my_disc.copy_strings = 1; /* required because we read more than one record before processing them */

  if (PDC_ERR == PDC_open(&pdc, &my_disc, in_io_disc)) {
    error(ERROR_FATAL, "\n*** PDC_open failed ***");
  }

  in_file_init(pdc, in_rep);
  in_file_m_init(pdc, in_m, PDC_CheckAndSet);
  in_file_pd_init(pdc, in_pd);

  if (PDC_ERR == PDC_IO_fopen(pdc, "/dev/stdin")) {
    error(ERROR_FATAL, "\n*** PDC_IO_fopen failed ***");
  }

  if (PDC_ERR == in_file_read(pdc, in_m, in_pd, in_rep)) {
    error(ERROR_FATAL, "\n*** in_file_read failed ***");
  }

  if (PDC_ERR == PDC_IO_close(pdc)) {
    error(ERROR_FATAL, "\n*** PDC_IO_close failed ***");
  }

  if (PDC_ERR == PDC_set_IO_disc(pdc, out_io_disc)) {
    error(ERROR_FATAL, "\n*** PDC_set_IO_disc failed ***");
  }

  out_file_write2io(pdc, sfstdout, out_pd, out_rep);

  /* done */

  if (PDC_ERR == PDC_close(pdc)) {
    error(ERROR_FATAL, "\n*** PDC_close failed ***");
  }

  return 0;
};

