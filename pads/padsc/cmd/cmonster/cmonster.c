/*
 * cmonster or 'cookie monster' command
 *
 * Usage: cmonster '<input_io_disc>([<io_disc_params>])' '<cookies>'
 * 
 *     cmonster reads a record at a time from stdin, using the
 *     specified input IO discipline, and writes a record at a time to
 *     stdout using the specified 'cookies' to read parts of the input
 *     record; each value read is written to stdout in canonical
 *     (machine-independent) form.
 *
 * Kathleen Fisher, Robert Gruber
 * AT&T Labs Research
 */

#include "cmonster.h"
#include "cmhelpers.h"

int main(int argc, char** argv) {
  PDC_t*                  pdc;
  PDC_disc_t              my_disc;
  PDC_IO_disc_t*          io_disc;
  CM_iodisc               ispec;
  CM_iodisc_m             ispec_m;
  CM_iodisc_pd            ispec_pd;

  CM_cookie               cspec;
  CM_cookie_m             cspec_m;
  CM_cookie_pd            cspec_pd;

  Sfio_t                 *io;
  size_t                  skipped_bytes = 0, rec_len;
  int                     eor = 0, eof = 0, skip_rec = 0;
  PDC_byte               *begin = 0, *end = 0;

  if (argc != 3) goto usage;
  /* Parse command line */

  io_disc = PDC_norec_make(0);
  if (!io_disc) {
    error(ERROR_FATAL, "\nFailed to install IO discipline to parse command line");
  }

  my_disc = PDC_default_disc;
  my_disc.flags |= (PDC_flags_t)PDC_WSPACE_OK;

  if (PDC_ERR == PDC_open(&pdc, &my_disc, io_disc)) {
    error(ERROR_FATAL, "\n*** PDC_open failed ***");
  }

  CM_iodisc_init(pdc, &ispec);
  CM_iodisc_pd_init(pdc, &ispec_pd);
  CM_iodisc_m_init(pdc, &ispec_m, PDC_CheckAndSet);

  CM_cookie_init(pdc, &cspec);
  CM_cookie_pd_init(pdc, &cspec_pd);
  CM_cookie_m_init(pdc, &cspec_m, PDC_CheckAndSet);

  if (!(io = sfopen(NULL, argv[1], "s"))) {
    error(ERROR_FATAL, "\nXXX unexpected: sfopen(NULL, argv[1], \"s\") failed.");
  }
  if (PDC_ERR == PDC_IO_set(pdc, io)) {
    error(ERROR_FATAL, "\n*** PDC_IO_set failed ***");
  }
  if (PDC_ERR == CM_iodisc_read(pdc, &ispec_m, &ispec_pd, &ispec)) {
    error(0, "\nCould not parse arg1 io_spec");
    goto usage;
  }
  if (PDC_ERR == PDC_IO_close(pdc)) {
    error(ERROR_FATAL, "\n*** PDC_IO_close failed ***");
  }
  sfclose(io);

  if (!(io = sfopen(NULL, argv[2], "s"))) {
    error(ERROR_FATAL, "\nXXX unexpected: sfopen(NULL, argv[2], \"s\") failed.");
  }
  if (PDC_ERR == PDC_IO_set(pdc, io)) {
    error(ERROR_FATAL, "\n*** PDC_IO_set failed ***");
  }
  if (PDC_ERR == CM_cookie_read(pdc, &cspec_m, &cspec_pd, &cspec)) {
    error(0, "\nCould not parse arg2 cookie");
    goto usage;
  }
  if (PDC_ERR == PDC_IO_close(pdc)) {
    error(ERROR_FATAL, "\n*** PDC_IO_close failed ***");
  }
  sfclose(io);
  if (PDC_ERR == PDC_close(pdc)) {
    error(ERROR_FATAL, "\n*** PDC_close failed ***");
  }
  error(0, "XXX_REMOVE description of cookie");
  describe_cookie(&cspec);

  if (-1 == CMR_open_iodisc(&ispec, &io_disc)) goto usage;

  if (!io_disc) {
    error(ERROR_FATAL, "\nFailed to install IO discipline %s", argv[1]);
  } else {
    error(0, "\nXXX_REMOVE Installed IO discipline %s", argv[1]);
  }

  if (PDC_ERR == PDC_open(&pdc, &my_disc, io_disc)) {
    error(ERROR_FATAL, "\n*** PDC_open failed ***");
  }
  if (PDC_ERR == PDC_IO_fopen(pdc, "/dev/stdin")) {
    error(ERROR_FATAL, "\n*** PDC_IO_fopen failed ***");
  }

  /* read all the records */
  while (1) {
    if (PDC_ERR == PDCI_IO_need_rec_bytes(pdc, skip_rec, &begin, &end, &eor, &eof, &skipped_bytes)) {
      error(0, "\nXXX_REMOVE need_rec_bytes returned PDC_ERR, break from while loop");
      break;
    }
    skip_rec = 1;
    rec_len = end-begin;
    error(0, "\nXXX_REMOVE rec_len = %ld  skipped_bytes = %ld   eor = %d   eof = %d",
	  (long)rec_len, (long)skipped_bytes, eor, eof);
    if (rec_len) {
      error(0, "XXX_REMOVE: do something with bytes = [%.*s]", rec_len, begin);
    }
    if (eof) {
      error(0, "\nXXX_REMOVE need_rec_bytes returned PDC_ERR, break from while loop");
      break;
    }
  }
  /* done */

  if (PDC_ERR == PDC_IO_close(pdc)) {
    error(ERROR_FATAL, "\n*** PDC_IO_close failed ***");
  }

  if (PDC_ERR == PDC_close(pdc)) {
    error(ERROR_FATAL, "\n*** PDC_close failed ***");
  }

  return 0;

 usage:
  error(0, "\nUsage: cmonster iodisc cookie\n");
  error(0, "    See cmdline.p for details\n");
  return -1;
}
