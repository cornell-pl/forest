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

int main(int argc, char** argv) {
  PDC_t*                  pdc;
  PDC_disc_t              my_disc;
  PDC_IO_disc_t*          io_disc;
  CMDLINE_iodisc_spec     ispec;
  CMDLINE_iodisc_spec_m   ispec_m;
  CMDLINE_iodisc_spec_pd  ispec_pd;

  CMDLINE_cookie_spec     cspec;
  CMDLINE_cookie_spec_m   cspec_m;
  CMDLINE_cookie_spec_pd  cspec_pd;

  Sfio_t                 *io;
  size_t                  bytes_skipped;

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

  CMDLINE_iodisc_spec_init(pdc, &ispec);
  CMDLINE_iodisc_spec_pd_init(pdc, &ispec_pd);
  CMDLINE_iodisc_spec_m_init(pdc, &ispec_m, PDC_CheckAndSet);

  CMDLINE_cookie_spec_init(pdc, &cspec);
  CMDLINE_cookie_spec_pd_init(pdc, &cspec_pd);
  CMDLINE_cookie_spec_m_init(pdc, &cspec_m, PDC_CheckAndSet);

  if (!(io = sfopen(NULL, argv[1], "s"))) {
    error(ERROR_FATAL, "\nXXX unexpected: sfopen(NULL, argv[1], \"s\") failed.");
  }
  if (PDC_ERR == PDC_IO_set(pdc, io)) {
    error(ERROR_FATAL, "\n*** PDC_IO_set failed ***");
  }
  if (PDC_ERR == CMDLINE_iodisc_spec_read(pdc, &ispec_m, &ispec_pd, &ispec)) {
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
  if (PDC_ERR == CMDLINE_cookie_spec_read(pdc, &cspec_m, &cspec_pd, &cspec)) {
    error(0, "\nCould not parse arg2 cookie_spec");
    goto usage;
  }
  if (PDC_ERR == PDC_IO_close(pdc)) {
    error(ERROR_FATAL, "\n*** PDC_IO_close failed ***");
  }
  sfclose(io);
  if (PDC_ERR == PDC_close(pdc)) {
    error(ERROR_FATAL, "\n*** PDC_close failed ***");
  }

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
    if (PDC_IO_at_EOF(pdc)) {
      error(0, "\nXXX_REMOVE Main program found eof");
      break;
    }
    if (PDC_ERR == PDC_IO_next_rec(pdc, &bytes_skipped)) {
      error(0, "\nXXX_REMOVE Could not find EOR (newline), ending program");
      break;
    }
    error(0, "\nXXX_REMOVE bytes_skipped = %ld", (long)bytes_skipped);
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
  error(0, "\nUsage: cmonster io_spec cookie_spec\n");
  return -1;
}
