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

  CMDLINE_iodisc_spec_init(pdc, &ispec);
  CMDLINE_iodisc_spec_pd_init(pdc, &ispec_pd);
  CMDLINE_iodisc_spec_m_init(pdc, &ispec_m, PDC_CheckAndSet);

  CMDLINE_cookie_spec_init(pdc, &cspec);
  CMDLINE_cookie_spec_pd_init(pdc, &cspec_pd);
  CMDLINE_cookie_spec_m_init(pdc, &cspec_m, PDC_CheckAndSet);

  io_disc = PDC_norec_make(0);
  if (!io_disc) {
    error(ERROR_FATAL, "\nFailed to install IO discipline to parse command line");
  }

  my_disc = PDC_default_disc;
  my_disc.flags |= (PDC_flags_t)PDC_WSPACE_OK;

  if (PDC_ERR == PDC_open(&pdc, &my_disc, io_disc)) {
    error(ERROR_FATAL, "\n*** PDC_open failed ***");
  }

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

  io_disc = 0;

  switch (ispec.iodisc) {

  case fwrec:
  case fwrec_noseek: {
    size_t    leader_len, data_len, trailer_len;
    if (ispec.params.length != 3) {
      error(0, "\nio discipline fwrec requires 3 params, e.g., fwrec(:0,24,1:)");
      goto usage;
    }
    leader_len      = ispec.params.elts[0];
    data_len        = ispec.params.elts[1];
    trailer_len     = ispec.params.elts[2];
    io_disc = PDC_fwrec_noseek_make(leader_len, data_len, trailer_len);
    break;
  }

  case nlrec:
  case nlrec_noseek: {
    size_t    block_size_hint;
    if (ispec.params.length != 1) {
      error(0, "\nio discipline nlrec requires 1 params, e.g., nlrec(:0:)");
      goto usage;
    }
    block_size_hint = ispec.params.elts[0];
    io_disc = PDC_nlrec_noseek_make(block_size_hint);
    break;
  }

  case ctrec:
  case ctrec_noseek: {
    PDC_byte  term_char;
    size_t    block_size_hint;
    if (ispec.params.length != 2) {
      error(0, "\nio discipline ctrec requires 2 params, e.g., ctrec(:10,0:) ");
      goto usage;
    }
    term_char       = ispec.params.elts[0];
    block_size_hint = ispec.params.elts[1];
    io_disc = PDC_ctrec_noseek_make(term_char, block_size_hint);
    break;
  }

  case vlrec:
  case vlrec_noseek: {
    int       blocked;
    size_t    avg_rlen_hint;
    if (ispec.params.length != 2) {
      error(0, "\nio discipline vlrec requires 2 params, e.g., vlrec(:0,0:) ");
      goto usage;
    }
    blocked         = ispec.params.elts[0];
    avg_rlen_hint   = ispec.params.elts[1];
    io_disc = PDC_vlrec_noseek_make(blocked, avg_rlen_hint);
    break;
  }
  }

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
