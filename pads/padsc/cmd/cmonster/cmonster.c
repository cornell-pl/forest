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
  PDC_t*          pdc;
  PDC_disc_t      my_disc;
  PDC_IO_disc_t*  io_disc;

  /* Parse command line */

  io_disc = PDC_norec_make(0);
  if (!io_disc) {
    error(ERROR_FATAL, "\nFailed to install IO discipline to parse command line");
  }

  my_disc = PDC_default_disc;
  my_disc.flags |= (PDC_flags_t)PDC_WSPACE_OK;

  if (PDC_ERR == PDC_open(&pdc, &my_disc, io_disc)) {
    error(ERROR_FATAL, "*** PDC_open failed ***");
  }
  if (PDC_ERR == PDC_IO_fopen(pdc, "/dev/stdin")) {
    error(ERROR_FATAL, "*** PDC_IO_fopen failed ***");
  }

  return 0;
}
