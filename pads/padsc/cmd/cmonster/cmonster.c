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
#include "helpers.h"

int main(int argc, char **argv)
{
  CM_t                    cm;

  PDC_t                  *pdc;
  PDC_disc_t              my_disc;
  PDC_IO_disc_t          *io_disc;

  CM_ispec                ispec;
  CM_ispec_m              ispec_m;
  CM_ispec_pd             ispec_pd;

  CM_cspec               cspec;
  CM_cspec_m             cspec_m;
  CM_cspec_pd            cspec_pd;

  Sfio_t                 *io;
  size_t                  skipped_bytes = 0, rec_len;
  int                     eor = 0, eof = 0, skip_rec = 0;
  PDC_byte               *begin = 0, *end = 0;

  if (argc == 2 && ((strcmp(argv[1], "-h") == 0) || (strcmp(argv[1], "--help") == 0))) goto detailed_usage;
  if (argc != 3) goto usage;

  /* Parse command line */

  io_disc = PDC_norec_make(0);
  if (!io_disc) {
    error(ERROR_FATAL, "\n*** FATAL: Failed to install IO discipline to parse command line\n");
  }

  my_disc = PDC_default_disc;
  my_disc.flags |= (PDC_flags_t)PDC_WSPACE_OK;

  if (PDC_ERR == PDC_open(&pdc, &my_disc, io_disc)) {
    error(ERROR_FATAL, "\n*** FATAL: PDC_open failed\n");
  }

  CM_ispec_init(pdc, &ispec);
  CM_ispec_pd_init(pdc, &ispec_pd);
  CM_ispec_m_init(pdc, &ispec_m, PDC_CheckAndSet);

  CM_cspec_init(pdc, &cspec);
  CM_cspec_pd_init(pdc, &cspec_pd);
  CM_cspec_m_init(pdc, &cspec_m, PDC_CheckAndSet);

  if (!(io = sfopen(NULL, argv[1], "s"))) {
    error(ERROR_FATAL, "\n*** FATAL: Unexpected: sfopen(NULL, argv[1], \"s\") failed\n");
  }
  if (PDC_ERR == PDC_IO_set(pdc, io)) {
    error(ERROR_FATAL, "\n*** FATAL: PDC_IO_set failed\n");
  }
  if (PDC_ERR == CM_ispec_read(pdc, &ispec_m, &ispec_pd, &ispec)) {
    error(0, "\nCould not parse arg1 io_spec");
    goto usage;
  }
  if (PDC_ERR == PDC_IO_close(pdc)) {
    error(ERROR_FATAL, "\n*** FATAL: PDC_IO_close failed\n");
  }
  sfclose(io);

  if (!(io = sfopen(NULL, argv[2], "s"))) {
    error(ERROR_FATAL, "\n*** FATAL: Unexpected: sfopen(NULL, argv[2], \"s\") failed\n");
  }
  if (PDC_ERR == PDC_IO_set(pdc, io)) {
    error(ERROR_FATAL, "\n*** FATAL: PDC_IO_set failed\n");
  }
  if (PDC_ERR == CM_cspec_read(pdc, &cspec_m, &cspec_pd, &cspec)) {
    error(0, "\nCould not parse arg2 cookie");
    goto usage;
  }
  if (PDC_ERR == PDC_IO_close(pdc)) {
    error(ERROR_FATAL, "\n*** FATAL: PDC_IO_close failed\n");
  }
  sfclose(io);
  if (PDC_ERR == PDC_close(pdc)) {
    error(ERROR_FATAL, "\n*** FATAL: PDC_close failed\n");
  }
  error(0, "XXX_REMOVE description of cookie");
  describe_cookie(&cspec);


  /* initialize cm */
  if (!(cm.vm = vmopen(Vmdcheap, Vmbest, 0))) {
    error(ERROR_FATAL, "\n*** FATAL: vmopen failed\n");
  }
  cm.outbuf_sz = out_sz_cookie(&cspec);
  if (!(cm.outbuf = vmoldof(cm.vm, 0, PDC_byte, cm.outbuf_sz, 1))) {
    error(ERROR_FATAL, "\n*** FATAL: Memory alloc failed\n");
  }

  if (-1 == CM_open_iodisc(&ispec, &io_disc)) goto usage;

  if (!io_disc) {
    error(ERROR_FATAL, "\n*** FATAL: Failed to install IO discipline %s\n", argv[1]);
  } else {
    error(0, "\nXXX_REMOVE Installed IO discipline %s", argv[1]);
  }

  if (PDC_ERR == PDC_open(&pdc, &my_disc, io_disc)) {
    error(ERROR_FATAL, "\n*** FATAL: PDC_open failed\n");
  }
  if (PDC_ERR == PDC_IO_fopen(pdc, "/dev/stdin")) {
    error(ERROR_FATAL, "\n*** FATAL: PDC_IO_fopen failed\n");
  }

  /* read all the records */
  while (1) {
    if (PDC_ERR == PDCI_IO_need_rec_bytes(pdc, skip_rec, &begin, &end, &eor, &eof, &skipped_bytes)) {
      error(0, "\nXXX_REMOVE need_rec_bytes returned PDC_ERR, break from while loop");
      break;
    }
    skip_rec = 1;
    rec_len = end-begin;
    cm.cursor = cm.outbuf;
    error(0, "\nXXX_REMOVE rec_len = %ld  skipped_bytes = %ld   eor = %d   eof = %d",
	  (long)rec_len, (long)skipped_bytes, eor, eof);
    if (rec_len) {
      // error(0, "XXX_REMOVE: do something with bytes = [%.*s]", rec_len, begin);
      switch (cspec.cookie.tag) {
      case CM_c_or_s_err:
	/* never happens */
	return -1;
      case c_cookie:
	if (PDC_ERR == rw_c_cookie(&cm, &(cspec.cookie.val.c_cookie), begin, end)) {
	  /* already reported error */
	  return -1;
	}
	break;
      case s_cookie:
	if (PDC_ERR == rw_s_cookie(&cm, &(cspec.cookie.val.s_cookie), begin, end)) {
	  /* already reported error */
	  return -1;
	}
	break;
      }
    }
    if (eof) {
      error(0, "\nXXX_REMOVE need_rec_bytes returned PDC_ERR, break from while loop");
      break;
    }
  }
  /* done */

  /* cleanup cm */
  if (cm.vm) {
    vmclose(cm.vm); /* frees everything alloc'd using cm.vm */
  }

  if (PDC_ERR == PDC_IO_close(pdc)) {
    error(ERROR_FATAL, "\n*** FATAL: PDC_IO_close failed\n");
  }

  if (PDC_ERR == PDC_close(pdc)) {
    error(ERROR_FATAL, "\n*** FATAL: PDC_close failed\n");
  }

  return 0;

 usage:
  error(0, USAGE);
  return -1;

 detailed_usage:
  error(0, DETAILED_USAGE);
  return -1;
}
