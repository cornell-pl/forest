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

  CM_cspec                cspec;
  CM_cspec_m              cspec_m;
  CM_cspec_pd             cspec_pd;

  Sfio_t                 *io;
  size_t                  skipped_bytes = 0, rec_len;
  int                     eor = 0, eof = 0, skip_rec = 0;
  PDC_byte               *begin = 0, *end = 0;

  PDC_uint32              cmon;
  PDC_base_pd             dummy_pd;

  int                     buf_full = 0;

  PDC_base_pd_init(&dummy_pd);

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

  /* initialize cm */
  cm.pdc = pdc;
  if (!(cm.vm = vmopen(Vmdcheap, Vmbest, 0))) {
    error(ERROR_FATAL, "\n*** FATAL: vmopen failed\n");
  }
  cm.outbuf_sz = out_sz_cookie(&cspec);
  if (cm.outbuf_sz < 4) {
    cm.outbuf_sz = 4;
  }
  if (!(cm.outbuf = vmoldof(cm.vm, 0, PDC_byte, cm.outbuf_sz, 1))) {
    error(ERROR_FATAL, "\n*** FATAL: Memory alloc failed\n");
  }
  cm.outbuf_end = cm.outbuf + cm.outbuf_sz;

  /*
   * Construct and emit 2 special header records of size cm.outbuf_sz
   * (all records have this size)
   *
   * rec1: first 4 bytes contain cm.outbuf_sz encoded in SBL
   *      
   * rec2: first 4 bytes contain the 'cmon' numeric value
   *          = ('c' << 24) + ('m' << 16) + ('o' << 8) + 'n';
   *      encoded in SBL
   * In each case any bytes after byte 4 is zero.
   */

  /* zero the output record before filling it in */
  memset(cm.outbuf, 0, cm.outbuf_sz);
  /* fill in cm.outbuf_sz */
  if (4 != PDC_sbl_uint32_write2buf(pdc, cm.outbuf, cm.outbuf_sz, &buf_full, 4, &dummy_pd, &cm.outbuf_sz)) {
    error(ERROR_FATAL, "\n*** FATAL: Unexpected error calling PDC_sbl_uint32_write2buf");
  }
  /* write to stdout */
  if (cm.outbuf_sz != sfwrite(sfstdout, cm.outbuf, cm.outbuf_sz)) {
    error(ERROR_FATAL, "sfwrite error, failed to write %lu bytes", (unsigned long)cm.outbuf_sz);
  }

  /* zero the output record before filling it in */
  memset(cm.outbuf, 0, cm.outbuf_sz);
  /* fill in special cmon value */
  cmon = ('c' << 24) + ('m' << 16) + ('o' << 8) + 'n';
  error(0, "XXX_REMOVE cmon = %lu", (unsigned long)cmon);
  if (4 != PDC_sbl_uint32_write2buf(pdc, cm.outbuf, cm.outbuf_sz, &buf_full, 4, &dummy_pd, &cmon)) {
    error(ERROR_FATAL, "\n*** FATAL: Unexpected error calling PDC_sbl_uint32_write2buf");
  }
  /* write to stdout */
  if (cm.outbuf_sz != sfwrite(sfstdout, cm.outbuf, cm.outbuf_sz)) {
    error(ERROR_FATAL, "\n*** FATAL: sfwrite error, failed to write %lu bytes", (unsigned long)cm.outbuf_sz);
  }

  /* read all input records, generate output records */
  while (1) {
    if (PDC_ERR == PDCI_IO_need_rec_bytes(pdc, skip_rec, &begin, &end, &eor, &eof, &skipped_bytes)) {
      error(0, "\nXXX_REMOVE need_rec_bytes returned PDC_ERR, break from while loop");
      break;
    }
    skip_rec = 1;
    rec_len = end - begin;
    error(0, "\nXXX_REMOVE rec_len = %ld  skipped_bytes = %ld, eor = %d, eof = %d, bytes = [%.*s]",
	  (long)rec_len, (long)skipped_bytes, eor, eof, rec_len, begin);
    if (rec_len) {
      /* zero the next output record before filling it in */
      memset(cm.outbuf, 0, cm.outbuf_sz+1);
      /* init outbuf_cursor */
      cm.outbuf_cursor = cm.outbuf;
      /* fill in outbuf using cspec */
      switch (cspec.cookie.tag) {
      case CM_c_or_s_err:
	/* never happens */
	continue; /* skip to next record without writing to stdout */
      case c_cookie:
	if (PDC_ERR == rw_c_cookie(&cm, &(cspec.cookie.val.c_cookie), begin, end)) {
	  /* already reported error */
	  continue; /* skip to next record without writing to stdout */
	}
	break;
      case s_cookie:
	if (PDC_ERR == rw_s_cookie(&cm, &(cspec.cookie.val.s_cookie), begin, end)) {
	  /* already reported error */
	  continue; /* skip to next record without writing to stdout */
	}
	break;
      }
      /* write outbuf to stdout */
      if (cm.outbuf_sz != sfwrite(sfstdout, cm.outbuf, cm.outbuf_sz)) {
	error(ERROR_FATAL, "sfwrite error, failed to write %lu bytes", (unsigned long)cm.outbuf_sz);
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
