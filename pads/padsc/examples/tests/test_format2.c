#include "padsc-internal.h" /* for testing - normally do not include internal */

#if 0
#include "padsc.h"
#include "format2.h"
#endif

int main(int argc, char** argv) {
  PDC_IO_disc_t   *io_disc;
  int             ctr;
  size_t          n;
  unsigned char   c;
  PDC_t*          pdc;
  PDC_base_pd     pd = {0};
  PDC_base_m      m  = PDC_CheckAndSet;

  if (!(io_disc = PDC_norec_make(0))) {
    error(2, "*** PDC_norec_make failed ***");
    return -1;
  } 

  if (PDC_ERR == PDC_open(&pdc, 0, io_disc)) {
    error(2, "*** PDC_open failed ***");
    return -1;
  }
  if (PDC_ERR == PDC_IO_fopen(pdc, "../../data/ex_data.format2")) {
    error(2, "*** PDC_IO_fopen failed ***");
    return -1;
  }

  /*
   * XXX Process the data here XXX
   */
  ctr = 0;
  while (!PDC_IO_at_EOF(pdc)) {
    if (PDC_OK == PDC_a_char_lit_scan(pdc, '|', '\n', 1, 1, &c, &n)) {
      if (c == '|') { 
	ctr++;
	PDC_IO_checkpoint(pdc, 1);
	if (PDC_OK == PDC_a_char_lit_read(pdc, &m, &pd, 'a')) {
	  error(2, "found an 'a' after a vbar");
	}
	PDC_IO_commit(pdc);
      } else { /* found newline */
	error(2, "Found %d vertical bars on line", ctr);
	ctr = 0;
      }
      continue;
    }
    break;    /* found neither vbar or nl */
  }

  if (ctr) {
    error(2, "Found %d vertical bars on line", ctr);
  }
  error(2, "Could not find newline, ending program");

  if (PDC_ERR == PDC_IO_close(pdc)) {
    error(2, "*** PDC_IO_close failed ***");
    return -1;
  }

  if (PDC_ERR == PDC_close(pdc)) {
    error(2, "*** PDC_close failed ***");
    return -1;
  }

  return 0;
}
