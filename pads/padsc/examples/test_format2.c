#include "libpadsc-internal.h" /* for testing - normally do not include internal */

#if 0
#include "libpadsc.h"
#include "format1.h"
#endif

int main(int argc, char** argv) {
  int             ctr;
  PDC_t*          pdc;
  PDC_base_em     em;
  PDC_base_ed     ed;

  if (PDC_ERROR == PDC_open(0, &pdc)) {
    error(2, "*** PDC_open failed ***");
    exit(-1);
  }
  if (PDC_ERROR == PDC_IO_fopen(pdc, "ex_data.format2", 0)) {
    error(2, "*** PDC_IO_fopen failed ***");
    exit(-1);
  }

  /*
   * XXX Process the data here XXX
   */
  while (!PDC_IO_peek_EOF(pdc, 0)) {
    ctr = 0;
    while (PDC_OK == PDC_char_lit_scan(pdc, &em, &ed, '|', 0)) {
      ctr++;
      if (PDC_OK == PDC_char_lit_read(pdc, &em, &ed, 'a', 0)) {
	error(2, "found an 'a' after a vbar");
      }
    }
    error(2, "Found %d vertical bars on line", ctr);
    PDC_IO_refill(pdc, 0);
  }

  if (PDC_ERROR == PDC_IO_fclose(pdc, 0)) {
    error(2, "*** PDC_IO_fclose failed ***");
    exit(-1);
  }

  if (PDC_ERROR == PDC_close(pdc, 0)) {
    error(2, "*** PDC_close failed ***");
    exit(-1);
  }

  return 0;
}
