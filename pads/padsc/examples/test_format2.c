#include "libpadsc-internal.h" /* for testing - normally do not include internal */

#if 0
#include "libpadsc.h"
#include "format2.h"
#endif

int main(int argc, char** argv) {
  int             ctr;
  size_t          n;
  unsigned char   c;
  PDC_t*          pdc;
  PDC_base_em     em = {0};
  PDC_base_ed     ed = {0};

  if (PDC_ERR == PDC_open(0, &pdc)) {
    error(2, "*** PDC_open failed ***");
    exit(-1);
  }
  if (PDC_ERR == PDC_IO_fopen(pdc, "../ex_data.format2", 0)) {
    error(2, "*** PDC_IO_fopen failed ***");
    exit(-1);
  }

  /*
   * XXX Process the data here XXX
   */
  while (!PDC_IO_peek_EOF(pdc, 0)) {
    ctr = 0;
    while (PDC_OK == PDCI_char_lit_scan(pdc, '|', '|', &c, &n, 0)) {
      ctr++;
      if (PDC_OK == PDC_char_lit_read(pdc, &em, &ed, 'a', 0)) {
	error(2, "found an 'a' after a vbar");
      }
    }
    error(2, "Found %d vertical bars on line", ctr);
    if (PDC_ERR == PDCI_char_lit_scan(pdc, '\n', '\n', 0, 0, 0)) {
      error(2, "Could not find newline, ending program");
      break;
    }
    /* PDC_IO_refill(pdc, 0); */
  }

  if (PDC_ERR == PDC_IO_fclose(pdc, 0)) {
    error(2, "*** PDC_IO_fclose failed ***");
    exit(-1);
  }

  if (PDC_ERR == PDC_close(pdc, 0)) {
    error(2, "*** PDC_close failed ***");
    exit(-1);
  }

  return 0;
}
