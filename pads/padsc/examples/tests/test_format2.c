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
  PDC_base_ed     ed = {0};

  if (PDC_ERR == PDC_open(&pdc, 0, 0)) {
    error(2, "*** PDC_open failed ***");
    exit(-1);
  }
  if (PDC_ERR == PDC_IO_fopen(pdc, "../data/ex_data.format2")) {
    error(2, "*** PDC_IO_fopen failed ***");
    exit(-1);
  }

  /*
   * XXX Process the data here XXX
   */
  while (!PDC_IO_at_EOF(pdc)) {
    ctr = 0;
    while (PDC_OK == PDCI_char_lit_scan(pdc, '|', '|', 1, &c, &n)) {
      ctr++;
      if (PDC_OK == PDC_char_lit_read(pdc, 0, &ed, 'a')) {
	error(2, "found an 'a' after a vbar");
      }
    }
    error(2, "Found %d vertical bars on line", ctr);
    if (PDC_ERR == PDCI_char_lit_scan(pdc, '\n', '\n', 1, 0, 0)) {
      error(2, "Could not find newline, ending program");
      break;
    }
  }

  if (PDC_ERR == PDC_IO_fclose(pdc)) {
    error(2, "*** PDC_IO_fclose failed ***");
    exit(-1);
  }

  if (PDC_ERR == PDC_close(pdc)) {
    error(2, "*** PDC_close failed ***");
    exit(-1);
  }

  return 0;
}
