#include "libpadsc.h"
#include "readinttest.h"

int main(int argc, char** argv) {
  PDC_t*          pdc;
  record          r;

  if (PDC_ERROR == PDC_open(0, &pdc)) {
    error(2, "*** PDC_open failed ***");
    exit(-1);
  }
  if (PDC_ERROR == PDC_IO_fopen(pdc, "../ex_data.readinttest", 0)) {
    error(2, "*** PDC_IO_fopen failed ***");
    exit(-1);
  }

  /*
   * Try to read each line of data
   */
  while (!PDC_IO_peek_EOF(pdc, 0)) {
    if (PDC_OK == record_read(pdc, 0, 0, &r, 0)) {
      /* do something with the data */
    } else {
      error(2, "record_read returned: error");
    }
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
