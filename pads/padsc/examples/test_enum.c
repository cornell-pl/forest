#include "libpadsc.h"
#include "enum.h"

int main(int argc, char** argv) {
  PDC_t*          pdc;
  orderStates     enumdata;

  if (PDC_ERR == PDC_open(0, &pdc)) {
    error(2, "*** PDC_open failed ***");
    exit(-1);
  }
  if (PDC_ERR == PDC_IO_fopen(pdc, "../ex_data.enum", 0)) {
    error(2, "*** PDC_IO_fopen failed ***");
    exit(-1);
  }

  /*
   * Try to read each line of data
   */
  while (!PDC_IO_peek_EOF(pdc, 0)) {
    if (PDC_OK == orderStates_read(pdc, 0, 0, &enumdata, 0)) {
      /* do something with the data */
      error(2, "orderStates_read returned:  %d", enumdata);
    } else {
      error(2, "orderStates_read returned: error");
    }
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
