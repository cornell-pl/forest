#include "padsc.h"
#include "enum.h"

int main(int argc, char** argv) {
  PDC_t*          pdc;
  orderStates     enumdata;
  orderStates_m   enumdata_m;
  orderStates_pd  enumdata_pd;

  PDC_open(&pdc, 0, 0);
  if (PDC_ERR == PDC_IO_fopen(pdc, "../../data/ex_data.enum")) {
    error(ERROR_FATAL, "*** PDC_IO_fopen failed ***");
  }

  PDC_INIT_ALL(orderStates, pdc, enumdata, enumdata_m, enumdata_pd, PDC_CheckAndSet);

  /*
   * Try to read each line of data
   */

  while (!PDC_IO_at_EOF(pdc)) {
    if (PDC_OK == orderStates_read(pdc, &enumdata_m, &enumdata_pd, &enumdata)) {
      /* do something with the data */
      error(2, "orderStates_read returned:  %d", enumdata);
    } else {
      error(2, "orderStates_read returned: error");
    }
  }

  PDC_CLEANUP_ALL(orderStates, pdc, enumdata, enumdata_pd);
  PDC_IO_close(pdc);
  PDC_close(pdc);
  return 0;
}
