#include "pads.h"
#include "enum.h"

int main(int argc, char** argv) {
  P_t*          pads;
  orderStates     enumdata;

  if (P_ERR == P_open(&pads, 0, 0)) {
    error(2, "*** P_open failed ***");
    exit(-1);
  }
  if (P_ERR == P_io_fopen(pads, "../../data/ex_data.enum")) {
    error(2, "*** P_io_fopen failed ***");
    exit(-1);
  }

  /*
   * Try to read each line of data
   */
  while (!P_io_at_eof(pads)) {
    if (P_OK == orderStates_read(pads, 0, 0, &enumdata)) {
      /* do something with the data */
      error(2, "orderStates_read returned:  %d", enumdata);
    } else {
      error(2, "orderStates_read returned: error");
    }
  }

  if (P_ERR == P_io_close(pads)) {
    error(2, "*** P_io_close failed ***");
    exit(-1);
  }

  if (P_ERR == P_close(pads)) {
    error(2, "*** P_close failed ***");
    exit(-1);
  }

  return 0;
}
