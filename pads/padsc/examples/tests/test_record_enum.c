#include "pads.h"
#include "record_enum.h"

int main(int argc, char** argv) {
  P_t*          pads;
  orderStates            f1data;
  orderStates_acc        accum;
  orderStates_pd         pd = {0};

  if (P_ERR == P_open(&pads,0,0)) {
    error(2, "*** P_open failed ***");
    exit(-1);
  }
  if (P_ERR == P_io_fopen(pads, "../../data/record_enum")) {
    error(2, "*** P_io_fopen failed ***");
    exit(-1);
  }

  error(0, "\ninit the accum");
  if (P_ERR == orderStates_acc_init(pads, &accum)) {
    error(2, "** init failed **");
    exit(-1);
  }

  /*
   * Try to read each line of data
   */
  while (!P_io_at_eof(pads)) {
    error(0, "\ncalling orderStates_read");
    if (P_OK == orderStates_read(pads, 0, &pd, &f1data)) {
      /* do something with the data */
      error(2, "orderStates_read returned: id %s", orderStates2str(f1data));
      if (P_ERR == orderStates_acc_add(pads, &accum, &pd, &f1data)) {
	error(0, "** accum_add failed **");
      }
    } else {
      error(2, "orderStates_read returned: error");
      if (P_ERR == orderStates_acc_add(pads, &accum, &pd, &f1data)) {
	error(0, "** accum_add failed **");
      }
    }
  }
  error(0, "\nFound eof");
  error(0, "\nDescribe the accum");
  if (P_ERR == orderStates_acc_report(pads, "top", 0, 0, &accum)) {
    error(0, "** accum_report failed **");
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
