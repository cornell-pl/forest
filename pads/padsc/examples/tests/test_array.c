#include "pads.h"
#include "array.h"

int main(int argc, char** argv) {
  P_t*             pads;
  intList            rep;
  intList_m          m;
  intList_pd         pd;
  intList_acc        accum;



  if (P_ERR == P_open(&pads,0,0)) {
    error(ERROR_FATAL, "*** P_open failed ***");
  }
  if (P_ERR == P_io_fopen(pads, "../../data/array")) {
    error(ERROR_FATAL, "*** P_io_fopen failed ***");
  }

  error(0, "\ninit everything");
  intList_init(pads, &rep);
  intList_m_init(pads, &m, P_CheckAndSet);
  intList_pd_init(pads, &pd);
  intList_acc_init(pads, &accum);

  /*
   * Try to read each line of data
   */
  while (!P_io_at_eof(pads)) {
    error(0, "\ncalling intList_read");
    if (P_OK == intList_read(pads, &m, &pd, &rep)) {
      /* do something with the data */
      error(2, "intList_read returned array of length: %d", rep.length);
      if (P_ERR == intList_acc_add(pads, &accum, &pd, &rep)) {
	error(0, "** accum_add failed **");
      }
    } else {
      error(2, "intList_read returned: error");
      if (P_ERR == intList_acc_add(pads, &accum, &pd, &rep)) {
	error(0, "** accum_add failed **");
      }
    }
  }
  error(0, "\nFound eof");
  error(0, "\nDescribe the accum");
  if (P_ERR == intList_acc_report(pads, "top", 0, 0, &accum)) {
    error(0, "** accum_report failed **");
  }

  if (P_ERR == P_io_close(pads)) {
    error(ERROR_FATAL, "*** P_io_close failed ***");
  }

  if (P_ERR == P_close(pads)) {
    error(ERROR_FATAL, "*** P_close failed ***");
  }

  return 0;
}
