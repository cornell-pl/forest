#include "pads.h"
#include "array-pred.h"

int main(int argc, char** argv) {
  P_t*             pdc;
  entry            rep;
  entry_m          m;
  entry_pd         pd;
  entry_acc        accum;



  if (P_ERR == P_open(&pdc,0,0)) {
    error(ERROR_FATAL, "*** P_open failed ***");
  }
  if (P_ERR == P_io_fopen(pdc, "../../data/array-pred2")) {
    error(ERROR_FATAL, "*** P_io_fopen failed ***");
  }

  error(0, "\ninit everything");
  entry_init(pdc, &rep);
  entry_m_init(pdc, &m, P_CheckAndSet);
  entry_pd_init(pdc, &pd);
  entry_acc_init(pdc, &accum);

  /*
   * Try to read each line of data
   */
  while (!P_io_at_eof(pdc)) {
    error(0, "\ncalling entry_read");
    if (P_OK == entry_read(pdc, &m, &pd, &rep)) {
      /* do something with the data */
      error(2, "entry_read returned array of length: %d", rep.seq.length);
      if (P_ERR == entry_acc_add(pdc, &accum, &pd, &rep)) {
	error(0, "** accum_add failed **");
      }
    } else {
      error(2, "entry_read returned: error");
      if (P_ERR == entry_acc_add(pdc, &accum, &pd, &rep)) {
	error(0, "** accum_add failed **");
      }
    }
  }
  error(0, "\nFound eof");
  error(0, "\nDescribe the accum");
  if (P_ERR == entry_acc_report(pdc, "top", 0, 0, &accum)) {
    error(0, "** accum_report failed **");
  }

  if (P_ERR == P_io_close(pdc)) {
    error(ERROR_FATAL, "*** P_io_close failed ***");
  }

  if (P_ERR == P_close(pdc)) {
    error(ERROR_FATAL, "*** P_close failed ***");
  }

  return 0;
}
