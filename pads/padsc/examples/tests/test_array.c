#include "libpadsc.h"
#include "array.h"

int main(int argc, char** argv) {
  PDC_t*             pdc;
  intList            rep;
  intList_m          m;
  intList_pd         pd;
  intList_acc        accum;



  if (PDC_ERR == PDC_open(&pdc,0,0)) {
    error(ERROR_FATAL, "*** PDC_open failed ***");
  }
  if (PDC_ERR == PDC_IO_fopen(pdc, "../../data/array")) {
    error(ERROR_FATAL, "*** PDC_IO_fopen failed ***");
  }

  error(0, "\ninit everything");
  intList_init(pdc, &rep);
  intList_m_init(pdc, &m, PDC_CheckAndSet);
  intList_pd_init(pdc, &pd);
  intList_acc_init(pdc, &accum);

  /*
   * Try to read each line of data
   */
  while (!PDC_IO_at_EOF(pdc)) {
    error(0, "\ncalling intList_read");
    if (PDC_OK == intList_read(pdc, &m, &pd, &rep)) {
      /* do something with the data */
      error(2, "intList_read returned array of length: %d", rep.length);
      if (PDC_ERR == intList_acc_add(pdc, &accum, &pd, &rep)) {
	error(0, "** accum_add failed **");
      }
    } else {
      error(2, "intList_read returned: error");
      if (PDC_ERR == intList_acc_add(pdc, &accum, &pd, &rep)) {
	error(0, "** accum_add failed **");
      }
    }
  }
  error(0, "\nFound eof");
  error(0, "\nDescribe the accum");
  if (PDC_ERR == intList_acc_report(pdc, "top", 0, 0, &accum)) {
    error(0, "** accum_report failed **");
  }

  if (PDC_ERR == PDC_IO_close(pdc)) {
    error(ERROR_FATAL, "*** PDC_IO_close failed ***");
  }

  if (PDC_ERR == PDC_close(pdc)) {
    error(ERROR_FATAL, "*** PDC_close failed ***");
  }

  return 0;
}
