#include "libpadsc.h"
#include "array.h"

int main(int argc, char** argv) {
  PDC_t*           pdc;
  intList            f1data = {0,0,0};
  intList_acc        accum;
  intList_ed         ed = {0};

  if (PDC_ERR == PDC_open(&pdc,0,0)) {
    error(2, "*** PDC_open failed ***");
    exit(-1);
  }
  if (PDC_ERR == PDC_IO_fopen(pdc, "../data/array")) {
    error(2, "*** PDC_IO_fopen failed ***");
    exit(-1);
  }

  error(0, "\ninit the accum");
  if (PDC_ERR == intList_acc_init(pdc, &accum)) {
    error(2, "** init failed **");
    exit(-1);
  }

  /*
   * Try to read each line of data
   */
  while (!PDC_IO_at_EOF(pdc)) {
    error(0, "\ncalling intList_read");
    if (PDC_OK == intList_read(pdc, 0, &ed, &f1data)) {
      /* do something with the data */
      error(2, "intList_read returned array of length: %d", f1data.length);
      if (PDC_ERR == intList_acc_add(pdc, &accum, &ed, &f1data)) {
	error(0, "** accum_add failed **");
      }
    } else {
      error(2, "intList_read returned: error");
      if (PDC_ERR == intList_acc_add(pdc, &accum, &ed, &f1data)) {
	error(0, "** accum_add failed **");
      }
    }
  }
  error(0, "\nFound eof");
  error(0, "\nDescribe the accum");
  if (PDC_ERR == intList_acc_report(pdc, "top", 0, 0, &accum)) {
    error(0, "** accum_report failed **");
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
