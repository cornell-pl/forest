#include "libpadsc.h"
#include "format1.h"

int main(int argc, char** argv) {
  PDC_t*          pdc;
  test            f1data;
  test_acc        accum;
  test_ed         ed = {0};

  if (PDC_ERR == PDC_open(&pdc,0,0)) {
    error(2, "*** PDC_open failed ***");
    exit(-1);
  }
  if (PDC_ERR == PDC_IO_fopen(pdc, "../data/ex_data.format1")) {
    error(2, "*** PDC_IO_fopen failed ***");
    exit(-1);
  }

  error(0, "\ninit the accum");
  if (PDC_ERR == test_acc_init(pdc, &accum)) {
    error(2, "** init failed **");
    exit(-1);
  }

  /*
   * Try to read each line of data
   */
  while (!PDC_IO_at_EOF(pdc)) {
    error(0, "\ncalling test_read");
    if (PDC_OK == test_read(pdc, 0, &ed, &f1data)) {
      /* do something with the data */
      error(2, "test_read returned: id %d  ts %d", f1data.id, f1data.ts);
      if (PDC_ERR == test_acc_add(pdc, &accum, &ed, &f1data)) {
	error(0, "** accum_add failed **");
      }
    } else {
      error(2, "test_read returned: error");
      if (PDC_ERR == test_acc_add(pdc, &accum, &ed, &f1data)) {
	error(0, "** accum_add failed **");
      }
    }
  }
  error(0, "\nFound eof");

  if (PDC_ERR == test_acc_report(pdc, "entire struct", 0, 0, &accum)) {
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
