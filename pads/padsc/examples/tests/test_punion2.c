#include "libpadsc.h"
#include "punion2.h"

int main(int argc, char** argv) {
  PDC_t*          pdc;
  test2            f1data;
  test2_acc        accum;
  test2_pd         pd = {0};

  if (PDC_ERR == PDC_open(&pdc,0,0)) {
    error(2, "*** PDC_open failed ***");
    exit(-1);
  }
  if (PDC_ERR == PDC_IO_fopen(pdc, "../../data/punion2")) {
    error(2, "*** PDC_IO_fopen failed ***");
    exit(-1);
  }

  error(0, "\ninit the accum");
  if (PDC_ERR == test2_acc_init(pdc, &accum)) {
    error(2, "** init failed **");
    exit(-1);
  }

  /*
   * Try to read each line of data
   */
  while (!PDC_IO_at_EOF(pdc)) {
    error(0, "\ncalling test2_read");
    if (PDC_OK == test2_read(pdc, 0, &pd, &f1data)) {
      /* do something with the data */
      error(2, "test2_read returned: id %s", test2_tag2str(f1data.tag));
      if (PDC_ERR == test2_acc_add(pdc, &accum, &pd, &f1data)) {
	error(0, "** accum_add failed **");
      }
    } else {
      error(2, "test2_read returned: error");
      if (PDC_ERR == test2_acc_add(pdc, &accum, &pd, &f1data)) {
	error(0, "** accum_add failed **");
      }
    }
  }
  error(0, "\nFound eof");
  error(0, "\nDescribe the accum");
  if (PDC_ERR == test2_acc_report(pdc, "top", 0, 0, &accum)) {
    error(0, "** accum_report failed **");
  }

  if (PDC_ERR == PDC_IO_close(pdc)) {
    error(2, "*** PDC_IO_close failed ***");
    exit(-1);
  }

  if (PDC_ERR == PDC_close(pdc)) {
    error(2, "*** PDC_close failed ***");
    exit(-1);
  }

  return 0;
}
