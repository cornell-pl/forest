#include "pads.h"
#include "struct2.h"

int main(int argc, char** argv) {
  P_t*          pads;
  test            f1data;
  test_acc        accum;
  test_pd         pd = {0};

  if (P_ERR == P_open(&pads,0,0)) {
    error(2, "*** P_open failed ***");
    exit(-1);
  }
  if (P_ERR == P_io_fopen(pads, "../../data/ex_data.struct2")) {
    error(2, "*** P_io_fopen failed ***");
    exit(-1);
  }

  error(0, "\ninit the accum");
  if (P_ERR == test_acc_init(pads, &accum)) {
    error(2, "** init failed **");
    exit(-1);
  }

  error(0, "\ninit the rep");
  if (P_ERR == test_init(pads, &f1data)) {
    error(2, "** init rep failed **");
    exit(-1);
  }

  error(0, "\ninit the ed");
  if (P_ERR == test_pd_init(pads, &pd)) {
    error(2, "** init rep failed **");
    exit(-1);
  }

  /*
   * Try to read each line of data
   */
  while (!P_io_at_eof(pads)) {
    error(0, "\ncalling test_read");
    if (P_OK == test_read(pads, 0, &pd, &f1data)) {
      /* do something with the data */
      error(2, "test_read returned: id %d  ts %d  f %d ", f1data.id, f1data.ts, f1data.f);
      if (P_ERR == test_acc_add(pads, &accum, &pd, &f1data)) {
	error(0, "** accum_add failed **");
      }
    } else {
      error(2, "test_read returned: error");
      if (P_ERR == test_acc_add(pads, &accum, &pd, &f1data)) {
	error(0, "** accum_add failed **");
      }
    }
  }
  error(0, "\nFound eof");
  error(0, "\nDescribe the accum");

  if (P_ERR == test_acc_report(pads, "", 0, 0, &accum)) {
    error(0, "** accum_report failed **");
  }

  if (P_ERR == test_cleanup(pads, &f1data)) {
    error(0, "** test_cleanup failed **");
  }

  if (P_ERR == test_pd_cleanup(pads, &pd)) {
    error(0, "** test_pd_cleanup failed **");
  }

  if (P_ERR == test_acc_cleanup(pads, &accum)) {
    error(0, "** test_pd_cleanup failed **");
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
