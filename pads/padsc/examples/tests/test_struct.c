#include "pads.h"
#include "struct.h"
#define FILENAME  "stdin"
// #define FILENAME  "../../data/ex_data.struct"

int main(int argc, char** argv) {
  P_t*          pads;
  testtwo            f1data;
  testtwo_acc        accum;
  testtwo_pd         pd = {0};

  if (P_ERR == P_open(&pads,0,0)) {
    error(2, "*** P_open failed ***");
    exit(-1);
  }

  if (strcasecmp(FILENAME, "stdin") == 0) {
    error(0, "Data file = standard in\n");
    if (P_ERR == P_io_set(pads, sfstdin)) {
      error(2, "*** P_io_set(sfstdin) failed ***");
      exit(-1);
    }
  } else {
    error(0, "Data file = %s\n", FILENAME);
    if (P_ERR == P_io_fopen(pads, FILENAME)) {
      error(2, "*** P_io_fopen failed ***");
      exit(-1);
    }
  }

  error(0, "\ninit the accum");
  if (P_ERR == testtwo_acc_init(pads, &accum)) {
    error(2, "** init failed **");
    exit(-1);
  }

  /*
   * Try to read each line of data
   */
  while (!P_io_at_eof(pads)) {
    error(0, "\ncalling testtwo_read");
    if (P_OK == testtwo_read(pads, 0, &pd, &f1data)) {
      /* do something with the data */
      error(2, "testtwo_read returned: id %d  ts %d  f %d ", f1data.header.id, f1data.header.ts, f1data.f);
      if (P_ERR == testtwo_acc_add(pads, &accum, &pd, &f1data)) {
	error(0, "** accum_add failed **");
      }
    } else {
      error(2, "testtwo_read returned: error");
      if (P_ERR == testtwo_acc_add(pads, &accum, &pd, &f1data)) {
	error(0, "** accum_add failed **");
      }
    }
  }
  error(0, "\nFound eof");
  error(0, "\nDescribe the accum");

  if (P_ERR == testtwo_acc_report(pads, "", 0, 0, &accum)) {
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
