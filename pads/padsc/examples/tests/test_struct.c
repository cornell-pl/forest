#include "padsc.h"
#include "struct.h"
#define FILENAME  "stdin"
// #define FILENAME  "../../data/ex_data.struct"

int main(int argc, char** argv) {
  PDC_t*          pdc;
  testtwo            f1data;
  testtwo_acc        accum;
  testtwo_pd         pd = {0};

  if (PDC_ERR == PDC_open(&pdc,0,0)) {
    error(2, "*** PDC_open failed ***");
    exit(-1);
  }

  if (strcasecmp(FILENAME, "stdin") == 0) {
    error(0, "Data file = standard in\n");
    if (PDC_ERR == PDC_IO_set(pdc, sfstdin)) {
      error(2, "*** PDC_IO_set(sfstdin) failed ***");
      exit(-1);
    }
  } else {
    error(0, "Data file = %s\n", FILENAME);
    if (PDC_ERR == PDC_IO_fopen(pdc, FILENAME)) {
      error(2, "*** PDC_IO_fopen failed ***");
      exit(-1);
    }
  }

  error(0, "\ninit the accum");
  if (PDC_ERR == testtwo_acc_init(pdc, &accum)) {
    error(2, "** init failed **");
    exit(-1);
  }

  /*
   * Try to read each line of data
   */
  while (!PDC_IO_at_EOF(pdc)) {
    error(0, "\ncalling testtwo_read");
    if (PDC_OK == testtwo_read(pdc, 0, &pd, &f1data)) {
      /* do something with the data */
      error(2, "testtwo_read returned: id %d  ts %d  f %d ", f1data.header.id, f1data.header.ts, f1data.f);
      if (PDC_ERR == testtwo_acc_add(pdc, &accum, &pd, &f1data)) {
	error(0, "** accum_add failed **");
      }
    } else {
      error(2, "testtwo_read returned: error");
      if (PDC_ERR == testtwo_acc_add(pdc, &accum, &pd, &f1data)) {
	error(0, "** accum_add failed **");
      }
    }
  }
  error(0, "\nFound eof");
  error(0, "\nDescribe the accum");

  if (PDC_ERR == testtwo_acc_report(pdc, "", 0, 0, &accum)) {
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
