#include "libpadsc.h"
#include "format1.h"

int main(int argc, char** argv) {
  PDC_t*          pdc;
  test            f1data;
  test_acc        accum;
  test_em         em = {0};
  test_ed         ed = {0};

  if (PDC_ERR == PDC_open(0, &pdc)) {
    error(2, "*** PDC_open failed ***");
    exit(-1);
  }
  if (PDC_ERR == PDC_IO_fopen(pdc, "../ex_data.format1", 0)) {
    error(2, "*** PDC_IO_fopen failed ***");
    exit(-1);
  }

  error(0, "\ninit the accum");
  if (PDC_ERR == test_acc_init(pdc, &accum, 0)) {
    error(2, "** init failed **");
    exit(-1);
  }

  /*
   * Try to read each line of data
   */
  while (!PDC_IO_peek_EOF(pdc, 0)) {
    if (PDC_OK == test_read(pdc, &em, &ed, &f1data, 0)) {
      /* do something with the data */
      error(2, "test_read returned: id %d  ts %d", f1data.id, f1data.ts);
      if (PDC_ERR == test_acc_add(pdc, &accum, &ed, &f1data, 0)) {
	error(0, "** accum_add failed **");
      }
    } else {
      error(2, "test_read returned: error");
    }
  }

  error(0, "\ndescribe the accum");
  if (PDC_ERR == PDC_int32_acc_report(pdc, "id", 0, 0, &(accum.id), 0)) {
    error(0, "** accum_report failed **");
  }
  if (PDC_ERR == PDC_int32_acc_report(pdc, "ts", 0, 0, &(accum.ts), 0)) {
    error(0, "** accum_report failed **");
  }

  if (PDC_ERR == PDC_IO_fclose(pdc, 0)) {
    error(2, "*** PDC_IO_fclose failed ***");
    exit(-1);
  }

  if (PDC_ERR == PDC_close(pdc, 0)) {
    error(2, "*** PDC_close failed ***");
    exit(-1);
  }

  return 0;
}
