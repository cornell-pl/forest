#include "libpadsc.h"
#include "typedef.h"

int main(int argc, char** argv) {
  PDC_t*          pdc;
  pn_t            f1data;
  pn_t_acc        accum;
  pn_t_ed         ed = {0};

  if (PDC_ERR == PDC_open(&pdc,0,0)) {
    error(2, "*** PDC_open failed ***");
    exit(-1);
  }
  if (PDC_ERR == PDC_IO_fopen(pdc, "../../data/typedef")) {
    error(2, "*** PDC_IO_fopen failed ***");
    exit(-1);
  }

  error(0, "\ninit the accum");
  if (PDC_ERR == pn_t_acc_init(pdc, &accum)) {
    error(2, "** init failed **");
    exit(-1);
  }

  /*
   * Try to read each line of data
   */
  while (!PDC_IO_at_EOF(pdc)) {
    error(0, "\ncalling pn_t_read");
    if (PDC_OK == pn_t_read(pdc, 0, 0, 9999999999LL, &ed, &f1data)) {
      /* do something with the data */
      error(2, "pn_t_read returned: id %lld", f1data);
      if (PDC_ERR == pn_t_acc_add(pdc, &accum, &ed, &f1data)) {
	error(0, "** accum_add failed **");
      }
    } else {
      error(2, "pn_t_read returned: error");
      if (PDC_ERR == pn_t_acc_add(pdc, &accum, &ed, &f1data)) {
	error(0, "** accum_add failed **");
      }
    }
  }
  error(0, "\nFound eof");
  error(0, "\nDescribe the accum");
  if (PDC_ERR == pn_t_acc_report(pdc, "top", 0, 0, &accum)) {
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
