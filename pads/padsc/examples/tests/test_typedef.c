#include "pads.h"
#include "typedef.h"

int main(int argc, char** argv) {
  P_t*          pads;
  pn_t            f1data;
  pn_t_acc        accum;
  pn_t_pd         pd = {0};

  if (P_ERR == P_open(&pads,0,0)) {
    error(2, "*** P_open failed ***");
    exit(-1);
  }
  if (P_ERR == P_io_fopen(pads, "../../data/typedef")) {
    error(2, "*** P_io_fopen failed ***");
    exit(-1);
  }

  error(0, "\ninit the accum");
  if (P_ERR == pn_t_acc_init(pads, &accum)) {
    error(2, "** init failed **");
    exit(-1);
  }

  /*
   * Try to read each line of data
   */
  while (!P_io_at_eof(pads)) {
    error(0, "\ncalling pn_t_read");
    if (P_OK == pn_t_read(pads, 0, 0, 9999999999LL, &pd, &f1data)) {
      /* do something with the data */
      error(2, "pn_t_read returned: id %lld", f1data);
      if (P_ERR == pn_t_acc_add(pads, &accum, &pd, &f1data)) {
	error(0, "** accum_add failed **");
      }
    } else {
      error(2, "pn_t_read returned: error");
      if (P_ERR == pn_t_acc_add(pads, &accum, &pd, &f1data)) {
	error(0, "** accum_add failed **");
      }
    }
  }
  error(0, "\nFound eof");
  error(0, "\nDescribe the accum");
  if (P_ERR == pn_t_acc_report(pads, "top", 0, 0, &accum)) {
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
