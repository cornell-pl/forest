#include "pads.h"
#include "format1.h"


int main(int argc, char** argv) {
  P_t*          pads;
  Pdisc_t      mydisc = Pdefault_disc;
  test            f1data;
  test_acc        accum;
  test_pd         pd ;
  test_m          m;

  mydisc.flags |= P_WSPACE_OK;

  if (P_ERR == P_open(&pads,&mydisc,0)) {
    error(2, "*** P_open failed ***");
    exit(-1);
  }
  if (P_ERR == P_io_fopen(pads, "../../data/ex_data.format1")) {
    error(2, "*** P_io_fopen failed ***");
    exit(-1);
  }

  test_init(pads, &f1data);
  test_pd_init(pads, &pd);
  /* init mask -- must do this! */
  test_m_init(pads, &m, P_CheckAndSet);

  error(0, "\ninit the accum");
  if (P_ERR == test_acc_init(pads, &accum)) {
    error(2, "** init failed **");
    exit(-1);
  }

  /*
   * Try to read each line of data
   */
  while (!P_io_at_eof(pads)) {
    error(0, "\ncalling test_read");
    if (P_OK == test_read(pads, &m, &pd, &f1data)) {
      /* do something with the data */
      error(2, "test_read returned: id %d  ts %d", f1data.id, f1data.ts);
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

  if (P_ERR == test_acc_report(pads, "entire struct", 0, 0, &accum)) {
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
