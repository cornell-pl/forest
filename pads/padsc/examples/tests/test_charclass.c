#include "pads.h"
#include "charclass.h"


int main(int argc, char** argv) {
  P_t*            pads;
  Pdisc_t         mydisc = Pdefault_disc;
  entry            f1data;
  entry_acc        accum;
  entry_pd         pd ;
  entry_m          m;

  mydisc.flags |= P_WSPACE_OK;

  if (P_ERR == P_open(&pads,&mydisc,0)) {
    error(2, "*** P_open failed ***");
    exit(-1);
  }
  if (P_ERR == P_io_fopen(pads, "../../data/charclass")) {
    error(2, "*** P_io_fopen failed ***");
    exit(-1);
  }

  entry_init(pads, &f1data);
  entry_pd_init(pads, &pd);
  /* init mask -- must do this! */
  entry_m_init(pads, &m, P_CheckAndSet);

  error(0, "\ninit the accum");
  if (P_ERR == entry_acc_init(pads, &accum)) {
    error(2, "** init failed **");
    exit(-1);
  }

  /*
   * Try to read each line of data
   */
  while (!P_io_at_eof(pads)) {
    error(0, "\ncalling entry_read");
    if (P_OK == entry_read(pads, &m, &pd, &f1data)) {
      /* do something with the data */
      if (P_ERR == entry_acc_add(pads, &accum, &pd, &f1data)) {
	error(0, "** accum_add failed **");
      }
    } else {
      error(2, "entry_read returned: error");
      if (P_ERR == entry_acc_add(pads, &accum, &pd, &f1data)) {
	error(0, "** accum_add failed **");
      }
    }
  }
  error(0, "\nFound eof");

  if (P_ERR == entry_acc_report(pads, "entire struct", 0, 0, &accum)) {
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
