#include "dibbler-notrailer.h"


int main(int argc, char** argv) {
  PDC_t                    *pdc;
  PDC_IO_disc_t            *io_disc;
  out_sum_header           header;
  out_sum_data_line        dline;
  out_sum_data_line_pd     dline_pd;
  out_sum_data_line_m      dline_m;
  out_sum_data_line_acc    acc;
  char                     *fname = "../../data/jun9.out_sum.stream";

  if (argc >= 2 ) {
    fname = argv[1];
  }
  error(0, "\nUsing input file %s", fname);

  io_disc = PDC_nlrec_noseek_make(0);
  if (!io_disc) {
    error(ERROR_FATAL, "\nFailed to install IO discipline nlrec_noseek");
  } else {
    error(0, "\nInstalled IO discipline nlrec_noseek");
  }

  if (PDC_ERR == PDC_open(&pdc, 0, io_disc)) {
    error(2, "*** PDC_open failed ***");
    return -1;
  }

  /* INIT dline -- must do this for all variable data types */
  out_sum_data_line_init(pdc, &dline);
  out_sum_data_line_pd_init(pdc, &dline_pd);
  out_sum_data_line_acc_init(pdc, &acc);
  out_sum_data_line_m_init(pdc, &dline_m, PDC_CheckAndSet);
  if (PDC_ERR == PDC_IO_fopen(pdc, fname)) {
    error(2, "*** PDC_IO_fopen failed ***");
    return -1;
  }

  /*
   * Try to read header
   */

  if (PDC_OK == out_sum_header_read(pdc, 0, 0, &header)) {
    error(0, "reading header returned: OK");
  } else {
    error(2, "reading header returned: error");
  }

  /*
   * Try to read each line of data
   */
  while (!PDC_IO_at_EOF(pdc)) {
    out_sum_data_line_read(pdc, &dline_m, &dline_pd, &dline);
    out_sum_data_line_acc_add(pdc, &acc, &dline_pd, &dline);
  }
      
  out_sum_data_line_acc_report(pdc, "dline", 0, 0, &acc);

  if (PDC_ERR == PDC_IO_close(pdc)) {
    error(2, "*** PDC_IO_close failed ***");
    return -1;
  }

  if (PDC_ERR == PDC_close(pdc)) {
    error(2, "*** PDC_close failed ***");
    return -1;
  }

  return 0;
}
