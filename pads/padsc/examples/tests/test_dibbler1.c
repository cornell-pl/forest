#include "libpadsc.h"
#include "dibbler.h"

int main(int argc, char** argv) {
  PDC_t*              pdc;
  out_sum_header_t    header;
  out_sum_data_line_t dline;

  if (PDC_ERR == PDC_open(&pdc, 0, 0)) {
    error(2, "*** PDC_open failed ***");
    exit(-1);
  }

  /* INIT dline -- must do this for all variable data types */
  out_sum_data_line_t_init(pdc, &dline);

  if (PDC_ERR == PDC_IO_fopen(pdc, "../data/ex_data.dibbler1")) {
    error(2, "*** PDC_IO_fopen failed ***");
    exit(-1);
  }

  /*
   * Try to read header
   */

  if (PDC_OK == out_sum_header_t_read(pdc, 0, 0, &header)) {
    error(0, "reading header returned: OK");
  } else {
    error(2, "reading header returned: error");
  }

  /*
   * Try to read each line of data
   */
  while (!PDC_IO_at_EOF(pdc)) {
    if (PDC_OK == out_sum_data_line_t_read(pdc, 0, 0, &dline)) {
      /* do something with the data */
      error(0, "data line read returned OK, number of events = %d", dline.events.length);
    } else {
      error(2, "data line read returned: error");
    }
  }

  if (PDC_ERR == PDC_IO_fclose(pdc)) {
    error(2, "*** PDC_IO_fclose failed ***");
    exit(-1);
  }

  if (PDC_ERR == PDC_close(pdc)) {
    error(2, "*** PDC_close failed ***");
    exit(-1);
  }

  return 0;
}
