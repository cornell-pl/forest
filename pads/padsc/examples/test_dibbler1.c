#include "libpadsc.h"
#include "dibbler.h"

int main(int argc, char** argv) {
  PDC_t*              pdc;
  out_sum_header_t    header;
  out_sum_data_line_t dline;

  if (PDC_ERROR == PDC_open(0, &pdc)) {
    error(2, "*** PDC_open failed ***");
    exit(-1);
  }
  if (PDC_ERROR == PDC_IO_fopen(pdc, "../ex_data.dibbler1", 0)) {
    error(2, "*** PDC_IO_fopen failed ***");
    exit(-1);
  }

  /*
   * Try to read header
   */

  if (PDC_OK == out_sum_header_t_read(pdc, 0, 0, &header, 0)) {
    error(0, "reading header returned: OK");
  } else {
    error(2, "reading header returned: error");
  }

  /*
   * Try to read each line of data
   */
  while (!PDC_IO_peek_EOF(pdc, 0)) {
    if (PDC_OK == out_sum_data_line_t_read(pdc, 0, 0, &dline, 0)) {
      /* do something with the data */
      error(0, "data line read returned: OK");
    } else {
      error(2, "data line read returned: error");
    }
  }

  if (PDC_ERROR == PDC_IO_fclose(pdc, 0)) {
    error(2, "*** PDC_IO_fclose failed ***");
    exit(-1);
  }

  if (PDC_ERROR == PDC_close(pdc, 0)) {
    error(2, "*** PDC_close failed ***");
    exit(-1);
  }

  return 0;
}
