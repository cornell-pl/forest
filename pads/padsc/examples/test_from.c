#include "libpadsc.h"
#include "from.h"

int main(int argc, char** argv) {
  PDC_t*              pdc;
  out_sum_data_line_t d;

  if (PDC_ERROR == PDC_open(0, &pdc)) {
    error(2, "*** PDC_open failed ***");
    exit(-1);
  }
  if (PDC_ERROR == PDC_IO_fopen(pdc, "../ex_data.from", 0)) {
    error(2, "*** PDC_IO_fopen failed ***");
    exit(-1);
  }

  /*
   * Try to read each line of data
   */
  while (!PDC_IO_peek_EOF(pdc, 0)) {
    PDC_error_t res;
    int i;
    res= out_sum_data_line_t_read(pdc, 0, 0, &d, 0);

    if (res == PDC_OK) {
      printf("Record okay:\t");
    } else {
      printf("Record not okay:\t");
    }
    printf("%d %d\n", d.order_item, d.create_id);
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
