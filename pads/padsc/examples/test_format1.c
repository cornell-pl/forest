#include "libpadsc.h"
#include "format1.h"

int main(int argc, char** argv) {
#if 0
  unsigned char   c;
#endif
  PDC_t*          pdc;
  test_rep        f1data;

  if (PDC_ERROR == PDC_open(0, &pdc)) {
    error(2, "*** PDC_open failed ***");
    exit(-1);
  }
  if (PDC_ERROR == PDC_IO_fopen(pdc, "ex_data.format1", 0)) {
    error(2, "*** PDC_IO_fopen failed ***");
    exit(-1);
  }

  /*
   * Try to read each line of data
   */
  while (!PDC_IO_peek_EOF(pdc, 0)) {
    if (PDC_OK == test_read(pdc, 0, 0, &f1data, 0)) {
      /* do something with the data */
      error(2, "test_read returned: id %d  ts %d", f1data.id, f1data.ts);
    } else {
      error(2, "test_read returned:error");
    }
  }

#if 0
  while (PDC_OK == PDC_IO_getchar(pdc, &c, 0, 0)) {
    /* do nothing -- should hit EOF */
  }
#endif

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
