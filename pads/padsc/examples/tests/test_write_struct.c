#include "libpadsc.h"
#include "struct.h"
/* #define FILENAME  "stdin" */
 #define FILENAME  "../../data/ex_data.struct" 
/* #define FILENAME  "../../data/ex_data.struct_valid" */

int main(int argc, char** argv) {
  PDC_t*          pdc;
  testtwo            f1data;
  testtwo_ed         ed = {0};

  if (PDC_ERR == PDC_open(&pdc,0,0)) {
    error(2, "*** PDC_open failed ***");
    exit(-1);
  }

  if (strcasecmp(FILENAME, "stdin") == 0) {
    error(0, "Data file = standard in\n");
    if (PDC_ERR == PDC_IO_set(pdc, sfstdin)) {
      error(2, "*** PDC_IO_set(sfstdin) failed ***");
      exit(-1);
    }
  } else {
    error(0, "Data file = %s\n", FILENAME);
    if (PDC_ERR == PDC_IO_fopen(pdc, FILENAME)) {
      error(2, "*** PDC_IO_fopen failed ***");
      exit(-1);
    }
  }

  /*
   * Try to read each line of data
   */
  while (!PDC_IO_at_EOF(pdc)) {
    error(0, "\ncalling testtwo_read");
    if (PDC_OK == testtwo_read(pdc, 0, &ed, &f1data)) {
      /* do something with the data */
      error(2, "testtwo_read returned: id %d  ts %d  f %d ", f1data.header.id, f1data.header.ts, f1data.f);
      testtwo_write2io(pdc, sfstdout, &ed, &f1data);
    } else {
      error(2, "testtwo_read returned: error");
      testtwo_write2io(pdc, sfstdout, &ed, &f1data);
    }
  }
  error(0, "\nFound eof");

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
