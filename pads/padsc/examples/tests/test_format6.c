#include "libpadsc.h"
#include "format6.h"

int main(int argc, char** argv) {
  PDC_t*          pdc;
  defPN_ed        ped;
  defPN           pdata;

  /* Open pdc handle */
  if (PDC_ERR == PDC_open(&pdc, 0, 0)) {
    error(2, "*** PDC_open failed ***");
    exit(-1);
  }

  /* Open output file */
  if (PDC_ERR == PDC_IO_fopen(pdc, "../data/ex_data.format6")) {
    error(2, "*** PDC_IO_fopen failed ***");
    exit(-1);
  }

  /*
   * Try to read each line of data
   */
  while (!PDC_IO_at_EOF(pdc)) {
    PDC_error_t res;
    res= defPN_read(pdc, 0, 1999999999LL, 9999999999LL, &ped, &pdata);

    if (res == PDC_OK) {
      printf("Record okay:\t");
    } else {
      printf("Record not okay:\t");
    }
    printf("x = %llu\n", pdata.id);
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
