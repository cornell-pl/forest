#include "libpadsc.h"
#include "format6.h"

int main(int argc, char** argv) {
  PDC_t*          pdc;
  defPN_em        pem = {PDC_CheckAndSet, {PDC_CheckAndSet, PDC_CheckAndSet}};
  defPN_ed        ped;
  defPN           pdata;

  /* Open pdc handle */
  if (PDC_ERR == PDC_open(0, &pdc)) {
    error(2, "*** PDC_open failed ***");
    exit(-1);
  }

  /* Open output file */
  if (PDC_ERR == PDC_IO_fopen(pdc, "../ex_data.format6", 0)) {
    error(2, "*** PDC_IO_fopen failed ***");
    exit(-1);
  }

  /*
   * Try to read each line of data
   */
  while (!PDC_IO_peek_EOF(pdc, 0)) {
    PDC_error_t res;
    int i;
    res= defPN_read(pdc, &pem, 1999999999LL, 9999999999LL, &ped, &pdata, 0);

    if (res == PDC_OK) {
      printf("Record okay:\t");
    } else {
      printf("Record not okay:\t");
    }
    printf("x = %llu\n", pdata.id);
  }

  if (PDC_ERR == PDC_IO_fclose(pdc, 0)) {
    error(2, "*** PDC_IO_fclose failed ***");
    exit(-1);
  }

  if (PDC_ERR == PDC_close(pdc, 0)) {
    error(2, "*** PDC_close failed ***");
    exit(-1);
  }

  return 0;
}
