#include "libpadsc.h"
#include "format3.h"

int main(int argc, char** argv) {
  PDC_t*          pdc;
  intList         f3data;
  intList_ed      f3ed;
  intList_em      f3em = {0};


  if (PDC_ERROR == PDC_open(0, &pdc)) {
    error(2, "*** PDC_open failed ***");
    exit(-1);
  }

  /* INIT f3data, f3ed -- must do this for all variable data types */
  intList_init   (pdc, &f3data, 0);
  intList_ed_init(pdc, &f3ed, 0);

  if (PDC_ERROR == PDC_IO_fopen(pdc, "../ex_data.format3", 0)) {
    error(2, "*** PDC_IO_fopen failed ***");
    exit(-1);
  }

  /*
   * Try to read each line of data
   */
  while (!PDC_IO_peek_EOF(pdc, 0)) {
    PDC_error_t res;
    int i;
    res= intList_read(pdc, &f3em, &f3ed, &f3data, 0);

    if (res == PDC_OK) {
      printf("Record okay:\t");
    } else if (f3data.length) {
      printf("Record not okay:\t");
    }
    for (i = 0; i < f3data.length; i++){
      printf("%d", f3data.intList[i]);
      if (i != f3data.length-1) {
	printf("|");
      }  else {
	printf("\n");
      }
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
