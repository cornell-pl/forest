#include "libpadsc.h"
#include "format5.h"

int main(int argc, char** argv) {
  PDC_t*          pdc;
  call_ed         ced = {0};
  call            cdata;

  /* Open pdc handle */
  if (PDC_ERR == PDC_open(&pdc, 0, 0)) {
    error(2, "*** PDC_open failed ***");
    exit(-1);
  }

  /* Open output file */
  if (PDC_ERR == PDC_IO_fopen(pdc, "../data/ex_data.format5")) {
    error(2, "*** PDC_IO_fopen failed ***");
    exit(-1);
  }

  /*
   * Try to read each line of data
   */
  while (!PDC_IO_at_EOF(pdc)) {
    PDC_error_t res;
    res= call_read(pdc, 0, &ced, &cdata);

    if (res == PDC_OK) {
      printf("Record okay:\t");
    } else {
      printf("Record not okay:\t");
    }
    printf("x = %d\t", cdata.x.x);
    switch (cdata.pn.tag ){
    case code : 
	printf("tagged as code: %d\n",cdata.pn.val.code );
	break;
    case pn :
	printf("tagged as phone number: %d\n", cdata.pn.val.pn);
	break;
    default:
	printf("bogus tag. \n");
	break;      
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
