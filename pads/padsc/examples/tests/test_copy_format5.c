#include "padsc.h"
#include "format5.h"

int main(int argc, char** argv) {
  PDC_t*          pdc;
  call_pd         cpd, cpdCpy;
  call            cdata, cdataCpy;
  PDC_disc_t      mydisc = PDC_default_disc;
  
  mydisc.flags |= PDC_WSPACE_OK;

  /* Open pdc handle */
  if (PDC_ERR == PDC_open(&pdc, &mydisc, 0)) {
    error(2, "*** PDC_open failed ***");
    exit(-1);
  }

  /* Open output file */
  if (PDC_ERR == PDC_IO_fopen(pdc, "../data/ex_data.format5.cpy")) {
    error(2, "*** PDC_IO_fopen failed ***");
    exit(-1);
  }

  /*
   * Try to read each line of data
   */
  while (!PDC_IO_at_EOF(pdc)) {
    PDC_error_t res;
    res= call_read(pdc, 0, &cpd, &cdata);

    if (res == PDC_OK) {
      sfprintf(sfstdout, "Record okay:\t");
      call_copy(pdc, &cdataCpy, &cdata);
    } else {
      sfprintf(sfstdout, "Record not okay:\t");
      call_copy(pdc, &cdataCpy, &cdata);
      call_pd_copy(pdc, &cpdCpy, &cpd);
    }
    sfprintf(sfstdout, "x = %d\t", cdata.x.x);
    switch (cdata.pn.tag ){
    case code : 
	sfprintf(sfstdout, "tagged as code: %d\n",cdata.pn.val.code );
	break;
    case pn :
	sfprintf(sfstdout, "tagged as phone number: %d\n", cdata.pn.val.pn);
	break;
    default:
	sfprintf(sfstdout, "bogus tag. \n");
	break;      
    }
  }

  if (PDC_ERR == PDC_IO_close(pdc)) {
    error(2, "*** PDC_IO_fclose failed ***");
    exit(-1);
  }

  if (PDC_ERR == PDC_close(pdc)) {
    error(2, "*** PDC_close failed ***");
    exit(-1);
  }

  return 0;
}
