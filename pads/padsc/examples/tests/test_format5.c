#include "padsc.h"
#include "format5.h"


#define NO_NL 0|ERROR_PROMPT

int main(int argc, char** argv) {
  PDC_t*          pdc;
  call_pd         cpd = {0};
  call            cdata;
  call_m          cm;

  /* Open pdc handle */
  if (PDC_ERR == PDC_open(&pdc, 0, 0)) {
    error(2, "*** PDC_open failed ***");
    exit(-1);
  }

  /* Open output file */
  if (PDC_ERR == PDC_IO_fopen(pdc, "../../data/ex_data.format5")) {
    error(2, "*** PDC_IO_fopen failed ***");
    exit(-1);
  }

  /* Init mask -- must do this! */
  call_m_init(pdc, &cm, PDC_CheckAndSet);

  /*
   * Try to read each line of data
   */
  while (!PDC_IO_at_EOF(pdc)) {
    PDC_error_t res;
    res= call_read(pdc, &cm, &cpd, &cdata);

    if (res == PDC_OK) {
      error(NO_NL, "Record okay:\t");
    } else {
      error(NO_NL, "Record not okay:\t");
    }
    error(NO_NL, "x = %d\t", cdata.x.x);
    switch (cdata.pn.tag ){
    case code : 
	error(0, "tagged as code: %d",cdata.pn.val.code );
	break;
    case pn :
	error(0, "tagged as phone number: %d", cdata.pn.val.pn);
	break;
    default:
	error(0, "bogus tag. ");
	break;      
    }
  }

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
