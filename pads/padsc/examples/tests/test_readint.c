#include "libpadsc-internal.h"
#include "readinttest.h"
#include <error.h>


int main(int argc, char** argv) {
  PDC_t*          pdc;
  rec             r;
  rec_pd          pd = {0};
  rec_m           m;

  if (PDC_ERR == PDC_open(&pdc, 0, 0)) {
    error(2, "*** PDC_open failed ***");
    exit(-1);
  }
  if (PDC_ERR == PDC_IO_fopen(pdc, "../../data/ex_data.readinttest")) {
    error(2, "*** PDC_IO_fopen failed ***");
    exit(-1);
  }

  /* init mask -- must do this! */
  rec_maskFill(pdc, &m, PDC_CheckAndSet);

  /*
   * Try to read each line of data
   */
  while (!PDC_IO_at_EOF(pdc)) {
    if (PDC_OK == rec_read(pdc, &m, &pd, &r)) {
      /* do something with the data */
    } else {
      error(0, "rec_read returned: error");
      if (pd.l.i8.errCode) {
	error(0|ERROR_PROMPT, "i8 error: ");
	PDCI_report_err(pdc, 0, &(pd.l.i8.loc), pd.l.i8.errCode, "main", 0);
      }
      if (pd.l.i16.errCode) {
	error(0|ERROR_PROMPT, "i16 error: ");
	PDCI_report_err(pdc, 0, &(pd.l.i16.loc), pd.l.i16.errCode, "main", 0);
      }
      if (pd.l.i32.errCode) {
	error(0|ERROR_PROMPT, "i32 error: ");
	PDCI_report_err(pdc, 0, &(pd.l.i32.loc), pd.l.i32.errCode, "main", 0);
      }
      if (pd.l.i64.errCode) {
	error(0|ERROR_PROMPT, "i64 error: ");
	PDCI_report_err(pdc, 0, &(pd.l.i64.loc), pd.l.i64.errCode, "main", 0);
      }
      if (pd.l.ui8.errCode) {
	error(0|ERROR_PROMPT, "ui8 error: ");
	PDCI_report_err(pdc, 0, &(pd.l.ui8.loc), pd.l.ui8.errCode, "main", 0);
      }
      if (pd.l.ui16.errCode) {
	error(0|ERROR_PROMPT, "ui16 error: ");
	PDCI_report_err(pdc, 0, &(pd.l.ui16.loc), pd.l.ui16.errCode, "main", 0);
      }
      if (pd.l.ui32.errCode) {
	error(0|ERROR_PROMPT, "ui32 error: ");
	PDCI_report_err(pdc, 0, &(pd.l.ui32.loc), pd.l.ui32.errCode, "main", 0);
      }
      if (pd.l.ui64.errCode) {
	error(0|ERROR_PROMPT, "ui64 error: ");
	PDCI_report_err(pdc, 0, &(pd.l.ui64.loc), pd.l.ui64.errCode, "main", 0);
      }
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
