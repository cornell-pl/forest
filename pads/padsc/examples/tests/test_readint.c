#include "libpadsc.h"
#include "readinttest.h"
#include <error.h>

int main(int argc, char** argv) {
  PDC_t*          pdc;
  rec             r;
  rec_ed          ed = {0};

  if (PDC_ERR == PDC_open(&pdc, 0, 0)) {
    error(2, "*** PDC_open failed ***");
    exit(-1);
  }
  if (PDC_ERR == PDC_IO_fopen(pdc, "../data/ex_data.readinttest")) {
    error(2, "*** PDC_IO_fopen failed ***");
    exit(-1);
  }

  /*
   * Try to read each line of data
   */
  while (!PDC_IO_at_EOF(pdc)) {
    if (PDC_OK == rec_read(pdc, 0, &ed, &r)) {
      /* do something with the data */
    } else {
      error(0, "rec_read returned: error");
      if (ed.l.i8.errCode) {
	error(0|ERROR_PROMPT, "i8 error: ");
	PDCI_report_err(pdc, 0, 0, &(ed.l.i8.loc), ed.l.i8.errCode);
      }
      if (ed.l.i16.errCode) {
	error(0|ERROR_PROMPT, "i16 error: ");
	PDCI_report_err(pdc, 0, 0, &(ed.l.i16.loc), ed.l.i16.errCode);
      }
      if (ed.l.i32.errCode) {
	error(0|ERROR_PROMPT, "i32 error: ");
	PDCI_report_err(pdc, 0, 0, &(ed.l.i32.loc), ed.l.i32.errCode);
      }
      if (ed.l.i64.errCode) {
	error(0|ERROR_PROMPT, "i64 error: ");
	PDCI_report_err(pdc, 0, 0, &(ed.l.i64.loc), ed.l.i64.errCode);
      }
      if (ed.l.ui8.errCode) {
	error(0|ERROR_PROMPT, "ui8 error: ");
	PDCI_report_err(pdc, 0, 0, &(ed.l.ui8.loc), ed.l.ui8.errCode);
      }
      if (ed.l.ui16.errCode) {
	error(0|ERROR_PROMPT, "ui16 error: ");
	PDCI_report_err(pdc, 0, 0, &(ed.l.ui16.loc), ed.l.ui16.errCode);
      }
      if (ed.l.ui32.errCode) {
	error(0|ERROR_PROMPT, "ui32 error: ");
	PDCI_report_err(pdc, 0, 0, &(ed.l.ui32.loc), ed.l.ui32.errCode);
      }
      if (ed.l.ui64.errCode) {
	error(0|ERROR_PROMPT, "ui64 error: ");
	PDCI_report_err(pdc, 0, 0, &(ed.l.ui64.loc), ed.l.ui64.errCode);
      }
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
