#include "libpadsc.h"
#include "readinttest.h"
#include <error.h>

int main(int argc, char** argv) {
  PDC_t*          pdc;
  record          r;
  record_ed       ed = {0};

  if (PDC_ERROR == PDC_open(0, &pdc)) {
    error(2, "*** PDC_open failed ***");
    exit(-1);
  }
  if (PDC_ERROR == PDC_IO_fopen(pdc, "../ex_data.readinttest", 0)) {
    error(2, "*** PDC_IO_fopen failed ***");
    exit(-1);
  }

  /*
   * Try to read each line of data
   */
  while (!PDC_IO_peek_EOF(pdc, 0)) {
    if (PDC_OK == record_read(pdc, 0, &ed, &r, 0)) {
      /* do something with the data */
    } else {
      error(0, "record_read returned: error");
      if (ed.l.i8.errCode) {
	error(0|ERROR_PROMPT, "i8 error: ");
	PDC_report_err(pdc, 0, 0, &(ed.l.i8.loc), ed.l.i8.errCode, 0);
      }
      if (ed.l.i16.errCode) {
	error(0|ERROR_PROMPT, "i16 error: ");
	PDC_report_err(pdc, 0, 0, &(ed.l.i16.loc), ed.l.i16.errCode, 0);
      }
      if (ed.l.i32.errCode) {
	error(0|ERROR_PROMPT, "i32 error: ");
	PDC_report_err(pdc, 0, 0, &(ed.l.i32.loc), ed.l.i32.errCode, 0);
      }
      if (ed.l.i64.errCode) {
	error(0|ERROR_PROMPT, "i64 error: ");
	PDC_report_err(pdc, 0, 0, &(ed.l.i64.loc), ed.l.i64.errCode, 0);
      }
      if (ed.l.ui8.errCode) {
	error(0|ERROR_PROMPT, "ui8 error: ");
	PDC_report_err(pdc, 0, 0, &(ed.l.ui8.loc), ed.l.ui8.errCode, 0);
      }
      if (ed.l.ui16.errCode) {
	error(0|ERROR_PROMPT, "ui16 error: ");
	PDC_report_err(pdc, 0, 0, &(ed.l.ui16.loc), ed.l.ui16.errCode, 0);
      }
      if (ed.l.ui32.errCode) {
	error(0|ERROR_PROMPT, "ui32 error: ");
	PDC_report_err(pdc, 0, 0, &(ed.l.ui32.loc), ed.l.ui32.errCode, 0);
      }
      if (ed.l.ui64.errCode) {
	error(0|ERROR_PROMPT, "ui64 error: ");
	PDC_report_err(pdc, 0, 0, &(ed.l.ui64.loc), ed.l.ui64.errCode, 0);
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
