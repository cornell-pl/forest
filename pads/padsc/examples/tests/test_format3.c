#include "libpadsc.h"
#include "format3.h"

/* XXX_REMOVE NEXT 2 LINES: */
#include "libpadsc-internal.h"
#define intList_m_init(pdc, mask_ptr, base_mask) PDCI_fill_mask((PDC_base_m*)mask_ptr, base_mask, sizeof(*(mask_ptr)))

int main(int argc, char** argv) {
  PDC_t           *pdc;
  PDC_IO_disc_t   *io_disc;
  intList         f3data;
  intList_ed      f3ed;
  intList_m       f3m;

  io_disc = PDC_norec_make(0);
  if (!io_disc) {
    error(ERROR_FATAL, "\nFailed to install IO discipline norec");
  } else {
    error(0, "\nInstalled IO discipline norec");
  }

  if (PDC_ERR == PDC_open(&pdc, 0, io_disc)) {
    error(2, "*** PDC_open failed ***");
    exit(-1);
  }

  /* INIT f3data, f3ed -- must do this for all variable data types */
  intList_init   (pdc, &f3data);
  intList_ed_init(pdc, &f3ed);

  /* INIT mask -- must do this! */
  intList_m_init(pdc, &f3m, PDC_CheckAndSet);

  if (PDC_ERR == PDC_IO_fopen(pdc, "../../data/ex_data.format3")) {
    error(2, "*** PDC_IO_fopen failed ***");
    exit(-1);
  }

  /*
   * Try to read each line of data
   */
  while (!PDC_IO_at_EOF(pdc)) {
    PDC_error_t res;
    int i;
    error(0, "\nCalling intList_read");
    res= intList_read(pdc, &f3m, &f3ed, &f3data);

    if (res == PDC_OK) {
      error(0|ERROR_PROMPT, "Record okay:\t");
      for (i = 0; i < f3data.length; i++){
	error(0|ERROR_PROMPT, "%d", f3data.elts[i]);
	if (i != f3data.length-1) {
	  error(0|ERROR_PROMPT, "|");
	}  else {
	  error(0, "");
	}
      }
    } else if (f3data.length) {
      error(0, "Record not okay");
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
