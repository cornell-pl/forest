#include "libpadsc.h"
#include "format6.h"

#define NO_NL 0|ERROR_PROMPT

int main(int argc, char** argv) {
  PDC_t*          pdc;
  defPN_pd        ppd;
  defPN           pdata;
  defPN_m         pm;

  /* Open pdc handle */
  if (PDC_ERR == PDC_open(&pdc, 0, 0)) {
    error(2, "*** PDC_open failed ***");
    exit(-1);
  }

  /* Open output file */
  if (PDC_ERR == PDC_IO_fopen(pdc, "../../data/ex_data.format6")) {
    error(2, "*** PDC_IO_fopen failed ***");
    exit(-1);
  }

  /* init mask -- must do this! */
  defPN_m_init(pdc, &pm, PDC_CheckAndSet);

  /*
   * Try to read each line of data
   */
  while (!PDC_IO_at_EOF(pdc)) {
    PDC_error_t res;
    res= defPN_read(pdc, &pm, 1999999999LL, 9999999999LL, &ppd, &pdata);

    if (res == PDC_OK) {
      error(NO_NL, "Record okay:\t");
    } else {
      error(NO_NL, "Record not okay:\t");
    }
    error(NO_NL, "x = %llu\n", pdata.id);
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
