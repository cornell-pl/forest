#include "padsc.h"
#include "format7.h"

int main(int argc, char** argv) {
  PDC_t*          pdc;
  myfile          rep;
  myfile_pd       pd ;
  myfile_m        m;

  PDC_ERR == PDC_open(&pdc, 0, 0);

  if (PDC_ERR == PDC_IO_fopen(pdc, "../../data/format7")) {
    error(ERROR_FATAL, "*** Could not open data file %s", "../../data/format7");
  }

  /* init -- must do this! */
  PDC_INIT_ALL(pdc, myfile, rep, m, pd, PDC_CheckAndSet);

  /* Try to read entire file */
  error(0, "\ncalling myfile_read");
  if (PDC_OK == myfile_read(pdc, &m, &pd, &rep)) {
    error(0, "myfile_read worked");
  } else {
    error(0, "myfile_read returned: error");
  }

  PDC_CLEANUP_ALL(pdc, myfile, rep, pd);
  PDC_IO_close(pdc);
  PDC_close(pdc);
  return 0;
}
