#include "padsc.h"
#include "me.h"

int main(int argc, char** argv) {
  PDC_t*          pdc;
  T1              t;
  T1_m            t_m;
  T1_pd           t_pd;

  PDC_open(&pdc, 0, 0);
  if (PDC_ERR == PDC_IO_fopen(pdc, "../../data/test_me.dat")) {
    error(ERROR_FATAL, "*** PDC_IO_fopen failed ***");
  }

  PDC_INIT_ALL(T1, pdc, t, t_m, t_pd, PDC_CheckAndSet);

  /*
   * Try to read each line of data
   */

  while (!PDC_IO_at_EOF(pdc)) {
    if (PDC_OK == T1_read(pdc, &t_m, &t_pd, &t)) {
      /* do something with the data */
      error(2, "line contains: %s|%s|%s", PDC_fmt_str(&(t.id1)), PDC_fmt_str(&(t.id2)), PDC_fmt_str(&(t.id3)));
    } else {
      error(2, "read returned error");
    }
  }

  PDC_CLEANUP_ALL(T1, pdc, t, t_pd);
  PDC_IO_close(pdc);
  PDC_close(pdc);
  return 0;
}
