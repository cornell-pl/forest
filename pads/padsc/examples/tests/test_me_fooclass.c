#include "padsc.h"
#include "me_fooclass.h"

#define DAT_FILE "../../data/test_me.dat"

#define PROC_DATA(t) do { \
  error(2, "line contains: %s|%s|%s", PDC_fmt_str(&(t.id1)), PDC_fmt_str(&(t.id2)), PDC_fmt_str(&(t.id3))); \
} while (0)

#define SUF(x, s)     x ## s

/* following is generic, can do sed -e 's/ T1 / T2 /' to use other type T2 */

int main(int argc, char** argv) {
  PDC_t*             pdc;
  T1                 t;
  SUF( T1 , _m)      t_m;
  SUF( T1 , _pd)     t_pd;

  reg_foo();

  PDC_open(&pdc, 0, 0);
  if (PDC_ERR == PDC_IO_fopen(pdc, DAT_FILE)) {
    error(ERROR_FATAL, "*** PDC_IO_fopen failed on file %s ***", DAT_FILE);
  }

  PDC_INIT_ALL(pdc, T1 , t, t_m, t_pd, PDC_CheckAndSet);

  /*
   * Try to read each line of data
   */

  while (!PDC_IO_at_EOF(pdc)) {
    if (PDC_OK == SUF( T1 , _read)(pdc, &t_m, &t_pd, &t)) {
      /* do something with the data */
      PROC_DATA(t);
    }
  }

  PDC_CLEANUP_ALL(pdc, T1 , t, t_pd);
  PDC_IO_close(pdc);
  PDC_close(pdc);
  return 0;
}
