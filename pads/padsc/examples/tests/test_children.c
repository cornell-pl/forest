#include "libpadsc.h"
#include "format1.h"
#include "pglx.h"

int main(int argc, char** argv) {
  PDC_t*          pdc;
  PDC_disc_t      mydisc = PDC_default_disc;
  test            rep;
  test_pd         pd ;
  test_m          m;
  PDCI_node_t    *top_node;

  mydisc.flags |= PDC_WSPACE_OK;

  if (PDC_ERR == PDC_open(&pdc,&mydisc,0)) {
    error(2, "*** PDC_open failed ***");
    exit(-1);
  }
  if (PDC_ERR == PDC_IO_fopen(pdc, "../../data/ex_data.format1")) {
    error(2, "*** PDC_IO_fopen failed ***");
    exit(-1);
  }

  test_init(pdc, &rep);
  test_pd_init(pdc, &pd);
  /* init mask -- must do this! */
  test_m_init(pdc, &m, PDC_CheckAndSet);

  /* make the top-level node */
  PDCI_MK_TOP_NODE_NORET (top_node, &test_vtable, pdc, "top", &m, &pd, &rep, "main");

  /*
   * Try to read each line of data
   */
  while (!PDC_IO_at_EOF(pdc)) {
    error(0, "\ncalling test_read");
    if (PDC_OK == test_read(pdc, &m, &pd, &rep)) {
      /* do something with the data */
      error(2, "test_read returned: id %d  ts %d", rep.id, rep.ts);
      walk_children(top_node, 0);
    } else {
      error(2, "test_read returned: error");
      walk_children(top_node, 0);
    }
  }
  error(0, "\nFound eof");

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
