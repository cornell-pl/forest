/* test_struct3.c
 * uses the walk_children function to pretty-print with XML format
 * data sources for the PADS file examples/p/struct3.p
 * For an example see: examples/data/struct3
 */

#include "pads.h"
#include "struct3.h"
#include "pglx.h"
#include <stdio.h>

int main(int argc, char** argv) {   
  PDC_t*          pdc;
  PDC_disc_t      mydisc = PDC_default_disc;
  line            rep;
  line_pd         pd;
  line_m          m;
  PDCI_node_t    *top_node;

  /* When linking with the Galax library, which contains a custom O'Caml runtime system, 
     it is necessary to call glx_init first, so the runtime is initialized and then 
     can delegate control back to the C program 
  */
  char *fake_argv[2];

  fake_argv[0] = "caml";
  fake_argv[1] = 0;

  if (argc != 2) { error(2, "Usage: test_struct3 <struct3-data-file>\n"); exit(-1); }

  mydisc.flags |= PDC_WSPACE_OK;

  if (PDC_ERR == PDC_open(&pdc,&mydisc,0)) {
    error(2, "*** PDC_open failed ***");
    exit(-1);
  }
  if (PDC_ERR == PDC_IO_fopen(pdc, argv[1])) {
    error(2, "*** PDC_IO_fopen failed ***");
    exit(-1);
  }

  line_init(pdc, &rep);
  line_pd_init(pdc, &pd);

  /* init mask -- must do this! */
  line_m_init(pdc, &m, PDC_CheckAndSet);

  /* make the top-level node */
  PDCI_MK_TOP_NODE_NORET (top_node, &line_vtable, pdc, "top", &m, &pd, &rep, "main");
  
  /* try to read each line of data
   * and pretty-print the data or the error information
   */
  while (!PDC_IO_at_EOF(pdc)) 
    if (PDC_OK == line_read(pdc, &m, &pd, &rep)) 
      /* use walk_children to print data */
      walk_children(top_node, 0);
    else {
      error(2, "line_read returned: error");
      walk_children(top_node, 0);
    };
  
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
