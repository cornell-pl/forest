/* test_ai.c
 * uses the walk_children function to pretty-print with XML format
 * data sources for the PADS file examples/p/ai.p
 * For examples see: examples/data/ex_data.ai 
 *                   examples/data/ai.big
 */

#include "pads.h"
#include "ai.h"
#include "pglx.h"
#include <ast.h>
#include <error.h>

int main(int argc, char** argv) {
  PDC_t           *pdc;
  PDC_disc_t      mydisc = PDC_default_disc;
  http_clf_t_pd   pd;
  http_clf_t      ai;
  http_clf_t_acc  acc;
  http_clf_t_m    m;
  PDCI_node_t     *top_node;
  
  char *fake_argv[2];
  fake_argv[0]="caml";
  fake_argv[1]=0;

  mydisc.flags |= PDC_WSPACE_OK;

  if (PDC_ERR == PDC_open(&pdc, &mydisc, 0)) {
    error(2, "*** PDC_open failed ***");
    exit(-1);
  }
  if (PDC_ERR == PDC_IO_fopen(pdc, argv[1])) {
    error(2, "*** PDC_IO_fopen failed ***");
    exit(-1);
  }

  if (PDC_ERR == http_clf_t_init(pdc, &ai)) {
    error(2, "*** http_clt_t_init failed ***");
    exit(-1);
  }
  if (PDC_ERR == http_clf_t_pd_init(pdc, &pd)) {
    error(2, "*** http_clt_t_pd_init failed ***");
    exit(-1);
  }
  if (PDC_ERR == http_clf_t_acc_init(pdc, &acc)) {
    error(2, "*** http_clt_t_acc_init failed ***");
    exit(-1);
  }

  http_clf_t_init(pdc,&ai);
  http_clf_t_pd_init(pdc,&pd);
  /* init mask -- must do this! */
  http_clf_t_m_init(pdc, &m, PDC_CheckAndSet);

  /* make the top-level node */
  PDCI_MK_TOP_NODE_NORET (top_node, &http_clf_t_vtable, pdc, "top", &m, &pd, &ai, "main");
  
  /* try to read each line of data and pretty-print it */
  while (!PDC_IO_at_EOF(pdc)) 
    if (PDC_OK == http_clf_t_read(pdc, &m, &pd, &ai)) 
      /* use walk_children to print data */
      walk_children(top_node,0);
    else 
      error(2, "read returned: error");
  
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
