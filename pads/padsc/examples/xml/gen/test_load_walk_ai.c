#include "pads.h"
#include "ai.h"
#include "pglx.h"

#define exit_on_error(_Expr) {err = _Expr; if (err != 0) {error(0, "%s\n", galax_error_string); exit(err);}}	

int main(int argc, char** argv) {
  P_t*          pads;
  log_t          rep;
  log_t_pd       pd ;
  log_t_m        m;
  PDCI_node_t    *doc_node;

  galax_err err;
  item doc;
  node n; 
  char *str = "";
  itemlist k, docitems;
  atomicValue_list av;
  int i;

  /* When linking with the Galax library, which contains a custom O'Caml runtime system, 
     it is necessary to call galax_init first, so the runtime is initialized and then 
     can delegate control back to the C program 
  */
  char *fake_argv[2];

  galax_init();

  if (argc != 2) { error(2, "Usage: test_children <ai-data-file>\n"); exit(-1); }

  if (P_ERR == P_open(&pads,0,0)) {
    error(2, "*** P_open failed ***");
    exit(-1);
  }
  if (P_ERR == P_io_fopen(pads, argv[1])) {
    error(2, "*** P_io_fopen failed ***");
    exit(-1);
  }

  /* init -- must do this! */
  P_INIT_ALL(pads, log_t, rep, m, pd, P_CheckAndSet);

  /* make the top-level node */
  PDCI_MK_TOP_NODE_NORET (doc_node, &log_t_vtable, pads, "doc", &m, &pd, &rep, "main");

  /* Try to read entire file */
  if (P_OK == log_t_read(pads, &m, &pd, &rep)) {

    walk_children(doc_node, ""); 

    exit_on_error(padsDocument(argv[1], (nodeRep)doc_node, &doc)); 
    /*    exit_on_error(galax_eval_statement_with_context_item_from_xml(doc, "<doc>{./*}</doc>", &docitems));  */
    docitems = itemlist_cons(doc, itemlist_empty()); 
    exit_on_error(galax_serialize_to_stdout(docitems));
  } else {
    error(0, "log_t_read returned: error");
  }

  P_CLEANUP_ALL(pads, log_t, rep, pd);
  P_io_close(pads);
  P_close(pads);
  return 0;
}
