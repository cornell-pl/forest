#include "pads.h"
#include "format7.h"
#include "pglx.h"

#define exit_on_error(_Expr) {err = _Expr; if (err != 0) {error(0, "%s\n", galax_error_string); exit(err);}}	

int main(int argc, char** argv) {
  P_t*          pads;
  Pdisc_t      mydisc = Pdefault_disc;
  myfile          rep;
  myfile_pd       pd ;
  myfile_m        m;
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

  fake_argv[0] = "caml";
  fake_argv[1] = 0;
  galax_init(fake_argv);

  if (argc != 2) { error(2, "Usage: test_children <format7-data-file>\n"); exit(-1); }

  if (P_ERR == P_open(&pads,0,0)) {
    error(2, "*** P_open failed ***");
    exit(-1);
  }
  if (P_ERR == P_io_fopen(pads, argv[1])) {
    error(2, "*** P_io_fopen failed ***");
    exit(-1);
  }

  /* init -- must do this! */
  P_INIT_ALL(pads, myfile, rep, m, pd, P_CheckAndSet);

  /* make the top-level node */
  PDCI_MK_TOP_NODE_NORET (doc_node, &myfile_vtable, pads, "doc", &m, &pd, &rep, "main");

  /* Try to read entire file */
  if (P_OK == myfile_read(pads, &m, &pd, &rep)) {
    exit_on_error(padsDocument(argv[1], (nodeRep)doc_node, &doc)); 
    exit_on_error(galax_eval_statement_with_context_item_from_xml(doc, "<doc>{./*}</doc>", &docitems)); 
		  /*    docitems = itemlist_cons(doc, itemlist_empty()); */
    exit_on_error(galax_serialize_to_stdout(docitems));
  } else {
    error(0, "myfile_read returned: error");
  }

  P_CLEANUP_ALL(pads, myfile, rep, pd);
  P_io_close(pads);
  P_close(pads);
  return 0;
}
