#include "pads.h"
#include "format7.h"
#include "pglx.h"

#define exit_on_error(_Expr) {err = _Expr; if (err != 0) {error(0, "%s\n", galax_error_string); exit(err);}}	

int try_galax() { 
  processing_context pc; 
  module_context sc;
  itemlist items;
  galax_err err;
  char *str;

  exit_on_error(galax_default_processing_context(&pc)); 
  exit_on_error(galax_load_standard_library(pc, &sc)); 

  exit_on_error(galax_eval_statement_from_string(sc, "<a/>", &items));
  exit_on_error(galax_serialize_to_string(items, &str));
  error(0, "%s\n", str); 
  return err;
}

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
  galax_init();

  if (argc != 2) { error(2, "Usage: test_children <format7-data-file>\n"); exit(-1); }

  /* Try out some Galax functions first */
  try_galax();

  mydisc.flags |= P_WSPACE_OK;

  if (P_ERR == P_open(&pads,&mydisc,0)) {
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
  error(0, "\ncalling myfile_read");
  if (P_OK == myfile_read(pads, &m, &pd, &rep)) {
    for (i = 0; i < rep.length; i++) {
      error(0, "line %d  id %d ts %d", i, rep.elts[i].id, rep.elts[i].ts); 
    }
    exit_on_error(padsDocument(argv[1], (nodeRep)doc_node, &doc)); 
    docitems = itemlist_cons(doc, itemlist_empty());
    exit_on_error(galax_serialize_to_string(docitems, &str));
    error(0, "%d: %s\n", strlen(str), str);  
    exit_on_error(galax_serialize_to_stdout(docitems));

    exit_on_error(galax_children(doc, &k)); 	
    for (i = 0; !is_empty(k); i++) {
      error(0, "%d...", i);
      n = items_first(k); 
      exit_on_error(galax_node_kind(n, &str)); 
      error(0, "%s\n", str);  
      exit_on_error(galax_node_name(n, &av)); 
      exit_on_error(galax_serialize_to_string(av, &str));
      error(0, "%s\n", str);  
      k = items_next(k); 
    }
    exit_on_error(galax_node_kind(doc, &str)); 
    error(0, "%s\n", str);  
    /* 
       exit_on_error(galax_serialize_to_string(k, &str));
    error(0, "%s\n", str); */
    /* 
    */
    error(0, "\nmyfile_read returned: ok");
  } else {
    error(0, "myfile_read returned: error");
  }

  P_CLEANUP_ALL(pads, myfile, rep, pd);
  P_io_close(pads);
  P_close(pads);
  return 0;
}
