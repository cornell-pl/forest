#include "pads.h"
#include "pglx.h"

#define exit_on_error(_Expr) {err = _Expr; if (err != 0) {error(0, "%s\n", galax_error_string); exit(err);}}	

#ifndef MAX_RECS
#  define MAX_RECS 0
#endif

int main(int argc, char** argv) {
  P_t*          pads;
  PADS_TY( )         rep;
  PADS_TY(_pd)       pd ;
  PADS_TY(_m)        m;
  PDCI_node_t    *doc_node;

  galax_err err;
  item doc;
  itemlist docitems;

  Pdisc_t       mydisc; 

  mydisc = Pdefault_disc; 
  mydisc.copy_strings = 1; 
#ifdef WSPACE_OK
  mydisc.flags |= (Pflags_t)P_WSPACE_OK;
#endif


  /* When linking with the Galax library, which contains a custom O'Caml runtime system, 
     it is necessary to call galax_init first, so the runtime is initialized and then 
     can delegate control back to the C program 
  */
  galax_init();

  if (argc != 2) { error(2, "Usage: test_load_XXX <XXX-data-file>\n"); exit(-1); }

  if (P_ERR == P_open(&pads,&mydisc,0)) {
    error(2, "*** P_open failed ***");
    exit(-1);
  }
  if (P_ERR == P_io_fopen(pads, argv[1])) {
    error(2, "*** P_io_fopen failed ***");
    exit(-1);
  }

  /* init -- must do this! */

  /* Initialize NodeMM. */
  NodeMM_initMM(pads,50);  
  
  if (P_ERR == PADS_TY(_init)(pads, &rep)) {
    error(ERROR_FATAL, "*** representation initialization failed ***");
  }
  if (P_ERR == PADS_TY(_pd_init)(pads, &pd)) {
    error(ERROR_FATAL, "*** parse description initialization failed ***");
  }
  /* init mask -- must do this! */
  PADS_TY(_m_init)(pads, &m, P_CheckAndSet);

  /* Try to read entire file */
  PADS_TY(_read)(pads, &m, &pd, &rep);
  if (!P_PS_isPanic(&pd)) { 
    /* make the top-level node */
    PDCI_MK_TOP_NODE_NORET (doc_node, &PADS_TY(_node_vtable), pads, "doc", &m, &pd, &rep, "main");
    err = padsDocument(argv[1], (nodeRep)doc_node, &doc);
    exit_on_error(err); 
    docitems = itemlist_cons(doc, itemlist_empty()); 
    err = galax_serialize_to_stdout(docitems);
    exit_on_error(err);
  } else {
    error(0, "read raised panic error");
  }

  // P_CLEANUP_ALL(pads, PADS_TY_, rep, pd);
  if (P_ERR == PADS_TY(_cleanup)(pads, &rep)) {
    error(ERROR_FATAL, "** representation cleanup failed **");
  }
  if (P_ERR == PADS_TY(_pd_cleanup)(pads, &pd)) {
    error(ERROR_FATAL, "** parse descriptor cleanup failed **");
  }
  P_io_close(pads);

  NodeMM_freeMM(pads);
  P_close(pads);
  return 0;
}
