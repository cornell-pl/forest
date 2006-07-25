/*
/usr/bin/cc -c -Wall -O2 -DNDEBUG -DUSE_GALAX -I. -I.. -I/home/mff/pads/ast-ast/arch/linux.i386/include/ast -I../../gen -I/home/mff/Galax-0.6.0-Linux/galax/lib/c -I/home/mff/pads/padsc/pads-glx -I/home/mff/pads/padsc/pads-glx/linux.i386 -I/home/mff/godi/lib/ocaml/std-lib -I/home/mff/pads/padsc/include -I/home/mff/pads/padsc/include/template ../format7-padx.c
*/
#include "pads-internal.h"
#include "pglx-internal.h"
#include "format7.h"

/* A flag indicating that given type is the main unit of processing. */
#define UNIT 0x01

typedef struct padx_handle *PADX_Handle;

/* Prototypes for PADX vtable functions */
typedef PADX_Handle      (* PDCI_open_fn)  (PADX_Handle px, void *mask, void *value); 
typedef PADX_Handle      (* PDCI_close_fn) (PADX_Handle px, void *mask); 
typedef PADX_Handle      (* PDCI_final_fn)  (P_t pads, void *mask, void *reploc, void *pdloc); 
typedef PADX_Handle      (* PDCI_children_vtable_fn)  (PADX_Handle px, void *mask, char *fieldname); 

/* For type T, constructor functions are specified at compile time: */
typedef struct constructor_functions { 
  PDCI_open_fn             open_T;
  PDCI_close_fn            close_T;
  PDCI_final_fn            final_T; 
  PDCI_children_vtable_fn  vtable_fun;  
} *PADX_CF; 

/* For instance of type T, constructor handle contains rep & pd
   locations and pointer to constructor functions */
typedef struct constructor_handle { 
  /* Run-time values that change */
  void *reploc;
  void *pdloc; 
  void *mask;
  PADX_CF functions;
} *PADX_CH; 

/* PADX handle contains PADS handle + constructor handle for
   "current" node */
struct padx_handle { 
  /* Run-time value that is unique */
  P_t    *pads;
  /* Resizable buffer */
  RBuf_t *rbuf;
  Sfio_t *output_io; 
  struct constructor_handle ch;
};

/*
 * BASE TYPE CONSTRUCTORS
 *
 */

void Pbase_pd_gen (P_t *pads, Pbase_pd *pd) { 
  Pbase_pd_init_no_err(pd);
}

void Pstring_pd_gen (P_t *pads, Pbase_pd *pd) { 
  Pstring_pd_init(pads, pd);
}

PADX_Handle open_Puint32 (PADX_Handle px, void *m, void *x) {
  Puint32 *rep; 
  Pbase_m  mask = *((Pbase_m *)m); 
  atomicInteger atomic = (atomicInteger) x;

  if (mask & UNIT) {
      /* Some region initialization has happened */
    RBuf_RESERVE(px->rbuf, px->ch.reploc, Puint32, 1);
    RBuf_RESERVE(px->rbuf, px->ch.pdloc, Pbase_pd, 1);

    /* Initialize pd & rep -- this function recursively initializes the pd for
       all constituent types in Puint32. 
     */
    /* There is no initialization function for Puint32 Puint32_init(px->ph, rep); */
  }

  rep = (Puint32 *)px->ch.reploc;

  /* This function converts the boxed Integer to a native int and writes into rep */
  galax_integer_of_atomicInteger(atomic, rep);

  return px; 
}

void close_Puint32(PADX_Handle px, void *m) { 
  Puint32    *rep = (Puint32 *)px->ch.reploc; 
  Pbase_pd *pd  = (Pbase_pd *)px->ch.pdloc; 
  Pbase_m  mask = *(Pbase_m *)m; 
  size_t   i; 

  /* Generate pd -- this function recursively generates the pd for
     all constituent types in Puint32. 

     Gen PD functions do not exist for base types yet. 
     For now, we should have a function that just creates a "good" PD.
  */
  if (mask & UNIT) 
    Pbase_pd_gen (px->pads, pd); 

  if (mask & UNIT) 
    i = Puint32_write2io(px->pads, px->output_io, pd, rep);

  if (mask & ~UNIT)
    RMM_free_buf(P_rmm_zero(px->pads), (void*)px);
    
} 

void final_Puint32(P_t *pads, void *m, void *reploc, void *pdloc) { 
  Pbase_m  mask = *(Pbase_m *)m; 

  if (mask & UNIT) {
      /* Return rep & pd to region allocator */
    RMM_free_buf(P_rmm_zero(pads), reploc);
    RMM_free_buf(P_rmm_zero(pads), pdloc);
  }
} 

PADX_Handle open_Pstring (PADX_Handle px, void *m, void *x) {
  Pstring *rep; 
  char *str;
  Pbase_m  mask = *((Pbase_m *)m); 
  atomicInteger atomic = (atomicInteger) x;

  if (mask & UNIT) {
      /* Some region initialization has happened */
    RBuf_RESERVE(px->rbuf, px->ch.reploc, Pstring, 1);
    RBuf_RESERVE(px->rbuf, px->ch.pdloc, Pbase_pd, 1);

    /* Initialize pd & rep -- this function recursively initializes the pd for
       all constituent types in Pstring. 
     */
    Pstring_init(px->pads, px->ch.reploc);
  }
  rep = (Pstring *)px->ch.reploc;

  /* This function converts the boxed String to a native C string. 
     Oof.  Lots of string copying going on here...
  */
  galax_string_of_atomicValue(atomic, &str);
  Pstring_cstr_copy(px->pads, rep, str, strlen(str));
  return px; 
}

void close_Pstring(PADX_Handle px, void *m) { 
  Pstring  *rep = (Pstring *)px->ch.reploc; 
  Pbase_pd *pd  = (Pbase_pd *)px->ch.pdloc; 
  Pbase_m  mask = *(Pbase_m *)m; 
  size_t   i; 

  /* Generate pd -- this function recursively generates the pd for
     all constituent types in Pstring. 
  */
  if (mask & UNIT) 
    Pstring_pd_gen(px->pads, pd);

  if (mask & UNIT) 
    i = Pstring_FW_write2io(px->pads, px->output_io, pd, rep, rep->len);

  if (mask & ~UNIT)
    RMM_free_buf(P_rmm_zero(px->pads), (void*)px);
    
} 

void final_Pstring(P_t *pads, void *m, void *reploc, void *pdloc) { 
  Pbase_m  mask = *(Pbase_m *)m; 

  if (mask & UNIT) {
      /* Return rep & pd to region allocator */
    RMM_free_buf(P_rmm_zero(pads), reploc);
    RMM_free_buf(P_rmm_zero(pads), pdloc);
  }
} 

/*
 * Format7 'Precord Pstruct test' example
 */

/* In close_test function, rep (and possibly pd) have already been
   written.  Free any temp space allocated in open_test(). I don't
   think we can assume that constructor handler can be freed at this
   point, because we might have to deallocate a memory buffer
   maintained in the CH at some point. 

*/

/* These functions are generated by the compiler */

PADX_Handle children_vtable_test (PADX_Handle px, void *m, char *field_name) {
  test    *rep  = (test *)px->ch.reploc; 
  test_pd *pd   = (test_pd *)px->ch.pdloc; 
  test_m  *mask = (test_m *)m; 
  PADX_Handle new_px;

  /* Create new PADX handle and copy current one */
  RBuf_RESERVE (px->rbuf, new_px, struct padx_handle, 1); 
  *new_px = *px; 

  if (strcmp(field_name, "id") == 0) {
    /* Return new constructor handler */
    new_px->ch.reploc = (void *)&(rep->id);
    new_px->ch.pdloc  = (void *)&(pd->id);
    new_px->ch.mask   = (void *)&(mask->id);
    /*    new_px->ch.functions = Puint32_functions; */
  }	
  else if (strcmp(field_name, "ts") == 0) {
    /* Return new constructor handler */
    new_px->ch.reploc = (void *)&(rep->ts);
    new_px->ch.pdloc  = (void *)&(pd->ts);
    new_px->ch.mask   = (void *)&(mask->ts);
    /*    new_px->ch.functions = Puint32_functions; */
  }
  else       
    fatal("Field does not occur in Pstruct test");
  return new_px;
}	

void close_test (PADX_Handle px, void *m) { 
  test *rep     = (test *)px->ch.reploc; 
  test_pd *pd   = (test_pd *)px->ch.pdloc; 
  test_m  *mask = (test_m *)m;

  /* Generate pd -- this function recursively generates the pd for
     all constituent types in test. */
  if (mask->compoundLevel & UNIT) 
    test_genPD (px->pads, rep, pd); 

  /* Hack! As a side-effect of closing an element, we might call
     serialization */
  if (mask->compoundLevel & UNIT) 
    test_write2io (px->pads, px->output_io, pd, rep);

  if (mask->compoundLevel & ~UNIT)
    RMM_free_buf(P_rmm_zero(px->pads), (void*)px);
}

void final_test(P_t *pads, void *m, void *reploc, void *pdloc) { 
  test_m  *mask = (test_m *)m;

  /* As a side effect of finalization, de-allocation functions. */
  if (mask->compoundLevel & UNIT) {
    RMM_free_buf(P_rmm_zero(pads), reploc);
    RMM_free_buf(P_rmm_zero(pads), pdloc);
  }
} 
  
/* The startup function is generic. 

   Before loading begins, the Galax Load_pads operator needs to call
   startup() to construct a PADX_Handle, which contains the memory
   manager, the PADS handle, and the vtable of functions that
   construct instances of PADS types.

   The toplevel_type_constructors value contains the vtable for the
   distinguished type that corresponds to the top-level PADS type.

   For format7, toplevel_type_constructors should correspond to myfile.

*/
PADX_Handle startup(P_t *pads, char *output_file, PADX_CH toplevel_type_constructors) { 
  PADX_Handle px = (PADX_Handle)NULL; 

  /* Get PADS' memory manager */
  RMM_t *memory_mgr = P_rmm_zero(pads);

  /* Get a new resizable buffer */
  RBuf_t *rbuf = RMM_new_rbuf(memory_mgr);
  
  /* Don't put parens around type name */
  RBuf_RESERVE (rbuf, px, struct padx_handle, 1); 

  if (!px) { 
    px->pads = pads;
    px->rbuf = rbuf;
    /* Initialize top level type. */
    px->ch = *toplevel_type_constructors;

    /* Open the output stream */
    px->output_io = sfopen(0, output_file, "w");
  }
  else 
    PDCI_report_err (pads,P_LEV_FATAL,0,P_ALLOC_ERR,"padx_startup","");
  return px;
}

