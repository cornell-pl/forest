/* generic calls from galax to pads */

#include "libpadsc.h"       /* Need to include ast stuff before caml stuff for some reason */
#include "caml/mlvalues.h"  /* Need value */

void**      PGLX_generic_children    (void *ocaml_n);
void*       PGLX_generic_parent      (void *ocaml_n);
value       PGLX_generic_typed_value (void *ocaml_n);
const char* PGLX_generic_string_value(void *ocaml_n);
const char* PGLX_generic_name        (void *ocaml_n);

void        PGLX_node_free           (void *ocaml_n);
