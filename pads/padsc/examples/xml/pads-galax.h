/* generic calls from galax to pads */
#include <caml/mlvalues.h>  /* value type */

void**      PGLX_generic_children    (void *ocaml_n);
void*       PGLX_generic_parent      (void *ocaml_n);
value       PGLX_generic_typed_value (void *ocaml_n);
const char* PGLX_generic_string_value(void *ocaml_nn);

void        PGLX_node_free(void *node);
