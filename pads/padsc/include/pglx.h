/* generic calls from galax to pads */
#if 0 /* XXX_REMOVE */
#include <caml/mlvalues.h>  /* value type */
#else
typedef long value; /* XXX_REMOVE */
#endif

void**      PGLX_generic_children    (void *ocaml_n);
void*       PGLX_generic_parent      (void *ocaml_n);
value       PGLX_generic_typed_value (void *ocaml_n);
const char* PGLX_generic_string_value(void *ocaml_n);
const char* PGLX_generic_name        (void *ocaml_n);

void        PGLX_node_free           (void *ocaml_n);
