// Wrapper for padsc_stubs.c that includes pads.h before anything else
// in padsc_stubs.c

#include "pads.h"

/* 
  Specialized conversion functions for IDL types that need to
  combine functionality of ptr and unique attributes.
*/

#define ml2c_opt_decl(module,type)\
void ml2c_ ## module ## _ ## type ## Opt(value input, type ## Opt * output);

#define ml2c_opt(module,type)\
void ml2c_ ## module ## _ ## type ## Opt(value input, type ## Opt * output)\
{\
value in2;\
  if (input == Val_int(0)) {\
    (*output) = NULL;\
  } else {\
    in2 = Field(input, 0);\
    camlidl_ml2c_ ## module ## _ ## type (in2, output, (camlidl_ctx)NULL);\
  }\
}

#define c2ml_opt_decl(module,type)\
value c2ml_ ## module ## _ ## type ## Opt( type ## Opt * input);

#define c2ml_opt(module,type)\
value c2ml_ ## module ## _ ## type ## Opt( type ## Opt * input)\
{\
value out;\
value tmpv;\
  if ((*input) == NULL) {\
    out = Val_int(0);\
  } else {\
    tmpv = camlidl_c2ml_ ## module ## _ ## type (input, (camlidl_ctx)NULL);\
    Begin_root(tmpv)\
      out = camlidl_alloc_small(1, 0);\
      Field(out, 0) = tmpv;\
    End_roots();\
  }\
  return out;\
}

#define cnv_opt_decl(module,type)\
  ml2c_opt_decl(module,type)\
  c2ml_opt_decl(module,type)

#define cnv_opt(module,type)\
  ml2c_opt(module,type)\
  c2ml_opt(module,type)

#include "padsc_stubs.c"

/* Add dummy defintion of P_lib_init as there is no generated code to define this function.
   For more information, see pads.h. 
 */
P_NOGEN;

