// Wrapper for padsc_stubs.c that includes pads.h before anything else
// in padsc_stubs.c

#include "pads.h"
#include "padsc_stubs.c"

/* Add dummy defintion of P_lib_init as there is no generated code to define this function.
   For more information, see pads.h. 
 */
P_NOGEN;

