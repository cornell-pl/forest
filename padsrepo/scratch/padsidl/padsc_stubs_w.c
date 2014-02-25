// Wrapper for padsc_stubs.c that includes pads.h before anything else
// in padsc_stubs.c

#include "padsc.h"
#include "padsc_stubs.c"

/* Add dummy defintion of P_lib_init as there is no generated code to define this function.
   For more information, see pads.h. 
 */
P_NOGEN;

/* Function wrappers for macros. Prefix function name with i (for idl)*/
int iP_POS_EQ(Ppos_t first, Ppos_t second)
{return P_POS_EQ(first,second);}

int iP_POS_GT(Ppos_t first, Ppos_t second)
{return P_POS_EQ(first,second);}

/* PADS has no fclose corresponding to fopen. We hack it by adding it by hand here. */
Perror_t P_fclose(SfioPtr io){
  if (io == sfstdin || sfstdout || sfstderr)
    return P_OK; /* Do nothing. These streams can't be closed. */
  /* Otherwise, try to close it. */
  switch (sfclose(io)){
  case 0: return P_OK;
  case -1: return P_ERR;
  default: return P_ERR;
  }
}

