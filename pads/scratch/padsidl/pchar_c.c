#include "pads.h"

/* Prefix function name with i (for idl) b/c Pchar_read is an existing macro. */
Perror_t iPchar_read (P_t *pads, Pbase_pd *pd, Pchar *c_out){
  Pbase_m m = P_CheckAndSet;
  return Pchar_read(pads,&m,pd,c_out);
}
