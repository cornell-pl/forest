#include "pads.h"

Perror_t iPtimestamp_explicit_FW_read(P_t *pads, Pbase_pd *pd,
                    Puint32 *res_out, size_t width, const char *format,
                    Tm_zone_t *tzone)
{
  Pbase_m m = P_CheckAndSet;
  return Ptimestamp_explicit_FW_read(pads,&m,pd,res_out,width,format,tzone);
}
