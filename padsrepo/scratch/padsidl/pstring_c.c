#include "pads.h"

Perror_t iPstring_FW_read(/*in*/ P_t *pads, /*out*/ Pbase_pd *pd, /*out*/ Pstring *s_out, /*in*/ size_t width){
  Pbase_m m = P_CheckAndSet;
  return Pstring_FW_read(pads,&m,pd,s_out,width);
}

Perror_t iPstring_read(/*in*/ P_t *pads, /*out*/ Pbase_pd *pd, /*out*/ Pstring *s_out, /*in*/ Pchar stopChar){
  Pbase_m m = P_CheckAndSet;
  return Pstring_read(pads,&m,pd,s_out,stopChar);
}

Perror_t iPstring_ME_read(/*in*/ P_t *pads, /*out*/ Pbase_pd *pd, /*out*/ Pstring *s_out, /*in*/ char const *matchRegexp){
  Pbase_m m = P_CheckAndSet;
  return Pstring_ME_read(pads,&m,pd,s_out,matchRegexp);
}

Perror_t iPstring_SE_read(/*in*/ P_t *pads, /*out*/ Pbase_pd *pd, /*out*/ Pstring *s_out, /*in*/ char const *stopRegexp){
  Pbase_m m = P_CheckAndSet;
  return Pstring_SE_read(pads,&m,pd,s_out,stopRegexp);

}
