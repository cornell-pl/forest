#include "pads.h"
#include "pint_c.h"

P_NOGEN;

/* Perror_t Pint_read (P_t *pads, Pbase_pd *pd, Pint32 *res_out){ */
/*   return P_OK; */
/* } */

/* Perror_t Puint_read (P_t *pads, Pbase_pd *pd, Puint32 *res_out){ */
/*   return P_OK; */
/* } */

/* Perror_t Pint_FW_read ( P_t *pads, Pbase_pd *pd, Pint32 *res_out, size_t width){ */
/*   return P_OK; */
/* } */

/* Perror_t Puint_FW_read ( P_t *pads, Pbase_pd *pd, Puint32 *res_out, size_t width){ */
/*   return P_OK; */
/* } */

Perror_t Pint_read (P_t *pads, Pbase_pd *pd, Pint32 *res_out){
  Pbase_m m = P_CheckAndSet;
  return Pint32_read(pads,&m,pd,res_out);
}

Perror_t Puint_read (P_t *pads, Pbase_pd *pd, Puint32 *res_out){
  Pbase_m m = P_CheckAndSet;
  return Puint32_read(pads,&m,pd,res_out);
}

Perror_t Pint_FW_read ( P_t *pads, Pbase_pd *pd, Pint32 *res_out, size_t width){
  Pbase_m m = P_CheckAndSet;
  return Pint32_FW_read(pads,&m,pd,res_out, width);
}

Perror_t Puint_FW_read ( P_t *pads, Pbase_pd *pd, Puint32 *res_out, size_t width){
  Pbase_m m = P_CheckAndSet;
  return Puint32_FW_read(pads,&m,pd,res_out, width);
}

