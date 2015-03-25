#include "pads.h"

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

Perror_t iPint32_read (P_t *pads, Pbase_pd *pd, Pint32 *res_out){
  Pbase_m m = P_CheckAndSet;
  return Pint32_read(pads,&m,pd,res_out);
}

Perror_t iPuint32_read (P_t *pads, Pbase_pd *pd, Puint32 *res_out){
  Pbase_m m = P_CheckAndSet;
  return Puint32_read(pads,&m,pd,res_out);
}

Perror_t iPint32_FW_read ( P_t *pads, Pbase_pd *pd, Pint32 *res_out, size_t width){
  Pbase_m m = P_CheckAndSet;
  return Pint32_FW_read(pads,&m,pd,res_out, width);
}

Perror_t iPuint32_FW_read ( P_t *pads, Pbase_pd *pd, Puint32 *res_out, size_t width){
  Pbase_m m = P_CheckAndSet;
  return Puint32_FW_read(pads,&m,pd,res_out, width);
}

ssize_t iPint32_write2io(P_t *pads,Sfio_t *io,Pbase_pd *pd,Pint32 *val){
  return Pint32_write2io(pads,io,pd,val);
}

ssize_t iPuint32_write2io(P_t *pads,Sfio_t *io,Pbase_pd *pd,Puint32 *val){
  return Puint32_write2io(pads,io,pd,val);
}

ssize_t iPint32_FW_write2io(P_t *pads,Sfio_t *io,Pbase_pd *pd,Pint32 *val, size_t width){
  return Pint32_FW_write2io(pads,io,pd,val,width);
}

ssize_t iPuint32_FW_write2io(P_t *pads,Sfio_t *io,Pbase_pd *pd,Puint32 *val, size_t width){
  return Puint32_FW_write2io(pads,io,pd,val,width);
}
