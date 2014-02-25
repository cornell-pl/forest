#if defined(FOR_CKIT)
#include "xdr-replace.h"
#endif

Pinclude(:#include <rpc/rpc.h>:)

// Assume argument pointers point to valid space
void Pstr2float(Pstring *src, Pbase_pd *src_pd, Pfloat32 *dest, Pbase_pd *dest_pd){
  XDR xin;
  *dest = 0;
  *dest_pd = *src_pd;  /* set destination parse descriptor from source */
  xdrmem_create(&xin, src->str, src->len, XDR_DECODE);
  if (!xdr_float(&xin, dest)) {
    // Error: XDR conversion failed
      dest_pd->nerr++;
      dest_pd->errCode = P_TRANSFORM_FAILED;
  };
};

void Pfloat2str(P_t *p, Pfloat32 *src, Pbase_pd *src_pd, Pstring *dest, Pbase_pd *dest_pd){
  XDR xout;
  char str[8];
  *dest_pd = *src_pd;
  xdrmem_create(&xout, str, 4, XDR_ENCODE);
  xdr_float(&xout, src);
  if (P_OK != Pstring_cstr_copy(p, dest, str, 4)) {
    // Error: copy into Pstring failed
      dest_pd->nerr++;
      dest_pd->errCode = P_TRANSFORM_FAILED;
  };
}


Ptrans XDR_float32{
 Pstr2float: Pstring_FW(:4:) <=> Pfloat32: Pfloat2str(:pads:);
};

// Assume argument pointers point to valid space
void Pstr2float64(Pstring *src, Pbase_pd *src_pd, Pfloat64 *dest, Pbase_pd *dest_pd){
  XDR xin;
  *dest = 0;
  *dest_pd = *src_pd;  /* set destination parse descriptor from source */
  xdrmem_create(&xin, src->str, src->len, XDR_DECODE);
  if (!xdr_double(&xin, dest)) {
    // Error: XDR conversion failed
      dest_pd->nerr++;
      dest_pd->errCode = P_TRANSFORM_FAILED;
  };
};

void Pfloat642str(P_t *p, Pfloat64 *src, Pbase_pd *src_pd, Pstring *dest, Pbase_pd *dest_pd){
  XDR xout;
  char str[8];
  *dest_pd = *src_pd;
  xdrmem_create(&xout, str, 8, XDR_ENCODE);
  xdr_double(&xout, src);
  if (P_OK != Pstring_cstr_copy(p, dest, str, 8)) {
    // Error: copy into Pstring failed
      dest_pd->nerr++;
      dest_pd->errCode = P_TRANSFORM_FAILED;
  };
}


Ptrans XDR_float64{
 Pstr2float64: Pstring_FW(:8:) <=> Pfloat64: Pfloat642str(:pads:);
};


Pstruct XDR_string{
  Psbh_uint32(:4:)  len;
  Pstring_FW(:len:) str;
};


// Assume argument pointers point to valid space
void Psbh_uint322ip(Puint32 *src, Pbase_pd *src_pd, Pip *dest, Pbase_pd *dest_pd){
  *dest_pd = *src_pd;
  *dest = *src;
};

void Pip2Psbh_uint322ip(Pip *src, Pbase_pd *src_pd, Puint32 *dest, Pbase_pd *dest_pd){
  *dest_pd = *src_pd;
  *dest = *src;
};


Ptrans Psbh_ip{
  Psbh_uint322ip : Psbh_uint32(:4:) <=> Pip : Pip2Psbh_uint322ip;
};


// Assume argument pointers point to valid space
void uint322timestamp(Puint32 *src, Pbase_pd *src_pd, Ptimestamp *dest, Pbase_pd *dest_pd){
  *dest_pd = *src_pd;
  *dest = *src;
};

void timestamp2uint32(Ptimestamp *src, Pbase_pd *src_pd, Puint32 *dest, Pbase_pd *dest_pd){
  *dest_pd = *src_pd;
  *dest = *src;
};

Ptrans Psbh_timestamp{
  uint322timestamp : Psbh_uint32(:4:) <=> Ptimestamp(:' ':) : timestamp2uint32;
};

Ptrans Pb_timestamp{
  uint322timestamp : Pb_uint32 <=> Ptimestamp(:' ':) : timestamp2uint32;
};
