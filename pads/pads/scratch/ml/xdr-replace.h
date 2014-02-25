typedef void * XDR;
void xdrmem_create(XDR *xin, void * buf, size_t s, int code);
int xdr_float(XDR *xin, Pfloat32* f);
int xdr_double(XDR *xin, Pfloat64* f);
enum{XDR_ENCODE, XDR_DECODE} xdr_codes;
