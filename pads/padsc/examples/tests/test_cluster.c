/*
#define EXTRA_INIT_CODE \
Perror_t myFromFn (Pfloat64 f, Pint16 *v)  { *v = (Pint16)f + 100; return P_OK; } \
Perror_t myToFn   (Pint16 *v, Pfloat64 *f) { *f = (Pfloat64)(*v); return P_OK; } \
h.foo.fromFloat = (P_fromFloat_fn)myFromFn; \
h.foo.toFloat = (P_toFloat_fn)myToFn;
*/
#define DEF_INPUT_FILE "../../data/ai"
#define PADS_TY(suf) http_clf_t ## suf
#include "ai.h"
#include "template/cluster_report.h"

// Test programe to build histograms.

////////////////////////////////////////////////////////////////////////////////

