#include "pads-internal.h"
#include "array_skip.h"

#define _PADS_TY seq_t
#define PADS_TY(suf) seq_t ## suf

#define _PADS_ELT_TY Pint32
#define PADS_ELT_TY(suf) Pint32 ## suf

#define _PADS_ELT_PD_TY Pbase_pd

#include "template/read_and_reread.h"
