#define IO_DISC_MK P_nlrec_noseek_make(0)
#define COPY_STRINGS 1
#define PADS_TY(suf) bio_t ## suf
#define PPADS_TY(pref) pref ## bio_t
//#define READ_MASK P_Set
#define TRACE 1
#include "bio2.h"
#include "template/accum_report.h"
