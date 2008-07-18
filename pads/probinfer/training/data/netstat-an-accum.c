#define DEF_INPUT_FILE  "netstat-an"
#define PADS_TY(suf) entries_t ## suf
#define IO_DISC_MK P_nlrec_make(0)
#include "netstat-an.h"

#include "template/accum_report.h"
