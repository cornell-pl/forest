#define DEF_INPUT_FILE  "../data/rpmpkgs.txt"
#define PADS_TY(suf) entry_t ## suf
#define IO_DISC_MK P_nlrec_make(0)
#include "rpmpkgs.h"

#include "template/accum_report.h"
