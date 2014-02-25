#define DEF_INPUT_FILE  "data/go"
#define PADS_HDR_TY(suf) OBO_header ## suf
#define PADS_TY(suf) OBO_stanza ## suf
#define IO_DISC_MK P_nlrec_make(0)

#include "go.h"
#include "template/accum_report.h"

