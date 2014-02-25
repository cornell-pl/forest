#define DEF_INPUT_FILE  "coraldnssrv.log.head"
#define PADS_TY(suf) entry_t ## suf
#define IO_DISC_MK P_nlrec_make(0)
#include "coraldnssrv.h"
#define DELIMS "|"
#include "template/read_format.h"
