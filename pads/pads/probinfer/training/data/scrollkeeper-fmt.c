#define DEF_INPUT_FILE  "scrollkeeper.log"
#define PADS_TY(suf) Union_all ## suf
#define IO_DISC_MK P_nlrec_make(0)
#include "scrollkeeper.h"
#define DELIMS "|"
#include "template/read_format.h"
