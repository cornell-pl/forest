#define DEF_INPUT_FILE  "asl.log"
#define PADS_TY(suf) Struct_1 ## suf
#define IO_DISC_MK P_nlrec_make(0)
#include "asl.log.h"
#define DELIMS "|"
#include "template/read_format.h"
