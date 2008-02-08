#define DEF_INPUT_FILE  "../data/convert"
#define PADS_TY(suf) log_t ## suf
#define IO_DISC_MK P_nlrec_make(0)
#include "convert.h"
#define DELIMS "|"
#include "template/read_format.h"
