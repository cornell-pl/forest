#define DEF_INPUT_FILE  "ls-l.txt"
#define PADS_TY(suf) ls_l_t ## suf
#define IO_DISC_MK P_nlrec_make(0)
#include "ls-l.h"
#define DELIMS "|"
#include "template/read_format.h"
