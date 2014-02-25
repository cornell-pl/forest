#define DEF_INPUT_FILE  "../data/dibbler.1000"
#define PADS_HDR_TY(suf) summary_header_t ## suf
#define PADS_TY(suf) entry_t ## suf
#define IO_DISC_MK P_nlrec_make(0)
#include "dibbler.h"
#define DELIMS "|"
#include "template/read_format.h"
