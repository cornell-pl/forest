#define DEF_INPUT_FILE "data/updates"
#include "bgp.h"
#define PADS_TY(suf) MRT_Message_t ## suf
#define IO_DISC_MK P_norec_make(0)
#define DATE_OUT_FMT "%D:%T"
#define DELIMS "|"
#include "template/read_format.h"
