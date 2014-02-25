#define DEF_INPUT_FILE  "../data/boot.txt"
#define PADS_TY(suf) entry_t ## suf
#define IO_DISC_MK P_nlrec_make(0)
#include "boot.h"
#include "template/read_orig_write_xml.h"
