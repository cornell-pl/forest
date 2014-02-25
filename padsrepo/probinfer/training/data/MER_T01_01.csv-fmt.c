#define DEF_INPUT_FILE  "../data/MER_T01_01.csv"
#define PADS_TY(suf) entry_t ## suf
#define IO_DISC_MK P_nlrec_make(0)
#include "MER_T01_01.csv.h"
#define DELIMS "|"
#include "template/read_format.h"
