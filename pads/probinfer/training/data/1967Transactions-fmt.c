#define DEF_INPUT_FILE  "1967Transactions.short"
#define PADS_TY(suf) entry_t ## suf
#define IO_DISC_MK P_nlrec_make(0)
#include "1967Transactions.h"

#define DELIMS "|"
#include "template/read_format.h"
