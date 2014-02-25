#define DEF_INPUT_FILE  "../data/quarterlypersonalincome.txt"
#define PADS_TY(suf) quarter_income_t ## suf
#define IO_DISC_MK P_nlrec_make(0)
#include "quarterincome.h"
#define DELIMS "|"
#include "template/read_format.h"
