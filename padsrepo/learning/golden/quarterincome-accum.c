#define DEF_INPUT_FILE  "../data/quarterlypersonalincome.txt"
//#define PADS_TY(suf) quarter_income_t ## suf
#define PADS_TY(suf) entry_t ## suf
#define IO_DISC_MK P_nlrec_make(0)
#include "quarterincome.h"
#include "template/accum_report.h"
