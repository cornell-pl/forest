// #define DEF_INPUT_FILE "../../data/dibbler.10001"

#define PADS_HDR_TY(suf) summary_header_t ## suf
#define PADS_TY(suf) entry_t ## suf
#define PPADS_TY(pref) pref ## entry_t

#include "dibbler_new.h"
#include "template/accum_report.h"

