#define DEF_INPUT_FILE "../../../data/dibbler1"

#define PADS_HDR_TY_ out_sum_header
#define PADS_HDR_TY(suf) out_sum_header ## suf
#define PPADS_HDR_TY(pref) out_sum_header ## myfile

#define PADS_TY_ out_sum_data_lines
#define PADS_TY(suf) out_sum_data_lines ## suf
#define PPADS_TY(pref) out_sum_data_lines ## myfile

#include "dibbler.h"
