#define DEF_INPUT_FILE "gmond_trace2"
#define PADS_HDR_TY(suf) libpcapHeader_t ## suf
#define PADS_TY(suf) packet_t ## suf
#define IO_DISC_MK P_norec_noseek_make(0)
#define COPY_STRINGS 1
#include "ml.h"
#include "template/accum_report.h"
