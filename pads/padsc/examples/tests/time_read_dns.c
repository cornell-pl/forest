#define DEF_INPUT_FILE "../../data/dns"
#define PADS_TY(suf) dns_msg ## suf
#define IO_DISC_MK P_norec_noseek_make(0)
#define COPY_STRINGS 1
#include "dns.h"
#include "template/timed_read.h"
