#define DEF_INPUT_FILE "../../data/date"
#define PADS_TY(suf) threedates ## suf
#include "date.h"
#define DATE_OUT_FMT "%K"
#include "template/read_orig_write_xml.h"
