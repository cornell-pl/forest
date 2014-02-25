#define DEF_INPUT_FILE  "foo.data"
#define WSPACE_OK 1
#define PADS_TY(suf) source ## suf
#define IO_DISC_MK P_nlrec_make(0)
#include "foo.h"

#include "template/read_orig_write_xml.h"
