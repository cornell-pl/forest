#define DEF_INPUT_FILE "../data/go_old"
#define DEF_QUERY_FILE "../../queries/dot.xq"
#define PADS_TY(suf) OBO_file ## suf
#define IO_DISC_MK P_nlrec_make(0)

#include "go.h"
#include "template/pglx_bulk_query.h"
