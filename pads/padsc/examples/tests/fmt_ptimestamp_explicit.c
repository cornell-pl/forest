#define DEF_INPUT_FILE "../../data/ptimestamp_explicit"

#define WSPACE_OK 1
#define PADS_TY(suf) threetimestamps ## suf
#define DELIMS "|,"
#include "ptimestamp_explicit.h"
#include "template/read_format.h"
