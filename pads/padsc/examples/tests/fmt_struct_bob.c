#define DEF_INPUT_FILE "../../data/struct_bob"

#define EXTRA_GOOD_READ_CODE do { \
  error(2, "testtwo_read returned: id %d  ts %d  f %d ", rep.header.id, rep.header.ts, rep.f); \
} while (0)

#define EXTRA_BAD_READ_CODE do { \
  error(2, "testtwo_read returned: error"); \
} while (0)

//#define MASK_MOD   P_Dont_Write(m.compoundLevel);
//#define MASK_MOD   P_Dont_Write(m.header.c); P_Do_WriteVoid(m.header.c); P_Dont_Write(m.header.ts); P_Do_WriteVoid(m.header.ts);
//#define MASK_MOD   P_Dont_Write(m.header.compoundLevel);

#define WSPACE_OK 1
#define PADS_TY(suf) testtwo ## suf
#define PPADS_TY(pref) pref ## testtwo
#define DELIMS "|,"
#include "struct_bob.h"
#include "template/read_format.h"

