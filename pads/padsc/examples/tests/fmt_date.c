#define DEF_INPUT_FILE "../../data/date"

//#define MASK_MOD   P_Dont_Write(m.compoundLevel);
//#define MASK_MOD   P_Dont_Write(m.header.c); P_Do_WriteVoid(m.header.c); P_Dont_Write(m.header.ts); P_Do_WriteVoid(m.header.ts);
//#define MASK_MOD   P_Dont_Write(m.header.compoundLevel);

#define DATE_FMT "%K"

#define WSPACE_OK 1
#define PADS_TY(suf) threedates ## suf
#define DELIMS "|,"
#include "date.h"
#include "template/read_format.h"
