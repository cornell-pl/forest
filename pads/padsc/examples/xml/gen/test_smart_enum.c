#include "enum.h"

#define PADS_TY_ barArray
#define PADS_TY(suf) barArray ## suf
#define PPADS_TY(pref) pref ## barArray

//#define EXTRA_ARGS ,1000,'C',1

#include "test_smart.h"
