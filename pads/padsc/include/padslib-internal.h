#include "padslib.h"
error_f USER_ERROR_FUN(toolState_t *ts);      /* prob a macro */

typedef struct _toolDisc_s{
  /* Fields to allow user to control behavior. */
  error_f f;
} _toolDisc_t;

typedef struct _toolState_s{
  _toolDisc_t td;
} _toolState_t;

void get_location_info(toolState_t *ts, loc_t *l); 

void IO_checkpoint(toolState_t *ts);
void IO_commit(toolState_t *ts);
void IO_restore(toolState_t *ts);

int is_EOF(toolState_t *ts);

typedef struct TS_buffer_s{
  int elementSize;
  int numElements;
  int numAllocated;
  void *buffer;
} TS_buffer;

TS_buffer * TS_malloc(int element_Size, int numElements);   
void * TS_grow(TS_buffer *buffer, int numRequired);  
void * TS_getBuffer(TS_buffer *buf); 
void * TS_free(TS_buffer *buf, int dealloc); 
void TS_buffFree(void *);

toolError_t aint8_scan(toolState_t *ts, base_em *em,
                       base_ed *error, int8 res);

toolError_t auint8_scan(toolState_t *ts, base_em *em,
                       base_ed *error, uint8 res);
