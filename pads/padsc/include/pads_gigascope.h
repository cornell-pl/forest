#include <stdio.h>

////////////////////////////////////////////////////////////////////////////////
// PADS functions

// returns 0 on OK, -1 on error
// this initializes global data structures
int PADS_INIT();

// returns 0 on OK, -1 on error
// sets up IO from specified file
// inName can be "/dev/stdin" or a file path
int PADS_INPUT_FILE(const char *inName);

// returns 0 on OK, -1 on error
// sets up IO from specified buffer/buffer length
int PADS_INPUT_BUF(char *buf, size_t buf_len);

// call when done
int PADS_CLEANUP();

// returns 0 on OK, -1 on error
// if OK, global_rec has been updated
// if no more records, sets *done to 1
int PADS_NEXT(int *done);

