#include <stdio.h>

////////////////////////////////////////////////////////////////////////////////
// PADS functions

// Some return result codes
#define  PADS_RET_OK              0  // no error
#define  PADS_RET_ERR            -1  // error other than 'too few bytes'
#define  PADS_RET_TOO_FEW_BYTES  -2  // too few bytes

// PADS_INIT: initialize pads data structures.
// Returns PADS_RET_OK or PADS_RET_ERR.
int PADS_INIT();

// PADS_INPUT_FILE: (re)initialize pads to read from file "inName",
// which can be "/dev/stdin" or a file path.  If "continuation" is
// non-zero then the new input is treated as a continuation of the
// bytes already *successfully* parsed by PADS_NEXT calls (i.e.,
// PADS_NEXT calls with a return value of PADS_RET_OK or
// PADS_RET_ERR).  If "continuation" is 0, the new bytes begin a fresh
// parse, which resets the pads state.
// Returns PADS_RET_OK or PADS_RET_ERR.
int PADS_INPUT_FILE(const char *inName, int expect_new_header);

// PADS_INPUT_BUF: (re)initialize pads to read from a buffer "buf" of
// length "buf_len".  If "continuation" is non-zero then the new input
// is treated as a continuation of the bytes already *successfully*
// parsed by PADS_NEXT calls (i.e., PADS_NEXT calls with a return
// value of PADS_RET_OK or PADS_RET_ERR).  If "continuation" is 0, the
// new bytes begin a fresh parse, which resets the pads state.
// Returns PADS_RET_OK or PADS_RET_ERR.
int PADS_INPUT_BUF(char *buf, size_t buf_len, int expect_new_header);

// PADS_CLEANUP: done using pads, clean up pads data structures.
// Returns PADS_RET_OK or PADS_RET_ERR.
int PADS_CLEANUP();

// PADS_NEXT: read the next record.
//
// Returns one of three result codes:
//   PADS_RET_OK             => successful parse, no errors
//   PADS_RET_ERR            => entire record was read,
//                              but there were some field-level errors
//   PADS_RET_TOO_FEW_BYTES  => not enough bytes available to read entire record
//
// In addition, sets *done to one of two values:
//    0  => there are more records to be read
//    1  => there are no more records to be read
//          (another PADS_INPUT_FILE or PADS_INPUT_BUF call
//           must be used before using PADS_NEXT again).
int PADS_NEXT(int *done);

// PADS_NUM_BYTES_PARSED: returns the number of bytes parsed by calls
// to PADS_NEXT since the last PADS_INPUT_BUF or PADS_INPUT_FILE call.
// Note: a call to PADS_NEXT that produces PADS_TOO_FEW_BYTES does not
// change the number of bytes parsed.
int PADS_NUM_BYTES_PARSED();

