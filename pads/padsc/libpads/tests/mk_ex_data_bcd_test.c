#include "pads-internal.h" /* for testing - normally do not include internal */

P_NOGEN

#include <stdio.h>

int main(int argc, char** argv) {
  P_t*          pads;
  const char* fname = "../../data/ex_data.bcd_test";
  char tmp[100];
  Sfio_t* io;
  int len;

  for (len = 0; len < 100; len++) {
    tmp[len] = P_EBCDIC_SPACE;
  }

  if (P_ERR == P_libopen(&pads, 0, 0, 1)) {
    error(2, "*** P_libopen failed ***");
    return -1;
  }

  printf("fname = %s\n", fname);
  io = sfopen(0, fname, "w");

  if (2 != PDCI_int8_2bcd(pads, io, 0, 3))  goto write_err;
  sfwrite(io, tmp, 18);
  sfputc(io, 0xFF);

  if (2 != PDCI_int8_2bcd(pads, io, P_MIN_INT8, 3)) goto write_err;
  sfwrite(io, tmp, 18);
  sfputc(io, 0xFF);

  if (2 != PDCI_int8_2bcd(pads, io, P_MAX_INT8, 3)) goto write_err;
  sfwrite(io, tmp, 18);
  sfputc(io, 0xFF);

  if (2 != PDCI_uint8_2bcd(pads, io, P_MAX_UINT8, 3)) goto write_err;
  sfwrite(io, tmp, 18);
  sfputc(io, 0xFF);

  if (3 != PDCI_int16_2bcd(pads, io, P_MIN_INT16, 5)) goto write_err;
  sfwrite(io, tmp, 17);
  sfputc(io, 0xFF);

  if (3 != PDCI_int16_2bcd(pads, io, P_MAX_INT16, 5)) goto write_err;
  sfwrite(io, tmp, 17);
  sfputc(io, 0xFF);

  if (3 != PDCI_uint16_2bcd(pads, io, P_MAX_UINT16, 5)) goto write_err;
  sfwrite(io, tmp, 17);
  sfputc(io, 0xFF);

  if (6 != PDCI_int32_2bcd(pads, io, P_MIN_INT32, 11)) goto write_err;
  sfwrite(io, tmp, 14);
  sfputc(io, 0xFF);

  if (5 != PDCI_int32_2bcd(pads, io, P_MAX_INT32, 10)) goto write_err;
  sfwrite(io, tmp, 15);
  sfputc(io, 0xFF);

  if (5 != PDCI_uint32_2bcd(pads, io, P_MAX_UINT32, 10)) goto write_err;
  sfwrite(io, tmp, 15);
  sfputc(io, 0xFF);

  if (10 != PDCI_int64_2bcd(pads, io, P_MIN_INT64, 19)) goto write_err;
  sfwrite(io, tmp, 10);
  sfputc(io, 0xFF);

  if (10 != PDCI_int64_2bcd(pads, io, P_MAX_INT64, 19)) goto write_err;
  sfwrite(io, tmp, 10);
  sfputc(io, 0xFF);

  if (10 != PDCI_uint64_2bcd(pads, io, P_MAX_UINT64, 20)) goto write_err;
  sfwrite(io, tmp, 10);
  sfputc(io, 0xFF);

  sfclose(io);
  return 0;

 write_err:
  error(0, "XXX write error occurred");
  sfclose(io);
  return -1;
}
