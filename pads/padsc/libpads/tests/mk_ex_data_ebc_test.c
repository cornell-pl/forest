#include "libpadsc-internal.h" /* for testing - normally do not include internal */
#include <stdio.h>

int main(int argc, char** argv) {
  PDC_t*          pdc;
  const char* fname = "../../data/ex_data.ebc_test";
  char tmp[100];
  Sfio_t* io;
  int len;

  for (len = 0; len < 100; len++) {
    tmp[len] = PDC_EBCDIC_SPACE;
  }

  if (PDC_ERR == PDC_open(&pdc, 0, 0)) {
    error(2, "*** PDC_open failed ***");
    return -1;
  }

  printf("fname = %s\n", fname);
  io = sfopen(0, fname, "w");

  if (3 != PDCI_int8_2ebc(pdc, io, 0, 3))  goto write_err;
  sfwrite(io, tmp, 17);
  sfputc(io, PDC_EBCDIC_NEWLINE);

  if (3 != PDCI_int8_2ebc(pdc, io, PDC_MIN_INT8, 3)) goto write_err;
  sfwrite(io, tmp, 17);
  sfputc(io, PDC_EBCDIC_NEWLINE);

  if (3 != PDCI_int8_2ebc(pdc, io, PDC_MAX_INT8, 3)) goto write_err;
  sfwrite(io, tmp, 17);
  sfputc(io, PDC_EBCDIC_NEWLINE);

  if (3 != PDCI_uint8_2ebc(pdc, io, PDC_MAX_UINT8, 3)) goto write_err;
  sfwrite(io, tmp, 17);
  sfputc(io, PDC_EBCDIC_NEWLINE);

  if (5 != PDCI_int16_2ebc(pdc, io, PDC_MIN_INT16, 5)) goto write_err;
  sfwrite(io, tmp, 15);
  sfputc(io, PDC_EBCDIC_NEWLINE);

  if (5 != PDCI_int16_2ebc(pdc, io, PDC_MAX_INT16, 5)) goto write_err;
  sfwrite(io, tmp, 15);
  sfputc(io, PDC_EBCDIC_NEWLINE);

  if (5 != PDCI_uint16_2ebc(pdc, io, PDC_MAX_UINT16, 5)) goto write_err;
  sfwrite(io, tmp, 15);
  sfputc(io, PDC_EBCDIC_NEWLINE);

  if (10 != PDCI_int32_2ebc(pdc, io, PDC_MIN_INT32, 10)) goto write_err;
  sfwrite(io, tmp, 10);
  sfputc(io, PDC_EBCDIC_NEWLINE);

  if (10 != PDCI_int32_2ebc(pdc, io, PDC_MAX_INT32, 10)) goto write_err;
  sfwrite(io, tmp, 10);
  sfputc(io, PDC_EBCDIC_NEWLINE);

  if (10 != PDCI_uint32_2ebc(pdc, io, PDC_MAX_UINT32, 10)) goto write_err;
  sfwrite(io, tmp, 10);
  sfputc(io, PDC_EBCDIC_NEWLINE);

  if (19 != PDCI_int64_2ebc(pdc, io, PDC_MIN_INT64, 19)) goto write_err;
  sfwrite(io, tmp, 1);
  sfputc(io, PDC_EBCDIC_NEWLINE);

  if (19 != PDCI_int64_2ebc(pdc, io, PDC_MAX_INT64, 19)) goto write_err;
  sfwrite(io, tmp, 1);
  sfputc(io, PDC_EBCDIC_NEWLINE);

  if (20 != PDCI_uint64_2ebc(pdc, io, PDC_MAX_UINT64, 20)) goto write_err;
  sfputc(io, PDC_EBCDIC_NEWLINE);

  sfclose(io);
  return 0;

 write_err:
  error(0, "XXX write error occurred");
  sfclose(io);
  return -1;
}
