#include "padsc-internal.h" /* for testing - normally do not include internal */
#include <stdio.h>

int main(int argc, char** argv) {
  PDC_t*          pdc;
  const char* fname = "../../data/ex_data.ebcdic_test";
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

  if (-1 == (len = PDCI_int64_2e(pdc, io, 0)))  goto write_err;
  sfwrite(io, tmp, 20-len);
  sfputc(io, PDC_EBCDIC_NEWLINE);

  if (-1 == (len = PDCI_int64_2e(pdc, io, PDC_MIN_INT8))) goto write_err;
  sfwrite(io, tmp, 20-len);
  sfputc(io, PDC_EBCDIC_NEWLINE);

  if (-1 == (len = PDCI_int64_2e(pdc, io, PDC_MAX_INT8))) goto write_err;
  sfwrite(io, tmp, 20-len);
  sfputc(io, PDC_EBCDIC_NEWLINE);

  if (-1 == (len = PDCI_uint64_2e(pdc, io, PDC_MAX_UINT8))) goto write_err;
  sfwrite(io, tmp, 20-len);
  sfputc(io, PDC_EBCDIC_NEWLINE);

  if (-1 == (len = PDCI_int64_2e(pdc, io, PDC_MIN_INT16))) goto write_err;
  sfwrite(io, tmp, 20-len);
  sfputc(io, PDC_EBCDIC_NEWLINE);

  if (-1 == (len = PDCI_int64_2e(pdc, io, PDC_MAX_INT16))) goto write_err;
  sfwrite(io, tmp, 20-len);
  sfputc(io, PDC_EBCDIC_NEWLINE);

  if (-1 == (len = PDCI_uint64_2e(pdc, io, PDC_MAX_UINT16))) goto write_err;
  sfwrite(io, tmp, 20-len);
  sfputc(io, PDC_EBCDIC_NEWLINE);

  if (-1 == (len = PDCI_int64_2e(pdc, io, PDC_MIN_INT32))) goto write_err;
  sfwrite(io, tmp, 20-len);
  sfputc(io, PDC_EBCDIC_NEWLINE);

  if (-1 == (len = PDCI_int64_2e(pdc, io, PDC_MAX_INT32))) goto write_err;
  sfwrite(io, tmp, 20-len);
  sfputc(io, PDC_EBCDIC_NEWLINE);

  if (-1 == (len = PDCI_uint64_2e(pdc, io, PDC_MAX_UINT32))) goto write_err;
  sfwrite(io, tmp, 20-len);
  sfputc(io, PDC_EBCDIC_NEWLINE);

  if (-1 == (len = PDCI_int64_2e(pdc, io, PDC_MIN_INT64))) goto write_err;
  sfwrite(io, tmp, 20-len);
  sfputc(io, PDC_EBCDIC_NEWLINE);

  if (-1 == (len = PDCI_int64_2e(pdc, io, PDC_MAX_INT64))) goto write_err;
  sfwrite(io, tmp, 20-len);
  sfputc(io, PDC_EBCDIC_NEWLINE);

  if (-1 == (len = PDCI_uint64_2e(pdc, io, PDC_MAX_UINT64))) goto write_err;
  sfwrite(io, tmp, 20-len);
  sfputc(io, PDC_EBCDIC_NEWLINE);

  sfclose(io);
  return 0;

 write_err:
  error(0, "XXX write error occurred");
  sfclose(io);
  return -1;
}
