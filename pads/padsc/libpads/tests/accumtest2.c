#include "libpadsc-internal.h"

int main(int argc, char** argv) {
  int                i;
  PDC_t*             pdc;
  PDC_int8_acc       accum8;
  PDC_int8           key8;
  PDC_int16_acc      accum16;
  PDC_int16          key16;
  PDC_int32_acc      accum32;
  PDC_int32          key32;
  PDC_int64_acc      accum64;
  PDC_int64          key64;

  PDC_int8_acc       accum8min;
  PDC_int8           key8min;
  PDC_int16_acc      accum16min;
  PDC_int16          key16min;
  PDC_int32_acc      accum32min;
  PDC_int32          key32min;
  PDC_int64_acc      accum64min;
  PDC_int64          key64min;

  PDC_uint8_acc      accum8u;
  PDC_uint8          key8u;
  PDC_uint16_acc     accum16u;
  PDC_uint16         key16u;
  PDC_uint32_acc     accum32u;
  PDC_uint32         key32u;
  PDC_uint64_acc     accum64u;
  PDC_uint64         key64u;
  PDC_uint64_acc     accum64u20th;
  PDC_uint64         key64u20th;
  PDC_base_ed        ed = {0};

  if (PDC_ERR == PDC_open(&pdc, 0, 0)) {
    error(2, "*** PDC_open failed ***");
    exit(-1);
  }
  error(0, "\ninit all the accums");
  if (PDC_ERR == PDC_int8_acc_init(pdc, &accum8)) {
    error(2, "** accum8 init failed **");
    exit(-1);
  }
  if (PDC_ERR == PDC_int16_acc_init(pdc, &accum16)) {
    error(2, "** accum16 init failed **");
    exit(-1);
  }
  if (PDC_ERR == PDC_int32_acc_init(pdc, &accum32)) {
    error(2, "** accum32 init failed **");
    exit(-1);
  }
  if (PDC_ERR == PDC_int64_acc_init(pdc, &accum64)) {
    error(2, "** accum64 init failed **");
    exit(-1);
  }

  if (PDC_ERR == PDC_int8_acc_init(pdc, &accum8min)) {
    error(2, "** accum8min init failed **");
    exit(-1);
  }
  if (PDC_ERR == PDC_int16_acc_init(pdc, &accum16min)) {
    error(2, "** accum16min init failed **");
    exit(-1);
  }
  if (PDC_ERR == PDC_int32_acc_init(pdc, &accum32min)) {
    error(2, "** accum32min init failed **");
    exit(-1);
  }
  if (PDC_ERR == PDC_int64_acc_init(pdc, &accum64min)) {
    error(2, "** accum64min init failed **");
    exit(-1);
  }

  if (PDC_ERR == PDC_uint8_acc_init(pdc, &accum8u)) {
    error(2, "** accum8u init failed **");
    exit(-1);
  }
  if (PDC_ERR == PDC_uint16_acc_init(pdc, &accum16u)) {
    error(2, "** accum16u init failed **");
    exit(-1);
  }
  if (PDC_ERR == PDC_uint32_acc_init(pdc, &accum32u)) {
    error(2, "** accum32u init failed **");
    exit(-1);
  }
  if (PDC_ERR == PDC_uint64_acc_init(pdc, &accum64u)) {
    error(2, "** accum64u init failed **");
    exit(-1);
  }
  if (PDC_ERR == PDC_uint64_acc_init(pdc, &accum64u20th)) {
    error(2, "** accum64u20th init failed **");
    exit(-1);
  }

  error(0, "\nadd MIN/MAX vals to all the accums");
  ed.errCode = PDC_NO_ERR;
  for (i = 0; i < 100000; i++) {
    key8      = PDC_MAX_INT8;
    key16     = PDC_MAX_INT16;
    key32     = PDC_MAX_INT32;
    key64     = PDC_MAX_INT64;

    key8min   = PDC_MIN_INT8;
    key16min  = PDC_MIN_INT16;
    key32min  = PDC_MIN_INT32;
    key64min  = PDC_MIN_INT64;

    key8u     = PDC_MAX_UINT8;
    key16u    = PDC_MAX_UINT16;
    key32u    = PDC_MAX_UINT32;
    key64u    = PDC_MAX_UINT64;
    key64u20th = PDC_MAX_UINT64 / 20;

    if (PDC_ERR == PDC_int8_acc_add(pdc, &accum8, &ed, &key8)) {
      error(0, "** accum8 add failed **");
    }
    if (PDC_ERR == PDC_int16_acc_add(pdc, &accum16, &ed, &key16)) {
      error(0, "** accum16 add failed **");
    }
    if (PDC_ERR == PDC_int32_acc_add(pdc, &accum32, &ed, &key32)) {
      error(0, "** accum32 add failed **");
    }
    if (PDC_ERR == PDC_int64_acc_add(pdc, &accum64, &ed, &key64)) {
      error(0, "** accum64 add failed **");
    }

    if (PDC_ERR == PDC_int8_acc_add(pdc, &accum8min, &ed, &key8min)) {
      error(0, "** accum8min add failed **");
    }
    if (PDC_ERR == PDC_int16_acc_add(pdc, &accum16min, &ed, &key16min)) {
      error(0, "** accum16min add failed **");
    }
    if (PDC_ERR == PDC_int32_acc_add(pdc, &accum32min, &ed, &key32min)) {
      error(0, "** accum32min add failed **");
    }
    if (PDC_ERR == PDC_int64_acc_add(pdc, &accum64min, &ed, &key64min)) {
      error(0, "** accum64min add failed **");
    }

    if (PDC_ERR == PDC_uint8_acc_add(pdc, &accum8u, &ed, &key8u)) {
      error(0, "** accum8u add failed **");
    }
    if (PDC_ERR == PDC_uint16_acc_add(pdc, &accum16u, &ed, &key16u)) {
      error(0, "** accum16u add failed **");
    }
    if (PDC_ERR == PDC_uint32_acc_add(pdc, &accum32u, &ed, &key32u)) {
      error(0, "** accum32u add failed **");
    }
    if (PDC_ERR == PDC_uint64_acc_add(pdc, &accum64u, &ed, &key64u)) {
      error(0, "** accum64u add failed **");
    }
    if (PDC_ERR == PDC_uint64_acc_add(pdc, &accum64u20th, &ed, &key64u20th)) {
      error(0, "** accum64u20th add failed **");
    }
  }
  error(0, "\ndescribe the accums");

  if (PDC_ERR == PDC_int8_acc_report(pdc, "int8max", 0, 0, &accum8)) {
    error(0, "** accum8 report failed **");
  }
  if (PDC_ERR == PDC_int16_acc_report(pdc, "int16max", 0, 0, &accum16)) {
    error(0, "** accum16 report failed **");
  }
  if (PDC_ERR == PDC_int32_acc_report(pdc, "int32max", 0, 0, &accum32)) {
    error(0, "** accum32 report failed **");
  }
  if (PDC_ERR == PDC_int64_acc_report(pdc, "int64max", 0, 0, &accum64)) {
    error(0, "** accum64 report failed **");
  }

  if (PDC_ERR == PDC_int8_acc_report(pdc, "int8min", 0, 0, &accum8min)) {
    error(0, "** accum8min report failed **");
  }
  if (PDC_ERR == PDC_int16_acc_report(pdc, "int16min", 0, 0, &accum16min)) {
    error(0, "** accum16min report failed **");
  }
  if (PDC_ERR == PDC_int32_acc_report(pdc, "int32min", 0, 0, &accum32min)) {
    error(0, "** accum32min report failed **");
  }
  if (PDC_ERR == PDC_int64_acc_report(pdc, "int64min", 0, 0, &accum64min)) {
    error(0, "** accum64min report failed **");
  }

  if (PDC_ERR == PDC_uint8_acc_report(pdc, "uint8", 0, 0, &accum8u)) {
    error(0, "** accum8u report failed **");
  }
  if (PDC_ERR == PDC_uint16_acc_report(pdc, "uint16", 0, 0, &accum16u)) {
    error(0, "** accum16u report failed **");
  }
  if (PDC_ERR == PDC_uint32_acc_report(pdc, "uint32", 0, 0, &accum32u)) {
    error(0, "** accum32u report failed **");
  }
  if (PDC_ERR == PDC_uint64_acc_report(pdc, "uint64", 0, 0, &accum64u)) {
    error(0, "** accum64u report failed **");
  }
  if (PDC_ERR == PDC_uint64_acc_report(pdc, "uint64u20th", 0, 0, &accum64u20th)) {
    error(0, "** accum64u20th report failed **");
  }
  error(0, "NB ==> u64 20th = %llu", key64u20th);

  if (PDC_ERR == PDC_close(pdc)) {
    error(2, "*** PDC_close failed ***");
    exit(-1);
  }
  return 0;
}
