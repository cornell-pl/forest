#include "libpadsc-internal.h"

int main(int argc, char** argv) {
  int                i;
  PDC_t*             pdc;
  PDC_int32_acc      accum;
  PDC_int32          key1;
  PDC_base_ed        ed = {0};

  if (PDC_ERROR == PDC_open(0, &pdc)) {
    error(2, "*** PDC_open failed ***");
    exit(-1);
  }
  error(0, "\n\ninit the accum\n");
  if (PDC_ERROR == PDC_int32_acc_init(pdc, &accum, 0)) {
    error(2, "** init failed **");
    exit(-1);
  }

  error(0, "\n\nadd vals to the accum\n");
  ed.errCode = PDC_NO_ERROR;
  for (i = 0; i < 100000; i++) {
    for (key1 = -20; key1 < 100; key1 += 10) {
      if (PDC_ERROR == PDC_int32_acc_add(pdc, &accum, &ed, &key1, 0)) {
	error(0, "** accum_add failed **");
      }
      if (key1 <= 10) {
	if (PDC_ERROR == PDC_int32_acc_add(pdc, &accum, &ed, &key1, 0)) {
	  error(0, "** accum_add failed **");
	}
      }
      if (key1 <= 50) {
	if (PDC_ERROR == PDC_int32_acc_add(pdc, &accum, &ed, &key1, 0)) {
	  error(0, "** accum_add failed **");
	}
      }
    }
  }
  ed.errCode = PDC_INVALID_AINT;
  for (i = 0; i < 100000; i++) {
    for (key1 = -100; key1 < -40; key1 += 10) {
      if (PDC_ERROR == PDC_int32_acc_add(pdc, &accum, &ed, &key1, 0)) {
	error(0, "** accum_add failed **");
      }
    }
  }
  error(0, "\n\ndescribe the accum\n");
  if (PDC_ERROR == PDC_int32_acc_report(pdc, "foo_prefix", &accum, 0)) {
    error(0, "** accum_report failed **");
  }


  error(0, "\n\nadd MORE vals to the accum\n");
  ed.errCode = PDC_NO_ERROR;
  for (i = 0; i < 100000; i++) {
    for (key1 = -20; key1 < 100; key1 += 10) {
      if (PDC_ERROR == PDC_int32_acc_add(pdc, &accum, &ed, &key1, 0)) {
	error(0, "** accum_add failed **");
      }
      if (key1 <= 10) {
	if (PDC_ERROR == PDC_int32_acc_add(pdc, &accum, &ed, &key1, 0)) {
	  error(0, "** accum_add failed **");
	}
      }
      if (key1 <= 50) {
	if (PDC_ERROR == PDC_int32_acc_add(pdc, &accum, &ed, &key1, 0)) {
	  error(0, "** accum_add failed **");
	}
      }
    }
  }
  ed.errCode = PDC_INVALID_AINT;
  for (i = 0; i < 100000; i++) {
    for (key1 = -100; key1 < -40; key1 += 10) {
      if (PDC_ERROR == PDC_int32_acc_add(pdc, &accum, &ed, &key1, 0)) {
	error(0, "** accum_add failed **");
      }
    }
  }
  error(0, "\n\ndescribe the accum\n");
  if (PDC_ERROR == PDC_int32_acc_report(pdc, "foo_prefix", &accum, 0)) {
    error(0, "** accum_report failed **");
  }

  if (PDC_ERROR == PDC_close(pdc, 0)) {
    error(2, "*** PDC_close failed ***");
    exit(-1);
  }
  return 0;
}
