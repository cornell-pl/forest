#include "libpadsc-internal.h"

const char* names[] = {
  "foo",
  "bobozot",
  "hi there Greg",
  "testing" ,
  "woo hoo hoo hoo",
  "link",
  "a",
  "nimbus 200",
  "bjork bjork bjorken",
  "mr ed",
  "frankly my dear",
  "zot",
}; 

const char* mymap(PDC_int32 v) {
  int idx = (v + 20)/10; /* -20 -> 0 ; 90 -> 11 */
  return names[idx];
}

int main(int argc, char** argv) {
  int                i;
  PDC_t*             pdc;
  PDC_disc_t*        disc = 0;
  PDC_int32_acc      accum;
  PDC_int32          key1;
  PDC_base_ed        ed = {0};

  if (PDC_ERR == PDC_open(&pdc, 0, 0)) {
    error(2, "*** PDC_open failed ***");
    exit(-1);
  }
  if (!disc) {
    disc = pdc->disc;
  }
  error(0, "\n\ninit the accum\n");
  if (PDC_ERR == PDC_int32_acc_init(pdc, &accum)) {
    error(2, "** init failed **");
    exit(-1);
  }

  error(0, "\n\nadd vals to the accum\n");
  ed.errCode = PDC_NO_ERR;
  for (i = 0; i < 100000; i++) {
    for (key1 = -20; key1 < 100; key1 += 10) {
      if (PDC_ERR == PDC_int32_acc_add(pdc, &accum, &ed, &key1)) {
	error(0, "** accum_add failed **");
      }
      if (key1 <= 10) {
	if (PDC_ERR == PDC_int32_acc_add(pdc, &accum, &ed, &key1)) {
	  error(0, "** accum_add failed **");
	}
      }
      if (key1 <= 50) {
	if (PDC_ERR == PDC_int32_acc_add(pdc, &accum, &ed, &key1)) {
	  error(0, "** accum_add failed **");
	}
      }
    }
  }
  ed.errCode = PDC_INVALID_AINT;
  for (i = 0; i < 100000; i++) {
    for (key1 = -100; key1 < -40; key1 += 10) {
      if (PDC_ERR == PDC_int32_acc_add(pdc, &accum, &ed, &key1)) {
	error(0, "** accum_add failed **");
      }
    }
  }
  error(0, "\n\ndescribe the accum\n");
  if (PDC_ERR == PDC_int32_acc_report (pdc, "foo_prefix", 0, 0, &accum)) {
    error(0, "** accum_report failed **");
  }


  error(0, "\n\nadd MORE vals to the accum\n");
  ed.errCode = PDC_NO_ERR;
  for (i = 0; i < 100000; i++) {
    for (key1 = -20; key1 < 100; key1 += 10) {
      if (PDC_ERR == PDC_int32_acc_add(pdc, &accum, &ed, &key1)) {
	error(0, "** accum_add failed **");
      }
      if (key1 <= 10) {
	if (PDC_ERR == PDC_int32_acc_add(pdc, &accum, &ed, &key1)) {
	  error(0, "** accum_add failed **");
	}
      }
      if (key1 <= 50) {
	if (PDC_ERR == PDC_int32_acc_add(pdc, &accum, &ed, &key1)) {
	  error(0, "** accum_add failed **");
	}
      }
    }
  }
  ed.errCode = PDC_INVALID_AINT;
  for (i = 0; i < 100000; i++) {
    for (key1 = -100; key1 < -40; key1 += 10) {
      if (PDC_ERR == PDC_int32_acc_add(pdc, &accum, &ed, &key1)) {
	error(0, "** accum_add failed **");
      }
    }
  }
  error(0, "\n\ndescribe the accum\n");
  if (PDC_ERR == PDC_int32_acc_report_map (pdc, "foo", "union tag", 0, mymap, &accum)) {
    error(0, "** accum_report failed **");
  }

  if (PDC_ERR == PDC_close(pdc)) {
    error(2, "*** PDC_close failed ***");
    exit(-1);
  }
  return 0;
}
