#include "padsc-internal.h"

static const char* names[] = {
  "bob",
  "kathleen",
  "foo",
  "foobar",
  "ba",
  "bar",
  "sam I am",
  "cat in the hat",
  "a",
  ""
};

int main(int argc, char** argv) {
  char*        nm;
  int                i;
  PDC_t*             pdc;
  PDC_string_acc     accum;
  PDC_string         key1;
  PDC_base_pd        pd = {0};

  if (PDC_ERR == PDC_open(&pdc, 0, 0)) {
    error(2, "*** PDC_open failed ***");
    exit(-1);
  }
  error(0, "\ninit the accum");
  if (PDC_ERR == PDC_string_acc_init(pdc, &accum)) {
    error(2, "** init failed **");
    exit(-1);
  }

  error(0, "\nadd vals to the accum");
  pd.errCode = PDC_NO_ERR;
  for (i = 0; i < 100000; i++) {
    nm = (char*)names[i % 10];
    key1.str = nm;
    key1.len = strlen(nm);
    if (PDC_ERR == PDC_string_acc_add(pdc, &accum, &pd, &key1)) {
      error(0, "** accum_add failed **");
    }
    if (i % 10 < 3) {
      if (PDC_ERR == PDC_string_acc_add(pdc, &accum, &pd, &key1)) {
	error(0, "** accum_add failed **");
      }
    }
    if (i % 10 < 7) {
      if (PDC_ERR == PDC_string_acc_add(pdc, &accum, &pd, &key1)) {
	error(0, "** accum_add failed **");
      }
    }
  }
  pd.errCode = PDC_CHAR_LIT_NOT_FOUND; /* typical error for string term by char lit */
  for (i = 0; i < 100000; i++) {
    nm = (char*)names[i % 10];
    key1.str = nm;
    key1.len = strlen(nm);
    if (PDC_ERR == PDC_string_acc_add(pdc, &accum, &pd, &key1)) {
      error(0, "** accum_add failed **");
    }
  }
  error(0, "\ndescribe the accum");
  if (PDC_ERR == PDC_string_acc_report(pdc, "foo_prefix", 0, 0, &accum)) {
    error(0, "** accum_report failed **");
  }

  if (PDC_ERR == PDC_close(pdc)) {
    error(2, "*** PDC_close failed ***");
    exit(-1);
  }
  return 0;
}
