#include "libpadsc.h"
#include "depends.h"
#define PADS_TY line
#define PADS_TY_INIT line_init
#define PADS_TY_READ line_read
#define PADS_TY_CLEANUP line_cleanup
#define PADS_TY_ED line_ed
#define PADS_TY_ED_INIT line_ed_init
#define PADS_TY_ED_CLEANUP line_ed_cleanup
#define PADS_TY_ACC line_acc
#define PADS_TY_ACC_INIT line_acc_init
#define PADS_TY_ACC_ADD line_acc_add
#define PADS_TY_ACC_REPORT line_acc_report
#define PADS_TY_ACC_CLEANUP line_acc_cleanup
int main(int argc, char** argv) {
  PDC_t           *pdc;
  PADS_TY         rep;
  PADS_TY_ED      ed;
  PADS_TY_ACC     acc;
  char            *fileName;
  
  if (argc == 2) {
    fileName = argv[1];
    error(0, "Data file = %s\n", fileName);
  } else {
    fileName = "/dev/stdin";
    error(0, "Data file = standard in\n");
  }

  if (PDC_ERR == PDC_open(&pdc, 0, 0)) {
    error(2, "*** PDC_open failed ***");
    exit(-1);
  }

  if (PDC_ERR == PDC_IO_fopen(pdc, fileName)) {
    error(2, "*** PDC_IO_fopen failed ***");
    exit(-1);
  }

  if (PDC_ERR == PADS_TY_INIT(pdc, &rep)) {
    error(2, "*** representation initialization failed ***");
    exit(-1);
  }

  if (PDC_ERR == PADS_TY_ED_INIT(pdc, &ed)) {
    error(2, "*** error description initialization failed ***");
    exit(-1);
  }

  if (PDC_ERR == PADS_TY_ACC_INIT(pdc, &acc)) {
    error(2, "*** accumulator initialization failed ***");
    exit(-1);
  }

  /*
   * Try to read each line of data
   */
  while (!PDC_IO_at_EOF(pdc)) {
    if (PDC_OK != PADS_TY_READ(pdc, 0, &ed, &rep)) {
      error(2, "read returned error");
    }
    /* accum both good and bad vals */
    if (PDC_ERR == PADS_TY_ACC_ADD(pdc, &acc, &ed, &rep)) {
      error(2, "*** accumulator add failed ***");
      exit(-1);
    }	
  }
  if (PDC_ERR == PADS_TY_ACC_REPORT(pdc, "", 0, 0, &acc)) {
    error(0, "** accum_report failed **");
  }

  if (PDC_ERR == PDC_IO_fclose(pdc)) {
    error(2, "*** PDC_IO_fclose failed ***");
    exit(-1);
  }

  if (PDC_ERR == PADS_TY_CLEANUP(pdc, &rep)) {
    error(0, "** representation cleanup failed **");
  }

  if (PDC_ERR == PADS_TY_ED_CLEANUP(pdc, &ed)) {
    error(0, "** error descriptor cleanup failed **");
  }

  if (PDC_ERR == PADS_TY_ACC_CLEANUP(pdc, &acc)) {
    error(0, "** accumulator cleanup failed **");
  }

  if (PDC_ERR == PDC_close(pdc)) {
    error(2, "*** PDC_close failed ***");
    exit(-1);
  }

  return 0;
}
