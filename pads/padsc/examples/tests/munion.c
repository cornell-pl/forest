#include "padsc.h"
#include "punion-manifest.h"
/* This program was generated by running
 * padsc examples/p/punion-manifest.p -a str2
 */
#define PADS_TY str2
#define PADS_TY_READ str2_read
#define PADS_TY_PD str2_pd
#define PADS_TY_M str2_m
#define PADS_TY_M_INIT str2_m_init
#define PADS_TY_ACC str2_acc
#define PADS_TY_ACC_INIT str2_acc_init
#define PADS_TY_ACC_ADD str2_acc_add
#define PADS_TY_ACC_REPORT str2_acc_report
#define PADS_TY_ACC_CLEANUP str2_acc_cleanup

/* XXX_REMOVE next 2 lines: */
#include "padsc-internal.h"
#define str2_m_init(pdc, mask_ptr, base_mask) PDCI_fill_mask((PDC_base_m*)mask_ptr, base_mask, sizeof(*(mask_ptr)))

int main(int argc, char** argv) {
  PDC_t*             pdc;
  PADS_TY            rep;
  PADS_TY_ACC        accum;
  PADS_TY_PD         pd = {0};
  PADS_TY_M          m;
  char*              fileName = 0;

  if (PDC_ERR == PDC_open(&pdc,0,0)) {
    error(2, "*** PDC_open failed ***");
    exit(-1);
  }

  if (argc == 2) {
    fileName = argv[1];
    error(0, "Data file = %s\n", fileName);
  } else {
    fileName = "/dev/stdin";
    error(0, "Data file = standard in\n");
  }

  if (PDC_ERR == PDC_IO_fopen(pdc, fileName)) {
    error(2, "*** PDC_IO_fopen failed ***");
    exit(-1);
  }

  error(0, "\nInitializing the accumulator");
  if (PDC_ERR == PADS_TY_ACC_INIT(pdc, &accum)) {
    error(2, "** init failed **");
    exit(-1);
  }

  /* init mask -- must do this! */
  PADS_TY_M_INIT(pdc, &m, PDC_CheckAndSet);

  /*
   * Try to read each line of data
   */
  while (!PDC_IO_at_EOF(pdc)) {
    error(0, "\nCalling read function");
    if (PDC_OK == PADS_TY_READ(pdc, &m, &pd, &rep)) {
      if (PDC_ERR == PADS_TY_ACC_ADD(pdc, &accum, &pd, &rep)) {
	error(0, "** accum_add failed **");
      }
    } else {
      error(2, "Read returned error");
      if (PDC_ERR == PADS_TY_ACC_ADD(pdc, &accum, &pd, &rep)) {
	error(0, "** accum_add failed **");
      }
    }
  }
  error(0, "\nFound eof");
  error(0, "\nDescribe the accumulator");

  if (PDC_ERR == PADS_TY_ACC_REPORT(pdc, "", 0, 0, &accum)) {
    error(0, "** accum_report failed **");
  }

  if (PDC_ERR == PADS_TY_ACC_CLEANUP(pdc, &accum)) {
    error(0, "** accumulator cleanup failed **");
  }

  if (PDC_ERR == PDC_IO_close(pdc)) {
    error(2, "*** PDC_IO_close failed ***");
    exit(-1);
  }

  if (PDC_ERR == PDC_close(pdc)) {
    error(2, "*** PDC_close failed ***");
    exit(-1);
  }

  return 0;
}
