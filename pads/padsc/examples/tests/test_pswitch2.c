#include "pads.h"
#include "pswitch2.h"
#define PADS_TY choice
#define PADS_TY_INIT choice_init
#define PADS_TY_READ choice_read
#define PADS_TY_CLEANUP choice_cleanup
#define PADS_TY_PD choice_pd
#define PADS_TY_M choice_m
#define PADS_TY_M_INIT choice_m_init
#define PADS_TY_PD_INIT choice_pd_init
#define PADS_TY_PD_CLEANUP choice_pd_cleanup
#define PADS_TY_ACC choice_acc
#define PADS_TY_ACC_INIT choice_acc_init
#define PADS_TY_ACC_ADD choice_acc_add
#define PADS_TY_ACC_REPORT choice_acc_report
#define PADS_TY_ACC_CLEANUP choice_acc_cleanup

int main(int argc, char** argv) {
  P_t           *pads;
  Pdisc_t      my_disc = Pdefault_disc;
  PADS_TY         rep;
  PADS_TY_PD      pd;
  PADS_TY_M       m;
  PADS_TY_ACC     acc;
  char            *fileName;
  
  my_disc.flags |= (Pflags_t)P_WSPACE_OK;

  if (P_ERR == P_open(&pads, &my_disc, 0)) {
    error(2, "*** P_open failed ***");
    exit(-1);
  }

  if (argc == 2) {
    fileName = argv[1];
    error(0, "Data file = %s\n", fileName);
  } else {
    fileName = "/dev/stdin";
    error(0, "Data file = standard in\n");
  }

  if (P_ERR == P_io_fopen(pads, fileName)) {
    error(2, "*** P_io_fopen failed ***");
    exit(-1);
  }

  if (P_ERR == PADS_TY_INIT(pads, &rep)) {
    error(2, "*** representation initialization failed ***");
    exit(-1);
  }

  if (P_ERR == PADS_TY_PD_INIT(pads, &pd)) {
    error(2, "*** error description initialization failed ***");
    exit(-1);
  }

  if (P_ERR == PADS_TY_ACC_INIT(pads, &acc)) {
    error(2, "*** accumulator initialization failed ***");
    exit(-1);
  }

  /* init mask -- must do this! */
  PADS_TY_M_INIT(pads, &m, P_CheckAndSet);

  /*
   * Try to read each line of data
   */
  while (!P_io_at_eof(pads)) {
    if (P_OK != PADS_TY_READ(pads, &m, &pd, &rep)) {
      error(2, "read returned error");
    }
    /* accum both good and bad vals */
    if (P_ERR == PADS_TY_ACC_ADD(pads, &acc, &pd, &rep)) {
      error(2, "*** accumulator add failed ***");
      exit(-1);
    }	
  }
  if (P_ERR == PADS_TY_ACC_REPORT(pads, "", 0, 0, &acc)) {
    error(0, "** accum_report failed **");
  }

  if (P_ERR == P_io_close(pads)) {
    error(2, "*** P_io_close failed ***");
    exit(-1);
  }

  if (P_ERR == PADS_TY_CLEANUP(pads, &rep)) {
    error(0, "** representation cleanup failed **");
  }

  if (P_ERR == PADS_TY_PD_CLEANUP(pads, &pd)) {
    error(0, "** error descriptor cleanup failed **");
  }

  if (P_ERR == PADS_TY_ACC_CLEANUP(pads, &acc)) {
    error(0, "** accumulator cleanup failed **");
  }

  if (P_ERR == P_close(pads)) {
    error(2, "*** P_close failed ***");
    exit(-1);
  }

  return 0;
}
