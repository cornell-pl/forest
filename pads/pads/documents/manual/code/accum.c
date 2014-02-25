/*@FILE wsl-accum-hand.tex */
/*@BEGIN wsl-accum-hand.tex */
#include "wsl.h"
#define DEF_INPUT_FILE  "data/wsl"

int main(int argc, char** argv) {
  P_t                  *pads;
  Pio_disc_t           *io_disc;
  entry_t              rep;
  entry_t_pd           pd;
  entry_t_m            mask;
  entry_t_acc          acc;
  char                 *fname = DEF_INPUT_FILE;

  io_disc = P_nlrec_noseek_make(0);
  P_open(&pads, 0, io_disc);

  entry_t_init(pads, &rep);
  entry_t_pd_init(pads, &pd);
  entry_t_m_init(pads, &mask, P_CheckAndSet);

  if (P_ERR == P_io_fopen(pads, fname)) {
    error(2, "*** P_io_fopen failed ***");
    return -1;
  }

  entry_t_acc_init(pads, &acc);
  while (!P_io_at_eof(pads)) {
    entry_t_read(pads, &mask, &pd, &rep);
    entry_t_acc_add(pads, &acc, &pd, &rep);
  };
  entry_t_acc_report(pads, "", 0, 0, &acc);

  P_io_close(pads);
  entry_t_cleanup(pads, &rep);
  entry_t_pd_cleanup(pads, &pd);
  entry_t_acc_cleanup(pads, &acc);
  P_close(pads);
  return 0;
}
/*@END wsl-accum-hand.tex */
