#include "dalpiv-prefix.h"
#define READ_MASK P_CheckAndSet

#ifndef DEF_INPUT_FILE
#  define DEF_INPUT_FILE "/dev/stdin"
#endif

#ifndef DEF_OUTPUT_FILE
#  define DEF_OUTPUT_FILE "/dev/stdout"
#endif


int main(int argc, char** argv) {
  P_t              *pads;
  Pdisc_t           my_disc = Pdefault_disc;
  Pio_disc_t       *io_disc = 0;

  entry_t           rep;
  entry_t_pd        pd;
  entry_t_m         m;
  char             *fileName = 0;
  int              count = 0;

  if (argc >= 2) {
    fileName = argv[1];
  } else {
    fileName = DEF_INPUT_FILE;
  }
  error(0, "\nData file = %s\n", fileName);

  if (argc == 3) {
    outName = argv[2];
  } else {
    outName = DEF_OUTPUT_FILE;
  }
  error(0, "Output file = %s\n", outName);


  /* Initialize PADS library */
  io_disc = P_nlrec_make(0);
  P_open(&pads, &my_disc, io_disc);

  /* Initialize in-memory representation, parse descriptor, and mask */
  entry_t_init(pads, &rep);
  entry_t_pd_init(pads, &pd);
  entry_t_m_init(pads, &m, READ_MASK);

  /* Open data file */
  if (P_ERR == P_io_fopen(pads, fileName)) {
    error(ERROR_FATAL, "*** P_io_fopen failed ***");
  }

  /* Open output file */
  if (!(io = P_fopen(outName, "w"))) {
    P_SYSERR1(pads->disc, "Failed to open output file \"%s\" for writing", outName);
  }

  /* Read each line of data  */
  while (!P_io_at_eof(pads)) {
    if (P_OK == entry_t_read(pads, &m, &pd, &rep)) {
      if (P_ERR == Keep_t_write2io(pads, io, &pd->keep, &rep->keep)) {
        error(ERROR_FATAL, "*** IO error during write");
    }
  };
  P_io_close(pads);

  entry_t_cleanup(pads, &rep);
  entry_t_pd_cleanup(pads, &pd);
  P_close(pads);
  return 0;
}
