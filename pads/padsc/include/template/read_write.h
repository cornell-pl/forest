#include "out_macros.h"

#ifndef PDCI_MacroArg2String
#define PDCI_MacroArg2String(s) #s
#endif

#ifndef EXTRA_READ_ARGS
#  define EXTRA_READ_ARGS
#endif

#ifndef DEF_INPUT_FILE
#  define DEF_INPUT_FILE "/dev/stdin"
#endif

#ifndef DEF_OUTPUT_FILE
#  define DEF_OUTPUT_FILE "/dev/stdout"
#endif

#ifndef MAX_RECS
#  define MAX_RECS 0
#endif

int main(int argc, char** argv) {
  P_t              *pads;
  Pdisc_t           my_disc = Pdefault_disc;
  Pio_disc_t       *io_disc = 0;
  PADS_TY()         rep;
  PADS_TY(_pd)      pd;
  PADS_TY(_m)       m;
  Sfio_t           *io;
  char             *inName  = 0;
  char             *outName = 0;
  Puint64           num_recs = 0;

#ifdef WSPACE_OK
  my_disc.flags |= (Pflags_t)P_WSPACE_OK;
#endif
#ifdef COPY_STRINGS
  my_disc.copy_strings = 1;
#endif

#ifdef IO_DISC_MK
  if (!(io_disc = IO_DISC_MK)) {
    error(ERROR_FATAL, "IO discipline make call [ " PDCI_MacroArg2String(IO_DISC_MK) " ] failed");
  }
#ifdef IO_DISC_DESCR
  else { 
    error(0, "Installed " IO_DISC_DESCR);
  }
#endif
#endif

  if (argc >= 2) {
    inName = argv[1];
  } else {
    inName = DEF_INPUT_FILE;
  }
  error(0, "Input file = %s\n", inName);
  if (argc == 3) {
    outName = argv[2];
  } else {
    outName = DEF_OUTPUT_FILE;
  }
  error(0, "Output file = %s\n", outName);

  if (P_ERR == P_open(&pads, &my_disc, io_disc)) {
    error(ERROR_FATAL, "*** P_open failed ***");
  }
  if (P_ERR == P_io_fopen(pads, inName)) {
    error(ERROR_FATAL, "*** P_io_fopen failed ***");
  }
  if (!(io = P_fopen(outName, "w"))) {
    P_SYSERR1(pads->disc, "Failed to open output file \"%s\" for writing", outName);
  }
  if (P_ERR == PADS_TY(_init)(pads, &rep)) {
    error(ERROR_FATAL, "*** representation initialization failed ***");
  }
  if (P_ERR == PADS_TY(_pd_init)(pads, &pd)) {
    error(ERROR_FATAL, "*** parse description initialization failed ***");
  }
  /* init mask -- must do this! */
  PADS_TY(_m_init)(pads, &m, P_CheckAndSet);

  /*
   * Try to read each line of data
   */
  while (!P_io_at_eof(pads) && (MAX_RECS == 0 || num_recs++ < MAX_RECS)) {
    if (P_OK != PADS_TY(_read)(pads, &m, EXTRA_READ_ARGS &pd, &rep)) {
#ifdef EXTRA_BAD_READ_CODE
      EXTRA_BAD_READ_CODE;
#else
      error(2, "read returned error");
#endif
    }
    else {
      if (P_ERR == PADS_TY(_write2io)(pads, io, EXTRA_READ_ARGS &pd, &rep)) {
        error(ERROR_FATAL, "*** IO error during write");
      }
#ifdef EXTRA_GOOD_READ_CODE
      EXTRA_GOOD_READ_CODE;
#endif
    }
  }
  if (P_ERR == P_io_close(pads)) {
    error(ERROR_FATAL, "*** P_io_close failed ***");
  }
  if (P_ERR == PADS_TY(_cleanup)(pads, &rep)) {
    error(ERROR_FATAL, "** representation cleanup failed **");
  }
  if (P_ERR == PADS_TY(_pd_cleanup)(pads, &pd)) {
    error(ERROR_FATAL, "** parse descriptor cleanup failed **");
  }
  if (P_ERR == P_close(pads)) {
    error(ERROR_FATAL, "*** P_close failed ***");
  }
  sfclose(io);
  return 0;
}
