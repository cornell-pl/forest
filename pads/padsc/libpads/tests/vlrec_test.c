#include "padsc-internal.h"

int main(int argc, char** argv) {
  PDC_t*             pdc;
  PDC_IO_disc_t*     io_disc;
  PDC_disc_t         my_disc = PDC_default_disc;
  size_t             bytes_skipped;
  const char        *fname;
  int                blocked;


  /* fname = "../../data/ND01.HP57WD1.OAK.5247135.20010513134851"; */
  /*   fname = "/tmp/foo/ND01.HP57WD1.OAK.5247135.20010513134851.stripped"; */
  fname = "/tmp/foo/ONE1.HP568T1A.G0155V00.70312937.20020412111044";
  blocked = 1;

  if (argc > 3) goto usage;
  if (argc >= 2) {
    fname = argv[1];
  }
  if (argc == 3) {
    if (strcmp(argv[2], "0")==0) {
      blocked = 0;
    } else if (strcmp(argv[2], "1")==0) {
      blocked = 1;
    } else goto usage;
  }

  io_disc = PDC_vlrec_noseek_make(blocked, 0); /* no avg rlen hint */
  if (!io_disc) {
    error(ERROR_FATAL, "\nFailed to install IO discipline vlrec_noseek");
  } else {
    error(0, "\nInstalled IO discipline vlrec_noseek");
  }

  if (PDC_ERR == PDC_open(&pdc, &my_disc, io_disc)) {
    error(2, "*** PDC_open failed ***");
    return -1;
  }
  if (PDC_ERR == PDC_IO_fopen(pdc, (char*)fname)) {
    error(2, "*** PDC_IO_fopen failed ***");
    return -1;
  }

  while (1) {
    if (PDC_ERR == PDC_IO_next_rec(pdc, &bytes_skipped)) {
      error(2, "no next record, ending program");
      goto done;
    }
  }

 done:
  if (PDC_ERR == PDC_IO_close(pdc)) {
    error(2, "*** PDC_IO_close failed ***");
    return -1;
  }

  if (PDC_ERR == PDC_close(pdc)) {
    error(2, "*** PDC_close failed ***");
    return -1;
  }

  return 0;

 usage:
  error(2, "\nUsage: %s [ <fname> ] [ <blocked> ] \n\n\twhere <fname> is a data file and <blocked> is either 0 or 1\n",
	argv[0]);
  return -1;
}
