#include "padsc.h"
#include "test2out.h"

int main(int argc, char** argv) {
  PDC_t             *pdc;
  PDC_disc_t         my_disc = PDC_default_disc;
  PDC_IO_disc_t     *io_disc;

  hdr_rec            hdr;
  hdr_rec_pd         hdr_pd;
  hdr_rec_m          hdr_m;

  magic_rec          magic;
  magic_rec_pd       magic_pd;
  magic_rec_m        magic_m;

  data_rec           data;
  data_rec_pd        data_pd;
  data_rec_m         data_m;

  if (argc != 2) {
    goto usage;
  }
  io_disc = PDC_fwrec_make(0, 12, 1);
  PDC_open(&pdc,&my_disc,io_disc);
  error(0, "\nOpening data file %s\n", argv[1]);
  if (PDC_ERR == PDC_IO_fopen(pdc, argv[1])) {
    error(ERROR_FATAL, "*** PDC_IO_fopen failed ***");
    return -1;
  }

  hdr_rec_init(pdc, &hdr);
  hdr_rec_pd_init(pdc, &hdr_pd);
  hdr_rec_m_init(pdc, &hdr_m, PDC_CheckAndSet);

  magic_rec_init(pdc, &magic);
  magic_rec_pd_init(pdc, &magic_pd);
  magic_rec_m_init(pdc, &magic_m, PDC_CheckAndSet);

  data_rec_init(pdc, &data);
  data_rec_pd_init(pdc, &data_pd);
  data_rec_m_init(pdc, &data_m, PDC_CheckAndSet);

  /* Read the hdr rec */
  if (PDC_ERR == hdr_rec_read(pdc, &hdr_m, &hdr_pd, &hdr)) {
    error(ERROR_FATAL, "*** hdr_rec_read failed ***");
  }

  /* Read the magic rec */
  if (PDC_ERR == magic_rec_read(pdc, &magic_m, &magic_pd, &magic)) {
    error(ERROR_FATAL, "*** magic_rec_read failed ***");
  }

  /*
   * Try to read each line of data
   */
  while (!PDC_IO_at_EOF(pdc)) {
    /* PDC_IO_checkpoint(pdc, 0); */
    if (PDC_ERR == data_rec_read(pdc, &data_m, &data_pd, &data)) {
      error(ERROR_FATAL, "*** data_rec_read failed ***");
    }
    /* output data description */
    error(0, "*** TODO: describe data ***");
    switch (data.switch_val) {
    case -1:
      error(0, "switch_val -1  qy1 = %s", PDC_fmt_str(&(data.arms.val.a1.qy1)));
      break;
    case 0:
      error(0, "switch_val  0  qy2 = %s", PDC_fmt_str(&(data.arms.val.a2.qy2)));
      break;
    case 1:
      error(0, "switch_val  1  qy3 = %s  qy4 = %s",
	    PDC_fmt_str(&(data.arms.val.a3.qy3)),
	    PDC_fmt_str(&(data.arms.val.a3.qy4)));
      break;
    }
    /* PDC_IO_commit(pdc); */
  }

  if (PDC_ERR == PDC_IO_close(pdc)) {
    error(ERROR_FATAL, "*** PDC_IO_close failed ***");
  }

  if (PDC_ERR == PDC_close(pdc)) {
    error(ERROR_FATAL, "*** PDC_close failed ***");
  }

  return 0;

 usage:
  error(0,
	"\nUsage: print_test2out <data_file>"
	"\n    "
	"\n    Where <data_file> could be /dev/stdin or a file pathname"
	"\n    ");
  return -1;
}
