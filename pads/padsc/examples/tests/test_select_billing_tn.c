#include "dibbler.h"

#define TEST_VAL 9733608667ULL

int main(int argc, char** argv) {
  PDC_t                    *pdc;
  PDC_disc_t               *disc;
  PDC_IO_disc_t            *io_disc;
  out_sum_header           header;
  out_sum_header_ed        header_ed;
  out_sum_header_em        header_em = { 0 };
  out_sum_data_line        dline;
  out_sum_data_line_ed     dline_ed;
  out_sum_data_line_em     dline_em = { 0 };
  char                    *fname = "../../data/ex_data.dibbler1";
  int                      commit;

  error(0, "\nUsing input file %s", fname);

  disc = &PDC_default_disc;
  io_disc = PDC_nlrec_noseek_make(0);

  if (!io_disc) {
    error(ERROR_FATAL, "\nFailed to install IO discipline nlrec_noseek");
  } else {
    error(0, "\nInstalled IO discipline nlrec_noseek");
  }

  if (PDC_ERR == PDC_open(&pdc, disc, io_disc)) {
    error(ERROR_FATAL, "*** PDC_open failed ***");
  }
  if (PDC_ERR == PDC_IO_fopen(pdc, fname)) {
    error(2, "*** PDC_IO_fopen failed ***");
    return -1;
  }

  /* INIT all data and ed types */
  out_sum_header_init(pdc, &header);
  out_sum_header_ed_init(pdc, &header_ed);
  out_sum_data_line_init(pdc, &dline);
  out_sum_data_line_ed_init(pdc, &dline_ed);

  /*
   * Try to read header
   */

  if (PDC_OK == out_sum_header_read(pdc, &header_em, &header_ed, &header)) {
    error(0, "reading header returned: OK");
  } else {
    error(2, "reading header returned: error");
  }

  /* Init list */
  /* could init a list here (list of pointer to TARGET_TYPE) */

  /*
   * Try to read each line of data
   */
  while (!PDC_IO_at_EOF(pdc)) {
    PDC_IO_checkpoint(pdc, 0);
    commit = 1;
    out_sum_data_line_read(pdc, &dline_em, &dline_ed, &dline);
    /* Fields that contribute to the query expression must be error free, 
       even though entire order need not be error free.
       What does it mean if you say nothing about errors at all */
    if (dline_ed.billing_tn.nerr == 0) {
      if (dline.billing_tn.tag == yesPN) { /* should be dib_pn_vbar_yesPN ??? */
	if (dline.billing_tn.val.yesPN.val == TEST_VAL) {
	  /* could call a copy method here and add to list */
	  /* PDC_IO_commit_dump(pdc, sfstdout); */
	  PDC_IO_commit(pdc);
	  commit = 0;
	}
      }
    } else {
      /* skip it b/c billing_tn is invalid */
    }
    if (commit) PDC_IO_commit(pdc);
  }
  
  if (PDC_ERR == PDC_IO_close(pdc)) {
    error(2, "*** PDC_IO_close failed ***");
    return -1;
  }

  if (PDC_ERR == PDC_close(pdc)) {
    error(2, "*** PDC_close failed ***");
    return -1;
  }

  return 0;
}
