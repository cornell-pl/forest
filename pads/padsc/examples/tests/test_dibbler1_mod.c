#include "dibbler.h"

#define FASTEST 1

typedef enum behave_e { count_all, out_all, accum_all, count_first21, out_first21 } behave;

const char * behave_descr[] = {
  "count_all",
  "out_all",
  "accum_all",
  "count_first21",
  "out_first21"
 };

int main(int argc, char** argv) {
  PDC_t                    *pdc;
  PDC_IO_disc_t            *io_disc;
  out_sum_header           header;
  out_sum_data_line        dline;
  out_sum_data_line_ed     dline_ed;
  out_sum_data_line_acc    acc;
  char                     *fname = "../../data/ex_data.dibbler1";
  behave                   b = count_first21;
  unsigned long            good_21 = 0, good = 0, bad = 0;
  out_sum_data_line_csm dline_csm={PDC_Ignore,{PDC_Ignore,PDC_Ignore},{PDC_Ignore,PDC_Ignore},{{PDC_Ignore,PDC_Ignore},{PDC_Ignore,PDC_Ignore}},{{PDC_Ignore,PDC_Ignore},{PDC_Ignore,PDC_Ignore}},{PDC_Ignore,PDC_Ignore},{{PDC_Ignore,PDC_Ignore},{PDC_Ignore,PDC_Ignore}},{{PDC_Ignore,PDC_Ignore},{PDC_Ignore,PDC_Ignore}},PDC_Ignore,{{PDC_Ignore,PDC_Ignore,PDC_Ignore},PDC_Ignore},{{PDC_Ignore,PDC_Ignore},{PDC_Ignore,PDC_Ignore}},{{PDC_Ignore,PDC_Ignore},{PDC_Ignore,PDC_Ignore}},{{PDC_Ignore,PDC_Ignore},{PDC_Ignore,PDC_Ignore}},{PDC_Ignore,PDC_Ignore},PDC_Ignore};

  /* all we are interested in is the state field: */
  dline_csm.events.element.state = PDC_CheckAndSet;

  if (argc > 3) {
    goto usage;
  }
  if (argc >= 2 ) {
    fname = argv[1];
  }
#ifndef FASTEST
  if (argc == 3 ) {
    if (strcmp(argv[2], "count_all") == 0) {
      b = count_all;
    } else if (strcmp(argv[2], "out_all") == 0) {
      b = out_all;
    } else if (strcmp(argv[2], "accum_all") == 0) {
      b = accum_all;
    } else if (strcmp(argv[2], "count_first21") == 0) {
      b = count_first21;
    } else if (strcmp(argv[2], "out_first21") == 0) {
      b = out_first21;
    } else {
      goto usage;
    }
  }

  error(0, "\nUsing input file %s", fname);
  error(0, "\nUsing behavior %s", behave_descr[b]);
#endif

  io_disc = PDC_nlrec_noseek_make(0);
#ifndef FASTEST
  if (!io_disc) {
    error(ERROR_FATAL, "\nFailed to install IO discipline nlrec_noseek");
  } else {
    error(0, "\nInstalled IO discipline nlrec_noseek");
  }
#endif

  if (PDC_ERR == PDC_open(&pdc, 0, io_disc)) {
    error(2, "*** PDC_open failed ***");
    return -1;
  }
#ifdef FASTEST
  pdc->disc->errorf = 0;
#endif

  /* INIT dline -- must do this for all variable data types */
  out_sum_data_line_init(pdc, &dline);
  out_sum_data_line_ed_init(pdc, &dline_ed);
  if (b == accum_all) {
    out_sum_data_line_acc_init(pdc, &acc);
  }

  if (PDC_ERR == PDC_IO_fopen(pdc, fname)) {
    error(2, "*** PDC_IO_fopen failed ***");
    return -1;
  }

  /*
   * Try to read header
   */

  if (PDC_OK == out_sum_header_read(pdc, 0, 0, &header)) {
#ifndef FASTEST
    error(0, "reading header returned: OK");
#endif
  } else {
    error(2, "reading header returned: error");
  }

  /*
   * Try to read each line of data
   */
  switch (b) {
    case count_all: {
      while (!PDC_IO_at_EOF(pdc)) {
	if (PDC_OK == out_sum_data_line_read (pdc, 0, &dline_ed, &dline)) {
	  good++;
	} else {
	  bad++;
	}
	error(0, "\ngood: %lu bad: %lu", good, bad);
      }
    } break;

    case accum_all: {
      while (!PDC_IO_at_EOF(pdc)) {
	out_sum_data_line_read(pdc, 0, &dline_ed, &dline);
	out_sum_data_line_acc_add(pdc, &acc, &dline_ed, &dline);
      }
      out_sum_data_line_acc_report(pdc, "dline", 0, 0, &acc);
    } break;

    case out_all: {
      error(0, "\nout_all not implemented");
    } break;

    case count_first21: {
      while (!PDC_IO_at_EOF(pdc)) {
	if (PDC_OK == out_sum_data_line_read_internal (pdc, &dline_csm, &dline_ed, &dline)) {
	  /* do something with the data */
	  /* 	  error(0, "data line read returned OK, number of events = %d", dline.events.length); */
	  good++;
	  if (dline.events.length && PDC_string_eq_Cstr(&(dline.events.eventSeq[0].state), "21")) {
	    good_21++;
	  }
	} else {
	  /*	  error(2, "data line read returned: error"); */
	  bad++;
	}
      }
      error(0, "\ntest_dibbler1_mod: good_21: %lu  tot_good: %lu  tot_bad: %lu", good_21, good, bad);
    } break;

    case out_first21: {
      error(0, "\nout_first21 not implemented");
    } break;
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

 usage:
  error(0, "\nUsage: %s <data-file> <behavior>\n\twhere behavior is one of: count_all, out_all, accum_all, count_first21, out_first21\n", argv[0]);
  return -1;
}
