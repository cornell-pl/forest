#include "dibbler2.h"

#define FASTEST 1

typedef enum behave_e { count_first21, out_first21 } behave;

const char * behave_descr[] = {
  "count_first21",
  "out_first21"
 };

int main(int argc, char** argv) {
  PDC_t                    *pdc;
  PDC_IO_disc_t            *io_disc;
  out_sum_header           header;
  out_sum_fixed1           f1;
  out_sum_fixed1_ed        f1_ed;
  event                    ev;
  event_ed                 ev_ed;
  size_t                   bytes_skipped;
  char                     *fname          = "../../data/ex_data.dibbler1";
  behave                   b               = count_first21;
  unsigned long            good_21         = 0, good = 0, bad = 0;
  /* out_sum_fixed1_em        f1_em           = { 0 }; */
  out_sum_fixed1_em        f1_em           = {PDC_Ignore,{PDC_Ignore,PDC_Ignore},{PDC_Ignore,PDC_Ignore},{{PDC_Ignore,PDC_Ignore},{PDC_Ignore,PDC_Ignore}},{{PDC_Ignore,PDC_Ignore},{PDC_Ignore,PDC_Ignore}},{PDC_Ignore,PDC_Ignore},{{PDC_Ignore,PDC_Ignore},{PDC_Ignore,PDC_Ignore}},{{PDC_Ignore,PDC_Ignore},{PDC_Ignore,PDC_Ignore}}};

  if (argc > 3) {
    goto usage;
  }
  if (argc >= 2 ) {
    fname = argv[1];
  }
#ifndef FASTEST
  if (argc == 3 ) {
    if (strcmp(argv[2], "count_first21") == 0) {
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
    case count_first21: {
      while (!PDC_IO_at_EOF(pdc)) {
	if (PDC_OK == out_sum_fixed1_read(pdc, &f1_em, &f1_ed, &f1)) {
	  if (PDC_OK == event_read(pdc, 0, &ev_ed, &ev)) {
	    good++;
	    if (PDC_string_eq_Cstr(&(ev.state), "21")) {
	      good_21++;
	    } else {
	      /*	      error(0, "first state = %.*s", ev.state.len, ev.state.str); */
	    }
	  } else {
	    bad++;
	  }
	} else {
	  /*	  error(2, "data line read returned: error"); */
	  bad++;
	}
	if (PDC_ERR == PDC_IO_next_rec(pdc, &bytes_skipped)) {
#ifndef FASTEST
	  error(2, "Could not find EOR (newline), ending program");
#endif
	  break;
	}
      }
      error(0, "\ntest_fast_dib_mod: good_21: %lu  tot_good: %lu  tot_bad: %lu", good_21, good, bad);
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
  error(0, "\nUsage: %s <data-file> <behavior>\n\twhere behavior is one of: count_first21, out_first21\n", argv[0]);
  return -1;
}
