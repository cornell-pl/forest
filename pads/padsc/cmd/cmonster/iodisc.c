/*
 * io discipline helpers
 * 
 * Kathleen Fisher, Robert Gruber
 * AT&T Labs Research
 */

#include "cmonster.h"

int
CMR_open_iodisc(CMDLINE_iodisc_spec *ispec, PDC_IO_disc_t ** iodisc_out) {

  PDC_IO_disc_t *io_disc = 0;

  switch (ispec->iodisc) {

  case fwrec: {
    size_t    leader_len, data_len, trailer_len;
    if (ispec->params.length != 3) {
      error(0, "\nio discipline fwrec requires 3 params, e.g., fwrec(:0,24,1:)");
      return -1;
    }
    leader_len      = ispec->params.elts[0];
    data_len        = ispec->params.elts[1];
    trailer_len     = ispec->params.elts[2];
    io_disc = PDC_fwrec_make(leader_len, data_len, trailer_len);
    break;
  }

  case fwrec_noseek: {
    size_t    leader_len, data_len, trailer_len;
    if (ispec->params.length != 3) {
      error(0, "\nio discipline fwrec_noseek requires 3 params, e.g., fwrec_noseek(:0,24,1:)");
      return -1;
    }
    leader_len      = ispec->params.elts[0];
    data_len        = ispec->params.elts[1];
    trailer_len     = ispec->params.elts[2];
    io_disc = PDC_fwrec_noseek_make(leader_len, data_len, trailer_len);
    break;
  }

  case nlrec: {
    size_t    block_size_hint;
    if (ispec->params.length != 1) {
      error(0, "\nio discipline nlrec requires 1 params, e.g., nlrec(:0:)");
      return -1;
    }
    block_size_hint = ispec->params.elts[0];
    io_disc = PDC_nlrec_make(block_size_hint);
    break;
  }

  case nlrec_noseek: {
    size_t    block_size_hint;
    if (ispec->params.length != 1) {
      error(0, "\nio discipline nlrec_noseek requires 1 params, e.g., nlrec_noseek(:0:)");
      return -1;
    }
    block_size_hint = ispec->params.elts[0];
    io_disc = PDC_nlrec_noseek_make(block_size_hint);
    break;
  }

  case ctrec: {
    PDC_byte  term_char;
    size_t    block_size_hint;
    if (ispec->params.length != 2) {
      error(0, "\nio discipline ctrec requires 2 params, e.g., ctrec(:10,0:) ");
      return -1;
    }
    term_char       = ispec->params.elts[0];
    block_size_hint = ispec->params.elts[1];
    io_disc = PDC_ctrec_make(term_char, block_size_hint);
    break;
  }

  case ctrec_noseek: {
    PDC_byte  term_char;
    size_t    block_size_hint;
    if (ispec->params.length != 2) {
      error(0, "\nio discipline ctrec_noseek requires 2 params, e.g., ctrec_noseek(:10,0:) ");
      return -1;
    }
    term_char       = ispec->params.elts[0];
    block_size_hint = ispec->params.elts[1];
    io_disc = PDC_ctrec_noseek_make(term_char, block_size_hint);
    break;
  }

  case vlrec: {
    int       blocked;
    size_t    avg_rlen_hint;
    if (ispec->params.length != 2) {
      error(0, "\nio discipline vlrec requires 2 params, e.g., vlrec(:0,0:) ");
      return -1;
    }
    blocked         = ispec->params.elts[0];
    avg_rlen_hint   = ispec->params.elts[1];
    io_disc = PDC_vlrec_make(blocked, avg_rlen_hint);
    break;
  }

  case vlrec_noseek: {
    int       blocked;
    size_t    avg_rlen_hint;
    if (ispec->params.length != 2) {
      error(0, "\nio discipline vlrec_noseek requires 2 params, e.g., vlrec_noseek(:0,0:) ");
      return -1;
    }
    blocked         = ispec->params.elts[0];
    avg_rlen_hint   = ispec->params.elts[1];
    io_disc = PDC_vlrec_noseek_make(blocked, avg_rlen_hint);
    break;
  }

  }

  (*iodisc_out) = io_disc;
  return 0;
}
