
#include <ast.h>
#include <ast_common.h>
#include <sfio.h>
#include <regex.h>
#include <error.h>

#include "padsc-internal.h"

const char* false_true [] = { "FALSE", "TRUE" };

int main(int argc, char** argv) {
  PDC_t          *pdc;
  PDC_disc_t      my_disc = PDC_default_disc;
  PDC_IO_disc_t  *io_disc;
  PDC_regexp_t   *regexp;
  size_t          matchlen;
  int             i, n, eret, bor, eor;
  PDC_byte       *begin, *end;
  const char     *exp, *str;
  size_t          exp_len, str_len;

  io_disc = PDC_nlrec_make(0);
  if (PDC_ERR == PDC_open(&pdc, &my_disc, io_disc)) {
    error(ERROR_FATAL, "*** PDC_open failed ***");
  }

  if (argc != 3 && argc != 5) {
    error(ERROR_FATAL, "\nusage: test_re2 <pattern> <string> [ bor eor ]\n");
  }
  bor = eor = 0;
  if (argc == 5) {
    bor = atoi(argv[3]);
    eor = atoi(argv[4]);
  }
  exp      = argv[1];
  str      = argv[2];
  exp_len  = strlen(exp);
  str_len  = strlen(str);
  begin = (PDC_byte*)str;
  end   = (PDC_byte*)(str + str_len);

  if (PDC_ERR == PDC_regexp_compile(pdc, exp, &regexp)) {
    error(ERROR_FATAL, "Failed to compile re %s", PDC_qfmt_Cstr(exp, exp_len));
  }
  error(0, "\ncompiled regexp, nsub = %d\n", regexp->preg.re_nsub);

  eret = PDCI_regexp_match(pdc, regexp, begin, end, bor, eor, PDC_charset_ASCII, &matchlen);
  error(0, "match of RE %s against string %s produced matchlen %d, res %s",
	PDC_qfmt_Cstr(exp, exp_len), PDC_qfmt_Cstr(str, str_len), (int)matchlen, false_true[eret]);
  if (eret) {
#ifdef DEBUG_REGEX
    n = regexp->preg.re_nsub;
#else
    n = 0;
#endif
    for (i = 0; i <= n; i++) {
      error(0, "      sub %d so %d eo %d = \"%.*s\"",
	    i,
	    regexp->match[i].rm_so,
	    regexp->match[i].rm_eo,
	    regexp->match[i].rm_eo - regexp->match[i].rm_so,
	    str + regexp->match[i].rm_so);
    }
  }
  error(0, "");
  return 0;
}
