
#include <ast.h>
#include <ast_common.h>
#include <sfio.h>
#include <regex.h>
#include <error.h>

#include "padsc-internal.h"

const char* false_true [] = { "TRUE", "FALSE" };

int is_foo(int c) { return c == 'f' || c == 'o' || isdigit(c); }

/* used to register the new class: */
void reg_foo() { regaddclass("foo", is_foo); }

int main(int argc, char** argv) {
  PDC_t          *pdc;
  PDC_disc_t      my_disc = PDC_default_disc;
  PDC_IO_disc_t  *io_disc;
  PDC_REGEXP_DECL_NULL(regexp);
  size_t          matchlen;
  int             i, n, eret, bor, eor, pin;
  PDC_byte       *begin, *end;
  const char     *exp, *str;
  size_t          exp_len, str_len;
  regflags_t      e_flags;

  reg_foo();

  io_disc = PDC_nlrec_make(0);
  if (PDC_ERR == PDC_open(&pdc, &my_disc, io_disc)) {
    error(ERROR_FATAL, "*** PDC_open failed ***");
  }

  if (argc != 3 && argc != 6) {
    error(ERROR_FATAL, "\nusage: test_re2 <pattern> <string> [ bor eor pin ]\n");
  }
  bor = eor = pin = 0;
  if (argc == 6) {
    bor = atoi(argv[3]);
    eor = atoi(argv[4]);
    pin = atoi(argv[5]);
  }
  exp      = argv[1];
  str      = argv[2];
  exp_len  = strlen(exp);
  str_len  = strlen(str);
  begin = (PDC_byte*)str;
  end   = (PDC_byte*)(str + str_len);

  if (PDC_ERR == PDC_regexp_compile_Cstr(pdc, exp, &regexp)) {
    error(ERROR_FATAL, "Failed to compile re %s", PDC_qfmt_Cstr(exp, exp_len));
  }
  error(0, "\ncompiled regexp, nsub = %d\n", regexp.preg.re_nsub);

  e_flags = 0;
  if (pin) { e_flags |= REG_LEFT; }
  if (!bor) { e_flags |= REG_NOTBOL; }
  if (!eor) { e_flags |= REG_NOTEOL; }
  eret = PDCI_regexp_match(pdc, &regexp, begin, end, e_flags, PDC_charset_ASCII);
  matchlen = regexp.match[0].rm_eo - regexp.match[0].rm_so;
  error(0, "match of RE %s against string %s produced matchlen %d, res %s",
	PDC_qfmt_Cstr(exp, exp_len), PDC_qfmt_Cstr(str, str_len), (int)matchlen, false_true[eret]);
  if (!eret) {
#ifdef DEBUG_REGEX
    n = regexp.preg.re_nsub;
#else
    n = 0;
#endif
    for (i = 0; i <= n; i++) {
      error(0, "      sub %d so %d eo %d = \"%.*s\"",
	    i,
	    regexp.match[i].rm_so,
	    regexp.match[i].rm_eo,
	    regexp.match[i].rm_eo - regexp.match[i].rm_so,
	    str + regexp.match[i].rm_so);
    }
  }
  error(0, "");
  return 0;
}
