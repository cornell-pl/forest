
#include <ast.h>
#include <ast_common.h>
#include <sfio.h>
#include <regex.h>
#include <error.h>

#include "pads.h"

int main(int argc, char** argv) {
  regex_t          preg;
  int              i;
  char             delim;
  char            *exp, *str, *end, *rdelim;
  int              c_flags   = REG_AUGMENTED|REG_EXTENDED|REG_DELIMITED|REG_MULTIREF;
  int              e_flags   = 0;
  int              cret, eret;
  regmatch_t       match[100];


  c_flags |= REG_LEFT;  /* all PADS regular expressions are pinned left */

  if (argc != 3) {
    error(ERROR_FATAL, "\nCall me with 2 args, re and string\n");
  }
  exp = argv[1];
  str = argv[2];
  delim = exp[0];
  rdelim = strrchr(exp, delim);
  if (rdelim == exp) {
    error(ERROR_FATAL, "pattern beginning with a delimiter %s has no matching ending %s",
	  P_qfmt_char(delim), P_qfmt_char(delim));
  }
  for (end = exp + strlen(exp) - 1; *end != delim; end--) {
    if (*end == 'i') {
      c_flags |= REG_ICASE;
      error(0, "** Added flag REG_ICASE");
      continue;
    }
    if (*end == 'f') {
      c_flags |= REG_FIRST;
      error(0, "** Added flag REG_FIRST");
      continue;
    }
    if (*end == 'm') {
      c_flags |= REG_MINIMAL;
      error(0, "** Added flag REG_MINIMAL");
      continue;
    }
    if (*end == 'l') {
      c_flags |= REG_LITERAL;
      error(0, "** Added flag REG_LITERAL");
      continue;
    }
    error(ERROR_FATAL, "Bad pattern modifier: %s ", P_qfmt_char(*end));
  }
  cret = regcomp(&preg, exp, c_flags);
  if (cret) {
    error(ERROR_FATAL, "Failed to compile re %s, cret = %d", P_qfmt_cstr(exp, strlen(exp)), cret);
  }
  error(0, "compiled %s, nsub = %d", P_qfmt_cstr(exp, strlen(exp)), preg.re_nsub);
  eret = regexec(&preg, str, preg.re_nsub+1, match, e_flags);
  error(0, "match of RE %s against string %s produced %d",
	P_qfmt_cstr(exp, strlen(exp)), P_qfmt_cstr(str, strlen(str)), eret);
  if (!eret) {
    for (i = 0; i <= preg.re_nsub; i++) {
      error(0, "      sub %d so %d eo %d = \"%.*s\"", i, match[i].rm_so, match[i].rm_eo, match[i].rm_eo - match[i].rm_so, str + match[i].rm_so);
    }
  }
  return 0;
}
