
#include <ast.h>
#include <ast_common.h>
#include <sfio.h>

int main(int argc, char** argv) {
  Re_program_t    *r;
  int              flags   = 0;
  int              res;

  if (argc != 3) {
    printf("\nCall me with 2 args, re and string\n");
    return -1;
  }
  r = recomp(argv[1], flags);
  if (!r) {
    error(ERROR_FATAL, "Failed to compile re %s", argv[1]);
  }
  res = reexec(r, argv[2]);
  error(0, "match of RE %s against string %s produced %d",
	argv[1], argv[2], res);
  return 0;
}
