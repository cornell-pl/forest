
#include <ast.h>
#include <ast_common.h>
#include <sfio.h>
#include <error.h>

#include "padsc-internal.h"

int main(int argc, char** argv) {
  int             i;
  int             mapped_vals;
  mapped_vals = 0;
  for (i = 0; i < 255; i++) {
    if (PDC_mod_ae_tab[i] != 255) {
      mapped_vals++;
      if (PDC_mod_ea_tab[PDC_mod_ae_tab[i]] != i) {
	error(0, "  i = %02x, ae[i] = %02x, ea[%02x] = %02x",
	      i, PDC_mod_ae_tab[i], PDC_mod_ae_tab[i], PDC_mod_ea_tab[PDC_mod_ae_tab[i]]);
      }
    }
  }
  error(0, "mapped ae vals = %d", mapped_vals);
  mapped_vals = 0;
  for (i = 0; i < 255; i++) {
    if (PDC_mod_ea_tab[i] != 255) {
      mapped_vals++;
      if (PDC_mod_ae_tab[PDC_mod_ea_tab[i]] != i) {
	error(0, "  i = %02x, ea[i] = %02x, ae[%02x] = %02x",
	      i, PDC_mod_ea_tab[i], PDC_mod_ea_tab[i], PDC_mod_ae_tab[PDC_mod_ea_tab[i]]);
      }
    }
  }
  error(0, "mapped ea vals = %d", mapped_vals);
  return 0;
}
