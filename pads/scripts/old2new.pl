#!/usr/bin/env perl

while (<>) {
  s/PDC_achar/PDC_a_char/g;
  s/PDC_astr/PDC_a_str/g;
  s/PDC_aint/PDC_a_int/g;
  s/PDC_auint/PDC_a_uint/g;
  s/PDC_eint/PDC_e_int/g;
  s/PDC_euint/PDC_e_uint/g;
  s/PDC_bint/PDC_b_int/g;
  s/PDC_buint/PDC_b_uint/g;

  s/PDC_adate/PDC_a_date/g;
  s/PDC_edate/PDC_e_date/g;

  s/stringFW/string_FW/g;
  s/stringSE/string_SE/g;
  s/stringCSE/string_CSE/g;

  s/8FW/8_FW/g;
  s/16FW/16_FW/g;
  s/32FW/32_FW/g;
  s/64FW/64_FW/g;

  s/INVALID_AUINT/INVALID_A_NUM/g;
  s/INVALID_AINT/INVALID_A_NUM/g;
  s/INVALID_EUINT/INVALID_E_NUM/g;
  s/INVALID_EINT/INVALID_E_NUM/g;

  print;
}
