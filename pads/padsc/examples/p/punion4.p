Punion branches(:Pa_uint32 a, int line_number:) {
  // included fields

  /* Pcase  1 : */ Pa_int32                         inc_normal_static_w_check : a == 1 && inc_normal_static_w_check % 2 == 0;
  /* Pcase  2 : */ Pa_int32                         inc_normal_static_wc2 : a == 2;
  /* Pcase  3 : */ Pa_string_ME(:"/(.*)X$/":)       inc_normal_dynamic_w_check : a == 3 && inc_normal_dynamic_w_check.len > 4;
  /* Pcase  4 : */ Pa_string_ME(:"/(.*)X$/":)       inc_normal_dynamic_wc2 : a == 4;
  /* Pcase  5 : */ Pcompute Pa_int32                inc_computed_static_w_check = line_number : a == 5 && inc_computed_static_w_check % 2 == 1;
  /* Pcase  6 : */ Pcompute Pa_int32                inc_computed_static_wc2     = line_number : a == 6;

  // omitted fields

  /* Pcase 11 : */ Pomit Pa_int32                   omit_normal_static_w_check : a == 11 && omit_normal_static_w_check % 2 == 0;
  /* Pcase 12 : */ Pomit Pa_int32                   omit_normal_static_wc2 : a == 12;
  /* Pcase 13 : */ Pomit Pa_string_ME(:"/(.*)X$/":) omit_normal_dynamic_w_check : a == 13 && omit_normal_dynamic_w_check.len > 4;
  /* Pcase 14 : */ Pomit Pa_string_ME(:"/(.*)X$/":) omit_normal_dynamic_wc2 : a == 14;
  /* Pcase 15 : */ Pcompute Pomit Pa_int32          omit_computed_static_w_check = line_number : a == 15 && omit_computed_static_w_check % 2 == 1;
  /* Pcase 16 : */ Pcompute Pomit Pa_int32          omit_computed_static_wc2     = line_number : a == 16;

  /* Pcase  2 : */ Pa_int32                         inc_normal_static;
  /* Pcase  4 : */ Pa_string_ME(:"/(.*)X$/":)       inc_normal_dynamic;
  /* Pcase  6 : */ Pcompute Pa_int32                inc_computed_static         = line_number;
  /* Pcase 12 : */ Pomit Pa_int32                   omit_normal_static;
  /* Pcase 14 : */ Pomit Pa_string_ME(:"/(.*)X$/":) omit_normal_dynamic;
  /* Pcase 16 : */ Pcompute Pomit Pa_int32          omit_computed_static        = line_number;
} 

Precord Pstruct choice{
  Pcompute  int                line_number = position.num;
  Pa_uint32                    a; ' ';
  branches(:a,line_number:)    b;
};
