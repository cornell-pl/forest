pstruct out_sum_data_line_t {
  a_uint32             order_item;           '|';
  omit a_uint32        vbars;                '|';
  a_uint32             create_id : create_id == vbars;
  EOR;
};
