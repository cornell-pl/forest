pstruct out_sum_data_line_t {
  auint32             order_item;           '|';
  omit auint32        vbars;                '|';
  auint32             create_id : create_id == vbars;
  EOR;
};
