pstruct price {
  ebc_uint32(:6:)         stock_id;
  bcd_fpoint64(:7,2:)     cost : cost.num > 0;
}
