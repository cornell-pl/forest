pstruct a_uint32_vbar {
  a_uint32 val; '|';
};
pstruct a_uint64_vbar {
  a_uint64 val; '|';
};
pstruct just_vbar {
  '|';
  compute PDC_int8 d = 0; /-- padsc should provide dummy field for 'empty' structs
};
punion opt_a_uint32_vbar {
  a_uint32_vbar yes32;
  just_vbar    no32;
};
punion opt_a_uint64_vbar {
  a_uint64_vbar yes64;
  just_vbar    no64;
};
pstruct no_pn_vbar {
  "no_TN|";
  compute PDC_int8 d = 0; /-- padsc should provide dummy field for 'empty' structs
};
punion dib_pn_vbar {
  a_uint64_vbar yesPN;
  no_pn_vbar   noPN;
};
pstruct event {
  a_string(:'|':) state;   '|';
  a_uint32        tstamp;  '|';
};
pstruct out_sum_header {
  "0|";
  a_uint32        tstamp;
  EOR;
};
parray eventSeq(int size) {
  event [size];
};

int getLen(int numBars){ return (numBars - 4)/2; }

pstruct out_sum_data_line {
  a_uint32_vbar             order_num;
  a_uint32_vbar             order_item;
  dib_pn_vbar              servicen;
  dib_pn_vbar              billing_tn;
  a_uint32_vbar             zip_code;
  dib_pn_vbar              nlp_service_tn;
  dib_pn_vbar              nlp_billing_tn;
  omit countX(:'|',1:) bars;
  eventSeq(:getLen(bars):) events;
  opt_a_uint32_vbar         siid;
  opt_a_uint32_vbar         create_id;
  opt_a_uint64_vbar         rampII;
  a_uint32_vbar             order_type;
  a_uint32                  parent_order;
  EOR;
};
