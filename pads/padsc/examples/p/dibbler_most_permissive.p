pstruct a_uint16_vbar {
  a_uint16 val; '|';
};

pstruct a_uint32_vbar {
  a_uint32 val; '|';
};

pstruct a_uint64_vbar {
  a_uint64 val; '|';
};

pstruct a_string_vbar {
  a_string(:'|':) val; '|';
};

pstruct just_vbar {
  countX(:'|', 1:) vbars;
  '|';
};

pstruct nada {
  countX(:'|', 1:) vbars;
};

punion a_uint16_field {
  a_uint16_vbar num16;
  just_vbar    blank16;
};

punion a_uint32_field {
  a_uint32_vbar num32;
  just_vbar    blank32;
};

punion a_uint64_field {
  a_uint64_vbar num64;
  just_vbar    blank64;
};

punion a_string_field {
  a_string_vbar yesSTR;
  just_vbar    noSTR;
};

punion a_uint32_last_field {
  a_uint32      yesNUM;
  nada         noNUM;
};

pstruct gen_pn_vbar {
  "no_TN"; a_uint32 id;  /- generated unique id
  '|';
};

pstruct no_pn_vbar {
  countX(:'|', 1:) vbars;
  "no_TN"; /* just no_TN by itself */
  '|'; 
};

punion dib_pn_field {
  a_uint64_vbar normPN; 
  gen_pn_vbar  genPN;         
  no_pn_vbar   noPN;         
  just_vbar    blankPN;
};

pstruct event {
  a_string_field  state;
  a_uint32_field  tstamp;
};

pstruct out_sum_header {
  "0|";
  a_uint32      tstamp;
  EOR;
};

parray eventSeq(int size) {
  event [size];
};

int getLength(int numBars){ return (numBars - 4)/2; }

pstruct out_sum_data_line {
  a_uint32_field       order_num;
  a_uint32_field       order_item;
  dib_pn_field        servicen;
  dib_pn_field        billing_tn;
  a_uint32_field       zip_code;
  dib_pn_field        nlp_service_tn;
  dib_pn_field        nlp_billing_tn;
  omit countX(:'|', 1:) vbars;
  eventSeq(:getLength(vbars):)  events;
  a_uint32_field       siid;
  a_uint32_field       create_id;
  a_uint64_field       rampII;
  a_uint32_field       order_type;
  a_uint32_last_field  parent_order;
  EOR;
};

