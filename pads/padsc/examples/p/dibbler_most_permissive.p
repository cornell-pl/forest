Pstruct a_uint16_vbar {
  Pa_uint16 val; '|';
};

Pstruct a_uint32_vbar {
  Pa_uint32 val; '|';
};

Pstruct a_uint64_vbar {
  Pa_uint64 val; '|';
};

Pstruct a_string_vbar {
  Pa_string(:'|':) val; '|';
};

Pstruct just_vbar {
  PcountX(:'|',1,0:) vbars;
  '|';
};

Pstruct nada {
  PcountX(:'|',1,0:) vbars;
};

Punion a_uint16_field {
  a_uint16_vbar num16;
  just_vbar      blank16;
};

Punion a_uint32_field {
  a_uint32_vbar num32;
  just_vbar     blank32;
};

Punion a_uint64_field {
  a_uint64_vbar num64;
  just_vbar     blank64;
};

Punion a_string_field {
  a_string_vbar yesSTR;
  just_vbar    noSTR;
};

Punion a_uint32_last_field {
  Pa_uint32     yesNUM;
  nada          noNUM;
};

Pstruct gen_pn_vbar {
  "no_TN"; Pa_uint32 id;  /- generated unique id
  '|';
};

Pstruct no_pn_vbar {
  PcountX(:'|',1,0:) vbars;
  "no_TN"; /* just no_TN by itself */
  '|'; 
};

Punion dib_pn_field {
  a_uint64_vbar normPN; 
  gen_pn_vbar   genPN;         
  no_pn_vbar    noPN;         
  just_vbar     blankPN;
};

Pstruct event {
  a_string_field  state;
  a_uint32_field  tstamp;
};

Precord Pstruct out_sum_header {
  "0|";
  Pa_uint32     tstamp;
};

Parray eventSeq(int size) {
  event [size];
};

int getLength(int numBars){ return (numBars - 4)/2; }

Precord Pstruct out_sum_data_line {
  a_uint32_field                order_num;
  a_uint32_field                order_item;
  dib_pn_field                  servicen;
  dib_pn_field                  billing_tn;
  a_uint32_field                zip_code;
  dib_pn_field                  nlp_service_tn;
  dib_pn_field                  nlp_billing_tn;
  Pomit PcountX(:'|',1,0:)      vbars;
  eventSeq(:getLength(vbars):)  events;
  a_uint32_field                siid;
  a_uint32_field                create_id;
  a_uint64_field                rampII;
  a_uint32_field                order_type;
  a_uint32_last_field           parent_order;
};

