pstruct auint16_vbar {
  auint16 val; '|';
};

pstruct auint32_vbar {
  auint32 val; '|';
};

pstruct auint64_vbar {
  auint64 val; '|';
};

pstruct astring_vbar {
  astring(:'|':) val; '|';
};

pstruct just_vbar {
  countX(:'|', 1:) vbars;
  '|';
};

pstruct nada {
  countX(:'|', 1:) vbars;
};

punion auint16_field {
  auint16_vbar num16;
  just_vbar    blank16;
};

punion auint32_field {
  auint32_vbar num32;
  just_vbar    blank32;
};

punion auint64_field {
  auint64_vbar num64;
  just_vbar    blank64;
};

punion astring_field {
  astring_vbar yesSTR;
  just_vbar    noSTR;
};

punion auint32_last_field {
  auint32      yesNUM;
  nada         noNUM;
};

pstruct gen_pn_vbar {
  "no_TN"; auint32 id;  /- generated unique id
  '|';
};

pstruct no_pn_vbar {
  countX(:'|', 1:) vbars;
  "no_TN"; /* just no_TN by itself */
  '|'; 
};

punion dib_pn_field {
  auint64_vbar normPN; 
  gen_pn_vbar  genPN;         
  no_pn_vbar   noPN;         
  just_vbar    blankPN;
};

pstruct event {
  astring_field  state;
  auint32_field  tstamp;
};

pstruct out_sum_header {
  "0|";
  auint32      tstamp;
  EOR;
};

parray eventSeq(int size) {
  event [size];
};

int getLength(int numBars){ return (numBars - 4)/2; }

pstruct out_sum_data_line {
  auint32_field       order_num;
  auint32_field       order_item;
  dib_pn_field        servicen;
  dib_pn_field        billing_tn;
  auint32_field       zip_code;
  dib_pn_field        nlp_service_tn;
  dib_pn_field        nlp_billing_tn;
  pvirtual countX(:'|', 1:) vbars;
  eventSeq(:getLength(vbars):)  events;
  auint32_field       siid;
  auint32_field       create_id;
  auint64_field       rampII;
  auint32_field       order_type;
  auint32_last_field  parent_order;
  EOR;
};

