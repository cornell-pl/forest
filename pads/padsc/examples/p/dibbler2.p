Pstruct a_uint32_vbar {
  Puint32 val; '|';
};
Pstruct a_uint64_vbar {
  Puint64 val; '|';
};
Pstruct just_vbar {
  '|';
  Pcompute Pint8 d = 0; /-- padsc should provide dummy field for 'empty' structs
};
Punion opt_a_uint32_vbar {
  a_uint32_vbar yes32;
  just_vbar     no32;
};
Punion opt_a_uint64_vbar {
  a_uint64_vbar yes64;
  just_vbar     no64;
};
Pstruct no_pn_vbar {
  "no_TN|";
  Pcompute Pint8 d = 0; /-- padsc should provide dummy field for 'empty' structs
};
Punion dib_pn_vbar {
  a_uint64_vbar yesPN;
  no_pn_vbar    noPN;
};
Pstruct event {
  Pa_string(:'|':) state;   '|';
  Pa_uint32        tstamp;  '|';
};
Precord Pstruct out_sum_header {
  "0|";
  Pa_uint32        tstamp;
};
Parray eventSeq(int size) {
  event [size];
};

int getLen(int numBars){ return (numBars - 4)/2; }

Pstruct out_sum_fixed1 {
  a_uint32_vbar             order_num;
  a_uint32_vbar             order_item;
  dib_pn_vbar               servicen;
  dib_pn_vbar               billing_tn;
  a_uint32_vbar             zip_code;
  dib_pn_vbar               nlp_service_tn;
  dib_pn_vbar               nlp_billing_tn;
};
Pstruct out_sum_fixed2 {
  opt_a_uint32_vbar         siid;
  opt_a_uint32_vbar         create_id;
  opt_a_uint64_vbar         rampII;
  a_uint32_vbar             order_type;
  Puint32                   parent_order;
};
Pstruct do_ev_count {
  Pomit PcountX(:'|',1,0:)  bars;
  Pcompute Pint32        ev_count = getLen(bars);
};
Precord Pstruct out_sum_data_line {
  out_sum_fixed1            f1;
  do_ev_count               c;
  eventSeq(:c.ev_count:)    events;
  out_sum_fixed2            f2;
};
