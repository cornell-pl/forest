pstruct auint32_vbar {
  auint32 val; '|';
};
pstruct auint64_vbar {
  auint64 val; '|';
};
pstruct just_vbar {
  '|';
  dummy(:0:) d;
};
punion opt_auint32_vbar {
  auint32_vbar yes32;
  just_vbar    no32;
};
punion opt_auint64_vbar {
  auint64_vbar yes64;
  just_vbar    no64;
};
pstruct no_pn_vbar {
  "no_TN|";
  dummy(:0:) d;
};
punion dib_pn_vbar {
  auint64_vbar yesPN;
  no_pn_vbar   noPN;
};
pstruct event {
  astring(:'|':) state;   '|';
  auint32        tstamp;  '|';
};
pstruct out_sum_header {
  "0|";
  auint32        tstamp;
  EOR;
};
parray eventSeq(int size) {
  event [size];
};

int getLen(int numBars){ return (numBars - 4)/2; }

pstruct out_sum_fixed1 {
  auint32_vbar             order_num;
  auint32_vbar             order_item;
  dib_pn_vbar              servicen;
  dib_pn_vbar              billing_tn;
  auint32_vbar             zip_code;
  dib_pn_vbar              nlp_service_tn;
  dib_pn_vbar              nlp_billing_tn;
};
pstruct out_sum_fixed2 {
  opt_auint32_vbar         siid;
  opt_auint32_vbar         create_id;
  opt_auint64_vbar         rampII;
  auint32_vbar             order_type;
  auint32                  parent_order;
};
pstruct do_ev_count {
  pvirtual countX(:'|',1:) bars;
  dummy(:getLen(bars):) ev_count;
};
pstruct out_sum_data_line {
  out_sum_fixed1           f1;
  do_ev_count              c;
  eventSeq(:c.ev_count:)   events;
  out_sum_fixed2           f2;
  EOR;
};
