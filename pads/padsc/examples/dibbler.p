#if 0
ptypedef auint64 pn_t    :: pn_t   x => {x > 1999999999 && x < 10000000000};
ptypedef auint16 zip5_t  :: zip5_t x => {x < 100000};
#else
ptypedef auint64 pn_t    :: pn_t   x => { x <= 0 || x >= 0 };
ptypedef auint16 zip5_t  :: zip5_t x => { x <= 0 || x >= 0 };
#endif

pstruct gen_pn_t {
  "no_PN";
  auint32 id;
}

punion dib_pn_t {
    pn_t normal; 
    gen_pn_t derived;         /- Missing data: generated unique id
};

pstruct event_t {
  astring(:'|':) state;                '|';
  auint32        tstamp;
};

pstruct out_sum_header_t {
  "0|";
  auint32      tstamp;
  '\n';
};

parray eventSeq(int size) {
  event_t [size] : sep == '|' &&
                   forall i in [0 .. length - 2]
                            { eventSeq[i].tstamp <= eventSeq[i+1].tstamp};
};

int getLength(int numBars){ return (numBars - 4)/2; }

pstruct out_sum_data_line_t {
  auint32             order_num;            '|';
  auint32             order_item;           '|';
  dib_pn_t            service_tn;           '|';
  dib_pn_t            billing_tn;           '|';
  zip5_t              zip_code;             '|';
  dib_pn_t            nlp_service_tn;       '|';
  dib_pn_t            nlp_billing_tn;       '|';
  pvirtual countXtoY(:'|', '\n':)vbars;
  eventSeq(:getLength(vbars):)  events;     '|';
  auint32             siid;                 '|';  //- why did kf omit
  auint32             create_id;            '|';
  auint64             rampII;               '|';
  auint32             order_type;           '|';
  auint32             parent_order;         '\n';
};
parray records { out_sum_data_line_t [];};
pstruct out_sum_file_t {
  out_sum_header_t      h;
  records               rs;
};


