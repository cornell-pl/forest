/* add these as a Pase base type */
Ptypedef Puint32 zip_t;
Ptypedef Puint64 pn_t;

Precord Pstruct summary_header {
  "0|";
  Puint32       tstamp;
};

Pstruct no_ramp {
  "no_ii";
  Puint64 id;
};

Punion dib_ramp {
  Puint64  ramp;
  no_ramp  genRamp;
};

Pstruct order_header {
       Puint32             order_num;
 '|';  Puint32             att_order_num;
 '|';  Puint32             ord_version;
 '|';  Popt pn_t           service_tn;
 '|';  Popt pn_t           billing_tn;
 '|';  Popt pn_t           nlp_service_tn;
 '|';  Popt pn_t           nlp_billing_tn;
 '|';  Popt zip_t          zip_code;
 '|';  dib_ramp            ramp;
 '|';  Pstring(:'|':)      unknown1;
 '|';  Puint32             order_type;
 '|';  Pstring(:'|':)      unknown2;
 '|';  Pstring(:'|':)      unknown3;
 '|';
};

Pstruct event {
  Pstring(:'|':) state;   '|';
  Puint32        tstamp;  
};

Parray eventSeq {
  event [] : Psep('|');
};


Precord Pstruct entry {
  order_header     h;
  eventSeq         events;
};

Parray entries_t {
  entry[];
}

Psource Pstruct out_sum{
  summary_header h;
  entries_t      es;
}



