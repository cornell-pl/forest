Ptypedef Puint64 pn_t;

/- don't know what to do with this one:
Ptypedef Pchar zipSep_t : 
   zipSep_t x => {x == '-' || x == '/' || x == ' '};

Pstruct extended_zip_t{
  Puint32 zip;
  zipSep_t sep;
  Puint32 suffix;
};

Punion Pzip{
  extended_zip_t extendedZip;
  Puint32        smallZip;
  Puint64        largeZip;
};

Precord Pstruct summary_header_t {
  Pint z : z == 0; '|';        /- separated 0 from | here
  Puint32       tstamp;
};

Pstruct no_ramp_t {
  Pid noii : noii == "no_ii";  /- legal Kathleen?
  Puint64 id;
};

Punion dib_ramp_t {
  Pint64     ramp;
  no_ramp_t  genRamp;
};

Pstruct order_header_t {
       Puint32             order_num;
 '|';  Puint32             att_order_num;
 '|';  Puint32             ord_version;
 '|';  Popt pn_t           service_tn;
 '|';  Popt pn_t           billing_tn;
 '|';  Popt pn_t           nlp_service_tn;
 '|';  Popt pn_t           nlp_billing_tn;
 '|';  Popt Pzip           zip_code;
 '|';  dib_ramp_t          ramp;
 '|';  Pid                 order_type;
 '|';  Puint32             order_details;
 '|';  Pid                 unused;
 '|';  Pid                 stream;
 '|';
};

Pstruct event_t {
  Pid state;   '|';
  Puint32        tstamp;
};

Parray eventSeq {
  event_t[] : Psep('|') && Pterm(Peor) ;
} Pwhere {
  Pforall (i Pin [0..length-2] : 
           (elts[i].tstamp <= elts[i+1].tstamp));
};

Precord Pstruct entry_t {
  order_header_t  header;
  eventSeq        events;
};

Parray entries_t {
  entry_t[];
};

Psource Pstruct out_sum{
  summary_header_t  h;
  entries_t         es;
};
