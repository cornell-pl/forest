#include "basetokens.p"

Ptypedef Pstring_ME(:"/[-0-9A-Za-z_. ]+/":) PPmyid;

Punion zipSep {
  PPpunc_hyphen h1;
  PPpunc_bslash bs;
  PPwhite w1;
};

Pstruct extended_zip_t{
  Puint32 zip;
  zipSep sep;
  Puint32 suffix;
};

Punion Pzip{
  extended_zip_t extendedZip;
  Puint32        smallZip;
  Puint64        largeZip;
};

Precord Pstruct summary_header_t {
  PPint int1;
  PPpunc_bar bar14;                               
  PPint       tstamp;
};

Pstruct no_ramp_t {
  PPid id1;
  Puint64 id;
};

Punion dib_ramp_t {
  Pint64     ramp;
//  no_ramp_t  genRamp;
  PPid genRamp;
};

Pstruct order_header_t {
       Puint32             order_num;
 PPpunc_bar bar1;  Puint32             att_order_num;
 PPpunc_bar bar2;  Puint32             ord_version;
 PPpunc_bar bar3;  Popt Puint64           service_tn;
 PPpunc_bar bar4;  Popt Puint64           billing_tn;
 PPpunc_bar bar5;  Popt Puint64           nlp_service_tn;
 PPpunc_bar bar6;  Popt Puint64           nlp_billing_tn;
 PPpunc_bar bar7;  Popt Pzip           zip_code;
 PPpunc_bar bar8;  dib_ramp_t          ramp;
 PPpunc_bar bar9;  PPid      order_type;
 PPpunc_bar bar10;  Puint32             order_details;
 PPpunc_bar bar11;  PPmyid      unused;
 PPpunc_bar bar12;  PPid      stream;
 PPpunc_bar bar13;
};

Pstruct event_t {
//  Pstring(:'|':) state;
  PPid id2;   
  PPpunc_bar bar15;   
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
