typedef int bool;
#define true 1
#define false 0

#include "/n/fs/pads/pads/probinfer/training/data/basetoken.p"
Ptypedef Pstring_ME(:"/-/":) PPpunc_hyphen
Ptypedef Pstring_ME(:"/:/":) PPpunc_colon
Ptypedef Pstring_ME(:"/,/":) PPpunc_comma
Ptypedef Pstring_ME(:"/[\"]/":) PPpunc_quote
Ptypedef Pstring_ME(:"/[{]/":) PPpunc_lbsqu
Ptypedef Pstring_ME(:"/[\}]/":) PPpunc_rbsqu
Ptypedef Pstring_ME(:"/[0-9A-Za-z]+/":) PPidt
Ptypedef Pstring_ME(:"/[A-Za-z]+/":) PPwordt

Pstruct content1 {
  PPpunc_quote quote1;
  PPwordt word1;
  PPpunc_quote quote2;
};

Pstruct content2 {
  PPpunc_quote quote3;
  PPidt idt2;
  PPpunc_quote quote4;
};

Pstruct content3 {
  PPpunc_quote quote5;
  PPidt idt1;
  PPpunc_hyphen hyphen1;
  PPpunc_hyphen hyphen2;
  PPip ip1;
  PPpunc_colon colon5;
  PPint int5;
  PPpunc_quote quote6;
};

Punion content {
  content1 con1;
  content2 con2;
  content3 con3;
  PPint int6;
};

Pstruct arrayBody {
  PPpunc_quote q11;
  PPwordt word4;
  PPpunc_quote q12;
  PPpunc_colon c1;
  content ct;
};

PPstruct seqs {
  PPpunc_comma comma5;
  PPwhite white10;
};

seqs s1;

Parray eventSeq1 {
  arrayBody[] : Psep(", ") && Pterm(Peor) ; 
};

Parray eventSeq2 {
  arrayBody[] : Psep(", ") && Pterm('}') ; 
};

Punion eventSeq {
  eventSeq2 es1;
  eventSeq1 es2;
};

Precord Pstruct entry_t {
         PPfloat f1;          
         PPwhite w1;
         PPpunc_lbsqu lsqu;
         eventSeq es;
         Popt PPpunc_rbsqu rsqu;
};

Psource Parray clt_t {
  entry_t [];
}
