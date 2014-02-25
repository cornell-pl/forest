#include "basetokens.p"
Ptypedef Pstring_ME(:"/-/":) PPpunc_hyphen
Ptypedef Pstring_ME(:"/:/":) PPpunc_colon
Ptypedef Pstring_ME(:"/,/":) PPpunc_comma
Ptypedef Pstring_ME(:"/[\"]/":) PPpunc_quote
Ptypedef Pstring_ME(:"/[{]/":) PPpunc_lbsqu
Ptypedef Pstring_ME(:"/[\}]/":) PPpunc_rbsqu
Ptypedef Pstring_ME(:"/\"[0-9A-Za-z ]+\"/":) PPmymessage

Pstruct arrayBody {
  PPpunc_quote quote7;
  PPip ip3;
  PPpunc_quote quote8;
  PPpunc_comma comma7;
  PPwhite white9;
};

Parray iplist {
  arrayBody[] : Pterm('}') ;
};

Pstruct content1 {
  PPpunc_lbsqu lbsqu1;
  PPmymessage mymessage1;
  PPpunc_comma comma6;
  PPwhite white8;
  iplist iplist1;
  PPpunc_rbsqu rbsqu1;
};

Precord Pstruct entry_t {
         PPfloat float1;
         PPpunc_comma comma1;
         PPwhite white1;
         PPpunc_quote quote1;
         PPip ip1;
         PPpunc_colon colon1;
         PPint int1;
         PPpunc_quote quote2;
         PPpunc_comma comma2;
         PPwhite white2;
         PPpunc_quote quote3;
         PPhostname hostname1;
         PPwhite white3;
         PPword word1;
         PPwhite white4;
         PPword word2;
         PPpunc_quote quote4;
         PPpunc_comma comma3;
         PPwhite white5;
         PPint int2;
         PPpunc_comma comma4;
         PPwhite white6;
         PPint int3;
         PPpunc_comma comma9;
         PPwhite white13;
         PPpunc_quote quote5;
         PPip ip2;
         PPpunc_quote quote6;
         PPpunc_comma comma5;
         PPwhite white11;
         content1 con1;
         PPpunc_comma comma8;
         PPwhite white12;
         content1 con2;
};

Psource Parray clt_t {
  entry_t [];
}
