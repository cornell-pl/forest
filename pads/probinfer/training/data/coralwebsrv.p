#include "/n/fs/pads/pads/probinfer/training/data/basetoken.p"
Ptypedef Pstring_ME(:"/-/":) PPpunc_hyphen
Ptypedef Pstring_ME(:"/:/":) PPpunc_colon
Ptypedef Pstring_ME(:"/,/":) PPpunc_comma
Ptypedef Pstring_ME(:"/[\"]/":) PPpunc_quote
Ptypedef Pstring_ME(:"/[{]/":) PPpunc_lbsqu
Ptypedef Pstring_ME(:"/[\}]/":) PPpunc_rbsqu
Ptypedef Pstring_ME(:"/\"[0-9A-Za-z ().]+\"|[(][^)]+[)]/":) PPmymessage
Ptypedef Pstring_ME(:"/[0-9]+|\-[0-9]+/":) PPmyint
Ptypedef Pstring_ME(:"/[^\"]+/":) PPmyurl
Ptypedef Pstring_ME(:"/[(]/":) PPpunc_lpar
Ptypedef Pstring_ME(:"/[)]/":) PPpunc_rpar

Pstruct content1 {
         PPpunc_comma comma11;
         PPwhite white11;       
         PPmyint int7;
};

Pstruct struct1 {
  PPpunc_quote quote11;
  PPip ip3;
  PPpunc_quote quote12;
};

Punion union1 {
  struct1 str1;
  PPmymessage message2;
};

Pstruct content2 {
         PPpunc_comma comma15;
         PPwhite white15;
         union1 uni1;
};

Pstruct struct2 {
  PPfloat float2;
  PPwhite white17;
  PPhostname hostname2;
  PPpunc_colon colon3;
  PPint int8;
  PPwhite white18;
  PPmymessage message3;
};

Punion union2 {
  struct2 str2;
  PPmymessage message3;
};

Pstruct content3 {
         PPpunc_comma comma16;
         PPwhite white16;
         union2 uni2;
};

Pstruct content4 {
         PPpunc_quote quote1;
         PPword word1;
         PPpunc_quote quote2;
         PPpunc_comma comma3;
         PPwhite white3;
};

Precord Pstruct entry_t {
         PPmyint int1; 
         PPpunc_comma comma1;
         PPwhite white1;

         PPfloat float1;
         PPpunc_comma comma2;
         PPwhite white2;

         content4 con7;

         Popt content4 con8; 

         PPpunc_quote quote5;
         PPip ip1;
         PPpunc_colon colon1;
         PPint int2;
         PPpunc_quote quote6;
         PPpunc_comma comma5;
         PPwhite white5;

         PPpunc_quote quote7;
         PPip ip2;
         PPpunc_colon colon2;
         PPint int3;
         PPpunc_quote quote8;
         PPpunc_comma comma6;
         PPwhite white6;

         PPpunc_quote quote9;
         PPmyurl url1;
         PPpunc_quote quote10;
         PPpunc_comma comma7;
         PPwhite white7;

         Popt PPmymessage message1;
         Popt PPpunc_comma comma8;
         Popt PPwhite white8;   

         PPmyint int4;
         PPpunc_comma comma9;
         PPwhite white9;       

         PPmyint int5;
         PPpunc_comma comma10;
         PPwhite white10;       

         PPmyint int6;

         Popt content1 con1;

         Popt content1 con2;

         Popt content1 con3;

         Popt content1 con4;

         Popt content2 con5;
        
         Popt content3 con6;

};

Psource Parray clt_t {
  entry_t [];
}
