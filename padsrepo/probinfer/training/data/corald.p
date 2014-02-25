#include "basetokens.p"
//Ptypedef Pstring_ME(:"/-/":) PPpunc_hyphen
//Ptypedef Pstring_ME(:"/:/":) PPpunc_colon
//Ptypedef Pstring_ME(:"/,/":) PPpunc_comma
//Ptypedef Pstring_ME(:"/[\"]/":) PPpunc_quote
//Ptypedef Pstring_ME(:"/[{]/":) PPpunc_lbsqu
//Ptypedef Pstring_ME(:"/[\}]/":) PPpunc_rbsqu
//Ptypedef Pstring_ME(:"/[0-9A-Za-z]+/":) PPidt
//Ptypedef Pstring_ME(:"/[A-Za-z]+/":) PPwordt
Ptypedef Pstring_ME(:"//":) PPemptyt

Pstruct content1 {
  PPpunc_dquote v1;
  PPword v2;
  PPpunc_dquote v3;
  PPpunc_colon v4;
  PPpunc_dquote v5;
  PPword v6;
  PPpunc_dquote v7;
  PPpunc_comma v8;
  PPwhite v9;
  PPpunc_dquote v10;
  PPword v11;
  PPpunc_dquote v12;
  PPpunc_colon v13;
  PPpunc_dquote v14;
  PPhstring v15;
  PPpunc_dquote v16;
  PPpunc_comma v17;
  PPwhite v18;
  PPpunc_dquote v19;
  PPword v20;
  PPpunc_dquote v21;
  PPpunc_colon v22;
  PPint v23;
  PPpunc_comma v24;
  PPwhite v25;
  PPpunc_dquote v26;
  PPword v27;
  PPpunc_dquote v28;
  PPpunc_colon v29;
  PPint v30;
  PPpunc_comma v31;
  PPwhite v32;
  PPpunc_dquote v33;
  PPword v34;
  PPpunc_dquote v35;
  PPpunc_colon v36;
  PPint v37;
  PPpunc_comma v38;
  PPwhite v39;
  PPpunc_dquote v40;
  PPword v41;
  PPpunc_dquote v42;
  PPpunc_colon v43;
  PPint v44;
  PPpunc_comma v45;
  PPwhite v46;
  PPpunc_dquote v47;
  PPword v48;
  PPpunc_dquote v49;
  PPpunc_colon v50;
  PPint v51;
};

Pstruct content2 {
  PPpunc_dquote f1;
  PPword f2;
  PPpunc_dquote f3;
  PPpunc_colon f4;
  PPpunc_dquote f5;
  PPword f6;
  PPpunc_dquote f7;
  PPpunc_comma f8;
  PPwhite f9;
  PPpunc_dquote f10;
  PPword f11;
  PPpunc_dquote f12;
  PPpunc_colon f13;
  PPpunc_dquote f14;
  PPhstring f15;
  PPpunc_hyphen hyphen1;
  PPpunc_hyphen hyphen2;
  PPip ip1;
  PPpunc_colon f45;
  PPint f46;
  PPpunc_dquote f16;
  PPpunc_comma f17;
  PPwhite f18;
  PPpunc_dquote f19;
  PPword f20;
  PPpunc_dquote f21;
  PPpunc_colon f22;
  PPint f23;
  PPpunc_comma f24;
  PPwhite f25;
  PPpunc_dquote f26;
  PPword f27;
  PPpunc_dquote f28;
  PPpunc_colon f29;
  PPint f30;
  PPpunc_comma f31;
  PPwhite f32;
  PPpunc_dquote f33;
  PPword f34;
  PPpunc_dquote f35;
  PPpunc_colon f36;
  PPint f37;
  PPpunc_comma f38;
  PPwhite f39;
  PPpunc_dquote f40;
  PPword f41;
  PPpunc_dquote f42;
  PPpunc_colon f43;
  PPint f44;
};

Pstruct content3 {
  PPpunc_dquote e1;
  PPword e2;
  PPpunc_dquote e3;
  PPpunc_colon e4;
  PPpunc_dquote e5;
  PPword e6;
  PPpunc_dquote e7;
};

Punion content {
  content1 con1;
  content2 con2;
  content3 con3;
};

Pstruct my_entry_t {
         PPfloat float1;          
         PPwhite w1;
         PPpunc_lbrac lsqu;
         content cont;
         Popt PPpunc_rbrac rsqu;
};

Precord Popt my_entry_t entry_t;

Psource Parray clt_t {
  entry_t [];
}
