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
  PPpunc_quote v1;
  PPwordt v2;
  PPpunc_quote v3;
  PPpunc_colon v4;
  PPpunc_quote v5;
  PPwordt v6;
  PPpunc_quote v7;
  PPpunc_comma v8;
  PPwhite v9;
  PPpunc_quote v10;
  PPwordt v11;
  PPpunc_quote v12;
  PPpunc_colon v13;
  PPpunc_quote v14;
  PPidt v15;
  PPpunc_quote v16;
  PPpunc_comma v17;
  PPwhite v18;
  PPpunc_quote v19;
  PPwordt v20;
  PPpunc_quote v21;
  PPpunc_colon v22;
  PPint v23;
  PPpunc_comma v24;
  PPwhite v25;
  PPpunc_quote v26;
  PPwordt v27;
  PPpunc_quote v28;
  PPpunc_colon v29;
  PPint v30;
  PPpunc_comma v31;
  PPwhite v32;
  PPpunc_quote v33;
  PPwordt v34;
  PPpunc_quote v35;
  PPpunc_colon v36;
  PPint v37;
  PPpunc_comma v38;
  PPwhite v39;
  PPpunc_quote v40;
  PPwordt v41;
  PPpunc_quote v42;
  PPpunc_colon v43;
  PPint v44;
  PPpunc_comma v45;
  PPwhite v46;
  PPpunc_quote v47;
  PPwordt v48;
  PPpunc_quote v49;
  PPpunc_colon v50;
  PPint v51;
};

Pstruct content2 {
  PPpunc_quote f1;
  PPwordt f2;
  PPpunc_quote f3;
  PPpunc_colon f4;
  PPpunc_quote f5;
  PPwordt f6;
  PPpunc_quote f7;
  PPpunc_comma f8;
  PPwhite f9;
  PPpunc_quote f10;
  PPwordt f11;
  PPpunc_quote f12;
  PPpunc_colon f13;
  PPpunc_quote f14;
  PPidt f15;
  PPpunc_hyphen hyphen1;
  PPpunc_hyphen hyphen2;
  PPip ip1;
  PPpunc_colon f45;
  PPint f46;
  PPpunc_quote f16;
  PPpunc_comma f17;
  PPwhite f18;
  PPpunc_quote f19;
  PPwordt f20;
  PPpunc_quote f21;
  PPpunc_colon f22;
  PPint f23;
  PPpunc_comma f24;
  PPwhite f25;
  PPpunc_quote f26;
  PPwordt f27;
  PPpunc_quote f28;
  PPpunc_colon f29;
  PPint f30;
  PPpunc_comma f31;
  PPwhite f32;
  PPpunc_quote f33;
  PPwordt f34;
  PPpunc_quote f35;
  PPpunc_colon f36;
  PPint f37;
  PPpunc_comma f38;
  PPwhite f39;
  PPpunc_quote f40;
  PPwordt f41;
  PPpunc_quote f42;
  PPpunc_colon f43;
  PPint f44;
};

Pstruct content3 {
  PPpunc_quote e1;
  PPwordt e2;
  PPpunc_quote e3;
  PPpunc_colon e4;
  PPpunc_quote e5;
  PPwordt e6;
  PPpunc_quote e7;
};

Punion content {
  content1 con1;
  content2 con2;
  content3 con3;
};

Precord Pstruct entry_t {
         PPfloat float1;          
         PPwhite w1;
         PPpunc_lbsqu lsqu;
         content cont;
         Popt PPpunc_rbsqu rsqu;
};

Psource Parray clt_t {
  entry_t [];
}
