#include "basetokens.p"

Pstruct Struct_8 { // Display...
        PPtext text10;
        PPmessage message11;
};
Pstruct Struct_7 { //combined with Struct_8
        PPtext text9;
        PPpunc_colon colon8;
        PPwhite white18;
        PPint int2;
};
Pstruct Struct_6 {   //(ipc/send)... can't use message...
        PPmessage message4;
        PPwhite white14;
        PPtext text7;
        PPpunc_colon colon6;
        PPwhite white15;
        PPid id8;
        PPpunc_colon colon7;
        PPwhite white16;
        PPid id9;
        PPwhite white17;
        PPmessage message5;
        PPtext text8;
        PPmessage message6;
};
Pstruct Struct_5 { //CGXRestartSessionWorkspace...
        PPid id7;
/*
        PPpunc_colon colon5;
        PPwhite white12;
        PPtext text6;
        PPint int1;
        PPwhite white13;
*/
        PPmessage message3;
};
Pstruct Struct_4 {
        PPid id4;
        PPpunc_colon colon3;
        PPwhite white9;
        PPid id5;
        Popt PPwhite white10;
        PPpunc_colon colon4;
        PPwhite white11;
        PPtext text5;
        PPid id6;
        Popt PPtext text6;
};
Pstruct Struct_3 {
        PPid id2;
        PPpunc_colon colon1;
        PPwhite white6;
        PPid id3;
        Popt PPwhite white7;
/*
        PPpunc_colon colon2;
        PPwhite white8;
        PPtext text3;
        PPpunc_quote quote3;
        PPword word2;
        PPpunc_quote quote4;
        PPtext text4;
        PPint int5;
        PPwhite white20;
        Popt PPmessage message1;
*/
        PPmessage message1;
};
Pstruct Struct_2 {
        PPmessage message2;
        PPwhite white4;
        PPpunc_lpar lpar1;
        PPid id1;
        PPpunc_rpar rpar1;
        PPwhite white5;
        PPtext text2;
};
Punion Union_2 {
  Struct_6 var_6;
  Struct_3 var_3;
  Struct_3 var_4;
  Struct_2 var_2;
  Struct_8 var_8;
  Struct_5 var_5;
  PPtext text1;          /* Hot key operating mode is now normal */
};
Precord Pstruct Struct_1 {
        PPdate var_96;
        PPwhite white1;
        PPtime var_98;
        PPwhite white2;
        PPpunc_lsqubrac lsqu;
        PPint var_101;
        PPpunc_rsqubrac rsqu;
        PPwhite white3;
        Union_2 var_105;
};
Psource Parray entries_t {
        Struct_1[];
};

