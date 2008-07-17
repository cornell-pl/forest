#include "./basetokens.p"

Pstruct Struct_2 {
        PPpunc_lsqubrac lsqubrac1;
        PPword word1;
        PPwhite white9;
        PPdate var_6;
        PPwhite white10;
        PPtime var_8;
        PPwhite white11;
        PPword word2;
        PPpunc_rsqubrac rsqubrac2;
};
Pstruct Struct_12 {
        PPmessage message1;
};

Pstruct Struct_18 {
        PPpunc_lsqubrac lsqubrac2;
        PPword word3;
        PPwhite white12;
        PPpath path1;
        PPpunc_rsqubrac rsqubrac2;
};
Punion union1 {
       Struct_18 var_18;
       Struct_12 var_12;
};
Pstruct Struct_24 {
        PPpunc_lsqubrac lsqubrac3;
        PPword word4;
        PPwhite white13;
        PPint var_28;
        PPpunc_rsqubrac rsqubrac3;
};
Pstruct Struct_32 {
        Pstring_ME(:"/[^\\\\]+/":) var_33;
        "\\]";
        Pstring_ME(:"/[^]]+/":) var_35;
};
Pstruct Struct_33 {
        PPpunc_lsqubrac lsqubrac8;
        PPword word9;
        PPwhite white20;
        Struct_32 var_44;
        PPpunc_rsqubrac rsqubrac8;
};
Punion Union_31 {
        Struct_33 var_37;
//        Pstring_ME(:"/[^\]]+/":) var_39;
        PPmessage message5;
};
Pstruct Struct_30 {
        PPmessage message2;
};
Pstruct Struct_47 {
        PPpunc_lsqubrac lsqubrac4;
        PPword word5;
        PPwhite white13;
        PPint var_51;
        PPpunc_rsqubrac rsqubrac4;
};
Pstruct Struct_53 {
        '[';
        "UID";
        ' ';
        Pint64 var_57;
        ']';
};
Pstruct Struct_59 {
        '[';
        "GID";
        ' ';
        Pint64 var_63;
        ']';
};
Pstruct Struct_65 {
        PPpunc_lsqubrac lsqubrac5;
        PPtext text1;
        PPpunc_rsqubrac rsqubrac5;
};

Pstruct Struct_67 {
        PPpunc_lpar lsqubrac11;
        PPtext text2;
        PPpunc_rpar rbrac1;
        PPwhite white11;
        PPpunc_lpar lbrac2;
        PPtext text3;
        PPpunc_rpar rbrac2;
};

Punion Union_2 {
       Struct_67 struct67;
       PPhostname hostname1;
};

Pstruct Struct_66 {
        PPpunc_lsqubrac lsqubrac5;
        PPtext text1;
        Popt Union_2 union2;
        PPpunc_rsqubrac rsqubrac5;
};

Precord Pstruct Struct_1 {
        Struct_2 var_72;
        PPwhite white1;
        Struct_65 var_76;
        PPwhite white2;
        Struct_18 var_80;
        PPwhite white3;
        Struct_24 var_84;
        PPwhite white4;
        Struct_66 var_88;
        PPwhite white5;
        Struct_47 var_92;
        PPwhite white6;
        Struct_47 var_96;
        PPwhite white7;
        Struct_47 var_100;
        PPwhite white8;
        Struct_65 var_104;
};
Psource Parray entries_t {
        Struct_1[];
};
