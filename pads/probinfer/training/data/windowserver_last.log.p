#include "basetokens.p"

Ptypedef Pstring_ME(:"/[[]/":) PPpunc_lsqu
Ptypedef Pstring_ME(:"/[]]/":) PPpunc_rsqu

Pstruct Struct_4 {
        '\"';
        Pstring_ME(:"/[^"]+/":) var_6;
        '\"';
        Pstring_ME(:"/[ \t]+/":) var_8;
        '(';
        Pstring_ME(:"/[0-9a-fx]+/":) var_10;
        ')';
};
Pstruct Struct_3 {
        Struct_14 var_13;
        " set hot key operating mode to normal";
};
Pstruct Struct_17 {
        '\"';
        Pstring_ME(:"/[^"]+/":) var_19;
        '\"';
        Pstring_ME(:"/[ \t]+/":) var_21;
        '(';
        Pstring_ME(:"/[0-9a-fx]+/":) var_23;
        ')';
};
Popt Pstring_ME(:"/ but UA/":)  Opt_26;
Pstruct Struct_25 {
        "all";
        Opt_31 var_30;
};
Pstruct Struct_16 {
        Struct_34 var_33;
        " set hot key operating mode to ";
        Struct_38 var_37;
        " disabled";
};
Popt Pstring_ME(:"/ but UA/":)  Opt_42;
Pstruct Struct_41 {
        "all";
        Opt_47 var_46;
};
Pstruct Struct_40 {
        "Hot key operating mode is now ";
        Struct_51 var_50;
        " disabled";
};
Pstruct Struct_53 {
        "Display ";
        Pstring_ME(:"/[0-9a-fx]+/":) var_55;
        ':';
        Pstring_ME(:"/.*/":) var_57;
};
Popt Pstring_ME(:"/[ \t]+/":)  Opt_59;
Pstruct Struct_58 {
        Pstring_ME(:"/[^:]+/":) var_61;
        ':';
        Pstring_ME(:"/[ \t]+/":) var_63;
        Pstring_ME(:"/[^"]+/":) var_64;
        Opt_67 var_66;
        ':';
        Pstring_ME(:"/.*/":) var_69;
};
Pstruct Struct_70 {
        Pstring_ME(:"/[^:]+/":) var_71;
        ':';
        Pstring_ME(:"/[ \t]+/":) var_73;
        Pstring_ME(:"/.*/":) var_74;
};
Punion Union_2 {
        Struct_77 var_76;
        Struct_80 var_79;
        var_81 Pfrom("Hot key operating mode is now normal");
        Struct_84 var_83;
        var_85 Pfrom("Server is starting up");
        Struct_88 var_87;
        Struct_91 var_90;
        Struct_94 var_93;
        Pstring_ME(:"/.*/":) var_95;
};
Precord Pstruct Struct_1 {
        PPdate var_96;
//        Pstring_ME(:"/[ \t]+/":) var_97;
        PPwhite white1;
        PPtime var_98;
//        Pstring_ME(:"/[ \t]+/":) var_99;
        PPwhite white2;
//        '[';
        PPpunc_lsqu lsqu;
        Pint64 var_101;
//        ']';
        PPpunc_rsqu rsqu;
//        Pstring_ME(:"/[ \t]+/":) var_103;
        PPwhite white3
        Union_2 var_105;
};
Psource Parray entries_t {
        Struct_1[];
};

