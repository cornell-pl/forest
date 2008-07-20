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
Punion myword {
        PPword w;
        PPid id;
}

Parray stuff {
        myword [] : Psep(' ') && Pterm('\]');
}

Pstruct printer {
        PPid id;
        '(';
        PPword w;
        ')';
}

Punion Union_3 {
        PPpath path;
        ee Pfrom ("E_e,");
        printer p;
        stuff s;
}
   
Pstruct Struct_18 {
        PPpunc_lsqubrac lsqubrac2;
        PPword word3;
        PPwhite white12;
        Union_3 union3;
        PPpunc_rsqubrac rsqubrac2;
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
        PPint var_57;
        ']';
};
Pstruct Struct_59 {
        '[';
        "GID";
        ' ';
        PPint var_63;
        ']';
};
Pstruct Struct_65 {
        PPpunc_lsqubrac lsqubrac5;
        PPword w1;
        PPwhite whitesp;
        PPword w2;
        PPpunc_rsqubrac rsqubrac5;
};

Punion Union_2 {
        PPtime myt;
        PPdate myd;
        PPip myip;
        PPword my_w;
        PPid myid;
        PPint myint;
        PPhostname myhostname1;
        PPpunc_scolon sc;
        PPpunc_colon colon;
        PPwhite wh_sp;
        Pstring(:'\]':) anystring;
};

Parray mystuff1 {
        Union_2 [] : Pterm('\]');
}
 
Pstruct Struct_66 {
        PPpunc_lsqubrac lsqubrac5;
        mystuff1 stuff1;
        PPpunc_rsqubrac rsqubrac5;
};

Precord Pstruct Struct_1 {
        Struct_2 var_72;
        PPwhite white1;
        Struct_65 var_76;
        PPwhite white2;
        Struct_18 var_80;
        PPwhite white3;
        Struct_47 var_84;
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
