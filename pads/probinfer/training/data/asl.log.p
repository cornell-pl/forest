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
        PPpunc_lpar paren;
        PPwhite wh23;
}

Parray stuff {
        myword [] : Pterm('\]');
}

Pstruct printer {
        PPid id;
        '(';
        PPword w;
        ')';
}

Pstruct postfix {
  PPword w1;
  PPpunc_slash slash;
  PPword w2;
}

Punion Union_3 {
        Pstring_ME(:"/Ej[^Q]+Q/":) ej;
        postfix pf;
        PPpath path;
        ee Pfrom ("E_e,");
        printer p;
        stuff s;
        PPid compath;
}
   
Pstruct Struct_18 {
        PPpunc_lsqubrac lsqubrac2;
        PPword sender;
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

Pstruct brac_number {
        '[';
        PPint anum;
        "\\]";
}

Pstruct paren_stuff {
        '(';
        Pstring (:')':) stuffinparents;
        ')';
}
        
Punion Union_2 {
        brac_number bnum;
        PPtime myt;
        PPdate myd;
        PPip myip;
        PPword my_w;
        PPpath filepath;
        PPid myid;
        PPint myint;
        PPhostname myhostname1;
        paren_stuff ps;
        PPpunc_scolon sc;
        PPpunc_colon colon;
        PPpunc_dquote quote;
        PPwhite wh_sp;
        Pstring_SE(:"/[\\]\\[]/":) anystring;
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
