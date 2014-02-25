#include "vanilla.p"
Pstruct Struct_2 {
        '[';
        "Time";
        ' ';
        PPdate var_6;
        ' ';
        PPtime var_8;
        ' ';
        "UTC";
        ']';
};
Pstruct Struct_12 {
        '[';
        "Facility";
        ' ';
        PPstring var_16;
        ']';
};
Pstruct Struct_18 {
        '[';
        "Sender";
        ' ';
        Pstring_ME(:"/[^\]]+/":) var_22;
        ']';
};
Pstruct Struct_24 {
        '[';
        "PID";
        ' ';
        Pint64 var_28;
        ']';
};
Pstruct Struct_32 {
        Pstring_ME(:"/[^\\]+/":) var_33;
        "\\]";
        Pstring_ME(:"/[^\]]+/":) var_35;
};
Punion Union_31 {
        Struct_38 var_37;
        Pstring_ME(:"/[^\]]+/":) var_39;
};
Pstruct Struct_30 {
        '[';
        "Message";
        ' ';
        Union_45 var_44;
        ']';
};
Pstruct Struct_47 {
        '[';
        "Level";
        ' ';
        Pint64 var_51;
        ']';
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
        '[';
        "Host";
        ' ';
        PPstring var_69;
        ']';
};
Precord Pstruct Struct_1 {
        Struct_73 var_72;
        ' ';
        Struct_77 var_76;
        ' ';
        Struct_81 var_80;
        ' ';
        Struct_85 var_84;
        ' ';
        Struct_89 var_88;
        ' ';
        Struct_93 var_92;
        ' ';
        Struct_97 var_96;
        ' ';
        Struct_101 var_100;
        ' ';
        Struct_105 var_104;
};
Psource Parray entries_t {
        Struct_1[];
};

