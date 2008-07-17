//Ptypedef Ptimestamp_explicit_FW (:15, "%b %d %H:%M:%S", P_cstr2timezone("-0500"):) timestamp_t;

#include "basetokens.p"

Pstruct daemon_message_t {
        Pstring(:':':) daemon;
        ": ";
        Popt Pstring_SE(:Peor:) msg;
};

Pstruct system_message_t {
	"last message repeated ";
	Puint32 times;
	" times";
};

Punion message_t{
	daemon_message_t dm;
	system_message_t sm;
};
	
Pstruct Struct_1 {
    PPid id2;
    PPpunc_colon colon1;
    PPtext text1;
    PPmessage message1;
    Popt Pstring_SE(:Peor:) msg;
};
Pstruct Struct_2 {
    PPid id3;
    PPmessage message2;
    Popt Pstring_SE(:Peor:) msg;
};
Pstruct Struct_3 {
    PPtext text1;
};
Punion Union_0 {
    Struct_1 var_1;
    Struct_2 var_2;
    Struct_3 var_3;
};
Precord Pstruct entry_t {
    PPdate date1;
    PPwhite white1;
    PPtime time1;
    PPwhite white2;
	PPid server;
	PPwhite white3;
    Union_0 var_0;
};

Psource Parray boot_t {
	entry_t[];
};

