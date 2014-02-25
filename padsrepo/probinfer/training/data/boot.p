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

Pstruct variable_t {
        '$';
        PPid var_name;
}

Punion word_t {
        PPurl url;
        PPhostname host;
        PPip ip1;
        PPmac mac1;
        PPtime  time1;
        PPdate  date1;
        PPpath path;
        PPid id;
        PPword w;
        variable_t var1;
        PPwhite sp;
        PPpunc_equa equalsign;
        PPpunc_colon colon;
        PPpunc_slash slash;
        PPpunc_comma comma;
        PPpunc_lpar lp;
        PPpunc_rpar rp;
        PPpunc_dot dot;
        Pstring_SE (:Peor:) mymsg;
}       
        
Parray text_t {
        word_t[];
}

Pstruct lpf {
  PPword w1;
  PPwhite w2;
  PPword w3;
  PPwhite w4;
  "LPF/eth";
  PPint eth_index;
  PPpunc_slash slash1;
  PPmac mac;
   PPpunc_slash slash2;
  PPid id;
  PPpunc_slash slash3;
  PPint num;
}
          
Pstruct Struct_1 {
    PPid id1;
    PPpunc_colon colon1;
    PPwhite wh;
    text_t text1;
};
Pstruct Struct_2 {
    PPid id3;
    PPpunc_colon colon1;
    PPwhite wh;
    lpf my_lpf;
};
Pstruct Struct_3 {
    text_t text3;
};
Punion Union_0 {
    Struct_2 var_2; 
    Struct_1 var_1;
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

