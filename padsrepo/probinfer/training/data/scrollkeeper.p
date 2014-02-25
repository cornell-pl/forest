#include "basetokens.p"
Ptypedef Pstring_ME(:"/[:]/":) PPmypunc_colon;

Punion word_t {
  PPword w1;
  PPwhite sp;
  PPint i1;
  PPpunc_dot dot;
}

Parray text_t {
  word_t[] : Pterm(Peor);
}

Pstruct Struct_21 { // scrollkeeper-rebuilddb: Rebuilding ScrollKeeper database...
    PPid myid1;
    PPpunc_colon mycolon1;
    PPwhite mywhite1;
    text_t mytext2;
};

Pstruct Struct_22 { // scrollkeeper-update: scrollkeeper-update: /usr/local/share/omf: No such file or directory
    PPid myid2;
    PPpunc_colon mycolon2;
    PPwhite mywhite2;
    PPid myid3;
    PPpunc_colon mycolon3;
    PPwhite mywhite3;
    PPpath mypath1;
    PPpunc_colon mycolon4;
    PPwhite mywhite4;
    text_t mytext3;
};

Pstruct Struct_25 { // scrollkeeper-update: scrollkeeper-update: /usr/local/share/omf: No such file or directory
    PPid myid2;
    PPpunc_colon mycolon2;
    PPwhite mywhite2;
    PPpath mypath3;
    PPpunc_colon mycolon3;
    PPwhite mywhite3;
    PPpath mypath1;
    PPpunc_colon mycolon4;
    PPwhite mywhite4;
    text_t mytext3;
};
 
Pstruct Struct_24 { // Registering /usr/share/omf/scrollkeeper/writing_scrollkeeper_omf_files-C.omf
    PPid myid5;
    PPpunc_colon mycolon5;
    PPwhite mywhite6;    
    PPword myword1;
    PPwhite mywhite5;
    PPpath mypath2;
};

Punion Union_6 {
    Struct_22 myunion_22;
    Struct_25 myunion_23;
    Struct_24 mystruct_25;
//    Struct_21 myunion_21;
    text_t mytext1; // Installing ScrollKeeper 0.3.14...
};

Pstruct Struct_0 {
	PPdate var_1;
	PPwhite white1;
	PPtime var_3;
	PPwhite white2;
	Union_6 var_6;
};

Pstruct Struct_1 {
	PPdate var_1;
	PPwhite white1;
	PPtime var_3;
	PPwhite white2;
    Struct_21 mystruct_21;
};

Precord Punion Union_all {
    Struct_0 mystruct_0;
    Struct_1 mystruct_1;
};

Psource Parray entries_t {
	Union_all[];
};
