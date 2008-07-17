#include "basetokens.p"
Ptypedef Pstring_ME(:"/[:]/":) PPmypunc_colon;

Pstruct Struct_8 {
	Pint64 var_9;
	'.';
	Pint64 var_11;
	'.';
	Pint64 var_13;
};

Pstruct Struct_6 {
    Pstring_ME(:"/Installing ScrollKeeper 0\.3\.14\.\.\./":) string1;
};

Penum Enum_16 {
	var_27 Pfrom("scrollkeeper-rebuilddb: "),
	var_28 Pfrom("scrollkeeper-update: ")
};
Penum Enum_18 {
	var_29 Pfrom("Rebuilding ScrollKeeper database..."),
	var_30 Pfrom("Done rebuilding ScrollKeeper database.")
};
Pstruct Struct_20 {
	PPword word3;
    PPwhite white4;
	PPpath var_22;
};
Penum Enum_24 {
	var_31 Pfrom("/usr/bin/scrollkeeper-update: "),
	var_32 Pfrom("scrollkeeper-update: ")
};
Punion union2 {
    PPword word5;
    PPpath path1;
};
Pstruct Struct_23 {
	union2 var_24;
//    PPword word5;
    PPpunc_colon colon2;
    PPwhite white5;
	PPpath var_25;
    PPpunc_colon colon3;
    PPwhite white6;
//    "No such file or directory";
    PPtext text1;
};
Punion Union_19 {
	Struct_20 var_20;
	Struct_23 var_23;
};
Punion Switch_17 {
	Pstring_ME(:"/Rebuilding ScrollKeeper database\.\.\./":) string2;
    Pstring_ME(:"/Done rebuilding ScrollKeeper database\./":) string3;  
    Union_19 union19;
};
Pstruct Struct_15 {
	PPword var_16;
    PPpunc_colon colon1;
    PPwhite white3;
	Switch_17 var_17;
};
Punion Union_5 {
	Struct_6 var_6;
	Struct_15 var_15;
};



Pstruct Struct_21 { // scrollkeeper-rebuilddb: Rebuilding ScrollKeeper database...
    PPid myid1;
    PPpunc_colon mycolon1;
    PPwhite mywhite1;
    PPtext mytext2;
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
    PPtext mytext3;
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
    PPtext mytext3;
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
    PPtext mytext1; // Installing ScrollKeeper 0.3.14...
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
