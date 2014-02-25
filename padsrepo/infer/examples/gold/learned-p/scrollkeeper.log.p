#include "vanilla.p"
Penum Enum_10 {
	string10_0 Pfrom("scrollkeeper-rebuilddb"),
	string10_1 Pfrom("scrollkeeper-update"),
	Installing10 Pfrom("Installing")
};
Penum Enum_17 {
	string17_0 Pfrom("scrollkeeper-update"),
	Registering17 Pfrom("Registering")
};
Popt Pstring_ME(:"/:/":) Opt_22;
Pstruct Struct_29 {
	Enum_17  v_enum_17;
	Opt_22  v_opt_22;
	' ';
};
Popt Struct_29 Opt_30;
Pstruct Struct_69 {
	PPpath  v_path_63;
	": ";
};
Popt Struct_69 Opt_61;
Pstruct Struct_92 {
	": ";
	Opt_61  v_opt_61;
	"No such file or directory";
};
Popt Struct_92 Opt_46;
Pstruct Struct_93 {
	": ";
	Opt_30  v_opt_30;
	PPpath  v_path_41;
	Opt_46  v_opt_46;
};
Pstruct Struct_145 {
	PPwhite  v_white_138;
	PPstring  v_string_140;
};
Punion Union_146 {
	Struct_145  v_struct_145;
	v_stringconst_150 Pfrom("..");
};
Pstruct Struct_157 {
	':';
	PPwhite  v_white_111;
	PPstring  v_string_116;
	PPwhite  v_white_121;
	PPstring  v_string_126;
	PPwhite  v_white_131;
	PPstring  v_string_135;
	Union_146  v_union_146;
};
Pstruct Struct_186 {
	PPwhite  v_white_164;
	PPstring  v_string_166;
	PPwhite  v_white_168;
	Pfloat64  v_float_170;
	'.';
	Puint8  v_intconst178 : v_intconst178 == 14;
	"..";
};
Punion Union_158 {
	Struct_157  v_struct_157;
	Struct_186  v_struct_186;
};
Pstruct Struct_187 {
	Union_158  v_union_158;
	'.';
};
Punion Switch_94 (:Enum_10 v_enum_10:) {
  Pswitch (v_enum_10) {
	Pcase string10_1: 	Struct_93  v_struct_93;
	Pcase Installing10: 	Struct_187  v_struct_187;
	Pcase string10_0: 	Struct_187  v_struct_187_1;
  }
};
Precord Pstruct Struct_191 {
	PPdate  v_date_1;
	' ';
	PPtime  v_time_6;
	' ';
	Enum_10  v_enum_10;
	Switch_94 (:v_enum_10:) v_switch_94;
};
Psource Parray entries_t {
	Struct_191[];
};
