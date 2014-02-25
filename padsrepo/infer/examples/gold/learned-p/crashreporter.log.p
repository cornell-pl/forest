#include "vanilla.p"
Penum Enum_15 {
	crashreporterd15 Pfrom("crashreporterd"),
	crashdump15 Pfrom("crashdump")
};
Penum Enum_31 {
	crashdump31 Pfrom("crashdump"),
	mach_msg31 Pfrom("mach_msg"),
	Finished31 Pfrom("Finished"),
	Started31 Pfrom("Started"),
	Unable31 Pfrom("Unable"),
	Failed31 Pfrom("Failed")
};
Penum Enum_35 {
	writing35 Pfrom("writing"),
	to35 Pfrom("to")
};
Penum Enum_39 {
	string39_0 Pfrom("re-launch"),
	crash39 Pfrom("crash")
};
Popt Pstring_ME(:"/report to: /":) Opt_54;
Penum Enum_82 {
	PowerPoint82 Pfrom("PowerPoint"),
	process82 Pfrom("process"),
	Word82 Pfrom("Word")
};
Pstruct Struct_95 {
	' ';
	Enum_82  v_enum_82;
	".crash.log";
};
Pstruct Struct_114 {
	PPwhite  v_white_98;
	'-';
	PPwhite  v_white_102;
	"CGSError:";
	PPwhite  v_white_108;
	Puint16  v_intconst110 : v_intconst110 == 1025;
	'!';
};
Punion Union_78 {
	Struct_95  v_struct_95;
	Struct_114  v_struct_114;
	Pempty;
};
Pstruct Struct_115 {
	' ';
	Enum_35  v_enum_35;
	' ';
	Enum_39  v_enum_39;
	' ';
	Opt_54  v_opt_54;
	PPpath  v_path_73;
	Union_78  v_union_78;
};
Popt Pstring_ME(:"/\\(\\)/":) Opt_127;
Penum Enum_139 {
	started139 Pfrom("started"),
	reply139 Pfrom("reply"),
	to139 Pfrom("to")
};
Penum Enum_152 {
	determine152 Pfrom("determine"),
	failed152 Pfrom("failed")
};
Pstruct Struct_188 {
	PPwhite  v_white_156;
	"task_t";
	PPwhite  v_white_160;
	"for";
	PPwhite  v_white_164;
	"pid:";
	PPwhite  v_white_170;
	Puint8  v_intconst172 : v_intconst172 == 95;
	PPwhite  v_white_174;
	"name:";
};
Pstruct Struct_219 {
	':';
	PPwhite  v_white_192;
	"(ipc/send)";
	PPwhite  v_white_207;
	"invalid";
};
Punion Switch_220 (:Enum_152 v_enum_152:) {
  Pswitch (v_enum_152) {
	Pcase determine152: 	Struct_188  v_struct_188;
	Pcase failed152: 	Struct_219  v_struct_219;
  }
};
Penum Enum_182 {
	destination182 Pfrom("destination"),
	Exited182 Pfrom("Exited")
};
Penum Enum_186 {
	process186 Pfrom("process"),
	port186 Pfrom("port")
};
Pstruct Struct_145 {
	' ';
	Enum_152  v_enum_152;
	Switch_220 (:v_enum_152:) v_switch_220;
	' ';
	Enum_182  v_enum_182;
	' ';
	Enum_186  v_enum_186;
};
Popt Struct_145 Opt_144;
Pstruct Struct_221 {
	Opt_127  v_opt_127;
	' ';
	Enum_139  v_enum_139;
	Opt_144  v_opt_144;
};
Punion Switch_116 (:Enum_31 v_enum_31:) {
  Pswitch (v_enum_31) {
	Pcase Failed31: 	Struct_115  v_struct_115;
	Pcase Finished31: 	Struct_115  v_struct_115_1;
	Pcase Started31: 	Struct_115  v_struct_115_2;
	Pcase Unable31: 	Struct_221  v_struct_221;
	Pcase crashdump31: 	Struct_221  v_struct_221_1;
	Pcase mach_msg31: 	Struct_221  v_struct_221_2;
  }
};
Precord Pstruct Struct_222 {
	PPdate  v_date_1;
	' ';
	PPtime  v_time_6;
	' ';
	Puint16  v_intconst11 : v_intconst11 == 2006;
	' ';
	Enum_15  v_enum_15;
	'[';
	Puint16  v_intrange_21;
	"]: ";
	Enum_31  v_enum_31;
	Switch_116 (:v_enum_31:) v_switch_116;
};
Psource Parray entries_t {
	Struct_222[];
};
