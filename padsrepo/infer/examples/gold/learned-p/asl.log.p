#include "vanilla.p"
Penum Enum_31 {
	authpriv31 Pfrom("authpriv"),
	install31 Pfrom("install"),
	daemon31 Pfrom("daemon"),
	user31 Pfrom("user"),
	auth31 Pfrom("auth")
};
Penum Enum_49 {
	Solutions49 Pfrom("Solutions"),
	Software49 Pfrom("Software"),
	Database49 Pfrom("Database"),
	Support49 Pfrom("Support"),
	Sender49 Pfrom("Sender"),
	Office49 Pfrom("Office"),
	Earth49 Pfrom("Earth"),
	UI49 Pfrom("UI"),
	X49 Pfrom("X")
};
Penum Enum_58 {
	Microsoft58 Pfrom("Microsoft"),
	Norton58 Pfrom("Norton"),
	Google58 Pfrom("Google")
};
Pstruct Struct_60 {
	'/';
	Enum_58  v_enum_58;
};
Popt Struct_60 Opt_54;
Pstruct Struct_61 {
	Enum_49  v_enum_49;
	Opt_54  v_opt_54;
};
Punion Union_62 {
	PPpath  v_path_64;
	Struct_61  v_struct_61;
};
Pstruct Struct_69 {
	Union_62  v_union_62;
	' ';
};
Parray Array_41 {
	Struct_69[] : Plongest;
};
Pstruct Struct_103 {
	'.';
	PPstring  v_string_97;
	'.';
	PPstring  v_string_101;
};
Punion Union_76 {
	v_stringconst_84 Pfrom("(Printer)");
	Struct_103  v_struct_103;
	v_stringconst_73 Pfrom(",");
	Pempty;
};
Pstruct Struct_104 {
	PPstring  v_string_71;
	Union_76  v_union_76;
};
Punion Union_105 {
	PPpath  v_path_107;
	Struct_104  v_struct_104;
};
Popt Pstring_ME(:"/_/":) Opt_159;
Pstruct Struct_164 {
	'_';
	Opt_159  v_opt_159;
};
Punion Union_152 {
	Struct_164  v_struct_164;
	Puint8  v_intrange_167;
	Pempty;
};
Punion Union_188 {
	PPip  v_ip_185;
	PPstring  v_string_190;
};
Pstruct Struct_197 {
	'(';
	Union_188  v_union_188;
	')';
};
Popt Struct_197 Opt_180;
Pstruct Struct_200 {
	Opt_180  v_opt_180;
	':';
};
Punion Union_175 {
	Struct_200  v_struct_200;
	v_stringconst_203 Pfrom("\'t");
	v_stringconst_209 Pfrom(",");
	v_stringconst_213 Pfrom(".");
	v_stringconst_217 Pfrom("=");
	Pempty;
};
Pstruct Struct_221 {
	Union_152  v_union_152;
	PPstring  v_string_170;
	Union_175  v_union_175;
};
Pstruct Struct_277 {
	" =";
	Puint8  v_intrange_243;
	"xffff9d24, secErrStr=User interaction is not allowed. ";
};
Punion Union_278 {
	PPip  v_ip_280;
	Struct_277  v_struct_277;
};
Popt Pstring_ME(:"/\\;/":) Opt_290;
Pstruct Struct_295 {
	'(';
	Union_278  v_union_278;
	')';
	Opt_290  v_opt_290;
};
Pstruct Struct_312 {
	'[';
	Puint16  v_intconst300 : v_intconst300 == 263;
	"\\]:";
	Puint16  v_intconst310 : v_intconst310 == 19227;
};
Pstruct Struct_320 {
	'#';
	Puint8  v_intrange_316;
	',';
};
Parray Array_328 {
	Pstring_ME(:"/\\*/":)[3] : Plongest;
};
Ptypedef Puint16 Intconst332 : Intconst332 x => {x == 25308};
Popt Intconst332 Opt_335;
Pstruct Struct_337 {
	'-';
	Opt_335  v_opt_335;
};
Punion Union_222 {
	PPtime  v_time_224;
	PPdate  v_date_228;
	PPtext  v_text_343;
	Struct_221  v_struct_221;
	Struct_295  v_struct_295;
	Struct_312  v_struct_312;
	Struct_320  v_struct_320;
	Array_328  v_array_328;
	Struct_337  v_struct_337;
	Puint8  v_intrange_232;
	v_stringconst_339 Pfrom("=");
};
Pstruct Struct_349 {
	Union_222  v_union_222;
	PPwhite  v_white_347;
};
Parray Array_141 {
	Struct_349[] : Plongest;
};
Pstruct Struct_365 {
	PPstring  v_string_361;
	'=';
};
Punion Union_354 {
	Struct_365  v_struct_365;
	v_stringconst_356 Pfrom("-");
	Pempty;
};
Punion Union_373 {
	v_stringconst_368 Pfrom(";;");
	v_stringconst_380 Pfrom(";");
	PPstring  v_string_385;
	Pempty;
};
Pstruct Struct_388 {
	Union_354  v_union_354;
	Pint64  v_int_366;
	Union_373  v_union_373;
};
Popt Pstring_ME(:"/\\./":) Opt_396;
Pstruct Struct_398 {
	PPstring  v_string_391;
	Opt_396  v_opt_396;
};
Punion Union_389 {
	v_stringconst_402 Pfrom("(current= WiFi)");
	PPpath  v_path_424;
	PPtext  v_text_419;
	Struct_388  v_struct_388;
	Struct_398  v_struct_398;
};
Precord Pstruct Struct_495 {
	"[Time ";
	PPdate  v_date_8;
	' ';
	PPtime  v_time_12;
	" UTC] [Facility ";
	Enum_31  v_enum_31;
	"] [";
	Array_41  v_array_41;
	Union_105  v_union_105;
	"] [PID ";
	Pint64  v_int_131;
	"] [";
	Array_141  v_array_141;
	Union_389  v_union_389;
	"] [Level ";
	Puint8  v_intrange_440;
	"] [UID ";
	Pint8  v_intrange_457;
	"] [GID ";
	Pint8  v_intrange_474;
	"] [Host Babylon]";
};
Psource Parray entries_t {
	Struct_495[];
};
