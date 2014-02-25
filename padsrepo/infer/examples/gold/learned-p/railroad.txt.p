#include "vanilla.p"
Popt Pstring_ME(:"/:/":) Opt_10;
Pstruct Struct_17 {
	Opt_10  v_opt_10;
	PPwhite  v_white_15;
};
Popt Struct_17 Opt_18;
Popt Pstring_ME(:"/\\;/":) Opt_26;
Pstruct Struct_31 {
	PPstring  v_string_21;
	Opt_26  v_opt_26;
};
Pstruct Struct_45 {
	Pint8  v_intconst41 : v_intconst41 == -9;
	':';
};
Punion Switch_50 (:Pint64 v_int_35:) {
  Pswitch (v_int_35) {
	Pcase 1: 	Struct_45  v_struct_45_0;
	Pcase 1992: 	v_stringconst_47_1 Pfrom(";");
  }
};
Pstruct Struct_33 {
	Pint64  v_int_35;
	Switch_50 (:v_int_35:) v_switch_50;
};
Punion Union_32 {
	Struct_31  v_struct_31;
	Struct_33  v_struct_33;
	v_stringconst_52 Pfrom("-");
	v_stringconst_56 Pfrom("/");
	v_stringconst_60 Pfrom("=");
};
Pstruct Struct_66 {
	Union_32  v_union_32;
	PPwhite  v_white_64;
};
Parray Array_5 {
	Struct_66[] : Plongest;
};
Pstruct Struct_87 {
	'(';
	PPstring  v_string_81;
	')';
};
Punion Union_71 {
	Struct_87  v_struct_87;
	PPstring  v_string_68;
	Pempty;
};
Pstruct Struct_0 {
	PPstring  v_string_2;
	Opt_18  v_opt_18;
	Array_5  v_array_5;
	Union_71  v_union_71;
};
Pstruct Struct_210 {
	'\"';
	PPstring  v_string_91;
	'.';
	PPstring  v_string_95;
	'.';
	PPwhite  v_white_99;
	PPstring  v_string_101;
	PPwhite  v_white_103;
	PPstring  v_string_105;
	PPwhite  v_white_107;
	PPstring  v_string_109;
	',';
	PPwhite  v_white_113;
	PPstring  v_string_115;
	PPwhite  v_white_117;
	PPstring  v_string_119;
	PPwhite  v_white_121;
	PPstring  v_string_123;
	',';
	PPwhite  v_white_127;
	PPstring  v_string_129;
	PPwhite  v_white_131;
	PPstring  v_string_133;
	PPwhite  v_white_135;
	PPstring  v_string_137;
	PPwhite  v_white_139;
	'(';
	PPstring  v_string_144;
	',';
	PPwhite  v_white_148;
	PPstring  v_string_150;
	':';
	PPwhite  v_white_154;
	PPstring  v_string_156;
	PPwhite  v_white_158;
	PPstring  v_string_160;
	"),";
	PPwhite  v_white_168;
	PPstring  v_string_170;
	PPwhite  v_white_172;
	Puint8  v_intconst174 : v_intconst174 == 21;
	',';
	PPwhite  v_white_178;
	PPstring  v_string_180;
	PPwhite  v_white_182;
	PPstring  v_string_184;
	PPwhite  v_white_186;
	PPurl  v_url_188;
	PPstring  v_string_190;
	PPwhite  v_white_192;
	PPstring  v_string_194;
	PPwhite  v_white_196;
	PPstring  v_string_198;
	PPwhite  v_white_200;
	PPdate  v_date_202;
	Puint8  v_intconst204 : v_intconst204 == 4;
	".\"";
};
Punion Union_216 {
	PPtext  v_text_212;
	Struct_0  v_struct_0;
	Struct_210  v_struct_210;
	Pempty;
};
Pstruct Struct_239 {
	PPstring  v_string_229;
	PPwhite  v_white_231;
	PPstring  v_string_233;
	PPwhite  v_white_235;
	PPstring  v_string_237;
};
Punion Union_222 {
	PPtext  v_text_219;
	Struct_239  v_struct_239;
	Pempty;
};
Penum Enum_252 {
	NA252 Pfrom("NA"),
	U252 Pfrom("U")
};
Punion Union_245 {
	Struct_239  v_struct_267;
	Enum_252  v_enum_252;
	Pint64  v_int_242;
	Pempty;
};
Punion Union_273 {
	Enum_252  v_enum_275;
	Pint64  v_int_270;
	Pempty;
};
Punion Union_321 {
	v_stringconst_323 Pfrom("U");
	Pint64  v_int_318;
	Pempty;
};
Punion Union_333 {
	Pint64  v_int_330;
	PPstring  v_string_340;
	Pempty;
};
Parray Array_374 {
	PPstring[4] : Psep(' ') && Plongest;
};
Punion Union_348 {
	Array_374  v_array_374;
	Enum_252  v_enum_355;
	Pint64  v_int_345;
	Pempty;
};
Precord Pstruct Struct_450 {
	Union_216  v_union_216;
	',';
	Union_222  v_union_222;
	',';
	Union_245  v_union_245;
	',';
	Union_273  v_union_273;
	',';
	Union_273  v_union_285;
	',';
	Union_273  v_union_297;
	',';
	Union_273  v_union_309;
	',';
	Union_321  v_union_321;
	',';
	Union_333  v_union_333;
	',';
	Union_348  v_union_348;
	',';
	Union_273  v_union_380;
	',';
	Union_273  v_union_392;
	',';
	Union_273  v_union_404;
	',';
	Union_273  v_union_416;
	',';
	Union_321  v_union_428;
	',';
	Union_333  v_union_440;
};
Psource Parray entries_t {
	Struct_450[];
};
