#include "vanilla.p"
Penum Enum_1 {
	udp61 Pfrom("udp6"),
	udp41 Pfrom("udp4"),
	tcp61 Pfrom("tcp6"),
	tcp41 Pfrom("tcp4"),
	icm61 Pfrom("icm6")
};
Punion Union_16 {
	PPip  v_ip_13;
	v_stringconst_18 Pfrom("*");
};
Punion Union_27 {
	v_stringconst_29 Pfrom("*");
	Pint64  v_int_24;
};
Punion Union_41 {
	PPip  v_ip_34;
	v_stringconst_38 Pfrom("*");
};
Punion Union_52 {
	v_stringconst_49 Pfrom("*");
	Pint64  v_int_45;
};
Penum Enum_55 {
	ESTABLISHED55 Pfrom("ESTABLISHED"),
	CLOSE_WAIT55 Pfrom("CLOSE_WAIT"),
	LISTEN55 Pfrom("LISTEN"),
	CLOSED55 Pfrom("CLOSED")
};
Popt Enum_55 Opt_58;
Pstruct Struct_61 {
	Enum_1  v_enum_1;
	PPwhite  v_white_3;
	Pint64  v_int_5;
	"      ";
	Puint8  v_intrange_9;
	"  ";
	Union_16  v_union_16;
	'.';
	Union_52  v_union_27;
	PPwhite  v_white_32;
	Union_41  v_union_41;
	'.';
	Union_52  v_union_52;
	PPwhite  v_white_53;
	Opt_58  v_opt_58;
};
Popt PPstring Opt_72;
Penum Enum_76 {
	stream76 Pfrom("stream"),
	dgram76 Pfrom("dgram")
};
Pstruct Struct_144 {
	' ';
	PPpath  v_path_142;
};
Punion Union_133 {
	Struct_144  v_struct_144;
	PPstring  v_string_135;
	Pempty;
};
Pstruct Struct_145 {
	' ';
	Pint64  v_int_67;
	Opt_72  v_opt_72;
	' ';
	Enum_76  v_enum_76;
	PPwhite  v_white_79;
	Pint64  v_int_82;
	"      ";
	Puint8  v_intrange_87;
	PPwhite  v_white_89;
	Pint64  v_int_92;
	Opt_72  v_opt_97;
	PPwhite  v_white_102;
	Pint64  v_int_105;
	Opt_72  v_opt_110;
	PPwhite  v_white_112;
	Pint64  v_int_115;
	Opt_72  v_opt_120;
	PPwhite  v_white_125;
	Pint64  v_int_128;
	Union_133  v_union_133;
};
Penum Enum_149 {
	Active149 Pfrom("Active"),
	Proto149 Pfrom("Proto")
};
Penum Enum_195 {
	Foreign195 Pfrom("Foreign"),
	Address195 Pfrom("Address"),
	Local195 Pfrom("Local")
};
Pstruct Struct_276 {
	Enum_195  v_enum_195;
	PPwhite  v_white_197;
};
Parray Array_199 {
	Struct_276[4] : Plongest;
};
Popt Array_199 Opt_201;
Pstruct Struct_172 {
	PPstring  v_string_174;
	PPwhite  v_white_179;
	Opt_201  v_opt_201;
};
Popt Struct_172 Opt_171;
Pstruct Struct_219 {
	PPwhite  v_white_215;
	PPstring  v_string_217;
};
Popt Struct_219 Opt_213;
Pstruct Struct_237 {
	PPwhite  v_white_229;
	PPstring  v_string_231;
	PPwhite  v_white_233;
	PPstring  v_string_235;
};
Popt Struct_237 Opt_227;
Pstruct Struct_238 {
	Enum_149  v_enum_149;
	PPwhite  v_white_154;
	PPstring  v_string_159;
	PPwhite  v_white_164;
	Opt_171  v_opt_171;
	'(';
	PPstring  v_string_206;
	Opt_213  v_opt_213;
	')';
	Opt_227  v_opt_227;
};
Pstruct Struct_275 {
	PPstring  v_string_241;
	PPwhite  v_white_243;
	PPstring  v_string_245;
	PPwhite  v_white_247;
	PPstring  v_string_249;
	PPwhite  v_white_251;
	PPstring  v_string_253;
	PPwhite  v_white_255;
	PPstring  v_string_257;
	PPwhite  v_white_259;
	PPstring  v_string_261;
	PPwhite  v_white_263;
	PPstring  v_string_265;
	PPwhite  v_white_267;
	PPstring  v_string_269;
	PPwhite  v_white_271;
	PPstring  v_string_273;
};
Precord Punion Union_62 {
	Struct_61  v_struct_61;
	Struct_145  v_struct_145;
	Struct_238  v_struct_238;
	Struct_275  v_struct_275;
};
Psource Parray entries_t {
	Union_62[];
};
