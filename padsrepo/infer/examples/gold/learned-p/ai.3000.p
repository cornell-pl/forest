#include "vanilla.p"
Ptypedef Puint8 Intconst17 : Intconst17 x => {x == 130};
Popt Intconst17 Opt_15;
Parray Array_10 {
	PPstring[] : Psep('.') && Plongest;
};
Pstruct Struct_150 {
	Opt_15  v_opt_15;
	PPstring  v_string_20;
	'.';
	Array_10  v_array_10;
};
Punion Union_4 {
	PPip  v_ip_1;
	PPhostname  v_host_6;
	Struct_150  v_struct_150;
};
Punion Union_45 {
	v_stringconst_42 Pfrom("-");
	PPstring  v_string_47;
};
Penum Enum_70 {
	POST70 Pfrom("POST"),
	GET70 Pfrom("GET")
};
Penum Enum_90 {
	html90 Pfrom("html"),
	txt90 Pfrom("txt")
};
Popt Pstring_ME(:"/\\*\\*/":) Opt_95;
Pstruct Struct_103 {
	PPstring  v_string_85;
	'.';
	Enum_90  v_enum_90;
	Opt_95  v_opt_95;
};
Penum Enum_106 {
	education106 Pfrom("education"),
	casework106 Pfrom("casework"),
	refugee106 Pfrom("refugee"),
	country106 Pfrom("country"),
	amnesty106 Pfrom("amnesty"),
	urgact106 Pfrom("urgact"),
	argact106 Pfrom("argact")
};
Popt Pstring_ME(:"/\\//":) Opt_111;
Pstruct Struct_113 {
	Enum_106  v_enum_106;
	Opt_111  v_opt_111;
};
Punion Union_104 {
	Struct_103  v_struct_103;
	Struct_113  v_struct_113;
	Pempty;
};
Pstruct Struct_79 {
	'/';
	Union_104  v_union_104;
};
Punion Union_78 {
	PPpath  v_path_75;
	Struct_79  v_struct_79;
};
Punion Union_144 {
	v_stringconst_146 Pfrom("-");
	Pint64  v_int_141;
};
Precord Pstruct Struct_149 {
	Union_4  v_union_4;
	" - ";
	Union_45  v_union_45;
	" [";
	PPdate  v_date_55;
	':';
	PPtime  v_time_59;
	"] \"";
	Enum_70  v_enum_70;
	' ';
	Union_78  v_union_78;
	" HTTP/";
	Pfloat64  v_float_123;
	"\" ";
	Puint16  v_intrange_136;
	' ';
	Union_144  v_union_144;
};
Psource Parray entries_t {
	Struct_149[];
};
