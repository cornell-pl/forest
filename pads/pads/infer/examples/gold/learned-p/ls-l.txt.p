#include "vanilla.p"
Precord Pstruct Struct_95 {
	PPstring  v_string_89;
	PPwhite  v_white_91;
	Puint32  v_intconst93 : v_intconst93 == 275528;
};
Popt Pstring_ME(:"/\\-/":) Opt_5;
Penum Enum_6 {
	string6_0 Pfrom("drwxr-xr-x"),
	string6_1 Pfrom("drwx------"),
	string6_2 Pfrom("rw-r--r--"),
	string6_3 Pfrom("rw-r-----")
};
Pstruct Struct_45 {
	':';
	Puint8  v_intrange_43;
};
Popt Struct_45 Opt_39;
Penum Enum_60 {
	zip60 Pfrom("zip"),
	txt60 Pfrom("txt"),
	pst60 Pfrom("pst"),
	pdf60 Pfrom("pdf"),
	doc60 Pfrom("doc"),
	ps60 Pfrom("ps")
};
Pstruct Struct_71 {
	'.';
	PPstring  v_string_69;
};
Punion Union_65 {
	Struct_71  v_struct_71;
	v_stringconst_73 Pfrom("~");
	Pempty;
};
Pstruct Struct_77 {
	'.';
	Enum_60  v_enum_60;
	Union_65  v_union_65;
};
Pstruct Struct_84 {
	PPwhite  v_white_80;
	Puint8  v_intconst82 : v_intconst82 == 8;
};
Punion Union_56 {
	Struct_77  v_struct_77;
	Struct_84  v_struct_84;
	Pempty;
};
Precord Pstruct Struct_86 {
	Opt_5  v_opt_5;
	Enum_6  v_enum_6;
	PPwhite  v_white_8;
	Pint64  v_int_10;
	" dpw fac";
	PPwhite  v_white_20;
	Pint64  v_int_22;
	' ';
	PPdate  v_date_27;
	PPwhite  v_white_29;
	Puint16  v_intrange_32;
	Opt_39  v_opt_39;
	' ';
	PPstring  v_string_49;
	Union_56  v_union_56;
};
Psource Pstruct Struct_87 {
	Struct_95 v_Struct_95;
	Struct_86[] v_Struct_86 : Plongest;
};
