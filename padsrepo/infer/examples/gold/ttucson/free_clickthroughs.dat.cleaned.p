#include "vanilla.p"
Punion Union_32 {
	Puint32  v_intrange_34;
	PPstring  v_string_29;
	Pempty;
};
Punion Union_80 {
	null Pfrom ("null");
	Puint8  v_intrange_39;
};
Pstruct Struct_94 {
	':';
	Puint8  v_intconst_88 : v_intconst_88 == 1;
	':';
	Puint8  v_intconst_92 : v_intconst_92 == 5;
};
Pstruct Struct_102 {
	PPdate  v_date_44;
	' ';
	PPtime  v_time_48;
};
Precord Pstruct Struct_55 {
//	Popt Pint64 v_opt_4;
//	PPstring  v_string_9;
        Pstring_ME(:"/[0-9A-Z]+/":) v_string_9;
	'|';
	Pint64  v_int_14;
	'|';
	Pint64  v_int_19;
	'|';
	Popt Pint64 v_opt_60;
	'|';
	Union_32  v_union_32;
	'|';
	Union_80 v_union_80;
	'|';
	Popt Struct_102 v_opt_67;
	'|';
	PPstring  v_string_52;
};
Psource Parray entries_t {
	Struct_55[];
};
