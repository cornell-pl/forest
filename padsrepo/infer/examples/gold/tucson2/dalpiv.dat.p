#include "vanilla.p"
Pstruct Struct_78 {
	'/';
	PPhostname  v_host_62;
};
Punion Union_52 {
	PPpath  v_path_35;
	Struct_78  v_struct_78;
};
Pstruct Struct_368 {
	PPpath  v_path_93;
	Pstring(:'"':)  v_blob_98;
};
Punion Union_369 {
	Struct_368  v_struct_368;
	v_stringconst_371 Pfrom("-");
};

Pstruct dash_t {
        '-';
        Pstring_SE (:"/ /":) v_null;
};

Punion Str_opt {
        PPstring v_str_id;
        dash_t v_dash;
        //v_nothing_str Pfrom("-");
}; 

Punion Int_opt {
        Pint64 v_response_id;
        v_nothing_int Pfrom("-");
};

Punion value_t {
        Pstring_SE(:"/;/":) v_val_1;
        Pstring_SE(:Peor:) v_val_last;
};
Pstruct key_val_t {
        Popt PPwhite v_space_b4_key;
        Pstring_ME(:"/[a-zA-Z0-9_\\-.$]+/":) v_key;
        Popt Pstring_ME(:"/=/":) v_eq;
        Popt value_t v_val;
};
Punion key_val_opt{
        key_val_t v_true_key_val;
        PPwhite v_single_space;
        Pempty;
};
Parray query_string_t {
        key_val_opt[]: Psep(';') && Plongest;
};

Punion query_string_opt {
        v_dash_only Pfrom("-");
        query_string_t v_normal_queries;
        Pempty;
};

Punion int_or_str {
        Puint32 v_it;
        PPstring v_str;
};

Precord Pstruct entry_t {
	PPip  v_ip_1;
	" - - [";
	PPdate  v_date_16;
	':';
	PPtime  v_time_20;
	"] \"";
	PPstring  v_string_31;
	' ';
        Pstring_SE(:"/ HTTP/":) v_local_path;
//	Union_52  v_union_52;
	" HTTP/";
	Pfloat64  v_float_43;
	"\" ";
	int_or_str v_it_str;
	' ';
	Pint64  v_int_86;
	" \"";
	Pstring_SE(:"/\" /":) v_url;
	"\" \"";
	Pstring_SE(:"/\" /":)  v_blob_1019;
	"\" ";
	Str_opt v_string_1043;
	' ';
	Int_opt v_int_1047;
        Popt PPwhite v_space_3;
        query_string_opt v_qs;
};
Psource Parray entries_t {
	entry_t[];
};
