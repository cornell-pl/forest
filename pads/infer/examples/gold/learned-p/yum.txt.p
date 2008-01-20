#include "vanilla.p"
Penum Enum_10 {
	Installed10 Pfrom("Installed"),
	Updated10 Pfrom("Updated"),
	Erased10 Pfrom("Erased")
};
Popt Pstring_ME(:"/\\+\\+/":) Opt_22;
Penum Enum_31 {
	x86_6431 Pfrom("x86_64"),
	noarch31 Pfrom("noarch"),
	i68631 Pfrom("i686"),
	i38631 Pfrom("i386")
};
Pstruct Struct_53 {
	PPstring  v_string_49;
	'.';
};
Popt Struct_53 Opt_55;
Pstruct Struct_56 {
	PPip  v_ip_42;
	'.';
	Opt_55  v_opt_55;
};
Popt Struct_56 Opt_40;
Pstruct Struct_59 {
	Opt_40  v_opt_40;
	Pint64  v_int_57;
};
Popt Struct_59 Opt_60;
Punion Union_66 {
	v_stringconst_63 Pfrom(".");
	v_stringconst_68 Pfrom(":");
	v_stringconst_73 Pfrom("_");
};
Pstruct Struct_78 {
	Union_66  v_union_66;
	Pint64  v_int_76;
};
Parray Array_35 {
	Struct_78[] : Plongest;
};
Popt PPip Opt_83;
Penum Enum_106 {
	centos4106 Pfrom("centos4"),
	rhel4106 Pfrom("rhel4"),
	RHEL4106 Pfrom("RHEL4"),
	pre5106 Pfrom("pre5"),
	rc1106 Pfrom("rc1"),
	p23106 Pfrom("p23"),
	el4106 Pfrom("el4"),
	EL4106 Pfrom("EL4"),
	c4106 Pfrom("c4"),
	EL106 Pfrom("EL")
};
Pstruct Struct_110 {
	Enum_106  v_enum_106;
	'.';
};
Popt Struct_110 Opt_112;
Pstruct Struct_100 {
	'.';
	Opt_112  v_opt_112;
};
Punion Union_113 {
	v_stringconst_95 Pfrom("E.");
	Struct_100  v_struct_100;
};
Pstruct Struct_116 {
	Union_113  v_union_113;
	Pint64  v_int_114;
};
Parray Array_89 {
	Struct_116[] : Plongest;
};
Penum Enum_133 {
	centos4133 Pfrom("centos4"),
	rhel4133 Pfrom("rhel4"),
	RHEL4133 Pfrom("RHEL4"),
	ent133 Pfrom("ent"),
	el4133 Pfrom("el4"),
	EL4133 Pfrom("EL4"),
	c4133 Pfrom("c4"),
	EL133 Pfrom("EL")
};
Penum Enum_144 {
	centos4144 Pfrom("centos4"),
	kb144 Pfrom("kb")
};
Pstruct Struct_146 {
	'.';
	Enum_144  v_enum_144;
};
Popt Struct_146 Opt_140;
Pstruct Struct_126 {
	'.';
	Enum_133  v_enum_133;
	Opt_140  v_opt_140;
};
Penum Enum_150 {
	nonptl150 Pfrom("nonptl"),
	EL4150 Pfrom("EL4")
};
Pstruct Struct_152 {
	'_';
	Enum_150  v_enum_150;
};
Punion Union_121 {
	Struct_126  v_struct_126;
	Struct_152  v_struct_152;
	PPstring  v_string_123;
	Pempty;
};
Pstruct Struct_154 {
	Opt_60  v_opt_60;
	Array_35  v_array_35;
	Opt_83  v_opt_83;
	'-';
	Pint64  v_int_91;
	Array_89  v_array_89;
	Union_121  v_union_121;
};
Popt PPstring Opt_163;
Penum Enum_174 {
	string174_0 Pfrom("CENTOS-0"),
	string174_1 Pfrom("p1-8"),
	string174_2 Pfrom("a-43"),
	string174_3 Pfrom("a-33"),
	string174_4 Pfrom("a-16"),
	string174_5 Pfrom("a-3")
};
Popt Enum_174 Opt_178;
Pstruct Struct_179 {
	Pint64  v_int_172;
	Opt_178  v_opt_178;
};
Penum Enum_182 {
	centos4182 Pfrom("centos4"),
	RHEL4182 Pfrom("RHEL4"),
	string182_2 Pfrom("EL-1"),
	EL4182 Pfrom("EL4")
};
Punion Union_180 {
	Struct_179  v_struct_179;
	Enum_182  v_enum_182;
};
Parray Array_156 {
	Union_180[] : Psep('.') && Plongest;
};
Pstruct Struct_223 {
	Pint64  v_int_158;
	Opt_163  v_opt_163;
	'.';
	Array_156  v_array_156;
};
Punion Union_155 {
	Struct_154  v_struct_154;
	Struct_223  v_struct_223;
};
Pstruct Struct_205 {
	Opt_22  v_opt_22;
	'.';
	Enum_31  v_enum_31;
	' ';
	Union_155  v_union_155;
};
Popt Struct_205 Opt_206;
Precord Pstruct Struct_219 {
	PPdate  v_date_1;
	' ';
	PPtime  v_time_6;
	' ';
	Enum_10  v_enum_10;
	": ";
	PPstring  v_string_17;
	Opt_206  v_opt_206;
};
Psource Parray entries_t {
	Struct_219[];
};
