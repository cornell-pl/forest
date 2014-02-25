#include "vanilla.p"
Penum Enum_1 {
	hp_color_LaserJet_3700__378392_1 Pfrom("hp_color_LaserJet_3700__378392_"),
	officejet_7100_series1 Pfrom("officejet_7100_series"),
	Andrew_s_printer1 Pfrom("Andrew_s_printer")
};
Popt Pstring_ME(:"/\\(Printer\\)/":) Opt_6;
Precord Pstruct Struct_57 {
	Enum_1  v_enum_1;
	Opt_6  v_opt_6;
	" kfisher ";
	Puint8  v_intrange_24;
	" [";
	PPdate  v_date_31;
	':';
	PPtime  v_time_35;
	"] ";
	Pint64  v_int_43;
	' ';
	Puint8  v_intrange_47;
	" - localhost";
};
Psource Parray entries_t {
	Struct_57[];
};
