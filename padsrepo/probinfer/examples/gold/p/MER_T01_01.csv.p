#include "vanilla.p"
Penum Enum_3 {
	var_12 Pfrom("\"TEAJBUS\""),
	var_13 Pfrom("\"TEEXBUS\""),
	var_14 Pfrom("\"TEIMBUS\""),
	var_15 Pfrom("\"TEPRBUS\""),
	var_16 Pfrom("\"TETCBUS\"")
};
Pstruct Struct_2 {
	Enum_3 var_3;
	',';
	Pint64 var_5;
	',';
	Pfloat64 var_7;
	',';
	"Quadrillion Btu";
	',';
	Pint64 var_11;
};
Precord Punion Union_0 {
	var_1 Pfrom("\"MSN\",\"YYYYMM\",\"Publication Value\",\"Publication Unit\",\"Column Order\"");
	Struct_2 var_2;
};
Psource Parray entries_t {
	Union_0[];
};
