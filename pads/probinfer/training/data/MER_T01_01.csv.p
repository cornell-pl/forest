#include "basetokens.p"
Penum Enum_3 {
	var_12 Pfrom("\"TEAJBUS\""),
	var_13 Pfrom("\"TEEXBUS\""),
	var_14 Pfrom("\"TEIMBUS\""),
	var_15 Pfrom("\"TEPRBUS\""),
	var_16 Pfrom("\"TETCBUS\"")
};
Pstruct Struct_2 {
	'\"';
 	PPword word;
	'\"';
	PPpunc_comma comma1;
	PPint var_5;
	PPpunc_comma comma2;
	PPfloat var_7;
	PPpunc_comma comma3;
    PPword word1;
    PPwhite white1;
    PPword word2;             
	PPpunc_comma comma4;
	PPint var_11;
};
Precord Punion entry_t {
	var_1 Pfrom("\"MSN\",\"YYYYMM\",\"Publication Value\",\"Publication Unit\",\"Column Order\"");
	Struct_2 var_2;
};
Psource Parray entries_t {
	entry_t[];
};
