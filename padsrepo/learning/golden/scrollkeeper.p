#include "vanilla.p"
Pstruct Struct_8 {
	Pint64 var_9;
	'.';
	Pint64 var_11;
	'.';
	Pint64 var_13;
};
Pstruct Struct_6 {
	"Installing ScrollKeeper ";
	Struct_8 var_8;
	"...";
};
Penum Enum_16 {
	var_27 Pfrom("scrollkeeper-rebuilddb: "),
	var_28 Pfrom("scrollkeeper-update: ")
};
Penum Enum_18 {
	var_29 Pfrom("Rebuilding ScrollKeeper database..."),
	var_30 Pfrom("Done rebuilding ScrollKeeper database.")
};
Pstruct Struct_20 {
	"Registering ";
	PPpath var_22;
};
Penum Enum_24 {
	var_31 Pfrom("/usr/bin/scrollkeeper-update: "),
	var_32 Pfrom("scrollkeeper-update: ")
};
Pstruct Struct_23 {
	Enum_24 var_24;
	PPpath var_25;
	": No such file or directory";
};
Punion Union_19 {
	Struct_20 var_20;
	Struct_23 var_23;
};
Punion Switch_17(:Enum_16 var_16:) {
  Pswitch (var_16) {
	Pcase 0 : 	Enum_18 var_18_0;
	Pcase 1 : 	Union_19 var_19_1;
  }
};
Pstruct Struct_15 {
	Enum_16 var_16;
	Switch_17(:var_16:) var_17;
};
Punion Union_5 {
	Struct_6 var_6;
	Struct_15 var_15;
};
Precord Pstruct Struct_0 {
	PPdate var_1;
	' ';
	PPtime var_3;
	' ';
	Union_5 var_5;
};
Psource Parray entries_t {
	Struct_0[];
};
