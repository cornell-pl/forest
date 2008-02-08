#include "vanilla.p"
Penum Enum_4 {
	tcp44 Pfrom("tcp4"),
	tcp64 Pfrom("tcp6"),
	udp44 Pfrom("udp4"),
	udp64 Pfrom("udp6"),
	icm64 Pfrom("icm6")
};
Pstruct Struct_11 {
	PPip var_12;
	'.';
	Pint64 var_14;
};
Punion Union_18 {
	Pint64 var_19;
	var_20 Pfrom('*');
};
Pstruct Struct_15 {
	'*';
	'.';
	Union_18 var_18;
};
Punion Union_10 {
	Struct_11 var_11;
	Struct_15 var_15;
};
Pstruct Struct_23 {
	PPip var_24;
	'.';
	Pint64 var_26;
};
Punion Union_30 {
		var_32 Pfrom('*');
};
Pstruct Struct_27 {
	'*';
	'.';
	Union_30 var_30;
};
Punion Union_22 {
	Struct_23 var_23;
	Struct_27 var_27;
};
Penum Enum_35 {
	CLOSE_WAIT35 Pfrom("CLOSE_WAIT"),
	ESTABLISHED35 Pfrom("ESTABLISHED"),
	LISTEN35 Pfrom("LISTEN"),
	CLOSED35 Pfrom("CLOSED")
};
Popt Enum_35 Opt_34;
Pstruct Struct_3 {
	Enum_4 var_4;
	Pstring_ME(:"/[ 	]+/":) var_5;
	Pint64 var_6;
	Pstring_ME(:"/[ 	]+/":) var_7;
	Pint64 var_8;
	Pstring_ME(:"/[ 	]+/":) var_9;
	Union_10 var_10;
	Pstring_ME(:"/[ 	]+/":) var_21;
	Union_22 var_22;
	Pstring_ME(:"/[ 	]+/":) var_33;
	Opt_34 var_34;
};
Penum Enum_42 {
	stream42 Pfrom("stream"),
	dgram42 Pfrom("dgram")
};
Popt Pstring_ME(:"/[ 	]+/":)  Opt_55;
Popt PPpath  Opt_57;
Pstruct Struct_38 {
	Pstring_ME(:"/[ 	]+/":) var_39;
	Pstring_ME(:"/[0-9a-f]+/":) var_40;
	Pstring_ME(:"/[ 	]+/":) var_41;
	Enum_42 var_42;
	Pstring_ME(:"/[ 	]+/":) var_43;
	Pint64 var_44;
	Pstring_ME(:"/[ 	]+/":) var_45;
	Pint64 var_46;
	Pstring_ME(:"/[ 	]+/":) var_47;
	Pstring_ME(:"/[0-9a-f]+/":) var_48;
	Pstring_ME(:"/[ 	]+/":) var_49;
	Pstring_ME(:"/[0-9a-f]+/":) var_50;
	Pstring_ME(:"/[ 	]+/":) var_51;
	Pstring_ME(:"/[0-9a-f]+/":) var_52;
	Pstring_ME(:"/[ 	]+/":) var_53;
	Pstring_ME(:"/[0-9a-f]+/":) var_54;
	Opt_55 var_55;
	Opt_57 var_57;
};
Precord Punion Union_0 {
	var_1 Pfrom("Active Internet connections (including servers)");
	var_2 Pfrom("Proto Recv-Q Send-Q  Local Address          Foreign Address        (state)");
	Struct_3 var_3;
	var_36 Pfrom("Active LOCAL (UNIX) domain sockets");
	var_37 Pfrom("Address  Type   Recv-Q Send-Q    Inode     Conn     Refs  Nextref Addr");
	Struct_38 var_38;
};
Psource Parray entries_t {
	Union_0[];
};
