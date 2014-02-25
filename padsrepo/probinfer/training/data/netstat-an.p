#include "basetokens.p"
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
Punion ipoption {
    PPip ip1;
    PPpunc_star star1;
};
Punion portoption {
    PPint int3;
    PPpunc_star star2;
};
Pstruct Struct_3 {
    PPid id1;
    PPwhite white1;
    PPint int1;
    PPwhite white2;
    PPint int2;
    PPwhite white3;
    ipoption myipoption;
    PPpunc_dot dot1;
    portoption myportoption;
    PPwhite white4;
    ipoption myipoption1;
    PPpunc_dot dot2;
    portoption myportoption1;
    PPwhite white5;
    Popt PPword word1;
    Popt PPpunc_underscore underscore;
    Popt PPword word5;
};
Penum Enum_42 {
	stream42 Pfrom("stream"),
	dgram42 Pfrom("dgram")
};
Popt Pstring_ME(:"/[ 	]+/":)  Opt_55;
Popt PPpath  Opt_57;
Pstruct Struct_38 {
    PPwhite white14;
    PPid id2;
    PPwhite white6;
    PPword word2;
    PPwhite white7;
    PPint int3;
    PPwhite white8;
    PPint int4;
    PPwhite white9;
    PPid id3;
    PPwhite white10;
    PPid id4;
    PPwhite white11;
    PPid id5;
    PPwhite white12;
    PPid id6;
    Popt PPwhite white13;
    Popt PPpath path1;
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
