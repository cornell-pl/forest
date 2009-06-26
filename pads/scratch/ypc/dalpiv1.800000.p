#include "vanilla.p"
Penum Enum_31 {
	GET31 Pfrom("GET"),
	HEAD31 Pfrom("HEAD"),
	OPTIONS31 Pfrom("OPTIONS"),
	T31 Pfrom("T")
};
Popt PPurl Opt_69;
Punion Union_38 {
	Opt_69  v_opt_69;
	PPpath  v_path_35;
};
Pstruct Struct_85 {
	"favicon.ico";
};
Punion Union_77 {
	PPpath  v_path_74;
	Struct_85  v_struct_85;
};
Pstruct Struct_86 {
	'/';
	Union_77  v_union_77;
};
Pstruct Struct_99 {
	"\?&domain=rp";
};
Punion Union_87 {
	Struct_86  v_struct_86;
	Struct_99  v_struct_99;
};
Popt Union_87 Opt_70;
Popt PPpath Opt_102;
Penum Enum_119 {
	Total119 Pfrom("Total"),
	Holt119 Pfrom("Holt")
};
Popt Enum_119 Opt_117;
Punion Union_133 {
	v_stringconst_137 Pfrom("-");
	PPstring  v_string_128;
};
Pstruct Struct_438 {
	Puint8  v_intrange_126;
	Union_133  v_union_133;
};
Parray Array_112 {
	Struct_438[] : Psep('%') && Plongest;
};
Pstruct Struct_148 {
	'*';
	Opt_117  v_opt_117;
	'%';
	Array_112  v_array_112;
};
Pstruct Struct_162 {
	"-g/carithersflowershop-g.gif";
};
Popt Pstring(:' ':) Opt_185;
Pstruct Struct_163 {
	'/';
	Opt_185  v_opt_185;
};
Penum Enum_212 {
	detroit212 Pfrom("detroit"),
	sports212 Pfrom("sports"),
	Co212 Pfrom("Co")
};
Penum Enum_227 {
	searchId227 Pfrom("searchId"),
	airport227 Pfrom("airport"),
	dome227 Pfrom("dome")
};
Penum Enum_242 {
	searchId242 Pfrom("searchId"),
	D0242 Pfrom("D0")
};
Penum Enum_257 {
	impressionId257 Pfrom("impressionId"),
	D9257 Pfrom("D9"),
	D2257 Pfrom("D2"),
	D0257 Pfrom("D0")
};
Penum Enum_272 {
	impressionId272 Pfrom("impressionId"),
	D6272 Pfrom("D6")
};
Penum Enum_287 {
	linkType287 Pfrom("linkType"),
	Dnull287 Pfrom("Dnull"),
	D63287 Pfrom("D63"),
	D61287 Pfrom("D61"),
	D34287 Pfrom("D34"),
	D18287 Pfrom("D18"),
	D17287 Pfrom("D17")
};
Penum Enum_302 {
	linkType302 Pfrom("linkType"),
	D19302 Pfrom("D19")
};
Penum Enum_317 {
	listingId317 Pfrom("listingId"),
	D19317 Pfrom("D19")
};
Penum Enum_332 {
	listingId332 Pfrom("listingId"),
	D83058283332 Pfrom("D83058283")
};
Penum Enum_362 {
	DLondon362 Pfrom("DLondon"),
	ci362 Pfrom("ci")
};
Penum Enum_377 {
	DBrewster377 Pfrom("DBrewster"),
	DDetroit377 Pfrom("DDetroit"),
	zp377 Pfrom("zp")
};
Penum Enum_392 {
	zp392 Pfrom("zp"),
	D392 Pfrom("D")
};
Penum Enum_407 {
	D48242407 Pfrom("D48242"),
	st407 Pfrom("st"),
	D407 Pfrom("D")
};
Penum Enum_418 {
	DKY418 Pfrom("DKY"),
	st418 Pfrom("st")
};
Penum Enum_425 {
	DNY425 Pfrom("DNY"),
	DMI425 Pfrom("DMI")
};
Pstruct Struct_428 {
	'%';
	Puint8  v_intconst_423 : v_intconst_423 == 3;
	Enum_425  v_enum_425;
};
Popt Struct_428 Opt_429;
Pstruct Struct_436 {
	"@%";
	Puint8  v_intconst_207 : v_intconst_207 == 20;
	Enum_212  v_enum_212;
	'%';
	Puint8  v_intrange_222;
	Enum_227  v_enum_227;
	'%';
	Puint8  v_intrange_237;
	Enum_242  v_enum_242;
	'%';
	Puint8  v_intrange_252;
	Enum_257  v_enum_257;
	'%';
	Puint8  v_intrange_267;
	Enum_272  v_enum_272;
	'%';
	Puint8  v_intrange_282;
	Enum_287  v_enum_287;
	'%';
	Puint8  v_intrange_297;
	Enum_302  v_enum_302;
	'%';
	Puint8  v_intrange_312;
	Enum_317  v_enum_317;
	'%';
	Puint8  v_intrange_327;
	Enum_332  v_enum_332;
	'%';
	Puint8  v_intrange_342;
	PPstring  v_string_347;
	'%';
	Puint8  v_intrange_357;
	Enum_362  v_enum_362;
	'%';
	Puint8  v_intrange_372;
	Enum_377  v_enum_377;
	'%';
	Puint8  v_intrange_387;
	Enum_392  v_enum_392;
	'%';
	Puint8  v_intrange_402;
	Enum_407  v_enum_407;
	'%';
	Puint8  v_intrange_416;
	Enum_418  v_enum_418;
	Opt_429  v_opt_429;
};
Punion Union_437 {
	Struct_148  v_struct_148;
	Struct_162  v_struct_162;
	Struct_163  v_struct_163;
	Struct_99  v_struct_197;
	Struct_436  v_struct_436;
};
Popt Union_437 Opt_109;
Precord Pstruct Struct_67 {
	PPip  v_ip_1;
	" - - [";
	PPdate  v_date_16;
	'+';
	PPtime  v_time_20;
	"] \"";
	Enum_31  v_enum_31;
	' ';
	Union_38  v_union_38;
	Opt_70  v_opt_70;
	Opt_102  v_opt_102;
	Opt_109  v_opt_109;
	" HTTP/";
	Pfloat64  v_float_49;
	"\" ";
	Puint8  v_intrange_61;
	' ';
	Pint64  v_int_65;
};
Psource Parray entries_t {
	Struct_67[];
};
