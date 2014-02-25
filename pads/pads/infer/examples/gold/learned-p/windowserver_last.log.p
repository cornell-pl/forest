#include "vanilla.p"
Popt Pstring_ME(:"/kCGErrorCannotComplete: CGXPostNotification2 : Time out waiting for reply from /":) Opt_25;
Pstruct Struct_92 {
	"for notification type ";
	Puint8  v_intrange_88;
	' ';
};
Popt Struct_92 Opt_74;
Popt Pstring_ME(:"/CID /":) Opt_100;
Penum Enum_109 {
	x1f707109 Pfrom("x1f707"),
	xd5d7109 Pfrom("xd5d7"),
	x530b109 Pfrom("x530b"),
	x468f109 Pfrom("x468f"),
	x467f109 Pfrom("x467f")
};
Pstruct Struct_126 {
	", PID ";
	Pint64  v_int_124;
};
Popt Struct_126 Opt_114;
Penum Enum_198 {
	normal198 Pfrom("normal"),
	all198 Pfrom("all")
};
Penum Enum_204 {
	disabled204 Pfrom("disabled"),
	but204 Pfrom("but")
};
Pstruct Struct_219 {
	PPwhite  v_white_211;
	PPstring  v_string_213;
	PPwhite  v_white_215;
	PPstring  v_string_217;
};
Popt Struct_219 Opt_209;
Pstruct Struct_220 {
	' ';
	Enum_204  v_enum_204;
	Opt_209  v_opt_209;
};
Popt Struct_220 Opt_222;
Pstruct Struct_131 {
	" set hot key operating mode to ";
	Enum_198  v_enum_198;
	Opt_222  v_opt_222;
};
Popt Struct_131 Opt_224;
Pstruct Struct_225 {
	' ';
	Opt_25  v_opt_25;
	PPtext  v_text_67;
	' ';
	Opt_74  v_opt_74;
	'(';
	Opt_100  v_opt_100;
	Puint8  v_intrange_107;
	Enum_109  v_enum_109;
	Opt_114  v_opt_114;
	')';
	Opt_224  v_opt_224;
};
Popt Puint8 Opt_236;
Punion Union_246 {
	v_stringconst_257 Pfrom("/N");
	v_stringconst_253 Pfrom(".");
	v_stringconst_248 Pfrom(":");
	v_stringconst_263 Pfrom(";");
	Pempty;
};
Pstruct Struct_269 {
	Opt_236  v_opt_236;
	PPstring  v_string_241;
	Union_246  v_union_246;
};
Pstruct Struct_297 {
	Pint64  v_int_291;
	',';
	Puint8  v_intrange_295;
};
Punion Union_284 {
	"gServiceSet";
	v_stringconst_275 Pfrom("ipc/send");
	Struct_297  v_struct_297;
};
Pstruct Struct_326 {
	'[';
	Pint64  v_int_310;
	PPwhite  v_white_312;
	'x';
	PPwhite  v_white_316;
	Pint64  v_int_318;
	"],";
};
Popt Struct_326 Opt_305;
Pstruct Struct_327 {
	'(';
	Union_284  v_union_284;
	')';
	Opt_305  v_opt_305;
};
Popt Pstring_ME(:"/\\;/":) Opt_344;
Pstruct Struct_345 {
	Pint64  v_int_338;
	Opt_344  v_opt_344;
};
Punion Union_270 {
	PPtext  v_text_348;
	Struct_269  v_struct_269;
	Struct_327  v_struct_327;
	Struct_345  v_struct_345;
	v_stringconst_353 Pfrom(":");
};
Pstruct Struct_441 {
	Union_270  v_union_270;
	' ';
};
Parray Array_227 {
	Struct_441[] : Plongest;
};
Penum Enum_368 {
	CGXPerformInitialDisplayConfiguration368 Pfrom("CGXPerformInitialDisplayConfiguration"),
	DashboardClient368 Pfrom("DashboardClient"),
	SecurityAgent368 Pfrom("SecurityAgent"),
	connection368 Pfrom("connection"),
	xb0015000368 Pfrom("xb0015000"),
	disabled368 Pfrom("disabled"),
	server368 Pfrom("server"),
	normal368 Pfrom("normal"),
	them368 Pfrom("them"),
	up368 Pfrom("up")
};
Popt Pstring_ME(:"/\\./":) Opt_373;
Pstruct Struct_378 {
	Opt_236  v_opt_363;
	Enum_368  v_enum_368;
	Opt_373  v_opt_373;
};
Popt PPwhite Opt_391;
Penum Enum_396 {
	last396 Pfrom("last"),
	off396 Pfrom("off")
};
Penum Enum_401 {
	console401 Pfrom("console"),
	RPC401 Pfrom("RPC")
};
Pstruct Struct_417 {
	PPwhite  v_white_411;
	',';
};
Punion Switch_409 (:Enum_401 v_enum_401:) {
  Pswitch (v_enum_401) {
	Pcase console401: 	Struct_417  v_struct_417;
	Pcase RPC401: 	v_stringconst_403 Pfrom(":");
  }
};
Penum Enum_418 {
	GetPortStreamOutofline418 Pfrom("GetPortStreamOutofline"),
	defunct418 Pfrom("defunct")
};
Pstruct Struct_420 {
	Opt_391  v_opt_391;
	Enum_396  v_enum_396;
	' ';
	Enum_401  v_enum_401;
	Switch_409 (:v_enum_401:) v_switch_409;
	' ';
	Enum_418  v_enum_418;
};
Pstruct Struct_433 {
	PPwhite  v_white_423;
	PPstring  v_string_425;
	PPwhite  v_white_427;
	PPstring  v_string_429;
	PPwhite  v_white_431;
};
Punion Union_421 {
	Struct_420  v_struct_420;
	Struct_433  v_struct_433;
};
Pstruct Struct_438 {
	'(';
	Union_421  v_union_421;
	')';
};
Punion Union_379 {
	Struct_378  v_struct_378;
	Struct_438  v_struct_438;
	Puint16  v_intrange_381;
};
Pstruct Struct_443 {
	PPwhite  v_white_229;
	Array_227  v_array_227;
	Union_379  v_union_379;
};
Punion Union_226 {
	Struct_225  v_struct_225;
	Struct_443  v_struct_443;
};
Precord Pstruct Struct_440 {
	PPdate  v_date_1;
	' ';
	PPtime  v_time_6;
	"  [";
	Puint8  v_intrange_14;
	']';
	Union_226  v_union_226;
};
Psource Parray entries_t {
	Struct_440[];
};
