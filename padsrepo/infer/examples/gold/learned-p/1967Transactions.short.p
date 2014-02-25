#include "vanilla.p"
Punion Switch_56 (:Puint16 v_intrange_6:) {
  Pswitch (v_intrange_6) {
	Pcase 9100: 	Puint8  v_intrange_58_0;
	Pcase 9300: 	Puint8  v_intrange_58_1;
	Pcase 9400: 	Puint8  v_intrange_58_2;
	Pcase 9710: 	Puint8  v_intrange_58_3;
	Pcase 9720: 	Puint8  v_intrange_58_4;
	Pcase 9860: 	Puint8  v_intrange_58_5;
	Pcase 9870: 	Puint8  v_intrange_58_6;
	Pcase 9880: 	Puint8  v_intrange_58_7;
	Pcase 9890: 	Puint8  v_intrange_58_8;
	Pdefault: 	Pfloat64  v_float_49_1;
  }
};
Precord Pstruct Struct_63 {
	Puint8  v_intrange_1;
	"      ";
	Puint16  v_intrange_6;
	"      ";
	Pfloat64  v_float_19;
	PPwhite  v_white_32;
	Pfloat64  v_float_34;
	PPwhite  v_white_47;
	Switch_56 (:v_intrange_6:) v_switch_56;
	PPwhite  v_white_61;
};
Psource Parray entries_t {
	Struct_63[];
};
