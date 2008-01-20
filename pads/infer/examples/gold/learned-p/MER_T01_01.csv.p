#include "vanilla.p"
Precord Parray Array_56 {
	PPtext[5] : Psep(',') && Plongest;
};
Precord Pstruct Struct_35 {
	PPtext  v_text_1;
	',';
	Puint32  v_intrange_5;
	',';
	Pfloat64  v_float_17;
	",Quadrillion Btu,";
	Puint8  v_intrange_33;
};
Psource Pstruct Struct_36 {
	Array_56 v_Array_56;
	Struct_35[] v_Struct_35 : Plongest;
};
