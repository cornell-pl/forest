#include "/n/fs/pads/pads/probinfer/training/data/basetoken.p"

//Ptypedef Pstring_ME(:"/\\s*/":) space_t;

Precord Pstruct entry_t {
        Puint16_FW(:4:) type;
        PPwhite space1;
        Puint16_FW(:4:) id;
        PPwhite space2;
        Pfloat32 value1;
        PPwhite space3;
        Pfloat32 value2;
        PPwhite space4;
        Pfloat32 value3;
        PPwhite space5;
};

Psource Parray entries_t {
        entry_t[];
};                       
