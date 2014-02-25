#include "basetokens.p"

//Ptypedef Pstring_ME(:"/\\s*/":) space_t;

Precord Pstruct entry_t {
        PPint type;
        PPwhite space1;
        PPint id;
        PPwhite space2;
        PPfloat value1;
        PPwhite space3;
        PPfloat value2;
        PPwhite space4;
        PPfloat value3;
        PPwhite space5;
};

Psource Parray entries_t {
        entry_t[];
};                       
