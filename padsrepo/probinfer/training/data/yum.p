#include "basetokens.p"
Ptypedef Pstring_ME(:"/[-0-9A-Za-z_.:+]+/":) PPmyid;

Precord Pstruct entry_t {
    PPdate d;
    PPwhite white3;
    PPtime t;
    PPwhite white1;
    PPword word1;

    PPpunc_colon colon;
    PPwhite white2;
    PPmyid id1;
    Popt PPwhite white4;
//    Popt PPint int1;
//    Popt PPpunc_colon colon1;
    Popt PPmyid id2;
/*
    PPmessage message;
    Popt PPmessage message2;
*/
};

Psource Parray yum {
	entry_t[];
};

