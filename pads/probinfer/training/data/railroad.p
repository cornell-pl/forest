#include "basetokens.p"

Pstruct secHeader {
        PPtext text1;
};
Punion title {
        Pstring_ME(: "/\"[^\"]\*\"/" :) noteinquotes; 
        Pstring_ME(: "/[^,]\*/" :) noteoutsidequotes; 
};
Pstruct comments {
	PPmessage message1;
	PPtext text2;
};
Popt Pstring_ME(: "/\\s/" :) spaceop;
Pstruct city {
	'"';
	Pstring_ME(:"/[^,]\*/":) vBTy_17;
	',';
	spaceop spop;
	Pstring_ME(:"/[A-Z][A-Z]/":) vBTy_20;
	'"';
};
Penum na {
	vBTy_25 Pfrom("U"),
	vBTy_26 Pfrom("NA")
};
Punion nostations {
	PPint vBTy_27;
	PPword word2;
};
Parray stations {
   	nostations [14] : Psep(',');
};
Pstruct abbrev_t {
    PPpunc_lpar lpar1;
    PPword word3;
    PPpunc_rpar rpar1;
    PPpunc_comma comma2;
};
Pstruct Struct_1 {
    PPmessage message3;
    PPpunc_comma comma3;
};
Punion Union_1 {
    Struct_1 struct1;
    PPtext text3;
};
Pstruct record {
    Union_1 union1;
    Popt abbrev_t abbrev;
    PPtext text4;
	PPpunc_comma comma1;
	stations vBTy_38;
};
Pstruct my_key_t {
    PPword word4;
    PPmessage message5;
};
Precord Punion entry_t{
	tableheader Pfrom("Type of rail transit / agency,Primary city served,Number of stations,,,,,,,Number of ADA-accessible stations,,,,,,");
	thecolumns Pfrom("Table 1-9:  ADA-Accessible Rail Transit Stations by Agency,,,,,,,,,,,,,,,");
	years Pfrom(",,1996,1997,1998,1999,2000,2001,2002,1996,1997,1998,1999,2000,2001,2002");
	record vBTy_47;
    my_key_t key;
	comments vBTy_45;
	secHeader vBTy_43;
};
Psource Parray entries_t {
	entry_t[];
};
