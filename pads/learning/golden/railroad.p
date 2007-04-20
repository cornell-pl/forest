Ptypedef Pstring_ME(:"/[A-Za-z][0-9a-zA-Z_\-]\*/":) Word;
Pstruct secHeader {
        Word vBTy_3;
        ' ';
        "rail";
        ",,,,,,,,,,,,,,,";
};
Punion title {
        Pstring_ME(: "/\"[^\"]\*\"/" :) noteinquotes; 
        Pstring_ME(: "/[^,]\*/" :) noteoutsidequotes; 
};
Pstruct comments {
	title vBTy_12;
	",,,,,,,,,,,,,,,";
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
	Pint32 vBTy_27;
	na vBTy_29;
};
Parray stations {
	nostations [14] : Psep(',');
};
Pstruct record {
	title vBTy_32;
	',';
	city vBTy_35;
	',';
	stations vBTy_38;
};
Precord Punion entry_t{
	tableheader Pfrom("Type of rail transit / agency,Primary city served,Number of stations,,,,,,,Number of ADA-accessible stations,,,,,,");
	thecolumns Pfrom("Table 1-9:  ADA-Accessible Rail Transit Stations by Agency,,,,,,,,,,,,,,,");
	years Pfrom(",,1996,1997,1998,1999,2000,2001,2002,1996,1997,1998,1999,2000,2001,2002");
	secHeader vBTy_43;
	comments vBTy_45;
	record vBTy_47;
};
Psource Parray entries_t {
	entry_t[];
};
