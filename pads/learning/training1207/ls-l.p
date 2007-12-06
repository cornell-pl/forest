Pstruct permission_t
{
 Pchar dir: dir== '-' || dir== 'd';
 Pchar uread: uread == '-' || uread == 'r';
 Pchar uwrite: uwrite == '-' || uwrite == 'w';
 Pchar uexe:uexe == '-' || uexe == 'x';
 Pchar gread: gread == '-' || gread == 'r';
 Pchar gwrite: gwrite == '-' || gwrite == 'w';
 Pchar gexe:gexe == '-' || gexe == 'x';
 Pchar oread: oread == '-' || oread == 'r';
 Pchar owrite: owrite == '-' || owrite == 'w';
 Pchar oexe:oexe == '-' || oexe == 'x';
};

Ptypedef Ptimestamp_explicit_FW(: 12, "%b %d %H:%M", P_cstr2timezone("-0500"):) this_year_timestamp_t;
Ptypedef Ptimestamp_explicit_FW(: 12, "%b %d  %Y", P_cstr2timezone("-0500"):) prev_year_timestamp_t;

Punion timestamp_t {
	this_year_timestamp_t thisyear;
	prev_year_timestamp_t prevyear;
};

Precord Pstruct header_t {
  Pword total : total == "total"; ' ';
  Puint32 dirsize; 
  '\r'; 
};

Precord Pstruct entry_t
{
 permission_t permissions;
 Pspace sp1;
 Puint32 nfiles;
 ' ';
 Pid owner;
 ' ';
 Pid group;
 Pspace sp2;
 Puint32 bytes;
 ' ';
 timestamp_t timestamp;
 ' ';
 Ppath filename;
};

Psource Pstruct ls_l_t {
	header_t header;
	entry_t[] lsentries;
};

