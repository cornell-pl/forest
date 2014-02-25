
// Ptypedef Ptimestamp_explicit_FW(: 12, "%b %d %H:%M", P_cstr2timezone("-0500"):) this_year_timestamp_t;
// Ptypedef Ptimestamp_explicit_FW(: 12, "%b %d  %Y", P_cstr2timezone("-0500"):) prev_year_timestamp_t;

Pstruct foo {
    Pdate_FW(:6:) thisyear; ' ';
    Ptime_FW(:5:) atime;
};

Punion timestamp_t {
	Pdate_FW(:12:) prevyear;
	foo blah;
};

Precord Pstruct header_t {
  Pword total; ' ';
  Puint32 dirsize; 
  '\r'; 
};

Precord Pstruct entry_t
{
 Ppermission permissions;
 Pspace sp1;
 Puint32 nfiles;
 ' ';
 Pid owner;
 ' ';
 Pid group;
 Pspace sp2;
 Puint32 bytes;
 ' ';
 timestamp_t date;
 ' ';
 Ppath filename;
};

Psource Pstruct ls_l_t {
	header_t header;
	entry_t[] lsentries;
};

