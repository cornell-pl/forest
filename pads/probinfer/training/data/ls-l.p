//#include "/n/fs/pads/qian/pads/probinfer/training/data/basetokens.p"

#include "basetokens.p"

Ptypedef Pstring_ME(:"/\\s*/":) space_t;

Ptypedef Ptimestamp_explicit_FW(: 12, "%b %d %H:%M", P_cstr2timezone("-0500"):) this_year_timestamp_t;
Ptypedef Ptimestamp_explicit_FW(: 12, "%b %d  %Y", P_cstr2timezone("-0500"):) prev_year_timestamp_t;

Punion timestamp_t {
	this_year_timestamp_t thisyear;
	prev_year_timestamp_t prevyear;
};

Precord Pstruct header_t {
  PPword total;
  PPwhite white6;                       
  PPint dirsize; 
};

Punion my_time_t {
 PPtime time1;
 PPint year;
};

Precord Pstruct entry_t
{
 PPpermission permissions;
 PPwhite sp1;
 PPint nfiles;
 PPwhite white2;
 PPid owner;
 PPwhite white3;
 PPid group;
 PPwhite sp2;
 PPint bytes;
 PPwhite white4;
 PPdate date1;
 PPwhite white6;
 my_time_t mytime;
 PPwhite white5;
 PPpath filename;
};

Psource Pstruct ls_l_t {
	header_t header;
	entry_t[] lsentries;
};

