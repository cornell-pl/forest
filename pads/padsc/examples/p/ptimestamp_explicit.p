Precord Pstruct threetimestamps {
  Ptimestamp_explicit(: '|',      "%Y-%m-%d+%H:%M:%S", P_cstr2timezone("-0300") :) ts1; '|';
  Ptimestamp_explicit(: '|',      "%y-%m-%d+%H:%M:%S", P_cstr2timezone("-0400") :) ts2; '|';
  Ptimestamp_explicit_SE(: "/$/", "%m-%d-%y+%H:%M",    P_cstr2timezone("-0500") :) ts3;
}; 
