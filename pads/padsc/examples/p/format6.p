ptypedef a_uint64 pn_t(a_uint64 lo, a_uint64 hi) :: pn_t x => {x > lo && x <= hi};

pstruct defPN (a_uint64 lo, a_uint64 hi){
  pn_t(:lo, hi:) id; 
  EOR;
};

#if 0
pstruct defPN{
  "no_PN";
  a_uint32 id;
};



#define MINPN 19999999999
#define MAXPN 99999999999

punion dibblerPN{
  defPN                  derived;   //- No phone number from data source; generated dummy for tracking
  pn_t(:MINPN, MAXPN:)   supplied;   //- Phone number given in data source;
};
#endif








