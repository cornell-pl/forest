ptypedef auint64 pn_t(auint64 lo, auint64 hi) :: pn_t x => {x > lo && x <= hi};

pstruct defPN (auint64 lo, auint64 hi){
  pn_t(:lo, hi:) id; 
  '\n';
};

#if 0
pstruct defPN{
  "no_PN";
  auint32 id;
};



#define MINPN 19999999999
#define MAXPN 99999999999

punion dibblerPN{
  defPN                  derived;   //- No phone number from data source; generated dummy for tracking
  pn_t(:MINPN, MAXPN:)   supplied;   //- Phone number given in data source;
};
#endif








