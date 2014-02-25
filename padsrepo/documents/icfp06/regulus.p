/*@FILE @LEFT regulus.tex */
Ptypedef Puint32 Ptstamp;
/*@BEGIN regulus.tex */
/* Pstring terminated by a semicolon or vertical bar */
Ptypedef Pstring_SE(:"/;|\\|/":) SVString;

Pstruct Nvp_string(:char * s:){
  s; "="; SVString val;
};

Pstruct Nvp_ip(:char * s:){
  s; "="; Pip val;
};

Pstruct Nvp_timestamp(:char * s:){
  s; "="; Ptstamp val;
};

Pstruct Nvp_Puint32(:char * s:){
  s; "="; Puint32 val;
};

Pstruct Nvp_a{
      Pstring(:'=':) name;
 '='; SVString       val;
};

Pstruct Details{
      Nvp_ip(:"src_addr":) source;
';';  Nvp_ip(:"dst_addr":) dest;
';';  Nvp_timestamp(:"start_time":) start_time;
';';  Nvp_timestamp(:"end_time":)   end_time;
';';  Nvp_Puint32(:"cycle_time":)   cycle_time;
};

Parray Nvp_seq{
  Nvp_a [] : Psep(';') && Pterm('|');
};

Punion Info(:int alarm_code:){
  Pswitch (alarm_code){
    Pcase 5074: Details   details;
    Pdefault:   Nvp_seq   generic;
  }
};

Penum Service {
   DOMESTIC,
   INTERNATIONAL,
   SPECIAL
}; 

Pstruct Raw_alarm {
       Puint32 alarm : alarm == 2 || alarm == 3;
 ':';  Popt Ptstamp start;
 '|';  Popt Ptstamp clear;
 '|';  Puint32      code;
 '|';  Nvp_string(:"dns1":) src_dns;
 ';';  Nvp_string(:"dns2":) dest_dns;
 '|';  Info(:code:) info;
 '|';  Service      service;
};

int chkCorr(Raw_alarm ra) { return 1;};

Precord Ptypedef Raw_alarm Alarm : 
         Alarm a => {chkCorr(a)};

Psource Parray Source {
  Alarm[];
};
/*@END regulus.tex */
