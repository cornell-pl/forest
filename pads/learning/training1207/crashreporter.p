
Precord Pstruct entry_t {
   	 Pdate_FW(:10:) mydate; 
   ' ';  Ptime(:' ':) mytime;
   ' ';  Pint year;
   ' ';  Pword crash;
   '[';  Puint32        dumpid;
   "]: "; Pmessage(:Peor:);
};

Psource Parray entries_t {
  entry_t[];
}
