// array of integers separated by spaces and terminated by Peor
Parray intarray_t {
  Puint32[]: Pterm(Pre "/\\s*$/");
}

// array of floats separated by spaces and terminated by Peor
Parray floatarray_t {
  Pfloat32[]: Pterm(Pre "/\\s*$/");
}

// VMStat: 'vmstat' numbers -- max numbers over the past 5 mins.
Precord Pstruct vmstat_t {
  "VMStat:"; 
  intarray_t maxNumbers;
  Pstring_ME(:"/\\s*$/":) s;
}

// Purks: unknown
Precord Pstruct purks_t {
  "Purks:";
  floatarray_t data;
  Pstring_ME(:"/\\s*$/":) s;
}

Psource Pstruct source {
  vmstat_t     VMStat;
  purks_t      Purks;
  Pomit Pstring_ME(: "/[\\000]*/" :) junk;
}
