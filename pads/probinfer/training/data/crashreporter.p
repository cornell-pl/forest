#include "basetokens.p"

Ptypedef Pstring_SE(:"/ \\-|$/":) FilePath;

Penum Crashdump_t {
        crashdump = 1,
        crashreporterd = 2
};

Pstruct StartPath {
  PPtext text1;
  PPpunc_colon colon1;
  PPwhite white5;                
  PPpath file1;
};

Pstruct FinishPath {
  "Finished writing crash report to: ";
  PPpath file2;
};

Pstruct Unable_t {
  PPtext text2;
  PPpunc_colon colon2;
  PPwhite white6;                
  PPint pid;
  PPwhite white7;
  PPword word1;
  PPpunc_colon colon3;
  PPwhite white8;
  PPtext text3;
//  " name: Exited process";
};
 
Pstruct Failed {
//  "Failed to re-launch ";
  PPtext text4;
  PPpath file3;
  PPwhite white9;
  PPpunc_hyphen hyphen1;
  PPwhite white10;
//  " - ";
  Pstring_SE(:Peor:) errormsg;
};

Punion Dumpreport_t
{
  started Pfrom ("crashdump started");
  Unable_t unable;
  Failed fail;
  StartPath sp;
  StartPath fp;
};

Pstruct Reporterreport_t {
//  Pstring_ME (: "/[^ ]\*/" :) function;
//  " reply failed: ";
  Pstring_SE(:Peor:) failedmsg;
};

Punion Report_t 
{
  Dumpreport_t dumpreport;
  Reporterreport_t reporterreport;
};

Precord Pstruct entry_t {
   PPdate date1;
   PPwhite white1;
   PPtime time1;
   PPwhite white2;
   PPint date2;
   PPwhite white3;  
   PPword crash;
   PPpunc_lsqubrac lsqubrac1;
   PPint        dumpid;
   PPpunc_rsqubrac rsqubrac2;
   PPpunc_colon colon1;
   PPwhite white4;
   Report_t report; 
};

Psource Parray entries_t {
  entry_t[];
}
