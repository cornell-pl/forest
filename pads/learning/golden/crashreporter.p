Ptypedef Pstring(:' ':) FilePath;

Penum Crashdump_t {
        crashdump,
        crashreporterd
};

Pstruct StartPath {
  "Started writing crash report to: ";
  FilePath file1;
};

Pstruct FinishPath {
  "Finished writing crash report to: ";
  FilePath file2;
};

Pstruct Unable_t {
  "Unable to determine task_t for pid: ";
  Puint32 pid;
  " name: Exited process";
};
 
Pstruct Failed {
  "Failed to re-launch ";
  FilePath file3;
  " - ";
  Pstring_SE(:Peor:) errormsg;
};

Punion Dumpreport_t
{
  started Pfrom ("crashdump started");
  StartPath sp;
  FinishPath fp;
  Unable_t unable;
  Failed fail;
};

Pstruct Reporterreport_t {
  Pstring_ME (: "/[^ ]\*/" :) function;
  " reply failed: ";
  Pstring_SE(:Peor:) failedmsg;
};

Punion Report_t (:Crashdump_t crash:)
{
 Pswitch (crash) {
        Pcase 1 : Dumpreport_t dumpreport;
        Pcase 2: Reporterreport_t reporterreport;
 }
};

Precord Pstruct entry_t {
         Pdate(:' ':)   date;          
   ' ';  Ptime(:' ':)   time;
   ' ';  Puint32  year;
   ' ';  Crashdump_t crash;
   '[';  Puint32        dumpid;
   "]: "; Report_t(:crash:) report; 
};

Psource Parray entries_t {
  entry_t[];
}
