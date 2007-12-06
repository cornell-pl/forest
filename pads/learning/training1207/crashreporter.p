\- Ptypedef Pstring_SE(:"/ \\-|$/":) FilePath;

\- what to do with the string constants??? Pmessage for the whole thing?
\- what is this:  Pstring_ME (: "/[^ ]\*/" :)

Penum Crashdump_t {
        crashdump = 1,
        crashreporterd = 2
};

Pstruct StartPath {
  "Started writing crash report to: ";
  Ppath file1;
};

Pstruct FinishPath {
  "Finished writing crash report to: ";
  Ppath file2;
};

Pstruct Unable_t {
  "Unable to determine task_t for pid: ";
  Puint32 pid;
  " name: Exited process";
};
 
Pstruct Failed {
  "Failed to re-launch ";
  Ppath file3;
  " - ";
  Pmessage(:Peor:) errormsg;
};

Punion Dumpreport_t
{
  started Pfrom ("crashdump started");
  StartPath sp;
  FinishPath fp;
  Unable_t unable;
  Failed fail;
};

/- just make this whole struct Pmessage?? or mach_msg() replay failed: Pmessage
   
Pstruct Reporterreport_t {
  Pstring_ME (: "/[^ ]\*/" :) function;
  " reply failed: ";
  Pmessage(:Peor:) failedmsg;
};

Punion Report_t (:Crashdump_t crash:)
{
 Pswitch (crash) {
        Pcase 1 : Dumpreport_t dumpreport;
        Pcase 2: Reporterreport_t reporterreport;
 }
};

Ptypedef Ptimestamp_explicit_FW(: 24, "%a %b %d %H:%M:%S %Y", P_cstr2timezone("-0500") :) timestamp_t;
Precord Pstruct entry_t {
         timestamp_t mytime;
   ' ';  Crashdump_t crash;
   '[';  Puint32        dumpid;
   "]: "; \- Report_t(:crash:) report;
	  Pmessage(:Peor:);
};

Psource Parray entries_t {
  entry_t[];
}
