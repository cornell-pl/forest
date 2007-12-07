/- is this the same format as Ptime? or Pdate; ' '; Ptime

Ptypedef Ptimestamp_explicit_FW (:15, "%b %d %H:%M:%S", P_cstr2timezone("-0500"):) timestamp_t;

Pstruct daemon_message_t {
        Pid daemon;
        ": ";
        Popt Pmessage(:Peor:) msg;
};

Punion message_t{
	daemon_message_t dm;
	Pmessage(:Peor:) sm;
};
	
Precord Pstruct entry_t {
	Pdate(:' ':) ts;
	' ';
	Ptime(:' ':) mydate;
	' ';
	Pid server;
	' ';
	message_t msg;
};

Psource Parray boot_t {
	entry_t[];
};

