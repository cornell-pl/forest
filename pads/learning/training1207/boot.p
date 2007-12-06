/- is this the same format as Ptime? or Pdate; ' '; Ptime

Ptypedef Ptimestamp_explicit_FW (:15, "%b %d %H:%M:%S", P_cstr2timezone("-0500"):) timestamp_t;

Pstruct daemon_message_t {
        Pid daemon;
        ": ";
        Popt Pmessage(:Peor:) msg;
};

/- should the following be Pmessage?
/- how to deal with string literals?

Pstruct system_message_t {
	"last message repeated ";
	Puint32 times;
	" times";
};

Punion message_t{
	daemon_message_t dm;
	system_message_t sm;
};
	
Precord Pstruct entry_t {
	timestamp_t ts;
	' ';
	Pid server;
	' ';
	message_t msg;
};

Psource Parray boot_t {
	entry_t[];
};

