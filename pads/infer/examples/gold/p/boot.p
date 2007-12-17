Ptypedef Ptimestamp_explicit_FW (:15, "%b %d %H:%M:%S", P_cstr2timezone("-0500"):) timestamp_t;

Pstruct daemon_message_t {
        Pstring(:':':) daemon;
        ": ";
        Popt Pstring_SE(:Peor:) msg;
};

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
	Pstring(:' ':) server;
	' ';
	message_t msg;
};

Psource Parray boot_t {
	entry_t[];
};

