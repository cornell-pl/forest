Punion method {
	"Installed";
	"Updated";
	"Erased";
};

Pstruct ins_update_package_t {
	Pstring(:'.':) packagename;
	'.';
	Pstring(:' ':) arch;
	' ';
	Pstring(:'-':) major;
	'-';
	Pstring_SE(:Peor:) minor;
};

Ptypedef Pstring_SE(:Peor:) erase_package_t;

Punion package_t {
		erase_package_t erased_package;
		ins_update_package_t ins_update_package;
};

Precord Pstruct entry_t {
	Ptimestamp_explicit_FW (:15, "%b %d %H:%M:%S", P_cstr2timezone("-0500"):) ts;
	' ';
	method m;
	": ";
	package_t package;
};

Psource Parray yum {
	entry_t[];
};

