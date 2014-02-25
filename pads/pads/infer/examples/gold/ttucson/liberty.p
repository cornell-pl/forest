#include "vanilla.p"
Pstruct msg_t {
        ' ';
        Pstring_SE(:Peor:)  v_blob_380;
};
 
Precord Pstruct entry_t {
	"- ";
	Puint64 v_intrange_5;
	' ';
	PPdate  v_date_10;
	' ';
        PPstring v_machine_name;
        ' ';
	PPdate  v_date_19;
	' ';
	PPtime  v_time_24;
        ' ';
        Pstring_SE (:"/ |$/":) v_user;
        Popt msg_t v_msg;
};
Psource Parray entries_t {
	entry_t[];
};
