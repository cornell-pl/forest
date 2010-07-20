#include "vanilla.p"
Pstruct proc_id_t {
        '[';
        Puint32 id;
        ']';
}

Pstruct daemon_t {
        Pstring_SE (:"/[:\\[]/":) name;
        Popt proc_id_t v_proc_id;
        ':';
}

Pstruct msg_body_t {
        daemon_t v_daemon_pri;
        PPwhite v_space;
        Pstring_SE(:Peor:) v_msg;
};

Punion message_t  {
        msg_body_t v_normal_msg;
        Pstring_SE(:Peor:) v_other_msg;
/*        v_othermsg Pfrom ("-- MARK -- "); */
};

Precord Pstruct entry_t {
	PPdate  v_date;
        ' ';
        PPtime v_time;
        ' ';
        PPstring v_id;
        ' ';
        message_t v_message;
};
Psource Parray entries_t {
	entry_t[];
};
