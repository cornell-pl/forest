#include "vanilla.p"

Ptypedef Pstring_ME(:"/[0-9a-f][0-9a-f]/":) hex_t;

Punion category {
        dash Pfrom('-');
        PPstring v_cat;
};

Pstruct node_num {
        " [";
        hex_t v_node_id;
        ']';
};
      
Pstruct weird_machine_name {
        '#';
        Puint16 v_num;
        '#';
};
 
Punion machine_name {
        weird_machine_name v_weird;
        PPip v_ip;
        PPstring v_normal;
};

Pstruct normal_msg {
        '|';
        PPdate v_date_3;
        ' ';
        PPtime v_time_2;
        '|';
        PPstring v_logname;
        '|';
        Pstring(:'|':) v_machine_name1;
        '|';
        Pstring_SE (:Peor:) v_msg;
}

Pstruct special_msg {
        PPwhite sp1;
        Pstring_SE (:Peor:) v_special_msg;
};
 
Punion message_t {
        normal_msg v_norm;
        special_msg v_special;
};      

Precord Pstruct entry_t {
        category v_category;
        ' ';
        Puint64 v_intrange_5;
        ' ';
        PPdate  v_date_1;
        ' ';
        machine_name v_machine_name;
        Popt node_num v_node_num;
        ' ';
        PPdate  v_date_2;
        ' ';
        PPtime  v_time_1;
        message_t v_msgs;
};
Psource Parray entries_t {
        entry_t[];
};

