#include "vanilla.p"

Precord Pstruct entry_t {
        Puint64         v_event_id;
        ',';
        Pstring_SE(:"/,/":) v_sub_sys;
        ',';
        Pstring_SE(:"/,/":) v_event_type_class;
        ',';
        Pstring_SE(:"/,/":) v_event_type;
        ',';
        Pfloat64         v_timestamp;
        ',';
        Pint8          v_handled;
        ',';
        Pstring_SE(:Peor:)  v_desc_event;
};

Psource Parray table_t {
        entry_t[];
};
