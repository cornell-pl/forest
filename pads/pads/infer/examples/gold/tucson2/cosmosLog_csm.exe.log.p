#include "vanilla.p"
Pstruct val_t {
        '"';
        Pstring(:'"':) v_quoted_string;
        '"';
};

Pstruct pair_t {
  PPstring v_key;
  '=';
  val_t v_val;
};

Parray pairs_t {
  pair_t [] : Psep (' ') && Plongest; 
};

Pstruct mytime_t {
 PPdate this_date;
 "T";
 PPtime this_time;
 ".";
 Puint8 ms;
 "Z";
}

Pstruct quoted_t {
  '"';
  Pstring(:'"':) str_content;
  '"';
};

Punion newval_t {
  mytime_t my_time;
  PPpath my_pathname;
  quoted_t my_quote;
  Pstring_SE(:"/,|$/":) other_val;
}

Pstruct newpair_t {
  PPstring my_key;
  "=";
  newval_t my_val;
}

Parray event_t {
  newpair_t[] : Psep(',') && Pterm(Peor);
};

Precord Pstruct entry_t {
    PPstring  v_string_1;
    ',';
    PPdate  v_date_5;
    ' ';
    PPtime  v_time_10;
    '.';
    Puint16  v_intrange_14;
    ',';
    PPstring  v_string_18;
    ',';
    Pstring_SE (:"/,SrcFile=/":) v_stuff;
    ',';
    pairs_t v_pairs;
    Popt event_t v_event;
};

Psource Parray entries_t {
    entry_t[];
};
