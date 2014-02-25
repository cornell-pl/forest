#include "vanilla.p"

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

Punion val_t {
  mytime_t my_time;
  PPpath my_pathname;
  quoted_t my_quote;
  Pstring_SE(:"/,|$/":) other_val;
}

Pstruct pair_t {
  PPstring my_key;
  "=";
  val_t my_val;
}

Precord Parray entry_t {
  pair_t[] : Psep(',') && Pterm(Peor);
};

Psource Parray entries_t {
  entry_t[];
};

