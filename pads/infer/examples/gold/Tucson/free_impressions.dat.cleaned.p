#include "vanilla.p"

Pstruct norm_entry_t {
  Puint32 v_count;
  '|';
  Pint64 v_large;
  "||";
  Popt Puint16 v_small;
  '|';
  Pint64  v_large1;
  '|';
  Pstring (:'|':) v_id;
  '|';
  Popt Pint64  v_large2;
  '|';
  Pstring (:'|':) v_stuff;
  '|';
  Popt Pint64 v_opt_60;
  "|SR|";
  PPdate v_date;
  ' ';
  PPtime v_time;
  '|';
  Pstring_SE(:Peor:) v_serial;
}

Precord Punion entry_t{
	norm_entry_t v_rec;
        Pempty;
};

Psource Parray entries_t {
	entry_t[];
};
