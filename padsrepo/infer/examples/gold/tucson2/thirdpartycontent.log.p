#include "vanilla.p"

Punion content_id {
  Puint64 v_int_id;
  v_cg Pfrom ("CG");
};

Punion sub_id {
  Puint64 v_sub_int_id;
  v_null Pfrom ("null");
};

Punion url_t {
  Pstring (:'|':) v_url;
  v_nul1 Pfrom ("null");
};

Ptypedef Pstring_ME(:"/[0-9A-Z]+/":) encoding_t;

Pstruct date_time_t {
  PPdate v_date;
  ' ';
  PPtime v_time;
};

Precord Pstruct entry_t {
	content_id v_content_id;
	'|';
	sub_id v_sub_id;
	'|';
	url_t v_url_null;
	'|';
	encoding_t v_encoding;
	'|';
        date_time_t v_date_time;
};
Psource Parray entries_t {
	entry_t[];
};
