#include "vanilla.p"
/*There's altogether 29 bars. */
Punion Union_1 {
  PPstring v_str_1;
  Puint32 v_int_1;
  Pempty;
}

Parray Array_1 {
	Union_1[] : Psep(',') && Pterm('|');
};

Parray Array_2 {
	Union_1[] : Psep(' ') && Pterm('|');
};

Pstruct url_t {
  '"';
  PPwhite v_space1;
  "a href=";
  Pstring (:'|':) v_url;
  "||";
}

Pstruct string_t {
  '"';
  PPwhite v_space1;
  Pstring (:'|':) v_url;
  "|";
}

Punion address_t {
  url_t v_url;
  string_t v_quoted_str;
  Pstring (:'|':) v_msg1;
}

Ptypedef Pstring_ME (:"/[A-Za-z][A-Za-z]/":) state_t;
  
Pstruct Struct_2614 {
	Pstring_ME (:"/[A-Z0-9]+/":)  v_string_575;
	'|';
	PPdate  v_date_580;
	' ';
	PPtime  v_time_585;
	'|';
	Puint32  v_intrange_589;
	'|';
	Puint32  v_intrange_594;
	'|';
	Puint32  v_intrange_599;
	'|';
        Pstring (:'|':) v_subjects;
        '|';
        address_t v_address;
        '|';
        Pstring (:'|':) v_city;
        '|';
        Popt state_t v_state;
        '|';
        Pstring (:'|':) v_unknown2;
        '|';
        Pstring (:'|':) v_unknown3;
        '|';
        Pint32 v_int_small;
        '|';
        Pint32 v_int_small1;
        '|';
        Pfloat64 v_floatsmall1;
        '|';
        Pfloat64 v_floatsmall2;
        '|';
        Pstring (:'|':) v_unknown9;
        '|';
        Popt Pstring (:'|':) v_city_1;
        '|';
        Popt state_t v_state_1;
        '|';
        Popt Pstring (:'|':) v_unknown4;
        '|';
        Popt Pstring (:'|':) v_unknown5;
        '|';
        Array_1 v_array_names;
        '|';
        Array_2 v_array_ints;
        '|';
        Pstring (:'|':) v_unknown6;
        '|';
        Pstring (:'|':) v_unknown7;
        '|';
        Pstring (:'|':) v_unknown12;
        '|';
        Pstring (:'|':) v_unknown13;
        '|';
        Pstring (:'|':) v_unknown8;
        '|';
        Pstring (:'|':) v_unknown11;
        '|';
	Puint32  v_intrange_2567;
	'|';
	Popt Puint8 v_opt_2575;
};

Punion stuff_t {
  Pstring(:'|':) v_non_empty;
  Pempty;
};

Parray catch_all_t {
    stuff_t[] : Psep ('|') && Plongest;
};

Precord Punion entry_t {
  Struct_2614 v_normal;
  catch_all_t v_catch;
};

Psource Parray entries_t {
	entry_t[];
};
