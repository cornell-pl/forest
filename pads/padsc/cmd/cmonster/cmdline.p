/*
 * This file describes two kinds of command-line arguments, iodisc_spec and cookie_spec.
 * 
 * Example cookie_spec:
 *
 *     c{qy1=Pa_string_FW(:8:)[172,8]|qy2=Pb_uint64(::)[202,8]}
 *
 *     s{Pe_int32_FW(:3:)[90,3]/1:c{qy1=Pa_string_FW(:8:)[172,8]}/2:c{qy2=Pb_uint64(::)[202,8]}}
 *
 * Example iodisc_spec format:
 *
 *     fwrec(:80:)
 * OR
 *     nlrec(::)
 */

/* Comma-sep list of integers terminated by left paren: */

/* note that this array 'eats' the terminating colon */
Parray CMDLINE_intList {
  Pa_uint64 [] : Psep == ',' && Pterm == ':';
};

Pstruct CMDLINE_c_cookie_elt {
  Pa_string(:'=':)       qy;
  '=';
  Pa_string(:'(':)       ty;
  "(:";
  CMDLINE_intList        params;
  ")[";
  Pa_uint64              off;
  ',';
  Pa_uint64              sz;
  ']';
};

/* note that this array 'eats' the terminating curly */
Parray CMDLINE_c_cookie_ar {
  CMDLINE_c_cookie_elt [1:] : Psep == '|' && Pterm == '}';
}

Pstruct CMDLINE_c_cookie_spec {
  "c{";
  CMDLINE_c_cookie_ar    cookies;
};

Pstruct CMDLINE_sw_val {
  Pa_string(:'(':)       ty;
  "(:";
  CMDLINE_intList        params;
  ")[";
  Pa_uint64              off;
  ',';
  Pa_uint64              sz;
  ']';
};

Pstruct CMDLINE_alt_elt {
  Pa_int32               tag;
  CMDLINE_c_cookie_spec  cookie;
};

/* note that this array 'eats' the terminating curly */
Parray CMDLINE_alt_ar {
  CMDLINE_alt_elt [1:] : Psep == '/' && Pterm == '}';
}

Pstruct CMDLINE_s_cookie_spec {
  "s{";
  CMDLINE_sw_val         val;
  '/';
  CMDLINE_alt_ar         alts;
};

Punion CMDLINE_cookie_spec {
  CMDLINE_c_cookie_spec  c_cookie;
  CMDLINE_s_cookie_spec  s_cookie;
}

Penum CMDLINE_discipline {
  fwrec_noseek,
  ctrec_noseek,
  nlrec_noseek,
  vlrec_noseek,
  fwrec,
  ctrec,
  nlrec,
  vlrec
};

Pstruct CMDLINE_iodisc_spec {
  CMDLINE_discipline     iodisc;
  "(:";
  CMDLINE_intList        params;
  ")";
};
