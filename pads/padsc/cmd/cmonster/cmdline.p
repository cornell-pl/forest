/*
 * This file describes two kinds of command-line arguments, iodisc and cookie
 * 
 * Example iodisc_spec format:
 *
 *     fwrec(:80:)
 * OR
 *     nlrec(::)
 *
 * Example cookie_spec:
 *
 *     c{qy1=Pa_string_FW(:8:)[172,8]|qy2=Pb_uint64(::)[202,8]}
 *
 *     s{qy1=Pe_int32_FW(:3:)[90,3]/1:c{qy2=Pa_string_FW(:8:)[172,8]}/2:c{qy3=Pb_uint64(::)[202,8]}}
 *
 * Informal BNF for cookie_spec:
 *
 *          cookie :: s_cookie | c_cookie
 *        s_cookie :: "s{" s_qy "/" arms "}"
 *            s_qy :: query
 *            arms :: arm | arm "/" arms
 *             arm :: s_val ":" c_cookie
 *        c_cookie :: "c{" queries "}"
 *         queries :: query | query "|" queries
 *           query :: qy_id "=" ty_id "(" params ")[" off "," sz "]"
 *          params :: param | param "," params
 *           qy_id :: ID
 *           ty_id :: ID
 *           s_val :: INT
 *           param :: UINT
 *             off :: UINT
 *              sz :: UINT
 */

/* Comma-sep list of integers terminated by left paren: */

/* note that this array 'eats' the terminating colon */
Parray CM_params {
  Pa_uint64 [] : Psep == ',' && Pterm == ':';
};

Pstruct CM_query {
  Pa_string(:'=':)       qy_id;
  '=';
  Pa_string(:'(':)       ty_id;
  "(:";
  CM_params              params;
  ")[";
  Pa_uint64              off;
  ',';
  Pa_uint64              sz;
  ']';
};

/* note that this array 'eats' the terminating curly */
Parray CM_queries {
  CM_query [1:] : Psep == '|' && Pterm == '}';
}

Pstruct CM_c_cookie {
  "c{";
  CM_queries             queries;
};

Pstruct CM_arm {
  Pa_int32               s_val;
  ':';
  CM_c_cookie            cookie;
};

/* note that this array 'eats' the terminating curly */
Parray CM_arms {
  CM_arm [1:] : Psep == '/' && Pterm == '}';
}

Pstruct CM_s_cookie {
  "s{";
  CM_query               s_qy;
  '/';
  CM_arms                arms;
};

Punion CM_cookie {
  CM_c_cookie            c_cookie;
  CM_s_cookie            s_cookie;
}

Penum CM_disc {
  fwrec_noseek,
  ctrec_noseek,
  nlrec_noseek,
  vlrec_noseek,
  fwrec,
  ctrec,
  nlrec,
  vlrec
};

Pstruct CM_iodisc {
  CM_disc               disc;
  "(:";
  CM_params             params;
  ")";
};
