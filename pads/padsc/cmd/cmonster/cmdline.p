/*
 * This file describes two kinds of command-line arguments, CM_spec and CM_cspec
 * 
 * Some examples of valid CM_dspec:
 *
 *     fwrec(:80:)
 * OR
 *     nlrec(::)
 *
 * Some examples of valid CM_cspec:
 *
 *     c{qy1=Pa_string_FW(:8:)[172]|qy2=Pb_uint64(::)[8]}
 *
 *     s-{qy1=Pe_int32_FW(:3:)[90]/1:c{qy2=Pa_string_FW(:8:)[172]}}
 *
 *     s+{qy1=Pe_int32_FW(:3:)[90]/1:c{qy2=Pa_string_FW(:8:)[172]}/2:c{qy3=Pb_uint64(::)[202]}}
 *
 * Informal BNF for cookie_spec:
 *
 *          cookie :: s_cookie | c_cookie
 *        s_cookie :: "s" sval_out "{" s_qy "/" arms "}"
 *        sval_out :: "-" | "+"
 *            s_qy :: query
 *            arms :: arm | arm "/" arms
 *             arm :: s_val ":" c_cookie
 *        c_cookie :: "c{" queries "}"
 *         queries :: query | query "|" queries
 *           query :: qy_id "=" ty_id "(" params ")" "[" off "]"
 *          params :: param | param "," params
 *           qy_id :: ID
 *           ty_id :: ID
 *           s_val :: INT
 *           param :: UINT
 *             off :: UINT
 */

/* predeclare a couple of types */
typedef struct CM_query_s CM_qy_t; /* aka CM_query */
typedef struct CM_s       CM_t;

/* signature of a read-write function */
typedef PDC_error_t (*CM_rw_fn)(CM_t *cm, CM_qy_t *qy, PDC_byte *begin, PDC_byte *end);

/* signature of a switch-val function */
typedef PDC_error_t (*CM_sval_fn)(CM_t *cm, CM_qy_t *qy, PDC_byte *begin, PDC_byte *end, PDC_int32 *res_out);

/* signature of a calculate-in-sz function */
typedef size_t (*CM_in_sz_fn)(CM_qy_t *qy);

/* signature of a calculate-out-sz function */
typedef size_t (*CM_out_sz_fn)(CM_qy_t *qy);

/* helper function that computes in_sz and out_sz for a query */
/* should return 1 on success, 0 if there is a problem */
int CM_calc_in_out_sz(CM_qy_t *q, PDC_int32 out_val);

/* A typemap entry */
typedef struct CM_tmentry_s {
  const char      *tname;
  CM_rw_fn         rw_fn;
  CM_sval_fn       sval_fn;
  CM_in_sz_fn      in_sz_fn;
  CM_out_sz_fn     out_sz_fn;
} CM_tmentry_t;

/* helper function that maps: ty_id x switch_qy --> CM_tmentry_t */
CM_tmentry_t* CM_get_tmentry(PDC_string *ty_id, int switch_qy);

/* note that this array 'eats' the terminating colon */
Parray CM_params {
  Pa_uint32 [] : Psep == ',' && Pterm == ':';
};

Pstruct CM_query(int switch_qy) {
  Pa_string(:'=':)         qy_id;
  '=';
  Pa_string(:'(':)         ty_id;
  "(:";
  CM_params                params;
  ")[";
  Pa_uint32                off;
  ']';
  Pcompute CM_tmentry_t   *entry  = CM_get_tmentry(&ty_id, switch_qy);
  Pcompute Puint32         in_sz = 0;
  Pcompute Puint32         out_sz = 0;
};

/* note that this array 'eats' the terminating curly */
Parray CM_queries {
  CM_query(:0:) [1:] : Psep == '|' && Pterm == '}'
                          && Pforall i Pin elts { CM_calc_in_out_sz(&(elts[i]), 1) };
};

Pstruct CM_c_cookie {
  "c{";
  CM_queries               queries;
};

Pstruct CM_arm {
  Pa_int32                 s_val;
  ':';
  CM_c_cookie              cookie;
};

/* note that this array 'eats' the terminating curly */
Parray CM_arms {
  CM_arm [1:] : Psep == '/' && Pterm == '}';
};

Pstruct CM_s_cookie {
  's';
  Pomit Pa_char            sval_out_char : sval_out_char == '+' || sval_out_char == '-';
  Pcompute Pint32          sval_out = ((sval_out_char == '+') ? 1 : 0);
  '{';
  CM_query(:1:)            s_qy : CM_calc_in_out_sz(&(s_qy), sval_out);
  '/';
  CM_arms                  arms;
};

Punion CM_c_or_s {
  CM_c_cookie              c_cookie;
  CM_s_cookie              s_cookie;
};

Pstruct CM_cspec {
  CM_c_or_s                cookie;
};

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

Pstruct CM_dspec {
  CM_disc                  disc;
  "(:";
  CM_params                params;
  ")";
};
