/*
 * This file describes two kinds of command-line arguments, iodisc_spec and cookies_spec.
 * 
 * Example cookies_spec:
 *
 *     {qy1=Pa_string_FW(:8:)[172,8]|qy2=Pb_uint64(::)[202,8]}
 *
 * Example iodisc_spec format:
 *
 *     fwrec(:80:)
 * OR
 *     nlrec(::)
 */

/* Comma-sep list of integers terminated by left paren: */

Parray intList {
  Pa_uint64 [] : Psep == ',' && Pterm == '(';
};

Pstruct cookie {
  Pa_string(:'=':)      qy;
  '=';
  Pa_string(:'(':)      ty;
  "(:";
  intList               params;
  ":)[";
  Pa_uint64             off;
  ',';
  Pa_uint64             sz;
  ']';
};

Parray cookie_ar {
  cookie [1:] : Psep == '|' && Pterm == '}';
}


Pstruct cookies_spec {
  '{';
  cookie_ar             cookies;
  '}';
};

Pstruct iodisc_spec {
  Pa_string(:'(':)      iodisc;
  "(:";
  intList               params;
  ":)";
};
