/* PADS spec for HTTP.  We are currently working on a subset. */

/* Problems

   1.  Pstring_SE, PString_ME only partially implemented
         : Bob connect re library
   2.  Representation of TEXT_t yields a string, not a char
         : try out Bob's munge proposal
         : munge may only allowed on no-seek I/O disciplines 
   3.  Non-parameterized character typedefs should produce
         a. predicates
         b. character class names for use in pattern expressions
   4. case-insenstive strings 
         : bob talk to glenn re syntax
   5. regular expression literals 
         : bob talk to glenn re syntax
*/

/* TJIM NOTES
   Parameterized definitions might be useful, see for example
   accept_encoding and accept_charset.
   It's annoying to have to name all types and fields.  At
   least tuples would be useful.
   Need a convenient way to express optional elements; Pvoid?
*/

/*
  HTTP is defined in RFC 2616, see
    http://zvon.org/tmRFC/RFC2616/Output/index.html
  for a nicely formatted version.

  WHITE SPACE CONVENTIONS: header lines are terminated with CRLF.
  The header lines and message body are separated by a CRLF on a
  line by itself.
  A header field VALUE can be continued to the next line by ending
  with CRLF as usual and beginning the next line with a space or
  horizontal tab.
  All linear white space has the same semantics as a single space.
  The RFC 2068 and its replacement, RFC 2616, mangle the definition
  of linear white space that originally (?) appeared in RFC 822.
  In RFC 822 it is defined

     LWSP-char   =  SPACE / HTAB                 ; semantics = SPACE

     linear-white-space =  1*([CRLF] LWSP-char)  ; semantics = SPACE
                                                 ; CRLF => folding 
 */


/* PDC_WSPACE_OK should not be set.  */
Penum d_method_t {
  OPTIONS,
  GET, 
  HEAD, 
  POST, 
  PUT, 
  DELETE, 
  TRACE,
  CONNECT
};

Punion method_t {
  d_method_t method;
  token_t    extension_method;
};

Pstruct version_t {
  "HTTP/";
  Puint8 major; /- FIX(?): should be 1*DIGIT
  '.';
  Puint8 minor; /- FIX(?): should be 1*DIGIT
};

Ptypedef Puchar reserved_t :: reserved_t(x) => {
  (x == ';') ||
  (x == '/') ||
  (x == '?') ||
  (x == ':') ||
  (x == '@') ||
  (x == '&') ||
  (x == '=') ||
  (x == '+') 
}

Ptypedef Puchar extra_t :: extra_t(x) => {
  (x == '!') ||
  (x == '*') ||
  (x == '\'') ||
  (x == '(') ||
  (x == ')') ||
  (x == ',')
}

Ptypedef Puchar safe_t :: safe_t(x) => {
  (x == '$') ||
  (x == '-') ||
  (x == '_') ||
  (x == '.')
}

Ptypedef Puchar ALPHA_t :: ALPHA_t(x) => {
  ('A' >= x && x <= 'Z') ||
  ('a' >= x && x <= 'z')
}

Ptypedef Puchar DIGIT_t :: DIGIT_t(x) => {
  ('0' >= x && x <= '9')
}

Ptypedef Puchar HEX_t :: HEX_t(x) => {
  ('A' >= x && x <= 'F') ||
  ('a' >= x && x <= 'f') ||
  DIGIT_t(x)
}

Ptypedef Puchar CTL_t :: CTL_t(x) => {
  (x < 32)
}

Ptypedef Puchar SP_t :: SP_t(x) => {
  (x == '_')
}

Ptypedef Puchar unsafe_t :: unsafe_t(x) => {
  CTL_t(x) ||
  SP_t(x) ||
  (x == '\"') ||
  (x == '#') ||
  (x == '%') ||
  (x == '<') ||
  (x == '>')
}

Ptypedef Puchar national_t :: national_t(x) => {
  !(ALPHA_t(x) ||
    DIGIT_t(x) ||
    reserved_t(x) ||
    extra_t(x) ||
    safe_t(x) ||
    unsafe_t(x))
}

Ptypedef Puchar unreserved_t :: unreserved_t(x) => {
  ALPHA_t(x) ||
  DIGIT_t(x) ||
  safe_t(x) ||
  extra_t(x) ||
  national_t(x)
}

Ptypedef Puchar uchar_t :: uchar_t(x) => {
  unreserved_t(x) ||
  escaped_t(x)
}

Ptypedef Puchar pchar_t :: pchar_t(x) => {
  uchar_t(x) ||
  (x == ':') ||
  (x == '@') ||
  (x == '&') ||
  (x == '=') ||
  (x == '+')
}

Ptypedef Puchar uchar_or_reserved_t :: uchar_or_reserved_t(x) => {
  uchar_t(x) || reserved_t(x)
}

Parray uchar_or_reserved_array_t {
  uchar_or_reserved_t[0:] ur : Pterm == ending;
}

Parray query_t {
  uchar_or_reserved_t[0:] ur : Pterm == ending;
}

Parray fragment_t {
  uchar_or_reserved_t[0:] ur : Pterm == ending;
}

Ptypedef Puchar scheme_char_t :: scheme_char_t(x) => {
  ALPHA_t(x) ||
  DIGIT_t(x) ||
  (x == '+') ||
  (x == '-') ||
  (x == '.')
}

Parray scheme_t {
  scheme_char_t[1:] sc : Pterm == ending;
}

Ptypedef Puchar net_loc_char_t :: net_loc_char_t(x) => {
  pchar_t(x) ||
  (x == ';') ||
  (x == '?')
}

Parray net_loc_t {
  net_loc_char_t[1:] sc : Pterm == ending;
}

Ptypedef Puchar param_char_t :: param_char_t(x) => {
  pchar_t(x) ||
  (x == '/')
}

Parray param_t {
  param_char_t[0:] sc : Pterm == ending;
}

/* Common: sequences with separators.
//params         = param *( ";" param ) 
Parray params_t {
  param_t[1:] ps: Pterm == ending;
}
*/

Ptypedef Puchar absoluteURI_char_t :: absoluteURI_char_t(x) => {
  uchar_t(x) || reserved_t(x)
}

Pstruct absoluteURI_t {
  scheme_t                  scheme;
  ":";
  uchar_or_reserved_array_t rest;
}

Pstruct rel_path {
/* Three optional parts!!
  [path] [';' params] ['?' query]
*/
}

Pstruct abs_path_t {
  "/";
  rel_path_t rel_path;
}

/* Defined in RFC 2396 */
Punion authority_t {
  server_t   server;
  reg_name_t reg_name;
};

Punion request_uri_t {
  "*"           applies_to_server;
  absoluteURI_t absoluteURI;
  abs_path_t    abs_path;
  authority_t   authority;
};


/* It looks like RFC 2068 has mangled the definition of
linear-white-space that is originally given in RFC 822.  In RFC 822
we have:

     LWSP-char   =  SPACE / HTAB                 ; semantics = SPACE

     linear-white-space =  1*([CRLF] LWSP-char)  ; semantics = SPACE
                                                 ; CRLF => folding 

In RFC 2068 we have:

   HTTP/1.1 headers can be folded onto multiple lines if the
   continuation line begins with a space or horizontal tab. All linear
   white space, including folding, has the same semantics as SP.

          LWS            = [CRLF] 1*( SP | HT ) 

In combination with the definition of TEXT:

          TEXT           = <any OCTET except CTLs,
                           but including LWS> 

the 2068 definition makes no sense, while the 822 definition is
sensible and means, "any octet except controls, or a sequence

  [CRLF] LWSP-char

that is interpreted as SP".

*/

#define LWS0 "[CRLF](' '|\t)"

/* Fields are supposed be separated by a single SP and terminated
   by CRLF, with no CR or LF anywhere except the terminator;
   not sure how this interacts with the whitespace discipline. */
/* FIX: should use request_uri_t */
Precord Pstruct request_line_t {
   method_t           method;               /- Method used during request
   Pstring_SE(:LWS0:) request_uri;          /- Requested uri.
   version_t          http_version;         /- HTTP version number of request 
};

int chkErr(int nerr, int * consume){
  if (nerr) {
    *consume = 0;
  };
  return nerr;
};

Pstruct delta_seconds_t {
  "=";
  Puint64 d;
};

Pstruct field_time_t(:char *s:){
  s;
  delta_seconds_t delta_seconds;
};


Punion delta_seconds_opt_t {
  delta_seconds_t delta_seconds;
  Pcompute Puint32 omitted = 0;  /* Pvoid ??*/
};

Pstruct max_stale_t {
  "max-stale";
  delta_seconds_opt_t delta_seconds_opt;
};

Punion cache_request_directive_t {
  "no-cache"       no_cache;                         
  "no-store"       no_store;                        
  field_time_t(:"max-age":)        max_age;
  field_time_t(:"max-stale":)      max_stale;
  field_time_t(:"min-fresh":)      min_fresh;
  "no-transform"   no_transform;
  "only-if-cached" only_if_cached;                  
  /* FIX: ignore for now
  cache-extension                     ; Section 14.9.6 */
};

Pstruct field_name_t {
  "=";
  "\"";
  /* to be filled in as above */
  "\"";
}

Punion cache_response_directive_t {
  "public"  public;
  /* FIX: to do
        | "private" [ "=" <"> 1#field-name <"> ] ; Section 14.9.1
        | "no-cache" [ "=" <"> 1#field-name <"> ]; Section 14.9.1
  */
  "no-store" no_store;
  "no-transform"  no_tranform
  "must-revalidate" must-revalidate;
  "proxy-revalidate" proxy-revalidate;
  field_time_t(:"max-age":) max_age;
  field_time_t(:"s-maxage":) s_maxage;
  /* FIX: ignore for now
  cache-extension                     ; Section 14.9.6 */
};

Punion cache_directive_t {
  cache_request_directive_t  cache_request_directive;
  cache_response_directive_t cache_response_directive;
};

#define Plongest Pended(chkErr(pd->nerr, &consume))
/* This is supposed to be a sequence of 1 or more cache_directive_t's,
   separated by commas and optional LWS */
Parray cache_directive_seq_t {
  cache_directive_t[] :  PSep(Pre "/,+/") && Plongest;
};

Parray token_seq_t {
  token_t[] :  PSep(Pre "/,+/") && Plongest;
};

Pstruct cache_control_t {
  "Cache-Control";
  ":";
  cache_directive_seq_t cache_directive_seq;
};

Pstruct connection_t {
  "Connection";
  ":";
  token_seq_t tokens; /* FIX: should be one or more */
};

Pstruct q_val_t {
  ";";
  "q";
  "=";
  /* FIX: actual grammar is a bit different */
  Pfloat(::)  value;  /* fill in appropriate base type */
};

Punion q_opt_t {
  q_val_t q;
  Pcompute Puint32 p = 0;  /* Pvoid ? */
};

Punion charset_or_star_t {
  token_t charset;
  "*"     star;
};

Pstruct accept_charset_elem_t {
  charset_or_star_t charset_or_star;
  q_opt_t           q_opt;
};

/* FIX: should be 1 or more */
Parray accept_charsetseq_t {
  accept_charset_elem_t[] :  PSep(Pre "/,+/") && Plongest;
};

Pstruct accept_charset_t {
  "Accept-Charset";
  ":";
  accept_charsetseq_t accept_charset;
};

/* NOTE: Accept-Encoding is practically identical to Accept-Charset */
Ptypedef token_t content_coding_t;
Punion content_coding_or_star_t {
  content_coding_t content_coding;
  "*"              star;
};
Pstruct accept_encoding_elem_t {
  content_coding_or_star_t content_coding_or_star;
  q_opt_t                  q_opt;
};
/* FIX: should be 1 or more */
Parray accept_encodingseq_t {
  accept_encoding_elem_t[] :  PSep(Pre "/,+/") && Plongest;
};
Pstruct accept_encoding_t {
  "Accept-Encoding";
  ":";
  accept_encodingseq_t accept_encoding;
};

/* NOTE: Accept-Language is practically identical to Accept-Charset */
Pstruct language_range_t {
  /* TODO: ( ( 1*8ALPHA *( "-" 1*8ALPHA ) ) | "*" ) */
};
Punion content_coding_or_star_t {
  language_range_t language_range;
  "*"              star;
};
Pstruct accept_language_elem_t {
  language_range_or_star_t language_range_or_star;
  q_opt_t                  q_opt;
};
/* FIX: should be 1 or more */
Parray accept_languageseq_t {
  accept_language_elem_t[] :  PSep(Pre "/,+/") && Plongest;
};
Pstruct accept_language_t {
  "Accept-Language";
  ":";
  accept_languageseq_t accept_language;
};

/* FIX: actual grammar is different */
Ptypedef Pfloat(::) qvalue_t;  /* fill in appropriate base type */
Pstruct accept_params_t {
  ";";
  "q";
  "=";
  qvalue_t qvalue;
  accept_extension_t[]; /* FIX: Not sure of termination condition */
};
Punion value_t {
  token_t token;
  quoted_string_t quoted_string; /* FIX: to fill in quoted string */
};
Ptypedef token_t attribute_t;
Pstruct parameter_t {
  attribute_t attribute;
  "=";
  value_t value;
};
Pstruct semi_parameter_t {
  ";";
  parameter_t parameter;
};
Punion media_range_head_t {
  "*/*"          star_star;
  type_star_t    type_star;
  type_subtype_t type_subtype;
};
Pstruct media_range_t {
  media_range_head_t media_range_head;
  semi_parameter_t[]; /* FIX: not sure of termination condition */
};
Pstruct accept_elem_t {
  media_range_t       media_range;
  accept_params_opt_t accept_params_opt;
};
Parray accept_seq_t {
  accept_elem_t[] :  PSep(Pre "/,+/") && Plongest;
};
Pstruct accept_t {
  "Accept";
  ":";
  accept_seq_t accept;
};

Precord Punion general_header_t {
  cache_control_t     cache_control;
  connection_t        connection;
  /* To fill in
  date_t              date;
  pragma_t            pragma;
  trailer_t           trailer;
  transfer_encoding_t transfer_encoding;
  upgrade_t           upgrade;
  via_t               via;
  warning_t           warning;
  */
};

Precord Pstruct status_line_t {
        version_t              version;
 ' ';   Puint16_FW(:3:)        status_code;
 ' ';   Pstring_SE(:"/$/":)    reason_phrase;
};

Punion start_line_t {
  request_line_t request_line;
  status_line_t  status_line;
};

int isIn(char x, char *s){
  int len = strlen(s);
  int i;
  int found = 0;
  for(i=0; i<len; i++){
    if (s[i] == x) {
      found = 1;
      break;
    }
  }
  return found;
}

/* in-memory rep is string not char */
/* Ptypedef Pstring_SE(:"(1*[CRLF](\t|' '))|[^CONTROL-characters, delete-char]":) TEXT_t; */

/* doesn't handle white space branch */
Ptypedef Pchar ctrl_t :: ctrl_t x => {(x < 32) || (x == 127)};

Ptypedef Pchar TEXT_t :: TEXT_t x => {! is_ctrl_t(&x) };  // LWS issue: definition is ambiguous

Ptypedef Pchar tspecial_t :: tspecial_t x => { isIn(x,"()<>@,;:\\\"/[]?={} \t")};  

Ptypedef Pchar token_char_t :: token_char_t x => {(x > 31) && (! is_tspecial_t(&x)) };   

Parray token_t {
  token_char_t[1:] : Plongest;
};

Precord Pstruct generic_message_header_t {
       token_t               field_name;
  ':'; Pstring_ME(:"/$/":)   field_value;  /- field content can be derived from field_value
};

Pstruct content_length_t {
  "/Content-Length/i";  // case-insenstive strings
  Pstring(:':':) content : strcmpl(content, "content-length") == 0;
  Pre "/:[ \t]*/";
  Puint32 length;
}

Punion message_header_t {
  content_length_t         contentLength;
  generic_message_header_t generic;
}

Parray entity_body_t(:Puint32 size:){
  Pchar[size:size];
}

Punion message_body_t(:Puint32 size:){
  entity_body_t(:size:)  entity_body;
}

Parray message_headers_t {
  message_header_t[]: Pterm("/$/");
}

int getlength(message_headers_t headers){
  int i;
  int result = 0;
  for(i=0; i<headers.length; i++){
    switch (headers[i].tag) {
    case contentLength: 
      return headers[i].val.length; 
    } 
  }
  return result;
}

/* Every HTTP message is a generic message, and there
   is a spec for generic messages.  However, every
   message is also either a request or reply, and these
   are defined completely separately from generic
   messages.  In other words, the spec for generic message
   is not useful for parsing requests and replies.
   However, we include its definition for reference.
*/
Pstruct generic_message_t {
  start_line_t        start_line;
  message_headers_t   message_headers;
  "\r\n";
  message_body_t(:getlength(message_headers):)  message_body;
}

Pstruct port_t {
  ":";
  Puint port; /* FIX: not sure if Puint is right */
};
Punion port_opt_t {
  port_t port;
  Pcompute Puint32 p = 0;  /* Pvoid ? */
};
Pstruct host_t {
  "Host";
  ":";
  token_t host; /* FIX: not sure if this should be token_t */
  port_opt_t port_opt;
};

Ptypedef token_t product_version_t;
Pstruct slash_product_version_t {
  "/";
  product_version_t product_version;
};
Punion product_version_opt_t {
  slash_product_version_t slash_product_version;
  Pcompute Puint32 p = 0;  /* Pvoid ? */
};
Pstruct product_t {
  token_t product;
  product_version_opt_t product_version_opt;
};
Punion product_or_comment_t {
  product_t product;
  comment_t comment;
};

Pstruct user_agent_t {
  "User-Agent";
  ":";
  product_or_comment_t[] product_or_comment; /* FIX: should be 1 or more */
};

Punion request_header_t {
  accept_t		accept;
  accept_charset_t	accept_charset;
  accept_encoding_t	accept_encoding;
  accept_language_t	accept_language;
  /*
  authorization_t	authorization;
  expect_t		expect;
  from_t		from;
  */
  host_t		host;
  /*
  if_match_t		if_match;
  if_modified_since_t	if_modified_since;
  if_none_match_t	if_none_match;
  if_range_t		if_range;
  if_unmodified_since_t	if_unmodified_since;
  max_forwards_t	max_forwards;
  proxy_authorization_t	proxy_authorization;
  range_t		range;
  referer_t		referer;
  te_t			te;
  */
  user_agent_t		user_agent;
}

/* NB: headers is not a grammar symbol from RFC 2616 */
Punion headers_t {
  general_header_t general_header;
  request_header_t request_header;
  entity_header_t entity_header;
}

Pstruct request_t {
  request_line_t  request_line;
  headers_t       headers;
  "\r\n";
  message_body_t  message_body;
}

