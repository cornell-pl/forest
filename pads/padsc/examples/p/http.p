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

/* PDC_WSPACE_OK should not be set.  */
Penum d_method_t {
    OPTION,
    GET, 
    HEAD, 
    POST, 
    PUT, 
    DELETE, 
    TRACE
};

Punion method_t {
  d_method_t      method;
  Pstring(:' ':)  extension_method;
};

Pstruct version_t {
  "HTTP/";
  Puint8 major; /- should be 1*DIGIT
  '.';
  Puint8 minor; /- should be 1*DIGIT
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

   // FIX: maybe use this below for request_uri field
Punion Request_URI_t {
  "*"           applies_to_server;
  absoluteURI_t absoluteURI;
  abs_path_t    abs_path;
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

/* requires white space elimination discipline */
Precord Pstruct request_line_t {
   method_t           meth;                       /- Method used during request
   Pstring_SE(:LWS0:) request_uri;          /- Requested uri.
   version_t          http_version;         /- HTTP version number of request 
};

int chkErr(int nerr, int * consume){
  if (nerr) {
    *consume = 0;
  };
  return nerr;
};

Pstruct delta_seconds_t{
  "=";
  Puint64 d;
};

Pstruct field_time_t(:char *s:){
  s;
  delta_seconds_t delta_seconds;
};


Punion delta_seconds_opt_t{
  delta_seconds_t delta_seconds;
  Pcompute Puint32 omitted = 0;  /* Pvoid ??*/
};

Pstruct max_stale_t{
  "max-stale";
  delta_seconds_opt_t delta_seconds_opt;
};

Punion cache_request_directive_t{
  "no-cache"       no_cache;                         
  "no-store"       no_store;                        
  field_time_t(:"max-age":)        max_age;
  field_time_t(:"max-stale":)      max_stale;
  min_fres_t       min_fresh;
  "no-transform"   no_transform;
  "only-if-cached" only_if_cached;                  
  /*  ignore for now:        cache-extension                     ; Section 14.9.6 */
};

Pstruct field_name_t{
  "=";
  "\"";
  /* to be filled in as above */
  "\"";
}

Punion cache_response_directive_t{
  "public"  public;
        | "private" [ "=" <"> 1#field-name <"> ] ; Section 14.9.1
        | "no-cache" [ "=" <"> 1#field-name <"> ]; Section 14.9.1
						     "no-store" no_store;
  "no-transform"  no_tranform                         ; Section 14.9.5
  "must-revalidate"                      ; Section 14.9.4
  "proxy-revalidate"                     ; Section 14.9.4
   field_time_t(:"max-age":) max_age;
   field_time_t(:"s-maxage":) s_maxage;
   /*         | cache-extension                        ; Section 14.9.6 */
};

Punion cache_directive_t{
  cache_request_directive_t  cache_request_directive;
  cache_response_directive_t cache_response_directive;
};

#define Plongest Pended(chkErr(pd->nerr, &consume))
Parray cache_directive_seq_t {
  cache_directive_t[] :  PSep(Pre "/,+/") && Plongest;
};

Parray token_seq_t {
  token_t[] :  PSep(Pre "/,+/") && Plongest;
};

Pstruct cache_control_t{
  "Cache-Control";
  ":";
  cache_directive_seq_t cache_directive_seq;
};

Pstruct connection_t{
  "Connection";
  ":";
  token_seq_t tokens;
};

Pstruct q_val_t{
  ";";
  "q";
  "=";
  Pfloat(::)  value;  /* fill in appropriate base type */
};

Punion q_opt_t{
  q_val_t q;
  Pcompute Puint32 p = 0;  /* Pvoid ? */
};


Pstruct accept_charset{
  token_t token;
  q_opt_t qopt;
};

Parray accept_charsetseq_t {
  accept_charset[] :  PSep(Pre "/,+/") && Plongest;
};

Pstruct accept_charset_t{
  "Accept-Charset";
  ":";
  accept_charsetseq_t accept_charset;
};

Precord Punion general_header_t {
  cache_control_t cache_control;
  connection_t    connection;
};

Precord Pstruct status_line_t{
        version_t              version;
 ' ';   Puint16_FW(:3:)        status_code;
 ' ';   Pstring_SE(:"/$/":)    reason_phrase;
};

Punion start_line_t{
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

Parray token_t{
  token_char_t[1:] : Plongest;
};


Precord Pstruct generic_message_header_t{
       token_t               field_name;
  ':'; Pstring_ME(:"/$/":)   field_value;  /- field content can be derived from field_value
};

Pstruct content_length_t{
  "/Content-Length/i";  // case-insenstive strings
  Pstring(:':':) content : strcmpl(content, "content-length") == 0;
  Pre "/:[ \t]*/";
  Puint32 length;
}

Punion message_header_t{
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

Pstruct generic_message_t{
  start_line_t        start_line;
  message_headers_t   message_headers;
  "\r\n";
  message_body_t(:getlength(message_headers):)  message_body;
}


Pstruct request_t{
  request_line_t  request_line;
  
}

