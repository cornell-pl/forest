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
  Puint8 major; '.';
  Puint8 minor;           /- http minor mode
};

Precord Pstruct request_line_t {
        method_t meth;                       /- Method used during request
  ' ';  Pstring(:' ':) request_uri;          /- Requested uri.
  ' ';  version_t      http_version;         /- HTTP version number of request 
};

Pstruct status_line_t{
        version_t              version;
 ' ';   Puint16_FW(:3:)        status_code;
 ' ';   Pstring_SE(:"\$\":)    reason_phrase;  /- *** only partially implemented
 Pre "\$\";
};

Punion start_line_t{
  request_line_t request_line;
  status_line_t  status_line;
};


/* in-memory rep is string not char */
Ptypedef Pstring_ME(:"(1*[CRLF](\t|' '))|[^CONTROL-characters, delete-char]":) TEXT_t;

/* doesn't handle white space branch */
Ptypedef Puchar ctrl_t :: ctrl_t(x) => {(x < 32) || (x == 127)}  

Ptypedef Puchar TEXT_t :: TEXT_t x => {! is_ctrl_t(x) }  // LWS issue: definition is ambiguous

// expand? syntactic support

Ptypedef Puchar tspecial_t :: tspecial_t(x) => { x in ["()<>@,;:\\\"/[]?={} \t"]}  


Ptypedef Puchar token_char_t :: token_char_t(x) => {(x > 31) && (! is_tspecial_t(x)) }   // implement this

Parray token_t(Puchar ending){
  token_char_t[1:] ts : Pterm == ending;
};

Precord Pstruct generic_message_header_t{
       token_t(:':':)        field_name;
  ':'; Pstring_ME(:"\$\":)   field_value;  /- field content can be derived from field_value
};

Pstruct content_length_t{
  "/Content-Length/i";  // case-insenstive strings
  Pstring(:':':) content : strcmpl(content, "content-length") == 0;
  //  "/:[ \t]*/";  
  Pomit Pstring_ME(":[ \t]*") foo;          // regular expression literals
  Puint32 length;
}

Punion message_header_t{
  content_length_t         contentLength;
  generic_message_header_t generic;
}

Parray entity_body_t(Puint32 size){
  Puchar[size:size] bytes;
}

Punion message_body_t(Puint32 size){
  entity_body_t(:size:)  entity_body;
}

Parray message_headers_t {
  message_header_t[] headers : Pterm = "\r\n";
}

Puint getlength(message_headers_t headers){
  int i;
  Puint result = 0;
  for(i=0; i<headers.length; i++){
    switch (headers[i].tag) {
    case contentLength: 
      return headers[i].val.length; 
    } 
  }
  return ???;
}

Pstruct generic_message_t{
  start_line_t        start_line;
  message_headers_t   message_headers;
  "\r\n";
  message_body_t(:getlength(message_headers):)  message_body;
}

(*  input filter *)
