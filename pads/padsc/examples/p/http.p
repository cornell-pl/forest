#define Pvoid(label) Pcompute Pomit Puint32 label = 0

int is_CHAR(Pchar x) { return (0 >= x) && (x <= 127); };
Ptypedef Pchar CHAR_t :: CHAR_t x => { is_CHAR(x) };
Pcharclass CHAR {is_CHAR};
#define RE_CHAR "[[:CHAR:]]"
int is_CR(Pchar x) { return (x == 13); };
Ptypedef Pchar CR_t :: CR_t x => { is_CR(x) };
Pcharclass CR {is_CR};
#define RE_CR "[[:CR:]]"
int is_CTL(Pchar x) { return
  ((x >= 0) && (x <= 31))
  || (x == 127)
  ;
};
Ptypedef Pchar CTL_t :: CTL_t x => { is_CTL(x) };
Pcharclass CTL {is_CTL};
#define RE_CTL "[[:CTL:]]"
int is_DIGIT(Pchar x) { return (48 >= x) && (x <= 57); };
Ptypedef Pchar DIGIT_t :: DIGIT_t x => { is_DIGIT(x) };
Pcharclass DIGIT {is_DIGIT};
#define RE_DIGIT "[[:DIGIT:]]"
int is_DQUOTE(Pchar x) { return
  (x == 34)
  ;
};
Ptypedef Pchar DQUOTE_t :: DQUOTE_t x => { is_DQUOTE(x) };
Pcharclass DQUOTE {is_DQUOTE};
#define RE_DQUOTE "[[:DQUOTE:]]"
int is_HEX(Pchar x) { return
  (x == 65)
  || (x == 66)
  || (x == 67)
  || (x == 68)
  || (x == 69)
  || (x == 70)
  || (x == 97)
  || (x == 98)
  || (x == 99)
  || (x == 100)
  || (x == 101)
  || (x == 102)
  || is_DIGIT(x)
  ;
};
Ptypedef Pchar HEX_t :: HEX_t x => { is_HEX(x) };
Pcharclass HEX {is_HEX};
#define RE_HEX "[[:HEX:]]"
int is_HT(Pchar x) { return (x == 9); };
Ptypedef Pchar HT_t :: HT_t x => { is_HT(x) };
Pcharclass HT {is_HT};
#define RE_HT "[[:HT:]]"
#define RE_HTTP_Version "(HTTP\\/" RE_DIGIT "+\\." RE_DIGIT "+)"
Ptypedef Pstring_ME(:"/" RE_HTTP_Version "/":) HTTP_Version_t;
int is_LF(Pchar x) { return (x == 10); };
Ptypedef Pchar LF_t :: LF_t x => { is_LF(x) };
Pcharclass LF {is_LF};
#define RE_LF "[[:LF:]]"
#define RE_CRLF "(" RE_CR "" RE_LF ")"
Ptypedef Pstring_ME(:"/" RE_CRLF "/":) CRLF_t;
int is_LHEX(Pchar x) { return
  (x == 48)
  || (x == 49)
  || (x == 50)
  || (x == 51)
  || (x == 52)
  || (x == 53)
  || (x == 54)
  || (x == 55)
  || (x == 56)
  || (x == 57)
  || (x == 97)
  || (x == 98)
  || (x == 99)
  || (x == 100)
  || (x == 101)
  || (x == 102)
  ;
};
Ptypedef Pchar LHEX_t :: LHEX_t x => { is_LHEX(x) };
Pcharclass LHEX {is_LHEX};
#define RE_LHEX "[[:LHEX:]]"
int is_LOALPHA(Pchar x) { return (97 >= x) && (x <= 122); };
Ptypedef Pchar LOALPHA_t :: LOALPHA_t x => { is_LOALPHA(x) };
Pcharclass LOALPHA {is_LOALPHA};
#define RE_LOALPHA "[[:LOALPHA:]]"
int is_NO_WS_CTL(Pchar x) { return
  ((x >= 1) && (x <= 8))
  || (x == 11)
  || (x == 12)
  || ((x >= 14) && (x <= 31))
  || (x == 127)
  ;
};
Ptypedef Pchar NO_WS_CTL_t :: NO_WS_CTL_t x => { is_NO_WS_CTL(x) };
Pcharclass NO_WS_CTL {is_NO_WS_CTL};
#define RE_NO_WS_CTL "[[:NO_WS_CTL:]]"
int is_OCTET(Pchar x) { return (0 >= x) && (x <= 255); };
Ptypedef Pchar OCTET_t :: OCTET_t x => { is_OCTET(x) };
Pcharclass OCTET {is_OCTET};
#define RE_OCTET "[[:OCTET:]]"
#define RE_Reason_Phrase "([\\x20-\\x7e]|[\\x80-\\xff])*"
Ptypedef Pstring_ME(:"/" RE_Reason_Phrase "/":) Reason_Phrase_t;
int is_SP(Pchar x) { return (x == 32); };
Ptypedef Pchar SP_t :: SP_t x => { is_SP(x) };
Pcharclass SP {is_SP};
#define RE_SP "[[:SP:]]"
#define RE_LWS "(" RE_CRLF "?(" RE_SP "|" RE_HT ")+)"
Ptypedef Pstring_ME(:"/" RE_LWS "/":) LWS_t;
int is_TEXT(Pchar x) { return
  ((x >= 32) && (x <= 126))
  || ((x >= 128) && (x <= 255))
  ;
};
Ptypedef Pchar TEXT_t :: TEXT_t x => { is_TEXT(x) };
Pcharclass TEXT {is_TEXT};
#define RE_TEXT "[[:TEXT:]]"
int is_UPALPHA(Pchar x) { return (65 >= x) && (x <= 90); };
Ptypedef Pchar UPALPHA_t :: UPALPHA_t x => { is_UPALPHA(x) };
Pcharclass UPALPHA {is_UPALPHA};
#define RE_UPALPHA "[[:UPALPHA:]]"
int is_ALPHA(Pchar x) { return
  is_UPALPHA(x)
  || is_LOALPHA(x)
  ;
};
Ptypedef Pchar ALPHA_t :: ALPHA_t x => { is_ALPHA(x) };
Pcharclass ALPHA {is_ALPHA};
#define RE_ALPHA "[[:ALPHA:]]"
int is_WSP(Pchar x) { return
  (x == 9)
  || (x == 32)
  ;
};
Ptypedef Pchar WSP_t :: WSP_t x => { is_WSP(x) };
Pcharclass WSP {is_WSP};
#define RE_WSP "[[:WSP:]]"
int is_atext(Pchar x) { return
  is_ALPHA(x)
  || is_DIGIT(x)
  || (x == 33)
  || (x == 35)
  || (x == 36)
  || (x == 37)
  || (x == 38)
  || (x == 39)
  || (x == 42)
  || (x == 43)
  || (x == 45)
  || (x == 47)
  || (x == 61)
  || (x == 63)
  || (x == 94)
  || (x == 95)
  || (x == 96)
  || (x == 123)
  || (x == 124)
  || (x == 125)
  || (x == 126)
  ;
};
Ptypedef Pchar atext_t :: atext_t x => { is_atext(x) };
Pcharclass atext {is_atext};
#define RE_atext "[[:atext:]]"
#define RE_base64_user_pass "XX"
Ptypedef Pstring_ME(:"/" RE_base64_user_pass "/":) base64_user_pass_t;
#define RE_basic_credentials RE_base64_user_pass
Ptypedef Pstring_ME(:"/" RE_basic_credentials "/":) basic_credentials_t;
#define RE_bytes_unit "bytes"
Ptypedef Pstring_ME(:"/" RE_bytes_unit "/":) bytes_unit_t;
#define RE_comment "XX"
Ptypedef Pstring_ME(:"/" RE_comment "/":) comment_t;
#define RE_comment_mailbox "XX"
Ptypedef Pstring_ME(:"/" RE_comment_mailbox "/":) comment_mailbox_t;
int is_ctext(Pchar x) { return
  ((x >= 32) && (x <= 39))
  || ((x >= 42) && (x <= 126))
  || ((x >= 128) && (x <= 255))
  ;
};
Ptypedef Pchar ctext_t :: ctext_t x => { is_ctext(x) };
Pcharclass ctext {is_ctext};
#define RE_ctext "[[:ctext:]]"
int is_ctext_mailbox(Pchar x) { return
  is_NO_WS_CTL(x)
  || ((x >= 33) && (x <= 39))
  || ((x >= 42) && (x <= 91))
  || ((x >= 93) && (x <= 126))
  ;
};
Ptypedef Pchar ctext_mailbox_t :: ctext_mailbox_t x => { is_ctext_mailbox(x) };
Pcharclass ctext_mailbox {is_ctext_mailbox};
#define RE_ctext_mailbox "[[:ctext_mailbox:]]"
#define RE_delta_seconds "" RE_DIGIT "+"
Ptypedef Pstring_ME(:"/" RE_delta_seconds "/":) delta_seconds_t;
#define RE_age_value RE_delta_seconds
Ptypedef Pstring_ME(:"/" RE_age_value "/":) age_value_t;
int is_digit(Pchar x) { return
  (x == 48)
  || (x == 49)
  || (x == 50)
  || (x == 51)
  || (x == 52)
  || (x == 53)
  || (x == 54)
  || (x == 55)
  || (x == 56)
  || (x == 57)
  ;
};
Ptypedef Pchar digit_t :: digit_t x => { is_digit(x) };
Pcharclass digit {is_digit};
#define RE_digit "[[:digit:]]"
#define RE_IPv4address "(" RE_digit "+\\." RE_digit "+\\." RE_digit "+\\." RE_digit "+)"
Ptypedef Pstring_ME(:"/" RE_IPv4address "/":) IPv4address_t;
#define RE_dot_atom_text "(" RE_atext "+(\\." RE_atext "+)*)"
Ptypedef Pstring_ME(:"/" RE_dot_atom_text "/":) dot_atom_text_t;
int is_dtext(Pchar x) { return
  is_NO_WS_CTL(x)
  || ((x >= 33) && (x <= 90))
  || ((x >= 94) && (x <= 126))
  ;
};
Ptypedef Pchar dtext_t :: dtext_t x => { is_dtext(x) };
Pcharclass dtext {is_dtext};
#define RE_dtext "[[:dtext:]]"
int is_hex(Pchar x) { return
  is_digit(x)
  || (x == 65)
  || (x == 66)
  || (x == 67)
  || (x == 68)
  || (x == 69)
  || (x == 70)
  || (x == 97)
  || (x == 98)
  || (x == 99)
  || (x == 100)
  || (x == 101)
  || (x == 102)
  ;
};
Ptypedef Pchar hex_t :: hex_t x => { is_hex(x) };
Pcharclass hex {is_hex};
#define RE_hex "[[:hex:]]"
#define RE_escaped "(\\%" RE_hex "" RE_hex ")"
Ptypedef Pstring_ME(:"/" RE_escaped "/":) escaped_t;
int is_lowalpha(Pchar x) { return
  (x == 97)
  || (x == 98)
  || (x == 99)
  || (x == 100)
  || (x == 101)
  || (x == 102)
  || (x == 103)
  || (x == 104)
  || (x == 105)
  || (x == 106)
  || (x == 107)
  || (x == 108)
  || (x == 109)
  || (x == 110)
  || (x == 111)
  || (x == 112)
  || (x == 113)
  || (x == 114)
  || (x == 115)
  || (x == 116)
  || (x == 117)
  || (x == 118)
  || (x == 119)
  || (x == 120)
  || (x == 121)
  || (x == 122)
  ;
};
Ptypedef Pchar lowalpha_t :: lowalpha_t x => { is_lowalpha(x) };
Pcharclass lowalpha {is_lowalpha};
#define RE_lowalpha "[[:lowalpha:]]"
int is_mark(Pchar x) { return
  (x == 45)
  || (x == 95)
  || (x == 46)
  || (x == 33)
  || (x == 126)
  || (x == 42)
  || (x == 39)
  || (x == 40)
  || (x == 41)
  ;
};
Ptypedef Pchar mark_t :: mark_t x => { is_mark(x) };
Pcharclass mark {is_mark};
#define RE_mark "[[:mark:]]"
#define RE_md5_digest "XX"
Ptypedef Pstring_ME(:"/" RE_md5_digest "/":) md5_digest_t;
#define RE_month "(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)"
Ptypedef Pstring_ME(:"/" RE_month "/":) month_t;
#define RE_date1 "(" RE_DIGIT "{2}" RE_SP "" RE_month "" RE_SP "" RE_DIGIT "{4})"
Ptypedef Pstring_ME(:"/" RE_date1 "/":) date1_t;
#define RE_date2 "(" RE_DIGIT "{2}\\-" RE_month "\\-" RE_DIGIT "{2})"
Ptypedef Pstring_ME(:"/" RE_date2 "/":) date2_t;
#define RE_date3 "(" RE_month "" RE_SP "(" RE_DIGIT "{2}|(" RE_SP "" RE_DIGIT "{1})))"
Ptypedef Pstring_ME(:"/" RE_date3 "/":) date3_t;
#define RE_obs_FWS "(" RE_WSP "+(" RE_CRLF "" RE_WSP "+)*)"
Ptypedef Pstring_ME(:"/" RE_obs_FWS "/":) obs_FWS_t;
#define RE_FWS "(((" RE_WSP "*" RE_CRLF ")?" RE_WSP "+)|" RE_obs_FWS ")"
Ptypedef Pstring_ME(:"/" RE_FWS "/":) FWS_t;
#define RE_CFWS "((" RE_FWS "?" RE_comment_mailbox ")*((" RE_FWS "?" RE_comment_mailbox ")|" RE_FWS "))"
Ptypedef Pstring_ME(:"/" RE_CFWS "/":) CFWS_t;
#define RE_atom "(" RE_CFWS "?" RE_atext "+" RE_CFWS "?)"
Ptypedef Pstring_ME(:"/" RE_atom "/":) atom_t;
#define RE_dot_atom "(" RE_CFWS "?" RE_dot_atom_text "" RE_CFWS "?)"
Ptypedef Pstring_ME(:"/" RE_dot_atom "/":) dot_atom_t;
int is_obs_char(Pchar x) { return
  ((x >= 0) && (x <= 9))
  || (x == 11)
  || (x == 12)
  || ((x >= 14) && (x <= 127))
  ;
};
Ptypedef Pchar obs_char_t :: obs_char_t x => { is_obs_char(x) };
Pcharclass obs_char {is_obs_char};
#define RE_obs_char "[[:obs_char:]]"
#define RE_obs_domain "(" RE_atom "(\\." RE_atom ")*)"
Ptypedef Pstring_ME(:"/" RE_obs_domain "/":) obs_domain_t;
#define RE_obs_qp "(\\\\[\\x0-\\x7f])"
Ptypedef Pstring_ME(:"/" RE_obs_qp "/":) obs_qp_t;
#define RE_obs_text "(" RE_LF "*" RE_CR "*(" RE_obs_char "" RE_LF "*" RE_CR "*)*)"
Ptypedef Pstring_ME(:"/" RE_obs_text "/":) obs_text_t;
#define RE_port "" RE_digit "*"
Ptypedef Pstring_ME(:"/" RE_port "/":) port_t;
#define RE_primary_tag "" RE_ALPHA "{1,8}"
Ptypedef Pstring_ME(:"/" RE_primary_tag "/":) primary_tag_t;
int is_qtext(Pchar x) { return
  is_NO_WS_CTL(x)
  || (x == 33)
  || ((x >= 35) && (x <= 91))
  || ((x >= 93) && (x <= 126))
  ;
};
Ptypedef Pchar qtext_t :: qtext_t x => { is_qtext(x) };
Pcharclass qtext {is_qtext};
#define RE_qtext "[[:qtext:]]"
#define RE_quoted_pair "(\\\\" RE_CHAR ")"
Ptypedef Pstring_ME(:"/" RE_quoted_pair "/":) quoted_pair_t;
int is_reserved(Pchar x) { return
  (x == 59)
  || (x == 47)
  || (x == 63)
  || (x == 58)
  || (x == 64)
  || (x == 38)
  || (x == 61)
  || (x == 43)
  || (x == 36)
  || (x == 44)
  ;
};
Ptypedef Pchar reserved_t :: reserved_t x => { is_reserved(x) };
Pcharclass reserved {is_reserved};
#define RE_reserved "[[:reserved:]]"
int is_separators(Pchar x) { return
  (x == 40)
  || (x == 41)
  || (x == 60)
  || (x == 62)
  || (x == 64)
  || (x == 44)
  || (x == 59)
  || (x == 58)
  || (x == 92)
  || (x == 34)
  || (x == 47)
  || (x == 91)
  || (x == 93)
  || (x == 63)
  || (x == 61)
  || (x == 123)
  || (x == 125)
  || is_SP(x)
  || is_HT(x)
  ;
};
Ptypedef Pchar separators_t :: separators_t x => { is_separators(x) };
Pcharclass separators {is_separators};
#define RE_separators "[[:separators:]]"
#define RE_subtag "" RE_ALPHA "{1,8}"
Ptypedef Pstring_ME(:"/" RE_subtag "/":) subtag_t;
#define RE_language_tag "(" RE_primary_tag "(\\-" RE_subtag ")*)"
Ptypedef Pstring_ME(:"/" RE_language_tag "/":) language_tag_t;
#define RE_text "([\\x1-\\x9]|\\xb|\\xc|[\\xe-\\x7f]|" RE_obs_text ")"
Ptypedef Pstring_ME(:"/" RE_text "/":) text_t;
#define RE_quoted_pair_mailbox "((\\\\" RE_text ")|" RE_obs_qp ")"
Ptypedef Pstring_ME(:"/" RE_quoted_pair_mailbox "/":) quoted_pair_mailbox_t;
#define RE_dcontent "(" RE_dtext "|" RE_quoted_pair_mailbox ")"
Ptypedef Pstring_ME(:"/" RE_dcontent "/":) dcontent_t;
#define RE_domain_literal "(" RE_CFWS "?\\[(" RE_FWS "?" RE_dcontent ")*" RE_FWS "?\\]" RE_CFWS "?)"
Ptypedef Pstring_ME(:"/" RE_domain_literal "/":) domain_literal_t;
#define RE_domain "(" RE_dot_atom "|" RE_domain_literal "|" RE_obs_domain ")"
Ptypedef Pstring_ME(:"/" RE_domain "/":) domain_t;
#define RE_obs_domain_list "(\\@" RE_domain "((" RE_CFWS "|\\,)*" RE_CFWS "?\\@" RE_domain ")*)"
Ptypedef Pstring_ME(:"/" RE_obs_domain_list "/":) obs_domain_list_t;
#define RE_obs_route "(" RE_CFWS "?" RE_obs_domain_list "\\:" RE_CFWS "?)"
Ptypedef Pstring_ME(:"/" RE_obs_route "/":) obs_route_t;
#define RE_qcontent "(" RE_qtext "|" RE_quoted_pair_mailbox ")"
Ptypedef Pstring_ME(:"/" RE_qcontent "/":) qcontent_t;
#define RE_quoted_string "(" RE_CFWS "?" RE_DQUOTE "(" RE_FWS "?" RE_qcontent ")*" RE_FWS "?" RE_DQUOTE "" RE_CFWS "?)"
Ptypedef Pstring_ME(:"/" RE_quoted_string "/":) quoted_string_t;
#define RE_nonce_value RE_quoted_string
Ptypedef Pstring_ME(:"/" RE_nonce_value "/":) nonce_value_t;
#define RE_cnonce_value RE_nonce_value
Ptypedef Pstring_ME(:"/" RE_cnonce_value "/":) cnonce_value_t;
#define RE_opaque_tag RE_quoted_string
Ptypedef Pstring_ME(:"/" RE_opaque_tag "/":) opaque_tag_t;
#define RE_realm_value RE_quoted_string
Ptypedef Pstring_ME(:"/" RE_realm_value "/":) realm_value_t;
int is_token_char(Pchar x) { return
  (x == 33)
  || ((x >= 35) && (x <= 39))
  || ((x >= 42) && (x <= 43))
  || ((x >= 45) && (x <= 46))
  || ((x >= 48) && (x <= 57))
  || (x == 63)
  || ((x >= 65) && (x <= 90))
  || ((x >= 94) && (x <= 122))
  || (x == 124)
  || ((x >= 126) && (x <= 127))
  ;
};
Ptypedef Pchar token_char_t :: token_char_t x => { is_token_char(x) };
Pcharclass token_char {is_token_char};
#define RE_token_char "[[:token_char:]]"
#define RE_token "" RE_token_char "+"
Ptypedef Pstring_ME(:"/" RE_token "/":) token_t;
#define RE_attribute RE_token
Ptypedef Pstring_ME(:"/" RE_attribute "/":) attribute_t;
#define RE_auth_scheme RE_token
Ptypedef Pstring_ME(:"/" RE_auth_scheme "/":) auth_scheme_t;
#define RE_charset RE_token
Ptypedef Pstring_ME(:"/" RE_charset "/":) charset_t;
#define RE_chunk_ext_name RE_token
Ptypedef Pstring_ME(:"/" RE_chunk_ext_name "/":) chunk_ext_name_t;
#define RE_chunk_ext_val "(" RE_token "|" RE_quoted_string ")"
Ptypedef Pstring_ME(:"/" RE_chunk_ext_val "/":) chunk_ext_val_t;
#define RE_connection_token RE_token
Ptypedef Pstring_ME(:"/" RE_connection_token "/":) connection_token_t;
#define RE_content_coding RE_token
Ptypedef Pstring_ME(:"/" RE_content_coding "/":) content_coding_t;
#define RE_codings "(" RE_content_coding "|\\*)"
Ptypedef Pstring_ME(:"/" RE_codings "/":) codings_t;
#define RE_disp_extension_token RE_token
Ptypedef Pstring_ME(:"/" RE_disp_extension_token "/":) disp_extension_token_t;
#define RE_disposition_type "(attachment|" RE_disp_extension_token ")"
Ptypedef Pstring_ME(:"/" RE_disposition_type "/":) disposition_type_t;
#define RE_extension_method RE_token
Ptypedef Pstring_ME(:"/" RE_extension_method "/":) extension_method_t;
#define RE_Method "(OPTIONS|GET|HEAD|POST|PUT|DELETE|TRACE|CONNECT|" RE_extension_method ")"
Ptypedef Pstring_ME(:"/" RE_Method "/":) Method_t;
#define RE_field_name RE_token
Ptypedef Pstring_ME(:"/" RE_field_name "/":) field_name_t;
#define RE_other_range_unit RE_token
Ptypedef Pstring_ME(:"/" RE_other_range_unit "/":) other_range_unit_t;
#define RE_product_version RE_token
Ptypedef Pstring_ME(:"/" RE_product_version "/":) product_version_t;
#define RE_protocol_name RE_token
Ptypedef Pstring_ME(:"/" RE_protocol_name "/":) protocol_name_t;
#define RE_protocol_version RE_token
Ptypedef Pstring_ME(:"/" RE_protocol_version "/":) protocol_version_t;
#define RE_pseudonym RE_token
Ptypedef Pstring_ME(:"/" RE_pseudonym "/":) pseudonym_t;
#define RE_qop_value "(auth|auth\\-int|" RE_token ")"
Ptypedef Pstring_ME(:"/" RE_qop_value "/":) qop_value_t;
#define RE_range_unit "(" RE_bytes_unit "|" RE_other_range_unit ")"
Ptypedef Pstring_ME(:"/" RE_range_unit "/":) range_unit_t;
#define RE_subtype RE_token
Ptypedef Pstring_ME(:"/" RE_subtype "/":) subtype_t;
#define RE_type RE_token
Ptypedef Pstring_ME(:"/" RE_type "/":) type_t;
int is_upalpha(Pchar x) { return
  (x == 65)
  || (x == 66)
  || (x == 67)
  || (x == 68)
  || (x == 69)
  || (x == 70)
  || (x == 71)
  || (x == 72)
  || (x == 73)
  || (x == 74)
  || (x == 75)
  || (x == 76)
  || (x == 77)
  || (x == 78)
  || (x == 79)
  || (x == 80)
  || (x == 81)
  || (x == 82)
  || (x == 83)
  || (x == 84)
  || (x == 85)
  || (x == 86)
  || (x == 87)
  || (x == 88)
  || (x == 89)
  || (x == 90)
  ;
};
Ptypedef Pchar upalpha_t :: upalpha_t x => { is_upalpha(x) };
Pcharclass upalpha {is_upalpha};
#define RE_upalpha "[[:upalpha:]]"
int is_alpha(Pchar x) { return
  is_lowalpha(x)
  || is_upalpha(x)
  ;
};
Ptypedef Pchar alpha_t :: alpha_t x => { is_alpha(x) };
Pcharclass alpha {is_alpha};
#define RE_alpha "[[:alpha:]]"
int is_alphanum(Pchar x) { return
  is_alpha(x)
  || is_digit(x)
  ;
};
Ptypedef Pchar alphanum_t :: alphanum_t x => { is_alphanum(x) };
Pcharclass alphanum {is_alphanum};
#define RE_alphanum "[[:alphanum:]]"
#define RE_domainlabel "(" RE_alphanum "(\\-?" RE_alphanum ")*)"
Ptypedef Pstring_ME(:"/" RE_domainlabel "/":) domainlabel_t;
#define RE_hostname "(" RE_domainlabel "(\\." RE_domainlabel ")*\\.?)"
Ptypedef Pstring_ME(:"/" RE_hostname "/":) hostname_t;
#define RE_host "(" RE_hostname "|" RE_IPv4address ")"
Ptypedef Pstring_ME(:"/" RE_host "/":) host_t;
#define RE_hostport "(" RE_host "(\\:" RE_port ")?)"
Ptypedef Pstring_ME(:"/" RE_hostport "/":) hostport_t;
#define RE_scheme "(" RE_alpha "(" RE_alpha "|" RE_digit "|\\+|\\-|\\.)*)"
Ptypedef Pstring_ME(:"/" RE_scheme "/":) scheme_t;
int is_unreserved(Pchar x) { return
  is_alphanum(x)
  || is_mark(x)
  ;
};
Ptypedef Pchar unreserved_t :: unreserved_t x => { is_unreserved(x) };
Pcharclass unreserved {is_unreserved};
#define RE_unreserved "[[:unreserved:]]"
#define RE_param_char "(" RE_unreserved "|" RE_escaped "|\\:|\\@|\\&|\\=|\\+|\\$|\\,)"
Ptypedef Pstring_ME(:"/" RE_param_char "/":) param_char_t;
#define RE_param "" RE_param_char "*"
Ptypedef Pstring_ME(:"/" RE_param "/":) param_t;
#define RE_reg_name "(" RE_unreserved "|" RE_escaped "|\\$|\\,|\\;|\\:|\\@|\\&|\\=|\\+)+"
Ptypedef Pstring_ME(:"/" RE_reg_name "/":) reg_name_t;
#define RE_rel_segment "(" RE_unreserved "|" RE_escaped "|\\;|\\@|\\&|\\=|\\+|\\$|\\,)+"
Ptypedef Pstring_ME(:"/" RE_rel_segment "/":) rel_segment_t;
#define RE_segment "(" RE_param_char "*(\\;" RE_param ")*)"
Ptypedef Pstring_ME(:"/" RE_segment "/":) segment_t;
#define RE_path_segments "(" RE_segment "(\\/" RE_segment ")*)"
Ptypedef Pstring_ME(:"/" RE_path_segments "/":) path_segments_t;
#define RE_abs_path "(\\/" RE_path_segments ")"
Ptypedef Pstring_ME(:"/" RE_abs_path "/":) abs_path_t;
#define RE_rel_path "(" RE_rel_segment "" RE_abs_path "?)"
Ptypedef Pstring_ME(:"/" RE_rel_path "/":) rel_path_t;
#define RE_uric "(" RE_reserved "|" RE_unreserved "|" RE_escaped ")"
Ptypedef Pstring_ME(:"/" RE_uric "/":) uric_t;
#define RE_query "" RE_uric "*"
Ptypedef Pstring_ME(:"/" RE_query "/":) query_t;
#define RE_http_URL "(http\\:\\/\\/" RE_host "(\\:" RE_port ")?(" RE_abs_path "(\\?" RE_query ")?)?)"
Ptypedef Pstring_ME(:"/" RE_http_URL "/":) http_URL_t;
#define RE_uric_no_slash "(" RE_unreserved "|" RE_escaped "|\\;|\\?|\\:|\\@|\\&|\\=|\\+|\\$|\\,)"
Ptypedef Pstring_ME(:"/" RE_uric_no_slash "/":) uric_no_slash_t;
#define RE_opaque_part "(" RE_uric_no_slash "" RE_uric "*)"
Ptypedef Pstring_ME(:"/" RE_opaque_part "/":) opaque_part_t;
#define RE_userinfo "(" RE_unreserved "|" RE_escaped "|\\;|\\:|\\&|\\=|\\+|\\$|\\,)*"
Ptypedef Pstring_ME(:"/" RE_userinfo "/":) userinfo_t;
#define RE_server "((" RE_userinfo "\\@)?" RE_hostport ")?"
Ptypedef Pstring_ME(:"/" RE_server "/":) server_t;
#define RE_authority "(" RE_server "|" RE_reg_name ")"
Ptypedef Pstring_ME(:"/" RE_authority "/":) authority_t;
#define RE_net_path "(\\/\\/" RE_authority "" RE_abs_path "?)"
Ptypedef Pstring_ME(:"/" RE_net_path "/":) net_path_t;
#define RE_hier_part "((" RE_net_path "|" RE_abs_path ")(\\?" RE_query ")?)"
Ptypedef Pstring_ME(:"/" RE_hier_part "/":) hier_part_t;
#define RE_absoluteURI "(" RE_scheme "\\:(" RE_hier_part "|" RE_opaque_part "))"
Ptypedef Pstring_ME(:"/" RE_absoluteURI "/":) absoluteURI_t;
#define RE_Request_URI "(\\*|" RE_absoluteURI "|" RE_abs_path "|" RE_authority ")"
Ptypedef Pstring_ME(:"/" RE_Request_URI "/":) Request_URI_t;
#define RE_Request_Line "(" RE_Method "" RE_SP "" RE_Request_URI "" RE_SP "" RE_HTTP_Version "" RE_CRLF ")"
Ptypedef Pstring_ME(:"/" RE_Request_Line "/":) Request_Line_t;
#define RE_digest_uri_value RE_Request_URI
Ptypedef Pstring_ME(:"/" RE_digest_uri_value "/":) digest_uri_value_t;
#define RE_relativeURI "((" RE_net_path "|" RE_abs_path "|" RE_rel_path ")(\\?" RE_query ")?)"
Ptypedef Pstring_ME(:"/" RE_relativeURI "/":) relativeURI_t;
#define RE_username_value RE_quoted_string
Ptypedef Pstring_ME(:"/" RE_username_value "/":) username_value_t;
#define RE_value "(" RE_token "|" RE_quoted_string ")"
Ptypedef Pstring_ME(:"/" RE_value "/":) value_t;
#define RE_warn_text RE_quoted_string
Ptypedef Pstring_ME(:"/" RE_warn_text "/":) warn_text_t;
#define RE_weak "W\\/"
Ptypedef Pstring_ME(:"/" RE_weak "/":) weak_t;
#define RE_weekday "(Monday|Tuesday|Wednesday|Thursday|Friday|Saturday|Sunday)"
Ptypedef Pstring_ME(:"/" RE_weekday "/":) weekday_t;
#define RE_wkday "(Mon|Tue|Wed|Thu|Fri|Sat|Sun)"
Ptypedef Pstring_ME(:"/" RE_wkday "/":) wkday_t;
#define RE_word "(" RE_atom "|" RE_quoted_string ")"
Ptypedef Pstring_ME(:"/" RE_word "/":) word_t;
#define RE_obs_local_part "(" RE_word "(\\." RE_word ")*)"
Ptypedef Pstring_ME(:"/" RE_obs_local_part "/":) obs_local_part_t;
#define RE_local_part "(" RE_dot_atom "|" RE_quoted_string "|" RE_obs_local_part ")"
Ptypedef Pstring_ME(:"/" RE_local_part "/":) local_part_t;
#define RE_addr_spec "(" RE_local_part "\\@" RE_domain ")"
Ptypedef Pstring_ME(:"/" RE_addr_spec "/":) addr_spec_t;
#define RE_obs_angle_addr "(" RE_CFWS "?<" RE_obs_route "?" RE_addr_spec ">" RE_CFWS "?)"
Ptypedef Pstring_ME(:"/" RE_obs_angle_addr "/":) obs_angle_addr_t;
#define RE_angle_addr "((" RE_CFWS "?<" RE_addr_spec ">" RE_CFWS "?)|" RE_obs_angle_addr ")"
Ptypedef Pstring_ME(:"/" RE_angle_addr "/":) angle_addr_t;
#define RE_obs_phrase "(" RE_word "(" RE_word "|\\.|" RE_CFWS ")*)"
Ptypedef Pstring_ME(:"/" RE_obs_phrase "/":) obs_phrase_t;
#define RE_phrase "(" RE_word "+|" RE_obs_phrase ")"
Ptypedef Pstring_ME(:"/" RE_phrase "/":) phrase_t;
#define RE_display_name RE_phrase
Ptypedef Pstring_ME(:"/" RE_display_name "/":) display_name_t;
#define RE_name_addr "(" RE_display_name "?" RE_angle_addr ")"
Ptypedef Pstring_ME(:"/" RE_name_addr "/":) name_addr_t;
#define RE_mailbox "(" RE_name_addr "|" RE_addr_spec ")"
Ptypedef Pstring_ME(:"/" RE_mailbox "/":) mailbox_t;
#define RE_ws "(\\x20|\\x9)*"
Ptypedef Pstring_ME(:"/" RE_ws "/":) ws_t;
#define RE_Age "(Age" RE_ws "\\:" RE_ws "" RE_age_value ")"
Ptypedef Pstring_ME(:"/" RE_Age "/":) Age_t;
#define RE_Content_Length "(Content\\-Length" RE_ws "\\:" RE_ws "" RE_DIGIT "(" RE_ws "" RE_DIGIT ")*)"
Ptypedef Pstring_ME(:"/" RE_Content_Length "/":) Content_Length_t;
#define RE_Content_Location "(Content\\-Location" RE_ws "\\:" RE_ws "(" RE_absoluteURI "|" RE_relativeURI "))"
Ptypedef Pstring_ME(:"/" RE_Content_Location "/":) Content_Location_t;
#define RE_Content_MD5 "(Content\\-MD5" RE_ws "\\:" RE_ws "" RE_md5_digest ")"
Ptypedef Pstring_ME(:"/" RE_Content_MD5 "/":) Content_MD5_t;
#define RE_From "(From" RE_ws "\\:" RE_ws "" RE_mailbox ")"
Ptypedef Pstring_ME(:"/" RE_From "/":) From_t;
#define RE_Host "(Host" RE_ws "\\:" RE_ws "" RE_host "" RE_ws "(\\:" RE_ws "" RE_port ")?)"
Ptypedef Pstring_ME(:"/" RE_Host "/":) Host_t;
#define RE_Location "(Location" RE_ws "\\:" RE_ws "" RE_absoluteURI ")"
Ptypedef Pstring_ME(:"/" RE_Location "/":) Location_t;
#define RE_MIME_Version "(MIME\\-Version" RE_ws "\\:" RE_ws "" RE_DIGIT "(" RE_ws "" RE_DIGIT ")*" RE_ws "\\." RE_ws "" RE_DIGIT "(" RE_ws "" RE_DIGIT ")*)"
Ptypedef Pstring_ME(:"/" RE_MIME_Version "/":) MIME_Version_t;
#define RE_Max_Forwards "(Max\\-Forwards" RE_ws "\\:" RE_ws "" RE_DIGIT "(" RE_ws "" RE_DIGIT ")*)"
Ptypedef Pstring_ME(:"/" RE_Max_Forwards "/":) Max_Forwards_t;
#define RE_Referer "(Referer" RE_ws "\\:" RE_ws "(" RE_absoluteURI "|" RE_relativeURI "))"
Ptypedef Pstring_ME(:"/" RE_Referer "/":) Referer_t;
#define RE_accept_extension "(\\;" RE_ws "" RE_token "" RE_ws "(\\=" RE_ws "(" RE_token "|" RE_quoted_string "))?)"
Ptypedef Pstring_ME(:"/" RE_accept_extension "/":) accept_extension_t;
#define RE_algorithm "(algorithm" RE_ws "\\=" RE_ws "(MD5|MD5\\-sess|" RE_token "))"
Ptypedef Pstring_ME(:"/" RE_algorithm "/":) algorithm_t;
#define RE_auth_param "(" RE_token "" RE_ws "\\=" RE_ws "(" RE_token "|" RE_quoted_string "))"
Ptypedef Pstring_ME(:"/" RE_auth_param "/":) auth_param_t;
#define RE_cache_extension "(" RE_token "" RE_ws "(\\=" RE_ws "(" RE_token "|" RE_quoted_string "))?)"
Ptypedef Pstring_ME(:"/" RE_cache_extension "/":) cache_extension_t;
#define RE_cache_request_directive "(no\\-cache|no\\-store|(max\\-age" RE_ws "\\=" RE_ws "" RE_delta_seconds ")|(max\\-stale" RE_ws "(\\=" RE_ws "" RE_delta_seconds ")?)|(min\\-fresh" RE_ws "\\=" RE_ws "" RE_delta_seconds ")|no\\-transform|only\\-if\\-cached|" RE_cache_extension ")"
Ptypedef Pstring_ME(:"/" RE_cache_request_directive "/":) cache_request_directive_t;
#define RE_chunk_extension "(\\;" RE_ws "" RE_chunk_ext_name "" RE_ws "(\\=" RE_ws "" RE_chunk_ext_val ")?(" RE_ws "\\;" RE_ws "" RE_chunk_ext_name "" RE_ws "(\\=" RE_ws "" RE_chunk_ext_val ")?)*)?"
Ptypedef Pstring_ME(:"/" RE_chunk_extension "/":) chunk_extension_t;
#define RE_chunk_size "(" RE_HEX "(" RE_ws "" RE_HEX ")*)"
Ptypedef Pstring_ME(:"/" RE_chunk_size "/":) chunk_size_t;
#define RE_chunk_data "(" RE_chunk_size "" RE_ws "" RE_OCTET ")"
Ptypedef Pstring_ME(:"/" RE_chunk_data "/":) chunk_data_t;
#define RE_chunk "(" RE_chunk_size "" RE_ws "" RE_chunk_extension "?" RE_ws "" RE_CRLF "" RE_ws "" RE_chunk_data "" RE_ws "" RE_CRLF ")"
Ptypedef Pstring_ME(:"/" RE_chunk "/":) chunk_t;
#define RE_cnonce "(cnonce" RE_ws "\\=" RE_ws "" RE_cnonce_value ")"
Ptypedef Pstring_ME(:"/" RE_cnonce "/":) cnonce_t;
#define RE_commas "(" RE_ws "\\," RE_ws ")+"
Ptypedef Pstring_ME(:"/" RE_commas "/":) commas_t;
#define RE_Allow "(Allow" RE_ws "\\:" RE_ws "(" RE_Method "(" RE_commas "" RE_Method ")*)?)"
Ptypedef Pstring_ME(:"/" RE_Allow "/":) Allow_t;
#define RE_Connection "(Connection" RE_ws "\\:" RE_ws "" RE_connection_token "(" RE_commas "" RE_connection_token ")*)"
Ptypedef Pstring_ME(:"/" RE_Connection "/":) Connection_t;
#define RE_Content_Encoding "(Content\\-Encoding" RE_ws "\\:" RE_ws "" RE_content_coding "(" RE_commas "" RE_content_coding ")*)"
Ptypedef Pstring_ME(:"/" RE_Content_Encoding "/":) Content_Encoding_t;
#define RE_Content_Language "(Content\\-Language" RE_ws "\\:" RE_ws "" RE_language_tag "(" RE_commas "" RE_language_tag ")*)"
Ptypedef Pstring_ME(:"/" RE_Content_Language "/":) Content_Language_t;
#define RE_Trailer "(Trailer" RE_ws "\\:" RE_ws "" RE_field_name "(" RE_commas "" RE_field_name ")*)"
Ptypedef Pstring_ME(:"/" RE_Trailer "/":) Trailer_t;
#define RE_Vary "(Vary" RE_ws "\\:" RE_ws "(\\*|(" RE_field_name "(" RE_commas "" RE_field_name ")*)))"
Ptypedef Pstring_ME(:"/" RE_Vary "/":) Vary_t;
#define RE_acceptable_ranges "((" RE_range_unit "(" RE_commas "" RE_range_unit ")*)|none)"
Ptypedef Pstring_ME(:"/" RE_acceptable_ranges "/":) acceptable_ranges_t;
#define RE_Accept_Ranges "(Accept\\-Ranges" RE_ws "\\:" RE_ws "" RE_acceptable_ranges ")"
Ptypedef Pstring_ME(:"/" RE_Accept_Ranges "/":) Accept_Ranges_t;
#define RE_cache_response_directive "(public|(private" RE_ws "(\\=" RE_ws "\\\"" RE_ws "" RE_field_name "(" RE_commas "" RE_field_name ")*" RE_ws "\\\")?)|(no\\-cache" RE_ws "(\\=" RE_ws "\\\"" RE_ws "" RE_field_name "(" RE_commas "" RE_field_name ")*" RE_ws "\\\")?)|no\\-store|no\\-transform|must\\-revalidate|proxy\\-revalidate|(max\\-age" RE_ws "\\=" RE_ws "" RE_delta_seconds ")|(s\\-maxage" RE_ws "\\=" RE_ws "" RE_delta_seconds ")|" RE_cache_extension ")"
Ptypedef Pstring_ME(:"/" RE_cache_response_directive "/":) cache_response_directive_t;
#define RE_cache_directive "(" RE_cache_request_directive "|" RE_cache_response_directive ")"
Ptypedef Pstring_ME(:"/" RE_cache_directive "/":) cache_directive_t;
#define RE_Cache_Control "(Cache\\-Control" RE_ws "\\:" RE_ws "" RE_cache_directive "(" RE_commas "" RE_cache_directive ")*)"
Ptypedef Pstring_ME(:"/" RE_Cache_Control "/":) Cache_Control_t;
#define RE_challenge "(" RE_auth_scheme "" RE_SP "+" RE_auth_param "(" RE_commas "" RE_auth_param ")*)"
Ptypedef Pstring_ME(:"/" RE_challenge "/":) challenge_t;
#define RE_Proxy_Authenticate "(Proxy\\-Authenticate" RE_ws "\\:" RE_ws "" RE_challenge "(" RE_commas "" RE_challenge ")*)"
Ptypedef Pstring_ME(:"/" RE_Proxy_Authenticate "/":) Proxy_Authenticate_t;
#define RE_WWW_Authenticate "(WWW\\-Authenticate" RE_ws "\\:" RE_ws "" RE_challenge "(" RE_commas "" RE_challenge ")*)"
Ptypedef Pstring_ME(:"/" RE_WWW_Authenticate "/":) WWW_Authenticate_t;
#define RE_digest_uri "(uri" RE_ws "\\=" RE_ws "" RE_digest_uri_value ")"
Ptypedef Pstring_ME(:"/" RE_digest_uri "/":) digest_uri_t;
#define RE_disp_extension_parm "(" RE_token "" RE_ws "\\=" RE_ws "(" RE_token "|" RE_quoted_string "))"
Ptypedef Pstring_ME(:"/" RE_disp_extension_parm "/":) disp_extension_parm_t;
#define RE_entity_body "(" RE_OCTET "(" RE_ws "" RE_OCTET ")*)?"
Ptypedef Pstring_ME(:"/" RE_entity_body "/":) entity_body_t;
#define RE_entity_tag "(" RE_weak "?" RE_ws "" RE_opaque_tag ")"
Ptypedef Pstring_ME(:"/" RE_entity_tag "/":) entity_tag_t;
#define RE_ETag "(ETag" RE_ws "\\:" RE_ws "" RE_entity_tag ")"
Ptypedef Pstring_ME(:"/" RE_ETag "/":) ETag_t;
#define RE_If_Match "(If\\-Match" RE_ws "\\:" RE_ws "(\\*|(" RE_entity_tag "(" RE_commas "" RE_entity_tag ")*)))"
Ptypedef Pstring_ME(:"/" RE_If_Match "/":) If_Match_t;
#define RE_If_None_Match "(If\\-None\\-Match" RE_ws "\\:" RE_ws "(\\*|(" RE_entity_tag "(" RE_commas "" RE_entity_tag ")*)))"
Ptypedef Pstring_ME(:"/" RE_If_None_Match "/":) If_None_Match_t;
#define RE_expect_params "(\\;" RE_ws "" RE_token "" RE_ws "(\\=" RE_ws "(" RE_token "|" RE_quoted_string "))?)"
Ptypedef Pstring_ME(:"/" RE_expect_params "/":) expect_params_t;
#define RE_expectation_extension "(" RE_token "" RE_ws "(\\=" RE_ws "(" RE_token "|" RE_quoted_string ")" RE_ws "(" RE_expect_params "(" RE_ws "" RE_expect_params ")*)?)?)"
Ptypedef Pstring_ME(:"/" RE_expectation_extension "/":) expectation_extension_t;
#define RE_expectation "(100\\-continue|" RE_expectation_extension ")"
Ptypedef Pstring_ME(:"/" RE_expectation "/":) expectation_t;
#define RE_Expect "(Expect" RE_ws "\\:" RE_ws "" RE_expectation "(" RE_commas "" RE_expectation ")*)"
Ptypedef Pstring_ME(:"/" RE_Expect "/":) Expect_t;
#define RE_extension_code "(" RE_DIGIT "(" RE_ws "" RE_DIGIT "){2})?"
Ptypedef Pstring_ME(:"/" RE_extension_code "/":) extension_code_t;
#define RE_Status_Code "(100|101|200|201|202|203|204|205|206|300|301|302|303|304|305|307|400|401|402|403|404|405|406|407|408|409|410|411|412|413|414|415|416|417|500|501|502|503|504|505|" RE_extension_code ")"
Ptypedef Pstring_ME(:"/" RE_Status_Code "/":) Status_Code_t;
#define RE_Status_Line "(" RE_HTTP_Version "" RE_SP "" RE_Status_Code "" RE_SP "" RE_Reason_Phrase "" RE_CRLF ")"
Ptypedef Pstring_ME(:"/" RE_Status_Line "/":) Status_Line_t;
#define RE_extension_pragma "(" RE_token "" RE_ws "(\\=" RE_ws "(" RE_token "|" RE_quoted_string "))?)"
Ptypedef Pstring_ME(:"/" RE_extension_pragma "/":) extension_pragma_t;
#define RE_field_content0 "((" RE_token "|" RE_separators "|" RE_quoted_string ")(" RE_ws "(" RE_token "|" RE_separators "|" RE_quoted_string "))*)?"
Ptypedef Pstring_ME(:"/" RE_field_content0 "/":) field_content0_t;
#define RE_field_content "(" RE_TEXT "*|" RE_field_content0 ")"
Ptypedef Pstring_ME(:"/" RE_field_content "/":) field_content_t;
#define RE_field_value "((" RE_field_content "|" RE_LWS ")(" RE_ws "(" RE_field_content "|" RE_LWS "))*)?"
Ptypedef Pstring_ME(:"/" RE_field_value "/":) field_value_t;
#define RE_filename_parm "(filename" RE_ws "\\=" RE_ws "" RE_quoted_string ")"
Ptypedef Pstring_ME(:"/" RE_filename_parm "/":) filename_parm_t;
#define RE_disposition_parm "(" RE_filename_parm "|" RE_disp_extension_parm ")"
Ptypedef Pstring_ME(:"/" RE_disposition_parm "/":) disposition_parm_t;
#define RE_content_disposition "(Content\\-Disposition" RE_ws "\\:" RE_ws "" RE_disposition_type "" RE_ws "(\\;" RE_ws "" RE_disposition_parm "(" RE_ws "\\;" RE_ws "" RE_disposition_parm ")*)?)"
Ptypedef Pstring_ME(:"/" RE_content_disposition "/":) content_disposition_t;
#define RE_first_byte_pos "(" RE_DIGIT "(" RE_ws "" RE_DIGIT ")*)"
Ptypedef Pstring_ME(:"/" RE_first_byte_pos "/":) first_byte_pos_t;
#define RE_instance_length "(" RE_DIGIT "(" RE_ws "" RE_DIGIT ")*)"
Ptypedef Pstring_ME(:"/" RE_instance_length "/":) instance_length_t;
#define RE_language_range "(((" RE_ALPHA "(" RE_ws "" RE_ALPHA "){0,7})?" RE_ws "(\\-" RE_ws "(" RE_ALPHA "(" RE_ws "" RE_ALPHA "){0,7})?(" RE_ws "\\-" RE_ws "(" RE_ALPHA "(" RE_ws "" RE_ALPHA "){0,7})?)*)?)|\\*)"
Ptypedef Pstring_ME(:"/" RE_language_range "/":) language_range_t;
#define RE_last_byte_pos "(" RE_DIGIT "(" RE_ws "" RE_DIGIT ")*)"
Ptypedef Pstring_ME(:"/" RE_last_byte_pos "/":) last_byte_pos_t;
#define RE_byte_range_resp_spec "((" RE_first_byte_pos "" RE_ws "\\-" RE_ws "" RE_last_byte_pos ")|\\*)"
Ptypedef Pstring_ME(:"/" RE_byte_range_resp_spec "/":) byte_range_resp_spec_t;
#define RE_byte_content_range_spec "(" RE_bytes_unit "" RE_SP "" RE_byte_range_resp_spec "\\/(" RE_instance_length "|\\*))"
Ptypedef Pstring_ME(:"/" RE_byte_content_range_spec "/":) byte_content_range_spec_t;
#define RE_byte_range_spec "(" RE_first_byte_pos "" RE_ws "\\-" RE_ws "" RE_last_byte_pos "?)"
Ptypedef Pstring_ME(:"/" RE_byte_range_spec "/":) byte_range_spec_t;
#define RE_content_range_spec RE_byte_content_range_spec
Ptypedef Pstring_ME(:"/" RE_content_range_spec "/":) content_range_spec_t;
#define RE_Content_Range "(Content\\-Range" RE_ws "\\:" RE_ws "" RE_content_range_spec ")"
Ptypedef Pstring_ME(:"/" RE_Content_Range "/":) Content_Range_t;
#define RE_last_chunk "(0(" RE_ws "0)*" RE_ws "" RE_chunk_extension "?" RE_ws "" RE_CRLF ")"
Ptypedef Pstring_ME(:"/" RE_last_chunk "/":) last_chunk_t;
#define RE_message_body RE_entity_body
Ptypedef Pstring_ME(:"/" RE_message_body "/":) message_body_t;
Punion _bnf_0_t {
  field_value_t field_value;
  Pvoid(_bnf_141);
};
Pstruct message_header_t {
  field_name_t field_name;
  ws_t ws;
  ":";
  ws_t _bnf_142;
  _bnf_0_t _bnf_0;
};
Ptypedef message_header_t extension_header_t;
Pstruct message_qop_t {
  "qop";
  ws_t ws;
  "=";
  ws_t _bnf_143;
  qop_value_t qop_value;
};
Pstruct _bnf_144_t {
  ws_t ws;
  LHEX_t LHEX;
};
Parray _bnf_2_t {
  _bnf_144_t[7];
};
Pstruct _bnf_1_t {
  LHEX_t LHEX;
  _bnf_2_t _bnf_2;
};
Punion nc_value_t {
  _bnf_1_t _bnf_1;
  Pvoid(_bnf_145);
};
Pstruct nonce_t {
  "nonce";
  ws_t ws;
  "=";
  ws_t _bnf_146;
  nonce_value_t nonce_value;
};
Pstruct nonce_count_t {
  "nc";
  ws_t ws;
  "=";
  ws_t _bnf_147;
  nc_value_t nc_value;
};
Pstruct opaque_t {
  "opaque";
  ws_t ws;
  "=";
  ws_t _bnf_148;
  quoted_string_t quoted_string;
};
Pstruct parameter_t {
  attribute_t attribute;
  ws_t ws;
  "=";
  ws_t _bnf_149;
  value_t value;
};
Pstruct _bnf_4_t {
  type_t type;
  ws_t ws;
  "/";
  ws_t _bnf_150;
  "*";
};
Pstruct _bnf_5_t {
  type_t type;
  ws_t ws;
  "/";
  ws_t _bnf_151;
  subtype_t subtype;
};
Punion _bnf_3_t {
  __cident____ Pfrom("*/*");
  _bnf_4_t _bnf_4;
  _bnf_5_t _bnf_5;
};
Pstruct _bnf_152_t {
  ws_t ws;
  ";";
  ws_t _bnf_153;
  parameter_t parameter;
};
Parray _bnf_8_t {
  _bnf_152_t[] : Plongest;
};
Pstruct _bnf_7_t {
  ";";
  ws_t ws;
  parameter_t parameter;
  _bnf_8_t _bnf_8;
};
Punion _bnf_6_t {
  _bnf_7_t _bnf_7;
  Pvoid(_bnf_154);
};
Pstruct media_range_t {
  _bnf_3_t _bnf_3;
  ws_t ws;
  _bnf_6_t _bnf_6;
};
Pstruct _bnf_155_t {
  ws_t ws;
  ";";
  ws_t _bnf_156;
  parameter_t parameter;
};
Parray _bnf_11_t {
  _bnf_155_t[] : Plongest;
};
Pstruct _bnf_10_t {
  ";";
  ws_t ws;
  parameter_t parameter;
  _bnf_11_t _bnf_11;
};
Punion _bnf_9_t {
  _bnf_10_t _bnf_10;
  Pvoid(_bnf_157);
};
Pstruct media_type_t {
  type_t type;
  ws_t ws;
  "/";
  ws_t _bnf_158;
  subtype_t subtype;
  ws_t _bnf_159;
  _bnf_9_t _bnf_9;
};
Pstruct Content_Type_t {
  "Content-Type";
  ws_t ws;
  ":";
  ws_t _bnf_160;
  media_type_t media_type;
};
Punion pragma_directive_t {
  no_cache Pfrom("no-cache");
  extension_pragma_t extension_pragma;
};
Pstruct _bnf_161_t {
  commas_t commas;
  pragma_directive_t pragma_directive;
};
Parray _bnf_12_t {
  _bnf_161_t[] : Plongest;
};
Pstruct Pragma_t {
  "Pragma";
  ws_t ws;
  ":";
  ws_t _bnf_162;
  pragma_directive_t pragma_directive;
  _bnf_12_t _bnf_12;
};
Pstruct _bnf_14_t {
  "/";
  ws_t ws;
  product_version_t product_version;
};
Punion _bnf_13_t {
  _bnf_14_t _bnf_14;
  Pvoid(_bnf_163);
};
Pstruct product_t {
  token_t token;
  ws_t ws;
  _bnf_13_t _bnf_13;
};
Punion _bnf_15_t {
  product_t product;
  comment_t comment;
};
Punion _bnf_17_t {
  product_t _bnf_164;
  comment_t _bnf_165;
};
Pstruct _bnf_166_t {
  ws_t ws;
  _bnf_17_t _bnf_17;
};
Parray _bnf_16_t {
  _bnf_166_t[] : Plongest;
};
Pstruct Server_t {
  "Server";
  ws_t ws;
  ":";
  ws_t _bnf_167;
  _bnf_15_t _bnf_15;
  _bnf_16_t _bnf_16;
};
Pstruct _bnf_168_t {
  commas_t commas;
  product_t product;
};
Parray _bnf_18_t {
  _bnf_168_t[] : Plongest;
};
Pstruct Upgrade_t {
  "Upgrade";
  ws_t ws;
  ":";
  ws_t _bnf_169;
  product_t product;
  _bnf_18_t _bnf_18;
};
Punion _bnf_19_t {
  product_t _bnf_170;
  comment_t _bnf_171;
};
Punion _bnf_21_t {
  product_t _bnf_172;
  comment_t _bnf_173;
};
Pstruct _bnf_174_t {
  ws_t ws;
  _bnf_21_t _bnf_21;
};
Parray _bnf_20_t {
  _bnf_174_t[] : Plongest;
};
Pstruct User_Agent_t {
  "User-Agent";
  ws_t ws;
  ":";
  ws_t _bnf_175;
  _bnf_19_t _bnf_19;
  _bnf_20_t _bnf_20;
};
Pstruct _bnf_176_t {
  ws_t ws;
  DIGIT_t DIGIT;
};
Parray _bnf_27_t {
  _bnf_176_t[0:2];
};
Pstruct _bnf_26_t {
  DIGIT_t DIGIT;
  _bnf_27_t _bnf_27;
};
Punion _bnf_25_t {
  _bnf_26_t _bnf_26;
  Pvoid(_bnf_177);
};
Pstruct _bnf_24_t {
  ".";
  ws_t ws;
  _bnf_25_t _bnf_25;
};
Punion _bnf_23_t {
  _bnf_24_t _bnf_24;
  Pvoid(_bnf_178);
};
Pstruct _bnf_22_t {
  "0";
  ws_t ws;
  _bnf_23_t _bnf_23;
};
Pstruct _bnf_179_t {
  ws_t ws;
  "0";
};
Parray _bnf_33_t {
  _bnf_179_t[0:2];
};
Pstruct _bnf_32_t {
  "0";
  _bnf_33_t _bnf_33;
};
Punion _bnf_31_t {
  _bnf_32_t _bnf_32;
  Pvoid(_bnf_180);
};
Pstruct _bnf_30_t {
  ".";
  ws_t ws;
  _bnf_31_t _bnf_31;
};
Punion _bnf_29_t {
  _bnf_30_t _bnf_30;
  Pvoid(_bnf_181);
};
Pstruct _bnf_28_t {
  "1";
  ws_t ws;
  _bnf_29_t _bnf_29;
};
Punion qvalue_t {
  _bnf_22_t _bnf_22;
  _bnf_28_t _bnf_28;
};
Punion _bnf_34_t {
  charset_t charset;
  __cident__ Pfrom("*");
};
Pstruct _bnf_36_t {
  ";";
  ws_t ws;
  "q";
  ws_t _bnf_182;
  "=";
  ws_t _bnf_183;
  qvalue_t qvalue;
};
Punion _bnf_35_t {
  _bnf_36_t _bnf_36;
  Pvoid(_bnf_184);
};
Punion _bnf_38_t {
  charset_t _bnf_185;
  _bnf_186 Pfrom("*");
};
Pstruct _bnf_40_t {
  ";";
  ws_t ws;
  "q";
  ws_t _bnf_187;
  "=";
  ws_t _bnf_188;
  qvalue_t qvalue;
};
Punion _bnf_39_t {
  _bnf_40_t _bnf_40;
  Pvoid(_bnf_189);
};
Pstruct _bnf_190_t {
  commas_t commas;
  _bnf_38_t _bnf_38;
  ws_t ws;
  _bnf_39_t _bnf_39;
};
Parray _bnf_37_t {
  _bnf_190_t[] : Plongest;
};
Pstruct Accept_Charset_t {
  "Accept-Charset";
  ws_t ws;
  ":";
  ws_t _bnf_191;
  _bnf_34_t _bnf_34;
  ws_t _bnf_192;
  _bnf_35_t _bnf_35;
  _bnf_37_t _bnf_37;
};
Pstruct _bnf_42_t {
  ";";
  ws_t ws;
  "q";
  ws_t _bnf_193;
  "=";
  ws_t _bnf_194;
  qvalue_t qvalue;
};
Punion _bnf_41_t {
  _bnf_42_t _bnf_42;
  Pvoid(_bnf_195);
};
Pstruct _bnf_45_t {
  ";";
  ws_t ws;
  "q";
  ws_t _bnf_196;
  "=";
  ws_t _bnf_197;
  qvalue_t qvalue;
};
Punion _bnf_44_t {
  _bnf_45_t _bnf_45;
  Pvoid(_bnf_198);
};
Pstruct _bnf_199_t {
  commas_t commas;
  codings_t codings;
  ws_t ws;
  _bnf_44_t _bnf_44;
};
Parray _bnf_43_t {
  _bnf_199_t[] : Plongest;
};
Pstruct Accept_Encoding_t {
  "Accept-Encoding";
  ws_t ws;
  ":";
  ws_t _bnf_200;
  codings_t codings;
  ws_t _bnf_201;
  _bnf_41_t _bnf_41;
  _bnf_43_t _bnf_43;
};
Pstruct _bnf_47_t {
  ";";
  ws_t ws;
  "q";
  ws_t _bnf_202;
  "=";
  ws_t _bnf_203;
  qvalue_t qvalue;
};
Punion _bnf_46_t {
  _bnf_47_t _bnf_47;
  Pvoid(_bnf_204);
};
Pstruct _bnf_50_t {
  ";";
  ws_t ws;
  "q";
  ws_t _bnf_205;
  "=";
  ws_t _bnf_206;
  qvalue_t qvalue;
};
Punion _bnf_49_t {
  _bnf_50_t _bnf_50;
  Pvoid(_bnf_207);
};
Pstruct _bnf_208_t {
  commas_t commas;
  language_range_t language_range;
  ws_t ws;
  _bnf_49_t _bnf_49;
};
Parray _bnf_48_t {
  _bnf_208_t[] : Plongest;
};
Pstruct Accept_Language_t {
  "Accept-Language";
  ws_t ws;
  ":";
  ws_t _bnf_209;
  language_range_t language_range;
  ws_t _bnf_210;
  _bnf_46_t _bnf_46;
  _bnf_48_t _bnf_48;
};
Pstruct _bnf_211_t {
  ws_t ws;
  accept_extension_t accept_extension;
};
Parray _bnf_53_t {
  _bnf_211_t[] : Plongest;
};
Pstruct _bnf_52_t {
  accept_extension_t accept_extension;
  _bnf_53_t _bnf_53;
};
Punion _bnf_51_t {
  _bnf_52_t _bnf_52;
  Pvoid(_bnf_212);
};
Pstruct accept_params_t {
  ";";
  ws_t ws;
  "q";
  ws_t _bnf_213;
  "=";
  ws_t _bnf_214;
  qvalue_t qvalue;
  ws_t _bnf_215;
  _bnf_51_t _bnf_51;
};
Punion _bnf_56_t {
  accept_params_t accept_params;
  Pvoid(_bnf_216);
};
Punion _bnf_58_t {
  accept_params_t _bnf_217;
  Pvoid(_bnf_218);
};
Pstruct _bnf_219_t {
  commas_t commas;
  media_range_t media_range;
  ws_t ws;
  _bnf_58_t _bnf_58;
};
Parray _bnf_57_t {
  _bnf_219_t[] : Plongest;
};
Pstruct _bnf_55_t {
  media_range_t media_range;
  ws_t ws;
  _bnf_56_t _bnf_56;
  _bnf_57_t _bnf_57;
};
Punion _bnf_54_t {
  _bnf_55_t _bnf_55;
  Pvoid(_bnf_220);
};
Pstruct Accept_t {
  "Accept";
  ws_t ws;
  ":";
  ws_t _bnf_221;
  _bnf_54_t _bnf_54;
};
Pstruct realm_t {
  "realm";
  ws_t ws;
  "=";
  ws_t _bnf_222;
  realm_value_t realm_value;
};
Pstruct _bnf_61_t {
  ":";
  ws_t ws;
  port_t port;
};
Punion _bnf_60_t {
  _bnf_61_t _bnf_61;
  Pvoid(_bnf_223);
};
Pstruct _bnf_59_t {
  host_t host;
  ws_t ws;
  _bnf_60_t _bnf_60;
};
Punion received_by_t {
  _bnf_59_t _bnf_59;
  pseudonym_t pseudonym;
};
Pstruct _bnf_63_t {
  protocol_name_t protocol_name;
  ws_t ws;
  "/";
};
Punion _bnf_62_t {
  _bnf_63_t _bnf_63;
  Pvoid(_bnf_224);
};
Pstruct received_protocol_t {
  _bnf_62_t _bnf_62;
  ws_t ws;
  protocol_version_t protocol_version;
};
Punion _bnf_64_t {
  comment_t _bnf_225;
  Pvoid(_bnf_226);
};
Punion _bnf_66_t {
  comment_t _bnf_227;
  Pvoid(_bnf_228);
};
Pstruct _bnf_229_t {
  commas_t commas;
  received_protocol_t received_protocol;
  ws_t ws;
  received_by_t received_by;
  ws_t _bnf_230;
  _bnf_66_t _bnf_66;
};
Parray _bnf_65_t {
  _bnf_229_t[] : Plongest;
};
Pstruct Via_t {
  "Via";
  ws_t ws;
  ":";
  ws_t _bnf_231;
  received_protocol_t received_protocol;
  ws_t _bnf_232;
  received_by_t received_by;
  ws_t _bnf_233;
  _bnf_64_t _bnf_64;
  _bnf_65_t _bnf_65;
};
Pstruct _bnf_234_t {
  ws_t ws;
  LHEX_t LHEX;
};
Parray _bnf_69_t {
  _bnf_234_t[31];
};
Pstruct _bnf_68_t {
  LHEX_t LHEX;
  _bnf_69_t _bnf_69;
};
Punion _bnf_67_t {
  _bnf_68_t _bnf_68;
  Pvoid(_bnf_235);
};
Pstruct request_digest_t {
  "\"";
  ws_t ws;
  _bnf_67_t _bnf_67;
  ws_t _bnf_236;
  "\"";
};
Pstruct response_t {
  "response";
  ws_t ws;
  "=";
  ws_t _bnf_237;
  request_digest_t request_digest;
};
Punion start_line_t {
  Request_Line_t Request_Line;
  Status_Line_t Status_Line;
};
Pstruct _bnf_238_t {
  ws_t ws;
  message_header_t message_header;
  ws_t _bnf_239;
  CRLF_t CRLF;
};
Parray _bnf_72_t {
  _bnf_238_t[] : Plongest;
};
Pstruct _bnf_71_t {
  message_header_t message_header;
  ws_t ws;
  CRLF_t CRLF;
  _bnf_72_t _bnf_72;
};
Punion _bnf_70_t {
  _bnf_71_t _bnf_71;
  Pvoid(_bnf_240);
};
Punion _bnf_73_t {
  message_body_t message_body;
  Pvoid(_bnf_241);
};
Pstruct generic_message_t {
  start_line_t start_line;
  ws_t ws;
  _bnf_70_t _bnf_70;
  ws_t _bnf_242;
  CRLF_t CRLF;
  ws_t _bnf_243;
  _bnf_73_t _bnf_73;
};
Pstruct _bnf_244_t {
  ws_t ws;
  DIGIT_t DIGIT;
};
Parray _bnf_74_t {
  _bnf_244_t[] : Plongest;
};
Pstruct suffix_length_t {
  DIGIT_t DIGIT;
  _bnf_74_t _bnf_74;
};
Pstruct suffix_byte_range_spec_t {
  "-";
  ws_t ws;
  suffix_length_t suffix_length;
};
Punion _bnf_75_t {
  byte_range_spec_t byte_range_spec;
  suffix_byte_range_spec_t suffix_byte_range_spec;
};
Punion _bnf_77_t {
  byte_range_spec_t _bnf_245;
  suffix_byte_range_spec_t _bnf_246;
};
Pstruct _bnf_247_t {
  commas_t commas;
  _bnf_77_t _bnf_77;
};
Parray _bnf_76_t {
  _bnf_247_t[] : Plongest;
};
Pstruct byte_range_set_t {
  _bnf_75_t _bnf_75;
  _bnf_76_t _bnf_76;
};
Pstruct byte_ranges_specifier_t {
  bytes_unit_t bytes_unit;
  ws_t ws;
  "=";
  ws_t _bnf_248;
  byte_range_set_t byte_range_set;
};
Ptypedef byte_ranges_specifier_t ranges_specifier_t;
Pstruct Range_t {
  "Range";
  ws_t ws;
  ":";
  ws_t _bnf_249;
  ranges_specifier_t ranges_specifier;
};
Pstruct _bnf_250_t {
  ws_t ws;
  DIGIT_t DIGIT;
};
Parray _bnf_80_t {
  _bnf_250_t[1];
};
Pstruct _bnf_79_t {
  DIGIT_t DIGIT;
  _bnf_80_t _bnf_80;
};
Punion _bnf_78_t {
  _bnf_79_t _bnf_79;
  Pvoid(_bnf_251);
};
Pstruct _bnf_252_t {
  ws_t ws;
  DIGIT_t DIGIT;
};
Parray _bnf_83_t {
  _bnf_252_t[1];
};
Pstruct _bnf_82_t {
  DIGIT_t DIGIT;
  _bnf_83_t _bnf_83;
};
Punion _bnf_81_t {
  _bnf_82_t _bnf_82;
  Pvoid(_bnf_253);
};
Pstruct _bnf_254_t {
  ws_t ws;
  DIGIT_t DIGIT;
};
Parray _bnf_86_t {
  _bnf_254_t[1];
};
Pstruct _bnf_85_t {
  DIGIT_t DIGIT;
  _bnf_86_t _bnf_86;
};
Punion _bnf_84_t {
  _bnf_85_t _bnf_85;
  Pvoid(_bnf_255);
};
Pstruct time_http_t {
  _bnf_78_t _bnf_78;
  ws_t ws;
  ":";
  ws_t _bnf_256;
  _bnf_81_t _bnf_81;
  ws_t _bnf_257;
  ":";
  ws_t _bnf_258;
  _bnf_84_t _bnf_84;
};
Ptypedef Pstring_ME(:"/[[:DIGIT:]]{4}/":) _bnf_87_t;
Pstruct asctime_date_t {
  wkday_t wkday;
  SP_t SP;
  date3_t date3;
  SP_t _bnf_259;
  time_http_t time_http;
  SP_t _bnf_260;
  _bnf_87_t _bnf_87;
};
Pstruct rfc1123_date_t {
  wkday_t wkday;
  ",";
  SP_t SP;
  date1_t date1;
  SP_t _bnf_261;
  time_http_t time_http;
  SP_t _bnf_262;
  "GMT";
};
Pstruct rfc850_date_t {
  weekday_t weekday;
  ",";
  SP_t SP;
  date2_t date2;
  SP_t _bnf_263;
  time_http_t time_http;
  SP_t _bnf_264;
  "GMT";
};
Punion HTTP_date_t {
  rfc1123_date_t rfc1123_date;
  rfc850_date_t rfc850_date;
  asctime_date_t asctime_date;
};
Pstruct Date_t {
  "Date";
  ws_t ws;
  ":";
  ws_t _bnf_265;
  HTTP_date_t HTTP_date;
};
Pstruct Expires_t {
  "Expires";
  ws_t ws;
  ":";
  ws_t _bnf_266;
  HTTP_date_t HTTP_date;
};
Pstruct If_Modified_Since_t {
  "If-Modified-Since";
  ws_t ws;
  ":";
  ws_t _bnf_267;
  HTTP_date_t HTTP_date;
};
Punion _bnf_88_t {
  entity_tag_t entity_tag;
  HTTP_date_t HTTP_date;
};
Pstruct If_Range_t {
  "If-Range";
  ws_t ws;
  ":";
  ws_t _bnf_268;
  _bnf_88_t _bnf_88;
};
Pstruct If_Unmodified_Since_t {
  "If-Unmodified-Since";
  ws_t ws;
  ":";
  ws_t _bnf_269;
  HTTP_date_t HTTP_date;
};
Pstruct Last_Modified_t {
  "Last-Modified";
  ws_t ws;
  ":";
  ws_t _bnf_270;
  HTTP_date_t HTTP_date;
};
Punion _bnf_89_t {
  HTTP_date_t _bnf_271;
  delta_seconds_t delta_seconds;
};
Pstruct Retry_After_t {
  "Retry-After";
  ws_t ws;
  ":";
  ws_t _bnf_272;
  _bnf_89_t _bnf_89;
};
Punion entity_header_t {
  Allow_t Allow;
  Content_Encoding_t Content_Encoding;
  Content_Language_t Content_Language;
  Content_Length_t Content_Length;
  Content_Location_t Content_Location;
  Content_MD5_t Content_MD5;
  Content_Range_t Content_Range;
  Content_Type_t Content_Type;
  Expires_t Expires;
  Last_Modified_t Last_Modified;
  extension_header_t extension_header;
};
Punion response_header_t {
  Accept_Ranges_t Accept_Ranges;
  Age_t Age;
  ETag_t ETag;
  Location_t Location;
  Proxy_Authenticate_t Proxy_Authenticate;
  Retry_After_t Retry_After;
  Server_t Server;
  Vary_t Vary;
  WWW_Authenticate_t WWW_Authenticate;
};
Pstruct _bnf_273_t {
  ws_t ws;
  entity_header_t entity_header;
  ws_t _bnf_274;
  CRLF_t CRLF;
};
Parray _bnf_91_t {
  _bnf_273_t[] : Plongest;
};
Pstruct _bnf_90_t {
  entity_header_t entity_header;
  ws_t ws;
  CRLF_t CRLF;
  _bnf_91_t _bnf_91;
};
Punion trailer_t {
  _bnf_90_t _bnf_90;
  Pvoid(_bnf_275);
};
Pstruct _bnf_276_t {
  ws_t ws;
  chunk_t chunk;
};
Parray _bnf_94_t {
  _bnf_276_t[] : Plongest;
};
Pstruct _bnf_93_t {
  chunk_t chunk;
  _bnf_94_t _bnf_94;
};
Punion _bnf_92_t {
  _bnf_93_t _bnf_93;
  Pvoid(_bnf_277);
};
Pstruct Chunked_Body_t {
  _bnf_92_t _bnf_92;
  ws_t ws;
  last_chunk_t last_chunk;
  ws_t _bnf_278;
  trailer_t trailer;
  ws_t _bnf_279;
  CRLF_t CRLF;
};
Pstruct _bnf_280_t {
  ws_t ws;
  ";";
  ws_t _bnf_281;
  parameter_t parameter;
};
Parray _bnf_97_t {
  _bnf_280_t[] : Plongest;
};
Pstruct _bnf_96_t {
  ";";
  ws_t ws;
  parameter_t parameter;
  _bnf_97_t _bnf_97;
};
Punion _bnf_95_t {
  _bnf_96_t _bnf_96;
  Pvoid(_bnf_282);
};
Pstruct transfer_extension_t {
  token_t token;
  ws_t ws;
  _bnf_95_t _bnf_95;
};
Punion _bnf_99_t {
  accept_params_t _bnf_283;
  Pvoid(_bnf_284);
};
Pstruct _bnf_98_t {
  transfer_extension_t transfer_extension;
  ws_t ws;
  _bnf_99_t _bnf_99;
};
Punion t_codings_t {
  "trailers";
  _bnf_98_t _bnf_98;
};
Pstruct _bnf_285_t {
  commas_t commas;
  t_codings_t t_codings;
};
Parray _bnf_102_t {
  _bnf_285_t[] : Plongest;
};
Pstruct _bnf_101_t {
  t_codings_t t_codings;
  _bnf_102_t _bnf_102;
};
Punion _bnf_100_t {
  _bnf_101_t _bnf_101;
  Pvoid(_bnf_286);
};
Pstruct TE_t {
  "TE";
  ws_t ws;
  ":";
  ws_t _bnf_287;
  _bnf_100_t _bnf_100;
};
Punion transfer_coding_t {
  "chunked";
  transfer_extension_t transfer_extension;
};
Pstruct _bnf_288_t {
  commas_t commas;
  transfer_coding_t transfer_coding;
};
Parray _bnf_103_t {
  _bnf_288_t[] : Plongest;
};
Pstruct Transfer_Encoding_t {
  "Transfer-Encoding";
  ws_t ws;
  ":";
  ws_t _bnf_289;
  transfer_coding_t transfer_coding;
  _bnf_103_t _bnf_103;
};
Pstruct username_t {
  "username";
  ws_t ws;
  "=";
  ws_t _bnf_290;
  username_value_t username_value;
};
Punion _bnf_105_t {
  algorithm_t algorithm;
  Pvoid(_bnf_291);
};
Punion _bnf_106_t {
  cnonce_t cnonce;
  Pvoid(_bnf_292);
};
Punion _bnf_107_t {
  opaque_t opaque;
  Pvoid(_bnf_293);
};
Punion _bnf_108_t {
  message_qop_t message_qop;
  Pvoid(_bnf_294);
};
Punion _bnf_109_t {
  nonce_count_t nonce_count;
  Pvoid(_bnf_295);
};
Punion _bnf_110_t {
  auth_param_t auth_param;
  Pvoid(_bnf_296);
};
Punion _bnf_104_t {
  username_t username;
  realm_t realm;
  nonce_t nonce;
  digest_uri_t digest_uri;
  response_t response;
  _bnf_105_t _bnf_105;
  _bnf_106_t _bnf_106;
  _bnf_107_t _bnf_107;
  _bnf_108_t _bnf_108;
  _bnf_109_t _bnf_109;
  _bnf_110_t _bnf_110;
};
Punion _bnf_113_t {
  algorithm_t _bnf_297;
  Pvoid(_bnf_298);
};
Punion _bnf_114_t {
  cnonce_t _bnf_299;
  Pvoid(_bnf_300);
};
Punion _bnf_115_t {
  opaque_t _bnf_301;
  Pvoid(_bnf_302);
};
Punion _bnf_116_t {
  message_qop_t _bnf_303;
  Pvoid(_bnf_304);
};
Punion _bnf_117_t {
  nonce_count_t _bnf_305;
  Pvoid(_bnf_306);
};
Punion _bnf_118_t {
  auth_param_t _bnf_307;
  Pvoid(_bnf_308);
};
Punion _bnf_112_t {
  username_t _bnf_309;
  realm_t _bnf_310;
  nonce_t _bnf_311;
  digest_uri_t _bnf_312;
  response_t _bnf_313;
  _bnf_113_t _bnf_113;
  _bnf_114_t _bnf_114;
  _bnf_115_t _bnf_115;
  _bnf_116_t _bnf_116;
  _bnf_117_t _bnf_117;
  _bnf_118_t _bnf_118;
};
Pstruct _bnf_314_t {
  commas_t commas;
  _bnf_112_t _bnf_112;
};
Parray _bnf_111_t {
  _bnf_314_t[] : Plongest;
};
Pstruct digest_response_t {
  _bnf_104_t _bnf_104;
  _bnf_111_t _bnf_111;
};
Pstruct _bnf_119_t {
  "Basic";
  ws_t ws;
  basic_credentials_t basic_credentials;
};
Pstruct _bnf_120_t {
  "Digest";
  ws_t ws;
  digest_response_t digest_response;
};
Punion credentials_t {
  _bnf_119_t _bnf_119;
  _bnf_120_t _bnf_120;
};
Pstruct Authorization_t {
  "Authorization";
  ws_t ws;
  ":";
  ws_t _bnf_315;
  credentials_t credentials;
};
Pstruct Proxy_Authorization_t {
  "Proxy-Authorization";
  ws_t ws;
  ":";
  ws_t _bnf_316;
  credentials_t credentials;
};
Punion request_header_t {
  Accept_t Accept;
  Accept_Charset_t Accept_Charset;
  Accept_Encoding_t Accept_Encoding;
  Accept_Language_t Accept_Language;
  Authorization_t Authorization;
  Expect_t Expect;
  From_t From;
  Host_t Host;
  If_Match_t If_Match;
  If_Modified_Since_t If_Modified_Since;
  If_None_Match_t If_None_Match;
  If_Range_t If_Range;
  If_Unmodified_Since_t If_Unmodified_Since;
  Max_Forwards_t Max_Forwards;
  Proxy_Authorization_t Proxy_Authorization;
  Range_t Range;
  Referer_t Referer;
  TE_t TE;
  User_Agent_t User_Agent;
};
Pstruct _bnf_123_t {
  ":";
  ws_t ws;
  port_t port;
};
Punion _bnf_122_t {
  _bnf_123_t _bnf_123;
  Pvoid(_bnf_317);
};
Pstruct _bnf_121_t {
  host_t host;
  ws_t ws;
  _bnf_122_t _bnf_122;
};
Punion warn_agent_t {
  _bnf_121_t _bnf_121;
  pseudonym_t _bnf_318;
};
Pstruct _bnf_319_t {
  ws_t ws;
  DIGIT_t DIGIT;
};
Parray _bnf_125_t {
  _bnf_319_t[2];
};
Pstruct _bnf_124_t {
  DIGIT_t DIGIT;
  _bnf_125_t _bnf_125;
};
Punion warn_code_t {
  _bnf_124_t _bnf_124;
  Pvoid(_bnf_320);
};
Pstruct warn_date_t {
  "\"";
  ws_t ws;
  HTTP_date_t HTTP_date;
  ws_t _bnf_321;
  "\"";
};
Pstruct _bnf_127_t {
  SP_t SP;
  warn_date_t warn_date;
};
Punion _bnf_126_t {
  _bnf_127_t _bnf_127;
  Pvoid(_bnf_322);
};
Pstruct warning_value_t {
  warn_code_t warn_code;
  SP_t SP;
  warn_agent_t warn_agent;
  SP_t _bnf_323;
  warn_text_t warn_text;
  _bnf_126_t _bnf_126;
};
Pstruct _bnf_324_t {
  commas_t commas;
  warning_value_t warning_value;
};
Parray _bnf_128_t {
  _bnf_324_t[] : Plongest;
};
Pstruct Warning_t {
  "Warning";
  ws_t ws;
  ":";
  ws_t _bnf_325;
  warning_value_t warning_value;
  _bnf_128_t _bnf_128;
};
Punion general_header_t {
  Cache_Control_t Cache_Control;
  Connection_t Connection;
  Date_t Date;
  Pragma_t Pragma;
  Trailer_t Trailer;
  Transfer_Encoding_t Transfer_Encoding;
  Upgrade_t Upgrade;
  Via_t Via;
  Warning_t Warning;
};
Punion _bnf_131_t {
  general_header_t general_header;
  request_header_t request_header;
  entity_header_t entity_header;
};
Punion _bnf_133_t {
  general_header_t _bnf_326;
  request_header_t _bnf_327;
  entity_header_t _bnf_328;
};
Pstruct _bnf_329_t {
  ws_t ws;
  _bnf_133_t _bnf_133;
  ws_t _bnf_330;
  CRLF_t CRLF;
};
Parray _bnf_132_t {
  _bnf_329_t[] : Plongest;
};
Pstruct _bnf_130_t {
  _bnf_131_t _bnf_131;
  ws_t ws;
  CRLF_t CRLF;
  _bnf_132_t _bnf_132;
};
Punion _bnf_129_t {
  _bnf_130_t _bnf_130;
  Pvoid(_bnf_331);
};
Punion _bnf_134_t {
  message_body_t _bnf_332;
  Pvoid(_bnf_333);
};
Pstruct Request_t {
  Request_Line_t Request_Line;
  ws_t ws;
  _bnf_129_t _bnf_129;
  ws_t _bnf_334;
  CRLF_t CRLF;
  ws_t _bnf_335;
  _bnf_134_t _bnf_134;
};
Punion _bnf_137_t {
  general_header_t _bnf_336;
  response_header_t response_header;
  entity_header_t _bnf_337;
};
Punion _bnf_139_t {
  general_header_t _bnf_338;
  response_header_t _bnf_339;
  entity_header_t _bnf_340;
};
Pstruct _bnf_341_t {
  ws_t ws;
  _bnf_139_t _bnf_139;
  ws_t _bnf_342;
  CRLF_t CRLF;
};
Parray _bnf_138_t {
  _bnf_341_t[] : Plongest;
};
Pstruct _bnf_136_t {
  _bnf_137_t _bnf_137;
  ws_t ws;
  CRLF_t CRLF;
  _bnf_138_t _bnf_138;
};
Punion _bnf_135_t {
  _bnf_136_t _bnf_136;
  Pvoid(_bnf_343);
};
Punion _bnf_140_t {
  message_body_t _bnf_344;
  Pvoid(_bnf_345);
};
Pstruct Response_t {
  Status_Line_t Status_Line;
  ws_t ws;
  _bnf_135_t _bnf_135;
  ws_t _bnf_346;
  CRLF_t CRLF;
  ws_t _bnf_347;
  _bnf_140_t _bnf_140;
};
Punion HTTP_message_t {
  Request_t Request;
  Response_t Response;
};
